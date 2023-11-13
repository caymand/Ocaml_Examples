module type GOTO = sig
  val label : string -> unit
  val goto : string -> unit
end

module Goto : GOTO = struct
  open Effect
  open Effect.Deep
  open Classes

  type thunk = { f : unit -> unit }
  type _ Effect.t +=
     | Label : string -> unit Effect.t
     | Goto : string -> unit Effect.t

  let list_monoid : (string * thunk) list monoid =
    let nat_elm : (string * thunk) list nat_elm = instance_nat_elm []
    in instance_monoid_list nat_elm
  open State.State(val list_monoid)


  let label l = perform (Label l)
  let goto l = perform (Goto l)

  let lookup_label l =
    let labels = get () in
    List.find (fun (s, _) -> s == l) labels
  
  (* Add label and duplicate lables will be handled by last declared. *)
  let add_label l f =
    modify @@ (fun ls -> (l, f) :: ls)

  let handle_effects f ~init =
    let box =
      match_with f init
        { exnc = raise;
          retc = (fun _ -> { f = fun _ -> () });
          (* retc = (fun () -> ()); *)
          effc = (fun (type a) (eff: a Effect.t) ->
            match eff with
            | Label l ->
               Some(fun (k: (a, _) continuation) ->
                   { f = fun () ->
                         let thunk = continue k in                         
                         ()
                   }
                 )
            | _ -> None
          );
        } in
    box
  
end
