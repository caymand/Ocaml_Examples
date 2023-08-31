module type STATE =
  functor (M : Classes.MONOID) ->
  sig
    open Classes
    type t = M.t
    val withState : t monoid -> t -> ('a -> 'b) -> 'a -> 'b * t
    val get : unit -> t
    val put : t -> unit                     
  end

module State : STATE =
  functor (M : Classes.MONOID) ->
  struct
  open Classes
  open Effect
  open Effect.Deep

  type t = M.t
  type 'a state = { s : t -> 'a * t }
  type _ Effect.t +=
     | Get : t Effect.t
     | Put : t -> unit Effect.t
     (* | State : (t -> 'a * t) -> unit Effect.t *)

  let put v = perform (Put v)
  let get () = perform Get
  
  let withState (state_mod: t monoid) (init_state : t) (comp : 'a -> 'b) (a : 'a) =
    let state_builder =
      let open (val state_mod) in
      match_with comp a
        { exnc = raise;
          retc = (fun r -> { s = fun e -> r, e });
          effc = (fun (type b) (eff : b Effect.t) ->
            match eff with
            | Get ->
               Some (fun (k : (b, _) continuation) ->
                   { s = fun x ->
                         let cur_state = continue k x in
                         cur_state.s x
                   }
                 )
            | Put v ->
               Some (fun (k : (b, _) continuation) ->
                   { s = fun _ ->
                         let cur_state = continue k () in
                         cur_state.s v
                   }
                 )
            | _ -> None
          );
        }
    in state_builder.s init_state
end
