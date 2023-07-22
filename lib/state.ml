open Effect

module type STATE = sig
  type t
  val get : unit -> t
  val set : t -> unit
  val run : (unit -> t) -> init:t -> t
end


module State (S : sig type t end) : STATE with type t = S.t = struct
  open Effect.Deep
  type t = S.t
  type box = { f: t -> t}
  type _ Effect.t += Get : t Effect.t | Set : t -> unit Effect.t

  let get () = perform Get
  let set v = perform (Set v)
  let make_env (comp : unit -> t) =
    match_with comp ()
      { retc = (fun x -> {f = fun _ -> x } );
        exnc = raise;
        effc = (fun (type b) (eff : b Effect.t) ->
          match eff with
          (* Continue with current state,
             and return function with same state *)
          | Get -> Some (fun (k : (b, box) continuation) ->
                       { f = fun x ->
                             let box = (continue k x) in
                             box.f x
                     })
          (* Continue with unit, and return function with new state *)
          | Set v -> Some (fun (k : (b,_) continuation) ->
                         { f = fun _ ->
                               let box = (continue k ()) in
                               box.f v
                       })
          | _ -> None
        );
      }
  let run f ~init =
    let boxed_env = make_env f
    in boxed_env.f init
end

let main =
  let open State (struct type t = int end) in
  let random_comp () =
    let n = get () in
    set (n+1);
    get ()
  in
  let result = run random_comp ~init:0
  in Printf.printf "n: %d\n" result



  
