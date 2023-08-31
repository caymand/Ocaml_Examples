module type ENV =
  functor (M : Classes.MONOID) ->
  sig
    open Classes
    
    type t = M.t
    type env = { e : t -> t}
    val runEnv : 'a. t monoid -> t -> (unit -> unit) -> init:unit -> t
    val ask : unit -> t
    val local : (t -> t) -> (unit -> unit) -> t
  end

module Env : ENV =
  functor(M: Classes.MONOID) -> struct
    open Classes
    open Effect
    open Effect.Deep
    
    type t = M.t
    type env = {e : t -> t }
    
    type _ Effect.t +=
       | Ask : t Effect.t
       | Local : ((t->t) * (unit -> unit)) -> t Effect.t
   
    (* Run a computation in an environment and return the final environment.
       It is required that the environment is a monoid.*)
    (* let rec runEnv : 'a. t monoid -> t -> ('a -> t) -> init:'a -> t = *)
    (*   fun env_impl initial_env comp ~init ->  *)
    let rec runEnv (env_impl : t monoid) (initial_env : t) (comp : unit -> unit) ~init =
      let open (val env_impl) in
      let env_builder =
        match_with comp init
          { retc =
              (fun _ -> {e = fun _ -> empty () });
            exnc = raise;
            effc = (fun (type a) (eff : a Effect.t) ->
              match eff with
              | Ask ->
                 Some(fun (k : (a, env) continuation) ->
                     { e =  fun x ->
                            let env = (continue k x) in
                            env.e x
                     }
                   )
              | Local (f, comp) ->
                 Some (fun (k: (a, _) continuation) ->
                     { e =  fun x ->
                            let _ = runEnv env_impl (f x) comp ~init:() in
                            let env = (continue k x) in
                            env.e x
                     }
                   )
              | _ -> None
            );
          } in
      env_builder.e initial_env
      
    let ask () : t = perform Ask
    let local f comp =
      perform (Local (f, comp))
      
  end

(* This module shows how the algebraic effects would be used to
   run computations that can read from a shared environment.

   Notice we spawn a computation comp. It in turns runs two computations.
   Each of them gets a different environment, and the initial environment is
   restored.
 *)
module Env_Example = struct
  open Classes
  (* The ENV module expects a monoid. Here we use a list monoid to store the env. *)
  let list_monoid : string list monoid =
    let nat_elm : string list nat_elm = instance_nat_elm []
    in instance_monoid_list nat_elm
  
  let main =
    let module List_Monoid = (val list_monoid) in
    let open Env(List_Monoid) in
    let open List_Monoid in
    let comp1 () =
      print_string "Starting comp 1\n";
      let env = ask () in
      print_string "comp 1 env: ";
      let _ = List.iter print_string env in
      print_string "\nend comp 1\n"; in
    let comp2 () =
      print_string "Starting comp 2\n";
      let env = ask () in
      print_string "comp 2 env: ";
      let _ = List.iter print_string env in
      print_string "\nend comp 2\n"; in
    let comp () =
      print_string "starting comp\n";
      let _ = comp1 () in
      let _ = local ((<>) ["bar"]) comp2 in
      let env = ask () in
      print_string "final env: ";
      let _ = List.iter print_string env in
      print_string "\nend comp\n";
    in runEnv list_monoid ["foo"] comp ~init:()
end
  
