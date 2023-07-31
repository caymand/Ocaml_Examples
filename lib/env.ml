module type ENV =
  functor (M : Classes.MONOID) ->
  sig
    open Classes
    
    type t = M.t
    type env = { e : t -> t}
    val runEnv : t monoid -> t -> ('a -> t) -> init:'a -> t
    val ask : unit -> t
    (* TODO: implement this.
       The intention is to run a computation in a modified environment.
       This original environment is then restored.*)
    (* val local : (t -> t) -> t *)
  end

module Env : ENV =
  functor(M: Classes.MONOID) -> struct
    open Classes
    open Effect
    open Effect.Deep
    
    type t = M.t
    type env = {e : t -> t }
    
    type _ Effect.t += Ask : t Effect.t
   
    (* Run a computation in an environment and return the final environment.
       It is required that the environment is a monoid.*)
    let build_env comp init =
      match_with comp init
        { retc = (fun x -> {e = fun _ -> x });
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
            | _ -> None
          );
        }
    
    (* Runs the computation and returns the final environment *)
    let runEnv (env_impl : t monoid) (initial_env : t) comp ~init =
      let open (val env_impl) in
      let env_builder = build_env comp init in
      env_builder.e initial_env

    let ask () : t = perform Ask
  end

(* This module shows how the algebraic effects would be used to run computations
 that can read from a shared environment.*)
module Env_Example = struct
  open Classes
  (* The ENV module expects a monoid. Here we use a list monoid to store the env. *)
  let list_monoid : string list monoid =
    let nat_elm : string list nat_elm = instance_nat_elm []
    in instance_monoid_list nat_elm
  
  let main =
    let module List_Monoid = (val list_monoid) in
    let open Env(List_Monoid) in
    let comp () =
      print_string "Starting comp\n";
      let env = ask () in
      let _ = List.iter print_string env in [""]
    in runEnv list_monoid ["foo"] comp ~init:()
end
  
