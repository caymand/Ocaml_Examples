module type ENV =
  functor (M : Classes.MONOID) ->
  sig
    open Classes
    
    type t = M.t
    val withEnv : 'a 'b. t monoid -> t -> ('a -> 'b) -> init:'a -> t * 'b option
    val ask : unit -> t
    val local : (t -> t) -> ('a -> 'b) -> 'a -> 'b
  end

module Env : ENV =
  functor(M: Classes.MONOID) -> struct
    open Classes
    open Effect
    open Effect.Deep
    
    type t = M.t
    (* type 'b env = {e : t -> t; ret : 'b option} *)
    type env = { e : t -> t }
    
    type _ Effect.t +=
       | Ask : t Effect.t
       | Local : ((t->t) * ('a -> 'b) * 'a) -> 'b Effect.t   
   
    (* Run a computation in an environment and return the final environment.
       It is required that the environment is a monoid.*)
    (* let rec withEnv (env_impl : t monoid) (initial_env : t) (comp : 'a -> 'b) ~init = *)
    let rec withEnv : 'a 'b. t monoid -> t -> ('a -> 'b) -> init:'a -> t * 'b option =
      fun env_impl initial_env comp ~init ->
      let open (val env_impl) in
      let env_builder =
        match_with comp init
          { retc = (fun x -> ({ e = (fun _ -> empty ()) }, Some x));
            exnc = raise;
            effc = (fun (type a) (eff : a Effect.t) ->
              match eff with
              | Ask ->
                 Some(fun (k : (a, env * 'b option) continuation) ->
                     ({ e =  (fun x ->
                         let (env, _) = (continue k x) in
                         env.e x)
                     }, None)
                   )
              | Local (f, c, v) ->
                 Some (fun (k: (a, env * 'b option) continuation) ->
                     ({ e =  (fun x ->
                          let (_, res) = withEnv env_impl (f x) c ~init:v in
                          let (env, _) = (continue k (Option.get res)) in
                          env.e x)
                     }, None)
                   )
              | _ -> None
            );
          } in
      let (env, res) = env_builder in
      (env.e initial_env, res)
      
    let ask () : t = perform Ask
    let local f comp v = perform (Local (f, comp, v))
      
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
      print_string "This is my env:\n";
      List.iter print_string env in
    let comp2 x =
      print_string "Starting comp 2\n";
      let env = ask () in
      List.iter print_string env;
      2 + x in
    let comp () =
      let _ = local (fun bar -> ["Foo"] <> bar) comp1 () in
      print_string "First comp returned: ()\n";
      let r2 = local Fun.id comp2 40 in
      Printf.printf "Second comp returned: %d\n" r2 in
    let (env, _) = withEnv list_monoid ["Bar"] comp ~init:() in
    env
      
end
  
