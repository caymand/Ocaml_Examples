
module type ENV = sig
  open Classes
  open Effect

  type t
  type env = { e : t -> t}
  val ask : unit -> t
  val runEnv : t monoid -> t -> ('a -> t) -> init:'a -> t 
end

module Env : ENV = struct
  open Classes
  open Effect

  type t
  type env = {e : t -> t }
  
  type _ Effect.t += Ask : t Effect.t
  (* Run a computation in an environment and return the final environment.
     It is required that the environment is a monoid.*)
  let build_env comp init =
    let open Effect.Deep in
    match_with comp init
      { retc = (fun x -> {e = fun _ -> x });
        exnc = raise;
        effc = (fun (type b) (eff : b Effect.t) ->
          match eff with
          | Ask ->
             Some(fun (k : (b, env) continuation) ->
                 { e =  fun x ->
                        let env = (continue k x) in
                        env.e x
                 }
               )
          | _ -> None
        );
      }
  let runEnv (env_impl : t monoid) (initial_env : t) comp ~init =
    let open (val env_impl) in
    let env_builder = build_env comp init in
    env_builder.e initial_env

  let ask () = perform Ask
  
end

module Env_Example = struct
  let list_env = (module Env : ENV with with type t = char list) in
  let open (val list_env) in
  let main =
    let comp () =
      print_string "Starting comp";
      let env = ask () in
      List.iter print_char env
      
    in comp ()
end
  


  (* Initialize the list monoid module *)
  (* let list_monoid : char list monoid = *)
  (*   let nat_elm : char list nat_elm = instance_nat_elm [] *)
  (*   in instance_monoid_list nat_elm *)

  (* let main () = *)
  (*   (\* Open the list monoid for local use *\) *)
  (*   let open (val list_monoid) in *)
  (*   let es = (empty ()) <> ['a';'b'] in *)
  (*   List.iter print_char es *)
