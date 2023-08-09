open Ocaml_examples

let () =
  List.iter (fun x -> x ^ "\n" |> print_string) Env.Env_Example.main;
