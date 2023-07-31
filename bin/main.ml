open Ocaml_examples

let () =
  print_string "\nFinal env\n";
  List.iter (fun x -> x ^ "\n" |> print_string) Env.Env_Example.main;
