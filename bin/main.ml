open Ocaml_examples

let () =
  let a, s = State_example.main () in
  print_endline ("Final result: " ^ (string_of_int a));
  print_endline "Final state";
  List.iter print_endline s
  (* List.iter (fun x -> x ^ "\n" |> print_string) Env.Env_Example.main; *)
