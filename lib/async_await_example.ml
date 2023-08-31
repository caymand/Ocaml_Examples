open Async_await.Scheduler

let main () =
  let task name () =
    Printf.printf "starting %s\n%!" name;
    let v = Random.int 100 in
    Printf.printf "yielding %s\n%!" name;
    yield ();
    Printf.printf "ending %s with %d\n%!" name v;
    v
  in
  let pa = async (task "a") in
  let pb = async (task "b") in
  let pc = async (fun () -> await pa + await pb) in
  Printf.printf "Sum is %d\n" (await pc);
  assert (await pa + await pb = await pc)

let foo () = run main
