
module type Scheduler = sig
  type 'a promise
  (** Type of promises *)
  
  val async : (unit -> 'a) -> 'a promise
  (** [async f] runs [f] concurrently *)
  
  val await : 'a promise -> 'a
  (** [await p] returns the result of the promise. *)
  
  val yield : unit -> unit
  (** yields control to another task *)
  
  val run   : (unit -> 'a) -> unit
  (** Runs the scheduler *)
end

module Scheduler : Scheduler = struct

  open Effect
  open Effect.Deep

  type 'a _promise =
    Waiting of ('a,unit) continuation list
  | Done of 'a

  type 'a promise = 'a _promise ref

  type _ Effect.t += Async : (unit -> 'a) -> 'a promise Effect.t
                   | Yield : unit Effect.t
                   | Await : 'a promise -> 'a Effect.t

  let async f = perform (Async f)

  let yield () = perform Yield

  let await p = perform (Await p)

  let q = Queue.create ()
  let enqueue t = Queue.push t q
  let dequeue () =
    if Queue.is_empty q then ()
    else Queue.pop q ()

  let run main =
    let rec fork : 'a. 'a promise -> (unit -> 'a) -> unit =
      fun pr main ->
        match_with main ()
          { retc = (fun v ->
              begin match !pr with
              | Waiting ks ->
                 (* Promise is now resolved and waiting threads
                  can resume with resolved promise.*)
                 pr := Done v;
                 List.iter (fun k -> continue k v) ks;
                 (* Now resume other threads *)
                 dequeue ();
              | _ -> failwith "Should not happen"
              end
            );
          exnc = (fun e -> print_endline "FAILED"; raise e);
          effc = (fun (type b) (eff: b Effect.t) ->
              match eff with
              | Async f -> (
                Some (fun (k: (b,_) continuation) ->
                    (* Promise of this async operation *)
                    let p = ref (Waiting []) in
                    (* Enqueue the work after this function
                     and fork off to run the async function*)
                    enqueue (fun () -> continue k p);
                    fork p f
              ))
              | Yield -> (
                Some (fun k ->
                    enqueue (continue k);
                    dequeue ()
              ))
              | Await p -> (Some (fun (k: (b,_) continuation) ->
                begin match !p with
                | Done v -> continue k v
                | Waiting ks ->
                   (* The continuation is also a task
                      waiting for the promise to resolve *)
                   p := Waiting (k :: ks);
                   dequeue ()
                end
              ))
              | _ -> None
          )}
    in
    fork (ref (Waiting [])) main
end
