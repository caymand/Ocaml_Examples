(* Attempt of implementing type classes in OCaml. *)

module type NAT_ELM = sig
  type t
  val nat_elm : unit -> t
end

type 'a nat_elm = (module NAT_ELM with type t = 'a)

(* This is how you make a type an instance of natural element. *)
let instance_nat_elm : 'a -> 'a nat_elm = fun (type a) (e : a) ->
  (module struct
     type t = a
     let nat_elm () = e
   end : NAT_ELM with type t = a)

(* Definition of a monoid *)
module type MONOID =
  sig
    type t
    val empty : unit -> t
    val (<>) : t -> t -> t
  end

type 'a monoid = (module MONOID with type t = 'a)

(* Here we use the natural element, which is a polymorphic list, to make
 list an instance of monoid*)
let instance_monoid_list : 'a list nat_elm -> 'a list monoid =
  fun (type a) (e : a list nat_elm) ->
  (module struct
     type t = a list
     let empty () = let module M = (val e) in M.nat_elm ()
     let (<>) = List.append
   end : MONOID with type t = a list
  )

                          

