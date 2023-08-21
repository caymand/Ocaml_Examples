open Classes
let list_monoid : string list monoid =
  let nat_elm : string list nat_elm = instance_nat_elm []
  in instance_monoid_list nat_elm

let main () =
  let module List_Monoid = (val list_monoid) in
  let module S = State.State(List_Monoid) in
  let open List_Monoid in
  let comp x =
    let y = x + 2 in
    let expr = (string_of_int x) ^ " + 2 = " ^ (string_of_int y) in
    let _ = S.put ["foo"] in    (* ignored *)
    let _ = S.put [expr] in
    let s = S.get () in
    let s' = ["Doing comp:\n"] <> s in
    let _ = S.put s' in
    y in
  S.withState list_monoid [] comp 40
