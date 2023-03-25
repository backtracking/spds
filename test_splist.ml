
open Splist

let is_interval i j l =
  assert (length l = j-i+1);
  for k = i to j do assert (mem k l) done

let l0 = cons 0 (nil ())
let () = is_interval 0 0 l0

let l1 = cons 1 l0
let l2 = cons 2 l1
let () = assert (head l2 = 2)
let () = is_interval 0 2 l2
let l3 = cons 2 l1
let l4 = cons 3 l3
let () = assert (head l4 = 3)
let () = is_interval 0 3 l4

let () = assert (head l0 = 0) (* back to l0 *)
let l5 = cons 2 l0
let l6 = cons 1 l5
let () = is_interval 0 2 l6
