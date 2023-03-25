
open Dp

let (==>) = fimp
let (&&&) = fand

let () =
  let x = var "x" in
  let y = var "y" in
  let z = var "z" in
  assert (decide (fequal x x));
  assert (not (decide (fequal x y)));
  (* A1 *) assert (decide (fpath x x));
  (* A2 *) assert (decide (apath x (prev y) ==> fpath x y));
  (* A3 *) assert (decide (apath x y ==> (apath y z ==> fpath x z)));
  assert (decide (fpath (prev x) x));
  assert (not (decide (fpath x (prev x))));
  assert (decide (apath x y ==> fpath (prev x) y));
  assert (not (decide (apath x y ==> (apath z y ==> fpath x z))));
  assert (decide (aequal x y ==> fpath (prev x) y));
  ()

(* examples from the paper *)
let () =
  (* C(f) *)
  let x0 = var "x0" and cur = var "cur" in
  let x1 = var "x1" and cur1 = var "cur1" in
  let x2 = var "x2" and cur2 = var "cur2" in
  let cf =
    forall "x0" (forall "cur" (apath x0 cur ==> (
    fpath x0 cur &&&
    forall "x1" (forall "cur1" (aequal (prev x1) x0 ==> (aequal cur1 x1 ==>
    fpath x0 cur1 &&&
    forall "x2" (forall "cur2" (aequal (prev x2) x0 ==> (aequal cur2 x2 ==>
    fpath x2 cur2
  ))) ))) ))) in
  assert (decide cf);

  (* C(g) =
        forall cur. forall x0. path x0 cur ->
          path x0 cur /\
          forall cur1. let x1 = cur1 in prev cur1 = x0 ->
            path x0 cur1 /\
             forall cur2. let x2 = cur2 in prev cur2 = x0 ->
               path cur1 cur2
 *)
  let cg =
    forall "x0" (forall "cur" (apath x0 cur ==> (
    fpath x0 cur &&&
    forall "x1" (forall "cur1" (aequal (prev x1) x0 ==> (aequal cur1 x1 ==>
    fpath x0 cur1 &&&
    forall "x2" (forall "cur2" (aequal (prev x2) x0 ==> (aequal cur2 x2 ==>
    fpath x1 cur2
  ))) ))) ))) in
  assert (not (decide cg));

  (* C(bt) *)

  let x3 = var "x3" and cur3 = var "cur3" in
  let x4 = var "x4" and cur4 = var "cur4" in

  let cbt =
    forall "x0" (forall "cur" (apath x0 cur ==> (
    fpath x0 cur &&&
    forall "x1" (forall "cur1" (aequal (prev x1) x0 ==> (aequal cur1 x1 ==>
    fpath x1 cur1 &&&
    forall "x2" (forall "cur2" (apath x1 cur2 ==> (
    fpath x0 cur2 &&&
    forall "x3" (forall "cur3" (aequal (prev x3) x0 ==> (aequal cur3 x3 ==>
    fpath x3 cur3 &&&
    forall "x4" (forall "cur4" (apath x3 cur4 ==> fpath x0 cur4
   ))))) ))))))))) in

  assert (decide cbt);

  ()

(* And now the bootstrap! *)

(* C(decide) =
  forall cur x. path x cur ->
    (* Fatom n *)
    path x cur /\ path x cur
    /\
    (* Fand f1 f2 *)
    path x cur /\
    forall cur1. path x cur1 ->
      path x cur1 /\
      path x cur1
    /\
    (* Fimp _ f1 *)
    path x cur /\
    forall cur1. prev cur1 = x ->
      path cur1 cur1 /\
      forall cur2. path cur1 cur2 ->
      path x cur2
*)

let () =
  let x = var "x" and cur = var "cur" in
  let cur1 = var "cur1" and cur2 = var "cur2" in
  let bootstrap =
    forall "cur" (forall "x" (apath x cur ==> (
      (* atom *)
      fpath x cur &&& fpath x cur
      &&&
      (* and *)
      fpath x cur &&& forall "cur1" (apath x cur1 ==> (
        fpath x cur1 &&& fpath x cur1
      ))
      &&&
      (* implies *)
      fpath x cur &&& forall "cur1" (aequal (prev cur1) x ==> (
        fpath cur1 cur1 &&& forall "cur2" (apath cur1 cur2 ==> (
          fpath x cur2
      )) ))
  ))) in
  assert (decide bootstrap)
