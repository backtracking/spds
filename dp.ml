
(* A decision procedure for the theory of semi-persistence,
   using semi-persistent data structures!

   This is far from being efficient by many respects.
   This is just a proof of concept.
*)

type term =
  { tid  : int;
    tkind: tkind; }
and tkind =
  | Tvar  of string
  | Tprev of term

type atom =
  | Aeq   of term * term
  | Apath of term * term

type fmla =
  | Fatom   of atom
  | Fand    of fmla * fmla
  | Fimp    of atom * fmla
  | Fforall of string * fmla

(* term numbering and smart constructors *)

let next = ref 0
let terms = Hashtbl.create 16
let memo k =
    try
      Hashtbl.find terms k
    with Not_found ->
      let t = { tid = !next; tkind = k } in incr next; Hashtbl.add terms k t; t

let var  x = memo (Tvar  x)
let prev t = memo (Tprev t)

let aequal t1 t2 = Aeq   (t1, t2)
let apath  t1 t2 = Apath (t1, t2)

let fequal t1 t2 = Fatom   (aequal t1 t2)
let fpath  t1 t2 = Fatom   (apath  t1 t2)
let fand   f1 f2 = Fand    (f1, f2)
let fimp   al f  = Fimp    (al, f)
let forall x  f  = Fforall (x, f)

(* decision procedure *)

type ctx = {
  uf: Spuf.t;
  gr: Spgr.t;
}
(* INVARIANTS:
   1 - `uf` is closed by congruence (for both `prev` and `path`)
   2 - if `t1=t2` in `uf`, then `t1->t2` and `t2->t1` in the graph
   3 - for any term `prev(t)`, we have an arc 'prev(t) -> t` in the graph
*)

(* ensure invariant 3 *)
let consider_term ctx t = match t.tkind with
  | Tprev p -> { ctx with gr = Spgr.add_edge ctx.gr t.tid p.tid }
  | Tvar _  -> ctx
let consider_atom ctx = function
  | Aeq (t1, t2) | Apath (t1, t2) -> consider_term (consider_term ctx t1) t2
let rec consider_fmla ctx = function
  | Fatom a        -> consider_atom ctx a
  | Fand (f1, f2)  -> consider_fmla (consider_fmla ctx f1) f2
  | Fimp (a, f)    -> consider_fmla (consider_atom ctx a) f
  | Fforall (_, f) -> consider_fmla ctx f

(* NOTE: we could return the modified UF data structures *)
let test_equal ctx t1 t2 =
  let _, r1 = Spuf.find ctx.uf t1.tid in
  let _, r2 = Spuf.find ctx.uf t2.tid in
  r1 = r2

let test_path ctx t1 t2 =
  Spgr.exists_path ctx.gr t1.tid t2.tid

let fold_terms2 acc f =
  Hashtbl.fold (fun _ t1 acc ->
  Hashtbl.fold (fun _ t2 acc -> f acc t1 t2) terms acc) terms acc

let add_path ctx t1 t2 =
 if test_path ctx t1 t2 then ctx else
 { ctx with gr = Spgr.add_edge ctx.gr t1.tid t2.tid }

let rec add_equality ctx t1 t2 =
  if test_equal ctx t1 t2 then ctx else
  let ctx = { ctx with uf = Spuf.union ctx.uf t1.tid t2.tid } in
  (* invariant 2: *)
  let ctx = add_path (add_path ctx t1 t2) t2 t1 in
  (* invariant 1: *)
  let cc ctx t1 t2 = match t1.tkind, t2.tkind with
    | Tprev u1, Tprev u2 when test_equal ctx u1 u2 -> add_equality ctx t1 t2
    | _ -> ctx in
  fold_terms2 ctx cc

let decide_atom ctx = function
  | Aeq   (t1, t2) -> test_equal ctx t1 t2
  | Apath (t1, t2) -> test_path  ctx t1 t2

let assume ctx = function
  | Aeq   (t1, t2) -> add_equality ctx t1 t2
  | Apath (t1, t2) -> add_path     ctx t1 t2

let decide f =
  Format.eprintf "decide: %d terms@." !next;
  let rec dec ctx = function
    | Fatom a -> decide_atom ctx a
    | Fand (f1, f2) -> dec ctx f1 && dec ctx f2
    | Fimp (a, f) -> dec (assume ctx a) f
    | Fforall (_, f) -> dec ctx f
  in
  let ctx = { uf = Spuf.create !next; gr = Spgr.create !next; } in
  let ctx = consider_fmla ctx f in
  dec ctx f


