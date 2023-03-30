
(* Semi-persistent lists.

   Principle: reuse previous `cons` (using the field `prev` below).

   Note: Contrary to semi-persistent arrays, it would be really difficult to
   make a defensive versions of semi-persistent lists. *)

type t = {
  mutable head: int;
          tail: t;
  mutable prev: t; (* a previous `cons` on that list, if not nil *)
}

let nil () =
  let rec nil = { head = 42; tail = nil; prev = nil } in nil

let is_nil l =
  l.tail == l

let head l =
  if is_nil l then invalid_arg "head";
  l.head

let tail l =
  if is_nil l then invalid_arg "tail";
  l.tail

let cons x l =
  if is_nil l.prev then (
    let n = { l with head = x; tail = l } in
    l.prev <- n;
    n
  ) else (
    let c = l.prev in
    c.head <- x;
    c
  )

let rec length l =
  if is_nil l then 0 else 1 + length l.tail

let rec mem x l =
  not (is_nil l) && (l.head = x || mem x l.tail)

let rec iter f l =
  if not (is_nil l) then (f l.head; iter f l.tail)

(* Note: It is tempting to implement semi-persistent lists as follows,
   using "pointers" into a single resizable array. But if we do so,
   we ruin the space benefits of semi-persistent lists.

type t = {
    v: int Vector.t;
  idx: int;
}

let nil () =
  { v = Vector.create ~dummy:42; idx = 0; }

let is_nil l =
  l.idx = 0

let reroot l =
  Vector.resize l.v l.idx

let head l =
  if is_nil l then invalid_arg "head";
  reroot l;
  Vector.top l.v

let tail l =
  if is_nil l then invalid_arg "tail";
  reroot l;
  ignore (Vector.pop l.v);
  { l with idx = l.idx - 1 }

let cons x l =
  reroot l;
  Vector.push l.v x;
  { l with idx = l.idx + 1 }

let length l =
  l.idx

let mem x l =
  reroot l;
  let rec mem i = i < l.idx && (Vector.get l.v i = x || mem (i+1)) in
  mem 0

let iter f l =
  reroot l;
  Vector.iter f l.v

*)
