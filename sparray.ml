
(* Semi-persistent arrays.

   Principle: use Baker's persistent arrays, but save some of the work.

   Note: this is a defensive version, where we use constructor `Invalid`
   to invalidate a version. This is not needed if we statically check
   that the semi-persistent array is correctly used. *)

type 'a t =
  'a data ref

and 'a data =
| Base of 'a array
| Diff of int * 'a * 'a t
| Invalid

let make n v =
  if n < 0 then invalid_arg "make";
  ref (Base (Array.make n v))

let init n f =
  if n < 0 then invalid_arg "init";
  ref (Base (Array.init n f))

let rec reroot t = match !t with
  | Base a ->
      a
  | Diff (i, v, t') ->
      let a = reroot t' in
      a.(i) <- v;
      t := !t';
      t' := Invalid;
      a
  | Invalid ->
      assert false

let get t i =
  let a = reroot t in
  a.(i)

let set t i v =
  let a = reroot t in
  let old = a.(i) in
  a.(i) <- v;
  let res = ref !t in
  t := Diff (i, old, res);
  res

let rec nd_get t i = match !t with
  | Base a          -> a.(i)
  | Diff (j, v, t') -> if i = j then v else nd_get t' i
  | Invalid         -> assert false
