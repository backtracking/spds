
type 'a t =
  'a data ref

and 'a data =
| A of 'a array
| D of int * 'a * 'a t
| I

let make n v =
  if n < 0 then invalid_arg "make";
  ref (A (Array.make n v))

let init n f =
  if n < 0 then invalid_arg "init";
  ref (A (Array.init n f))

let rec reroot t = match !t with
  | A a ->
      a
  | D (i, v, t') ->
      let a = reroot t' in
      a.(i) <- v;
      t := !t';
      t' := I;
      a
  | I ->
      assert false

let get t i =
  let a = reroot t in
  a.(i)

let set t i v =
  let a = reroot t in
  let old = a.(i) in
  a.(i) <- v;
  let res = ref !t in
  t := D (i, old, res);
  res

let rec nd_get t i = match !t with
  | A a          -> a.(i)
  | D (j, v, t') -> if i = j then v else nd_get t' i
  | I            -> assert false
