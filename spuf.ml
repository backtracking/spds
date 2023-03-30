
module A = Sparray

type t = {
  link: int A.t;
  rank: int A.t;
}

let create n =
  if n < 0 then invalid_arg "create";
  { link = A.init n (fun i -> i);
    rank = A.make n 0; }

let rec find t i =
  let p = A.get t.link i in
  if p = i then
    t, i
  else
    let t, r = find t p in
    { t with link = A.set t.link i r }, r

let union t i j =
  let t, ri = find t i in
  let t, rj = find t j in
  if ri <> rj then
    if A.get t.rank ri < A.get t.rank rj then
      { t with link = A.set t.link ri rj }
    else
      let t = { t with link = A.set t.link rj ri } in
      if A.get t.rank ri = A.get t.rank rj then
	{ t with rank = A.set t.rank ri (A.get t.rank ri + 1) }
      else
        t
  else
    t
