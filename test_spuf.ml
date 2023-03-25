
open Spuf

let same uf i j =
  let _, ri = find uf i in
  let _, rj = find uf j in
  assert (ri = rj)

let test n =
  let uf0 = create n in
  let limit = n in
  let rec loop uf i = if i < limit then (
    let j = (i+5) mod n in
    let uf = union uf i j in
    same uf i j;
    loop uf (i+1)
  ) else
     for i = 0 to n-1 do same uf i ((i+10) mod n) done
  in
  loop uf0 0;
  loop uf0 0

let () =
  let uf = create 10 in
  for i = 0 to 9 do
    let _, ri = find uf i in assert (i = ri)
  done;
  for n = 1 to 34 do test n done
