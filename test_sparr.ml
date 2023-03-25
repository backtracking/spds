
open Sparr

let () =
  let v0 = make 7 1 in
  let v1 = set v0 2 5 in
  let v2 = set v1 4 3 in
  assert (get v2 4 = 3);
  let v3 = set v1 5 8 in
  assert (get v3 4 = 1);
  let v4 = set v3 2 3 in
  assert (get v4 2 = 3);
  assert (get v0 2 = 1);
  let v5 = set v0 6 2 in
  let v6 = set v5 3 8 in
  ()

let test n =
  let a0 = make n 0 in
  let rec loop a i = if i < n then (
    assert (get a i = 0);
    let a = set a i (i+1) in
    for j = 0 to i do assert (get a j = j+1) done;
    for j = i+1 to n-1 do assert (get a j = 0) done;
    loop a (i+1)
  ) in
  loop a0 0;
  loop a0 0

let () =
  for n = 0 to 10 do test n done

