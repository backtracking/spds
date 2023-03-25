
module A = Sparr
module L = Splist

type t = {
  size: int;
  adj : L.t A.t;
}

let create n =
  { size = n;
    adj  = A.init n (fun _ -> L.nil ()); }

let size g =
  g.size

let add_edge g i j =
  if i < 0 || i >= g.size then invalid_arg "add_edge";
  if j < 0 || j >= g.size then invalid_arg "add_edge";
  { g with adj = A.set g.adj i (L.cons j (A.get g.adj i)) }

let has_edge g i j =
  L.mem j (A.get g.adj i)

(* FIXME: stop as soon as `j` is reached *)
let exists_path g i j =
  let visited = Array.make g.size false in
  let rec dfs i =
    if not visited.(i) then (
      visited.(i) <- true;
      L.iter dfs (A.get g.adj i)
    ) in
  dfs i;
  visited.(j)

