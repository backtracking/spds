
open Format
open Unix

let time f x =
  let u = (times()).tms_utime in
  let y = f x in
  let ut = (times()).tms_utime -. u in
  y, ut

let print_time f x =
  let _, ut = time f x in
  printf "%2.2f@." ut

module Bench(X: sig
  type t
  val create: unit -> t
  val update: t -> int -> t
end) = struct

  let live = ref 0
  let bytes = ref 0.

  let run n =
    Gc.full_major ();
    bytes := Gc.allocated_bytes ();
    Random.init 42;
    let rec descend h k t =
      if k < n then
        let t = X.update t k in
        descend h (k+1) t
      else if h < 6 then (
        descend (h+1) 0 t;
        descend (h+1) 0 t;
        descend (h+1) 0 t;
        descend (h+1) 0 t;
      ) in
    descend 0 0 (X.create ())

  let bench name nl =
    printf "%s@." name;
    let run n =
      printf "  %4d: " n; print_time run n;
      let m = (Gc.allocated_bytes () -. !bytes) /. 1024. in
      printf "    %.2f kb / %.2f Mb@." m (m /. 1024.)
    in
    List.iter run nl


end

let values = [1_000; 5_000; 10_000]

module PA = Bench(struct
  type t = int Parray.t
  let create () = Parray.make 256 0
  let update t x = Parray.set t (x land 0xff) x
end)
module SPA = Bench(struct
  type t = int Sparray.t
  let create () = Sparray.make 256 0
  let update t x = Sparray.set t (x land 0xff) x
end)
(* let () = PA.bench "persistent arrays" values
 * let () = SPA.bench "semi-persistent arrays" values *)

module PL = Bench(struct
  type t = int list
  let create () = []
  let update t x = x :: t
end)
module SPL = Bench(struct
  type t = Splist.t
  let create () = Splist.nil ()
  let update t x = Splist.cons x t
end)
(* let () = PL.bench "persistent lists" values
 * let () = SPL.bench "semi-persistent lists" values *)

module PG = Bench(struct
  type t = int list Parray.t
  let create () = Parray.make 256 []
  let update t x =
    let i = x land 0xff and j = (x+3) land 0xff in
    Parray.set t i (j :: Parray.get t i)
end)
module SPG = Bench(struct
  type t = Spgraph.t
  let create () = Spgraph.create 256
  let update t x =
    let i = x land 0xff and j = (x+3) land 0xff in
    Spgraph.add_edge t i j
end)
let () = PG.bench "persistent graphs" values
let () = SPG.bench "semi-persistent graphs" values
