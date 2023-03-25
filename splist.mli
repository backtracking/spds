
(* Semi-persistent lists

   Elements are integers, to make the API a little bit simpler.
   This is enough to build graph with integer vertives (see `Spgr`).
*)

type t

val nil: unit -> t

val is_nil: t -> bool

val cons: int -> t -> t

val head: t -> int

val tail: t -> t

val length: t -> int

val mem: int -> t -> bool

val iter: (int -> unit) -> t -> unit

