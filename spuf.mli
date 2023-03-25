
(* Semi-persistent Union-Find *)

type t

val create: int -> t

val find: t -> int -> t * int

val union: t -> int -> int -> t

