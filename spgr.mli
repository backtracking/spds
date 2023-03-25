
(* Semi-persistent directed graphs

   Vertices are integers 0,...,N-1
*)

type t

val size: t -> int

val create: int -> t

val add_edge: t -> int -> int -> t

val exists_path: t -> int -> int -> bool
