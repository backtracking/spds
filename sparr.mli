
(* Semi-persistent arrays *)

type 'a t

val make: int -> 'a -> 'a t
val init: int -> (int -> 'a) -> 'a t

val get: 'a t -> int -> 'a

val nd_get: 'a t -> int -> 'a

val set: 'a t -> int -> 'a -> 'a t
