open Core

type t

val create : t
val add : t -> string -> int -> unit
val get : t -> string -> int Hash_set.t