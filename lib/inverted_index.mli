open Core

type t

val create : unit -> t
val add : t -> string -> int -> unit
val get : t -> string -> int Hash_set.t