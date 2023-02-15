open Types

type t

val create : t
val add : t -> description -> tag list -> todo_item
val done_with_item : t -> index -> unit
val search : t -> word list -> tag list -> todo_item list
