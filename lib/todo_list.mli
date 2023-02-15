open Types

type t

val create : t
val add : t -> add_params -> todo_item
val done_with_item : t -> index -> unit
val search : t -> search_params -> todo_item list