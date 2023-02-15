open Types

type t

val create : unit -> t
val add : t -> description -> tag list -> todo_item
val delete : t -> index -> unit
val search : t -> search_word list -> search_tag list -> todo_item list
