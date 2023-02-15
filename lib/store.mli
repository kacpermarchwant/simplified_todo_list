open Types

type t

val create : unit -> t
val add : t -> description -> tag list -> todo_item
val done_with_item : t -> index -> unit
val search : t -> search_word list -> search_tag list -> todo_item list
