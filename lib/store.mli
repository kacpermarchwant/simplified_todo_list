open Types

type t

val create : t
val add : t -> description -> todo_item
val done_with_item : t -> index -> unit
val search : t -> searched_phrase list -> todo_item array