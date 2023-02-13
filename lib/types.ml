open Core

type index = Index of int [@@deriving sexp]
type description = Description of string [@@deriving sexp]
type searched_phrase = Searched_phrase of string [@@deriving sexp]

type query =
  | Add of { description : description }
  | Done of { item_index : index }
  | Search of { searched_phrases : searched_phrase list }
  | Invalid
[@@deriving sexp]

type todo_item = {
  index : index;
  description : description;
  mutable is_done : bool;
}
[@@deriving sexp]
