open Core

type index = Index of int [@@deriving sexp]
type description = Description of string [@@deriving sexp]
type word = Word of string [@@deriving sexp]
type tag = Tag of string [@@deriving sexp]
type search_word = SearchWord of string [@@deriving sexp]
type search_tag = SearchTag of string [@@deriving sexp]

type add_params = { description : description; tags : tag list }
[@@deriving sexp]

type search_params = { words : search_word list; tags : search_tag list }
[@@deriving sexp]

type query =
  | Add of add_params
  | Done of index
  | Search of search_params
  | Invalid
[@@deriving sexp]

type todo_item = {
  index : index;
  description : description;
  tags : tag list;
  mutable is_done : bool;
}
[@@deriving sexp]

module Index = struct
  type t = index

  let next (Index i) = Index (i + 1)
end

module Description = struct
  type t = description

  let to_words (Description desc) =
    String.split desc ~on:' '
    |> List.filter ~f:(fun word -> not (String.is_empty word))
end

module Todo_item = struct
  type t = todo_item

  let to_output_format { index = Index idx; _ } : string = Int.to_string idx
end
