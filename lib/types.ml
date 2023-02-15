open Core

type index = Index of int [@@deriving sexp]
type description = Description of string [@@deriving sexp]
type word = Word of string [@@deriving sexp]
type tag = Tag of string [@@deriving sexp]

type add_params = { description : description; tags : tag list }
[@@deriving sexp]

type search_params = { words : word list; tags : tag list } [@@deriving sexp]

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
