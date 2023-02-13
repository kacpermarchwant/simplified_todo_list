open Core

type Index = int

type query =
  | Add of { description : string }
  | Done of { item_index : int }
  | Search of { words : string list }
  | Unsupported of { msg : string }
[@@deriving sexp]
