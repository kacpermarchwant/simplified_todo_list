open Core

type todo_item = { index : int; description : string; mutable is_done : bool }
[@@deriving sexp]

module Store = struct
  type t = { mutable items : todo_item array; mutable next_index : int }

  let create = { items = [||]; next_index = 0 }

  let add store description =
    let new_item = { index = store.next_index; description; is_done = false } in
    store.next_index <- store.next_index + 1;
    store.items <- Array.append store.items [| new_item |];
    new_item

  let remove store index =
    if index < Array.length store.items then
      let item = store.items.(index) in
      item.is_done <- true

  let search store searched_words =
    let items = store.items in
    let result =
      Array.filter items ~f:(fun item ->
          (not item.is_done)
          && List.for_all searched_words ~f:(fun word ->
                 String.is_substring ~substring:word item.description))
    in
    result
end