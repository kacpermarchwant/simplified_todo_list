open Core
open Types

type t = { mutable items : todo_item array; mutable next_index : index }

let create = { items = [||]; next_index = Index 0 }

let add store description : todo_item =
  let new_item = { index = store.next_index; description; is_done = false } in
  let (Index i) = new_item.index in
  store.next_index <- Index (i + 1);
  store.items <- Array.append store.items [| new_item |];
  new_item

let done_with_item store index : unit =
  let (Index i) = index in
  if i < Array.length store.items then
    let item = store.items.(i) in
    item.is_done <- true

let search store searched_phrases : todo_item array =
  let items = store.items in
  let result =
    Array.filter items ~f:(fun item ->
        let (Description desc) = item.description in
        (not item.is_done)
        && List.for_all searched_phrases ~f:(fun (Searched_phrase phrase) ->
               String.is_substring ~substring:phrase desc))
  in
  result
