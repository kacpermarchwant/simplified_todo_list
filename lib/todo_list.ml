open Types

type t = { store : Store.t }

let add todo_list description : todo_item =
  let store = todo_list.store in
  Store.add store description

let done_with_item todo_list index : unit =
  let store = todo_list.store in
  Store.done_with_item store index

let search todo_list searched_phrases : todo_item array =
  let store = todo_list.store in
  Store.search store searched_phrases

let create : t =
  let store = Store.create in
  { store }
