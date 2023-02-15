open Types

(* Can I turn it into a functor?

   Store can be a parameter - different implementation - Bruteforce and InvertedIndex

   Functors are functors from modules to modules

   Functors let you create parametrized modules and then provide other modules as parameters to get specific implementation *)

type t = { store : Store.t }

let add (todo_list : t) (add_params : add_params) : todo_item =
  let store = todo_list.store in
  Store.add store add_params.description add_params.tags

let done_with_item todo_list index : unit =
  let store = todo_list.store in
  Store.done_with_item store index

let search todo_list search_params : todo_item list =
  let store = todo_list.store in
  Store.search store search_params.words search_params.tags

let create () : t =
  let store = Store.create () in
  { store }
