open Simplified_todo_list.Parser
open Simplified_todo_list.Types
open Simplified_todo_list.Todo_list
open Core

let execute store input_line : unit =
  match parse_expr input_line with
  | Add { description } ->
      let item = Store.add store description in
      Out_channel.print_endline (Sexp.to_string (sexp_of_todo_item item))
  | Done { item_index } ->
      Store.remove store item_index;
      Out_channel.print_endline "Done"
  | Search { words } -> (
      match Store.search store words with
      | [||] -> Out_channel.print_endline "No results"
      | results ->
          Array.iter results ~f:(fun item ->
              Out_channel.print_endline
                (Sexp.to_string (sexp_of_todo_item item))))
  | Unsupported _ -> print_endline "Unsupported!"

let rec run store : unit =
  let input_line = In_channel.(input_line_exn stdin) in
  execute store input_line;
  run store

let () =
  let store = Store.create in
  run store
