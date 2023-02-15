open Core
open Types

let execute_query todo_list query : unit =
  match query with
  | Add add_params ->
      let item = Todo_list.add todo_list add_params in
      Out_channel.print_endline (Todo_item.to_output_format item)
  | Done item_index ->
      Todo_list.done_with_item todo_list item_index;
      Out_channel.print_endline "Done"
  | Search search_params ->
      let results = Todo_list.search todo_list search_params in
      Out_channel.print_endline
        (string_of_int (List.length results) ^ " item(s) found");
      List.iter results ~f:(fun item ->
          Out_channel.print_endline (Todo_item.to_output_format item))
  | Invalid -> print_endline "Invalid input..."

let rec run todo_list n : unit =
  if n > 0 then (
    let query = In_channel.(input_line_exn stdin) in
    Parser.parse_query query |> execute_query todo_list;
    run todo_list (n - 1))

let run_n_queries n : unit =
  let todo_list = Todo_list.create () in
  run todo_list n
