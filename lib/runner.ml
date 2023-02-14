open Core
open Types

let execute_query todo_list query : unit =
  match query with
  | Add description ->
      let item = Todo_list.add todo_list description in
      Out_channel.print_endline (Sexp.to_string (sexp_of_todo_item item))
  | Done item_index ->
      Todo_list.done_with_item todo_list item_index;
      Out_channel.print_endline "Done"
  | Search searched_phrases ->
      let results = Todo_list.search todo_list searched_phrases in
      Out_channel.print_endline
        (string_of_int (List.length results) ^ " item(s) found");
      List.iter results ~f:(fun item ->
          Out_channel.print_endline (Sexp.to_string (sexp_of_todo_item item)))
  | Invalid -> print_endline "Invalid query!"

let rec run todo_list : unit =
  In_channel.(input_line_exn stdin)
  |> Parser.parse_query |> execute_query todo_list;

  run todo_list

let start = Todo_list.create |> run
