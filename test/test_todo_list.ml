open Simplified_todo_list
open Simplified_todo_list.Types
open Core
open Common

let compare_search_results ~result ~expected_result =
  let cmp = Poly.compare in
  let sorted_result = List.sort result ~compare:cmp in
  let sorted_expected_result = List.sort expected_result ~compare:cmp in
  Alcotest.(check testable_sexp)
    "result = expected_result"
    (Sexp.List
       (List.map ~f:(fun item -> sexp_of_todo_item item) sorted_expected_result))
    (Sexp.List (List.map ~f:(fun item -> sexp_of_todo_item item) sorted_result))

let search_words_do_not_query_on_tags () =
  let todo_list = Todo_list.create () in
  let item =
    Todo_list.add todo_list { description = Description "buy bread"; tags = [] }
  in
  let _ =
    Todo_list.add todo_list
      { description = Description "groceries"; tags = [ Tag "bread" ] }
  in
  let search_params = { words = [ Word "bread" ]; tags = [] } in
  compare_search_results
    ~result:(Todo_list.search todo_list search_params)
    ~expected_result:[ item ]

let search_tags_do_not_query_on_words () =
  let todo_list = Todo_list.create () in
  let item =
    Todo_list.add todo_list
      { description = Description "buy bread"; tags = [ Tag "groceries" ] }
  in
  let _ =
    Todo_list.add todo_list
      { description = Description "groceries"; tags = [ Tag "bread" ] }
  in
  let search_params = { words = []; tags = [ Tag "groceries" ] } in
  compare_search_results
    ~result:(Todo_list.search todo_list search_params)
    ~expected_result:[ item ]

let queries_whole_words_properly () =
  let todo_list = Todo_list.create () in
  let item =
    Todo_list.add todo_list { description = Description "buy bread"; tags = [] }
  in
  let _ =
    Todo_list.add todo_list { description = Description "bu bread"; tags = [] }
  in
  let search_params = { words = [ Word "buy" ]; tags = [] } in
  compare_search_results
    ~result:(Todo_list.search todo_list search_params)
    ~expected_result:[ item ]

let queries_whole_tags_properly () =
  let todo_list = Todo_list.create () in
  let item1 =
    Todo_list.add todo_list
      { description = Description "buy bread"; tags = [ Tag "groceries" ] }
  in
  let item2 =
    Todo_list.add todo_list
      { description = Description "buy milk"; tags = [ Tag "groceries" ] }
  in
  let _ =
    Todo_list.add todo_list
      { description = Description "call parents"; tags = [ Tag "relatives" ] }
  in
  let search_params = { words = []; tags = [ Tag "groceries" ] } in
  compare_search_results
    ~result:(Todo_list.search todo_list search_params)
    ~expected_result:[ item1; item2 ]

let queries_subsequences_properly () =
  let todo_list = Todo_list.create () in
  let item =
    Todo_list.add todo_list
      { description = Description "buy milk"; tags = [ Tag "bread" ] }
  in
  let search_params =
    { words = [ Word "il"; Word "b" ]; tags = [ Tag "ea" ] }
  in
  compare_search_results
    ~result:(Todo_list.search todo_list search_params)
    ~expected_result:[ item ]

let queries_only_items_that_match_all_search_words_and_all_search_tags () =
  let todo_list = Todo_list.create () in
  let item =
    Todo_list.add todo_list
      {
        description = Description "buy bread";
        tags = [ Tag "groceries"; Tag "today" ];
      }
  in
  let _ =
    Todo_list.add todo_list
      { description = Description "buy milk"; tags = [ Tag "groceries" ] }
  in
  let _ =
    Todo_list.add todo_list
      {
        description = Description "call parents";
        tags = [ Tag "relatives"; Tag "today" ];
      }
  in
  let search_params = { words = [ Word "buy" ]; tags = [ Tag "day" ] } in
  compare_search_results
    ~result:(Todo_list.search todo_list search_params)
    ~expected_result:[ item ]

let given_empty_search_params_returns_all_items_that_are_not_done () =
  let todo_list = Todo_list.create () in
  let item1 =
    Todo_list.add todo_list { description = Description "buy bread"; tags = [] }
  in
  let item2 =
    Todo_list.add todo_list { description = Description "buy bread"; tags = [] }
  in
  let item3 =
    Todo_list.add todo_list { description = Description "buy bread"; tags = [] }
  in
  let search_params = { words = [ Word "bread" ]; tags = [] } in
  Todo_list.done_with_item todo_list item2.index;
  compare_search_results
    ~result:(Todo_list.search todo_list search_params)
    ~expected_result:[ item1; item3 ]

let () =
  let open Alcotest in
  run "Todo_list"
    [
      ( "Todo_list",
        [
          test_case "Search words do not query on tags" `Quick
            search_words_do_not_query_on_tags;
          test_case "Search tags do not query on words" `Quick
            search_tags_do_not_query_on_words;
          test_case "Queries whole words properly" `Quick
            queries_whole_words_properly;
          test_case "Queries whole tags properly" `Quick
            queries_whole_tags_properly;
          test_case "Queries subsequences properly" `Quick
            queries_subsequences_properly;
          test_case
            "Queries only items that match all search words and all search tags"
            `Quick
            queries_only_items_that_match_all_search_words_and_all_search_tags;
          test_case
            "Given empty search params, returns all items that are not done"
            `Quick given_empty_search_params_returns_all_items_that_are_not_done;
        ] );
    ]
