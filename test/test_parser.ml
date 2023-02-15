open Simplified_todo_list
open Simplified_todo_list.Types
open Core

let testable_sexp = Alcotest.testable Sexp.pp Sexp.equal

let serialize_result (res : query) : query =
  match res with
  | Add add_params ->
      Add
        {
          add_params with
          tags = List.sort add_params.tags ~compare:Poly.compare;
        }
  | Search search_params ->
      let sorted_words = List.sort search_params.words ~compare:Poly.compare in
      let sorted_tags = List.sort search_params.tags ~compare:Poly.compare in
      Search { words = sorted_words; tags = sorted_tags }
  | q -> q

let compare_queries ~result ~expected_result =
  let result = serialize_result result in
  let expected_result = serialize_result expected_result in
  Alcotest.(check testable_sexp)
    "result = expected result"
    (sexp_of_query expected_result)
    (sexp_of_query result)

let does_not_accept_expr_containing_only_whitespace () =
  let expr = " " in
  compare_queries ~result:(Parser.parse_query expr) ~expected_result:Invalid

let does_not_accept_expr_starting_with_whitespace () =
  let expr = " search buy milk" in
  compare_queries ~result:(Parser.parse_query expr) ~expected_result:Invalid

let add_accepts_string_as_description () =
  let description = "buy milk" in
  let expr = "add \"" ^ description ^ "\"" in
  compare_queries ~result:(Parser.parse_query expr)
    ~expected_result:(Add { description = Description description; tags = [] })

let add_requires_description_to_be_enclosed_in_double_quotes () =
  let expr = "add \"buy milk" in
  compare_queries ~result:(Parser.parse_query expr) ~expected_result:Invalid

let add_requires_description_to_only_contain_lowercase_letters_dashes_and_whitespaces
    () =
  let description = "buy milk -" in
  let valid_expr = "add \"" ^ description ^ "\"" in
  let expr_with_capitalized_letter = "add \"" ^ "B" ^ "\"" in
  let expr_with_special_character = "add \"" ^ "!" ^ "\"" in

  compare_queries
    ~result:(Parser.parse_query valid_expr)
    ~expected_result:(Add { description = Description description; tags = [] });
  compare_queries
    ~result:(Parser.parse_query expr_with_capitalized_letter)
    ~expected_result:Invalid;
  compare_queries
    ~result:(Parser.parse_query expr_with_special_character)
    ~expected_result:Invalid

let add_ignores_extra_whitespaces_outside_of_the_description () =
  let description = "buy milk" in
  let expr = "add     \"" ^ description ^ "\"      " in
  compare_queries ~result:(Parser.parse_query expr)
    ~expected_result:(Add { description = Description description; tags = [] })

let done_accepts_digit () =
  let expr = "done 10" in
  compare_queries ~result:(Parser.parse_query expr)
    ~expected_result:(Done (Index 10))

let done_ignores_extra_whitespaces () =
  let expr = "done    10     " in
  compare_queries ~result:(Parser.parse_query expr)
    ~expected_result:(Done (Index 10))

let done_accepts_only_digits () =
  let expr_with_letter = "done a" in
  let expr_with_letter_and_digit = "done 1a" in
  let expr_with_special_character = "done !" in

  compare_queries
    ~result:(Parser.parse_query expr_with_letter)
    ~expected_result:Invalid;
  compare_queries
    ~result:(Parser.parse_query expr_with_letter_and_digit)
    ~expected_result:Invalid;
  compare_queries
    ~result:(Parser.parse_query expr_with_special_character)
    ~expected_result:Invalid

let done_does_not_accept_multiple_digits () =
  let expr = "done 1 4" in
  compare_queries ~result:(Parser.parse_query expr) ~expected_result:Invalid

let search_parses_phrases_correctly () =
  let expr = "search buy milk" in
  compare_queries ~result:(Parser.parse_query expr)
    ~expected_result:(Search { words = [ Word "buy"; Word "milk" ]; tags = [] })

let search_parses_ignores_extra_whitespaces () =
  let expr = "search   buy   milk   " in
  compare_queries ~result:(Parser.parse_query expr)
    ~expected_result:(Search { words = [ Word "buy"; Word "milk" ]; tags = [] })

let () =
  let open Alcotest in
  run "Parser"
    [
      ( "Parser",
        [
          test_case "Doesn't accept expr containing only whitespace" `Quick
            does_not_accept_expr_containing_only_whitespace;
          test_case "Doesn't accept expr starting with whitespace" `Quick
            does_not_accept_expr_starting_with_whitespace;
        ] );
      ( "Parser: add",
        [
          test_case "Accepts string as description" `Quick
            add_accepts_string_as_description;
          test_case "Ignores extra whitespaces outside of description" `Quick
            add_ignores_extra_whitespaces_outside_of_the_description;
          test_case "Requires description to be enclosed in double quotes"
            `Quick add_requires_description_to_be_enclosed_in_double_quotes;
          test_case
            "Requires description to only contain lowercase letters, dashes, \
             and whitespaces"
            `Quick
            add_requires_description_to_only_contain_lowercase_letters_dashes_and_whitespaces;
        ] );
      ( "Parser: done",
        [
          test_case "Accepts digit" `Quick done_accepts_digit;
          test_case "Ignores extra whitespaces" `Quick
            done_ignores_extra_whitespaces;
          test_case "Doesn't accept letter" `Quick done_accepts_only_digits;
          test_case "Accepts only one digit" `Quick
            done_does_not_accept_multiple_digits;
        ] );
      ( "Parser: search",
        [
          test_case "Parses phrases correctly" `Quick
            search_parses_phrases_correctly;
          test_case "Ignores extra whitespaces" `Quick
            search_parses_ignores_extra_whitespaces;
        ] );
    ]