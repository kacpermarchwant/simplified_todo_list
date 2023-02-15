open Simplified_todo_list
open Simplified_todo_list.Types
open Core

let testable_sexp = Alcotest.testable Sexp.pp Sexp.equal

let compare_results ~result ~expected_result =
  Alcotest.(check testable_sexp)
    "result = expected result"
    (sexp_of_query expected_result)
    (sexp_of_query result)

let test_parser_does_not_accept_expr_containing_only_whitespace () =
  let expr = " " in
  compare_results ~result:(Parser.parse_query expr) ~expected_result:Invalid

let test_parser_does_not_accept_expr_starting_with_whitespace () =
  let expr = " search buy milk" in
  compare_results ~result:(Parser.parse_query expr) ~expected_result:Invalid

let test_parser_add_accepts_string_as_description () =
  let description = "buy milk" in
  let expr = "add \"" ^ description ^ "\"" in
  compare_results ~result:(Parser.parse_query expr)
    ~expected_result:(Add (Description description))

let test_parser_add_requires_description_to_be_enclosed_in_double_quotes () =
  let expr = "add \"buy milk" in
  compare_results ~result:(Parser.parse_query expr) ~expected_result:Invalid

let test_parser_add_requires_description_to_only_contain_lowercase_letters_dashes_and_whitespaces
    () =
  let description = "buy milk -" in
  let valid_expr = "add \"" ^ description ^ "\"" in
  let expr_with_capitalized_letter = "add \"" ^ "B" ^ "\"" in
  let expr_with_special_character = "add \"" ^ "!" ^ "\"" in

  compare_results
    ~result:(Parser.parse_query valid_expr)
    ~expected_result:(Add (Description description));
  compare_results
    ~result:(Parser.parse_query expr_with_capitalized_letter)
    ~expected_result:Invalid;
  compare_results
    ~result:(Parser.parse_query expr_with_special_character)
    ~expected_result:Invalid

let test_parser_add_ignores_extra_whitespaces_outside_of_the_description () =
  let description = "buy milk" in
  let expr = "add     \"" ^ description ^ "\"      " in
  compare_results ~result:(Parser.parse_query expr)
    ~expected_result:(Add (Description description))

let test_parser_done_accepts_digit () =
  let expr = "done 10" in
  compare_results ~result:(Parser.parse_query expr)
    ~expected_result:(Done (Index 10))

let test_parser_done_ignores_extra_whitespaces () =
  let expr = "done    10     " in
  compare_results ~result:(Parser.parse_query expr)
    ~expected_result:(Done (Index 10))

let test_parser_done_accepts_only_digits () =
  let expr_with_letter = "done a" in
  let expr_with_letter_and_digit = "done 1a" in
  let expr_with_special_character = "done !" in

  compare_results
    ~result:(Parser.parse_query expr_with_letter)
    ~expected_result:Invalid;
  compare_results
    ~result:(Parser.parse_query expr_with_letter_and_digit)
    ~expected_result:Invalid;
  compare_results
    ~result:(Parser.parse_query expr_with_special_character)
    ~expected_result:Invalid

let test_parser_done_does_not_accept_multiple_digits () =
  let expr = "done 1 4" in
  compare_results ~result:(Parser.parse_query expr) ~expected_result:Invalid

let test_parser_search_parses_phrases_correctly () =
  let expr = "search buy milk" in
  compare_results ~result:(Parser.parse_query expr)
    ~expected_result:
      (Search { words = [ Subsequence "buy"; Subsequence "milk" ]; tags = [] })

let test_parser_search_parses_ignores_extra_whitespaces () =
  let expr = "search   buy   milk   " in
  compare_results ~result:(Parser.parse_query expr)
    ~expected_result:
      (Search { words = [ Subsequence "buy"; Subsequence "milk" ]; tags = [] })

let run =
  let open Alcotest in
  run "Parser"
    [
      ( "Parser",
        [
          test_case "Doesn't accept expr containing only whitespace" `Quick
            test_parser_does_not_accept_expr_containing_only_whitespace;
          test_case "Doesn't accept expr starting with whitespace" `Quick
            test_parser_does_not_accept_expr_starting_with_whitespace;
        ] );
      ( "Parser: add",
        [
          test_case "Accepts string as description" `Quick
            test_parser_add_accepts_string_as_description;
          test_case "Ignores extra whitespaces outside of description" `Quick
            test_parser_add_ignores_extra_whitespaces_outside_of_the_description;
          test_case "Requires description to be enclosed in double quotes"
            `Quick
            test_parser_add_requires_description_to_be_enclosed_in_double_quotes;
          test_case
            "Requires description to only contain lowercase letters, dashes, \
             and whitespaces"
            `Quick
            test_parser_add_requires_description_to_only_contain_lowercase_letters_dashes_and_whitespaces;
        ] );
      ( "Parser: done",
        [
          test_case "Accepts digit" `Quick test_parser_done_accepts_digit;
          test_case "Ignores extra whitespaces" `Quick
            test_parser_done_ignores_extra_whitespaces;
          test_case "Doesn't accept letter" `Quick
            test_parser_done_accepts_only_digits;
          test_case "Accepts only one digit" `Quick
            test_parser_done_does_not_accept_multiple_digits;
        ] );
      ( "Parser: search",
        [
          test_case "Parses phrases correctly" `Quick
            test_parser_search_parses_phrases_correctly;
          test_case "Ignores extra whitespaces" `Quick
            test_parser_search_parses_ignores_extra_whitespaces;
        ] );
    ]