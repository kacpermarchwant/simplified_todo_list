open Core
open Angstrom
open Types

type search_expr_element = WordEl of word | TagEl of tag

let separate_words_from_tags search_query : word list * tag list =
  List.fold_left search_query ~init:([], []) ~f:(fun (words, tags) el ->
      match el with
      | WordEl word -> (word :: words, tags)
      | TagEl tag -> (words, tag :: tags))

let is_whitespace (c : char) : bool =
  match c with ' ' | '\t' -> true | _ -> false

let is_lowercase_or_dash (c : char) : bool =
  Char.is_lowercase c || Char.equal c '-'

let is_lowercase_or_dash_or_whitespace (c : char) : bool =
  is_lowercase_or_dash c || is_whitespace c

let is_digit (c : char) : bool = Char.is_digit c
let take_word = take_while1 is_lowercase_or_dash
let take_tag = char '#' *> take_word

let take_tags =
  many
    (satisfy is_whitespace *> skip_while is_whitespace
    *> (take_tag >>| fun tag -> Tag tag))

let take_description =
  skip_while is_whitespace *> char '\"'
  *> take_while is_lowercase_or_dash_or_whitespace
  <* char '\"'
  >>| fun desc -> Description desc

let parse_add : (description * tag list) t =
  string "add" *> both take_description take_tags <* skip_while is_whitespace

let parse_done : string t =
  string "done" *> char ' ' *> skip_while is_whitespace *> take_while1 is_digit
  <* skip_while is_whitespace

let parse_search : search_expr_element list t =
  string "search"
  *> many
       (satisfy is_whitespace *> skip_while is_whitespace
       *> (take_tag
          >>| (fun tag -> TagEl (Tag tag))
          <|> (take_word >>| fun word -> WordEl (Word word))))
  <* skip_while is_whitespace

let parse_query (expr : string) : query =
  match parse_string ~consume:All parse_add expr with
  | Ok (description, tags) -> Add { description; tags }
  | Error _ -> (
      match parse_string ~consume:All parse_done expr with
      | Ok idx -> Done (Index (int_of_string idx))
      | Error _ -> (
          match parse_string ~consume:All parse_search expr with
          | Ok search_query ->
              let words, tags = separate_words_from_tags search_query in
              Search { words; tags }
          | Error _ -> Invalid))
