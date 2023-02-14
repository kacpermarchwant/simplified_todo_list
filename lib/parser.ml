open Core
open Angstrom
open Types

let is_whitespace (c : char) : bool =
  match c with ' ' | '\t' -> true | _ -> false

let is_lowercase_or_dash (c : char) : bool =
  Char.is_lowercase c || Char.equal c '-'

let is_lowecase_or_dash_or_whitespace (c : char) : bool =
  is_lowercase_or_dash c || is_whitespace c

let is_digit (c : char) : bool = Char.is_digit c

let parse_add : string t =
  string "add" *> skip_while is_whitespace *> char '\"'
  *> take_while is_lowecase_or_dash_or_whitespace
  <* char '\"' <* skip_while is_whitespace

let parse_done : string t =
  string "done" *> char ' ' *> skip_while is_whitespace *> take_while1 is_digit
  <* skip_while is_whitespace

let parse_search : string list t =
  string "search" *> skip_while is_whitespace *> peek_char >>= function
  | None -> return []
  | Some _ ->
      sep_by (take_while1 is_whitespace) (take_while is_lowercase_or_dash)

let parse_query (expr : string) : query =
  match parse_string ~consume:All parse_add expr with
  | Ok desc -> Add (Description desc)
  | Error _ -> (
      match parse_string ~consume:All parse_done expr with
      | Ok idx -> Done (Index (int_of_string idx))
      | Error _ -> (
          match parse_string ~consume:All parse_search expr with
          | Ok searched_phrases ->
              let searched_phrases =
                List.map searched_phrases ~f:(fun phrase ->
                    Searched_phrase phrase)
              in
              Search searched_phrases
          | Error _ -> Invalid))
