open Angstrom
open Types

let is_whitespace = function ' ' | '\t' -> true | _ -> false
let is_lowercase_or_dash c = Core.Char.is_lowercase c || c = '-'

let is_lowecase_or_dash_or_whitespace (c : char) =
  is_lowercase_or_dash c || is_whitespace c

let is_digit c = Core.Char.is_digit c

let parse_add =
  string "add" *> skip_while is_whitespace *> char '\"'
  *> take_while is_lowecase_or_dash_or_whitespace
  <* char '\"' <* skip_while is_whitespace

let parse_done =
  string "done" *> char ' ' *> skip_while is_whitespace *> take_while1 is_digit
  <* skip_while is_whitespace

let parse_search =
  string "search" *> skip_while is_whitespace *> peek_char >>= function
  | None -> return []
  | Some _ ->
      sep_by (take_while1 is_whitespace) (take_while is_lowercase_or_dash)

let parse_expr (line : string) : query =
  match parse_string ~consume:All parse_add line with
  | Ok description -> Add { description }
  | Error _ -> (
      match parse_string ~consume:All parse_done line with
      | Ok idx -> Done { item_index = int_of_string idx }
      | Error _ -> (
          match parse_string ~consume:Prefix parse_search line with
          | Ok words -> Search { words }
          | Error msg -> Unsupported { msg }))
