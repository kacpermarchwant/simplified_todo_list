open Core
open Types

type t = {
  mutable items : todo_item array;
  mutable next_index : index;
  substrings_to_ids : Inverted_index.t;
}

let create =
  {
    items = [||];
    next_index = Index 0;
    substrings_to_ids = Inverted_index.create;
  }

let all_substrings (s : string) : string list =
  let length = String.length s in
  let substrings = ref [] in
  for i = 0 to length - 1 do
    for j = i + 1 to length do
      let substring = String.sub s ~pos:i ~len:(j - i) in
      substrings := substring :: !substrings
    done
  done;
  !substrings

let add store description : todo_item =
  let substrings_to_ids = store.substrings_to_ids in
  let new_item = { index = store.next_index; description; is_done = false } in
  let (Index i) = new_item.index in
  let (Description desc) = description in

  String.split desc ~on:' '
  |> List.filter ~f:(fun word -> not (String.is_empty word))
  |> List.iter ~f:(fun word ->
         all_substrings word
         |> List.iter ~f:(fun substring ->
                Inverted_index.add substrings_to_ids substring i));

  store.next_index <- Index (i + 1);
  store.items <- Array.append store.items [| new_item |];

  new_item

let done_with_item store (Index index) : unit =
  if index < Array.length store.items then
    let item = store.items.(index) in
    item.is_done <- true

let intersect_sets sets : 'a list =
  let sorted_sets =
    List.sort sets ~compare:(fun set1 set2 ->
        Hash_set.length set1 - Hash_set.length set2)
  in
  match sorted_sets with
  | [] -> []
  | [ set ] -> Hash_set.to_list set
  | smallest_set :: tail ->
      Hash_set.filter smallest_set ~f:(fun el ->
          List.for_all tail ~f:(fun set -> Hash_set.mem set el))
      |> Hash_set.to_list

let search store searched_phrases : todo_item list =
  let items = store.items in
  if List.is_empty searched_phrases then
    Array.filter items ~f:(fun item -> not item.is_done) |> Array.to_list
  else
    let substrings_to_ids = store.substrings_to_ids in
    let results =
      List.fold_left ~init:[]
        ~f:(fun acc (Searched_phrase phrase) ->
          Inverted_index.get substrings_to_ids phrase :: acc)
        searched_phrases
    in
    let indexes = intersect_sets results in
    List.filter_map indexes ~f:(fun index ->
        let item = Array.get items index in
        if item.is_done then None else Some item)
