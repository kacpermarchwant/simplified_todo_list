open Core
open Types

type t = {
  mutable items : todo_item array;
  mutable next_index : index;
  words_to_ids : Inverted_index.t;
  tags_to_ids : Inverted_index.t;
}

let create () =
  {
    items = [||];
    next_index = Index 0;
    words_to_ids = Inverted_index.create ();
    tags_to_ids = Inverted_index.create ();
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

let description_to_words (Description desc) =
  String.split desc ~on:' '
  |> List.filter ~f:(fun word -> not (String.is_empty word))

let populat_indexes store item : unit =
  let (Index i) = item.index in

  description_to_words item.description
  |> List.iter ~f:(fun word ->
         all_substrings word
         |> List.iter ~f:(fun substring ->
                Inverted_index.add store.words_to_ids substring i));

  List.iter item.tags ~f:(fun (Tag tag) ->
      all_substrings tag
      |> List.iter ~f:(fun substring ->
             Inverted_index.add store.tags_to_ids substring i))

let add store description tags : todo_item =
  let (Index new_item_index) = store.next_index in
  let new_item =
    { index = Index new_item_index; description; tags; is_done = false }
  in

  store.next_index <- Index (new_item_index + 1);
  store.items <- Array.append store.items [| new_item |];
  populat_indexes store new_item;

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

let search store search_words search_tags : todo_item list =
  let items = store.items in
  if List.is_empty search_words && List.is_empty search_tags then
    Array.filter items ~f:(fun item -> not item.is_done) |> Array.to_list
  else
    let item_ids_matching_words =
      List.fold_left ~init:[]
        ~f:(fun acc (SearchWord word) ->
          Inverted_index.get store.words_to_ids word :: acc)
        search_words
    in
    let item_ids_matching_tags =
      List.fold_left ~init:[]
        ~f:(fun acc (SearchTag tag) ->
          Inverted_index.get store.tags_to_ids tag :: acc)
        search_tags
    in
    let results = item_ids_matching_tags @ item_ids_matching_words in
    let indexes = intersect_sets results in

    List.filter_map indexes ~f:(fun index ->
        let item = Array.get items index in
        if item.is_done then None else Some item)
