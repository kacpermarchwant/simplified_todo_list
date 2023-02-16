open Core
open Types

type t = {
  mutable items : todo_item array;
  mutable next_index : index;
  words_to_ids : Inverted_index.t;
  tags_to_ids : Inverted_index.t;
}

let to_subsequences strings : string list =
  let subsequences = ref [] in

  List.iter strings ~f:(fun s ->
      let length = String.length s in
      for i = 0 to length - 1 do
        for j = i + 1 to length do
          let subsequence = String.sub s ~pos:i ~len:(j - i) in
          subsequences := subsequence :: !subsequences
        done
      done);

  !subsequences

let populate_inverted_index inverted_index key subsequences =
  List.iter subsequences ~f:(fun subsequence ->
      Inverted_index.add inverted_index subsequence key)

let populate_inverted_indexes store item : unit =
  let (Index idx) = item.index in

  item.description |> Description.to_words |> to_subsequences
  |> populate_inverted_index store.words_to_ids idx;

  item.tags
  |> List.map ~f:(fun (Tag tag_value) -> tag_value)
  |> to_subsequences
  |> populate_inverted_index store.tags_to_ids idx

let sort_sets_by_length sets =
  List.sort sets ~compare:(fun set1 set2 ->
      Hash_set.length set1 - Hash_set.length set2)

let intersect_sets sets : 'a list =
  match sort_sets_by_length sets with
  | [] -> []
  | [ set ] -> Hash_set.to_list set
  | smallest_set :: other_sets ->
      smallest_set
      |> Hash_set.filter ~f:(fun el ->
             List.for_all other_sets ~f:(fun set -> Hash_set.mem set el))
      |> Hash_set.to_list

let get_indexes_matching_words store search_words : int Hash_set.t list =
  List.fold_left ~init:[]
    ~f:(fun acc (SearchWord word) ->
      Inverted_index.get store.words_to_ids word :: acc)
    search_words

let get_indexes_matching_tags store search_tags : int Hash_set.t list =
  List.fold_left ~init:[]
    ~f:(fun acc (SearchTag tag) ->
      Inverted_index.get store.tags_to_ids tag :: acc)
    search_tags

let create () =
  {
    items = [||];
    next_index = Index 0;
    words_to_ids = Inverted_index.create ();
    tags_to_ids = Inverted_index.create ();
  }

let add store description tags : todo_item =
  let new_item =
    { index = store.next_index; description; tags; is_done = false }
  in
  store.next_index <- Index.next new_item.index;
  store.items <- Array.append store.items [| new_item |];
  populate_inverted_indexes store new_item;

  new_item

let delete store (Index index) : unit =
  if index < Array.length store.items then
    let item = store.items.(index) in
    item.is_done <- true

let search store search_words search_tags : todo_item list =
  if List.is_empty search_words && List.is_empty search_tags then
    store.items
    |> Array.filter ~f:(fun item -> not item.is_done)
    |> Array.to_list
  else
    let indexes_matching_words =
      get_indexes_matching_words store search_words
    in
    let indexes_matching_tags = get_indexes_matching_tags store search_tags in
    let matching_indexes =
      intersect_sets (indexes_matching_tags @ indexes_matching_words)
    in
    List.filter_map matching_indexes ~f:(fun index ->
        let item = Array.get store.items index in
        if item.is_done then None else Some item)
