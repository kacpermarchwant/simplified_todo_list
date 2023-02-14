open Core
module Int_set = Hash_set.Make (Int) [@@deriving hash compare sexp]

type t = (string, Int_set.t) Hashtbl.t

let create = Hashtbl.create (module String)

let add (inverted_index : t) (phrase : string) (idx : int) : unit =
  Hashtbl.update inverted_index phrase ~f:(function
    | Some set ->
        Hash_set.add set idx;
        set
    | None ->
        let set = Hash_set.create (module Int) in
        ignore (Hashtbl.add inverted_index ~key:phrase ~data:set);
        Hash_set.add set idx;
        set)

let get inverted_index phrase : Int_set.t =
  match Hashtbl.find inverted_index phrase with
  | Some set -> set
  | None -> Hash_set.create (module Int)
