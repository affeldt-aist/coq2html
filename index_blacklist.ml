module Glob = Dune_glob.V1
open Common

type t = {
    filepath: string;
    items: Glob.t list;
}

let items_of_file filepath =
  read_lines filepath
  |> List.map Glob.of_string

let from_file filepath = {
    filepath = filepath;
    items = items_of_file filepath;
}

let is_listed blacklist name =
  List.exists (fun glob ->
      Glob.test glob name) blacklist.items
