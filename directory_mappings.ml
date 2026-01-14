(**
   mappings of phisical path -> logical path
*)
open Common

type t = (string list * string list) list

let empty = []

let add mappings physical_dir path =
  let physical_dir =
    if physical_dir = "." then []
    else String.split_on_char '/' physical_dir
  in
  let logical_path = String.split_on_char '.' path in
  (physical_dir, logical_path) :: mappings

let find (mappings: t) physical_path =
  let is_prefix prefix =
    list_take (List.length prefix) physical_path = prefix
  in
  List.filter_map (fun (dir, path) ->
      if is_prefix dir then Some (dir, path) else None)
    mappings
  |> list_max_by (fun (dir, _) -> List.length dir)

let apply (mappings: t) physical_path =
  match find mappings physical_path with
  | Some (physical_dir, path) ->
     path @ list_drop (List.length physical_dir) physical_path
  | None -> physical_path


let to_mapping_options mappings =
  let smapping (phy, log) =
    !%"-Q %s %s" (String.concat "/" phy) (String.concat "." log)
  in
  String.concat " " @@ List.map smapping mappings

