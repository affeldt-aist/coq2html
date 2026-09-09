open Common
open Glob

let sort entries =
  let compare ent1 ent2 =
    compare (Glob.pos_of_entry ent1) (Glob.pos_of_entry ent2)
  in
  List.sort compare entries

let group_by_definitions entries =
  let take_refs entries =
    let rec iter refs = function
      | Reference ref :: es -> iter (ref::refs) es
      | Definition def :: es -> (Some (def, es), List.rev refs)
      | [] -> (None, List.rev refs)
    in
    iter [] entries
  in
  let take_group def entries =
    let (next, refs) = take_refs entries in
    ((def, refs), next)
  in
  let rec first_def = function
    | Definition def :: es -> Some (def, es)
    | Reference _ :: es -> first_def es (* ignore before the first definition *)
    | [] -> None
  in
  let rec iter store def es =
    match take_group def es with
    | (grp, None) -> List.rev (grp :: store)
    | (grp, Some (nextdef, es')) ->
      iter (grp :: store) nextdef es'
  in
  match first_def entries with
  | None -> []
  | Some (def0, rest) ->
    iter [] def0 rest

let path sp id =
  match sp, id with
  | "<>", "<>" -> ""
  | "<>", _    -> id
  | _   , "<>" -> sp
  | _   , _    -> sp ^ "." ^ id

let path_of_def file_mod (def: def) =
  (file_mod, path def.section_path def.id)
let path_of_ref ref =
  (ref.logical_path, path ref.section_path ref.id)

type path = string * string
type grp = path * path list

let is_def_ref = function
  | Definition def when def.kind = Glob_kind.Binder -> false
  | _ -> true

let from_glob glob : grp list =
  sort glob.entries
  |> List.filter is_def_ref
  |> group_by_definitions
  |> List.map (fun (def, refs) ->
      (path_of_def glob.file_module def, List.map path_of_ref refs |> list_uniq))

module Key = struct
  type t = string * string
  let compare = compare
end


module Map = Map.Make (Key)

type t = path list Map.t

let empty : t = Map.empty

let map_add_to_list key value map =
  let f = function
    | None -> Some [value]
    | Some vs -> Some (value :: vs)
  in
  Map.update key f map

let create_inv_map_from_globs globs =
  let add_grp_entries (def, refs) map =
    List.fold_left (fun map ref -> map_add_to_list ref def map) map refs
  in
  (List.concat_map from_glob globs)
  |> List.fold_left (fun map grp -> add_grp_entries grp map) Map.empty

let find map ref =
  Map.find_opt ref map

(* Module-level aggregates built on top of the per-identifier table above.
   These reflect actual reference usage from .glob data, not Require
   statements: a required-but-unused module will not appear here. *)

(* Modules that reference some identifier defined in [module_name]
   (displayed as "Used by"). *)
let referencing_modules table module_name =
  Map.fold (fun (ref_mod, _ref_id) defs acc ->
      if ref_mod = module_name then
        List.map fst defs @ acc
      else acc)
    table []
  |> list_uniq
  |> List.filter (fun m -> m <> module_name)

(* Modules defining an identifier referenced from some definition of
   [module_name] (displayed as "Uses"). *)
let referenced_modules table module_name =
  Map.fold (fun (ref_mod, _ref_id) defs acc ->
      if List.exists (fun (def_mod, _) -> def_mod = module_name) defs then
        ref_mod :: acc
      else acc)
    table []
  |> list_uniq
  |> List.filter (fun m -> m <> module_name)
