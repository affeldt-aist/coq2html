module Key = struct
  type t = string * int
  let compare = compare
end

module Map = Map.Make (Key)

type xref =
  | Defs of (string * string) list    (* path, type *)
  | Ref of string * string * string (* unit, path, type *)

let sxref = function
  | Defs defs -> "Defs[" ^ (List.map fst defs |> String.concat ", ")^"]"
  | Ref (unit, path, ty) ->
    Printf.sprintf "Ref(%s,%s,%s)" unit path ty

type t = (Range.t * xref) Map.t

let find map module_name pos =
  match Map.find_last_opt (fun key -> key <= (module_name, pos)) map with
  | None -> None
  | Some (_key, (range, xref)) ->
    if Range.in_ pos range then Some (range, xref)
    else None

let empty = Map.empty

let add_reference xref_table curmod pos_from pos_to dp path ty =
  if ty = "sec" then xref_table else
  let range = (pos_from, pos_to) in
  match Map.find_opt (curmod, pos_from) xref_table with
  | Some (range0, xref) when range = range ->
     (* ignore references if the glob file has a reference and definitions at a
        same position.
        issue: https://github.com/yoshihiro503/coq2html/issues/2
      *)
    xref_table
  | _ ->
  Map.add (curmod, pos_from) (range, Ref (dp, path, ty)) xref_table

let add_definition xref_table curmod pos_from pos_to path ty =
  if ty = "sec" then xref_table else
  (*eprintf "add_definition %s %d %s %s %s\n" curmod pos_from sp id ty;*)
  let range = (pos_from, pos_to) in
  match Map.find_opt (curmod, pos_from) xref_table with
  | None ->
     Map.add (curmod, pos_from) (range, Defs [path, ty]) xref_table
  | Some (range0, Defs defs) ->
    if range <> range0 then
      Printf.eprintf "Warning: different pathes which have same starting position exists: module '%s', '%s' [%d:%d]\n" curmod path pos_from pos_to;
     Map.add (curmod, pos_from) (range, Defs ((path, ty) :: defs)) xref_table
  | Some (_, Ref (unit, path_, typ)) ->
     (* ignore references if the glob file has a reference and definitions at a
        same position.
        issue: https://github.com/yoshihiro503/coq2html/issues/2
      *)
    Map.add  (curmod, pos_from) (range, Defs [path, ty]) xref_table

let fold f xref_table init =
  Map.fold f xref_table init

let dump t =
  Printf.printf"====DUMP====\n";
  Map.iter (fun (m, pos) (range, xref) ->
      Printf.printf "%s:%d: %s\n" m pos (sxref xref)) t;
  Printf.printf"============\n"
