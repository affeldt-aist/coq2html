open Common

type input =
  | FromDependFile of string
  | FromDotFile of string

type node = string list * string * string
type edge = node * node

let parse_filepath directory_mappings name =
  let ext = Filename.extension name in
  match List.rev @@ String.split_on_char '/' name with
  | [] -> failwith (!%"file_graph.ml: parse_filepath: The depend file has an item where the filename '%s' could not be read correctly" name)
  | [base] ->
     ([], Filename.remove_extension base, ext)
  | base :: path ->
     let logical_path =
       Directory_mappings.apply directory_mappings (List.rev path)
     in
     (logical_path, Filename.remove_extension base, ext)

let url (path, base, _ext) =
  String.concat "." path ^ "." ^ base ^ ".html"

let path (path, _base, _ext) = path
let key (path, base, _ext) = String.concat "." (path @ [base])
let basename (_path, base, _ext) = base

let parse_line directory_mappings (nodes, edges) line =
  let open Str in
  if string_match (regexp {|\([^ ]*\)\.vo.*: \(.*\)|}) line 0 then begin
      let file = matched_group 1 line ^ ".vo" in
      let dst = parse_filepath directory_mappings file in
      let srcs =
        matched_group 2 line |> String.trim |> String.split_on_char ' '
        |> List.map (parse_filepath directory_mappings)
        |> List.filter (fun (_,_,ext) -> ext = ".vo")
      in
      let new_edges = List.map (fun src -> (src, dst)) srcs in
      (list_uniq (dst :: nodes @ srcs), edges @ new_edges)
    end
  else (nodes, edges)

type dir_tree =
  | Dir of string * dir_tree list
  | File of node

let make_namespace_tree (nodes: node list) : dir_tree list =
  let chop_path_head (rel_path, node) = (List.tl rel_path, node) in
  let rec iter path_nodes =
  list_group_by (fun (rel_path, node) ->
      match rel_path with
      | dir :: _ -> `Dirname dir
      | [] -> `File (basename node)) path_nodes
  |> List.map (function
         | `Dirname dir, grp ->
            let sub_nodes = List.map chop_path_head grp in
            Dir (dir, iter sub_nodes)
         | `File name, [(_,node)] -> File node)
  in
  iter (List.map (fun node -> (path node, node)) nodes)

let make_dot (nodes, edges) : string =
  let trees = make_namespace_tree nodes in
  let indent depth = String.make (2 + 2 * depth) ' ' in
  let color = function 0 -> "white" | 1 -> "#ababab" | _ -> "1" in
  let rec snode depth = function
    | Dir (dir, trees) ->
       let ind = indent depth in
       !%"%ssubgraph cluster_%s {\n" ind dir
       ^ !%{|%slabel = "%s";|} (indent (depth+1)) dir ^ "\n"
       ^ !%{|%sfillcolor = "%s";|} (indent (depth+1)) (color depth) ^ "\n"
       ^ String.concat "\n" (List.map (snode (depth + 1)) trees)
       ^ !%"\n%s};" ind
    | File node -> !%{|%s"%s" [label="%s", URL="%s"]|}
                     (indent depth)
                     (key node) (basename node) (url node)
  in
  let style =
    {|  bgcolor=white; splines=true; nodesep=1; node [fontsize=18, shape=rect, color="#dbc3b6", style="rounded,filled"];|}
  in
  let sedge (src, dst) = !%{|  "%s" -> "%s";|} (key src) (key dst) in
  "digraph depend {\n"
  ^ style ^ "\n"
  ^ String.concat "\n" (List.map (snode 0) trees)
  ^ "\n\n"
  ^ String.concat "\n" (List.map sedge edges)
  ^ "\n}"
  |> (fun s -> Log.debug s; s)

let make_graphviz (nodes, edges) =
  Graphviz.of_string @@ make_dot (nodes, edges)

let parse_dep directory_mappings ch =
  let rec loop store =
    try
      let line = input_line ch in
      let store' = parse_line directory_mappings store line in
      loop store'
    with
    | End_of_file -> store
  in
  loop ([], []) |> make_graphviz

let parse_dep_file directory_mappings filename =
  file_using_r filename (fun ch -> parse_dep directory_mappings ch)
