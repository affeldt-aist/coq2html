open Common
open Yojson.Safe

type input =
  | FromDependFile of string
  | FromDotFile of string

type renderer =
  | Graphviz
  | Cytoscape

type node = string list * string * string
type edge = node * node
type graph = { nodes : node list; edges : edge list }

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

let graph_of_store (nodes, edges) =
  { nodes = list_uniq nodes; edges }

let transitive_reduction ({ nodes; edges } as graph) =
  let node_by_key = Hashtbl.create (List.length nodes) in
  List.iter (fun node -> Hashtbl.replace node_by_key (key node) node) nodes;
  let matrix = Hashtbl.create (List.length nodes) in
  let ensure_row node_key =
    match Hashtbl.find_opt matrix node_key with
    | Some row -> row
    | None ->
       let row = Hashtbl.create (List.length nodes) in
       Hashtbl.add matrix node_key row;
       row
  in
  List.iter (fun node -> ignore (ensure_row (key node))) nodes;
  List.iter (fun (src, dst) ->
      let row = ensure_row (key src) in
      Hashtbl.replace row (key dst) true) edges;
  List.iter (fun mid ->
      let mid_key = key mid in
      let mid_row = ensure_row mid_key in
      List.iter (fun src ->
          let src_key = key src in
          let src_row = ensure_row src_key in
          if Hashtbl.mem src_row mid_key then
            List.iter (fun dst ->
                let dst_key = key dst in
                if Hashtbl.mem mid_row dst_key then
                  Hashtbl.replace src_row dst_key true) nodes) nodes) nodes;
  List.iter (fun mid ->
      let mid_key = key mid in
      let mid_row = ensure_row mid_key in
      List.iter (fun src ->
          let src_key = key src in
          let src_row = ensure_row src_key in
          if Hashtbl.mem src_row mid_key then
            List.iter (fun dst ->
                let dst_key = key dst in
                if Hashtbl.mem mid_row dst_key then
                  Hashtbl.remove src_row dst_key) nodes) nodes) nodes;
  let reduced_edges =
    List.fold_left (fun store src ->
        let src_key = key src in
        let src_row = ensure_row src_key in
        List.fold_left (fun store dst ->
            let dst_key = key dst in
            if Hashtbl.mem src_row dst_key then
              match Hashtbl.find_opt node_by_key dst_key with
              | Some dst_node -> (src, dst_node) :: store
              | None -> store
            else store) store nodes) [] nodes
    |> List.rev
  in
  { graph with edges = reduced_edges }

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

let make_dot ({ nodes; edges } : graph) : string =
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

let to_graphviz graph =
  Graphviz.of_string @@ make_dot graph

let parse_dep directory_mappings ch =
  let rec loop store =
    try
      let line = input_line ch in
      let store' = parse_line directory_mappings store line in
      loop store'
    with
    | End_of_file -> store
  in
  loop ([], []) |> graph_of_store |> transitive_reduction

let parse_dep_file directory_mappings filename =
  file_using_r filename (fun ch -> parse_dep directory_mappings ch)

let cluster_paths node =
  let rec iter current store = function
    | [] -> List.rev store
    | dir :: rest ->
       let next = current @ [dir] in
       iter next (next :: store) rest
  in
  iter [] [] (path node)

let cluster_id path =
  "cluster:" ^ String.concat "." path

let cluster_label = function
  | [] -> ""
  | path -> List.hd (List.rev path)

let cluster_element path =
  let data =
    [ "id", `String (cluster_id path);
      "name", `String (cluster_label path) ]
    |> fun data ->
    match List.rev path with
    | _ :: parent_rev ->
       let parent = List.rev parent_rev in
       if parent = [] then data
       else ("parent", `String (cluster_id parent)) :: data
    | [] -> data
  in
  `Assoc ["data", `Assoc data]

let node_element node =
  let data =
    [ "id", `String (key node);
      "name", `String (basename node);
      "url", `String (url node) ]
    |> fun data ->
    match path node with
    | [] -> data
    | p -> ("parent", `String (cluster_id p)) :: data
  in
  `Assoc ["data", `Assoc data]

let edge_element i (src, dst) =
  `Assoc [
      "data", `Assoc [
          "id", `String (!%"edge:%d" i);
          "source", `String (key src);
          "target", `String (key dst)
        ]
    ]

let to_cytoscape_elements_json ({ nodes; edges } : graph) =
  let clusters =
    List.map cluster_paths nodes
    |> List.flatten
    |> list_uniq
    |> List.sort (fun x y -> compare (List.length x, x) (List.length y, y))
  in
  let elements =
    List.map cluster_element clusters
    @ List.map node_element nodes
    @ List.mapi edge_element edges
  in
  pretty_to_string (`List elements)
