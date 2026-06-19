(* File-dependency graph: parsing, transitive reduction, and rendering.
   Pipeline adapted and extended from the Lua script [etc/buildlibgraph]
   originally written by Maxime Dénès for math-comp, removed in math-comp
   PR #1534. Initial OCaml draft generated with GitHub Copilot, then
   reviewed and adapted by hand. *)

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
  let reduced_edges =
    Graph_reduction.transitive_reduction_by_key
      ~nodes
      ~key
      ~edges
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

let rec list_starts_with prefix lst =
  match prefix, lst with
  | [], _ -> true
  | _, [] -> false
  | x :: xs, y :: ys -> x = y && list_starts_with xs ys

let to_cytoscape_elements_json ({ nodes; edges } : graph) =
  let clusters =
    List.map cluster_paths nodes
    |> List.flatten
    |> list_uniq
    |> List.sort (fun x y -> compare (List.length x, x) (List.length y, y))
  in
  (* Compute cluster-to-cluster meta-edges.
     For every module-level edge (src→dst), add a cluster-level meta-edge
     for every pair of ancestor clusters (one from the src side, one from the
     dst side) that are not in an ancestor/descendant relationship with each
     other.  This ensures that, no matter at what depth the user collapses a
     group, the inter-group connections remain visible.
     The resulting set is then transitively reduced. *)
  let path_prefixes lst =
    let rec aux acc cur = function
      | [] -> List.rev acc
      | x :: rest -> let p = cur @ [x] in aux (p :: acc) p rest
    in aux [] [] lst
  in
  let raw_meta = Hashtbl.create 16 in
  List.iter (fun (src, dst) ->
    let sp = path_prefixes (path src)
    and dp = path_prefixes (path dst) in
    List.iter (fun sc ->
      List.iter (fun dc ->
        if not (list_starts_with sc dc) && not (list_starts_with dc sc) then
          Hashtbl.replace raw_meta (sc, dc) ()
      ) dp
    ) sp
  ) edges;
  let all_meta_cls =
    Hashtbl.fold (fun (s, d) () acc -> s :: d :: acc) raw_meta []
    |> list_uniq
  in
  let raw_meta_edges =
    Hashtbl.fold (fun (src_c, dst_c) () acc -> (src_c, dst_c) :: acc) raw_meta []
  in
  let reduced_meta_edges =
    Graph_reduction.transitive_reduction_by_key
      ~nodes:all_meta_cls
      ~key:cluster_id
      ~edges:raw_meta_edges
  in
  let meta_edge_elements =
    let i = ref (List.length edges) in
    List.fold_left (fun acc (src_c, dst_c) ->
      let elt = `Assoc ["data", `Assoc [
          "id",     `String (!%"meta:%d" !i);
          "source", `String (cluster_id src_c);
          "target", `String (cluster_id dst_c)
        ]] in
      incr i; elt :: acc
    ) [] reduced_meta_edges
  in
  (* One '+' expand/collapse button node per cluster *)
  let plus_element cluster_path =
    `Assoc ["data", `Assoc [
        "id",     `String (cluster_id cluster_path ^ ":plus");
        "name",   `String "+";
        "parent", `String (cluster_id cluster_path)
      ]]
  in
  let elements =
    List.map cluster_element clusters
    @ List.map plus_element clusters
    @ List.map node_element nodes
    @ List.mapi edge_element edges
    @ meta_edge_elements
  in
  pretty_to_string (`List elements)
