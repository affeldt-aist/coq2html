open Common
open Yojson.Safe

type node = {
  id : string;
  label : string;
  url : string option;
  group : string option;
}

type edge = string * string

type graph = {
  nodes : node list;
  edges : edge list;
}

let group_of_id id =
  match String.split_on_char '_' id with
  | grp :: _ :: _ -> Some grp
  | _ -> None

let ensure_node tbl id =
  match Hashtbl.find_opt tbl id with
  | Some n -> n
  | None ->
     let n = { id; label = id; url = None; group = group_of_id id } in
     Hashtbl.add tbl id n;
     n

let update_node tbl node =
  Hashtbl.replace tbl node.id node

let parse_dot_file filename =
  let json_tmp = Filename.temp_file "rocqnavi-structure-graph" ".json" in
  let quote = Filename.quote in
  let run_dot_to_file format =
    let cmd =
      !%"dot %s %s > %s"
        format (quote filename) (quote json_tmp)
    in
    Sys.command cmd
  in
  let run_with_fallback runner =
    let s0 = runner "-Tdot_json" in
    if s0 = 0 then s0
    else
      let s1 = runner "-Tjson" in
      if s1 = 0 then s1 else runner "-Txdot_json"
  in
  let status = run_with_fallback run_dot_to_file in
  if status <> 0 then begin
    Sys.remove json_tmp;
    failwith (!%"Could not convert structure DOT graph '%s' to Graphviz JSON (-Tdot_json/-Tjson/-Txdot_json)." filename)
  end;
  let json =
    try Yojson.Safe.from_file json_tmp
    with exn ->
      Sys.remove json_tmp;
      raise exn
  in
  Sys.remove json_tmp;

  let nodes = Hashtbl.create 256 in
  let edges = ref [] in
  let node_id_by_gvid = Hashtbl.create 256 in

  let field_opt key = function
    | `Assoc fields -> List.assoc_opt key fields
    | _ -> None
  in
  let string_field key obj =
    match field_opt key obj with
    | Some (`String s) -> Some s
    | _ -> None
  in
  let int_field key obj =
    match field_opt key obj with
    | Some (`Int i) -> Some i
    | Some (`Intlit s) -> int_of_string_opt s
    | _ -> None
  in
  let as_list = function
    | `List xs -> xs
    | _ -> []
  in

  let objects =
    match field_opt "objects" json with
    | Some j -> as_list j
    | None -> []
  in
  List.iter (fun obj ->
      match int_field "_gvid" obj, string_field "name" obj with
      | Some gvid, Some id ->
         let prev = ensure_node nodes id in
         let url = string_field "URL" obj in
         let merged_url = match url with Some _ -> url | None -> prev.url in
         update_node nodes { prev with url = merged_url };
         Hashtbl.replace node_id_by_gvid gvid id
      | _ -> ()) objects;

  let json_edges =
    match field_opt "edges" json with
    | Some j -> as_list j
    | None -> []
  in
  List.iter (fun e ->
      match int_field "tail" e, int_field "head" e with
      | Some t, Some h ->
         begin match Hashtbl.find_opt node_id_by_gvid t,
                     Hashtbl.find_opt node_id_by_gvid h with
         | Some src, Some dst when src <> dst ->
            edges := (src, dst) :: !edges
         | _ -> ()
         end
      | _ -> ()) json_edges;

  let nodes = Hashtbl.fold (fun _ v acc -> v :: acc) nodes [] |> List.rev in
  let node_ids = List.map (fun node -> node.id) nodes in
  let edges =
    Graph_reduction.transitive_reduction_by_key
      ~nodes:node_ids
      ~key:(fun id -> id)
      ~edges:!edges
  in

  {
    nodes;
    edges;
  }

let cluster_id grp = "cluster:" ^ grp
let plus_id grp = cluster_id grp ^ ":plus"

let cluster_element grp =
  `Assoc ["data", `Assoc [
      "id", `String (cluster_id grp);
      "name", `String grp
    ]]

let plus_element grp =
  `Assoc ["data", `Assoc [
      "id", `String (plus_id grp);
      "name", `String "+";
      "parent", `String (cluster_id grp)
    ]]

let node_element n =
  let data =
    [ "id", `String n.id;
      "name", `String n.label ]
    |> fun xs ->
    let xs = match n.url with Some u -> ("url", `String u) :: xs | None -> xs in
    match n.group with
    | Some g -> ("parent", `String (cluster_id g)) :: xs
    | None -> xs
  in
  `Assoc ["data", `Assoc data]

let edge_element i (src, dst) =
  `Assoc ["data", `Assoc [
      "id", `String (!%"edge:%d" i);
      "source", `String src;
      "target", `String dst
    ]]

let to_cytoscape_elements_json ({ nodes; edges } : graph) =
  let groups =
    nodes
    |> List.filter_map (fun n -> n.group)
    |> list_uniq
    |> List.sort compare
  in

  let group_edges = Hashtbl.create 32 in
  let node_by_id = Hashtbl.create (List.length nodes) in
  List.iter (fun n -> Hashtbl.replace node_by_id n.id n) nodes;

  List.iter (fun (src, dst) ->
      match Hashtbl.find_opt node_by_id src, Hashtbl.find_opt node_by_id dst with
      | Some sn, Some dn ->
         begin match sn.group, dn.group with
         | Some sg, Some dg when sg <> dg -> Hashtbl.replace group_edges (sg, dg) ()
         | _ -> ()
         end
      | _ -> ()) edges;

  (* Transitive closure + reduction on group graph. *)
  let groups_in_meta =
    Hashtbl.fold (fun (s, d) () acc -> s :: d :: acc) group_edges []
    |> list_uniq
  in
  let raw_meta_edges =
    Hashtbl.fold (fun (sg, dg) () acc -> (sg, dg) :: acc) group_edges []
  in
  let reduced_meta_edges =
    Graph_reduction.transitive_reduction_by_key
      ~nodes:groups_in_meta
      ~key:(fun x -> x)
      ~edges:raw_meta_edges
  in

  let meta_edge_elements =
    let i = ref (List.length edges) in
    List.fold_left (fun acc (sg, dg) ->
        let elt = `Assoc ["data", `Assoc [
            "id", `String (!%"meta:%d" !i);
            "source", `String (cluster_id sg);
            "target", `String (cluster_id dg)
          ]] in
        incr i;
        elt :: acc) [] reduced_meta_edges
  in

  let elements =
    List.map cluster_element groups
    @ List.map plus_element groups
    @ List.map node_element nodes
    @ List.mapi edge_element edges
    @ meta_edge_elements
  in
  pretty_to_string (`List elements)
