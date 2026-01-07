open Common

type input =
  | FromDependFile of string
  | FromDotFile of string

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

let key (path, base, _ext) = String.concat "." (path @ [base])

let basename (_path, base, _ext) = base

let parse_line directory_mappings (nodes, edges) line =
  let open Str in
  if string_match (regexp {|\([^ ]*\)\.vo.*: \(.*\)|}) line 0 then begin
      let file = matched_group 1 line ^ ".vo" in
      let src = parse_filepath directory_mappings file in
      let dests =
        matched_group 2 line |> String.trim |> String.split_on_char ' '
        |> List.map (parse_filepath directory_mappings)
        |> List.filter (fun (_,_,ext) -> ext = ".vo")
      in
      let new_edges = List.map (fun dst -> (src, dst)) dests in
      (list_uniq (src :: nodes @ dests), edges @ new_edges)
    end
  else (nodes, edges)

let make_dot (nodes, edges) : string =
  let style =
    {|  bgcolor=white; splines=true; nodesep=1; node [fontsize=18, shape=rect, color="#dbc3b6", style="rounded,filled"];|}
  in
  let snode node =
    !%{|  "%s" [label="%s" URL="%s"]|} (key node) (basename node) (url node)
  in
  let sedge (src, dst) = !%{|  "%s" -> "%s";|} (key src) (key dst) in
  "digraph depend {\n"
  ^ style ^ "\n"
  ^ String.concat "\n" (List.map snode nodes)
  ^ "\n\n"
  ^ String.concat "\n" (List.map sedge edges)
  ^ "\n}"
  |> (fun s -> Log.debug s; s)

let make_graphviz (nodes, edges) =
  Graphviz.of_string @@ make_dot (nodes, edges)

let parse directory_mappings ch =
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
  file_using_r filename (fun ch -> parse directory_mappings ch)
