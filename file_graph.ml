open Common

type input =
  | FromDependFile of string
  | FromDotFile of string

let parse_filepath name =
  let ext = Filename.extension name in
  match List.rev @@ String.split_on_char '/' name with
  | [] -> failwith (!%"file_graph.ml: parse_filepath: The depend file has an item where the filename '%s' could not be read correctly" name)
  | [base] -> ([], base, ext)
  | base :: path -> (List.rev path, base, ext)

let url (path, base, _ext) =
  String.concat "." path ^ base ^ ".html"

let key (path, base, _ext) = String.concat "." (path @ [base])

let parse_line (nodes, edges) line =
  let open Str in
  if string_match (regexp {|\([^ \t\n]*\)\.vo.*: \(.*\)|}) line 0 then begin
      let file = matched_group 1 line ^ ".vo" in
      let src = parse_filepath file in
      let dests =
        matched_group 2 line |> String.trim |> String.split_on_char ' '
        |> List.map parse_filepath
      in
      let new_edges = List.map (fun dst -> (src, dst)) dests in
      (list_uniq (nodes @ dests), edges @ new_edges)
    end
  else (nodes, edges)

let make_graphviz (nodes, edges) =
  let sedge (src, dst) = !%{|"%s" -> "%s";|} (key src) (key dst) in
  let body = List.map sedge edges |> String.concat "\n" in
  !%"digraph depend {\n%s\n}" body
  |> Graphviz.of_string

let parse ch =
  let rec loop store =
    try
      let line = input_line ch in
      let store' = parse_line store line in
      loop store'
    with
    | End_of_file -> store
  in
  loop ([], []) |> make_graphviz

let parse_dep_file filename =
  file_using_r filename (fun ch -> parse ch)
