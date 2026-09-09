(* *********************************************************************)
(*                                                                     *)
(*        Addition to the the Coq2HTML documentation generator         *)
(*                                                                     *)
(*  Copyright National Institute of Advanced Industrial Science and    *)
(*  Technology.  All rights reserved.  This file is distributed        *)
(*  under the terms of the GNU General Public License as published by  *)
(*  the Free Software Foundation, either version 2 of the License, or  *)
(*  (at your option) any later version.                                *)
(*                                                                     *)
(* *********************************************************************)

open Common
open Glob_kind

type range = Range.t

let sanitize_linkname s =
  let rec loop esc i =
    if i < 0 then if esc then html_escaped s else s
    else match s.[i] with
         | 'a'..'z' | 'A'..'Z' | '0'..'9' | '.' | '_' -> loop esc (i-1)
         | '<' | '>' | '&' | '\'' | '\"' -> loop true (i-1)
         | '-' | ':' -> loop esc (i-1) (* should be safe in HTML5 attribute name syntax *)
         | _ ->
            (* This name contains complex characters:
               this is probably a notation string, we simply hash it. *)
            Digest.to_hex (Digest.string s)
  in loop false (String.length s - 1)

type initial_letter =
  | Alphabetic of char (* A .. Z *)
  | Underscore (* '_' *)

let string_of_initial_letter = function
  | Alphabetic a -> String.make 1 a
  | Underscore -> "_"

(**
 * The initial charactors of Coq identifiers
 * - '_' : it can start with '_' as well as the regular alphabet
 * - '*' : Some notations begin with a symbol, such as `\sum_`.
 **)
let initials = (* ['A'; ...; 'Z'; '_'] *)
  let rec iter code store =
    if code <= Char.code 'Z' then iter (succ code) (Char.chr code :: store)
    else List.rev store
  in
  let alphas = iter (Char.code 'A') [] |> List.map (fun c -> Alphabetic c) in
  alphas @ [Underscore]

type file_path =
  | Dir of (string * file_path list)
  | File of string

let left_sidebar_files all_files =
  let sort_for_directories files =
    let comp x y =
      match x, y with
      | Dir _, File _ -> -1
      | File _, Dir _ -> 1
      | _, _ -> compare x y
    in
    List.sort comp files
  in
  let rec tag_of_file_path parents = function
    | File name ->
       let link = (String.concat "." (List.rev (name :: parents))) ^ ".html" in
       !%{|<li><a href="%s">%s</a></li>|} link name
    | Dir (name, fs) ->
       let current_path = List.rev (name :: parents) |> String.concat "." in
       let children =
         sort_for_directories fs
         |> List.map (tag_of_file_path (name :: parents))
       in
       !%{|<li><details id="%s"><summary>%s</summary>
          <ul>
          %s
          </ul>
          </details>
          </li>|} current_path name (String.concat "\n" children)
  in
  sort_for_directories all_files
  |> List.map (tag_of_file_path [])
  |> String.concat "\n"

let start_html_page ch title h1 project_name all_files =
  let open Str in
  global_replace (regexp_string "$NAME") title Resources.header
  |> global_replace (regexp_string "$H1") h1
  |> global_replace (regexp_string "$PROJECT") project_name
  |> global_replace (regexp_string "$FILES") (left_sidebar_files all_files)
  |> output_string ch

let compare_case_insensitive s1 s2 =
  String.(compare (lowercase_ascii s1) (lowercase_ascii s2))

let module_list_items modules =
  modules
  |> List.sort compare_case_insensitive
  |> List.map (fun m -> !%{|<li><a href="%s.html">%s</a></li>|} m m)
  |> String.concat "\n"

let uses_panel usedby_table module_name =
  let uses_modules =
    UsedByTable.referenced_modules usedby_table module_name |> module_list_items in
  let used_modules =
    UsedByTable.referencing_modules usedby_table module_name |> module_list_items in
  !%{|<details id="uses-right-pane">
        <summary>Uses</summary>
        <ul>
          %s
        </ul>
      </details>

      <details id="usedby-right-pane">
        <summary>Used By</summary>
        <ul>
          %s
        </ul>
      </details>|} uses_modules used_modules

let end_page_footer ?repo_file uses_panel_html ch =
  let open Str in
  let link_to_source_tag =
    Option.map (!%{|<a href="%s">Source</a>|}) repo_file
    |> Option.value ~default:""
  in
  global_replace (regexp_string "$LINK_TO_SOURCE") link_to_source_tag Resources.footer
  |> global_replace (regexp_string "$USES_PANEL") uses_panel_html
  |> output_string ch

(* For a page documenting a single Rocq module: [usedby_table] and
   [module_name] are mandatory, and are used to fill in the "Uses" /
   "Used by" panel with the modules it actually references. *)
let end_html_page ch ?repo_file usedby_table module_name =
  end_page_footer ?repo_file (uses_panel usedby_table module_name) ch

(* For a generated listing page (index.html, notation index, per-letter
   indexes) that is not about any single module: there is no "Uses" /
   "Used by" panel to show at all, so it is omitted entirely. *)
let end_index_page ch ?repo_file () =
  end_page_footer ?repo_file "" ch

let write_html_file ?repo_root all_files txt filename title project_name =
  let oc = open_out filename in
  let repo_file = repo_root in
  start_html_page oc title title project_name all_files;
  output_string oc txt;
  end_index_page oc ?repo_file ();
  close_out oc

type kind = Global | EntryKind of string

let kinds = [EntryKind "file";
             EntryKind "def";
             EntryKind "prf";
             EntryKind "abbrev";
             Global;
            ]

let skind = function Global -> "Global Index"
                   | EntryKind "def" -> "Definitions"
                   | EntryKind "prf" -> "Lemmas"
                   | EntryKind "abbrev" -> "Abbreviations"
                   | EntryKind "file" -> "Files"
                   | EntryKind other -> other

let is_kind = function
  | Global -> fun _ -> true
  | EntryKind k -> fun s -> s = k

let linkname_of_kind = function Global -> "global"
                              | EntryKind s -> s

let linkname_of_capital = string_of_initial_letter

type item = {kind: kind; name: string; linkname: string; module_: string}

let table citems =
  let mkrow kind =
    (!%"<td>%s</td>\n" (skind kind))
    ^ (List.map (fun (c, items) ->
           if List.exists (fun item -> kind = Global || item.kind = kind) items then
             !%{|<td><a href="index_%s_%s.html">%s</a></td>|} (linkname_of_kind kind) (linkname_of_capital c) (string_of_initial_letter c)
           else
             !%{|<td>%s</td>|} (string_of_initial_letter c)) citems
    |> String.concat "")
    |> fun s -> "<tr>" ^ s ^ "</tr>\n"
  in
  "<table><tbody>\n"
  ^ (List.map mkrow kinds |> String.concat "")
  ^ (!%{|<tr><td><a href="%s">Notations</a></td></tr>|} Notation_index.filename)
  ^ "</tbody></table>"

let generate_notation_list ?repo_root output_dir proj_name table all_files items =
  let body =
    List.map (fun item -> item.module_, item.linkname, item.name) items
    |> Notation_index.generate_body table
  in
  let filename = Filename.concat output_dir Notation_index.filename in
  let title = "Notations" in
  write_html_file ?repo_root all_files body filename title proj_name

(*
 * generate an html file, e.g., mathcomp.classical.functions.html
 *)
let generate_with_capital ?repo_root output_dir proj_name table all_files kind (c, items) =
  let html_of_item item =
    !%{|<a href="%s">%s</a> [%s, in %s]|} item.linkname item.name (linkname_of_kind item.kind) item.module_
  in
  if items = [] then () else
    let title = !%"%s (%s)" (string_of_initial_letter c) (skind kind) in
    let body =
      let h2 = if kind = Global then string_of_initial_letter c else title in
      List.filter (fun item -> kind = Global || item.kind = kind) items
      |> List.map html_of_item
      |> String.concat "<br>"
      |> (^) (!%"%s<h2>%s</h2>" table h2)
    in
    let filename = Filename.concat output_dir
        (!%"index_%s_%s.html" (linkname_of_kind kind) (linkname_of_capital c))
    in
    write_html_file ?repo_root all_files body filename title proj_name

let overwrite_dot_file_with_url xref_table dot_file = (* dirty *)
  let dot_content = String.concat "\n" (Common.read_lines dot_file) in
  let is_exists_in_dot_file name = Common.grep name dot_content in
  let all_hb_defs =
    XrefTable.fold (fun (mod_,_) (_, xref) store ->
        match xref with
        | Defs ds ->
          begin match List.find_opt (fun (path,typ) ->
              String.ends_with ~suffix:".pack_" path) ds with
            | Some (path,typ) ->
              (mod_, path) :: store
            | None -> store
          end
        | _ -> store)
      xref_table []
  in
  let hb_defs =
    all_hb_defs
    |> List.map (fun (mod_, path) ->
        (mod_, String.sub path 0 (String.length path - String.length ".pack_")))
    |> List.filter (fun (_, name) -> is_exists_in_dot_file name)
  in
  let node_with_node (mod_, name) =
    let url = mod_ ^ ".html#" ^ name in
    !%{|"%s" [URL="%s"]|}  name url
  in
  let links = String.concat ";\n" (List.map node_with_node hb_defs) in
  let target = {|  node [target="_blank"]|}(*リンクを別タブで表示*) in
  let lines = Common.read_lines dot_file in
  let lines = match lines with (* insert links to second line *)
     | line1 :: rest -> line1 :: target :: links :: rest
     | [] ->
       failwith ("empty lines: " ^ dot_file)
  in
  Common.write_lines dot_file lines

let generate_hierarchy_graph title xref_table output_dir dot_file =
  overwrite_dot_file_with_url xref_table dot_file;
  let svg_path = Filename.concat output_dir "hierarchy_graph.svg" in
  let dot = Graphviz.from_file dot_file in
  Graphviz.generate_svg svg_path dot;
  !%"<h2>Mathematical Structures (%s only)</h2>\n" title
  ^ Zoomable_svgtag.div "hierarchy_graph" svg_path

let generate_dependency_graph_from_dot output_dir dot =
  let svg_path = Filename.concat output_dir "dependency_graph.svg" in
  Graphviz.generate_svg svg_path dot;
  !%"<h2>Clickable Dependency Graph of Files</h2>\n"
  ^ Zoomable_svgtag.div "file-graph" svg_path

(*
 * generate index.html
 *)
let generate_topfile ?repo_root output_dir all_files xrefs title xref_table
      directory_mapping hierarchy_graph_dot_file file_graph_input =

  let hierarchy_graph =
    if hierarchy_graph_dot_file = "" then "" else
      generate_hierarchy_graph title xref_table output_dir hierarchy_graph_dot_file
  in
  let file_graph_dot =
    file_graph_input
    |> Option.map (function
           | File_graph.FromDotFile dot -> Graphviz.from_file dot
           | File_graph.FromDependFile dep ->
              File_graph.parse_dep_file directory_mapping dep)
  in
  let file_graph =
    Option.map (generate_dependency_graph_from_dot output_dir) file_graph_dot
    |> Option.value ~default:""
  in
  let body = table xrefs ^ hierarchy_graph ^ file_graph in
  write_html_file ?repo_root all_files body (Filename.concat output_dir "index.html") title title


let is_initial init s =
  if s = "" then false else
    match String.get s 0 with
    | '_' -> Underscore = init
    | ('a'..'z' as s0) | ('A'..'Z' as s0) ->
       begin match init with
       | Alphabetic a when Char.uppercase_ascii s0 = a -> true
       | _ -> false
       end
    | _ -> false


let all_files xref_modules =
  let rec iter = function
    | [] -> []
    | [single_name] :: rest ->
       File single_name :: iter rest
    | (dir_name :: path) :: rest ->
       let (brothers, rest) =
         List.partition (fun p -> List.hd p = dir_name) rest
       in
       let fs =
         (path :: List.map List.tl brothers)
         |> iter
       in
       Dir (dir_name, fs) :: iter rest
    | [] :: _ ->
      failwith "Generate_index.all_files: Please report: This is an unexpected case."
  in
  Hashtbl.to_seq_keys xref_modules
  |> List.of_seq
  |> List.sort compare_case_insensitive
  |> List.map (String.split_on_char '.')
  |> iter


let is_subproof path = String.ends_with ~suffix:"_subproof" path

let item_of kind module_ path =
  let linkname = !%"%s.html#%s" module_ (sanitize_linkname path) in
  {kind; name=path; linkname; module_}

let generate ?repo_root output_dir (xref_table:XrefTable.t) xref_modules
      title directory_mapping file_graph_input dependency_dot_file index_blacklist =
  let is_blacklisted =
    match index_blacklist with
    | None -> fun name -> false
    | Some blacklist ->
       fun name -> Index_blacklist.is_listed blacklist name
  in
  let notation_items =
    XrefTable.fold (fun (module_, pos) xref store ->
        match xref with
        | range, XrefTable.Defs defs ->
           List.filter_map (function (path, Notation) -> Some (item_of (EntryKind "not") module_ path)
                                   | _ -> None) defs
           |> fun items -> items @ store
        | _ -> store
      ) xref_table []
  in
  let indexed_items = (* exclude notations *)
    List.map (fun c ->
        let items =
          XrefTable.fold (fun (module_, pos) xref store ->
            match xref with
            | range, XrefTable.Defs defs ->
               List.filter (fun (path, _) -> is_initial c path) defs
               |> List.filter (fun (_, typ) -> typ <> Binder)
               |> List.filter (fun (_, typ) -> typ <> SectionVariableReference)
               |> List.filter (fun (_, typ) -> typ <> Notation)
               |> List.filter (fun (path, _) -> not (is_subproof path))
               |> List.filter (fun (path, _) -> not (is_blacklisted path))
               |> List.map (fun (path, typ) -> item_of (EntryKind (Glob_kind.to_string typ)) module_ path)
               |> fun is -> is @ store
            | range, Ref _ -> store) xref_table []
        in

        Hashtbl.fold (fun filename _ store ->
            let basename = Str.(split (regexp_string ".") filename) |> List.rev |> List.hd in
            if is_initial c basename then
              let linkname = !%"%s.html" filename in
              {kind=EntryKind "file"; name=basename; linkname; module_=filename} :: store
            else store) xref_modules items
        |> List.sort (fun x y -> compare (String.lowercase_ascii x.name)
                                   (String.lowercase_ascii y.name))
        |> fun items -> (c, items))
      initials
  in
  let all_files = all_files xref_modules in
  let table = table indexed_items in
  List.iter (fun kind ->
      List.iter (generate_with_capital ?repo_root output_dir title table all_files kind) indexed_items)
    kinds;
  generate_notation_list ?repo_root output_dir title table all_files notation_items;
  generate_topfile ?repo_root output_dir all_files indexed_items title xref_table
    directory_mapping file_graph_input dependency_dot_file
