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

let use_file filename f =
  let ch = open_in filename in
  try
    let y = f ch in
    close_in ch; y
  with
  | e -> close_in ch; raise e

let read_file filename = use_file filename (fun ch ->
    really_input_string ch (in_channel_length ch))

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

let sidebar_files all_files =
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

let start_html_page ch ?repo_file title h1 project_name all_files =
  let open Str in
  let link_to_source_tag =
    Option.map (!%{|<a href="%s">source</a>|}) repo_file
    |> Option.value ~default:""
  in
  global_replace (regexp_string "$NAME") title Resources.header
  |> global_replace (regexp_string "$H1") h1
  |> global_replace (regexp_string "$PROJECT") project_name
  |> global_replace (regexp_string "$FILES") (sidebar_files all_files)
  |> global_replace (regexp_string "$LINK_TO_SOURCE") link_to_source_tag
  |> output_string ch

let end_html_page ch =
  output_string ch Resources.footer

let write_html_file ?repo_root all_files txt filename title project_name =
  let oc = open_out filename in
  let repo_file = repo_root in
  start_html_page oc ?repo_file title title project_name all_files;
  output_string oc txt;
  end_html_page oc;
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

let notations_html_filename = "index_notations.html"

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
  ^ (!%{|<tr><td><a href="%s">Notations</a></td></tr>|} notations_html_filename)
  ^ "</tbody></table>"

let notation_of_item item =
  match Str.(bounded_split_delim (regexp ":") item.name 4) with
  | [_; _; ""; notation] -> (`NoScope, notation, item)
  | [_; _; scope; notation] -> (`Scope scope, notation, item)
  | _ ->
     failwith (!%"unexpected notation format in glob file: name=%s" item.name)

let show_scope = function
  | `NoScope -> "no scope"
  | `Scope scope -> scope

let compare_scope x y =
  match x, y with
  | `NoScope, `NoScope -> 0
  | `NoScope, _ -> -1
  | _, `NoScope -> 1
  | `Scope x, `Scope y -> compare x y

let html_of_notation scope notation item =
  let scope =
    match scope with
    | `NoScope -> "<span class=\"warning\">no scope</span>"
    | `Scope scope -> "in " ^ scope
  in
  let show notation =
    let len = String.length notation in
    let rec iter pos tags =
      let text_of_placeholder s =
        Str.(global_replace (regexp_string "_")  s " ")
(*        |> fun s -> Str.(global_replace (regexp_string "x") s "x")*)
      in
      if pos < len then
        match String.index_from_opt notation pos '\'' with
        | Some pos' when pos = pos' -> quoted (pos+1) tags []
        | Some pos' ->
           quoted (pos'+1)
             (text_of_placeholder (String.sub notation pos (pos'-pos)) :: tags) []
        | None ->
           List.rev (
               text_of_placeholder (String.sub notation pos (len - pos)) :: tags)
      else List.rev tags
    and quoted pos tags store =
      let tag_of_quoted ss =
        String.concat "" (List.rev ss)
        |> !%"<span class=\"notation-symbol\">%s</span>"
      in
      if pos < len then
        match String.index_from_opt notation pos '\'' with
        | Some pos' when pos' = len - 1 ->
           tag_of_quoted (String.sub notation pos (pos'-pos) :: store) :: tags
           |> List.rev
        | Some pos' when String.get notation (pos'+1) = '\'' ->
           (* two contiguous quotations *)
           let s = String.sub notation pos (pos' - pos) in
           quoted (pos'+2) tags ("\'" :: s :: store)
        | Some pos' ->
           (* termination of the quote *)
           let tag = tag_of_quoted (String.sub notation pos (pos' - pos) :: store) in
           iter (pos' + 1) (tag :: tags)
        | None ->
           failwith "unclosed quote"
      else
        List.rev (tag_of_quoted store :: tags)
    in
    String.concat "" (iter 0 [])
  in
  !%{|<a href="%s">%s</a> [%s, in %s] (%s)|} item.linkname (show notation) (linkname_of_kind item.kind) item.module_ scope

let generate_notation_list ?repo_root output_dir proj_name table all_files items =
  let grouped =
    List.map notation_of_item items
    |> Common.list_group_by (fun (scope, not, item) -> scope)
    |> List.sort (fun (s1, _) (s2, _) -> compare_scope s1 s2)
    |> List.map (fun (scope, nots) -> scope, Common.list_sort_by (fun (_, not, _) -> not) nots)
  in
  let html_of_group (scope, notations) =
    let h2 = !%"<h2>%s</h2>" (show_scope scope) in
    let tags = List.map (fun (scope, not, item) -> html_of_notation scope not item) notations in
    h2 ^ String.concat "<br>\n" tags
  in
  let body =
    table ^ (String.concat "" @@ List.map html_of_group grouped)
  in
  let filename = Filename.concat output_dir notations_html_filename in
  let title = "Notations" in
  write_html_file ?repo_root all_files body filename title proj_name

let compare_case_insensitive s1 s2 =
  String.(compare (lowercase_ascii s1) (lowercase_ascii s2))

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
  let links = String.concat "; " (List.map node_with_node hb_defs) in
  let lines = Common.read_lines dot_file in
  let lines = match lines with (* insert links to second line *)
     | line1 :: rest -> line1 :: links :: rest
     | [] ->
       failwith ("empty lines: " ^ dot_file)
  in
  Common.write_lines dot_file lines

let generate_hierarchy_graph title xref_table output_dir dot_file =
  overwrite_dot_file_with_url xref_table dot_file;
  let png_filename = "hierarchy_graph.png" in
  let png_path = Filename.concat output_dir png_filename in
  let map_path = Filename.concat output_dir "hierarchy_graph.map" in
  Graphviz.from_file dot_file
  |> Graphviz.generate_file png_path map_path;
  let map = read_file map_path in
  (*TODO: ↓ The map id (#Hierarchy) should be taken from dot file *)
  Printf.sprintf {|<h2>Mathematical Structures (%s only)</h2><img src="%s" title usemap="#Hierarchy" class="img-darkmode-enable"/>
%s|} title png_filename map

let generate_dependency_graph_from_dot output_dir dot =
  let png_filename = "dependency_graph.png" in
  let png_path = Filename.concat output_dir png_filename in
  let map_path = Filename.concat output_dir "dependency_graph.map" in
  dot
  |> Graphviz.generate_file png_path map_path;
  let map = read_file map_path in
  Printf.sprintf {|<h2>Clickable Dependency Graph of Files</h2><img src="%s" usemap="#depend" class="img-darkmode-enable"/>%s|} png_filename map

let generate_dependency_graph_cytoscape _output_dir graph =
  let elements = File_graph.to_cytoscape_elements_json graph in
  Printf.sprintf {|
<style>
  #rocqnavi-graph-section {
    position: relative;
    background: #fff;
  }
  .rocqnavi-graph-toolbar {
    display: flex;
    gap: 6px;
    margin: 0.5em 0 0.4em;
    flex-wrap: wrap;
    align-items: center;
  }
  .rocqnavi-graph-toolbar button {
    display: inline-flex;
    align-items: center;
    gap: 4px;
    padding: 5px 11px;
    font-size: 13px;
    cursor: pointer;
    border: 1px solid #bbb;
    border-radius: 4px;
    background: #f5f5f5;
    color: #333;
    user-select: none;
    white-space: nowrap;
  }
  .rocqnavi-graph-toolbar button:hover { background: #e2e2e2; }
  .rocqnavi-graph-toolbar .sep {
    color: #bbb;
    padding: 0 2px;
    user-select: none;
  }
  .rocqnavi-graph-hint {
    font-size: 12px;
    color: #888;
    margin: 0 0 0.4em;
  }
  #dependency-graph-cytoscape {
    width: 100%%;
    height: 70vh;
    min-height: 300px;
    border: 1px solid #d7d7d7;
    margin: 0 0 1em;
  }
</style>
<div id="rocqnavi-graph-section">
<h2>Interactive Dependency Graph of Files</h2>
<div class="rocqnavi-graph-toolbar">
  <button type="button" id="graph-btn-zoom-in"      title="Zoom in">&#xFF0B; Zoom in</button>
  <button type="button" id="graph-btn-zoom-out"     title="Zoom out">&#xFF0D; Zoom out</button>
  <button type="button" id="graph-btn-fit"          title="Fit whole graph in view">&#x229E; Fit</button>
  <span class="sep">|</span>
  <button type="button" id="graph-btn-collapse-all" title="Collapse all cluster groups">&#x229F; Collapse groups</button>
  <button type="button" id="graph-btn-expand-all"   title="Expand all cluster groups">&#x229E; Expand groups</button>
  <span class="sep">|</span>
  <button type="button" id="graph-btn-fs"           title="Toggle full-screen">&#x26F6; Full screen</button>
</div>
<p class="rocqnavi-graph-hint">Click a group label to collapse or expand it. Click &#xFF0B; inside a collapsed group to expand it. Click a module node to navigate to its documentation.</p>
<div id="dependency-graph-cytoscape"></div>
</div>
<script src="https://unpkg.com/cytoscape@latest/dist/cytoscape.min.js"></script>
<script src="https://unpkg.com/dagre@0.8.5/dist/dagre.min.js"></script>
<script src="https://unpkg.com/cytoscape-dagre@2.5.0/cytoscape-dagre.js"></script>
<script>
(function () {
  var dagreLayout = {
    name: 'dagre',
    nodeSep: 20,
    edgeSep: 10,
    rankSep: 50,
    padding: 20,
    fit: false,
    animate: false
  };

  var boot = function () {
    var container = document.getElementById("dependency-graph-cytoscape");
    if (!container || typeof cytoscape === "undefined") return;

    var cy = cytoscape({
      container: container,
      elements: %s,
      boxSelectionEnabled: false,
      autoungrabify: false,
      wheelSensitivity: 0.15,
      style: [
        {
          selector: '.hidden',
          style: { 'display': 'none' }
        },
        {
          selector: 'node',
          style: {
            'label': 'data(name)',
            'background-color': '#2f6f9f',
            'color': '#ffffff',
            'text-wrap': 'wrap',
            'text-max-width': 120,
            'text-valign': 'center',
            'text-halign': 'center',
            'font-size': 11,
            'shape': 'round-rectangle',
            'padding': '8px'
          }
        },
        {
          selector: ':parent',
          style: {
            'label': 'data(name)',
            'background-color': '#f4efe9',
            'background-opacity': 0.35,
            'border-color': '#c9b6a9',
            'border-width': 2,
            'color': '#4a3b32',
            'text-valign': 'top',
            'text-halign': 'center',
            'font-size': 12,
            'padding': '18px',
            'cursor': 'pointer'
          }
        },
        {
          /* '+' expand button node inside each cluster */
          selector: 'node[name="+"]',
          style: {
            'background-color': '#5a8a5a',
            'color': '#ffffff',
            'font-size': 16,
            'font-weight': 'bold',
            'width': 26,
            'height': 26,
            'shape': 'ellipse',
            'padding': 0,
            'cursor': 'pointer'
          }
        },
        {
          selector: 'edge',
          style: {
            'curve-style': 'bezier',
            'width': 2,
            'line-color': '#8b8b8b',
            'target-arrow-color': '#8b8b8b',
            'target-arrow-shape': 'triangle'
          }
        },
        {
          selector: 'node:selected',
          style: {
            'background-color': '#b85042',
            'border-width': 3,
            'border-color': '#5e201a'
          }
        }
      ],
      layout: dagreLayout
    });

    /* ---- initialise + nodes as hidden (after dagre has placed everything) ---- */
    cy.nodes().forEach(function (n) {
      if (n.data('name') === '+') {
        n.addClass('hidden');
        n.relativePosition({ x: 0, y: 0 });
      }
    });
    cy.fit(undefined, 30);

    /* ---- collapse / expand helpers ---- */
    var collapseToggle = function (parent) {
      parent.children().forEach(function (child) {
        child.toggleClass('hidden');
        if (child.data('name') === '+' && !child.hasClass('hidden')) {
          child.relativePosition({ x: 0, y: 0 });
        }
      });
    };

    var collapseAll = function () {
      cy.nodes().forEach(function (n) {
        if (n.isParent()) {
          n.children().forEach(function (child) {
            if (child.data('name') === '+') {
              child.removeClass('hidden');
              child.relativePosition({ x: 0, y: 0 });
            } else {
              child.addClass('hidden');
            }
          });
        }
      });
      cy.layout(dagreLayout).run();
      cy.fit(undefined, 30);
    };

    var expandAll = function () {
      cy.nodes().forEach(function (n) {
        if (n.data('name') === '+') {
          n.addClass('hidden');
          n.relativePosition({ x: 0, y: 0 });
        } else {
          n.removeClass('hidden');
        }
      });
      cy.layout(dagreLayout).run();
      cy.fit(undefined, 30);
    };

    /* ---- toolbar wiring ---- */
    var section = document.getElementById("rocqnavi-graph-section");
    var fsBtn   = document.getElementById("graph-btn-fs");
    var toolbar = section ? section.querySelector(".rocqnavi-graph-toolbar") : null;
    var heading = section ? section.querySelector("h2") : null;

    var dimensionInPixels = function (value) {
      var n = parseFloat(value);
      return Number.isFinite(n) ? n : 0;
    };
    var elementOuterHeight = function (elt) {
      if (!elt) return 0;
      var style = window.getComputedStyle(elt);
      var margins = dimensionInPixels(style.marginTop) + dimensionInPixels(style.marginBottom);
      return elt.getBoundingClientRect().height + margins;
    };
    var updateViewportSize = function (refit) {
      var inFs = (document.fullscreenElement === section) || (document.webkitFullscreenElement === section);
      if (inFs) {
        var hint = section ? section.querySelector(".rocqnavi-graph-hint") : null;
        var available = window.innerHeight
          - elementOuterHeight(heading)
          - elementOuterHeight(toolbar)
          - elementOuterHeight(hint);
        container.style.height = Math.max(220, Math.floor(available)) + "px";
      } else {
        container.style.height = "";
      }
      cy.resize();
      if (refit) cy.fit(undefined, 30);
    };

    document.getElementById("graph-btn-zoom-in").addEventListener("click", function () {
      cy.zoom({ level: cy.zoom() * 1.3, renderedPosition: { x: container.offsetWidth / 2, y: container.offsetHeight / 2 } });
    });
    document.getElementById("graph-btn-zoom-out").addEventListener("click", function () {
      cy.zoom({ level: cy.zoom() / 1.3, renderedPosition: { x: container.offsetWidth / 2, y: container.offsetHeight / 2 } });
    });
    document.getElementById("graph-btn-fit").addEventListener("click", function () {
      cy.fit(undefined, 30);
    });
    document.getElementById("graph-btn-collapse-all").addEventListener("click", collapseAll);
    document.getElementById("graph-btn-expand-all").addEventListener("click", expandAll);

    fsBtn.addEventListener("click", function () {
      if (!document.fullscreenElement && !document.webkitFullscreenElement) {
        (section.requestFullscreen || section.webkitRequestFullscreen).call(section);
      } else {
        (document.exitFullscreen || document.webkitExitFullscreen).call(document);
      }
    });
    var onFsChange = function () {
      var inFs = !!(document.fullscreenElement || document.webkitFullscreenElement);
      fsBtn.innerHTML = inFs ? "&#x2715; Exit full screen" : "&#x26F6; Full screen";
      updateViewportSize(true);
    };
    document.addEventListener("fullscreenchange",       onFsChange);
    document.addEventListener("webkitfullscreenchange", onFsChange);
    window.addEventListener("resize", function () { updateViewportSize(false); });

    /* ---- node interactions ---- */
    cy.on('tap', 'node', function (evt) {
      var node = evt.target;
      if (node.data('name') === '+') {
        /* expand collapsed parent */
        collapseToggle(node.parent());
      } else if (node.isParent()) {
        /* collapse/expand the group */
        collapseToggle(node);
      } else {
        var url = node.data('url');
        if (url) window.location.href = url;
      }
    });

    updateViewportSize(true);
  };

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', boot, { once: true });
  } else {
    boot();
  }
})();
</script>
|} elements

(*
 * generate index.html
 *)
let generate_topfile ?repo_root output_dir all_files xrefs title xref_table
  directory_mapping hierarchy_graph_dot_file file_graph_input
  file_graph_renderer =

  let hierarchy_graph =
    if hierarchy_graph_dot_file = "" then "" else
      generate_hierarchy_graph title xref_table output_dir hierarchy_graph_dot_file
  in
  let file_graph =
     match file_graph_input with
     | None -> ""
     | Some (File_graph.FromDotFile dot) ->
       begin match file_graph_renderer with
       | File_graph.Graphviz ->
         generate_dependency_graph_from_dot output_dir (Graphviz.from_file dot)
       | File_graph.Cytoscape ->
         Log.warn "Cytoscape file graph rendering currently supports only -file-graph-from-depend; falling back to Graphviz for -file-graph.";
         generate_dependency_graph_from_dot output_dir (Graphviz.from_file dot)
       end
     | Some (File_graph.FromDependFile dep) ->
       let graph = File_graph.parse_dep_file directory_mapping dep in
       begin match file_graph_renderer with
       | File_graph.Graphviz ->
         generate_dependency_graph_from_dot output_dir (File_graph.to_graphviz graph)
       | File_graph.Cytoscape ->
         generate_dependency_graph_cytoscape output_dir graph
       end
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
  title directory_mapping hierarchy_graph_dot_file file_graph_input
  file_graph_renderer index_blacklist =
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
    directory_mapping hierarchy_graph_dot_file file_graph_input
    file_graph_renderer
