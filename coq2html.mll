(* *********************************************************************)
(*                                                                     *)
(*              The Coq2HTML documentation generator                   *)
(*                                                                     *)
(*          Xavier Leroy, Collège de France and INRIA Paris            *)
(*                                                                     *)
(*  Copyright Institut National de Recherche en Informatique et en     *)
(*  Automatique.  All rights reserved.  This file is distributed       *)
(*  under the terms of the GNU General Public License as published by  *)
(*  the Free Software Foundation, either version 2 of the License, or  *)
(*  (at your option) any later version.                                *)
(*                                                                     *)
(* *********************************************************************)

{
open Printf
open Generate_index

(** Cross-referencing *)

let current_module = ref ""

(* Record cross-references found in .glob files *)

(* (name of module, character position in file) -> cross-reference *)
let xref_table : (string * int, xref) Hashtbl.t = Hashtbl.create 273

(* Records all module names for which a .glob file is given *)
let xref_modules : (string, unit) Hashtbl.t = Hashtbl.create 29

let path sp id =
  match sp, id with
  | "<>", "<>" -> ""
  | "<>", _    -> id
  | _   , "<>" -> sp
  | _   , _    -> sp ^ "." ^ id

let add_module m =
  (*eprintf "add_module %s\n" m;*)
  Hashtbl.add xref_modules m ()

let add_reference curmod pos dp sp id ty =
  (*eprintf "add_reference %s %d %s %s %s %s\n" curmod pos dp sp id ty;*)
  if not (Hashtbl.mem xref_table (curmod, pos))
  then Hashtbl.add xref_table (curmod, pos) (Ref(dp, path sp id, ty))

let add_definition curmod pos sp id ty =
  (*eprintf "add_definition %s %d %s %s %s\n" curmod pos sp id ty;*)
  match Hashtbl.find_opt xref_table (curmod, pos) with
  | None ->
     Hashtbl.add xref_table (curmod, pos) (Defs [path sp id, ty])
  | Some (Defs defs) ->
     Hashtbl.replace xref_table (curmod, pos) (Defs ((path sp id, ty) :: defs))
  | Some (Ref (unit, path_, typ)) ->
     (* ignore references if the glob file has a reference and definitions at a
        same position.
        issue: https://github.com/yoshihiro503/coq2html/issues/2
      *)
     Hashtbl.add xref_table (curmod, pos) (Defs [path sp id, ty])

(* Map module names to URLs *)

let coqlib_url = "https://coq.inria.fr/library/"

(* logical name with final '.' -> absolute or relative URL *)
let documentation_urls : (string * string) list ref = ref [("Coq.", coqlib_url)]

let add_documentation_url logicalname url =
  documentation_urls := (logicalname ^ ".", url) :: !documentation_urls

let starts_with s x =
  let ls = String.length s and lx = String.length x in
  ls >= lx && String.sub s 0 lx = x

let ends_with s x =
  let ls = String.length s and lx = String.length x in
  ls >= lx && String.sub s (ls - lx) lx = x

let url_concat url suff =
  (if ends_with url "/" then url else url ^ "/") ^ suff

let url_for_module m =
  (*eprintf "url_for_module %s\n" m;*)
  let rec url_for = function
  | [] ->
      if Hashtbl.mem xref_modules m then m ^ ".html" else ("NOTFOUND the module url for "^m)
  | (pref, url) :: rem ->
      if starts_with m pref then url_concat url m ^ ".html" else url_for rem
  in url_for !documentation_urls

let directory_mappings : (string * string) list ref = ref []

let add_directory_mapping physical_dir path =
  let physical_dir = if physical_dir = "." then "" else physical_dir in
  directory_mappings := (physical_dir, path) :: !directory_mappings

let find_directory_mapping physical_path =
  List.filter_map (fun (dir, path) ->
      if starts_with physical_path dir then Some (dir, path) else None)
    !directory_mappings
  |> List.sort (fun (x,_) (y,_) -> compare (String.length x) (String.length y))
  |> List.rev
  |> function
    | [] -> None
    | dir :: _ -> Some dir



(*let module_name_of_file_name f =
  let concat f = Str.(split (regexp "/")) f
                 |> List.filter (fun s -> s <> "." && s <> "..")
                 |> String.concat "."
  in
  match List.find_opt (fun (dir, _) -> starts_with f dir) !directory_mappings with
  | None -> concat f
  | Some (physical_dir, path) ->
     Str.(replace_first (regexp_string physical_dir)) path f
     |> concat
*)
let module_name_of_file_name f =
  let concat f = Str.(split (regexp "/")) f
                 |> List.filter (fun s -> s <> "." && s <> "..")
                 |> String.concat "."
  in
  match find_directory_mapping f with
  | Some (physical_dir, path) ->
     Str.(replace_first (regexp_string physical_dir)) path f
     |> concat
  | None -> concat f

(* Produce a HTML link if possible *)

type link = Link of string | Anchors of string list | Nolink

let re_sane_path = Str.regexp "[A-Za-z0-9_.\x80-\xFF]+$"

let crossref m pos =
  (*eprintf "crossref %s %d\n" m pos;*)
  try match Hashtbl.find xref_table (m, pos) with
  | Defs defs ->
      Anchors (List.map (fun (path, _) -> sanitize_linkname path) defs)
  | Ref(m', p, _) ->
      let url = url_for_module m' in  (* can raise Not_found *)
      if p = "" then
        Link url
      else
        Link(url ^ "#" ^ (sanitize_linkname p))
  with Not_found ->
    Nolink

(** Keywords *)

module StringSet = Set.Make(String)

let mkset l = List.fold_right StringSet.add l StringSet.empty

let coq_vernaculars = mkset [
(* "The following character sequences are keywords defined in the
    main Coq grammar that cannot be used as identifiers"
    (reference manual) *)
  "Axiom"; "CoFixpoint"; "Definition"; "Fixpoint"; "Hypothesis";
  "Parameter"; "Theorem"; "Variable";
(* The commands from the "Command Index" part of the reference manual.
   Some commands I don't expect to see in .v files were removed. *)
  "Abort"; "About"; "Admitted"; "Arguments"; "Axiom"; "Axioms";
  "Canonical"; "Cd"; "Check"; "Class"; "Close"; "CoFixpoint";
  "CoInductive"; "Coercion"; "Collection"; "Combined"; "Comments";
  "Compute"; "Conjecture"; "Conjectures"; "Constraint"; "Context";
  "Corollary"; "Defined"; "Definition"; "End"; "Eval"; "Example";
  "Existential"; "Export"; "Fact"; "Fail"; "Fixpoint"; "Focus"; "From";
  "Function"; "Functional"; "Goal"; "Hint"; "Hypotheses"; "Hypothesis";
  "Implicit"; "Import"; "Include"; "Inductive"; "Infix"; "Instance";
  "Lemma"; "Let"; "Ltac"; "Ltac2"; "Module"; "Next"; "Notation";
  "Obligation"; "Obligations"; "Opaque"; "Open"; "Parameter";
  "Parameters"; "Proof"; "Proposition"; "Qed"; "Record"; "Remark";
  "Require"; "Reserved"; "Scheme"; "Scope"; "Section";
  "Strategy"; "Structure"; "SubClass"; "Tactic"; "Theorem";
  "Transparent"; "Universe"; "Variable"; "Variables"; "Variant";
  "Unset"; "Strict"; "Printing"; "Defensive"; "Number"; "Declare";
  "Delimit"; "Bind"; "Local";
]

let coq_gallina_keywords = mkset [
  "Prop"; "SProp"; "Set"; "Type";
  "as"; "at"; "cofix"; "else"; "end"; "fix"; "for"; "forall"; "fun";
  "if"; "in"; "let"; "match"; "return"; "then"; "where"; "with";
  "using"; "with";
(* "The following are keywords defined in notations or plugins
    loaded in the prelude" (reference manual) *)
  "IF"; "by"; "exists"; "exists2"; "using";
]

let mathcomp_hierarchy_builders = mkset [
  "HB.check"; "HB.locate"; "HB.about"; "HB.howto";
  "HB.status"; "HB.graph"; "HB.mixin"; "HB.structure";
  "HB.saturate"; "HB.instance"; "HB.factory"; "HB.builders";
  "HB.end"; "HB.export"; "HB.reexport"; "HB.declare";
]

(** HTML generation *)

let oc = ref stdout

let character = function
  | '<' -> output_string !oc "&lt;"
  | '>' -> output_string !oc "&gt;"
  | '&' -> output_string !oc "&amp;"
  |  c  -> output_char !oc c

let section_level = function
  | "*" -> 1
  | "**" -> 2
  | _ -> 3

let start_section sect =
  fprintf !oc "<h%d>" (section_level sect)
let end_section sect =
  fprintf !oc "</h%d>\n" (section_level sect)

let start_doc_right () =
  fprintf !oc "<span class=\"docright\">(* "
let end_doc_right () =
  fprintf !oc " *)</span>"

let enum_depth = ref 0

let set_enum_depth d =
  if !enum_depth < d then begin
    fprintf !oc "<ul>\n";
    fprintf !oc "<li>\n";
    incr enum_depth;
  end
  else if !enum_depth > d then begin
    fprintf !oc "</li>\n";
    fprintf !oc "</ul>\n";
    decr enum_depth;
  end
  else if !enum_depth > 0 then begin
    fprintf !oc "</li>\n";
    fprintf !oc "<li>\n"
  end

let start_doc () =
  fprintf !oc "<div class=\"doc\">"
let end_doc () =
  set_enum_depth 0;
  fprintf !oc "</div>\n"

let nested_ids_anchor classes ids text =
  let id0 = List.hd ids in
  let opens =
    List.map (fun id ->sprintf "<span id=\"%s\" class=\"id\">"id ) ids
    |> String.concat ""
  in
  let closes = List.map (fun _ -> "</span>") ids |> String.concat "" in
  fprintf !oc {|%s<a name="%s" class="%s">%s</a>%s|} opens id0 classes
    text closes

let ident pos id =
  if StringSet.mem id coq_gallina_keywords then
    fprintf !oc "<span class=\"gallina-kwd\">%s</span>" id
  else if id = "_" then
    fprintf !oc "_"
  else if StringSet.mem id coq_vernaculars then
    fprintf !oc "<span class=\"vernacular\">%s</span>" id
  else match crossref !current_module pos with
  | Nolink ->
      fprintf !oc "<span class=\"id\">%s</span>" id
  | Link p ->
      fprintf !oc "<span class=\"id\"><a href=\"%s\">%s</a></span>" p id
  | Anchors ps ->
       let classes =
         if StringSet.mem id mathcomp_hierarchy_builders then
           "hierarchy-builder" else ""
       in
       nested_ids_anchor classes ps id

let space s =
  for _ = 1 to String.length s do fprintf !oc "&nbsp;" done

let newline () =
  fprintf !oc "<br/>\n"

let dashes = function
  | "-" -> set_enum_depth 1
  | "--" -> set_enum_depth 2
  | "---" -> set_enum_depth 3
  | "----" -> set_enum_depth 4
  | _ -> fprintf !oc "<hr/>\n"

let start_verbatim () =
  fprintf !oc "<pre>\n"

let end_verbatim () =
  fprintf !oc "</pre>\n"

let start_comment () =
  fprintf !oc "<span class=\"comment\">(*"

let end_comment () =
  fprintf !oc "*)</span>"

let start_bracket () =
  fprintf !oc "<span class=\"bracket\">"

let end_bracket () =
  fprintf !oc "</span>"

let start_string () =
  fprintf !oc  "<span class=\"string\">\""

let end_string () =
  fprintf !oc  "\"</span>"

let in_proof = ref false

let start_proof s kwd =
  fprintf !oc "<details>\n";
  space s;
  fprintf !oc "<summary class=\"toggleproof\">%s</summary>\n" kwd;
  fprintf !oc "<div class=\"proofscript\">\n"

let end_proof kwd =
  fprintf !oc "%s</div></details>\n" kwd;
  in_proof := false

(* Like Str.global_replace but don't interpret '\1' etc in replacement text *)
let global_replace re subst txt =
  Str.global_substitute re (fun _ -> subst) txt

let start_html_page modname all_files =
  global_replace (Str.regexp "\\$NAME") modname Resources.header
  |> global_replace (Str.regexp_string "$FILES") (sidebar_files all_files)
  |> output_string !oc

let end_html_page () =
  output_string !oc Resources.footer

}

let space = [' ' '\t']
let utf8 = ['\192'-'\255'] ['\128'-'\191']*
let identstart = ['A'-'Z' 'a'-'z' '_'] | utf8
let identnext  = ['A'-'Z' 'a'-'z' '_'  '0'-'9' '\''] | utf8
let ident = identstart identnext*
let path = ident ("." ident)*
let start_proof = ("Proof" space* ".") | ("Proof" space+ "with") | ("Next" space+ "Obligation.")
let end_proof = "Qed." | "Defined." | "Save." | "Admitted." | "Abort."
let globkind = ['a'-'z']+

let quoted = ['\"'] [' '-'~']* ['\"']
let symbol = ['!' '#' '$' '*' '\'' '*'-'-' '/' ':'-'?' '['-'`' '{'-'~'] (*'"', '%', '.', '(', ')' *)
let non_whites = (['A'-'Z' 'a'-'z' '0'-'9'] | symbol)+

let xref = (['A'-'Z' 'a'-'z' '0'-'9' '#'-'~'] | utf8)+ | "<>"

let integer = ['0'-'9']+

rule coq_bol = parse
  | (space* as s) (start_proof as sp)
      { start_proof s sp;
        skip_newline lexbuf }
  (* Enter special syntax mode e.g. markdown syntax *)
  | space* "(**" (['a'-'z' '-']+ as mode)
      { fprintf !oc "<div class=\"doc %s\">" mode;
        custom_mode lexbuf;
        end_doc();
        skip_newline lexbuf }
  | space* "(** " ("*"+ as sect)
      { start_section sect;
        doc lexbuf;
        end_section sect;
        skip_newline lexbuf }
  | space* "(** "
      { start_doc();
        doc lexbuf;
        end_doc();
        skip_newline lexbuf }
  | (space* as s) "(*"
      { if !in_proof then (space s; start_comment());
	comment lexbuf;
        if !in_proof then coq lexbuf else skip_newline lexbuf }
  (* Enter verbatim mode *)
  | space* ("(***" "*"+ "***)" "\n" as s)
      { fprintf !oc "<pre class=\"ssrdoc\">\n";
        ssr_doc_bol lexbuf;
	fprintf !oc "%s" "</pre>\n";
	skip_newline lexbuf
      }
  (* Enter ssrdoc with special syntax mode e.g. markdown syntax *)
  | space* ("(**" (['a'-'z' '-']+ as mode) "*"+ "***)" "\n" as s)
      { fprintf !oc "<div class=\"ssrdoc %s\">\n" mode;
        ssr_doc_bol lexbuf;
	fprintf !oc "%s" "</div>\n";
	skip_newline lexbuf
      }
  | space* ("(***" (['a'-'z' '-']+ as mode) "*"+ "***)" "\n" as s)
      { fprintf !oc "<div class=\"ssrdoc %s\">\n" mode;
        ssr_doc_bol lexbuf;
	fprintf !oc "%s" "</div>\n";
	skip_newline lexbuf
      }
  | eof
      { () }
  | space* as s
      { space s;
        coq lexbuf }

and skip_newline = parse
  | space* "\n"
      { coq_bol lexbuf }
  | ""
      { coq lexbuf }

and coq = parse
  | end_proof as ep
      { end_proof ep;
        skip_newline lexbuf }
  | "(**r "
      { start_doc_right();
        doc lexbuf;
        end_doc_right();
        coq lexbuf }
  | "(*"
      { if !in_proof then start_comment();
        comment lexbuf;
        coq lexbuf }
  | path as id
      { ident (Lexing.lexeme_start lexbuf) id; coq lexbuf }
  | "\""
      { start_string();
        string lexbuf;
        end_string();
        coq lexbuf }
  | (". ") (space* as s) (start_proof as sp)
      { newline();
        start_proof s sp;
	skip_newline lexbuf ;
        coq lexbuf }
  | "\n"
      { newline(); coq_bol lexbuf }
  | eof
      { () }
  | quoted as q
      {ident (Lexing.lexeme_start lexbuf) (escaped q); coq lexbuf}
  | ' ' (symbol non_whites* as id)
      {output_char !oc ' ';
       (* special hack:
          The references of notations in glob file sometime include white space.
          c.f. https://coq.zulipchat.com/#narrow/stream/237656-Coq-devs-.26-plugin-devs/topic/Bug.3F.3A.20position.20of.20reference.20of.20notations.20in.20glob.20file/near/406709205
        *)
       let pos' =
         let pos = Lexing.lexeme_start lexbuf in
         match crossref !current_module pos with
         | Nolink -> pos + 1
         | _ -> pos
       in
       ident pos' (escaped id); coq lexbuf}
  | non_whites as id
      {ident (Lexing.lexeme_start lexbuf) (escaped id); coq lexbuf}
  | _ as c
      { character c; coq lexbuf }

and string = parse
  | "\"\""
      { character '\"'; character '\"'; string lexbuf }
  | "\""
      { () }
  | eof
      { () }
  | _ as c
      { character c; string lexbuf }

and bracket = parse
  | ']'
      { () }
  | '['
      { character '['; bracket lexbuf; character ']'; bracket lexbuf }
  | path as id
      { ident (Lexing.lexeme_start lexbuf) id; bracket lexbuf }
  | "\""
      { start_string();
        string lexbuf;
        end_string();
        coq lexbuf }
  | eof
      { () }
  | _ as c
      { character c; bracket lexbuf }

and comment = parse
  | "*)"
      { if !in_proof then end_comment() }
  | "(*"
      { if !in_proof then start_comment();
        comment lexbuf; comment lexbuf }
  | eof
      { () }
  | "\n"
      { if !in_proof then newline();
        comment lexbuf }
  | space* as s
      { if !in_proof then space s;
        comment lexbuf }
  | eof
      { () }
  | _ as c
      { if !in_proof then character c;
        comment lexbuf }

and doc_bol = parse
  | "<<" space* "\n"
      { start_verbatim();
        verbatim lexbuf;
        end_verbatim();
        doc_bol lexbuf }
  | "-"+ as d
      { dashes d; doc lexbuf }
  | "\n"
      { set_enum_depth 0; doc_bol lexbuf }
  | ""
      { doc lexbuf }

and doc = parse
  | "*)"
      { () }
  | "\n"
      { character '\n'; doc_bol lexbuf }
  | "["
      { start_bracket(); bracket lexbuf; end_bracket(); doc lexbuf }
  | "#" ([^ '\n' '#']* as html) "#"
      { output_string !oc html; doc lexbuf }
  | eof
      { () }
  | _ as c
      { character c; doc lexbuf }

and custom_mode = parse
  | "*)"
      { () }
  | eof
      { () }
  | _ as c
      { character c; custom_mode lexbuf }

(* beginning of line *)
and ssr_doc_bol = parse
  (* Leave verbatim mode *)
  | space* ("(***" "*"+ "***)" as s)
      { () }
  | "(* "
      { ssr_doc_bol lexbuf }
  | "\n"
      { ssr_doc_bol lexbuf }
  | ""
      { ssr_doc lexbuf }

and ssr_doc = parse
  | "*)"
      { ssr_doc lexbuf }
  | "\n"
      { character '\n'; ssr_doc_bol lexbuf }
  | eof
      { () }
  | _ as c
      { character c; ssr_doc lexbuf }

and verbatim = parse
  | "\n>>" space* "\n"
      { () }
  | eof
      { () }
  | _ as c
      { character c; verbatim lexbuf }

and globfile = parse
  | eof
      { () }
  | "F" (path as m) space* "\n"
      { current_module := m; add_module m;
        globfile lexbuf }
  | "R" (integer as pos) ":" (integer)
    space+ (xref as dp)
    space+ (xref as sp)
    space+ (xref as id)
    space+ (globkind as ty)
    space* "\n"
      { add_reference !current_module (int_of_string pos) dp sp id ty;
        globfile lexbuf }
  | (globkind as ty)
    space+ (integer as pos) ":" (integer)
    space+ (xref as sp)
    space+ (xref as id)
    space* "\n"
      { add_definition !current_module (int_of_string pos) sp id ty;
        globfile lexbuf }
  | [^ '\n']* "\n"
      { globfile lexbuf }

{

let make_redirect fromfile toURL =
  let oc = open_out fromfile in
  output_string oc
    (global_replace (Str.regexp "\\$URL") toURL Resources.redirect);
  close_out oc

let default_title () = Filename.basename @@ Sys.getcwd ()

let title = ref (default_title ())
let output_dir = ref Filename.current_dir_name
let logical_name_base = ref ""
let generate_css = ref true
let use_short_names = ref false
let generate_redirects = ref false

let process_v_file all_files f =
  let pref_f = Filename.chop_suffix f ".v" in
  let base_f = Filename.basename pref_f in
  let module_name = !logical_name_base ^ module_name_of_file_name pref_f in
  current_module := module_name;
  let friendly_name = if !use_short_names then base_f else module_name in
  let ic = open_in f in
  oc := open_out (Filename.concat !output_dir (module_name ^ ".html"));
  enum_depth := 0; in_proof := false;
  start_html_page friendly_name all_files;
  coq_bol (Lexing.from_channel ic);
  end_html_page();
  close_out !oc; oc := stdout;
  close_in ic;
  if !generate_redirects && !logical_name_base <> "" then
    make_redirect (Filename.concat !output_dir (base_f ^ ".html"))
                  (module_name ^ ".html")

let process_glob_file f =
  current_module := "";
  let ic = open_in f in
  globfile (Lexing.from_channel ic);
  close_in ic

let write_file txt filename =
  let oc = open_out filename in
  output_string oc txt;
  close_out oc

let _ =
  let v_files = ref [] and glob_files = ref [] in
  let process_file f =
    if Filename.check_suffix f ".v" then
      v_files := f :: !v_files
    else if Filename.check_suffix f ".glob" then
      glob_files := f :: !glob_files
    else begin
      eprintf "Don't know what to do with file %s\n" f; exit 2
    end in
  Arg.parse (Arg.align [
    "-title", Arg.String (fun s -> title := s),
      "<title>  Set the title of the index.html";
    "-base", Arg.String (fun s -> logical_name_base := s ^ "."),
      "<coqdir>  Set the name space for the modules being processed";
    "-coqlib", Arg.String (fun s -> add_documentation_url "Coq" s),
      "<url>   Set base URL for Coq standard library";
    "-d", Arg.Set_string output_dir,
      "<dir>   Output files to directory <dir> (default: current directory)";
    "-Q",
      (let dir = ref "" in
       Arg.Tuple
         [Arg.Set_string dir;
          Arg.String (fun path -> add_directory_mapping !dir path)]),
      "<directory> <dirpath>  Map physical directory to path";
    "-external",
      (let x = ref "" in
       Arg.Tuple [
         Arg.Set_string x;
         Arg.String (fun y -> add_documentation_url y !x)
       ]),
      "<url> <coqdir> Set base URL for linking references whose names start with <coqdir>";
    "-no-css", Arg.Clear generate_css,
      "   Do not add coq2html.css to the output directory";
    "-redirect", Arg.Set generate_redirects,
      "   Generate redirection files modname.html -> coqdir.modname.html";
    "-short-names", Arg.Set use_short_names,
      "   Use short, unqualified module names in the output"
  ])
  process_file
  "Usage: coq2html [options] file.glob ... file.v ...\nOptions are:";
  if !v_files = [] then begin
    eprintf "No .v file provided, aborting\n";
    exit 2
  end;
  if (try not (Sys.is_directory !output_dir) with Sys_error _ -> true)
  then begin
    eprintf "Error: output directory %s does not exist or is not a directory.\n" !output_dir;
    exit 2
  end;
  List.iter process_glob_file (List.rev !glob_files);
  let all_files = Generate_index.all_files xref_modules in
  List.iter (process_v_file all_files) (List.rev !v_files);
  Generate_index.generate !output_dir xref_table xref_modules !title;
  write_file Resources.js (Filename.concat !output_dir "coq2html.js");
  if !generate_css then
    write_file Resources.css (Filename.concat !output_dir "coq2html.css")
}
