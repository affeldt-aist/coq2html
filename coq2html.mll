(* *********************************************************************)
(*                                                                     *)
(*              The Coq2HTML documentation generator                   *)
(*                                                                     *)
(*                   Xavier Leroy, INRIA Paris                         *)
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
let xref_table : (string * int, range * xref) Hashtbl.t = Hashtbl.create 273

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

let add_reference curmod pos_from pos_to dp sp id ty =
  (*eprintf "add_reference %s %d %s %s %s %s\n" curmod pos dp sp id ty;*)
  let range = (pos_from, pos_to) in
  if not (Hashtbl.mem xref_table (curmod, pos_from))
  then Hashtbl.add xref_table (curmod, pos_from) (range, Ref(dp, path sp id, ty))

let add_definition curmod pos_from pos_to sp id ty =
  (*eprintf "add_definition %s %d %s %s %s\n" curmod pos sp id ty;*)
  let range = (pos_from, pos_to) in
  match Hashtbl.find_opt xref_table (curmod, pos_from) with
  | None ->
     Hashtbl.add xref_table (curmod, pos_from) (range, Defs [path sp id, ty])
  | Some (range0, Defs defs) ->
    if range <> range0 then eprintf "Warning: different pathes which have same starting position exists: module '%s', '%s' [%d:%d]\n" curmod (path sp id) pos_from pos_to;
     Hashtbl.replace xref_table (curmod, pos_from) (range, Defs ((path sp id, ty) :: defs))
  | Some (_, Ref (unit, path_, typ)) ->
     (* ignore references if the glob file has a reference and definitions at a
        same position.
        issue: https://github.com/yoshihiro503/coq2html/issues/2
      *)
     Hashtbl.add xref_table (curmod, pos_from) (range, Defs [path sp id, ty])

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

let directory_mappings : (string list * string) list ref = ref []

let add_directory_mapping physical_dir path =
  let physical_dir =
    if physical_dir = "." then []
    else String.split_on_char '/' physical_dir
  in
  directory_mappings := (physical_dir, path) :: !directory_mappings

let list_take n xs =
  let rec iter store = function
    | (n, _) when n <= 0 -> List.rev store
    | (n, []) -> List.rev store
    | (n, x :: xs) -> iter (x :: store) (n - 1, xs)
  in
  iter [] (n, xs)

let list_drop n xs =
  let rec iter = function
    | (n, xs) when n <= 0 -> xs
    | (n, []) -> []
    | (n, _ :: xs) -> iter (n - 1, xs)
  in
  iter (n, xs)

let list_max_by measure xs =
  match xs with
  | [] -> None
  | x0 :: xs ->
     List.fold_left (fun (m, y) x -> if measure x > m then (measure x, x) else (m, y))
       (measure x0, x0) xs
     |> snd
     |> Option.some

let find_directory_mapping physical_path =
  let is_prefix prefix =
    list_take (List.length prefix) physical_path = prefix
  in
  List.filter_map (fun (dir, path) ->
      if is_prefix dir then Some (dir, path) else None)
    !directory_mappings
  |> list_max_by (fun (dir, _) -> List.length dir)

let module_name_of_file_name f =
(*  let concat f =
    String.split_on_char '/' f
    |> List.filter (fun s -> s <> "." && s <> "..")
    |> String.concat "."
  in*)
  let file_path = String.split_on_char '/' f |> List.filter ((<>) ".") in
  match find_directory_mapping file_path with
  | Some (physical_dir, path) ->
     path :: list_drop (List.length physical_dir) file_path
     |> String.concat "."
  | None -> String.concat "." file_path

(* Produce a HTML link if possible *)

type link = Link of int * string | Anchors of int * string list | Nolink of int option

let find_pos xref_table (m, pos) =
  match Hashtbl.find_opt xref_table (m, pos) with
  | Some res -> Some res
  | None ->
    begin match Seq.find (fun (_, (range, _)) -> Range.in_ pos range) (Hashtbl.to_seq xref_table) with
      | None -> None
      | Some (_, res) -> Some res
    end

let crossref m pos max_pos =
(*  eprintf "crossref %s %d\n" m pos;*)
  match find_pos xref_table (m, pos) with
  | Some (_range, Defs [(path, "not")]) ->
    let pos' = pos + String.length path in
    Anchors (pos', [sanitize_linkname path])
  | Some (range, Defs defs) ->
    Anchors (snd range + 1, List.map (fun (path, _) -> sanitize_linkname path) defs)
  | Some (range, Ref(m', p, _)) ->
      let url = url_for_module m' in
      if p = "" then
        Link (snd range + 1, url)
      else
        Link (snd range + 1, url ^ "#" ^ (sanitize_linkname p))
  | None ->
    let rec search_next pos =
      if pos > max_pos then None
      else if Hashtbl.find_opt xref_table (m, pos) = None then
        search_next (pos + 1)
      else Some pos
    in
    Nolink (search_next pos)

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
  "using";
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

let  set_enum_depth d =
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
  sprintf {|%s<a name="%s" class="%s">%s</a>%s|} opens id0 classes
    (escaped text) closes

let is_gallina_keyword id =
  StringSet.find_opt id coq_gallina_keywords

let is_vernacular id =
  StringSet.to_seq coq_vernaculars
  |> Seq.find (fun key -> String.starts_with ~prefix:key id)

let ident_partial pos id =
  let name pos' id =
    if pos' - pos > String.length id then id
    else String.sub id 0 (pos' - pos)
  in
  match is_gallina_keyword (String.trim id), is_vernacular (String.trim id) with
  | Some keyword, _ ->
    let tags = sprintf "<span class=\"gallina-kwd\">%s</span>" (escaped id) in
    (pos + String.length id, tags)
  | None, Some vernac ->
    let tags = sprintf "<span class=\"vernacular\">%s</span>" (escaped id) in
    (pos + String.length vernac, tags)
  | None, None ->
    let max_pos = pos + String.length id in
    match crossref !current_module pos max_pos with
    | Nolink None ->
(*      eprintf "   Nolink '%s'\n" id; *)
      pos, sprintf "<span class=\"id\">%s</span>" (escaped id)
    | Nolink (Some pos') ->
(*      eprintf "   Nolink '%s'\n" (name pos' id); *)
      pos', sprintf "<span class=\"id\">%s</span>" (escaped (name pos' id))
    | Link (pos', p) ->
(*      eprintf "   Link '%s'\n" (name pos' id); *)
      pos', sprintf "<span class=\"id\"><a href=\"%s\">%s</a></span>" p (escaped (name pos' id))
    | Anchors (pos', ps) ->
(*      eprintf "   Anchors '%s'\n" (name pos' id); *)
       let classes =
         if StringSet.mem id mathcomp_hierarchy_builders then
           "hierarchy-builder" else ""
       in
       pos', nested_ids_anchor classes ps (name pos' id)

let idents pos id =
(*  eprintf "idents: %d '%s'\n" pos id;*)
  let rec iter pos id =
    if id = "" then () else begin
      let (pos', tags) = ident_partial pos id in
      fprintf !oc "%s" tags;
      let rpos' = pos' - pos in
      if pos' <= pos then begin
        iter pos' ""
      end else if rpos' > String.length id then begin
        iter pos' ""
      end else
      let id' = String.sub id rpos' (String.length id - rpos') in
      iter pos' id'
    end
  in
  iter pos id

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

let in_proof = ref false
let proof_counter = ref 0

let start_proof s kwd =
  in_proof := true;
  incr proof_counter;
  fprintf !oc "<div>";
  space s;
  fprintf !oc
    "<span class=\"toggleproof\" onclick=\"toggleDisplay('proof%d')\">%s</span></div>\n"
    !proof_counter
    kwd;
  fprintf !oc "<div class=\"proofscript\" id=\"proof%d\">\n" !proof_counter

let end_proof spaces kwd =
  fprintf !oc "%s%s</div>\n" spaces kwd;
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
let ident = ['A'-'Z' 'a'-'z' '_'] ['A'-'Z' 'a'-'z' '0'-'9' '_']*
let path = ident ("." ident)*
let start_proof = ("Proof" space* ".") | ("Proof" space+ "with") | ("Next" space+ "Obligation.")
let end_proof = "Qed." | "Defined." | "Save." | "Admitted." | "Abort."
let quoted = ['\"'] [' '-'~']* ['\"']
let symbol = ['!' '#'-'\'' '*'-'/' ':'-'@' '['-'`' '{'-'~'] (*'"', '(', ')' *)
let non_whites = (['A'-'Z' 'a'-'z' '0'-'9'] | symbol)+

let xref = ['A'-'Z' 'a'-'z' '0'-'9' '!' '#'-'~']+ | "<>"
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
  | space* ("(***" "*"+ "***)" "\n")
      { fprintf !oc "<pre class=\"ssrdoc\">\n";
        ssr_doc_bol lexbuf;
	fprintf !oc "%s" "</pre>\n";
	skip_newline lexbuf
      }
  (* Enter ssrdoc with special syntax mode e.g. markdown syntax *)
  | space* ("(**" (['a'-'z' '-']+ as mode) "*"+ "***)" "\n")
      { fprintf !oc "<div class=\"ssrdoc %s\">\n" mode;
        ssr_doc_bol lexbuf;
	fprintf !oc "%s" "</div>\n";
	skip_newline lexbuf
      }
  | space* ("(***" (['a'-'z' '-']+ as mode) "*"+ "***)" "\n")
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
  | (space* as s) (end_proof as ep)
      { if !in_proof then end_proof s ep;
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
(*  | path as id
      { ident (Lexing.lexeme_start lexbuf) id; coq lexbuf }*)
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
      {idents (Lexing.lexeme_start lexbuf) q; coq lexbuf}
  | (' '? non_whites+ as id)
      {(*output_char !oc ' ';*)
       (* special hack:
          The references of notations in glob file sometime include white space.
          c.f. https://coq.zulipchat.com/#narrow/stream/237656-Coq-devs-.26-plugin-devs/topic/Bug.3F.3A.20position.20of.20reference.20of.20notations.20in.20glob.20file/near/406709205
        *)
(*       let pos' =
         let pos = Lexing.lexeme_start lexbuf in
         match crossref !current_module pos with
         | Nolink -> pos + 1
         | _ -> pos
         in*)

       idents (Lexing.lexeme_start lexbuf) id; coq lexbuf}
(*  | non_whites as id
      {idents (Lexing.lexeme_start lexbuf) id; coq lexbuf}*)
  | _ as c
      { character c; coq lexbuf }

and bracket = parse
  | ']'
      { () }
  | '['
      { character '['; bracket lexbuf; character ']'; bracket lexbuf }
  | path as id
      { idents (Lexing.lexeme_start lexbuf) id; bracket lexbuf }
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
  | space* "*)"
      { () }
  | eof
      { () }
  | _ as c
      { character c; custom_mode lexbuf }

(* beginning of line *)
and ssr_doc_bol = parse
  (* Leave verbatim mode *)
  | space* ("(***" "*"+ "***)")
      { () }
  | "(* "
      { ssr_doc_bol lexbuf }
  | "\n"
      { ssr_doc_bol lexbuf }
  | ""
      { ssr_doc lexbuf }

and ssr_doc = parse
  | space* "*)"
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
  | "R" (integer as pos1) ":" (integer as pos2)
    space+ (xref as dp)
    space+ (xref as sp)
    space+ (xref as id)
    space+ (ident as ty)
    space* "\n"
      { add_reference !current_module (int_of_string pos1) (int_of_string pos2)
          dp sp id ty;
        globfile lexbuf }
  | (ident as ty)
    space+ (integer as pos1) ":" (integer as pos2)
    space+ (xref as sp)
    space+ (xref as id)
    space* "\n"
      { add_definition !current_module (int_of_string pos1) (int_of_string pos2)
          sp id ty;
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
  enum_depth := 0; in_proof := false; proof_counter := 0;
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
      "   Use short, unqualified module names in the output";
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
