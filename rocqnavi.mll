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
open Common
open Generate_index
open Env
module K = Glob_kind

let warn lexbuf message =
  let open Lexing in
  let position = lexbuf.lex_curr_p in
  Log.warn
    (!%"File: %s, line %d, culumn %d: %s" position.pos_fname
       position.pos_lnum (position.pos_cnum - position.pos_bol + 1) message)

(** Cross-referencing *)

let current_module = ref ""

(* Track the vernacular commands being read for when the option to display type
   information is specified. However, this may not be necessary when using LSP.
 *)
let current_command = ref ""
let proceed_current_command s =
  current_command := !current_command ^ s
let end_current_command env s =
  proceed_current_command s;
  let is_loading_command s =
    let cmd = String.trim s in
    String.starts_with ~prefix:"Require" cmd
    || String.starts_with ~prefix:"Import" cmd
    || String.starts_with ~prefix:"Export" cmd
    || String.starts_with ~prefix:"From" cmd
  in
  begin match env.type_lookup with
  | Some conn ->
     let cmd = !current_command in
     if is_loading_command cmd then Type_lookup.load cmd conn
  | _ -> ()
  end;
  current_command := ""

(* Record cross-references found in .glob files *)

(* (name of module, character position in file) -> cross-reference *)
let xref_table = ref XrefTable.empty

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
  let tbl = XrefTable.add_reference !xref_table curmod pos_from pos_to dp (path sp id) (Glob_kind.of_string ty) in
  xref_table := tbl

let add_definition curmod pos_from pos_to sp id ty =
  let tbl = XrefTable.add_definition !xref_table curmod pos_from pos_to (path sp id) (Glob_kind.of_string ty) in
  xref_table := tbl

(* Map module names to URLs *)

let default_coqlib_url_coq8 = "https://coq.inria.fr/library/"
let default_stdlib_url_rocq = "https://rocq-prover.org/stdlib/"
let default_corelib_url_rocq = "https://rocq-prover.org/corelib/"

(* logical name with final '.' -> absolute or relative URL *)
let documentation_urls : (string * string) list ref =
  ref [
      ("Coq.",     default_coqlib_url_coq8);
      ("Stdlib.",  default_stdlib_url_rocq);
      ("Corelib.", default_corelib_url_rocq);
    ]

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

let directory_mappings : Directory_mappings.t ref = ref Directory_mappings.empty

let add_directory_mapping physical_dir path =
  directory_mappings := Directory_mappings.add !directory_mappings physical_dir path

let module_name_of_file_name f =
  let file_path = String.split_on_char '/' f |> List.filter ((<>) ".") in
  Directory_mappings.apply !directory_mappings file_path
  |> String.concat "."

(* Produce a HTML link if possible *)

type link = Link of int * string | Anchors of int * (string * Glob_kind.t) list | Nolink of int option

let re_sane_path = Str.regexp "[A-Za-z0-9_.\x80-\xFF]+$"

let find_pos xref_table (m, pos) = XrefTable.find xref_table m pos

let crossref m pos max_pos =
(*  eprintf "crossref %s %d\n" m pos;*)
  match find_pos !xref_table (m, pos) with
  | Some (_range, Defs [(path, Notation)]) ->
    let pos' = pos + String.length path in
    Anchors (pos', [(sanitize_linkname path, Notation)])
  | Some (range, Defs defs) ->
    Anchors (snd range + 1, List.map (fun (path, kind) -> (sanitize_linkname path, kind)) defs)
  | Some (range, Ref(m', p, _)) ->
      let url = url_for_module m' in
      if p = "" then
        Link (snd range + 1, url)
      else
        Link (snd range + 1, url ^ "#" ^ (sanitize_linkname p))
  | None ->
    let rec search_next pos =
      if pos > max_pos then None
      else if find_pos !xref_table (m, pos) = None then
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

(* If the option to show type infomation is enabled, return the type infomation *)
let lookup_type_info conn id loc =
      let position = Lexing.(loc.pos_lnum - 1, loc.pos_cnum - loc.pos_bol + 1) in
      let filename = Lexing.(loc.pos_fname) in
      match Type_lookup.ask_type_info_of id filename position conn with
      | Ok ty -> Some ty
      | Error message -> Log.warn (!%"fail: lookup_type_info '%s'" id);
                         None

let nested_ids_anchor env classes ids text loc =
  let (id0, kind0) = List.hd ids in
  let ids = List.map fst ids in
  let opens =
    List.map (fun id ->sprintf "<span id=\"%s\" class=\"id\">"id ) ids
    |> String.concat ""
  in
  let closes = List.map (fun _ -> "</span>") ids |> String.concat "" in
  let is_black =
    match env.definition_blacklist with
    | None -> false
    | Some list -> Index_blacklist.is_listed list id0
  in
  match env.type_lookup, kind0 with
  | Some conn, K.Definition when is_black = false ->
     let type_information = lookup_type_info conn id0 loc
                            |> Option.value ~default:(Type_lookup.PlainText "")
     in
     let atag = Tooltip.tag_with_tooltip "a" id0 classes type_information text in
     sprintf {|%s%s%s|} opens atag closes
  | _ ->
     sprintf {|%s<a name="%s" class="%s">%s</a>%s|} opens id0 classes
       (html_escaped text) closes

let is_gallina_keyword id =
  StringSet.find_opt id coq_gallina_keywords

let is_vernacular id =
  StringSet.to_seq coq_vernaculars
  |> Seq.find (fun key -> String.starts_with ~prefix:key id)

let ident_partial env pos id loc =
  let name pos' id =
    if pos' - pos > String.length id then id
    else String.sub id 0 (pos' - pos)
  in
  if id = "_" then (pos + 1, "_") else
    let max_pos = pos + String.length id in
    begin match crossref !current_module pos max_pos with
    | Nolink None ->
       begin match
         is_gallina_keyword (String.trim id),
         is_vernacular (String.trim id)
       with
       | Some keyword, _ ->
          let tags = sprintf "<span class=\"gallina-kwd\">%s</span>" (html_escaped id) in
          (pos + String.length id, tags)
       | None, Some vernac ->
          let tags = sprintf "<span class=\"vernacular\">%s</span>" (html_escaped id) in
          (pos + String.length id, tags)
       | None, None ->
(*      eprintf "   Nolink '%s'\n" id; *)
          pos, sprintf "<span class=\"id\">%s</span>" (html_escaped id)
       end
    | Nolink (Some pos') ->
(*      eprintf "   Nolink '%s'\n" (name pos' id); *)
       pos', sprintf "<span class=\"id\">%s</span>" (html_escaped (name pos' id))
    | Link (pos', p) ->
(*      eprintf "   Link '%s'\n" (name pos' id); *)
       pos', sprintf "<span class=\"id\"><a href=\"%s\">%s</a></span>" p (html_escaped (name pos' id))
    | Anchors (pos', ps) ->
(*      eprintf "   Anchors '%s'\n" (name pos' id); *)
       let classes =
         if StringSet.mem id mathcomp_hierarchy_builders then
           "hierarchy-builder" else ""
       in
       pos', nested_ids_anchor env classes ps (name pos' id) loc
    end


let idents env pos id loc =
(*  eprintf "idents: %d '%s'\n" pos id;*)
  let rec iter pos id =
    if id = "" then () else begin
      let (pos', tags) = ident_partial env pos id loc in
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
  fprintf !oc "<code class=\"bracket\">"

let end_bracket () =
  fprintf !oc "</code>"

let start_string () =
  fprintf !oc  "<span class=\"string\">\""

let end_string () =
  fprintf !oc  "\"</span>"

let in_proof = ref false

let start_proof s kwd =
  in_proof := true;
  fprintf !oc "<details>\n";
  space s;
  fprintf !oc "<summary class=\"toggleproof\">%s</summary>\n" kwd;
  fprintf !oc "<div class=\"proofscript\">\n"

let end_proof spaces kwd =
  fprintf !oc "%s%s</div></details>\n" spaces kwd;
  in_proof := false

(* Like Str.global_replace but don't interpret '\1' etc in replacement text *)
let global_replace re subst txt =
  Str.global_substitute re (fun _ -> subst) txt

let env : Env.t ref = ref Env.default
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

let quoted = ['\"'] ([' ' '!' '#'-'~'] | utf8)* ['\"']
let symbol = ['!' '#'-'\'' '*'-'-' '/' ':'-'@' '['-'`' '{'-'~'] (*'"', '(', ')' *)
let non_whites = (['A'-'Z' 'a'-'z' '0'-'9'] | symbol | utf8)+

let xref = (['A'-'Z' 'a'-'z' '0'-'9' '!' '#'-'~'] | utf8)+ | "<>"
let integer = ['0'-'9']+

let end_of_command = '.' (space | '\n')

rule coq_bol = parse
  | (space* as s) (start_proof as sp)
      { start_proof s sp;
        end_current_command !env (Lexing.lexeme lexbuf);
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
        Lexing.new_line lexbuf;
        ssr_doc_bol lexbuf;
	fprintf !oc "%s" "</pre>\n";
	skip_newline lexbuf
      }
  (* Enter ssrdoc with special syntax mode e.g. markdown syntax *)
  | space* ("(**" (['a'-'z' '-']+ as mode) "*"+ "***)" "\n")
      { fprintf !oc "<div class=\"ssrdoc %s\">\n" mode;
        Lexing.new_line lexbuf;
        ssr_doc_bol lexbuf;
	fprintf !oc "%s" "</div>\n";
	skip_newline lexbuf
      }
  | space* ("(***" (['a'-'z' '-']+ as mode) "*"+ "***)" "\n")
      { fprintf !oc "<div class=\"ssrdoc %s\">\n" mode;
        Lexing.new_line lexbuf;
        ssr_doc_bol lexbuf;
	fprintf !oc "%s" "</div>\n";
	skip_newline lexbuf
      }
  | eof
      { () }
  | space* as s
      { space s;
        proceed_current_command (Lexing.lexeme lexbuf);
        coq lexbuf }

and skip_newline = parse
  | space* "\n"
      { Lexing.new_line lexbuf; coq_bol lexbuf }
  | ""
      { coq lexbuf }

and coq = parse
  | (space* as s) (end_proof as ep)
      { if !in_proof then end_proof s ep;
        end_current_command !env (Lexing.lexeme lexbuf);
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
  | '.' '\n'
      {
        end_current_command !env (Lexing.lexeme lexbuf);
        character '.'; Lexing.new_line lexbuf; newline();
        coq_bol lexbuf
      }
  | '.' space as s
      {
        end_current_command !env (Lexing.lexeme lexbuf);
        output_string !oc s;
        coq lexbuf
      }
  | (". ") (space* as s) (start_proof as sp)
      { newline();
        proceed_current_command (Lexing.lexeme lexbuf);
        start_proof s sp;
	skip_newline lexbuf ;
        coq lexbuf }
  | "\n"
      { Lexing.new_line lexbuf; newline(); coq_bol lexbuf }
  | eof
      { () }
  | quoted as q
      {
        proceed_current_command (Lexing.lexeme lexbuf);
        idents !env (Lexing.lexeme_start lexbuf) q (Lexing.lexeme_start_p lexbuf); coq lexbuf
      }
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
       proceed_current_command (Lexing.lexeme lexbuf);
       idents !env (Lexing.lexeme_start lexbuf) id  (Lexing.lexeme_start_p lexbuf); coq lexbuf}
(*  | non_whites as id
      {idents (Lexing.lexeme_start lexbuf) id; coq lexbuf}*)
  | _ as c
      {
        proceed_current_command (Lexing.lexeme lexbuf);
        character c; coq lexbuf
      }

and string = parse
  | "\"\""
      { character '\"'; character '\"'; string lexbuf }
  | "\""
      { () }
  | eof
      { () }
  | _ as c
      { character c; string lexbuf }

and bracket level = parse
  | "*)"
      { warn lexbuf "Warning: unterminated `]`"; end_bracket() }
  | "\\[" { character '['; bracket level lexbuf }
  | "\\]" { character ']'; bracket level lexbuf }
  | ']'
      { if level = 0 then (end_bracket(); doc lexbuf) else (character ']'; bracket (level - 1) lexbuf) }
  | '['
      { character '['; bracket (level + 1) lexbuf;}
  | path as id
      { idents !env (Lexing.lexeme_start lexbuf) id (Lexing.lexeme_start_p lexbuf); bracket level lexbuf }
  | "\""
      { start_string();
        string lexbuf;
        end_string();
        coq lexbuf }
  | eof
      { () }
  | _ as c
      { character c; bracket level lexbuf }

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
        Lexing.new_line lexbuf;
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
      { Lexing.new_line lexbuf; set_enum_depth 0; doc_bol lexbuf }
  | ""
      { doc lexbuf }

and doc = parse
  | "*)"
      { () }
  | "\n"
      { Lexing.new_line lexbuf; character '\n'; doc_bol lexbuf }
  | "["
      { start_bracket(); bracket 0 lexbuf }
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
      { Lexing.new_line lexbuf; ssr_doc_bol lexbuf }
  | ""
      { ssr_doc lexbuf }

and ssr_doc = parse
  | space* "*)"
      { ssr_doc lexbuf }
  | "\n"
      { Lexing.new_line lexbuf; character '\n'; ssr_doc_bol lexbuf }
  | eof
      { () }
  | _ as c
      { character c; ssr_doc lexbuf }

and verbatim = parse
  | "\n>>" space* "\n"
      { Lexing.new_line lexbuf; () }
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
    space+ (globkind as ty)
    space* "\n"
      { add_reference !current_module (int_of_string pos1) (int_of_string pos2)
          dp sp id ty;
        globfile lexbuf }
  | (globkind as ty)
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
let hierarchy_graph_dot_file = ref ""
let file_graph_dot_file = ref ""
let file_graph_depend_file = ref ""
let index_blacklist_file = ref ""
let show_type_information_using_coqtop_process = ref false
let show_type_information_using_rocq_lsp_process = ref false
let doc_source_url = ref ""

let file_graph dot_file depend_file =
  match dot_file, depend_file with
  | "", ""     -> None
  | "", depend -> Some (File_graph.FromDependFile depend)
  | dot, _     -> Some (File_graph.FromDotFile dot)

let process_v_file ?repo_root proj_name env all_files f =
  let pref_f = Filename.chop_suffix f ".v" in
  let base_f = Filename.basename pref_f in
  let module_name = !logical_name_base ^ module_name_of_file_name pref_f in
  let filepath = Sys.getcwd() ^ "/" ^ f in
  Option.iter (Type_lookup.open_file filepath module_name) env.type_lookup;
  current_module := module_name;
  let friendly_name = if !use_short_names then base_f else module_name in
  let title = "Module " ^ friendly_name in
  let ic = open_in f in
  oc := open_out (Filename.concat !output_dir (module_name ^ ".html"));
  enum_depth := 0; in_proof := false;
  Log.debug (!%"process_v_file: %s" f);
  let repo_file =
    Option.map (fun root -> root ^ "/" ^ f) repo_root
  in
  Generate_index.start_html_page !oc ?repo_file title title proj_name all_files;
  let lexbuf = Lexing.from_channel ~with_positions:true ic in
  Lexing.set_filename lexbuf filepath;
  coq_bol lexbuf;
  Generate_index.end_html_page !oc;
  close_out !oc; oc := stdout;
  close_in ic;
  Option.iter (Type_lookup.close_file filepath module_name) env.type_lookup;
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

let arg_deprecated_set_string msg sref : Arg.spec =
  Arg.String (fun s ->
      Log.warn (!%"DEPRECATED: %s" msg); sref := s)

let () =
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
    "-debug", Arg.Set Log.debug_flag, "Print debug messages to stderr";
    "-title", Arg.String (fun s -> title := s),
      "<title>  Set the title of the index.html";
    "-base", Arg.String (fun s -> logical_name_base := s ^ "."),
      "DEPRECATED: use -Q\n<coqdir>  Set the name space for the modules being processed";
    "-coqlib", Arg.String (fun s -> add_documentation_url "Stdlib" s),
      (!%"<url>   Set URL for Rocq standard library (default: %s)"
         default_stdlib_url_rocq);
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
      "   Do not add rocqnavi.css to the output directory";
    "-redirect", Arg.Set generate_redirects,
      "   Generate redirection files modname.html -> coqdir.modname.html";
    "-short-names", Arg.Set use_short_names,
      "   Use short, unqualified module names in the output";
    "-structure-graph", Arg.Set_string hierarchy_graph_dot_file,
      "   Show the hierarchy graph of <dot-file> on the index.html";
    "-hierarchy-graph", arg_deprecated_set_string "Use `-structure-graph`" hierarchy_graph_dot_file,
      "";
    "-file-graph", Arg.Set_string file_graph_dot_file,
      "   Show the dependency graph of <dot-file> on the index.html";
    "-dependency-graph", arg_deprecated_set_string "Use `-file-graph`" file_graph_dot_file,
    "";
    "-file-graph-from-depend", Arg.Set_string file_graph_depend_file,
      "   Show the file dependency graph from <depend.d> on the index.html";
    "-index-blacklist", Arg.Set_string index_blacklist_file,
      "   Exclude specified items from the index";
    "-show-type-information-using-coqtop-process", Arg.Set show_type_information_using_coqtop_process,
      "   Show type information of definitions as a tooltip (consider using -show-type-infomation-using-rocq-lsp)";
    "-show-type-information-using-rocq-lsp", Arg.Set show_type_information_using_rocq_lsp_process,
      "   Show type information of definitions as a tooltip";
    "-doc-source-url", Arg.Set_string doc_source_url,
      "   The Link to the source repository";
  ])
  process_file
  "Usage: rocqnavi [options] file.glob ... file.v ...\nOptions are:";
  if !v_files = [] then begin
    eprintf "No .v file provided, aborting\n";
    exit 1
  end;
  if (try not (Sys.is_directory !output_dir) with Sys_error _ -> true)
  then begin
    eprintf "Error: output directory %s does not exist or is not a directory.\n" !output_dir;
    exit 1
  end;
  if "" <> !hierarchy_graph_dot_file && not (Sys.file_exists !hierarchy_graph_dot_file) then begin
    eprintf "Error: The dot file does not exists: '%s'\n" !hierarchy_graph_dot_file;
    exit 1
  end;
  if "" <> !index_blacklist_file && not (Sys.file_exists !index_blacklist_file) then begin
    eprintf "Error: The file '%s' does not exists, which file was specified by the -index-blacklist option.\n"
      !index_blacklist_file;
    exit 1
  end;
  List.iter process_glob_file (List.rev !glob_files);
  let mapping_options =
    Directory_mappings.to_mapping_options !directory_mappings
  in
(*  XrefTable.dump !xref_table;*)
  let all_files = Generate_index.all_files xref_modules in
  let index_blacklist_opt =
    if !index_blacklist_file = "" then None
    else Some (Index_blacklist.from_file !index_blacklist_file)
  in
  let repo_root =
    if !doc_source_url = "" then None else Some !doc_source_url
  in
  write_file Resources.js (Filename.concat !output_dir "rocqnavi.js");
  if !generate_css then
    write_file Resources.css (Filename.concat !output_dir "rocqnavi.css");
  let file_graph_input = file_graph !file_graph_dot_file !file_graph_depend_file in
  Generate_index.generate ?repo_root !output_dir
    !xref_table xref_modules
    !title !directory_mappings !hierarchy_graph_dot_file file_graph_input
    index_blacklist_opt;

  if !show_type_information_using_coqtop_process
     || !show_type_information_using_rocq_lsp_process then
    let method_ = if !show_type_information_using_coqtop_process then
                    Type_lookup.Coqtop_emacs ("coqtop -emacs " ^ mapping_options)
                  else Rocq_LSP
    in
    Type_lookup.using method_ (fun conn ->
        env := Env.{type_lookup=Some conn; definition_blacklist=index_blacklist_opt;};
        List.iter (process_v_file ?repo_root !title !env all_files) (List.rev !v_files))
  else
    List.iter (process_v_file ?repo_root !title !env all_files) (List.rev !v_files)
}
