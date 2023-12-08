let (!%) s = Printf.sprintf s

let html_escape s =
  String.to_seq s
  |> Seq.map (function
       | '<' -> "&lt;"
       | '>' -> "&gt;"
       | '&' -> "&amp;"
       | c -> String.make 1 c)
  |> List.of_seq
  |> String.concat ""
                
type xref =
  | Def of string * string    (* path, type *)
  | Ref of string * string * string (* unit, path, type *)

let alphabets = (* ['A'; ...; 'Z'; '_'] *)
  let rec iter code store =
    if code <= Char.code 'Z' then iter (succ code) (Char.chr code :: store)
    else store
  in
  List.rev ('*' :: '_' :: iter (Char.code 'A') [])

let write_html_file txt filename =
  let oc = open_out filename in
  output_string oc (Str.global_replace (Str.regexp "<h1.*</h1>") "" Resources.header);
  output_string oc txt;
  output_string oc Resources.footer;
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

type item = {kind: kind; name: string; linkname: string; module_: string}

let table citems =
  let mkrow kind =
    (!%"<td>%s</td>" (skind kind))
    ^ (List.map (fun (c, items) ->
           if List.exists (fun item -> kind = Global || item.kind = kind) items then
             !%{|<td><a href="index_%s_%c.html">%c</a></td>|} (linkname_of_kind kind) c c
           else
             !%{|<td>%c</td>|} c) citems
    |> String.concat "")
    |> fun s -> "<tr>" ^ s ^ "</tr>"
  in
  "<table><tbody>"
  ^ (List.map mkrow kinds |> String.concat "")
  ^ "</tbody></table>"

(* generate an html file e.g. mathcomp.classical.functions.html *)
let generate_with_capital output_dir table kind (c, items) =
  let html_of_item item =
    if item.kind = EntryKind "not" then
      let (scope, notation) = 
        match Str.(bounded_split_delim (regexp ":") item.name 4) with
        | [_; _; ""; notation] -> ("<span class=\"warning\">no scope</span>", notation)
        | [_; _; scope; notation] -> ("in " ^ scope, notation)
        | _ ->
           Printf.eprintf "=== %s\n" item.name;
             failwith "HOGEHOGE"
      in
      !%{|<a href="%s">%s</a> [%s, in %s] (%s)|} item.linkname (html_escape notation) (linkname_of_kind item.kind) item.module_ scope
    else
      !%{|<a href="%s">%s</a> [%s, in %s]|} item.linkname item.name (linkname_of_kind item.kind) item.module_
  in
  if items = [] then () else
    let body =
      let h2 = if kind = Global then !%"%c" c else !%"%c (%s)" c (skind kind) in
      List.filter (fun item -> kind = Global || item.kind = kind) items
      |> List.map html_of_item
      |> String.concat "<br>"
      |> (^) (!%"%s<h2>%s</h2>" table h2)
    in
    write_html_file body (Filename.concat output_dir (!%"index_%s_%c.html" (linkname_of_kind kind) c))

(* generate index.html *)
let generate_topfile output_dir xrefs =
  let body = table xrefs in
  write_html_file body (Filename.concat output_dir "index.html")

let is_initial c s =
  if s = "" then false else
    match c, String.get s 0 with
    | _, '_' -> c = '_'
    | _, ('a'..'z' as s0) -> Char.uppercase_ascii s0 = c
    | _, ('A'..'Z' as s0) -> s0 = c
    | '*', _ -> true
    | _, _ -> false


let generate output_dir xref_table xref_modules =
  let indexed_items =
    List.map (fun c ->
        let items =
          Hashtbl.fold (fun (name, pos) xref store ->
            match xref with
            | Def (path, "binder") -> (*ignore binders*)
               store
            | Def (path, typ) when is_initial c path ->
               let linkname = !%"%s.html#%s" name path in
               let module_ = name in
               {kind=EntryKind typ; name=path; linkname; module_} :: store
            | _ -> store) xref_table []
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
      alphabets
  in
  List.iter (fun kind ->
      List.iter (generate_with_capital output_dir (table indexed_items) kind) indexed_items)
    kinds;
  generate_topfile output_dir indexed_items

