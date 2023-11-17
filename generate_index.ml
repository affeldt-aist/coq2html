let (!%) s = Printf.sprintf s

type xref =
  | Def of string * string    (* path, type *)
  | Ref of string * string * string (* unit, path, type *)

let alphabets = (* ['A'; ...; 'Z'; '_'] *)
  let rec iter code store =
    if code <= Char.code 'Z' then iter (succ code) (Char.chr code :: store)
    else store
  in
  List.rev ('_' :: iter (Char.code 'A') [])

let write_html_file txt filename =
  let oc = open_out filename in
  output_string oc (Str.global_replace (Str.regexp "<h1.*</h1>") "" Resources.header);
  output_string oc txt;
  output_string oc Resources.footer;
  close_out oc
  
type kind = Global | EntryKind of string

let kinds = [Global;
             EntryKind "def";
             EntryKind "prf";
             EntryKind "abbrev";
             EntryKind "file";
            ]

let skind = function Global -> "Global"
                   | EntryKind "def" -> "Definition"
                   | EntryKind "prf" -> "Lemma"
                   | EntryKind "abbrev" -> "Abbreviation"
                   | EntryKind other -> other

let is_kind = function
  | Global -> fun _ -> true
  | EntryKind k -> fun s -> s = k

let linkname_of_kind = function Global -> "global"
                              | EntryKind s -> s

type item = {kind: kind; name: string; linkname: string; module_: string}

let table citems =
  let mkrow kind =
    (!%"<td>%s Index</td>" (skind kind))
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
  if items = [] then () else
    let body =
      let h2 = if kind = Global then !%"%c" c else !%"%c (%s)" c (skind kind) in
      List.filter (fun item -> kind = Global || item.kind = kind) items
      |> List.map (fun item ->
             !%{|<a href="%s">%s</a> [%s, in %s]|} item.linkname item.name (skind item.kind) item.module_)
      |> String.concat "<br>"
      |> (^) (!%"%s<h2>%s</h2>" table h2)
    in
    write_html_file body (Filename.concat output_dir (!%"index_%s_%c.html" (linkname_of_kind kind) c))

(* generate index.html *)
let generate_topfile output_dir xrefs =
  let body = table xrefs in
  write_html_file body (Filename.concat output_dir "index.html")

let is_initial c s =
  if s = "" then false else Char.uppercase_ascii (String.get s 0) = c

let generate output_dir xref_table xref_modules =
  let indexed_items =
    List.map (fun c ->
        let items =
          Hashtbl.fold (fun (name, pos) xref store ->
            match xref with
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

