let (!%) s = Printf.sprintf s

let escaped =
  let buff = Buffer.create 5 in
  fun s ->
  Buffer.clear buff;
  for i = 0 to String.length s - 1 do
    match s.[i] with
    | '<' -> Buffer.add_string buff "&lt;"
    | '>' -> Buffer.add_string buff "&gt;"
    | '&' -> Buffer.add_string buff "&amp;"
    | '\"' -> Buffer.add_string buff "&quot;"
    | c -> Buffer.add_char buff c
  done;
  Buffer.contents buff

let sanitize_linkname s =
  let rec loop esc i =
    if i < 0 then if esc then escaped s else s
    else match s.[i] with
         | 'a'..'z' | 'A'..'Z' | '0'..'9' | '.' | '_' -> loop esc (i-1)
         | '<' | '>' | '&' | '\'' | '\"' -> loop true (i-1)
         | '-' | ':' -> loop esc (i-1) (* should be safe in HTML5 attribute name syntax *)
         | _ ->
            (* This name contains complex characters:
               this is probably a notation string, we simply hash it. *)
            Digest.to_hex (Digest.string s)
  in loop false (String.length s - 1)

type xref =
  | Defs of (string * string) list    (* path, type *)
  | Ref of string * string * string (* unit, path, type *)

let alphabets = (* ['A'; ...; 'Z'; '_'] *)
  let rec iter code store =
    if code <= Char.code 'Z' then iter (succ code) (Char.chr code :: store)
    else store
  in
  List.rev ('*' :: '_' :: iter (Char.code 'A') [])

let write_html_file txt filename title =
  let oc = open_out filename in
  output_string oc (Str.global_replace (Str.regexp "<title>.*</title>") (!%"<title>%s</title>" title)
                      (Str.global_replace (Str.regexp "<h1.*</h1>") "" Resources.header));   
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

let html_of_notation_item item =
  let (scope, notation) = 
    match Str.(bounded_split_delim (regexp ":") item.name 4) with
    | [_; _; ""; notation] -> ("<span class=\"warning\">no scope</span>", notation)
    | [_; _; scope; notation] -> ("in " ^ scope, notation)
    | _ ->
       failwith (!%"unexpected notation format in glob file: name=%s" item.name)
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
           (* continuous two quotation *)
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

(* generate an html file e.g. mathcomp.classical.functions.html *)
let generate_with_capital output_dir table kind (c, items) =
  let html_of_item item =
    if item.kind = EntryKind "not" then
      html_of_notation_item item
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
    let title = !%"%C (%s)" c (skind kind) in
    write_html_file body (Filename.concat output_dir (!%"index_%s_%c.html" (linkname_of_kind kind) c)) title

(* generate index.html *)
let generate_topfile output_dir xrefs title =
  let body = table xrefs in
  write_html_file body (Filename.concat output_dir "index.html") title

let is_initial c s =
  if s = "" then false else
    match c, String.get s 0 with
    | _, '_' -> c = '_'
    | _, ('a'..'z' as s0) -> Char.uppercase_ascii s0 = c
    | _, ('A'..'Z' as s0) -> s0 = c
    | '*', _ -> true
    | _, _ -> false


let generate output_dir xref_table xref_modules title =
  let indexed_items =
    List.map (fun c ->
        let items =
          Hashtbl.fold (fun (name, pos) xref store ->
            match xref with
            | Defs defs ->
               List.filter (fun (_, typ) -> typ <> "binder") defs
               |> List.filter (fun (path, _) -> is_initial c path)
               |> List.map (fun (path, typ) ->
                      let linkname = !%"%s.html#%s" name (sanitize_linkname path) in
                      let module_ = name in
                      {kind=EntryKind typ; name=path; linkname; module_})
               |> fun is -> is @ store
            | Ref _ -> store) xref_table []
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
  generate_topfile output_dir indexed_items title

