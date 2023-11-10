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
             EntryKind "mod";
             EntryKind "var";
             EntryKind "thm";
             EntryKind "abbrev";
             EntryKind "def"]

let skind = function Global -> "Global Index"
                   | EntryKind "mod" -> "Module Index"
                   | EntryKind "var" -> "Variable Index"
                   | EntryKind "thm" -> "Theorem Index"
                   | EntryKind "abbrev" -> "Abbreviation Index"
                   | EntryKind "def" -> "Definition Index"
                   | EntryKind other -> other

let is_kind = function
  | Global -> fun _ -> true
  | EntryKind k -> fun s -> s = k

let linkname_of_kind = function Global -> "global"
                              | EntryKind s -> s

let table xrefs =
  let mkrow kind =
    (!%"<td>%s</td>" (skind kind))
    ^ (List.map (fun (c, xrefs) ->
           if List.exists
                (function (_key, (_name,_pos,_path, typ)) when is_kind kind typ -> true
                         |_ -> false) xrefs
           then
             !%{|<td><a href="index_%s_%c.html">%c</a></td>|} (linkname_of_kind kind) c c
           else
             !%{|<td>%c</td>|} c) xrefs
    |> String.concat "")
    |> fun s -> "<tr>" ^ s ^ "</tr>"
  in
  "<table><tbody>"
  ^ (List.map mkrow kinds |> String.concat "")
  ^ "</tbody></table>"

let generate_with_capital output_dir table kind (c, xrefs) =
  if xrefs = [] then () else
    let body =
      List.filter (fun (_, (_, _, _, typ)) -> is_kind kind typ) xrefs
      |> List.map (fun (key, (name, pos, path, typ)) ->
             !%{|<a href="%s.html#%s">%s</a> [%s, in %s:%d]|} name path path typ name pos)
      |> String.concat "<br>"
      |> (^) (!%"%s<h2>%c</h2>" table c)
    in
    write_html_file body (Filename.concat output_dir (!%"index_%s_%c.html" (linkname_of_kind kind) c))

let generate output_dir xref_table =
  let xrefs = 
    List.map (fun c ->
        Hashtbl.fold (fun (name, pos) xref store ->
            match xref with
            | Def (path, typ) when
                   String.get path 0 = c
                   || String.get path 0 = Char.lowercase_ascii c ->
               (String.lowercase_ascii path, (name, pos, path, typ)) :: store
            | _ -> store)
          xref_table []
        |> List.sort compare
        |> fun xrefs -> (c, xrefs))
      alphabets
  in
  List.iter (fun kind ->
      List.iter (generate_with_capital output_dir (table xrefs) kind) xrefs)
    kinds

