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
  
let table = 
  "<table>"
  ^ "<tbody><tr>"
  ^ "<td>Global Index</td>"
  ^ (List.map (fun c ->
        !%{|"<td><a href="index_global_%c.html">%c</a></td>|} c c) alphabets
    |> String.concat "")
  ^ "</tr></table>"

let generate_with_capital output_dir xref_table cap =
  let xrefs =
    Hashtbl.fold (fun (name, pos) xref store ->
        match xref with
        | Def (path, typ) when
               String.get path 0 = cap
               || String.get path 0 = Char.lowercase_ascii cap ->
           (String.lowercase_ascii path, (name, pos, path, typ)) :: store
        | _ -> store)
      xref_table []
  in
  if xrefs = [] then () else
    let body =
      List.sort compare xrefs
      |> List.map (fun (key, (name, pos, path, typ)) ->
             !%{|<a href="%s.html#%s">%s</a> [%s, in %s:%d]|} name path path typ name pos)
      |> String.concat "<br>"
      |> (^) (!%"%s<h2>%c</h2>" table cap)
    in
    write_html_file body (Filename.concat output_dir (!%"index_global_%c.html" cap))

let generate output_dir xref_table =
  List.iter (generate_with_capital output_dir xref_table) alphabets

