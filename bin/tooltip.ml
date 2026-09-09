open Common
open Env

type t = string

let span_of_tooltip content =
  !%"<span class='tooltip-area'>%s</span>" content

let tag_with_tooltip tagname name classes tooltip_content display_text =
  let tooltip = span_of_tooltip tooltip_content in
  !%{|<%s name="%s" class="%s tooltip">%s%s</%s>|} tagname name classes
    (html_escaped display_text) tooltip tagname

let lookup_type_info conn id loc =
  let position = Lexing.(loc.pos_lnum - 1, loc.pos_cnum - loc.pos_bol + 1) in
  let filename = Lexing.(loc.pos_fname) in
  match Type_lookup.ask_type_info_of id filename position conn with
  | Ok ty -> Some ty
  | Error message -> Log.warn (!%"fail: lookup_type_info '%s' : '%s'" id message);
                     None

let make env current_module loc id kind =
  let is_black =
    match env.definition_blacklist with
    | None -> false
    | Some list -> Index_blacklist.is_listed list id
  in
  let type_information =
    match env.type_lookup, kind with
    | Some conn, Glob_kind.Definition when is_black = false ->
       begin match lookup_type_info conn id loc with
       | None -> ""
       | Some (Type_lookup.Markdown md) ->
          !%"<span class='markdown'>%s</span>" md
       | Some (PlainText txt) -> !%"<p>%s</p>" txt
       end |> Option.some
    | _ -> None
  in
  let used_by =
    let defs = UsedByTable.find env.usedby_table (current_module, id) in
    Option.value ~default:[] defs
    |> List.map (fun (dmod, _dpath) -> dmod)
    |> list_uniq
    |> List.filter (fun m -> m <> current_module)
    |> List.map !%"<li>%s</li>"
    |> function
      | [] -> None
      | ms -> Some ("Used by\n<ul>" ^ String.concat "" ms ^ "</ul>")
  in
  let source_url =
    match env.repository_root_url with
    | Some repo_root ->
       let line = loc.Lexing.pos_lnum in
       let filepath =
         String.split_on_char '.' current_module
         |> Directory_mappings.inverse_apply env.directory_mappings
         |> String.concat "/"
       in
       let url = !%"%s/%s.v#L%d"repo_root filepath line in
       Some (!%"<a href='%s' target='_blank'>Source code</a>" url)
    | None -> None
  in
  [type_information; used_by; source_url]
  |> List.filter_map Fun.id
  |> function
    | [] -> None
    | contents -> Some (String.concat "<hr/>" contents)
