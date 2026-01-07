open Common
module Json = Yojson.Basic

type method_ =
  | Coqtop_emacs of string
  | Rocq_LSP

type conn =
  | Coqtop_emacs_conn of Coqtop_command.conn
  | Rocq_LSP_conn of Lsp_client.conn

let using method_ f =
  match method_ with
  | Coqtop_emacs command ->
     Coqtop_command.using ~coqtop_bin:command
       (fun conn -> f (Coqtop_emacs_conn conn))
  | Rocq_LSP ->
     Lsp_client.using (fun conn ->
         let rootpath = Sys.getcwd () in
         Lsp_client.initialize rootpath conn
         |> fun json -> Log.warn (!%"DID OPEN(%s): %s" rootpath (Json.pretty_to_string json));
         f (Rocq_LSP_conn conn))

let open_file filepath module_name = function
  | Coqtop_emacs_conn conn ->
     let cmd = !%"Require Import %s.\n" module_name in
     Coqtop_command.send conn cmd |> ignore
  | Rocq_LSP_conn conn ->
     Lsp_client.did_open filepath conn

let close_file filepath module_name = function
  | Coqtop_emacs_conn conn -> ()
  | Rocq_LSP_conn conn ->
     Lsp_client.did_close filepath conn

type info =
  | Markdown of string
  | PlainText of string

let ask_type_info_of name filepath (line, col) conn =
  match conn with
  | Coqtop_emacs_conn conn ->
     Coqtop_command.about conn name
     |> Result.map (fun t -> PlainText t)
  | Rocq_LSP_conn conn ->
     let open Json.Util in
     let pos = Lsp_client.Location.{line; character=col} in
     try
       let json = Lsp_client.hover pos filepath conn in
       let content = json |> member "contents" |> member "value" |> to_string in
       Ok (Markdown content)
     with
     | Json.Util.Type_error (msg, json) ->
        let params = !%"%s, %s [%d,%d]" name filepath line col in
        Error
          (!%"Json type error %s:\n```json\n%s\n```\n%s" params (Json.pretty_to_string json) msg)
     | e -> Error (Printexc.to_string e)

let load cmd conn =
  match conn with
  | Coqtop_emacs_conn conn ->
     Coqtop_command.send conn cmd |> ignore
  | Rocq_LSP_conn conn ->
     ()
