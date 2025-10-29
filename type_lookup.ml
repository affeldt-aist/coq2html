type method_ =
  | Coqtop_emacs of string
(*  | Rocq_LSP *)

type conn =
  | Coqtop_emacs_conn of Coqtop_command.conn

let using method_ f =
  match method_ with
  | Coqtop_emacs command ->
     Coqtop_command.using ~coqtop_bin:command (fun conn -> f(Coqtop_emacs_conn conn))

let ask_type_info_of name conn =
  match conn with
  | Coqtop_emacs_conn conn ->
     Coqtop_command.about conn name
let load cmd conn =
  match conn with
  | Coqtop_emacs_conn conn ->
     Coqtop_command.send conn cmd |> ignore
