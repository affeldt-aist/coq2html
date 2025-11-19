type method_ =
  | Coqtop_emacs of string
  | Rocq_LSP

type conn

type info =
  | Markdown of string
  | PlainText of string

val using : method_ -> (conn -> 'a) -> 'a
val open_file : string -> string -> conn -> unit
val close_file : string -> string -> conn -> unit
val ask_type_info_of : string -> string -> (int*int) -> conn -> (info, string) result
val load: string -> conn -> unit
