type method_ =
  | Coqtop_emacs of string
(*  | Rocq_LSP *)

type conn

val using : method_ -> (conn -> 'a) -> 'a
val ask_type_info_of : string -> conn -> (string, string) result
val load: string -> conn -> unit
