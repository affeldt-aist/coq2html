type xref =
  | Def of string * string
  | Ref of string * string * string

val html_escape : string -> string

val generate : string -> (string * int, xref) Hashtbl.t -> (string, unit) Hashtbl.t -> unit
