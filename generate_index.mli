type xref =
  | Defs of (string * string) list
  | Ref of string * string * string

val html_escape : string -> string

val generate : string -> (string * int, xref) Hashtbl.t -> (string, unit) Hashtbl.t -> unit
