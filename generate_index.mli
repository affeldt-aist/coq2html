type xref =
  | Defs of (string * string) list
  | Ref of string * string * string

val escaped : string -> string

val sanitize_linkname : string -> string

val generate : string -> (string * int, xref) Hashtbl.t -> (string, unit) Hashtbl.t -> string -> unit
