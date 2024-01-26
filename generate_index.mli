type xref =
  | Defs of (string * string) list
  | Ref of string * string * string

val escaped : string -> string

val sanitize_linkname : string -> string

type file_path
val all_files : (string, unit) Hashtbl.t -> file_path list
val sidebar_files : file_path list -> string

val generate : string -> (string * int, xref) Hashtbl.t -> (string, unit) Hashtbl.t -> string -> unit
