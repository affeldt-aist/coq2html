type xref =
  | Def of string * string
  | Ref of string * string * string

val generate : string -> (string * int, xref) Hashtbl.t -> unit
