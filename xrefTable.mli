type t

type xref =
  | Defs of (string * string) list    (* path, type *)
  | Ref of string * string * string (* unit, path, type *)


(** [find module_name pos] *)
val find : t -> string -> int -> (Range.t * xref) option

val empty : t

val add_reference: t -> string -> int -> int -> string -> string -> string -> t
val add_definition: t -> string -> int -> int -> string -> string -> t

val fold : ((string * int) -> (Range.t * xref) -> 'b -> 'b) -> t -> 'b -> 'b

val dump : t -> unit
