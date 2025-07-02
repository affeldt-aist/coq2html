type t

type xref =
  | Defs of (string * Glob_kind.t) list    (* path, type *)
  | Ref of string * string * Glob_kind.t (* unit, path, type *)


(** [find module_name pos] *)
val find : t -> string -> int -> (Range.t * xref) option

val empty : t

val add_reference: t -> string -> int -> int -> string -> string -> Glob_kind.t -> t
val add_definition: t -> string -> int -> int -> string -> Glob_kind.t -> t

val fold : ((string * int) -> (Range.t * xref) -> 'b -> 'b) -> t -> 'b -> 'b

val dump : t -> unit
