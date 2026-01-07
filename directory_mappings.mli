type t

val empty : t
val add : t -> string -> string -> t
(*val find : t -> string list -> (string list * string) option*)
val apply : t -> string list -> string list

val to_mapping_options : t -> string
