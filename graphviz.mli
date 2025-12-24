type t
type dot = t

val from_file : string -> t

val generate_file : string -> string -> t -> unit

val of_string : string -> t
