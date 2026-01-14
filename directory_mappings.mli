type t

val empty : t

val add : t -> string -> string -> t
(**
   Example: [add "theories" "mathcomp.analysis"] register the
   correspondence between physical path 'theories/' and logical path
   'mathcomp.analysis'.
 *)

val apply : t -> string list -> string list

val to_mapping_options : t -> string
