val (!%) : ('a, unit, string) format -> 'a
val shell : string -> unit


val read_lines : string -> string list
val write_lines : string -> string list -> unit

val grep : string -> string -> bool

val list_group_by : ('a -> 'b) -> 'a list -> ('b * 'a list) list
val list_sort_by : ('a -> 'b) -> 'a list -> 'a list
