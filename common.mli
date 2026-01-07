val (!%) : ('a, unit, string) format -> 'a
val shell : string -> unit

val file_using_r : string -> (in_channel -> 'a) -> 'a
val file_using_w : string -> (out_channel -> 'a) -> 'a
val read_lines : string -> string list
val write_lines : string -> string list -> unit

val grep : string -> string -> bool

val list_group_by : ('a -> 'b) -> 'a list -> ('b * 'a list) list
val list_sort_by : ('a -> 'b) -> 'a list -> 'a list
val list_uniq : 'a list -> 'a list
val list_take : int -> 'a list -> 'a list
val list_drop : int -> 'a list -> 'a list
val list_max_by : ('a -> 'b) -> 'a list -> 'a option
val warn : string -> unit

val html_escaped : string -> string
