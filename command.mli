val using : string -> (in_channel * out_channel * in_channel -> 'a) -> 'a
val send : out_channel -> string -> unit
val read_available : ?max:int -> in_channel -> string option
val input_line_with_timeout : float -> in_channel -> string option
