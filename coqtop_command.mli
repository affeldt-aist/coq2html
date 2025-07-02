type conn
val using : ?coqtop_bin:string -> (conn -> 'a) -> 'a
val about : conn -> string -> (string, string) result

val send : ?wait:float -> conn -> string -> (string, string) result
val exit : conn -> unit
