module Json = Yojson.Basic

module Location : sig
  type position = {line: int; character: int}
  type range = {start: position; end_: position}
  val range_of_json : Json.t -> range
  type location = {uri: string; range: range}
  val json_of_position : position -> Json.t
end

type conn

val using: (conn -> 'a) -> 'a

val initialize: string -> ?options:Json.t -> conn -> Json.t
val initialized : conn -> unit
val set_trace : string -> conn -> unit
val did_open : string -> conn -> unit
val did_close : string -> conn -> unit

val document_symbol : string -> conn -> Json.t
val hover : Location.position -> string -> conn -> Json.t
val coq_getDocument : string -> conn -> Json.t
