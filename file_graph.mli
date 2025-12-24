type input =
  | FromDependFile of string
  | FromDotFile of string

val parse : in_channel -> Graphviz.dot

val parse_dep_file : string -> Graphviz.dot
