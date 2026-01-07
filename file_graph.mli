type input =
  | FromDependFile of string
  | FromDotFile of string

val parse : Directory_mappings.t -> in_channel -> Graphviz.dot

val parse_dep_file : Directory_mappings.t -> string -> Graphviz.dot
