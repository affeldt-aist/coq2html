(** HB structure graph parsing and Cytoscape element generation. *)

type graph

val parse_dot_file : string -> graph
(** Parse a DOT file produced by [HB.graph]. *)

val to_cytoscape_elements_json : graph -> string
(** Convert the parsed graph to Cytoscape elements JSON.
    Clusters are created from the first underscore-separated prefix:
    for instance [GRing_Field] is grouped under [GRing].
    Nodes without underscore remain at root level. *)
