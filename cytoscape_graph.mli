(** Shared Cytoscape HTML renderer for compound/collapsible graphs. *)

val render_compound_graph :
  id_prefix:string ->
  title:string ->
  hint:string ->
  elements_json:string ->
  string
