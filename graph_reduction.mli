(** Shared transitive-reduction utilities for directed acyclic graph-like data. *)

val transitive_reduction_by_key :
  nodes:'a list ->
  key:('a -> string) ->
  edges:('a * 'a) list ->
  ('a * 'a) list
(** Compute a transitive reduction over [edges] using [nodes] as the universe.
    Node identity is provided by [key]. *)
