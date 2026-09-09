open Glob_kind

type def = { pos_from : int; pos_to : int;
             section_path : string; id : string; kind : kind }
type ref =
  { pos_from : int; pos_to : int;
    logical_path : string; section_path : string; id : string; kind : kind }

type entry =
  | Definition of def
  | Reference of ref

let pos_of_entry = function
  | Definition def -> def.pos_from
  | Reference ref  -> ref.pos_from

type t =
  { file_module : string; entries : entry list }
