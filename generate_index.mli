(* *********************************************************************)
(*                                                                     *)
(*        Addition to the the Coq2HTML documentation generator         *)
(*                                                                     *)
(*  Copyright National Institute of Advanced Industrial Science and    *)
(*  Technology.  All rights reserved.  This file is distributed        *)
(*  under the terms of the GNU General Public License as published by  *)
(*  the Free Software Foundation, either version 2 of the License, or  *)
(*  (at your option) any later version.                                *)
(*                                                                     *)
(* *********************************************************************)

module Range : sig
  type t = (int * int)
  val in_ : int -> t -> bool
end

type range = Range.t

type xref =
  | Defs of (string * string) list
  | Ref of string * string * string

val escaped : string -> string

val sanitize_linkname : string -> string

type file_path
val all_files : (string, unit) Hashtbl.t -> file_path list
val sidebar_files : file_path list -> string

val generate : string -> (string * int, range * xref) Hashtbl.t -> (string, unit) Hashtbl.t -> string -> unit
