(***************************************************************************)
(*  OASIS2OPAM: Convert OASIS metadata to OPAM package descriptions        *)
(*                                                                         *)
(*  Copyright (C) 2013-2014, Christophe Troestler                          *)
(*                                                                         *)
(*  This library is free software; you can redistribute it and/or modify   *)
(*  it under the terms of the GNU General Public License as published by   *)
(*  the Free Software Foundation; either version 3 of the License, or (at  *)
(*  your option) any later version, with the OCaml static compilation      *)
(*  exception.                                                             *)
(*                                                                         *)
(*  This library is distributed in the hope that it will be useful, but    *)
(*  WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file      *)
(*  COPYING for more details.                                              *)
(*                                                                         *)
(*  You should have received a copy of the GNU Lesser General Public       *)
(*  License along with this library; if not, write to the Free Software    *)
(*  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301   *)
(*  USA.                                                                   *)
(***************************************************************************)

(** Some additional functions to work with [OASISVersion.t] *)

open OASISVersion

(* OASISVersion.string_of_comparator is unfortunately not good because
   OPAM requires the versions be between quotes. *)
let rec string_of_comparator = function
  | VGreater v  -> "> \"" ^ string_of_version v ^ "\""
  | VEqual v    -> "= \"" ^ string_of_version v ^ "\""
  | VLesser v   -> "< \"" ^ string_of_version v ^ "\""
  | VGreaterEqual v -> ">= \"" ^ string_of_version v ^ "\""
  | VLesserEqual v  -> "<= \"" ^ string_of_version v ^ "\""
  (* FIXME: does OPAM use parentheses to delimit clauses? *)
  | VOr (c1, c2)  ->
     "(" ^ string_of_comparator c1 ^ ") || (" ^ string_of_comparator c2 ^ ")"
  | VAnd (c1, c2) ->
     "(" ^ string_of_comparator c1 ^ ") && (" ^ string_of_comparator c2 ^ ")"


(* Satisfy both (optional) version constrains. *)
let satisfy_both v1 v2 =
  match v1, v2 with
  | None, None -> None
  | Some _, None -> v1
  | None, Some _ -> v2
  | Some v1, Some v2 -> Some(comparator_reduce (VAnd(v1, v2)))
