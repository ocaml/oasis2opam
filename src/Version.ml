(***************************************************************************)
(*  OASIS2OPAM: Convert OASIS metadata to OPAM package descriptions        *)
(*                                                                         *)
(*  Copyright (C) 2013-2015, Christophe Troestler                          *)
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

let to_string (p, v) =
  p ^ " (" ^ OASISVersion.string_of_version v ^ ")"

module Ordered = struct
  type t = OASISVersion.t
  let compare = OASISVersion.version_compare
end

module Set = struct
  include Set.Make(Ordered)

  let to_string s =
    let l = fold (fun e l -> string_of_version e :: l) s [] in
    String.concat ", " l
end

(* OASISVersion.string_of_comparator is unfortunately not good because
   OPAM requires the versions be between quotes and use [&] and [|]. *)
let rec opam_string_of_comparator = function
  | VGreater v  -> "> \"" ^ string_of_version v ^ "\""
  | VEqual v    -> "= \"" ^ string_of_version v ^ "\""
  | VLesser v   -> "< \"" ^ string_of_version v ^ "\""
  | VGreaterEqual v -> ">= \"" ^ string_of_version v ^ "\""
  | VLesserEqual v  -> "<= \"" ^ string_of_version v ^ "\""
  (* FIXME: does OPAM use parentheses to delimit clauses? *)
  | VOr (c1, c2)  ->
     "(" ^ opam_string_of_comparator c1
     ^ ") | (" ^ opam_string_of_comparator c2 ^ ")"
  | VAnd (c1, c2) ->
     "(" ^ opam_string_of_comparator c1
     ^ ") & (" ^ opam_string_of_comparator c2 ^ ")"

(* Satisfy both (optional) version constrains. *)
let satisfy_both v1 v2 =
  match v1, v2 with
  | None, None -> None
  | Some _, None -> v1
  | None, Some _ -> v2
  | Some v1, Some v2 -> Some(comparator_reduce (VAnd(v1, v2)))


let max v1 v2 =
  if OASISVersion.version_compare v1 v2 >= 0 then v1 else v2

let min v1 v2 =
  if OASISVersion.version_compare v1 v2 <= 0 then v1 else v2

(* To be ported back to oasis eventually.
   At least internally, one would benefit from true and false values. *)
let rec comparator_reduce = function
  | VAnd (v1, v2) ->
     let v1 = comparator_reduce v1
     and v2 = comparator_reduce v2 in
     (match v1, v2 with
      | VGreater v1, VGreater v2 -> VGreater(max v1 v2)
      | VGreaterEqual v1, VGreaterEqual v2 -> VGreaterEqual(max v1 v2)
      | VGreaterEqual v1, VGreater v2
      | VGreater v2, VGreaterEqual v1 ->
         if OASISVersion.version_compare v1 v2 > 0 then VGreaterEqual v1
         else VGreater v2
      | VLesser v1, VLesser v2 -> VLesser(min v1 v2)
      | VLesserEqual v1, VLesserEqual v2 -> VLesserEqual(min v1 v2)
      | VLesserEqual v1, VLesser v2
      | VLesser v2, VLesserEqual v1 ->
         if OASISVersion.version_compare v1 v2 < 0 then VLesserEqual v1
         else VLesser v2
      | (VEqual v1 as c1), c2
      | c2, (VEqual v1 as c1) ->
         if OASISVersion.comparator_apply v1 c2 then c1
         else VAnd (c1, c2) (* v1 does not satisfy v2, always false *)
      | _ -> VAnd (v1, v2))
  | VOr (v1, v2) ->
     let v1 = comparator_reduce v1
     and v2 = comparator_reduce v2 in
     (match v1, v2 with
      | VGreater v1, VGreater v2 -> VGreater(min v1 v2)
      | VGreaterEqual v1, VGreaterEqual v2 -> VGreaterEqual(min v1 v2)
      | VGreaterEqual v1, VGreater v2
      | VGreater v2, VGreaterEqual v1 ->
         if OASISVersion.version_compare v1 v2 <= 0 then VGreaterEqual v1
         else VGreater v2
      | VLesser v1, VLesser v2 -> VLesser(max v1 v2)
      | VLesserEqual v1, VLesserEqual v2 -> VLesserEqual(max v1 v2)
      | VLesserEqual v1, VLesser v2
      | VLesser v2, VLesserEqual v1 ->
         if OASISVersion.version_compare v1 v2 >= 0 then VLesserEqual v1
         else VLesser v2
      | (VEqual v1 as c1), c2
      | c2, (VEqual v1 as c1) ->
         if OASISVersion.comparator_apply v1 c2 then c2
         else VOr (c1, c2) (* v1 does not satisfy c2 *)
      | _ -> VOr (v1, v2))
  | cmp -> cmp

let string_of_comparator v =
  (* If one wants to print the constraints, a short form is always
     desirable. *)
  opam_string_of_comparator(comparator_reduce v)

type kind = Std | Build | Test

type constraints = { kind: kind;
                     cmp: OASISVersion.comparator option }

let constrain ?(kind=Std) cmp = { kind; cmp }

let no_constraint = { kind = Std;  cmp = None }

let is_unconstrained c = c.kind = Std && c.cmp = None

let string_of_constraint c =
  match c.kind, c.cmp with
  | Std, None -> ""
  | Std, Some cmp -> string_of_comparator cmp
  | Build, None -> "build"
  | Build, Some cmp -> "build & " ^ string_of_comparator cmp
  | Test, None -> "test"
  | Test, Some cmp -> "test & " ^ string_of_comparator cmp

let satisfy_both_constraints c1 c2 =
  let kind = match c1.kind, c2.kind with
    | _, Std -> c1.kind
    | Std, _ -> c2.kind
    | Build, (Build|Test) | Test, Build -> Build (* Build comes before test *)
    | Test, Test -> Test in
  { kind;
    cmp = satisfy_both c1.cmp c2.cmp }
