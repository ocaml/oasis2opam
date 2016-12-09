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
open Utils

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
let rec opam_string_of_comparator ~var = function
  | VGreater v  -> var ^ "> \"" ^ string_of_version v ^ "\""
  | VEqual v    -> var ^ "= \"" ^ string_of_version v ^ "\""
  | VLesser v   -> var ^ "< \"" ^ string_of_version v ^ "\""
  | VGreaterEqual v -> var ^ ">= \"" ^ string_of_version v ^ "\""
  | VLesserEqual v  -> var ^ "<= \"" ^ string_of_version v ^ "\""
  | VOr (c1, c2)  ->
     "(" ^ opam_string_of_comparator ~var c1
     ^ ") | (" ^ opam_string_of_comparator ~var c2 ^ ")"
  | VAnd (c1, c2) ->
     "(" ^ opam_string_of_comparator ~var c1
     ^ ") & (" ^ opam_string_of_comparator ~var c2 ^ ")"

(* Satisfy both (optional) version constrains. *)
let satisfy_both v1 v2 =
  match v1, v2 with
  | None, None -> None
  | Some _, None -> v1
  | None, Some _ -> v2
  | Some v1, Some v2 -> Some(comparator_reduce (VAnd(v1, v2)))

(* Satisfy any (optional) version constrains. *)
let satisfy_any v1 v2 =
  match v1, v2 with
  | None, None -> None
  | Some _, None -> v1
  | None, Some _ -> v2
  | Some v1, Some v2 -> Some(comparator_reduce (VOr(v1, v2)))

let max v1 v2 =
  if OASISVersion.version_compare v1 v2 >= 0 then v1 else v2

let min v1 v2 =
  if OASISVersion.version_compare v1 v2 <= 0 then v1 else v2

let rec collect_ands = function
  | VAnd(v1, v2) -> let le1, ge1, other1 = collect_ands v1
                    and le2, ge2, other2 = collect_ands v2 in
                    (le1 @ le2, ge1 @ ge2, other1 @ other2)
  | (VLesser _ | VLesserEqual _) as v -> [v], [], []
  | (VGreater _ | VGreaterEqual _ | VEqual _) as v -> [], [v], []
  | v -> [], [], [v]

let rec collect_ors = function
  | VOr(v1, v2) -> let le1, ge1, other1 = collect_ors v1
                   and le2, ge2, other2 = collect_ors v2 in
                   (le1 @ le2, ge1 @ ge2, other1 @ other2)
  | (VLesser _ | VLesserEqual _) as v -> [v], [], []
  | (VGreater _ | VGreaterEqual _ | VEqual _) as v -> [], [v], []
  | v -> [], [], [v]

let rec and_le = function
  | [] -> None
  | [v] -> Some v
  | VLesser v1 :: VLesser v2 :: tl ->
     and_le (VLesser(min v1 v2) :: tl)
  | VLesserEqual v1 :: VLesserEqual v2 :: tl ->
     and_le (VLesserEqual(min v1 v2) :: tl)
  | (VLesserEqual v1 as c1) :: (VLesser v2 as c2) :: tl
  | (VLesser v2 as c2) :: (VLesserEqual v1 as c1) :: tl ->
     let v = if OASISVersion.version_compare v1 v2 < 0 then c1 else c2 in
     and_le (v :: tl)
  | (VEqual v1 as c1) :: c2 :: tl
  | c2 :: (VEqual v1 as c1) :: tl ->
     if OASISVersion.comparator_apply v1 c2 then and_le (c1 :: tl)
     else Some(VAnd(c1, c2)) (* v1 does not satisfy c2, witness of falseness *)
  | _ -> assert false

let rec and_ge = function
  | [] -> None
  | [v] -> Some v
  | VGreater v1 :: VGreater v2 :: tl ->
     and_ge (VGreater(max v1 v2) :: tl)
  | VGreaterEqual v1 :: VGreaterEqual v2 :: tl ->
     and_ge (VGreaterEqual(max v1 v2) :: tl)
  | (VGreaterEqual v1 as c1) :: (VGreater v2 as c2) :: tl
  | (VGreater v2 as c2) :: (VGreaterEqual v1 as c1) :: tl ->
     let v = if OASISVersion.version_compare v1 v2 > 0 then c1 else c2 in
     and_ge (v :: tl)
  | (VEqual v1 as c1) :: c2 :: tl
  | c2 :: (VEqual v1 as c1) :: tl ->
     if OASISVersion.comparator_apply v1 c2 then and_ge (c1 :: tl)
     else Some(VAnd(c1, c2)) (* v1 does not satisfy v2, witness of falseness *)
  | _ -> assert false

let rec or_le = function
  | [] -> None
  | [v] -> Some v
  | VLesser v1 :: VLesser v2 :: tl ->
     or_le (VLesser(max v1 v2) :: tl)
  | VLesserEqual v1 :: VLesserEqual v2 :: tl ->
     or_le (VLesserEqual(max v1 v2) :: tl)
  | (VLesserEqual v1 as c1) :: (VLesser v2 as c2) :: tl
  | (VLesser v2 as c2) :: (VLesserEqual v1 as c1) :: tl ->
     let v = if OASISVersion.version_compare v1 v2 >= 0 then c1 else c2 in
     or_le (v :: tl)
  | (VEqual v1 as c1) :: c2 :: tl
  | c2 :: (VEqual v1 as c1) :: tl ->
     (match or_le (c2 :: tl) with
      | None -> Some c1
      | Some c2 -> if OASISVersion.comparator_apply v1 c2 then Some c2
                   else Some(VOr(c1, c2))) (* v1 does not satisfy c2 *)
  | _ -> assert false

let rec or_ge = function
  | [] -> None
  | [v] -> Some v
  | VGreater v1 :: VGreater v2 :: tl ->
     or_ge (VGreater(min v1 v2) :: tl)
  | VGreaterEqual v1 :: VGreaterEqual v2 :: tl ->
     or_ge (VGreaterEqual(min v1 v2) :: tl)
  | (VGreaterEqual v1 as c1) :: (VGreater v2 as c2) :: tl
  | (VGreater v2 as c2) :: (VGreaterEqual v1 as c1) :: tl ->
     let v = if OASISVersion.version_compare v1 v2 <= 0 then c1 else c2 in
     or_ge (v :: tl)
  | (VEqual v1 as c1) :: c2 :: tl
  | c2 :: (VEqual v1 as c1) :: tl ->
     (match or_ge (c2 :: tl) with
      | None -> Some c1
      | Some c2 -> if OASISVersion.comparator_apply v1 c2 then Some c2
                   else Some(VOr(c1, c2))) (* v1 does not satisfy c2 *)
  | _ -> assert false

let and_other v1 = function
  | [] -> v1
  | v2 :: tl -> VAnd(v1, List.fold_left (fun a v -> VAnd(a, v)) v2 tl)

let or_other v1 = function
  | [] -> v1
  | v2 :: tl -> VOr(v1, List.fold_left (fun a v -> VOr(a, v)) v2 tl)

let dummy_comparator =
  VEqual(OASISVersion.version_of_string "")

(* To be ported back to oasis eventually.
   At least internally, one would benefit from true and false values. *)
let rec comparator_reduce v =
  match v with
  | VAnd _ ->
     let le, ge, other = collect_ands v in
     let other = List.map comparator_reduce other in
     (match and_ge ge, and_le le with
      | Some v1, Some v2 -> and_other (VAnd(v1, v2)) other
      | Some v, None | None, Some v -> and_other v other
      | None, None ->
         (match other with v :: tl -> and_other v tl
                         | [] -> dummy_comparator))
  | VOr _ ->
     let le, ge, other = collect_ors v in
     let other = List.map comparator_reduce other in
     (match or_ge ge, or_le le with
      | Some v1, Some v2 -> or_other (VOr(v1, v2)) other
      | Some v, None | None, Some v -> or_other v other
      | None, None ->
         (match other with v :: tl -> or_other v tl
                         | [] -> dummy_comparator))
  | cmp -> cmp

let rec complement = function
  | VAnd (v1, v2) -> VOr (complement v1, complement v2)
  | VOr (v1, v2) -> VAnd (complement v1, complement v2)
  | VGreater v -> VLesserEqual v
  | VEqual v -> VOr (VLesser v, VGreater v)
  | VLesser v -> VGreaterEqual v
  | VGreaterEqual v -> VLesser v
  | VLesserEqual v -> VGreater v

let string_of_comparator ?var v =
  (* If one wants to print the constraints, a short form is always
     desirable. *)
  let var = match var with
    | None | Some "" -> ""
    | Some p -> String.trim p ^ " " in
  opam_string_of_comparator ~var (comparator_reduce v)

let rec iter_disjunction c f = match c with
  | VOr(c1, c2) -> iter_disjunction c1 f;
                   iter_disjunction c2 f
  | c -> f c


(** Dependency flags. *)
(* The odd rules about the flags is because one can write
   {build & doc & >= "3.14"}
   but its semantics as a _constraint_ (i.e. a predicate to satisfy to
   trigger the action) read
   ((build || doc) && >= "3.14").
   Moreover, an empty list means "true" in that semantic. *)
type dep_flags =
  | Build (** The package (with this constraint) is needed for the build *)
  | Test  (** The package is needed when flag(tests) is true *)
  | Doc   (** dependencies only required to build the package doc. *)

let string_of_dep_flags = function
  | Build -> "build"
  | Test -> "test"
  | Doc -> "doc"

let make_unique_dep_flags (f: dep_flags list) =
  make_unique ~cmp:compare ~merge:(fun f _ -> f) f

let both_dep_flags d1 d2 =
  (* If one of the two dep_flags is empty, it means that the package
     is always needed. *)
  if d1 = [] || d2 = [] then []
  else make_unique_dep_flags (d1 @ d2)

type constraints = {
    dep_flags: dep_flags list; (* [] = always needed *)
    required: bool; (* In the "depends" section.  If not compulsory,
                       it will be put in the "depopts" section and
                       negation of its constraints in "conflicts". *)
    cmp: OASISVersion.comparator option }

let constrain ?(dep=[]) ~required cmp =
  { dep_flags = dep;  required;  cmp }

let no_constraint = { dep_flags = [];  required = true;  cmp = None }

(* No dependency flag & no version constraint. *)
let is_unconstrained c = c.dep_flags = [] && c.cmp = None

let complement_reduce cmp = comparator_reduce (complement cmp)

let constraint_complement c = match c.cmp with
  | None -> None
  | Some cmp -> Some (complement_reduce cmp)

let string_of_constraint c =
  (* It may be that the flags are repeated in the list.  Do not issue
     the opam dependency flag several times. *)
  let dep_flags = make_unique_dep_flags c.dep_flags in
  let dep_flags = List.map string_of_dep_flags dep_flags in
  let s_dep_flags = String.concat " | " dep_flags in
  match c.cmp with
  | None -> s_dep_flags
  | Some cmp ->
     match dep_flags with
     | [] -> string_of_comparator cmp
     | [_] -> s_dep_flags ^ " & " ^ string_of_comparator cmp
     | _ :: _ -> "(" ^ s_dep_flags ^ ") & " ^ string_of_comparator cmp

let satisfy_both_constraints c1 c2 =
  let dep_flags, required =
    match c1.dep_flags, c1.required, c2.dep_flags, c2.required with
    | d1, true, d2, true -> (both_dep_flags d1 d2, true)
    | d, true, d_opt, false | d_opt, false, d, true ->
       (* The dependency that is NOT required has an empty flag list,
          it should not remove the flags for the one in the *)
       let dep = if d_opt = [] then d else both_dep_flags d d_opt in
       (dep, true)
    | d1, false, d2, false -> (both_dep_flags d1 d2, false) in
  { dep_flags;
    required;
    cmp = satisfy_both c1.cmp c2.cmp }
