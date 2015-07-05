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

type kind = Required | Build | Test

type constraints = { kind: kind;
                     cmp: OASISVersion.comparator option }

let constrain ?(kind=Required) cmp = { kind; cmp }

let no_constraint = { kind = Required;  cmp = None }

let is_unconstrained c = c.kind = Required && c.cmp = None

let is_test c = c.kind = Test

let complement_reduce cmp = comparator_reduce (complement cmp)

let constraint_complement c = match c.cmp with
  | None -> None
  | Some cmp -> Some (complement_reduce cmp)

let string_of_constraint c =
  match c.kind, c.cmp with
  | Required, None -> ""
  | Required, Some cmp -> string_of_comparator cmp
  | Build, None -> "build"
  | Build, Some cmp -> "build & " ^ string_of_comparator cmp
  | Test, None -> "test"
  | Test, Some cmp -> "test & " ^ string_of_comparator cmp

let satisfy_both_constraints c1 c2 =
  let kind = match c1.kind, c2.kind with
    | _, Required | Required, _ -> Required
    | Build, (Build|Test) | Test, Build -> Build (* Build comes before test *)
    | Test, Test -> Test in
  { kind;
    cmp = satisfy_both c1.cmp c2.cmp }
