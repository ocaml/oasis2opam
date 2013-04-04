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

open Printf
open OASISTypes

module S = Set.Make(String)
module M = Map.Make(String)

let findlib_with_ocaml =
  let pkg = [ "bigarray"; "camlp4"; "dynlink"; "graphics"; "labltk"; "num";
              "ocamlbuild"; "stdlib"; "str"; "threads"; "unix" ] in
  List.fold_left (fun s e -> S.add e s) S.empty pkg

let add_depends_of_build d cond deps =
  let findlib deps = function
    | FindlibPackage(p, v) -> (p, v, cond) :: deps
    | InternalLibrary _ -> deps in
  List.fold_left findlib deps d

let findlib_of_section deps = function
  | Library(_, bs, _)
  | Executable(_, bs, _) ->
     add_depends_of_build bs.bs_build_depends bs.bs_build deps
  | _ -> deps

let rec merge_findlib_depends_sorted = function
  | [_] | [] as deps -> deps
  | (p1, v1, c1) :: (p2, v2, c2) :: tl when p1 = p2 ->
     let v = Version.satisfy_both v1 v2 in
     merge_findlib_depends_sorted ((p1, v, c1 @ c2) :: tl)
  | dep :: tl -> dep :: merge_findlib_depends_sorted tl

let merge_findlib_depends d =
  let d = List.sort (fun (p1,_,_) (p2,_,_) -> String.compare p1 p2) d in
  merge_findlib_depends_sorted d

let opam_of_findlib (p, v) =
  (p, v)

let output_depend fh (p, v,_) =
  (* FIXME: must make a reverse search: findlib package → opam package *)
  fprintf fh "%S " p;
  match v with
  | None -> ()
  | Some v -> fprintf fh "{%s} " (Version.string_of_comparator v)


let get_flags sections =
  let add m = function
    | Flag(cs, f) -> M.add cs.cs_name f.flag_default m
    | _ -> m in
  List.fold_left add M.empty sections

let is_compulsory flags cond =
  (* If any condition returns [true] (i.e. a section with these dep
     must be built), then the dependency is compulsory. *)
  let eval_tst name =
    try
      let t = M.find name flags in
      (* FIXME: how to eval flags? *)
      string_of_bool(OASISExpr.choose (fun _ -> "false") t)
    with Not_found -> "false" in
  OASISExpr.choose eval_tst cond

let output fh pkg =
  let d = List.fold_left findlib_of_section [] pkg.sections in
  let d = merge_findlib_depends d in
  (* filter out the packages coming with OCaml *)
  let d = List.filter (fun (p,_,_) -> not(S.mem p findlib_with_ocaml)) d in
  let flags = get_flags pkg.sections in
  let d, opt = List.partition (fun (_,_,c) -> is_compulsory flags c) d in

  let d = ("ocamlfind", None, []) :: d in
  output_string fh "depends: [ ";
  List.iter (output_depend fh) d;
  output_string fh "]\n";
  if opt <> [] then (
    output_string fh "depopts: [ ";
    List.iter (output_depend fh) opt;
    output_string fh "]\n";
  )
