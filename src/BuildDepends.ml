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

let findlib_with_ocaml =
  let pkg = [ "bigarray"; "camlp4"; "dynlink"; "graphics"; "labltk"; "num";
              "ocamlbuild"; "stdlib"; "str"; "threads"; "unix" ] in
  List.fold_left (fun s e -> S.add e s) S.empty pkg

let add_depends_of_build =
  let findlib deps = function
    | FindlibPackage(p, v) -> (p, v) :: deps
    | InternalLibrary _ -> deps in
  fun d deps -> List.fold_left findlib deps d

let findlib_of_section deps = function
  | Library(_, bs, _)
  | Executable(_, bs, _) -> add_depends_of_build bs.bs_build_depends deps
  | _ -> deps

let rec merge_findlib_depends_sorted = function
  | [_] | [] as deps -> deps
  | (p1, v1) :: (p2, v2) :: tl when p1 = p2 ->
     merge_findlib_depends_sorted ((p1, Version.satisfy_both v1 v2) :: tl)
  | dep :: tl -> dep :: merge_findlib_depends_sorted tl

let merge_findlib_depends d =
  let d = List.sort (fun (p1,_) (p2,_) -> String.compare p1 p2) d in
  merge_findlib_depends_sorted d

let output_depend fh (p, v) =
  (* FIXME: must make a reverse search: findlib package → opam package *)
  fprintf fh "%S " p;
  match v with
  | None -> ()
  | Some v -> fprintf fh "{%s} " (Version.string_of_comparator v)

let output fh pkg =
  let d = List.fold_left findlib_of_section [] pkg.sections in
  let d = merge_findlib_depends d in
  (* filter out the packages coming with OCaml *)
  let d = List.filter (fun (p,_) -> not(S.mem p findlib_with_ocaml)) d in
  let d = ("ocamlfind", None) :: d in
  List.iter (output_depend fh) d
