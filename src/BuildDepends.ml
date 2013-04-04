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
open Utils

module S = Set.Make(String)
module M = Map.Make(String)

let findlib_with_ocaml =
  let pkg = [ "bigarray"; "camlp4"; "dynlink"; "graphics"; "labltk"; "num";
              "ocamlbuild"; "stdlib"; "str"; "threads"; "unix" ] in
  List.fold_left (fun s e -> S.add e s) S.empty pkg

module Opam = struct

  let opam =
    let f_exit_code c =
      if c <> 0 then
        if c = 127 then fatal_error "\"opam\" not found.\n"
        else fatal_error(sprintf "\"opam\" returned the error code %d.\n" c) in
    fun cmd ->
    OASISExec.run_read_one_line
      ~ctxt:!OASISContext.default ~f_exit_code
      "opam" cmd

  let root = opam ["config"; "var"; "root"]

  let space_re = Str.regexp "[ \t\n\r]+"
  let pkg_re = Str.regexp "\\([a-zA-Z0-9_-]+\\)\\.\\(.+\\)\\.opam"
  let findlib_re =
    Str.regexp "\"ocamlfind\" +\"remove\" +\"\\([a-zA-Z0-9_.-]+\\)\""

  let rec add_all_findlib m opam s ofs =
    try
      ignore(Str.search_forward findlib_re s ofs);
      let ofs = Str.match_end() in
      let lib = Str.matched_group 1 s in
      let pkgs_list = try opam :: (M.find lib !m)
                      with Not_found -> [opam] in
      m := M.add lib pkgs_list !m;
      add_all_findlib m opam s ofs
    with Not_found -> ()

  (* Keep only the larger version for each package. *)
  let merge_versions pkgs =
    make_unique ~cmp:(fun (p1,_) (p2,_) -> String.compare p1 p2)
                ~merge:(fun (p, v1) (_, v2) -> (p, Version.max v1 v2))
                pkgs

  (* FIXME: we may want to track versions and add a constraint ">="
     when a findlib package is provided only be later OPAM versions. *)
  let findlib =
    (* FIXME: Cache result? *)
    let m = ref M.empty in
    let dir = Filename.concat root "opam" in
    let pkg_ver = Sys.readdir dir in
    let add pkg_ver =
      if Str.string_match pkg_re pkg_ver 0 then (
        let opam = Str.matched_group 1 pkg_ver in
        let version = Str.matched_group 2 pkg_ver in
        let version = OASISVersion.version_of_string version in

        let s = read_whole_file (Filename.concat dir pkg_ver) in
        let s = Str.global_replace space_re " " s in
        add_all_findlib m (opam, version) s 0;
      ) in
    Array.iter add pkg_ver;
    M.map (fun pkgs -> merge_versions pkgs) !m

  let to_string (p, v) =
    p ^ " (" ^ OASISVersion.string_of_version v ^ ")"

  (* Return the OPAM package containing the findlib module.  See
     https://github.com/OCamlPro/opam/issues/573 *)
  let of_findlib lib =
    try
      let pkgs = M.find lib findlib in
      match pkgs with
      | [] | [_] -> pkgs
      | _ ->
         warn(sprintf "%S provided by OPAM %s."
                      lib (String.concat ", " (List.map to_string pkgs)));
         pkgs
    with Not_found ->
      fatal_error(sprintf "OPAM package for %S not found." lib)

  (* Tells whether the two list of packages are equal.  Do not care
     about versions.  Assume the list are sorted as [of_findlib]
     provides them.  *)
  let rec compare_pkgs p1 p2 = match p1, p2 with
    | [], [] -> 0
    | (p1,_) :: tl1, (p2,_) :: tl2 ->
       let cp = String.compare p1 p2 in
       if cp <> 0 then cp else compare_pkgs tl1 tl2
    | _, [] -> 1
    | [], _ -> -1
end

(* Gather findlib packages
 ***********************************************************************)

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

let merge_findlib_depends =
  let merge (p1, v1, c1) (p2, v2, c2) =
    (p1, Version.satisfy_both v1 v2, c1 @ c2) in
  fun d -> make_unique ~cmp:(fun (p1,_,_) (p2,_,_) -> String.compare p1 p2)
                    ~merge
                    d

(* Compulsory and optional findlib packages
 ***********************************************************************)

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
      (* FIXME: how to eval flags?  See:
         https://github.com/gildor478/oasis2debian/blob/master/src/Expr.ml
         https://github.com/gildor478/oasis2debian/blob/master/src/Arch.ml
       *)
      string_of_bool(OASISExpr.choose (fun _ -> "false") t)
    with Not_found -> "false" in
  OASISExpr.choose eval_tst cond

(* Format OPAM output
 ***********************************************************************)

let string_of_package pkg version =
  match version with
  | None -> sprintf "%S" pkg
  | Some v -> sprintf "%S {%s}" pkg (Version.string_of_comparator v)

let output_packages fh (pkgs, v) =
  match pkgs with
  | [] -> ()
  | [pkg, _version] ->
     output_string fh (string_of_package pkg v);
     output_char fh ' '
  | _ ->
     (* When multiple packages provide a given ocamlfind library, do
        not choose, list them all as possible choices.  *)
     output_char fh '(';
     let pkgs = List.map (fun (p,_) -> string_of_package p v) pkgs in
     output_string fh (String.concat " | " pkgs);
     output_string fh ") "

let output fh pkg =
  let deps = List.fold_left findlib_of_section [] pkg.sections in
  let deps = merge_findlib_depends deps in
  (* filter out the packages coming with OCaml *)
  let deps = List.filter (fun (p,_,_) -> not(S.mem p findlib_with_ocaml)) deps in
  let flags = get_flags pkg.sections in
  let deps, opt = List.partition (fun (_,_,c) -> is_compulsory flags c) deps in

  output_string fh "depends: [ \"ocamlfind\" ";
  let pkgs = List.map (fun (l,v,_) -> (Opam.of_findlib l, v)) deps in
  let pkgs = make_unique
               ~cmp:(fun (p1,_) (p2, _) -> Opam.compare_pkgs p1 p2)
               ~merge:(fun (p, v1) (_, v2) -> (p, Version.satisfy_both v1 v2))
               pkgs in
  List.iter (output_packages fh) pkgs;
  output_string fh "]\n";
  if opt <> [] then (
    (* Optional packages are a simple "or-formula".  Gather all packages. *)
    output_string fh "depopts: [ ";
    let add_pkgs pkgs (l,v,_) =
      List.fold_left (fun pk (p,_) -> (p,v) :: pk) pkgs (Opam.of_findlib l) in
    let pkgs = List.fold_left add_pkgs [] opt in
    let pkgs = make_unique
                 ~cmp:(fun (p1,_) (p2,_) -> String.compare p1 p2)
                 ~merge:(fun (p, v1) (_, v2) -> (p, Version.satisfy_both v1 v2))
                 pkgs in
    List.iter (fun (p,v) -> output_string fh (string_of_package p v)) pkgs;
    output_string fh "]\n";
  )


(* Output findlib libraries provided by several OPAM packages. *)
let output_duplicates fh =
  let output lib pkgs =
    match pkgs with
    | [] | [_] -> ()
    | _ ->
       let pkgs = String.concat ", " (List.map Opam.to_string pkgs) in
       fprintf fh "%s -> %s\n" lib pkgs in
  fprintf fh "Findlib -> multiple OPAM\n";
  M.iter output Opam.findlib;

