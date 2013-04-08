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

let findlib_with_ocaml =
  let pkg = [ "bigarray"; "camlp4"; "dynlink"; "graphics"; "labltk"; "num";
              "ocamlbuild"; "stdlib"; "str"; "threads"; "unix"; "camlp4" ] in
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

  (* Scan the content of [s] and add to [m] all Findlib libraries found. *)
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

  (* Keep the set of versions for each package. *)
  let merge_versions pkgs =
    make_unique ~cmp:(fun (p1,_) (p2,_) -> String.compare p1 p2)
                ~merge:(fun (p, v1) (_, v2) -> (p, Version.Set.union v1 v2))
                pkgs

  let findlib =
    (* Cache the result.  We know we are not up to date if
       "state.cache" is newer than us. *)
    let cache = Filename.concat root "oasis2opam.cache" in
    let m_cache = try (Unix.stat cache).Unix.st_mtime
                  with Unix.Unix_error(Unix.ENOENT,_,_) -> neg_infinity in
    let state = Filename.concat root "state.cache" in
    let m_state = try (Unix.stat state).Unix.st_mtime
                  with Unix.Unix_error(Unix.ENOENT,_,_) -> infinity in
    (* FIXME: when oasis2opam upgrades, the cache must bu updated (the
       type may change). *)
    if m_cache < Conf.compilation_time || m_cache < m_state then (
      (* Need to browse the opam dir again. *)
      let m = ref M.empty in
      let dir = Filename.concat root "opam" in
      let all_pkg_ver = Sys.readdir dir in
      let add pkg_ver =
        if Str.string_match pkg_re pkg_ver 0 then (
          let opam = Str.matched_group 1 pkg_ver in
          let version = Str.matched_group 2 pkg_ver in
          let version = OASISVersion.version_of_string version in

          let s = read_whole_file (Filename.concat dir pkg_ver) in
          let s = Str.global_replace space_re " " s in
          add_all_findlib m (opam, Version.Set.singleton version) s 0;
        ) in
      Array.iter add all_pkg_ver;
      m := M.add "findlib" [("ocamlfind", Version.Set.empty)] !m;
      let m = M.map (fun pkgs -> merge_versions pkgs) !m in
      (* Cache *)
      let fh = open_out_bin cache in
      output_value fh m;
      close_out fh;
      m
    )
    else (
      (* Use the cache. *)
      let fh = open_in_bin cache in
      let m = input_value fh in
      close_in fh;
      m (* will ge given a type by unification with the "then" clause. *)
    )

  (* Return the OPAM package containing the findlib module.  See
     https://github.com/OCamlPro/opam/issues/573 *)
  let of_findlib lib =
    try M.find lib findlib (* <> [] *) with Not_found -> []

  let to_string (p, v) =
    p ^ " (" ^ Version.Set.to_string v ^ ")"

  let of_findlib_warn lib =
    let pkgs = of_findlib lib in
    match pkgs with
    | [_] -> pkgs
    | _ :: _ ->
       warn(sprintf "%S provided by OPAM %s."
                    lib (String.concat ", " (List.map to_string pkgs)));
       pkgs
    | [] ->
       error(sprintf "OPAM package for %S not found." lib);
       [lib, Version.Set.empty]

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
    | FindlibPackage(lib, v) ->
       (* If the Findlib library contains a dot, it is a
          sub-library.  Only keep the main lib. *)
       let lib = try String.sub lib 0 (String.index lib '.')
                 with Not_found -> lib in
       (lib, v, cond) :: deps
    | InternalLibrary _ -> deps in
  List.fold_left findlib deps d

let findlib_of_section deps = function
  | Library(_, bs, _)
  | Executable(_, bs, _) ->
     (* A dep. is compulsory of the lib/exec is built & installed *)
     let cond flags = eval_conditional flags bs.bs_build
                      &&  eval_conditional flags bs.bs_install in
     add_depends_of_build bs.bs_build_depends cond deps
  | _ -> deps

let merge_findlib_depends =
  let merge (p1, v1, c1) (p2, v2, c2) =
    (* A dep. is compulsory as soon as any occurrence of it is. *)
    let cond flags = c1 flags || c2 flags in
    (p1, Version.satisfy_both v1 v2, cond) in
  fun d -> make_unique ~cmp:(fun (p1,_,_) (p2,_,_) -> String.compare p1 p2)
                    ~merge
                    d

(* Findlib libraries on which pkg depends.  Sort them as compulsory
   and optional. *)
let get_findlib_dependencies flags pkg =
  let deps = List.fold_left findlib_of_section [] pkg.sections in
  let deps = merge_findlib_depends deps in
  (* Filter out the packages coming with OCaml *)
  let deps = List.filter (fun (p,_,_) -> not(S.mem p findlib_with_ocaml)) deps in
  (* Only keep only installed packages. *)
  let deps, opt = List.partition (fun (_,_,c) -> c flags) deps in
  deps, opt

(* Findlib Libraries produced by this package. *)
let get_findlib_libraries flags pkg =
  let add_libs libs = function
    | Library(cs,bs,l) ->
       if eval_conditional flags bs.bs_install then (
         (match l.lib_findlib_parent with
          | None -> cs.cs_name
          | Some parent -> parent
         ) :: libs
       )
       else libs
    | _ -> libs in
  let libs = List.fold_left add_libs [] pkg.sections in
  make_unique libs ~cmp:String.compare ~merge:(fun l1 l2 -> l1)


(* Format OPAM output
 ***********************************************************************)

let string_of_package (pkg, version) =
  match version with
  | None -> sprintf "%S" pkg
  | Some v -> sprintf "%S {%s}" pkg (Version.string_of_comparator v)

let string_of_packages (pkgs, v) =
  match pkgs with
  | [] -> ""
  | [pkg, _version] -> string_of_package (pkg, v)
  | _ ->
     (* When multiple packages provide a given ocamlfind library, do
        not choose, list them all as possible choices.  *)
     let pkgs = List.map (fun (p,_) -> string_of_package (p,v)) pkgs in
     "(" ^ (String.concat " | " pkgs) ^ ")"

let output fmt flags pkg =
  let deps, opt = get_findlib_dependencies flags pkg in

  (* Required dependencies. *)
  let pkgs = List.map (fun (l,v,_) -> (Opam.of_findlib_warn l, v)) deps in
  let pkgs = (["ocamlfind", Version.Set.empty], pkg.findlib_version) :: pkgs in
  let pkgs = make_unique
               ~cmp:(fun (p1,_) (p2, _) -> Opam.compare_pkgs p1 p2)
               ~merge:(fun (p, v1) (_, v2) -> (p, Version.satisfy_both v1 v2))
               pkgs in
  Format.fprintf fmt "@[<2>depends: [";
  List.iter (fun p -> Format.fprintf fmt "@\n%s" (string_of_packages p)) pkgs;
  Format.fprintf fmt "@]@\n]@\n";
  if opt <> [] then (
    (* Optional packages are a simple "or-formula".  Gather all packages. *)
    let add_pkgs pkgs (l,v,_) =
      List.fold_left (fun pk (p,_) -> (p,v) :: pk) pkgs (Opam.of_findlib_warn l) in
    let pkgs = List.fold_left add_pkgs [] opt in
    let pkgs = make_unique
                 ~cmp:(fun (p1,_) (p2,_) -> String.compare p1 p2)
                 ~merge:(fun (p, v1) (_, v2) -> (p, Version.satisfy_both v1 v2))
                 pkgs in
    Format.fprintf fmt "@[<2>depopts: [";
    List.iter (fun p -> Format.fprintf fmt "@\n%s" (string_of_package p)) pkgs;
    Format.fprintf fmt "@]@\n]@\n";
  );
  (* Conflicts.  There are other packages (& version in case the
     conflict is removed) which provide the same library. *)
  let libs = get_findlib_libraries flags pkg in
  let add_conflict c lib =
    let pkgs = Opam.of_findlib lib in
    let pkgs = List.filter (fun (p,_) -> p <> pkg.name) pkgs in
    if pkgs <> [] then pkgs @ c
    else c in
  let conflicts = List.fold_left add_conflict [] libs in
  if conflicts <> [] then (
    let conflicts =
      make_unique ~cmp:(fun (p1,_) (p2,_) -> String.compare p1 p2)
                  ~merge:(fun (p1,v1) (p2,v2) -> (p1, Version.Set.union v1 v2))
                  conflicts in
    Format.fprintf fmt "@[<2>conflicts: [";
    List.iter (fun (p,v_set) ->
               let conflict_version v =
                 Format.fprintf fmt "@\n%S {= %S}"
                                p (OASISVersion.string_of_version v) in
               Version.Set.iter conflict_version v_set;
              ) conflicts;
    Format.fprintf fmt "@]@\n]@\n";
  )
;;

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

