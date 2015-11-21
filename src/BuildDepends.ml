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

open Printf
open OASISTypes
open Utils

module S = Set.Make(String)

(* Findlib libraries and their corresponding virtual base OPAM package. *)
let opam_base_packages = [ "bigarray", "base-bigarray";
                           "bytes", "base-bytes";
                           "threads", "base-threads";
                           "unix", "base-unix" ]

(* Findlib libraries coming with OCaml — no OPAM package. *)
let findlib_with_ocaml =
  let pkg = [ "dynlink"; "graphics"; "labltk"; "num";
              "ocamlbuild"; "stdlib"; "str"; "compiler-libs" ] in
  List.fold_left (fun s e -> S.add e s) S.empty pkg

let buildtools_with_ocaml =
  let names = [ "ocamllex"; "ocamlyacc" ] in
  List.fold_left (fun s e -> S.add e s) S.empty names

let findlib_for_bytes =
  Some(OASISVersion.(VGreaterEqual(version_of_string "1.5")))

module Opam = struct

  exception Error of int

  (** [opam args] execute opam with the list os arguments [args] and
      return the output. *)
  let opam =
    let f_exit_code c =
      if c <> 0 then
        if c = 127 then fatal_error "\"opam\" not found.\n"
        else raise(Error c) in
    fun cmd ->
    OASISExec.run_read_one_line
      ~ctxt:!OASISContext.default ~f_exit_code
      "opam" cmd

  (** The local directory, root of OPAM installation. *)
  let root =
    try Sys.getenv "OPAMROOT"
    with Not_found ->
      try opam ["config"; "var"; "root"]
      with Error 66 | Error 1 ->
        (* "root" not an opam variable; use same as OpamGlobals.home *)
        let home = try Sys.getenv "HOME"
                   with Not_found -> Sys.getcwd () in
        let r = Filename.concat home ".opam" in
        info(sprintf "Using %s as opam root." r);
        r

  let space_re = Str.regexp "[ \t\n\r]+"
  let pkg_re_1_1 = Str.regexp "^\\([a-zA-Z0-9_-]+\\)\\.\\(.+\\)$"
  let pkg_re_1_0 = Str.regexp "^\\([a-zA-Z0-9_-]+\\)\\.\\(.+\\)\\.opam"
  let findlib_re =
    Str.regexp "\"ocamlfind\" +\"remove\" +\"\\([a-zA-Z0-9_.-]+\\)\""

  let add_is_provided_by m ~findlib ~opam =
    let pkgs_list = try opam :: (M.find findlib !m)
                    with Not_found -> [opam] in
    m := M.add findlib pkgs_list !m

  (* Scan the content of [s] and add to [m] all Findlib libraries found. *)
  let rec add_all_findlib m opam s ofs =
    try
      ignore(Str.search_forward findlib_re s ofs);
      let ofs = Str.match_end() in
      let lib = Str.matched_group 1 s in
      add_is_provided_by m ~findlib:lib ~opam;
      add_all_findlib m opam s ofs
    with Not_found -> ()

  let add_opam_version packages (opam_pkg: string) version =
    let v_set = try Version.Set.add version (M.find opam_pkg !packages)
                with Not_found -> Version.Set.singleton version in
    packages := M.add opam_pkg v_set !packages

  (* Keep the set of versions for each package. *)
  let merge_versions pkgs =
    make_unique ~cmp:(fun (p1,_) (p2,_) -> String.compare p1 p2)
                ~merge:(fun (p, v1) (_, v2) -> (p, Version.Set.union v1 v2))
                pkgs

  let findlib, packages =
    (* Cache the result.  We know we are not up to date if
       "state.cache" is newer than us. *)
    let cache = Filename.concat root "oasis2opam.cache" in
    let m_cache = try (Unix.stat cache).Unix.st_mtime
                  with Unix.Unix_error(Unix.ENOENT,_,_) -> neg_infinity in
    let state = Filename.concat root "state.cache" in
    let m_state = try (Unix.stat state).Unix.st_mtime
                  with Unix.Unix_error(Unix.ENOENT,_,_) -> infinity in
    (* FIXME: when oasis2opam upgrades, the cache must be updated (the
       type may change). *)
    if m_cache < Conf.compilation_time || m_cache < m_state then (
      (* Need to browse the opam dir again. *)
      eprintf "Synchronizing the list of available packages... %!";
      let m = ref M.empty in
      let pkgs = ref M.empty in
      let rec add_of_dir dir =
        if (try Sys.is_directory dir with _ -> false) then
          Array.iter (add_opam dir) (Sys.readdir dir)
      and add_opam dir pkg_ver =
        let fname = Filename.concat dir pkg_ver in
        (* One first checks if an .opam extension is present. *)
        if Str.string_match pkg_re_1_0 pkg_ver 0 then (
          let opam = Str.matched_group 1 pkg_ver in
          let version = Str.matched_group 2 pkg_ver in
          add opam version fname
        )
        else if Str.string_match pkg_re_1_1 pkg_ver 0 then (
          let opam_pkg = Str.matched_group 1 pkg_ver in
          let version = Str.matched_group 2 pkg_ver in
          let findlib_file = Filename.concat fname "findlib" in
          let opam_file = Filename.concat fname "opam" in
          if Sys.file_exists opam_file then (
            if Sys.file_exists findlib_file then (
              (* Use the file "findlib" listing all provided libraries *)
              let version = OASISVersion.version_of_string version in
              let opam = (opam_pkg, Version.Set.singleton version) in
              let s = read_whole_file findlib_file in
              let findlibs = Str.split space_re s in
              List.iter (fun f -> add_is_provided_by m ~findlib:f ~opam) findlibs;
              add_opam_version pkgs opam_pkg version;
            )
            else
              add opam_pkg version opam_file
          )
        )
        else (* OPAM 1.1 stores packages hierarchically.  Recurse. *)
          add_of_dir fname
      and add opam_pkg version opam_file =
        let version = OASISVersion.version_of_string version in
        let s = read_whole_file opam_file in
        let s = Str.global_replace space_re " " s in
        add_all_findlib m (opam_pkg, Version.Set.singleton version) s 0;
        (* We also want to keep a correspondence of OPAM packages →
           versions (even those with no findlib detectable lib) *)
        add_opam_version pkgs opam_pkg version
      in
      add_of_dir (Filename.concat root "repo");
      (* For OPAM < 1.1, the sub-dir "opam" was used: *)
      add_of_dir (Filename.concat root "opam");

      m := M.add "findlib" [("ocamlfind", Version.Set.empty)] !m;
      List.iter (fun (fl, p) -> m := M.add fl [(p, Version.Set.empty)] !m)
                opam_base_packages;
      (* Camlp4 was split out of the standard distribution.  A dummy
         OPAM package was created for older compilersn, thus one can
         consider that all versions of the OPAM "camlp4" package have
         the lib (which is what [Version.Set.empty] means) even though
         it is not detected automatically. *)
      m := M.add "camlp4" ["camlp4", Version.Set.empty] !m;
      let findlib = M.map (fun pkgs -> merge_versions pkgs) !m in
      (* Cache *)
      let to_be_cached = (findlib, !pkgs) in
      let fh = open_out_bin cache in
      output_value fh to_be_cached;
      close_out fh;
      eprintf "done.\n%!";
      to_be_cached
    )
    else (
      (* Use the cache. *)
      let fh = open_in_bin cache in
      let cached = input_value fh in
      close_in fh;
      cached (* will ge given a type by unification with the "then" clause. *)
    )

  (** Return the set of available versions for the OPAM package [pkg]
      or raise [Not_found]. *)
  let package_versions_exn (pkg: string) = M.find pkg packages

  let package_versions pkg =
    try package_versions_exn pkg with Not_found -> Version.Set.empty

  (** Return the OPAM package(s) (with their OPAM versions) containing
      the findlib library [lib].  An empty set of OPAM versions means that
      any version is accepted (for example, versions constraints for
      "camlp4" are set by the compiler).
      See https://github.com/ocaml/opam/issues/573 *)
  let of_findlib (lib: string) =
    try M.find lib findlib (* <> [] *) with Not_found -> []

  let to_string (p, v) =
    p ^ " (" ^ Version.Set.to_string v ^ ")"

  let of_findlib_warn (lib: string) : (string * Version.Set.t) list =
    let pkgs = of_findlib lib in
    match pkgs with
    | [_] -> pkgs
    | _ :: _ ->
       warn(sprintf "%S provided by OPAM %s."
                    lib (String.concat ", " (List.map to_string pkgs)));
       pkgs
    | [] ->
       (* FIXME: do we want to check that there is a version
          satisfying the possible version constraints? *)
       try
         let v_set = package_versions_exn lib in (* or raise Not_found *)
         warn(sprintf "Guessing that OPAM package %S provides Findlib %S."
                      lib lib);
         [lib, v_set]
       with Not_found ->
         error(sprintf "OPAM package %S not found!" lib);
         [lib, Version.Set.empty]

  let of_buildtool_warn (name: string) : (string * Version.Set.t) list =
    try
      let _v_set = package_versions_exn name in (* or raise Not_found *)
      [name, Version.Set.empty]
    with Not_found ->
      warn(sprintf "No OPAM package found for buildtool %S!" name);
      []

  (* Tells whether the two lists of packages are equal.  Do not care
     about versions.  Assume the list are sorted as [of_findlib]
     provides them.  *)
  let rec compare_pkgs p1 p2 = match p1, p2 with
    | [], [] -> 0
    | (p1,_) :: tl1, (p2,_) :: tl2 ->
       let cp = String.compare p1 p2 in
       if cp <> 0 then cp else compare_pkgs tl1 tl2
    | _, [] -> 1
    | [], _ -> -1

  (* Order on packages that consider equal two sets with one included
     in the other.  Otherwise the package set with the smaller name
     not in the other set is declared smaller.
     Assume the lists are sorted in increasing order as [of_findlib]
     provides them.*)
  let rec cmp_diff_pkgs_loop cmp p1s p2s = match p1s, p2s with
    | [], [] -> 0 (* p1s ⊆ p2s or  *)
    | (_::_), [] -> if cmp <= 0 then 0 (* p1s ⊇ p2s *) else cmp
    | [], (_::_) -> if cmp >= 0 then 0 (* p1s ⊆ p2s *) else cmp
    | (n1, _) :: tl1, (n2, _) :: tl2 ->
       let cp = String.compare n1 n2 in
       if cp = 0 then cmp_diff_pkgs_loop cmp tl1 tl2 (* skip common element *)
       else if cp < 0 then
         if cmp <= 0 then cmp_diff_pkgs_loop cp tl1 p2s
         else cmp (* p1\p2 ≠ ∅ and p2\p1 ≠ ∅, first (= smaller) wins *)
       else (* cp > 0 *)
         if cmp >= 0 then cmp_diff_pkgs_loop cp p1s tl2
         else cmp (* p1\p2 ≠ ∅ and p2\p1 ≠ ∅ *)

   let cmp_diff_pkgs p1 p2 = cmp_diff_pkgs_loop 0 p1 p2

  (* Merge two lists of packages, taking only those common to the two
     lists and the versions in both lists.  An empty version set is
     interpreted as "any version is OK".  Assume the list are sorted
     in increasing order as [of_findlib] provides them.  *)
  let rec intersect_pkgs p1 p2 = match p1, p2 with
    | (name1, v1) :: tl1, (name2, v2) :: tl2 ->
       let cp = String.compare name1 name2 in
       if cp < 0 then intersect_pkgs tl1 p2       (* name1 not in p2 *)
       else if cp > 0 then intersect_pkgs p1 tl2  (* name2 not in p1 *)
       else (* name1 = name2 *)
         let v_set = if Version.Set.is_empty v1 then v2
                     else if Version.Set.is_empty v2 then v1
                     else Version.Set.inter v1 v2 in
         (name1, v_set) :: intersect_pkgs tl1 tl2
    | _, [] | [], _ -> []
end

(* Gather findlib packages
 ***********************************************************************)

type dependency = {
    name: dependency_name;
    constraints: Version.constraints;
    compulsory: bool;
  }
 and dependency_name =
   | Lib of OASISTypes.findlib_full
   | BuildTool of OASISTypes.name

let dependency_compare d1 d2 = match d1.name, d2.name with
  | (Lib n1 | BuildTool n1), (Lib n2 | BuildTool n2) -> String.compare n1 n2

(* Assume [dependency_compare d1 d2 = 0]. *)
let dependency_merge d1 d2 =
  (* If any is a library, this takes precedence. *)
  let name = match d1.name, d2.name with
    | BuildTool _, BuildTool _ | Lib _, _ -> d1.name
    | BuildTool _, Lib _ -> d2.name in
  let constraints =
    Version.satisfy_both_constraints d1.constraints d2.constraints in
  (* A dep. is compulsory as soon as any occurrence of it is. *)
  let compulsory = d1.compulsory || d2.compulsory in
  { name;  constraints;  compulsory }

let opam_of_dependency_warn d =
  match d.name with
  | Lib lib -> Opam.of_findlib_warn lib
  | BuildTool name -> Opam.of_buildtool_warn name

let has_flag_test =
  let is_test = function
    | (OASISExpr.EFlag "tests", _) -> true
    | _ -> false in
  fun choices -> List.exists is_test choices

let is_buildtool_in_section flags name = function
  | Library(_, bs, _)
  | Executable(_, bs, _) ->
     let is_built = eval_conditional flags bs.bs_build in
     is_built && List.mem (InternalExecutable name) bs.bs_build_tools
  | _ -> false

(* Check whether [name] is an internal executable used as a buildtool
   in another section. *)
let is_buildtool flags pkg name =
  List.exists (is_buildtool_in_section flags name) pkg.sections


let findlib_of_section_gen flags pkg deps cs bs ~executable ~kind =
  (* A dep. is compulsory of the lib/exec is built, regardless of
     whether it is installed or not (we need the resources to perform
     the compilation).  In some rare cases one may want an executable
     for internal purposes only. *)
  let is_built = eval_conditional flags bs.bs_build
  and is_installed = eval_conditional flags bs.bs_install in
  if is_built && not is_installed && not(is_buildtool flags pkg cs.cs_name) then
    (* If the executable is used as a "BuildTools" in another section
       that is built, no need to issue a warning. *)
    warn (sprintf "Section %S is built but not installed (missing Build$ \
                   flag of BuildTools declaration?)" cs.cs_name);
  let findlib_depends deps = function
    | FindlibPackage(lib, v) ->
       (* If the Findlib library contains a dot, it is a
             sub-library.  Only keep the main lib. *)
       let lib = try String.sub lib 0 (String.index lib '.')
                 with Not_found -> lib in
       let kind = if has_flag_test bs.bs_build then Version.Test
                  else if is_built && not is_installed then Version.Build
                  else kind in
       { name = Lib lib;
         constraints = Version.constrain v ~kind;
         compulsory = is_built } :: deps
    | InternalLibrary _ -> deps in
  let findlib_tools deps = function
    | ExternalTool name ->
       { name = BuildTool name;
         constraints = Version.constrain None ~kind:Version.Build;
         compulsory = is_built } :: deps
    | InternalExecutable _ -> deps in
  let deps = List.fold_left findlib_depends deps bs.bs_build_depends in
  let deps = List.fold_left findlib_tools deps bs.bs_build_tools in
  deps

let dependencies_of_section flags pkg deps = function
  | Library(cs, bs, _) ->
     findlib_of_section_gen
       flags pkg deps cs bs ~executable:false ~kind:Version.Required
  | Executable(cs, bs, _) ->
     (* Dependencies for executables are "build" only (i.e. their
        change does not trigger a recompilation of the executable).
        FIXME: Is this smart? (fixed bug in a library) *)
     findlib_of_section_gen
       flags pkg deps cs bs ~executable:true ~kind:Version.Build
  | _ -> deps

let merge_dependencies =
  fun d -> make_unique ~cmp:dependency_compare
                     ~merge:dependency_merge
                     d

let get_all_dependencies flags pkg =
  let deps =
    List.fold_left (dependencies_of_section flags pkg) [] pkg.sections in
  let deps = merge_dependencies deps in
  (* Distinguish findlib packages that are going to be installed from
     optional ones. *)
  let group1 d = Version.is_test d.constraints || d.compulsory in
  let deps_opt = List.partition group1 deps in
  deps_opt

(** Tell whether "compiler-libs" is a mandatory dependency. *)
let on_compiler_libs flags pkg =
  let deps, _ = get_all_dependencies flags pkg in
  List.exists (fun d -> d.name = Lib "compiler-libs") deps

(** [get_dependencies flags pkg] return the Findlib libraries and build tools
    on which [pkg] depends.  Sort them as compulsory and optional. *)
let does_not_come_with_OCaml d = match d.name with
  | Lib lib -> not(S.mem lib findlib_with_ocaml)
  | BuildTool name -> not(S.mem name buildtools_with_ocaml)
let get_dependencies flags pkg =
  let deps, opt = get_all_dependencies flags pkg in
  (* Filter out the packages coming with OCaml *)
  (List.filter does_not_come_with_OCaml deps,
   List.filter does_not_come_with_OCaml opt)

(* Findlib Libraries produced by this package. *)
let get_findlib_libraries flags pkg =
  let add_libs libs = function
    | Library(cs,bs,l) ->
       if eval_conditional flags bs.bs_install then (
         (match l.lib_findlib_parent with
          | Some parent ->
             (* This may point to the *internal name* (to oasis) of
                the Findlib library.  The parent is must already be
                present, so do nothing. *)
             libs
          | None -> (match l.lib_findlib_name with
                    | Some name -> name :: libs
                    | None -> cs.cs_name :: libs)
         )
       )
       else libs
    | _ -> libs in
  let libs = List.fold_left add_libs [] pkg.sections in
  make_unique libs ~cmp:String.compare ~merge:(fun l1 l2 -> l1)


(* Format OPAM output
 ***********************************************************************)

(* Simplify ⋀ (⋁ OPAM packages, version constraint). *)
let simplify_packages =
  (* When a set of packages A is included in A ∪ B, the formula
     A ∧ (A ∨ B) is equivalent to A = A ∩ (A ∪ B). *)
  let cmp (p1,_) (p2,_) = Opam.cmp_diff_pkgs p1 p2 in
  (* List of dependencies repeated => satisfy both constraints. *)
  let merge (p1, v1) (p2, v2) =
    (Opam.intersect_pkgs p1 p2, Version.satisfy_both_constraints v1 v2) in
  fun pkgs ->
  let pkgs = List.filter (fun (p, _) -> p <> []) pkgs in
  make_unique ~cmp ~merge pkgs

type suitable_opam =
  | Only_findlib (* No OPAM package satisfying the constraints was found *)
  | Opam (* Suitable OPAM packages were found *)

(* Add to [l] the packages [name] with possible version constraints if
   not all package versions are present.  It is assumed that the oasis
   versions and the OPAM ones coincide. *)
let add_suitable_pakages pkg_oasis_constraint (name, versions) l =
  let opam_versions = Opam.package_versions name in
  if Version.Set.is_empty opam_versions then
    (* The package does not exists in OPAM.  Maybe it was not yet
       added.  Just add the name with the oasis constraint *)
    (Only_findlib, name, pkg_oasis_constraint) :: l
  else (
    let latest = Version.Set.max_elt opam_versions in
    match pkg_oasis_constraint.Version.cmp with
    | None ->
       if Version.Set.equal versions opam_versions
          || Version.Set.is_empty versions then
         (* All OPAM versions have the library (an empty set of versions
            means anything is accepted).  One expects it will still be
            the case in the future, so no version constraint. *)
         let c = { pkg_oasis_constraint with Version.cmp = None } in
         (Opam, name, c) :: l
       else
         (* Only some OPAM versions have the lib.  If the [latest] is
            among them, then assume it will remain the case in the future. *)
         let add v l =
           let open OASISVersion in
           let cmp = if version_compare v latest = 0 then VGreaterEqual v
                     else VEqual v in
           let c = { pkg_oasis_constraint with Version.cmp = Some cmp } in
           (Opam, name, c) :: l in
         Version.Set.fold add versions l
    | Some pkg_oasis_version ->
       (* Only keep the versions that satisfy the constraint. *)
       let satisfy_constraint v =
         OASISVersion.comparator_apply v pkg_oasis_version in
       let good_versions = Version.Set.filter satisfy_constraint versions in
       (* If the latest version is in the set providing the library, then
          we assume that the [pkg_oasis_version] is the only constraint
          the author wants.  Otherwise, the package may have evolved
          and we only add the [good_versions]. *)
       if Version.Set.mem latest good_versions
          || Version.Set.is_empty versions then
         (Opam, name, pkg_oasis_constraint) :: l
       else if Version.Set.is_empty good_versions then
         (Only_findlib, name, pkg_oasis_constraint) :: l
       else
         let add v l =
           let c = { pkg_oasis_constraint with
                     Version.cmp = Some(OASISVersion.VEqual v) } in
           (Opam, name, c) :: l in
         Version.Set.fold add good_versions l
  )

(* [pkgs] are all the OPAM packages and versions providing a given
   findlib library. *)
let constrain_opam_package (pkgs, pkg_oasis_version) =
  let p = List.fold_right (add_suitable_pakages pkg_oasis_version) pkgs [] in
  let p =
    if List.for_all (fun (d,_,_) -> d = Only_findlib) p then p
    else
      (* Suitable OPAM packages exist, remove the ones guessed from findlib. *)
      List.filter (fun (d,_,_) -> d <> Only_findlib) p in
  List.map (fun (_, p, v) -> (p, v)) p

let constrain_opam_packages pkgs =
  List.map constrain_opam_package pkgs

let strings_of_packages =
  let to_string (name, v_constraint) =
    if Version.is_unconstrained v_constraint then
      sprintf "%S" name
    else
      sprintf "%S {%s}" name (Version.string_of_constraint v_constraint) in
  fun p -> List.map to_string p


let string_of_packages pkgs_version =
  match strings_of_packages pkgs_version with
  | [] -> ""
  | [pkg] -> pkg
  | pkgs ->
     (* When multiple packages (or multiple package versions) provide
        a given ocamlfind library, do not choose, list them all as
        possible choices.  *)
     "(" ^ (String.concat " | " pkgs) ^ ")"


let output t fmt flags =
  let pkg = Tarball.oasis t in
  let deps, opt = get_dependencies flags pkg in

  (* Required dependencies. *)
  let pkgs = List.map (fun d -> (opam_of_dependency_warn d, d.constraints))
                      deps in
  let pkgs =
    let v = if List.exists (fun d -> d.name = Lib "bytes") deps then
              Version.satisfy_both pkg.findlib_version findlib_for_bytes
            else pkg.findlib_version in
    let c = Version.(constrain v ~kind:Build) in
    (["ocamlfind", Version.Set.empty], c) :: pkgs in
  let pkgs = if Tarball.needs_oasis t then
               let v = OASISVersion.VGreaterEqual pkg.oasis_version in
               let c = Version.constrain (Some v) ~kind:Version.Build in
               (Opam.of_findlib "oasis", c) :: pkgs
             else pkgs in
  let pkgs = simplify_packages pkgs in
  let pkgs = constrain_opam_packages pkgs in
  Format.fprintf fmt "@[<2>depends: [";
  List.iter (fun p -> Format.fprintf fmt "@\n%s" (string_of_packages p)) pkgs;
  let opam_depends = Tarball.opam_depends t in
  if opam_depends <> "" then (
    Format.fprintf fmt "@\n# Included from _opam file@\n%s" opam_depends
  );
  Format.fprintf fmt "@]@\n]@\n";
  (* Optional packages are a simple "or-formula".  Gather all packages
     individually (but use the same data-structure as above). *)
  let add_pkgs pkgs d =
    List.fold_left (fun pk p -> ([p], d.constraints) :: pk)
                   pkgs (opam_of_dependency_warn d) in
  let opt_pkgs = List.fold_left add_pkgs [] opt in
  let opt_pkgs = simplify_packages opt_pkgs in
  let opt_pkgs = constrain_opam_packages opt_pkgs in
  if opt_pkgs <> [] then (
    Format.fprintf fmt "@[<2>depopts: [";
    (* Optional dependencies do not allow version constraints. *)
    List.iter (function
                | (p0, _) :: tl ->
                   Format.fprintf fmt "@\n%S" p0;
                   assert(List.for_all (fun (p,_) -> p = p0) tl)
                | [] -> ()) opt_pkgs;
    Format.fprintf fmt "@]@\n]@\n";
  );
  let opt_conflicts =
    let add_cplt acc (p, v) = (p, Version.constraint_complement v) :: acc in
    List.fold_left (fun accum l -> List.fold_left add_cplt accum l)
                   [] opt_pkgs in
  (* Conflicts.  There are other packages (& version in case the
     conflict is or will be removed) which provide the same library. *)
  let libs = get_findlib_libraries flags pkg in
  let add_conflict c lib =
    let pkgs = Opam.of_findlib lib in
    let pkgs = List.filter (fun (p,_) -> p <> pkg.name) pkgs in
    (* Transform the set of versions into a constraint formula. *)
    let any_version v acc =
      Version.satisfy_any (Some (OASISVersion.VEqual v)) acc in
    let to_constraint v_set = Version.Set.fold any_version v_set None in
    let pkgs = List.map (fun (p, v_set) -> p, to_constraint v_set) pkgs in
    pkgs @ c in
  let conflicts = List.fold_left add_conflict opt_conflicts libs in
  let conflicts = List.filter (fun (_, v) -> v <> None) conflicts in
  if conflicts <> [] then (
    let conflicts =
      make_unique ~cmp:(fun (p1,_) (p2,_) -> String.compare p1 p2)
                  ~merge:(fun (p1,v1) (p2,v2) -> (p1, Version.satisfy_any v1 v2))
                  conflicts in
    (* Simplify the constraints. *)
    let conflicts =
      List.map (function
                | (_, None) as p -> p
                | (p, Some c) -> (p, Some(Version.comparator_reduce c))
               ) conflicts in
    Format.fprintf fmt "@[<2>conflicts: [";
    let print p v =
      Format.fprintf fmt "@\n%S {%s}" p (Version.string_of_comparator v) in
    let add_conflicts = function
      | p, Some v -> Version.iter_disjunction v (print p)
      | _, None -> () in
    List.iter add_conflicts conflicts;
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
  M.iter output Opam.findlib
