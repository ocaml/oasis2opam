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

(* Findlib libraries coming with OCaml — no OPAM package. *)
let findlib_with_ocaml =
  let pkg = [ "dynlink"; "graphics"; "labltk";
              "stdlib"; "str"; "compiler-libs" ] in
  List.fold_left (fun s e -> S.add e s) S.empty pkg

let buildtools_with_ocaml =
  let names = [ "ocamllex"; "ocamlyacc"; "ocamldoc" ] in
  List.fold_left (fun s e -> S.add e s) S.empty names

let findlib_for_bytes =
  Some(OASISVersion.(VGreaterEqual(version_of_string "1.5")))

(* We need a version that supports the "-C" flag. *)
let min_oasis_version =
  OASISVersion.version_of_string "0.4.7"

(* Gather findlib packages
 ***********************************************************************)

type dependency = {
    name: dependency_name;
    constraints: Version.constraints;
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
  { name;  constraints }

let opam_of_dependency_warn d =
  match d.name with
  | Lib lib -> Opam.of_findlib_warn lib
  | BuildTool name -> Opam.of_buildtool_warn name

(* Check whether it _may_ be triggered when "flag(tests)" is true (the
   real trigger may require other flags to be set).  Thus, when this
   function returns [false], it means that, even when flag(tests) is
   true, no value of the other flags may trigger the condition.
   One must have:
   eval_conditional flags c ~tests:true ⇒ may_be_triggered_flag_tests c
 *)
let may_be_triggered_flag_tests =
  (* Transport negations on atoms. *)
  let rec tests_triggered ~neg = function
    | OASISExpr.EFlag flag -> flag = "tests" && not neg
    | OASISExpr.ENot t -> tests_triggered t ~neg:(not neg)
    | OASISExpr.EAnd(t1, t2) | OASISExpr.EOr(t1, t2) ->
       (* Suffice to the present in at least one of the clauses. *)
       tests_triggered t1 ~neg || tests_triggered t2 ~neg
    | OASISExpr.EBool _ | OASISExpr.ETest _ -> false in
  fun (choices: _ OASISExpr.choices) ->
  List.exists (fun (t, b) -> b && tests_triggered t ~neg:false) choices

type oasis_flags = bool OASISExpr.choices StringMap.t

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

let findlib_of_tools ~dep ~required deps = function
  | ExternalTool name ->
     { name = BuildTool name;
       constraints = Version.constrain None ~dep ~required } :: deps
  | InternalExecutable _ -> deps


let findlib_of_section_gen flags pkg deps cs bs ~executable =
  (* A dep. is compulsory if the lib/exec is built, regardless of
     whether it is installed or not (we need the resources to perform
     the compilation).  In some rare cases one may want an executable
     for internal purposes only. *)
  let is_built = eval_conditional flags bs.bs_build ~tests:false
  and is_built_tests = eval_conditional flags bs.bs_build ~tests:true
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
       let dep = if may_be_triggered_flag_tests bs.bs_build then [Version.Test]
                 else [] in
       (* Dependencies for executables are never required but at most
          "build" (i.e. their change does not trigger a recompilation
          of the executable). *)
       let dep = if is_built && (executable || not is_installed) then
                   Version.Build :: dep
                 else dep in
       let required = if executable then dep <> []
                      else is_built_tests in
       let constraints = Version.constrain v ~dep ~required in
       { name = Lib lib;  constraints } :: deps
    | InternalLibrary _ -> deps in
  let findlib_tools = findlib_of_tools ~dep:[Version.Build]
                        ~required:is_built_tests in
  let deps = List.fold_left findlib_depends deps bs.bs_build_depends in
  let deps = List.fold_left findlib_tools deps bs.bs_build_tools in
  deps

let dependencies_of_section flags pkg deps = function
  | Library(cs, bs, _) ->
     findlib_of_section_gen flags pkg deps cs bs ~executable:false
  | Executable(cs, bs, _) ->
     findlib_of_section_gen flags pkg deps cs bs ~executable:true
  | Test(_, tst) ->
     (* Deps in the "depends" section ⇒ required. *)
     List.fold_left (findlib_of_tools ~dep:[Version.Test] ~required:true)
       deps tst.test_tools
  | Doc(_, doc) ->
     List.fold_left (findlib_of_tools ~dep:[Version.Doc] ~required:true)
       deps doc.doc_build_tools
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
  let is_required d = d.constraints.Version.required in
  let deps_opt = List.partition is_required deps in
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
let provided_findlib_libraries flags pkg =
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

(* Simplify as form: ⋀ (⋁ OPAM packages, version constraint). *)
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
    let c = Version.(constrain v ~dep:[Build; Test; Doc] ~required:true) in
    (["ocamlfind", Version.Set.empty], c) :: pkgs in
  let pkgs = if Tarball.needs_oasis t then
               let v = Version.max pkg.oasis_version min_oasis_version in
               let v = OASISVersion.VGreaterEqual v in
               let c = Version.(constrain (Some v) ~dep:[Build; Test; Doc]
                                                   ~required:true) in
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
  let libs = provided_findlib_libraries flags pkg in
  let add_conflict c lib =
    let pkgs = Opam.of_findlib lib in
    let pkgs = List.filter (fun (p,_) -> p <> pkg.OASISTypes.name) pkgs in
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
  StringMap.iter output Opam.findlib
