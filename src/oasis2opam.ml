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

(* Important OPAM versions to compare to. *)
let v1_2 = OASISVersion.version_of_string "1.2"


(* Write OPAM "descr" & "url" files.
 ***********************************************************************)
let opam_descr t =
  let pkg = Tarball.oasis t in
  let fh = open_out(Filename.concat (Tarball.pkg_opam_dir t) "descr") in
  output_string fh pkg.synopsis;
  output_char fh '\n';
  (match pkg.description with
   | Some d -> output_wrapped fh d; output_char fh '\n'
   | None -> warn "Consider adding \"Description:\" to your _oasis file");
  close_out fh

let opam_url t =
  let url = Tarball.url t
  and md5 = Tarball.md5 t in
  if url <> "" || md5 <> "" then (
    let fh = open_out(Filename.concat (Tarball.pkg_opam_dir t) "url") in
    fprintf fh "archive: %S\nchecksum: %S\n" url md5;
    close_out fh
  )

(* Write OPAM "opam" file.
 ***********************************************************************)

let email_re = Str.regexp "[0-9a-zA-Z.]+@[0-9a-zA-Z.]+"

(* From a list of names (possibly with email), get the first email. *)
let rec get_first_email = function
  | [] -> None
  | name :: tl ->
     try ignore(Str.search_forward email_re name 0);
         Some(name)
     with Not_found -> get_first_email tl

let output_maintainer fmt pkg =
  if pkg.maintainers = [] then
    warn "Consider adding \"Maintainers:\" to your _oasis file.";
  match get_first_email pkg.maintainers with
  | Some e -> Format.fprintf fmt "maintainer: \"%s\"@\n" (Utils.escaped e)
  | None ->
     match get_first_email pkg.authors with
     | Some e -> Format.fprintf fmt "maintainer: \"%s\"@\n" (Utils.escaped e)
     | None ->
        error "Give an email in oasis \"Maintainers:\" or \
               \"Authors:\" fields (preferably the former).";
        Format.fprintf fmt "maintainer: \"opam-devel@@lists.ocaml.org\"@\n"

let output_authors fmt pkg =
  match pkg.authors with
  | [] -> fatal_error "You must set \"Authors:\" in your _oasis file.";
  | [a] -> Format.fprintf fmt "authors: [ \"%s\" ]@\n" (Utils.escaped a)
  | a :: tl ->
     Format.fprintf fmt "@[<11>authors: [ \"%s\"" (Utils.escaped a);
     List.iter (fun a -> Format.fprintf fmt "@\n\"%s\"" (Utils.escaped a)) tl;
     Format.fprintf fmt " ]@]@\n"

let clib_re = Str.regexp "-l\\([a-zA-Z0-9]+\\)"

let get_potential_clibs pkg =
  let add_libs libs = function
    | Library(_,bs,_)
    | Object(_,bs,_)
    | Executable(_,bs,_) ->
       (* Get all potential C libs, independently of the flags *)
       let args = List.flatten (List.map snd bs.bs_cclib) in
       let add_lib libs arg =
         if Str.string_match clib_re arg 0 then
           Str.matched_group 1 arg :: libs
         else libs in
       List.fold_left add_lib libs args
    | Flag _ | SrcRepo _ | Test _ | Doc _ -> libs in
  let libs = List.fold_left add_libs [] pkg.sections in
  make_unique ~cmp:String.compare ~merge:(fun l1 l2 -> l1) libs


let output_tags fmt pkg =
  let tag cat =
    (* FIXME: how do we generate tags from categories? *)
    Filename.basename cat in
  let tags = List.map tag pkg.categories in
  (* Add "clib:<lib>" to tags for each C library detected.  This
     serves as an indication that these libraries may need to be
     installed first. *)
  let libs = get_potential_clibs pkg in
  let tags = List.fold_left (fun t l -> ("clib:" ^ l) :: t) tags libs in
  if tags <> [] then (
    Format.fprintf fmt "tags: [ @[";
    List.iter (fun t -> Format.fprintf fmt "\"%s\"@ " (Utils.escaped t)) tags;
    Format.fprintf fmt "@] ]@\n";
  )

(* Return an associative list of flags which match findlib packages
   and OPAM packages providing them (without version number).  *)
let opam_for_flags flags =
  let add_findlib n _ l =
    match BuildDepends.Opam.of_findlib n with
    | [] -> l
    | pkgs ->
       let pkgs = make_unique (List.map fst pkgs)
                              ~cmp:String.compare ~merge:(fun p _ -> p) in
       (n, pkgs) :: l in
  M.fold add_findlib flags []

let output_build_install t fmt flags opam_file_version =
  let pkg = Tarball.oasis t in
  Format.fprintf fmt "@[<2>build: [@\n";
  if not(Tarball.setup_ml_exists t) then
    Format.fprintf fmt "[\"oasis\" \"setup\"]@\n";
  (* FIXME: convention: if a flag is the name of a Findlib package,
     enable it only if the corresponding OPAM package is installed. *)
  Format.fprintf fmt "@[<2>[\"ocaml\" \"setup.ml\" \"-configure\" \
                      \"--prefix\" prefix";
  let flag_enable (flag, pkgs) =
    match pkgs with
    | [] -> ()
    | [p] -> Format.fprintf fmt "@\n\"--%%{%s:enable}%%-%s\"" p flag
    | p0 :: pkgs ->
       let open Format in
       fprintf fmt "@\n\"--enable-%s\" @[{ \"%%{%s:installed}%%\"" flag p0;
       List.iter (fun p -> fprintf fmt "@ | \"%%{%s:installed}%%\"" p) pkgs;
       fprintf fmt " }@]" in
  List.iter flag_enable (opam_for_flags flags);
  Format.fprintf fmt "@]]@\n\
                      [\"ocaml\" \"setup.ml\" \"-build\"]";
  if OASISVersion.version_compare opam_file_version v1_2 >= 0 then (
    (* OPAM 1.2 has an "install" field *)
    Format.fprintf fmt "@]@\n]@\n\
                        install: [\"ocaml\" \"setup.ml\" \"-install\"]@\n";
    (* Build for testing. *)
    Format.fprintf fmt "@[<2>build-test: [@\n\
                        @[<2>[\"ocaml\" \"setup.ml\" \"-configure\" \
                        \"--enable-tests\"";
    List.iter flag_enable (opam_for_flags flags);
    Format.fprintf fmt "@]]@\n\
                        [\"ocaml\" \"setup.ml\" \"-build\"]@\n\
                        [\"ocaml\" \"setup.ml\" \"-test\"]\
                        @]@\n]@\n";
    (* Build documentation. *)
    Format.fprintf fmt "build-doc: [\"ocaml\" \"setup.ml\" \"-doc\"]@\n";
  )
  else
    Format.fprintf fmt "@\n[\"ocaml\" \"setup.ml\" \"-install\"]\
                        @]@\n]@\n";
  let libs = BuildDepends.get_findlib_libraries flags pkg in
  if libs <> [] then (
    Format.fprintf fmt "@[<2>remove: [";
    List.iter (fun l -> Format.fprintf fmt "@\n[\"ocamlfind\" \"remove\" %S]" l
              ) libs;
    Format.fprintf fmt "@]@\n]@\n";
    if OASISVersion.version_compare opam_file_version v1_2 > 0 then (
      Format.fprintf fmt "@[<2>libraries: [";
      List.iter (fun l -> Format.fprintf fmt "@\n%S" l) libs;
      Format.fprintf fmt "@]@\n]@\n";
    );
  )


let opam_opam t flags opam_file_version =
  let pkg = Tarball.oasis t in
  let fh = open_out(Filename.concat (Tarball.pkg_opam_dir t) "opam") in
  let fmt = Format.formatter_of_out_channel fh in
  Format.fprintf fmt "opam-version: \"%s\"@\n"
                 (OASISVersion.string_of_version opam_file_version);
  Format.fprintf fmt "name: \"%s\"@\n" pkg.name;
  Format.fprintf fmt "version: \"%s\"@\n"
                 (OASISVersion.string_of_version pkg.version);
  output_maintainer fmt pkg;
  output_authors fmt pkg;
  let prefix =
    if OASISVersion.version_compare opam_file_version v1_2 < 0 then "# "
    else "" in
  Format.fprintf fmt "%slicense: %S@\n"
                 prefix (OASISLicense.to_string pkg.license);
  (match pkg.homepage with
   | Some url -> Format.fprintf fmt "%shomepage: %S@\n" prefix url
   | None -> warn "Consider adding \"Homepage:\" to your _oasis file");
  (* Source repository *)
  let get_source_repository = function
    | SrcRepo (_, src) -> Some src
    | _ -> None in
  let source_repository =
    match Utils.map_find pkg.sections get_source_repository with
    | Some src ->
       Format.fprintf fmt "%sdev-repo: %S@\n" prefix src.src_repo_location;
       Some src.src_repo_location
   | None -> None in
  (* Bug reports URL — with Github heuristics *)
  let bugreports =
    match pkg.homepage, source_repository with
    | Some url, _ when start_with url "https://github.com" ->
       Some(url_concat url "issues")
    | _, Some url when start_with url "https://github.com" ->
       Some(url_concat (Filename.chop_extension url) "issues")
    | _ -> None in
  (match bugreports with
   | Some url -> Format.fprintf fmt "%sbug-reports: %S@\n" prefix url
   | None ->
      (* See https://github.com/ocaml/oasis/pull/62 *)
      (* warn "Consider adding \"BugReports:\" to your _oasis file" *)
      ());
  output_tags fmt pkg;
  output_build_install t fmt flags opam_file_version;
  if List.exists (function Doc _ -> true | _  -> false) pkg.sections then
    Format.fprintf fmt "build-doc: [ \"ocaml\" \"setup.ml\" \"-doc\" ]@\n";
  BuildDepends.output t fmt flags;
  (* OCaml version *)
  let compiler_libs_version =
    if BuildDepends.on_compiler_libs flags pkg then
      Some(OASISVersion.comparator_of_string ">= 4.00.1")
    else None in
  (match Version.satisfy_both pkg.ocaml_version compiler_libs_version with
   | Some v ->
      let v = Version.comparator_reduce v in
      if OASISVersion.version_compare opam_file_version v1_2 < 0 then
        Format.fprintf fmt "ocaml-version: [ %s ]@\n"
                       (Version.string_of_comparator v)
      else
        Format.fprintf fmt "available: [ ocaml-version %s ]@\n"
                       (Version.string_of_comparator v)
   | None -> ());
  (* If an _opam file (say with "depexts") exists in the archive, append it. *)
  let opam = Tarball.opam t in
  if opam <> "" then (
    info "The _opam file found in tarball has been appended to opam.";
    Format.fprintf fmt "%s\n" opam
  );
  Format.pp_print_flush fmt ();
  close_out fh

let opam_findlib t flags =
  let pkg = Tarball.oasis t in
  let libs = BuildDepends.get_findlib_libraries flags pkg in
  if libs <> [] then (
    let fh = open_out(Filename.concat (Tarball.pkg_opam_dir t) "findlib") in
    (* One findlib package per line *)
    List.iter (fun l -> output_string fh l; output_char fh '\n') libs;
    close_out fh
  )

let opam_install t flags =
  let pkg = Tarball.oasis t in
  (* FIXME: This is extremely naive.  This functionality should be in
     an oasis plugin.  Until then, we can live with this... *)
  let gather_exec bins = function
    | Executable(cs, bs, es) ->
       if eval_conditional flags bs.bs_install then (
         (* Binary that will be installed *)
         let path = Filename.concat "_build" bs.bs_path in
         let exec = try Filename.chop_extension es.exec_main_is
                    with _ -> es.exec_main_is in
         let exec = Filename.concat path exec in
         (exec, cs.cs_name) :: bins
       )
       else bins
    | _ -> bins (* skip other sections *) in
  let bins = List.fold_left gather_exec [] pkg.sections in

  if bins <> [] then (
    let dir = Filename.concat (Tarball.pkg_opam_dir t) "files" in
    (try Unix.mkdir dir 0o777 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
    let fh = open_out(Filename.concat dir (pkg.name ^ ".install")) in

    output_string fh "bin: [\n";
    let output_bin (exec, name) =
      let exec = Utils.escaped exec in
      let name = Utils.escaped name in
      fprintf fh "  \"?%s.byte\" {\"%s\"}\n" exec name;
      fprintf fh "  \"?%s.native\" {\"%s\"}\n" exec name;
    in
    List.iter output_bin bins;
    output_string fh "]\n";

    close_out fh
  )


let () =
  OASISBuiltinPlugins.init ();
  let local = ref false in
  let version = ref false in
  let duplicates = ref false in
  let query_findlib = ref "" in
  let opam_file_version = ref "1.2" in
  let specs = [
    "--local", Arg.Set local,
    " create an opam dir for the _oasis in the current dir";
    "--duplicates", Arg.Set duplicates,
    " output a list of packages providing the same ocamlfind library";
    "--query", Arg.Set_string query_findlib,
    "LIB return the list of OPAM packages providing LIB";
    "--version", Arg.Set version,
    " print the oasis2opam version";
    "--opam1", Arg.Unit(fun () -> opam_file_version := "1"),
    " use format 1 for the file \"opam\"";
    ] in
  let url = ref "" in
  let specs = Arg.align(specs @ fst (OASISContext.fspecs ())) in
  let usage_msg = "oasis2opam <url or tarball>" in
  Arg.parse specs (fun u -> url := u) usage_msg;
  if !version then (
    printf "Version: %s\n" Conf.version;
    if Conf.git_hash <> "" then printf "Git hash: %s\n" Conf.git_hash;
    exit 0
  );
  if !duplicates then (
    BuildDepends.output_duplicates stdout;
    exit 0;
  );
  if !query_findlib <> "" then (
    (match BuildDepends.Opam.of_findlib !query_findlib with
     | [] -> printf "%S is NOT provided by an OPAM package.\n" !query_findlib
     | pkgs -> let pkgs = BuildDepends.constrain_opam_package
                            (pkgs, Version.no_constraint) in
               printf "%S is provided by: %s\n" !query_findlib
                      (BuildDepends.string_of_packages pkgs));
    exit 0;
  );
  if !url = "" && not !local then (Arg.usage specs usage_msg; exit 1);
  (* Thus [!url = ""] iff !local, from now on. *)

  let opam_file_version = OASISVersion.version_of_string !opam_file_version in
  let t = Tarball.get !url in
  let pkg = Tarball.oasis t in
  let flags = get_flags pkg.sections in
  let dir = Tarball.pkg_opam_dir t in
  (try Unix.mkdir dir 0o777
   with Unix.Unix_error (Unix.EEXIST, _, _) ->
        if !local && not(y_or_n "The existing opam dir is going to be \
                                overwritten. Continue?" ~default:true) then
          exit 0);
  opam_descr t;
  opam_url t;
  opam_opam t flags opam_file_version;
  opam_findlib t flags;
  opam_install t flags;
  info (sprintf "OPAM directory %S created." dir)
