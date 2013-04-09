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

(* Write OPAM "descr" & "url" files.
 ***********************************************************************)
let opam_descr pkg =
  let fh = open_out(Filename.concat (opam_dir pkg) "descr") in
  output_string fh pkg.synopsis;
  output_char fh '\n';
  (match pkg.description with
   | Some d -> output_wrapped fh d; output_char fh '\n'
   | None -> warn "Consider setting \"Description:\" in your _oasis file");
  close_out fh

let opam_url pkg url md5 =
  let fh = open_out(Filename.concat (opam_dir pkg) "url") in
  fprintf fh "archive: %S\nchecksum: %S\n" url md5;
  close_out fh

(* Write OPAM "opam" file.
 ***********************************************************************)

let email_re = Str.regexp "[0-9a-zA-Z.]+@[0-9a-zA-Z.]+"

(* From a list of names (possibly with email), get the first email. *)
let rec get_first_email = function
  | [] -> None
  | name :: tl ->
     try ignore(Str.search_forward email_re name 0);
         Some(Str.matched_string name)
     with Not_found -> get_first_email tl

let output_maintainer fmt pkg =
  if pkg.maintainers = [] then
    warn "Consider setting \"Maintainers:\" in your _oasis file.";
  match get_first_email pkg.maintainers with
  | Some e -> Format.fprintf fmt "maintainer: %S@\n" e
  | None ->
     match get_first_email pkg.authors with
     | Some e -> Format.fprintf fmt "maintainer: %S@\n" e
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

let output_tags fmt pkg =
  if pkg.categories <> [] then (
    let tag cat =
      (* FIXME: how do we generate tags from categories? *)
      let t = Filename.basename cat in
      "\"" ^ String.escaped t ^ "\"" in
    let tags = List.map tag pkg.categories in
    Format.fprintf fmt "tags: [ @[%s";
    List.iter (fun t -> Format.fprintf fmt "%s@ " t) tags;
    Format.fprintf fmt "@] ]@\n";
  )

let output_build_install fmt flags pkg =
  Format.fprintf fmt "@[<2>build: [@\n\
                      [\"ocaml\" \"setup.ml\" \"-configure\" \
                         \"--prefix\" prefix]@\n\
                      [\"ocaml\" \"setup.ml\" \"-build\"]@\n\
                      [\"ocaml\" \"setup.ml\" \"-install\"]\
                      @]@\n]@\n";
  let libs = BuildDepends.get_findlib_libraries flags pkg in
  if libs <> [] then (
    Format.fprintf fmt "@[<2>remove: [";
    List.iter (fun l -> Format.fprintf fmt "@\n[\"ocamlfind\" \"remove\" %S]" l
              ) libs;
    Format.fprintf fmt "@]@\n]@\n"
  )


let opam_opam flags pkg =
  let fh = open_out(Filename.concat (opam_dir pkg) "opam") in
  let fmt = Format.formatter_of_out_channel fh in
  Format.fprintf fmt "opam-version: \"1\"@\n";
  (* "name:" and "version:" deemed unnecessary. *)
  output_maintainer fmt pkg;
  output_authors fmt pkg;
  Format.fprintf fmt "license: %S@\n" (OASISLicense.to_string pkg.license);
  (match pkg.homepage with
   | Some url -> Format.fprintf fmt "homepage: %S@\n" url
   | None -> warn "Consider setting \"Homepage:\" in your _oasis file");
  output_tags fmt pkg;
  output_build_install fmt flags pkg;
  if List.exists (function Doc _ -> true | _  -> false) pkg.sections then
    Format.fprintf fmt "build-doc: [ \"ocaml\" \"setup.ml\" \"-doc\" ]@\n";
  BuildDepends.output fmt flags pkg;
  (match pkg.ocaml_version with
   | Some v -> Format.fprintf fmt "ocaml-version: [ %s ]@\n"
                             (Version.string_of_comparator v)
   | None -> ());
  Format.pp_print_flush fmt ();
  close_out fh


let opam_install flags pkg =
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
    let dir = Filename.concat (opam_dir pkg) "files" in
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
  let version = ref false in
  let duplicates = ref false in
  let specs = [
    "--duplicates", Arg.Set duplicates,
    " output a list of packages providing the same ocamlfind library";
    "--version", Arg.Set version,
    " print the oasis2opam version (including the Git hash if relevant)";
  ] in
  let url = ref "" in
  let specs = Arg.align(specs @ OASISContext.args()) in
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
  if !url = "" then (Arg.usage specs usage_msg; exit 1);

  let pkg, md5 = Tarball.get_oasis_md5 !url in
  let flags = get_flags pkg.sections in
  let dir = opam_dir pkg in
  (try Unix.mkdir dir 0o777 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  opam_descr pkg;
  opam_url pkg !url md5;
  opam_opam flags pkg;
  opam_install flags pkg;
  info (sprintf "OPAM package %S created." dir)
