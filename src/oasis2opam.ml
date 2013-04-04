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
   | Some d -> output_string fh d; output_char fh '\n'
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

let output_maintainer fh pkg =
  if pkg.maintainers = [] then
    warn "Consider setting \"Maintainers:\" in your _oasis file.";
  match get_first_email pkg.maintainers with
  | Some e -> fprintf fh "maintainer: %S\n" e
  | None ->
     match get_first_email pkg.authors with
     | Some e -> fprintf fh "maintainer: %S\n" e
     | None -> fatal_error "You must give an email in the oasis \
                           \"Maintainers:\" or \"Authors:\" field (preferably \
                           the former)."

let output_authors fh pkg =
  if pkg.authors = [] then
    fatal_error "You must set \"Authors:\" in your _oasis file.";
  let a = String.concat " " (List.map (sprintf "%S") pkg.authors) in
  fprintf fh "authors: [%s]\n" a

let output_tags fh pkg =
  if pkg.categories <> [] then (
    let tag cat =
      (* FIXME: how do we generate tags from categories? *)
      let t = Filename.basename cat in
      "\"" ^ String.escaped t ^ "\"" in
    fprintf fh "tags: [%s]\n" (String.concat " " (List.map tag pkg.categories))
  )

let output_build_install fh pkg =
  fprintf fh "build: [\n  \
               [\"ocaml\" \"setup.ml\" \"-configure\" \
                 \"--prefix\" \"%%{prefix}%%\"]\n  \
               [\"ocaml\" \"setup.ml\" \"-build\"]\n  \
               [\"ocaml\" \"setup.ml\" \"-install\"]\n  \
             ]\n\
             remove: [\n  \
               [\"ocamlfind\" \"remove\" %S]\n\
             ]\n" pkg.name


let opam_opam pkg =
  let fh = open_out(Filename.concat (opam_dir pkg) "opam") in
  output_string fh "opam-version: \"1\"\n";
  output_maintainer fh pkg;
  output_authors fh pkg;
  fprintf fh "license: %S\n" (OASISLicense.to_string pkg.license);
  (match pkg.homepage with
   | Some url -> fprintf fh "homepage: %S\n" url
   | None -> warn "Consider setting \"Homepage:\" in your _oasis file");
  output_tags fh pkg;
  output_build_install fh pkg;
  output_string fh "depends: [";
  BuildDepends.output fh pkg;
  output_string fh "]\n";
  (match pkg.ocaml_version with
   | Some v -> fprintf fh "ocaml-version: %s\n"
                      (Version.string_of_comparator v)
   | None -> ());
  close_out fh

let opam_install pkg =
  let fh = open_out(Filename.concat (opam_dir pkg) (pkg.name ^ ".install")) in
  (* TODO *)
  close_out fh
;;

let () =
  let url = ref "" in
  let specs = Arg.align(OASISContext.args()) in
  let usage_msg = "oasis2opam <url> or oasis2opam <tarball>" in
  Arg.parse specs (fun u -> url := u) usage_msg;
  if !url = "" then (Arg.usage specs usage_msg; exit 1);

  let pkg, md5 = Tarball.get_oasis_md5 !url in
  let dir = opam_dir pkg in
  (try Unix.mkdir dir 0o777 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  opam_descr pkg;
  opam_url pkg !url md5;
  opam_opam pkg;
  info (sprintf "OPAM package %S created." dir)
