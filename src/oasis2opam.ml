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

(* Global variabes & helpers
 ***********************************************************************)

let oasis_file = ref "_oasis"
let url = ref "" (* tarball URL *)

let specs = [
  "-f", Arg.Set_string oasis_file,
  "<file> oasis file used fo generate the OPAM package (default: from the URL)";
]

let () =
  let open OASISContext in
  default := { !default with ignore_plugins = true }

let info s = (!OASISContext.default).OASISContext.printf `Info s
let warn s = (!OASISContext.default).OASISContext.printf `Warning s
let error s = (!OASISContext.default).OASISContext.printf `Error s; exit 1

(* Directory name that OPAM expects for a package. *)
let opam_dir pkg =
  pkg.name ^ "." ^ OASISVersion.string_of_version pkg.version

let rec rm_recursively d =
  if Sys.is_directory d then (
    let fn = Array.map (Filename.concat d) (Sys.readdir d) in
    Array.iter rm_recursively fn;
    Unix.rmdir d
  )
  else Unix.unlink d


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

let opam_url pkg url tarball =
  let fh = open_out(Filename.concat (opam_dir pkg) "url") in
  let md5 = Digest.to_hex(Digest.file tarball) in
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
    error "Please set \"Maintainers:\" in your _oasis file.";
  match get_first_email pkg.maintainers with
  | Some e -> fprintf fh "maintainer: %S\n" e
  | None ->
     error "You must give an email in the oasis \"Maintainers:\" field."

let output_authors fh pkg =
  if pkg.authors = [] then
    error "Please set \"Authors:\" in your _oasis file.";
  let a = String.concat " " (List.map (sprintf "%S") pkg.authors) in
  fprintf fh "authors: [%s]\n" a

let output_tags fh pkg =
  let tag cat =
    (* FIXME: how do we generate tags from categories? *)
    let t = Filename.basename cat in
    "\"" ^ String.escaped t ^ "\"" in
  fprintf fh "tags: [%s]\n" (String.concat " " (List.map tag pkg.categories))

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
  Arg.parse (specs @ OASISContext.args()) (fun u -> url := u)
            "oasis2opam <url>";
  let tmp_dir = Filename.temp_file "oasis2opam" "" in
  let tarball = Filename.concat tmp_dir "tarball.tar.gz" in
  (* TODO: Download the package *)
  Unix.unlink tmp_dir;
  Unix.mkdir tmp_dir 0o777;
  (let fh = open_out tarball in close_out fh);
  let pkg = OASISParse.from_file !OASISContext.default !oasis_file in
  let dir = opam_dir pkg in
  (try Unix.mkdir dir 0o777 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  info (sprintf "Create OPAM package %S." dir);
  opam_descr pkg;
  opam_url pkg !url tarball;
  opam_opam pkg;
  (* Clean up *)
  rm_recursively tmp_dir

