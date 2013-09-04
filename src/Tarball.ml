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
open Utils

exception Program_not_found of string

(* The handle will cache some results so the extraction of the tarball
   is only done once.  *)
type t = {
  url: string;
  tarball: string;
  md5: string;
  content: string list;
  mutable pkg: OASISTypes.package option;
  mutable opam: string option;
}

let check_exists tarball =
  if not(Sys.file_exists tarball) then
    fatal_error(sprintf "The tarball %S does not exist" tarball)

let md5 t = t.md5
let url t = t.url

let on_err pgm msg c =
  if c <> 0 then
    if c = 127 then raise(Program_not_found pgm)
    else fatal_error msg

let wget ~retry url =
  let verbosity =
    let open OASISContext in
    if !default.quiet then "--quiet"
    else if !default.debug then "--verbose"
    else "--no-verbose" in
  OASISExec.run
    ~ctxt:!OASISContext.default
    ~f_exit_code:(on_err "wget" (sprintf "wget could not download %S.\n" url))
    "wget" ["--no-check-certificate"; "-t"; string_of_int retry;
            verbosity; url ]

let curl ~retry url =
  let verbosity =
    let open OASISContext in
    if !default.quiet then "--silent"
    else if !default.debug then "--verbose"
    else "--progress-bar" in
  OASISExec.run
    ~ctxt:!OASISContext.default
    ~f_exit_code:(on_err "curl" (sprintf "curl could not download %S.\n" url))
    "curl" ["--insecure"; "--retry"; string_of_int retry;
            "--retry-delay"; "2"; verbosity; "--location";
            "--remote-name"; url ]

let tar cmd tarball =
  let f_exit_code c =
    if c <> 0 then
      if c = 127 then raise(Program_not_found "tar")
      else fatal_error (sprintf "\"tar\" returned the error code %d.\n" c) in
  OASISExec.run_read_output ~ctxt:!OASISContext.default ~f_exit_code
                            "tar" ("--file" :: tarball :: cmd)

let download ?(retry=2) url =
  try wget ~retry url
  with Program_not_found _ ->
    try curl ~retry url
    with Program_not_found _ ->
      fatal_error "Cannot find \"wget\" nor \"curl\"."

let t_of_tarball ~url tarball =
  check_exists tarball;
  {
    url;
    tarball;
    md5 = Digest.to_hex(Digest.file tarball);
    content = tar ["--list"] tarball;
    pkg = None;
    opam = None;
  }

let get url =
  let is_http = OASISString.starts_with "http://" url
                || OASISString.starts_with "https://" url in
  if is_http then (
    let cwd = Unix.getcwd() in
    let tmp_dir = make_temp_dir() in
    try
      Unix.chdir tmp_dir;
      download url;
      let tarball = single_filename tmp_dir in
      Unix.chdir cwd;
      at_exit (fun () -> rm_recursively tmp_dir);
      t_of_tarball ~url tarball
    with
    | Program_not_found p ->
       Unix.chdir cwd;
       rm_recursively tmp_dir;
       fatal_error (sprintf "The program %S was not found." p)
    | e ->
       Unix.chdir cwd;
       rm_recursively tmp_dir;
       raise e
  )
  else
    (* Local tarball assumed. *)
    t_of_tarball ~url:"" url


(* Return the content of the file in the tarball or raise [Not_found]
   if it does not exist. *)
let get_file t re =
  let path = List.find (fun s -> Str.string_match re s 0) t.content in
  let file_content = tar ["--to-stdout"; "--extract"; path] t.tarball in
  String.concat "\n" file_content


let oasis_re = Str.regexp "\\(.*/\\|\\)_oasis$"

let oasis t =
  match t.pkg with
  | Some pkg -> pkg
  | None ->
     check_exists t.tarball;
     let oasis =
       try get_file t oasis_re
       with Not_found -> fatal_error "No file _oasis found in tarball\n." in
     let pkg = OASISParse.from_string !OASISContext.default oasis in
     t.pkg <- Some pkg;
     pkg


let opam_re = Str.regexp "\\(.*/\\|\\)_opam$"

let opam t =
  match t.opam with
  | Some o -> o
  | None ->
     check_exists t.tarball;
     let opam = try get_file t opam_re with Not_found -> "" in
     t.opam <- Some opam;
     opam
