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
open Utils

exception Program_not_found of string

(* The handle will cache some results so the extraction of the tarball
   is only done once.  *)
type t = {
  url: string;
  tarball: string; (* = "" iff the CWD is the root of the project *)
  md5: string; (* = "" iff tarball = "" *)
  content: string list;
  mutable pkg: OASISTypes.package option;
  mutable opam: string option;
  mutable needs_oasis: bool option;
  mutable setup_ml_exists: bool option;
  mutable pkg_opam_dir: string; (* = "" iff undetermined yet *)
}

let no_tarball t = t.tarball = ""

let check_exists tarball =
  if tarball <> "" && not(Sys.file_exists tarball) then
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
  let content =
    if tarball = "" then
      (* All files one tries to get must be at the root of the
         project.  Do not recurse for the contents *)
      let fn = Array.to_list (Sys.readdir ".") in
      List.filter (fun f -> not(Sys.is_directory f)) fn
    else tar ["--list"] tarball in
  {
    url;
    tarball;
    md5 = if tarball = "" then "" else Digest.to_hex(Digest.file tarball);
    content;
    pkg = None;
    opam = None;
    needs_oasis = None;
    setup_ml_exists = None;
    pkg_opam_dir = "";
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


(* Return the content of the file in the tarball or [None]. *)
let get_file t re =
  try
    (* FIXME: List.find may raise Not_found.  Match "exception
       Not-found" when OCaml < 4.02 can be dropped. *)
    let path = List.find (fun s -> Str.string_match re s 0) t.content in
    if no_tarball t then
      Some(read_whole_file path)
    else
      let file_content = tar ["--to-stdout"; "--extract"; path] t.tarball in
      Some(String.concat "\n" file_content)
  with Not_found ->
    None

let oasis_re = Str.regexp "\\([^/]*/\\|\\)_oasis$"

let oasis t =
  match t.pkg with
  | Some pkg -> pkg
  | None ->
     check_exists t.tarball;
     match get_file t oasis_re with
     | None ->
        fatal_error "No file _oasis found at the root of the tarball\n."
     | Some oasis ->
        let pkg = OASISParse.from_string !OASISContext.default oasis in
        t.pkg <- Some pkg;
        pkg


let opam_re = Str.regexp "\\(.*/\\|\\)_opam$"

let opam_file t =
  match t.opam with
  | Some o -> o
  | None ->
     check_exists t.tarball;
     let opam = match get_file t opam_re with Some o -> o
                                            | None -> "" in
     t.opam <- Some opam;
     opam


let depends_re =
  Str.regexp "depends:[ \t\n\r]*\\[\\([^][]*\\)[ \t\n\r]*\\][ \t\n\r]*"

let opam_depends t =
  let s = opam_file t in
  try
    ignore(Str.search_forward depends_re s 0);
    String.trim(Str.matched_group 1 s)
  with Not_found -> ""

let opam t =
  let s = opam_file t in
  Str.global_replace depends_re "" s

let setup_re = Str.regexp "\\(.*/\\|\\)setup\\.ml"
let newline_re = Str.regexp "[\n\r]+"
let dynamic_re =
  Str.regexp "^\\(#require +\"oasis.dynrun\"\\|open *OASISDynRun\\)"

let setup_ml_exists t =
  match t.setup_ml_exists with
  | Some b -> b
  | None ->
     let e = try ignore(get_file t setup_re); true with Not_found -> false in
     t.setup_ml_exists <- Some e;
     e

let needs_oasis t =
  match t.needs_oasis with
  | Some b -> b
  | None ->
     check_exists t.tarball;
     let setup = match get_file t setup_re with Some s -> s
                                              | None -> "" in
     let need =
       if setup = "" then true
       else (
         t.setup_ml_exists <- Some true;
         (* Explore the file to see whether dynamic mode is used. *)
         let l = Str.split newline_re setup in
         List.exists (fun l -> Str.string_match dynamic_re l 0) l
       ) in
     t.needs_oasis <- Some need;
     need


let pkg_opam_dir t =
  if t.pkg_opam_dir = "" then (
    let d =
      if no_tarball t then "opam" (* local/devel mode *)
      else
        let open OASISTypes in
        let pkg = oasis t in
        let d = pkg.name ^ "." ^ OASISVersion.string_of_version pkg.version in
        if Sys.file_exists pkg.name then
          (* The way packages are stored in the OPAM repository groups
             the many versions of a package in a directory named [pkg.name]. *)
          Filename.concat pkg.name d
        else d in
    t.pkg_opam_dir <- d;
    d
  )
  else
    t.pkg_opam_dir

let install t =
  let pkg = oasis t in
  let re = Str.regexp("\\([^/]*/\\|\\)"
                      ^ pkg.OASISTypes.name ^ "\\.install") in
  get_file t re

let file_contents t fname =
  let re = Str.regexp("\\([^/]*/\\|\\)" ^ Str.quote fname) in
  get_file t re
