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

let () =
  let open OASISContext in
  default := { !default with ignore_plugins = true }

let info s = (!OASISContext.default).OASISContext.printf `Info s
let warn s = (!OASISContext.default).OASISContext.printf `Warning s
let error s = (!OASISContext.default).OASISContext.printf `Error s
let fatal_error s = error s; exit 1

(* Directory name that OPAM expects for a package. *)
let opam_dir pkg =
  pkg.name ^ "." ^ OASISVersion.string_of_version pkg.version

let rm_no_error fname =
  (try Unix.unlink fname with _ -> ())

let rec rm_recursively d =
  if Sys.is_directory d then (
    let fn = Array.map (Filename.concat d) (Sys.readdir d) in
    Array.iter rm_recursively fn;
    Unix.rmdir d
  )
  else Unix.unlink d

let make_temp_dir () =
  let d = Filename.temp_file "oasis2opam" "" in
  Unix.unlink d;
  Unix.mkdir d 0o777;
  d

let single_filename d =
  assert(Sys.is_directory d);
  let fn = Sys.readdir d in
  if Array.length fn <> 1 then
    warn(sprintf "oasis2opam: %S does not contain a single file: %s."
                 d (String.concat ", " (Array.to_list fn)));
  Filename.concat d fn.(0)
