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
