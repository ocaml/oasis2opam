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

(** Functions to write a <pkg>.install file. *)

open Printf
open OASISTypes
open Utils


let opam t ~local flags =
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
    let fname = pkg.name ^ ".install" in
    let fh, close =
      if local then
        (* In local mode, the goal is to generate the opam files in
           the repository itself. *)
        open_out fname, close_out
      else (
        if Tarball.has_install t then (
          info(sprintf "A %s.install file was found in the tarball.  Make \
                        sure it containts the instructions below." pkg.name);
          stdout, flush
        )
        else
          let dir = Filename.concat (Tarball.pkg_opam_dir t) "files" in
          (try Unix.mkdir dir 0o777
           with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
          open_out(Filename.concat dir (pkg.name ^ ".install")), close_out
      ) in
    output_string fh "bin: [\n";
    let output_bin (exec, name) =
      let exec = Utils.escaped exec in
      let name = Utils.escaped name in
      fprintf fh "  \"?%s.byte\" {\"%s\"}\n" exec name;
      fprintf fh "  \"?%s.native\" {\"%s\"}\n" exec name;
    in
    List.iter output_bin bins;
    output_string fh "]\n";

    close fh
  )
