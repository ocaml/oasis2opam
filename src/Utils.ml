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

module M = Map.Make(String)

let () =
  let open OASISContext in
  default := { !default with ignore_plugins = true }

let info s = (!OASISContext.default).OASISContext.printf `Info s
let warn s = (!OASISContext.default).OASISContext.printf `Warning s
let error s = (!OASISContext.default).OASISContext.printf `Error s
let fatal_error s = error s; exit 1
let debug s = (!OASISContext.default).OASISContext.printf `Debug s

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

let read_whole_file fname =
  let buf = Buffer.create 1024 in
  let fh = open_in fname in
  let chunk = String.create 1024 in
  let len = ref 1 in (* enter loop *)
  while !len > 0 do
    len := input fh chunk 0 1024;
    Buffer.add_substring buf chunk 0 !len
  done;
  close_in fh;
  Buffer.contents buf

let rec make_unique_loop ~cmp ~merge = function
  | [] -> []
  | e1 :: e2 :: tl when cmp e1 e2 = 0 ->
     make_unique_loop ~cmp ~merge (merge e1 e2 :: tl)
  | e :: tl -> e :: make_unique_loop ~cmp ~merge tl

(* Make sure that all elements in [l] occurs one time only.  If
   duplicate elements [e1] and [e2] are found (i.e. [cmp e1 e2 = 0]),
   they are merged (i.e. replaced by [merge e1 e2]). *)
let make_unique ~cmp ~merge l =
  make_unique_loop ~cmp ~merge (List.sort cmp l)


let quote_re = Str.regexp "\""

(* Similar String.escaped but only escape '"' so UTF-8 chars are not
   escaped. *)
let escaped s =
  if String.contains s '"' then
    Str.global_replace quote_re "\\\"" s
  else s


let space_re = Str.regexp "[ \t\n\r]+"

(* TODO: implement the more sophisticated TeX version? *)
let output_paragraph fh ?(width=70) text =
  match Str.split space_re text with
  | [] -> ()
  | [w] -> output_string fh w
  | w0 :: words ->
     output_string fh w0;
     let space_left = ref(width - String.length w0) in
     let output_word w =
       let len_w = String.length w in
       if 1 + len_w > !space_left then (
         output_char fh '\n';
         space_left := width - len_w;
       )
       else (
         space_left := !space_left - 1 - len_w;
         output_char fh ' ';
       );
       output_string fh w in
     List.iter output_word words

let paragraph_delim = Str.regexp "[\n\r][ \t]*[\n\r][ \t\n\r]*"

(* Assume that blanks lines delimit paragraphs (which we do not want
   to merge together). *)
let output_wrapped fh ?width text =
  let paragraphs = Str.split paragraph_delim text in
  let rec output = function
    | [] -> ()
    | [p] -> output_paragraph fh ?width p
    | p :: tl ->
       output_paragraph fh ?width p;
       output_string fh "\n\n";
       output tl in
  output paragraphs

(* Evaluate OASIS conditonals
 ***********************************************************************)

let get_flags sections =
  let add m = function
    | Flag(cs, f) -> M.add cs.cs_name f.flag_default m
    | _ -> m in
  List.fold_left add M.empty sections


let eval_conditional flags cond =
  (* If any condition returns [true] (i.e. a section with these dep
     must be built), then the dependency is compulsory. *)
  let eval_tst name =
    try
      let t = M.find name flags in
      (* FIXME: how to eval flags?  See:
         https://github.com/gildor478/oasis2debian/blob/master/src/Expr.ml
         https://github.com/gildor478/oasis2debian/blob/master/src/Arch.ml
       *)
      string_of_bool(OASISExpr.choose (fun _ -> "false") t)
    with Not_found -> "false" in
  OASISExpr.choose eval_tst cond

;;
