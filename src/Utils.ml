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

module StringMap = Map.Make(String)

(* Compatibility with OCaml 3.12 *)
module String = struct
  include String

  let is_space = function
    | ' ' | '\012' | '\n' | '\r' | '\t' -> true
    | _ -> false

  let trim s =
    let len = String.length s in
    let i = ref 0 in
    while !i < len && is_space (String.unsafe_get s !i) do
      incr i
    done;
    let j = ref (len - 1) in
    while !j >= !i && is_space (String.unsafe_get s !j) do
      decr j
    done;
    if !j >= !i then
      String.sub s !i (!j - !i + 1)
    else
      ""
end

let () =
  let open OASISContext in
  default := { !default with ignore_plugins = true }

let info s = (!OASISContext.default).OASISContext.printf `Info s
let warn s = (!OASISContext.default).OASISContext.printf `Warning s
let error s = (!OASISContext.default).OASISContext.printf `Error s
let fatal_error s = error s; exit 1
let debug s = (!OASISContext.default).OASISContext.printf `Debug s


let start_with s prefix =
  let len = String.length prefix in
  String.length s >= len && String.sub s 0 len = prefix

let url_concat url path =
  if url = "" then path
  else if path = "" then url
  else if url.[String.length url - 1] = '/' || path.[0] = '/' then
    url ^ path
  else url ^ "/" ^ path

let concat_map l f =
  (* FIXME: a more efficient implementation may be desirable *)
  List.concat (List.map f l)

let rec map_find l f =
  match l with
  | [] -> None
  | x :: tl -> match f x with
              | (Some _ as x') -> x'
              | None -> map_find tl f

let rec ls_rec d =
  if Sys.is_directory d then (
    let fn = if d = "." then Sys.readdir d
             else Array.map (Filename.concat d) (Sys.readdir d) in
    concat_map (Array.to_list fn) ls_rec
  )
  else [d]

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
  let chunk = Bytes.create 1024 in
  let len = ref 1 in (* enter loop *)
  while !len > 0 do
    len := input fh chunk 0 1024;
    (* FIXME: Bytes.unsafe_to_string does not cost much and allows us
       to ba compatible with earlier versions of OCaml. *)
    Buffer.add_substring buf (Bytes.unsafe_to_string chunk) 0 !len
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


let to_quote_re = Str.regexp "\\(\"\\|\\\\\\)"

(* Similar String.escaped but only escape '"' and '\'' so UTF-8 chars
   are not escaped. *)
let escaped s =
  Str.global_replace to_quote_re "\\\\\\1" (* \ \1 *) s


let space_re = Str.regexp "[ \t\n\r]+"

(* TODO: implement the more sophisticated TeX version? *)
let gen_out_paragraph out_char out_string newline ?(width=70) text =
  match Str.split space_re text with
  | [] -> ()
  | [w] -> out_string w
  | w0 :: words ->
     out_string w0;
     let space_left = ref(width - String.length w0) in
     let output_word w =
       let len_w = String.length w in
       if 1 + len_w > !space_left then (
         out_string newline;
         space_left := width - len_w;
       )
       else (
         space_left := !space_left - 1 - len_w;
         out_char ' ';
       );
       out_string w in
     List.iter output_word words

let rec gen_out_wrapped out_char out_string ?width
          (text: OASISText.t) =
  let open OASISText in
  match text with
  | Para p :: BlankLine :: tl | Para p :: (_ :: _ as tl) ->
     gen_out_paragraph out_char out_string "\n" ?width p;
     out_string "\n\n";
     gen_out_wrapped out_char out_string ?width tl
  | [Para p] -> gen_out_paragraph out_char out_string "\n" ?width p;
                out_char '\n'
  | Verbatim s :: ([] as tl) | Verbatim s :: (Verbatim _ :: _ as tl) ->
     (* Verbatim text is scanned line per line. *)
     out_string s; out_char '\n';
     gen_out_wrapped out_char out_string ?width tl
  | Verbatim s :: BlankLine :: tl | Verbatim s :: (Para _ :: _ as tl) ->
     out_string s; out_string "\n\n";
     gen_out_wrapped out_char out_string ?width tl
  | BlankLine :: tl -> out_string "\n\n";
                       gen_out_wrapped out_char out_string ?width tl
  | [] -> ()

let output_wrapped fh ?width text =
  gen_out_wrapped (output_char fh) (output_string fh) ?width text

let wrapped_sprintf ?(ofs=0) ?width fmt =
  let newline = "\n" ^ String.make ofs ' ' in
  Printf.ksprintf (fun s ->
      let b = Buffer.create 256 in
      gen_out_paragraph (Buffer.add_char b) (Buffer.add_string b) newline
        ?width s;
      Buffer.contents b
    ) fmt

(* Evaluate OASIS conditionals
 ***********************************************************************)

let get_flags sections =
  let add m = function
    | Flag(cs, f) -> StringMap.add cs.cs_name f.flag_default m
    | _ -> m in
  List.fold_left add StringMap.empty sections


let eval_conditional flags cond =
  (* If any condition returns [true] (i.e. a section with these dep
     must be built), then the dependency is compulsory. *)
  let eval_tst name =
    try
      let t = StringMap.find name flags in
      (* FIXME: how to eval flags?  See:
         https://github.com/ocaml/oasis2debian/blob/master/src/Expr.ml
         https://github.com/ocaml/oasis2debian/blob/master/src/Arch.ml
       *)
      string_of_bool(OASISExpr.choose (fun _ -> "false") t)
    with Not_found -> "false" in
  OASISExpr.choose eval_tst cond


(* Yes/No question
 ***********************************************************************)

let y_or_n ~default q =
  let continue = ref true in (* enter the loop *)
  let r = ref false in
  let yn = if default then "Y/n" else "y/N" in
  while !continue do
    printf "%s [%s] %!" q yn;
    match String.trim(read_line()) with
    | "" -> continue := false;  r := default
    | "y" | "Y" -> continue := false;  r := true
    | "n" | "N" -> continue := false;  r := false
    |_ -> printf "Please answer 'Y' or 'N'.\n%!"
  done;
  !r


;;
