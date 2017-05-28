open Printf
open Utils

(* Findlib libraries and their corresponding virtual base OPAM package. *)
let opam_base_packages = [ "bigarray", "base-bigarray";
                           "bytes", "base-bytes";
                           "num", "num"; (* split from the compiler *)
                           "threads", "base-threads";
                           "unix", "base-unix" ]


exception Error of int

(** [opam args] execute opam with the list os arguments [args] and
    return the output. *)
let opam =
  let f_exit_code c =
    if c <> 0 then
      if c = 127 then fatal_error "\"opam\" not found.\n"
      else raise(Error c) in
  fun cmd ->
  OASISExec.run_read_one_line
    ~ctxt:!OASISContext.default ~f_exit_code
    "opam" cmd

(** The local directory, root of OPAM installation. *)
let root =
  try Sys.getenv "OPAMROOT"
  with Not_found ->
       try opam ["config"; "var"; "root"]
       with Error 66
          | Error 1 ->
             (* "root" not an opam variable; use same as OpamGlobals.home *)
             let home = try Sys.getenv "HOME"
                        with Not_found -> Sys.getcwd () in
             let r = Filename.concat home ".opam" in
             info(sprintf "Using %s as opam root." r);
             r

let space_re = Str.regexp "[ \t\n\r]+"
let pkg_re_1_1 = Str.regexp "^\\([a-zA-Z0-9_-]+\\)\\.\\(.+\\)$"
let pkg_re_1_0 = Str.regexp "^\\([a-zA-Z0-9_-]+\\)\\.\\(.+\\)\\.opam"
let findlib_re =
  Str.regexp "\"ocamlfind\" +\"remove\" +\"\\([a-zA-Z0-9_.-]+\\)\""

let add_is_provided_by m ~findlib ~opam =
  let pkgs_list = try opam :: (StringMap.find findlib !m)
                  with Not_found -> [opam] in
  m := StringMap.add findlib pkgs_list !m

(* Scan the content of [s] and add to [m] all Findlib libraries found. *)
let rec add_all_findlib m opam s ofs =
  try
    ignore(Str.search_forward findlib_re s ofs);
    let ofs = Str.match_end() in
    let lib = Str.matched_group 1 s in
    add_is_provided_by m ~findlib:lib ~opam;
    add_all_findlib m opam s ofs
  with Not_found -> ()

let add_opam_version packages (opam_pkg: string) version =
  let v_set = try Version.Set.add version (StringMap.find opam_pkg !packages)
              with Not_found -> Version.Set.singleton version in
  packages := StringMap.add opam_pkg v_set !packages

(* Keep the set of versions for each package. *)
let merge_versions pkgs =
  make_unique ~cmp:(fun (p1,_) (p2,_) -> String.compare p1 p2)
    ~merge:(fun (p, v1) (_, v2) -> (p, Version.Set.union v1 v2))
    pkgs

let findlib, packages =
  (* Cache the result.  We know we are not up to date if
     "state.cache" is newer than us. *)
  let cache = Filename.concat root "oasis2opam.cache" in
  let m_cache = try (Unix.stat cache).Unix.st_mtime
                with Unix.Unix_error(Unix.ENOENT,_,_) -> neg_infinity in
  let state = Filename.concat root "state.cache" in
  let m_state = try (Unix.stat state).Unix.st_mtime
                with Unix.Unix_error(Unix.ENOENT,_,_) -> infinity in
  (* FIXME: when oasis2opam upgrades, the cache must be updated (the
     type may change). *)
  if m_cache < Conf.compilation_time || m_cache < m_state then (
    (* Need to browse the opam dir again. *)
    eprintf "Synchronizing the list of available packages... %!";
    let m = ref StringMap.empty in
    let pkgs = ref StringMap.empty in
    let rec add_of_dir dir =
      if (try Sys.is_directory dir with _ -> false) then
        Array.iter (add_opam dir) (Sys.readdir dir)
    and add_opam dir pkg_ver =
      let fname = Filename.concat dir pkg_ver in
      (* One first checks if an .opam extension is present. *)
      if Str.string_match pkg_re_1_0 pkg_ver 0 then (
        let opam = Str.matched_group 1 pkg_ver in
        let version = Str.matched_group 2 pkg_ver in
        add opam version fname
      )
      else if Str.string_match pkg_re_1_1 pkg_ver 0 then (
        let opam_pkg = Str.matched_group 1 pkg_ver in
        let version = Str.matched_group 2 pkg_ver in
        let findlib_file = Filename.concat fname "findlib" in
        let opam_file = Filename.concat fname "opam" in
        if Sys.file_exists opam_file then (
          if Sys.file_exists findlib_file then (
            (* Use the file "findlib" listing all provided libraries *)
            let version = OASISVersion.version_of_string version in
            let opam = (opam_pkg, Version.Set.singleton version) in
            let s = read_whole_file findlib_file in
            let findlibs = Str.split space_re s in
            List.iter (fun f -> add_is_provided_by m ~findlib:f ~opam) findlibs;
            add_opam_version pkgs opam_pkg version;
          )
          else
            add opam_pkg version opam_file
        )
      )
      else (* OPAM 1.1 stores packages hierarchically.  Recurse. *)
        add_of_dir fname
    and add opam_pkg version opam_file =
      let version = OASISVersion.version_of_string version in
      let s = read_whole_file opam_file in
      let s = Str.global_replace space_re " " s in
      add_all_findlib m (opam_pkg, Version.Set.singleton version) s 0;
      (* We also want to keep a correspondence of OPAM packages →
         versions (even those with no findlib detectable lib) *)
      add_opam_version pkgs opam_pkg version
    in
    add_of_dir (Filename.concat root "repo");
    (* For OPAM < 1.1, the sub-dir "opam" was used: *)
    add_of_dir (Filename.concat root "opam");

    m := StringMap.add "findlib" [("ocamlfind", Version.Set.empty)] !m;
    List.iter (fun (fl, p) -> m := StringMap.add fl [(p, Version.Set.empty)] !m)
      opam_base_packages;
    (* Camlp4 was split out of the standard distribution.  A dummy
         OPAM package was created for older compilersn, thus one can
         consider that all versions of the OPAM "camlp4" package have
         the lib (which is what [Version.Set.empty] means) even though
         it is not detected automatically. *)
    m := StringMap.add "camlp4" ["camlp4", Version.Set.empty] !m;
    let findlib = StringMap.map (fun pkgs -> merge_versions pkgs) !m in
    (* Cache *)
    let to_be_cached = (findlib, !pkgs) in
    let fh = open_out_bin cache in
    output_value fh to_be_cached;
    close_out fh;
    eprintf "done.\n%!";
    to_be_cached
  )
  else (
    (* Use the cache. *)
    let fh = open_in_bin cache in
    let cached = input_value fh in
    close_in fh;
    cached (* will ge given a type by unification with the "then" clause. *)
  )

(** Return the set of available versions for the OPAM package [pkg]
    or raise [Not_found]. *)
let package_versions_exn (pkg: string) = StringMap.find pkg packages

let package_versions pkg =
  try package_versions_exn pkg with Not_found -> Version.Set.empty

(** Return the OPAM package(s) (with their OPAM versions) containing
    the findlib library [lib].  An empty set of OPAM versions means
    that any version is accepted (for example, versions constraints
    for "camlp4" are set by the compiler).  See
    https://github.com/ocaml/opam/issues/573 *)
let of_findlib (lib: string) =
  try StringMap.find lib findlib (* <> [] *) with Not_found -> []

let to_string (p, v) =
  p ^ " (" ^ Version.Set.to_string v ^ ")"

let of_findlib_warn (lib: string) : (string * Version.Set.t) list =
  let pkgs = of_findlib lib in
  match pkgs with
  | [_] -> pkgs
  | _ :: _ ->
     warn(sprintf "%S provided by OPAM %s."
            lib (String.concat ", " (List.map to_string pkgs)));
     pkgs
  | [] ->
     (* FIXME: do we want to check that there is a version
          satisfying the possible version constraints? *)
     try
       let v_set = package_versions_exn lib in (* or raise Not_found *)
       warn(sprintf "Guessing that OPAM package %S provides Findlib %S."
              lib lib);
       [lib, v_set]
     with Not_found ->
       error(sprintf "OPAM package %S not found!" lib);
       [lib, Version.Set.empty]

let of_buildtool_warn (name: string) : (string * Version.Set.t) list =
  try
    let _v_set = package_versions_exn name in (* or raise Not_found *)
    [name, Version.Set.empty]
  with Not_found ->
    warn(sprintf "No OPAM package found for buildtool %S!" name);
    []

(* Tells whether the two lists of packages are equal.  Do not care
   about versions.  Assume the list are sorted as [of_findlib]
   provides them.  *)
let rec compare_pkgs p1 p2 = match p1, p2 with
  | [], [] -> 0
  | (p1,_) :: tl1, (p2,_) :: tl2 ->
     let cp = String.compare p1 p2 in
     if cp <> 0 then cp else compare_pkgs tl1 tl2
  | _, [] -> 1
  | [], _ -> -1

(* Order on packages that consider equal two sets with one included in
   the other.  Otherwise the package set with the smaller name not in
   the other set is declared smaller.  Assume the lists are sorted in
   increasing order as [of_findlib] provides them.*)
let rec cmp_diff_pkgs_loop cmp p1s p2s = match p1s, p2s with
  | [], [] -> 0 (* p1s ⊆ p2s or  *)
  | (_::_), [] -> if cmp <= 0 then 0 (* p1s ⊇ p2s *) else cmp
  | [], (_::_) -> if cmp >= 0 then 0 (* p1s ⊆ p2s *) else cmp
  | (n1, _) :: tl1, (n2, _) :: tl2 ->
     let cp = String.compare n1 n2 in
     if cp = 0 then cmp_diff_pkgs_loop cmp tl1 tl2 (* skip common element *)
     else if cp < 0 then
       if cmp <= 0 then cmp_diff_pkgs_loop cp tl1 p2s
       else cmp (* p1\p2 ≠ ∅ and p2\p1 ≠ ∅, first (= smaller) wins *)
     else (* cp > 0 *)
       if cmp >= 0 then cmp_diff_pkgs_loop cp p1s tl2
       else cmp (* p1\p2 ≠ ∅ and p2\p1 ≠ ∅ *)

let cmp_diff_pkgs p1 p2 = cmp_diff_pkgs_loop 0 p1 p2

(* Merge two lists of packages, taking only those common to the two
   lists and the versions in both lists.  An empty version set is
   interpreted as "any version is OK".  Assume the list are sorted in
   increasing order as [of_findlib] provides them.  *)
let rec intersect_pkgs p1 p2 = match p1, p2 with
  | (name1, v1) :: tl1, (name2, v2) :: tl2 ->
     let cp = String.compare name1 name2 in
     if cp < 0 then intersect_pkgs tl1 p2       (* name1 not in p2 *)
     else if cp > 0 then intersect_pkgs p1 tl2  (* name2 not in p1 *)
     else (* name1 = name2 *)
       let v_set = if Version.Set.is_empty v1 then v2
                   else if Version.Set.is_empty v2 then v1
                   else Version.Set.inter v1 v2 in
       (name1, v_set) :: intersect_pkgs tl1 tl2
  | _, [] | [], _ -> []
