
type t
(** A handle to the tarball that we can use to query for further
    information. *)

val get : string -> t
(** Get the tarball corresponding to an URL (http or https) or a local
    file.  The call [get ""] assumes that the current working
    directory is the root of the project. *)

val no_tarball : t -> bool
(** [no_tarball t] is [true] if [t] was created from the current
    working directory (root of the project). *)

val md5 : t -> string
(** Return the MD5 hash of the tarball.  Returns [""] if and only if
    no tarball was provided (i.e., the current working directory is
    the root of the project). *)

val url : t -> string
(** Returns the URL of the package or [""] if it was a local file (and
    thus cannot be used in the "url" file for OPAM). *)

val oasis : t -> OASISTypes.package
(** Return the content of the _oasis file.  A fatal error is issued if
    the tarball does not contain an _oasis file. *)

val needs_oasis : t -> bool
(** Whether oasis is required to compile, either because there is no
    setup.ml file or because dynamic mode is used. *)

val setup_ml_exists : t -> bool
(** Whether a file "setup.ml" exists in the tarball. *)

val opam_depends : t -> string
(** Return the content of the depends section in the _opam file (or
    [""] if no such section or no such file exists). *)

val opam : t -> string
(** Return the content of the _opam file in the tarball where the
    sections matched by the above [opam_*] functions have been
    filtered out (or [""] if no such file exists). *)

val pkg_opam_dir : t -> string
(** Returns the name of the directory in which the OPAM files for the
    package will be put.  May use the contents of the current working
    directory to improve the answer.  *)


;;
