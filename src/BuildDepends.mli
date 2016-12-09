open Utils

type oasis_flags = bool OASISExpr.choices StringMap.t
(** A map from flag names to the Boolean expression defining them.   *)

val min_oasis_version : OASISVersion.t
(** The minimal OASIS version required by oasis2opam. *)

val output : Tarball.t -> Format.formatter -> oasis_flags -> unit
(** [output t fmt flags] given a tarball [t] and OASIS [flags], write
    on [fmt] the "depends: [...]", "depopts: [...]", and "conflicts:
    [...]" OPAM sections. *)

val on_compiler_libs : oasis_flags -> OASISTypes.package -> bool
(** [on_compiler_libs flags pkg] tells whether "compiler-libs" is a
    mandatory dependency for [pkg] given the [flags]. *)

val get_findlib_libraries : oasis_flags -> OASISTypes.package -> string list
(** [get_findlib_libraries flags pkg] returns the findlib libraries
    provided by this package.  *)


val constrain_opam_package :
  (string * Version.Set.t) list * Version.constraints ->
  (string * Version.constraints) list
(** [constrain_opam_package (pkgs, pkg_oasis_version)] given a list
    [pkgs] of OPAM packages with a set of OPAM versions (typically
    providing a findlib library, see {!Opam.of_findlib}) and a
    constraint [pkg_oasis_version], return the list of OPAM packages
    filtered and constrained, so that it matches the original list
    constrained by [pkg_oasis_version] (leaving the constraint open or
    closed to future versions depending on the available data).  *)

val string_of_packages : (string * Version.constraints) list -> string
(** [string_of_packages pkgs] returns an OPAM "or" list of the
    constrained packages [pkgs].
    Example of output: "(pkg1 | pkg2 {>= 3.1})". *)


val output_duplicates : out_channel -> unit
(** [output_duplicates fh] writes on [fh] the findlib libraries that
    are provided by multiple OPAM packages. *)
