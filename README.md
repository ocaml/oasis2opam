[![Build Status](https://travis-ci.org/ocaml/oasis2opam.svg?branch=master)](https://travis-ci.org/ocaml/oasis2opam)

oasis2opam
==========

Tool to convert [OASIS](https://github.com/ocaml/oasis) metadata to
[OPAM](https://github.com/OCamlPro/opam) package descriptions.

Usage
-----

Go to the `packages` sub-directory of your local clone of
[opam-repository](https://github.com/OCamlPro/opam-repository) and
issue

    oasis2opam <URL of your tarball>

It will download the tarball into a temporary directory, extract the
`_oasis` file and use it to produce a `<package>.<version>` directory.
Along the way, `oasis2opam` may display suggestions so your OPAM
package has richer metadata.

Conventions
-----------

If a Flag name matches a findlib library, ``oasis2opam`` will assume
it is to be enabled if and only if this library is present and will
add a --enable-<flag> to the configure step conditioned by the
presence of the corresponding OPAM packages (without version
constraints, you must set these next to the library, in the
``BuildDepends:`` field).

The default value of flags will be used to determine whether the
libraries appearing in the conditional sections are to be considered
optional or mandatory.

OPAM tags are generated from the Oasis "Tags:" field.  "clib:" tags
will automatically be added with the names of the C libraries that are
used in order to ease searches.


Dependencies
------------

This tool uses the [oasis](https://github.com/ocaml/oasis) library.
It also relies on the presence of external programs: you need `wget`
or `curl`, and `tar`.
