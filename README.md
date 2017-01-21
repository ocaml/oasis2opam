[![Build Status](https://travis-ci.org/ocaml/oasis2opam.svg?branch=master)](https://travis-ci.org/ocaml/oasis2opam)

oasis2opam
==========

Tool to convert [OASIS](https://github.com/ocaml/oasis) metadata to
[OPAM](https://github.com/OCamlPro/opam) package descriptions.

Install
-------

The easiest way to install this program is to use
[opam](https://opam.ocaml.org/):

    opam install oasis2opam


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

If you want to "opamify" your project, so that for example you can
`opam pin` it, use

    oasis2opam --local

To keep the generated OPAM files up to date with the changes you make
to your `_oasis` file, add a rule executing `oasis2opam --local -y`
in your `Makefile`.


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

The OPAM field `dev-repo` is generated using the `SourceRepository`
section.  If there is such a section named `opam-pin`, it is
preferred.  If not, one named `head`, then `master`, then any
`SourceRepository` are used (in that order).  If no
`SourceRepository` is present in `_oasis` a warning is issued.

If an `_opam` file is found, its content is merged into the file
`opam` being created.  Specifically, the content of the "depends"
section (if it exists) is added to same section in `opam` and the rest
of the file is appended to the `opam` file.  This is useful to add
[build only dependencies][] that are not declared in `_oasis` and a
`depexts` section.

[build only dependencies]: https://opam.ocaml.org/doc/Manual.html#opamfield-depends

Dependencies
------------

This tool uses the [oasis](https://github.com/ocaml/oasis) library.
It also relies on the presence of external programs: you need `wget`
or `curl`, and `tar`.
