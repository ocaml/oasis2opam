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

Dependencies
------------

This tool uses the [oasis](https://github.com/ocaml/oasis) library.
It also relies on the presence of external programs: you need `wget`
or `curl`, and `tar`.
