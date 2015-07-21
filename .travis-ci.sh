
OPAM_PKGS="oasis base-bytes"
OPAM_PKGS_TEST="ounit qcheck"

export OPAMYES=1

if [ -f "$HOME/.opam/config" ]; then
    opam update
    opam upgrade
else
    opam init
fi

if [ -n "${OPAM_SWITCH}" ]; then
    opam switch ${OPAM_SWITCH}
fi
eval `opam config env`

opam install $OPAM_PKGS

export OCAMLRUNPARAM=b
oasis setup
if [ "$OPAM_SWITCH" = "3.12.1" ]; then
    ocaml setup.ml -configure
    ocaml setup.ml -build
else
    opam install $OPAM_PKGS_TEST
    ocaml setup.ml -configure --enable-tests
    ocaml setup.ml -build
    ocaml setup.ml -test
fi
