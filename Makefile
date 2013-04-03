###########################################################################
#  OASIS2OPAM: Convert OASIS metadata to OPAM package descriptions        #
#                                                                         #
#  Copyright (C) 2013-2014, Christophe Troestler                          #
#                                                                         #
#  This library is free software; you can redistribute it and/or modify   #
#  it under the terms of the GNU General Public License as published by   #
#  the Free Software Foundation; either version 3 of the License, or (at  #
#  your option) any later version, with the OCaml static compilation      #
#  exception.                                                             #
#                                                                         #
#  This library is distributed in the hope that it will be useful, but    #
#  WITHOUT ANY WARRANTY; without even the implied warranty of             #
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file      #
#  COPYING for more details.                                              #
#                                                                         #
#  You should have received a copy of the GNU Lesser General Public       #
#  License along with this library; if not, write to the Free Software    #
#  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301   #
#  USA.                                                                   #
###########################################################################

PKGNAME	    = $(shell oasis query name)
PKGVERSION  = $(shell oasis query version)
PKG_TARBALL = $(PKGNAME)-$(PKGVERSION).tar.gz

DISTFILES   = AUTHORS.txt INSTALL.txt README.txt _oasis \
  Makefile setup.ml myocamlbuild.ml $(wildcard _tags src/)

.PHONY: all byte native configure doc test install uninstall reinstall

all byte native: configure
	ocaml setup.ml -build

configure: setup.ml
	ocaml $< -configure

setup.ml: _oasis
	oasis setup -setup-update dynamic

test doc install uninstall reinstall: all
	ocaml setup.ml -$@

.PHONY: dist tar headache
dist tar: setup.ml
	mkdir $(PKGNAME)-$(PKGVERSION)
	cp -r $(DISTFILES) $(PKGNAME)-$(PKGVERSION)/
#	There is no point in having a setup.ml independent of oasis
#	because the program depends on oasis anyway.
	tar -zcvf $(PKG_TARBALL) $(PKGNAME)-$(PKGVERSION)
	$(RM) -rf $(PKGNAME)-$(PKGVERSION)

headache:
	find ./ -name .git -prune -false -o -name _build -prune -false \
	  -o -name 'oasis2opam.*' -prune -false -o -name '*[^~]' -type f \
	  | xargs headache -h _header -c _headache.config


.PHONY: clean distclean
clean::
	ocaml setup.ml -clean
	$(RM) $(PKG_TARBALL)

distclean:
	ocaml setup.ml -distclean
	$(RM) $(wildcard *.ba[0-9] *.bak *~)
