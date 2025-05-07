#!/usr/bin/make -f
# Makefile for tlsada
#
# Requires:
# - POSIX make
# - gprbuild(1)
# - gprclean(1)
# - gprinstall(1)
# - Ada 2012 compiler
#
# Copyright (c) 2022 nytpu <alex [at] nytpu.com>
# SPDX-License-Identifier: MPL-2.0
# For more license details, see LICENSE or <https://www.mozilla.org/en-US/MPL/2.0/>.
.POSIX:

### macros ###
GPRBUILD = gprbuild -p
GPRCLEAN = gprclean -p -q -r
GPRINSTALL = gprinstall

DESTDIR =
PREFIX = /usr/local


### default target, major phony targets ###
build:
	$(GPRBUILD) -P tlsada.gpr

example:
	$(GPRBUILD) -XTLSADA_BUILD_MODE=debug -P tlsada_example.gpr

clean:
	$(GPRCLEAN) -P tlsada_example.gpr
	$(GPRCLEAN) -P tlsada.gpr

install: build
	$(GPRINSTALL) -p --prefix='$(DESTDIR)/$(PREFIX)' -P tlsada.gpr

uninstall:
	-$(GPRINSTALL) --uninstall -p --prefix='$(DESTDIR)/$(PREFIX)' -P tlsada.gpr


# technically .PHONY isn't POSIX, but targets with a leading period aren't
# reserved so it's still valid, it'd just be useless to actually execute
.PHONY: build example clean install uninstall
