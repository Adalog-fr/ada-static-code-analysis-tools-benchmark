#!/usr/bin/make -f
# Makefile for lmdb-ada
#
# Requires:
# - POSIX make
# - gprbuild(1)
# - gprclean(1)
# - gprinstall(1)
# - Ada 2012 compiler
#
# SPDX-License-Identifier: BSD-2-Clause
# For more license details, see LICENSE.
.POSIX:

### macros ###
GPRBUILD = gprbuild -p
GPRCLEAN = gprclean -p -q -r
GPRINSTALL = gprinstall

DESTDIR =
PREFIX = /usr/local

RM = rm -rf


### default target, major phony targets ###
build:
	$(GPRBUILD) -P lmdb.gpr

example:
	$(GPRBUILD) -XLMDB_BUILD_MODE=debug -P lmdb_example.gpr

clean:
	$(GPRCLEAN) -P lmdb_example.gpr
	$(GPRCLEAN) -P lmdb.gpr
	-$(RM) example.db

install: build
	$(GPRINSTALL) -p --prefix='$(DESTDIR)/$(PREFIX)' -P lmdb.gpr

uninstall:
	-$(GPRINSTALL) --uninstall -p --prefix='$(DESTDIR)/$(PREFIX)' -P lmdb.gpr


# technically .PHONY isn't POSIX, but targets with a leading period aren't
# reserved so it's still valid, it'd just be useless to actually execute
.PHONY: build example clean install uninstall
