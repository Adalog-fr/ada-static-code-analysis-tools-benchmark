#!/usr/bin/make -f
# Makefile for linenoise-ada
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


### default target, major phony targets ###
build:
	$(GPRBUILD) -P linenoise.gpr

example:
	$(GPRBUILD) -XLINENOISE_BUILD_MODE=debug -P linenoise_example.gpr

clean:
	$(GPRCLEAN) -P linenoise_example.gpr
	$(GPRCLEAN) -P linenoise.gpr

install: build
	$(GPRINSTALL) -p --prefix='$(DESTDIR)/$(PREFIX)' -P linenoise.gpr

uninstall:
	-$(GPRINSTALL) --uninstall -p --prefix='$(DESTDIR)/$(PREFIX)' -P linenoise.gpr


# technically .PHONY isn't POSIX, but targets with a leading period aren't
# reserved so it's still valid, it'd just be useless to actually execute
.PHONY: build example clean install uninstall
