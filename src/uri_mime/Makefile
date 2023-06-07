#!/usr/bin/make -f
# Makefile for uri-ada
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
	$(GPRBUILD) -P uri_mime.gpr

test:
	$(GPRBUILD) -XURI_MIME_BUILD_MODE=debug -P uri_mime_test.gpr
	bin/run_tests

clean:
	$(GPRCLEAN) -P uri_mime_test.gpr
	$(GPRCLEAN) -P uri_mime.gpr

install: build
	$(GPRINSTALL) -p --prefix='$(DESTDIR)/$(PREFIX)' -P uri_mime.gpr

uninstall:
	-$(GPRINSTALL) --uninstall -p --prefix='$(DESTDIR)/$(PREFIX)' -P uri_mime.gpr


# technically .PHONY isn't POSIX, but targets with a leading period aren't
# reserved so it's still valid, it'd just be useless to actually execute
.PHONY: build test clean install uninstall
