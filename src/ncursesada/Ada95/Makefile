# $Id: Makefile.in,v 1.28 2021/07/03 18:54:54 tom Exp $
##############################################################################
# Copyright 2020,2021 Thomas E. Dickey                                       #
# Copyright 1998-2010,2015 Free Software Foundation, Inc.                    #
#                                                                            #
# Permission is hereby granted, free of charge, to any person obtaining a    #
# copy of this software and associated documentation files (the "Software"), #
# to deal in the Software without restriction, including without limitation  #
# the rights to use, copy, modify, merge, publish, distribute, distribute    #
# with modifications, sublicense, and/or sell copies of the Software, and to #
# permit persons to whom the Software is furnished to do so, subject to the  #
# following conditions:                                                      #
#                                                                            #
# The above copyright notice and this permission notice shall be included in #
# all copies or substantial portions of the Software.                        #
#                                                                            #
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR #
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   #
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    #
# THE ABOVE COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER      #
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    #
# FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        #
# DEALINGS IN THE SOFTWARE.                                                  #
#                                                                            #
# Except as contained in this notice, the name(s) of the above copyright     #
# holders shall not be used in advertising or otherwise to promote the sale, #
# use or other dealings in this Software without prior written               #
# authorization.                                                             #
##############################################################################
#
#  Author:  Juergen Pfeifer, 1996
#
#  Version Control
#  $Revision: 1.28 $
#
SHELL		= /bin/sh

THIS		= Makefile

SUBDIRS         = gen src samples

TOP_MFLAGS	=  DESTDIR="$(DESTDIR)" RPATH_LIST="$(RPATH_LIST)"


################################################################################

.PHONY :	all
.PHONY :	clean
.PHONY :	depend
.PHONY :	distclean
.PHONY :	install
.PHONY :	install.libs
.PHONY :	libs
.PHONY :	mostlyclean
.PHONY :	realclean
.PHONY :	sources
.PHONY :	uninstall
.PHONY :	uninstall.libs

all \
libs \
sources \
install \
install.libs \
uninstall \
uninstall.libs ::
	for d in $(SUBDIRS); do \
		( cd $$d && $(MAKE) $(TOP_MFLAGS) $@ ) ;\
	done

clean \
mostlyclean ::
	for d in $(SUBDIRS); do \
		( cd $$d && $(MAKE) $(TOP_MFLAGS) $@ ) ;\
	done

distclean \
realclean ::
	for d in $(SUBDIRS); do \
		( cd $$d && $(MAKE) $(TOP_MFLAGS) $@ ) ;\
	done
	rm -rf lib
	for lib_kind in static dynamic; do \
		rm -rf $${lib_kind}-ali; \
		rm -rf $${lib_kind}-obj; \
	done
	-rm -f config.cache config.log config.status include/ncurses_cfg.h
	-rm -f Makefile

depend :

tags :
	@

preinstall :
	@

install.data :
	@
