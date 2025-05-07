##############################################################################
# Copyright 2018-2020,2021 Thomas E. Dickey                                  #
# Copyright 1998-2015,2018 Free Software Foundation, Inc.                    #
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
#  $Id: Makefile.in,v 1.64 2021/07/03 15:45:33 tom Exp $
#
.SUFFIXES:

SHELL		= /bin/sh

THIS		= Makefile

x		= 

srcdir		= .
prefix		= /usr
exec_prefix	= ${prefix}
bindir		= ${exec_prefix}/bin
datarootdir	= ${prefix}/share
datadir		= ${datarootdir}
libdir		= ${exec_prefix}/lib
includedir	= ${prefix}/include

INSTALL		= /usr/bin/install -c 
INSTALL_DATA	= ${INSTALL} -m 644

AWK		= mawk
LN_S		= ln -s -f

CC		= gcc
CFLAGS		= -O2 --param max-inline-insns-single=1200 

CPPFLAGS	= -I. -I../include -I../../include  \
		  -DHAVE_CONFIG_H -I$(srcdir)

CCFLAGS		= $(CPPFLAGS) $(CFLAGS)

CFLAGS_NORMAL	= $(CCFLAGS) -DNCURSES_STATIC
CFLAGS_DEBUG	= $(CCFLAGS) -DNCURSES_STATIC -g -DTRACE
CFLAGS_PROFILE	= $(CCFLAGS) -DNCURSES_STATIC -pg
CFLAGS_SHARED	= $(CCFLAGS) -fPIC

CFLAGS_DEFAULT	= $(CFLAGS_NORMAL)

REL_VERSION	= 6.3
ABI_VERSION	= 6
LOCAL_LIBDIR	= /workspaces/bench-source/src/ncursesada/lib

LINK		= $(CC)
LDFLAGS		=   

RANLIB		= ranlib
################################################################################
BINDIR		= $(DESTDIR)$(bindir)
DATADIR		= $(DESTDIR)$(datadir)
LIBDIR		= $(DESTDIR)$(libdir)

MY_DATADIR	= $(DATADIR)/AdaCurses
THIS_DATADIR	= $(datadir)/AdaCurses

################################################################################
ada_srcdir	= ../src

LD_FLAGS	=  $(LOCAL_LIBS)    $(LDFLAGS)

ADA		= gnatmake
ADAPREP		= gnatprep
ADAFLAGS	=  -gnatpn -O2 -I$(srcdir)

ADAMAKE		= gnatmake
ADAMAKEFLAGS	= -a \
	-A$(srcdir) \
	-A$(ada_srcdir) \
	-A$(srcdir)/$(ada_srcdir)

ALIB		= terminal_interface
ABASE		= $(ALIB)-curses
THISLIB		= sample

CARGS		= -cargs $(ADAFLAGS)
LARGS		= -largs -L../lib -lAdaCurses -L../../lib -lform -lmenu -lpanel -lncurses  $(LD_FLAGS) 

PROGS		= tour$x ncurses$x  rain$x

################################################################################

.PHONY :	all
.PHONY :	clean
.PHONY :	distclean
.PHONY :	install
.PHONY :	install.examples
.PHONY :	install.libs
.PHONY :	libs
.PHONY :	mostlyclean
.PHONY :	realclean
.PHONY :	sources
.PHONY :	uninstall
.PHONY :	uninstall.examples
.PHONY :	uninstall.libs

all ::  $(PROGS)
	@echo made $@

sources :
	@echo made $@

libs \
install \
install.libs ::
	@echo made $@

uninstall \
uninstall.libs ::
	@echo made $@

install.examples :: $(BINDIR) $(PROGS)
	$(INSTALL) $(PROGS) $(BINDIR)

install.examples :: $(MY_DATADIR)
	$(INSTALL_DATA) explain.txt $(MY_DATADIR)

uninstall.examples ::
	-( cd $(BINDIR) && rm -f $(PROGS) )
	-rmdir $(BINDIR)
	-rm -f $(MY_DATADIR)/explain.txt
	-rmdir $(MY_DATADIR)

$(BINDIR) \
$(MY_DATADIR) :
	mkdir -p $@

AUTO_SRC = $(THISLIB)-explanation.adb

ncurses$x : $(AUTO_SRC)
	$(ADAMAKE) $(ADAMAKEFLAGS) ncurses $(CARGS) $(LARGS)

tour$x : $(AUTO_SRC)
	$(ADAMAKE) $(ADAMAKEFLAGS) tour $(CARGS) $(LARGS)

rain$x : $(AUTO_SRC)
	$(ADAMAKE) $(ADAMAKEFLAGS) rain $(CARGS) $(LARGS)

mostlyclean:
	@echo made $@

clean :: mostlyclean
	rm -f *.o *.ali b_t*.* *.s $(PROGS) a.out core b_*_test.c *.xr[bs] \
	rm -f $(AUTO_SRC)
	rm -f trace screendump b~*.ad[bs]

distclean :: clean
	rm -f Makefile

realclean :: distclean
	@echo made $@

$(THISLIB)-explanation.adb : $(srcdir)/$(THISLIB)-explanation.adb_p
	rm -f $@
	$(AWK) -v this_datadir=$(THIS_DATADIR) -f $(srcdir)/split-path.awk < $(srcdir)/$(THISLIB)-explanation.adb_p  >$@
