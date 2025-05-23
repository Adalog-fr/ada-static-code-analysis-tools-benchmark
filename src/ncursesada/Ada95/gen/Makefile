##############################################################################
# Copyright 2019-2020,2021 Thomas E. Dickey                                  #
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
#     and:  Thomas E. Dickey, 1997
#
#  $Id: Makefile.in,v 1.97 2021/07/03 15:45:33 tom Exp $
#
.SUFFIXES:

SHELL		= /bin/sh

THIS		= Makefile

x		= 

top_srcdir	= ../..
DESTDIR		= 
srcdir		= .
prefix		= /usr
exec_prefix	= ${prefix}
bindir		= ${exec_prefix}/bin
includedir	= ${prefix}/include

INSTALL		= /usr/bin/install -c 
INSTALL_SCRIPT	= ${INSTALL}
INSTALL_DATA	= ${INSTALL} -m 644

AWK		= mawk
LN_S		= ln -s -f

CC		= gcc
HOST_CC		= ${CC}

CFLAGS		= -O2 --param max-inline-insns-single=1200 
HOST_CFLAGS	= ${CFLAGS}

CPPFLAGS	= -I. -I../include -D_DEFAULT_SOURCE -D_XOPEN_SOURCE=600 -DNDEBUG
HOST_CPPFLAGS	= -I. -I../include -I../../include  ${CPPFLAGS} \
		  -DHAVE_CONFIG_H -I$(srcdir)

CCFLAGS		= $(HOST_CPPFLAGS) $(HOST_CFLAGS)
CFLAGS_NORMAL	= $(CCFLAGS) -DNCURSES_STATIC
CFLAGS_DEBUG	= $(CCFLAGS) -DNCURSES_STATIC -g -DTRACE
CFLAGS_PROFILE	= $(CCFLAGS) -DNCURSES_STATIC -pg
CFLAGS_SHARED	= $(CCFLAGS) -fPIC

CFLAGS_DEFAULT	= $(CFLAGS_NORMAL)

REL_VERSION	= 6.3
ABI_VERSION	= 6
LOCAL_LIBDIR	= /workspaces/bench-source/src/ncursesada/lib

LINK		= $(HOST_CC)
LDFLAGS		= 
LIBS		= 

# For the wide-character configuration combined with broken_linker, we need
# addresses of ACS_xxx constants, which requires linking with the newly built
# ncurses library.  If cross-compiling, the developer has to fill in a workable
# native library for this purpose.
NATIVE_LDFLAGS	=  $(LOCAL_LIBS) -L../../lib $(LDFLAGS)   $(LDFLAGS) -lncurses
CROSS_LDFLAGS	= ${LDFLAGS} ${LIBS}

ACTUAL_LDFLAGS	= $(NATIVE_LDFLAGS)

RANLIB		= ranlib

M4		= m4
M4FLAGS		= -DNCURSES_EXT_FUNCS=1

ADACURSES_CONFIG = adacurses6-config

WRAPPER		= /bin/sh $(top_srcdir)/misc/shlib

PROG_GENERATE	= ./generate$x
GENERATE	= $(PROG_GENERATE) ''
DEL_ADAMODE	= sed -e '/^\-\-\ \ \-\*\-\ ada\ \-\*\-.*/d'

GNATHTML	= `type -p gnathtml || type -p gnathtml.pl 2>/dev/null`
GNATHP          = www.gnat.com

################################################################################
ALIB            = terminal_interface
ABASE		= $(ALIB)-curses

ADA_SRCDIR	= ../src

GEN_TARGETS     = $(ADA_SRCDIR)/$(ABASE).ads         			\
		  $(ADA_SRCDIR)/$(ABASE).adb         			\
	          $(ADA_SRCDIR)/$(ABASE)-aux.ads   			\
	          $(ADA_SRCDIR)/$(ABASE)-trace.ads   			\
	          $(ADA_SRCDIR)/$(ABASE)-menus.ads   			\
	          $(ADA_SRCDIR)/$(ABASE)-forms.ads   			\
	          $(ADA_SRCDIR)/$(ABASE)-mouse.ads 			\
	          $(ADA_SRCDIR)/$(ABASE)-panels.ads			\
	          $(ADA_SRCDIR)/$(ABASE)-menus-menu_user_data.ads	\
	          $(ADA_SRCDIR)/$(ABASE)-menus-item_user_data.ads	\
	          $(ADA_SRCDIR)/$(ABASE)-forms-form_user_data.ads	\
	          $(ADA_SRCDIR)/$(ABASE)-forms-field_types.ads		\
	          $(ADA_SRCDIR)/$(ABASE)-forms-field_user_data.ads	\
	          $(ADA_SRCDIR)/$(ABASE)-panels-user_data.ads           \
                  $(ADA_SRCDIR)/$(ABASE)_constants.ads

GEN_SRC		= $(srcdir)/$(ABASE).ads.m4				\
		  $(srcdir)/$(ABASE).adb.m4				\
	          $(srcdir)/$(ABASE)-aux.ads.m4 	  		\
	          $(srcdir)/$(ABASE)-trace.ads.m4 	  		\
	          $(srcdir)/$(ABASE)-menus.ads.m4 	  		\
	          $(srcdir)/$(ABASE)-forms.ads.m4   			\
	          $(srcdir)/$(ABASE)-mouse.ads.m4			\
	          $(srcdir)/$(ABASE)-panels.ads.m4			\
	          $(srcdir)/$(ABASE)-menus-menu_user_data.ads.m4	\
	          $(srcdir)/$(ABASE)-menus-item_user_data.ads.m4	\
	          $(srcdir)/$(ABASE)-forms-form_user_data.ads.m4	\
	          $(srcdir)/$(ABASE)-forms-field_types.ads.m4		\
	          $(srcdir)/$(ABASE)-forms-field_user_data.ads.m4	\
	          $(srcdir)/$(ABASE)-panels-user_data.ads.m4

################################################################################

.PHONY :	all
.PHONY :	clean
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
libs :	$(GEN_TARGETS)
	@echo made $@

sources:

$(DESTDIR)$(bindir) :
	mkdir -p $@

install \
install.libs :: $(DESTDIR)$(bindir) $(ADACURSES_CONFIG)
	$(INSTALL_SCRIPT) $(ADACURSES_CONFIG) $(DESTDIR)$(bindir)/$(ADACURSES_CONFIG)

uninstall \
uninstall.libs ::
	-rm -f $(DESTDIR)$(bindir)/$(ADACURSES_CONFIG)

$(PROG_GENERATE):  gen.o
	 $(LINK) $(CFLAGS_NORMAL) gen.o $(ACTUAL_LDFLAGS) -o $@

gen.o:	$(srcdir)/gen.c
	$(HOST_CC) $(CFLAGS_NORMAL) -c -o $@ $(srcdir)/gen.c

$(ADA_SRCDIR)/$(ABASE)_constants.ads: $(PROG_GENERATE)
	$(WRAPPER) "$(GENERATE) $@"
################################################################################
$(ADA_SRCDIR)/$(ABASE).ads: 	$(srcdir)/$(ABASE).ads.m4 \
				$(srcdir)/normal.m4
	$(M4) $(M4FLAGS) -DM4MACRO=$(srcdir)/normal.m4 \
	$(srcdir)/$(ABASE).ads.m4 |\
	$(DEL_ADAMODE) >$@

$(ADA_SRCDIR)/$(ABASE).adb: 	$(srcdir)/$(ABASE).adb.m4 \
				$(srcdir)/normal.m4
	$(M4) $(M4FLAGS) -DM4MACRO=$(srcdir)/normal.m4 \
	$(srcdir)/$(ABASE).adb.m4 |\
	$(DEL_ADAMODE) >$@

$(ADA_SRCDIR)/$(ABASE)-aux.ads: $(srcdir)/$(ABASE)-aux.ads.m4 \
				$(srcdir)/normal.m4
	$(M4) $(M4FLAGS) -DM4MACRO=$(srcdir)/normal.m4 \
	$(srcdir)/$(ABASE)-aux.ads.m4 |\
	$(DEL_ADAMODE) >$@

$(ADA_SRCDIR)/$(ABASE)-trace.ads: $(srcdir)/$(ABASE)-trace.ads.m4 \
				  $(srcdir)/normal.m4
	$(M4) $(M4FLAGS) -DM4MACRO=$(srcdir)/normal.m4 \
	$(srcdir)/$(ABASE)-trace.ads.m4 |\
	$(DEL_ADAMODE) >$@

$(ADA_SRCDIR)/$(ABASE)-menus.ads: $(srcdir)/$(ABASE)-menus.ads.m4 \
				  $(srcdir)/normal.m4
	$(M4) $(M4FLAGS) -DM4MACRO=$(srcdir)/normal.m4 \
	$(srcdir)/$(ABASE)-menus.ads.m4 |\
	$(DEL_ADAMODE) >$@

$(ADA_SRCDIR)/$(ABASE)-forms.ads: $(srcdir)/$(ABASE)-forms.ads.m4 \
				  $(srcdir)/normal.m4
	$(M4) $(M4FLAGS) -DM4MACRO=$(srcdir)/normal.m4 \
	$(srcdir)/$(ABASE)-forms.ads.m4 |\
	$(DEL_ADAMODE) >$@

$(ADA_SRCDIR)/$(ABASE)-mouse.ads: $(srcdir)/$(ABASE)-mouse.ads.m4 \
				  $(srcdir)/normal.m4
	$(M4) $(M4FLAGS) -DM4MACRO=$(srcdir)/normal.m4 \
	$(srcdir)/$(ABASE)-mouse.ads.m4 |\
	$(DEL_ADAMODE) >$@

$(ADA_SRCDIR)/$(ABASE)-panels.ads: $(srcdir)/$(ABASE)-panels.ads.m4 \
				   $(srcdir)/normal.m4
	$(M4) $(M4FLAGS) -DM4MACRO=$(srcdir)/normal.m4 \
	$(srcdir)/$(ABASE)-panels.ads.m4 |\
	$(DEL_ADAMODE) >$@

$(ADA_SRCDIR)/$(ABASE)-menus-menu_user_data.ads: \
		$(srcdir)/$(ABASE)-menus-menu_user_data.ads.m4 \
		$(srcdir)/normal.m4
	$(M4) $(M4FLAGS) -DM4MACRO=$(srcdir)/normal.m4 \
	$(srcdir)/$(ABASE)-menus-menu_user_data.ads.m4 |\
	$(DEL_ADAMODE) >$@

$(ADA_SRCDIR)/$(ABASE)-menus-item_user_data.ads: \
		$(srcdir)/$(ABASE)-menus-item_user_data.ads.m4 \
		$(srcdir)/normal.m4
	$(M4) $(M4FLAGS) -DM4MACRO=$(srcdir)/normal.m4 \
	$(srcdir)/$(ABASE)-menus-item_user_data.ads.m4 |\
	$(DEL_ADAMODE) >$@

$(ADA_SRCDIR)/$(ABASE)-forms-form_user_data.ads: \
		$(srcdir)/$(ABASE)-forms-form_user_data.ads.m4 \
		$(srcdir)/normal.m4
	$(M4) $(M4FLAGS) -DM4MACRO=$(srcdir)/normal.m4 \
	$(srcdir)/$(ABASE)-forms-form_user_data.ads.m4 |\
	$(DEL_ADAMODE) >$@

$(ADA_SRCDIR)/$(ABASE)-forms-field_types.ads: \
		$(srcdir)/$(ABASE)-forms-field_types.ads.m4 \
		$(srcdir)/normal.m4
	$(M4) $(M4FLAGS) -DM4MACRO=$(srcdir)/normal.m4 \
	$(srcdir)/$(ABASE)-forms-field_types.ads.m4 |\
	$(DEL_ADAMODE) >$@

$(ADA_SRCDIR)/$(ABASE)-forms-field_user_data.ads: \
		$(srcdir)/$(ABASE)-forms-field_user_data.ads.m4 \
		$(srcdir)/normal.m4
	$(M4) $(M4FLAGS) -DM4MACRO=$(srcdir)/normal.m4 \
	$(srcdir)/$(ABASE)-forms-field_user_data.ads.m4 |\
	$(DEL_ADAMODE) >$@

$(ADA_SRCDIR)/$(ABASE)-panels-user_data.ads: \
		$(srcdir)/$(ABASE)-panels-user_data.ads.m4 \
		$(srcdir)/normal.m4
	$(M4) $(M4FLAGS) -DM4MACRO=$(srcdir)/normal.m4 \
	$(srcdir)/$(ABASE)-panels-user_data.ads.m4 |\
	$(DEL_ADAMODE) >$@

install.progs ::

tags:
	ctags *.[ch]

#TAGS:
#	etags *.[ch]

mostlyclean ::
	-rm -f a.out core $(PROG_GENERATE) *.o

clean :: mostlyclean
	-rm -f $(GEN_TARGETS) instab.tmp *.ad[bs] *.html *.ali *.tmp

distclean :: clean
	-rm -f $(ADACURSES_CONFIG)
	-rm -f Makefile

realclean :: distclean

HTML_DIR = ../../doc/html/ada

instab.tmp : table.m4 $(GEN_SRC)
	@rm -f $@
	@for f in $(GEN_SRC) ; do \
	  $(M4) $(M4FLAGS) -DM4MACRO=table.m4 $$f | $(DEL_ADAMODE) >> $@ ;\
	done;

$(HTML_DIR)/table.html : instab.tmp
	@-touch $@
	@-chmod +w $@
	@echo '<!DOCTYPE HTML'  > $@
	@echo 'PUBLIC "-//IETF//DTD HTML 3.0//EN">' >> $@
	@echo '<HTML>' >> $@
	@echo '<HEAD>' >> $@
	@echo '<TITLE>Correspondence between ncurses C and Ada functions</TITLE>' >>$@
	@echo '</HEAD>' >> $@
	@echo '<BODY>' >> $@
	@echo '<H1>Correspondence between ncurses C and Ada functions</H1>' >>$@
	@echo '<H2>Sorted by C function name</H2>' >>$@
	@echo '<TABLE ALIGN=CENTER BORDER>' >>$@
	@echo '<TR ALIGN=LEFT>' >>$@
	@echo '<TH>C name</TH><TH>Ada name</TH><TH>man page</TH></TR>' >>$@
	@sort < instab.tmp >> $@
	@echo '</TABLE></BODY></HTML>' >>$@
	@rm -f instab.tmp

adahtml:
	test -n "$(GNATHTML)" || exit 1
	@find $(HTML_DIR) -type f -exec rm -f {} \;
	@mkdir -p $(HTML_DIR)
	( cp -p ../src/*.ad[sb] . && chmod +w *.ad[sb] )
#	ln -sf ../src/*.ali .
	ln -sf ../static-ali/*.ali .
	@echo "Filtering generated files"
	@for f in $(GEN_SRC); do \
	   h=`basename $$f` ;\
	   g=`basename $$f .ads.m4` ;\
	   if test "$$g" != "$$h" ; then \
	      $(M4) $(M4FLAGS) -DM4MACRO=html.m4 $$f | $(DEL_ADAMODE) > $$g.ads ;\
	      echo "... $$g.ads" ;\
	   fi \
	done
	@-rm -f $(HTML_DIR)/$(ALIB)*.htm*
	$(GNATHTML) -d -f $(ALIB)*.ads
	for f in html/$(ALIB)*.htm*; do \
	   a=`basename $$f` ; \
	   sed -e 's/You may also.*body.*//' <$$f |\
	   sed -e 's%GNAT%<A HREF="http://$(GNATHP)">GNAT</A>%g' |\
	   sed -e 's%&lt;A HREF%<A HREF%g' |\
	   sed -e 's%"&gt;%">%g' |\
	   sed -e 's%#1A\([[:space:]]*NAME="[^"]*"\)#2%<A\1>%' |\
	   sed -e 's%#1/A#2%</A>%' |\
	   sed -e 's/3X/3x/g' |\
	   sed -e 's/$$\([ABCDEFGHIJKLMNOPQRSTUVWXZabcdefghijklmnopqrstuvwxz0123456789_]*:.*\)\$$/@\1@/' |\
	   sed -e 's%&lt;/A&gt;%</A>%g' > $$a.tmp ;\
	   mv $$a.tmp $$f ;\
	done
	@rm -f *.ad[sb] *.ali *.tmp
	@for f in funcs.htm main.htm ; do \
	  sed -e "\%<A HREF=funcs/ .htm>\[ \]</A>%d" < html/$$f > $$f ;\
	  mv $$f html/$$f ;\
	done
	@rm -f "html/funcs/ .htm"
	@cp -pdrf html/* $(HTML_DIR)/
	@rm -rf html

html : adahtml $(HTML_DIR)/table.html
	@echo made $@

###############################################################################
# The remainder of this file is automatically generated during configuration
###############################################################################
