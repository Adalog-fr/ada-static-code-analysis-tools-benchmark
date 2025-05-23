# Build elisp code, Ada executables, manuals; publish to web and ELPA
#
# The gprbuild commands depend on the GPR_PROJECT_PATH environment
# variable, that is set in the project files loaded in the file local
# variables below.

export ADA_MODE_VERSION       := 8.0.5
export ADA_REF_MAN_VERSION    := 2012.5

TEST_DIR ?= source

ELPA_ROOT ?= $(shell cd ../../elpa; pwd)

# emacs to test with
#
# This can be overridden on the 'make' command line or by an external
# environment variable.
ifeq ($(shell uname),Linux)
EMACS_EXE ?= emacs
else ifeq ($(shell uname),Darwin)
EMACS_EXE ?= "/Applications/Emacs.app/Contents/MacOS/Emacs"
else
# windows
EMACS_EXE ?= emacs
endif

# compile with debug with default Standard_Common_Assertions to get
# assertions for testing new code, or to allow running debugger.
# assertions off for speed
# export Standard_Common_Build := Debug
# export Standard_Common_Assertions := Off

all : byte-compile-clean compile-ada-test-clean update compile-ada-test test-clean test docs

# 'pub' does not do install, because 'build-elpa' uses the last
# commit, not the current workspace.
pub : pub-ada
# (dvc-state-multiple "c:/Projects/elpa/packages")
pub-install: build-elpa install-elpa

docs : ada-mode.info ada-mode.html

recover-compare : wisitoken-ada-recover-compare.stamp
recover-compare : libadalang-recover-compare.stamp
recover-compare : wisitoken-ada_lite-recover-compare.stamp
recover-compare : tree_sitter-ada_lite-recover-compare.stamp
recover-compare : compare_lengths.stamp

# This updates elisp and related executables after any source change,
# without running the tests.
update : build_executables
update : autoloads
update : byte-compile

# This updates and installs everything after any source change, without running the
# tests or publishing to elpa
update-install : update
update-install : install
update-install : docs

install : install_executables

# Doing byte-compile-clean first avoids errors caused by
# loading new source on old .elc.
#
# 'package-initialize' is required for 'uniquify-files'.
byte-compile : byte-compile-clean
	$(MAKE) -C $(WISI)/build byte-compile autoloads
	$(MAKE) -C $(GNAT_COMPILER) -f ELPA.make byte-compile autoloads
	$(MAKE) -C $(GPR_QUERY) -f ELPA.make byte-compile autoloads
#	$(MAKE) -C /Projects/eglot-stephe compile
	cd ../; $(EMACS_EXE) -Q -batch -L . -L $(WISI) -L $(GNAT_COMPILER) -L $(GPR_QUERY) -l build/exclude-elpa.el --eval '(progn (package-initialize)(setq byte-compile-error-on-warn t)(batch-byte-compile))' *.el

byte-compile-clean :
	cd ..; rm -f *.elc

ifeq ($(TEST_DIR),source)
test : build_executables
endif
test : test-ada-lalr-partial-process-gpr_query.stamp
test : test-ada-lalr-incremental-process-gpr_query.stamp
test : test-ada-lalr-process-gnatxref.stamp
test : test-ada-eglot.stamp
test : summarize

summarize :
	cat test.log

benchmark : RUN_ARGS ?= c:/eurocontrol/gnatcoll-xref.adb
benchmark :
	$(EMACS_EXE) -Q -L . $(ADA_MODE_DIR) -l benchmark.el $(RUN_ARGS)

# For running a test with ELPA build
#ADA_MODE_DIR := -L c:/Projects/elpa_release/ada-mode -l c:/Projects/elpa_release/ada-mode/autoloads.el -L c:/Projects/elpa_release/wisi -l c:/Projects/elpa_release/wisi/autoloads.el -l exclude-elpa.el

ONE_TEST_FILE ?= ada_mode-child_procedure.adb
one :: ELISP ?= (setq-default debug-on-error t save-parser-log (list (list "*EGLOT (_default_/(ada-mode)) events*" "/Projects/org.emacs.ada-mode/debug-1.log")))
#one :: ELISP ?= (setq-default save-parser-log "../debug-1.log" save-edited-text "../debug_edited-1")
#one :: ELISP ?= (setq-default debug-on-error t)
one :: IGNORE_CONFLICTS := --generate LALR Ada_Emacs Process re2c
#one :: build_executables
#one :: byte-compile autoloads
#one :: compile-ada-test
one :: one-clean
one-clean :: force
	for file in $(ONE_TEST_FILE) ; do rm -f $$file.* ; done
#	rm -f ../test/gpr_query.db*
#one :: RUNTEST := run-indent-test-elpa.el
#one :: RUNTEST := run-indent-test-lalr-incremental-process-gpr_query.el
#one :: RUNTEST := run-indent-test-lalr-partial-process-gpr_query.el
#one :: RUNTEST := run-indent-test-lalr-process-gnatxref.el
one :: RUNTEST := run-indent-test-eglot.el
#one :: $(addsuffix .diff-run, $(ONE_TEST_FILE))
one :: $(addsuffix .diff, $(ONE_TEST_FILE))

#one-debug :: RUNTEST := run-indent-test-lalr-incremental-process-gpr_query.el
#one-debug :: RUNTEST := run-indent-test-lalr-partial-process-gpr_query.el
#one-debug :: RUNTEST := run-indent-test-lalr-process-gnatxref.el
one-debug :: RUNTEST := run-indent-test-eglot.el
one-debug :: ELISP ?= (setq-default debug-on-error t)
one-debug :: force
	$(EMACS_EXE) -Q -L . $(ADA_MODE_DIR) -l $(RUNTEST) --eval '(progn $(ELISP))'

oneb :: one-clean
#oneb :: RUNTEST := run-indent-test-lalr-incremental-process-gpr_query.el
oneb :: RUNTEST := run-indent-test-lalr-partial-process-gpr_query.el
oneb :: $(addsuffix .diff, $(ONE_TEST_FILE))

#two : RUN_ARGS ?= command_file debug.cmd
two : LOG_ARGS ?= > debug.log 2>&1
two : RUN_ARGS ?= parse_partial none test/debug.adb --verbosity "debug=1 parse=2"
two : IGNORE_CONFLICTS := --generate LALR Ada_Emacs Process re2c
two : ../run_ada_lalr_parse.exe
	cd ../; bin/run_ada_lalr_parse.exe $(RUN_ARGS) $(LOG_ARGS)

two-lr1 : LOG_ARGS ?= > debug-lr1.log 2>&1
two-lr1 : RUN_ARGS ?= parse_partial none test/debug.adb --verbosity "debug=1 parse=2"
two-lr1 : ../run_ada_lr1_parse.exe
	cd ../; bin/run_ada_lr1_parse.exe $(RUN_ARGS) $(LOG_ARGS)

two_mem : export Standard_Common_Mem_Check := On
two_mem : build_executables
	../exec_mem/run_ada_lalr_parse.exe c:/tmp/regulation-iflight.adb Face
	gnatmem ../exec_mem/run_ada_lalr_parse.exe > regulation_iflight.memory

two_pro : export Standard_Common_Profile := On
two_pro : build_executables
	../exec_pro/run_ada_lalr_parse.exe ../test/ada_mode-recover_partial_02_lr1.adb Indent --enqueue_limit 120_000 --task_count 1
	gprof ../exec_pro/run_ada_lalr_parse.exe > recover_partial_02-lalr.profile

three : RUN_ARGS := c:/Eurocontrol 10
three : run_libadalang.exe
	cd ..; ./run_libadalang.exe $(RUN_ARGS)

# WISITOKEN is correct for Stephe's development machines;
# it can be overridden on the 'make' command line or by an
# external environment variable.
#
# ADA_PROJECT_PATH is used by the ../test/*.ad? files to test Ada mode
# interaction with gpr project files.
export WISITOKEN     ?= $(shell cd ../../org.wisitoken; pwd)
export WISI          ?= $(shell cd ../../org.emacs.wisi; pwd)
export GNAT_COMPILER ?= $(shell cd ../../elpa/packages/gnat-compiler; pwd)
export GPR_QUERY     ?= $(shell cd ../../elpa/packages/gpr-query; pwd)
export ADA_PROJECT_PATH=../test/:../test/subdir

# WISITOKEN_GENERATE must be after WISITOKEN
ifeq ($(TEST_DIR),source)
# test with development source
ADA_MODE_DIR := -L .. -l ../autoloads.el -L $(WISI) -l $(WISI)/autoloads.el -L $(GNAT_COMPILER) -l $(GNAT_COMPILER)/autoloads.el -L $(GPR_QUERY) -l $(GPR_QUERY)/autoloads.el -l exclude-elpa.el

# ".exe", not $(EXE_EXT); wisitoken hard-codes .exe
WISITOKEN_GENERATE := $(WISITOKEN)/build/wisitoken-bnf-generate.exe

else
# test with installed elpa package; don't rebuild grammars
ADA_MODE_DIR :=
WISITOKEN_GENERATE :=
endif

# 'include' must be after WISITOKEN_GENERATE and other vars are set.
include rules.make

update-test-log : force
	find . -maxdepth 1 -name "*.diff" -not -size 0 >> test.log

test-ada-eglot : RUNTEST := run-indent-test-eglot.el
test-ada-eglot : $(addsuffix .diff, $(subst subdir/,,$(GNATXREF_TEST_FILES)))

test-ada-eglot.stamp : force
	rm -f *.diff *.tmp
	$(MAKE) test-ada-eglot
	touch $@
	echo "test-ada-eglot" >> test.log
	find . -name "*.diff" -not -size 0 >> test.log

test-ada-lalr-process-gnatxref : RUNTEST := run-indent-test-lalr-process-gnatxref.el
test-ada-lalr-process-gnatxref : $(addsuffix .diff, $(subst subdir/,,$(GNATXREF_TEST_FILES)))

test-ada-lalr-process-gnatxref.stamp : force
	rm -f *.diff *.tmp
	$(MAKE) test-ada-lalr-process-gnatxref
	touch $@
	echo "test-ada-lalr-process-gnatxref" >> test.log
	find . -name "*.diff" -not -size 0 >> test.log

test-ada-lalr-partial-process-gpr_query : RUNTEST := run-indent-test-lalr-partial-process-gpr_query.el
test-ada-lalr-partial-process-gpr_query : $(addsuffix .diff, $(subst subdir/,,$(ADA_TEST_FILES)))

test-ada-lalr-partial-process-gpr_query.stamp : force
	rm -f *.diff *.tmp
	$(MAKE) test-ada-lalr-partial-process-gpr_query
	touch $@
	echo "test-ada-lalr-partial-process-gpr_query" >> test.log
	find . -name "*.diff" -not -size 0 >> test.log

test-ada-lalr-incremental-process-gpr_query : RUNTEST := run-indent-test-lalr-incremental-process-gpr_query.el
test-ada-lalr-incremental-process-gpr_query : $(addsuffix .diff, $(subst subdir/,,$(ADA_TEST_FILES)))

test-ada-lalr-incremental-process-gpr_query.stamp : force
	rm -f *.diff *.tmp
	$(MAKE) test-ada-lalr-incremental-process-gpr_query
	touch $@
	echo "test-ada-lalr-incremental-process-gpr_query" >> test.log
	find . -name "*.diff" -not -size 0 >> test.log

ifeq ($(shell uname),Linux)
EXE_EXT :=

GNAT_EXE    := $(shell which gnat)
INSTALL_BIN := $(dir $(GNAT_EXE))

else ifeq ($(shell uname),Darwin)
EXE_EXT :=

GNAT_EXE    := $(shell which gnat)
INSTALL_BIN := $(dir $(GNAT_EXE))

else
# windows
EXE_EXT := .exe

# make can't see 'type', so we use 'which'
GNAT_EXE    := $(shell cygpath --mixed $(shell which gnat))
INSTALL_BIN := $(dir $(GNAT_EXE))

endif

BRANCH := $(notdir $(shell cd ..; pwd))

ifeq ($(BRANCH),org.emacs.ada-mode)
  TAR_FILE := org.emacs.ada-mode-$(ADA_MODE_VERSION)
else
  TAR_FILE := $(BRANCH)
endif

wisitoken-ada-recover-compare.stamp : ada-correct-tokens.stamp wisitoken-tokens.stamp
	-for file in $(RECOVER_TEST_FILES) ; do diff -u ada-correct-tokens/$$file.tokens wisitoken-tokens/$$file.tokens > wisitoken-tokens/$$file.diff ; done
	touch $@

wisitoken-ada_lite-recover-compare.stamp : ada_lite-correct-tokens.stamp  wisitoken-ada_lite-tokens.stamp force
	-for file in $(RECOVER_TEST_FILES) ; do if test -f ../test/ada_lite/$$file ; then diff -u ada_lite-correct-tokens/$$file.tokens wisitoken-ada_lite-tokens/$$file.tokens > wisitoken-ada_lite-tokens/$$file.diff ; fi ; done
	touch $@

tree_sitter-ada_lite-recover-compare.stamp : ada_lite-correct-tokens.stamp  tree_sitter-ada_lite-tokens.stamp force
	-for file in $(RECOVER_TEST_FILES) ; do if test -f ../test/ada_lite/$$file ; then diff -u ada_lite-correct-tokens/$$file.tokens tree_sitter-ada_lite-tokens/$$file.tokens > tree_sitter-ada_lite-tokens/$$file.diff ; fi ; done
	touch $@

libadalang-recover-compare.stamp : ada-correct-tokens.stamp  libadalang-tokens force
	-for file in $(RECOVER_TEST_FILES) ; do diff -u ada-correct-tokens/$$file.tokens libadalang-tokens/$$file.tokens > libadalang-tokens/$$file.diff ; done
	touch $@

wisitoken-libadalang-diff : wisitoken-tokens.stamp libadalang-tokens.stamp
	rm -f *.diff
	-for file in $(RECOVER_TEST_FILES) ; do diff -u wisitoken-tokens/$$file.tokens libadalang-tokens/$$file.tokens > $$file.diff ; done

compare_lengths.log : compare_lengths$(EXE_EXT) sum_diff_lengths$(EXE_EXT)
	cd wisitoken-tokens; ../../sum_diff_lengths > ../compare_lengths.log
	cd libadalang-tokens; ../../sum_diff_lengths >> ../compare_lengths.log
	cd wisitoken-ada_lite-tokens; ../../sum_diff_lengths >> ../compare_lengths.log
	cd tree_sitter-ada_lite-tokens; ../../sum_diff_lengths >> ../compare_lengths.log
	../compare_lengths wisitoken-tokens libadalang-tokens >> compare_lengths.log
	../compare_lengths wisitoken-ada_lite-tokens tree_sitter-ada_lite-tokens >> compare_lengths.log

# default to LR1 for error correction
DUMP_ALG ?= LR1

wisitoken-tokens.stamp : ../dump_wisitoken_corrected$(EXE_EXT)
	mkdir -p wisitoken-tokens
	for file in $(RECOVER_TEST_FILES) ; do ../dump_wisitoken_corrected $(DUMP_ALG)  ../test/$$file > wisitoken-tokens/$$file.tokens ; done
	touch $@

wisitoken-ada_lite-tokens.stamp : $(WISITOKEN)/build/dump_wisitoken_ada_lite_corrected.exe
	mkdir -p wisitoken-ada_lite-tokens
	for file in $(RECOVER_TEST_FILES) ; do if test -f ../test/ada_lite/$$file ; then $(WISITOKEN)/build/dump_wisitoken_ada_lite_corrected $(DUMP_ALG) ../test/ada_lite/$$file > wisitoken-ada_lite-tokens/$$file.tokens ; fi ; done
	touch $@

$(WISITOKEN)/build/dump_wisitoken_ada_lite_corrected.exe : force
	$(MAKE) -C $(WISITOKEN)/build dump_wisitoken_ada_lite_corrected.exe

wisitoken-ada_lite-tokens/%.tokens : ../test/ada_lite/% $(WISITOKEN)/build/dump_wisitoken_ada_lite_corrected.exe force
	$(WISITOKEN)/build/dump_wisitoken_ada_lite_corrected.exe LR1 $< $(VERBOSITY) > $@

wisitoken-ada_lite-tokens/%.diff : wisitoken-ada_lite-tokens/%.tokens ada_lite-correct-tokens/%.tokens
	-diff -u ada_lite-correct-tokens/$*.tokens wisitoken-ada_lite-tokens/$*.tokens > $@

tree_sitter-ada_lite-tokens.stamp : $(WISITOKEN)/build/dump_tree_sitter_ada_lite_corrected.exe
	mkdir -p tree_sitter-ada_lite-tokens
	for file in $(RECOVER_TEST_FILES) ; do if test -f ../test/ada_lite/$$file ; then $(WISITOKEN)/build/dump_tree_sitter_ada_lite_corrected.exe ../test/ada_lite/$$file > tree_sitter-ada_lite-tokens/$$file.tokens ; fi ; done
	touch $@

tree_sitter-ada_lite-tokens/%.tokens : ../test/ada_lite/% $(WISITOKEN)/build/dump_tree_sitter_ada_lite_corrected.exe force
	$(WISITOKEN)/build/dump_tree_sitter_ada_lite_corrected.exe LR1 $< $(VERBOSITY) > $@

tree_sitter-ada_lite-tokens/%.diff : tree_sitter-ada_lite-tokens/%.tokens ada_lite-correct-tokens/%.tokens
	-diff -u ada_lite-correct-tokens/$*.tokens tree_sitter-ada_lite-tokens/$*.tokens > $@

$(WISITOKEN)/build/dump_tree_sitter_ada_lite_corrected.exe : force
	$(MAKE) -C $(WISITOKEN)/build dump_tree_sitter_ada_lite_corrected.exe

libadalang-tokens : ../dump_libadalang_corrected$(EXE_EXT)
	mkdir -p libadalang-tokens
	for file in $(RECOVER_TEST_FILES) ; do ../dump_libadalang_corrected  ../test/$$file > libadalang-tokens/$$file.tokens ; done

ada-correct-tokens.stamp : ../dump_wisitoken_corrected$(EXE_EXT)
	mkdir -p ada-correct-tokens
	for file in $(RECOVER_TEST_FILES) ; do ../dump_wisitoken_corrected LALR ../test/correct/$$file > ada-correct-tokens/$$file.tokens ; done
	touch $@

ada_lite-correct-tokens.stamp : $(WISITOKEN)/build/dump_wisitoken_ada_lite_corrected$(EXE_EXT)
	mkdir -p ada_lite-correct-tokens
	for file in $(RECOVER_TEST_FILES) ; do $(WISITOKEN)/build/dump_wisitoken_ada_lite_corrected LALR ../test/ada_lite-correct/$$file > ada_lite-correct-tokens/$$file.tokens ; done
	touch $@

ada_lite-correct-tokens/%.tokens : ../test/ada_lite-correct/% $(WISITOKEN)/build/dump_wisitoken_ada_lite_corrected$(EXE_EXT)
	$(WISITOKEN)/build/dump_wisitoken_ada_lite_corrected$(EXE_EXT) LR1 $< $(VERBOSITY) > $@

zip :
	rm -rf ../../$(TAR_FILE)
	mtn checkout --branch $(BRANCH) ../../$(TAR_FILE)
	tar jcf $(TAR_FILE).tar.bz2 --exclude _MTN -C ../.. $(TAR_FILE)


# tarball of ELPA packages source, for sending to customers with their
# own CM but without good network access
DATE := $(shell date +%Y-%m-%d)
elpa-zip : ada-mode-elpa-$(DATE).tar.gz

ada-mode-elpa-$(DATE).tar.gz :
	tar zcf ada-mode-elpa-$(DATE).tar.gz -C $(ELPA_ROOT)/packages ada-ref-man ada-mode wisi

# tarball to create local ELPA archive
elpa-archive-zip : ada-mode-elpa-archive-$(DATE).tar.bz2

ada-mode-elpa-archive-$(DATE).tar.bz2 : $(ELPA_ROOT)/archive/packages/archive-contents
	tar -c -C $(ELPA_ROOT) archive/packages | bzip2 -9 > ada-mode-elpa-archive-$(DATE).tar.bz2

VPATH := ..

### Publish Gnu ELPA packages
# First delete all files in Gnu ELPA, so we catch files deleted here.
#
# Copy files to Gnu ELPA ada-mode package
pub-ada : force | $(ELPA_ROOT)/packages/ada-mode
	rm -rf $(ELPA_ROOT)/packages/ada-mode/*
	cp ../*.el ../*.texi ../ada_license.text $(ELPA_ROOT)/packages/ada-mode
	cp ../build.sh ../install.sh ../ada_mode_wisi_parse.gpr $(ELPA_ROOT)/packages/ada-mode/
	cp ../*.make $(ELPA_ROOT)/packages/ada-mode/
	cp ../ada_mode.prj ../ada-mode.casing ../prj.el $(ELPA_ROOT)/packages/ada-mode/
	cp ../NEWS $(ELPA_ROOT)/packages/ada-mode/NEWS
	cp ../README $(ELPA_ROOT)/packages/ada-mode/README
	cp ../ada_annex_p.wy ../ada*.ad? ../run_ada*.ad? ../wisi-ada*.ad? $(ELPA_ROOT)/packages/ada-mode
	cd $(ELPA_ROOT)/packages/ada-mode; rm -f ada-ref-man.el ada_annex_p_bnf.wy ada_annex_p_txt.wy autoloads.el
	cp ../wisitoken-*-ada.ad? $(ELPA_ROOT)/packages/ada-mode
	cp ../*.c $(ELPA_ROOT)/packages/ada-mode

# copy files to ELPA ada-ref-man package
# FIXME: move to arm_form
pub-ada-ref-man: ARM_INFO ?= c:/Projects/org.adaic.arm_form
pub-ada-ref-man : force
	rm -rf $(ELPA_ROOT)/packages/ada-ref-man/*
	rm -f $(ARM_INFO)/build/*2005.info
	make -C $(ARM_INFO)/build create-dir
	cp ../ada-ref-man.el $(ELPA_ROOT)/packages/ada-ref-man/
	cp $(ARM_INFO)/NEWS $(ELPA_ROOT)/packages/ada-ref-man
	cp $(ARM_INFO)/build/dir $(ELPA_ROOT)/packages/ada-ref-man
	cp $(ARM_INFO)/build/arm2012.info $(ELPA_ROOT)/packages/ada-ref-man
	cp $(ARM_INFO)/build/aarm2012.info $(ELPA_ROOT)/packages/ada-ref-man
	cp $(ARM_INFO)/source_2012/* $(ELPA_ROOT)/packages/ada-ref-man/source_2012
	cp $(ARM_INFO)/build/arm2020.info $(ELPA_ROOT)/packages/ada-ref-man
	cp $(ARM_INFO)/build/aarm2020.info $(ELPA_ROOT)/packages/ada-ref-man
	cp $(ARM_INFO)/source_2020/* $(ELPA_ROOT)/packages/ada-ref-man/source_2020
	cp $(ARM_INFO)/progs/* $(ELPA_ROOT)/packages/ada-ref-man/progs
	cp $(ARM_INFO)/build/Makefile $(ELPA_ROOT)/packages/ada-ref-man/build
	cp $(ARM_INFO)/build/arm_info.gpr $(ELPA_ROOT)/packages/ada-ref-man/build
	cp $(ARM_INFO)/build/arm_info.prj $(ELPA_ROOT)/packages/ada-ref-man/build
	cp $(ARM_INFO)/build/download.py $(ELPA_ROOT)/packages/ada-ref-man/build
	cp $(ARM_INFO)/build/emacs_case_exceptions $(ELPA_ROOT)/packages/ada-ref-man/build


# builds $(ELPA_ROOT)/archive-devel/*, from the last commit, _not_ the
# current workspace Also checks copyright; run elpa/GNUMakefile
# check/<pkg> first if added files.
build-elpa : force
	rm -rf $(ELPA_ROOT)/archive-devel
	make -C $(ELPA_ROOT)/ build/wisi build/gnat-compiler build/ada-mode

uninstall-elpa :
	emacs -Q --eval '(progn (load-file "uninstall-elpa.el")(kill-emacs))'

# We don't kill emacs here, so we can check for compilation errors/warnings
install-elpa :
	emacs -Q --eval '(load-file "install-elpa.el")'

$(WISI)/wisi.gpr : force
	$(MAKE) -C $(WISI)/build ../wisi.gpr

# RUN_ARG is a list of addresses from a stack dump
trace :
	addr2line -e ../run_ada_lalr_parse.exe $(RUN_ARG)

%$(EXE_EXT) : %.adb force
	cd ..; gprbuild -p -j8 -P ada_mode_wisi_parse.gpr $*.adb

build_executables : $(WISI)/wisi.gpr ../ada_annex_p_re2c.c force
	gprbuild -p -j8 ../ada_mode_wisi_parse.gpr
	cd ..; if [ -f ada_annex_p_lr1_parse_table.txt ] ; then mv ada_annex_p_lr1_parse_table.txt bin ; fi

install_executables : build_executables
	gprinstall -f -p -P ../ada_mode_wisi_parse.gpr --install-name=ada_mode_wisi_parse

switch_compiler : force
	rm -rf ../obj
	rm -rf $(WISI)/obj
	rm -rf ../../org.stephe_leake.aunit_ext/build/objects
	rm -rf ../../org.stephe_leake.sal/build/obj
	rm -rf ../../org.stephe_leake.sal/build/devel_obj
	rm -rf ../../org.wisitoken/build/obj
	rm -rf ../../org.wisitoken/build/devel_obj
	rm -rf ../../org.wisitoken.grammar_mode/obj

../ada_annex_p_txt.wy : ../ada_annex_p.txt annex_p_to_wy.exe
	../annex_p_to_wy.exe ../ada_annex_p.txt ../ada_annex_p_txt.wy
	dos2unix -q ../ada_annex_p_txt.wy

compare_annex_p.stamp : ../ada_annex_p_txt.wy ../ada_annex_p.wy compare_annex_p.exe
	../compare_annex_p.exe ../ada_annex_p_txt.wy ../ada_annex_p.wy
	touch $@

# Local Variables:
# eval: (unless dvc-doing-ediff-p (load-file "prj.el"))
# end:
# end of file
