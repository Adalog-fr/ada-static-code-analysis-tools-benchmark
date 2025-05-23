# Gnu Makefile for WisiToken
#
# Top level rules here for visibility; lower level rules in rules.make

ZIP_VERSION := 4.1
#w-$(shell date +%Y-%m-%d)

all : test-clean tests recover_stats.exe wisitoken-followed_by.exe doc

#export Standard_Common_Build := Debug
#export Standard_Common_Assertions := Off
export GENERATE := BNF_EBNF

# test_one_harness.adb
# test_all_harness.adb
#
#t_one :: one-clean
t_one :: wisitoken-parse-lr-mckenzie_recover-ada_lite.ads
t_one :: wisitoken-parse-lr-mckenzie_recover-ada_lite.adb
t_one :: wisitoken-parse-lr-mckenzie_recover-ada_lite_ebnf.ads
t_one :: wisitoken-parse-lr-mckenzie_recover-ada_lite_ebnf.adb
t_one :: ada_lite_re2c.c
t_one :: ada_lite_ebnf_re2c.c
t_one :: grammar_grammar_01_re2c.c
t_one :: optimized_list_re2c.c
t_one :: skip_to_grammar_re2c.c
t_one :: test_one_harness.run

t_all :: wisitoken_test.gpr
t_all :: wisitoken-bnf-generate.exe gen test-executables
t_all :: test_all_harness.run

t_bnf :: wisitoken-bnf-generate.exe
t_bnf :: gen test-executables
t_bnf :: test_bnf_harness.run

t_mck :: wisitoken-bnf-generate.exe
#t_mck :: GENERATE_ARGS := -v 0 0 4
t_mck :: ada_lite_re2c.c
t_mck :: wisitoken-parse-lr-mckenzie_recover-ada_lite.adb
t_mck :: wisitoken-parse-lr-mckenzie_recover-ada_lite.ads
t_mck :: test_mckenzie_harness.run

GRAMMAR := object_declaration

one-clean :
	rm -rf $(GRAMMAR)*

#one :: one-clean
one :: wisitoken-bnf-generate.exe
one :: VERBOSITY := debug=1 parse=2
one :: $(GRAMMAR)_re2c.c
#one :: $(GRAMMAR)_lalr_run.exe
one :: $(GRAMMAR)_packrat_proc_run.exe
#one :: $(GRAMMAR)_packrat_gen_run.exe
	./$^  --verbosity "$(VERBOSITY)" ../test/bnf/$(GRAMMAR).input > $(GRAMMAR).parse

two-clean :
	rm -rf $(GRAMMAR)*

two :: VERBOSITY := debug=1
two :: two-clean
two :: wisitoken-bnf-generate.exe
	./wisitoken-bnf-generate.exe --output_bnf --verbosity "$(VERBOSITY)" $(GENERATE_ARGS) ../test/bnf/$(GRAMMAR).wy

three : wisitoken-followed_by.exe
	./wisitoken-followed_by.exe ../test/bnf/optimized_conflict_03.wy subtype_indication PLUS 2

four : wisitoken_grammar-clean
	cd ../; c:/Projects/org.wisitoken.stephe-2/build/wisitoken-bnf-generate.exe wisitoken_grammar.wy
	re2c --no-generation-date --debug-output --input custom -W -Werror --utf-8 -o ../wisitoken_grammar_re2c.c ../wisitoken_grammar.re2c
	dos2unix -q ../wisitoken_grammar*

# rule copied from gprbuild_rules.make; that rule gets overridden by
# %.exe rules in rules.make
exec_pro/wisitoken-bnf-generate.exe : force
	gprbuild -XStandard_Common_Profile=On -p -P wisitoken.gpr wisitoken-bnf-generate

gmon.out : exec_pro/wisitoken-bnf-generate.exe
	exec_pro/wisitoken-bnf-generate.exe --generate LR1 Ada re2c ../wisi/test/ada_lite.wy

generate_ada_lite_lr1.gprof : gmon.out
	gprof exec_pro/wisitoken-bnf-generate.exe > generate_ada_lite_lr1.gprof

doc :: error_correction_algorithm.dvi
doc :: error_correction_algorithm.bbl
doc :: error_correction_algorithm.ps
doc :: error_correction_algorithm.pdf
doc :: ../Docs/wisitoken-user_guide.info
doc :: ../Docs/wisitoken-user_guide.html
doc :: create-dir

create-dir : force
	cd ../Docs/; for file in *.info; do install-info $$file dir; done

trace :
	addr2line -e test_all_harness.exe 0x6eb427 0x7728ff3f 0x7728ff11 0x7729068d

include ../../org.stephe_leake.makerules/texinfo_rules.make
include ../../org.stephe_leake.makerules/latex_rules.make
include rules.make

# Local Variables:
# eval: (unless dvc-doing-ediff-p (load-file "prj.el"))
# End:
# end of file
