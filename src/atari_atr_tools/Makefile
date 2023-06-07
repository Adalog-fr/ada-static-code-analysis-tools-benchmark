############################################################# {{{1 ##########
#  Copyright © 2022 … 2023 Martin Krischik «krischik@users.sourceforge.net»
#############################################################################
#  This program is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 3 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program.  If not, see «http://www.gnu.org/licenses/».
############################################################# }}}1 ##########

.PHONY: all test clean update

Project_Name	:= atari_atr_tools
Tool_Name	:= atr_tools-main

all:					    \
    bin/$(Tool_Name)

test:
	$(MAKE) -C "test" run

pretty:
	gnat pretty -P $(Project_Name).gpr

clean:
	-rm -r obj
	-rm -r bin
	-rm -r alire
	-rm -r config
	-$(MAKE) -C "test" clean
 
update:
	$(MAKE) clean
	alr index --update-all
	alr update
	$(MAKE) all

bin/$(Tool_Name):			    \
    $(Project_Name).gpr			    \
    src/*.adb				    \
    src/*.ads
	alr build

share/atr_tools/test-SD-SS-40.atr:	    \
    bin/$(Tool_Name)			    \
    Makefile
	bin/$(Tool_Name) --verbose --format-floppy share/atr_tools/test-SD-SS-40.atr

share/atr_tools/test-ED-SS-40.atr:	    \
    bin/$(Tool_Name)			    \
    Makefile
	bin/$(Tool_Name) --verbose --format-floppy --density=ED share/atr_tools/test-ED-SS-40.atr


share/atr_tools/test-DD-SS-40.atr:	    \
    bin/$(Tool_Name)			    \
    Makefile
	bin/$(Tool_Name) --verbose --format-floppy --density=DD share/atr_tools/test-DD-SS-40.atr

share/atr_tools/test-SD-DS-40.atr:	    \
    bin/$(Tool_Name)			    \
    Makefile
	bin/$(Tool_Name) --verbose --format-floppy --side=DS share/atr_tools/test-SD-DS-40.atr

share/atr_tools/test-DD-DS-40.atr:	    \
    bin/$(Tool_Name)			    \
    Makefile
	bin/$(Tool_Name) --verbose --format-floppy --density=DD --side=DS share/atr_tools/test-DD-DS-40.atr

share/atr_tools/test-DD-SS-80.atr:	    \
    bin/$(Tool_Name)			    \
    Makefile
	bin/$(Tool_Name) --verbose --format-floppy --density=DD --side=SS --track=80 share/atr_tools/test-DD-SS-80.atr

share/atr_tools/test-SD-SS-80.atr:	    \
    bin/$(Tool_Name)			    \
    Makefile
	bin/$(Tool_Name) --verbose --format-floppy --track=80 share/atr_tools/test-SD-SS-80.atr

share/atr_tools/test-DD-DS-80.atr:	    \
    bin/$(Tool_Name)			    \
    Makefile
	bin/$(Tool_Name) -v -f -d DD -s DS -t 80 share/atr_tools/test-DD-DS-80.atr

share/atr_tools/test-SD-DS-80.atr:	    \
    bin/$(Tool_Name)			    \
    Makefile
	bin/$(Tool_Name) -v -f -d SD -s DS -t 80 share/atr_tools/test-SD-DS-80.atr

test_header:				    \
    share/atr_tools/test-DD-DS-40.atr	    \
    share/atr_tools/test-DD-DS-80.atr	    \
    share/atr_tools/test-DD-SS-40.atr	    \
    share/atr_tools/test-DD-SS-80.atr	    \
    share/atr_tools/test-ED-SS-40.atr	    \
    share/atr_tools/test-SD-DS-40.atr	    \
    share/atr_tools/test-SD-SS-40.atr	    \
    share/atr_tools/test-SD-SS-80.atr	    \
    share/atr_tools/test-SD-DS-80.atr	    \
    bin/$(Tool_Name)
	bin/$(Tool_Name) --print-header share/atr_tools/test-DD-DS-40.atr
	bin/$(Tool_Name) --print-header share/atr_tools/test-DD-DS-80.atr
	bin/$(Tool_Name) --print-header share/atr_tools/test-DD-SS-40.atr
	bin/$(Tool_Name) --print-header share/atr_tools/test-DD-SS-80.atr
	bin/$(Tool_Name) --print-header share/atr_tools/test-ED-SS-40.atr
	bin/$(Tool_Name) --print-header share/atr_tools/test-SD-DS-40.atr
	bin/$(Tool_Name) --print-header share/atr_tools/test-SD-SS-40.atr
	bin/$(Tool_Name) --print-header share/atr_tools/test-SD-SS-80.atr
	bin/$(Tool_Name) --print-header share/atr_tools/test-SD-DS-80.atr

test_help:				    \
    bin/$(Tool_Name)
	bin/$(Tool_Name) --help

test_trace:				    \
    bin/$(Tool_Name)
	bin/$(Tool_Name) --TRACE=ON --TRACETO=STDOUT 

############################################################# {{{1 ##########
# vim: set nowrap tabstop=8 shiftwidth=4 softtabstop=4 noexpandtab :
# vim: set textwidth=0 filetype=make foldmethod=marker :
# vim: set spell spelllang=en_gb :
