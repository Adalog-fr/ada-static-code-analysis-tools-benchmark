############################################################# {{{1 ##########
#  Copyright © 2012 … 2020 Martin Krischik «krischik@users.sourceforge.net»
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

.PHONY: all test rom burn clean update pretty

Project_Name	:= mos6502_create_reset
Tool_Name	:= create_reset
Rom_Name	:= Reset.rom

all:					    \
    bin/$(Tool_Name)

test:
	$(MAKE) -C "test" run

rom:					    \
    bin/$(Rom_Name)

burn:					    \
    bin/$(Rom_Name)
	minipro -p "AT28C256" -w $(<)

pretty:
	gnat pretty -P $(Project_Name).gpr

clean:
	-rm -r *.out
	-rm -r *.rom
	-rm -r alire
	-rm -r bin
	-rm -r config
	-rm -r obj
	-$(MAKE) -C "test" clean

update:
	$(MAKE) clean
	alr update
	$(MAKE) all

bin/$(Rom_Name):			    \
    bin/$(Tool_Name)
	bin/$(Tool_Name) $(@)

bin/$(Tool_Name):			    \
    $(Project_Name).gpr			    \
    src/*.adb				    
	alr build

############################################################# {{{1 ##########
# vim: set nowrap tabstop=8 shiftwidth=4 softtabstop=4 noexpandtab :
# vim: set textwidth=0 filetype=make foldmethod=marker :
# vim: set spell spelllang=en_gb :
