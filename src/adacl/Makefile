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

.PHONY: all run post-fetch pretty clean update

Project_Name	:= adacl
Tool_Name	:= libAdaCL.a

all:					    \
    lib/$(Tool_Name)			    \
    run

run:
	$(MAKE) -C "test" run

test:
	$(MAKE) -C "test" run

pretty:
	gnat pretty -P $(Project_Name).gpr

clean:
	-rm -r obj
	-rm -r lib
	-rm -r alire
	-rm -r config
	-$(MAKE) -C "test" clean

update:
	$(MAKE) clean
	alr index --update-all
	alr update
	$(MAKE) all

lib/$(Tool_Name):			    \
    $(Project_Name).gpr			    \
    src/*.adb				    \
    src/*.ads				    \
    alire.toml
	alr build

############################################################# {{{1 ##########
# vim: set nowrap tabstop=8 shiftwidth=4 softtabstop=4 noexpandtab :
# vim: set textwidth=0 filetype=make foldmethod=marker :
# vim: set spell spelllang=en_gb :
