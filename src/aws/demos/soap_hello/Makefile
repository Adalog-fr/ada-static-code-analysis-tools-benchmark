############################################################################
#                              Ada Web Server                              #
#                                                                          #
#                       Copyright (C) 2021, AdaCore                        #
#                                                                          #
#  This is free software;  you can redistribute it  and/or modify it       #
#  under terms of the  GNU General Public License as published  by the     #
#  Free Software  Foundation;  either version 3,  or (at your option) any  #
#  later version.  This software is distributed in the hope  that it will  #
#  be useful, but WITHOUT ANY WARRANTY;  without even the implied warranty #
#  of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU     #
#  General Public License for  more details.                               #
#                                                                          #
#  You should have  received  a copy of the GNU General  Public  License   #
#  distributed  with  this  software;   see  file COPYING3.  If not, go    #
#  to http://www.gnu.org/licenses for a complete copy of the license.      #
############################################################################

-include ../../makefile.setup

URL=http://localhost:1234

ifneq (${XMLADA},false)
all:
	ada2wsdl -f -a $(URL)  -o hello.wsdl -P hello.gpr hello.ads
	wsdl2aws -f -spec hello -cb -main hello_server hello.wsdl
	gprbuild hello.gpr
else
all:
endif

clean:
	gprclean hello.gpr
	-rm -f hello_service* hello_server.adb hello.wsdl
