--  Abstract :
--
--  Test one .wy file; compare to known good output files.
--
--  Copyright (C) 2013, 2017 - 2021 Stephen Leake.  All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or (at
--  your option) any later version. This program is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
--  MA 02111-1307, USA.

pragma License (GPL);

with AUnit.Test_Cases;
package BNF_WY_Test is

   type Test_Case
     (Root_Name  : not null access String;
      Input_Name : access String)
     is new AUnit.Test_Cases.Test_Case with null record;
   --  Always parse <Root_Name>.input. If Input_Name is non-null, also
   --  parse <Input_Name>.input.
   --
   --  Handles env var GENERATE as wistoken_test.gpr does.

   type Test_Case_Access is access all Test_Case;

   overriding procedure Register_Tests (T : in out Test_Case);

   overriding function Name (T : Test_Case) return AUnit.Message_String;

end BNF_WY_Test;
