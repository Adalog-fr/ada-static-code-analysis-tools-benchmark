--------------------------------------------------------------- {{{1 ----------
--  Copyright © 2023 … 2023 Martin Krischik «krischik@users.sourceforge.net»
-------------------------------------------------------------------------------
--  This library is free software; you can redistribute it and/or modify it
--  under the terms of the GNU Library General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or (at your
--  option) any later version.
--
--  This library is distributed in the hope that it will be useful, but
--  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
--  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public
--  License for more details.
--
--  You should have received a copy of the GNU Library General Public License
--  along with this library; if not, write to the Free Software Foundation,
--  Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
--------------------------------------------------------------- }}}1 ----------
pragma License (Modified_Gpl);
pragma Ada_2022;

with AUnit;
with AUnit.Test_Cases;

package Atr_Tools_Test.Create_Floppy is

   type Test_Case is new AUnit.Test_Cases.Test_Case with null record;

   overriding procedure Register_Tests (T : in out Test_Case);

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is (AUnit.Format ("Create_Floppy"));

   procedure Test_Create_Floppy_01 (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Create_Floppy_02 (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Create_Floppy_03 (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Create_Floppy_04 (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Create_Floppy_05 (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Create_Floppy_06 (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Create_Floppy_07 (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Create_Floppy_08 (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Create_Floppy_09 (T : in out AUnit.Test_Cases.Test_Case'Class);
end Atr_Tools_Test.Create_Floppy;

---------------------------------------------------------------- {{{ ----------
--  vim: set textwidth=0 nowrap tabstop=8 shiftwidth=3 softtabstop=3 expandtab :
--  vim: set filetype=ada fileencoding=utf-8 fileformat=unix foldmethod=marker :
--  vim: set spell spelllang=en_gb
