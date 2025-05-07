--------------------------------------------------------------- {{{1 ----------
--  Copyright © 2023 … 2023 Martin Krischik «krischik@users.sourceforge.net»
------------------------------------------------------------------------------
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

with AdaCL.Reference.Element;
with AdaCL.Reference.Holder;
with AUnit.Test_Cases;
with AUnit.Test_Fixtures;
with AUnit;

package AdaCL_Test.Reference_Holder is
   package Holder is new AdaCL.Reference.Holder (
      Element_Type   => AdaCL.Reference.Element.Object,
      Element_Class  => AdaCL.Reference.Element.Object_Class);

   type Test_Case is new AUnit.Test_Cases.Test_Case with null record;

   overriding procedure Register_Tests (T : in out Test_Case);

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is (AUnit.Format ("AdaCL.Reference.Holder"));

   procedure Test_Create_01 (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Create_02 (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Set_01 (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Set_02 (T : in out AUnit.Test_Cases.Test_Case'Class);
end AdaCL_Test.Reference_Holder;

---------------------------------------------------------------- {{{ ----------
--  vim: set textwidth=0 nowrap tabstop=8 shiftwidth=3 softtabstop=3 expandtab :
--  vim: set filetype=ada fileencoding=utf-8 fileformat=unix foldmethod=expr :
--  vim: set spell spelllang=en_gb
