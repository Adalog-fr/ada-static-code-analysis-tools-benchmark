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

with AdaCL.Trace;
with AdaCL_Test;
with GNAT.Source_Info;

package body AdaCL.Reference.Element.Test is

   use AdaCL_Test;

   overriding procedure Register_Tests (T : in out Test_Case) is
      Trace : AdaCL.Trace.Object := AdaCL.Trace.Function_Trace;
      pragma Unreferenced (Trace);

      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Add_Reference_01'Access,    "Test_Add_Reference_01 : simple add");
      Register_Routine (T, Test_Add_Reference_02'Access,    "Test_Add_Reference_02 : two add");
      Register_Routine (T, Test_Remove_Reference_01'Access, "Test_Remove_Reference_01 : add remove");
      Register_Routine (T, Test_Remove_Reference_02'Access, "Test_Remove_Reference_02 : add remove");
   end Register_Tests;

   procedure Test_Add_Reference_01 (T : in out AUnit.Test_Cases.Test_Case'Class) is
      Trace : AdaCL.Trace.Object := AdaCL.Trace.Function_Trace;
      pragma Unreferenced (Trace);

      Test_Data : AdaCL.Reference.Element.Object;
   begin
      Test_Data.Add_Reference;

      Assert.Equal (
         Actual   => Test_Data.Use_Count,
         Expected => 1,
         Name     => "Reference counter");
   end Test_Add_Reference_01;

   procedure Test_Add_Reference_02 (T : in out AUnit.Test_Cases.Test_Case'Class) is
      Trace : AdaCL.Trace.Object := AdaCL.Trace.Function_Trace;
      pragma Unreferenced (Trace);

      Test_Data : AdaCL.Reference.Element.Object;
   begin
      Test_Data.Add_Reference;
      Test_Data.Add_Reference;

      Assert.Equal (
         Actual   => Test_Data.Use_Count,
         Expected => 2,
         Name     => "Reference counter");
   end Test_Add_Reference_02;

   procedure Test_Remove_Reference_01 (T : in out AUnit.Test_Cases.Test_Case'Class) is
      Trace : AdaCL.Trace.Object := AdaCL.Trace.Function_Trace;
      pragma Unreferenced (Trace);

      Test_Data : AdaCL.Reference.Element.Object;
   begin
      Test_Data.Add_Reference;
      Test_Data.Remove_Reference;

      Assert.Equal (
         Actual   => Test_Data.Use_Count,
         Expected => 0,
         Name     => "Reference counter");
   end Test_Remove_Reference_01;

   procedure Test_Remove_Reference_02 (T : in out AUnit.Test_Cases.Test_Case'Class) is
      Trace : AdaCL.Trace.Object := AdaCL.Trace.Function_Trace;
      pragma Unreferenced (Trace);

      Test_Data : AdaCL.Reference.Element.Object;
   begin
      Test_Data.Add_Reference;
      Test_Data.Add_Reference;
      Test_Data.Remove_Reference;

      Assert.Equal (
         Actual   => Test_Data.Use_Count,
         Expected => 1,
         Name     => "Reference counter");

      Test_Data.Remove_Reference;

      Assert.Equal (
         Actual   => Test_Data.Use_Count,
         Expected => 0,
         Name     => "Reference counter");
   end Test_Remove_Reference_02;
end AdaCL.Reference.Element.Test;

---------------------------------------------------------------- {{{ ----------
--  vim: set textwidth=0 nowrap tabstop=8 shiftwidth=3 softtabstop=3 expandtab :
--  vim: set filetype=ada fileencoding=utf-8 fileformat=unix foldmethod=expr :
--  vim: set spell spelllang=en_gb
