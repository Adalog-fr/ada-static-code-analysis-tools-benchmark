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

with AdaCL.Limited_Base;
with AdaCL.Trace;
with GNAT.Source_Info;

package body AdaCL_Test.Reference_Holder is

   use AdaCL_Test;

   overriding procedure Register_Tests (T : in out Test_Case) is
      Trace : AdaCL.Trace.Object := AdaCL.Trace.Function_Trace;
      pragma Unreferenced (Trace);

      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Create_01'Access, "Test_Create_01 : one level");
      Register_Routine (T, Test_Create_02'Access, "Test_Create_02 : add remove");
      Register_Routine (T, Test_Set_01'Access,    "Test_Set_01 : one level");
      Register_Routine (T, Test_Set_02'Access,    "Test_Set_02 : add remove");
   end Register_Tests;

   procedure Test_Create_01 (T : in out AUnit.Test_Cases.Test_Case'Class) is
      Trace : AdaCL.Trace.Object := AdaCL.Trace.Function_Trace;
      pragma Unreferenced (Trace);

      use AdaCL.Reference.Element;

      Test_Instance : constant Object_Class  := AdaCL.Reference.Element.Create;
   begin
      Assert.Equal (
         Actual   => Test_Instance.Use_Count,
         Expected => 0,
         Name     => "New instance");

      declare
         Test_Holder : Holder.Object := Holder.Create (Test_Instance);
      begin
         Assert.Equal (
            Actual   => Test_Instance.Use_Count,
            Expected => 1,
            Name     => "referenced instance");
      end;
   end Test_Create_01;

   procedure Test_Create_02 (T : in out AUnit.Test_Cases.Test_Case'Class) is
      Trace : AdaCL.Trace.Object := AdaCL.Trace.Function_Trace;
      pragma Unreferenced (Trace);

      use AdaCL.Reference.Element;

      Test_Holder_1 : constant Holder.Object := Holder.Create (AdaCL.Reference.Element.Create);
   begin
      Assert.Equal (
         Actual   => Test_Holder_1.Get.Use_Count,
         Expected => 1,
         Name     => "New instance");

      declare
         Test_Holder_2 : constant Holder.Object := Test_Holder_1;
      begin
         Assert.Equal (
            Actual   => Test_Holder_1.Get.Use_Count,
            Expected => 2,
            Name     => "referenced instance 1");
         Assert.Equal (
            Actual   => Test_Holder_2.Get.Use_Count,
            Expected => 2,
            Name     => "referenced instance 2");
      end;

      Assert.Equal (
         Actual   => Test_Holder_1.Get.Use_Count,
         Expected => 1,
         Name     => "New instance");
   end Test_Create_02;

   procedure Test_Set_01 (T : in out AUnit.Test_Cases.Test_Case'Class) is
      Trace : AdaCL.Trace.Object := AdaCL.Trace.Function_Trace;
      pragma Unreferenced (Trace);

      use AdaCL.Reference.Element;
      use AUnit.Assertions;

      Test_Instance : constant Object_Class  := AdaCL.Reference.Element.Create;
      Test_Holder   : Holder.Object;
   begin
      Assert.Equal (
         Actual   => Test_Instance.Use_Count,
         Expected => 0,
         Name     => "New instance");

      Test_Holder.Set (Test_Instance);

      Assert.Equal (
         Actual   => Test_Instance.Use_Count,
         Expected => 1,
         Name     => "referenced instance");

      Test_Holder.Set;

      Assert.Is_Null (
         Actual => Test_Holder.Get_Base,
         Name   => "Reference deleted");
   end Test_Set_01;

   procedure Test_Set_02 (T : in out AUnit.Test_Cases.Test_Case'Class) is
      Trace : AdaCL.Trace.Object := AdaCL.Trace.Function_Trace;
      pragma Unreferenced (Trace);

      use AdaCL.Reference.Element;
      use AUnit.Assertions;

      Test_Holder_1 : Holder.Object := Holder.Create (AdaCL.Reference.Element.Create);
      Test_Holder_2 : Holder.Object;
   begin
      Assert.Equal (
         Actual   => Test_Holder_1.Get.Use_Count,
         Expected => 1,
         Name     => "New instance");

      Test_Holder_2.Set (Test_Holder_1);

      Assert.Equal (
         Actual   => Test_Holder_2.Get.Use_Count,
         Expected => 2,
         Name     => "referenced instance 1");
      Assert.Equal (
         Actual   => Test_Holder_2.Get.Use_Count,
         Expected => 2,
         Name     => "referenced instance 2");

      Test_Holder_2.Set;

      Assert.Equal (
         Actual   => Test_Holder_1.Get.Use_Count,
         Expected => 1,
         Name     => "New instance");

      Test_Holder_1.Set;

      Assert.Is_Null (
         Actual => Test_Holder_1.Get_Base,
         Name   => "Reference deleted");
   end Test_Set_02;
end AdaCL_Test.Reference_Holder;

---------------------------------------------------------------- {{{ ----------
--  vim: set textwidth=0 nowrap tabstop=8 shiftwidth=3 softtabstop=3 expandtab :
--  vim: set filetype=ada fileencoding=utf-8 fileformat=unix foldmethod=expr :
--  vim: set spell spelllang=en_gb
