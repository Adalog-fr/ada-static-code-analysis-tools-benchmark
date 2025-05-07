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

with Ada.Directories;
with AdaCL_Test;

package body AdaCL.Trace.Test is

   use type Ada.Directories.File_Size;
   use AdaCL_Test;
   use GNAT.Source_Info;

   overriding procedure Register_Tests (T : in out Test_Case) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Enable_Trace_01'Access,     "Test_Enable_Trace_01 : enable trace");
      Register_Routine (T, Test_Write_To_File_01'Access,    "Test_Write_To_File_01 : trace to file");
      Register_Routine (T, Test_Write_01'Access,            "Test_Write_String_01 : simple string output");
      Register_Routine (T, Test_Write_02'Access,            "Test_Write_String_02 : unbounded string output");
      Register_Routine (T, Test_Function_Trace_01'Access,   "Test_Function_Trace_01 : function trace one string");
      Register_Routine (T, Test_Function_Trace_02'Access,   "Test_Function_Trace_02 : function trace two strings");
      Register_Routine (T, Test_Function_Trace_03'Access,   "Test_Function_Trace_03 : function trace no parameter");
      Register_Routine (T, Test_Function_Trace_04'Access,   "Test_Function_Trace_04 : function trace two level");
   end Register_Tests;

   overriding procedure Tear_Down (T : in out Test_Case) is
   begin
      AdaCL.Trace.Write_To_File (AdaCL_Test.Trace_File);
      AdaCL.Trace.Enable_Trace;
   end Tear_Down;

   procedure Test_Enable_Trace_01 (T : in out AUnit.Test_Cases.Test_Case'Class) is
   begin
      Trace.Enable_Trace;

      Assert.Is_True (Trace.Is_Trace_Enabled, "Is_Trace_Enabled");
   end Test_Enable_Trace_01;

   procedure Test_Write_To_File_01 (T : in out AUnit.Test_Cases.Test_Case'Class) is
   begin
      Trace.Write_To_File ("Test_Write_To_File_01.out");

      Assert.Is_True (Trace.Trace_Destination = File, "Trace_Destination = File");
   end Test_Write_To_File_01;

   procedure Test_Write_01 (T : in out AUnit.Test_Cases.Test_Case'Class) is
      File_Name : constant String := Enclosing_Entity & ".out";
      Test      : constant String := "Test_Write_01";
   begin
      Trace.Write_To_File (File_Name);
      Trace.Enable_Trace;

      Trace.Write (Test);
      Trace.Write (Test'Image);

      Assert.Is_True  (Ada.Directories.Exists (File_Name), "Ada.Directories.Exists (File_Name)");
      Assert_File_Size.In_Range (
         Actual => Ada.Directories.Size (File_Name),
         First  => 58,
         Last   => 2 * 58,
         Name   => "Ada.Directories.Size");
   end Test_Write_01;

   procedure Test_Write_02 (T : in out AUnit.Test_Cases.Test_Case'Class) is
      use Ada.Strings.Unbounded;

      File_Name : constant String           := Enclosing_Entity & ".out";
      Test      : constant Unbounded_String := To_Unbounded_String ("Test_Write_02");
   begin
      Trace.Write_To_File (File_Name);
      Trace.Enable_Trace;

      Trace.Write (Test);
      Trace.Write (To_String (Test));
      Trace.Write (Test'Image);

      Assert.Is_True  (Ada.Directories.Exists (File_Name), "Ada.Directories.Exists (File_Name)");
      Assert_File_Size.In_Range (
         Actual => Ada.Directories.Size (File_Name),
         First  => 86,
         Last   => 2 * 86,
         Name   => "Ada.Directories.Size");
   end Test_Write_02;

   procedure Test_Function_Trace_01 (T : in out AUnit.Test_Cases.Test_Case'Class) is
      File_Name : constant String := Enclosing_Entity & ".out";
   begin
      Trace.Write_To_File (File_Name);
      Trace.Enable_Trace;

      declare
         Test : Trace.Object := Trace.Function_Trace (Name => Enclosing_Entity & ':' & Source_Location);
         pragma Unreferenced (Test);
      begin
         Trace.Write ("Test_Function_Trace_01");
      end;

      Assert.Is_True   (Ada.Directories.Exists (File_Name), "Ada.Directories.Exists (File_Name)");
      Assert_File_Size.In_Range  (
         Actual => Ada.Directories.Size (File_Name),
         First  => 195,
         Last   => 2 * 195,
         Name   => "Ada.Directories.Size (File_Name)");
   end Test_Function_Trace_01;

   procedure Test_Function_Trace_02 (T : in out AUnit.Test_Cases.Test_Case'Class) is
      File_Name : constant String := Enclosing_Entity & ".out";
   begin
      Trace.Write_To_File (File_Name);
      Trace.Enable_Trace;

      declare
         Test : Trace.Object := Trace.Function_Trace (Enclosing_Entity, Source_Location);
         pragma Unreferenced (Test);
      begin
         Trace.Write ("Test_Function_Trace_02");
      end;

      Assert.Is_True  (Ada.Directories.Exists (File_Name), "Ada.Directories.Exists (File_Name)");
      Assert_File_Size.In_Range (
      Actual => Ada.Directories.Size (File_Name),
         First  => 197,
         Last   => 2 * 197,
         Name   => "Ada.Directories.Size (File_Name)");
   end Test_Function_Trace_02;

   procedure Test_Function_Trace_03 (T : in out AUnit.Test_Cases.Test_Case'Class) is
      File_Name : constant String := Enclosing_Entity & ".out";
   begin
      Trace.Write_To_File (File_Name);
      Trace.Enable_Trace;

      declare
         Test : Trace.Object := Trace.Function_Trace;
         pragma Unreferenced (Test);
      begin
         Trace.Write ("Test_Function_Trace_02");
      end;

      Assert.Is_True  (Ada.Directories.Exists (File_Name), "Ada.Directories.Exists (File_Name)");
      Assert_File_Size.In_Range (
         Actual => Ada.Directories.Size (File_Name),
         First  => 197,
         Last   => 2 * 197,
         Name   => "Ada.Directories.Size (File_Name)");
   end Test_Function_Trace_03;

   procedure Test_Function_Trace_04 (T : in out AUnit.Test_Cases.Test_Case'Class) is
      File_Name : constant String := Enclosing_Entity & ".out";
   begin
      Trace.Write_To_File (File_Name);
      Trace.Enable_Trace;

      declare
         Test_01 : Trace.Object := Trace.Function_Trace (Enclosing_Entity, Source_Location);
         pragma Unreferenced (Test_01);
      begin
         Trace.Write ("Test_Function_Trace_03 begin outer");

         declare
            Test_02 : Trace.Object := Trace.Function_Trace (Enclosing_Entity, Source_Location);
            pragma Unreferenced (Test_02);
         begin
            Trace.Write ("Test_Function_Trace_03 inner");
         end;

         Trace.Write ("Test_Function_Trace_03 end outer");
      end;

      Assert.Is_True   (Ada.Directories.Exists (File_Name), "Ada.Directories.Exists (File_Name)");
      Assert_File_Size.In_Range  (
         Actual => Ada.Directories.Size (File_Name),
         First  => 467,
         Last   => 2 * 467,
         Name   => "Ada.Directories.Size (File_Name)");
   end Test_Function_Trace_04;

end AdaCL.Trace.Test;

---------------------------------------------------------------- {{{ ----------
--  vim: set textwidth=0 nowrap tabstop=8 shiftwidth=3 softtabstop=3 expandtab :
--  vim: set filetype=ada fileencoding=utf-8 fileformat=unix foldmethod=expr :
--  vim: set spell spelllang=en_gb
