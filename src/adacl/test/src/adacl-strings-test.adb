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

with Ada.Strings.Maps;
with Ada.Strings.Unbounded;
with AdaCL.Trace;
with AdaCL_Test;
with GNAT.Source_Info;

package body AdaCL.Strings.Test is

   use AdaCL_Test;

   package Maps      renames Ada.Strings.Maps;
   package Unbounded renames Ada.Strings.Unbounded;

   overriding procedure Register_Tests (T : in out Test_Case) is
      Trace : AdaCL.Trace.Object := AdaCL.Trace.Function_Trace;
      pragma Unreferenced (Trace);

      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Append_All_01'Access, "Test_Append_All_01 : simple append");
      Register_Routine (T, Test_Append_All_02'Access, "Test_Append_All_02 : skip append");
      Register_Routine (T, Test_Append_All_03'Access, "Test_Append_All_03 : append once");
   end Register_Tests;

   procedure Test_Append_All_01 (T : in out AUnit.Test_Cases.Test_Case'Class) is
      Trace : AdaCL.Trace.Object := AdaCL.Trace.Function_Trace;
      pragma Unreferenced (Trace);

      Test_Data : Unbounded.Unbounded_String := Unbounded.To_Unbounded_String ("AAA BBB CCC");
   begin
      AdaCL.Strings.Append_All (
         Source   => Test_Data,
         Search   => "BBB",
         New_Item => " DDD",
         Mapping  => Maps.Identity);

      Assert.Equal (
         Actual   => Test_Data,
         Expected => "AAA BBB DDD CCC",
         Name     => "“DDD” should be inserted once.");
   end Test_Append_All_01;

   procedure Test_Append_All_02 (T : in out AUnit.Test_Cases.Test_Case'Class) is
      Trace : AdaCL.Trace.Object := AdaCL.Trace.Function_Trace;
      pragma Unreferenced (Trace);

      Test_Data : Unbounded.Unbounded_String := Unbounded.To_Unbounded_String ("AAA BBB DDD CCC");
   begin
      AdaCL.Strings.Append_All (
         Source   => Test_Data,
         Search   => "BBB",
         New_Item => " DDD",
         Mapping  => Maps.Identity);

      Assert.Equal (
         Actual   => Test_Data,
         Expected => "AAA BBB DDD CCC",
         Name     => "“DDD” should be not be inserted.");

   end Test_Append_All_02;

   procedure Test_Append_All_03 (T : in out AUnit.Test_Cases.Test_Case'Class) is
      Trace : AdaCL.Trace.Object := AdaCL.Trace.Function_Trace;
      pragma Unreferenced (Trace);

      Test_Data : Unbounded.Unbounded_String := Unbounded.To_Unbounded_String ("AAA BBB DDD BBB CCC");
   begin
      AdaCL.Strings.Append_All (
         Source   => Test_Data,
         Search   => "BBB",
         New_Item => " DDD",
         Mapping  => Maps.Identity);

      Assert.Equal (
         Actual   => Test_Data,
         Expected => "AAA BBB DDD BBB DDD CCC",
         Name     => "“DDD” should be inserted only once.");
   end Test_Append_All_03;
end AdaCL.Strings.Test;

---------------------------------------------------------------- {{{ ----------
--  vim: set textwidth=0 nowrap tabstop=8 shiftwidth=3 softtabstop=3 expandtab :
--  vim: set filetype=ada fileencoding=utf-8 fileformat=unix foldmethod=expr :
--  vim: set spell spelllang=en_gb
