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

with Ada.Directories;
with AdaCL.Trace;
with Create_NOP;

package body Create_NOP_Test.Call is

   overriding procedure Register_Tests (T : in out Test_Case) is
      Trace : AdaCL.Trace.Object := AdaCL.Trace.Function_Trace;
      pragma Unreferenced (Trace);

      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Write_01'Access, "Create ROM file");
   end Register_Tests;

   procedure Test_Write_01 (T : in out AUnit.Test_Cases.Test_Case'Class) is
      Trace : AdaCL.Trace.Object := AdaCL.Trace.Function_Trace;
      pragma Unreferenced (Trace);
      pragma Unreferenced (T);

      File_Name : constant String := "create_nop.rom";
   begin
      Create_NOP;

      Assert.Is_True  (Ada.Directories.Exists (File_Name), "Ada.Directories.Exists (File_Name)");
      Assert_File_Size.Equal (
         Actual   => Ada.Directories.Size (File_Name),
         Expected => 32768,
         Name     => "Ada.Directories.Size");
   end Test_Write_01;
end Create_NOP_Test.Call;

---------------------------------------------------------------- {{{ ----------
--  vim: set textwidth=0 nowrap tabstop=8 shiftwidth=3 softtabstop=3 expandtab :
--  vim: set filetype=ada fileencoding=utf-8 fileformat=unix foldmethod=marker :
--  vim: set spell spelllang=en_gb
