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
with Atr_Tools.Create_Floppy;
with Atr_Tools.File_Body;
with Atr_Tools.File_Header;

package body Atr_Tools_Test.Create_Floppy is

   use type Ada.Directories.File_Size;
   use Atr_Tools;

   ---
   --  Convert floppy size in paragraphs into exptexted filesize.
   --
   function File_Size (Paragraphs : File_Header.Double_Word) return Ada.Directories.File_Size
   is (Ada.Directories.File_Size (Paragraphs) * File_Header.Paragraph_Size + 16);

   overriding procedure Register_Tests (T : in out Test_Case) is
      Trace : AdaCL.Trace.Object := AdaCL.Trace.Function_Trace;
      pragma Unreferenced (Trace);

      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Create_Floppy_01'Access, "SD_SS_40");
      Register_Routine (T, Test_Create_Floppy_02'Access, "ED_SS_40");
      Register_Routine (T, Test_Create_Floppy_03'Access, "DD_SS_40");
      Register_Routine (T, Test_Create_Floppy_04'Access, "SD_DS_40");
      Register_Routine (T, Test_Create_Floppy_05'Access, "SD_SS_80");
      Register_Routine (T, Test_Create_Floppy_06'Access, "DD_DS_40");
      Register_Routine (T, Test_Create_Floppy_07'Access, "DD_SS_80");
      Register_Routine (T, Test_Create_Floppy_08'Access, "SD_DS_80");
      Register_Routine (T, Test_Create_Floppy_09'Access, "DD_DS_80");
   end Register_Tests;

   procedure Test_Create_Floppy_01 (T : in out AUnit.Test_Cases.Test_Case'Class) is
      Trace : AdaCL.Trace.Object := AdaCL.Trace.Function_Trace;
      pragma Unreferenced (Trace);
      pragma Unreferenced (T);

      File_Name : constant String := "SD_SS_40.atr";
   begin
      Atr_Tools.Create_Floppy (
         File_Name        => File_Name,
         Sector_Size      => File_Body.Single_Density,
         Sector_Per_Track => File_Body.Atari_Standard,
         Tracks_Per_Side  => File_Body.Standard_Tracks,
         Sides_Per_Disk   => 1);

      Assert.Is_True  (Ada.Directories.Exists (File_Name), "Ada.Directories.Exists (File_Name)");
      Assert_File_Size.Equal (
         Actual   => Ada.Directories.Size (File_Name),
         Expected => File_Size (File_Header.P_SD_SS_40),
         Name     => "Ada.Directories.Size");
   end Test_Create_Floppy_01;

   procedure Test_Create_Floppy_02 (T : in out AUnit.Test_Cases.Test_Case'Class) is
      Trace : AdaCL.Trace.Object := AdaCL.Trace.Function_Trace;
      pragma Unreferenced (Trace);
      pragma Unreferenced (T);

      File_Name : constant String := "ED_SS_40.atr";
   begin
      Atr_Tools.Create_Floppy (
         File_Name        => File_Name,
         Sector_Size      => File_Body.Single_Density,
         Sector_Per_Track => File_Body.Atari_Enhanced,
         Tracks_Per_Side  => File_Body.Standard_Tracks,
         Sides_Per_Disk   => 1);

      Assert.Is_True  (Ada.Directories.Exists (File_Name), "Ada.Directories.Exists (File_Name)");
      Assert_File_Size.Equal (
         Actual   => Ada.Directories.Size (File_Name),
         Expected => File_Size (File_Header.P_ED_SS_40),
         Name     => "Ada.Directories.Size");
   end Test_Create_Floppy_02;

   procedure Test_Create_Floppy_03 (T : in out AUnit.Test_Cases.Test_Case'Class) is
      Trace : AdaCL.Trace.Object := AdaCL.Trace.Function_Trace;
      pragma Unreferenced (Trace);
      pragma Unreferenced (T);

      File_Name : constant String := "DD_SS_40.atr";
   begin
      Atr_Tools.Create_Floppy (
         File_Name        => File_Name,
         Sector_Size      => File_Body.Double_Density,
         Sector_Per_Track => File_Body.Atari_Standard,
         Tracks_Per_Side  => File_Body.Standard_Tracks,
         Sides_Per_Disk   => 1);

      Assert.Is_True  (Ada.Directories.Exists (File_Name), "Ada.Directories.Exists (File_Name)");
      Assert_File_Size.Equal (
         Actual   => Ada.Directories.Size (File_Name),
         Expected => File_Size (File_Header.P_DD_SS_40),
         Name     => "Ada.Directories.Size");
   end Test_Create_Floppy_03;

   procedure Test_Create_Floppy_04 (T : in out AUnit.Test_Cases.Test_Case'Class) is
      Trace : AdaCL.Trace.Object := AdaCL.Trace.Function_Trace;
      pragma Unreferenced (Trace);
      pragma Unreferenced (T);

      File_Name : constant String := "SD_DS_40.atr";
   begin
      Atr_Tools.Create_Floppy (
         File_Name        => File_Name,
         Sector_Size      => File_Body.Single_Density,
         Sector_Per_Track => File_Body.Atari_Standard,
         Tracks_Per_Side  => File_Body.Standard_Tracks,
         Sides_Per_Disk   => 2);

      Assert.Is_True  (Ada.Directories.Exists (File_Name), "Ada.Directories.Exists (File_Name)");
      Assert_File_Size.Equal (
         Actual   => Ada.Directories.Size (File_Name),
         Expected => File_Size (File_Header.P_SD_DS_40),
         Name     => "Ada.Directories.Size");
   end Test_Create_Floppy_04;

   procedure Test_Create_Floppy_05 (T : in out AUnit.Test_Cases.Test_Case'Class) is
      Trace : AdaCL.Trace.Object := AdaCL.Trace.Function_Trace;
      pragma Unreferenced (Trace);
      pragma Unreferenced (T);

      File_Name : constant String := "SD_SS_80.atr";
   begin
      Atr_Tools.Create_Floppy (
         File_Name        => File_Name,
         Sector_Size      => File_Body.Single_Density,
         Sector_Per_Track => File_Body.Atari_Standard,
         Tracks_Per_Side  => File_Body.Double_Tracks,
         Sides_Per_Disk   => 1);

      Assert.Is_True  (Ada.Directories.Exists (File_Name), "Ada.Directories.Exists (File_Name)");
      Assert_File_Size.Equal (
         Actual   => Ada.Directories.Size (File_Name),
         Expected => File_Size (File_Header.P_SD_SS_80),
         Name     => "Ada.Directories.Size");
   end Test_Create_Floppy_05;

   procedure Test_Create_Floppy_06 (T : in out AUnit.Test_Cases.Test_Case'Class) is
      Trace : AdaCL.Trace.Object := AdaCL.Trace.Function_Trace;
      pragma Unreferenced (Trace);
      pragma Unreferenced (T);

      File_Name : constant String := "DD_DS_40.atr";
   begin
      Atr_Tools.Create_Floppy (
         File_Name        => File_Name,
         Sector_Size      => File_Body.Double_Density,
         Sector_Per_Track => File_Body.Atari_Standard,
         Tracks_Per_Side  => File_Body.Standard_Tracks,
         Sides_Per_Disk   => 2);

      Assert.Is_True  (Ada.Directories.Exists (File_Name), "Ada.Directories.Exists (File_Name)");
      Assert_File_Size.Equal (
         Actual   => Ada.Directories.Size (File_Name),
         Expected => File_Size (File_Header.P_DD_DS_40),
         Name     => "Ada.Directories.Size");
   end Test_Create_Floppy_06;

   procedure Test_Create_Floppy_07 (T : in out AUnit.Test_Cases.Test_Case'Class) is
      Trace : AdaCL.Trace.Object := AdaCL.Trace.Function_Trace;
      pragma Unreferenced (Trace);
      pragma Unreferenced (T);

      File_Name : constant String := "DD_SS_80.atr";
   begin
      Atr_Tools.Create_Floppy (
         File_Name        => File_Name,
         Sector_Size      => File_Body.Double_Density,
         Sector_Per_Track => File_Body.Atari_Standard,
         Tracks_Per_Side  => File_Body.Double_Tracks,
         Sides_Per_Disk   => 1);

      Assert.Is_True  (Ada.Directories.Exists (File_Name), "Ada.Directories.Exists (File_Name)");
      Assert_File_Size.Equal (
         Actual   => Ada.Directories.Size (File_Name),
         Expected => File_Size (File_Header.P_DD_SS_80),
         Name     => "Ada.Directories.Size");
   end Test_Create_Floppy_07;

   procedure Test_Create_Floppy_08 (T : in out AUnit.Test_Cases.Test_Case'Class) is
      Trace : AdaCL.Trace.Object := AdaCL.Trace.Function_Trace;
      pragma Unreferenced (Trace);
      pragma Unreferenced (T);

      File_Name : constant String := "SD_DS_80.atr";
   begin
      Atr_Tools.Create_Floppy (
         File_Name        => File_Name,
         Sector_Size      => File_Body.Single_Density,
         Sector_Per_Track => File_Body.Atari_Standard,
         Tracks_Per_Side  => File_Body.Double_Tracks,
         Sides_Per_Disk   => 2);

      Assert.Is_True  (Ada.Directories.Exists (File_Name), "Ada.Directories.Exists (File_Name)");
      Assert_File_Size.Equal (
         Actual   => Ada.Directories.Size (File_Name),
         Expected => File_Size (File_Header.P_SD_DS_80),
         Name     => "Ada.Directories.Size");
   end Test_Create_Floppy_08;

   procedure Test_Create_Floppy_09 (T : in out AUnit.Test_Cases.Test_Case'Class) is
      Trace : AdaCL.Trace.Object := AdaCL.Trace.Function_Trace;
      pragma Unreferenced (Trace);
      pragma Unreferenced (T);

      File_Name : constant String := "DD_DS_80.atr";
   begin
      Atr_Tools.Create_Floppy (
         File_Name        => File_Name,
         Sector_Size      => File_Body.Double_Density,
         Sector_Per_Track => File_Body.Atari_Standard,
         Tracks_Per_Side  => File_Body.Double_Tracks,
         Sides_Per_Disk   => 2);

      Assert.Is_True  (Ada.Directories.Exists (File_Name), "Ada.Directories.Exists (File_Name)");
      Assert_File_Size.Equal (
         Actual   => Ada.Directories.Size (File_Name),
         Expected => File_Size (File_Header.P_DD_DS_80),
         Name     => "Ada.Directories.Size");
   end Test_Create_Floppy_09;

end Atr_Tools_Test.Create_Floppy;

---------------------------------------------------------------- {{{ ----------
--  vim: set textwidth=0 nowrap tabstop=8 shiftwidth=3 softtabstop=3 expandtab :
--  vim: set filetype=ada fileencoding=utf-8 fileformat=unix foldmethod=marker :
--  vim: set spell spelllang=en_gb
