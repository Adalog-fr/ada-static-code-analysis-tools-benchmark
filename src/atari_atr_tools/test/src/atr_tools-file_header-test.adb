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

with AdaCL.Trace;
with Atr_Tools.File_Body;
with Atr_Tools_Test;

package body Atr_Tools.File_Header.Test is

   use Atr_Tools_Test;

   function Read_Header (File_Name : String) return Header_Type'Class
   is
      Trace : AdaCL.Trace.Object := AdaCL.Trace.Function_Trace;
      pragma Unreferenced (Trace);

      File : Byte_IO.File_Type;
   begin
      Byte_IO.Open (File, Byte_IO.In_File, File_Name, Form => "");

      declare
         Retval       : constant Header_Type'Class := Read (File);
      begin
         Byte_IO.Close (File);

         return Retval;
      end;
   end Read_Header;

   overriding procedure Register_Tests (T : in out Test_Case) is
      Trace : AdaCL.Trace.Object := AdaCL.Trace.Function_Trace;
      pragma Unreferenced (Trace);

      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Read_01'Access, "Read SD_SS_40");
      Register_Routine (T, Test_Read_02'Access, "Read ED_SS_40");
      Register_Routine (T, Test_Read_03'Access, "Read DD_SS_40");
      Register_Routine (T, Test_Read_04'Access, "Read SD_DS_40");
      Register_Routine (T, Test_Read_05'Access, "Read SD_SS_80");
      Register_Routine (T, Test_Read_06'Access, "Read DD_DS_40");
      Register_Routine (T, Test_Read_07'Access, "Read DD_SS_80");
      Register_Routine (T, Test_Read_08'Access, "Read SD_DS_80");
      Register_Routine (T, Test_Read_09'Access, "Read DD_DS_80");
   end Register_Tests;

   procedure Test_Read_01 (T : in out AUnit.Test_Cases.Test_Case'Class) is
      Trace : AdaCL.Trace.Object := AdaCL.Trace.Function_Trace;
      pragma Unreferenced (Trace);
      pragma Unreferenced (T);

      File_Name : constant String := "SD_SS_40.atr";
      Test      : constant Header_Type'Class := Read_Header (File_Name);
   begin
      Assert_Word.Equal        (Test.Magic,       Header_Id,                "Magic");
      Assert_Word.Equal        (Test.Sector_Size, File_Body.Single_Density, "Sector_Size");
      Assert_Double_Word.Equal (Test.Paragraphs,  P_SD_SS_40,               "Paragraphs");
      Assert_Byte.Equal        (Test.Flags,       1,                        "Flags");
      Assert_Word.Equal        (Test.Bad_Sector,  0,                        "Bad_Sector");
   end Test_Read_01;

   procedure Test_Read_02 (T : in out AUnit.Test_Cases.Test_Case'Class) is
      Trace : AdaCL.Trace.Object := AdaCL.Trace.Function_Trace;
      pragma Unreferenced (Trace);
      pragma Unreferenced (T);

      File_Name : constant String := "ED_SS_40.atr";
      Test      : constant Header_Type'Class := Read_Header (File_Name);
   begin
      Assert_Word.Equal        (Test.Magic,       Header_Id,                "Magic");
      Assert_Word.Equal        (Test.Sector_Size, File_Body.Single_Density, "Sector_Size");
      Assert_Double_Word.Equal (Test.Paragraphs,  P_ED_SS_40,               "Paragraphs");
      Assert_Byte.Equal        (Test.Flags,       1,                        "Flags");
      Assert_Word.Equal        (Test.Bad_Sector,  0,                        "Bad_Sector");
   end Test_Read_02;

   procedure Test_Read_03 (T : in out AUnit.Test_Cases.Test_Case'Class) is
      Trace : AdaCL.Trace.Object := AdaCL.Trace.Function_Trace;
      pragma Unreferenced (Trace);
      pragma Unreferenced (T);

      File_Name : constant String := "DD_SS_40.atr";
      Test      : constant Header_Type'Class := Read_Header (File_Name);
   begin
      Assert_Word.Equal        (Test.Magic,       Header_Id,                "Magic");
      Assert_Word.Equal        (Test.Sector_Size, File_Body.Double_Density, "Sector_Size");
      Assert_Double_Word.Equal (Test.Paragraphs,  P_DD_SS_40,               "Paragraphs");
      Assert_Byte.Equal        (Test.Flags,       1,                        "Flags");
      Assert_Word.Equal        (Test.Bad_Sector,  0,                        "Bad_Sector");
   end Test_Read_03;

   procedure Test_Read_04 (T : in out AUnit.Test_Cases.Test_Case'Class) is
      Trace : AdaCL.Trace.Object := AdaCL.Trace.Function_Trace;
      pragma Unreferenced (Trace);
      pragma Unreferenced (T);

      File_Name : constant String := "SD_DS_40.atr";
      Test      : constant Header_Type'Class := Read_Header (File_Name);
   begin
      Assert_Word.Equal        (Test.Magic,       Header_Id,                "Magic");
      Assert_Word.Equal        (Test.Sector_Size, File_Body.Single_Density, "Sector_Size");
      Assert_Double_Word.Equal (Test.Paragraphs,  P_SD_DS_40,               "Paragraphs");
      Assert_Byte.Equal        (Test.Flags,       1,                        "Flags");
      Assert_Word.Equal        (Test.Bad_Sector,  0,                        "Bad_Sector");
   end Test_Read_04;

   procedure Test_Read_05 (T : in out AUnit.Test_Cases.Test_Case'Class) is
      Trace : AdaCL.Trace.Object := AdaCL.Trace.Function_Trace;
      pragma Unreferenced (Trace);
      pragma Unreferenced (T);

      File_Name : constant String := "SD_SS_80.atr";
      Test      : constant Header_Type'Class := Read_Header (File_Name);
   begin
      Assert_Word.Equal        (Test.Magic,       Header_Id,                "Magic");
      Assert_Word.Equal        (Test.Sector_Size, File_Body.Single_Density, "Sector_Size");
      Assert_Double_Word.Equal (Test.Paragraphs,  P_SD_SS_80,               "Paragraphs");
      Assert_Byte.Equal        (Test.Flags,       1,                        "Flags");
      Assert_Word.Equal        (Test.Bad_Sector,  0,                        "Bad_Sector");
   end Test_Read_05;

   procedure Test_Read_06 (T : in out AUnit.Test_Cases.Test_Case'Class) is
      Trace : AdaCL.Trace.Object := AdaCL.Trace.Function_Trace;
      pragma Unreferenced (Trace);
      pragma Unreferenced (T);

      File_Name : constant String := "DD_DS_40.atr";
      Test      : constant Header_Type'Class := Read_Header (File_Name);
   begin
      Assert_Word.Equal        (Test.Magic,       Header_Id,                "Magic");
      Assert_Word.Equal        (Test.Sector_Size, File_Body.Double_Density, "Sector_Size");
      Assert_Double_Word.Equal (Test.Paragraphs,  P_DD_DS_40,               "Paragraphs");
      Assert_Byte.Equal        (Test.Flags,       1,                        "Flags");
      Assert_Word.Equal        (Test.Bad_Sector,  0,                        "Bad_Sector");
   end Test_Read_06;

   procedure Test_Read_07 (T : in out AUnit.Test_Cases.Test_Case'Class) is
      Trace : AdaCL.Trace.Object := AdaCL.Trace.Function_Trace;
      pragma Unreferenced (Trace);
      pragma Unreferenced (T);

      File_Name : constant String := "DD_SS_80.atr";
      Test      : constant Header_Type'Class := Read_Header (File_Name);
   begin
      Assert_Word.Equal        (Test.Magic,       Header_Id,                "Magic");
      Assert_Word.Equal        (Test.Sector_Size, File_Body.Double_Density, "Sector_Size");
      Assert_Double_Word.Equal (Test.Paragraphs,  P_DD_SS_80,               "Paragraphs");
      Assert_Byte.Equal        (Test.Flags,       1,                        "Flags");
      Assert_Word.Equal        (Test.Bad_Sector,  0,                        "Bad_Sector");
   end Test_Read_07;

   procedure Test_Read_08 (T : in out AUnit.Test_Cases.Test_Case'Class) is
      Trace : AdaCL.Trace.Object := AdaCL.Trace.Function_Trace;
      pragma Unreferenced (Trace);
      pragma Unreferenced (T);

      File_Name : constant String := "SD_DS_80.atr";
      Test      : constant Header_Type'Class := Read_Header (File_Name);
   begin
      Assert_Word.Equal        (Test.Magic,       Header_Id,                "Magic");
      Assert_Word.Equal        (Test.Sector_Size, File_Body.Single_Density, "Sector_Size");
      Assert_Double_Word.Equal (Test.Paragraphs,  P_SD_DS_80,               "Paragraphs");
      Assert_Byte.Equal        (Test.Flags,       1,                        "Flags");
      Assert_Word.Equal        (Test.Bad_Sector,  0,                        "Bad_Sector");
   end Test_Read_08;

   procedure Test_Read_09 (T : in out AUnit.Test_Cases.Test_Case'Class) is
      Trace : AdaCL.Trace.Object := AdaCL.Trace.Function_Trace;
      pragma Unreferenced (Trace);
      pragma Unreferenced (T);

      File_Name : constant String := "DD_DS_80.atr";
      Test      : constant Header_Type'Class := Read_Header (File_Name);
   begin
      Assert_Word.Equal        (Test.Magic,       Header_Id,                "Magic");
      Assert_Word.Equal        (Test.Sector_Size, File_Body.Double_Density, "Sector_Size");
      Assert_Double_Word.Equal (Test.Paragraphs,  P_DD_DS_80,               "Paragraphs");
      Assert_Byte.Equal        (Test.Flags,       1,                        "Flags");
      Assert_Word.Equal        (Test.Bad_Sector,  0,                        "Bad_Sector");
   end Test_Read_09;

end Atr_Tools.File_Header.Test;

---------------------------------------------------------------- {{{ ----------
--  vim: set textwidth=0 nowrap tabstop=8 shiftwidth=3 softtabstop=3 expandtab :
--  vim: set filetype=ada fileencoding=utf-8 fileformat=unix foldmethod=marker :
--  vim: set spell spelllang=en_gb
