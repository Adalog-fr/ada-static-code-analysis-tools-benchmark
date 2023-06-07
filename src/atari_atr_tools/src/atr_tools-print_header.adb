---------------------------------------------------------------- {{{ ----------
--  Copyright © 2020 … 2023 Martin Krischik «krischik@users.sourceforge.net»
-------------------------------------------------------------------------------
--  This library is free software; you can redistribute it and/or modify it
--  under the terms of the GNU Library General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or (at your
--  option) any later version.
--
--  This library is distributed in the hope that it will be useful, but WITHOUT
--  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public
--  License for more details.
--
--  You should have received a copy of the GNU Library General Public License
--  along with this library; if not, write to the Free Software Foundation,
--  Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
---------------------------------------------------------------- }}} ----------

pragma License (Modified_Gpl);
pragma Ada_2022;

with Ada.Text_IO;
with AdaCL.Trace;
with Atr_Tools.Boolean_Text_IO;
with Atr_Tools.Byte_IO;
with Atr_Tools.Byte_Text_IO;
with Atr_Tools.Double_Word_Text_IO;
with Atr_Tools.File_Body;
with Atr_Tools.File_Header;
with Atr_Tools.Word_Text_IO;
with Interfaces;

procedure Atr_Tools.Print_Header (File_Name : String) is
   Trace : AdaCL.Trace.Object := AdaCL.Trace.Function_Trace;
   pragma Unreferenced (Trace);

   File : Byte_IO.File_Type;

begin
   AdaCL.Trace.Write ("Filename : " & File_Name);

   Byte_IO.Open (File, Byte_IO.In_File, File_Name, Form => "");

   declare
      use Ada.Text_IO;
      use Byte_Text_IO;
      use Double_Word_Text_IO;
      use File_Header;
      use Word_Text_IO;
      use Boolean_Text_IO;

      use type File_Header.Word;

      Header       : constant Header_Type'Class := Read (File);
      Paragraphs   : constant Double_Word       := Header.Paragraphs;
      Sectors      : constant Double_Word       := Header.Sectors;
      Bytes        : constant Double_Word       := Header.Bytes;
      Boot_Sectors : constant Boolean           := Header.Correct_Boot_Sectors;

      procedure Put (Spare : in Spare_Type)
      is
         use type Interfaces.Unsigned_64;

         package Unsigned_64_IO is new Ada.Text_IO.Modular_IO (Interfaces.Unsigned_64);

         Value : Interfaces.Unsigned_64 := 0;
      begin
         for I of Spare loop
            Value := Value * 2 ** 8 + Interfaces.Unsigned_64 (I);
         end loop;

         Unsigned_64_IO.Put (Value, Width => 12,  Base => 16);
      end Put;

   begin
      Put ("File name        : "); Put (File_Name);                                              New_Line;
      Put ("Magic            : "); Put (Header.Magic,        Width => 12,  Base => 16);          New_Line;
      Put ("Paragraphs       : "); Put (Paragraphs,          Width => 12,  Base => 10);          New_Line;
      Put ("Sector size      : "); Put (Header.Sector_Size,  Width => 12,  Base => 10);          New_Line;
      Put ("Flags            : "); Put (Header.Flags,        Width => 12,  Base =>  2);          New_Line;
      Put ("Bad Sectors      : "); Put (Header.Bad_Sector,   Width => 12,  Base => 10);          New_Line;
      Put ("Unused           : "); Put (Header.Spare);                                           New_Line;
      Put ("Sectors          : "); Put (Sectors,             Width => 12,  Base => 10);          New_Line;
      Put ("Bytes            : "); Put (Bytes,               Width => 12,  Base => 10);          New_Line;
      Put ("Boot Sectors     : "); Put (Boot_Sectors,        Width => 12,  Set  => Lower_Case);  New_Line;

      New_Line;

      if Header.Magic /= Header_Id then
         Put ("Magic should be "); Put (Header_Id, Base => 16); Put_Line ("!");
      end if;

      if Header.Sector_Size = File_Body.High_Density then
         Put_Line ("Format is a hard drive image");
      else
         Put ("Floppy disk ");
         case Paragraphs is
            when P_DD_DS_40 =>
               Put ("double density, double sided, 40 track or ");
               Put_Line ("double density, single sided, 80 track");
            when P_DD_DS_80 =>
               Put_Line ("double density, double sided, 80 track");
            when P_DD_SS_40 =>
               Put_Line ("double density, single sided, 40 track");
            when P_ED_SS_40 =>
               Put_Line ("enhanced density, double sided, 40 track");
            when P_SD_DS_40 =>
               Put ("single density, double sided, 40 track or ");
               Put_Line ("single density, single sided, 80 track");
            when P_SD_DS_80 =>
               Put_Line ("single density, double sided, 80 track");
            when P_SD_SS_40 =>
               Put_Line ("single density, single sided, 40 track");
            when others =>
               Put_Line ("of unknown format.");
         end case;
      end if;
   end;
   Byte_IO.Close (File);
end Atr_Tools.Print_Header;

---------------------------------------------------------------- {{{ ----------
--  vim: set textwidth=0 nowrap tabstop=8 shiftwidth=3 softtabstop=3 expandtab :
--  vim: set filetype=ada fileencoding=utf-8 fileformat=unix foldmethod=expr :
--  vim: set spell spelllang=en_gb :
