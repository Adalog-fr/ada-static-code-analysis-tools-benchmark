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
with Ada.Integer_Text_IO;
with AdaCL.Trace;
with Atr_Tools.File_Body;
with Atr_Tools.File_Header;
with Atr_Tools.Byte_IO;

procedure Atr_Tools.Create_Floppy (
   File_Name        : String;
   Sector_Size      : Integer;
   Sector_Per_Track : Integer;
   Tracks_Per_Side  : Integer;
   Sides_Per_Disk   : Integer)
with
   Pre =>  (
      (Sector_Size      in Single_Density  | Double_Density) and then
      (Sector_Per_Track in Atari_Standard  | Atari_Enhanced) and then
      (Tracks_Per_Side  in Standard_Tracks | Double_Tracks)  and then
      (Sides_Per_Disk   in 1               | 2))
is
   use Ada.Text_IO;
   use Ada.Integer_Text_IO;

   Trace : AdaCL.Trace.Object := AdaCL.Trace.Function_Trace;
   pragma Unreferenced (Trace);

   File        : Byte_IO.File_Type;
   New_Body    : constant File_Body.Body_Type'Class     := File_Body.Create (
      Sector_Size,
      Sector_Per_Track,
      Tracks_Per_Side,
      Sides_Per_Disk);
   New_Header  : constant File_Header.Header_Type'Class := File_Header.Create (New_Body);
begin
   AdaCL.Trace.Write ("Filename         : " & File_Name);
   AdaCL.Trace.Write ("Sector size      : " & Sector_Size'Image);
   AdaCL.Trace.Write ("Sector per track : " & Sector_Per_Track'Image);
   AdaCL.Trace.Write ("Tracks per side  : " & Tracks_Per_Side'Image);
   AdaCL.Trace.Write ("Sides per disk   : " & Sides_Per_Disk'Image);

   Byte_IO.Create (File, Byte_IO.Out_File, File_Name, Form => "");
   New_Header.Write (File);
   New_Body.Write (File);
   Byte_IO.Close (File);

   if AdaCL.Trace.Is_Verbose_Enabled then
      Put ("File created     : "); Put (File_Name);                                  New_Line;
      Put ("Sector size      : "); Put (Sector_Size,      Width => 12,  Base => 10); New_Line;
      Put ("Sector per track : "); Put (Sector_Per_Track, Width => 12,  Base => 10); New_Line;
      Put ("Tracks per side  : "); Put (Tracks_Per_Side,  Width => 12,  Base => 10); New_Line;
      Put ("Sides per disk   : "); Put (Sides_Per_Disk,   Width => 12,  Base => 10); New_Line;
   end if;
end Atr_Tools.Create_Floppy;

---------------------------------------------------------------- {{{ ----------
--  vim: set textwidth=0 nowrap tabstop=8 shiftwidth=3 softtabstop=3 expandtab :
--  vim: set filetype=ada fileencoding=utf-8 fileformat=unix foldmethod=expr :
--  vim: set spell spelllang=en_gb :
