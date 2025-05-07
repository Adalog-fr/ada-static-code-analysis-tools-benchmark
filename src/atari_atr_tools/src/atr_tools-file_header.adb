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

with AdaCL.Trace;

package body Atr_Tools.File_Header is

   Spare_Data : constant Spare_Type := [others => 0];

   function Create (Disk_Body : File_Body.Body_Type'Class)
      return Header_Type'Class
   is
      Trace : AdaCL.Trace.Object := AdaCL.Trace.Function_Trace;
      pragma Unreferenced (Trace);

      Size_In_Paragraphs : constant Double_Word := Double_Word (File_Body.Disk_Size (Disk_Body) / Paragraph_Size);
   begin
      return Header_Type'(
         Magic       => File_Header.Header_Id,
         Pars_Low    => Word (Size_In_Paragraphs and 16#FFFF#),
         Sector_Size => Word (Disk_Body.Sector_Size),
         Pars_High   => Word (Interfaces.Shift_Right (Size_In_Paragraphs, 16)),
         Flags       => 1,
         Bad_Sector  => 0,
         Spare       => Spare_Data);
   end Create;

   function Read_Byte (File : Byte_IO.File_Type)
      return Byte
   is
      Retval : Byte;
   begin
      Byte_IO.Read (File, Retval);

      return Retval;
   end Read_Byte;

   function Read_Word (File : Byte_IO.File_Type)
      return Word
   is
      Retval : Word;
   begin
      Retval := Word (Read_Byte (File));
      Retval := Retval + Interfaces.Shift_Left (Word (Read_Byte (File)), 8);

      return Retval;
   end Read_Word;

   function Read_Spare (File : Byte_IO.File_Type)
      return Spare_Type
   is
      Retval : Spare_Type;
   begin
      for I in Spare_Type'Range loop
         Retval (I) := Read_Byte (File);
      end loop;

      return Retval;
   end Read_Spare;

   function Read (File : Byte_IO.File_Type)
      return Header_Type'Class
   is
      Trace : AdaCL.Trace.Object := AdaCL.Trace.Function_Trace;
      pragma Unreferenced (Trace);

      Retval : Header_Type;
   begin
      Retval.Magic       := Read_Word (File);
      Retval.Pars_Low    := Read_Word (File);
      Retval.Sector_Size := Read_Word (File);
      Retval.Pars_High   := Read_Word (File);
      Retval.Flags       := Read_Byte (File);
      Retval.Bad_Sector  := Read_Word (File);
      Retval.Spare       := Read_Spare (File);

      return Retval;
   end Read;

   procedure Write (
      File : Byte_IO.File_Type;
      Item : Word)
   is
      Current_Item : Word := Item;
   begin
      for I in 1 .. Interfaces.Unsigned_16'Size /  Interfaces.Unsigned_8'Size loop
         Byte_IO.Write (File, Byte (Current_Item and 16#FF#));
         Current_Item := Interfaces.Shift_Right (Current_Item, 8);
      end loop;
   end Write;

   procedure Write (
      File : Byte_IO.File_Type;
      Item : Spare_Type)
   is
   begin
      for I in Spare_Type'Range loop
         Byte_IO.Write (File, Item (I));
      end loop;
   end Write;

   procedure Write (
      This : Header_Type;
      File : Byte_IO.File_Type)
   is
      Trace : AdaCL.Trace.Object := AdaCL.Trace.Function_Trace;
      pragma Unreferenced (Trace);

      use Byte_IO;
   begin
      Write (File, This.Magic);
      Write (File, This.Pars_Low);
      Write (File, This.Sector_Size);
      Write (File, This.Pars_High);
      Write (File, This.Flags);
      Write (File, This.Bad_Sector);
      Write (File, This.Spare);
   end Write;

end Atr_Tools.File_Header;

---------------------------------------------------------------- {{{ ----------
--  vim: set textwidth=0 nowrap tabstop=8 shiftwidth=3 softtabstop=3 expandtab :
--  vim: set filetype=ada fileencoding=utf-8 fileformat=unix foldmethod=expr :
--  vim: set spell spelllang=en_gb :
