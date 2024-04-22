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

package body Atr_Tools.File_Header is

   function Create (Disk_Body : File_Body.Body_Type)
      return Header_Type
   is
      Test : Trace.Object := Trace.Function_Trace;
      pragma Unreferenced (Test);

      Size_In_Paragraphs : constant := Double_Word (File_Body.Disk_Size (Disk_Body) / Paragraph_Size);
   begin
      return Header_Type'(
         Magic       => File_Header.Header_Id,
         Pars_Low    => Word (Size_In_Paragraphs and 16#FFFF#),
         Sector_Size => Word (Disk_Body.Sector_Size),
         Pars_High   => Byte (Interfaces.Shift_Right (Size_In_Paragraphs, 16)),
         CRC         => 0,
         Unused      => 0,
         Flags       => 1);
   end Create;

   ---
   --  Recombine the two Pars fields
   --
   function Pars (This : Header_Type)
      return Double_Word
   is (Double_Word (This.Pars_Low) + Double_Word (This.Pars_High) * 2 ** 16);

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

   function Read (File : Byte_IO.File_Type)
      return Header_Type
   is
      Test : Trace.Object := Trace.Function_Trace;
      pragma Unreferenced (Test);

      Retval : Header_Type;
   begin
      Retval.Magic       := Read_Word (File);
      Retval.Pars_Low    := Read_Word (File);
      Retval.Sector_Size := Read_Word (File);
      Retval.Pars_High   := Read_Byte (File);

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
      Item : Double_Word)
   is
      Current_Item : Double_Word := Item;
   begin
      for I in 1 .. Interfaces.Unsigned_32'Size /  Interfaces.Unsigned_8'Size loop
         Byte_IO.Write (File, Byte (Current_Item and 16#FF#));
         Current_Item := Interfaces.Shift_Right (Current_Item, 8);
      end loop;
   end Write;

   procedure Write (
      File : Byte_IO.File_Type;
      Item : Header_Type)
   is
      Test : Trace.Object := Trace.Function_Trace;
      pragma Unreferenced (Test);

      use Byte_IO;
   begin
      Write (File, Item.Magic);
      Write (File, Item.Pars_Low);
      Write (File, Item.Sector_Size);
      Write (File, Item.Pars_High);
      Write (File, Item.CRC);
      Write (File, Item.Unused);
      Write (File, Item.Flags);
   end Write;

end Atr_Tools.File_Header;

---------------------------------------------------------------- {{{ ----------
--  vim: set textwidth=0 nowrap tabstop=8 shiftwidth=3 softtabstop=3 expandtab :
--  vim: set filetype=ada fileencoding=utf-8 fileformat=unix foldmethod=expr :
--  vim: set spell spelllang=en_gb :
