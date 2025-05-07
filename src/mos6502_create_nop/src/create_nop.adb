---------------------------------------------------------------- {{{ ----------
--  Copyright © 2020 … 2022 Martin Krischik «krischik@users.sourceforge.net»
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

with Ada.Command_Line;
with Ada.IO_Exceptions;
with Ada.Exceptions;
with Ada.Sequential_IO;
with Ada.Text_IO;
with Interfaces;

procedure Create_NOP is
   type Byte is new Interfaces.Unsigned_8;

   package Byte_IO is new Ada.Sequential_IO (Byte);

   Byte_Output : Byte_IO.File_Type;
   NOP         : constant Byte   := 16#EA#;
   Rom_Size    : constant        := 32 * 2**10;
   File_Name   : constant String := (
      if Ada.Command_Line.Argument_Count < 1 then
         "create_nop.rom"
      else
         Ada.Command_Line.Argument (1));
begin
   Byte_IO.Create (Byte_Output, Byte_IO.Out_File, File_Name);

   for I in 1 .. Rom_Size loop
      Byte_IO.Write (Byte_Output, NOP);
   end loop;

   Byte_IO.Close (Byte_Output);
exception
   when Error : Ada.IO_Exceptions.Name_Error =>
      Ada.Text_IO.Put_Line ("File “" & File_Name & "” couldn't be created.");
      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (Error));
   when Error : others =>
      Ada.Text_IO.Put_Line ("File “" & File_Name & "” couldn't be written.");
      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (Error));
end Create_NOP;

---------------------------------------------------------------- {{{ ----------
--  vim: set textwidth=0 nowrap tabstop=8 shiftwidth=3 softtabstop=3 expandtab :
--  vim: set filetype=ada fileencoding=utf-8 fileformat=unix foldmethod=marker :
--  vim: set spell spelllang=en_gb :
