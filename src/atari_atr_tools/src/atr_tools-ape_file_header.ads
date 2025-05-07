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

with Interfaces;
with Atr_Tools.Byte_IO;
with Atr_Tools.File_Body;
with Atr_Tools.File_Header;

---
--  AtariMax extensions to the standard ATR format.
--
package Atr_Tools.APE_File_Header is
   use File_Header;
   use type Interfaces.Unsigned_16;
   use type Interfaces.Unsigned_32;

   ---
   --  The size in paragraphs is 24 bits in ATR files with an
   --  APE header.
   --
   subtype Paragraph_Count is Double_Word range 0 .. 2 ** 24 - 1;

   ---
   --  Bit 0 ReadOnly
   type Header_Type is record
      Magic       : Word := Header_Id; -- $0296 (sum of 'NICKATARI')
      Pars_Low    : Word;              -- size of this disk image, in paragraphs (size/$10)
      Sector_Size : Word;              -- sector size. ($80 or $100) bytes/sector
      Pars_High   : Byte;              -- high part of size, in paragraphs (added by REV 3.00)
      CRC         : Double_Word;       -- 32bit CRC of file (added by APE?)
      Unused      : Double_Word;       -- unused
      Flags       : Byte;              -- bit 0 (ReadOnly) (added by APE?)
   end record;

   ---
   --  Create a header for a given body.
   --
   function Create (Disk_Body : File_Body.Body_Type'Class)
      return Header_Type
   with
      Post => Create'Result.Magic = Header_Id;

   ---
   --  Recombine the two Pars fields
   --
   function Paragraphs (This : Header_Type)
      return Paragraph_Count;

   ---
   --  Write APE Header to ATR file
   --
   --  File : File to write the header to
   --  Item : Header information to write
   --
   procedure Write (
      File : Byte_IO.File_Type;
      Item : Header_Type);

   ---
   --  Read APE header from ATR file. File pointer must be positions to the
   --  beginning of the file
   --
   --  File : File to read the header from
   --  return : Header information to read
   --
   function Read (File : Byte_IO.File_Type)
      return Header_Type;

private

end Atr_Tools.File_Header;

---------------------------------------------------------------- {{{ ----------
--  vim: set textwidth=0 nowrap tabstop=8 shiftwidth=3 softtabstop=3 expandtab :
--  vim: set filetype=ada fileencoding=utf-8 fileformat=unix foldmethod=expr :
--  vim: set spell spelllang=en_gb :
