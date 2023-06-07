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

package Atr_Tools.File_Header is
   use type Interfaces.Unsigned_16;
   use type Interfaces.Unsigned_32;

   subtype Double_Word  is Interfaces.Unsigned_32;
   subtype Word         is Interfaces.Unsigned_16;
   subtype Byte         is Interfaces.Unsigned_8;

   ---
   --  Magic header ID. This word is the 16 bit sum of the individual ASCII
   --  values of the string of bytes: "NICKATARI".
   --
   Header_Id : constant Word        := 16#0296#;

   ---
   --  Paragraphs is an old IBM term for 16 bytes.
   --
   Paragraph_Size : constant := 16#10#;

   ---
   --  Common disk sizes in Paragraphs. Abbreviation key:
   --
   --  SD: Single Density
   --  DD: Double Density
   --  ED: Atari Enhanced Density
   --
   --  SS: Single Sided
   --  DS: Double Sided
   --
   --  40: 40 Track
   --  80: 80 Track
   --
   --  Note 1: Only the first two formats are standard Atari formats.
   --  Note 2: Two disk formats have the same size and can't be distinguished
   --
   P_SD_SS_40  : constant Double_Word :=  92_160 / Paragraph_Size;
   P_ED_SS_40  : constant Double_Word := 133_120 / Paragraph_Size;
   P_DD_SS_40  : constant Double_Word := 183_936 / Paragraph_Size;
   P_SD_DS_40  : constant Double_Word := 184_320 / Paragraph_Size;
   P_SD_SS_80  : constant Double_Word := 184_320 / Paragraph_Size;
   P_DD_DS_40  : constant Double_Word := 368_256 / Paragraph_Size;
   P_DD_SS_80  : constant Double_Word := 368_256 / Paragraph_Size;
   P_SD_DS_80  : constant Double_Word := 368_640 / Paragraph_Size;
   P_DD_DS_80  : constant Double_Word := 736_896 / Paragraph_Size;

   type Spare_Type is array (1 .. 5) of Byte;

   ---
   --   Bit 4 = 1 means the disk image is treated as copy protected (has bad sectors).
   --   Bit 5 = 1 means the disk is write protected.
   type Header_Type is tagged private;

   ---
   --  $0296 (sum of 'NICKATARI')
   --
   function Magic (This : Header_Type) return Word;

   function Sector_Size (This : Header_Type) return Word;

   function Flags (This : Header_Type) return Byte;

   function Bad_Sector (This : Header_Type) return Word;

   function Spare (This : Header_Type) return Spare_Type;

   ---
   --  Create a header for a given body.
   --
   function Create (Disk_Body : File_Body.Body_Type'Class) return Header_Type'Class
   with
      Post => Create'Result.Magic = Header_Id;

   ---
   --  Recombine the two Pars fields
   --
   function Paragraphs (This : Header_Type) return Double_Word;

   ---
   --  Recombine the two Pars fields and convert to sector count
   --
   function Sectors (This : Header_Type) return Double_Word;

   ---
   --  Recombine the two Pars fields and convert to byte count
   --
   function Bytes (This : Header_Type) return Double_Word;

   ---
   --  The three Boot sectors should always be 128 bytes. Check of the Size of
   --  the image reflects that
   --
   function Correct_Boot_Sectors (This : Header_Type) return Boolean;

   ---
   --  Write Header to ATR file
   --
   --  File : File to write the header to
   --  Item : Header information to write
   --
   procedure Write (
      This : Header_Type;
      File : Byte_IO.File_Type);

   ---
   --  Read header from ATR file. File pointer must be positions to the
   --  beginning of the file
   --
   --  File : File to read the header from
   --  return : Header information to read
   --
   function Read (File : Byte_IO.File_Type) return Header_Type'Class;

private

   ---
   --   Bit 4 = 1 means the disk image is treated as copy protected (has bad sectors).
   --   Bit 5 = 1 means the disk is write protected.
   type Header_Type is tagged record
      Magic       : Word := Header_Id; -- 00 … 01 $0296 (sum of 'NICKATARI')
      Pars_Low    : Word;              -- 02 … 03 size of this disk image, in paragraphs (size / $10)
      Sector_Size : Word;              -- 04 … 05 sector size. ($80 or $100) bytes/sector
      Pars_High   : Word;              -- 06 … 07 high part of size, in paragraphs (added by REV 3.00)
      Flags       : Byte;              -- 08      Bitflags bit 0 (ReadOnly)
      Bad_Sector  : Word;              -- 09 … 0A 1st bad sector
      Spare       : Spare_Type;        -- 0B … 0F Spare bytes.
   end record;

   ---
   --  $0296 (sum of 'NICKATARI')
   --
   function Magic (This : Header_Type) return Word
   is (This.Magic);
   pragma Inline (Magic);
   pragma Pure_Function (Magic);

   function Sector_Size (This : Header_Type) return Word
   is (This.Sector_Size);
   pragma Inline (Sector_Size);
   pragma Pure_Function (Sector_Size);

   function Flags (This : Header_Type) return Byte
   is (This.Flags);
   pragma Inline (Flags);
   pragma Pure_Function (Flags);

   function Bad_Sector (This : Header_Type) return Word
   is (This.Bad_Sector);
   pragma Inline (Bad_Sector);
   pragma Pure_Function (Bad_Sector);

   function Spare (This : Header_Type) return Spare_Type
   is (This.Spare);
   pragma Inline (Spare);
   pragma Pure_Function (Spare);

   ---
   --  Recombine the two Pars fields
   --
   function Paragraphs (This : Header_Type) return Double_Word
   is (Double_Word (This.Pars_Low) + Double_Word (This.Pars_High) * 2 ** 16);
   pragma Inline (Paragraphs);
   pragma Pure_Function (Paragraphs);

   ---
   --  Recombine the two Pars fields and convert to sector count
   --
   function Sectors (This : Header_Type) return Double_Word
   is ((Bytes (This) - (3 * File_Body.Single_Density)) / Interfaces.Unsigned_32 (This.Sector_Size) + 3);
   pragma Inline (Sectors);
   pragma Pure_Function (Sectors);

   ---
   --  Recombine the two Pars fields and convert to byte count
   --
   function Bytes (This : Header_Type) return Double_Word
   is (Paragraphs (This) * Paragraph_Size);
   pragma Inline (Bytes);
   pragma Pure_Function (Bytes);

   ---
   --  Recombine the two Pars fields and convert to sector count
   --
   function Correct_Boot_Sectors (This : Header_Type) return Boolean
   is (((Bytes (This) - (3 * File_Body.Single_Density)) mod Interfaces.Unsigned_32 (This.Sector_Size)) = 0);
   pragma Inline (Correct_Boot_Sectors);
   pragma Pure_Function (Correct_Boot_Sectors);

end Atr_Tools.File_Header;

---------------------------------------------------------------- {{{ ----------
--  vim: set textwidth=0 nowrap tabstop=8 shiftwidth=3 softtabstop=3 expandtab :
--  vim: set filetype=ada fileencoding=utf-8 fileformat=unix foldmethod=expr :
--  vim: set spell spelllang=en_gb :
