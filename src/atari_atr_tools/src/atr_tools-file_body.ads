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

with Atr_Tools.Byte_IO;

package Atr_Tools.File_Body is

   ---
   --  Valid sector sizes
   --
   Single_Density : constant := 128;
   Double_Density : constant := 256;
   High_Density   : constant := 512;

   ---
   --  valid sector per track
   --
   Atari_Standard : constant := 18;
   Atari_Enhanced : constant := 26;

   ---
   --  valid sector per track
   --
   Standard_Tracks : constant := 40;
   Double_Tracks   : constant := 80;

   ---
   --  Maximum sectors allowed for Atari is 32 Giga-sectors minus 1.
   --
   Maximum_Sectors : constant := 2 ** 25 - 1;

   ---
   --  Body content
   --
   type Body_Type is tagged private;

   ---
   --  Create a floppy drive image
   --
   function Create (
      Sector_Size       : Integer;
      Sector_Per_Track  : Integer;
      Tracks_Per_Side   : Integer;
      Sides_Per_Disk    : Integer)
      return Body_Type'Class
   with Pre => (
      (Sector_Size      in Single_Density  | Double_Density) and then
      (Sector_Per_Track in Atari_Standard  | Atari_Enhanced) and then
      (Tracks_Per_Side  in Standard_Tracks | Double_Tracks)  and then
      (Sides_Per_Disk   in 1               | 2));

   ---
   --  Create a hard drive image
   --
   --  Hard drives are always High_Density and have otherwise little restrictions
   --
   function Create (
      Sector_Per_Track  : Integer;
      Tracks_Per_Side   : Integer;
      Sides_Per_Disk    : Integer)
      return Body_Type'Class
   with Pre => (
      High_Density * Sector_Per_Track * Tracks_Per_Side * Sides_Per_Disk <= Maximum_Sectors);

   ---
   --  Create a hard drive image
   --
   --  Hard drives are always High_Density and have otherwise little restrictions
   --
   function Create (Sector_Per_Disk : Integer) return Body_Type'Class
   with Pre => (
      Sector_Per_Disk * High_Density <= Maximum_Sectors);

   ---
   --  size in bytes of the disk image
   --
   function Disk_Size (This : Body_Type) return Integer;
   ---
   --  Size of a single sector.
   --
   function Sector_Size (This : Body_Type) return Integer;

   procedure Write (
      This : Body_Type;
      File : Byte_IO.File_Type);

private

   ---
   --  Body content
   --
   type Body_Type is tagged record
      Sector_Size     : Integer;
      Sector_Per_Disk : Integer;
   end record;

   ---
   --  size in bytes of the disk image
   --
   function Disk_Size (This : Body_Type) return Integer
   is (Single_Density * 3 + This.Sector_Size * (This.Sector_Per_Disk - 3));
   pragma Inline (Disk_Size);
   pragma Pure_Function (Disk_Size);

   ---
   --  Size of a single sector.
   --
   function Sector_Size (This : Body_Type) return Integer
   is (This.Sector_Size);
   pragma Inline (Sector_Size);
   pragma Pure_Function (Sector_Size);

end Atr_Tools.File_Body;

---------------------------------------------------------------- {{{ ----------
--  vim: set textwidth=0 nowrap tabstop=8 shiftwidth=3 softtabstop=3 expandtab :
--  vim: set filetype=ada fileencoding=utf-8 fileformat=unix foldmethod=expr :
--  vim: set spell spelllang=en_gb :
