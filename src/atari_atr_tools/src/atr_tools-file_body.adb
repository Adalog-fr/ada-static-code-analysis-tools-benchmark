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

package body Atr_Tools.File_Body is

   ---
   --  Create a floppy drive image
   --
   function Create (
      Sector_Size       : Integer;
      Sector_Per_Track  : Integer;
      Tracks_Per_Side   : Integer;
      Sides_Per_Disk    : Integer)
      return Body_Type'Class
   is
      Trace : AdaCL.Trace.Object := AdaCL.Trace.Function_Trace;
      pragma Unreferenced (Trace);

   begin
      return Body_Type'(
         Sector_Size     => Sector_Size,
         Sector_Per_Disk => Sector_Per_Track * Tracks_Per_Side * Sides_Per_Disk);
   end Create;

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
   is
      Trace : AdaCL.Trace.Object := AdaCL.Trace.Function_Trace;
      pragma Unreferenced (Trace);

   begin
      return Body_Type'(
         Sector_Size     => High_Density,
         Sector_Per_Disk => Sector_Per_Track * Tracks_Per_Side * Sides_Per_Disk);
   end Create;

   ---
   --  Create a hard drive image
   --
   --  Hard drives are always High_Density and have otherwise little restrictions
   --
   function Create (Sector_Per_Disk : Integer) return Body_Type'Class
   is
      Trace : AdaCL.Trace.Object := AdaCL.Trace.Function_Trace;
      pragma Unreferenced (Trace);

   begin
      return Body_Type'(
         Sector_Size     => High_Density,
         Sector_Per_Disk => Sector_Per_Disk);
   end Create;

   ---
   --  Write empty disk body
   --
   procedure Write (
      This : Body_Type;
      File : Byte_IO.File_Type)
   is
      Trace : AdaCL.Trace.Object := AdaCL.Trace.Function_Trace;
      pragma Unreferenced (Trace);

      use Byte_IO;
   begin
      for i in 1 .. This.Disk_Size loop
         Write (File, 16#0#);
      end loop;
   end Write;

end Atr_Tools.File_Body;

---------------------------------------------------------------- {{{ ----------
--  vim: set textwidth=0 nowrap tabstop=8 shiftwidth=3 softtabstop=3 expandtab :
--  vim: set filetype=ada fileencoding=utf-8 fileformat=unix foldmethod=expr :
--  vim: set spell spelllang=en_gb :
