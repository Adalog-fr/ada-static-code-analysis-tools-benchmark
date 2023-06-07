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

with Ada.Characters.Latin_1;
with Ada.Exceptions;
with Ada.Text_IO;
with AdaCL.Command_Line.GetOpt;
with AdaCL.Trace;
with Atr_Tools.CommandLine;
with Atr_Tools.Create_Floppy;
with Atr_Tools.Print_Header;

---
--  Tools to handle ATR files — main procedure
--
procedure Atr_Tools.Main is

   package GetOpt renames AdaCL.Command_Line.GetOpt;
   package Latin_1 renames Ada.Characters.Latin_1;

   ----------------------------------------------------------------------------

   Trace : AdaCL.Trace.Object := AdaCL.Trace.Function_Trace;
   pragma Unreferenced (Trace);

begin
   Try : declare
      use CommandLine;

      Options : CommandLine.Object;
   begin
      Options.Parse;

      case Options.Operation is
         when  Print_Header =>
            Atr_Tools.Print_Header (Options.Get_File);
         when  Create_Floppy =>
            Atr_Tools.Create_Floppy (
               Options.Get_File,
               Options.Get_Density,
               Options.Get_Sectors,
               Options.Get_Tracks,
               Options.Get_Sides);
         when None =>
            Ada.Text_IO.Put_Line (
               Ada.Text_IO.Standard_Error,
               "No operation given. User „--help“ to show available operations and options.");
      end case;
   exception
      when AnException : GetOpt.Option_Parse_Error =>
         AdaCL.Trace.Write_Error (
            "Error parsing command line options"            & Latin_1.LF &
            Ada.Exceptions.Exception_Message (AnException)  & Latin_1.LF &
            "Use '--help' for help");
      when AnException : others =>
         AdaCL.Trace.Write_Error (AnException);
   end Try;
end Atr_Tools.Main;

---------------------------------------------------------------- {{{ ----------
--  vim: set textwidth=0 nowrap tabstop=8 shiftwidth=3 softtabstop=3 expandtab :
--  vim: set filetype=ada fileencoding=utf-8 fileformat=unix foldmethod=expr :
--  vim: set spell spelllang=en_gb :
