------------------------------------------------------------------------------
--  Copyright © 2020 … 2023 Martin Krischik «krischik@users.sourceforge.net»
------------------------------------------------------------------------------
--  This program is free software; you can redistribute it and/or modify it
--  under the terms of the GNU General Public License as published by the Free
--  Software Foundation; either version 2 of the License, or (at your option)
--  any later version.
--
--  This program is distributed in the hope that it will be useful, but
--  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
--  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
--  for more details.
--
--  You should have received a copy of the GNU General Public License along
--  with this program; if not, write to the Free Software Foundation, Inc., 59
--  Temple Place - Suite 330, Boston, MA 02111-1307, USA.
------------------------------------------------------------------------------

pragma License (Modified_Gpl);
pragma Ada_2022;

with Ada.Characters.Handling;
with Ada.Text_IO;
with AdaCL.Trace;

---
--  Parse commandline of ATR tools.
--
package body Atr_Tools.CommandLine is

   ---------------------------------------------------------------------------^

   package Unbounded renames Ada.Strings.Unbounded;
   package GetOpt    renames AdaCL.Command_Line.GetOpt;

   ---------------------------------------------------------------------------

   use type Unbounded.Unbounded_String;

   ---------------------------------------------------------------------------

   --
   --  Print header of an atr file
   --
   Print_Header_Long    : constant String    := "print-header";
   Print_Header_Short   : constant Character := 'p';

   --
   --  Create an floppy image.
   --
   Create_Floppy_Long   : constant String    := "format-floppy";
   Create_Floppy_Short  : constant Character := 'f';

   --
   --  Densities for floppy images.
   --
   Density_Long         : constant String    := "density";
   Density_Short        : constant Character := 'd';
   Density_SD           : constant String    := "SD";
   Density_ED           : constant String    := "ED";
   Density_DD           : constant String    := "DD";

   --
   --  Densities for floppy images.
   --
   Side_Long            : constant String    := "side";
   Side_Short           : constant Character := 's';
   Side_SS              : constant String    := "SS";
   Side_DS              : constant String    := "DS";

   --
   --  Densities for floppy images.
   --
   Track_Long           : constant String    := "track";
   Track_Short          : constant Character := 't';
   Track_40             : constant String    := "40";
   Track_80             : constant String    := "80";

   ---------------------------------------------------------------------------

   Pattern : constant String := [
      GetOpt.Option_Error,
      Print_Header_Short,
      Create_Floppy_Short,
      Density_Short, GetOpt.Option_Argument,
      Side_Short, GetOpt.Option_Argument,
      Track_Short, GetOpt.Option_Argument];

   ---
   --  filename of the ATR file
   --
   function Get_File (This : Object) return String is
   begin
      if This.File = Unbounded.Null_Unbounded_String or else
         Unbounded.Length (This.File) = 0
      then
         raise GetOpt.Option_Parse_Error with "Operation needs at least one filename";
      end if;

      return Unbounded.To_String (This.File);
   end Get_File;

   ---
   --  raise an error if an command option has already been set
   --
   procedure Check_Command_None (This : Object) is
   begin
      if This.Operation /= None then
         raise GetOpt.Option_Parse_Error with "Only one option of type „commands“ can be given";
      end if;
   end Check_Command_None;

   ---
   --  raise an error if no command option has already been set
   --
   procedure Check_Command_Some (This : Object) is
   begin
      if This.Operation = None then
         raise GetOpt.Option_Parse_Error with "One option of type „commands“ must be set first.";
      end if;
   end Check_Command_Some;

   procedure Option_Density (
      This : in out Object;
      Argument : String)
   is
   begin
      This.Check_Command_Some;

      if Argument = Density_SD then
         This.Density := Single;
      elsif Argument = Density_ED then
         This.Density := Enhanced;
      elsif Argument = Density_DD then
         This.Density := Double;
      else
         raise GetOpt.Option_Parse_Error with "Only «SD», «ED» and «DD» are allowed density option.";
      end if;
   end Option_Density;

   procedure Option_Side (
      This : in out Object;
      Argument : String)
   is
   begin
      This.Check_Command_Some;

      if Argument = Side_SS then
         This.Sides := 1;
      elsif Argument = Side_DS then
         This.Sides := 2;
      else
         raise GetOpt.Option_Parse_Error with "Only «SS» and «DS» are allowed side option.";
      end if;
   end Option_Side;

   procedure Option_Track (
      This : in out Object;
      Argument : String)
   is
   begin
      This.Check_Command_Some;

      if Argument = Track_40 then
         This.Tracks := 40;
      elsif Argument = Track_80 then
         This.Tracks := 80;
      else
         raise GetOpt.Option_Parse_Error with "Only «40» and «80» are allowed track option.";
      end if;
   end Option_Track;

   ---
   --  A File was found on the commandline
   --
   overriding procedure Analyze_File (This : in out Object) is
      Trace : AdaCL.Trace.Object := AdaCL.Trace.Function_Trace;
      pragma Unreferenced (Trace);

      This_C   : Object'Class     renames Object'Class (This);
      This_S   : Inherited.Object renames Inherited.Object (This);
      Argument : constant String := This_C.Get_Argument;
   begin
      This_S.Analyze_File;

      This.File := Unbounded.To_Unbounded_String (Argument);
   end Analyze_File;

   ---
   --  Analyze GNU style options.
   --
   overriding procedure Analyze_GNU (This : in out Object) is
      Trace : AdaCL.Trace.Object := AdaCL.Trace.Function_Trace;
      pragma Unreferenced (Trace);

      This_C         : Object'Class renames Object'Class (This);
      This_S         : Inherited.Object renames Inherited.Object (This);
      Option         : constant String := This_C.Get_GNUOption;
      Argument       : constant String := This_C.Get_Argument;
      Upper_Argument : constant String := Ada.Characters.Handling.To_Upper (Argument);
   begin
      if Option = Print_Header_Long then
         This.Check_Command_None;
         This.Operation := Print_Header;
      elsif Option = Create_Floppy_Long then
         This.Check_Command_None;
         This.Operation := Create_Floppy;
         This.Density   := Single;
         This.Tracks    := 40;
         This.Sides     := 1;
      elsif Option = Density_Long then
         This.Option_Density (Upper_Argument);
      elsif Option = Side_Long then
         This.Option_Side (Upper_Argument);
      elsif Option = Track_Long then
         This.Option_Track (Upper_Argument);
      else
         This_S.Analyze_GNU;
      end if;
   end Analyze_GNU;

   ---
   --  A Classic Style Option without Argument was found on the commandline
   --
   --  This_C : the object itself
   --  This_S : parent object
   overriding procedure Analyze_WithArgument (This : in out Object) is
      Trace : AdaCL.Trace.Object := AdaCL.Trace.Function_Trace;
      pragma Unreferenced (Trace);

      This_C         : Object'Class     renames Object'Class (This);
      This_S         : Inherited.Object renames Inherited.Object (This);
      Option         : constant Character := This_C.Get_Option;
      Argument       : constant String := This_C.Get_Argument;
      Upper_Argument : constant String := Ada.Characters.Handling.To_Upper (Argument);
   begin
      if Option = Density_Short then
         This.Option_Density (Upper_Argument);
      elsif Option = Side_Short then
         This.Option_Side (Upper_Argument);
      elsif Option = Track_Short then
         This.Option_Track (Upper_Argument);
      else
         This_S.Analyze_WithoutArgument;
      end if;
   end Analyze_WithArgument;

   ---
   --  A Classic Style Option without Argument was found on the commandline
   --
   --  This_C : the object itself
   --  This_S : parent object
   overriding procedure Analyze_WithoutArgument (This : in out Object) is
      Trace : AdaCL.Trace.Object := AdaCL.Trace.Function_Trace;
      pragma Unreferenced (Trace);

      This_C : Object'Class     renames Object'Class (This);
      This_S : Inherited.Object renames Inherited.Object (This);
      Option : constant Character := This_C.Get_Option;
   begin
      if Option = Print_Header_Short then
         This.Check_Command_None;
         This.Operation := Print_Header;
      elsif Option = Create_Floppy_Short then
         This.Check_Command_None;
         This.Operation := Create_Floppy;
         This.Density   := Single;
         This.Tracks    := 40;
         This.Sides     := 1;
      else
         This_S.Analyze_WithoutArgument;
      end if;
   end Analyze_WithoutArgument;

   ---
   --  This_C : the object itself
   --  This_S : parent object
   overriding procedure Parse (This : in out Object) is
      Trace : AdaCL.Trace.Object := AdaCL.Trace.Function_Trace;
      pragma Unreferenced (Trace);

      This_C : Object'Class     renames Object'Class (This);
      This_S : Inherited.Object renames Inherited.Object (This);
   begin
      This_C.Set_Pattern (Pattern);
      This_C.Set_ExceptionOnError (True);
      This_C.Set_ExtractGNU (True);
      This_S.Parse;

      AdaCL.Trace.Write ("Parsed: " & This'Image);
   end Parse;

   ---
   --  Write help text
   --
   --  This_C : the object itself
   --  This_S : parent object
   overriding procedure WriteHelp (This : in out Object) is
      use Ada.Text_IO;
      use GetOpt;

      Trace : AdaCL.Trace.Object := AdaCL.Trace.Function_Trace;
      pragma Unreferenced (Trace);

      This_S : Inherited.Object renames Inherited.Object (This);
   begin
      New_Line;
      Put_Line ("atr_tools-main:");
      New_Line;
      Put_Line ("    Manage atr files");
      New_Line;
      Put_Line ("Usage:");
      New_Line;
      Put_Line ("    atr_tools-main [command] [options] {atrFile}");
      New_Line;
      Put_Line ("    atrFile  ATR file to operate");
      New_Line;
      Put_Line ("Commands:");
      New_Line;
      Put_Help_Line (Print_Header_Short,  Print_Header_Long,               "print header of ATR file.");
      Put_Help_Line (Create_Floppy_Short, Create_Floppy_Long,              "create floppy drive.");
      New_Line;
      Put_Line ("Options for " & Create_Floppy_Long & ":");
      New_Line;
      Put_Help_Line (Density_Short,       Density_Long,     Density_SD,    "create single density floppy");
      Put_Help_Line (Density_Short,       Density_Long,     Density_ED,    "create enhanced density floppy");
      Put_Help_Line (Density_Short,       Density_Long,     Density_DD,    "create double density floppy");
      Put_Help_Line (Side_Short,          Side_Long,        Side_SS,       "create single side floppy");
      Put_Help_Line (Side_Short,          Side_Long,        Side_DS,       "create double side floppy");
      Put_Help_Line (Track_Short,         Track_Long,       Track_40,      "create 40 track floppy");
      Put_Help_Line (Track_Short,         Track_Long,       Track_80,      "create 80 track floppy");
      New_Line;
      Put_Line ("Other options:");
      New_Line;

      This_S.WriteHelp;
   end WriteHelp;

end Atr_Tools.CommandLine;
