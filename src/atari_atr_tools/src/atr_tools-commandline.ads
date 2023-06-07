------------------------------------------------------------------------------
--  Copyright © 2020 … 2023 Martin Krischik «krischik@users.sourceforge.net»
----------------------------------------------------------------------------
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
----------------------------------------------------------------------------

pragma License (Modified_Gpl);
pragma Ada_2022;

with AdaCL.Command_Line.GetOpt;
with Ada.Strings.Unbounded;
with Atr_Tools.File_Body;

---
--  Parse commandline of ATR tools.
--
package Atr_Tools.CommandLine is

   ---
   --  Operation to perform
   --
   --  None          : No operation was passed on the
   --  Print_Header  : Print operation was
   --  Create_Floppy : Create a floppy. Default is SS, SD, 40 tracks.
   --
   type Operation_Type is (None, Print_Header, Create_Floppy);

   ---
   --  Floppy and hard drive density.
   --
   --  Single   : 18 × 128 bytes per track
   --  Enhanced : 26 × 128 bytes per track
   --  Double   : 18 × 256 bytes per track
   --  High     : 512 bytes per sector
   --
   type Density_Type is (Single, Enhanced, Double, High);

   ---
   --  Floppy disk sides. Ignored for High density drives.
   --
   type Track_Type is range 0 .. 80;

   ---
   --  Floppy disk sides. Ignored for High density drives.
   --
   type Side_Type is range 0 .. 2;

   --
   --  Specialized Version of GetOpt for use with Text Search and Replace
   --  tools
   --
   --  Operation  : the operation that should be performed.
   --  File       : the file to operate on.
   --  Density    : Floppy and hard drive density.
   --  Tracks     : Floppy disk tracks. Can be 40 or 80.
   --
   type Object is new AdaCL.Command_Line.GetOpt.Object with record
      Operation   : Operation_Type                          := None;
      File        : Ada.Strings.Unbounded.Unbounded_String  := Ada.Strings.Unbounded.Null_Unbounded_String;
      Density     : Density_Type                            := Single;
      Tracks      : Track_Type                              := 0;
      Sides       : Side_Type                               := 0;
      Sectors     : Integer                                 := 0;
   end record;

   ---
   --  Operation to perform
   --
   function Get_File (This : Object) return String
   with
      Post => Get_File'Result'Length > 0;

   function Get_Density (This : Object) return Integer
   is (
      case This.Density is
         when Single | Enhanced  => File_Body.Single_Density,
         when Double             => File_Body.Double_Density,
         when High               => File_Body.High_Density)
   with
      Post => Get_Density'Result in File_Body.Single_Density | File_Body.Double_Density | File_Body.High_Density;

   ---
   --  Sector_Per_Track
   --
   function Get_Sectors (This : Object) return Integer
   is (
      case This.Density is
         when Single | Double  => File_Body.Atari_Standard,
         when Enhanced         => File_Body.Atari_Enhanced,
         when High             => This.Sectors)
   with
      Post =>
         (This.Density /= High and then
            Get_Sectors'Result in File_Body.Atari_Standard | File_Body.Atari_Enhanced) or else
         (This.Density = High and then Get_Sectors'Result = This.Sectors);

   ---
   --  Tracks per side
   --
   function Get_Tracks (This : Object) return Integer
   is (Integer (This.Tracks))
   with
      Pre  => This.Density /= High,
      Post => Get_Tracks'Result in File_Body.Standard_Tracks | File_Body.Double_Tracks;

   ---
   --  Sides per Disk
   --
   function Get_Sides (This : Object) return Integer
   is (Integer (This.Sides))
   with
      Pre  => This.Density /= High,
      Post => Get_Sides'Result in 1 | 2;

   --
   --  Start Parsing the commandline.
   --
   --  This: the Object itself
   overriding procedure Parse (This : in out Object)
   with
      Post => This.Tracks = 0 or else This.Tracks = 40 or else This.Tracks = 80;

   --
   --  A Call for Help was found on the commandline
   --
   --  This: the Object itself
   overriding procedure WriteHelp (This : in out Object);

   --
   --  A Classic Style Option without Argument was found on the commandline
   --
   --  This: the Object itself
   overriding procedure Analyze_WithoutArgument (This : in out Object);

   --
   --  A Classic Style Option with Argument was found on the commandline
   --
   --  This: the Object itself
   overriding procedure Analyze_WithArgument (This : in out Object);

   --
   --  A GNU Style was found on the commandline
   --
   --  This: the Object itself
   overriding procedure Analyze_GNU (This : in out Object);

   --
   --  A File was found on the commandline
   --
   --  This: the Object itself
   overriding procedure Analyze_File (This : in out Object);

   --
   --  get next Option.
   --
   --  This: Object itself.
   --  A next element was found
   procedure Next (
      This  : in out AdaCL.Command_Line.GetOpt.Object;
      Found : out AdaCL.Command_Line.GetOpt.FoundFlag)
   renames AdaCL.Command_Line.GetOpt.Next;

   --
   --  Nr of Last Option processed
   --
   --  This: Object itself.
   function Get_Optind (
      This : in AdaCL.Command_Line.GetOpt.Object)
      return Positive
   renames AdaCL.Command_Line.GetOpt.Get_Optind;

   --
   --  Last Argument Option Found.
   --
   --  This: Object itself.
   function Get_Argument (
      This : in AdaCL.Command_Line.GetOpt.Object)
      return String
   renames AdaCL.Command_Line.GetOpt.Get_Argument;

   --
   --  Last Single Character Option Found.
   --
   --  This: Object itself.
   function Get_Option (
      This       : in AdaCL.Command_Line.GetOpt.Object)
      return Character
   renames AdaCL.Command_Line.GetOpt.Get_Option;

   --
   --  Last GNU-Option Found.
   --
   --  This: Object itself.
   function Get_GNUOption (
      This : in AdaCL.Command_Line.GetOpt.Object)
      return String
   renames AdaCL.Command_Line.GetOpt.Get_GNUOption;

   --
   --  Get Format string. The usual mix of options and ':'
   --
   --  Object itself.
   function Get_Pattern (
      This : in AdaCL.Command_Line.GetOpt.Object)
      return String
   renames AdaCL.Command_Line.GetOpt.Get_Pattern;

   --
   --  Set Format sting. The usual mix of options and ':'
   --
   --  Object itself.
   --  When true, gnuoptions are extracted.
   procedure Set_Pattern (
      This    : in out AdaCL.Command_Line.GetOpt.Object;
      Pattern : String)
   renames AdaCL.Command_Line.GetOpt.Set_Pattern;

   --
   --  Set Errorhandling on or off
   --
   --  Object itself.
   --  when true raise expetion on error else return option ':' or '?'
   procedure Set_ExceptionOnError (
      This : in out AdaCL.Command_Line.GetOpt.Object;
      ExceptionOnError : Boolean := True)
   renames AdaCL.Command_Line.GetOpt.Set_ExceptionOnError;

   --
   --  Get GNU Option extraction flag. GNU-Options start with "--" and have
   --  the Format --option=argument.
   --
   --  Object itself.
   function Get_ExtractGNU (
      This : in AdaCL.Command_Line.GetOpt.Object)
      return Boolean
   renames AdaCL.Command_Line.GetOpt.Get_ExtractGNU;

   --
   --  Set GNU Option extraction flag. GNU-Options start with "--" and have
   --  the Format --option=argument.
   --
   --  Object itself.
   --  When true, gnuoptions are extracted.
   procedure Set_ExtractGNU (
      This       : in out AdaCL.Command_Line.GetOpt.Object;
      ExtractGNU : Boolean := True)
   renames AdaCL.Command_Line.GetOpt.Set_ExtractGNU;

private

   package Inherited renames AdaCL.Command_Line.GetOpt;

end Atr_Tools.CommandLine;
