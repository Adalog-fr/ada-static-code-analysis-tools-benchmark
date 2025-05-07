---------------------------------------------------------------- {{{1 ----------
--                  Copyright © 1998 Nasser Abbasi                          --
--                  Copyright © 2003 Martin Krischik                        --
------------------------------------------------------------------------------
-- This is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. GETOPT is distributed in the hope that it will be useful, but WITH --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details. Free Software Foundation,  59 Temple Place - Suite    --
-- 330,  Boston, MA 02111-1307, USA.                                        --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
------------------------------------------------------------------------------
-- change history:                                                          --
--                                                                          --
-- name         changes                                                     --
-- ----------   --------------------------------------------------------------
-- NMA021899    created                                                     --
-- NMA030299    Made it modified GPL. chanegd header.                       --
--                                                                          --
-- description:                                                             --
--                                                                          --
-- This package is an Ada implementation of getopt() as specified by the    --
-- document "The Single UNIX Specification, Version 2", Copyright 1997 The  --
-- Open Group                                                               --
--                                                                          --
-- Compiler used: GNAT 3.11p                                                --
-- Platform:      Linux 2.0.36 ( Red hat 5.2)                               --
--------------------------------------------------------------- }}}1 ----------

pragma License (Modified_Gpl);
pragma Ada_2022;

with Ada.Characters.Latin_1;
with Ada.Strings.Unbounded;

---
--  Ada Class Library
--  Analyze commmandline
--
package AdaCL.Command_Line.GetOpt is

   ---
   --  An opject oriented version of getopt made for Ada - thats without the C
   --  style uglines. If you are looking for a 100% compatible Version of
   --  getopt see:
   --
   --  Also, unlike for exapmle GNAT.Command_Line this package is reentrant.
   --  All internal states are kept inside the class instanz so two tasks can
   --  parse the commandline in parallel.
   --
   --  last not least we support GNU style commandline options.
   --
   type Object is tagged private;

   --  Return values. The C version had only two states: -1 error, > 0 option
   --  character.
   --
   type FoundFlag is (
      EndOfOptions,
      NoOption,
      GNU_Style,
      WithArgument,
      WithoutArgument,
      Error);

   ---
   --  GNU Option to request Help
   --
   Help_GNU : constant String;

   ---
   --  Short Option to request Help
   --
   Help_Short : constant Character;

   ---
   --  Flags an Error, unkown options are errors
   --
   Option_Error : Character renames Ada.Characters.Latin_1.Question;

   ---
   --  Options with Arguments.
   --
   Option_Argument : Character renames Ada.Characters.Latin_1.Colon;

   ---
   --  Character with which all options start
   --
   Option_Marker : Character renames Ada.Characters.Latin_1.Hyphen;

   ---
   --  Option could not be parsed
   --
   Option_Parse_Error : exception;

   ---
   --  Option was not given
   --
   Option_Missing_Error : exception;

   ---
   --  Wrong combination of Options was not given
   --
   Option_Wrong_Error : exception;

   ---
   --  get next Option.
   --
   --  This  : Object itself.
   --  Found : Result of the Next command
   procedure Next (
      This  : in out Object;
      Found : out FoundFlag);

   ---
   --  Start Parsing the commandline.
   --
   --  This : the Object itself
   procedure Parse (This : in out Object);

   ---
   --  A Classic Style Option without Argument was found on the commandline
   --
   --  This : the Object itself
   procedure Analyze_WithoutArgument (This : in out Object);

   ---
   --  A Classic Style Option with Argument was found on the commandline
   --
   --  This : the Object itself
   procedure Analyze_WithArgument (This : in out Object);

   ---
   --  A GNU Style was found on the commandline
   --
   --  This : the Object itself
   procedure Analyze_GNU (This : in out Object);

   ---
   --  A File was found on the commandline
   --
   --  This : the Object itself
   procedure Analyze_File (This : in out Object);

   ---
   --  Print a help line
   --
   --  Short       : Short option
   --  Long        : Long GNU style option
   --  Description : descrition of option
   --
   procedure Put_Help_Line (
      Long        : String;
      Description : String)
   with
      Pre => (Long'Length < 20);

   ---
   --  Print a help line
   --
   --  Short       : Short option
   --  Long        : Long GNU style option
   --  Description : descrition of option
   --
   procedure Put_Help_Line (
      Short       : Character;
      Long        : String;
      Description : String)
   with
      Pre => (Long'Length < 20);

   ---
   --  Print a help line
   --
   --  Short       : Short option
   --  Long        : Long GNU style option
   --  Option      : parameter for option
   --  Description : descrition of option
   --
   procedure Put_Help_Line (
      Long        : String;
      Option      : String;
      Description : String)
   with
      Pre => (Option'Length + 1 < 10) and then (Long'Length + Option'Length + 1 < 20);

   ---
   --  Print a help line
   --
   --  Short       : Short option
   --  Long        : Long GNU style option
   --  Option      : parameter for option
   --  Description : descrition of option
   --
   procedure Put_Help_Line (
      Short       : Character;
      Long        : String;
      Option      : String;
      Description : String)
   with
      Pre => (Option'Length + 1 < 10) and then (Long'Length + Option'Length + 1 < 20);

   ---
   --  A Call for Help was found on the commandline
   --
   --  This : the Object itself
   procedure WriteHelp (This : in out Object);

   ---
   --  Nr of Last Option processed
   --
   --  This : Object itself.
   function Get_Optind (This : in Object) return Positive;
   pragma Inline (Get_Optind);

   ---
   --  Last Argument Option Found.
   --
   --  This : Object itself.
   function Get_Argument (This : in Object) return String;
   pragma Inline (Get_Argument);

   ---
   --  Last Single Character Option Found.
   --
   --  This : Object itself.
   function Get_Option (This : in Object) return Character;
   pragma Inline (Get_Option);

   ---
   --  Last GNU-Option Found.
   --
   --  This : Object itself.
   function Get_GNUOption (This : in Object) return String;
   pragma Inline (Get_GNUOption);

   ---
   --  Get Format string. The usual mix of options and ':'
   --
   --  This : Object itself.
   function Get_Pattern (This : in Object) return String;
   pragma Inline (Get_Pattern);

   ---
   --  Set Format string. The usual mix of options and ':'
   --
   --  This     : Object itself.
   --  Pattern  : When true, gnuoptions are extracted.
   procedure Set_Pattern (
      This     : in out Object;
      Pattern  : in String);
   pragma Inline (Set_Pattern);

   ---
   --  Set Errorhandling on or off
   --
   --  This             : Object itself.
   --  ExceptionOnError : when true raise expetion on error else return option ':' or '?'
   procedure Set_ExceptionOnError (
      This             : in out Object;
      ExceptionOnError : in Boolean := True);
   pragma Inline (Set_ExceptionOnError);

   ---
   --  Get GNU Option extraction flag. GNU-Options start with "--" and have
   --  the Format --option=argument.
   --
   --  This : Object itself.
   function Get_ExtractGNU (This : in Object) return Boolean;
   pragma Inline (Get_ExtractGNU);

   ---
   --  Set GNU Option extraction flag. GNU-Options start with "--" and have
   --  the Format --option=argument.
   --
   --  This       : Object itself.
   --  ExtractGNU : When true, gnuoptions are extracted.
   procedure Set_ExtractGNU (
      This       : in out Object;
      ExtractGNU : in Boolean := True);
   pragma Inline (Set_ExtractGNU);

private
   package Unbounded renames Ada.Strings.Unbounded;

   ---
   --  GNU Option to request Help
   --
   Help_GNU : constant String := "help";

   ---
   --  Short Option to request Help
   --
   Help_Short : constant Character := Ada.Characters.Latin_1.Question;

   ---
   --  An opject oriented version of getopt made for Ada - thats without the C
   --  style uglines. If you are looking for a 100% compatible Version of
   --  getopt see:
   --
   --  Also, unlike for exapmle GNAT.Command_Line this package is reentrant.
   --  All internal states are kept inside the class instanz so two tasks can
   --  parse the commandline in parallel.
   --
   --  last not least we support GNU style commandline options.
   --
   --  ExceptionOnError  : Errorhandling on or off
   --  Pattern           : Set Format string. The usual mix of options and ':'
   --  Argument          : Last Argument Option Found.
   --  GNUOption         : Last GNU-Option Found.
   --  Option            : Last Single Character Option Found.
   --  ExtractGNU        : GNU Option extraction flag. GNU-Options start with
   type Object is tagged record
      Curopt            : Natural                     := 2;
      Optind            : Positive                    := 1;
      ExceptionOnError  : Boolean                     := True;
      Pattern           : Unbounded.Unbounded_String  := Unbounded.Null_Unbounded_String;
      Argument          : Unbounded.Unbounded_String  := Unbounded.Null_Unbounded_String;
      GNUOption         : Unbounded.Unbounded_String  := Unbounded.Null_Unbounded_String;
      Option            : Character                   := Option_Error;
      ExtractGNU        : Boolean                     := False;
   end record;

end AdaCL.Command_Line.GetOpt;

---------------------------------------------------------------- {{{ ----------
--  vim: set textwidth=0 nowrap tabstop=8 shiftwidth=3 softtabstop=3 expandtab :
--  vim: set filetype=ada fileencoding=utf-8 fileformat=unix foldmethod=expr :
--  vim: set spell spelllang=en_gb
