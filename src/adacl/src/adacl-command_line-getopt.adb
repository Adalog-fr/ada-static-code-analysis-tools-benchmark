--------------------------------------------------------------- {{{1 ----------
--                  Copyright © 1998 Nasser Abbasi                          --
--                       nabbasi@pacbell.net                                --
--                  Copyright © 2003 Martin Krischik                        --
--                   krischik@users.sourceforge.net                         --
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
--                                                                          --
------------------------------------------------------------------------------
--                                                                          --
-- change history:                                                          --
--                                                                          --
-- name         changes                                                     --
-- ----------   --------------------------------------------------------------
-- NMA021899    created                                                     --
-- NMA030299    Changed header to make it modified GPL                      --
--                                                                          --
-- description:                                                             --
--                                                                          --
-- This package is an Ada implementation of getopt() as specified by the    --
-- document "The Single UNIX Specification, Version 2", Copyright 1997 The  --
-- Open Group                                                               --
--                                                                          --
-- This describes the items involveed using example                         --
--                                                                          --
--                                                                          --
--         curopt                                                           --
--           |                                                              --
--           V                                                              --
-- "-f foo -dbc -k"                                                         --
--  ^                                                                       --
--  |                                                                       --
-- optind                                                                   --
--                                                                          --
-- optind is position (index) that tells which command line argument is     --
-- being processed now.                                                     --
-- curopt tells which optchar is being processed within one command line    --
-- argument. This is needed only if more that one optchar are stuck         --
-- togother in one argument with no space, as in -df where both d and f     --
-- are valid optchar and d takes no optarg.                                 --
--                                                                          --
--                                                                          --
-- Compiler used: GNAT 3.11p                                                --
-- Platform:      Linux 2.0.36 ( Red hat 5.2)                               --
--------------------------------------------------------------- }}}1 ----------

pragma License (Modified_Gpl);
pragma Ada_2022;

with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Command_Line;
with Ada.Text_IO;
with AdaCL.Trace;
with Adacl_Config;
with GNAT.Source_Info;

--
--  Ada Class Library
--  Analyze commmandline
--
package body AdaCL.Command_Line.GetOpt is

   ---------------------------------------------------------------------------

   package Command_Line renames Ada.Command_Line;

   ---------------------------------------------------------------------------

   use type Ada.Strings.Unbounded.Unbounded_String;

   ---------------------------------------------------------------------------

   ---
   --  max lenght for text formated by the Put_Help_Line convinience methods.
   --
   Max_Lenght_Option     : constant := 10;
   Max_Lenght_GNU_Option : constant := 20;

   ---------------------------------------------------------------------------
   --
   --  Extract_NoArguments
   --
   --  Object itself.
   procedure Extract_NoArguments (This : in out Object'Class);

   --
   --  Extract GNU Options.
   --
   --  Object itself.
   procedure Extract_GNU (This : in out Object'Class);

   --
   --  Extract GNU Options.
   --
   --  Object itself.
   procedure Extract_Argument (This : in out Object'Class; Found : out FoundFlag);

   ---------------------------------------------------------------------------
   --
   --  Analyze option free parameter
   --
   --  the Object itself
   procedure Analyze_File (This : in out Object) is
      --  Trace : AdaCL.Trace.Object := AdaCL.Trace.Function_Trace;
      --  pragma Unreferenced (Trace);

      This_C   : Object'Class renames Object'Class (This);
      Argument : constant String := This_C.Get_Argument;
   begin
      AdaCL.Trace.Write ("Parameter       = " & Argument);
   end Analyze_File;

   ---------------------------------------------------------------------------
   --
   --  Analyze GNU style options.
   --
   --  the Object itself
   procedure Analyze_GNU (This : in out Object) is
      --  Trace : AdaCL.Trace.Object := AdaCL.Trace.Function_Trace;
      --  pragma Unreferenced (Trace);

      This_C : Object'Class renames Object'Class (This);

      Option   : constant String := This_C.Get_GNUOption;
      Argument : constant String := This_C.Get_Argument;
   begin
      AdaCL.Trace.Write ("Option          = " & Option);
      AdaCL.Trace.Write ("Argument        = " & Argument);

      if Option = Help_GNU then
         This_C.WriteHelp;
      end if;
   end Analyze_GNU;

   ---------------------------------------------------------------------------
   --
   --  Analyze options with parameter
   --
   --  the Object itself
   procedure Analyze_WithArgument (This : in out Object) is
      --  Trace : AdaCL.Trace.Object := AdaCL.Trace.Function_Trace;
      --  pragma Unreferenced (Trace);

      This_C : Object'Class renames Object'Class (This);

      Option   : constant Character := This_C.Get_Option;
      Argument : constant String    := This_C.Get_Argument;
   begin
      AdaCL.Trace.Write ("Option          = " & Option);
      AdaCL.Trace.Write ("Argument        = " & Argument);
   end Analyze_WithArgument;

   ---------------------------------------------------------------------------
   --
   --  Analyze options without parameter
   --
   --  the Object itself
   procedure Analyze_WithoutArgument (This : in out Object) is
      --  Trace : AdaCL.Trace.Object := AdaCL.Trace.Function_Trace;
      --  pragma Unreferenced (Trace);

      This_C : Object'Class renames Object'Class (This);

      Option : constant Character := This_C.Get_Option;
   begin
      AdaCL.Trace.Write ("Option          = " & Option);

      if Option = Help_Short then
         This_C.WriteHelp;
      end if;
   end Analyze_WithoutArgument;

   ---------------------------------------------------------------------------
   --
   --  Extract Argument
   --
   --  This  : Object itself.
   --  Found :
   --
   procedure Extract_Argument (
      This  : in out Object'Class;
      Found : out FoundFlag)
   is
      --  Trace : AdaCL.Trace.Object := AdaCL.Trace.Function_Trace;
      --  pragma Unreferenced (Trace);

      use Ada.Strings.Unbounded;

      Argument : constant String := Command_Line.Argument (This.Optind);
   begin
      if Argument'Length - This.Curopt > 0 then
         --
         --  see if optarg stuck to optchar
         --
         This.Argument := To_Unbounded_String (Argument (This.Curopt + 1 .. Argument'Length));
         This.Curopt   := This.Curopt + 1;
         This.Optind   := This.Optind + 1;
         Found         := WithArgument;
      elsif This.Optind < Command_Line.Argument_Count then
         --
         --  see if optarg on separate argument
         --
         This.Curopt   := 2;
         This.Optind   := This.Optind + 1;
         This.Argument := To_Unbounded_String (Command_Line.Argument (This.Optind));
         This.Optind   := This.Optind + 1;
         Found         := WithArgument;
      else
         ErrorHandler : declare
            First : constant Character := Element (This.Pattern, 1);
         begin
            This.Optind := This.Optind + 1;
            if First = Option_Argument then
               This.Option := Option_Argument;
               Found       := Error;
            elsif This.ExceptionOnError then
               AdaCL.Trace.Raise_Exception
                 (Raising => Option_Parse_Error'Identity,
                  Message => "Argument expected for the -" & This.Option & " option",
                  Entity  => GNAT.Source_Info.Enclosing_Entity,
                  Source  => GNAT.Source_Info.Source_Location);
            else
               This.Option := Option_Error;
               Found       := Error;
            end if;
         end ErrorHandler;
      end if;
   end Extract_Argument;

   ---------------------------------------------------------------------------
   --
   --  Extract GNU Options.
   --
   --  This : Object itself.
   --
   procedure Extract_GNU (This : in out Object'Class) is
      --  Trace : AdaCL.Trace.Object := AdaCL.Trace.Function_Trace;
      --  pragma Unreferenced (Trace);

      use Ada.Strings.Unbounded;

      Arg   : constant String  := Command_Line.Argument (This.Optind);
      Equal : constant Natural := Ada.Strings.Fixed.Index (
         Source  => Arg,
         Pattern => "=",
         Going   => Ada.Strings.Forward,
         Mapping => Ada.Strings.Maps.Identity);
   begin
      if Equal = Natural'First then
         --
         --  no options
         --
         This.Argument  := Null_Unbounded_String;
         This.GNUOption := To_Unbounded_String (Arg (3 .. Arg'Length));
      else
         --
         --  options
         --
         This.Argument  := To_Unbounded_String (Arg (Equal + 1 .. Arg'Length));
         This.GNUOption := To_Unbounded_String (Arg (3 .. Equal - 1));
      end if;
   end Extract_GNU;

   ---------------------------------------------------------------------------
   --
   --  Extract without Arguments
   --
   --  This  : This : Object itself.
   --
   procedure Extract_NoArguments (This : in out Object'Class) is
      --  Trace : AdaCL.Trace.Object := AdaCL.Trace.Function_Trace;
      --  pragma Unreferenced (Trace);

      Argument : constant String := Command_Line.Argument (This.Optind);
   begin
      if This.Curopt < Argument'Length then
         This.Curopt := This.Curopt + 1;
      else
         This.Curopt := 2;
         This.Optind := This.Optind + 1;
      end if;
   end Extract_NoArguments;

   ---------------------------------------------------------------------------
   --
   --  Last Argument Option Found.
   --
   --  This  : Object itself.
   --
   function Get_Argument (This : in Object) return String
   is (Unbounded.To_String (This.Argument));

   ---------------------------------------------------------------------------
   --
   --  Set GNU Option extraction. GNU-Options start with "--" and have the
   --  Format --option=argument.
   --
   --  This  : Object itself.
   --
   function Get_ExtractGNU (This : in Object) return Boolean
   is (This.ExtractGNU);

   ---------------------------------------------------------------------------
   --
   --  Last GNU-Option Found.
   --
   --  This  : Object itself.
   --
   function Get_GNUOption (This : in Object) return String
   is (Unbounded.To_String (This.GNUOption));

   ---------------------------------------------------------------------------
   --
   --  Last Option processed
   --
   --  This  : Object itself.
   --
   function Get_Optind (This : in Object) return Positive
   is (This.Optind);

   ---------------------------------------------------------------------------
   --
   --  Last Option Found.
   --
   --  This  : Object itself.
   --
   function Get_Option (This : in Object) return Character
   is (This.Option);

   ---------------------------------------------------------------------------
   --
   --  Get Format string
   --
   --  This  : Object itself.
   --
   function Get_Pattern (This : in Object) return String
   is (Unbounded.To_String (This.Pattern));

   ---------------------------------------------------------------------------
   --
   --  Getopt
   --
   --  This  : Object itself.
   --  Found :
   --
   procedure Next (
      This  : in out Object;
      Found : out FoundFlag)
   is
      --  Trace : AdaCL.Trace.Object := AdaCL.Trace.Function_Trace;
      --  pragma Unreferenced (Trace);

      use Ada.Strings.Unbounded;

      --
      --  For dispaching calls
      --
      This_C : Object'Class renames Object'Class (This);
   begin
      Found          := NoOption;
      This.Argument  := Null_Unbounded_String;
      This.GNUOption := Null_Unbounded_String;

      if Command_Line.Argument_Count = 0
        or else This.Optind > Command_Line.Argument_Count
      then
         --
         --  No more options left
         --
         Found       := EndOfOptions;
         This.Option := Option_Error;
      else
         Get_Next : declare
            OptStingLen : constant Positive := Length (This.Pattern);
            Argument    : constant String   := Command_Line.Argument (This.Optind);
         begin
            if Argument (1) /= Option_Marker or else Argument'Length = 1 then
               --
               --  Argument is not an option
               --
               This.Optind   := This.Optind + 1;
               This.Argument := To_Unbounded_String (Argument);
               Found         := NoOption;
            elsif Argument (2) = Option_Marker then
               --
               --  according to The Single UNIX Specification, Version 2, if
               --  "--" is found, return -1 after ++optind.
               --
               if This_C.Get_ExtractGNU then
                  --
                  --  If extract of GNU is active we extract them now
                  --
                  This.Extract_GNU;
               end if;

               Found       := GNU_Style;
               This.Option := Option_Marker;
               This.Optind := This.Optind + 1;
            else
               --
               --  if we get here, the command argument has "-X"
               --
               SearchOpt : for I in 1 .. OptStingLen loop
                  CheckOpt : declare
                     Option : constant Character := Element (This.Pattern, I);
                  begin
                     if Option = Argument (This.Curopt) then
                        if I < OptStingLen and then Element (This.Pattern, I + 1) = Option_Argument then
                           --
                           --  option with parameter
                           --
                           This.Option := Option;
                           This.Extract_Argument (Found);
                        else
                           --
                           --  current optchar matches and has no arg option
                           --  or last char in optstring, can't have argument
                           --
                           This.Option := Option;
                           Found       := WithoutArgument;
                           This.Extract_NoArguments;
                        end if;
                        exit SearchOpt;
                     end if;
                  end CheckOpt;
               end loop SearchOpt;
               if Found = NoOption then
                  This.Option := Argument (This.Curopt);
                  This.Extract_NoArguments;
                  --
                  --  we get here if current command argument not found in
                  --  optstring
                  --
                  This.Option := Option_Error;
                  Found       := Error;
               end if;
            end if;
         end Get_Next;
      end if;
      return;
   end Next;

   ---------------------------------------------------------------------------
   --
   --  This : the Object itself
   --
   procedure Parse (This : in out Object) is
      --  Trace : AdaCL.Trace.Object := AdaCL.Trace.Function_Trace;
      --  pragma Unreferenced (Trace);

      This_C : Object'Class renames Object'Class (This);

      Found : GetOpt.FoundFlag;
   begin
      LoopCL : loop
         Next (This => This_C, Found => Found);

         exit LoopCL when Found = GetOpt.EndOfOptions;

         if Found = GetOpt.WithArgument then
            This_C.Analyze_WithArgument;
         elsif Found = GetOpt.WithoutArgument then
            This_C.Analyze_WithoutArgument;
         elsif Found = GetOpt.GNU_Style then
            This_C.Analyze_GNU;
         elsif Found = GetOpt.NoOption then
            This_C.Analyze_File;
         end if;
      end loop LoopCL;
   end Parse;

   ---------------------------------------------------------------------------
   --
   --  Set Errorhandling
   --
   --  This             : Object itself.
   --  ExceptionOnError : when true raise expetion on error else return option ':' or '?'
   --
   procedure Set_ExceptionOnError (
      This             : in out Object;
      ExceptionOnError : Boolean := True)
   is
   begin
      This.ExceptionOnError := ExceptionOnError;
   end Set_ExceptionOnError;

   ---------------------------------------------------------------------------
   --
   --  Set GNU Option extraction. GNU-Options start with "--" and have the
   --  Format --option=argument.
   --
   --  This       : Object itself.
   --  ExtractGNU : When true, gnuoptions are extracted.
   --
   procedure Set_ExtractGNU (
      This       : in out Object;
      ExtractGNU : Boolean := True)
   is
   begin
      This.ExtractGNU := ExtractGNU;
   end Set_ExtractGNU;

   ---------------------------------------------------------------------------
   --
   --  Set Format sting
   --
   --  This    : Object itself.
   --  Pattern : When true, gnuoptions are extracted.
   procedure Set_Pattern (
      This    : in out Object;
      Pattern : String)
   is
   begin
      This.Pattern := Unbounded.To_Unbounded_String (Pattern);
   end Set_Pattern;

   procedure Put_Help_Line (
      Long        : String;
      Description : String)
   is
      use Ada.Strings.Fixed;
      use Ada.Text_IO;

      Padding_1 : constant String := (Max_Lenght_Option + 5 + 1) * " " & "--";
      Padding_2 : constant String := (Max_Lenght_GNU_Option - Long'Length) * " ";
   begin
      Put_Line (
         Padding_1   & Long  &
         Padding_2   & Description);
   end Put_Help_Line;

   procedure Put_Help_Line (
      Short       : Character;
      Long        : String;
      Description : String)
   is
      use Ada.Strings.Fixed;
      use Ada.Text_IO;

      Padding_1 : constant String := (Max_Lenght_Option) * " " & "--";
      Padding_2 : constant String := (Max_Lenght_GNU_Option - Long'Length) * " ";
   begin
      Put_Line (
         "    -"     & Short &
         Padding_1   & Long  &
         Padding_2   & Description);
   end Put_Help_Line;

   procedure Put_Help_Line (
      Long        : String;
      Option      : String;
      Description : String)
   is
      use Ada.Strings.Fixed;
      use Ada.Text_IO;

      Padding_1 : constant String := (Max_Lenght_Option + 5 + 1) * " " & "--";
      Padding_2 : constant String := (Max_Lenght_GNU_Option - Long'Length - 1 - Option'Length) * " ";
   begin
      Put_Line (
         Padding_1   & Long  &
         "="         & Option &
         Padding_2   & Description);
   end Put_Help_Line;

   procedure Put_Help_Line (
      Short       : Character;
      Long        : String;
      Option      : String;
      Description : String)
   is
      use Ada.Strings.Fixed;
      use Ada.Text_IO;

      Padding_1 : constant String := (Max_Lenght_Option - 1 - Option'Length) * " " & "--";
      Padding_2 : constant String := (Max_Lenght_GNU_Option - Long'Length - 1 - Option'Length) * " ";
   begin
      Put_Line (
         "    -"     & Short &
         " "         & Option &
         Padding_1   & Long  &
         "="         & Option &
         Padding_2   & Description);
   end Put_Help_Line;

   ---------------------------------------------------------------------------
   --
   --  Write help text
   --
   --  the Object itself
   procedure WriteHelp (This : in out Object) is
      --  Trace : AdaCL.Trace.Object := AdaCL.Trace.Function_Trace;
      --  pragma Unreferenced (Trace);

      use Ada.Text_IO;

      pragma Unreferenced (This);
   begin
      Put_Help_Line (Help_Short, Help_GNU, "this help");

      AdaCL.Trace.Write_Commandline_Help;

      Put_Line ("Made with AdaCL " & Adacl_Config.Crate_Version & " (https://adacl.sourceforge.net/).");
      New_Line;
   end WriteHelp;

end AdaCL.Command_Line.GetOpt;

---------------------------------------------------------------- {{{ ----------
--  vim: set textwidth=0 nowrap tabstop=8 shiftwidth=3 softtabstop=3 expandtab :
--  vim: set filetype=ada fileencoding=utf-8 fileformat=unix foldmethod=expr :
--  vim: set spell spelllang=en_gb
