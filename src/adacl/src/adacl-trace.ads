--------------------------------------------------------------- {{{1 ----------
--  Description: Trace and Logging Utilities
--    Copyright: Copyright © 2007 … 2023 Martin Krischik
--      Licence: GNU General Public License
--   Maintainer: Martin Krischik «krischik@users.sourceforge.net»
--      Version: 5.11.0
-----------------------------------------------------------------------------
--  Copyright © 2007 … 2023 Martin Krischik «krischik@users.sourceforge.net»
--
--  Ada_Demo is free software: you can redistribute it and/or modify it under
--  the terms of the GNU General Public License as published by the Free
--  Software Foundation, either version 3 of the License, or (at your option)
--  any later version.
--
--  Ada_Demo is distributed in the hope that it will be useful, but WITHOUT
--  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
--  more details.
--
--  You should have received a copy of the GNU General Public License along
--  with Ada_Demo. If not, see <http://www.gnu.org/licenses/>.
--------------------------------------------------------------- }}}1 ----------

pragma License (Modified_Gpl);
pragma Ada_2022;

with Ada.Assertions;
with Ada.Exceptions;
with Ada.Strings.Unbounded;
with AdaCL.Base;
with GNAT.Source_Info;
with System.Storage_Elements;
with System;

package AdaCL.Trace is

   ---
   --  Name_Length : Lenght of trace String
   --
   type Object (Name_Length : Positive) is new Base.Object with private;

   ---
   --  Trace Destination
   --
   type Destination is (Queue, Standard_Error, Standard_Output, File);

   ---
   --  Functrace is not quite as usefull as the C++ version. The reason are
   --  the missing constructors and destructors in Ada. With Controlled types
   --  you can't limit to just one call to Initialize and one to Finalize
   --  There are allways some extra Adjust with matching. Finalize.
   --
   --  Name of the function calls to be traced.
   --
   function Function_Trace (Name : in String) return Object;
   pragma Inline (Function_Trace);

   ---
   --  Functrace is not quite as usefull as the C++ version. The reason are
   --  the missing constructors and destructors in Ada. With Controlled types
   --  you can't limit to just one call to Initialize and one to Finalize
   --  There are allways some extra Adjust with matching. Finalize.
   --
   --  Name of the function calls to be traced.
   --
   function Function_Trace (
      Entity : in String := GNAT.Source_Info.Enclosing_Entity;
      Source : in String := GNAT.Source_Info.Source_Location)
      return Object
   is (Function_Trace (Name => Entity & ':' & Source));

   ---
   --  Functrace is not quite as usefull as the C++ version. The reason are
   --  the missing constructors and destructors in Ada. With Controlled types
   --  you can't limit to just one call to Initialize and one to Finalize
   --  There are allways some extra Adjust with matching. Finalize.
   --
   --  Name of the function calls to be traced.
   --
   --  function Function_Trace (
   --   Entity      : in String;
   --   Parameter   : in Parameter_Vector.Vector)
   --   return Object
   --  is (Function_Trace (Entity & ':' & Parameter.Image));

   ---
   --  Trace the given exeption details and then raise the exception.
   --
   --  Raising : Exeption which is raised Message : Free form Message
   --  Message : Message to print to trace
   --  Entity  : Location destriptor.
   --  Source  : Location destriptor.
   --
   procedure Raise_Exception (
      Raising : in Ada.Exceptions.Exception_Id := Ada.Assertions.Assertion_Error'Identity;
      Message : in String                      := "No Message given";
      Entity  : in String                      := GNAT.Source_Info.Enclosing_Entity;
      Source  : in String                      := GNAT.Source_Info.Source_Location);
   pragma No_Return (Raise_Exception);

   ---
   --  Trace the given exeption details and then raise the exception.
   --
   --  Raising : Exeption which is raised Message : Free form Message
   --  Message : Message to print to trace
   --  Source  : Filename.
   --  Line    : Line number.
   --
   procedure Raise_Exception (
      Raising : in Ada.Exceptions.Exception_Id := Ada.Assertions.Assertion_Error'Identity;
      Message : in String                      := "No Message given";
      Source  : in String                      := GNAT.Source_Info.File;
      Line    : in Natural                     := GNAT.Source_Info.Line);

   ---
   --  Report an assert condition. If the condition is not true create a trace entry
   --  describing the assertion and then raise an exception.
   --
   --  Condition : Condition which should be true
   --  Raising   : Exeption which is raised
   --  Message   : Free form Message
   --  Entity    : Location destriptor.
   --  Source    : Location destriptor.
   procedure Report_Assertion (
      Condition : in Boolean;
      Raising   : in Ada.Exceptions.Exception_Id := Ada.Assertions.Assertion_Error'Identity;
      Message   : in String                      := "No Message given.";
      Entity    : in String                      := GNAT.Source_Info.Enclosing_Entity;
      Source    : in String                      := GNAT.Source_Info.Source_Location);
   pragma Inline (Report_Assertion);

   ---
   --  Report an assert condition. If the condition is not true create a trace entry
   --  describing the assertion and then raise an exception.
   --
   --  This version used parameter which are compatible with AUnit
   --
   --  Condition : Condition which should be true
   --  Message   : Free form Message
   --  Entity    : Location destriptor.
   --  Source    : Location destriptor.
   procedure Report_Assertion (
      Condition : Boolean;
      Message   : String;
      Source    : String  := GNAT.Source_Info.File;
      Line      : Natural := GNAT.Source_Info.Line);

   ---
   --  Write Line numbers
   --
   procedure Enable_Write_Line_Number;
   pragma Inline (Enable_Write_Line_Number);

   ---
   --  Don't Write Line numbers
   --
   procedure Disable_Write_Line_Number;
   pragma Inline (Disable_Write_Line_Number);

   ---
   --  check if Line numbers are written
   --
   function Is_Write_Line_Number_Enabled return Boolean;
   pragma Inline (Is_Write_Line_Number_Enabled);

   ---
   --  Enable Trace
   --
   procedure Enable_Trace;
   pragma Inline (Enable_Trace);

   ---
   --  Enable Trace
   --
   procedure Disable_Trace;
   pragma Inline (Disable_Trace);

   ---
   --  check is trace is Enabled
   --
   function Is_Trace_Enabled return Boolean;
   pragma Inline (Is_Trace_Enabled);

   ---
   --  Enable Verbose Output
   --
   procedure Enable_Verbose;
   pragma Inline (Enable_Verbose);

   ---
   --  Disable Verbose Output
   --
   procedure Disable_Verbose;
   pragma Inline (Disable_Verbose);

   ---
   --  check is trace is Enabled
   --
   function Is_Verbose_Enabled return Boolean;
   pragma Inline (Is_Verbose_Enabled);

   ---
   --  Write to queue - not supported yet.
   --
   procedure Write_To_Queue;
   pragma Inline (Write_To_Queue);

   ---
   --  Write to Standart Error
   --
   procedure Write_To_Standard_Error;
   pragma Inline (Write_To_Standard_Error);

   ---
   --  Write to Standart Error
   --
   procedure Write_To_Standard_Output;
   pragma Inline (Write_To_Standard_Output);

   ---
   --  Write to queue - not supported yet.
   --
   procedure Write_To_File;
   pragma Inline (Write_To_File);

   ---
   --  Set Filename for Trace File
   --
   procedure Write_To_File (New_Filename : in String);

   ---
   --  Check the Trace Destination
   --
   function Trace_Destination return Destination;
   pragma Inline (Trace_Destination);

   ---
   --  Enable the write prefix
   --
   procedure Enable_Write_Prefix;
   pragma Inline (Enable_Write_Prefix);

   ---
   --  Disable_ the write prefix
   --
   procedure Disable_Write_Prefix;
   pragma Inline (Disable_Write_Prefix);

   ---
   --  Check the write prefix flag
   --
   function Is_Write_Prefix_Enabled return Boolean;
   pragma Inline (Is_Write_Prefix_Enabled);

   ---
   --  Write an IString using writeFormattedString after adding the
   --  appropriate padding for indentation.
   --
   --  A_String : String to be written
   --
   procedure Write (A_String : in String);

   ---
   --  Write an Address.
   --
   --  A_String : String to be written
   --
   procedure Write (A_String : in String; An_Address : in System.Address);

   ---
   --  Write an IString using writeFormattedString after adding the
   --  appropriate padding for indentation.
   --
   --  A_Unbounded : String to be written
   --
   procedure Write (A_Unbounded : in Ada.Strings.Unbounded.Unbounded_String);

   ---
   --  Write an Exception to the Trace
   --
   --  An_Exception : String to be written
   --
   procedure Write (An_Exception : in Ada.Exceptions.Exception_Occurrence);

   ---
   --  Write an Exception to the Trace
   --
   --  SAn_Exception : tring to be written
   --  PAn_Entity    : rocedure in which the exception was caught
   --  SA_Source     : ource File in which Entity is located.
   --
   procedure Write (
      An_Exception : in Ada.Exceptions.Exception_Occurrence;
      An_Entity    : in String;
      A_Source     : in String);

   ---
   --  Write an IString using writeFormattedString after adding the
   --  appropriate padding for indentation.
   --
   --  A_String : String to be written
   --
   procedure Write_Wide (A_String : in Wide_String);

   ---
   --  Create a memory dump, S
   --
   --  An_Address :  String to be written
   --  A_Size     :  Size in Storage_Elements.
   --
   procedure Write_Dump (
      An_Address : in System.Address;
      A_Size     : in System.Storage_Elements.Storage_Count);

   ---
   --  Create a memory dump. This Dump takes size in bits.
   --
   --  An_Address : String to be written
   --  A_Size     : Size in Bits - i.E. for 'Size.
   --
   procedure Write_Dump (
      An_Address : in System.Address;
      A_Size     : in Integer);

   ---
   --  Write an IString using writeFormattedString after adding the
   --  appropriate padding for indentation.
   --
   --  A_String : String to be written
   --
   procedure Write_Error (A_String : in String);

   ---
   --  Write an IString using writeFormattedString after adding the
   --  appropriate padding for indentation.
   --
   --  A_Unbounded : String to be written
   --
   procedure Write_Error (A_Unbounded : in Ada.Strings.Unbounded.Unbounded_String);

   ---
   --  Write an Exception to the Trace
   --
   --  An_Exception : String to be written
   --
   procedure Write_Error (An_Exception : in Ada.Exceptions.Exception_Occurrence);

   ---
   --  Write an Exception to the Trace
   --
   --  An_Exception :  String to be written
   --  An_Entity    :  Procedure in which the exception was caught
   --  A_Source     :  Source File in which Entity is located.
   --
   procedure Write_Error (
      An_Exception : in Ada.Exceptions.Exception_Occurrence;
      Entity       : in String;
      Source       : in String);

   ---
   --  When verbose is aktivated then an empty line is written to
   --  Standart_Output
   --
   procedure Write_Info;

   ---
   --  Write an IString using writeFormattedString after adding the
   --  appropriate padding for indentation.
   --
   --  When verbose is aktivated then the string is written to Standart_Output
   --  as well.
   --
   --  A_String : String to be written
   --
   procedure Write_Info (A_String : in String);

   ---
   --  When verbose is aktivated then the character is written to
   --  Standart_Output.
   --
   --  A_Character : String to be written
   --
   procedure Write_Info (A_Character : in Character);

   ---
   --  Write an IString using writeFormattedString after adding the
   --  appropriate padding for indentation.
   --
   --  When verbose is aktivated then the string is written to Standart_Output
   --  as well.
   --
   --  A_Unbounded : String to be written
   --
   procedure Write_Info (A_Unbounded : in Ada.Strings.Unbounded.Unbounded_String);

   ---
   --  Write Help for Commandline Options parsed from Trace
   --
   procedure Write_Commandline_Help;

private

   package Inherited renames AdaCL.Base;

   ---
   --  Ada Class Library
   --  Trace
   --
   --  Instanz Data
   --
   --  Name_Length : Lenght of trace String
   --
   type Object (Name_Length : Positive) is new
      Inherited.Object
   with record
      Trace_Name : String (1 .. Name_Length);
   end record;

   ---
   --  Trace Copy.
   --
   --  This : Object itself.
   --
   overriding procedure Adjust (This : in out Object);

   ---
   --  Trace end of function
   --
   --  This : Object itself.
   --
   overriding procedure Finalize (This : in out Object);

end AdaCL.Trace;

---------------------------------------------------------------- {{{ ----------
--  vim: set textwidth=0 nowrap tabstop=8 shiftwidth=3 softtabstop=3 expandtab :
--  vim: set filetype=ada fileencoding=utf-8 fileformat=unix foldmethod=expr :
--  vim: set spell spelllang=en_gb
