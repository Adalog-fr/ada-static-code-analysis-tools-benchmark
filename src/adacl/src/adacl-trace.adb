--------------------------------------------------------------- {{{1 ----------
--  Description: Trace and Logging Utilities
--    Copyright: Copyright © 2007 … 2023 Martin Krischik
--      Licence: GNU General Public License
--   Maintainer: Martin Krischik «krischik@users.sourceforge.net»
--      Version: 5.11.0
-------------------------------------------------------------------------------
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

with Ada.Characters.Conversions;
with Ada.Characters.Handling;
with Ada.Characters.Latin_1;
with Ada.Containers.Hashed_Maps;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Task_Identification;
with Ada.Text_IO;
with AdaCL.Command_Line.GetOpt;
with AdaCL.Strings;

--
--  Ada Class Library
--  Trace
--
package body AdaCL.Trace is

   ---------------------------------------------------------------------------

   use type Ada.Strings.Unbounded.Unbounded_String;

   ---------------------------------------------------------------------------

   package Unbounded renames Ada.Strings.Unbounded;
   package Latin_1   renames Ada.Characters.Latin_1;
   package Text_IO   renames Ada.Text_IO;

   ---------------------------------------------------------------------------
   package Address_IO is new Ada.Text_IO.Modular_IO (Num => System.Storage_Elements.Integer_Address);

   ---
   --  Thread_No : Each Thread has a number. A number is shorter then string.
   --  Indent    : Function indeting is counded separate for every thread
   --
   type Thread_ID is record
      Thread_No : Natural := Natural'First;
      Indent    : Natural := Natural'First;
   end record;

   ---
   --  The controlled object used in Function_Trace is adjusted multiple
   --  times. To get a better output two adjustments and two finals are
   --  ignored.
   --
   type Repeat_Count is range 0 .. 4;

   ---
   --  Some Container for Thread_IDs
   --
   package Thread_ID_Map is new Ada.Containers.Hashed_Maps (
      Key_Type          => Unbounded.Unbounded_String,
      Element_Type      => Thread_ID,
      Hash              => AdaCL.Strings.Hash,
      Equivalent_Keys   => "=",
      "="               => "=");

   ---
   --  Protect all global data.
   --
   protected Cl is
      ---
      --  Initialize Trace.
      --
      procedure Initialize;

      ---
      --  Icrement Trace line counter by one
      --
      procedure Inc_Sequence;
      pragma Inline (Inc_Sequence);

      ---
      --  Get Trace line counter
      --
      function Get_Sequence return Natural;
      pragma Inline (Get_Sequence);

      procedure Set_Filename (New_Filename : in String);
      pragma Inline (Set_Filename);

      ---
      --  Determine the threadId of the current thread
      --
      procedure Get_Thread_ID (Retval : out Thread_ID);

      ---
      --  Determine the threadId of the current thread
      --
      procedure Set_Thread_ID (Element : in Thread_ID);

      ---
      --  Trace is On
      --
      function Get_On return Boolean;
      pragma Inline (Get_On);

      ---
      --  Trace is On
      --
      procedure Set_On (On : Boolean);
      pragma Inline (Set_On);

      ---
      --  Trace is On
      --
      function Get_Verbose return Boolean;
      pragma Inline (Get_Verbose);

      ---
      --  The controlled object used in Function_Trace is adjusted multiple
      --  times. To get a better output two adjustments are ignored.
      --
      --  reset the last function counter and function name.
      --
      procedure Reset_Last_Function (Function_Name : String);

      ---
      --  The controlled object used in Function_Trace is adjusted multiple
      --  times. To get a better output two adjustments are ignored.
      --
      --  Check if counter is greater 0
      --
      function Ignore_Function  (Function_Name : String) return Boolean;

      ---
      --  The controlled object used in Function_Trace is adjusted multiple
      --  times. To get a better output two adjustments are ignored.
      --
      --  Decrement counter until it reaches 0
      --
      procedure Decrement_Last_Function;

      ---
      --  Trace is On
      --
      procedure Set_Verbose (Verbose : Boolean);
      pragma Inline (Set_Verbose);

      ---
      --  Trace with line numbers.
      --
      function Get_Write_Line_Number return Boolean;
      pragma Inline (Get_Write_Line_Number);

      ---
      --  Trace with line numbers.
      --
      procedure Set_Write_Line_Number (Write_Line_Number : Boolean);
      pragma Inline (Set_Write_Line_Number);

      ---
      --  Trace with thread profex and optional line numbers.
      --
      function Get_Write_Prefix return Boolean;
      pragma Inline (Get_Write_Prefix);

      ---
      --  Trace with thread profex and optional line numbers.
      --
      procedure Set_Write_Prefix (Write_Prefix : Boolean);
      pragma Inline (Set_Write_Prefix);

      ---
      --  Trace Destination
      --
      function Get_Trace_Location return Destination;
      pragma Inline (Get_Trace_Location);

      ---
      --  Trace Destination
      --
      procedure Set_Trace_Location (Location : in Destination);
      pragma Inline (Set_Trace_Location);

      ---
      --  Write Formated Text
      --
      --  Text   : Text to be written
      --  Marker : Marker to be used
      procedure Write_Formatted_String (
         Text   : in String;
         Marker : in Character);

      ---
      --  Write Text
      --
      --  Text : Text to be written
      procedure Write_String (Text : in String);

   private

      ---
      --  Trace line counter
      --
      Sequence : Natural := Natural'First;

      ---
      --  Filename of Trace if Destination ist File
      --
      Filename : Unbounded.Unbounded_String := Unbounded.To_Unbounded_String ("Trace.Out");

      ---
      --  The original IBM design opened and closed the File all the time.
      --  However, Ada.Text_IO won't allow that and of course, it is slow.
      --
      Filehandle : Text_IO.File_Type;

      ---
      --  Last Thread ID used
      --
      Thread_No : Natural := Natural'First;

      ---
      --  Current Indenting Level for each thread
      --
      Threads : Thread_ID_Map.Map;

      ---
      --  Trace Destination
      --
      Location : Destination  := Standard_Error;

      ---
      --  Trace is On
      --
      On : Boolean := False;

      ---
      --  Trace with line numbers.
      --
      Write_Line_Number : Boolean := True;

      ---
      --  Trace with thread profex and optional line numbers.
      Write_Prefix : Boolean := True;

      ---
      --   Verbose operation.
      Verbose : Boolean := False;

      ---
      --  The controlled object used in Function_Trace is adjusted multiple
      --  times. To get a better output two adjustments are ignored.
      --
      --  Counter to count how often Adjust was called.
      --
      Repeat_Counter : Repeat_Count := 0;

      ---
      --  The controlled object used in Function_Trace is adjusted multiple
      --  times. To get a better output two adjustments are ignored.
      --
      --  remembers the last function name traced
      --
      Last_Function : Unbounded.Unbounded_String := Unbounded.Null_Unbounded_String;

   end Cl;

   ---------------------------------------------------------------------------
   --
   --  Indent Level
   --
   Indent_Level         : constant Natural := 2;
   --
   --  Commandline options
   --
   Trace_Verbose        : constant String    := "verbose";

   Trace_Opt            : constant String    := "TRACE";
   Trace_Opt_On         : constant String    := "ON";
   Trace_Opt_NoPrefix   : constant String    := "NOPREFIX";

   Trace_Opt_To         : constant String    := "TRACETO";
   Trace_Opt_To_Err1    : constant String    := "STDERR";
   Trace_Opt_To_Err2    : constant String    := "ERR";
   Trace_Opt_To_Std1    : constant String    := "STDOUT";
   Trace_Opt_To_Std2    : constant String    := "OUT";
   Trace_Opt_To_File    : constant String    := "FILE";
   Trace_Opt_To_Queue1  : constant String    := "QUEUE";
   Trace_Opt_To_Queue2  : constant String    := "PMPRINTF";

   Trace_Opt_File       : constant String    := "TRACEFILE";

   Marker_Std           : constant Character := Latin_1.Greater_Than_Sign;
   Marker_Special       : constant Character := Latin_1.Exclamation;
   Marker_Outdent       : constant Character := Latin_1.Minus_Sign;
   Marker_Indent        : constant Character := Latin_1.Plus_Sign;

   ---------------------------------------------------------------------------
   --
   --  Protect all global data.
   --
   protected body Cl is

      ---
      --  Get Trace line counter
      --
      function Get_Sequence return Natural
      is (Sequence);

      ---
      --  Trace is On
      --
      function Get_On return Boolean
      is (On);

      ------------------------------------------------------------------------
      --
      --  Determine the threadId of the current thread
      --
      procedure Get_Thread_ID (Retval : out Thread_ID) is
         use Ada.Strings.Unbounded;
         use Thread_ID_Map;
         use Ada.Task_Identification;

         FixThread_ID : constant String           := Image (Current_Task);
         StrThread_ID : constant Unbounded_String := To_Unbounded_String (FixThread_ID);
      begin
         if Contains (Container => Threads, Key => StrThread_ID) then
            Retval := Element (Container => Threads, Key => StrThread_ID);
         else
            Retval := Thread_ID'(Thread_No => Thread_No, Indent => 0);
            Insert (Container => Threads, Key => StrThread_ID, New_Item => Retval);

            Thread_No := Natural'Succ (Thread_No);

            if On then
               Write_Formatted_String (
                  Text   => "New Thread : " & FixThread_ID,
                  Marker => Marker_Special);
            end if;
         end if;
      end Get_Thread_ID;

      ---
      --  Trace Destination
      --
      function Get_Trace_Location return Destination
      is (Location);

      ---
      --  Trace with line numbers.
      --
      function Get_Write_Line_Number return Boolean
      is (Write_Line_Number);

      ---
      --  Trace with thread profex and optional line numbers.
      --
      function Get_Write_Prefix return Boolean
      is (Write_Prefix);

      ---
      --  Trace is On
      --
      function Get_Verbose return Boolean
      is (Verbose);

      ------------------------------------------------------------------------
      --
      --  Icrement Sequence by one
      --
      procedure Inc_Sequence is
      begin
         Sequence := Natural'Succ (Sequence);
      end Inc_Sequence;

      ------------------------------------------------------------------------
      --
      --  Initialize Trace.
      --
      procedure Initialize is
         use AdaCL.Command_Line.GetOpt;

         Options : AdaCL.Command_Line.GetOpt.Object;
         Found   : FoundFlag;
      begin
         Options.Set_Pattern (":" & Trace_Verbose (1));
         Options.Set_ExceptionOnError (False);
         Options.Set_ExtractGNU (True);

         ParseCL : loop
            Options.Next (Found);

            --  To trace the trace ;-)
            --
            --  Text_IO.Put_Line (Text_IO.Standard_Error, Options'Image);
            --  Text_IO.Put_Line (Text_IO.Standard_Error, Found'Image);

            exit ParseCL when Found = EndOfOptions;

            if Found = GNU_Style then
               Analyze_GNU : declare
                  Option   : constant String := Options.Get_GNUOption;
                  Argument : constant String := Options.Get_Argument;
               begin
                  if Option = Trace_Opt then
                     if Argument = Trace_Opt_On then
                        On := True;
                     elsif Argument = Trace_Opt_NoPrefix then
                        On                := True;
                        Write_Prefix      := False;
                        Write_Line_Number := False;
                     end if;
                  elsif Option = Trace_Opt_To then
                     if Argument = Trace_Opt_To_Err1
                       or else Argument = Trace_Opt_To_Err2
                     then
                        Location := Standard_Error;
                     elsif Argument = Trace_Opt_To_Std1
                       or else Argument = Trace_Opt_To_Std2
                     then
                        Location := Standard_Output;
                     elsif Argument = Trace_Opt_To_File then
                        Location := File;
                     elsif Argument = Trace_Opt_To_Queue1
                       or else Argument = Trace_Opt_To_Queue2
                     then
                        Location := Queue;
                     end if;
                  elsif Option = Trace_Opt_File then
                     if Argument'Length > 0 then
                        Set_Filename (Argument);
                     end if;
                  elsif Option = Trace_Verbose then
                     Verbose := True;
                  end if;
               end Analyze_GNU;
            elsif Found = WithoutArgument then
               Analyze_Without : declare
                  Option : constant Character := Options.Get_Option;
               begin
                  if Option = Trace_Verbose (1) then
                     Verbose := True;
                  end if;
               end Analyze_Without;
            end if;
         end loop ParseCL;
      end Initialize;

      ---
      --  The controlled object used in Function_Trace is adjusted multiple
      --  times. To get a better output two adjustments are ignored.
      --
      --  reset the last function counter and function name.
      --
      procedure Reset_Last_Function (Function_Name : String) is
      begin
         Repeat_Counter := Repeat_Count'Last;
         Last_Function  := Unbounded.To_Unbounded_String (Function_Name);
      end Reset_Last_Function;

      ---
      --  The controlled object used in Function_Trace is adjusted multiple
      --  times. To get a better output two adjustments are ignored.
      --
      --  Check if counter is greater 0
      --
      function Ignore_Function  (Function_Name : String) return Boolean
      is (Repeat_Counter > 0 and then Last_Function = Function_Name);

      ---
      --  The controlled object used in Function_Trace is adjusted multiple
      --  times. To get a better output two adjustments are ignored.
      --
      --  Decrement counter until it reaches 0
      --
      procedure Decrement_Last_Function is
      begin
         if Repeat_Counter > 0 then
            Repeat_Counter := @ - 1;
         end if;

         if Repeat_Counter = 0 then
            Last_Function := Unbounded.Null_Unbounded_String;
         end if;
      end Decrement_Last_Function;

      ---
      --
      --  Set Filename for Trace File
      --
      procedure Set_Filename (New_Filename : in String) is
      begin
         if Text_IO.Is_Open (Filehandle) then
            Text_IO.Close (Filehandle);
         end if;

         Filename := Unbounded.To_Unbounded_String (New_Filename);
      end Set_Filename;

      ---
      --
      --  Trace is On
      --
      procedure Set_On (On : Boolean) is
      begin
         Cl.On := On;
      end Set_On;

      ---
      --
      --  Determine the threadId of the current thread
      --
      procedure Set_Thread_ID (Element : in Thread_ID)
      is
         use Ada.Strings.Unbounded;
         use Thread_ID_Map;
         use Ada.Task_Identification;

         FixThread_ID : constant String           := Image (Current_Task);
         StrThread_ID : constant Unbounded_String := To_Unbounded_String (FixThread_ID);
      begin
         if Contains (Container => Threads, Key => StrThread_ID) then
            Replace (Container => Threads, Key => StrThread_ID, New_Item => Element);
         else
            Insert (Container => Threads, Key => StrThread_ID, New_Item => Element);
         end if;
      end Set_Thread_ID;

      ---
      --
      --  Trace Destination
      --
      procedure Set_Trace_Location (Location : in Destination) is
      begin
         Cl.Location := Location;
      end Set_Trace_Location;

      ---
      --
      --  Trace is On
      --
      procedure Set_Verbose (Verbose : Boolean) is
      begin
         Cl.Verbose := Verbose;
      end Set_Verbose;

      ---
      --
      --  Trace with line numbers.
      --
      procedure Set_Write_Line_Number (Write_Line_Number : Boolean) is
      begin
         Cl.Write_Line_Number := Write_Line_Number;
      end Set_Write_Line_Number;

      ---
      --
      --  Trace with thread profex and optional line numbers.
      --
      procedure Set_Write_Prefix (Write_Prefix : Boolean) is
      begin
         Cl.Write_Prefix := Write_Prefix;
      end Set_Write_Prefix;

      ---
      --
      --  Write Formated Text
      --
      --  Text   : Text to be written
      --  Marker : Marker to be used
      procedure Write_Formatted_String (
         Text   : in String;
         Marker : in Character)
      is
         use Ada.Strings.Unbounded;

         Thread : Thread_ID;
      begin
         Get_Thread_ID (Thread);

         if Marker  = Marker_Outdent and then Thread.Indent >= Indent_Level then
            Thread.Indent := Thread.Indent - Indent_Level;
         end if;

         Format : declare
            StrOut    : Unbounded_String := To_Unbounded_String (Marker & ' ' & Text);
            StrPrefix : Unbounded_String := Thread.Indent * ' ';
            StrLF     : constant String  := [1 => Latin_1.LF];
         begin
            if Write_Prefix then
               Prefix : declare
                  use Ada.Strings.Fixed;

                  StrThread_ID : constant String := Head (Thread.Thread_No'Image, 5);
                  StrLineNo    : constant String := Head (Get_Sequence'Image, 5);
               begin
                  StrPrefix := StrLineNo &
                               ":" &
                               StrThread_ID &
                               ":" &
                               StrPrefix;
               end Prefix;
            end if;

            AdaCL.Strings.Append_All (
               Source   => StrOut,
               Search   => StrLF,
               New_Item => To_String (StrPrefix),
               Mapping  => Ada.Strings.Maps.Identity);
            StrOut := StrPrefix & StrOut;

            Write_String (To_String (StrOut));
         end Format;

         Inc_Sequence;

         if Marker = Marker_Indent then
            Thread.Indent := Thread.Indent + Indent_Level;
         end if;

         Set_Thread_ID (Thread);

      end Write_Formatted_String;

      ------------------------------------------------------------------------
      --
      --  Write Text
      --
      --  Text to be written
      procedure Write_String (Text : in String) is
         use Ada.Text_IO;
      begin
         case Location is
            when Queue =>
               null;
            when Standard_Error =>
               Put_Line (Standard_Error, Text);
            when Standard_Output =>
               Put_Line (Standard_Output, Text);
            when File =>
               if not Is_Open (Filehandle) then
                  Create (
                     File => Filehandle,
                     Mode => Out_File,
                     Name => Unbounded.To_String (Filename),
                     Form => "shared=yes");
               end if;

               Put_Line (Filehandle, Text);
               Flush (Filehandle);
         end case;
      end Write_String;
   end Cl;

   ---------------------------------------------------------------------------
   --
   --  Copy Instanz.
   --
   --  This :  Object itself.
   overriding procedure Adjust (This : in out Object)
   is
   begin
      if Is_Trace_Enabled then
         if Cl.Ignore_Function (This.Trace_Name) then
            Cl.Decrement_Last_Function;
         else
            Cl.Write_Formatted_String (
               Text   => This.Trace_Name,
               Marker => Marker_Indent);
         end if;
      end if;
   end Adjust;

   ---
   --
   --  Report_Assertion a Condition. If the condition is not true create a trace entry
   --  describing the assertion and then raise an exception.
   --
   --  Condition : Condition which should be true
   --  Raising   : Exeption which is raised
   --  Message   : Free form Message
   --  Entity    : Location destriptor.
   --  Source    : Location destriptor.
   --
   procedure Report_Assertion (
      Condition : in Boolean;
      Raising   : in Ada.Exceptions.Exception_Id := Ada.Assertions.Assertion_Error'Identity;
      Message   : in String                      := "No Message given.";
      Entity    : in String                      := GNAT.Source_Info.Enclosing_Entity;
      Source    : in String                      := GNAT.Source_Info.Source_Location)
   is
   begin
      if not Condition then
         Raise_Exception (
            Raising => Raising,
            Message => Message,
            Entity  => Entity,
            Source  => Source);
      end if;
   end Report_Assertion;

   ---
   --  Report_Assertion a Condition. If the condition is not true create a trace entry
   --  describing the assertion and then raise an exception.
   --
   --  Condition : Condition which should be true
   --  Message   : Free form Message
   --  Source    : Filename.
   --  Line      : Line number.
   procedure Report_Assertion (
      Condition : Boolean;
      Message   : String;
      Source    : String  := GNAT.Source_Info.File;
      Line      : Natural := GNAT.Source_Info.Line)
   is
   begin
      if not Condition then
         Raise_Exception (
            Raising => Ada.Assertions.Assertion_Error'Identity,
            Message => Message,
            Source  => Source,
            Line    => Line);
      end if;
   end Report_Assertion;

   ---------------------------------------------------------------------------
   --
   --  Enable Trace
   --
   procedure Disable_Trace is
   begin
      Cl.Set_On (False);
   end Disable_Trace;

   ---------------------------------------------------------------------------
   --
   --  Enable Trace
   --
   procedure Disable_Verbose is
   begin
      Cl.Set_Verbose (False);
   end Disable_Verbose;

   ---------------------------------------------------------------------------
   --
   --  Don't Write Line numbers
   --
   procedure Disable_Write_Line_Number is
   begin
      Cl.Set_Write_Line_Number (False);
   end Disable_Write_Line_Number;

   ---------------------------------------------------------------------------
   --
   --  Disable the Write prefix
   --
   procedure Disable_Write_Prefix is
   begin
      Cl.Set_Write_Prefix (False);
   end Disable_Write_Prefix;

   ---------------------------------------------------------------------------
   --
   --  Enable Trace
   --
   procedure Enable_Trace is
   begin
      Cl.Set_On (True);
   end Enable_Trace;

   ---------------------------------------------------------------------------
   --
   --  Enable Trace
   --
   procedure Enable_Verbose is
   begin
      Cl.Set_Verbose (True);
   end Enable_Verbose;

   ---------------------------------------------------------------------------
   --
   --  Write Line numbers
   --
   procedure Enable_Write_Line_Number is
   begin
      Cl.Set_Write_Line_Number (True);
   end Enable_Write_Line_Number;

   ---------------------------------------------------------------------------
   --
   --  Enable the Write prefix
   --
   procedure Enable_Write_Prefix is
   begin
      Cl.Set_Write_Prefix (True);
   end Enable_Write_Prefix;

   ---------------------------------------------------------------------------
   --
   --  Trace end of function
   --
   --  This : Object itself.
   overriding procedure Finalize (This : in out Object)
   is
   begin
      if Is_Trace_Enabled then
         if Cl.Ignore_Function (This.Trace_Name) then
            Cl.Decrement_Last_Function;
         else
            Cl.Write_Formatted_String (
               Text   => This.Trace_Name,
               Marker => Marker_Outdent);
         end if;
      end if;
   end Finalize;

   ---------------------------------------------------------------------------
   --
   --  Functrace is not quite as usefull as the C++ version. The reason are
   --  the missing constructors and destructors in Ada. With Controlled types
   --  you can't limit to just one call to Initialize and one to Finalize
   --  There are allways some extra Adjust with matching. Finalize.
   --
   --  Name : Name of the function calls to be traced.
   --
   function Function_Trace (Name : String) return Object
   is
      Retval : constant Object (Name'Length) := (
         Base.Object with
         Name_Length => Name'Length,
         Trace_Name  => Name);
   begin
         --
         --  The Initialize method is not realy a replacement for a proper
         --  contructor.
         --
         if Is_Trace_Enabled then
            Cl.Write_Formatted_String (
               Text   => Retval.Trace_Name,
               Marker => Marker_Indent);
            Cl.Reset_Last_Function (Name);
         end if;

      return Retval;
   end Function_Trace;

   ---------------------------------------------------------------------------
   --
   --  check is trace is Enabled
   --
   function Is_Trace_Enabled return Boolean
   is (Cl.Get_On);

   ---------------------------------------------------------------------------
   --
   --  check is trace is Enabled
   --
   function Is_Verbose_Enabled return Boolean
   is (Cl.Get_Verbose);

   ---------------------------------------------------------------------------
   --
   --  check if Line numbers are written
   --
   function Is_Write_Line_Number_Enabled return Boolean
   is (Cl.Get_Write_Line_Number);

   ---------------------------------------------------------------------------
   --
   --  Check the Write prefix flag
   --
   function Is_Write_Prefix_Enabled return Boolean
   is (Cl.Get_Write_Prefix);

   ---------------------------------------------------------------------------
   --
   --  Trace the given exeption details and then raise the exception.
   --
   --  Raising : Exeption which is raised
   --  Message : Free form Message
   --  Entity  : Location destriptor. Suggested content: AdaCL.Trace.Entity
   --  Source  : Location destriptor. Suggested content: AdaCL.Trace.Source
   --
   procedure Raise_Exception (
      Raising : in Ada.Exceptions.Exception_Id := Ada.Assertions.Assertion_Error'Identity;
      Message : in String                      := "No Message given";
      Entity  : in String                      := GNAT.Source_Info.Enclosing_Entity;
      Source  : in String                      := GNAT.Source_Info.Source_Location)
   is
      use Ada.Exceptions;
   begin
      Write ("Raise Exception " & Exception_Name (Raising));
      Write ("   with Message " & Message);
      Write ("   for Entity   " & Entity);
      Write ("   in Source    " & Source);

      Raise_Exception (
         E       => Raising,
         Message => Message & " Entity :" & Entity & "." & " Source :" & Source & ".");
      --
      --  GNAT designer forgot to add pragma No_Return to
      --  Ada.Exceptions.Raise_Exception.
      --
      --  raise Constraint_Error;
   end Raise_Exception;

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
      Line    : in Natural                     := GNAT.Source_Info.Line)
   is
      use Ada.Exceptions;
   begin
      Write ("Raise Exception " & Exception_Name (Raising));
      Write ("   with Message " & Message);
      Write ("   in File      " & Source);
      Write ("   in Line      " & Line'Image);

      Raise_Exception (
         E       => Raising,
         Message => Message & "." & " Source :" & Source & " Line :" & Line'Image  & ".");
      --
      --  GNAT designer forgot to add pragma No_Return to
      --  Ada.Exceptions.Raise_Exception.
      --
      --  raise Constraint_Error;
   end Raise_Exception;

   ---------------------------------------------------------------------------
   --
   --  Check the Trace Destination
   --
   function Trace_Destination return Destination
   is (Cl.Get_Trace_Location);

   ---------------------------------------------------------------------------
   --
   --  Write an IString using Write_Formatted_String after adding the
   --  appropriate padding for indentation.
   --
   --  A_String : String to be written
   --
   procedure Write (A_String : in String)
   is
   begin
      if Is_Trace_Enabled then
         Cl.Write_Formatted_String (Text => A_String, Marker => Marker_Std);
      end if;
   end Write;

   ---------------------------------------------------------------------------
   --
   --  Write an Address.
   --
   --  A_String : String to be written
   --
   procedure Write (
      A_String : in String;
      An_Address : in System.Address)
   is
   begin
      if Is_Trace_Enabled then
         Write_Address : declare

            Address_Text : String (1 .. 3 + 8 + 1);

         begin
            Address_IO.Put (
               To   => Address_Text,
               Item => System.Storage_Elements.To_Integer (An_Address),
               Base => 16);

            Cl.Write_Formatted_String (
               Text   => A_String & Address_Text,
               Marker => Marker_Std);
         end Write_Address;
      end if;
   end Write;

   ---------------------------------------------------------------------------
   --
   --  Write an IString using Write_Formatted_String after adding the
   --  appropriate padding for indentation.
   --
   --  A_Unbounded : String to be written
   procedure Write (A_Unbounded : in Unbounded.Unbounded_String)
   is
      use Ada.Strings.Unbounded;
   begin
      if Is_Trace_Enabled then
         Cl.Write_Formatted_String (
            Text   => To_String (A_Unbounded),
            Marker => Marker_Std);
      end if;
   end Write;

   ---------------------------------------------------------------------------
   --
   --  Write an Exception to the Trace
   --
   --  An_Exception : String to be written
   --
   procedure Write (An_Exception : in Ada.Exceptions.Exception_Occurrence)
   is
      use Ada.Exceptions;
   begin
      if Is_Trace_Enabled then
         Cl.Write_Formatted_String (
            Text   => Exception_Information (An_Exception),
            Marker => Marker_Special);
         --              Cl.Write_Formatted_String (Text   =>
         --  G_TB.Symbolic_Traceback (An_Exception),
         --                                         Marker => Marker_Special);
      end if;
   end Write;

   ---------------------------------------------------------------------------
   --
   --  Write an Exception to the Trace
   --
   --  An_Exception : String to be written
   --  An_Entity    : Procedure in which the exception was caught
   --  A_Source     : Source File in which Entity is located.
   procedure Write (
      An_Exception : in Ada.Exceptions.Exception_Occurrence;
      An_Entity    : in String;
      A_Source     : in String)
   is
      use Ada.Exceptions;
   begin
      if Is_Trace_Enabled then
         Cl.Write_Formatted_String (
            Text   => Exception_Information (An_Exception),
            Marker => Marker_Special);
         Cl.Write_Formatted_String (
            Text   => "Function: " & An_Entity,
            Marker => Marker_Special);
         Cl.Write_Formatted_String (
            Text   => "Source: " & A_Source,
            Marker => Marker_Special);
         --       Cl.Write_Formatted_String (
         --          Text   => G_TB.Symbolic_Traceback (An_Exception),
         --          Marker => Marker_Special);
      end if;
   end Write;

   ---------------------------------------------------------------------------
   --
   --  Write Help for Commandline Options parsed from Trace
   --
   procedure Write_Commandline_Help is
      use AdaCL.Command_Line.GetOpt;
   begin
      Text_IO.New_Line;
      Text_IO.Put_Line ("Trace options:");
      Text_IO.New_Line;
      Put_Help_Line (Trace_Verbose (1), Trace_Verbose,       "verbose operation.");
      Put_Help_Line (Trace_Opt,         Trace_Opt_On,        "activate trace.");
      Put_Help_Line (Trace_Opt,         Trace_Opt_NoPrefix,  "activate trace without prefix.");
      Text_IO.New_Line;
      Put_Help_Line (Trace_Opt_To,      Trace_Opt_To_Err1,   "trace in stderr.");
      Put_Help_Line (Trace_Opt_To,      Trace_Opt_To_Std1,   "trace in stdout.");
      Put_Help_Line (Trace_Opt_To,      Trace_Opt_To_File,   "trace to file.");
      Text_IO.New_Line;
      Put_Help_Line (Trace_Opt_File,    "Filename",          "trace file.");
      Text_IO.New_Line;
   end Write_Commandline_Help;

   --
   --  Create a memory dump
   --
   --  String to be written
   procedure Write_Dump (
      An_Address       : in System.Address;
      A_Size           : in System.Storage_Elements.Storage_Count)
   is
      use System.Storage_Elements;
   begin
      Write ("Address         : ", An_Address);
      Write ("Lenght          :" & A_Size'Image);

      if Is_Trace_Enabled then
         Dump : declare
            package Byte_IO is new Text_IO.Modular_IO (Num => Storage_Element);

            use Ada.Strings.Fixed;

            Data : Storage_Array (0 .. A_Size - 1);
            for Data'Address use An_Address;
            pragma Import (Ada, Data);

            Line_Len     : constant := 16;
            Address_Len  : constant := 8;
            Byte_Len     : constant := 2;
            Byte_Offset  : constant := 18;   --  Dump  [01234567]
            ASCII_Offset : constant := Byte_Offset + Line_Len * (Byte_Len + 1) + 1;
            Text_Len     : constant := ASCII_Offset + Line_Len;

            Byte_Text    : String (1 .. 3 + Byte_Len + 1);
            Address_Text : String (1 .. 3 + Address_Len + 1);
            Text         : String (1 .. Text_Len);
            Line         : Storage_Offset := Data'First;
            Col          : Storage_Offset := Data'First;
            Char         : Character;
            Byte_Col     : Integer;
         begin
            Dump_Line : while Line <= Data'Last loop
               Address_IO.Put (
                  To   => Address_Text,
                  Item => To_Integer (An_Address + Line),
                  Base => 16);

               if Address_Text (4) = '#' then
                  Address_Text (4) := '0';
               end if;

               Move (
                  Source => "Dump  [" & Address_Text (4 .. 11) & "]: ",
                  Target => Text);

               Col      := 0;
               Byte_Col := Byte_Offset;

               Dump_Column : while Col < Line_Len and then Col + Line < A_Size
               loop
                  Byte_IO.Put (
                     To   => Byte_Text,
                     Item => Data (Line + Col),
                     Base => 16);

                  if Byte_Text (4) = '#' then
                     Byte_Text (4) := '0';
                  end if;

                  Text (Byte_Col .. Byte_Col + 1) := Byte_Text (4 .. 5);

                  Char := Character'Val (Data (Line + Col));

                  if  Ada.Characters.Handling.Is_Graphic (Char) then
                     Text (Natural (ASCII_Offset + Col))  := Char;
                  else
                     Text (Natural (ASCII_Offset + Col))  := '.';
                  end if;

                  Col      := Col + 1;
                  Byte_Col := Byte_Col + (Byte_Len + 1);
               end loop Dump_Column;

               Cl.Write_Formatted_String (
                  Text   => Text,
                  Marker => Marker_Std);
               Line := Line + Line_Len;
            end loop Dump_Line;
         end Dump;
      end if;
   end Write_Dump;

   ---------------------------------------------------------------------------
   --
   --  Create a memory dump. This Dump takes size in bits.
   --
   --  An_Address :  String to be written
   --  A_Size     :  Size in Storage_Elements.
   procedure Write_Dump (
      An_Address : in System.Address;
      A_Size     : in Integer)
   is
      use System.Storage_Elements;

      Size : Storage_Count := Storage_Count (A_Size / System.Storage_Unit);
   begin
      if (A_Size mod System.Storage_Unit) /= 0 then
         Size := Size + 1;
      end if;

      Write_Dump (An_Address, Size);
   end Write_Dump;

   ---------------------------------------------------------------------------
   --
   --  Write an IString using Write_Formatted_String after adding the
   --  appropriate padding for indentation.
   --
   --  A_String : String to be written
   procedure Write_Error (A_String : in String) is
   begin
      if not Is_Trace_Enabled
        or else Trace_Destination /= Standard_Error
      then
         Text_IO.Put_Line (Text_IO.Standard_Error, A_String);
      end if;

      if Is_Trace_Enabled then
         Cl.Write_Formatted_String (Text => A_String, Marker => Marker_Std);
      end if;
   end Write_Error;

   ---------------------------------------------------------------------------
   --
   --  Write an IString using Write_Formatted_String after adding the
   --  appropriate padding for indentation.
   --
   --  A_Unbounded : String to be written
   procedure Write_Error (A_Unbounded : in Unbounded.Unbounded_String) is
      use Ada.Strings.Unbounded;
   begin
      if not Is_Trace_Enabled
        or else Trace_Destination /= Standard_Error
      then
         Text_IO.Put_Line (Text_IO.Standard_Error, To_String (A_Unbounded));
      end if;

      if Is_Trace_Enabled then
         Cl.Write_Formatted_String (
            Text   => To_String (A_Unbounded),
            Marker => Marker_Std);
      end if;
   end Write_Error;

   ---------------------------------------------------------------------------
   --
   --  Write an Exception to the Trace
   --
   --  An_Exception :  String to be written
   procedure Write_Error (
      An_Exception : in Ada.Exceptions.Exception_Occurrence)
   is
      use Ada.Exceptions;
   begin
      if not Is_Trace_Enabled
        or else Trace_Destination /= Standard_Error
      then
         Text_IO.Put_Line (
            Text_IO.Standard_Error,
            Exception_Information (An_Exception));
         --              Text_IO.Put_Line (Text_IO.Standard_Error,
         --  G_TB.Symbolic_Traceback (An_Exception));
      end if;

      Write (An_Exception);
   end Write_Error;

   ---------------------------------------------------------------------------
   --
   --  Write an Exception to the Trace
   --
   --  An_Exception :  String to be written
   --  An_Entity    :  Procedure in which the exception was caught
   --  A_Source     :  Source File in which Entity is located.
   procedure Write_Error (
      An_Exception : in Ada.Exceptions.Exception_Occurrence;
      Entity       : in String;
      Source       : in String)
   is
      use Ada.Exceptions;
   begin
      if not Is_Trace_Enabled or else Trace_Destination /= Standard_Error then
         Text_IO.New_Line (Text_IO.Standard_Error);
         Text_IO.Put (Text_IO.Standard_Error, Exception_Information (An_Exception));
         Text_IO.Put_Line (Text_IO.Standard_Error, "Function: " & Entity);
         Text_IO.Put_Line (Text_IO.Standard_Error, "Source: " & Source);
         --              Text_IO.Put_Line (Text_IO.Standard_Error,
         --  G_TB.Symbolic_Traceback (An_Exception));
      end if;

      Write (
         An_Exception => An_Exception,
         An_Entity    => Entity,
         A_Source     => Source);
   end Write_Error;

   ---------------------------------------------------------------------------
   --
   --  When verbose is aktivated then an empty line is written to
   --  Standart_Output
   --
   procedure Write_Info is
   begin
      if Is_Verbose_Enabled then
         Text_IO.New_Line (Text_IO.Standard_Output);
      end if;
   end Write_Info;

   ---------------------------------------------------------------------------
   --
   --  Write an IString using writeFormattedString after adding the
   --  appropriate padding for indentation.
   --
   --  When verbose is aktivated then the string is written to Standart_Output
   --  as well.
   --
   --  A_String : String to be written
   procedure Write_Info (A_String : in String) is
   begin
      if Is_Verbose_Enabled
        and then (not Is_Trace_Enabled
                 or else Trace_Destination /= Standard_Output)
      then
         Text_IO.Put_Line (Text_IO.Standard_Output, A_String);
      end if;

      if Is_Trace_Enabled then
         Cl.Write_Formatted_String (Text => A_String, Marker => Marker_Std);
      end if;
   end Write_Info;

   ---------------------------------------------------------------------------
   --
   --  When verbose is aktivated then the character is written to
   --  Standart_Output.
   --
   --  A_Character : String to be written
   procedure Write_Info (A_Character : in Character) is
   begin
      if Is_Verbose_Enabled then
         Text_IO.Put (Text_IO.Standard_Output, A_Character);
      end if;
   end Write_Info;

   ---------------------------------------------------------------------------
   --
   --  Write an IString using writeFormattedString after adding the
   --  appropriate padding for indentation.
   --
   --  When verbose is aktivated then the string is written to Standart_Output
   --  as well.
   --
   --  A_Unbounded : String to be written
   procedure Write_Info (A_Unbounded : in Unbounded.Unbounded_String) is
      use Ada.Strings.Unbounded;
   begin
      if Is_Verbose_Enabled and then (
          not Is_Trace_Enabled or else Trace_Destination /= Standard_Output)
      then
         Text_IO.Put_Line (Text_IO.Standard_Output, To_String (A_Unbounded));
      end if;

      if Is_Trace_Enabled then
         Cl.Write_Formatted_String (
            Text   => To_String (A_Unbounded),
            Marker => Marker_Std);
      end if;
   end Write_Info;

   ---------------------------------------------------------------------------
   --
   --  Write to queue - not supported yet.
   --
   procedure Write_To_File is
   begin
      Cl.Set_Trace_Location (File);
   end Write_To_File;

   ---------------------------------------------------------------------------
   --
   --  Set Filename for Trace File
   --
   procedure Write_To_File (New_Filename : in String) is
   begin
      Cl.Set_Filename (New_Filename);
      Cl.Set_Trace_Location (File);
   end Write_To_File;

   ---------------------------------------------------------------------------
   --
   --  Write to queue - not supported yet.
   --
   procedure Write_To_Queue is
   begin
      Cl.Set_Trace_Location (Queue);
   end Write_To_Queue;

   ---------------------------------------------------------------------------
   --
   --  Write to Standart Error
   --
   procedure Write_To_Standard_Error is
   begin
      Cl.Set_Trace_Location (Standard_Error);
   end Write_To_Standard_Error;

   ---------------------------------------------------------------------------
   --
   --  Write to Standart Error
   --
   procedure Write_To_Standard_Output is
   begin
      Cl.Set_Trace_Location (Standard_Output);
   end Write_To_Standard_Output;

   ---------------------------------------------------------------------------
   --
   --  Write an IString using Write_Formatted_String after adding the
   --  appropriate padding for indentation.
   --
   --  A_String : String to be written
   procedure Write_Wide (A_String : in Wide_String) is
   begin
      if Is_Trace_Enabled then
         Cl.Write_Formatted_String (
            Text   => Ada.Characters.Conversions.To_String (A_String),
            Marker => Marker_Std);
      end if;
   end Write_Wide;

begin
   Cl.Initialize;
   pragma Debug (Enable_Trace);
   pragma Debug (Write_To_File);
end AdaCL.Trace;

---------------------------------------------------------------- {{{ ----------
--  vim: set textwidth=0 nowrap tabstop=8 shiftwidth=3 softtabstop=3 expandtab :
--  vim: set filetype=ada fileencoding=utf-8 fileformat=unix foldmethod=expr :
--  vim: set spell spelllang=en_gb
