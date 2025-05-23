--  Abstract :
--
--  Run one WisiToken AUnit test
--
--  Copyright (C) 2009, 2010, 2012 - 2014, 2017 - 2022 Stephen Leake.  All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or (at
--  your option) any later version. This program is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
--  MA 02111-1307, USA.

with AUnit.Options;
with AUnit.Reporter.Text;
with AUnit.Test_Cases; use AUnit.Test_Cases;
with AUnit.Test_Filters.Verbose;
with AUnit.Test_Results;
with AUnit.Test_Suites; use AUnit.Test_Suites;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;
with GNAT.Traceback.Symbolic;
with Test_Incremental;
with WisiToken;
procedure Test_One_Harness
is
   Usage : constant String :=
     --  command line arguments (all optional, order matters):
     "test_name routine_name trace_config mckenzie_options";
   --  1         2            3           4
   --  trace_config is passed to Wisitoken.Enable_Trace
   --
   --  test_name, routine_name can be '' to set trace for all routines.

   McKenzie_Config : String_Access;
   --  pragma Unreferenced (McKenzie_Config);
   Filter : aliased AUnit.Test_Filters.Verbose.Filter;

   Options : constant AUnit.Options.AUnit_Options :=
     (Global_Timer     => False,
      Test_Case_Timer  => False,
      Report_Successes => True,
      Filter           => Filter'Unchecked_Access);

   Suite    : constant Access_Test_Suite := new Test_Suite;
   Reporter : AUnit.Reporter.Text.Text_Reporter;
   Result   : AUnit.Test_Results.Result;
   Status   : AUnit.Status;

   function "+" (Item : in String) return String_Access
   is begin
      return String_Access'(new String'(Item));
   end "+";
begin
   case Argument_Count is
   when 0 =>
      null;

   when 1 =>
      Filter.Test_Name := To_Unbounded_String (Argument (1)); -- test name only

   when 2 .. 4 =>
      Filter.Test_Name := To_Unbounded_String (Argument (1));
      Filter.Routine_Name := To_Unbounded_String (Argument (2));

      if Argument_Count >= 3 then
         WisiToken.Enable_Trace (Argument (3));
      end if;

      if Argument_Count >= 4 then
         McKenzie_Config := +Argument (4);
      end if;

   when others =>
      raise Constraint_Error with Usage;
   end case;

   Filter.Verbose := WisiToken.Trace_Tests > 0;

   Add_Test (Suite, Test_Case_Access'(new Test_Incremental.Test_Case (McKenzie_Config)));

   Run (Suite, Options, Result, Status);

   AUnit.Reporter.Text.Report (Reporter, Result);

exception
when E : others =>
   Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Name (E) & ": " & Ada.Exceptions.Exception_Message (E));
   Ada.Text_IO.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
end Test_One_Harness;
