--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2017 - 2022 Stephen Leake.  All Rights Reserved.
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

pragma License (GPL);

with AUnit.Assertions;
with AUnit.Checks.Containers;
with Ada.Exceptions;
with Ada.Text_IO;
with Test_Skip_To_Aux;
with WisiToken.Parse.LR.Parser_No_Recover;
with WisiToken.Syntax_Trees;
package body Test_Skip_To is

   Log_File  : Ada.Text_IO.File_Type;

   ----------
   --  Test procedures

   procedure Dos_Line_Endings (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use AUnit.Checks;
      use AUnit.Checks.Containers;

      File_Name : constant String := "../test/bnf/skip_to_grammar.input"; -- DOS line endings
   begin
      Test_Skip_To_Aux.Enable           := True;
      Test_Skip_To_Aux.DOS_Line_Endings := True;

      Test_Skip_To_Aux.Parser.Tree.Lexer.Reset_With_File (File_Name);
      Test_Skip_To_Aux.Parser.Tree.Lexer.Set_Verbosity (WisiToken.Trace_Lexer - 1);
      Test_Skip_To_Aux.Parser.Parse (Log_File);

      Check ("errors", Test_Skip_To_Aux.Parser.Tree.Error_Count, 0);

      Test_Skip_To_Aux.Parser.Execute_Actions;

      Check ("tests pass", Test_Skip_To_Aux.Test_Pass_Count, 7);
   exception
   when E : WisiToken.Syntax_Error | WisiToken.Parse_Error =>
      declare
         use Ada.Exceptions;
      begin
         Ada.Text_IO.Put_Line (Exception_Name (E) & ": " & Exception_Message (E));
         if Test_Skip_To_Aux.Parser.Tree.Editable then
            Test_Skip_To_Aux.Parser.Put_Errors;
         else
            Test_Skip_To_Aux.Parser.Put_Errors (Test_Skip_To_Aux.Parser.Tree.First_Parse_Stream);
         end if;
      end;
      AUnit.Assertions.Assert (False, "exception");
   end Dos_Line_Endings;

   procedure Unix_Line_Endings (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use AUnit.Checks;
      use AUnit.Checks.Containers;

      File_Name : constant String := "../test/bnf/skip_to_grammar_unix.input"; -- Unix line endings
   begin
      Test_Skip_To_Aux.Enable           := True;
      Test_Skip_To_Aux.DOS_Line_Endings := False;

      Test_Skip_To_Aux.Parser.Tree.Lexer.Reset_With_File (File_Name);
      Test_Skip_To_Aux.Parser.Tree.Lexer.Set_Verbosity (WisiToken.Trace_Lexer - 1);
      Test_Skip_To_Aux.Parser.Parse (Log_File);

      Check ("errors", Test_Skip_To_Aux.Parser.Tree.Error_Count, 0);

      Test_Skip_To_Aux.Parser.Execute_Actions;

      Check ("tests pass", Test_Skip_To_Aux.Test_Pass_Count, 7);
   exception
   when E : WisiToken.Syntax_Error | WisiToken.Parse_Error =>
      declare
         use Ada.Exceptions;
      begin
         Ada.Text_IO.Put_Line (Exception_Name (E) & ": " & Exception_Message (E));
         if Test_Skip_To_Aux.Parser.Tree.Editable then
            Test_Skip_To_Aux.Parser.Put_Errors;
         else
            Test_Skip_To_Aux.Parser.Put_Errors (Test_Skip_To_Aux.Parser.Tree.First_Parse_Stream);
         end if;
      end;
      AUnit.Assertions.Assert (False, "exception");
   end Unix_Line_Endings;

   ----------
   --  Public subprograms

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("test_skip_to.adb");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Dos_Line_Endings'Access, "Dos_Line_Endings");
      Register_Routine (T, Unix_Line_Endings'Access, "Unix_Line_Endings");
   end Register_Tests;

   overriding procedure Set_Up (T : in out Test_Case)
   is
      pragma Unreferenced (T);
   begin
      Test_Skip_To_Aux.Reset;
   end Set_Up;

end Test_Skip_To;
