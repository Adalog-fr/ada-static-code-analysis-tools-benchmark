--  Abstract :
--
--  Parse a file with the WisiToken lalr parser, output the corrected token stream.
--
--  Copyright (C) 2020 Stephen Leake All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with Ada.Characters.Handling;
with Ada.Command_Line;
with Ada.Directories;
with Ada.Exceptions;
with Ada.IO_Exceptions;
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Ada_Lite_Actions;
with Ada_Lite_LALR_Main;
with Ada_Lite_LR1_T1_Main;
with GNAT.Traceback.Symbolic;
with WisiToken.Parse.LR.McKenzie_Recover.Ada_Lite;
with WisiToken.Parse.LR.Parser;
with WisiToken.Syntax_Trees;
with WisiToken.Text_IO_Trace;
procedure Dump_WisiToken_Ada_Lite_Corrected
is
   use WisiToken;
   use all type Ada_Lite_Actions.Token_Enum_ID;

   procedure Put_Usage
   is begin
      Put_Line (Standard_Error, "dump_wisitoken_corrected <alg> <file> [trace_parse trace_mckenzie]");
      Put_Line ("alg {LR1 | LALR}");
   end Put_Usage;

   File_Name : Ada.Strings.Unbounded.Unbounded_String;

   Trace  : aliased WisiToken.Text_IO_Trace.Trace (Ada_Lite_Actions.Descriptor'Unrestricted_Access);
   Parser : WisiToken.Parse.LR.Parser.Parser;

   function Image (Node : in WisiToken.Valid_Node_Index) return String
   is
      use Ada_Lite_Actions;
      use WisiToken.Syntax_Trees;

      Punctuation_Image : constant Token_ID_Array_String (+AMPERSAND_ID .. +STAR_ID) :=
        (new String'("&"),
         new String'(":"),
         new String'(":="),
         new String'(","),
         new String'("."),
         new String'(".."),
         new String'("="),
         new String'("=>"),
         new String'(">"),
         new String'(">="),
         new String'("<"),
         new String'("<="),
         new String'("-"),
         new String'("+"),
         new String'(";"),
         new String'("/"),
         new String'("/="),
         new String'("*"));

      Tree : WisiToken.Syntax_Trees.Tree renames Parser.Tree;
      ID   : constant Token_ID := Tree.ID (Node);
   begin
      if Tree.Label (Node) = Shared_Terminal then
         declare
            Token : Base_Token renames Parser.Terminals (Tree.First_Shared_Terminal (Node));
         begin
            case To_Token_Enum (ID) is
            when IDENTIFIER_ID =>
               return "IDENTIFIER " & Parser.Lexer.Buffer_Text (Token.Byte_Region);
            when NUMERIC_LITERAL_ID =>
               return "NUMERIC_LITERAL";
            when STRING_LITERAL_ID =>
               return "STRING_LITERAL";
            when others =>
               return Parser.Lexer.Buffer_Text (Token.Byte_Region);
            end case;
         end;
      else
         if ID in Punctuation_Image'Range then
            return Punctuation_Image (ID).all;
         elsif -ID = LEFT_PAREN_ID then
            return "(";
         elsif -ID = RIGHT_PAREN_ID then
            return ")";
         elsif -ID = IDENTIFIER_ID then
            return "IDENTIFIER";
         else
            return Ada.Characters.Handling.To_Lower (Ada_Lite_Actions.Descriptor.Image (ID).all);
         end if;
      end if;
   end Image;

begin
   declare
      use Ada.Command_Line;
      use Ada.Directories;
   begin
      Set_Exit_Status (Success);
      if Argument_Count < 2 then
         Put_Usage;
         Set_Exit_Status (Failure);
         return;
      end if;

      if Argument (1) = "LR1" then
         Ada_Lite_LR1_T1_Main.Create_Parser
           (Parser,
            WisiToken.Parse.LR.McKenzie_Recover.Ada_Lite.Fixes'Access,
            WisiToken.Parse.LR.McKenzie_Recover.Ada_Lite.Matching_Begin_Tokens'Access,
            WisiToken.Parse.LR.McKenzie_Recover.Ada_Lite.String_ID_Set'Access,
            Trace'Unrestricted_Access,
            User_Data => null,
            Text_Rep_File_Name => Containing_Directory (Command_Name) & "/ada_lite_lr1_t1_re2c_parse_table.txt");
      else
         Ada_Lite_LALR_Main.Create_Parser
           (Parser,
            WisiToken.Parse.LR.McKenzie_Recover.Ada_Lite.Fixes'Access,
            WisiToken.Parse.LR.McKenzie_Recover.Ada_Lite.Matching_Begin_Tokens'Access,
            WisiToken.Parse.LR.McKenzie_Recover.Ada_Lite.String_ID_Set'Access,
            Trace'Unrestricted_Access,
            User_Data => null);
      end if;

      File_Name := +Argument (2);
      begin
         Parser.Lexer.Reset_With_File (-File_Name);
      exception
      when Ada.IO_Exceptions.Name_Error =>
         Put_Line (Standard_Error, "'" & (-File_Name) & "' cannot be opened");
         Set_Exit_Status (Failure);
         return;
      end;

      if Argument_Count >= 3 then
         WisiToken.Trace_Parse := Integer'Value (Argument (3));
      end if;

      if Argument_Count >= 4 then
         WisiToken.Trace_McKenzie := Integer'Value (Argument (4));
      end if;
   end;

   Parser.Trace.Set_Prefix (";; "); -- so we get the same debug messages as Emacs_Wisi_Common_Parse

   Parser.Table.McKenzie_Param.Task_Count := 1; -- minimize race conditions

   Parser.Table.McKenzie_Param.Enqueue_Limit := 58_000; -- same as ada.wy


   Parser.Parse;

   if Trace_Parse > 0 then
      Parser.Put_Errors;
   end if;

   declare
      Terminals : constant Valid_Node_Index_Array := Parser.Tree.Get_Terminals (Parser.Tree.Root);
   begin
      for T of Terminals loop
         Put_Line (Image (T));
      end loop;
   end;

exception
when E : others =>
   Put_Line (Standard_Error, -File_Name & ":1:1");
   Put_Line (Standard_Error,
             "exception " & Ada.Exceptions.Exception_Name (E) & ": " & Ada.Exceptions.Exception_Message (E));
   Put_Line (Standard_Error, GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
   Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
end Dump_WisiToken_Ada_Lite_Corrected;
--  Local Variables:
--  ada-case-strict: nil
--  End:
