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
with Ada.Characters.Latin_1;
with Ada.Exceptions;
with Ada.Text_IO;
with WisiToken.AUnit;
with WisiToken.Gen_Token_Enum;
with WisiToken.Generate.LR.LR1_Generate;
with WisiToken.Generate.LR1_Items.AUnit; use WisiToken.Generate.LR1_Items.AUnit;
with WisiToken.Generate.LR1_Items;
with WisiToken.Lexer.Regexp;
with WisiToken.Parse.LR.AUnit;
with WisiToken.Parse.LR.Parser;
with WisiToken.Productions;
with WisiToken.Syntax_Trees;
with WisiToken.Text_IO_Trace;
with WisiToken.Wisi_Ada; use WisiToken.Wisi_Ada;
package body Dragon_4_43_LR1_Test is

   --  [dragon] example 4.43 pg 235

   type Token_Enum_ID is
     (
      --  terminals
      Lower_C_ID,
      Lower_D_ID,
      EOI_ID,

      --  non-terminals
      Accept_ID,
      Upper_S_ID,
      Upper_C_ID,

      SOI_ID);

   package Token_Enum is new WisiToken.Gen_Token_Enum
     (Token_Enum_ID     => Token_Enum_ID,
      First_Terminal    => Lower_C_ID,
      Last_Terminal     => EOI_ID,
      First_Nonterminal => Accept_ID,
      Last_Nonterminal  => Upper_C_ID,
      SOI_ID            => SOI_ID,
      EOI_ID            => EOI_ID,
      Accept_ID         => Accept_ID,
      Case_Insensitive  => False);
   use Token_Enum;

   Null_Action : WisiToken.Syntax_Trees.Post_Parse_Action renames WisiToken.Syntax_Trees.Null_Action;

   Grammar : WisiToken.Productions.Prod_Arrays.Vector :=
     Accept_ID <= Upper_S_ID & EOI_ID + Null_Action -- 1.0
     and
     Upper_S_ID <= Upper_C_ID & Upper_C_ID + Null_Action -- 2.0
     and
     (Upper_C_ID <= Lower_C_ID & Upper_C_ID + Null_Action -- 3.0
      or
        Lower_D_ID + Null_Action) -- 3.1
     ;

   Map : constant array (WisiToken.State_Index range 0 .. 9) of WisiToken.Unknown_State_Index :=
     --  Map (dragon index) = our index; see comment in Test_LR1_Items
     (0 => 0,
      1 => 3,
      2 => 4,
      3 => 1,
      4 => 2,
      5 => 8,
      6 => 6,
      7 => 7,
      8 => 5,
      9 => 9);

   package Lexer renames WisiToken.Lexer.Regexp;

   Syntax : constant Lexer.Syntax := To_Syntax
     ((
       Lower_C_ID => Lexer.Get ("c"),
       Lower_D_ID => Lexer.Get ("d"),
       EOI_ID     => Lexer.Get ("" & Ada.Characters.Latin_1.EOT)
     ));

   Has_Empty_Production    : constant WisiToken.Token_ID_Set                 :=
     WisiToken.Generate.Has_Empty_Production (Grammar);
   First_Nonterm_Set       : constant WisiToken.Token_Array_Token_Set        := WisiToken.Generate.First
     (Grammar, Has_Empty_Production, Token_Enum.LALR_Descriptor.First_Terminal);
   First_Terminal_Sequence : constant WisiToken.Token_Sequence_Arrays.Vector :=
     WisiToken.Generate.To_Terminal_Sequence_Array (First_Nonterm_Set, Token_Enum.LALR_Descriptor);

   Trace : aliased WisiToken.Text_IO_Trace.Trace;
   Log_File : Ada.Text_IO.File_Type;

   ----------
   --  Test procedures

   procedure Test_First_Follow (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use WisiToken.AUnit;

      --  FIRST defined in [dragon] pg 189; we add nonterminals

      Expected_First_Nonterm_Set : constant WisiToken.Token_Array_Token_Set := To_Nonterminal_Array_Token_Set
        ((Accept_ID  => (Upper_S_ID | Upper_C_ID | Lower_C_ID | Lower_D_ID => True, others => False),
          Upper_S_ID => (Upper_C_ID | Lower_C_ID | Lower_D_ID => True, others => False),
          Upper_C_ID => (Lower_C_ID | Lower_D_ID => True, others => False)));

      --  FOLLOW defined in [dragon] pg 189
      Expected_Follow : constant WisiToken.Token_Array_Token_Set := To_Nonterminal_Array_Terminal_Set
        ((Accept_ID  => (others => False),
          Upper_S_ID => (EOI_ID => True, others => False),
          Upper_C_ID => (Lower_C_ID | Lower_D_ID | EOI_ID => True)));

      Computed_Follow : constant WisiToken.Token_Array_Token_Set := WisiToken.Generate.Follow
        (Grammar, LR1_Descriptor, First_Nonterm_Set, Has_Empty_Production);
   begin
      Check ("0", Has_Empty_Production, WisiToken.Token_ID_Set'(+Accept_ID .. +Upper_C_ID => False));
      Check ("1", First_Nonterm_Set, Expected_First_Nonterm_Set);

      if WisiToken.Trace_Action > WisiToken.Outline then
         Ada.Text_IO.Put_Line ("Follow:");
         WisiToken.Put (LR1_Descriptor, Computed_Follow);
      end if;

      Check ("2", Computed_Follow, Expected_Follow);
   end Test_First_Follow;

   procedure Test_LR1_Items (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      use WisiToken.Generate.LR1_Items;

      Computed : constant Item_Set_List := WisiToken.Generate.LR.LR1_Generate.LR1_Item_Sets
        (Has_Empty_Production, First_Terminal_Sequence, Grammar, LR1_Descriptor);

      Expected : Item_Set_List :=
        --  [dragon] fig 4.39 pg 235 shows the item sets and gotos. We
        --  search in a different order, which causes state numbers to
        --  not match, so we use Map, and list them in our search order.
        (Map (0) +
           (Get_Item (Grammar, (+Accept_ID, 0), 1, +EOI_ID) &
              Get_Item (Grammar, (+Upper_S_ID, 0), 1, +EOI_ID) &
              Get_Item (Grammar, (+Upper_C_ID, 0), 1, +(Lower_D_ID, Lower_C_ID)) &
              Get_Item (Grammar, (+Upper_C_ID, 1), 1, +(Lower_D_ID, Lower_C_ID)))) &
        (Map (3) +
           (Get_Item (Grammar, (+Upper_C_ID, 0), 2, +(Lower_C_ID, Lower_D_ID)) &
              Get_Item (Grammar, (+Upper_C_ID, 0), 1, +(Lower_C_ID, Lower_D_ID)) &
              Get_Item (Grammar, (+Upper_C_ID, 1), 1, +(Lower_C_ID, Lower_D_ID)))) &
        (Map (4) +
           Get_Item (Grammar, (+Upper_C_ID, 1), 2, +(Lower_C_ID, Lower_D_ID))) &
        (Map (1) +
           Get_Item (Grammar, (+Accept_ID, 0), 2, +EOI_ID)) &
        (Map (2) +
           (Get_Item (Grammar, (+Upper_S_ID, 0), 2, +EOI_ID) &
              Get_Item (Grammar, (+Upper_C_ID, 0), 1, +EOI_ID) &
              Get_Item (Grammar, (+Upper_C_ID, 1), 1, +EOI_ID))) &
        (Map (8) +
           Get_Item (Grammar, (+Upper_C_ID, 0), 3, +(Lower_C_ID, Lower_D_ID))) &
        (Map (6) +
           (Get_Item (Grammar, (+Upper_C_ID, 0), 2, +EOI_ID) &
              Get_Item (Grammar, (+Upper_C_ID, 0), 1, +EOI_ID) &
              Get_Item (Grammar, (+Upper_C_ID, 1), 1, +EOI_ID))) &
        (Map (7) +
           Get_Item (Grammar, (+Upper_C_ID, 1), 2, +EOI_ID)) &
        (Map (5) +
           Get_Item (Grammar, (+Upper_S_ID, 0), 3, +EOI_ID)) &
        (Map (9) +
           Get_Item (Grammar, (+Upper_C_ID, 0), 3, +EOI_ID))
      ;

   begin
      Add_Gotos
        (Expected, Map (0),
         +(+Lower_C_ID, Map (3)) &
           (+Lower_D_ID, Map (4)) &
           (+Upper_S_ID, Map (1)) &
           (+Upper_C_ID, Map (2)));

      Add_Gotos
        (Expected, Map (2),
         +(+Lower_C_ID, Map (6)) &
           (+Lower_D_ID, Map (7)) &
           (+Upper_C_ID, Map (5)));

      Add_Gotos
        (Expected, Map (3),
         +(+Lower_C_ID, Map (3)) &
           (+Lower_D_ID, Map (4)) &
           (+Upper_C_ID, Map (8)));

      Add_Gotos
        (Expected, Map (6),
         +(+Lower_C_ID, Map (6)) &
           (+Lower_D_ID, Map (7)) &
           (+Upper_C_ID, Map (9)));

      if WisiToken.Trace_Action > WisiToken.Outline then
         Ada.Text_IO.Put_Line ("computed:");
         Put (Grammar, LR1_Descriptor, Computed);
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line ("expected:");
         Put (Grammar, LR1_Descriptor, Expected);
      end if;
      Check ("", Computed, Expected);
   end Test_LR1_Items;

   procedure Parser_Table (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      use WisiToken;
      use WisiToken.Parse.LR;
      use WisiToken.Parse.LR.AUnit;

      Recursions : WisiToken.Generate.Recursions := WisiToken.Generate.Empty_Recursions;

      Computed : constant Parse_Table_Ptr := WisiToken.Generate.LR.LR1_Generate.Generate
        (Grammar, LR1_Descriptor, Grammar_File_Name => "", Error_Recover => False, Recursions => Recursions);

      Expected : Parse_Table
        (State_First       => 0,
         State_Last        => 9,
         First_Terminal    => +Lower_C_ID,
         Last_Terminal     => +EOI_ID,
         First_Nonterminal => +Accept_ID,
         Last_Nonterminal  => +Upper_C_ID);

   begin
      --  figure 4.40 pg 236
      --  'r1' means reduce by production 1, 0 indexed; our production 2
      --  'acc' = reduce by our production 1
      --  We don't have an explicit error action; Symbol not found => error.

      Add_Action (Expected.States (Map (0)), +Lower_C_ID, (3, 0), Map (3));
      Add_Action (Expected.States (Map (0)), +Lower_D_ID, (3, 1), Map (4));
      Add_Goto (Expected.States (Map (0)), +Upper_C_ID, Map (2));
      Add_Goto (Expected.States (Map (0)), +Upper_S_ID, Map (1));

      Add_Action (Expected.States (Map (1)), +EOI_ID, Accept_It, (+Accept_ID, 0), 1);

      Add_Action (Expected.States (Map (2)), +Lower_C_ID, (3, 0), Map (6));
      Add_Action (Expected.States (Map (2)), +Lower_D_ID, (3, 1), Map (7));
      Add_Goto (Expected.States (Map (2)), +Upper_C_ID, Map (5));

      Add_Action (Expected.States (Map (3)), +Lower_C_ID, (3, 0), Map (3));
      Add_Action (Expected.States (Map (3)), +Lower_D_ID, (3, 1), Map (4));
      Add_Goto (Expected.States (Map (3)), +Upper_C_ID, Map (8));

      Add_Action (Expected.States (Map (4)), +Lower_C_ID, Reduce, (+Upper_C_ID, 1), 1);
      Add_Action (Expected.States (Map (4)), +Lower_D_ID, Reduce, (+Upper_C_ID, 1), 1);

      Add_Action (Expected.States (Map (5)), +EOI_ID, Reduce, (+Upper_S_ID, 0), 2);

      Add_Action (Expected.States (Map (6)), +Lower_C_ID, (3, 0), Map (6));
      Add_Action (Expected.States (Map (6)), +Lower_D_ID, (3, 1), Map (7));
      Add_Goto (Expected.States (Map (6)), +Upper_C_ID, Map (9));

      Add_Action (Expected.States (Map (7)), +EOI_ID, Reduce, (+Upper_C_ID, 1), 1);

      Add_Action (Expected.States (Map (8)), +Lower_C_ID, Reduce, (+Upper_C_ID, 0), 2);
      Add_Action (Expected.States (Map (8)), +Lower_D_ID, Reduce, (+Upper_C_ID, 0), 2);

      Add_Action (Expected.States (Map (9)), +EOI_ID, Reduce, (+Upper_C_ID, 0), 2);

      Check ("", Computed.all, Expected);
   end Parser_Table;

   procedure Test_Parse (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      Parser : WisiToken.Parse.LR.Parser.Parser;

      procedure Execute_Command (Command : in String)
      is begin
         Parser.Tree.Lexer.Reset_With_String (Command);
         Parser.Parse (Log_File);
      exception
      when E : others =>
         AUnit.Assertions.Assert (False, "'" & Command & "': " & Ada.Exceptions.Exception_Message (E));
      end Execute_Command;

      Recursions : WisiToken.Generate.Recursions := WisiToken.Generate.Empty_Recursions;
   begin
      WisiToken.Parse.LR.Parser.New_Parser
        (Parser,
         Lexer.New_Lexer (Trace'Access, LR1_Descriptor'Access, Syntax),
         WisiToken.Generate.LR.LR1_Generate.Generate
           (Grammar, LR1_Descriptor, Grammar_File_Name => "", Error_Recover => False, Recursions => Recursions),
         WisiToken.Syntax_Trees.Production_Info_Trees.Empty_Vector,
         User_Data                      => null,
         Language_Fixes                 => null,
         Language_Matching_Begin_Tokens => null,
         Language_String_ID_Set         => null);

      Execute_Command ("cdcd");
   end Test_Parse;

   ----------
   --  Public subprograms

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("dragon_4_43_lr1_test.adb");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_First_Follow'Access, "Test_First_Follow");
      Register_Routine (T, Test_LR1_Items'Access, "Test_LR1_Items");
      Register_Routine (T, Parser_Table'Access, "Parser_Table");
      Register_Routine (T, Test_Parse'Access, "Test_Parse");
   end Register_Tests;

end Dragon_4_43_LR1_Test;
