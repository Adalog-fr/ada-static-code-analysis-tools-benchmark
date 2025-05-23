--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2015, 2017, 2018, 2020, 2021 Stephen Leake.  All Rights Reserved.
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
with AUnit.Checks;
with Ada.Exceptions;
with SAL.AUnit;
with SAL.Gen_Unbounded_Definite_Stacks;
with System.Assertions;
package body Test_Stacks is

   type Integer_Array_Type is array (SAL.Peek_Type range <>) of Integer;

   package Unbounded_Definite_Stacks is new SAL.Gen_Unbounded_Definite_Stacks (Integer);

   procedure Check
     (Label    : in String;
      Computed : in Unbounded_Definite_Stacks.Stack;
      Expected : in Integer_Array_Type)
   is
      use AUnit.Checks;
      use SAL.AUnit;
   begin
      Check (Label & " count", Computed.Depth, Expected'Length);

      for I in Expected'Range loop
         Check (Label & SAL.Base_Peek_Type'Image (I), Computed.Peek (I), Expected (I));
      end loop;
   end Check;

   ----------
   --  Test procedures

   procedure Nominal (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      use AUnit.Assertions;
      use AUnit.Checks;
      use SAL.AUnit;

      Stack : Unbounded_Definite_Stacks.Stack;
   begin

      Check ("0a", Stack.Is_Empty, True);
      Check ("0b", Stack.Depth, 0);

      --  Assign (copy) an empty stack; test Adjust
      declare
         Stack_2 : Unbounded_Definite_Stacks.Stack;
         pragma Unreferenced (Stack_2);
      begin
         Stack_2 := Stack;
      exception
      when E : others =>
         Assert (False, Ada.Exceptions.Exception_Message (E));
      end;

      for I in 1 .. 5 loop
         Stack.Push (I);
      end loop;

      Check ("1a", Stack, (5, 4, 3, 2, 1));
      Check ("1b", Stack.Is_Empty, False);
      Check ("1c", Stack.Depth, 5);

      Check ("2", Stack.Pop, 5);
      Check ("2a", Stack, (4, 3, 2, 1));
      Check ("2b", Stack.Depth, 4);

      Check ("3", Stack.Top, 4);
      Check ("3a", Stack, (4, 3, 2, 1));

      Stack.Push (6);
      Check ("4a", Stack, (6, 4, 3, 2, 1));

      Check ("5", Stack.Pop, 6);
      Check ("6", Stack.Pop, 4);
      Check ("7", Stack.Pop, 3);
      Check ("8", Stack.Pop, 2);
      Check ("9", Stack.Pop, 1);
      Check ("9a", Stack.Is_Empty, True);
      Check ("9b", Stack.Depth, 0);

      declare
         Junk : Integer;
      begin
         Junk := Stack.Pop;
         AUnit.Assertions.Assert (False, "10 did not get exception");
      exception
      when SAL.Container_Empty | System.Assertions.Assert_Failure =>
         null;
      end;

      for I in reverse 1 .. 3 loop
         Stack.Push (I);
      end loop;

      Check ("11", Stack, (1, 2, 3));
      Stack.Clear;

      Check ("12", Stack.Is_Empty, True);
   end Nominal;

   procedure Compare (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      use AUnit.Assertions;

      Stack_1 : Unbounded_Definite_Stacks.Stack;
      Stack_2 : Unbounded_Definite_Stacks.Stack;

      use all type Unbounded_Definite_Stacks.Stack; -- "="
   begin
      --  Compare stacks that have different Data'Last, but same content.
      --  Also test Adjust with non-empty stack.

      Stack_1.Set_Depth (10);
      for I in 1 .. 5 loop
         Stack_1.Set (SAL.Base_Peek_Type (I), 5, I);
      end loop;

      Stack_2.Set_Depth (5);
      for I in 1 .. 5 loop
         Stack_2.Set (SAL.Base_Peek_Type (I), 5, I);
      end loop;

      Assert (Stack_2 = Stack_1, "set not equal");

      --  test Adjust
      begin
         Stack_2 := Stack_1;
         Assert (Stack_2 = Stack_1, "copy not equal");
      exception
      when E : others =>
         Assert (False, Ada.Exceptions.Exception_Message (E));
      end;
   end Compare;

   procedure Test_Iterate (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      use AUnit.Assertions;

      Stack_1 : Unbounded_Definite_Stacks.Stack;
      I       : Integer := 1;
   begin
      --  Test Iterate

      for I in reverse 1 .. 10 loop
         Stack_1.Push (I);
      end loop;

      for J of Stack_1 loop
         Assert (J = I, "iterate 1" & I'Image & " got" & J'Image);
         I := I + 1;
      end loop;
   end Test_Iterate;

   procedure Test_Invert (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      use AUnit.Checks;

      Stack_1 : Unbounded_Definite_Stacks.Stack;
   begin
      for I in reverse 1 .. 10 loop
         Stack_1.Push (I);
      end loop;

      declare
         Stack_2 : constant Unbounded_Definite_Stacks.Stack := Stack_1.Invert;
      begin
         Check ("1", Stack_2.Peek (1), 10);
         Check ("2", Stack_2.Peek (2), 9);
         Check ("9", Stack_2.Peek (9), 2);
         Check ("10", Stack_2.Peek (10), 1);
      end;
   end Test_Invert;

   procedure Test_Copy_Slice (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      use AUnit.Checks;

      Stack_1 : Unbounded_Definite_Stacks.Stack;
      Stack_2 : Unbounded_Definite_Stacks.Stack;
   begin
      for I in 1 .. 10 loop
         Stack_1.Push (I);
      end loop;

      for I in 20 .. 25 loop
         Stack_2.Push (I);
      end loop;

      Unbounded_Definite_Stacks.Copy_Slice
        (Source             => Stack_1,
         Target             => Stack_2,
         Source_Start_Depth => 5,
         Target_Start_Depth => 5,
         Count              => 5);

      Check ("1", Stack_2.Peek (1), 10);
      Check ("2", Stack_2.Peek (2), 9);
      Check ("3", Stack_2.Peek (3), 8);
      Check ("5", Stack_2.Peek (5), 6);
      Check ("6", Stack_2.Peek (6), 20);
   end Test_Copy_Slice;

   ----------
   --  Public routines

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Nominal'Access, "Nominal");
      Register_Routine (T, Compare'Access, "Compare");
      Register_Routine (T, Test_Iterate'Access, "Test_Iterate");
      Register_Routine (T, Test_Invert'Access, "Test_Invert");
      Register_Routine (T, Test_Copy_Slice'Access, "Test_Copy_Slice");
   end Register_Tests;

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("test_stacks.adb");
   end Name;

end Test_Stacks;
