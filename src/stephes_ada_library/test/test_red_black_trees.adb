--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2017 - 2020 Stephen Leake.  All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. This program is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE. See the GNU General Public License for more
--  details. You should have received a copy of the GNU General Public
--  License distributed with this program; see file COPYING. If not,
--  write to the Free Software Foundation, 59 Temple Place - Suite
--  330, Boston, MA 02111-1307, USA.

pragma License (GPL);

with Ada.Containers;
with AUnit.Assertions;
with AUnit.Checks;
with AUnit.Checks.Containers;
with SAL.Gen_Unbounded_Definite_Red_Black_Trees.Gen_Test;
package body Test_Red_Black_Trees is

   type Cache_Type is record
      Pos : Integer;
   end record;

   function Key (Item : in Cache_Type) return Integer
   is (Item.Pos);

   function Key_Compare (Left, Right : in Integer) return SAL.Compare_Result is
     (if Left > Right then SAL.Greater
      elsif Left < Right then SAL.Less
      else SAL.Equal);

   package Trees is new SAL.Gen_Unbounded_Definite_Red_Black_Trees
     (Element_Type => Cache_Type,
      Key_Type     => Integer,
      Key          => Key,
      Key_Compare  => Key_Compare);

   package Tree_Test is new Trees.Gen_Test (Integer'Image);
   use Tree_Test;

   Red   : constant Boolean := True;
   Black : constant Boolean := False;

   procedure Check
     (Label    : in String;
      Computed : in Cache_Type;
      Expected : in Cache_Type)
   is
      use AUnit.Checks;
   begin
      Check (Label & ".pos", Computed.Pos, Expected.Pos);
   end Check;

   procedure Check
     (Label        : in String;
      Tree         : in Trees.Tree;
      Cursor       : in Trees.Cursor;
      Expected_Pos : in Integer;
      Expect_Red   : in Boolean)
   is
      use AUnit.Checks;
   begin
      Check_Non_Null (Label & ".null", Cursor);
      Check (Label & ".pos", Tree (Cursor).Pos, Expected_Pos);
      Check_Color (Label & ".color", Cursor, Expect_Red);
   end Check;

   type Integer_Array_Integer is array (Natural range <>) of Integer;

   procedure Check_Sorted
     (Label     : in String;
      Computed  : in Trees.Tree;
      Expected  : in Integer_Array_Integer;
      Ascending : in Boolean)
   is
      use AUnit.Assertions;
      use AUnit.Checks;
      use AUnit.Checks.Containers;
      use all type Ada.Containers.Count_Type;

      I     : Integer                   := Expected'First;
      Count : Ada.Containers.Count_Type := 0;
   begin
      Check (Label & " sorted.length 1", Computed.Count, Expected'Length);
      if Ascending then
         for Element of Computed loop
            Assert (I <= Expected'Last, Label & " sorted ascending.iterator returning too many items");
            Check (Label & " sorted ascending." & Integer'Image (I), Element.Pos, Expected (I));
            I     := I + 1;
            Count := Count + 1;
         end loop;
         Assert (Count = Expected'Length, Label & " sorted ascending iterator returning too few items");
      else
         for Element of reverse Computed loop
            Assert (I <= Expected'Last, Label & " sorted ascending.iterator returning too many items");
            Check (Label & " sorted descending." & Integer'Image (I), Element.Pos, Expected (I));
            I     := I + 1;
            Count := Count + 1;
         end loop;
         Assert (Count = Expected'Length, Label & " sorted descending iterator returning too few items");
      end if;

   end Check_Sorted;

   ----------
   --  Test procedures

   procedure Nominal (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use AUnit.Checks.Containers;
      use AUnit.Checks;
      use Trees;
      Tree : Trees.Tree;

      I : Cursor;
   begin
      --  Build and check a tree, covering all cases of the insert position
      --  of the new node.
      Tree.Insert ((Pos => 3)); -- insert into null tree
      Tree.Insert ((Pos => 7)); -- insert to right
      I := Tree.Insert ((Pos => 10)); -- insert to right, fixup
      Check ("0", Tree, I, 10, Red);

      --              7 b
      --          3 r     10 r
      Check ("1.count", Tree.Count, 3);
      Validate ("1", Tree);
      I := Root (Tree);
      Check ("1.bh", Black_Height (Tree, I), 0);
      Check ("1.0", Tree, I, 7, Black);
      I := Left (I);
      Check ("1.1", Tree, I, 3, Red);
      I := Parent (I);
      I := Right (I);
      Check ("1.2", Tree, I, 10, Red);

      Check_Sorted ("1", Tree, (3, 7, 10), Ascending => True);

      Tree.Insert ((Pos => 12));
      --              7 b
      --           3 b   10 b
      --                     12 r
      Check ("2.count", Tree.Count, 4);
      Validate ("2", Tree);
      I := Root (Tree);
      Check ("2.bh", Black_Height (Tree, I), 1);
      Check ("2.0", Tree, I, 7, Black);
      I := Left (I);
      Check ("2.1", Tree, I, 3, Black);
      I := Parent (I);
      I := Right (I);
      Check ("2.2", Tree, I, 10, Black);
      I := Right (I);
      Check ("2.3", Tree, I, 12, Red);

      Tree.Insert ((Pos => 14));
      --              7b
      --           3b    12b
      --              10r   14r
      Check ("3.count", Tree.Count, 5);
      Validate ("3", Tree);
      I := Root (Tree);
      Check ("3.bh", Black_Height (Tree, I), 1);
      Check ("3.0", Tree, I, 7, Black);
      I := Left (I);
      Check ("3.1", Tree, I, 3, Black);
      I := Parent (I);
      I := Right (I);
      Check ("3.2", Tree, I, 12, Black);
      I := Left (I);
      Check ("3.3", Tree, I, 10, Red);
      I := Parent (I);
      I := Right (I);
      Check ("3.4", Tree, I, 14, Red);

      Tree.Insert ((Pos => 16));
      --              7b
      --           3b    12r
      --              10b   14b
      --                       16r
      Check ("4.count", Tree.Count, 6);
      Validate ("4", Tree);
      I := Root (Tree);
      Check ("4.bh", Black_Height (Tree, I), 1);
      Check ("4.l.bh", Black_Height (Tree, Left (I)), 0);
      Check ("4.r.bh", Black_Height (Tree, Right (I)), 1);
      Check ("4.0", Tree, I, 7, Black);
      I := Left (I);
      Check ("4.1", Tree, I, 3, Black);
      I := Parent (I);
      I := Right (I);
      Check ("4.2", Tree, I, 12, Red);
      I := Left (I);
      Check ("4.3", Tree, I, 10, Black);
      I := Parent (I);
      I := Right (I);
      Check ("4.4", Tree, I, 14, Black);
      I := Right (I);
      Check ("4.4", Tree, I, 16, Red);

      Tree.Insert ((Pos => 15));
      --             7b
      --         3b      12r
      --              10b    15b
      --                  14r   16r
      Check ("5.count", Tree.Count, 7);
      Validate ("5", Tree);
      I := Root (Tree);
      Check ("5.bh", Black_Height (Tree, I), 1);
      Check ("5.0", Tree, I, 7, Black);
      I := Left (I);
      Check ("5.1", Tree, I, 3, Black);
      I := Parent (I);
      I := Right (I);
      Check ("5.2", Tree, I, 12, Red);
      I := Left (I);
      Check ("5.3", Tree, I, 10, Black);
      I := Parent (I);
      I := Right (I);
      Check ("5.4", Tree, I, 15, Black);
      I := Left (I);
      Check ("5.5", Tree, I, 14, Red);
      I := Parent (I);
      I := Right (I);
      Check ("5.6", Tree, I, 16, Red);

      Check_Sorted ("5", Tree, (3, 7, 10, 12, 14, 15, 16), Ascending => True);

      Tree.Insert ((Pos => 17));
      --              12b
      --        7r          15r
      --      3b  10b    14b   16b
      --                          17r
      Check ("6.count", Tree.Count, 8);
      Validate ("6", Tree);
      I := Root (Tree);
      Check ("6.0", Tree, I, 12, Black);
      I := Right (I);
      Check ("6.1", Tree, I, 15, Red);
      I := Right (I);
      Check ("6.2", Tree, I, 16, Black);
      I := Right (I);
      Check ("6.3", Tree, I, 17, Red);

      Tree.Insert ((Pos => 2));
      --              12b
      --        7r          15r
      --      3b  10b    14b   16b
      --    2r                    17r
      Check ("7.count", Tree.Count, 9);
      Validate ("7", Tree);
      I := Root (Tree);
      Check ("7.0", Tree, I, 12, Black);
      I := Left (I);
      Check ("7.1", Tree, I, 7, Red);
      I := Left (I);
      Check ("7.2", Tree, I, 3, Black);
      I := Left (I);
      Check ("7.3", Tree, I, 2, Red);

      Tree.Insert ((Pos => 1));
      --              12b
      --         7r          15r
      --      2b   10b    14b   16b
      --    1r  3r                 17r
      Check ("8.count", Tree.Count, 10);
      Validate ("8", Tree);
      I := Root (Tree);
      Check ("8.0", Tree, I, 12, Black);
      I := Left (I);
      Check ("8.1", Tree, I, 7, Red);
      I := Left (I);
      Check ("8.2", Tree, I, 2, Black);
      I := Left (I);
      Check ("8.3", Tree, I, 1, Red);

      Tree.Insert ((Pos => 9));
      --                12b
      --          7r          15r
      --      2b     10b   14b   16b
      --    1r  3r 9r               17r
      Check ("9.count", Tree.Count, 11);
      Validate ("9", Tree);
      I := Root (Tree);
      Check ("9.0", Tree, I, 12, Black);
      I := Left (I);
      Check ("9.1", Tree, I, 7, Red);
      I := Right (I);
      Check ("9.2", Tree, I, 10, Black);
      I := Left (I);
      Check ("9.3", Tree, I, 9, Red);

      Check_Sorted ("9a", Tree, (1, 2, 3, 7, 9, 10, 12, 14, 15, 16, 17), Ascending => True);
      Check_Sorted ("9b", Tree, (17, 16, 15, 14, 12, 10, 9, 7, 3, 2, 1), Ascending => False);

      Check ("present 10", Tree.Present (10), True);
      Check ("present 14", Tree.Present (14), True);
      Check ("present 13", Tree.Present (13), False);

      Check ("find 10", Tree, Find (Tree.Iterate, 10, Unknown), 10, Black);
      Check ("find 14", Tree, Find (Tree.Iterate, 14, Unknown), 14, Black);
      Check ("find 13", Has_Element (Find (Tree.Iterate, 13, Unknown)), False);

      Check ("index 10", Tree (10).Element.all, (Pos => 10));

      I := Previous (Tree.Iterate, 11);
      Check ("prev 11", Tree, I, 10, Black);
      I := Previous (Tree.Iterate, I);
      Check ("prev cursor", Tree, I, 9, Red);

      Check ("in_range 1", Tree, Find_In_Range (Tree.Iterate, Ascending, 10, 15), 10, Black);
      Check ("in_range 2", Tree, Find_In_Range (Tree.Iterate, Ascending, 11, 15), 12, Black);

      Check ("in_range 3", Tree, Find_In_Range (Tree.Iterate, Descending, 10, 15), 15, Red);
      Check ("in_range 4", Tree, Find_In_Range (Tree.Iterate, Descending, 10, 13), 12, Black);

      Check_Null ("in_range not found", Find_In_Range (Tree.Iterate, Ascending, 4, 5));

      Tree.Delete (10);
      Check_Null ("delete.find", Find (Tree.Iterate, 10, Unknown));
      --                12b
      --          7r          15r
      --      2b     9b    14b   16b
      --    1r  3r                  17r
      I := Root (Tree);
      Check ("delete 10.0", Tree, I, 12, Black);
      I := Left (I);
      Check ("delete 10.1", Tree, I, 7, Red);
      I := Right (I);
      Check ("delete 10.2", Tree, I, 9, Black);
      Validate ("delete 10", Tree);

      Tree.Delete (7);
      --                12b
      --          2r          15r
      --      1b     9b    14b   16b
      --           3r               17r
      I := Root (Tree);
      Check ("delete 7.0", Tree, I, 12, Black);
      I := Left (I);
      Check ("delete 7.1", Tree, I, 2, Red);
      I := Right (I);
      Check ("delete 7.2", Tree, I, 9, Black);
      I := Left (I);
      Check ("delete 7.3", Tree, I, 3, Red);
      Validate ("delete 7", Tree);

      Tree.Delete (17);
      --                12b
      --          2r          15r
      --      1b     9b    14b   16b
      --           3r
      Validate ("delete 17", Tree);

      Tree.Delete (16);
      --                12b
      --          2r          15r
      --      1b     9b    14b        FIXME: wrong
      --           3r
      Validate ("delete 16", Tree);

      Tree.Delete (15);
      Validate ("delete 15", Tree);
   end Nominal;

   procedure Test_Copy (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use Trees;
      Tree : Trees.Tree;
   begin
      --  Copy and finalize an empty tree; no exceptions;
      declare
         Tree_2 : constant Trees.Tree := Tree;
      begin
         Validate ("0 valid", Tree_2);
         Check_Sorted ("0", Tree, (1 .. 0 => 1), Ascending => True);
      end;

      --  Build and copy a tree.
      Tree.Insert ((Pos => 3));
      Tree.Insert ((Pos => 7));
      Tree.Insert ((Pos => 10));
      Tree.Insert ((Pos => 12));
      --              7 b
      --           3 b   10 b
      --                     12 r
      Validate ("1 valid", Tree);
      Check_Sorted ("1", Tree, (3, 7, 10, 12), Ascending => True);

      declare
         Tree_2 : constant Trees.Tree := Tree;
      begin
         Validate ("2 valid", Tree_2);

         Check_Sorted ("2", Tree, (3, 7, 10, 12), Ascending => True);

         --  Add to tree does not affect tree_2
         Tree.Insert ((Pos => 17));

         Validate ("3a valid", Tree_2);

         Check_Sorted ("3a", Tree_2, (3, 7, 10, 12), Ascending => True);

         Validate ("3b valid", Tree);

         Check_Sorted ("3b", Tree, (3, 7, 10, 12, 17), Ascending => True);
      end;
   end Test_Copy;

   procedure Test_Find_Or_Insert (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use AUnit.Checks;
      use Trees;
      Tree : Trees.Tree;
      Found_Cur : Cursor;
      Found : Boolean;
   begin
      --  Build a tree.
      Tree.Insert ((Pos => 3));
      Tree.Insert ((Pos => 7));
      Tree.Insert ((Pos => 10));
      Tree.Insert ((Pos => 12));
      Validate ("1 valid", Tree);
      Check_Sorted ("1", Tree, (3, 7, 10, 12), Ascending => True);

      Found_Cur := Tree.Find_Or_Insert ((Pos => 7), Found);
      Validate ("2 valid", Tree);
      Check_Sorted ("2", Tree, (3, 7, 10, 12), Ascending => True);

      Check ("2 found", Found, True);
      Check ("2 cur has_element", Has_Element (Found_Cur), True);
      Check ("2 cur value", Tree.Constant_Ref (Found_Cur).Element.all, (Pos => 7));

      Found_Cur := Tree.Find_Or_Insert ((Pos => 8), Found);
      Validate ("3 valid", Tree);
      Check_Sorted ("3", Tree, (3, 7, 8, 10, 12), Ascending => True);

      Check ("3 found", Found, False);
      Check ("3 cur has_element", Has_Element (Found_Cur), True);
      Check ("3 cur value", Tree.Constant_Ref (Found_Cur).Element.all, (Pos => 8));

   end Test_Find_Or_Insert;

   ----------
   --  Public routines

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Nominal'Access, "Nominal");
      Register_Routine (T, Test_Copy'Access, "Test_Copy");
      Register_Routine (T, Test_Find_Or_Insert'Access, "Test_Find_Or_Insert");
   end Register_Tests;

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("test_red_black_trees.adb");
   end Name;

end Test_Red_Black_Trees;
