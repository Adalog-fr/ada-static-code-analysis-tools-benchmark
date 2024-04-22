--------------------------------------------------------------- {{{1 ----------
--  Copyright © 2003 … 2023 Martin Krischik «krischik@users.sourceforge.net»
------------------------------------------------------------------------------
--  This library is free software; you can redistribute it and/or modify it
--  under the terms of the GNU Library General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or (at your
--  option) any later version.
--
--  This library is distributed in the hope that it will be useful, but
--  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
--  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public
--  License for more details.
--
--  You should have received a copy of the GNU Library General Public License
--  along with this library; if not, write to the Free Software Foundation,
--  Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
--------------------------------------------------------------- }}}1 ----------

pragma License (Modified_Gpl);
pragma Ada_2022;

with Ada.Strings.Fixed;
--  AdaCL.Strings can not use AdaCL.Trace since AdaCL.Trace uses AdaCL.Strings
--  with Ada.Text_IO;

--
--  No, I have not created some new String class - yet. Just a few string
--  tools.
--
package body AdaCL.Strings is

   ---------------------------------------------------------------------------

   package Maps   renames Ada.Strings.Maps;
   package S      renames Ada.Strings;
   package S_U    renames Ada.Strings.Unbounded;

   ---------------------------------------------------------------------------

   use type S_U.Unbounded_String;

   ---------------------------------------------------------------------------
   --
   --  Searches for all occurences of text "Search" and Inserts text "Insert"
   --  after the found text but only when "Insert" is not allready there.
   --
   --  String to be changed
   --  String we look for
   --  String we want to insert
   --  Search mapping
   --
   procedure Append_All (
      Source   : in out S_U.Unbounded_String;
      Search   : in String;
      New_Item : in String;
      Mapping  : in Maps.Character_Mapping := Maps.Identity)
   is
      Count : Natural := Natural'First;
   begin
      Append_All (
         Source   => Source,
         Search   => Search,
         New_Item => New_Item,
         Mapping  => Mapping,
         Count    => Count);
   end Append_All;

   ---------------------------------------------------------------------------
   --
   --  Searches for all occurences of text "Search" and Inserts text "Insert"
   --  after the found text but only when "Insert" is not allready there.
   --
   --  Source   : String to be changed
   --  Search   : String we look for
   --  New_Item : String we want to insert
   --  Mapping  : Search mapping
   --  Count    : Count of replaces done
   --
   procedure Append_All (
      Source   : in out S_U.Unbounded_String;
      Search   : in String;
      New_Item : in String;
      Mapping  : in Maps.Character_Mapping := Maps.Identity;
      Count    : out Natural)
   is
      use Ada.Strings.Unbounded;
      use Ada.Strings.Fixed;

      --
      --  Offset from which we start. 0 means search from the first character
      --
      Offset : Natural := Natural'First;
      --
      --  Lenght of the full string
      --
      Len : constant Natural := Length (Source);
   begin
      --
      --  nothing found yet
      --
      Count := Natural'First;

      Search_Next : loop
         --
         --  Last Found Item was at the end of the String
         --
         exit Search_Next when Len = 0 or else Offset >= Len;

         Next_Sub_String : declare
            --
            --  We slice from the Offset on to the end. One might be suprised
            --  to learn that Sub_String'First might not be 1
            --
            Sub_String : constant String :=
               Slice (Source => Source, Low => Offset + 1, High => Len);
            --
            --  We search for Pattern
            --
            Low  : constant Natural :=
               Index (
                  Source  => Sub_String,
                  Pattern => Search,
                  Going   => S.Forward,
                  Mapping => Mapping);
            High : constant Natural := Low + Search'Length;
         begin
            --  Ada.Text_IO.Put_Line ("New_Item'Length   = " & Natural'Image (New_Item'Length));
            --  Ada.Text_IO.Put_Line ("Sub_String'Length = " & Natural'Image (Sub_String'Length));
            --  Ada.Text_IO.Put_Line ("High              = " & Natural'Image (High));
            --  Ada.Text_IO.Put_Line ("High - NI'Len-1   = " & Natural'Image (High - New_Item'Length));
            --  Ada.Text_IO.Put_Line ("High - 1          = " & Natural'Image (High - 1));
            --
            --  Exit Loop when we havn't found anything
            --
            exit Search_Next when Low = 0;

            if New_Item'Length > Sub_String'Last - High + 1 or else
               New_Item /= Sub_String (High .. High + New_Item'Length - 1)
            then
               --
               --  We insert one character after the end of the found string
               --  when the new text does not allready follow. This can of
               --  course only happen when there are enouch characters behind
               --  the found string to contain the new text.
               --
               Insert (
                  Source   => Source,
                  Before   => High + Offset + 1 - Sub_String'First,
                  New_Item => New_Item);
               --
               --  Found one.
               --
               Count := Natural'Succ (Count);
            end if;
            --
            --  We set the offset to the end of the found string.
            --
            Offset := Offset + High;
         end Next_Sub_String;
      end loop Search_Next;
   end Append_All;

   ---------------------------------------------------------------------------
   --
   --  Searches for first occurence of text "Search" and Inserts text "Insert"
   --  after the found text but only when "Insert" is not allready there.
   --
   --  Source   : String to be changed
   --  Search   : String we look for
   --  New_Item : String we want to insert
   --  Mapping  : Search mapping
   --
   procedure Append_First (
      Source   : in out S_U.Unbounded_String;
      Search   : in String;
      New_Item : in String;
      Mapping  : in Maps.Character_Mapping := Maps.Identity)
   is
      Found : Boolean := Boolean'First;
   begin
      Append_First (
         Source   => Source,
         Search   => Search,
         New_Item => New_Item,
         Mapping  => Mapping,
         Found    => Found);
   end Append_First;

   ---------------------------------------------------------------------------
   --
   --  Searches for first occurence of text "Search" and Inserts text "Insert"
   --  after the found text but only when "Insert" is not allready there.
   --
   --  Source   : String to be changed
   --  Search   : String we look for
   --  New_Item : String we want to insert
   --  Mapping  : Search mapping
   --  Found    : Count of replaces done
   --
   procedure Append_First (
      Source   : in out S_U.Unbounded_String;
      Search   : in String;
      New_Item : in String;
      Mapping  : in Maps.Character_Mapping := Maps.Identity;
      Found    : out Boolean)
   is
      use Ada.Strings.Unbounded;

      Low  : constant Natural := Index (
         Source  => Source,
         Pattern => Search,
         Going   => S.Forward,
         Mapping => Mapping);
      High : constant Natural := Low + Search'Length;
      Len  : constant Natural := Length (Source);
   begin
      if Low = 0 then
         Found := False;
      elsif New_Item'Length >= Len - High or else
         New_Item /= Slice (
            Source => Source,
            Low    => High,
            High   => High + New_Item'Length - 1)
      then
         Insert (Source => Source, Before => High, New_Item => New_Item);
         Found := True;
      else
         Found := False;
      end if;
   end Append_First;

   ---------------------------------------------------------------------------
   --
   --  Searches for last occurence of text "Search" and Inserts text "Insert"
   --  after the found text but only when "Insert" is not allready there.
   --
   --  Source   : String to be changed
   --  Search   : String we look for
   --  New_Item : String we want to insert
   --  Mapping  : Search mapping
   --
   procedure Append_Last (
      Source   : in out S_U.Unbounded_String;
      Search   : in String;
      New_Item : in String;
      Mapping  : in Maps.Character_Mapping := Maps.Identity)
   is
      Found : Boolean := Boolean'First;
   begin
      Append_Last (
         Source   => Source,
         Search   => Search,
         New_Item => New_Item,
         Mapping  => Mapping,
         Found    => Found);
   end Append_Last;

   ---------------------------------------------------------------------------
   --
   --  Searches for last occurence of text "Search" and Inserts text "Insert"
   --  after the found text but only when "Insert" is not allready there.
   --
   --  Source   : String to be changed
   --  Search   : String we look for
   --  New_Item : String we want to insert
   --  Mapping  : Search mapping
   --  Found    : Count of replaces done
   --
   procedure Append_Last (
      Source   : in out S_U.Unbounded_String;
      Search   : in String;
      New_Item : in String;
      Mapping  : in Maps.Character_Mapping := Maps.Identity;
      Found    : out Boolean)
   is
      use Ada.Strings.Unbounded;

      Low  : constant Natural :=
         Index (
            Source  => Source,
            Pattern => Search,
            Going   => S.Backward,
            Mapping => Mapping);
      High : constant Natural := Low + Search'Length;
      Len  : constant Natural := Length (Source);
   begin
      if Low = 0 then
         Found := False;
      elsif New_Item'Length >= Len - High or else
         New_Item /= Slice (
            Source => Source,
            Low    => High,
            High   => High + New_Item'Length - 1)
      then
         Insert (Source => Source, Before => High, New_Item => New_Item);
         Found := True;
      else
         Found := False;
      end if;
   end Append_Last;

   ---------------------------------------------------------------------------
   --
   --  Replace all Search with Replace
   --
   --  Source  : String to be changed
   --  Search  : String we look for
   --  Replace : String we want to have
   --  Mapping : Search mapping
   --
   procedure Change_All (
      Source  : in out S_U.Unbounded_String;
      Search  : in String;
      Replace : in String;
      Mapping : in Maps.Character_Mapping := Maps.Identity)
   is
      Count : Natural := Natural'First;
   begin
      Change_All (
         Source  => Source,
         Search  => Search,
         Replace => Replace,
         Mapping => Mapping,
         Count   => Count);
   end Change_All;

   ---------------------------------------------------------------------------
   --
   --  Replace all Search with Replace and Count how often it was done.
   --
   --  Source  : String to be changed
   --  Search  : String we look for
   --  Replace : String we want to have
   --  Mapping : Search mapping
   --  Count   : Count of replaces done
   --
   procedure Change_All (
      Source  : in out S_U.Unbounded_String;
      Search  : in String;
      Replace : in String;
      Mapping : in Maps.Character_Mapping := Maps.Identity;
      Count   : out Natural)
   is
   begin
      Count := Natural'First;

      SearchAll : loop
         ReplaceOne : declare
            Found : Boolean;
         begin
            Change_First (
               Source  => Source,
               Search  => Search,
               Replace => Replace,
               Mapping => Mapping,
               Found   => Found);

            exit SearchAll when not Found;

            Count := Natural'Succ (Count);
         end ReplaceOne;
      end loop SearchAll;
   end Change_All;

   ---------------------------------------------------------------------------
   --
   --  Replace First Search with Replace and return success flag.
   --
   --  Source  : String to be changed
   --  Search  : String we look for
   --  Replace : String we want to have
   --  Mapping : Search mapping
   --  Found   : Count of replaces done
   --
   procedure Change_First (
      Source  : in out S_U.Unbounded_String;
      Search  : in String;
      Replace : in String;
      Mapping : in Maps.Character_Mapping := Maps.Identity;
      Found   : out Boolean)
   is
      use Ada.Strings.Unbounded;

      Low : constant Natural :=
         Index (
            Source  => Source,
            Pattern => Search,
            Going   => S.Forward,
            Mapping => Mapping);
   begin
      if Low /= 0 then
         ReplaceOne : declare
            High : constant Natural := Low + Search'Length - 1;
         begin
            Replace_Slice (
               Source => Source,
               Low    => Low,
               High   => High,
               By     => Replace);
         end ReplaceOne;
         Found := True;
      else
         Found := False;
      end if;
   end Change_First;

   ---------------------------------------------------------------------------
   --
   --  Replace all Search with Replace.
   --
   --  Source  : String to be changed
   --  Search  : String we look for
   --  Replace : String we want to have
   --  Mapping : Search mapping
   --
   procedure Change_First (
      Source  : in out S_U.Unbounded_String;
      Search  : in String;
      Replace : in String;
      Mapping : in Maps.Character_Mapping := Maps.Identity)
   is
      Found : Boolean := False;
   begin
      Change_First (
         Source  => Source,
         Search  => Search,
         Replace => Replace,
         Mapping => Mapping,
         Found   => Found);
   end Change_First;

   ---------------------------------------------------------------------------
   --
   --  Replace Last Search with Replace
   --
   --  Source  : Search mapping
   --  Search  : String we want to have
   --  Replace : String we look for
   --  Mapping : String to be changed
   --
   procedure Change_Last (
      Source  : in out S_U.Unbounded_String;
      Search  : in String;
      Replace : in String;
      Mapping : in Maps.Character_Mapping := Maps.Identity)
   is
      Found : Boolean := False;
   begin
      Change_First (
         Source  => Source,
         Search  => Search,
         Replace => Replace,
         Mapping => Mapping,
         Found   => Found);
   end Change_Last;

   ---------------------------------------------------------------------------
   --
   --  Replace Last Search with Replace and return success flag.
   --
   --  Source  : String to be changed
   --  Search  : String we look for
   --  Replace : String we want to have
   --  Mapping : Search mapping
   --  Found   : Count of replaces done
   --
   procedure Change_Last (
      Source  : in out S_U.Unbounded_String;
      Search  : in String;
      Replace : in String;
      Mapping : in Maps.Character_Mapping := Maps.Identity;
      Found   : out Boolean)
   is
      use Ada.Strings.Unbounded;

      Low : constant Natural :=
         Index (
            Source  => Source,
            Pattern => Search,
            Going   => S.Backward,
            Mapping => Mapping);
   begin
      if Low /= 0 then
         ReplaceOne : declare
            High : constant Natural := Low + Search'Length - 1;
         begin
            Replace_Slice (
               Source => Source,
               Low    => Low,
               High   => High,
               By     => Replace);
         end ReplaceOne;
         Found := True;
      else
         Found := False;
      end if;
   end Change_Last;

   ---------------------------------------------------------------------------
   --
   --  Return the end-of-field position in Data after "Starting_Index",
   --  assuming that fields are separated by the Field_Separator.
   --  If there's no Field_Separator, return the end of the Data.
   --
   --  Source          :  String to search in
   --  Field_Separator :  Field seperator.
   --  Starting_At     :  Start search at.
   --
   function Field_End (
      Source          : in String;
      Field_Separator : in Character;
      Starting_At     : Positive)
      return            Natural
   is
   begin
      for I in Starting_At .. Source'Last loop
         if Source (I) = Field_Separator then
            return I - 1;
         end if;
      end loop;
      return Source'Last;
   end Field_End;

   ---------------------------------------------------------------------------
   --
   --  Hash function for booch components.
   --
   --  String to calculate a hash value form
   --
   function Hash (Key : String) return Natural is
      Retval : Natural          := Natural'First;
      Len    : constant Natural := Key'Length;
   begin
      if Len /= 0 then
         Retval := (
             Character'Pos (Key (Key'First)) +
             Character'Pos (Key (Len)) +
             Character'Pos (Key (Len / 2)));
      end if;
      return Retval;
   end Hash;

   ---------------------------------------------------------------------------
   --
   --  Hash function for booch components.
   --
   --  String to calculate a hash value form
   --
   function Hash (Key : String) return Ada.Containers.Hash_Type is
      use type Ada.Containers.Hash_Type;

      Retval : Ada.Containers.Hash_Type := Ada.Containers.Hash_Type'First;
      Len    : constant Natural         := Key'Length;
   begin
      if Len /= 0 then
         Retval := (
             Character'Pos (Key (Key'First)) +
             Character'Pos (Key (Len)) +
             Character'Pos (Key (Len / 2)));
      end if;
      return Retval;
   end Hash;

   ---------------------------------------------------------------------------
   --
   --  Hash function for booch components.
   --
   --  String to calculate a hash value form
   --
   function Hash (Key : S_U.Unbounded_String) return Natural is
      use Ada.Strings.Unbounded;

      Retval : Natural          := Natural'First;
      Len    : constant Natural := Length (Key);
   begin
      if Len /= 0 then
         Retval := (
             Character'Pos (Element (Key, 1)) +
             Character'Pos (Element (Key, Len)) +
             Character'Pos (Element (Key, Len / 2)));
      end if;
      return Retval;
   end Hash;

   ---------------------------------------------------------------------------
   --
   --  String to calculate a hash value form
   --
   function Hash (Key : S_U.Unbounded_String) return Ada.Containers.Hash_Type is
      use type Ada.Containers.Hash_Type;
      use Ada.Strings.Unbounded;

      Retval : Ada.Containers.Hash_Type := Ada.Containers.Hash_Type'First;
      Len    : constant Natural         := Length (Key);
   begin
      if Len /= 0 then
         Retval := (
             Character'Pos (Element (Key, 1)) +
             Character'Pos (Element (Key, Len)) +
             Character'Pos (Element (Key, Len / 2)));
      end if;
      return Retval;
   end Hash;

   ---------------------------------------------------------------------------
   --
   --  Searches for all occurences of text "Search" and Inserts text "Insert"
   --  bevore when "Insert" is there.
   --
   --  Source   : String to be changed
   --  Search   : String we look for
   --  New_Item : String we want to insert
   --  Mapping  : Search mapping
   procedure Insert_All (
      Source   : in out S_U.Unbounded_String;
      Search   : in String;
      New_Item : in String;
      Mapping  : in Maps.Character_Mapping := Maps.Identity)
   is
      Count : Natural := Natural'First;
   begin
      Insert_All (
         Source   => Source,
         Search   => Search,
         New_Item => New_Item,
         Mapping  => Mapping,
         Count    => Count);
   end Insert_All;

   ---------------------------------------------------------------------------
   --
   --  Searches for all occurences of text "Search" and Inserts text "Insert"
   --  bevore when "Insert" is there.
   --
   --  Source   : String to be changed
   --  Search   : String we look for
   --  New_Item : String we want to insert
   --  Mapping  : Search mapping
   --  Count    : Count of replaces done
   --
   procedure Insert_All (
      Source   : in out S_U.Unbounded_String;
      Search   : in String;
      New_Item : in String;
      Mapping  : in Maps.Character_Mapping := Maps.Identity;
      Count    : out Natural)
   is
      use Ada.Strings.Unbounded;
      use Ada.Strings.Fixed;

      --
      --  Offset from which we start. 0 means search from the first character
      --
      Offset : Natural := Natural'First;
      --
      --  Lenght of the full string
      --
      Len : constant Natural := Length (Source);
   begin
      --
      --  nothing found yet
      --
      Count := Natural'First;

      Search_Next : loop
         --
         --  Last Found Item was at the end of the String
         --
         exit Search_Next when Len = 0 or else Offset >= Len;

         Next_Sub_String : declare
            --
            --  We slice from the Offset on to the end. Here I have learned an
            --  important new Ada lessons: Strings don't have a 'First of 1.
            --
            Sub_String : constant String :=
               Slice (Source => Source, Low => Offset + 1, High => Len);
            --
            --  We search for Pattern
            --
            High : constant Natural :=
               Index (
                  Source  => Sub_String,
                  Pattern => Search,
                  Going   => S.Forward,
                  Mapping => Mapping);
         begin
            --
            --  Exit Loop when we havn't found anything
            --
            exit Search_Next when High = 0;

            --  Ada.Text_IO.Put_Line ("New_Item'Length   = " & Natural'Image (New_Item'Length));
            --  Ada.Text_IO.Put_Line ("Sub_String'Length = " & Natural'Image (Sub_String'Length));
            --  Ada.Text_IO.Put_Line ("High              = " & Natural'Image (High));
            --  Ada.Text_IO.Put_Line ("High - NI'Len-1   = " & Natural'Image (High - New_Item'Length));
            --  Ada.Text_IO.Put_Line ("High - 1          = " & Natural'Image (High - 1));

            if New_Item'Length > High - Sub_String'First
              or else New_Item /=
                      Sub_String (High - New_Item'Length .. High - 1)
            then
               --
               --  We insert one character bevore the beginning of the found
               --  string when the new text does not allready follow. This can
               --  of course only happen when there are enouch characters
               --  behind the found string to contain the new text.
               --
               Insert (
                  Source   => Source,
                  Before   => High + Offset - Sub_String'First + 1,
                  New_Item => New_Item);
               --
               --  Next Element
               --
               Count := Natural'Succ (Count);
            end if;
            --
            --  We set the offset to the end of the found string.
            --
            Offset := Offset + High + Search'Length;
         end Next_Sub_String;
      end loop Search_Next;
   end Insert_All;

   ---------------------------------------------------------------------------
   --
   --  Searches for first occurence of text "Search" and Inserts text "Insert"
   --  bevore when "Insert" is there.
   --
   --  Source   : String to be changed
   --  Search   : String we look for
   --  New_Item : String we want to insert
   --  Mapping  : Search mapping
   --
   procedure Insert_First (
      Source   : in out S_U.Unbounded_String;
      Search   : in String;
      New_Item : in String;
      Mapping  : in Maps.Character_Mapping := Maps.Identity)
   is
      Found : Boolean := Boolean'First;
   begin
      Insert_Last (
         Source   => Source,
         Search   => Search,
         New_Item => New_Item,
         Mapping  => Mapping,
         Found    => Found);
   end Insert_First;

   ---------------------------------------------------------------------------
   --
   --  Searches for last occurence of text "Search" and Inserts text "Insert"
   --  bevore when "Insert" is there.
   --
   --  Source   : String to be changed
   --  Search   : String we look for
   --  New_Item : String we want to insert
   --  Mapping  : Search mapping
   --  Found    : Count of replaces done
   --
   procedure Insert_First (
      Source   : in out S_U.Unbounded_String;
      Search   : in String;
      New_Item : in String;
      Mapping  : in Maps.Character_Mapping := Maps.Identity;
      Found    : out Boolean)
   is
      use Ada.Strings.Unbounded;

      High : constant Natural := Index (
         Source  => Source,
         Pattern => Search,
         Going   => S.Forward,
         Mapping => Mapping);
   begin
      if High = 0 then
         Found := False;
      elsif New_Item'Length >= High
        or else New_Item /=
                Slice (
                    Source => Source,
                    Low    => High - New_Item'Length,
                    High   => High - 1)
      then
         Insert (Source => Source, Before => High, New_Item => New_Item);
         Found := True;
      else
         Found := False;
      end if;
   end Insert_First;

   ---------------------------------------------------------------------------
   --
   --  Searches for last occurence of text "Search" and Inserts text "Insert"
   --  bevore when "Insert" is there.
   --
   --  Source   : String to be changed
   --  Search   : String we look for
   --  New_Item : String we want to insert
   --  Mapping  : Search mapping
   --
   procedure Insert_Last (
      Source   : in out S_U.Unbounded_String;
      Search   : in String;
      New_Item : in String;
      Mapping  : in Maps.Character_Mapping := Maps.Identity)
   is
      Found : Boolean := Boolean'First;
   begin
      Insert_Last (
         Source   => Source,
         Search   => Search,
         New_Item => New_Item,
         Mapping  => Mapping,
         Found    => Found);
   end Insert_Last;

   ---------------------------------------------------------------------------
   --
   --  Searches for last occurence of text "Search" and Inserts text "Insert"
   --  bevore when "Insert" is there.
   --
   --  Source   : String to be changed
   --  Search   : String we look for
   --  New_Item : String we want to insert
   --  Mapping  : Search mapping
   --  Found    : Count of replaces done
   --
   procedure Insert_Last (
      Source   : in out S_U.Unbounded_String;
      Search   : in String;
      New_Item : in String;
      Mapping  : in Maps.Character_Mapping := Maps.Identity;
      Found    : out Boolean)
   is
      use Ada.Strings.Unbounded;

      High : constant Natural :=
         Index (
            Source  => Source,
            Pattern => Search,
            Going   => S.Backward,
            Mapping => Mapping);
   begin
      if High = 0 then
         Found := False;
      elsif New_Item'Length >= High or else
         New_Item /= Slice (
            Source => Source,
            Low    => High - New_Item'Length,
            High   => High - 1)
      then
         Insert (Source => Source, Before => High, New_Item => New_Item);
         Found := True;
      else
         Found := False;
      end if;
   end Insert_Last;

end AdaCL.Strings;

---------------------------------------------------------------- {{{ ----------
--  vim: set textwidth=0 nowrap tabstop=8 shiftwidth=3 softtabstop=3 expandtab :
--  vim: set filetype=ada fileencoding=utf-8 fileformat=unix foldmethod=expr :
--  vim: set spell spelllang=en_gb
