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

with Ada.Strings.Unbounded;
with Ada.Strings.Maps;
with Ada.Containers;

package AdaCL.Strings is
   --
   --  No, I have not created some new String class - yet. Just a few string
   --  tools.
   --

   pragma Preelaborate (AdaCL.Strings);

   subtype Hex_Digit is Natural range 0 .. 16#F#;

   ---
   --  Replace all Search with Replace
   --
   --  Source  : String to be changed
   --  Search  : String we look for
   --  Replace : String we want to have
   --  Mapping : Search mapping
   procedure Change_All (
      Source  : in out Ada.Strings.Unbounded.Unbounded_String;
      Search  : in String;
      Replace : in String;
      Mapping : in Ada.Strings.Maps.Character_Mapping := Ada.Strings.Maps.Identity);

   ---
   --  Replace all Search with Replace and Count how often it was done.
   --
   --  String to be changed
   --  String we look for
   --  String we want to have
   --  Search mapping
   --  Count of replaces done
   procedure Change_All (
      Source   : in out Ada.Strings.Unbounded.Unbounded_String;
      Search   : in String;
      Replace  : in String;
      Mapping  : in Ada.Strings.Maps.Character_Mapping := Ada.Strings.Maps.Identity;
      Count    : out Natural);

   ---
   --  Replace all Search with Replace
   --
   --  Source  : String to be changed
   --  Search  : String we look for
   --  Replace : String we want to have
   --  Mapping : Search mapping
   procedure Change_First (
      Source  : in out Ada.Strings.Unbounded.Unbounded_String;
      Search  : in String;
      Replace : in String;
      Mapping : in Ada.Strings.Maps.Character_Mapping := Ada.Strings.Maps.Identity);

   ---
   --  Replace First Search with Replace and return success flag.
   --
   --  Source  : String to be changed
   --  Search  : String we look for
   --  Replace : String we want to have
   --  Mapping : Search mapping
   --  Found   : Count of replaces done
   procedure Change_First (
      Source  : in out Ada.Strings.Unbounded.Unbounded_String;
      Search  : in String;
      Replace : in String;
      Mapping : in Ada.Strings.Maps.Character_Mapping := Ada.Strings.Maps.Identity;
      Found   : out Boolean);

   ---
   --  Replace Last Search with Replace
   --
   --  String we look for
   --  String to be changed
   --  String we want to have
   --  Search mapping
   procedure Change_Last (
   Source        : in out Ada.Strings.Unbounded.Unbounded_String;
      Search  : in String;
      Replace : in String;
      Mapping : in Ada.Strings.Maps.Character_Mapping := Ada.Strings.Maps.Identity);

   ---
   --  Replace Last Search with Replace and return success flag.
   --
   --  Source  : String to be changed
   --  Search  : String we look for
   --  Replace : String we want to have
   --  Mapping : Search mapping
   --  Found   : Count of replaces done
   procedure Change_Last (
      Source  : in out Ada.Strings.Unbounded.Unbounded_String;
      Search  : in String;
      Replace : in String;
      Mapping : in Ada.Strings.Maps.Character_Mapping := Ada.Strings.Maps.Identity;
      Found   : out Boolean);

   ---
   --  Searches for all occurences of text "Search" and Inserts text "Insert"
   --  after the found text but only when "Insert" is not allready there.
   --
   --  Source   : String to be changed
   --  Search   : String we look for
   --  New_Item : String we want to insert
   --  Mapping  : Search mapping
   procedure Append_All (
      Source   : in out Ada.Strings.Unbounded.Unbounded_String;
      Search   : in String;
      New_Item : in String;
      Mapping  : in Ada.Strings.Maps.Character_Mapping := Ada.Strings.Maps.Identity);

   ---
   --  Searches for all occurences of text "Search" and Inserts text "Insert"
   --  after the found text but only when "Insert" is not allready there.
   --
   --  Source   : String to be changed
   --  Search   : String we look for
   --  New_Item : String we want to insert
   --  Mapping  : Search mapping
   --  Count    : Count of replaces done
   procedure Append_All (
      Source   : in out Ada.Strings.Unbounded.Unbounded_String;
      Search   : in String;
      New_Item : in String;
      Mapping  : in Ada.Strings.Maps.Character_Mapping := Ada.Strings.Maps.Identity;
      Count    : out Natural);

   ---
   --  Searches for first occurence of text "Search" and Inserts text "Insert"
   --  after the found text but only when "Insert" is not allready there.
   --
   --  Source   : String to be changed
   --  Search   : String we look for
   --  New_Item : String we want to insert
   --  Mapping  : Search mapping
   procedure Append_First (
      Source   : in out Ada.Strings.Unbounded.Unbounded_String;
      Search   : in String;
      New_Item : in String;
      Mapping  : in Ada.Strings.Maps.Character_Mapping := Ada.Strings.Maps.Identity);

   ---
   --  Searches for first occurence of text "Search" and Inserts text "Insert"
   --  after the found text but only when "Insert" is not allready there.
   --
   --  Source   : String to be changed
   --  Search   : String we look for
   --  New_Item : String we want to insert
   --  Mapping  : Search mapping
   --  Found    : Count of replaces done
   procedure Append_First (
      Source   : in out Ada.Strings.Unbounded.Unbounded_String;
      Search   : in String;
      New_Item : in String;
      Mapping  : in Ada.Strings.Maps.Character_Mapping := Ada.Strings.Maps.Identity;
      Found    : out Boolean);

   ---
   --  Searches for last occurence of text "Search" and Inserts text "Insert"
   --  after the found text but only when "Insert" is not allready there.
   --
   --  Source   : String to be changed
   --  Search   : String we look for
   --  New_Item : String we want to insert
   --  Mapping  : Search mapping
   procedure Append_Last (
      Source   : in out Ada.Strings.Unbounded.Unbounded_String;
      Search   : in String;
      New_Item : in String;
      Mapping  : in Ada.Strings.Maps.Character_Mapping := Ada.Strings.Maps.Identity);

   ---
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
      Source   : in out Ada.Strings.Unbounded.Unbounded_String;
      Search   : in String;
      New_Item : in String;
      Mapping  : in Ada.Strings.Maps.Character_Mapping := Ada.Strings.Maps.Identity;
      Found    : out Boolean);

   ---
   --  Return the end-of-field position in Data after "Starting_Index",
   --  assuming that fields are separated by the Field_Separator. If there's
   --  no Field_Separator, return the end of the Data.
   --  Source          : String to search in
   --  Field_Separator : Field seperator.
   --  Starting_At     : Start search at.
   function Field_End (
      Source          : in String;
      Field_Separator : in Character;
      Starting_At     : Positive)
      return            Natural;
   pragma Pure_Function (Field_End);

   ---
   --  Searches for first occurence of text "Search" and Inserts text "Insert"
   --  bevore when "Insert" is there.
   --
   --  Source   : String to be changed
   --  Search   : String we look for
   --  New_Item : String we want to insert
   --  Mapping  : Search mapping
   procedure Insert_First (
      Source   : in out Ada.Strings.Unbounded.Unbounded_String;
      Search   : in String;
      New_Item : in String;
      Mapping  : in Ada.Strings.Maps.Character_Mapping := Ada.Strings.Maps.Identity);

   ---
   --  Searches for first occurence of text "Search" and Inserts text "Insert"
   --  bevore when "Insert" is there.
   --
   --  Source   : String to be changed
   --  Search   : String we look for
   --  New_Item : String we want to insert
   --  Mapping  : Search mapping
   --  Found    : Count of replaces done
   --
   procedure Insert_First (
      Source   : in out Ada.Strings.Unbounded.Unbounded_String;
      Search   : in String;
      New_Item : in String;
      Mapping  : in Ada.Strings.Maps.Character_Mapping := Ada.Strings.Maps.Identity;
      Found    : out Boolean);

   ---
   --  Searches for all occurences of text "Search" and Inserts text "Insert"
   --  bevore when "Insert" is there.
   --
   --  Source   : Search mapping
   --  Search   : String we want to insert
   --  New_Item : String we look for
   --  Mapping  : String to be changed
   --
   procedure Insert_All (
      Source   : in out Ada.Strings.Unbounded.Unbounded_String;
      Search   : in String;
      New_Item : in String;
      Mapping  : in Ada.Strings.Maps.Character_Mapping := Ada.Strings.Maps.Identity);

   ---
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
      Source   : in out Ada.Strings.Unbounded.Unbounded_String;
      Search   : in String;
      New_Item : in String;
      Mapping  : in Ada.Strings.Maps.Character_Mapping := Ada.Strings.Maps.Identity;
      Count    : out Natural);

   ---
   --  Searches for last occurence of text "Search" and Inserts text "Insert"
   --  bevore when "Insert" is there.
   --
   --  Source   : String to be changed
   --  Search   : String we look for
   --  New_Item : String we want to insert
   --  Mapping  : Search mapping
   procedure Insert_Last (
      Source   : in out Ada.Strings.Unbounded.Unbounded_String;
      Search   : in String;
      New_Item : in String;
      Mapping  : in Ada.Strings.Maps.Character_Mapping :=
      Ada.Strings.Maps.Identity);

   ---
   --  Searches for last occurence of text "Search" and Inserts text "Insert"
   --  bevore when "Insert" is there.
   --
   --  Source   : String to be changed
   --  Search   : String we look for
   --  New_Item : String we want to insert
   --  Mapping  : Search mapping
   --  Found    : Count of replaces done
   procedure Insert_Last (
      Source   : in out Ada.Strings.Unbounded.Unbounded_String;
      Search   : in String;
      New_Item : in String;
      Mapping  : in Ada.Strings.Maps.Character_Mapping := Ada.Strings.Maps.Identity;
      Found    : out Boolean);

   ---
   --  Hash function for booch components.
   --
   --  Key : String to calculate a hash value form
   function Hash (Key : String) return Natural;
   pragma Pure_Function (Hash);

   ---
   --  Hash function for booch components.
   --
   --  Key : String to calculate a hash value form
   function Hash (Key : Ada.Strings.Unbounded.Unbounded_String) return Natural;
   pragma Pure_Function (Hash);

   ---
   --  Hash function for Ada components.
   --
   --  Key : String to calculate a hash value form
   function Hash (Key : Ada.Strings.Unbounded.Unbounded_String) return Ada.Containers.Hash_Type;
   pragma Pure_Function (Hash);

   ---
   --  Hash function for Ada components.
   --
   --  Key : String to calculate a hash value form
   function Hash (Key : String) return Ada.Containers.Hash_Type;
   pragma Pure_Function (Hash);

   ---
   --  convert String into Integer
   --
   --  Image - String to to be shown as Integer
   function Value (Image : Ada.Strings.Unbounded.Unbounded_String) return Integer
   is (Integer'Value (Ada.Strings.Unbounded.To_String (Image)));
   pragma Inline (Value);
   pragma Pure_Function (Value);

   ---
   --  Given hex string, return its Value as a Natural.
   --
   --  Hex String without 16#...#.
   --
   function Hex_Value (Hex_String : in String) return Natural
   is (Natural'Value ("16#" & Hex_String & "#"));
   pragma Inline (Hex_Value);
   pragma Pure_Function (Hex_Value);

   ---
   --  Assumes ASCII (or at least continuity in 0..9 and A..F).
   --
   function To_Hex_Char (Number : Hex_Digit) return Character
   is (
      if Number < 10 then
         Character'Val (Number + Character'Pos ('0'))
      else
         Character'Val (Number + Character'Pos ('A') - 10));
   pragma Inline (To_Hex_Char);
   pragma Pure_Function (To_Hex_Char);

private

end AdaCL.Strings;

---------------------------------------------------------------- {{{ ----------
--  vim: set textwidth=0 nowrap tabstop=8 shiftwidth=3 softtabstop=3 expandtab :
--  vim: set filetype=ada fileencoding=utf-8 fileformat=unix foldmethod=expr :
--  vim: set spell spelllang=en_gb
