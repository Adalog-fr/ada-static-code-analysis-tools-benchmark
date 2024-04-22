--------------------------------------------------------------- {{{1 ----------
--  Description: Informative asserts
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

---
--  Additional Asserts which produce more detailed diagnostic messages
--  Diagnostics will be reported via the Report_Assertion function. The Report function
--  is passed as a generic ot avoid dependence to AUnit.
--
package body AdaCL.Assert is

   ---------- Integer ----------------------------------------- {{{1 ----------

   ---
   --  Assert that a integer is equal a given value
   --
   --  Actual    : Actual value
   --  Expected  : Expected value
   --  Name      : Name of variable or function restlt.
   --  Source    : Source code
   --  Line      : Line number
   --
   procedure Equal (
      Actual    : Integer;
      Expected  : Integer;
      Name      : String;
      Source    : String  := GNAT.Source_Info.File;
      Line      : Natural := GNAT.Source_Info.Line)
   is
   begin
      if not (Actual = Expected) then
         Report_Assertion (
            Message   => "Integer «" & Name & "» " & Actual'Image & " is not equal to " & Expected'Image,
            Source    => Source,
            Line      => Line);
      end if;
   end Equal;

   ---
   --  Assert that a integer is greater a given value
   --
   --  Actual    : Actual value
   --  Expected  : Expected value
   --  Name      : Name of variable or function restlt.
   --  Source    : Source code
   --  Line      : Line number
   --
   procedure Greater (
      Actual    : Integer;
      Expected  : Integer;
      Name      : String;
      Source    : String  := GNAT.Source_Info.File;
      Line      : Natural := GNAT.Source_Info.Line)
   is
   begin
      if not (Actual > Expected) then
         Report_Assertion (
            Message   => "Integer «" & Name & "» " & Actual'Image & " is not greater then " & Expected'Image,
            Source    => Source,
            Line      => Line);
      end if;
   end Greater;

   ---
   --  Assert that a integer is greater or equal a given value
   --
   --  Actual    : Actual value
   --  Expected  : Expected value
   --  Name      : Name of variable or function restlt.
   --  Source    : Source code
   --  Line      : Line number
   --
   procedure Greater_Equal (
      Actual    : Integer;
      Expected  : Integer;
      Name      : String;
      Source    : String  := GNAT.Source_Info.File;
      Line      : Natural := GNAT.Source_Info.Line)
   is
   begin
      if not (Actual >= Expected) then
         Report_Assertion (
            Message   => "Integer «" & Name & "» " & Actual'Image & " is not greater or equal then " & Expected'Image,
            Source    => Source,
            Line      => Line);
      end if;
   end Greater_Equal;

   ---
   --  Assert that a integer is less a given value
   --
   --  Actual    : Actual value
   --  Expected  : Expected value
   --  Name      : Name of variable or function restlt.
   --  Source    : Source code
   --  Line      : Line number
   --
   procedure Less (
      Actual    : Integer;
      Expected  : Integer;
      Name      : String;
      Source    : String  := GNAT.Source_Info.File;
      Line      : Natural := GNAT.Source_Info.Line)
   is
   begin
      if not (Actual < Expected) then
         Report_Assertion (
            Message   => "Integer «" & Name & "» " & Actual'Image & " is not less then " & Expected'Image,
            Source    => Source,
            Line      => Line);
      end if;
   end Less;

   ---
   --  Assert that a integer is less a given value
   --
   --  Actual    : Actual value
   --  Expected  : Expected value
   --  Name      : Name of variable or function restlt.
   --  Source    : Source code
   --  Line      : Line number
   --
   procedure Less_Equal (
      Actual    : Integer;
      Expected  : Integer;
      Name      : String;
      Source    : String  := GNAT.Source_Info.File;
      Line      : Natural := GNAT.Source_Info.Line)
   is
   begin
      if not (Actual <= Expected) then
         Report_Assertion (
            Message   => "Integer «" & Name & "» " & Actual'Image & " is not less or equal then " & Expected'Image,
            Source    => Source,
            Line      => Line);
      end if;
   end Less_Equal;

   ---
   --  Assert that a integer is inside range
   --
   --  Actual    : Actual value
   --  First     : Expected minimum value
   --  Last      : Expected maximum value
   --  Name      : Name of variable or function restlt.
   --  Source    : Source code
   --  Line      : Line number
   procedure In_Range (
      Actual    : Integer;
      First     : Integer;
      Last      : Integer;
      Name      : String;
      Source    : String  := GNAT.Source_Info.File;
      Line      : Natural := GNAT.Source_Info.Line)
   is
   begin
      if not (Actual in First .. Last) then
         Report_Assertion (
            Message   => "Integer «" & Name & "» " & Actual'Image &
                         " is not in range " & First'Image & "…" & Last'Image,
            Source    => Source,
            Line      => Line);
      end if;
   end In_Range;

   ---------- String  ----------------------------------------- {{{1 ----------

   ---
   --  Assert that a string is equal a given value
   --
   --  Actual    : Actual value
   --  Expected  : Expected value
   --  Name      : Name of variable or function restlt.
   --  Source    : Source code
   --  Line      : Line number
   --
   procedure Equal (
      Actual    : String;
      Expected  : String;
      Name      : String;
      Source    : String  := GNAT.Source_Info.File;
      Line      : Natural := GNAT.Source_Info.Line)
   is
   begin
      if not (Actual = Expected) then
         Report_Assertion (
            Message   => "String «" & Name & "» " & Actual'Image & " is not equal to " & Expected'Image,
            Source    => Source,
            Line      => Line);
      end if;
   end Equal;

   ---
   --  Assert that a string is of given length
   --
   --  Actual    : Actual value
   --  Expected  : Expected value
   --  Name      : Name of variable or function restlt.
   --  Source    : Source code
   --  Line      : Line number
   --
   procedure Lenght (
      Actual    : String;
      Expected  : Natural;
      Name      : String;
      Source    : String  := GNAT.Source_Info.File;
      Line      : Natural := GNAT.Source_Info.Line)
   is
   begin
      if not (Actual'Length = Expected) then
         Report_Assertion (
            Message   => "String «" & Name & "» " & Actual'Length'Image & " isn't of Length to " & Expected'Image,
            Source    => Source,
            Line      => Line);
      end if;
   end Lenght;

   ---------- Unbounded_String -------------------------------- {{{1 ----------

   ---
   --  Assert that a unbounded string is equal a given value
   --
   --  Actual    : Actual value
   --  Expected  : Expected value
   --  Name      : Name of variable or function restlt.
   --  Source    : Source code
   --  Line      : Line number
   --
   procedure Equal (
      Actual    : Ada.Strings.Unbounded.Unbounded_String;
      Expected  : String;
      Name      : String;
      Source    : String  := GNAT.Source_Info.File;
      Line      : Natural := GNAT.Source_Info.Line)
   is
      use Ada.Strings.Unbounded;
   begin
      if not (Actual = Expected) then
         Report_Assertion (
            Message   => "String «" & Name & "» " & Actual'Image & " is not equal to " & Expected'Image,
            Source    => Source,
            Line      => Line);
      end if;
   end Equal;

   ---
   --  Assert that a unbounded string is equal a given value
   --
   --  Actual    : Actual value
   --  Expected  : Expected value
   --  Name      : Name of variable or function restlt.
   --  Source    : Source code
   --  Line      : Line number
   --
   procedure Equal (
      Actual    : Ada.Strings.Unbounded.Unbounded_String;
      Expected  : Ada.Strings.Unbounded.Unbounded_String;
      Name      : String;
      Source    : String  := GNAT.Source_Info.File;
      Line      : Natural := GNAT.Source_Info.Line)
   is
      use Ada.Strings.Unbounded;
   begin
      if Actual /= Expected then
         Report_Assertion (
            Message   => "String «" & Name & "» " & Actual'Image & " is not equal to " & Expected'Image,
            Source    => Source,
            Line      => Line);
      end if;
   end Equal;

   ---
   --  Assert that a string is of given length
   --
   --  Actual    : Actual value
   --  Expected  : Expected value
   --  Name      : Name of variable or function restlt.
   --  Source    : Source code
   --  Line      : Line number
   --
   procedure Lenght (
      Actual    : Ada.Strings.Unbounded.Unbounded_String;
      Expected  : Natural;
      Name      : String;
      Source    : String  := GNAT.Source_Info.File;
      Line      : Natural := GNAT.Source_Info.Line)
   is
      use Ada.Strings.Unbounded;
   begin
      if not (Length (Actual) = Expected) then
         Report_Assertion (
            Message   => "String «" & Name & "» " & Length (Actual)'Image & " isn't of Length to " & Expected'Image,
            Source    => Source,
            Line      => Line);
      end if;
   end Lenght;

   ---------- Boolean ----------------------------------------- {{{1 ----------

   ---
   --  Assert that Boolean is true
   --
   --  Actual    : Actual value
   --  Name      : Name of variable or function restlt.
   --  Source    : Source code
   --  Line      : Line number
   procedure Is_True (
      Actual    : Boolean;
      Name      : String;
      Source    : String  := GNAT.Source_Info.File;
      Line      : Natural := GNAT.Source_Info.Line)
   is
   begin
      if not Actual then
         Report_Assertion (
            Message   => "AdaCL.Base.Object_Class «" & Name & "» " & Actual'Image & " is not true",
            Source    => Source,
            Line      => Line);
      end if;
   end Is_True;

   ---
   --  Assert that Boolean is false
   --
   --  Actual    : Actual value
   --  Name      : Name of variable or function restlt.
   --  Source    : Source code
   --  Line      : Line number
   procedure Is_False (
      Actual    : Boolean;
      Name      : String;
      Source    : String  := GNAT.Source_Info.File;
      Line      : Natural := GNAT.Source_Info.Line)
   is
   begin
      if not Actual then
         Report_Assertion (
            Message   => "AdaCL.Base.Object_Class «" & Name & "» " & Actual'Image & " is not false",
            Source    => Source,
            Line      => Line);
      end if;
   end Is_False;

   ---------- Access ----------------------------------------- {{{1 ----------

   ---
   --  Assert that  AdaCL.Base.Object_Class is null
   --
   --  Actual    : Actual value
   --  Name      : Name of variable or function restlt.
   --  Source    : Source code
   --  Line      : Line number
   procedure Is_Null (
      Actual    : AdaCL.Base.Object_Class;
      Name      : String;
      Source    : String  := GNAT.Source_Info.File;
      Line      : Natural := GNAT.Source_Info.Line)
   is
      use type AdaCL.Base.Object_Class;
   begin
      if Actual /= null then
         Report_Assertion (
            Message   => "AdaCL.Base.Object_Class «" & Name & "» " & Actual'Image & " is not null",
            Source    => Source,
            Line      => Line);
      end if;
   end Is_Null;

   ---
   --  Assert that AdaCL.Limited_Base.Object_Class is null
   --
   --  Actual    : Actual value
   --  Name      : Name of variable or function restlt.
   --  Source    : Source code
   --  Line      : Line number
   procedure Is_Null (
      Actual    : AdaCL.Limited_Base.Object_Class;
      Name      : String;
      Source    : String  := GNAT.Source_Info.File;
      Line      : Natural := GNAT.Source_Info.Line)
   is
      use type AdaCL.Limited_Base.Object_Class;
   begin
      if Actual /= null then
         Report_Assertion (
            Message   => "AdaCL.Limited_Base.Object_Class «" & Name & "» " & Actual'Image & " is not null",
            Source    => Source,
            Line      => Line);
      end if;
   end Is_Null;

   ----------------------------------------------------------- }}}1 ----------

end AdaCL.Assert;

---------------------------------------------------------------- {{{ ----------
--  vim: set textwidth=0 nowrap tabstop=8 shiftwidth=3 softtabstop=3 expandtab :
--  vim: set filetype=ada fileencoding=utf-8 fileformat=unix foldmethod=marker :
--  vim: set spell spelllang=en_gb
