--------------------------------------------------------------- {{{1 ----------
--  Description: Options setable by the Ada plugin
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

with Ada.Strings.Unbounded;
with AdaCL.Base;
with AdaCL.Limited_Base;
with GNAT.Source_Info;

---
--  Additional Asserts which produce more detailed diagnostic messages
--  Diagnostics will be reported via the Report_Assertion function. The Report function
--  is passed as a generic ot avoid dependence to AUnit.
--
generic

   with procedure Report_Assertion (
      Condition : Boolean := False;
      Message   : String;
      Source    : String;
      Line      : Natural);

package AdaCL.Assert is

   ---------- Integer ----------------------------------------- {{{1 ----------

   ---
   --  Assert that a integer is equal a given value
   --
   --  Actual    : Actual value
   --  Expected  : Expected value
   --  Name      : Name of variable or function restlt.
   --  Source    : Source code
   --  Line      : Line number
   procedure Equal (
      Actual    : Integer;
      Expected  : Integer;
      Name      : String;
      Source    : String  := GNAT.Source_Info.File;
      Line      : Natural := GNAT.Source_Info.Line);

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
      Line      : Natural := GNAT.Source_Info.Line);

   procedure Greater_Equal (
      Actual    : Integer;
      Expected  : Integer;
      Name      : String;
      Source    : String  := GNAT.Source_Info.File;
      Line      : Natural := GNAT.Source_Info.Line);

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
      Line      : Natural := GNAT.Source_Info.Line);

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
      Line      : Natural := GNAT.Source_Info.Line);

   ---
   --  Assert that an integer is inside range
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
      Line      : Natural := GNAT.Source_Info.Line);

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
      Line      : Natural := GNAT.Source_Info.Line);

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
      Line      : Natural := GNAT.Source_Info.Line);

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
      Line      : Natural := GNAT.Source_Info.Line);

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
      Line      : Natural := GNAT.Source_Info.Line);

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
      Line      : Natural := GNAT.Source_Info.Line);

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
      Line      : Natural := GNAT.Source_Info.Line);

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
      Line      : Natural := GNAT.Source_Info.Line);

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
      Line      : Natural := GNAT.Source_Info.Line);

   ---
   --  Assert that  AdaCL.Limited_Base.Object_Class is null
   --
   --  Actual    : Actual value
   --  Name      : Name of variable or function restlt.
   --  Source    : Source code
   --  Line      : Line number
   procedure Is_Null (
      Actual    : AdaCL.Limited_Base.Object_Class;
      Name      : String;
      Source    : String  := GNAT.Source_Info.File;
      Line      : Natural := GNAT.Source_Info.Line);

   ----------------------------------------------------------- }}}1 ----------

end AdaCL.Assert;

---------------------------------------------------------------- {{{ ----------
--  vim: set textwidth=0 nowrap tabstop=8 shiftwidth=3 softtabstop=3 expandtab :
--  vim: set filetype=ada fileencoding=utf-8 fileformat=unix foldmethod=marker :
--  vim: set spell spelllang=en_gb
