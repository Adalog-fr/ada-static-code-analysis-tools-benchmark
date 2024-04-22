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
package body AdaCL.Assert.Discrete is

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
      Actual    : Discrete_Type;
      Expected  : Discrete_Type;
      Name      : String;
      Source    : String  := GNAT.Source_Info.File;
      Line      : Natural := GNAT.Source_Info.Line)
   is
   begin
      if not (Actual = Expected) then
         Report_Assertion (
            Message   => "Discrete_Type «" & Name & "» " & Actual'Image & " is not equal to " & Expected'Image,
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
      Actual    : Discrete_Type;
      Expected  : Discrete_Type;
      Name      : String;
      Source    : String  := GNAT.Source_Info.File;
      Line      : Natural := GNAT.Source_Info.Line)
   is
   begin
      if not (Actual > Expected) then
         Report_Assertion (
            Message   => "Discrete_Type «" & Name & "» " &
                         Actual'Image & " is not greater then " & Expected'Image,
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
      Actual    : Discrete_Type;
      Expected  : Discrete_Type;
      Name      : String;
      Source    : String  := GNAT.Source_Info.File;
      Line      : Natural := GNAT.Source_Info.Line)
   is
   begin
      if not (Actual >= Expected) then
         Report_Assertion (
            Message   => "Discrete_Type «" & Name & "» " &
                         Actual'Image & " is not greater or equal then " & Expected'Image,
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
      Actual    : Discrete_Type;
      Expected  : Discrete_Type;
      Name      : String;
      Source    : String  := GNAT.Source_Info.File;
      Line      : Natural := GNAT.Source_Info.Line)
   is
   begin
      if not (Actual < Expected) then
         Report_Assertion (
            Message   => "Discrete_Type «" & Name & "» " & Actual'Image & " is not less then " & Expected'Image,
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
      Actual    : Discrete_Type;
      Expected  : Discrete_Type;
      Name      : String;
      Source    : String  := GNAT.Source_Info.File;
      Line      : Natural := GNAT.Source_Info.Line)
   is
   begin
      if not (Actual <= Expected) then
         Report_Assertion (
            Message   => "Discrete_Type «" & Name & "» " &
                         Actual'Image & " is not less or equal then " & Expected'Image,
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
      Actual    : Discrete_Type;
      First     : Discrete_Type;
      Last      : Discrete_Type;
      Name      : String;
      Source    : String  := GNAT.Source_Info.File;
      Line      : Natural := GNAT.Source_Info.Line)
   is
   begin
      if not (Actual in First .. Last) then
         Report_Assertion (
            Message   => "Discrete value" & Name & "» " & Actual'Image &
                         " is not in range " & First'Image & "…" & Last'Image,
            Source    => Source,
            Line      => Line);
      end if;
   end In_Range;
end AdaCL.Assert.Discrete;

---------------------------------------------------------------- {{{ ----------
--  vim: set textwidth=0 nowrap tabstop=8 shiftwidth=3 softtabstop=3 expandtab :
--  vim: set filetype=ada fileencoding=utf-8 fileformat=unix foldmethod=marker :
--  vim: set spell spelllang=en_gb
