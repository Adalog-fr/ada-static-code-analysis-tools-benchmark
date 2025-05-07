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

with GNAT.Source_Info;

---
--  Additional Asserts which produce more detailed diagnostic messages
--  Diagnostics will be reported via the Report_Assertion function. The Report function
--  is passed as a generic ot avoid dependence to AUnit.
--
generic

   type Discrete_Type is (<>);

package AdaCL.Assert.Discrete is
   ---
   --  Assert that a discrete value is equal a given value
   --
   --  Actual    : Actual value
   --  Expected  : Expected value
   --  Name      : Name of variable or function restlt.
   --  Source    : Source code
   --  Line      : Line number
   procedure Equal (
      Actual    : Discrete_Type;
      Expected  : Discrete_Type;
      Name      : String;
      Source    : String  := GNAT.Source_Info.File;
      Line      : Natural := GNAT.Source_Info.Line);

   ---
   --  Assert that a discrete value is greater a given value
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
      Line      : Natural := GNAT.Source_Info.Line);

   procedure Greater_Equal (
      Actual    : Discrete_Type;
      Expected  : Discrete_Type;
      Name      : String;
      Source    : String  := GNAT.Source_Info.File;
      Line      : Natural := GNAT.Source_Info.Line);

   ---
   --  Assert that a discrete value is less a given value
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
      Line      : Natural := GNAT.Source_Info.Line);

   ---
   --  Assert that a discrete value is less a given value
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
      Line      : Natural := GNAT.Source_Info.Line);

   ---
   --  Assert that an discrete value is inside range
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
      Line      : Natural := GNAT.Source_Info.Line);

end AdaCL.Assert.Discrete;

---------------------------------------------------------------- {{{ ----------
--  vim: set textwidth=0 nowrap tabstop=8 shiftwidth=3 softtabstop=3 expandtab :
--  vim: set filetype=ada fileencoding=utf-8 fileformat=unix foldmethod=marker :
--  vim: set spell spelllang=en_gb
