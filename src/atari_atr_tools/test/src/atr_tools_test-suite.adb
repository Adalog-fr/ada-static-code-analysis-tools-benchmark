--------------------------------------------------------------- {{{1 ----------
--  Copyright © 2023 … 2023 Martin Krischik «krischik@users.sourceforge.net»
-------------------------------------------------------------------------------
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

with AdaCL.Trace;
with AUnit;
with Atr_Tools_Test.Create_Floppy;
with Atr_Tools.File_Header.Test;
with Atr_Tools.File_Header;

package body Atr_Tools_Test.Suite is

   Result              : aliased AUnit.Test_Suites.Test_Suite;
   Create_Floppy_Tests : aliased Atr_Tools_Test.Create_Floppy.Test_Case;
   File_Header_Tests   : aliased Atr_Tools.File_Header.Test.Test_Case;

   function Suite return  AUnit.Test_Suites.Access_Test_Suite is
      Trace : AdaCL.Trace.Object := AdaCL.Trace.Function_Trace;
      pragma Unreferenced (Trace);

      use AUnit.Test_Suites;
   begin
      Add_Test (Result'Access, Create_Floppy_Tests'Access);
      Add_Test (Result'Access, File_Header_Tests'Access);

      return Result'Access;
   end Suite;

end  Atr_Tools_Test.Suite;

---------------------------------------------------------------- {{{ ----------
--  vim: set textwidth=0 nowrap tabstop=8 shiftwidth=3 softtabstop=3 expandtab :
--  vim: set filetype=ada fileencoding=utf-8 fileformat=unix foldmethod=marker :
--  vim: set spell spelllang=en_gb