--------------------------------------------------------------- {{{1 ----------
--  Copyright © 2023 … 2023 Martin Krischik «krischik@users.sourceforge.net»
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

with AdaCL.Reference.Element.Test;
with AdaCL.Reference.Element;
with AdaCL.Strings.Test;
with AdaCL.Trace.Test;
with AdaCL_Test.Reference_Holder;
with AUnit;

package body AdaCL_Test.Suite is

   Result                  : aliased AUnit.Test_Suites.Test_Suite;
   Reference_Element_Test  : aliased AdaCL.Reference.Element.Test.Test_Case;
   Reference_Holder_Test   : aliased Reference_Holder.Test_Case;
   Strings_Tests           : aliased AdaCL.Strings.Test.Test_Case;
   Trace_Tests             : aliased AdaCL.Trace.Test.Test_Case;

   function Suite return  AUnit.Test_Suites.Access_Test_Suite is
      use AUnit.Test_Suites;
   begin
      Add_Test (Result'Access, Reference_Element_Test'Access);
      Add_Test (Result'Access, Reference_Holder_Test'Access);
      Add_Test (Result'Access, Strings_Tests'Access);
      Add_Test (Result'Access, Trace_Tests'Access);

      return Result'Access;
   end Suite;

end  AdaCL_Test.Suite;

---------------------------------------------------------------- {{{ ----------
--  vim: set textwidth=0 nowrap tabstop=8 shiftwidth=3 softtabstop=3 expandtab :
--  vim: set filetype=ada fileencoding=utf-8 fileformat=unix foldmethod=expr :
--  vim: set spell spelllang=en_gb
