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

with AUnit.Assertions;
with Ada.Directories;
with AdaCL.Assert.Discrete;
with AdaCL.Assert;
with Atr_Tools.File_Header;

--
--  Root Package for Atari Atr_Tools tests
--
package Atr_Tools_Test is
   package Assert             is new AdaCL.Assert    (Report_Assertion => AUnit.Assertions.Assert);
   package Assert_File_Size   is new Assert.Discrete (Discrete_Type    => Ada.Directories.File_Size);
   package Assert_Byte        is new Assert.Discrete (Discrete_Type    => Atr_Tools.File_Header.Byte);
   package Assert_Word        is new Assert.Discrete (Discrete_Type    => Atr_Tools.File_Header.Word);
   package Assert_Double_Word is new Assert.Discrete (Discrete_Type    => Atr_Tools.File_Header.Double_Word);
end Atr_Tools_Test;

---------------------------------------------------------------- {{{ ----------
--  vim: set textwidth=0 nowrap tabstop=8 shiftwidth=3 softtabstop=3 expandtab :
--  vim: set filetype=ada fileencoding=utf-8 fileformat=unix foldmethod=marker :
--  vim: set spell spelllang=en_gb
