--------------------------------------------------------------- {{{1 ----------
--  Description: Assertion with trace as reporting backend
--    Copyright: Copyright © 2023 … 2023 Martin Krischik
--      Licence: GNU General Public License
--   Maintainer: Martin Krischik «krischik@users.sourceforge.net»
--      Version: 5.11.0
-----------------------------------------------------------------------------
--  Copyright © 2023 … 2023 Martin Krischik «krischik@users.sourceforge.net»
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

with AdaCL.Assert;

---
--  Assertion with trace as reporting backend
--
package AdaCL.Trace.Assert is new AdaCL.Assert (AdaCL.Trace.Report_Assertion);

---------------------------------------------------------------- {{{ ----------
--  vim: set textwidth=0 nowrap tabstop=8 shiftwidth=3 softtabstop=3 expandtab :
--  vim: set filetype=ada fileencoding=utf-8 fileformat=unix foldmethod=expr :
--  vim: set spell spelllang=en_gb
