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

with Ada.Command_Line;
with AdaCL.Trace;
with Atr_Tools_Test.Suite;
with AUnit.Reporter.Text;
with AUnit.Run;

procedure Atr_Tools_Test.Main is
   Trace : AdaCL.Trace.Object := AdaCL.Trace.Function_Trace;
   pragma Unreferenced (Trace);

   use type AUnit.Status;

   function Run is new AUnit.Run.Test_Runner_With_Status (Atr_Tools_Test.Suite.Suite);

   Reporter   : AUnit.Reporter.Text.Text_Reporter;
   Status     : constant AUnit.Status := Run (Reporter);
   Trace_File : constant String       := "create_nop_test.out";
begin
   AdaCL.Trace.Write_To_File (Trace_File);
   AdaCL.Trace.Enable_Trace;

   Ada.Command_Line.Set_Exit_Status (
      if Status = AUnit.Success then
         Ada.Command_Line.Success
      else
         Ada.Command_Line.Failure);
end Atr_Tools_Test.Main;

---------------------------------------------------------------- {{{ ----------
--  vim: set textwidth=0 nowrap tabstop=8 shiftwidth=3 softtabstop=3 expandtab :
--  vim: set filetype=ada fileencoding=utf-8 fileformat=unix foldmethod=marker :
--  vim: set spell spelllang=en_gb
