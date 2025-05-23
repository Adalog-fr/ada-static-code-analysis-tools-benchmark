--  Abstract:
--
--  see spec
--
--  Copyright (C) 2015, 2017 - 2022 Stephe Leake
--
--  This file is part of the WisiToken package.
--
--  The WisiToken package is free software; you can redistribute it
--  and/or modify it under the terms of the GNU General Public License
--  as published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. The WisiToken package is
--  distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
--  License for more details. You should have received a copy of the
--  GNU General Public License distributed with the WisiToken package;
--  see file GPL.txt. If not, write to the Free Software Foundation,
--  59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

pragma License (GPL);
with Parser_Run_Common;
with WisiToken.Parse;
with WisiToken.Text_IO_Trace;
procedure Gen_LR_Parser_Run
is
   Trace  : aliased WisiToken.Text_IO_Trace.Trace;
   Parser : WisiToken.Parse.Base_Parser'Class := Create_Parser
     (Trace'Unrestricted_Access, null, Language_Fixes, Language_Matching_Begin_Tokens, Language_String_ID_Set);
begin
   Parser_Run_Common (Parser);
end Gen_LR_Parser_Run;
