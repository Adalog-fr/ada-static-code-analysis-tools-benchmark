--  Abstract :
--
--  Stuff needed by Skip_To_Grammar and Test_Skip_To.
--
--  Copyright (C) 2017 - 2022 Stephen Leake All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with Skip_To_Grammar_LALR_Main;
with WisiToken.Parse.LR.Parser_No_Recover;
with WisiToken.Syntax_Trees;
with WisiToken.Text_IO_Trace;
package Test_Skip_To_Aux is

   Enable : Boolean;
   --  If True, the following procedures execute checks. If False, they do nothing.

   DOS_Line_Endings : Boolean := True;
   --  If True, input file has DOS line endings

   Test_Pass_Count : Integer := 0;

   Trace     : aliased WisiToken.Text_IO_Trace.Trace;
   User_Data : aliased WisiToken.Syntax_Trees.User_Data_Type;
   Parser    : WisiToken.Parse.LR.Parser_No_Recover.Parser := Skip_To_Grammar_LALR_Main.Create_Parser
     (Trace'Access, User_Data'Access, "skip_to_grammar_lalr_parse_table.txt");

   procedure Reset;
   --  Reset Test_Pass_Count and internal counts to 0, ready to test
   --  another input file.

   procedure Test_Declaration_0 (Nonterm : in WisiToken.Syntax_Trees.Valid_Node_Access);
   procedure Test_Compilation_Unit_0 (Nonterm : in WisiToken.Syntax_Trees.Valid_Node_Access);

end Test_Skip_To_Aux;
