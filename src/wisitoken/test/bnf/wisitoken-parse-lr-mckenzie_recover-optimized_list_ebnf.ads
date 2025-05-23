--  Abstract :
--
--  Optimized_List language specific subprograms for McKenzie_Recover
--
--  Copyright (C) 2022 Stephen Leake All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (Modified_GPL);

with WisiToken.Parse.LR.McKenzie_Recover.Base;
package WisiToken.Parse.LR.McKenzie_Recover.Optimized_List_Ebnf is

   procedure Fixes
     (Super             : in out Base.Supervisor;
      Shared_Parser     : in out Parser.Parser;
      Parser_Index      : in     SAL.Peek_Type;
      Local_Config_Heap : in out Config_Heaps.Heap_Type;
      Config            : in     Configuration)
   is null;

   procedure Matching_Begin_Tokens
     (Super                   :         in out Base.Supervisor;
      Shared_Parser           :         in out Parser.Parser;
      Tokens                  :         in     Token_ID_Array_1_3;
      Config                  : aliased in     Configuration;
      Matching_Tokens         :         in out Token_ID_Arrays.Vector;
      Forbid_Minimal_Complete :         in out Boolean)
   is null;

   function String_ID_Set
     (Descriptor        : in WisiToken.Descriptor;
      String_Literal_ID : in Token_ID)
     return Token_ID_Set;

end WisiToken.Parse.LR.McKenzie_Recover.Optimized_List_Ebnf;
