
------------------------------------------------------------------------------
--                                                                          --
--                            GPR PROJECT PARSER                            --
--                                                                          --
--            Copyright (C) 2015-2022, Free Software Foundation, Inc.       --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

--  DO NOT EDIT THIS IS AN AUTOGENERATED FILE


with Gpr_Parser_Support.Generic_API.Analysis;
use Gpr_Parser_Support.Generic_API.Analysis;
with Gpr_Parser_Support.Text; use Gpr_Parser_Support.Text;
with Gpr_Parser_Support.Token_Data_Handlers;
use Gpr_Parser_Support.Token_Data_Handlers;

with Gpr_Parser.Common;         use Gpr_Parser.Common;
with Gpr_Parser.Implementation; use Gpr_Parser.Implementation;

--  Internal package: provide implementation helpers to switch between public
--  types and implementation ones.

private package Gpr_Parser.Private_Converters is

   function From_Generic (Token : Lk_Token) return Common.Token_Reference
     with Import, External_Name => "Gpr_Parser__from_generic_token";
   function To_Generic (Token : Common.Token_Reference) return Lk_Token
     with Import, External_Name => "Gpr_Parser__to_generic_token";
   --  See the corresponding exports in $.Common's body

   type Token_Reference_Wrapper is access function
     (Context : Internal_Context;
      TDH     : Token_Data_Handler_Access;
      Index   : Token_Or_Trivia_Index) return Token_Reference;
   Wrap_Token_Reference : Token_Reference_Wrapper;

   type Token_Context_Getter is access function
     (Token : Token_Reference) return Internal_Context;
   Get_Token_Context : Token_Context_Getter;

   type Token_Unit_Getter is access function
     (Token : Token_Reference) return Internal_Unit;
   Get_Token_Unit : Token_Unit_Getter;

   type Token_TDH_Getter is access function
     (Token : Token_Reference) return Token_Data_Handler_Access;
   Get_Token_TDH : Token_TDH_Getter;

   type Token_Index_Getter is access function
     (Token : Token_Reference) return Token_Or_Trivia_Index;
   Get_Token_Index : Token_Index_Getter;

   type Token_Text_Extractor is access procedure
     (Token         : Token_Data_Type;
      Source_Buffer : out Text_Cst_Access;
      First         : out Positive;
      Last          : out Natural);
   Extract_Token_Text : Token_Text_Extractor;

end Gpr_Parser.Private_Converters;
