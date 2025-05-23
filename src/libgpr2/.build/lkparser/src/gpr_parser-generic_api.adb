
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


with Ada.Unchecked_Conversion;

with System;

with Gpr_Parser_Support.Internal.Analysis;
use Gpr_Parser_Support.Internal.Analysis;
with Gpr_Parser_Support.Internal.Conversions;

with Gpr_Parser.Generic_Impl;      use Gpr_Parser.Generic_Impl;
with Gpr_Parser.Implementation;
with Gpr_Parser.Public_Converters; use Gpr_Parser.Public_Converters;

package body Gpr_Parser.Generic_API is

   Desc_Address : constant System.Address := Desc'Address
     with Export, External_Name => "Gpr_Parser__language_id";

   package Lk_Convs renames Gpr_Parser_Support.Internal.Conversions;

   subtype Generic_Internal_Context is
     Gpr_Parser_Support.Internal.Analysis.Internal_Context;
   subtype Specific_Internal_Context is
     Gpr_Parser.Implementation.Internal_Context;

   subtype Generic_Internal_Unit is
     Gpr_Parser_Support.Internal.Analysis.Internal_Unit;
   subtype Specific_Internal_Unit is
     Gpr_Parser.Implementation.Internal_Unit;

   pragma Warnings (Off, "possible aliasing problem for type");
   function "+" is new Ada.Unchecked_Conversion
     (Generic_Internal_Context, Specific_Internal_Context);
   function "+" is new Ada.Unchecked_Conversion
     (Specific_Internal_Context, Generic_Internal_Context);

   function "+" is new Ada.Unchecked_Conversion
     (Generic_Internal_Unit, Specific_Internal_Unit);
   function "+" is new Ada.Unchecked_Conversion
     (Specific_Internal_Unit, Generic_Internal_Unit);
   pragma Warnings (On, "possible aliasing problem for type");

   ------------------------
   -- To_Generic_Context --
   ------------------------

   function To_Generic_Context (Context : Analysis_Context) return Lk_Context
   is
      Ctx : constant Specific_Internal_Context := Unwrap_Context.all (Context);
   begin
      return Lk_Convs.Wrap_Context (Self_Id, +Ctx);
   end To_Generic_Context;

   --------------------------
   -- From_Generic_Context --
   --------------------------

   function From_Generic_Context
     (Context : Lk_Context) return Analysis_Context
   is
      Ctx : constant Generic_Internal_Context :=
        Lk_Convs.Unwrap_Context (Context);
   begin
      if Language (Context) /= Self_Id then
         raise Precondition_Failure with "context belongs to another language";
      end if;
      return Wrap_Context.all (+Ctx);
   end From_Generic_Context;

   ---------------------
   -- To_Generic_Unit --
   ---------------------

   function To_Generic_Unit (Unit : Analysis_Unit) return Lk_Unit is
      U : constant Specific_Internal_Unit := Unwrap_Unit.all (Unit);
   begin
      return Lk_Convs.Wrap_Unit (Self_Id, +U);
   end To_Generic_Unit;

   -----------------------
   -- From_Generic_Unit --
   -----------------------

   function From_Generic_Unit (Unit : Lk_Unit) return Analysis_Unit is
      U : constant Generic_Internal_Unit := Lk_Convs.Unwrap_Unit (Unit);
   begin
      if Language (Unit) /= Self_Id then
         raise Precondition_Failure with "unit belongs to another language";
      end if;
      return Wrap_Unit.all (+U);
   end From_Generic_Unit;

   -----------------------------
   -- To_Generic_Grammar_Rule --
   -----------------------------

   function To_Generic_Grammar_Rule
     (Rule : Grammar_Rule) return Gpr_Parser_Support.Generic_API.Grammar_Rule_Ref
   is
   begin
      --  'Pos is 0-based whereas Grammar_Rule_Index is 1-based

      return From_Index (Self_Id, Grammar_Rule'Pos (Rule) + 1);
   end To_Generic_Grammar_Rule;

   -------------------------------
   -- From_Generic_Grammar_Rule --
   -------------------------------

   function From_Generic_Grammar_Rule
     (Rule : Gpr_Parser_Support.Generic_API.Grammar_Rule_Ref) return Grammar_Rule
   is
   begin
      if Rule = Gpr_Parser_Support.Generic_API.No_Grammar_Rule_Ref then
         raise Precondition_Failure
           with "null grammar rule";
      elsif Language (Rule) /= Self_Id then
         raise Precondition_Failure
           with "grammar rule belongs to another language";
      end if;

      --  'Pos is 0-based whereas Grammar_Rule_Index is 1-based

      return Grammar_Rule'Val (To_Index (Rule) - 1);
   end From_Generic_Grammar_Rule;

end Gpr_Parser.Generic_API;
