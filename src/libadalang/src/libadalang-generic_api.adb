--
--  Copyright (C) 2014-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Unchecked_Conversion;

with System;

with Langkit_Support.Internal.Analysis;
use Langkit_Support.Internal.Analysis;
with Langkit_Support.Internal.Conversions;

with Libadalang.Generic_Impl;      use Libadalang.Generic_Impl;
with Libadalang.Implementation;
with Libadalang.Public_Converters; use Libadalang.Public_Converters;

package body Libadalang.Generic_API is

   Desc_Address : constant System.Address := Desc'Address
     with Export, External_Name => "Libadalang__language_id";

   package Lk_Convs renames Langkit_Support.Internal.Conversions;

   subtype Generic_Internal_Context is
     Langkit_Support.Internal.Analysis.Internal_Context;
   subtype Specific_Internal_Context is
     Libadalang.Implementation.Internal_Context;

   subtype Generic_Internal_Unit is
     Langkit_Support.Internal.Analysis.Internal_Unit;
   subtype Specific_Internal_Unit is
     Libadalang.Implementation.Internal_Unit;

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
     (Rule : Grammar_Rule) return Langkit_Support.Generic_API.Grammar_Rule_Ref
   is
   begin
      --  'Pos is 0-based whereas Grammar_Rule_Index is 1-based

      return From_Index (Self_Id, Grammar_Rule'Pos (Rule) + 1);
   end To_Generic_Grammar_Rule;

   -------------------------------
   -- From_Generic_Grammar_Rule --
   -------------------------------

   function From_Generic_Grammar_Rule
     (Rule : Langkit_Support.Generic_API.Grammar_Rule_Ref) return Grammar_Rule
   is
   begin
      if Rule = Langkit_Support.Generic_API.No_Grammar_Rule_Ref then
         raise Precondition_Failure
           with "null grammar rule";
      elsif Language (Rule) /= Self_Id then
         raise Precondition_Failure
           with "grammar rule belongs to another language";
      end if;

      --  'Pos is 0-based whereas Grammar_Rule_Index is 1-based

      return Grammar_Rule'Val (To_Index (Rule) - 1);
   end From_Generic_Grammar_Rule;

end Libadalang.Generic_API;
