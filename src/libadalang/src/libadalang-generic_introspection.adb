--
--  Copyright (C) 2014-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

pragma Warnings (Off, "referenced");
with Langkit_Support.Internal.Analysis; use Langkit_Support.Internal.Analysis;
with Langkit_Support.Internal.Conversions;
use Langkit_Support.Internal.Conversions;

with Libadalang.Implementation;
with Libadalang.Generic_API;       use Libadalang.Generic_API;
with Libadalang.Generic_Impl;      use Libadalang.Generic_Impl;
with Libadalang.Public_Converters; use Libadalang.Public_Converters;
with Libadalang.Private_Converters;
use Libadalang.Private_Converters;
pragma Warnings (On, "referenced");

package body Libadalang.Generic_Introspection is

   


      

      ---------
      -- "=" --
      ---------

      overriding function "=" (Left, Right : Internal_Rec_Analysis_Unit_Kind) return Boolean is
      begin
         return Left.Value = Right.Value;
      end "=";

      -------------
      -- Type_Of --
      -------------

      overriding function Type_Of (Value : Internal_Rec_Analysis_Unit_Kind) return Type_Index is
      begin
         return Type_Index_For_Analysis_Unit_Kind;
      end Type_Of;

      -----------
      -- Image --
      -----------

      overriding function Image (Value : Internal_Rec_Analysis_Unit_Kind) return String is
      begin
         return "Analysis_Unit_Kind(" & Value.Value'Image & ")";
      end Image;

      -----------------
      -- Value_Index --
      -----------------

      overriding function Value_Index (Value : Internal_Rec_Analysis_Unit_Kind) return Enum_Value_Index
      is
      begin
         return Analysis_Unit_Kind'Pos (Value.Value) + 1;
      end Value_Index;


      

      ---------
      -- "=" --
      ---------

      overriding function "=" (Left, Right : Internal_Rec_Lookup_Kind) return Boolean is
      begin
         return Left.Value = Right.Value;
      end "=";

      -------------
      -- Type_Of --
      -------------

      overriding function Type_Of (Value : Internal_Rec_Lookup_Kind) return Type_Index is
      begin
         return Type_Index_For_Lookup_Kind;
      end Type_Of;

      -----------
      -- Image --
      -----------

      overriding function Image (Value : Internal_Rec_Lookup_Kind) return String is
      begin
         return "Lookup_Kind(" & Value.Value'Image & ")";
      end Image;

      -----------------
      -- Value_Index --
      -----------------

      overriding function Value_Index (Value : Internal_Rec_Lookup_Kind) return Enum_Value_Index
      is
      begin
         return Lookup_Kind'Pos (Value.Value) + 1;
      end Value_Index;


      

      ---------
      -- "=" --
      ---------

      overriding function "=" (Left, Right : Internal_Rec_Designated_Env_Kind) return Boolean is
      begin
         return Left.Value = Right.Value;
      end "=";

      -------------
      -- Type_Of --
      -------------

      overriding function Type_Of (Value : Internal_Rec_Designated_Env_Kind) return Type_Index is
      begin
         return Type_Index_For_Designated_Env_Kind;
      end Type_Of;

      -----------
      -- Image --
      -----------

      overriding function Image (Value : Internal_Rec_Designated_Env_Kind) return String is
      begin
         return "Designated_Env_Kind(" & Value.Value'Image & ")";
      end Image;

      -----------------
      -- Value_Index --
      -----------------

      overriding function Value_Index (Value : Internal_Rec_Designated_Env_Kind) return Enum_Value_Index
      is
      begin
         return Designated_Env_Kind'Pos (Value.Value) + 1;
      end Value_Index;


      

      ---------
      -- "=" --
      ---------

      overriding function "=" (Left, Right : Internal_Rec_Ref_Result_Kind) return Boolean is
      begin
         return Left.Value = Right.Value;
      end "=";

      -------------
      -- Type_Of --
      -------------

      overriding function Type_Of (Value : Internal_Rec_Ref_Result_Kind) return Type_Index is
      begin
         return Type_Index_For_Ref_Result_Kind;
      end Type_Of;

      -----------
      -- Image --
      -----------

      overriding function Image (Value : Internal_Rec_Ref_Result_Kind) return String is
      begin
         return "Ref_Result_Kind(" & Value.Value'Image & ")";
      end Image;

      -----------------
      -- Value_Index --
      -----------------

      overriding function Value_Index (Value : Internal_Rec_Ref_Result_Kind) return Enum_Value_Index
      is
      begin
         return Ref_Result_Kind'Pos (Value.Value) + 1;
      end Value_Index;


      

      ---------
      -- "=" --
      ---------

      overriding function "=" (Left, Right : Internal_Rec_Call_Expr_Kind) return Boolean is
      begin
         return Left.Value = Right.Value;
      end "=";

      -------------
      -- Type_Of --
      -------------

      overriding function Type_Of (Value : Internal_Rec_Call_Expr_Kind) return Type_Index is
      begin
         return Type_Index_For_Call_Expr_Kind;
      end Type_Of;

      -----------
      -- Image --
      -----------

      overriding function Image (Value : Internal_Rec_Call_Expr_Kind) return String is
      begin
         return "Call_Expr_Kind(" & Value.Value'Image & ")";
      end Image;

      -----------------
      -- Value_Index --
      -----------------

      overriding function Value_Index (Value : Internal_Rec_Call_Expr_Kind) return Enum_Value_Index
      is
      begin
         return Call_Expr_Kind'Pos (Value.Value) + 1;
      end Value_Index;


      

      ---------
      -- "=" --
      ---------

      overriding function "=" (Left, Right : Internal_Rec_Grammar_Rule) return Boolean is
      begin
         return Left.Value = Right.Value;
      end "=";

      -------------
      -- Type_Of --
      -------------

      overriding function Type_Of (Value : Internal_Rec_Grammar_Rule) return Type_Index is
      begin
         return Type_Index_For_Grammar_Rule;
      end Type_Of;

      -----------
      -- Image --
      -----------

      overriding function Image (Value : Internal_Rec_Grammar_Rule) return String is
      begin
         return "Grammar_Rule(" & Value.Value'Image & ")";
      end Image;

      -----------------
      -- Value_Index --
      -----------------

      overriding function Value_Index (Value : Internal_Rec_Grammar_Rule) return Enum_Value_Index
      is
      begin
         return Grammar_Rule'Pos (Value.Value) + 1;
      end Value_Index;


   -----------------
   -- Create_Enum --
   -----------------

   function Create_Enum
     (Enum_Type   : Type_Index;
      Value_Index : Enum_Value_Index) return Internal_Value_Access
   is
   begin
      case Enum_Type is
            when Type_Index_For_Analysis_Unit_Kind =>
               declare
                  Result : constant Internal_Acc_Analysis_Unit_Kind :=
                    new Internal_Rec_Analysis_Unit_Kind;
               begin
                  Result.Value := Analysis_Unit_Kind'Val (Value_Index - 1);
                  return Internal_Value_Access (Result);
               end;
            when Type_Index_For_Lookup_Kind =>
               declare
                  Result : constant Internal_Acc_Lookup_Kind :=
                    new Internal_Rec_Lookup_Kind;
               begin
                  Result.Value := Lookup_Kind'Val (Value_Index - 1);
                  return Internal_Value_Access (Result);
               end;
            when Type_Index_For_Designated_Env_Kind =>
               declare
                  Result : constant Internal_Acc_Designated_Env_Kind :=
                    new Internal_Rec_Designated_Env_Kind;
               begin
                  Result.Value := Designated_Env_Kind'Val (Value_Index - 1);
                  return Internal_Value_Access (Result);
               end;
            when Type_Index_For_Ref_Result_Kind =>
               declare
                  Result : constant Internal_Acc_Ref_Result_Kind :=
                    new Internal_Rec_Ref_Result_Kind;
               begin
                  Result.Value := Ref_Result_Kind'Val (Value_Index - 1);
                  return Internal_Value_Access (Result);
               end;
            when Type_Index_For_Call_Expr_Kind =>
               declare
                  Result : constant Internal_Acc_Call_Expr_Kind :=
                    new Internal_Rec_Call_Expr_Kind;
               begin
                  Result.Value := Call_Expr_Kind'Val (Value_Index - 1);
                  return Internal_Value_Access (Result);
               end;
            when Type_Index_For_Grammar_Rule =>
               declare
                  Result : constant Internal_Acc_Grammar_Rule :=
                    new Internal_Rec_Grammar_Rule;
               begin
                  Result.Value := Grammar_Rule'Val (Value_Index - 1);
                  return Internal_Value_Access (Result);
               end;

         when others =>
            --  Validation in public wrappers is supposed to prevent calling
            --  this function on non-enum types.
            raise Program_Error;
      end case;
   end Create_Enum;

      

      ---------
      -- "=" --
      ---------

      overriding function "=" (Left, Right : Internal_Rec_Discriminant_Values_Array) return Boolean is
      begin
         return Left.Value.all = Right.Value.all;
      end "=";

      -------------
      -- Destroy --
      -------------

      overriding procedure Destroy (Value : in out Internal_Rec_Discriminant_Values_Array) is
      begin
         Free (Value.Value);
      end Destroy;

      -------------
      -- Type_Of --
      -------------

      overriding function Type_Of (Value : Internal_Rec_Discriminant_Values_Array) return Type_Index is
      begin
         return Type_Index_For_Discriminant_Values_Array;
      end Type_Of;

      ------------------
      -- Array_Length --
      ------------------

      overriding function Array_Length (Value : Internal_Rec_Discriminant_Values_Array) return Natural is
      begin
         return Value.Value.all'Length;
      end Array_Length;

      ----------------
      -- Array_Item --
      ----------------

      overriding function Array_Item
        (Value : Internal_Rec_Discriminant_Values_Array; Index : Positive) return Internal_Value_Access
      is
         Item : Discriminant_Values renames Value.Value.all (Index);

         
            Result : Internal_Acc_Discriminant_Values :=  new Internal_Rec_Discriminant_Values;
      begin
            Result.Value := Item;
         return Internal_Value_Access (Result);
      end Array_Item;

      ------------------
      -- Create_Array --
      ------------------

      function Create_Array
        (Values : Internal_Value_Array) return Internal_Acc_Discriminant_Values_Array
      is
         Result_Index : Natural := 0;
      begin
         return Result : constant Internal_Acc_Discriminant_Values_Array := new Internal_Rec_Discriminant_Values_Array do
            Result.Value := new Discriminant_Values_Array (1 .. Values'Length);
            for I in Values'Range loop
               Result_Index := Result_Index + 1;
               declare
                  Result_Item : Discriminant_Values renames
                    Result.Value (Result_Index);
                  Value       : Internal_Rec_Discriminant_Values renames
                    Internal_Acc_Discriminant_Values (Values (I)).all;
               begin
                     Result_Item := Value.Value;
               end;
            end loop;
         end return;
      end Create_Array;

      

      ---------
      -- "=" --
      ---------

      overriding function "=" (Left, Right : Internal_Rec_Doc_Annotation_Array) return Boolean is
      begin
         return Left.Value.all = Right.Value.all;
      end "=";

      -------------
      -- Destroy --
      -------------

      overriding procedure Destroy (Value : in out Internal_Rec_Doc_Annotation_Array) is
      begin
         Free (Value.Value);
      end Destroy;

      -------------
      -- Type_Of --
      -------------

      overriding function Type_Of (Value : Internal_Rec_Doc_Annotation_Array) return Type_Index is
      begin
         return Type_Index_For_Doc_Annotation_Array;
      end Type_Of;

      ------------------
      -- Array_Length --
      ------------------

      overriding function Array_Length (Value : Internal_Rec_Doc_Annotation_Array) return Natural is
      begin
         return Value.Value.all'Length;
      end Array_Length;

      ----------------
      -- Array_Item --
      ----------------

      overriding function Array_Item
        (Value : Internal_Rec_Doc_Annotation_Array; Index : Positive) return Internal_Value_Access
      is
         Item : Doc_Annotation renames Value.Value.all (Index);

         
            Result : Internal_Acc_Doc_Annotation :=  new Internal_Rec_Doc_Annotation;
      begin
            Result.Value := Item;
         return Internal_Value_Access (Result);
      end Array_Item;

      ------------------
      -- Create_Array --
      ------------------

      function Create_Array
        (Values : Internal_Value_Array) return Internal_Acc_Doc_Annotation_Array
      is
         Result_Index : Natural := 0;
      begin
         return Result : constant Internal_Acc_Doc_Annotation_Array := new Internal_Rec_Doc_Annotation_Array do
            Result.Value := new Doc_Annotation_Array (1 .. Values'Length);
            for I in Values'Range loop
               Result_Index := Result_Index + 1;
               declare
                  Result_Item : Doc_Annotation renames
                    Result.Value (Result_Index);
                  Value       : Internal_Rec_Doc_Annotation renames
                    Internal_Acc_Doc_Annotation (Values (I)).all;
               begin
                     Result_Item := Value.Value;
               end;
            end loop;
         end return;
      end Create_Array;

      

      ---------
      -- "=" --
      ---------

      overriding function "=" (Left, Right : Internal_Rec_Accept_Stmt_Array) return Boolean is
      begin
         return Left.Value.all = Right.Value.all;
      end "=";

      -------------
      -- Destroy --
      -------------

      overriding procedure Destroy (Value : in out Internal_Rec_Accept_Stmt_Array) is
      begin
         Free (Value.Value);
      end Destroy;

      -------------
      -- Type_Of --
      -------------

      overriding function Type_Of (Value : Internal_Rec_Accept_Stmt_Array) return Type_Index is
      begin
         return Type_Index_For_Accept_Stmt_Array;
      end Type_Of;

      ------------------
      -- Array_Length --
      ------------------

      overriding function Array_Length (Value : Internal_Rec_Accept_Stmt_Array) return Natural is
      begin
         return Value.Value.all'Length;
      end Array_Length;

      ----------------
      -- Array_Item --
      ----------------

      overriding function Array_Item
        (Value : Internal_Rec_Accept_Stmt_Array; Index : Positive) return Internal_Value_Access
      is
         Item : Accept_Stmt renames Value.Value.all (Index);

         
            Result : Internal_Acc_Node :=  new Internal_Rec_Node;
      begin
            Set_Node (Result, Item);
         return Internal_Value_Access (Result);
      end Array_Item;

      ------------------
      -- Create_Array --
      ------------------

      function Create_Array
        (Values : Internal_Value_Array) return Internal_Acc_Accept_Stmt_Array
      is
         Result_Index : Natural := 0;
      begin
         return Result : constant Internal_Acc_Accept_Stmt_Array := new Internal_Rec_Accept_Stmt_Array do
            Result.Value := new Accept_Stmt_Array (1 .. Values'Length);
            for I in Values'Range loop
               Result_Index := Result_Index + 1;
               declare
                  Result_Item : Accept_Stmt renames
                    Result.Value (Result_Index);
                  Value       : Internal_Rec_Node renames
                    Internal_Acc_Node (Values (I)).all;
               begin
                     Result_Item := Get_Node (Value).As_Accept_Stmt;
               end;
            end loop;
         end return;
      end Create_Array;

      

      ---------
      -- "=" --
      ---------

      overriding function "=" (Left, Right : Internal_Rec_Ada_Node_Array) return Boolean is
      begin
         return Left.Value.all = Right.Value.all;
      end "=";

      -------------
      -- Destroy --
      -------------

      overriding procedure Destroy (Value : in out Internal_Rec_Ada_Node_Array) is
      begin
         Free (Value.Value);
      end Destroy;

      -------------
      -- Type_Of --
      -------------

      overriding function Type_Of (Value : Internal_Rec_Ada_Node_Array) return Type_Index is
      begin
         return Type_Index_For_Ada_Node_Array;
      end Type_Of;

      ------------------
      -- Array_Length --
      ------------------

      overriding function Array_Length (Value : Internal_Rec_Ada_Node_Array) return Natural is
      begin
         return Value.Value.all'Length;
      end Array_Length;

      ----------------
      -- Array_Item --
      ----------------

      overriding function Array_Item
        (Value : Internal_Rec_Ada_Node_Array; Index : Positive) return Internal_Value_Access
      is
         Item : Ada_Node renames Value.Value.all (Index);

         
            Result : Internal_Acc_Node :=  new Internal_Rec_Node;
      begin
            Set_Node (Result, Item);
         return Internal_Value_Access (Result);
      end Array_Item;

      ------------------
      -- Create_Array --
      ------------------

      function Create_Array
        (Values : Internal_Value_Array) return Internal_Acc_Ada_Node_Array
      is
         Result_Index : Natural := 0;
      begin
         return Result : constant Internal_Acc_Ada_Node_Array := new Internal_Rec_Ada_Node_Array do
            Result.Value := new Ada_Node_Array (1 .. Values'Length);
            for I in Values'Range loop
               Result_Index := Result_Index + 1;
               declare
                  Result_Item : Ada_Node renames
                    Result.Value (Result_Index);
                  Value       : Internal_Rec_Node renames
                    Internal_Acc_Node (Values (I)).all;
               begin
                     Result_Item := Get_Node (Value);
               end;
            end loop;
         end return;
      end Create_Array;

      

      ---------
      -- "=" --
      ---------

      overriding function "=" (Left, Right : Internal_Rec_Base_Formal_Param_Decl_Array) return Boolean is
      begin
         return Left.Value.all = Right.Value.all;
      end "=";

      -------------
      -- Destroy --
      -------------

      overriding procedure Destroy (Value : in out Internal_Rec_Base_Formal_Param_Decl_Array) is
      begin
         Free (Value.Value);
      end Destroy;

      -------------
      -- Type_Of --
      -------------

      overriding function Type_Of (Value : Internal_Rec_Base_Formal_Param_Decl_Array) return Type_Index is
      begin
         return Type_Index_For_Base_Formal_Param_Decl_Array;
      end Type_Of;

      ------------------
      -- Array_Length --
      ------------------

      overriding function Array_Length (Value : Internal_Rec_Base_Formal_Param_Decl_Array) return Natural is
      begin
         return Value.Value.all'Length;
      end Array_Length;

      ----------------
      -- Array_Item --
      ----------------

      overriding function Array_Item
        (Value : Internal_Rec_Base_Formal_Param_Decl_Array; Index : Positive) return Internal_Value_Access
      is
         Item : Base_Formal_Param_Decl renames Value.Value.all (Index);

         
            Result : Internal_Acc_Node :=  new Internal_Rec_Node;
      begin
            Set_Node (Result, Item);
         return Internal_Value_Access (Result);
      end Array_Item;

      ------------------
      -- Create_Array --
      ------------------

      function Create_Array
        (Values : Internal_Value_Array) return Internal_Acc_Base_Formal_Param_Decl_Array
      is
         Result_Index : Natural := 0;
      begin
         return Result : constant Internal_Acc_Base_Formal_Param_Decl_Array := new Internal_Rec_Base_Formal_Param_Decl_Array do
            Result.Value := new Base_Formal_Param_Decl_Array (1 .. Values'Length);
            for I in Values'Range loop
               Result_Index := Result_Index + 1;
               declare
                  Result_Item : Base_Formal_Param_Decl renames
                    Result.Value (Result_Index);
                  Value       : Internal_Rec_Node renames
                    Internal_Acc_Node (Values (I)).all;
               begin
                     Result_Item := Get_Node (Value).As_Base_Formal_Param_Decl;
               end;
            end loop;
         end return;
      end Create_Array;

      

      ---------
      -- "=" --
      ---------

      overriding function "=" (Left, Right : Internal_Rec_Base_Type_Decl_Array) return Boolean is
      begin
         return Left.Value.all = Right.Value.all;
      end "=";

      -------------
      -- Destroy --
      -------------

      overriding procedure Destroy (Value : in out Internal_Rec_Base_Type_Decl_Array) is
      begin
         Free (Value.Value);
      end Destroy;

      -------------
      -- Type_Of --
      -------------

      overriding function Type_Of (Value : Internal_Rec_Base_Type_Decl_Array) return Type_Index is
      begin
         return Type_Index_For_Base_Type_Decl_Array;
      end Type_Of;

      ------------------
      -- Array_Length --
      ------------------

      overriding function Array_Length (Value : Internal_Rec_Base_Type_Decl_Array) return Natural is
      begin
         return Value.Value.all'Length;
      end Array_Length;

      ----------------
      -- Array_Item --
      ----------------

      overriding function Array_Item
        (Value : Internal_Rec_Base_Type_Decl_Array; Index : Positive) return Internal_Value_Access
      is
         Item : Base_Type_Decl renames Value.Value.all (Index);

         
            Result : Internal_Acc_Node :=  new Internal_Rec_Node;
      begin
            Set_Node (Result, Item);
         return Internal_Value_Access (Result);
      end Array_Item;

      ------------------
      -- Create_Array --
      ------------------

      function Create_Array
        (Values : Internal_Value_Array) return Internal_Acc_Base_Type_Decl_Array
      is
         Result_Index : Natural := 0;
      begin
         return Result : constant Internal_Acc_Base_Type_Decl_Array := new Internal_Rec_Base_Type_Decl_Array do
            Result.Value := new Base_Type_Decl_Array (1 .. Values'Length);
            for I in Values'Range loop
               Result_Index := Result_Index + 1;
               declare
                  Result_Item : Base_Type_Decl renames
                    Result.Value (Result_Index);
                  Value       : Internal_Rec_Node renames
                    Internal_Acc_Node (Values (I)).all;
               begin
                     Result_Item := Get_Node (Value).As_Base_Type_Decl;
               end;
            end loop;
         end return;
      end Create_Array;

      

      ---------
      -- "=" --
      ---------

      overriding function "=" (Left, Right : Internal_Rec_Basic_Decl_Array) return Boolean is
      begin
         return Left.Value.all = Right.Value.all;
      end "=";

      -------------
      -- Destroy --
      -------------

      overriding procedure Destroy (Value : in out Internal_Rec_Basic_Decl_Array) is
      begin
         Free (Value.Value);
      end Destroy;

      -------------
      -- Type_Of --
      -------------

      overriding function Type_Of (Value : Internal_Rec_Basic_Decl_Array) return Type_Index is
      begin
         return Type_Index_For_Basic_Decl_Array;
      end Type_Of;

      ------------------
      -- Array_Length --
      ------------------

      overriding function Array_Length (Value : Internal_Rec_Basic_Decl_Array) return Natural is
      begin
         return Value.Value.all'Length;
      end Array_Length;

      ----------------
      -- Array_Item --
      ----------------

      overriding function Array_Item
        (Value : Internal_Rec_Basic_Decl_Array; Index : Positive) return Internal_Value_Access
      is
         Item : Basic_Decl renames Value.Value.all (Index);

         
            Result : Internal_Acc_Node :=  new Internal_Rec_Node;
      begin
            Set_Node (Result, Item);
         return Internal_Value_Access (Result);
      end Array_Item;

      ------------------
      -- Create_Array --
      ------------------

      function Create_Array
        (Values : Internal_Value_Array) return Internal_Acc_Basic_Decl_Array
      is
         Result_Index : Natural := 0;
      begin
         return Result : constant Internal_Acc_Basic_Decl_Array := new Internal_Rec_Basic_Decl_Array do
            Result.Value := new Basic_Decl_Array (1 .. Values'Length);
            for I in Values'Range loop
               Result_Index := Result_Index + 1;
               declare
                  Result_Item : Basic_Decl renames
                    Result.Value (Result_Index);
                  Value       : Internal_Rec_Node renames
                    Internal_Acc_Node (Values (I)).all;
               begin
                     Result_Item := Get_Node (Value).As_Basic_Decl;
               end;
            end loop;
         end return;
      end Create_Array;

      

      ---------
      -- "=" --
      ---------

      overriding function "=" (Left, Right : Internal_Rec_Compilation_Unit_Array) return Boolean is
      begin
         return Left.Value.all = Right.Value.all;
      end "=";

      -------------
      -- Destroy --
      -------------

      overriding procedure Destroy (Value : in out Internal_Rec_Compilation_Unit_Array) is
      begin
         Free (Value.Value);
      end Destroy;

      -------------
      -- Type_Of --
      -------------

      overriding function Type_Of (Value : Internal_Rec_Compilation_Unit_Array) return Type_Index is
      begin
         return Type_Index_For_Compilation_Unit_Array;
      end Type_Of;

      ------------------
      -- Array_Length --
      ------------------

      overriding function Array_Length (Value : Internal_Rec_Compilation_Unit_Array) return Natural is
      begin
         return Value.Value.all'Length;
      end Array_Length;

      ----------------
      -- Array_Item --
      ----------------

      overriding function Array_Item
        (Value : Internal_Rec_Compilation_Unit_Array; Index : Positive) return Internal_Value_Access
      is
         Item : Compilation_Unit renames Value.Value.all (Index);

         
            Result : Internal_Acc_Node :=  new Internal_Rec_Node;
      begin
            Set_Node (Result, Item);
         return Internal_Value_Access (Result);
      end Array_Item;

      ------------------
      -- Create_Array --
      ------------------

      function Create_Array
        (Values : Internal_Value_Array) return Internal_Acc_Compilation_Unit_Array
      is
         Result_Index : Natural := 0;
      begin
         return Result : constant Internal_Acc_Compilation_Unit_Array := new Internal_Rec_Compilation_Unit_Array do
            Result.Value := new Compilation_Unit_Array (1 .. Values'Length);
            for I in Values'Range loop
               Result_Index := Result_Index + 1;
               declare
                  Result_Item : Compilation_Unit renames
                    Result.Value (Result_Index);
                  Value       : Internal_Rec_Node renames
                    Internal_Acc_Node (Values (I)).all;
               begin
                     Result_Item := Get_Node (Value).As_Compilation_Unit;
               end;
            end loop;
         end return;
      end Create_Array;

      

      ---------
      -- "=" --
      ---------

      overriding function "=" (Left, Right : Internal_Rec_Defining_Name_Array) return Boolean is
      begin
         return Left.Value.all = Right.Value.all;
      end "=";

      -------------
      -- Destroy --
      -------------

      overriding procedure Destroy (Value : in out Internal_Rec_Defining_Name_Array) is
      begin
         Free (Value.Value);
      end Destroy;

      -------------
      -- Type_Of --
      -------------

      overriding function Type_Of (Value : Internal_Rec_Defining_Name_Array) return Type_Index is
      begin
         return Type_Index_For_Defining_Name_Array;
      end Type_Of;

      ------------------
      -- Array_Length --
      ------------------

      overriding function Array_Length (Value : Internal_Rec_Defining_Name_Array) return Natural is
      begin
         return Value.Value.all'Length;
      end Array_Length;

      ----------------
      -- Array_Item --
      ----------------

      overriding function Array_Item
        (Value : Internal_Rec_Defining_Name_Array; Index : Positive) return Internal_Value_Access
      is
         Item : Defining_Name renames Value.Value.all (Index);

         
            Result : Internal_Acc_Node :=  new Internal_Rec_Node;
      begin
            Set_Node (Result, Item);
         return Internal_Value_Access (Result);
      end Array_Item;

      ------------------
      -- Create_Array --
      ------------------

      function Create_Array
        (Values : Internal_Value_Array) return Internal_Acc_Defining_Name_Array
      is
         Result_Index : Natural := 0;
      begin
         return Result : constant Internal_Acc_Defining_Name_Array := new Internal_Rec_Defining_Name_Array do
            Result.Value := new Defining_Name_Array (1 .. Values'Length);
            for I in Values'Range loop
               Result_Index := Result_Index + 1;
               declare
                  Result_Item : Defining_Name renames
                    Result.Value (Result_Index);
                  Value       : Internal_Rec_Node renames
                    Internal_Acc_Node (Values (I)).all;
               begin
                     Result_Item := Get_Node (Value).As_Defining_Name;
               end;
            end loop;
         end return;
      end Create_Array;

      

      ---------
      -- "=" --
      ---------

      overriding function "=" (Left, Right : Internal_Rec_Expr_Array) return Boolean is
      begin
         return Left.Value.all = Right.Value.all;
      end "=";

      -------------
      -- Destroy --
      -------------

      overriding procedure Destroy (Value : in out Internal_Rec_Expr_Array) is
      begin
         Free (Value.Value);
      end Destroy;

      -------------
      -- Type_Of --
      -------------

      overriding function Type_Of (Value : Internal_Rec_Expr_Array) return Type_Index is
      begin
         return Type_Index_For_Expr_Array;
      end Type_Of;

      ------------------
      -- Array_Length --
      ------------------

      overriding function Array_Length (Value : Internal_Rec_Expr_Array) return Natural is
      begin
         return Value.Value.all'Length;
      end Array_Length;

      ----------------
      -- Array_Item --
      ----------------

      overriding function Array_Item
        (Value : Internal_Rec_Expr_Array; Index : Positive) return Internal_Value_Access
      is
         Item : Expr renames Value.Value.all (Index);

         
            Result : Internal_Acc_Node :=  new Internal_Rec_Node;
      begin
            Set_Node (Result, Item);
         return Internal_Value_Access (Result);
      end Array_Item;

      ------------------
      -- Create_Array --
      ------------------

      function Create_Array
        (Values : Internal_Value_Array) return Internal_Acc_Expr_Array
      is
         Result_Index : Natural := 0;
      begin
         return Result : constant Internal_Acc_Expr_Array := new Internal_Rec_Expr_Array do
            Result.Value := new Expr_Array (1 .. Values'Length);
            for I in Values'Range loop
               Result_Index := Result_Index + 1;
               declare
                  Result_Item : Expr renames
                    Result.Value (Result_Index);
                  Value       : Internal_Rec_Node renames
                    Internal_Acc_Node (Values (I)).all;
               begin
                     Result_Item := Get_Node (Value).As_Expr;
               end;
            end loop;
         end return;
      end Create_Array;

      

      ---------
      -- "=" --
      ---------

      overriding function "=" (Left, Right : Internal_Rec_Generic_Instantiation_Array) return Boolean is
      begin
         return Left.Value.all = Right.Value.all;
      end "=";

      -------------
      -- Destroy --
      -------------

      overriding procedure Destroy (Value : in out Internal_Rec_Generic_Instantiation_Array) is
      begin
         Free (Value.Value);
      end Destroy;

      -------------
      -- Type_Of --
      -------------

      overriding function Type_Of (Value : Internal_Rec_Generic_Instantiation_Array) return Type_Index is
      begin
         return Type_Index_For_Generic_Instantiation_Array;
      end Type_Of;

      ------------------
      -- Array_Length --
      ------------------

      overriding function Array_Length (Value : Internal_Rec_Generic_Instantiation_Array) return Natural is
      begin
         return Value.Value.all'Length;
      end Array_Length;

      ----------------
      -- Array_Item --
      ----------------

      overriding function Array_Item
        (Value : Internal_Rec_Generic_Instantiation_Array; Index : Positive) return Internal_Value_Access
      is
         Item : Generic_Instantiation renames Value.Value.all (Index);

         
            Result : Internal_Acc_Node :=  new Internal_Rec_Node;
      begin
            Set_Node (Result, Item);
         return Internal_Value_Access (Result);
      end Array_Item;

      ------------------
      -- Create_Array --
      ------------------

      function Create_Array
        (Values : Internal_Value_Array) return Internal_Acc_Generic_Instantiation_Array
      is
         Result_Index : Natural := 0;
      begin
         return Result : constant Internal_Acc_Generic_Instantiation_Array := new Internal_Rec_Generic_Instantiation_Array do
            Result.Value := new Generic_Instantiation_Array (1 .. Values'Length);
            for I in Values'Range loop
               Result_Index := Result_Index + 1;
               declare
                  Result_Item : Generic_Instantiation renames
                    Result.Value (Result_Index);
                  Value       : Internal_Rec_Node renames
                    Internal_Acc_Node (Values (I)).all;
               begin
                     Result_Item := Get_Node (Value).As_Generic_Instantiation;
               end;
            end loop;
         end return;
      end Create_Array;

      

      ---------
      -- "=" --
      ---------

      overriding function "=" (Left, Right : Internal_Rec_Param_Spec_Array) return Boolean is
      begin
         return Left.Value.all = Right.Value.all;
      end "=";

      -------------
      -- Destroy --
      -------------

      overriding procedure Destroy (Value : in out Internal_Rec_Param_Spec_Array) is
      begin
         Free (Value.Value);
      end Destroy;

      -------------
      -- Type_Of --
      -------------

      overriding function Type_Of (Value : Internal_Rec_Param_Spec_Array) return Type_Index is
      begin
         return Type_Index_For_Param_Spec_Array;
      end Type_Of;

      ------------------
      -- Array_Length --
      ------------------

      overriding function Array_Length (Value : Internal_Rec_Param_Spec_Array) return Natural is
      begin
         return Value.Value.all'Length;
      end Array_Length;

      ----------------
      -- Array_Item --
      ----------------

      overriding function Array_Item
        (Value : Internal_Rec_Param_Spec_Array; Index : Positive) return Internal_Value_Access
      is
         Item : Param_Spec renames Value.Value.all (Index);

         
            Result : Internal_Acc_Node :=  new Internal_Rec_Node;
      begin
            Set_Node (Result, Item);
         return Internal_Value_Access (Result);
      end Array_Item;

      ------------------
      -- Create_Array --
      ------------------

      function Create_Array
        (Values : Internal_Value_Array) return Internal_Acc_Param_Spec_Array
      is
         Result_Index : Natural := 0;
      begin
         return Result : constant Internal_Acc_Param_Spec_Array := new Internal_Rec_Param_Spec_Array do
            Result.Value := new Param_Spec_Array (1 .. Values'Length);
            for I in Values'Range loop
               Result_Index := Result_Index + 1;
               declare
                  Result_Item : Param_Spec renames
                    Result.Value (Result_Index);
                  Value       : Internal_Rec_Node renames
                    Internal_Acc_Node (Values (I)).all;
               begin
                     Result_Item := Get_Node (Value).As_Param_Spec;
               end;
            end loop;
         end return;
      end Create_Array;

      

      ---------
      -- "=" --
      ---------

      overriding function "=" (Left, Right : Internal_Rec_Pragma_Array) return Boolean is
      begin
         return Left.Value.all = Right.Value.all;
      end "=";

      -------------
      -- Destroy --
      -------------

      overriding procedure Destroy (Value : in out Internal_Rec_Pragma_Array) is
      begin
         Free (Value.Value);
      end Destroy;

      -------------
      -- Type_Of --
      -------------

      overriding function Type_Of (Value : Internal_Rec_Pragma_Array) return Type_Index is
      begin
         return Type_Index_For_Pragma_Array;
      end Type_Of;

      ------------------
      -- Array_Length --
      ------------------

      overriding function Array_Length (Value : Internal_Rec_Pragma_Array) return Natural is
      begin
         return Value.Value.all'Length;
      end Array_Length;

      ----------------
      -- Array_Item --
      ----------------

      overriding function Array_Item
        (Value : Internal_Rec_Pragma_Array; Index : Positive) return Internal_Value_Access
      is
         Item : Pragma_Node renames Value.Value.all (Index);

         
            Result : Internal_Acc_Node :=  new Internal_Rec_Node;
      begin
            Set_Node (Result, Item);
         return Internal_Value_Access (Result);
      end Array_Item;

      ------------------
      -- Create_Array --
      ------------------

      function Create_Array
        (Values : Internal_Value_Array) return Internal_Acc_Pragma_Array
      is
         Result_Index : Natural := 0;
      begin
         return Result : constant Internal_Acc_Pragma_Array := new Internal_Rec_Pragma_Array do
            Result.Value := new Pragma_Node_Array (1 .. Values'Length);
            for I in Values'Range loop
               Result_Index := Result_Index + 1;
               declare
                  Result_Item : Pragma_Node renames
                    Result.Value (Result_Index);
                  Value       : Internal_Rec_Node renames
                    Internal_Acc_Node (Values (I)).all;
               begin
                     Result_Item := Get_Node (Value).As_Pragma_Node;
               end;
            end loop;
         end return;
      end Create_Array;

      

      ---------
      -- "=" --
      ---------

      overriding function "=" (Left, Right : Internal_Rec_Type_Decl_Array) return Boolean is
      begin
         return Left.Value.all = Right.Value.all;
      end "=";

      -------------
      -- Destroy --
      -------------

      overriding procedure Destroy (Value : in out Internal_Rec_Type_Decl_Array) is
      begin
         Free (Value.Value);
      end Destroy;

      -------------
      -- Type_Of --
      -------------

      overriding function Type_Of (Value : Internal_Rec_Type_Decl_Array) return Type_Index is
      begin
         return Type_Index_For_Type_Decl_Array;
      end Type_Of;

      ------------------
      -- Array_Length --
      ------------------

      overriding function Array_Length (Value : Internal_Rec_Type_Decl_Array) return Natural is
      begin
         return Value.Value.all'Length;
      end Array_Length;

      ----------------
      -- Array_Item --
      ----------------

      overriding function Array_Item
        (Value : Internal_Rec_Type_Decl_Array; Index : Positive) return Internal_Value_Access
      is
         Item : Type_Decl renames Value.Value.all (Index);

         
            Result : Internal_Acc_Node :=  new Internal_Rec_Node;
      begin
            Set_Node (Result, Item);
         return Internal_Value_Access (Result);
      end Array_Item;

      ------------------
      -- Create_Array --
      ------------------

      function Create_Array
        (Values : Internal_Value_Array) return Internal_Acc_Type_Decl_Array
      is
         Result_Index : Natural := 0;
      begin
         return Result : constant Internal_Acc_Type_Decl_Array := new Internal_Rec_Type_Decl_Array do
            Result.Value := new Type_Decl_Array (1 .. Values'Length);
            for I in Values'Range loop
               Result_Index := Result_Index + 1;
               declare
                  Result_Item : Type_Decl renames
                    Result.Value (Result_Index);
                  Value       : Internal_Rec_Node renames
                    Internal_Acc_Node (Values (I)).all;
               begin
                     Result_Item := Get_Node (Value).As_Type_Decl;
               end;
            end loop;
         end return;
      end Create_Array;

      

      ---------
      -- "=" --
      ---------

      overriding function "=" (Left, Right : Internal_Rec_Param_Actual_Array) return Boolean is
      begin
         return Left.Value.all = Right.Value.all;
      end "=";

      -------------
      -- Destroy --
      -------------

      overriding procedure Destroy (Value : in out Internal_Rec_Param_Actual_Array) is
      begin
         Free (Value.Value);
      end Destroy;

      -------------
      -- Type_Of --
      -------------

      overriding function Type_Of (Value : Internal_Rec_Param_Actual_Array) return Type_Index is
      begin
         return Type_Index_For_Param_Actual_Array;
      end Type_Of;

      ------------------
      -- Array_Length --
      ------------------

      overriding function Array_Length (Value : Internal_Rec_Param_Actual_Array) return Natural is
      begin
         return Value.Value.all'Length;
      end Array_Length;

      ----------------
      -- Array_Item --
      ----------------

      overriding function Array_Item
        (Value : Internal_Rec_Param_Actual_Array; Index : Positive) return Internal_Value_Access
      is
         Item : Param_Actual renames Value.Value.all (Index);

         
            Result : Internal_Acc_Param_Actual :=  new Internal_Rec_Param_Actual;
      begin
            Result.Value := Item;
         return Internal_Value_Access (Result);
      end Array_Item;

      ------------------
      -- Create_Array --
      ------------------

      function Create_Array
        (Values : Internal_Value_Array) return Internal_Acc_Param_Actual_Array
      is
         Result_Index : Natural := 0;
      begin
         return Result : constant Internal_Acc_Param_Actual_Array := new Internal_Rec_Param_Actual_Array do
            Result.Value := new Param_Actual_Array (1 .. Values'Length);
            for I in Values'Range loop
               Result_Index := Result_Index + 1;
               declare
                  Result_Item : Param_Actual renames
                    Result.Value (Result_Index);
                  Value       : Internal_Rec_Param_Actual renames
                    Internal_Acc_Param_Actual (Values (I)).all;
               begin
                     Result_Item := Value.Value;
               end;
            end loop;
         end return;
      end Create_Array;

      

      ---------
      -- "=" --
      ---------

      overriding function "=" (Left, Right : Internal_Rec_Ref_Result_Array) return Boolean is
      begin
         return Left.Value.all = Right.Value.all;
      end "=";

      -------------
      -- Destroy --
      -------------

      overriding procedure Destroy (Value : in out Internal_Rec_Ref_Result_Array) is
      begin
         Free (Value.Value);
      end Destroy;

      -------------
      -- Type_Of --
      -------------

      overriding function Type_Of (Value : Internal_Rec_Ref_Result_Array) return Type_Index is
      begin
         return Type_Index_For_Ref_Result_Array;
      end Type_Of;

      ------------------
      -- Array_Length --
      ------------------

      overriding function Array_Length (Value : Internal_Rec_Ref_Result_Array) return Natural is
      begin
         return Value.Value.all'Length;
      end Array_Length;

      ----------------
      -- Array_Item --
      ----------------

      overriding function Array_Item
        (Value : Internal_Rec_Ref_Result_Array; Index : Positive) return Internal_Value_Access
      is
         Item : Ref_Result renames Value.Value.all (Index);

         
            Result : Internal_Acc_Ref_Result :=  new Internal_Rec_Ref_Result;
      begin
            Result.Value := Item;
         return Internal_Value_Access (Result);
      end Array_Item;

      ------------------
      -- Create_Array --
      ------------------

      function Create_Array
        (Values : Internal_Value_Array) return Internal_Acc_Ref_Result_Array
      is
         Result_Index : Natural := 0;
      begin
         return Result : constant Internal_Acc_Ref_Result_Array := new Internal_Rec_Ref_Result_Array do
            Result.Value := new Ref_Result_Array (1 .. Values'Length);
            for I in Values'Range loop
               Result_Index := Result_Index + 1;
               declare
                  Result_Item : Ref_Result renames
                    Result.Value (Result_Index);
                  Value       : Internal_Rec_Ref_Result renames
                    Internal_Acc_Ref_Result (Values (I)).all;
               begin
                     Result_Item := Value.Value;
               end;
            end loop;
         end return;
      end Create_Array;

      

      ---------
      -- "=" --
      ---------

      overriding function "=" (Left, Right : Internal_Rec_Shape_Array) return Boolean is
      begin
         return Left.Value.all = Right.Value.all;
      end "=";

      -------------
      -- Destroy --
      -------------

      overriding procedure Destroy (Value : in out Internal_Rec_Shape_Array) is
      begin
         Free (Value.Value);
      end Destroy;

      -------------
      -- Type_Of --
      -------------

      overriding function Type_Of (Value : Internal_Rec_Shape_Array) return Type_Index is
      begin
         return Type_Index_For_Shape_Array;
      end Type_Of;

      ------------------
      -- Array_Length --
      ------------------

      overriding function Array_Length (Value : Internal_Rec_Shape_Array) return Natural is
      begin
         return Value.Value.all'Length;
      end Array_Length;

      ----------------
      -- Array_Item --
      ----------------

      overriding function Array_Item
        (Value : Internal_Rec_Shape_Array; Index : Positive) return Internal_Value_Access
      is
         Item : Shape renames Value.Value.all (Index);

         
            Result : Internal_Acc_Shape :=  new Internal_Rec_Shape;
      begin
            Result.Value := Item;
         return Internal_Value_Access (Result);
      end Array_Item;

      ------------------
      -- Create_Array --
      ------------------

      function Create_Array
        (Values : Internal_Value_Array) return Internal_Acc_Shape_Array
      is
         Result_Index : Natural := 0;
      begin
         return Result : constant Internal_Acc_Shape_Array := new Internal_Rec_Shape_Array do
            Result.Value := new Shape_Array (1 .. Values'Length);
            for I in Values'Range loop
               Result_Index := Result_Index + 1;
               declare
                  Result_Item : Shape renames
                    Result.Value (Result_Index);
                  Value       : Internal_Rec_Shape renames
                    Internal_Acc_Shape (Values (I)).all;
               begin
                     Result_Item := Value.Value;
               end;
            end loop;
         end return;
      end Create_Array;

      

      ---------
      -- "=" --
      ---------

      overriding function "=" (Left, Right : Internal_Rec_Substitution_Array) return Boolean is
      begin
         return Left.Value.all = Right.Value.all;
      end "=";

      -------------
      -- Destroy --
      -------------

      overriding procedure Destroy (Value : in out Internal_Rec_Substitution_Array) is
      begin
         Free (Value.Value);
      end Destroy;

      -------------
      -- Type_Of --
      -------------

      overriding function Type_Of (Value : Internal_Rec_Substitution_Array) return Type_Index is
      begin
         return Type_Index_For_Substitution_Array;
      end Type_Of;

      ------------------
      -- Array_Length --
      ------------------

      overriding function Array_Length (Value : Internal_Rec_Substitution_Array) return Natural is
      begin
         return Value.Value.all'Length;
      end Array_Length;

      ----------------
      -- Array_Item --
      ----------------

      overriding function Array_Item
        (Value : Internal_Rec_Substitution_Array; Index : Positive) return Internal_Value_Access
      is
         Item : Substitution renames Value.Value.all (Index);

         
            Result : Internal_Acc_Substitution :=  new Internal_Rec_Substitution;
      begin
            Result.Value := Item;
         return Internal_Value_Access (Result);
      end Array_Item;

      ------------------
      -- Create_Array --
      ------------------

      function Create_Array
        (Values : Internal_Value_Array) return Internal_Acc_Substitution_Array
      is
         Result_Index : Natural := 0;
      begin
         return Result : constant Internal_Acc_Substitution_Array := new Internal_Rec_Substitution_Array do
            Result.Value := new Substitution_Array (1 .. Values'Length);
            for I in Values'Range loop
               Result_Index := Result_Index + 1;
               declare
                  Result_Item : Substitution renames
                    Result.Value (Result_Index);
                  Value       : Internal_Rec_Substitution renames
                    Internal_Acc_Substitution (Values (I)).all;
               begin
                     Result_Item := Value.Value;
               end;
            end loop;
         end return;
      end Create_Array;

      

      ---------
      -- "=" --
      ---------

      overriding function "=" (Left, Right : Internal_Rec_Analysis_Unit_Array) return Boolean is
      begin
         return Left.Value.all = Right.Value.all;
      end "=";

      -------------
      -- Destroy --
      -------------

      overriding procedure Destroy (Value : in out Internal_Rec_Analysis_Unit_Array) is
      begin
         Free (Value.Value);
      end Destroy;

      -------------
      -- Type_Of --
      -------------

      overriding function Type_Of (Value : Internal_Rec_Analysis_Unit_Array) return Type_Index is
      begin
         return Type_Index_For_Analysis_Unit_Array;
      end Type_Of;

      ------------------
      -- Array_Length --
      ------------------

      overriding function Array_Length (Value : Internal_Rec_Analysis_Unit_Array) return Natural is
      begin
         return Value.Value.all'Length;
      end Array_Length;

      ----------------
      -- Array_Item --
      ----------------

      overriding function Array_Item
        (Value : Internal_Rec_Analysis_Unit_Array; Index : Positive) return Internal_Value_Access
      is
         Item : Analysis_Unit renames Value.Value.all (Index);

         
            Result : Internal_Acc_Analysis_Unit :=  new Internal_Rec_Analysis_Unit;
      begin
            Set_Unit (Result, Item);
         return Internal_Value_Access (Result);
      end Array_Item;

      ------------------
      -- Create_Array --
      ------------------

      function Create_Array
        (Values : Internal_Value_Array) return Internal_Acc_Analysis_Unit_Array
      is
         Result_Index : Natural := 0;
      begin
         return Result : constant Internal_Acc_Analysis_Unit_Array := new Internal_Rec_Analysis_Unit_Array do
            Result.Value := new Analysis_Unit_Array (1 .. Values'Length);
            for I in Values'Range loop
               Result_Index := Result_Index + 1;
               declare
                  Result_Item : Analysis_Unit renames
                    Result.Value (Result_Index);
                  Value       : Internal_Rec_Analysis_Unit renames
                    Internal_Acc_Analysis_Unit (Values (I)).all;
               begin
                     Result_Item := Get_Unit (Value);
               end;
            end loop;
         end return;
      end Create_Array;

      

      ---------
      -- "=" --
      ---------

      overriding function "=" (Left, Right : Internal_Rec_Symbol_Array) return Boolean is
      begin
         return Left.Value.all = Right.Value.all;
      end "=";

      -------------
      -- Destroy --
      -------------

      overriding procedure Destroy (Value : in out Internal_Rec_Symbol_Array) is
      begin
         Free (Value.Value);
      end Destroy;

      -------------
      -- Type_Of --
      -------------

      overriding function Type_Of (Value : Internal_Rec_Symbol_Array) return Type_Index is
      begin
         return Type_Index_For_Symbol_Array;
      end Type_Of;

      ------------------
      -- Array_Length --
      ------------------

      overriding function Array_Length (Value : Internal_Rec_Symbol_Array) return Natural is
      begin
         return Value.Value.all'Length;
      end Array_Length;

      ----------------
      -- Array_Item --
      ----------------

      overriding function Array_Item
        (Value : Internal_Rec_Symbol_Array; Index : Positive) return Internal_Value_Access
      is
         Item : Unbounded_Text_Type renames Value.Value.all (Index);

         
            Result : Internal_Acc_Symbol :=  new Internal_Rec_Symbol;
      begin
            Result.Value := Item;
         return Internal_Value_Access (Result);
      end Array_Item;

      ------------------
      -- Create_Array --
      ------------------

      function Create_Array
        (Values : Internal_Value_Array) return Internal_Acc_Symbol_Array
      is
         Result_Index : Natural := 0;
      begin
         return Result : constant Internal_Acc_Symbol_Array := new Internal_Rec_Symbol_Array do
            Result.Value := new Unbounded_Text_Type_Array (1 .. Values'Length);
            for I in Values'Range loop
               Result_Index := Result_Index + 1;
               declare
                  Result_Item : Unbounded_Text_Type renames
                    Result.Value (Result_Index);
                  Value       : Internal_Rec_Symbol renames
                    Internal_Acc_Symbol (Values (I)).all;
               begin
                     Result_Item := Value.Value;
               end;
            end loop;
         end return;
      end Create_Array;


   ------------------
   -- Create_Array --
   ------------------

   function Create_Array
     (Array_Type : Type_Index;
      Values     : Internal_Value_Array) return Internal_Value_Access is
   begin
      case Array_Type is
            when Type_Index_For_Discriminant_Values_Array =>
               declare
                  Result : constant Internal_Acc_Discriminant_Values_Array :=
                    Create_Array (Values);
               begin
                  return Internal_Value_Access (Result);
               end;
            when Type_Index_For_Doc_Annotation_Array =>
               declare
                  Result : constant Internal_Acc_Doc_Annotation_Array :=
                    Create_Array (Values);
               begin
                  return Internal_Value_Access (Result);
               end;
            when Type_Index_For_Accept_Stmt_Array =>
               declare
                  Result : constant Internal_Acc_Accept_Stmt_Array :=
                    Create_Array (Values);
               begin
                  return Internal_Value_Access (Result);
               end;
            when Type_Index_For_Ada_Node_Array =>
               declare
                  Result : constant Internal_Acc_Ada_Node_Array :=
                    Create_Array (Values);
               begin
                  return Internal_Value_Access (Result);
               end;
            when Type_Index_For_Base_Formal_Param_Decl_Array =>
               declare
                  Result : constant Internal_Acc_Base_Formal_Param_Decl_Array :=
                    Create_Array (Values);
               begin
                  return Internal_Value_Access (Result);
               end;
            when Type_Index_For_Base_Type_Decl_Array =>
               declare
                  Result : constant Internal_Acc_Base_Type_Decl_Array :=
                    Create_Array (Values);
               begin
                  return Internal_Value_Access (Result);
               end;
            when Type_Index_For_Basic_Decl_Array =>
               declare
                  Result : constant Internal_Acc_Basic_Decl_Array :=
                    Create_Array (Values);
               begin
                  return Internal_Value_Access (Result);
               end;
            when Type_Index_For_Compilation_Unit_Array =>
               declare
                  Result : constant Internal_Acc_Compilation_Unit_Array :=
                    Create_Array (Values);
               begin
                  return Internal_Value_Access (Result);
               end;
            when Type_Index_For_Defining_Name_Array =>
               declare
                  Result : constant Internal_Acc_Defining_Name_Array :=
                    Create_Array (Values);
               begin
                  return Internal_Value_Access (Result);
               end;
            when Type_Index_For_Expr_Array =>
               declare
                  Result : constant Internal_Acc_Expr_Array :=
                    Create_Array (Values);
               begin
                  return Internal_Value_Access (Result);
               end;
            when Type_Index_For_Generic_Instantiation_Array =>
               declare
                  Result : constant Internal_Acc_Generic_Instantiation_Array :=
                    Create_Array (Values);
               begin
                  return Internal_Value_Access (Result);
               end;
            when Type_Index_For_Param_Spec_Array =>
               declare
                  Result : constant Internal_Acc_Param_Spec_Array :=
                    Create_Array (Values);
               begin
                  return Internal_Value_Access (Result);
               end;
            when Type_Index_For_Pragma_Array =>
               declare
                  Result : constant Internal_Acc_Pragma_Array :=
                    Create_Array (Values);
               begin
                  return Internal_Value_Access (Result);
               end;
            when Type_Index_For_Type_Decl_Array =>
               declare
                  Result : constant Internal_Acc_Type_Decl_Array :=
                    Create_Array (Values);
               begin
                  return Internal_Value_Access (Result);
               end;
            when Type_Index_For_Param_Actual_Array =>
               declare
                  Result : constant Internal_Acc_Param_Actual_Array :=
                    Create_Array (Values);
               begin
                  return Internal_Value_Access (Result);
               end;
            when Type_Index_For_Ref_Result_Array =>
               declare
                  Result : constant Internal_Acc_Ref_Result_Array :=
                    Create_Array (Values);
               begin
                  return Internal_Value_Access (Result);
               end;
            when Type_Index_For_Shape_Array =>
               declare
                  Result : constant Internal_Acc_Shape_Array :=
                    Create_Array (Values);
               begin
                  return Internal_Value_Access (Result);
               end;
            when Type_Index_For_Substitution_Array =>
               declare
                  Result : constant Internal_Acc_Substitution_Array :=
                    Create_Array (Values);
               begin
                  return Internal_Value_Access (Result);
               end;
            when Type_Index_For_Analysis_Unit_Array =>
               declare
                  Result : constant Internal_Acc_Analysis_Unit_Array :=
                    Create_Array (Values);
               begin
                  return Internal_Value_Access (Result);
               end;
            when Type_Index_For_Symbol_Array =>
               declare
                  Result : constant Internal_Acc_Symbol_Array :=
                    Create_Array (Values);
               begin
                  return Internal_Value_Access (Result);
               end;

         when others =>
            --  Validation in public wrappers is supposed to prevent calling
            --  this function on non-array types.
            raise Program_Error;
      end case;
   end Create_Array;

      

      ---------
      -- "=" --
      ---------

      overriding function "=" (Left, Right : Internal_Rec_Completion_Item_Iterator) return Boolean is
      begin
         return Left.Value = Right.Value;
      end "=";

      -------------
      -- Type_Of --
      -------------

      overriding function Type_Of (Value : Internal_Rec_Completion_Item_Iterator) return Type_Index is
      begin
         return Type_Index_For_Completion_Item_Iterator;
      end Type_Of;

      ----------
      -- Next --
      ----------

      overriding function Next (Value : Internal_Rec_Completion_Item_Iterator) return Internal_Value_Access is
         
         Item : Completion_Item;
      begin
         if Next (Value.Value, Item) then
            
            declare
Result : Internal_Acc_Completion_Item :=  new Internal_Rec_Completion_Item;
begin
Result.Value := Item;
return Internal_Value_Access (Result);
end;
         else
            return null;
         end if;
      end Next;


      

      ---------
      -- "=" --
      ---------

      overriding function "=" (Left, Right : Internal_Rec_Aspect) return Boolean is
      begin
         return Left.Value = Right.Value;
      end "=";

      -------------
      -- Type_Of --
      -------------

      overriding function Type_Of (Value : Internal_Rec_Aspect) return Type_Index is
      begin
         return Type_Index_For_Aspect;
      end Type_Of;

      -------------------
      -- Create_Struct --
      -------------------

      function Create_Struct
        (Values : Internal_Value_Array) return Internal_Acc_Aspect
      is
         
            F_Exists : Boolean renames Internal_Acc_Bool (Values (1)).Value;
            F_Node : Ada_Node := Get_Node (Internal_Acc_Node (Values (2)).all);
            F_Value : Expr := Get_Node (Internal_Acc_Node (Values (3)).all).As_Expr;
      begin

         return Result : constant Internal_Acc_Aspect := new Internal_Rec_Aspect do
            Result.Value := Create_Aspect (F_Exists, F_Node, F_Value);
         end return;
      end Create_Struct;

      -----------------
      -- Eval_Member --
      -----------------

      overriding function Eval_Member
        (Value  : Internal_Rec_Aspect;
         Member : Struct_Member_Index) return Internal_Value_Access is
      begin
         case Member is
               
               when Member_Index_For_Aspect_Exists =>
                  declare
                     Item : constant Boolean
                     := Analysis.Exists (Value.Value);

                     

                        Result : Internal_Acc_Bool :=  new Internal_Rec_Bool;
                  begin
                        Result.Value := Item;
                     return Internal_Value_Access (Result);
                  end;
               
               when Member_Index_For_Aspect_Node =>
                  declare
                     Item : constant Ada_Node
                           'Class
                     := Analysis.Node (Value.Value);

                     

                        Result : Internal_Acc_Node :=  new Internal_Rec_Node;
                  begin
                        Set_Node (Result, Item);
                     return Internal_Value_Access (Result);
                  end;
               
               when Member_Index_For_Aspect_Value =>
                  declare
                     Item : constant Expr
                           'Class
                     := Analysis.Value (Value.Value);

                     

                        Result : Internal_Acc_Node :=  new Internal_Rec_Node;
                  begin
                        Set_Node (Result, Item);
                     return Internal_Value_Access (Result);
                  end;

            when others =>
               --  Validation in public wrappers is supposed to prevent calling
               --  this function on invalid members.
               raise Program_Error;
         end case;
      end Eval_Member;

      

      ---------
      -- "=" --
      ---------

      overriding function "=" (Left, Right : Internal_Rec_Completion_Item) return Boolean is
      begin
         return Left.Value = Right.Value;
      end "=";

      -------------
      -- Type_Of --
      -------------

      overriding function Type_Of (Value : Internal_Rec_Completion_Item) return Type_Index is
      begin
         return Type_Index_For_Completion_Item;
      end Type_Of;

      -------------------
      -- Create_Struct --
      -------------------

      function Create_Struct
        (Values : Internal_Value_Array) return Internal_Acc_Completion_Item
      is
         
            F_Decl : Basic_Decl := Get_Node (Internal_Acc_Node (Values (1)).all).As_Basic_Decl;
            F_Is_Dot_Call : Boolean renames Internal_Acc_Bool (Values (2)).Value;
            F_Is_Visible : Boolean renames Internal_Acc_Bool (Values (3)).Value;
            F_Weight : Integer renames Internal_Acc_Int (Values (4)).Value;
      begin

         return Result : constant Internal_Acc_Completion_Item := new Internal_Rec_Completion_Item do
            Result.Value := Create_Completion_Item (F_Decl, F_Is_Dot_Call, F_Is_Visible, F_Weight);
         end return;
      end Create_Struct;

      -----------------
      -- Eval_Member --
      -----------------

      overriding function Eval_Member
        (Value  : Internal_Rec_Completion_Item;
         Member : Struct_Member_Index) return Internal_Value_Access is
      begin
         case Member is
               
               when Member_Index_For_Completion_Item_Decl =>
                  declare
                     Item : constant Basic_Decl
                           'Class
                     := Analysis.Decl (Value.Value);

                     

                        Result : Internal_Acc_Node :=  new Internal_Rec_Node;
                  begin
                        Set_Node (Result, Item);
                     return Internal_Value_Access (Result);
                  end;
               
               when Member_Index_For_Completion_Item_Is_Dot_Call =>
                  declare
                     Item : constant Boolean
                     := Analysis.Is_Dot_Call (Value.Value);

                     

                        Result : Internal_Acc_Bool :=  new Internal_Rec_Bool;
                  begin
                        Result.Value := Item;
                     return Internal_Value_Access (Result);
                  end;
               
               when Member_Index_For_Completion_Item_Is_Visible =>
                  declare
                     Item : constant Boolean
                     := Analysis.Is_Visible (Value.Value);

                     

                        Result : Internal_Acc_Bool :=  new Internal_Rec_Bool;
                  begin
                        Result.Value := Item;
                     return Internal_Value_Access (Result);
                  end;
               
               when Member_Index_For_Completion_Item_Weight =>
                  declare
                     Item : constant Integer
                     := Analysis.Weight (Value.Value);

                     

                        Result : Internal_Acc_Int :=  new Internal_Rec_Int;
                  begin
                        Result.Value := Item;
                     return Internal_Value_Access (Result);
                  end;

            when others =>
               --  Validation in public wrappers is supposed to prevent calling
               --  this function on invalid members.
               raise Program_Error;
         end case;
      end Eval_Member;

      

      ---------
      -- "=" --
      ---------

      overriding function "=" (Left, Right : Internal_Rec_Discrete_Range) return Boolean is
      begin
         return Left.Value = Right.Value;
      end "=";

      -------------
      -- Type_Of --
      -------------

      overriding function Type_Of (Value : Internal_Rec_Discrete_Range) return Type_Index is
      begin
         return Type_Index_For_Discrete_Range;
      end Type_Of;

      -------------------
      -- Create_Struct --
      -------------------

      function Create_Struct
        (Values : Internal_Value_Array) return Internal_Acc_Discrete_Range
      is
         
            F_Low_Bound : Expr := Get_Node (Internal_Acc_Node (Values (1)).all).As_Expr;
            F_High_Bound : Expr := Get_Node (Internal_Acc_Node (Values (2)).all).As_Expr;
      begin

         return Result : constant Internal_Acc_Discrete_Range := new Internal_Rec_Discrete_Range do
            Result.Value := Create_Discrete_Range (F_Low_Bound, F_High_Bound);
         end return;
      end Create_Struct;

      -----------------
      -- Eval_Member --
      -----------------

      overriding function Eval_Member
        (Value  : Internal_Rec_Discrete_Range;
         Member : Struct_Member_Index) return Internal_Value_Access is
      begin
         case Member is
               
               when Member_Index_For_Discrete_Range_Low_Bound =>
                  declare
                     Item : constant Expr
                           'Class
                     := Analysis.Low_Bound (Value.Value);

                     

                        Result : Internal_Acc_Node :=  new Internal_Rec_Node;
                  begin
                        Set_Node (Result, Item);
                     return Internal_Value_Access (Result);
                  end;
               
               when Member_Index_For_Discrete_Range_High_Bound =>
                  declare
                     Item : constant Expr
                           'Class
                     := Analysis.High_Bound (Value.Value);

                     

                        Result : Internal_Acc_Node :=  new Internal_Rec_Node;
                  begin
                        Set_Node (Result, Item);
                     return Internal_Value_Access (Result);
                  end;

            when others =>
               --  Validation in public wrappers is supposed to prevent calling
               --  this function on invalid members.
               raise Program_Error;
         end case;
      end Eval_Member;

      

      ---------
      -- "=" --
      ---------

      overriding function "=" (Left, Right : Internal_Rec_Discriminant_Values) return Boolean is
      begin
         return Left.Value = Right.Value;
      end "=";

      -------------
      -- Type_Of --
      -------------

      overriding function Type_Of (Value : Internal_Rec_Discriminant_Values) return Type_Index is
      begin
         return Type_Index_For_Discriminant_Values;
      end Type_Of;

      -------------------
      -- Create_Struct --
      -------------------

      function Create_Struct
        (Values : Internal_Value_Array) return Internal_Acc_Discriminant_Values
      is
         
            F_Discriminant : Identifier := Get_Node (Internal_Acc_Node (Values (1)).all).As_Identifier;
            F_Values : Alternatives_List := Get_Node (Internal_Acc_Node (Values (2)).all).As_Alternatives_List;
      begin

         return Result : constant Internal_Acc_Discriminant_Values := new Internal_Rec_Discriminant_Values do
            Result.Value := Create_Discriminant_Values (F_Discriminant, F_Values);
         end return;
      end Create_Struct;

      -----------------
      -- Eval_Member --
      -----------------

      overriding function Eval_Member
        (Value  : Internal_Rec_Discriminant_Values;
         Member : Struct_Member_Index) return Internal_Value_Access is
      begin
         case Member is
               
               when Member_Index_For_Discriminant_Values_Discriminant =>
                  declare
                     Item : constant Identifier
                           'Class
                     := Analysis.Discriminant (Value.Value);

                     

                        Result : Internal_Acc_Node :=  new Internal_Rec_Node;
                  begin
                        Set_Node (Result, Item);
                     return Internal_Value_Access (Result);
                  end;
               
               when Member_Index_For_Discriminant_Values_Values =>
                  declare
                     Item : constant Alternatives_List
                           'Class
                     := Analysis.Values (Value.Value);

                     

                        Result : Internal_Acc_Node :=  new Internal_Rec_Node;
                  begin
                        Set_Node (Result, Item);
                     return Internal_Value_Access (Result);
                  end;

            when others =>
               --  Validation in public wrappers is supposed to prevent calling
               --  this function on invalid members.
               raise Program_Error;
         end case;
      end Eval_Member;

      

      ---------
      -- "=" --
      ---------

      overriding function "=" (Left, Right : Internal_Rec_Doc_Annotation) return Boolean is
      begin
         return Left.Value = Right.Value;
      end "=";

      -------------
      -- Type_Of --
      -------------

      overriding function Type_Of (Value : Internal_Rec_Doc_Annotation) return Type_Index is
      begin
         return Type_Index_For_Doc_Annotation;
      end Type_Of;

      -------------------
      -- Create_Struct --
      -------------------

      function Create_Struct
        (Values : Internal_Value_Array) return Internal_Acc_Doc_Annotation
      is
         
            F_Key : Text_Type := To_Text (Internal_Acc_String (Values (1)).Value);
            F_Value : Text_Type := To_Text (Internal_Acc_String (Values (2)).Value);
      begin

         return Result : constant Internal_Acc_Doc_Annotation := new Internal_Rec_Doc_Annotation do
            Result.Value := Create_Doc_Annotation (F_Key, F_Value);
         end return;
      end Create_Struct;

      -----------------
      -- Eval_Member --
      -----------------

      overriding function Eval_Member
        (Value  : Internal_Rec_Doc_Annotation;
         Member : Struct_Member_Index) return Internal_Value_Access is
      begin
         case Member is
               
               when Member_Index_For_Doc_Annotation_Key =>
                  declare
                     Item : constant Text_Type
                     := Analysis.Key (Value.Value);

                     

                        Result : Internal_Acc_String :=  new Internal_Rec_String;
                  begin
                        Result.Value := To_Unbounded_Text (Item);
                     return Internal_Value_Access (Result);
                  end;
               
               when Member_Index_For_Doc_Annotation_Value =>
                  declare
                     Item : constant Text_Type
                     := Analysis.Value (Value.Value);

                     

                        Result : Internal_Acc_String :=  new Internal_Rec_String;
                  begin
                        Result.Value := To_Unbounded_Text (Item);
                     return Internal_Value_Access (Result);
                  end;

            when others =>
               --  Validation in public wrappers is supposed to prevent calling
               --  this function on invalid members.
               raise Program_Error;
         end case;
      end Eval_Member;

      

      ---------
      -- "=" --
      ---------

      overriding function "=" (Left, Right : Internal_Rec_Param_Actual) return Boolean is
      begin
         return Left.Value = Right.Value;
      end "=";

      -------------
      -- Type_Of --
      -------------

      overriding function Type_Of (Value : Internal_Rec_Param_Actual) return Type_Index is
      begin
         return Type_Index_For_Param_Actual;
      end Type_Of;

      -------------------
      -- Create_Struct --
      -------------------

      function Create_Struct
        (Values : Internal_Value_Array) return Internal_Acc_Param_Actual
      is
         
            F_Param : Defining_Name := Get_Node (Internal_Acc_Node (Values (1)).all).As_Defining_Name;
            F_Actual : Expr := Get_Node (Internal_Acc_Node (Values (2)).all).As_Expr;
      begin

         return Result : constant Internal_Acc_Param_Actual := new Internal_Rec_Param_Actual do
            Result.Value := Create_Param_Actual (F_Param, F_Actual);
         end return;
      end Create_Struct;

      -----------------
      -- Eval_Member --
      -----------------

      overriding function Eval_Member
        (Value  : Internal_Rec_Param_Actual;
         Member : Struct_Member_Index) return Internal_Value_Access is
      begin
         case Member is
               
               when Member_Index_For_Param_Actual_Param =>
                  declare
                     Item : constant Defining_Name
                           'Class
                     := Analysis.Param (Value.Value);

                     

                        Result : Internal_Acc_Node :=  new Internal_Rec_Node;
                  begin
                        Set_Node (Result, Item);
                     return Internal_Value_Access (Result);
                  end;
               
               when Member_Index_For_Param_Actual_Actual =>
                  declare
                     Item : constant Expr
                           'Class
                     := Analysis.Actual (Value.Value);

                     

                        Result : Internal_Acc_Node :=  new Internal_Rec_Node;
                  begin
                        Set_Node (Result, Item);
                     return Internal_Value_Access (Result);
                  end;

            when others =>
               --  Validation in public wrappers is supposed to prevent calling
               --  this function on invalid members.
               raise Program_Error;
         end case;
      end Eval_Member;

      

      ---------
      -- "=" --
      ---------

      overriding function "=" (Left, Right : Internal_Rec_Ref_Result) return Boolean is
      begin
         return Left.Value = Right.Value;
      end "=";

      -------------
      -- Type_Of --
      -------------

      overriding function Type_Of (Value : Internal_Rec_Ref_Result) return Type_Index is
      begin
         return Type_Index_For_Ref_Result;
      end Type_Of;

      -------------------
      -- Create_Struct --
      -------------------

      function Create_Struct
        (Values : Internal_Value_Array) return Internal_Acc_Ref_Result
      is
         
            F_Ref : Base_Id := Get_Node (Internal_Acc_Node (Values (1)).all).As_Base_Id;
            F_Kind : Ref_Result_Kind renames Internal_Acc_Ref_Result_Kind (Values (2)).Value;
      begin

         return Result : constant Internal_Acc_Ref_Result := new Internal_Rec_Ref_Result do
            Result.Value := Create_Ref_Result (F_Ref, F_Kind);
         end return;
      end Create_Struct;

      -----------------
      -- Eval_Member --
      -----------------

      overriding function Eval_Member
        (Value  : Internal_Rec_Ref_Result;
         Member : Struct_Member_Index) return Internal_Value_Access is
      begin
         case Member is
               
               when Member_Index_For_Ref_Result_Ref =>
                  declare
                     Item : constant Base_Id
                           'Class
                     := Analysis.Ref (Value.Value);

                     

                        Result : Internal_Acc_Node :=  new Internal_Rec_Node;
                  begin
                        Set_Node (Result, Item);
                     return Internal_Value_Access (Result);
                  end;
               
               when Member_Index_For_Ref_Result_Kind =>
                  declare
                     Item : constant Ref_Result_Kind
                     := Analysis.Kind (Value.Value);

                     

                        Result : Internal_Acc_Ref_Result_Kind :=  new Internal_Rec_Ref_Result_Kind;
                  begin
                        Result.Value := Item;
                     return Internal_Value_Access (Result);
                  end;

            when others =>
               --  Validation in public wrappers is supposed to prevent calling
               --  this function on invalid members.
               raise Program_Error;
         end case;
      end Eval_Member;

      

      ---------
      -- "=" --
      ---------

      overriding function "=" (Left, Right : Internal_Rec_Refd_Decl) return Boolean is
      begin
         return Left.Value = Right.Value;
      end "=";

      -------------
      -- Type_Of --
      -------------

      overriding function Type_Of (Value : Internal_Rec_Refd_Decl) return Type_Index is
      begin
         return Type_Index_For_Refd_Decl;
      end Type_Of;

      -------------------
      -- Create_Struct --
      -------------------

      function Create_Struct
        (Values : Internal_Value_Array) return Internal_Acc_Refd_Decl
      is
         
            F_Decl : Basic_Decl := Get_Node (Internal_Acc_Node (Values (1)).all).As_Basic_Decl;
            F_Kind : Ref_Result_Kind renames Internal_Acc_Ref_Result_Kind (Values (2)).Value;
      begin

         return Result : constant Internal_Acc_Refd_Decl := new Internal_Rec_Refd_Decl do
            Result.Value := Create_Refd_Decl (F_Decl, F_Kind);
         end return;
      end Create_Struct;

      -----------------
      -- Eval_Member --
      -----------------

      overriding function Eval_Member
        (Value  : Internal_Rec_Refd_Decl;
         Member : Struct_Member_Index) return Internal_Value_Access is
      begin
         case Member is
               
               when Member_Index_For_Refd_Decl_Decl =>
                  declare
                     Item : constant Basic_Decl
                           'Class
                     := Analysis.Decl (Value.Value);

                     

                        Result : Internal_Acc_Node :=  new Internal_Rec_Node;
                  begin
                        Set_Node (Result, Item);
                     return Internal_Value_Access (Result);
                  end;
               
               when Member_Index_For_Refd_Decl_Kind =>
                  declare
                     Item : constant Ref_Result_Kind
                     := Analysis.Kind (Value.Value);

                     

                        Result : Internal_Acc_Ref_Result_Kind :=  new Internal_Rec_Ref_Result_Kind;
                  begin
                        Result.Value := Item;
                     return Internal_Value_Access (Result);
                  end;

            when others =>
               --  Validation in public wrappers is supposed to prevent calling
               --  this function on invalid members.
               raise Program_Error;
         end case;
      end Eval_Member;

      

      ---------
      -- "=" --
      ---------

      overriding function "=" (Left, Right : Internal_Rec_Refd_Def) return Boolean is
      begin
         return Left.Value = Right.Value;
      end "=";

      -------------
      -- Type_Of --
      -------------

      overriding function Type_Of (Value : Internal_Rec_Refd_Def) return Type_Index is
      begin
         return Type_Index_For_Refd_Def;
      end Type_Of;

      -------------------
      -- Create_Struct --
      -------------------

      function Create_Struct
        (Values : Internal_Value_Array) return Internal_Acc_Refd_Def
      is
         
            F_Def_Name : Defining_Name := Get_Node (Internal_Acc_Node (Values (1)).all).As_Defining_Name;
            F_Kind : Ref_Result_Kind renames Internal_Acc_Ref_Result_Kind (Values (2)).Value;
      begin

         return Result : constant Internal_Acc_Refd_Def := new Internal_Rec_Refd_Def do
            Result.Value := Create_Refd_Def (F_Def_Name, F_Kind);
         end return;
      end Create_Struct;

      -----------------
      -- Eval_Member --
      -----------------

      overriding function Eval_Member
        (Value  : Internal_Rec_Refd_Def;
         Member : Struct_Member_Index) return Internal_Value_Access is
      begin
         case Member is
               
               when Member_Index_For_Refd_Def_Def_Name =>
                  declare
                     Item : constant Defining_Name
                           'Class
                     := Analysis.Def_Name (Value.Value);

                     

                        Result : Internal_Acc_Node :=  new Internal_Rec_Node;
                  begin
                        Set_Node (Result, Item);
                     return Internal_Value_Access (Result);
                  end;
               
               when Member_Index_For_Refd_Def_Kind =>
                  declare
                     Item : constant Ref_Result_Kind
                     := Analysis.Kind (Value.Value);

                     

                        Result : Internal_Acc_Ref_Result_Kind :=  new Internal_Rec_Ref_Result_Kind;
                  begin
                        Result.Value := Item;
                     return Internal_Value_Access (Result);
                  end;

            when others =>
               --  Validation in public wrappers is supposed to prevent calling
               --  this function on invalid members.
               raise Program_Error;
         end case;
      end Eval_Member;

      

      ---------
      -- "=" --
      ---------

      overriding function "=" (Left, Right : Internal_Rec_Shape) return Boolean is
      begin
         return Left.Value = Right.Value;
      end "=";

      -------------
      -- Type_Of --
      -------------

      overriding function Type_Of (Value : Internal_Rec_Shape) return Type_Index is
      begin
         return Type_Index_For_Shape;
      end Type_Of;

      -------------------
      -- Create_Struct --
      -------------------

      function Create_Struct
        (Values : Internal_Value_Array) return Internal_Acc_Shape
      is
         
            F_Components : Base_Formal_Param_Decl_Array renames Internal_Acc_Base_Formal_Param_Decl_Array (Values (1)).Value.all;
            F_Discriminants_Values : Discriminant_Values_Array renames Internal_Acc_Discriminant_Values_Array (Values (2)).Value.all;
      begin

         return Result : constant Internal_Acc_Shape := new Internal_Rec_Shape do
            Result.Value := Create_Shape (F_Components, F_Discriminants_Values);
         end return;
      end Create_Struct;

      -----------------
      -- Eval_Member --
      -----------------

      overriding function Eval_Member
        (Value  : Internal_Rec_Shape;
         Member : Struct_Member_Index) return Internal_Value_Access is
      begin
         case Member is
               
               when Member_Index_For_Shape_Components =>
                  declare
                     Item : constant Base_Formal_Param_Decl_Array
                     := Analysis.Components (Value.Value);

                     

                        Result : Internal_Acc_Base_Formal_Param_Decl_Array :=  new Internal_Rec_Base_Formal_Param_Decl_Array;
                  begin
                        Result.Value := new Base_Formal_Param_Decl_Array'(Item);
                     return Internal_Value_Access (Result);
                  end;
               
               when Member_Index_For_Shape_Discriminants_Values =>
                  declare
                     Item : constant Discriminant_Values_Array
                     := Analysis.Discriminants_Values (Value.Value);

                     

                        Result : Internal_Acc_Discriminant_Values_Array :=  new Internal_Rec_Discriminant_Values_Array;
                  begin
                        Result.Value := new Discriminant_Values_Array'(Item);
                     return Internal_Value_Access (Result);
                  end;

            when others =>
               --  Validation in public wrappers is supposed to prevent calling
               --  this function on invalid members.
               raise Program_Error;
         end case;
      end Eval_Member;

      

      ---------
      -- "=" --
      ---------

      overriding function "=" (Left, Right : Internal_Rec_Substitution) return Boolean is
      begin
         return Left.Value = Right.Value;
      end "=";

      -------------
      -- Type_Of --
      -------------

      overriding function Type_Of (Value : Internal_Rec_Substitution) return Type_Index is
      begin
         return Type_Index_For_Substitution;
      end Type_Of;

      -------------------
      -- Create_Struct --
      -------------------

      function Create_Struct
        (Values : Internal_Value_Array) return Internal_Acc_Substitution
      is
         
            F_From_Decl : Basic_Decl := Get_Node (Internal_Acc_Node (Values (1)).all).As_Basic_Decl;
            F_To_Value : Big_Integer;
            F_Value_Type : Base_Type_Decl := Get_Node (Internal_Acc_Node (Values (3)).all).As_Base_Type_Decl;
      begin
            Get_Big_Int (Internal_Acc_Big_Int (Values (2)).all, F_To_Value);

         return Result : constant Internal_Acc_Substitution := new Internal_Rec_Substitution do
            Result.Value := Create_Substitution (F_From_Decl, F_To_Value, F_Value_Type);
         end return;
      end Create_Struct;

      -----------------
      -- Eval_Member --
      -----------------

      overriding function Eval_Member
        (Value  : Internal_Rec_Substitution;
         Member : Struct_Member_Index) return Internal_Value_Access is
      begin
         case Member is
               
               when Member_Index_For_Substitution_From_Decl =>
                  declare
                     Item : constant Basic_Decl
                           'Class
                     := Analysis.From_Decl (Value.Value);

                     

                        Result : Internal_Acc_Node :=  new Internal_Rec_Node;
                  begin
                        Set_Node (Result, Item);
                     return Internal_Value_Access (Result);
                  end;
               
               when Member_Index_For_Substitution_To_Value =>
                  declare
                     Item : constant Big_Integer
                     := Analysis.To_Value (Value.Value);

                     

                        Result : Internal_Acc_Big_Int :=  new Internal_Rec_Big_Int;
                  begin
                        Set_Big_Int (Result, Item);
                     return Internal_Value_Access (Result);
                  end;
               
               when Member_Index_For_Substitution_Value_Type =>
                  declare
                     Item : constant Base_Type_Decl
                           'Class
                     := Analysis.Value_Type (Value.Value);

                     

                        Result : Internal_Acc_Node :=  new Internal_Rec_Node;
                  begin
                        Set_Node (Result, Item);
                     return Internal_Value_Access (Result);
                  end;

            when others =>
               --  Validation in public wrappers is supposed to prevent calling
               --  this function on invalid members.
               raise Program_Error;
         end case;
      end Eval_Member;


   -------------------
   -- Create_Struct --
   -------------------

   function Create_Struct
     (Struct_Type : Type_Index;
      Values      : Internal_Value_Array) return Internal_Value_Access is
   begin

      case Struct_Type is
            when Type_Index_For_Aspect =>
               declare
                  Result : constant Internal_Acc_Aspect :=
                    Create_Struct (Values);
               begin
                  return Internal_Value_Access (Result);
               end;
            when Type_Index_For_Completion_Item =>
               declare
                  Result : constant Internal_Acc_Completion_Item :=
                    Create_Struct (Values);
               begin
                  return Internal_Value_Access (Result);
               end;
            when Type_Index_For_Discrete_Range =>
               declare
                  Result : constant Internal_Acc_Discrete_Range :=
                    Create_Struct (Values);
               begin
                  return Internal_Value_Access (Result);
               end;
            when Type_Index_For_Discriminant_Values =>
               declare
                  Result : constant Internal_Acc_Discriminant_Values :=
                    Create_Struct (Values);
               begin
                  return Internal_Value_Access (Result);
               end;
            when Type_Index_For_Doc_Annotation =>
               declare
                  Result : constant Internal_Acc_Doc_Annotation :=
                    Create_Struct (Values);
               begin
                  return Internal_Value_Access (Result);
               end;
            when Type_Index_For_Param_Actual =>
               declare
                  Result : constant Internal_Acc_Param_Actual :=
                    Create_Struct (Values);
               begin
                  return Internal_Value_Access (Result);
               end;
            when Type_Index_For_Ref_Result =>
               declare
                  Result : constant Internal_Acc_Ref_Result :=
                    Create_Struct (Values);
               begin
                  return Internal_Value_Access (Result);
               end;
            when Type_Index_For_Refd_Decl =>
               declare
                  Result : constant Internal_Acc_Refd_Decl :=
                    Create_Struct (Values);
               begin
                  return Internal_Value_Access (Result);
               end;
            when Type_Index_For_Refd_Def =>
               declare
                  Result : constant Internal_Acc_Refd_Def :=
                    Create_Struct (Values);
               begin
                  return Internal_Value_Access (Result);
               end;
            when Type_Index_For_Shape =>
               declare
                  Result : constant Internal_Acc_Shape :=
                    Create_Struct (Values);
               begin
                  return Internal_Value_Access (Result);
               end;
            when Type_Index_For_Substitution =>
               declare
                  Result : constant Internal_Acc_Substitution :=
                    Create_Struct (Values);
               begin
                  return Internal_Value_Access (Result);
               end;

         when others =>
            --  Validation in public wrappers is supposed to prevent calling
            --  this function on non-array types.
            return (raise Program_Error);
      end case;
   end Create_Struct;

   ----------------------
   -- Eval_Node_Member --
   ----------------------

   function Eval_Node_Member
     (Node      : Internal_Acc_Node;
      Member    : Struct_Member_Index;
      Arguments : Internal_Value_Array) return Internal_Value_Access
   is
      Int_Entity : constant Implementation.Internal_Entity :=
        +Langkit_Support.Internal.Conversions.Unwrap_Node (Node.Value);
      N          : constant Ada_Node :=
        Public_Converters.Wrap_Node.all (Int_Entity.Node, Int_Entity.Info);
      Kind       : constant Ada_Node_Kind_Type := N.Kind;
      Result     : Internal_Value_Access;
   begin
      

      case Member is
when Member_Index_For_Ada_Node_P_Declarative_Scope =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N.P_Declarative_Scope);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Ada_Node_P_Enclosing_Compilation_Unit =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N.P_Enclosing_Compilation_Unit);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Ada_Node_P_Get_Uninstantiated_Node =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N.P_Get_Uninstantiated_Node);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Ada_Node_P_Complete =>
declare
R : Internal_Acc_Completion_Item_Iterator :=  new Internal_Rec_Completion_Item_Iterator;
begin
R.Value := N.P_Complete;
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Ada_Node_P_Valid_Keywords =>
declare
R : Internal_Acc_Symbol_Array :=  new Internal_Rec_Symbol_Array;
begin
R.Value := new Unbounded_Text_Type_Array'(N.P_Valid_Keywords);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Ada_Node_P_Generic_Instantiations =>
declare
R : Internal_Acc_Generic_Instantiation_Array :=  new Internal_Rec_Generic_Instantiation_Array;
begin
R.Value := new Generic_Instantiation_Array'(N.P_Generic_Instantiations);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Ada_Node_P_Semantic_Parent =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N.P_Semantic_Parent);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Ada_Node_P_Parent_Basic_Decl =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N.P_Parent_Basic_Decl);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Ada_Node_P_Filter_Is_Imported_By =>
declare
Arg_Units : Analysis_Unit_Array renames Internal_Acc_Analysis_Unit_Array (Arguments (1)).Value.all;
Arg_Transitive : Boolean renames Internal_Acc_Bool (Arguments (2)).Value;
begin
declare
R : Internal_Acc_Analysis_Unit_Array :=  new Internal_Rec_Analysis_Unit_Array;
begin
R.Value := new Analysis_Unit_Array'(N.P_Filter_Is_Imported_By (Arg_Units, Arg_Transitive));
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Ada_Node_P_Xref_Entry_Point =>
declare
R : Internal_Acc_Bool :=  new Internal_Rec_Bool;
begin
R.Value := N.P_Xref_Entry_Point;
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Ada_Node_P_Resolve_Names =>
declare
R : Internal_Acc_Bool :=  new Internal_Rec_Bool;
begin
R.Value := N.P_Resolve_Names;
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Ada_Node_P_Standard_Unit =>
declare
R : Internal_Acc_Analysis_Unit :=  new Internal_Rec_Analysis_Unit;
begin
Set_Unit (R, N.P_Standard_Unit);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Ada_Node_P_Std_Entity =>
declare
Arg_Sym : Unbounded_Text_Type renames Internal_Acc_Symbol (Arguments (1)).Value;
begin
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N.P_Std_Entity (Arg_Sym));
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Ada_Node_P_Bool_Type =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N.P_Bool_Type);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Ada_Node_P_Int_Type =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N.P_Int_Type);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Ada_Node_P_Universal_Int_Type =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N.P_Universal_Int_Type);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Ada_Node_P_Universal_Real_Type =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N.P_Universal_Real_Type);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Ada_Node_P_Std_Char_Type =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N.P_Std_Char_Type);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Ada_Node_P_Std_Wide_Char_Type =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N.P_Std_Wide_Char_Type);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Ada_Node_P_Std_Wide_Wide_Char_Type =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N.P_Std_Wide_Wide_Char_Type);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Ada_Node_P_Top_Level_Decl =>
declare
Arg_Unit : Analysis_Unit := Get_Unit (Internal_Acc_Analysis_Unit (Arguments (1)).all);
begin
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N.P_Top_Level_Decl (Arg_Unit));
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Ada_Node_P_Choice_Match =>
declare
Arg_Value : Big_Integer;
begin
Get_Big_Int (Internal_Acc_Big_Int (Arguments (1)).all, Arg_Value);
declare
R : Internal_Acc_Bool :=  new Internal_Rec_Bool;
begin
R.Value := N.P_Choice_Match (Arg_Value);
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Ada_Node_P_Gnat_Xref =>
declare
Arg_Imprecise_Fallback : Boolean renames Internal_Acc_Bool (Arguments (1)).Value;
begin
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N.P_Gnat_Xref (Arg_Imprecise_Fallback));
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Parent =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N.Parent);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Parents =>
declare
Arg_With_Self : Boolean renames Internal_Acc_Bool (Arguments (1)).Value;
begin
declare
R : Internal_Acc_Ada_Node_Array :=  new Internal_Rec_Ada_Node_Array;
begin
R.Value := new Ada_Node_Array'(N.Parents (Arg_With_Self));
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Children =>
declare
R : Internal_Acc_Ada_Node_Array :=  new Internal_Rec_Ada_Node_Array;
begin
R.Value := new Ada_Node_Array'(N.Children);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Token_Start =>
declare
R : Internal_Acc_Token :=  new Internal_Rec_Token;
begin
R.Value := To_Generic (N.Token_Start);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Token_End =>
declare
R : Internal_Acc_Token :=  new Internal_Rec_Token;
begin
R.Value := To_Generic (N.Token_End);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Child_Index =>
declare
R : Internal_Acc_Int :=  new Internal_Rec_Int;
begin
R.Value := N.Child_Index;
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Previous_Sibling =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N.Previous_Sibling);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Next_Sibling =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N.Next_Sibling);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Unit =>
declare
R : Internal_Acc_Analysis_Unit :=  new Internal_Rec_Analysis_Unit;
begin
Set_Unit (R, N.Unit);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Is_Ghost =>
declare
R : Internal_Acc_Bool :=  new Internal_Rec_Bool;
begin
R.Value := N.Is_Ghost;
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Full_Sloc_Image =>
declare
R : Internal_Acc_String :=  new Internal_Rec_String;
begin
R.Value := To_Unbounded_Text (N.Full_Sloc_Image);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
case Ada_Ada_Node (Kind) is
when Ada_Abort_Node =>
declare
N_Bare_Abort_Node : constant Analysis.Abort_Node := N.As_Abort_Node;
begin
case Member is
when Member_Index_For_Abort_Node_P_As_Bool =>
declare
R : Internal_Acc_Bool :=  new Internal_Rec_Bool;
begin
R.Value := N_Bare_Abort_Node.P_As_Bool;
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Abstract_Node =>
declare
N_Bare_Abstract_Node : constant Analysis.Abstract_Node := N.As_Abstract_Node;
begin
case Member is
when Member_Index_For_Abstract_Node_P_As_Bool =>
declare
R : Internal_Acc_Bool :=  new Internal_Rec_Bool;
begin
R.Value := N_Bare_Abstract_Node.P_As_Bool;
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Assoc_List_Range =>
declare
N_Bare_Assoc_List : constant Analysis.Assoc_List := N.As_Assoc_List;
begin
case Member is
when Member_Index_For_Assoc_List_P_Zip_With_Params =>
declare
Arg_Imprecise_Fallback : Boolean renames Internal_Acc_Bool (Arguments (1)).Value;
begin
declare
R : Internal_Acc_Param_Actual_Array :=  new Internal_Rec_Param_Actual_Array;
begin
R.Value := new Param_Actual_Array'(N_Bare_Assoc_List.P_Zip_With_Params (Arg_Imprecise_Fallback));
Result := Internal_Value_Access (R);
end;
end;
when others => null;
end case;
end;
when Ada_Aliased_Node =>
declare
N_Bare_Aliased_Node : constant Analysis.Aliased_Node := N.As_Aliased_Node;
begin
case Member is
when Member_Index_For_Aliased_Node_P_As_Bool =>
declare
R : Internal_Acc_Bool :=  new Internal_Rec_Bool;
begin
R.Value := N_Bare_Aliased_Node.P_As_Bool;
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_All_Node =>
declare
N_Bare_All_Node : constant Analysis.All_Node := N.As_All_Node;
begin
case Member is
when Member_Index_For_All_Node_P_As_Bool =>
declare
R : Internal_Acc_Bool :=  new Internal_Rec_Bool;
begin
R.Value := N_Bare_All_Node.P_As_Bool;
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Constrained_Array_Indices_Range =>
declare
N_Bare_Constrained_Array_Indices : constant Analysis.Constrained_Array_Indices := N.As_Constrained_Array_Indices;
begin
case Member is
when Member_Index_For_Constrained_Array_Indices_F_List =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Constrained_Array_Indices.F_List);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Unconstrained_Array_Indices_Range =>
declare
N_Bare_Unconstrained_Array_Indices : constant Analysis.Unconstrained_Array_Indices := N.As_Unconstrained_Array_Indices;
begin
case Member is
when Member_Index_For_Unconstrained_Array_Indices_F_Types =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Unconstrained_Array_Indices.F_Types);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Aspect_Assoc_Range =>
declare
N_Bare_Aspect_Assoc : constant Analysis.Aspect_Assoc := N.As_Aspect_Assoc;
begin
case Member is
when Member_Index_For_Aspect_Assoc_F_Id =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Aspect_Assoc.F_Id);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Aspect_Assoc_F_Expr =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Aspect_Assoc.F_Expr);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Aspect_Assoc_P_Is_Ghost_Code =>
declare
R : Internal_Acc_Bool :=  new Internal_Rec_Bool;
begin
R.Value := N_Bare_Aspect_Assoc.P_Is_Ghost_Code;
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_At_Clause_Range =>
declare
N_Bare_At_Clause : constant Analysis.At_Clause := N.As_At_Clause;
begin
case Member is
when Member_Index_For_At_Clause_F_Name =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_At_Clause.F_Name);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_At_Clause_F_Expr =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_At_Clause.F_Expr);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Attribute_Def_Clause_Range =>
declare
N_Bare_Attribute_Def_Clause : constant Analysis.Attribute_Def_Clause := N.As_Attribute_Def_Clause;
begin
case Member is
when Member_Index_For_Attribute_Def_Clause_F_Attribute_Expr =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Attribute_Def_Clause.F_Attribute_Expr);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Attribute_Def_Clause_F_Expr =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Attribute_Def_Clause.F_Expr);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Enum_Rep_Clause_Range =>
declare
N_Bare_Enum_Rep_Clause : constant Analysis.Enum_Rep_Clause := N.As_Enum_Rep_Clause;
begin
case Member is
when Member_Index_For_Enum_Rep_Clause_F_Type_Name =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Enum_Rep_Clause.F_Type_Name);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Enum_Rep_Clause_F_Aggregate =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Enum_Rep_Clause.F_Aggregate);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Enum_Rep_Clause_P_Params =>
declare
R : Internal_Acc_Param_Actual_Array :=  new Internal_Rec_Param_Actual_Array;
begin
R.Value := new Param_Actual_Array'(N_Bare_Enum_Rep_Clause.P_Params);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Record_Rep_Clause_Range =>
declare
N_Bare_Record_Rep_Clause : constant Analysis.Record_Rep_Clause := N.As_Record_Rep_Clause;
begin
case Member is
when Member_Index_For_Record_Rep_Clause_F_Name =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Record_Rep_Clause.F_Name);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Record_Rep_Clause_F_At_Expr =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Record_Rep_Clause.F_At_Expr);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Record_Rep_Clause_F_Components =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Record_Rep_Clause.F_Components);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Aspect_Spec_Range =>
declare
N_Bare_Aspect_Spec : constant Analysis.Aspect_Spec := N.As_Aspect_Spec;
begin
case Member is
when Member_Index_For_Aspect_Spec_F_Aspect_Assocs =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Aspect_Spec.F_Aspect_Assocs);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Base_Assoc =>
declare
N_Bare_Base_Assoc : constant Analysis.Base_Assoc := N.As_Base_Assoc;
begin
case Member is
when Member_Index_For_Base_Assoc_P_Assoc_Expr =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Base_Assoc.P_Assoc_Expr);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
case Ada_Base_Assoc (Kind) is
when Ada_Contract_Case_Assoc_Range =>
declare
N_Bare_Contract_Case_Assoc : constant Analysis.Contract_Case_Assoc := N_Bare_Base_Assoc.As_Contract_Case_Assoc;
begin
case Member is
when Member_Index_For_Contract_Case_Assoc_F_Guard =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Contract_Case_Assoc.F_Guard);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Contract_Case_Assoc_F_Consequence =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Contract_Case_Assoc.F_Consequence);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Pragma_Argument_Assoc_Range =>
declare
N_Bare_Pragma_Argument_Assoc : constant Analysis.Pragma_Argument_Assoc := N_Bare_Base_Assoc.As_Pragma_Argument_Assoc;
begin
case Member is
when Member_Index_For_Pragma_Argument_Assoc_F_Name =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Pragma_Argument_Assoc.F_Name);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Pragma_Argument_Assoc_F_Expr =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Pragma_Argument_Assoc.F_Expr);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when others => null;
end case;
end;
when Ada_Base_Formal_Param_Holder =>
declare
N_Bare_Base_Formal_Param_Holder : constant Analysis.Base_Formal_Param_Holder := N.As_Base_Formal_Param_Holder;
begin
case Member is
when Member_Index_For_Base_Formal_Param_Holder_P_Abstract_Formal_Params =>
declare
R : Internal_Acc_Base_Formal_Param_Decl_Array :=  new Internal_Rec_Base_Formal_Param_Decl_Array;
begin
R.Value := new Base_Formal_Param_Decl_Array'(N_Bare_Base_Formal_Param_Holder.P_Abstract_Formal_Params);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Base_Formal_Param_Holder_P_Formal_Params =>
declare
R : Internal_Acc_Defining_Name_Array :=  new Internal_Rec_Defining_Name_Array;
begin
R.Value := new Defining_Name_Array'(N_Bare_Base_Formal_Param_Holder.P_Formal_Params);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Base_Formal_Param_Holder_P_Nb_Min_Params =>
declare
R : Internal_Acc_Int :=  new Internal_Rec_Int;
begin
R.Value := N_Bare_Base_Formal_Param_Holder.P_Nb_Min_Params;
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Base_Formal_Param_Holder_P_Nb_Max_Params =>
declare
R : Internal_Acc_Int :=  new Internal_Rec_Int;
begin
R.Value := N_Bare_Base_Formal_Param_Holder.P_Nb_Max_Params;
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Base_Formal_Param_Holder_P_Param_Types =>
declare
Arg_Origin : Ada_Node := Get_Node (Internal_Acc_Node (Arguments (1)).all);
begin
declare
R : Internal_Acc_Base_Type_Decl_Array :=  new Internal_Rec_Base_Type_Decl_Array;
begin
R.Value := new Base_Type_Decl_Array'(N_Bare_Base_Formal_Param_Holder.P_Param_Types (Arg_Origin));
Result := Internal_Value_Access (R);
end;
end;
when others => null;
end case;
case Ada_Base_Formal_Param_Holder (Kind) is
when Ada_Base_Subp_Spec =>
declare
N_Bare_Base_Subp_Spec : constant Analysis.Base_Subp_Spec := N_Bare_Base_Formal_Param_Holder.As_Base_Subp_Spec;
begin
case Member is
when Member_Index_For_Base_Subp_Spec_P_Returns =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Base_Subp_Spec.P_Returns);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Base_Subp_Spec_P_Params =>
declare
R : Internal_Acc_Param_Spec_Array :=  new Internal_Rec_Param_Spec_Array;
begin
R.Value := new Param_Spec_Array'(N_Bare_Base_Subp_Spec.P_Params);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Base_Subp_Spec_P_Primitive_Subp_Types =>
declare
Arg_Imprecise_Fallback : Boolean renames Internal_Acc_Bool (Arguments (1)).Value;
begin
declare
R : Internal_Acc_Base_Type_Decl_Array :=  new Internal_Rec_Base_Type_Decl_Array;
begin
R.Value := new Base_Type_Decl_Array'(N_Bare_Base_Subp_Spec.P_Primitive_Subp_Types (Arg_Imprecise_Fallback));
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Base_Subp_Spec_P_Primitive_Subp_First_Type =>
declare
Arg_Imprecise_Fallback : Boolean renames Internal_Acc_Bool (Arguments (1)).Value;
begin
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Base_Subp_Spec.P_Primitive_Subp_First_Type (Arg_Imprecise_Fallback));
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Base_Subp_Spec_P_Primitive_Subp_Tagged_Type =>
declare
Arg_Imprecise_Fallback : Boolean renames Internal_Acc_Bool (Arguments (1)).Value;
begin
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Base_Subp_Spec.P_Primitive_Subp_Tagged_Type (Arg_Imprecise_Fallback));
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Base_Subp_Spec_P_Return_Type =>
declare
Arg_Origin : Ada_Node := Get_Node (Internal_Acc_Node (Arguments (1)).all);
begin
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Base_Subp_Spec.P_Return_Type (Arg_Origin));
Result := Internal_Value_Access (R);
end;
end;
when others => null;
end case;
case Ada_Base_Subp_Spec (Kind) is
when Ada_Entry_Spec_Range =>
declare
N_Bare_Entry_Spec : constant Analysis.Entry_Spec := N_Bare_Base_Subp_Spec.As_Entry_Spec;
begin
case Member is
when Member_Index_For_Entry_Spec_F_Entry_Name =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Entry_Spec.F_Entry_Name);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Entry_Spec_F_Family_Type =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Entry_Spec.F_Family_Type);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Entry_Spec_F_Entry_Params =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Entry_Spec.F_Entry_Params);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Subp_Spec_Range =>
declare
N_Bare_Subp_Spec : constant Analysis.Subp_Spec := N_Bare_Base_Subp_Spec.As_Subp_Spec;
begin
case Member is
when Member_Index_For_Subp_Spec_F_Subp_Kind =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Subp_Spec.F_Subp_Kind);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Subp_Spec_F_Subp_Name =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Subp_Spec.F_Subp_Name);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Subp_Spec_F_Subp_Params =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Subp_Spec.F_Subp_Params);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Subp_Spec_F_Subp_Returns =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Subp_Spec.F_Subp_Returns);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Synthetic_Binary_Spec_Range =>
declare
N_Bare_Synthetic_Binary_Spec : constant Analysis.Synthetic_Binary_Spec := N_Bare_Base_Subp_Spec.As_Synthetic_Binary_Spec;
begin
case Member is
when Member_Index_For_Synthetic_Binary_Spec_F_Left_Param =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Synthetic_Binary_Spec.F_Left_Param);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Synthetic_Binary_Spec_F_Right_Param =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Synthetic_Binary_Spec.F_Right_Param);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Synthetic_Binary_Spec_F_Return_Type_Expr =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Synthetic_Binary_Spec.F_Return_Type_Expr);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Synthetic_Unary_Spec_Range =>
declare
N_Bare_Synthetic_Unary_Spec : constant Analysis.Synthetic_Unary_Spec := N_Bare_Base_Subp_Spec.As_Synthetic_Unary_Spec;
begin
case Member is
when Member_Index_For_Synthetic_Unary_Spec_F_Right_Param =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Synthetic_Unary_Spec.F_Right_Param);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Synthetic_Unary_Spec_F_Return_Type_Expr =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Synthetic_Unary_Spec.F_Return_Type_Expr);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when others => null;
end case;
end;
when Ada_Component_List_Range =>
declare
N_Bare_Component_List : constant Analysis.Component_List := N_Bare_Base_Formal_Param_Holder.As_Component_List;
begin
case Member is
when Member_Index_For_Component_List_F_Components =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Component_List.F_Components);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Component_List_F_Variant_Part =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Component_List.F_Variant_Part);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Known_Discriminant_Part_Range =>
declare
N_Bare_Known_Discriminant_Part : constant Analysis.Known_Discriminant_Part := N_Bare_Base_Formal_Param_Holder.As_Known_Discriminant_Part;
begin
case Member is
when Member_Index_For_Known_Discriminant_Part_F_Discr_Specs =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Known_Discriminant_Part.F_Discr_Specs);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Entry_Completion_Formal_Params_Range =>
declare
N_Bare_Entry_Completion_Formal_Params : constant Analysis.Entry_Completion_Formal_Params := N_Bare_Base_Formal_Param_Holder.As_Entry_Completion_Formal_Params;
begin
case Member is
when Member_Index_For_Entry_Completion_Formal_Params_F_Params =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Entry_Completion_Formal_Params.F_Params);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Generic_Formal_Part_Range =>
declare
N_Bare_Generic_Formal_Part : constant Analysis.Generic_Formal_Part := N_Bare_Base_Formal_Param_Holder.As_Generic_Formal_Part;
begin
case Member is
when Member_Index_For_Generic_Formal_Part_F_Decls =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Generic_Formal_Part.F_Decls);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when others => null;
end case;
end;
when Ada_Base_Record_Def =>
declare
N_Bare_Base_Record_Def : constant Analysis.Base_Record_Def := N.As_Base_Record_Def;
begin
case Member is
when Member_Index_For_Base_Record_Def_F_Components =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Base_Record_Def.F_Components);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Basic_Assoc =>
declare
N_Bare_Basic_Assoc : constant Analysis.Basic_Assoc := N.As_Basic_Assoc;
begin
case Member is
when Member_Index_For_Basic_Assoc_P_Get_Params =>
declare
Arg_Imprecise_Fallback : Boolean renames Internal_Acc_Bool (Arguments (1)).Value;
begin
declare
R : Internal_Acc_Defining_Name_Array :=  new Internal_Rec_Defining_Name_Array;
begin
R.Value := new Defining_Name_Array'(N_Bare_Basic_Assoc.P_Get_Params (Arg_Imprecise_Fallback));
Result := Internal_Value_Access (R);
end;
end;
when others => null;
end case;
case Ada_Basic_Assoc (Kind) is
when Ada_Aggregate_Assoc_Range =>
declare
N_Bare_Aggregate_Assoc : constant Analysis.Aggregate_Assoc := N_Bare_Basic_Assoc.As_Aggregate_Assoc;
begin
case Member is
when Member_Index_For_Aggregate_Assoc_F_Designators =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Aggregate_Assoc.F_Designators);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Aggregate_Assoc_F_R_Expr =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Aggregate_Assoc.F_R_Expr);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Composite_Constraint_Assoc_Range =>
declare
N_Bare_Composite_Constraint_Assoc : constant Analysis.Composite_Constraint_Assoc := N_Bare_Basic_Assoc.As_Composite_Constraint_Assoc;
begin
case Member is
when Member_Index_For_Composite_Constraint_Assoc_F_Ids =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Composite_Constraint_Assoc.F_Ids);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Composite_Constraint_Assoc_F_Constraint_Expr =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Composite_Constraint_Assoc.F_Constraint_Expr);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Iterated_Assoc_Range =>
declare
N_Bare_Iterated_Assoc : constant Analysis.Iterated_Assoc := N_Bare_Basic_Assoc.As_Iterated_Assoc;
begin
case Member is
when Member_Index_For_Iterated_Assoc_F_Spec =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Iterated_Assoc.F_Spec);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Iterated_Assoc_F_R_Expr =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Iterated_Assoc.F_R_Expr);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Param_Assoc_Range =>
declare
N_Bare_Param_Assoc : constant Analysis.Param_Assoc := N_Bare_Basic_Assoc.As_Param_Assoc;
begin
case Member is
when Member_Index_For_Param_Assoc_F_Designator =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Param_Assoc.F_Designator);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Param_Assoc_F_R_Expr =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Param_Assoc.F_R_Expr);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when others => null;
end case;
end;
when Ada_Basic_Decl =>
declare
N_Bare_Basic_Decl : constant Analysis.Basic_Decl := N.As_Basic_Decl;
begin
case Member is
when Member_Index_For_Basic_Decl_P_Is_Formal =>
declare
R : Internal_Acc_Bool :=  new Internal_Rec_Bool;
begin
R.Value := N_Bare_Basic_Decl.P_Is_Formal;
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Basic_Decl_P_Doc_Annotations =>
declare
R : Internal_Acc_Doc_Annotation_Array :=  new Internal_Rec_Doc_Annotation_Array;
begin
R.Value := new Doc_Annotation_Array'(N_Bare_Basic_Decl.P_Doc_Annotations);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Basic_Decl_P_Doc =>
declare
R : Internal_Acc_String :=  new Internal_Rec_String;
begin
R.Value := To_Unbounded_Text (N_Bare_Basic_Decl.P_Doc);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Basic_Decl_P_Previous_Part_For_Decl =>
declare
Arg_Imprecise_Fallback : Boolean renames Internal_Acc_Bool (Arguments (1)).Value;
begin
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Basic_Decl.P_Previous_Part_For_Decl (Arg_Imprecise_Fallback));
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Basic_Decl_P_Canonical_Part =>
declare
Arg_Imprecise_Fallback : Boolean renames Internal_Acc_Bool (Arguments (1)).Value;
begin
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Basic_Decl.P_Canonical_Part (Arg_Imprecise_Fallback));
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Basic_Decl_P_All_Parts =>
declare
Arg_Imprecise_Fallback : Boolean renames Internal_Acc_Bool (Arguments (1)).Value;
begin
declare
R : Internal_Acc_Basic_Decl_Array :=  new Internal_Rec_Basic_Decl_Array;
begin
R.Value := new Basic_Decl_Array'(N_Bare_Basic_Decl.P_All_Parts (Arg_Imprecise_Fallback));
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Basic_Decl_P_Is_Static_Decl =>
declare
Arg_Imprecise_Fallback : Boolean renames Internal_Acc_Bool (Arguments (1)).Value;
begin
declare
R : Internal_Acc_Bool :=  new Internal_Rec_Bool;
begin
R.Value := N_Bare_Basic_Decl.P_Is_Static_Decl (Arg_Imprecise_Fallback);
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Basic_Decl_F_Aspects =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Basic_Decl.F_Aspects);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Basic_Decl_P_Get_Aspect_Assoc =>
declare
Arg_Name : Unbounded_Text_Type renames Internal_Acc_Symbol (Arguments (1)).Value;
begin
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Basic_Decl.P_Get_Aspect_Assoc (Arg_Name));
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Basic_Decl_P_Get_Aspect_Spec_Expr =>
declare
Arg_Name : Unbounded_Text_Type renames Internal_Acc_Symbol (Arguments (1)).Value;
begin
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Basic_Decl.P_Get_Aspect_Spec_Expr (Arg_Name));
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Basic_Decl_P_Get_Aspect =>
declare
Arg_Name : Unbounded_Text_Type renames Internal_Acc_Symbol (Arguments (1)).Value;
Arg_Imprecise_Fallback : Boolean renames Internal_Acc_Bool (Arguments (2)).Value;
begin
declare
R : Internal_Acc_Aspect :=  new Internal_Rec_Aspect;
begin
R.Value := N_Bare_Basic_Decl.P_Get_Aspect (Arg_Name, Arg_Imprecise_Fallback);
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Basic_Decl_P_Has_Aspect =>
declare
Arg_Name : Unbounded_Text_Type renames Internal_Acc_Symbol (Arguments (1)).Value;
Arg_Imprecise_Fallback : Boolean renames Internal_Acc_Bool (Arguments (2)).Value;
begin
declare
R : Internal_Acc_Bool :=  new Internal_Rec_Bool;
begin
R.Value := N_Bare_Basic_Decl.P_Has_Aspect (Arg_Name, Arg_Imprecise_Fallback);
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Basic_Decl_P_Get_Pragma =>
declare
Arg_Name : Unbounded_Text_Type renames Internal_Acc_Symbol (Arguments (1)).Value;
begin
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Basic_Decl.P_Get_Pragma (Arg_Name));
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Basic_Decl_P_Get_Representation_Clause =>
declare
Arg_Name : Unbounded_Text_Type renames Internal_Acc_Symbol (Arguments (1)).Value;
Arg_Imprecise_Fallback : Boolean renames Internal_Acc_Bool (Arguments (2)).Value;
begin
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Basic_Decl.P_Get_Representation_Clause (Arg_Name, Arg_Imprecise_Fallback));
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Basic_Decl_P_Get_At_Clause =>
declare
Arg_Imprecise_Fallback : Boolean renames Internal_Acc_Bool (Arguments (1)).Value;
begin
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Basic_Decl.P_Get_At_Clause (Arg_Imprecise_Fallback));
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Basic_Decl_P_Is_Imported =>
declare
R : Internal_Acc_Bool :=  new Internal_Rec_Bool;
begin
R.Value := N_Bare_Basic_Decl.P_Is_Imported;
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Basic_Decl_P_Is_Ghost_Code =>
declare
R : Internal_Acc_Bool :=  new Internal_Rec_Bool;
begin
R.Value := N_Bare_Basic_Decl.P_Is_Ghost_Code;
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Basic_Decl_P_Is_Compilation_Unit_Root =>
declare
R : Internal_Acc_Bool :=  new Internal_Rec_Bool;
begin
R.Value := N_Bare_Basic_Decl.P_Is_Compilation_Unit_Root;
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Basic_Decl_P_Is_Visible =>
declare
Arg_From_Node : Ada_Node := Get_Node (Internal_Acc_Node (Arguments (1)).all);
begin
declare
R : Internal_Acc_Bool :=  new Internal_Rec_Bool;
begin
R.Value := N_Bare_Basic_Decl.P_Is_Visible (Arg_From_Node);
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Basic_Decl_P_Base_Subp_Declarations =>
declare
Arg_Imprecise_Fallback : Boolean renames Internal_Acc_Bool (Arguments (1)).Value;
begin
declare
R : Internal_Acc_Basic_Decl_Array :=  new Internal_Rec_Basic_Decl_Array;
begin
R.Value := new Basic_Decl_Array'(N_Bare_Basic_Decl.P_Base_Subp_Declarations (Arg_Imprecise_Fallback));
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Basic_Decl_P_Root_Subp_Declarations =>
declare
Arg_Origin : Ada_Node := Get_Node (Internal_Acc_Node (Arguments (1)).all);
Arg_Imprecise_Fallback : Boolean renames Internal_Acc_Bool (Arguments (2)).Value;
begin
declare
R : Internal_Acc_Basic_Decl_Array :=  new Internal_Rec_Basic_Decl_Array;
begin
R.Value := new Basic_Decl_Array'(N_Bare_Basic_Decl.P_Root_Subp_Declarations (Arg_Origin, Arg_Imprecise_Fallback));
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Basic_Decl_P_Find_All_Overrides =>
declare
Arg_Units : Analysis_Unit_Array renames Internal_Acc_Analysis_Unit_Array (Arguments (1)).Value.all;
Arg_Imprecise_Fallback : Boolean renames Internal_Acc_Bool (Arguments (2)).Value;
begin
declare
R : Internal_Acc_Basic_Decl_Array :=  new Internal_Rec_Basic_Decl_Array;
begin
R.Value := new Basic_Decl_Array'(N_Bare_Basic_Decl.P_Find_All_Overrides (Arg_Units, Arg_Imprecise_Fallback));
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Basic_Decl_P_Defining_Names =>
declare
R : Internal_Acc_Defining_Name_Array :=  new Internal_Rec_Defining_Name_Array;
begin
R.Value := new Defining_Name_Array'(N_Bare_Basic_Decl.P_Defining_Names);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Basic_Decl_P_Defining_Name =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Basic_Decl.P_Defining_Name);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Basic_Decl_P_Type_Expression =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Basic_Decl.P_Type_Expression);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Basic_Decl_P_Subp_Spec_Or_Null =>
declare
Arg_Follow_Generic : Boolean renames Internal_Acc_Bool (Arguments (1)).Value;
begin
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Basic_Decl.P_Subp_Spec_Or_Null (Arg_Follow_Generic));
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Basic_Decl_P_Is_Subprogram =>
declare
R : Internal_Acc_Bool :=  new Internal_Rec_Bool;
begin
R.Value := N_Bare_Basic_Decl.P_Is_Subprogram;
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Basic_Decl_P_Relative_Name =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Basic_Decl.P_Relative_Name);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Basic_Decl_P_Relative_Name_Text =>
declare
R : Internal_Acc_Symbol :=  new Internal_Rec_Symbol;
begin
R.Value := N_Bare_Basic_Decl.P_Relative_Name_Text;
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Basic_Decl_P_Next_Part_For_Decl =>
declare
Arg_Imprecise_Fallback : Boolean renames Internal_Acc_Bool (Arguments (1)).Value;
begin
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Basic_Decl.P_Next_Part_For_Decl (Arg_Imprecise_Fallback));
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Basic_Decl_P_Body_Part_For_Decl =>
declare
Arg_Imprecise_Fallback : Boolean renames Internal_Acc_Bool (Arguments (1)).Value;
begin
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Basic_Decl.P_Body_Part_For_Decl (Arg_Imprecise_Fallback));
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Basic_Decl_P_Most_Visible_Part =>
declare
Arg_Origin : Ada_Node := Get_Node (Internal_Acc_Node (Arguments (1)).all);
Arg_Imprecise_Fallback : Boolean renames Internal_Acc_Bool (Arguments (2)).Value;
begin
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Basic_Decl.P_Most_Visible_Part (Arg_Origin, Arg_Imprecise_Fallback));
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Basic_Decl_P_Fully_Qualified_Name_Array =>
declare
Arg_Include_Profile : Boolean renames Internal_Acc_Bool (Arguments (1)).Value;
begin
declare
R : Internal_Acc_Symbol_Array :=  new Internal_Rec_Symbol_Array;
begin
R.Value := new Unbounded_Text_Type_Array'(N_Bare_Basic_Decl.P_Fully_Qualified_Name_Array (Arg_Include_Profile));
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Basic_Decl_P_Fully_Qualified_Name =>
declare
R : Internal_Acc_String :=  new Internal_Rec_String;
begin
R.Value := To_Unbounded_Text (N_Bare_Basic_Decl.P_Fully_Qualified_Name);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Basic_Decl_P_Canonical_Fully_Qualified_Name =>
declare
R : Internal_Acc_String :=  new Internal_Rec_String;
begin
R.Value := To_Unbounded_Text (N_Bare_Basic_Decl.P_Canonical_Fully_Qualified_Name);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Basic_Decl_P_Unique_Identifying_Name =>
declare
R : Internal_Acc_String :=  new Internal_Rec_String;
begin
R.Value := To_Unbounded_Text (N_Bare_Basic_Decl.P_Unique_Identifying_Name);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Basic_Decl_P_Is_Constant_Object =>
declare
R : Internal_Acc_Bool :=  new Internal_Rec_Bool;
begin
R.Value := N_Bare_Basic_Decl.P_Is_Constant_Object;
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
case Ada_Basic_Decl (Kind) is
when Ada_Abstract_State_Decl_Range =>
declare
N_Bare_Abstract_State_Decl : constant Analysis.Abstract_State_Decl := N_Bare_Basic_Decl.As_Abstract_State_Decl;
begin
case Member is
when Member_Index_For_Abstract_State_Decl_F_Name =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Abstract_State_Decl.F_Name);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Anonymous_Expr_Decl_Range =>
declare
N_Bare_Anonymous_Expr_Decl : constant Analysis.Anonymous_Expr_Decl := N_Bare_Basic_Decl.As_Anonymous_Expr_Decl;
begin
case Member is
when Member_Index_For_Anonymous_Expr_Decl_F_Expr =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Anonymous_Expr_Decl.F_Expr);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Anonymous_Expr_Decl_P_Get_Formal =>
declare
Arg_Imprecise_Fallback : Boolean renames Internal_Acc_Bool (Arguments (1)).Value;
begin
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Anonymous_Expr_Decl.P_Get_Formal (Arg_Imprecise_Fallback));
Result := Internal_Value_Access (R);
end;
end;
when others => null;
end case;
end;
when Ada_Base_Formal_Param_Decl =>
declare
N_Bare_Base_Formal_Param_Decl : constant Analysis.Base_Formal_Param_Decl := N_Bare_Basic_Decl.As_Base_Formal_Param_Decl;
begin
case Member is
when Member_Index_For_Base_Formal_Param_Decl_P_Formal_Type =>
declare
Arg_Origin : Ada_Node := Get_Node (Internal_Acc_Node (Arguments (1)).all);
begin
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Base_Formal_Param_Decl.P_Formal_Type (Arg_Origin));
Result := Internal_Value_Access (R);
end;
end;
when others => null;
end case;
case Ada_Base_Formal_Param_Decl (Kind) is
when Ada_Component_Decl_Range =>
declare
N_Bare_Component_Decl : constant Analysis.Component_Decl := N_Bare_Base_Formal_Param_Decl.As_Component_Decl;
begin
case Member is
when Member_Index_For_Component_Decl_F_Ids =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Component_Decl.F_Ids);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Component_Decl_F_Component_Def =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Component_Decl.F_Component_Def);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Component_Decl_F_Default_Expr =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Component_Decl.F_Default_Expr);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Discriminant_Spec_Range =>
declare
N_Bare_Discriminant_Spec : constant Analysis.Discriminant_Spec := N_Bare_Base_Formal_Param_Decl.As_Discriminant_Spec;
begin
case Member is
when Member_Index_For_Discriminant_Spec_F_Ids =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Discriminant_Spec.F_Ids);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Discriminant_Spec_F_Type_Expr =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Discriminant_Spec.F_Type_Expr);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Discriminant_Spec_F_Default_Expr =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Discriminant_Spec.F_Default_Expr);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Generic_Formal =>
declare
N_Bare_Generic_Formal : constant Analysis.Generic_Formal := N_Bare_Base_Formal_Param_Decl.As_Generic_Formal;
begin
case Member is
when Member_Index_For_Generic_Formal_F_Decl =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Generic_Formal.F_Decl);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Param_Spec_Range =>
declare
N_Bare_Param_Spec : constant Analysis.Param_Spec := N_Bare_Base_Formal_Param_Decl.As_Param_Spec;
begin
case Member is
when Member_Index_For_Param_Spec_F_Ids =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Param_Spec.F_Ids);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Param_Spec_F_Has_Aliased =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Param_Spec.F_Has_Aliased);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Param_Spec_F_Mode =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Param_Spec.F_Mode);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Param_Spec_F_Type_Expr =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Param_Spec.F_Type_Expr);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Param_Spec_F_Default_Expr =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Param_Spec.F_Default_Expr);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Synthetic_Formal_Param_Decl_Range =>
declare
N_Bare_Synthetic_Formal_Param_Decl : constant Analysis.Synthetic_Formal_Param_Decl := N_Bare_Base_Formal_Param_Decl.As_Synthetic_Formal_Param_Decl;
begin
case Member is
when Member_Index_For_Synthetic_Formal_Param_Decl_F_Param_Type =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Synthetic_Formal_Param_Decl.F_Param_Type);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when others => null;
end case;
end;
when Ada_Base_Package_Decl =>
declare
N_Bare_Base_Package_Decl : constant Analysis.Base_Package_Decl := N_Bare_Basic_Decl.As_Base_Package_Decl;
begin
case Member is
when Member_Index_For_Base_Package_Decl_F_Package_Name =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Base_Package_Decl.F_Package_Name);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Base_Package_Decl_F_Public_Part =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Base_Package_Decl.F_Public_Part);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Base_Package_Decl_F_Private_Part =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Base_Package_Decl.F_Private_Part);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Base_Package_Decl_F_End_Name =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Base_Package_Decl.F_End_Name);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Base_Package_Decl_P_Body_Part =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Base_Package_Decl.P_Body_Part);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Base_Type_Decl =>
declare
N_Bare_Base_Type_Decl : constant Analysis.Base_Type_Decl := N_Bare_Basic_Decl.As_Base_Type_Decl;
begin
case Member is
when Member_Index_For_Base_Type_Decl_F_Name =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Base_Type_Decl.F_Name);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Base_Type_Decl_P_Base_Subtype =>
declare
Arg_Origin : Ada_Node := Get_Node (Internal_Acc_Node (Arguments (1)).all);
begin
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Base_Type_Decl.P_Base_Subtype (Arg_Origin));
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Base_Type_Decl_P_Private_Completion =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Base_Type_Decl.P_Private_Completion);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Base_Type_Decl_P_Is_Inherited_Primitive =>
declare
Arg_P : Basic_Decl := Get_Node (Internal_Acc_Node (Arguments (1)).all).As_Basic_Decl;
begin
declare
R : Internal_Acc_Bool :=  new Internal_Rec_Bool;
begin
R.Value := N_Bare_Base_Type_Decl.P_Is_Inherited_Primitive (Arg_P);
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Base_Type_Decl_P_Get_Record_Representation_Clause =>
declare
Arg_Imprecise_Fallback : Boolean renames Internal_Acc_Bool (Arguments (1)).Value;
begin
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Base_Type_Decl.P_Get_Record_Representation_Clause (Arg_Imprecise_Fallback));
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Base_Type_Decl_P_Get_Enum_Representation_Clause =>
declare
Arg_Imprecise_Fallback : Boolean renames Internal_Acc_Bool (Arguments (1)).Value;
begin
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Base_Type_Decl.P_Get_Enum_Representation_Clause (Arg_Imprecise_Fallback));
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Base_Type_Decl_P_Is_Record_Type =>
declare
Arg_Origin : Ada_Node := Get_Node (Internal_Acc_Node (Arguments (1)).all);
begin
declare
R : Internal_Acc_Bool :=  new Internal_Rec_Bool;
begin
R.Value := N_Bare_Base_Type_Decl.P_Is_Record_Type (Arg_Origin);
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Base_Type_Decl_P_Is_Array_Type =>
declare
Arg_Origin : Ada_Node := Get_Node (Internal_Acc_Node (Arguments (1)).all);
begin
declare
R : Internal_Acc_Bool :=  new Internal_Rec_Bool;
begin
R.Value := N_Bare_Base_Type_Decl.P_Is_Array_Type (Arg_Origin);
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Base_Type_Decl_P_Find_Derived_Types =>
declare
Arg_Root : Ada_Node := Get_Node (Internal_Acc_Node (Arguments (1)).all);
Arg_Origin : Ada_Node := Get_Node (Internal_Acc_Node (Arguments (2)).all);
Arg_Imprecise_Fallback : Boolean renames Internal_Acc_Bool (Arguments (3)).Value;
begin
declare
R : Internal_Acc_Type_Decl_Array :=  new Internal_Rec_Type_Decl_Array;
begin
R.Value := new Type_Decl_Array'(N_Bare_Base_Type_Decl.P_Find_Derived_Types (Arg_Root, Arg_Origin, Arg_Imprecise_Fallback));
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Base_Type_Decl_P_Is_Real_Type =>
declare
Arg_Origin : Ada_Node := Get_Node (Internal_Acc_Node (Arguments (1)).all);
begin
declare
R : Internal_Acc_Bool :=  new Internal_Rec_Bool;
begin
R.Value := N_Bare_Base_Type_Decl.P_Is_Real_Type (Arg_Origin);
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Base_Type_Decl_P_Is_Float_Type =>
declare
Arg_Origin : Ada_Node := Get_Node (Internal_Acc_Node (Arguments (1)).all);
begin
declare
R : Internal_Acc_Bool :=  new Internal_Rec_Bool;
begin
R.Value := N_Bare_Base_Type_Decl.P_Is_Float_Type (Arg_Origin);
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Base_Type_Decl_P_Is_Fixed_Point =>
declare
Arg_Origin : Ada_Node := Get_Node (Internal_Acc_Node (Arguments (1)).all);
begin
declare
R : Internal_Acc_Bool :=  new Internal_Rec_Bool;
begin
R.Value := N_Bare_Base_Type_Decl.P_Is_Fixed_Point (Arg_Origin);
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Base_Type_Decl_P_Is_Enum_Type =>
declare
Arg_Origin : Ada_Node := Get_Node (Internal_Acc_Node (Arguments (1)).all);
begin
declare
R : Internal_Acc_Bool :=  new Internal_Rec_Bool;
begin
R.Value := N_Bare_Base_Type_Decl.P_Is_Enum_Type (Arg_Origin);
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Base_Type_Decl_P_Is_Access_Type =>
declare
Arg_Origin : Ada_Node := Get_Node (Internal_Acc_Node (Arguments (1)).all);
begin
declare
R : Internal_Acc_Bool :=  new Internal_Rec_Bool;
begin
R.Value := N_Bare_Base_Type_Decl.P_Is_Access_Type (Arg_Origin);
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Base_Type_Decl_P_Is_Char_Type =>
declare
Arg_Origin : Ada_Node := Get_Node (Internal_Acc_Node (Arguments (1)).all);
begin
declare
R : Internal_Acc_Bool :=  new Internal_Rec_Bool;
begin
R.Value := N_Bare_Base_Type_Decl.P_Is_Char_Type (Arg_Origin);
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Base_Type_Decl_P_Discrete_Range =>
declare
R : Internal_Acc_Discrete_Range :=  new Internal_Rec_Discrete_Range;
begin
R.Value := N_Bare_Base_Type_Decl.P_Discrete_Range;
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Base_Type_Decl_P_Is_Discrete_Type =>
declare
Arg_Origin : Ada_Node := Get_Node (Internal_Acc_Node (Arguments (1)).all);
begin
declare
R : Internal_Acc_Bool :=  new Internal_Rec_Bool;
begin
R.Value := N_Bare_Base_Type_Decl.P_Is_Discrete_Type (Arg_Origin);
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Base_Type_Decl_P_Is_Int_Type =>
declare
Arg_Origin : Ada_Node := Get_Node (Internal_Acc_Node (Arguments (1)).all);
begin
declare
R : Internal_Acc_Bool :=  new Internal_Rec_Bool;
begin
R.Value := N_Bare_Base_Type_Decl.P_Is_Int_Type (Arg_Origin);
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Base_Type_Decl_P_Accessed_Type =>
declare
Arg_Origin : Ada_Node := Get_Node (Internal_Acc_Node (Arguments (1)).all);
begin
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Base_Type_Decl.P_Accessed_Type (Arg_Origin));
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Base_Type_Decl_P_Is_Tagged_Type =>
declare
Arg_Origin : Ada_Node := Get_Node (Internal_Acc_Node (Arguments (1)).all);
begin
declare
R : Internal_Acc_Bool :=  new Internal_Rec_Bool;
begin
R.Value := N_Bare_Base_Type_Decl.P_Is_Tagged_Type (Arg_Origin);
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Base_Type_Decl_P_Base_Type =>
declare
Arg_Origin : Ada_Node := Get_Node (Internal_Acc_Node (Arguments (1)).all);
begin
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Base_Type_Decl.P_Base_Type (Arg_Origin));
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Base_Type_Decl_P_Base_Types =>
declare
Arg_Origin : Ada_Node := Get_Node (Internal_Acc_Node (Arguments (1)).all);
begin
declare
R : Internal_Acc_Base_Type_Decl_Array :=  new Internal_Rec_Base_Type_Decl_Array;
begin
R.Value := new Base_Type_Decl_Array'(N_Bare_Base_Type_Decl.P_Base_Types (Arg_Origin));
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Base_Type_Decl_P_Find_All_Derived_Types =>
declare
Arg_Units : Analysis_Unit_Array renames Internal_Acc_Analysis_Unit_Array (Arguments (1)).Value.all;
Arg_Imprecise_Fallback : Boolean renames Internal_Acc_Bool (Arguments (2)).Value;
begin
declare
R : Internal_Acc_Type_Decl_Array :=  new Internal_Rec_Type_Decl_Array;
begin
R.Value := new Type_Decl_Array'(N_Bare_Base_Type_Decl.P_Find_All_Derived_Types (Arg_Units, Arg_Imprecise_Fallback));
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Base_Type_Decl_P_Comp_Type =>
declare
Arg_Is_Subscript : Boolean renames Internal_Acc_Bool (Arguments (1)).Value;
Arg_Origin : Ada_Node := Get_Node (Internal_Acc_Node (Arguments (2)).all);
begin
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Base_Type_Decl.P_Comp_Type (Arg_Is_Subscript, Arg_Origin));
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Base_Type_Decl_P_Index_Type =>
declare
Arg_Dim : Integer renames Internal_Acc_Int (Arguments (1)).Value;
Arg_Origin : Ada_Node := Get_Node (Internal_Acc_Node (Arguments (2)).all);
begin
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Base_Type_Decl.P_Index_Type (Arg_Dim, Arg_Origin));
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Base_Type_Decl_P_Is_Derived_Type =>
declare
Arg_Other_Type : Base_Type_Decl := Get_Node (Internal_Acc_Node (Arguments (1)).all).As_Base_Type_Decl;
Arg_Origin : Ada_Node := Get_Node (Internal_Acc_Node (Arguments (2)).all);
begin
declare
R : Internal_Acc_Bool :=  new Internal_Rec_Bool;
begin
R.Value := N_Bare_Base_Type_Decl.P_Is_Derived_Type (Arg_Other_Type, Arg_Origin);
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Base_Type_Decl_P_Is_Interface_Type =>
declare
Arg_Origin : Ada_Node := Get_Node (Internal_Acc_Node (Arguments (1)).all);
begin
declare
R : Internal_Acc_Bool :=  new Internal_Rec_Bool;
begin
R.Value := N_Bare_Base_Type_Decl.P_Is_Interface_Type (Arg_Origin);
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Base_Type_Decl_P_Matching_Type =>
declare
Arg_Expected_Type : Base_Type_Decl := Get_Node (Internal_Acc_Node (Arguments (1)).all).As_Base_Type_Decl;
Arg_Origin : Ada_Node := Get_Node (Internal_Acc_Node (Arguments (2)).all);
begin
declare
R : Internal_Acc_Bool :=  new Internal_Rec_Bool;
begin
R.Value := N_Bare_Base_Type_Decl.P_Matching_Type (Arg_Expected_Type, Arg_Origin);
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Base_Type_Decl_P_Canonical_Type =>
declare
Arg_Origin : Ada_Node := Get_Node (Internal_Acc_Node (Arguments (1)).all);
begin
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Base_Type_Decl.P_Canonical_Type (Arg_Origin));
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Base_Type_Decl_P_Previous_Part =>
declare
Arg_Go_To_Incomplete : Boolean renames Internal_Acc_Bool (Arguments (1)).Value;
begin
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Base_Type_Decl.P_Previous_Part (Arg_Go_To_Incomplete));
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Base_Type_Decl_P_Next_Part =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Base_Type_Decl.P_Next_Part);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Base_Type_Decl_P_Full_View =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Base_Type_Decl.P_Full_View);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Base_Type_Decl_P_Is_Definite_Subtype =>
declare
Arg_Origin : Ada_Node := Get_Node (Internal_Acc_Node (Arguments (1)).all);
begin
declare
R : Internal_Acc_Bool :=  new Internal_Rec_Bool;
begin
R.Value := N_Bare_Base_Type_Decl.P_Is_Definite_Subtype (Arg_Origin);
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Base_Type_Decl_P_Is_Private =>
declare
R : Internal_Acc_Bool :=  new Internal_Rec_Bool;
begin
R.Value := N_Bare_Base_Type_Decl.P_Is_Private;
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Base_Type_Decl_P_Discriminants_List =>
declare
Arg_Origin : Ada_Node := Get_Node (Internal_Acc_Node (Arguments (1)).all);
begin
declare
R : Internal_Acc_Base_Formal_Param_Decl_Array :=  new Internal_Rec_Base_Formal_Param_Decl_Array;
begin
R.Value := new Base_Formal_Param_Decl_Array'(N_Bare_Base_Type_Decl.P_Discriminants_List (Arg_Origin));
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Base_Type_Decl_P_Root_Type =>
declare
Arg_Origin : Ada_Node := Get_Node (Internal_Acc_Node (Arguments (1)).all);
begin
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Base_Type_Decl.P_Root_Type (Arg_Origin));
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Base_Type_Decl_P_Shapes =>
declare
Arg_Include_Discriminants : Boolean renames Internal_Acc_Bool (Arguments (1)).Value;
Arg_Origin : Ada_Node := Get_Node (Internal_Acc_Node (Arguments (2)).all);
begin
declare
R : Internal_Acc_Shape_Array :=  new Internal_Rec_Shape_Array;
begin
R.Value := new Shape_Array'(N_Bare_Base_Type_Decl.P_Shapes (Arg_Include_Discriminants, Arg_Origin));
Result := Internal_Value_Access (R);
end;
end;
when others => null;
end case;
case Ada_Base_Type_Decl (Kind) is
when Ada_Base_Subtype_Decl =>
declare
N_Bare_Base_Subtype_Decl : constant Analysis.Base_Subtype_Decl := N_Bare_Base_Type_Decl.As_Base_Subtype_Decl;
begin
case Member is
when Member_Index_For_Base_Subtype_Decl_P_Get_Type =>
declare
Arg_Origin : Ada_Node := Get_Node (Internal_Acc_Node (Arguments (1)).all);
begin
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Base_Subtype_Decl.P_Get_Type (Arg_Origin));
Result := Internal_Value_Access (R);
end;
end;
when others => null;
end case;
case Ada_Base_Subtype_Decl (Kind) is
when Ada_Subtype_Decl_Range =>
declare
N_Bare_Subtype_Decl : constant Analysis.Subtype_Decl := N_Bare_Base_Subtype_Decl.As_Subtype_Decl;
begin
case Member is
when Member_Index_For_Subtype_Decl_F_Subtype =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Subtype_Decl.F_Subtype);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when others => null;
end case;
end;
when Ada_Incomplete_Type_Decl_Range =>
declare
N_Bare_Incomplete_Type_Decl : constant Analysis.Incomplete_Type_Decl := N_Bare_Base_Type_Decl.As_Incomplete_Type_Decl;
begin
case Member is
when Member_Index_For_Incomplete_Type_Decl_F_Discriminants =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Incomplete_Type_Decl.F_Discriminants);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
case Ada_Incomplete_Type_Decl_Range (Kind) is
when Ada_Incomplete_Formal_Type_Decl_Range =>
declare
N_Bare_Incomplete_Formal_Type_Decl : constant Analysis.Incomplete_Formal_Type_Decl := N_Bare_Incomplete_Type_Decl.As_Incomplete_Formal_Type_Decl;
begin
case Member is
when Member_Index_For_Incomplete_Formal_Type_Decl_F_Is_Tagged =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Incomplete_Formal_Type_Decl.F_Is_Tagged);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Incomplete_Formal_Type_Decl_F_Default_Type =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Incomplete_Formal_Type_Decl.F_Default_Type);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Incomplete_Tagged_Type_Decl_Range =>
declare
N_Bare_Incomplete_Tagged_Type_Decl : constant Analysis.Incomplete_Tagged_Type_Decl := N_Bare_Incomplete_Type_Decl.As_Incomplete_Tagged_Type_Decl;
begin
case Member is
when Member_Index_For_Incomplete_Tagged_Type_Decl_F_Has_Abstract =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Incomplete_Tagged_Type_Decl.F_Has_Abstract);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when others => null;
end case;
end;
when Ada_Protected_Type_Decl_Range =>
declare
N_Bare_Protected_Type_Decl : constant Analysis.Protected_Type_Decl := N_Bare_Base_Type_Decl.As_Protected_Type_Decl;
begin
case Member is
when Member_Index_For_Protected_Type_Decl_F_Discriminants =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Protected_Type_Decl.F_Discriminants);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Protected_Type_Decl_F_Interfaces =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Protected_Type_Decl.F_Interfaces);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Protected_Type_Decl_F_Definition =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Protected_Type_Decl.F_Definition);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Task_Type_Decl_Range =>
declare
N_Bare_Task_Type_Decl : constant Analysis.Task_Type_Decl := N_Bare_Base_Type_Decl.As_Task_Type_Decl;
begin
case Member is
when Member_Index_For_Task_Type_Decl_F_Discriminants =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Task_Type_Decl.F_Discriminants);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Task_Type_Decl_F_Definition =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Task_Type_Decl.F_Definition);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Type_Decl =>
declare
N_Bare_Type_Decl : constant Analysis.Type_Decl := N_Bare_Base_Type_Decl.As_Type_Decl;
begin
case Member is
when Member_Index_For_Type_Decl_F_Discriminants =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Type_Decl.F_Discriminants);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Type_Decl_F_Type_Def =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Type_Decl.F_Type_Def);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Type_Decl_P_Get_Primitives =>
declare
Arg_Only_Inherited : Boolean renames Internal_Acc_Bool (Arguments (1)).Value;
Arg_Include_Predefined_Operators : Boolean renames Internal_Acc_Bool (Arguments (2)).Value;
begin
declare
R : Internal_Acc_Basic_Decl_Array :=  new Internal_Rec_Basic_Decl_Array;
begin
R.Value := new Basic_Decl_Array'(N_Bare_Type_Decl.P_Get_Primitives (Arg_Only_Inherited, Arg_Include_Predefined_Operators));
Result := Internal_Value_Access (R);
end;
end;
when others => null;
end case;
case Ada_Type_Decl (Kind) is
when Ada_Formal_Type_Decl_Range =>
declare
N_Bare_Formal_Type_Decl : constant Analysis.Formal_Type_Decl := N_Bare_Type_Decl.As_Formal_Type_Decl;
begin
case Member is
when Member_Index_For_Formal_Type_Decl_F_Default_Type =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Formal_Type_Decl.F_Default_Type);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when others => null;
end case;
end;
when others => null;
end case;
end;
when Ada_Basic_Subp_Decl =>
declare
N_Bare_Basic_Subp_Decl : constant Analysis.Basic_Subp_Decl := N_Bare_Basic_Decl.As_Basic_Subp_Decl;
begin
case Member is
when Member_Index_For_Basic_Subp_Decl_P_Subp_Decl_Spec =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Basic_Subp_Decl.P_Subp_Decl_Spec);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
case Ada_Basic_Subp_Decl (Kind) is
when Ada_Classic_Subp_Decl =>
declare
N_Bare_Classic_Subp_Decl : constant Analysis.Classic_Subp_Decl := N_Bare_Basic_Subp_Decl.As_Classic_Subp_Decl;
begin
case Member is
when Member_Index_For_Classic_Subp_Decl_F_Overriding =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Classic_Subp_Decl.F_Overriding);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Classic_Subp_Decl_F_Subp_Spec =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Classic_Subp_Decl.F_Subp_Spec);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Classic_Subp_Decl_P_Body_Part =>
declare
Arg_Imprecise_Fallback : Boolean renames Internal_Acc_Bool (Arguments (1)).Value;
begin
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Classic_Subp_Decl.P_Body_Part (Arg_Imprecise_Fallback));
Result := Internal_Value_Access (R);
end;
end;
when others => null;
end case;
case Ada_Classic_Subp_Decl (Kind) is
when Ada_Formal_Subp_Decl =>
declare
N_Bare_Formal_Subp_Decl : constant Analysis.Formal_Subp_Decl := N_Bare_Classic_Subp_Decl.As_Formal_Subp_Decl;
begin
case Member is
when Member_Index_For_Formal_Subp_Decl_F_Default_Expr =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Formal_Subp_Decl.F_Default_Expr);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when others => null;
end case;
end;
when Ada_Entry_Decl_Range =>
declare
N_Bare_Entry_Decl : constant Analysis.Entry_Decl := N_Bare_Basic_Subp_Decl.As_Entry_Decl;
begin
case Member is
when Member_Index_For_Entry_Decl_F_Overriding =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Entry_Decl.F_Overriding);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Entry_Decl_F_Spec =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Entry_Decl.F_Spec);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Entry_Decl_P_Body_Part =>
declare
Arg_Imprecise_Fallback : Boolean renames Internal_Acc_Bool (Arguments (1)).Value;
begin
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Entry_Decl.P_Body_Part (Arg_Imprecise_Fallback));
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Entry_Decl_P_Accept_Stmts =>
declare
R : Internal_Acc_Accept_Stmt_Array :=  new Internal_Rec_Accept_Stmt_Array;
begin
R.Value := new Accept_Stmt_Array'(N_Bare_Entry_Decl.P_Accept_Stmts);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Enum_Literal_Decl_Range =>
declare
N_Bare_Enum_Literal_Decl : constant Analysis.Enum_Literal_Decl := N_Bare_Basic_Subp_Decl.As_Enum_Literal_Decl;
begin
case Member is
when Member_Index_For_Enum_Literal_Decl_F_Name =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Enum_Literal_Decl.F_Name);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Enum_Literal_Decl_P_Enum_Type =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Enum_Literal_Decl.P_Enum_Type);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
case Ada_Enum_Literal_Decl_Range (Kind) is
when Ada_Synthetic_Char_Enum_Lit_Range =>
declare
N_Bare_Synthetic_Char_Enum_Lit : constant Analysis.Synthetic_Char_Enum_Lit := N_Bare_Enum_Literal_Decl.As_Synthetic_Char_Enum_Lit;
begin
case Member is
when Member_Index_For_Synthetic_Char_Enum_Lit_P_Expr =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Synthetic_Char_Enum_Lit.P_Expr);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when others => null;
end case;
end;
when Ada_Generic_Subp_Internal_Range =>
declare
N_Bare_Generic_Subp_Internal : constant Analysis.Generic_Subp_Internal := N_Bare_Basic_Subp_Decl.As_Generic_Subp_Internal;
begin
case Member is
when Member_Index_For_Generic_Subp_Internal_F_Subp_Spec =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Generic_Subp_Internal.F_Subp_Spec);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Synthetic_Subp_Decl_Range =>
declare
N_Bare_Synthetic_Subp_Decl : constant Analysis.Synthetic_Subp_Decl := N_Bare_Basic_Subp_Decl.As_Synthetic_Subp_Decl;
begin
case Member is
when Member_Index_For_Synthetic_Subp_Decl_F_Spec =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Synthetic_Subp_Decl.F_Spec);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when others => null;
end case;
end;
when Ada_Body_Node =>
declare
N_Bare_Body_Node : constant Analysis.Body_Node := N_Bare_Basic_Decl.As_Body_Node;
begin
case Member is
when Member_Index_For_Body_Node_P_Previous_Part =>
declare
Arg_Imprecise_Fallback : Boolean renames Internal_Acc_Bool (Arguments (1)).Value;
begin
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Body_Node.P_Previous_Part (Arg_Imprecise_Fallback));
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Body_Node_P_Decl_Part =>
declare
Arg_Imprecise_Fallback : Boolean renames Internal_Acc_Bool (Arguments (1)).Value;
begin
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Body_Node.P_Decl_Part (Arg_Imprecise_Fallback));
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Body_Node_P_Subunit_Root =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Body_Node.P_Subunit_Root);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
case Ada_Body_Node (Kind) is
when Ada_Base_Subp_Body =>
declare
N_Bare_Base_Subp_Body : constant Analysis.Base_Subp_Body := N_Bare_Body_Node.As_Base_Subp_Body;
begin
case Member is
when Member_Index_For_Base_Subp_Body_F_Overriding =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Base_Subp_Body.F_Overriding);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Base_Subp_Body_F_Subp_Spec =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Base_Subp_Body.F_Subp_Spec);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
case Ada_Base_Subp_Body (Kind) is
when Ada_Expr_Function_Range =>
declare
N_Bare_Expr_Function : constant Analysis.Expr_Function := N_Bare_Base_Subp_Body.As_Expr_Function;
begin
case Member is
when Member_Index_For_Expr_Function_F_Expr =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Expr_Function.F_Expr);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Subp_Body_Range =>
declare
N_Bare_Subp_Body : constant Analysis.Subp_Body := N_Bare_Base_Subp_Body.As_Subp_Body;
begin
case Member is
when Member_Index_For_Subp_Body_F_Decls =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Subp_Body.F_Decls);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Subp_Body_F_Stmts =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Subp_Body.F_Stmts);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Subp_Body_F_End_Name =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Subp_Body.F_End_Name);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Subp_Renaming_Decl_Range =>
declare
N_Bare_Subp_Renaming_Decl : constant Analysis.Subp_Renaming_Decl := N_Bare_Base_Subp_Body.As_Subp_Renaming_Decl;
begin
case Member is
when Member_Index_For_Subp_Renaming_Decl_F_Renames =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Subp_Renaming_Decl.F_Renames);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when others => null;
end case;
end;
when Ada_Body_Stub =>
declare
N_Bare_Body_Stub : constant Analysis.Body_Stub := N_Bare_Body_Node.As_Body_Stub;
begin
case Member is
when Member_Index_For_Body_Stub_P_Syntactic_Fully_Qualified_Name =>
declare
R : Internal_Acc_Symbol_Array :=  new Internal_Rec_Symbol_Array;
begin
R.Value := new Unbounded_Text_Type_Array'(N_Bare_Body_Stub.P_Syntactic_Fully_Qualified_Name);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
case Ada_Body_Stub (Kind) is
when Ada_Package_Body_Stub_Range =>
declare
N_Bare_Package_Body_Stub : constant Analysis.Package_Body_Stub := N_Bare_Body_Stub.As_Package_Body_Stub;
begin
case Member is
when Member_Index_For_Package_Body_Stub_F_Name =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Package_Body_Stub.F_Name);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Protected_Body_Stub_Range =>
declare
N_Bare_Protected_Body_Stub : constant Analysis.Protected_Body_Stub := N_Bare_Body_Stub.As_Protected_Body_Stub;
begin
case Member is
when Member_Index_For_Protected_Body_Stub_F_Name =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Protected_Body_Stub.F_Name);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Subp_Body_Stub_Range =>
declare
N_Bare_Subp_Body_Stub : constant Analysis.Subp_Body_Stub := N_Bare_Body_Stub.As_Subp_Body_Stub;
begin
case Member is
when Member_Index_For_Subp_Body_Stub_F_Overriding =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Subp_Body_Stub.F_Overriding);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Subp_Body_Stub_F_Subp_Spec =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Subp_Body_Stub.F_Subp_Spec);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Task_Body_Stub_Range =>
declare
N_Bare_Task_Body_Stub : constant Analysis.Task_Body_Stub := N_Bare_Body_Stub.As_Task_Body_Stub;
begin
case Member is
when Member_Index_For_Task_Body_Stub_F_Name =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Task_Body_Stub.F_Name);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when others => null;
end case;
end;
when Ada_Entry_Body_Range =>
declare
N_Bare_Entry_Body : constant Analysis.Entry_Body := N_Bare_Body_Node.As_Entry_Body;
begin
case Member is
when Member_Index_For_Entry_Body_F_Entry_Name =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Entry_Body.F_Entry_Name);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Entry_Body_F_Index_Spec =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Entry_Body.F_Index_Spec);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Entry_Body_F_Params =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Entry_Body.F_Params);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Entry_Body_F_Barrier =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Entry_Body.F_Barrier);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Entry_Body_F_Decls =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Entry_Body.F_Decls);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Entry_Body_F_Stmts =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Entry_Body.F_Stmts);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Entry_Body_F_End_Name =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Entry_Body.F_End_Name);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Package_Body_Range =>
declare
N_Bare_Package_Body : constant Analysis.Package_Body := N_Bare_Body_Node.As_Package_Body;
begin
case Member is
when Member_Index_For_Package_Body_F_Package_Name =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Package_Body.F_Package_Name);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Package_Body_F_Decls =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Package_Body.F_Decls);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Package_Body_F_Stmts =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Package_Body.F_Stmts);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Package_Body_F_End_Name =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Package_Body.F_End_Name);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Protected_Body_Range =>
declare
N_Bare_Protected_Body : constant Analysis.Protected_Body := N_Bare_Body_Node.As_Protected_Body;
begin
case Member is
when Member_Index_For_Protected_Body_F_Name =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Protected_Body.F_Name);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Protected_Body_F_Decls =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Protected_Body.F_Decls);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Protected_Body_F_End_Name =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Protected_Body.F_End_Name);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Task_Body_Range =>
declare
N_Bare_Task_Body : constant Analysis.Task_Body := N_Bare_Body_Node.As_Task_Body;
begin
case Member is
when Member_Index_For_Task_Body_F_Name =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Task_Body.F_Name);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Task_Body_F_Decls =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Task_Body.F_Decls);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Task_Body_F_Stmts =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Task_Body.F_Stmts);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Task_Body_F_End_Name =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Task_Body.F_End_Name);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when others => null;
end case;
end;
when Ada_Entry_Index_Spec_Range =>
declare
N_Bare_Entry_Index_Spec : constant Analysis.Entry_Index_Spec := N_Bare_Basic_Decl.As_Entry_Index_Spec;
begin
case Member is
when Member_Index_For_Entry_Index_Spec_F_Id =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Entry_Index_Spec.F_Id);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Entry_Index_Spec_F_Subtype =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Entry_Index_Spec.F_Subtype);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Exception_Decl_Range =>
declare
N_Bare_Exception_Decl : constant Analysis.Exception_Decl := N_Bare_Basic_Decl.As_Exception_Decl;
begin
case Member is
when Member_Index_For_Exception_Decl_F_Ids =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Exception_Decl.F_Ids);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Exception_Decl_F_Renames =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Exception_Decl.F_Renames);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Exception_Handler_Range =>
declare
N_Bare_Exception_Handler : constant Analysis.Exception_Handler := N_Bare_Basic_Decl.As_Exception_Handler;
begin
case Member is
when Member_Index_For_Exception_Handler_F_Exception_Name =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Exception_Handler.F_Exception_Name);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Exception_Handler_F_Handled_Exceptions =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Exception_Handler.F_Handled_Exceptions);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Exception_Handler_F_Stmts =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Exception_Handler.F_Stmts);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_For_Loop_Var_Decl_Range =>
declare
N_Bare_For_Loop_Var_Decl : constant Analysis.For_Loop_Var_Decl := N_Bare_Basic_Decl.As_For_Loop_Var_Decl;
begin
case Member is
when Member_Index_For_For_Loop_Var_Decl_F_Id =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_For_Loop_Var_Decl.F_Id);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_For_Loop_Var_Decl_F_Id_Type =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_For_Loop_Var_Decl.F_Id_Type);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Generic_Decl =>
declare
N_Bare_Generic_Decl : constant Analysis.Generic_Decl := N_Bare_Basic_Decl.As_Generic_Decl;
begin
case Member is
when Member_Index_For_Generic_Decl_F_Formal_Part =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Generic_Decl.F_Formal_Part);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
case Ada_Generic_Decl (Kind) is
when Ada_Generic_Package_Decl_Range =>
declare
N_Bare_Generic_Package_Decl : constant Analysis.Generic_Package_Decl := N_Bare_Generic_Decl.As_Generic_Package_Decl;
begin
case Member is
when Member_Index_For_Generic_Package_Decl_F_Package_Decl =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Generic_Package_Decl.F_Package_Decl);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Generic_Package_Decl_P_Body_Part =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Generic_Package_Decl.P_Body_Part);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Generic_Subp_Decl_Range =>
declare
N_Bare_Generic_Subp_Decl : constant Analysis.Generic_Subp_Decl := N_Bare_Generic_Decl.As_Generic_Subp_Decl;
begin
case Member is
when Member_Index_For_Generic_Subp_Decl_F_Subp_Decl =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Generic_Subp_Decl.F_Subp_Decl);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Generic_Subp_Decl_P_Body_Part =>
declare
Arg_Imprecise_Fallback : Boolean renames Internal_Acc_Bool (Arguments (1)).Value;
begin
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Generic_Subp_Decl.P_Body_Part (Arg_Imprecise_Fallback));
Result := Internal_Value_Access (R);
end;
end;
when others => null;
end case;
end;
when others => null;
end case;
end;
when Ada_Generic_Instantiation =>
declare
N_Bare_Generic_Instantiation : constant Analysis.Generic_Instantiation := N_Bare_Basic_Decl.As_Generic_Instantiation;
begin
case Member is
when Member_Index_For_Generic_Instantiation_P_Designated_Generic_Decl =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Generic_Instantiation.P_Designated_Generic_Decl);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Generic_Instantiation_P_Inst_Params =>
declare
R : Internal_Acc_Param_Actual_Array :=  new Internal_Rec_Param_Actual_Array;
begin
R.Value := new Param_Actual_Array'(N_Bare_Generic_Instantiation.P_Inst_Params);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
case Ada_Generic_Instantiation (Kind) is
when Ada_Generic_Package_Instantiation_Range =>
declare
N_Bare_Generic_Package_Instantiation : constant Analysis.Generic_Package_Instantiation := N_Bare_Generic_Instantiation.As_Generic_Package_Instantiation;
begin
case Member is
when Member_Index_For_Generic_Package_Instantiation_F_Name =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Generic_Package_Instantiation.F_Name);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Generic_Package_Instantiation_F_Generic_Pkg_Name =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Generic_Package_Instantiation.F_Generic_Pkg_Name);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Generic_Package_Instantiation_F_Params =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Generic_Package_Instantiation.F_Params);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Generic_Subp_Instantiation_Range =>
declare
N_Bare_Generic_Subp_Instantiation : constant Analysis.Generic_Subp_Instantiation := N_Bare_Generic_Instantiation.As_Generic_Subp_Instantiation;
begin
case Member is
when Member_Index_For_Generic_Subp_Instantiation_F_Overriding =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Generic_Subp_Instantiation.F_Overriding);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Generic_Subp_Instantiation_F_Kind =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Generic_Subp_Instantiation.F_Kind);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Generic_Subp_Instantiation_F_Subp_Name =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Generic_Subp_Instantiation.F_Subp_Name);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Generic_Subp_Instantiation_F_Generic_Subp_Name =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Generic_Subp_Instantiation.F_Generic_Subp_Name);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Generic_Subp_Instantiation_F_Params =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Generic_Subp_Instantiation.F_Params);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Generic_Subp_Instantiation_P_Designated_Subp =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Generic_Subp_Instantiation.P_Designated_Subp);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when others => null;
end case;
end;
when Ada_Generic_Package_Renaming_Decl_Range =>
declare
N_Bare_Generic_Package_Renaming_Decl : constant Analysis.Generic_Package_Renaming_Decl := N_Bare_Basic_Decl.As_Generic_Package_Renaming_Decl;
begin
case Member is
when Member_Index_For_Generic_Package_Renaming_Decl_F_Name =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Generic_Package_Renaming_Decl.F_Name);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Generic_Package_Renaming_Decl_F_Renames =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Generic_Package_Renaming_Decl.F_Renames);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Generic_Subp_Renaming_Decl_Range =>
declare
N_Bare_Generic_Subp_Renaming_Decl : constant Analysis.Generic_Subp_Renaming_Decl := N_Bare_Basic_Decl.As_Generic_Subp_Renaming_Decl;
begin
case Member is
when Member_Index_For_Generic_Subp_Renaming_Decl_F_Kind =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Generic_Subp_Renaming_Decl.F_Kind);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Generic_Subp_Renaming_Decl_F_Name =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Generic_Subp_Renaming_Decl.F_Name);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Generic_Subp_Renaming_Decl_F_Renames =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Generic_Subp_Renaming_Decl.F_Renames);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Label_Decl_Range =>
declare
N_Bare_Label_Decl : constant Analysis.Label_Decl := N_Bare_Basic_Decl.As_Label_Decl;
begin
case Member is
when Member_Index_For_Label_Decl_F_Name =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Label_Decl.F_Name);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Named_Stmt_Decl_Range =>
declare
N_Bare_Named_Stmt_Decl : constant Analysis.Named_Stmt_Decl := N_Bare_Basic_Decl.As_Named_Stmt_Decl;
begin
case Member is
when Member_Index_For_Named_Stmt_Decl_F_Name =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Named_Stmt_Decl.F_Name);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Number_Decl_Range =>
declare
N_Bare_Number_Decl : constant Analysis.Number_Decl := N_Bare_Basic_Decl.As_Number_Decl;
begin
case Member is
when Member_Index_For_Number_Decl_F_Ids =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Number_Decl.F_Ids);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Number_Decl_F_Expr =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Number_Decl.F_Expr);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Object_Decl_Range =>
declare
N_Bare_Object_Decl : constant Analysis.Object_Decl := N_Bare_Basic_Decl.As_Object_Decl;
begin
case Member is
when Member_Index_For_Object_Decl_F_Ids =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Object_Decl.F_Ids);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Object_Decl_F_Has_Aliased =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Object_Decl.F_Has_Aliased);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Object_Decl_F_Has_Constant =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Object_Decl.F_Has_Constant);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Object_Decl_F_Mode =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Object_Decl.F_Mode);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Object_Decl_F_Type_Expr =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Object_Decl.F_Type_Expr);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Object_Decl_F_Default_Expr =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Object_Decl.F_Default_Expr);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Object_Decl_F_Renaming_Clause =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Object_Decl.F_Renaming_Clause);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Object_Decl_P_Private_Part_Decl =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Object_Decl.P_Private_Part_Decl);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Object_Decl_P_Public_Part_Decl =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Object_Decl.P_Public_Part_Decl);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Package_Renaming_Decl_Range =>
declare
N_Bare_Package_Renaming_Decl : constant Analysis.Package_Renaming_Decl := N_Bare_Basic_Decl.As_Package_Renaming_Decl;
begin
case Member is
when Member_Index_For_Package_Renaming_Decl_F_Name =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Package_Renaming_Decl.F_Name);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Package_Renaming_Decl_F_Renames =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Package_Renaming_Decl.F_Renames);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Package_Renaming_Decl_P_Renamed_Package =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Package_Renaming_Decl.P_Renamed_Package);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Package_Renaming_Decl_P_Final_Renamed_Package =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Package_Renaming_Decl.P_Final_Renamed_Package);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Single_Protected_Decl_Range =>
declare
N_Bare_Single_Protected_Decl : constant Analysis.Single_Protected_Decl := N_Bare_Basic_Decl.As_Single_Protected_Decl;
begin
case Member is
when Member_Index_For_Single_Protected_Decl_F_Name =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Single_Protected_Decl.F_Name);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Single_Protected_Decl_F_Interfaces =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Single_Protected_Decl.F_Interfaces);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Single_Protected_Decl_F_Definition =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Single_Protected_Decl.F_Definition);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Single_Task_Decl_Range =>
declare
N_Bare_Single_Task_Decl : constant Analysis.Single_Task_Decl := N_Bare_Basic_Decl.As_Single_Task_Decl;
begin
case Member is
when Member_Index_For_Single_Task_Decl_F_Task_Type =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Single_Task_Decl.F_Task_Type);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when others => null;
end case;
end;
when Ada_Case_Stmt_Alternative_Range =>
declare
N_Bare_Case_Stmt_Alternative : constant Analysis.Case_Stmt_Alternative := N.As_Case_Stmt_Alternative;
begin
case Member is
when Member_Index_For_Case_Stmt_Alternative_F_Choices =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Case_Stmt_Alternative.F_Choices);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Case_Stmt_Alternative_F_Stmts =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Case_Stmt_Alternative.F_Stmts);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Compilation_Unit_Range =>
declare
N_Bare_Compilation_Unit : constant Analysis.Compilation_Unit := N.As_Compilation_Unit;
begin
case Member is
when Member_Index_For_Compilation_Unit_F_Prelude =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Compilation_Unit.F_Prelude);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Compilation_Unit_F_Body =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Compilation_Unit.F_Body);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Compilation_Unit_F_Pragmas =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Compilation_Unit.F_Pragmas);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Compilation_Unit_P_Syntactic_Fully_Qualified_Name =>
declare
R : Internal_Acc_Symbol_Array :=  new Internal_Rec_Symbol_Array;
begin
R.Value := new Unbounded_Text_Type_Array'(N_Bare_Compilation_Unit.P_Syntactic_Fully_Qualified_Name);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Compilation_Unit_P_Unit_Kind =>
declare
R : Internal_Acc_Analysis_Unit_Kind :=  new Internal_Rec_Analysis_Unit_Kind;
begin
R.Value := N_Bare_Compilation_Unit.P_Unit_Kind;
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Compilation_Unit_P_Withed_Units =>
declare
R : Internal_Acc_Compilation_Unit_Array :=  new Internal_Rec_Compilation_Unit_Array;
begin
R.Value := new Compilation_Unit_Array'(N_Bare_Compilation_Unit.P_Withed_Units);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Compilation_Unit_P_Imported_Units =>
declare
R : Internal_Acc_Compilation_Unit_Array :=  new Internal_Rec_Compilation_Unit_Array;
begin
R.Value := new Compilation_Unit_Array'(N_Bare_Compilation_Unit.P_Imported_Units);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Compilation_Unit_P_Unit_Dependencies =>
declare
R : Internal_Acc_Compilation_Unit_Array :=  new Internal_Rec_Compilation_Unit_Array;
begin
R.Value := new Compilation_Unit_Array'(N_Bare_Compilation_Unit.P_Unit_Dependencies);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Compilation_Unit_P_Decl =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Compilation_Unit.P_Decl);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Compilation_Unit_P_Is_Preelaborable =>
declare
Arg_Imprecise_Fallback : Boolean renames Internal_Acc_Bool (Arguments (1)).Value;
begin
declare
R : Internal_Acc_Bool :=  new Internal_Rec_Bool;
begin
R.Value := N_Bare_Compilation_Unit.P_Is_Preelaborable (Arg_Imprecise_Fallback);
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Compilation_Unit_P_Other_Part =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Compilation_Unit.P_Other_Part);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Compilation_Unit_P_Has_Restriction =>
declare
Arg_Name : Unbounded_Text_Type renames Internal_Acc_Symbol (Arguments (1)).Value;
begin
declare
R : Internal_Acc_Bool :=  new Internal_Rec_Bool;
begin
R.Value := N_Bare_Compilation_Unit.P_Has_Restriction (Arg_Name);
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Compilation_Unit_P_All_Config_Pragmas =>
declare
R : Internal_Acc_Pragma_Array :=  new Internal_Rec_Pragma_Array;
begin
R.Value := new Pragma_Node_Array'(N_Bare_Compilation_Unit.P_All_Config_Pragmas);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Compilation_Unit_P_Config_Pragmas =>
declare
Arg_Name : Unbounded_Text_Type renames Internal_Acc_Symbol (Arguments (1)).Value;
begin
declare
R : Internal_Acc_Pragma_Array :=  new Internal_Rec_Pragma_Array;
begin
R.Value := new Pragma_Node_Array'(N_Bare_Compilation_Unit.P_Config_Pragmas (Arg_Name));
Result := Internal_Value_Access (R);
end;
end;
when others => null;
end case;
end;
when Ada_Component_Clause_Range =>
declare
N_Bare_Component_Clause : constant Analysis.Component_Clause := N.As_Component_Clause;
begin
case Member is
when Member_Index_For_Component_Clause_F_Id =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Component_Clause.F_Id);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Component_Clause_F_Position =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Component_Clause.F_Position);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Component_Clause_F_Range =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Component_Clause.F_Range);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Component_Def_Range =>
declare
N_Bare_Component_Def : constant Analysis.Component_Def := N.As_Component_Def;
begin
case Member is
when Member_Index_For_Component_Def_F_Has_Aliased =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Component_Def.F_Has_Aliased);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Component_Def_F_Has_Constant =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Component_Def.F_Has_Constant);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Component_Def_F_Type_Expr =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Component_Def.F_Type_Expr);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Constant_Node =>
declare
N_Bare_Constant_Node : constant Analysis.Constant_Node := N.As_Constant_Node;
begin
case Member is
when Member_Index_For_Constant_Node_P_As_Bool =>
declare
R : Internal_Acc_Bool :=  new Internal_Rec_Bool;
begin
R.Value := N_Bare_Constant_Node.P_As_Bool;
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Composite_Constraint_Range =>
declare
N_Bare_Composite_Constraint : constant Analysis.Composite_Constraint := N.As_Composite_Constraint;
begin
case Member is
when Member_Index_For_Composite_Constraint_F_Constraints =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Composite_Constraint.F_Constraints);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Composite_Constraint_P_Is_Index_Constraint =>
declare
R : Internal_Acc_Bool :=  new Internal_Rec_Bool;
begin
R.Value := N_Bare_Composite_Constraint.P_Is_Index_Constraint;
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Composite_Constraint_P_Is_Discriminant_Constraint =>
declare
R : Internal_Acc_Bool :=  new Internal_Rec_Bool;
begin
R.Value := N_Bare_Composite_Constraint.P_Is_Discriminant_Constraint;
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Delta_Constraint_Range =>
declare
N_Bare_Delta_Constraint : constant Analysis.Delta_Constraint := N.As_Delta_Constraint;
begin
case Member is
when Member_Index_For_Delta_Constraint_F_Digits =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Delta_Constraint.F_Digits);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Delta_Constraint_F_Range =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Delta_Constraint.F_Range);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Digits_Constraint_Range =>
declare
N_Bare_Digits_Constraint : constant Analysis.Digits_Constraint := N.As_Digits_Constraint;
begin
case Member is
when Member_Index_For_Digits_Constraint_F_Digits =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Digits_Constraint.F_Digits);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Digits_Constraint_F_Range =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Digits_Constraint.F_Range);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Range_Constraint_Range =>
declare
N_Bare_Range_Constraint : constant Analysis.Range_Constraint := N.As_Range_Constraint;
begin
case Member is
when Member_Index_For_Range_Constraint_F_Range =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Range_Constraint.F_Range);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Declarative_Part_Range =>
declare
N_Bare_Declarative_Part : constant Analysis.Declarative_Part := N.As_Declarative_Part;
begin
case Member is
when Member_Index_For_Declarative_Part_F_Decls =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Declarative_Part.F_Decls);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Elsif_Expr_Part_Range =>
declare
N_Bare_Elsif_Expr_Part : constant Analysis.Elsif_Expr_Part := N.As_Elsif_Expr_Part;
begin
case Member is
when Member_Index_For_Elsif_Expr_Part_F_Cond_Expr =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Elsif_Expr_Part.F_Cond_Expr);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Elsif_Expr_Part_F_Then_Expr =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Elsif_Expr_Part.F_Then_Expr);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Elsif_Stmt_Part_Range =>
declare
N_Bare_Elsif_Stmt_Part : constant Analysis.Elsif_Stmt_Part := N.As_Elsif_Stmt_Part;
begin
case Member is
when Member_Index_For_Elsif_Stmt_Part_F_Cond_Expr =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Elsif_Stmt_Part.F_Cond_Expr);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Elsif_Stmt_Part_F_Stmts =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Elsif_Stmt_Part.F_Stmts);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Expr =>
declare
N_Bare_Expr : constant Analysis.Expr := N.As_Expr;
begin
case Member is
when Member_Index_For_Expr_P_Expression_Type =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Expr.P_Expression_Type);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Expr_P_Expected_Expression_Type =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Expr.P_Expected_Expression_Type);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Expr_P_Is_Dynamically_Tagged =>
declare
Arg_Imprecise_Fallback : Boolean renames Internal_Acc_Bool (Arguments (1)).Value;
begin
declare
R : Internal_Acc_Bool :=  new Internal_Rec_Bool;
begin
R.Value := N_Bare_Expr.P_Is_Dynamically_Tagged (Arg_Imprecise_Fallback);
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Expr_P_Is_Dispatching_Call =>
declare
Arg_Imprecise_Fallback : Boolean renames Internal_Acc_Bool (Arguments (1)).Value;
begin
declare
R : Internal_Acc_Bool :=  new Internal_Rec_Bool;
begin
R.Value := N_Bare_Expr.P_Is_Dispatching_Call (Arg_Imprecise_Fallback);
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Expr_P_Is_Static_Expr =>
declare
Arg_Imprecise_Fallback : Boolean renames Internal_Acc_Bool (Arguments (1)).Value;
begin
declare
R : Internal_Acc_Bool :=  new Internal_Rec_Bool;
begin
R.Value := N_Bare_Expr.P_Is_Static_Expr (Arg_Imprecise_Fallback);
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Expr_P_First_Corresponding_Decl =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Expr.P_First_Corresponding_Decl);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Expr_P_Eval_As_Int =>
declare
R : Internal_Acc_Big_Int :=  new Internal_Rec_Big_Int;
begin
Set_Big_Int (R, N_Bare_Expr.P_Eval_As_Int);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Expr_P_Eval_As_Int_In_Env =>
declare
Arg_Env : Substitution_Array renames Internal_Acc_Substitution_Array (Arguments (1)).Value.all;
begin
declare
R : Internal_Acc_Big_Int :=  new Internal_Rec_Big_Int;
begin
Set_Big_Int (R, N_Bare_Expr.P_Eval_As_Int_In_Env (Arg_Env));
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Expr_P_Eval_As_String =>
declare
R : Internal_Acc_String :=  new Internal_Rec_String;
begin
R.Value := To_Unbounded_Text (N_Bare_Expr.P_Eval_As_String);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Expr_P_Eval_As_String_In_Env =>
declare
Arg_Env : Substitution_Array renames Internal_Acc_Substitution_Array (Arguments (1)).Value.all;
begin
declare
R : Internal_Acc_String :=  new Internal_Rec_String;
begin
R.Value := To_Unbounded_Text (N_Bare_Expr.P_Eval_As_String_In_Env (Arg_Env));
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Expr_P_Matching_Nodes =>
declare
R : Internal_Acc_Ada_Node_Array :=  new Internal_Rec_Ada_Node_Array;
begin
R.Value := new Ada_Node_Array'(N_Bare_Expr.P_Matching_Nodes);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
case Ada_Expr (Kind) is
when Ada_Abstract_State_Decl_Expr_Range =>
declare
N_Bare_Abstract_State_Decl_Expr : constant Analysis.Abstract_State_Decl_Expr := N_Bare_Expr.As_Abstract_State_Decl_Expr;
begin
case Member is
when Member_Index_For_Abstract_State_Decl_Expr_F_State_Decl =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Abstract_State_Decl_Expr.F_State_Decl);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Allocator_Range =>
declare
N_Bare_Allocator : constant Analysis.Allocator := N_Bare_Expr.As_Allocator;
begin
case Member is
when Member_Index_For_Allocator_F_Subpool =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Allocator.F_Subpool);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Allocator_F_Type_Or_Expr =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Allocator.F_Type_Or_Expr);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Allocator_P_Get_Allocated_Type =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Allocator.P_Get_Allocated_Type);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Base_Aggregate =>
declare
N_Bare_Base_Aggregate : constant Analysis.Base_Aggregate := N_Bare_Expr.As_Base_Aggregate;
begin
case Member is
when Member_Index_For_Base_Aggregate_F_Ancestor_Expr =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Base_Aggregate.F_Ancestor_Expr);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Base_Aggregate_F_Assocs =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Base_Aggregate.F_Assocs);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Base_Aggregate_P_Aggregate_Params =>
declare
R : Internal_Acc_Param_Actual_Array :=  new Internal_Rec_Param_Actual_Array;
begin
R.Value := new Param_Actual_Array'(N_Bare_Base_Aggregate.P_Aggregate_Params);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Base_Aggregate_P_Is_Subaggregate =>
declare
R : Internal_Acc_Bool :=  new Internal_Rec_Bool;
begin
R.Value := N_Bare_Base_Aggregate.P_Is_Subaggregate;
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Bin_Op_Range =>
declare
N_Bare_Bin_Op : constant Analysis.Bin_Op := N_Bare_Expr.As_Bin_Op;
begin
case Member is
when Member_Index_For_Bin_Op_F_Left =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Bin_Op.F_Left);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Bin_Op_F_Op =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Bin_Op.F_Op);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Bin_Op_F_Right =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Bin_Op.F_Right);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Case_Expr_Alternative_Range =>
declare
N_Bare_Case_Expr_Alternative : constant Analysis.Case_Expr_Alternative := N_Bare_Expr.As_Case_Expr_Alternative;
begin
case Member is
when Member_Index_For_Case_Expr_Alternative_F_Choices =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Case_Expr_Alternative.F_Choices);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Case_Expr_Alternative_F_Expr =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Case_Expr_Alternative.F_Expr);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Concat_Op_Range =>
declare
N_Bare_Concat_Op : constant Analysis.Concat_Op := N_Bare_Expr.As_Concat_Op;
begin
case Member is
when Member_Index_For_Concat_Op_F_First_Operand =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Concat_Op.F_First_Operand);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Concat_Op_F_Other_Operands =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Concat_Op.F_Other_Operands);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Concat_Op_P_Operands =>
declare
R : Internal_Acc_Expr_Array :=  new Internal_Rec_Expr_Array;
begin
R.Value := new Expr_Array'(N_Bare_Concat_Op.P_Operands);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Concat_Operand_Range =>
declare
N_Bare_Concat_Operand : constant Analysis.Concat_Operand := N_Bare_Expr.As_Concat_Operand;
begin
case Member is
when Member_Index_For_Concat_Operand_F_Operator =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Concat_Operand.F_Operator);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Concat_Operand_F_Operand =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Concat_Operand.F_Operand);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Cond_Expr =>
declare
N_Bare_Cond_Expr : constant Analysis.Cond_Expr := N_Bare_Expr.As_Cond_Expr;
begin
case Member is
when Member_Index_For_Cond_Expr_P_Dependent_Exprs =>
declare
R : Internal_Acc_Expr_Array :=  new Internal_Rec_Expr_Array;
begin
R.Value := new Expr_Array'(N_Bare_Cond_Expr.P_Dependent_Exprs);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
case Ada_Cond_Expr (Kind) is
when Ada_Case_Expr_Range =>
declare
N_Bare_Case_Expr : constant Analysis.Case_Expr := N_Bare_Cond_Expr.As_Case_Expr;
begin
case Member is
when Member_Index_For_Case_Expr_F_Expr =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Case_Expr.F_Expr);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Case_Expr_F_Cases =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Case_Expr.F_Cases);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_If_Expr_Range =>
declare
N_Bare_If_Expr : constant Analysis.If_Expr := N_Bare_Cond_Expr.As_If_Expr;
begin
case Member is
when Member_Index_For_If_Expr_F_Cond_Expr =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_If_Expr.F_Cond_Expr);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_If_Expr_F_Then_Expr =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_If_Expr.F_Then_Expr);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_If_Expr_F_Alternatives =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_If_Expr.F_Alternatives);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_If_Expr_F_Else_Expr =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_If_Expr.F_Else_Expr);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when others => null;
end case;
end;
when Ada_Contract_Cases_Range =>
declare
N_Bare_Contract_Cases : constant Analysis.Contract_Cases := N_Bare_Expr.As_Contract_Cases;
begin
case Member is
when Member_Index_For_Contract_Cases_F_Contract_Cases =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Contract_Cases.F_Contract_Cases);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Decl_Expr_Range =>
declare
N_Bare_Decl_Expr : constant Analysis.Decl_Expr := N_Bare_Expr.As_Decl_Expr;
begin
case Member is
when Member_Index_For_Decl_Expr_F_Decls =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Decl_Expr.F_Decls);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Decl_Expr_F_Expr =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Decl_Expr.F_Expr);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Membership_Expr_Range =>
declare
N_Bare_Membership_Expr : constant Analysis.Membership_Expr := N_Bare_Expr.As_Membership_Expr;
begin
case Member is
when Member_Index_For_Membership_Expr_F_Expr =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Membership_Expr.F_Expr);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Membership_Expr_F_Op =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Membership_Expr.F_Op);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Membership_Expr_F_Membership_Exprs =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Membership_Expr.F_Membership_Exprs);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Name =>
declare
N_Bare_Name : constant Analysis.Name := N_Bare_Expr.As_Name;
begin
case Member is
when Member_Index_For_Name_P_Enclosing_Defining_Name =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Name.P_Enclosing_Defining_Name);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Name_P_Is_Defining =>
declare
R : Internal_Acc_Bool :=  new Internal_Rec_Bool;
begin
R.Value := N_Bare_Name.P_Is_Defining;
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Name_P_Name_Is =>
declare
Arg_Sym : Unbounded_Text_Type renames Internal_Acc_Symbol (Arguments (1)).Value;
begin
declare
R : Internal_Acc_Bool :=  new Internal_Rec_Bool;
begin
R.Value := N_Bare_Name.P_Name_Is (Arg_Sym);
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Name_P_Is_Direct_Call =>
declare
R : Internal_Acc_Bool :=  new Internal_Rec_Bool;
begin
R.Value := N_Bare_Name.P_Is_Direct_Call;
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Name_P_Is_Access_Call =>
declare
R : Internal_Acc_Bool :=  new Internal_Rec_Bool;
begin
R.Value := N_Bare_Name.P_Is_Access_Call;
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Name_P_Is_Call =>
declare
R : Internal_Acc_Bool :=  new Internal_Rec_Bool;
begin
R.Value := N_Bare_Name.P_Is_Call;
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Name_P_Is_Dot_Call =>
declare
Arg_Imprecise_Fallback : Boolean renames Internal_Acc_Bool (Arguments (1)).Value;
begin
declare
R : Internal_Acc_Bool :=  new Internal_Rec_Bool;
begin
R.Value := N_Bare_Name.P_Is_Dot_Call (Arg_Imprecise_Fallback);
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Name_P_Failsafe_Referenced_Def_Name =>
declare
Arg_Imprecise_Fallback : Boolean renames Internal_Acc_Bool (Arguments (1)).Value;
begin
declare
R : Internal_Acc_Refd_Def :=  new Internal_Rec_Refd_Def;
begin
R.Value := N_Bare_Name.P_Failsafe_Referenced_Def_Name (Arg_Imprecise_Fallback);
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Name_P_Referenced_Defining_Name =>
declare
Arg_Imprecise_Fallback : Boolean renames Internal_Acc_Bool (Arguments (1)).Value;
begin
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Name.P_Referenced_Defining_Name (Arg_Imprecise_Fallback));
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Name_P_All_Env_Elements =>
declare
Arg_Seq : Boolean renames Internal_Acc_Bool (Arguments (1)).Value;
Arg_Seq_From : Ada_Node := Get_Node (Internal_Acc_Node (Arguments (2)).all);
begin
declare
R : Internal_Acc_Ada_Node_Array :=  new Internal_Rec_Ada_Node_Array;
begin
R.Value := new Ada_Node_Array'(N_Bare_Name.P_All_Env_Elements (Arg_Seq, Arg_Seq_From));
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Name_P_Called_Subp_Spec =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Name.P_Called_Subp_Spec);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Name_P_Referenced_Decl =>
declare
Arg_Imprecise_Fallback : Boolean renames Internal_Acc_Bool (Arguments (1)).Value;
begin
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Name.P_Referenced_Decl (Arg_Imprecise_Fallback));
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Name_P_Failsafe_Referenced_Decl =>
declare
Arg_Imprecise_Fallback : Boolean renames Internal_Acc_Bool (Arguments (1)).Value;
begin
declare
R : Internal_Acc_Refd_Decl :=  new Internal_Rec_Refd_Decl;
begin
R.Value := N_Bare_Name.P_Failsafe_Referenced_Decl (Arg_Imprecise_Fallback);
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Name_P_Referenced_Decl_Internal =>
declare
Arg_Imprecise_Fallback : Boolean renames Internal_Acc_Bool (Arguments (1)).Value;
begin
declare
R : Internal_Acc_Refd_Decl :=  new Internal_Rec_Refd_Decl;
begin
R.Value := N_Bare_Name.P_Referenced_Decl_Internal (Arg_Imprecise_Fallback);
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Name_P_Name_Designated_Type =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Name.P_Name_Designated_Type);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Name_P_Is_Static_Subtype =>
declare
Arg_Imprecise_Fallback : Boolean renames Internal_Acc_Bool (Arguments (1)).Value;
begin
declare
R : Internal_Acc_Bool :=  new Internal_Rec_Bool;
begin
R.Value := N_Bare_Name.P_Is_Static_Subtype (Arg_Imprecise_Fallback);
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Name_P_Name_Matches =>
declare
Arg_N : Name := Get_Node (Internal_Acc_Node (Arguments (1)).all).As_Name;
begin
declare
R : Internal_Acc_Bool :=  new Internal_Rec_Bool;
begin
R.Value := N_Bare_Name.P_Name_Matches (Arg_N);
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Name_P_Relative_Name =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Name.P_Relative_Name);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Name_P_Is_Operator_Name =>
declare
R : Internal_Acc_Bool :=  new Internal_Rec_Bool;
begin
R.Value := N_Bare_Name.P_Is_Operator_Name;
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Name_P_Is_Write_Reference =>
declare
Arg_Imprecise_Fallback : Boolean renames Internal_Acc_Bool (Arguments (1)).Value;
begin
declare
R : Internal_Acc_Bool :=  new Internal_Rec_Bool;
begin
R.Value := N_Bare_Name.P_Is_Write_Reference (Arg_Imprecise_Fallback);
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Name_P_Is_Static_Call =>
declare
Arg_Imprecise_Fallback : Boolean renames Internal_Acc_Bool (Arguments (1)).Value;
begin
declare
R : Internal_Acc_Bool :=  new Internal_Rec_Bool;
begin
R.Value := N_Bare_Name.P_Is_Static_Call (Arg_Imprecise_Fallback);
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Name_P_As_Symbol_Array =>
declare
R : Internal_Acc_Symbol_Array :=  new Internal_Rec_Symbol_Array;
begin
R.Value := new Unbounded_Text_Type_Array'(N_Bare_Name.P_As_Symbol_Array);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Name_P_Canonical_Text =>
declare
R : Internal_Acc_Symbol :=  new Internal_Rec_Symbol;
begin
R.Value := N_Bare_Name.P_Canonical_Text;
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Name_P_Is_Constant =>
declare
R : Internal_Acc_Bool :=  new Internal_Rec_Bool;
begin
R.Value := N_Bare_Name.P_Is_Constant;
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Name_P_Call_Params =>
declare
R : Internal_Acc_Param_Actual_Array :=  new Internal_Rec_Param_Actual_Array;
begin
R.Value := new Param_Actual_Array'(N_Bare_Name.P_Call_Params);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
case Ada_Name (Kind) is
when Ada_Attribute_Ref_Range =>
declare
N_Bare_Attribute_Ref : constant Analysis.Attribute_Ref := N_Bare_Name.As_Attribute_Ref;
begin
case Member is
when Member_Index_For_Attribute_Ref_F_Prefix =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Attribute_Ref.F_Prefix);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Attribute_Ref_F_Attribute =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Attribute_Ref.F_Attribute);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Attribute_Ref_F_Args =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Attribute_Ref.F_Args);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Call_Expr_Range =>
declare
N_Bare_Call_Expr : constant Analysis.Call_Expr := N_Bare_Name.As_Call_Expr;
begin
case Member is
when Member_Index_For_Call_Expr_F_Name =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Call_Expr.F_Name);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Call_Expr_F_Suffix =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Call_Expr.F_Suffix);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Call_Expr_P_Kind =>
declare
R : Internal_Acc_Call_Expr_Kind :=  new Internal_Rec_Call_Expr_Kind;
begin
R.Value := N_Bare_Call_Expr.P_Kind;
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Call_Expr_P_Is_Array_Slice =>
declare
R : Internal_Acc_Bool :=  new Internal_Rec_Bool;
begin
R.Value := N_Bare_Call_Expr.P_Is_Array_Slice;
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Defining_Name_Range =>
declare
N_Bare_Defining_Name : constant Analysis.Defining_Name := N_Bare_Name.As_Defining_Name;
begin
case Member is
when Member_Index_For_Defining_Name_F_Name =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Defining_Name.F_Name);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Defining_Name_P_Canonical_Fully_Qualified_Name =>
declare
R : Internal_Acc_String :=  new Internal_Rec_String;
begin
R.Value := To_Unbounded_Text (N_Bare_Defining_Name.P_Canonical_Fully_Qualified_Name);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Defining_Name_P_Unique_Identifying_Name =>
declare
R : Internal_Acc_String :=  new Internal_Rec_String;
begin
R.Value := To_Unbounded_Text (N_Bare_Defining_Name.P_Unique_Identifying_Name);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Defining_Name_P_Fully_Qualified_Name_Array =>
declare
R : Internal_Acc_Symbol_Array :=  new Internal_Rec_Symbol_Array;
begin
R.Value := new Unbounded_Text_Type_Array'(N_Bare_Defining_Name.P_Fully_Qualified_Name_Array);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Defining_Name_P_Fully_Qualified_Name =>
declare
R : Internal_Acc_String :=  new Internal_Rec_String;
begin
R.Value := To_Unbounded_Text (N_Bare_Defining_Name.P_Fully_Qualified_Name);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Defining_Name_P_Basic_Decl =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Defining_Name.P_Basic_Decl);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Defining_Name_P_Find_Refs =>
declare
Arg_Root : Ada_Node := Get_Node (Internal_Acc_Node (Arguments (1)).all);
Arg_Imprecise_Fallback : Boolean renames Internal_Acc_Bool (Arguments (2)).Value;
begin
declare
R : Internal_Acc_Ref_Result_Array :=  new Internal_Rec_Ref_Result_Array;
begin
R.Value := new Ref_Result_Array'(N_Bare_Defining_Name.P_Find_Refs (Arg_Root, Arg_Imprecise_Fallback));
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Defining_Name_P_Find_All_References =>
declare
Arg_Units : Analysis_Unit_Array renames Internal_Acc_Analysis_Unit_Array (Arguments (1)).Value.all;
Arg_Follow_Renamings : Boolean renames Internal_Acc_Bool (Arguments (2)).Value;
Arg_Imprecise_Fallback : Boolean renames Internal_Acc_Bool (Arguments (3)).Value;
begin
declare
R : Internal_Acc_Ref_Result_Array :=  new Internal_Rec_Ref_Result_Array;
begin
R.Value := new Ref_Result_Array'(N_Bare_Defining_Name.P_Find_All_References (Arg_Units, Arg_Follow_Renamings, Arg_Imprecise_Fallback));
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Defining_Name_P_Find_All_Calls =>
declare
Arg_Units : Analysis_Unit_Array renames Internal_Acc_Analysis_Unit_Array (Arguments (1)).Value.all;
Arg_Follow_Renamings : Boolean renames Internal_Acc_Bool (Arguments (2)).Value;
Arg_Imprecise_Fallback : Boolean renames Internal_Acc_Bool (Arguments (3)).Value;
begin
declare
R : Internal_Acc_Ref_Result_Array :=  new Internal_Rec_Ref_Result_Array;
begin
R.Value := new Ref_Result_Array'(N_Bare_Defining_Name.P_Find_All_Calls (Arg_Units, Arg_Follow_Renamings, Arg_Imprecise_Fallback));
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Defining_Name_P_Next_Part =>
declare
Arg_Imprecise_Fallback : Boolean renames Internal_Acc_Bool (Arguments (1)).Value;
begin
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Defining_Name.P_Next_Part (Arg_Imprecise_Fallback));
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Defining_Name_P_Previous_Part =>
declare
Arg_Imprecise_Fallback : Boolean renames Internal_Acc_Bool (Arguments (1)).Value;
begin
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Defining_Name.P_Previous_Part (Arg_Imprecise_Fallback));
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Defining_Name_P_Canonical_Part =>
declare
Arg_Imprecise_Fallback : Boolean renames Internal_Acc_Bool (Arguments (1)).Value;
begin
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Defining_Name.P_Canonical_Part (Arg_Imprecise_Fallback));
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Defining_Name_P_Most_Visible_Part =>
declare
Arg_Origin : Ada_Node := Get_Node (Internal_Acc_Node (Arguments (1)).all);
Arg_Imprecise_Fallback : Boolean renames Internal_Acc_Bool (Arguments (2)).Value;
begin
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Defining_Name.P_Most_Visible_Part (Arg_Origin, Arg_Imprecise_Fallback));
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Defining_Name_P_All_Parts =>
declare
Arg_Imprecise_Fallback : Boolean renames Internal_Acc_Bool (Arguments (1)).Value;
begin
declare
R : Internal_Acc_Defining_Name_Array :=  new Internal_Rec_Defining_Name_Array;
begin
R.Value := new Defining_Name_Array'(N_Bare_Defining_Name.P_All_Parts (Arg_Imprecise_Fallback));
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Defining_Name_P_Get_Aspect =>
declare
Arg_Name : Unbounded_Text_Type renames Internal_Acc_Symbol (Arguments (1)).Value;
Arg_Imprecise_Fallback : Boolean renames Internal_Acc_Bool (Arguments (2)).Value;
begin
declare
R : Internal_Acc_Aspect :=  new Internal_Rec_Aspect;
begin
R.Value := N_Bare_Defining_Name.P_Get_Aspect (Arg_Name, Arg_Imprecise_Fallback);
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Defining_Name_P_Has_Aspect =>
declare
Arg_Name : Unbounded_Text_Type renames Internal_Acc_Symbol (Arguments (1)).Value;
Arg_Imprecise_Fallback : Boolean renames Internal_Acc_Bool (Arguments (2)).Value;
begin
declare
R : Internal_Acc_Bool :=  new Internal_Rec_Bool;
begin
R.Value := N_Bare_Defining_Name.P_Has_Aspect (Arg_Name, Arg_Imprecise_Fallback);
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Defining_Name_P_Get_Pragma =>
declare
Arg_Name : Unbounded_Text_Type renames Internal_Acc_Symbol (Arguments (1)).Value;
begin
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Defining_Name.P_Get_Pragma (Arg_Name));
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Defining_Name_P_Get_Representation_Clause =>
declare
Arg_Name : Unbounded_Text_Type renames Internal_Acc_Symbol (Arguments (1)).Value;
Arg_Imprecise_Fallback : Boolean renames Internal_Acc_Bool (Arguments (2)).Value;
begin
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Defining_Name.P_Get_Representation_Clause (Arg_Name, Arg_Imprecise_Fallback));
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Defining_Name_P_Get_At_Clause =>
declare
Arg_Imprecise_Fallback : Boolean renames Internal_Acc_Bool (Arguments (1)).Value;
begin
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Defining_Name.P_Get_At_Clause (Arg_Imprecise_Fallback));
Result := Internal_Value_Access (R);
end;
end;
when Member_Index_For_Defining_Name_P_Is_Imported =>
declare
R : Internal_Acc_Bool :=  new Internal_Rec_Bool;
begin
R.Value := N_Bare_Defining_Name.P_Is_Imported;
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Defining_Name_P_Is_Ghost_Code =>
declare
R : Internal_Acc_Bool :=  new Internal_Rec_Bool;
begin
R.Value := N_Bare_Defining_Name.P_Is_Ghost_Code;
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Discrete_Subtype_Name_Range =>
declare
N_Bare_Discrete_Subtype_Name : constant Analysis.Discrete_Subtype_Name := N_Bare_Name.As_Discrete_Subtype_Name;
begin
case Member is
when Member_Index_For_Discrete_Subtype_Name_F_Subtype =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Discrete_Subtype_Name.F_Subtype);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Dotted_Name_Range =>
declare
N_Bare_Dotted_Name : constant Analysis.Dotted_Name := N_Bare_Name.As_Dotted_Name;
begin
case Member is
when Member_Index_For_Dotted_Name_F_Prefix =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Dotted_Name.F_Prefix);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Dotted_Name_F_Suffix =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Dotted_Name.F_Suffix);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_End_Name_Range =>
declare
N_Bare_End_Name : constant Analysis.End_Name := N_Bare_Name.As_End_Name;
begin
case Member is
when Member_Index_For_End_Name_F_Name =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_End_Name.F_Name);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_End_Name_P_Basic_Decl =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_End_Name.P_Basic_Decl);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Explicit_Deref_Range =>
declare
N_Bare_Explicit_Deref : constant Analysis.Explicit_Deref := N_Bare_Name.As_Explicit_Deref;
begin
case Member is
when Member_Index_For_Explicit_Deref_F_Prefix =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Explicit_Deref.F_Prefix);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Qual_Expr_Range =>
declare
N_Bare_Qual_Expr : constant Analysis.Qual_Expr := N_Bare_Name.As_Qual_Expr;
begin
case Member is
when Member_Index_For_Qual_Expr_F_Prefix =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Qual_Expr.F_Prefix);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Qual_Expr_F_Suffix =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Qual_Expr.F_Suffix);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Reduce_Attribute_Ref_Range =>
declare
N_Bare_Reduce_Attribute_Ref : constant Analysis.Reduce_Attribute_Ref := N_Bare_Name.As_Reduce_Attribute_Ref;
begin
case Member is
when Member_Index_For_Reduce_Attribute_Ref_F_Prefix =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Reduce_Attribute_Ref.F_Prefix);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Reduce_Attribute_Ref_F_Attribute =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Reduce_Attribute_Ref.F_Attribute);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Reduce_Attribute_Ref_F_Args =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Reduce_Attribute_Ref.F_Args);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Char_Literal_Range =>
declare
N_Bare_Char_Literal : constant Analysis.Char_Literal := N_Bare_Name.As_Char_Literal;
begin
case Member is
when Member_Index_For_Char_Literal_P_Denoted_Value =>
declare
R : Internal_Acc_Character :=  new Internal_Rec_Character;
begin
R.Value := N_Bare_Char_Literal.P_Denoted_Value;
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_String_Literal_Range =>
declare
N_Bare_String_Literal : constant Analysis.String_Literal := N_Bare_Name.As_String_Literal;
begin
case Member is
when Member_Index_For_String_Literal_P_Denoted_Value =>
declare
R : Internal_Acc_String :=  new Internal_Rec_String;
begin
R.Value := To_Unbounded_Text (N_Bare_String_Literal.P_Denoted_Value);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Int_Literal_Range =>
declare
N_Bare_Int_Literal : constant Analysis.Int_Literal := N_Bare_Name.As_Int_Literal;
begin
case Member is
when Member_Index_For_Int_Literal_P_Denoted_Value =>
declare
R : Internal_Acc_Big_Int :=  new Internal_Rec_Big_Int;
begin
Set_Big_Int (R, N_Bare_Int_Literal.P_Denoted_Value);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Update_Attribute_Ref_Range =>
declare
N_Bare_Update_Attribute_Ref : constant Analysis.Update_Attribute_Ref := N_Bare_Name.As_Update_Attribute_Ref;
begin
case Member is
when Member_Index_For_Update_Attribute_Ref_F_Prefix =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Update_Attribute_Ref.F_Prefix);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Update_Attribute_Ref_F_Attribute =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Update_Attribute_Ref.F_Attribute);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Update_Attribute_Ref_F_Values =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Update_Attribute_Ref.F_Values);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when others => null;
end case;
end;
when Ada_Paren_Expr_Range =>
declare
N_Bare_Paren_Expr : constant Analysis.Paren_Expr := N_Bare_Expr.As_Paren_Expr;
begin
case Member is
when Member_Index_For_Paren_Expr_F_Expr =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Paren_Expr.F_Expr);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Quantified_Expr_Range =>
declare
N_Bare_Quantified_Expr : constant Analysis.Quantified_Expr := N_Bare_Expr.As_Quantified_Expr;
begin
case Member is
when Member_Index_For_Quantified_Expr_F_Quantifier =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Quantified_Expr.F_Quantifier);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Quantified_Expr_F_Loop_Spec =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Quantified_Expr.F_Loop_Spec);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Quantified_Expr_F_Expr =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Quantified_Expr.F_Expr);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Raise_Expr_Range =>
declare
N_Bare_Raise_Expr : constant Analysis.Raise_Expr := N_Bare_Expr.As_Raise_Expr;
begin
case Member is
when Member_Index_For_Raise_Expr_F_Exception_Name =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Raise_Expr.F_Exception_Name);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Raise_Expr_F_Error_Message =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Raise_Expr.F_Error_Message);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Un_Op_Range =>
declare
N_Bare_Un_Op : constant Analysis.Un_Op := N_Bare_Expr.As_Un_Op;
begin
case Member is
when Member_Index_For_Un_Op_F_Op =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Un_Op.F_Op);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Un_Op_F_Expr =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Un_Op.F_Expr);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when others => null;
end case;
end;
when Ada_Handled_Stmts_Range =>
declare
N_Bare_Handled_Stmts : constant Analysis.Handled_Stmts := N.As_Handled_Stmts;
begin
case Member is
when Member_Index_For_Handled_Stmts_F_Stmts =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Handled_Stmts.F_Stmts);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Handled_Stmts_F_Exceptions =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Handled_Stmts.F_Exceptions);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Library_Item_Range =>
declare
N_Bare_Library_Item : constant Analysis.Library_Item := N.As_Library_Item;
begin
case Member is
when Member_Index_For_Library_Item_F_Has_Private =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Library_Item.F_Has_Private);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Library_Item_F_Item =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Library_Item.F_Item);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Limited_Node =>
declare
N_Bare_Limited_Node : constant Analysis.Limited_Node := N.As_Limited_Node;
begin
case Member is
when Member_Index_For_Limited_Node_P_As_Bool =>
declare
R : Internal_Acc_Bool :=  new Internal_Rec_Bool;
begin
R.Value := N_Bare_Limited_Node.P_As_Bool;
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_For_Loop_Spec_Range =>
declare
N_Bare_For_Loop_Spec : constant Analysis.For_Loop_Spec := N.As_For_Loop_Spec;
begin
case Member is
when Member_Index_For_For_Loop_Spec_F_Var_Decl =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_For_Loop_Spec.F_Var_Decl);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_For_Loop_Spec_F_Loop_Type =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_For_Loop_Spec.F_Loop_Type);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_For_Loop_Spec_F_Has_Reverse =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_For_Loop_Spec.F_Has_Reverse);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_For_Loop_Spec_F_Iter_Expr =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_For_Loop_Spec.F_Iter_Expr);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_For_Loop_Spec_F_Iter_Filter =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_For_Loop_Spec.F_Iter_Filter);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_While_Loop_Spec_Range =>
declare
N_Bare_While_Loop_Spec : constant Analysis.While_Loop_Spec := N.As_While_Loop_Spec;
begin
case Member is
when Member_Index_For_While_Loop_Spec_F_Expr =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_While_Loop_Spec.F_Expr);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Multi_Abstract_State_Decl_Range =>
declare
N_Bare_Multi_Abstract_State_Decl : constant Analysis.Multi_Abstract_State_Decl := N.As_Multi_Abstract_State_Decl;
begin
case Member is
when Member_Index_For_Multi_Abstract_State_Decl_F_Decls =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Multi_Abstract_State_Decl.F_Decls);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Not_Null =>
declare
N_Bare_Not_Null : constant Analysis.Not_Null := N.As_Not_Null;
begin
case Member is
when Member_Index_For_Not_Null_P_As_Bool =>
declare
R : Internal_Acc_Bool :=  new Internal_Rec_Bool;
begin
R.Value := N_Bare_Not_Null.P_As_Bool;
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Params_Range =>
declare
N_Bare_Params : constant Analysis.Params := N.As_Params;
begin
case Member is
when Member_Index_For_Params_F_Params =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Params.F_Params);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Paren_Abstract_State_Decl_Range =>
declare
N_Bare_Paren_Abstract_State_Decl : constant Analysis.Paren_Abstract_State_Decl := N.As_Paren_Abstract_State_Decl;
begin
case Member is
when Member_Index_For_Paren_Abstract_State_Decl_F_Decl =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Paren_Abstract_State_Decl.F_Decl);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Pp_Elsif_Directive_Range =>
declare
N_Bare_Pp_Elsif_Directive : constant Analysis.Pp_Elsif_Directive := N.As_Pp_Elsif_Directive;
begin
case Member is
when Member_Index_For_Pp_Elsif_Directive_F_Expr =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Pp_Elsif_Directive.F_Expr);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Pp_Elsif_Directive_F_Then_Kw =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Pp_Elsif_Directive.F_Then_Kw);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Pp_If_Directive_Range =>
declare
N_Bare_Pp_If_Directive : constant Analysis.Pp_If_Directive := N.As_Pp_If_Directive;
begin
case Member is
when Member_Index_For_Pp_If_Directive_F_Expr =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Pp_If_Directive.F_Expr);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Pp_If_Directive_F_Then_Kw =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Pp_If_Directive.F_Then_Kw);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Pragma_Node_Range =>
declare
N_Bare_Pragma_Node : constant Analysis.Pragma_Node := N.As_Pragma_Node;
begin
case Member is
when Member_Index_For_Pragma_Node_F_Id =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Pragma_Node.F_Id);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Pragma_Node_F_Args =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Pragma_Node.F_Args);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Pragma_Node_P_Is_Ghost_Code =>
declare
R : Internal_Acc_Bool :=  new Internal_Rec_Bool;
begin
R.Value := N_Bare_Pragma_Node.P_Is_Ghost_Code;
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Pragma_Node_P_Associated_Entities =>
declare
R : Internal_Acc_Defining_Name_Array :=  new Internal_Rec_Defining_Name_Array;
begin
R.Value := new Defining_Name_Array'(N_Bare_Pragma_Node.P_Associated_Entities);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Private_Node =>
declare
N_Bare_Private_Node : constant Analysis.Private_Node := N.As_Private_Node;
begin
case Member is
when Member_Index_For_Private_Node_P_As_Bool =>
declare
R : Internal_Acc_Bool :=  new Internal_Rec_Bool;
begin
R.Value := N_Bare_Private_Node.P_As_Bool;
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Protected_Def_Range =>
declare
N_Bare_Protected_Def : constant Analysis.Protected_Def := N.As_Protected_Def;
begin
case Member is
when Member_Index_For_Protected_Def_F_Public_Part =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Protected_Def.F_Public_Part);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Protected_Def_F_Private_Part =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Protected_Def.F_Private_Part);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Protected_Def_F_End_Name =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Protected_Def.F_End_Name);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Protected_Node =>
declare
N_Bare_Protected_Node : constant Analysis.Protected_Node := N.As_Protected_Node;
begin
case Member is
when Member_Index_For_Protected_Node_P_As_Bool =>
declare
R : Internal_Acc_Bool :=  new Internal_Rec_Bool;
begin
R.Value := N_Bare_Protected_Node.P_As_Bool;
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Range_Spec_Range =>
declare
N_Bare_Range_Spec : constant Analysis.Range_Spec := N.As_Range_Spec;
begin
case Member is
when Member_Index_For_Range_Spec_F_Range =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Range_Spec.F_Range);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Renaming_Clause_Range =>
declare
N_Bare_Renaming_Clause : constant Analysis.Renaming_Clause := N.As_Renaming_Clause;
begin
case Member is
when Member_Index_For_Renaming_Clause_F_Renamed_Object =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Renaming_Clause.F_Renamed_Object);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Reverse_Node =>
declare
N_Bare_Reverse_Node : constant Analysis.Reverse_Node := N.As_Reverse_Node;
begin
case Member is
when Member_Index_For_Reverse_Node_P_As_Bool =>
declare
R : Internal_Acc_Bool :=  new Internal_Rec_Bool;
begin
R.Value := N_Bare_Reverse_Node.P_As_Bool;
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Select_When_Part_Range =>
declare
N_Bare_Select_When_Part : constant Analysis.Select_When_Part := N.As_Select_When_Part;
begin
case Member is
when Member_Index_For_Select_When_Part_F_Cond_Expr =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Select_When_Part.F_Cond_Expr);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Select_When_Part_F_Stmts =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Select_When_Part.F_Stmts);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Stmt =>
declare
N_Bare_Stmt : constant Analysis.Stmt := N.As_Stmt;
begin
case Member is
when Member_Index_For_Stmt_P_Is_Ghost_Code =>
declare
R : Internal_Acc_Bool :=  new Internal_Rec_Bool;
begin
R.Value := N_Bare_Stmt.P_Is_Ghost_Code;
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
case Ada_Stmt (Kind) is
when Ada_Accept_Stmt_Range =>
declare
N_Bare_Accept_Stmt : constant Analysis.Accept_Stmt := N_Bare_Stmt.As_Accept_Stmt;
begin
case Member is
when Member_Index_For_Accept_Stmt_F_Name =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Accept_Stmt.F_Name);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Accept_Stmt_F_Entry_Index_Expr =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Accept_Stmt.F_Entry_Index_Expr);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Accept_Stmt_F_Params =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Accept_Stmt.F_Params);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Accept_Stmt_P_Corresponding_Entry =>
declare
Arg_Origin : Ada_Node := Get_Node (Internal_Acc_Node (Arguments (1)).all);
begin
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Accept_Stmt.P_Corresponding_Entry (Arg_Origin));
Result := Internal_Value_Access (R);
end;
end;
when others => null;
end case;
case Ada_Accept_Stmt_Range (Kind) is
when Ada_Accept_Stmt_With_Stmts_Range =>
declare
N_Bare_Accept_Stmt_With_Stmts : constant Analysis.Accept_Stmt_With_Stmts := N_Bare_Accept_Stmt.As_Accept_Stmt_With_Stmts;
begin
case Member is
when Member_Index_For_Accept_Stmt_With_Stmts_F_Stmts =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Accept_Stmt_With_Stmts.F_Stmts);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Accept_Stmt_With_Stmts_F_End_Name =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Accept_Stmt_With_Stmts.F_End_Name);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when others => null;
end case;
end;
when Ada_Base_Loop_Stmt =>
declare
N_Bare_Base_Loop_Stmt : constant Analysis.Base_Loop_Stmt := N_Bare_Stmt.As_Base_Loop_Stmt;
begin
case Member is
when Member_Index_For_Base_Loop_Stmt_F_Spec =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Base_Loop_Stmt.F_Spec);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Base_Loop_Stmt_F_Stmts =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Base_Loop_Stmt.F_Stmts);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Base_Loop_Stmt_F_End_Name =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Base_Loop_Stmt.F_End_Name);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Begin_Block_Range =>
declare
N_Bare_Begin_Block : constant Analysis.Begin_Block := N_Bare_Stmt.As_Begin_Block;
begin
case Member is
when Member_Index_For_Begin_Block_F_Stmts =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Begin_Block.F_Stmts);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Begin_Block_F_End_Name =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Begin_Block.F_End_Name);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Decl_Block_Range =>
declare
N_Bare_Decl_Block : constant Analysis.Decl_Block := N_Bare_Stmt.As_Decl_Block;
begin
case Member is
when Member_Index_For_Decl_Block_F_Decls =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Decl_Block.F_Decls);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Decl_Block_F_Stmts =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Decl_Block.F_Stmts);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Decl_Block_F_End_Name =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Decl_Block.F_End_Name);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Case_Stmt_Range =>
declare
N_Bare_Case_Stmt : constant Analysis.Case_Stmt := N_Bare_Stmt.As_Case_Stmt;
begin
case Member is
when Member_Index_For_Case_Stmt_F_Expr =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Case_Stmt.F_Expr);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Case_Stmt_F_Pragmas =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Case_Stmt.F_Pragmas);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Case_Stmt_F_Alternatives =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Case_Stmt.F_Alternatives);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Extended_Return_Stmt_Range =>
declare
N_Bare_Extended_Return_Stmt : constant Analysis.Extended_Return_Stmt := N_Bare_Stmt.As_Extended_Return_Stmt;
begin
case Member is
when Member_Index_For_Extended_Return_Stmt_F_Decl =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Extended_Return_Stmt.F_Decl);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Extended_Return_Stmt_F_Stmts =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Extended_Return_Stmt.F_Stmts);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_If_Stmt_Range =>
declare
N_Bare_If_Stmt : constant Analysis.If_Stmt := N_Bare_Stmt.As_If_Stmt;
begin
case Member is
when Member_Index_For_If_Stmt_F_Cond_Expr =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_If_Stmt.F_Cond_Expr);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_If_Stmt_F_Then_Stmts =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_If_Stmt.F_Then_Stmts);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_If_Stmt_F_Alternatives =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_If_Stmt.F_Alternatives);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_If_Stmt_F_Else_Stmts =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_If_Stmt.F_Else_Stmts);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Named_Stmt_Range =>
declare
N_Bare_Named_Stmt : constant Analysis.Named_Stmt := N_Bare_Stmt.As_Named_Stmt;
begin
case Member is
when Member_Index_For_Named_Stmt_F_Decl =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Named_Stmt.F_Decl);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Named_Stmt_F_Stmt =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Named_Stmt.F_Stmt);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Select_Stmt_Range =>
declare
N_Bare_Select_Stmt : constant Analysis.Select_Stmt := N_Bare_Stmt.As_Select_Stmt;
begin
case Member is
when Member_Index_For_Select_Stmt_F_Guards =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Select_Stmt.F_Guards);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Select_Stmt_F_Else_Stmts =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Select_Stmt.F_Else_Stmts);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Select_Stmt_F_Abort_Stmts =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Select_Stmt.F_Abort_Stmts);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Abort_Stmt_Range =>
declare
N_Bare_Abort_Stmt : constant Analysis.Abort_Stmt := N_Bare_Stmt.As_Abort_Stmt;
begin
case Member is
when Member_Index_For_Abort_Stmt_F_Names =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Abort_Stmt.F_Names);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Assign_Stmt_Range =>
declare
N_Bare_Assign_Stmt : constant Analysis.Assign_Stmt := N_Bare_Stmt.As_Assign_Stmt;
begin
case Member is
when Member_Index_For_Assign_Stmt_F_Dest =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Assign_Stmt.F_Dest);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Assign_Stmt_F_Expr =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Assign_Stmt.F_Expr);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Call_Stmt_Range =>
declare
N_Bare_Call_Stmt : constant Analysis.Call_Stmt := N_Bare_Stmt.As_Call_Stmt;
begin
case Member is
when Member_Index_For_Call_Stmt_F_Call =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Call_Stmt.F_Call);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Delay_Stmt_Range =>
declare
N_Bare_Delay_Stmt : constant Analysis.Delay_Stmt := N_Bare_Stmt.As_Delay_Stmt;
begin
case Member is
when Member_Index_For_Delay_Stmt_F_Has_Until =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Delay_Stmt.F_Has_Until);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Delay_Stmt_F_Expr =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Delay_Stmt.F_Expr);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Exit_Stmt_Range =>
declare
N_Bare_Exit_Stmt : constant Analysis.Exit_Stmt := N_Bare_Stmt.As_Exit_Stmt;
begin
case Member is
when Member_Index_For_Exit_Stmt_F_Loop_Name =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Exit_Stmt.F_Loop_Name);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Exit_Stmt_F_Cond_Expr =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Exit_Stmt.F_Cond_Expr);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Goto_Stmt_Range =>
declare
N_Bare_Goto_Stmt : constant Analysis.Goto_Stmt := N_Bare_Stmt.As_Goto_Stmt;
begin
case Member is
when Member_Index_For_Goto_Stmt_F_Label_Name =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Goto_Stmt.F_Label_Name);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Label_Range =>
declare
N_Bare_Label : constant Analysis.Label := N_Bare_Stmt.As_Label;
begin
case Member is
when Member_Index_For_Label_F_Decl =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Label.F_Decl);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Raise_Stmt_Range =>
declare
N_Bare_Raise_Stmt : constant Analysis.Raise_Stmt := N_Bare_Stmt.As_Raise_Stmt;
begin
case Member is
when Member_Index_For_Raise_Stmt_F_Exception_Name =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Raise_Stmt.F_Exception_Name);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Raise_Stmt_F_Error_Message =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Raise_Stmt.F_Error_Message);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Requeue_Stmt_Range =>
declare
N_Bare_Requeue_Stmt : constant Analysis.Requeue_Stmt := N_Bare_Stmt.As_Requeue_Stmt;
begin
case Member is
when Member_Index_For_Requeue_Stmt_F_Call_Name =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Requeue_Stmt.F_Call_Name);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Requeue_Stmt_F_Has_Abort =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Requeue_Stmt.F_Has_Abort);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Return_Stmt_Range =>
declare
N_Bare_Return_Stmt : constant Analysis.Return_Stmt := N_Bare_Stmt.As_Return_Stmt;
begin
case Member is
when Member_Index_For_Return_Stmt_F_Return_Expr =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Return_Stmt.F_Return_Expr);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when others => null;
end case;
end;
when Ada_Subunit_Range =>
declare
N_Bare_Subunit : constant Analysis.Subunit := N.As_Subunit;
begin
case Member is
when Member_Index_For_Subunit_F_Name =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Subunit.F_Name);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Subunit_F_Body =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Subunit.F_Body);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Subunit_P_Body_Root =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Subunit.P_Body_Root);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Synchronized_Node =>
declare
N_Bare_Synchronized_Node : constant Analysis.Synchronized_Node := N.As_Synchronized_Node;
begin
case Member is
when Member_Index_For_Synchronized_Node_P_As_Bool =>
declare
R : Internal_Acc_Bool :=  new Internal_Rec_Bool;
begin
R.Value := N_Bare_Synchronized_Node.P_As_Bool;
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Tagged_Node =>
declare
N_Bare_Tagged_Node : constant Analysis.Tagged_Node := N.As_Tagged_Node;
begin
case Member is
when Member_Index_For_Tagged_Node_P_As_Bool =>
declare
R : Internal_Acc_Bool :=  new Internal_Rec_Bool;
begin
R.Value := N_Bare_Tagged_Node.P_As_Bool;
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Task_Def_Range =>
declare
N_Bare_Task_Def : constant Analysis.Task_Def := N.As_Task_Def;
begin
case Member is
when Member_Index_For_Task_Def_F_Interfaces =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Task_Def.F_Interfaces);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Task_Def_F_Public_Part =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Task_Def.F_Public_Part);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Task_Def_F_Private_Part =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Task_Def.F_Private_Part);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Task_Def_F_End_Name =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Task_Def.F_End_Name);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Access_Def =>
declare
N_Bare_Access_Def : constant Analysis.Access_Def := N.As_Access_Def;
begin
case Member is
when Member_Index_For_Access_Def_F_Has_Not_Null =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Access_Def.F_Has_Not_Null);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
case Ada_Access_Def (Kind) is
when Ada_Access_To_Subp_Def_Range =>
declare
N_Bare_Access_To_Subp_Def : constant Analysis.Access_To_Subp_Def := N_Bare_Access_Def.As_Access_To_Subp_Def;
begin
case Member is
when Member_Index_For_Access_To_Subp_Def_F_Has_Protected =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Access_To_Subp_Def.F_Has_Protected);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Access_To_Subp_Def_F_Subp_Spec =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Access_To_Subp_Def.F_Subp_Spec);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Anonymous_Type_Access_Def_Range =>
declare
N_Bare_Anonymous_Type_Access_Def : constant Analysis.Anonymous_Type_Access_Def := N_Bare_Access_Def.As_Anonymous_Type_Access_Def;
begin
case Member is
when Member_Index_For_Anonymous_Type_Access_Def_F_Type_Decl =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Anonymous_Type_Access_Def.F_Type_Decl);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Type_Access_Def_Range =>
declare
N_Bare_Type_Access_Def : constant Analysis.Type_Access_Def := N_Bare_Access_Def.As_Type_Access_Def;
begin
case Member is
when Member_Index_For_Type_Access_Def_F_Has_All =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Type_Access_Def.F_Has_All);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Type_Access_Def_F_Has_Constant =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Type_Access_Def.F_Has_Constant);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Type_Access_Def_F_Subtype_Indication =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Type_Access_Def.F_Subtype_Indication);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when others => null;
end case;
end;
when Ada_Array_Type_Def_Range =>
declare
N_Bare_Array_Type_Def : constant Analysis.Array_Type_Def := N.As_Array_Type_Def;
begin
case Member is
when Member_Index_For_Array_Type_Def_F_Indices =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Array_Type_Def.F_Indices);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Array_Type_Def_F_Component_Type =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Array_Type_Def.F_Component_Type);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Derived_Type_Def_Range =>
declare
N_Bare_Derived_Type_Def : constant Analysis.Derived_Type_Def := N.As_Derived_Type_Def;
begin
case Member is
when Member_Index_For_Derived_Type_Def_F_Has_Abstract =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Derived_Type_Def.F_Has_Abstract);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Derived_Type_Def_F_Has_Limited =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Derived_Type_Def.F_Has_Limited);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Derived_Type_Def_F_Has_Synchronized =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Derived_Type_Def.F_Has_Synchronized);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Derived_Type_Def_F_Subtype_Indication =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Derived_Type_Def.F_Subtype_Indication);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Derived_Type_Def_F_Interfaces =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Derived_Type_Def.F_Interfaces);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Derived_Type_Def_F_Record_Extension =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Derived_Type_Def.F_Record_Extension);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Derived_Type_Def_F_Has_With_Private =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Derived_Type_Def.F_Has_With_Private);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Enum_Type_Def_Range =>
declare
N_Bare_Enum_Type_Def : constant Analysis.Enum_Type_Def := N.As_Enum_Type_Def;
begin
case Member is
when Member_Index_For_Enum_Type_Def_F_Enum_Literals =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Enum_Type_Def.F_Enum_Literals);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Interface_Type_Def_Range =>
declare
N_Bare_Interface_Type_Def : constant Analysis.Interface_Type_Def := N.As_Interface_Type_Def;
begin
case Member is
when Member_Index_For_Interface_Type_Def_F_Interface_Kind =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Interface_Type_Def.F_Interface_Kind);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Interface_Type_Def_F_Interfaces =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Interface_Type_Def.F_Interfaces);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Mod_Int_Type_Def_Range =>
declare
N_Bare_Mod_Int_Type_Def : constant Analysis.Mod_Int_Type_Def := N.As_Mod_Int_Type_Def;
begin
case Member is
when Member_Index_For_Mod_Int_Type_Def_F_Expr =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Mod_Int_Type_Def.F_Expr);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Private_Type_Def_Range =>
declare
N_Bare_Private_Type_Def : constant Analysis.Private_Type_Def := N.As_Private_Type_Def;
begin
case Member is
when Member_Index_For_Private_Type_Def_F_Has_Abstract =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Private_Type_Def.F_Has_Abstract);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Private_Type_Def_F_Has_Tagged =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Private_Type_Def.F_Has_Tagged);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Private_Type_Def_F_Has_Limited =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Private_Type_Def.F_Has_Limited);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Decimal_Fixed_Point_Def_Range =>
declare
N_Bare_Decimal_Fixed_Point_Def : constant Analysis.Decimal_Fixed_Point_Def := N.As_Decimal_Fixed_Point_Def;
begin
case Member is
when Member_Index_For_Decimal_Fixed_Point_Def_F_Delta =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Decimal_Fixed_Point_Def.F_Delta);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Decimal_Fixed_Point_Def_F_Digits =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Decimal_Fixed_Point_Def.F_Digits);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Decimal_Fixed_Point_Def_F_Range =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Decimal_Fixed_Point_Def.F_Range);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Floating_Point_Def_Range =>
declare
N_Bare_Floating_Point_Def : constant Analysis.Floating_Point_Def := N.As_Floating_Point_Def;
begin
case Member is
when Member_Index_For_Floating_Point_Def_F_Num_Digits =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Floating_Point_Def.F_Num_Digits);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Floating_Point_Def_F_Range =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Floating_Point_Def.F_Range);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Ordinary_Fixed_Point_Def_Range =>
declare
N_Bare_Ordinary_Fixed_Point_Def : constant Analysis.Ordinary_Fixed_Point_Def := N.As_Ordinary_Fixed_Point_Def;
begin
case Member is
when Member_Index_For_Ordinary_Fixed_Point_Def_F_Delta =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Ordinary_Fixed_Point_Def.F_Delta);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Ordinary_Fixed_Point_Def_F_Range =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Ordinary_Fixed_Point_Def.F_Range);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Record_Type_Def_Range =>
declare
N_Bare_Record_Type_Def : constant Analysis.Record_Type_Def := N.As_Record_Type_Def;
begin
case Member is
when Member_Index_For_Record_Type_Def_F_Has_Abstract =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Record_Type_Def.F_Has_Abstract);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Record_Type_Def_F_Has_Tagged =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Record_Type_Def.F_Has_Tagged);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Record_Type_Def_F_Has_Limited =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Record_Type_Def.F_Has_Limited);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Record_Type_Def_F_Record_Def =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Record_Type_Def.F_Record_Def);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Signed_Int_Type_Def_Range =>
declare
N_Bare_Signed_Int_Type_Def : constant Analysis.Signed_Int_Type_Def := N.As_Signed_Int_Type_Def;
begin
case Member is
when Member_Index_For_Signed_Int_Type_Def_F_Range =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Signed_Int_Type_Def.F_Range);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Type_Expr =>
declare
N_Bare_Type_Expr : constant Analysis.Type_Expr := N.As_Type_Expr;
begin
case Member is
when Member_Index_For_Type_Expr_P_Type_Name =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Type_Expr.P_Type_Name);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Type_Expr_P_Designated_Type_Decl =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Type_Expr.P_Designated_Type_Decl);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Type_Expr_P_Designated_Type_Decl_From =>
declare
Arg_Origin_Node : Ada_Node := Get_Node (Internal_Acc_Node (Arguments (1)).all);
begin
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Type_Expr.P_Designated_Type_Decl_From (Arg_Origin_Node));
Result := Internal_Value_Access (R);
end;
end;
when others => null;
end case;
case Ada_Type_Expr (Kind) is
when Ada_Anonymous_Type_Range =>
declare
N_Bare_Anonymous_Type : constant Analysis.Anonymous_Type := N_Bare_Type_Expr.As_Anonymous_Type;
begin
case Member is
when Member_Index_For_Anonymous_Type_F_Type_Decl =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Anonymous_Type.F_Type_Decl);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Subtype_Indication_Range =>
declare
N_Bare_Subtype_Indication : constant Analysis.Subtype_Indication := N_Bare_Type_Expr.As_Subtype_Indication;
begin
case Member is
when Member_Index_For_Subtype_Indication_F_Has_Not_Null =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Subtype_Indication.F_Has_Not_Null);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Subtype_Indication_F_Name =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Subtype_Indication.F_Name);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Subtype_Indication_F_Constraint =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Subtype_Indication.F_Constraint);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Subtype_Indication_P_Subtype_Constraints =>
declare
R : Internal_Acc_Param_Actual_Array :=  new Internal_Rec_Param_Actual_Array;
begin
R.Value := new Param_Actual_Array'(N_Bare_Subtype_Indication.P_Subtype_Constraints);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Subtype_Indication_P_Is_Static_Subtype =>
declare
Arg_Imprecise_Fallback : Boolean renames Internal_Acc_Bool (Arguments (1)).Value;
begin
declare
R : Internal_Acc_Bool :=  new Internal_Rec_Bool;
begin
R.Value := N_Bare_Subtype_Indication.P_Is_Static_Subtype (Arg_Imprecise_Fallback);
Result := Internal_Value_Access (R);
end;
end;
when others => null;
end case;
end;
when Ada_Synthetic_Type_Expr_Range =>
declare
N_Bare_Synthetic_Type_Expr : constant Analysis.Synthetic_Type_Expr := N_Bare_Type_Expr.As_Synthetic_Type_Expr;
begin
case Member is
when Member_Index_For_Synthetic_Type_Expr_F_Target_Type =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Synthetic_Type_Expr.F_Target_Type);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when others => null;
end case;
end;
when Ada_Unconstrained_Array_Index_Range =>
declare
N_Bare_Unconstrained_Array_Index : constant Analysis.Unconstrained_Array_Index := N.As_Unconstrained_Array_Index;
begin
case Member is
when Member_Index_For_Unconstrained_Array_Index_F_Subtype_Indication =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Unconstrained_Array_Index.F_Subtype_Indication);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Until_Node =>
declare
N_Bare_Until_Node : constant Analysis.Until_Node := N.As_Until_Node;
begin
case Member is
when Member_Index_For_Until_Node_P_As_Bool =>
declare
R : Internal_Acc_Bool :=  new Internal_Rec_Bool;
begin
R.Value := N_Bare_Until_Node.P_As_Bool;
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Use_Package_Clause_Range =>
declare
N_Bare_Use_Package_Clause : constant Analysis.Use_Package_Clause := N.As_Use_Package_Clause;
begin
case Member is
when Member_Index_For_Use_Package_Clause_F_Packages =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Use_Package_Clause.F_Packages);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Use_Type_Clause_Range =>
declare
N_Bare_Use_Type_Clause : constant Analysis.Use_Type_Clause := N.As_Use_Type_Clause;
begin
case Member is
when Member_Index_For_Use_Type_Clause_F_Has_All =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Use_Type_Clause.F_Has_All);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Use_Type_Clause_F_Types =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Use_Type_Clause.F_Types);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Value_Sequence_Range =>
declare
N_Bare_Value_Sequence : constant Analysis.Value_Sequence := N.As_Value_Sequence;
begin
case Member is
when Member_Index_For_Value_Sequence_F_Iter_Assoc =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Value_Sequence.F_Iter_Assoc);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Variant_Range =>
declare
N_Bare_Variant : constant Analysis.Variant := N.As_Variant;
begin
case Member is
when Member_Index_For_Variant_F_Choices =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Variant.F_Choices);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Variant_F_Components =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Variant.F_Components);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_Variant_Part_Range =>
declare
N_Bare_Variant_Part : constant Analysis.Variant_Part := N.As_Variant_Part;
begin
case Member is
when Member_Index_For_Variant_Part_F_Discr_Name =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Variant_Part.F_Discr_Name);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_Variant_Part_F_Variant =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_Variant_Part.F_Variant);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_With_Clause_Range =>
declare
N_Bare_With_Clause : constant Analysis.With_Clause := N.As_With_Clause;
begin
case Member is
when Member_Index_For_With_Clause_F_Has_Limited =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_With_Clause.F_Has_Limited);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_With_Clause_F_Has_Private =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_With_Clause.F_Has_Private);
Result := Internal_Value_Access (R);
end;
when Member_Index_For_With_Clause_F_Packages =>
declare
R : Internal_Acc_Node :=  new Internal_Rec_Node;
begin
Set_Node (R, N_Bare_With_Clause.F_Packages);
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when Ada_With_Private =>
declare
N_Bare_With_Private : constant Analysis.With_Private := N.As_With_Private;
begin
case Member is
when Member_Index_For_With_Private_P_As_Bool =>
declare
R : Internal_Acc_Bool :=  new Internal_Rec_Bool;
begin
R.Value := N_Bare_With_Private.P_As_Bool;
Result := Internal_Value_Access (R);
end;
when others => null;
end case;
end;
when others => null;
end case;
      pragma Assert (Result /= null);
      return Result;
   end Eval_Node_Member;

   --------------
   -- Set_Unit --
   --------------

   procedure Set_Unit
     (Intr_Value   : Internal_Acc_Analysis_Unit;
      Actual_Value : Analysis_Unit)
   is
      U : constant Internal_Unit :=
        +Public_Converters.Unwrap_Unit (Actual_Value);
   begin
      Intr_Value.Value :=
        Langkit_Support.Internal.Conversions.Wrap_Unit (Self_Id, U);
   end Set_Unit;

   --------------
   -- Get_Unit --
   --------------

   function Get_Unit
     (Intr_Value : Internal_Rec_Analysis_Unit)
      return Analysis_Unit
   is
      U : constant Implementation.Internal_Unit :=
        +Langkit_Support.Internal.Conversions.Unwrap_Unit (Intr_Value.Value);
   begin
      return Public_Converters.Wrap_Unit (U);
   end Get_Unit;

   -----------------
   -- Set_Big_Int --
   -----------------

   procedure Set_Big_Int
     (Intr_Value   : Internal_Acc_Big_Int;
      Actual_Value : Big_Integer) is
   begin
      Intr_Value.Value.Set (Actual_Value);
   end Set_Big_Int;

   -----------------
   -- Get_Big_Int --
   -----------------

   procedure Get_Big_Int
     (Intr_Value   : Internal_Rec_Big_Int;
      Actual_Value : out Big_Integer)
   is
   begin
      Actual_Value.Set (Intr_Value.Value);
   end Get_Big_Int;

   --------------
   -- Set_Node --
   --------------

   procedure Set_Node
     (Intr_Value   : Internal_Acc_Node;
      Actual_Value : Ada_Node'Class)
   is
      E : constant Internal_Entity := +Unwrap_Entity (Actual_Value);
   begin
      Intr_Value.Value :=
        Langkit_Support.Internal.Conversions.Wrap_Node (Self_Id, E);
   end Set_Node;

   --------------
   -- Get_Node --
   --------------

   function Get_Node
     (Intr_Value : Internal_Rec_Node)
      return Ada_Node
   is
      E : constant Implementation.Internal_Entity :=
        +Langkit_Support.Internal.Conversions.Unwrap_Node (Intr_Value.Value);
   begin
      return Public_Converters.Wrap_Node (E.Node, E.Info);
   end Get_Node;

end Libadalang.Generic_Introspection;
