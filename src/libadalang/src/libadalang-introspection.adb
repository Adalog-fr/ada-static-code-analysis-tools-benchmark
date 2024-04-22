--
--  Copyright (C) 2014-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with Libadalang.Implementation;    use Libadalang.Implementation;
with Libadalang.Introspection_Implementation;
with Libadalang.Public_Converters; use Libadalang.Public_Converters;

package body Libadalang.Introspection is

   package Impl renames Introspection_Implementation;

   --  TODO: move implementation of functions dealing with values (Satisfies,
   --  Eval_Property, ...) to Impl. This is not not done yet as substantial
   --  work is required in order to convert back and forth public values
   --  (structures, symbols) to their internal representations.

   function Allocate (Kind : Value_Kind) return Value_Type;
   --  Allocate a polymorphic value of the given kind

   pragma Warnings (Off, "is not referenced");
   function To_Internal_Value
     (Value : Any_Value_Type) return Impl.Internal_Value;
   function From_Internal_Value
     (Value : Impl.Internal_Value) return Any_Value_Type;
   pragma Warnings (On, "is not referenced");

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (Self : in out Value_Access_Wrapper) is
   begin
      if Self.Value = null then
         return;
      end if;

      declare
         Rec : Value_Record renames Self.Value.all;
      begin
         Rec.Ref_Count := Rec.Ref_Count + 1;
      end;
   end Adjust;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Value_Access_Wrapper) is
   begin
      if Self.Value = null then
         return;
      end if;

      --  If Self is non-null, decrement the reference count of the referenced
      --  value.

      declare
         Rec : Value_Record renames Self.Value.all;
      begin
         Rec.Ref_Count := Rec.Ref_Count - 1;

         if Rec.Ref_Count = 0 then
            --  Reference count dropped to 0: time to free the value and what
            --  is inside.

            case Rec.Kind is
                     when Discriminant_Values_Array_Value =>
                        Free (Rec.Discriminant_Values_Array_Value);
                     when Doc_Annotation_Array_Value =>
                        Free (Rec.Doc_Annotation_Array_Value);
                     when Accept_Stmt_Array_Value =>
                        Free (Rec.Accept_Stmt_Array_Value);
                     when Ada_Node_Array_Value =>
                        Free (Rec.Ada_Node_Array_Value);
                     when Base_Formal_Param_Decl_Array_Value =>
                        Free (Rec.Base_Formal_Param_Decl_Array_Value);
                     when Base_Type_Decl_Array_Value =>
                        Free (Rec.Base_Type_Decl_Array_Value);
                     when Basic_Decl_Array_Value =>
                        Free (Rec.Basic_Decl_Array_Value);
                     when Compilation_Unit_Array_Value =>
                        Free (Rec.Compilation_Unit_Array_Value);
                     when Defining_Name_Array_Value =>
                        Free (Rec.Defining_Name_Array_Value);
                     when Expr_Array_Value =>
                        Free (Rec.Expr_Array_Value);
                     when Generic_Instantiation_Array_Value =>
                        Free (Rec.Generic_Instantiation_Array_Value);
                     when Param_Spec_Array_Value =>
                        Free (Rec.Param_Spec_Array_Value);
                     when Pragma_Node_Array_Value =>
                        Free (Rec.Pragma_Node_Array_Value);
                     when Type_Decl_Array_Value =>
                        Free (Rec.Type_Decl_Array_Value);
                     when Param_Actual_Array_Value =>
                        Free (Rec.Param_Actual_Array_Value);
                     when Ref_Result_Array_Value =>
                        Free (Rec.Ref_Result_Array_Value);
                     when Shape_Array_Value =>
                        Free (Rec.Shape_Array_Value);
                     when Substitution_Array_Value =>
                        Free (Rec.Substitution_Array_Value);
                     when Analysis_Unit_Array_Value =>
                        Free (Rec.Analysis_Unit_Array_Value);
                     when Unbounded_Text_Type_Array_Value =>
                        Free (Rec.Unbounded_Text_Type_Array_Value);
               when others => null;
            end case;

            Free (Self.Value);
         end if;
      end;
   end Finalize;

   ----------
   -- Kind --
   ----------

   function Kind (Self : Value_Type) return Value_Kind is
   begin
      return Self.Value.Value.Kind;
   end Kind;

   --------------
   -- Allocate --
   --------------

   function Allocate (Kind : Value_Kind) return Value_Type is
      Result : Any_Value_Type;
   begin
      Result.Value.Value := new Value_Record (Kind);
      Result.Value.Value.Ref_Count := 1;
      return Result;
   end Allocate;

   -----------------------
   -- To_Internal_Value --
   -----------------------

   function To_Internal_Value
     (Value : Any_Value_Type) return Impl.Internal_Value is
   begin
      if Value = No_Value then
         return Impl.No_Internal_Value;
      end if;

      case Kind (Value) is
         when Boolean_Value =>
            return Impl.Create_Boolean (As_Boolean (Value));

         when Integer_Value =>
            return Impl.Create_Integer (As_Integer (Value));

         when Character_Value =>
            return Impl.Create_Character (As_Character (Value));

         when String_Value =>
            return Impl.Create_String (Create_String (As_String (Value)));

         when Node_Value =>
            return Impl.Create_Node (Unwrap_Entity (As_Node (Value)));

         when others =>
            --  For now we use this only to handle default values, so this
            --  should be unreachable.
            raise Program_Error;
      end case;
   end To_Internal_Value;

   -------------------------
   -- From_Internal_Value --
   -------------------------

   function From_Internal_Value
     (Value : Impl.Internal_Value) return Any_Value_Type is
   begin
      case Value.Kind is
         when None =>
            return No_Value;

         when Boolean_Value =>
            return Create_Boolean (Impl.As_Boolean (Value));

         when Integer_Value =>
            return Create_Integer (Impl.As_Integer (Value));

         when Character_Value =>
            return Create_Character (Impl.As_Character (Value));

         when String_Value =>
            return Create_String (Value.String_Value.Content);

            when Analysis_Unit_Kind_Value =>
               return Create_Analysis_Unit_Kind
                 (Impl.As_Analysis_Unit_Kind (Value));
            when Lookup_Kind_Value =>
               return Create_Lookup_Kind
                 (Impl.As_Lookup_Kind (Value));
            when Designated_Env_Kind_Value =>
               return Create_Designated_Env_Kind
                 (Impl.As_Designated_Env_Kind (Value));
            when Ref_Result_Kind_Value =>
               return Create_Ref_Result_Kind
                 (Impl.As_Ref_Result_Kind (Value));
            when Call_Expr_Kind_Value =>
               return Create_Call_Expr_Kind
                 (Impl.As_Call_Expr_Kind (Value));
            when Grammar_Rule_Value =>
               return Create_Grammar_Rule
                 (Impl.As_Grammar_Rule (Value));

         when Node_Value =>
            declare
               N : constant Internal_Entity := Impl.As_Node (Value);
            begin
               return Create_Node (Wrap_Node (N.Node, N.Info));
            end;
      end case;
   end From_Internal_Value;

   ----------------
   -- As_Boolean --
   ----------------

   function As_Boolean (Self : Value_Type) return Boolean is
   begin
      return Self.Value.Value.Boolean_Value;
   end As_Boolean;

   --------------------
   -- Create_Boolean --
   --------------------

   function Create_Boolean (Value : Boolean) return Value_Type is
   begin
      return Result : constant Value_Type := Allocate (Boolean_Value) do
         Result.Value.Value.Boolean_Value := Value;
      end return;
   end Create_Boolean;

   ----------------
   -- As_Integer --
   ----------------

   function As_Integer (Self : Value_Type) return Integer is
   begin
      return Self.Value.Value.Integer_Value;
   end As_Integer;

   --------------------
   -- Create_Integer --
   --------------------

   function Create_Integer (Value : Integer) return Value_Type is
   begin
      return Result : constant Value_Type := Allocate (Integer_Value) do
         Result.Value.Value.Integer_Value := Value;
      end return;
   end Create_Integer;

   --------------------
   -- As_Big_Integer --
   --------------------

   function As_Big_Integer (Self : Value_Type) return Big_Integer is
   begin
      return Result : Big_Integer do
         Result.Set (Self.Value.Value.Big_Integer_Value);
      end return;
   end As_Big_Integer;

   ------------------------
   -- Create_Big_Integer --
   ------------------------

   function Create_Big_Integer (Value : Big_Integer) return Value_Type is
   begin
      return Result : constant Value_Type := Allocate (Big_Integer_Value) do
         Result.Value.Value.Big_Integer_Value.Set (Value);
      end return;
   end Create_Big_Integer;

   ------------------
   -- As_Character --
   ------------------

   function As_Character (Self : Value_Type) return Character_Type is
   begin
      return Self.Value.Value.Character_Value;
   end As_Character;

   ----------------------
   -- Create_Character --
   ----------------------

   function Create_Character (Value : Character_Type) return Value_Type is
   begin
      return Result : constant Value_Type := Allocate (Character_Value) do
         Result.Value.Value.Character_Value := Value;
      end return;
   end Create_Character;

   ---------------
   -- As_String --
   ---------------

   function As_String (Self : Value_Type) return Text_Type is
   begin
      return To_Text (Self.Value.Value.String_Value);
   end As_String;

   -------------------
   -- Create_String --
   -------------------

   function Create_String (Value : Text_Type) return Value_Type is
   begin
      return Result : constant Value_Type := Allocate (String_Value) do
         Result.Value.Value.String_Value := To_Unbounded_Text (Value);
      end return;
   end Create_String;

   --------------
   -- As_Token --
   --------------

   function As_Token (Self : Value_Type) return Token_Reference is
   begin
      return Self.Value.Value.Token_Value;
   end As_Token;

   ------------------
   -- Create_Token --
   ------------------

   function Create_Token (Value : Token_Reference) return Value_Type is
   begin
      return Result : constant Value_Type := Allocate (Token_Value)
      do
         Result.Value.Value.Token_Value := Value;
      end return;
   end Create_Token;

   -----------------------
   -- As_Unbounded_Text --
   -----------------------

   function As_Unbounded_Text (Self : Value_Type) return Unbounded_Text_Type is
   begin
      return Self.Value.Value.Unbounded_Text_Value;
   end As_Unbounded_Text;

   ---------------------------
   -- Create_Unbounded_Text --
   ---------------------------

   function Create_Unbounded_Text
     (Value : Unbounded_Text_Type) return Value_Type is
   begin
      return Result : constant Value_Type := Allocate (Unbounded_Text_Value)
      do
         Result.Value.Value.Unbounded_Text_Value := Value;
      end return;
   end Create_Unbounded_Text;

   ----------------------
   -- As_Analysis_Unit --
   ----------------------

   function As_Analysis_Unit (Self : Value_Type) return Analysis_Unit is
   begin
      return Self.Value.Value.Analysis_Unit_Value;
   end As_Analysis_Unit;

   --------------------------
   -- Create_Analysis_Unit --
   --------------------------

   function Create_Analysis_Unit (Value : Analysis_Unit) return Value_Type is
   begin
      return Result : constant Value_Type := Allocate (Analysis_Unit_Value) do
         Result.Value.Value.Analysis_Unit_Value := Value;
      end return;
   end Create_Analysis_Unit;

   -------------
   -- As_Node --
   -------------

   function As_Node (Self : Value_Type) return Ada_Node is
   begin
      return Self.Value.Value.Node_Value;
   end As_Node;

   -----------------
   -- Create_Node --
   -----------------

   function Create_Node
     (Value : Ada_Node'Class) return Value_Type is
   begin
      return Result : constant Value_Type := Allocate (Node_Value) do
         Result.Value.Value.Node_Value := Value.As_Ada_Node;
      end return;
   end Create_Node;

      function As_Analysis_Unit_Kind
        (Self : Value_Type) return Analysis_Unit_Kind is
      begin
         return Self.Value.Value.Analysis_Unit_Kind_Value;
      end As_Analysis_Unit_Kind;

      function Create_Analysis_Unit_Kind
        (Value : Analysis_Unit_Kind) return Value_Type is
      begin
         return Result : constant Value_Type := Allocate
           (Analysis_Unit_Kind_Value)
         do
            Result.Value.Value.Analysis_Unit_Kind_Value := Value;
         end return;
      end Create_Analysis_Unit_Kind;
      function As_Lookup_Kind
        (Self : Value_Type) return Lookup_Kind is
      begin
         return Self.Value.Value.Lookup_Kind_Value;
      end As_Lookup_Kind;

      function Create_Lookup_Kind
        (Value : Lookup_Kind) return Value_Type is
      begin
         return Result : constant Value_Type := Allocate
           (Lookup_Kind_Value)
         do
            Result.Value.Value.Lookup_Kind_Value := Value;
         end return;
      end Create_Lookup_Kind;
      function As_Designated_Env_Kind
        (Self : Value_Type) return Designated_Env_Kind is
      begin
         return Self.Value.Value.Designated_Env_Kind_Value;
      end As_Designated_Env_Kind;

      function Create_Designated_Env_Kind
        (Value : Designated_Env_Kind) return Value_Type is
      begin
         return Result : constant Value_Type := Allocate
           (Designated_Env_Kind_Value)
         do
            Result.Value.Value.Designated_Env_Kind_Value := Value;
         end return;
      end Create_Designated_Env_Kind;
      function As_Ref_Result_Kind
        (Self : Value_Type) return Ref_Result_Kind is
      begin
         return Self.Value.Value.Ref_Result_Kind_Value;
      end As_Ref_Result_Kind;

      function Create_Ref_Result_Kind
        (Value : Ref_Result_Kind) return Value_Type is
      begin
         return Result : constant Value_Type := Allocate
           (Ref_Result_Kind_Value)
         do
            Result.Value.Value.Ref_Result_Kind_Value := Value;
         end return;
      end Create_Ref_Result_Kind;
      function As_Call_Expr_Kind
        (Self : Value_Type) return Call_Expr_Kind is
      begin
         return Self.Value.Value.Call_Expr_Kind_Value;
      end As_Call_Expr_Kind;

      function Create_Call_Expr_Kind
        (Value : Call_Expr_Kind) return Value_Type is
      begin
         return Result : constant Value_Type := Allocate
           (Call_Expr_Kind_Value)
         do
            Result.Value.Value.Call_Expr_Kind_Value := Value;
         end return;
      end Create_Call_Expr_Kind;
      function As_Grammar_Rule
        (Self : Value_Type) return Grammar_Rule is
      begin
         return Self.Value.Value.Grammar_Rule_Value;
      end As_Grammar_Rule;

      function Create_Grammar_Rule
        (Value : Grammar_Rule) return Value_Type is
      begin
         return Result : constant Value_Type := Allocate
           (Grammar_Rule_Value)
         do
            Result.Value.Value.Grammar_Rule_Value := Value;
         end return;
      end Create_Grammar_Rule;

         function As_Aspect
           (Self : Value_Type) return Aspect is
         begin
               return Self.Value.Value.Aspect_Value;
         end As_Aspect;

         function Create_Aspect
           (Value : Aspect) return Value_Type is
         begin
            return Result : constant Value_Type := Allocate
              (Aspect_Value)
            do
                  Result.Value.Value.Aspect_Value := Value;
            end return;
         end Create_Aspect;
         function As_Completion_Item
           (Self : Value_Type) return Completion_Item is
         begin
               return Self.Value.Value.Completion_Item_Value;
         end As_Completion_Item;

         function Create_Completion_Item
           (Value : Completion_Item) return Value_Type is
         begin
            return Result : constant Value_Type := Allocate
              (Completion_Item_Value)
            do
                  Result.Value.Value.Completion_Item_Value := Value;
            end return;
         end Create_Completion_Item;
         function As_Completion_Item_Iterator
           (Self : Value_Type) return Completion_Item_Iterator is
         begin
               return Self.Value.Value.Completion_Item_Iterator_Value;
         end As_Completion_Item_Iterator;

         function Create_Completion_Item_Iterator
           (Value : Completion_Item_Iterator) return Value_Type is
         begin
            return Result : constant Value_Type := Allocate
              (Completion_Item_Iterator_Value)
            do
                  Result.Value.Value.Completion_Item_Iterator_Value := Value;
            end return;
         end Create_Completion_Item_Iterator;
         function As_Discrete_Range
           (Self : Value_Type) return Discrete_Range is
         begin
               return Self.Value.Value.Discrete_Range_Value;
         end As_Discrete_Range;

         function Create_Discrete_Range
           (Value : Discrete_Range) return Value_Type is
         begin
            return Result : constant Value_Type := Allocate
              (Discrete_Range_Value)
            do
                  Result.Value.Value.Discrete_Range_Value := Value;
            end return;
         end Create_Discrete_Range;
         function As_Discriminant_Values
           (Self : Value_Type) return Discriminant_Values is
         begin
               return Self.Value.Value.Discriminant_Values_Value;
         end As_Discriminant_Values;

         function Create_Discriminant_Values
           (Value : Discriminant_Values) return Value_Type is
         begin
            return Result : constant Value_Type := Allocate
              (Discriminant_Values_Value)
            do
                  Result.Value.Value.Discriminant_Values_Value := Value;
            end return;
         end Create_Discriminant_Values;
         function As_Discriminant_Values_Array
           (Self : Value_Type) return Discriminant_Values_Array is
         begin
               return Result : Discriminant_Values_Array
                 (Self.Value.Value.Discriminant_Values_Array_Value'Range)
               do
                  for I in Result'Range loop
                        Result (I) :=
                           Self.Value.Value.Discriminant_Values_Array_Value.all (I);
                  end loop;
               end return;

         end As_Discriminant_Values_Array;

         function Create_Discriminant_Values_Array
           (Value : Discriminant_Values_Array) return Value_Type is
         begin
            return Result : constant Value_Type := Allocate
              (Discriminant_Values_Array_Value)
            do
                  Result.Value.Value.Discriminant_Values_Array_Value :=
                     new Discriminant_Values_Array (Value'Range);
                  for I in Value'Range loop
                        Result.Value.Value.Discriminant_Values_Array_Value.all (I) :=
                           Value (I);
                  end loop;

            end return;
         end Create_Discriminant_Values_Array;
         function As_Doc_Annotation
           (Self : Value_Type) return Doc_Annotation is
         begin
               return Self.Value.Value.Doc_Annotation_Value;
         end As_Doc_Annotation;

         function Create_Doc_Annotation
           (Value : Doc_Annotation) return Value_Type is
         begin
            return Result : constant Value_Type := Allocate
              (Doc_Annotation_Value)
            do
                  Result.Value.Value.Doc_Annotation_Value := Value;
            end return;
         end Create_Doc_Annotation;
         function As_Doc_Annotation_Array
           (Self : Value_Type) return Doc_Annotation_Array is
         begin
               return Result : Doc_Annotation_Array
                 (Self.Value.Value.Doc_Annotation_Array_Value'Range)
               do
                  for I in Result'Range loop
                        Result (I) :=
                           Self.Value.Value.Doc_Annotation_Array_Value.all (I);
                  end loop;
               end return;

         end As_Doc_Annotation_Array;

         function Create_Doc_Annotation_Array
           (Value : Doc_Annotation_Array) return Value_Type is
         begin
            return Result : constant Value_Type := Allocate
              (Doc_Annotation_Array_Value)
            do
                  Result.Value.Value.Doc_Annotation_Array_Value :=
                     new Doc_Annotation_Array (Value'Range);
                  for I in Value'Range loop
                        Result.Value.Value.Doc_Annotation_Array_Value.all (I) :=
                           Value (I);
                  end loop;

            end return;
         end Create_Doc_Annotation_Array;
         function As_Accept_Stmt_Array
           (Self : Value_Type) return Accept_Stmt_Array is
         begin
               return Result : Accept_Stmt_Array
                 (Self.Value.Value.Accept_Stmt_Array_Value'Range)
               do
                  for I in Result'Range loop
                        Result (I) :=
                           Self.Value.Value.Accept_Stmt_Array_Value.all (I);
                  end loop;
               end return;

         end As_Accept_Stmt_Array;

         function Create_Accept_Stmt_Array
           (Value : Accept_Stmt_Array) return Value_Type is
         begin
            return Result : constant Value_Type := Allocate
              (Accept_Stmt_Array_Value)
            do
                  Result.Value.Value.Accept_Stmt_Array_Value :=
                     new Accept_Stmt_Array (Value'Range);
                  for I in Value'Range loop
                        Result.Value.Value.Accept_Stmt_Array_Value.all (I) :=
                           Value (I);
                  end loop;

            end return;
         end Create_Accept_Stmt_Array;
         function As_Ada_Node_Array
           (Self : Value_Type) return Ada_Node_Array is
         begin
               return Result : Ada_Node_Array
                 (Self.Value.Value.Ada_Node_Array_Value'Range)
               do
                  for I in Result'Range loop
                        Result (I) :=
                           Self.Value.Value.Ada_Node_Array_Value.all (I);
                  end loop;
               end return;

         end As_Ada_Node_Array;

         function Create_Ada_Node_Array
           (Value : Ada_Node_Array) return Value_Type is
         begin
            return Result : constant Value_Type := Allocate
              (Ada_Node_Array_Value)
            do
                  Result.Value.Value.Ada_Node_Array_Value :=
                     new Ada_Node_Array (Value'Range);
                  for I in Value'Range loop
                        Result.Value.Value.Ada_Node_Array_Value.all (I) :=
                           Value (I);
                  end loop;

            end return;
         end Create_Ada_Node_Array;
         function As_Base_Formal_Param_Decl_Array
           (Self : Value_Type) return Base_Formal_Param_Decl_Array is
         begin
               return Result : Base_Formal_Param_Decl_Array
                 (Self.Value.Value.Base_Formal_Param_Decl_Array_Value'Range)
               do
                  for I in Result'Range loop
                        Result (I) :=
                           Self.Value.Value.Base_Formal_Param_Decl_Array_Value.all (I);
                  end loop;
               end return;

         end As_Base_Formal_Param_Decl_Array;

         function Create_Base_Formal_Param_Decl_Array
           (Value : Base_Formal_Param_Decl_Array) return Value_Type is
         begin
            return Result : constant Value_Type := Allocate
              (Base_Formal_Param_Decl_Array_Value)
            do
                  Result.Value.Value.Base_Formal_Param_Decl_Array_Value :=
                     new Base_Formal_Param_Decl_Array (Value'Range);
                  for I in Value'Range loop
                        Result.Value.Value.Base_Formal_Param_Decl_Array_Value.all (I) :=
                           Value (I);
                  end loop;

            end return;
         end Create_Base_Formal_Param_Decl_Array;
         function As_Base_Type_Decl_Array
           (Self : Value_Type) return Base_Type_Decl_Array is
         begin
               return Result : Base_Type_Decl_Array
                 (Self.Value.Value.Base_Type_Decl_Array_Value'Range)
               do
                  for I in Result'Range loop
                        Result (I) :=
                           Self.Value.Value.Base_Type_Decl_Array_Value.all (I);
                  end loop;
               end return;

         end As_Base_Type_Decl_Array;

         function Create_Base_Type_Decl_Array
           (Value : Base_Type_Decl_Array) return Value_Type is
         begin
            return Result : constant Value_Type := Allocate
              (Base_Type_Decl_Array_Value)
            do
                  Result.Value.Value.Base_Type_Decl_Array_Value :=
                     new Base_Type_Decl_Array (Value'Range);
                  for I in Value'Range loop
                        Result.Value.Value.Base_Type_Decl_Array_Value.all (I) :=
                           Value (I);
                  end loop;

            end return;
         end Create_Base_Type_Decl_Array;
         function As_Basic_Decl_Array
           (Self : Value_Type) return Basic_Decl_Array is
         begin
               return Result : Basic_Decl_Array
                 (Self.Value.Value.Basic_Decl_Array_Value'Range)
               do
                  for I in Result'Range loop
                        Result (I) :=
                           Self.Value.Value.Basic_Decl_Array_Value.all (I);
                  end loop;
               end return;

         end As_Basic_Decl_Array;

         function Create_Basic_Decl_Array
           (Value : Basic_Decl_Array) return Value_Type is
         begin
            return Result : constant Value_Type := Allocate
              (Basic_Decl_Array_Value)
            do
                  Result.Value.Value.Basic_Decl_Array_Value :=
                     new Basic_Decl_Array (Value'Range);
                  for I in Value'Range loop
                        Result.Value.Value.Basic_Decl_Array_Value.all (I) :=
                           Value (I);
                  end loop;

            end return;
         end Create_Basic_Decl_Array;
         function As_Compilation_Unit_Array
           (Self : Value_Type) return Compilation_Unit_Array is
         begin
               return Result : Compilation_Unit_Array
                 (Self.Value.Value.Compilation_Unit_Array_Value'Range)
               do
                  for I in Result'Range loop
                        Result (I) :=
                           Self.Value.Value.Compilation_Unit_Array_Value.all (I);
                  end loop;
               end return;

         end As_Compilation_Unit_Array;

         function Create_Compilation_Unit_Array
           (Value : Compilation_Unit_Array) return Value_Type is
         begin
            return Result : constant Value_Type := Allocate
              (Compilation_Unit_Array_Value)
            do
                  Result.Value.Value.Compilation_Unit_Array_Value :=
                     new Compilation_Unit_Array (Value'Range);
                  for I in Value'Range loop
                        Result.Value.Value.Compilation_Unit_Array_Value.all (I) :=
                           Value (I);
                  end loop;

            end return;
         end Create_Compilation_Unit_Array;
         function As_Defining_Name_Array
           (Self : Value_Type) return Defining_Name_Array is
         begin
               return Result : Defining_Name_Array
                 (Self.Value.Value.Defining_Name_Array_Value'Range)
               do
                  for I in Result'Range loop
                        Result (I) :=
                           Self.Value.Value.Defining_Name_Array_Value.all (I);
                  end loop;
               end return;

         end As_Defining_Name_Array;

         function Create_Defining_Name_Array
           (Value : Defining_Name_Array) return Value_Type is
         begin
            return Result : constant Value_Type := Allocate
              (Defining_Name_Array_Value)
            do
                  Result.Value.Value.Defining_Name_Array_Value :=
                     new Defining_Name_Array (Value'Range);
                  for I in Value'Range loop
                        Result.Value.Value.Defining_Name_Array_Value.all (I) :=
                           Value (I);
                  end loop;

            end return;
         end Create_Defining_Name_Array;
         function As_Expr_Array
           (Self : Value_Type) return Expr_Array is
         begin
               return Result : Expr_Array
                 (Self.Value.Value.Expr_Array_Value'Range)
               do
                  for I in Result'Range loop
                        Result (I) :=
                           Self.Value.Value.Expr_Array_Value.all (I);
                  end loop;
               end return;

         end As_Expr_Array;

         function Create_Expr_Array
           (Value : Expr_Array) return Value_Type is
         begin
            return Result : constant Value_Type := Allocate
              (Expr_Array_Value)
            do
                  Result.Value.Value.Expr_Array_Value :=
                     new Expr_Array (Value'Range);
                  for I in Value'Range loop
                        Result.Value.Value.Expr_Array_Value.all (I) :=
                           Value (I);
                  end loop;

            end return;
         end Create_Expr_Array;
         function As_Generic_Instantiation_Array
           (Self : Value_Type) return Generic_Instantiation_Array is
         begin
               return Result : Generic_Instantiation_Array
                 (Self.Value.Value.Generic_Instantiation_Array_Value'Range)
               do
                  for I in Result'Range loop
                        Result (I) :=
                           Self.Value.Value.Generic_Instantiation_Array_Value.all (I);
                  end loop;
               end return;

         end As_Generic_Instantiation_Array;

         function Create_Generic_Instantiation_Array
           (Value : Generic_Instantiation_Array) return Value_Type is
         begin
            return Result : constant Value_Type := Allocate
              (Generic_Instantiation_Array_Value)
            do
                  Result.Value.Value.Generic_Instantiation_Array_Value :=
                     new Generic_Instantiation_Array (Value'Range);
                  for I in Value'Range loop
                        Result.Value.Value.Generic_Instantiation_Array_Value.all (I) :=
                           Value (I);
                  end loop;

            end return;
         end Create_Generic_Instantiation_Array;
         function As_Param_Spec_Array
           (Self : Value_Type) return Param_Spec_Array is
         begin
               return Result : Param_Spec_Array
                 (Self.Value.Value.Param_Spec_Array_Value'Range)
               do
                  for I in Result'Range loop
                        Result (I) :=
                           Self.Value.Value.Param_Spec_Array_Value.all (I);
                  end loop;
               end return;

         end As_Param_Spec_Array;

         function Create_Param_Spec_Array
           (Value : Param_Spec_Array) return Value_Type is
         begin
            return Result : constant Value_Type := Allocate
              (Param_Spec_Array_Value)
            do
                  Result.Value.Value.Param_Spec_Array_Value :=
                     new Param_Spec_Array (Value'Range);
                  for I in Value'Range loop
                        Result.Value.Value.Param_Spec_Array_Value.all (I) :=
                           Value (I);
                  end loop;

            end return;
         end Create_Param_Spec_Array;
         function As_Pragma_Node_Array
           (Self : Value_Type) return Pragma_Node_Array is
         begin
               return Result : Pragma_Node_Array
                 (Self.Value.Value.Pragma_Node_Array_Value'Range)
               do
                  for I in Result'Range loop
                        Result (I) :=
                           Self.Value.Value.Pragma_Node_Array_Value.all (I);
                  end loop;
               end return;

         end As_Pragma_Node_Array;

         function Create_Pragma_Node_Array
           (Value : Pragma_Node_Array) return Value_Type is
         begin
            return Result : constant Value_Type := Allocate
              (Pragma_Node_Array_Value)
            do
                  Result.Value.Value.Pragma_Node_Array_Value :=
                     new Pragma_Node_Array (Value'Range);
                  for I in Value'Range loop
                        Result.Value.Value.Pragma_Node_Array_Value.all (I) :=
                           Value (I);
                  end loop;

            end return;
         end Create_Pragma_Node_Array;
         function As_Type_Decl_Array
           (Self : Value_Type) return Type_Decl_Array is
         begin
               return Result : Type_Decl_Array
                 (Self.Value.Value.Type_Decl_Array_Value'Range)
               do
                  for I in Result'Range loop
                        Result (I) :=
                           Self.Value.Value.Type_Decl_Array_Value.all (I);
                  end loop;
               end return;

         end As_Type_Decl_Array;

         function Create_Type_Decl_Array
           (Value : Type_Decl_Array) return Value_Type is
         begin
            return Result : constant Value_Type := Allocate
              (Type_Decl_Array_Value)
            do
                  Result.Value.Value.Type_Decl_Array_Value :=
                     new Type_Decl_Array (Value'Range);
                  for I in Value'Range loop
                        Result.Value.Value.Type_Decl_Array_Value.all (I) :=
                           Value (I);
                  end loop;

            end return;
         end Create_Type_Decl_Array;
         function As_Param_Actual
           (Self : Value_Type) return Param_Actual is
         begin
               return Self.Value.Value.Param_Actual_Value;
         end As_Param_Actual;

         function Create_Param_Actual
           (Value : Param_Actual) return Value_Type is
         begin
            return Result : constant Value_Type := Allocate
              (Param_Actual_Value)
            do
                  Result.Value.Value.Param_Actual_Value := Value;
            end return;
         end Create_Param_Actual;
         function As_Param_Actual_Array
           (Self : Value_Type) return Param_Actual_Array is
         begin
               return Result : Param_Actual_Array
                 (Self.Value.Value.Param_Actual_Array_Value'Range)
               do
                  for I in Result'Range loop
                        Result (I) :=
                           Self.Value.Value.Param_Actual_Array_Value.all (I);
                  end loop;
               end return;

         end As_Param_Actual_Array;

         function Create_Param_Actual_Array
           (Value : Param_Actual_Array) return Value_Type is
         begin
            return Result : constant Value_Type := Allocate
              (Param_Actual_Array_Value)
            do
                  Result.Value.Value.Param_Actual_Array_Value :=
                     new Param_Actual_Array (Value'Range);
                  for I in Value'Range loop
                        Result.Value.Value.Param_Actual_Array_Value.all (I) :=
                           Value (I);
                  end loop;

            end return;
         end Create_Param_Actual_Array;
         function As_Ref_Result
           (Self : Value_Type) return Ref_Result is
         begin
               return Self.Value.Value.Ref_Result_Value;
         end As_Ref_Result;

         function Create_Ref_Result
           (Value : Ref_Result) return Value_Type is
         begin
            return Result : constant Value_Type := Allocate
              (Ref_Result_Value)
            do
                  Result.Value.Value.Ref_Result_Value := Value;
            end return;
         end Create_Ref_Result;
         function As_Ref_Result_Array
           (Self : Value_Type) return Ref_Result_Array is
         begin
               return Result : Ref_Result_Array
                 (Self.Value.Value.Ref_Result_Array_Value'Range)
               do
                  for I in Result'Range loop
                        Result (I) :=
                           Self.Value.Value.Ref_Result_Array_Value.all (I);
                  end loop;
               end return;

         end As_Ref_Result_Array;

         function Create_Ref_Result_Array
           (Value : Ref_Result_Array) return Value_Type is
         begin
            return Result : constant Value_Type := Allocate
              (Ref_Result_Array_Value)
            do
                  Result.Value.Value.Ref_Result_Array_Value :=
                     new Ref_Result_Array (Value'Range);
                  for I in Value'Range loop
                        Result.Value.Value.Ref_Result_Array_Value.all (I) :=
                           Value (I);
                  end loop;

            end return;
         end Create_Ref_Result_Array;
         function As_Refd_Decl
           (Self : Value_Type) return Refd_Decl is
         begin
               return Self.Value.Value.Refd_Decl_Value;
         end As_Refd_Decl;

         function Create_Refd_Decl
           (Value : Refd_Decl) return Value_Type is
         begin
            return Result : constant Value_Type := Allocate
              (Refd_Decl_Value)
            do
                  Result.Value.Value.Refd_Decl_Value := Value;
            end return;
         end Create_Refd_Decl;
         function As_Refd_Def
           (Self : Value_Type) return Refd_Def is
         begin
               return Self.Value.Value.Refd_Def_Value;
         end As_Refd_Def;

         function Create_Refd_Def
           (Value : Refd_Def) return Value_Type is
         begin
            return Result : constant Value_Type := Allocate
              (Refd_Def_Value)
            do
                  Result.Value.Value.Refd_Def_Value := Value;
            end return;
         end Create_Refd_Def;
         function As_Shape
           (Self : Value_Type) return Shape is
         begin
               return Self.Value.Value.Shape_Value;
         end As_Shape;

         function Create_Shape
           (Value : Shape) return Value_Type is
         begin
            return Result : constant Value_Type := Allocate
              (Shape_Value)
            do
                  Result.Value.Value.Shape_Value := Value;
            end return;
         end Create_Shape;
         function As_Shape_Array
           (Self : Value_Type) return Shape_Array is
         begin
               return Result : Shape_Array
                 (Self.Value.Value.Shape_Array_Value'Range)
               do
                  for I in Result'Range loop
                        Result (I) :=
                           Self.Value.Value.Shape_Array_Value.all (I);
                  end loop;
               end return;

         end As_Shape_Array;

         function Create_Shape_Array
           (Value : Shape_Array) return Value_Type is
         begin
            return Result : constant Value_Type := Allocate
              (Shape_Array_Value)
            do
                  Result.Value.Value.Shape_Array_Value :=
                     new Shape_Array (Value'Range);
                  for I in Value'Range loop
                        Result.Value.Value.Shape_Array_Value.all (I) :=
                           Value (I);
                  end loop;

            end return;
         end Create_Shape_Array;
         function As_Substitution
           (Self : Value_Type) return Substitution is
         begin
               return Self.Value.Value.Substitution_Value;
         end As_Substitution;

         function Create_Substitution
           (Value : Substitution) return Value_Type is
         begin
            return Result : constant Value_Type := Allocate
              (Substitution_Value)
            do
                  Result.Value.Value.Substitution_Value := Value;
            end return;
         end Create_Substitution;
         function As_Substitution_Array
           (Self : Value_Type) return Substitution_Array is
         begin
               return Result : Substitution_Array
                 (Self.Value.Value.Substitution_Array_Value'Range)
               do
                  for I in Result'Range loop
                        Result (I) :=
                           Self.Value.Value.Substitution_Array_Value.all (I);
                  end loop;
               end return;

         end As_Substitution_Array;

         function Create_Substitution_Array
           (Value : Substitution_Array) return Value_Type is
         begin
            return Result : constant Value_Type := Allocate
              (Substitution_Array_Value)
            do
                  Result.Value.Value.Substitution_Array_Value :=
                     new Substitution_Array (Value'Range);
                  for I in Value'Range loop
                        Result.Value.Value.Substitution_Array_Value.all (I) :=
                           Value (I);
                  end loop;

            end return;
         end Create_Substitution_Array;
         function As_Analysis_Unit_Array
           (Self : Value_Type) return Analysis_Unit_Array is
         begin
               return Result : Analysis_Unit_Array
                 (Self.Value.Value.Analysis_Unit_Array_Value'Range)
               do
                  for I in Result'Range loop
                        Result (I) :=
                           Self.Value.Value.Analysis_Unit_Array_Value.all (I);
                  end loop;
               end return;

         end As_Analysis_Unit_Array;

         function Create_Analysis_Unit_Array
           (Value : Analysis_Unit_Array) return Value_Type is
         begin
            return Result : constant Value_Type := Allocate
              (Analysis_Unit_Array_Value)
            do
                  Result.Value.Value.Analysis_Unit_Array_Value :=
                     new Analysis_Unit_Array (Value'Range);
                  for I in Value'Range loop
                        Result.Value.Value.Analysis_Unit_Array_Value.all (I) :=
                           Value (I);
                  end loop;

            end return;
         end Create_Analysis_Unit_Array;
         function As_Unbounded_Text_Type_Array
           (Self : Value_Type) return Unbounded_Text_Type_Array is
         begin
               return Result : Unbounded_Text_Type_Array
                 (Self.Value.Value.Unbounded_Text_Type_Array_Value'Range)
               do
                  for I in Result'Range loop
                        Result (I) :=
                           Self.Value.Value.Unbounded_Text_Type_Array_Value.all (I);
                  end loop;
               end return;

         end As_Unbounded_Text_Type_Array;

         function Create_Unbounded_Text_Type_Array
           (Value : Unbounded_Text_Type_Array) return Value_Type is
         begin
            return Result : constant Value_Type := Allocate
              (Unbounded_Text_Type_Array_Value)
            do
                  Result.Value.Value.Unbounded_Text_Type_Array_Value :=
                     new Unbounded_Text_Type_Array (Value'Range);
                  for I in Value'Range loop
                        Result.Value.Value.Unbounded_Text_Type_Array_Value.all (I) :=
                           Value (I);
                  end loop;

            end return;
         end Create_Unbounded_Text_Type_Array;

   --------------
   -- DSL_Name --
   --------------

   function DSL_Name (Id : Node_Type_Id) return Text_Type is
   begin
      return Impl.DSL_Name (Id);
   end DSL_Name;

   ---------------------
   -- Lookup_DSL_Name --
   ---------------------

   function Lookup_DSL_Name (Name : Text_Type) return Any_Node_Type_Id is
   begin
      return Impl.Lookup_DSL_Name (Name);
   end Lookup_DSL_Name;

   -----------------
   -- Is_Abstract --
   -----------------

   function Is_Abstract (Id : Node_Type_Id) return Boolean is
   begin
      return Impl.Is_Abstract (Id);
   end Is_Abstract;

   --------------
   -- Kind_For --
   --------------

   function Kind_For (Id : Node_Type_Id) return Ada_Node_Kind_Type is
   begin
      return Impl.Kind_For (Id);
   end Kind_For;

   --------------------
   -- First_Kind_For --
   --------------------

   function First_Kind_For (Id : Node_Type_Id) return Ada_Node_Kind_Type is
   begin
      return Impl.First_Kind_For (Id);
   end First_Kind_For;

   -------------------
   -- Last_Kind_For --
   -------------------

   function Last_Kind_For (Id : Node_Type_Id) return Ada_Node_Kind_Type is
   begin
      return Impl.Last_Kind_For (Id);
   end Last_Kind_For;

   -----------------
   -- Id_For_Kind --
   -----------------

   function Id_For_Kind (Kind : Ada_Node_Kind_Type) return Node_Type_Id is
   begin
      return Impl.Id_For_Kind (Kind);
   end Id_For_Kind;

   ------------------
   -- Is_Root_Node --
   ------------------

   function Is_Root_Node (Id : Node_Type_Id) return Boolean is
   begin
      return Impl.Is_Root_Node (Id);
   end Is_Root_Node;

   ---------------
   -- Base_Type --
   ---------------

   function Base_Type (Id : Node_Type_Id) return Node_Type_Id is
   begin
      return Impl.Base_Type (Id);
   end Base_Type;

   -------------------
   -- Derived_Types --
   -------------------

   function Derived_Types (Id : Node_Type_Id) return Node_Type_Id_Array is
   begin
      return Impl.Derived_Types (Id);
   end Derived_Types;

   ---------------------
   -- Is_Derived_From --
   ---------------------

   function Is_Derived_From (Id, Parent : Node_Type_Id) return Boolean is
   begin
      return Impl.Is_Derived_From (Id, Parent);
   end Is_Derived_From;

   --------------
   -- DSL_Name --
   --------------

   function DSL_Name (Constraint : Type_Constraint) return Text_Type is
   begin
      
      case Constraint.Kind is
               when Boolean_Value =>
                  return "Bool";
               when Integer_Value =>
                  return "Int";
               when Big_Integer_Value =>
                  return "BigInt";
               when Character_Value =>
                  return "Character";
               when String_Value =>
                  return "String";
               when Token_Value =>
                  return "Token";
               when Unbounded_Text_Value =>
                  return "Symbol";
               when Analysis_Unit_Value =>
                  return "AnalysisUnit";
               when Analysis_Unit_Kind_Value =>
                  return "AnalysisUnitKind";
               when Lookup_Kind_Value =>
                  return "LookupKind";
               when Designated_Env_Kind_Value =>
                  return "DesignatedEnvKind";
               when Ref_Result_Kind_Value =>
                  return "RefResultKind";
               when Call_Expr_Kind_Value =>
                  return "CallExprKind";
               when Grammar_Rule_Value =>
                  return "GrammarRule";
               when Aspect_Value =>
                  return "Aspect";
               when Completion_Item_Value =>
                  return "CompletionItem";
               when Completion_Item_Iterator_Value =>
                  return "InternalCompletionItemIteratorAccess";
               when Discrete_Range_Value =>
                  return "DiscreteRange";
               when Discriminant_Values_Value =>
                  return "DiscriminantValues";
               when Discriminant_Values_Array_Value =>
                  return "DiscriminantValues.array";
               when Doc_Annotation_Value =>
                  return "DocAnnotation";
               when Doc_Annotation_Array_Value =>
                  return "DocAnnotation.array";
               when Accept_Stmt_Array_Value =>
                  return "AcceptStmt.entity.array";
               when Ada_Node_Array_Value =>
                  return "AdaNode.entity.array";
               when Base_Formal_Param_Decl_Array_Value =>
                  return "BaseFormalParamDecl.entity.array";
               when Base_Type_Decl_Array_Value =>
                  return "BaseTypeDecl.entity.array";
               when Basic_Decl_Array_Value =>
                  return "BasicDecl.entity.array";
               when Compilation_Unit_Array_Value =>
                  return "CompilationUnit.entity.array";
               when Defining_Name_Array_Value =>
                  return "DefiningName.entity.array";
               when Expr_Array_Value =>
                  return "Expr.entity.array";
               when Generic_Instantiation_Array_Value =>
                  return "GenericInstantiation.entity.array";
               when Param_Spec_Array_Value =>
                  return "ParamSpec.entity.array";
               when Pragma_Node_Array_Value =>
                  return "Pragma.entity.array";
               when Type_Decl_Array_Value =>
                  return "TypeDecl.entity.array";
               when Param_Actual_Value =>
                  return "ParamActual";
               when Param_Actual_Array_Value =>
                  return "ParamActual.array";
               when Ref_Result_Value =>
                  return "RefResult";
               when Ref_Result_Array_Value =>
                  return "RefResult.array";
               when Refd_Decl_Value =>
                  return "RefdDecl";
               when Refd_Def_Value =>
                  return "RefdDef";
               when Shape_Value =>
                  return "Shape";
               when Shape_Array_Value =>
                  return "Shape.array";
               when Substitution_Value =>
                  return "Substitution";
               when Substitution_Array_Value =>
                  return "Substitution.array";
               when Analysis_Unit_Array_Value =>
                  return "AnalysisUnit.array";
               when Unbounded_Text_Type_Array_Value =>
                  return "Symbol.array";

         when Node_Value =>
            return DSL_Name (Constraint.Node_Type);
      end case;
   end DSL_Name;

   ---------------
   -- Satisfies --
   ---------------

   function Satisfies
     (Value : Value_Type; Constraint : Type_Constraint) return Boolean is
   begin
      if Value.Value.Value.Kind /= Constraint.Kind then
         return False;
      end if;

      case Constraint.Kind is
         when Node_Value =>
            return

              --  A null node always satisfies the type constraint
              Value.Value.Value.Node_Value.Is_Null

              --  Else, check that the type of the node is derived from the
              --  type of the constraint.
              or else Is_Derived_From
                (Id_For_Kind (Value.Value.Value.Node_Value.Kind),
                 Constraint.Node_Type);

         when others =>
            return True;
      end case;
   end Satisfies;

   

   ---------------------
   -- Enum_Last_Value --
   ---------------------

   function Enum_Last_Value (Kind : Enum_Value_Kind) return Enum_Value_Index is
   begin
      case Kind is
            when Analysis_Unit_Kind_Value =>
               return 2;
            when Lookup_Kind_Value =>
               return 3;
            when Designated_Env_Kind_Value =>
               return 4;
            when Ref_Result_Kind_Value =>
               return 4;
            when Call_Expr_Kind_Value =>
               return 4;
            when Grammar_Rule_Value =>
               return 209;
      end case;
   end Enum_Last_Value;

   ------------------------
   -- Enum_Default_Value --
   ------------------------

   function Enum_Default_Value
     (Kind : Enum_Value_Kind) return Any_Enum_Value_Index is
   begin
      case Kind is
            when Analysis_Unit_Kind_Value =>
                  return No_Enum_Value_Index;
            when Lookup_Kind_Value =>
                  return No_Enum_Value_Index;
            when Designated_Env_Kind_Value =>
                  return 1;
            when Ref_Result_Kind_Value =>
                  return 1;
            when Call_Expr_Kind_Value =>
                  return No_Enum_Value_Index;
            when Grammar_Rule_Value =>
                  return No_Enum_Value_Index;
      end case;
   end Enum_Default_Value;

   ---------------------
   -- Enum_Value_Name --
   ---------------------

   function Enum_Value_Name
     (Kind : Enum_Value_Kind; Index : Enum_Value_Index) return Text_Type is
   begin
      case Kind is
            when Analysis_Unit_Kind_Value =>
               case Index is
                     when 1 =>
                        return "unit_specification";
                     when 2 =>
                        return "unit_body";

                  when others => null;
               end case;
            when Lookup_Kind_Value =>
               case Index is
                     when 1 =>
                        return "recursive";
                     when 2 =>
                        return "flat";
                     when 3 =>
                        return "minimal";

                  when others => null;
               end case;
            when Designated_Env_Kind_Value =>
               case Index is
                     when 1 =>
                        return "none";
                     when 2 =>
                        return "current_env";
                     when 3 =>
                        return "named_env";
                     when 4 =>
                        return "direct_env";

                  when others => null;
               end case;
            when Ref_Result_Kind_Value =>
               case Index is
                     when 1 =>
                        return "no_ref";
                     when 2 =>
                        return "precise";
                     when 3 =>
                        return "imprecise";
                     when 4 =>
                        return "error";

                  when others => null;
               end case;
            when Call_Expr_Kind_Value =>
               case Index is
                     when 1 =>
                        return "call";
                     when 2 =>
                        return "array_slice";
                     when 3 =>
                        return "array_index";
                     when 4 =>
                        return "type_conversion";

                  when others => null;
               end case;
            when Grammar_Rule_Value =>
               case Index is
                     when 1 =>
                        return "parent_list_rule";
                     when 2 =>
                        return "protected_type_decl_rule";
                     when 3 =>
                        return "protected_op_rule";
                     when 4 =>
                        return "protected_el_rule";
                     when 5 =>
                        return "protected_def_rule";
                     when 6 =>
                        return "protected_decl_rule";
                     when 7 =>
                        return "task_item_rule";
                     when 8 =>
                        return "task_def_rule";
                     when 9 =>
                        return "task_type_decl_rule";
                     when 10 =>
                        return "subtype_decl_rule";
                     when 11 =>
                        return "interface_type_def_rule";
                     when 12 =>
                        return "unconstrained_index_rule";
                     when 13 =>
                        return "array_type_def_rule";
                     when 14 =>
                        return "discrete_subtype_definition_rule";
                     when 15 =>
                        return "constraint_list_rule";
                     when 16 =>
                        return "signed_int_type_def_rule";
                     when 17 =>
                        return "mod_int_type_def_rule";
                     when 18 =>
                        return "derived_type_def_rule";
                     when 19 =>
                        return "composite_constraint_assoc_rule";
                     when 20 =>
                        return "composite_constraint_rule";
                     when 21 =>
                        return "digits_constraint_rule";
                     when 22 =>
                        return "delta_constraint_rule";
                     when 23 =>
                        return "range_constraint_rule";
                     when 24 =>
                        return "constraint_rule";
                     when 25 =>
                        return "discriminant_spec_rule";
                     when 26 =>
                        return "discr_spec_list_rule";
                     when 27 =>
                        return "discriminant_part_rule";
                     when 28 =>
                        return "enum_literal_decl_rule";
                     when 29 =>
                        return "formal_discrete_type_def_rule";
                     when 30 =>
                        return "record_def_rule";
                     when 31 =>
                        return "range_spec_rule";
                     when 32 =>
                        return "real_type_def_rule";
                     when 33 =>
                        return "sexpr_or_box_rule";
                     when 34 =>
                        return "ordinary_fixed_point_def_rule";
                     when 35 =>
                        return "decimal_fixed_point_def_rule";
                     when 36 =>
                        return "floating_point_def_rule";
                     when 37 =>
                        return "record_type_def_rule";
                     when 38 =>
                        return "access_def_rule";
                     when 39 =>
                        return "enum_type_def_rule";
                     when 40 =>
                        return "type_def_rule";
                     when 41 =>
                        return "variant_rule";
                     when 42 =>
                        return "anonymous_type_decl_rule";
                     when 43 =>
                        return "incomplete_type_decl_rule";
                     when 44 =>
                        return "type_decl_rule";
                     when 45 =>
                        return "variant_part_rule";
                     when 46 =>
                        return "component_def_rule";
                     when 47 =>
                        return "component_item_rule";
                     when 48 =>
                        return "component_decl_rule";
                     when 49 =>
                        return "component_list_rule";
                     when 50 =>
                        return "generic_decl_rule";
                     when 51 =>
                        return "generic_formal_part_rule";
                     when 52 =>
                        return "generic_formal_decl_rule";
                     when 53 =>
                        return "formal_type_decl_rule";
                     when 54 =>
                        return "formal_subp_decl_rule";
                     when 55 =>
                        return "renaming_clause_rule";
                     when 56 =>
                        return "generic_renaming_decl_rule";
                     when 57 =>
                        return "generic_instantiation_rule";
                     when 58 =>
                        return "exception_decl_rule";
                     when 59 =>
                        return "basic_decls_rule";
                     when 60 =>
                        return "package_renaming_decl_rule";
                     when 61 =>
                        return "package_decl_rule";
                     when 62 =>
                        return "basic_decl_rule";
                     when 63 =>
                        return "object_decl_rule";
                     when 64 =>
                        return "sub_object_decl_rule";
                     when 65 =>
                        return "no_type_object_renaming_decl_rule";
                     when 66 =>
                        return "ext_ret_stmt_object_decl_rule";
                     when 67 =>
                        return "defining_id_list_rule";
                     when 68 =>
                        return "number_decl_rule";
                     when 69 =>
                        return "contract_case_assoc_rule";
                     when 70 =>
                        return "contract_cases_expr_rule";
                     when 71 =>
                        return "abstract_state_decl_rule";
                     when 72 =>
                        return "multi_abstract_state_decl_rule";
                     when 73 =>
                        return "aspect_assoc_rule";
                     when 74 =>
                        return "aspect_spec_rule";
                     when 75 =>
                        return "single_task_decl_rule";
                     when 76 =>
                        return "overriding_indicator_rule";
                     when 77 =>
                        return "entry_decl_rule";
                     when 78 =>
                        return "component_clause_rule";
                     when 79 =>
                        return "aspect_clause_rule";
                     when 80 =>
                        return "param_spec_rule";
                     when 81 =>
                        return "param_specs_rule";
                     when 82 =>
                        return "subp_spec_rule";
                     when 83 =>
                        return "expr_fn_rule";
                     when 84 =>
                        return "null_subp_decl_rule";
                     when 85 =>
                        return "abstract_subp_decl_rule";
                     when 86 =>
                        return "subp_renaming_decl_rule";
                     when 87 =>
                        return "simple_subp_decl_rule";
                     when 88 =>
                        return "subp_decl_rule";
                     when 89 =>
                        return "with_clause_rule";
                     when 90 =>
                        return "context_item_rule";
                     when 91 =>
                        return "use_clause_rule";
                     when 92 =>
                        return "use_package_clause_rule";
                     when 93 =>
                        return "use_type_clause_rule";
                     when 94 =>
                        return "subtype_indication_rule";
                     when 95 =>
                        return "discrete_subtype_indication_rule";
                     when 96 =>
                        return "constrained_subtype_indication_rule";
                     when 97 =>
                        return "type_expr_rule";
                     when 98 =>
                        return "anonymous_type_rule";
                     when 99 =>
                        return "mode_rule";
                     when 100 =>
                        return "pragma_argument_rule";
                     when 101 =>
                        return "pragma_rule";
                     when 102 =>
                        return "subunit_rule";
                     when 103 =>
                        return "library_unit_body_rule";
                     when 104 =>
                        return "library_unit_renaming_decl_rule";
                     when 105 =>
                        return "library_item_rule";
                     when 106 =>
                        return "compilation_unit_rule";
                     when 107 =>
                        return "compilation_rule";
                     when 108 =>
                        return "decl_part_rule";
                     when 109 =>
                        return "entry_body_rule";
                     when 110 =>
                        return "protected_body_rule";
                     when 111 =>
                        return "protected_body_stub_rule";
                     when 112 =>
                        return "task_body_rule";
                     when 113 =>
                        return "task_body_stub_rule";
                     when 114 =>
                        return "package_body_stub_rule";
                     when 115 =>
                        return "package_body_rule";
                     when 116 =>
                        return "terminate_alternative_rule";
                     when 117 =>
                        return "select_stmt_rule";
                     when 118 =>
                        return "accept_stmt_rule";
                     when 119 =>
                        return "case_alt_rule";
                     when 120 =>
                        return "case_stmt_rule";
                     when 121 =>
                        return "ext_return_stmt_rule";
                     when 122 =>
                        return "iblock_stmt_rule";
                     when 123 =>
                        return "block_stmt_rule";
                     when 124 =>
                        return "while_loop_spec_rule";
                     when 125 =>
                        return "iloop_stmt_rule";
                     when 126 =>
                        return "loop_stmt_rule";
                     when 127 =>
                        return "compound_stmt_rule";
                     when 128 =>
                        return "elsif_part_rule";
                     when 129 =>
                        return "if_stmt_rule";
                     when 130 =>
                        return "raise_stmt_rule";
                     when 131 =>
                        return "delay_stmt_rule";
                     when 132 =>
                        return "abort_stmt_rule";
                     when 133 =>
                        return "body_rule";
                     when 134 =>
                        return "body_stub_rule";
                     when 135 =>
                        return "subp_body_stub_rule";
                     when 136 =>
                        return "recov_decl_part_rule";
                     when 137 =>
                        return "subp_body_rule";
                     when 138 =>
                        return "handled_stmts_rule";
                     when 139 =>
                        return "exception_handler_rule";
                     when 140 =>
                        return "stmts_rule";
                     when 141 =>
                        return "label_rule";
                     when 142 =>
                        return "stmt_rule";
                     when 143 =>
                        return "call_stmt_rule";
                     when 144 =>
                        return "simple_stmt_rule";
                     when 145 =>
                        return "null_stmt_rule";
                     when 146 =>
                        return "assignment_stmt_rule";
                     when 147 =>
                        return "goto_stmt_rule";
                     when 148 =>
                        return "exit_stmt_rule";
                     when 149 =>
                        return "return_stmt_rule";
                     when 150 =>
                        return "requeue_stmt_rule";
                     when 151 =>
                        return "identifier_rule";
                     when 152 =>
                        return "char_literal_rule";
                     when 153 =>
                        return "string_literal_rule";
                     when 154 =>
                        return "defining_id_rule";
                     when 155 =>
                        return "dec_literal_rule";
                     when 156 =>
                        return "int_literal_rule";
                     when 157 =>
                        return "num_literal_rule";
                     when 158 =>
                        return "null_literal_rule";
                     when 159 =>
                        return "allocator_rule";
                     when 160 =>
                        return "for_loop_param_spec_rule";
                     when 161 =>
                        return "quantified_expr_rule";
                     when 162 =>
                        return "case_expr_rule";
                     when 163 =>
                        return "case_expr_alt_rule";
                     when 164 =>
                        return "raise_expr_rule";
                     when 165 =>
                        return "if_expr_rule";
                     when 166 =>
                        return "conditional_expr_rule";
                     when 167 =>
                        return "box_expr_rule";
                     when 168 =>
                        return "others_designator_rule";
                     when 169 =>
                        return "iterated_assoc_rule";
                     when 170 =>
                        return "aggregate_assoc_rule";
                     when 171 =>
                        return "regular_aggregate_rule";
                     when 172 =>
                        return "bracket_aggregate_rule";
                     when 173 =>
                        return "aggregate_rule";
                     when 174 =>
                        return "direct_name_rule";
                     when 175 =>
                        return "param_assoc_rule";
                     when 176 =>
                        return "call_suffix_rule";
                     when 177 =>
                        return "attr_suffix_rule";
                     when 178 =>
                        return "qualified_name_rule";
                     when 179 =>
                        return "qual_name_internal_rule";
                     when 180 =>
                        return "value_sequence_rule";
                     when 181 =>
                        return "name_rule";
                     when 182 =>
                        return "defining_name_rule";
                     when 183 =>
                        return "direct_name_or_target_name_rule";
                     when 184 =>
                        return "target_name_rule";
                     when 185 =>
                        return "update_attr_aggregate_rule";
                     when 186 =>
                        return "update_attr_content_rule";
                     when 187 =>
                        return "multidim_array_assoc_rule";
                     when 188 =>
                        return "subtype_name_rule";
                     when 189 =>
                        return "static_name_rule";
                     when 190 =>
                        return "primary_rule";
                     when 191 =>
                        return "paren_expr_rule";
                     when 192 =>
                        return "declare_expr_rule";
                     when 193 =>
                        return "factor_rule";
                     when 194 =>
                        return "term_rule";
                     when 195 =>
                        return "unop_term_rule";
                     when 196 =>
                        return "simple_expr_rule";
                     when 197 =>
                        return "boolean_op_rule";
                     when 198 =>
                        return "discrete_range_rule";
                     when 199 =>
                        return "choice_rule";
                     when 200 =>
                        return "choice_list_rule";
                     when 201 =>
                        return "rel_op_rule";
                     when 202 =>
                        return "membership_choice_rule";
                     when 203 =>
                        return "membership_choice_list_rule";
                     when 204 =>
                        return "relation_rule";
                     when 205 =>
                        return "expr_rule";
                     when 206 =>
                        return "pp_directive_rule";
                     when 207 =>
                        return "pp_then_rule";
                     when 208 =>
                        return "pp_expr_rule";
                     when 209 =>
                        return "pp_term_rule";

                  when others => null;
               end case;
      end case;

      return (raise Out_Of_Bounds_Error with "out of bounds enum value index");
   end Enum_Value_Name;

   -----------------------
   -- Lookup_Enum_Value --
   -----------------------

   function Lookup_Enum_Value
     (Kind : Enum_Value_Kind; Name : Text_Type) return Any_Enum_Value_Index is
   begin
      case Kind is
            when Analysis_Unit_Kind_Value =>
               if
                     Name = "unit_specification"
                  then
                     return 1;
                     elsif
                     Name = "unit_body"
                  then
                     return 2;
               end if;
            when Lookup_Kind_Value =>
               if
                     Name = "recursive"
                  then
                     return 1;
                     elsif
                     Name = "flat"
                  then
                     return 2;
                     elsif
                     Name = "minimal"
                  then
                     return 3;
               end if;
            when Designated_Env_Kind_Value =>
               if
                     Name = "none"
                  then
                     return 1;
                     elsif
                     Name = "current_env"
                  then
                     return 2;
                     elsif
                     Name = "named_env"
                  then
                     return 3;
                     elsif
                     Name = "direct_env"
                  then
                     return 4;
               end if;
            when Ref_Result_Kind_Value =>
               if
                     Name = "no_ref"
                  then
                     return 1;
                     elsif
                     Name = "precise"
                  then
                     return 2;
                     elsif
                     Name = "imprecise"
                  then
                     return 3;
                     elsif
                     Name = "error"
                  then
                     return 4;
               end if;
            when Call_Expr_Kind_Value =>
               if
                     Name = "call"
                  then
                     return 1;
                     elsif
                     Name = "array_slice"
                  then
                     return 2;
                     elsif
                     Name = "array_index"
                  then
                     return 3;
                     elsif
                     Name = "type_conversion"
                  then
                     return 4;
               end if;
            when Grammar_Rule_Value =>
               if
                     Name = "parent_list_rule"
                  then
                     return 1;
                     elsif
                     Name = "protected_type_decl_rule"
                  then
                     return 2;
                     elsif
                     Name = "protected_op_rule"
                  then
                     return 3;
                     elsif
                     Name = "protected_el_rule"
                  then
                     return 4;
                     elsif
                     Name = "protected_def_rule"
                  then
                     return 5;
                     elsif
                     Name = "protected_decl_rule"
                  then
                     return 6;
                     elsif
                     Name = "task_item_rule"
                  then
                     return 7;
                     elsif
                     Name = "task_def_rule"
                  then
                     return 8;
                     elsif
                     Name = "task_type_decl_rule"
                  then
                     return 9;
                     elsif
                     Name = "subtype_decl_rule"
                  then
                     return 10;
                     elsif
                     Name = "interface_type_def_rule"
                  then
                     return 11;
                     elsif
                     Name = "unconstrained_index_rule"
                  then
                     return 12;
                     elsif
                     Name = "array_type_def_rule"
                  then
                     return 13;
                     elsif
                     Name = "discrete_subtype_definition_rule"
                  then
                     return 14;
                     elsif
                     Name = "constraint_list_rule"
                  then
                     return 15;
                     elsif
                     Name = "signed_int_type_def_rule"
                  then
                     return 16;
                     elsif
                     Name = "mod_int_type_def_rule"
                  then
                     return 17;
                     elsif
                     Name = "derived_type_def_rule"
                  then
                     return 18;
                     elsif
                     Name = "composite_constraint_assoc_rule"
                  then
                     return 19;
                     elsif
                     Name = "composite_constraint_rule"
                  then
                     return 20;
                     elsif
                     Name = "digits_constraint_rule"
                  then
                     return 21;
                     elsif
                     Name = "delta_constraint_rule"
                  then
                     return 22;
                     elsif
                     Name = "range_constraint_rule"
                  then
                     return 23;
                     elsif
                     Name = "constraint_rule"
                  then
                     return 24;
                     elsif
                     Name = "discriminant_spec_rule"
                  then
                     return 25;
                     elsif
                     Name = "discr_spec_list_rule"
                  then
                     return 26;
                     elsif
                     Name = "discriminant_part_rule"
                  then
                     return 27;
                     elsif
                     Name = "enum_literal_decl_rule"
                  then
                     return 28;
                     elsif
                     Name = "formal_discrete_type_def_rule"
                  then
                     return 29;
                     elsif
                     Name = "record_def_rule"
                  then
                     return 30;
                     elsif
                     Name = "range_spec_rule"
                  then
                     return 31;
                     elsif
                     Name = "real_type_def_rule"
                  then
                     return 32;
                     elsif
                     Name = "sexpr_or_box_rule"
                  then
                     return 33;
                     elsif
                     Name = "ordinary_fixed_point_def_rule"
                  then
                     return 34;
                     elsif
                     Name = "decimal_fixed_point_def_rule"
                  then
                     return 35;
                     elsif
                     Name = "floating_point_def_rule"
                  then
                     return 36;
                     elsif
                     Name = "record_type_def_rule"
                  then
                     return 37;
                     elsif
                     Name = "access_def_rule"
                  then
                     return 38;
                     elsif
                     Name = "enum_type_def_rule"
                  then
                     return 39;
                     elsif
                     Name = "type_def_rule"
                  then
                     return 40;
                     elsif
                     Name = "variant_rule"
                  then
                     return 41;
                     elsif
                     Name = "anonymous_type_decl_rule"
                  then
                     return 42;
                     elsif
                     Name = "incomplete_type_decl_rule"
                  then
                     return 43;
                     elsif
                     Name = "type_decl_rule"
                  then
                     return 44;
                     elsif
                     Name = "variant_part_rule"
                  then
                     return 45;
                     elsif
                     Name = "component_def_rule"
                  then
                     return 46;
                     elsif
                     Name = "component_item_rule"
                  then
                     return 47;
                     elsif
                     Name = "component_decl_rule"
                  then
                     return 48;
                     elsif
                     Name = "component_list_rule"
                  then
                     return 49;
                     elsif
                     Name = "generic_decl_rule"
                  then
                     return 50;
                     elsif
                     Name = "generic_formal_part_rule"
                  then
                     return 51;
                     elsif
                     Name = "generic_formal_decl_rule"
                  then
                     return 52;
                     elsif
                     Name = "formal_type_decl_rule"
                  then
                     return 53;
                     elsif
                     Name = "formal_subp_decl_rule"
                  then
                     return 54;
                     elsif
                     Name = "renaming_clause_rule"
                  then
                     return 55;
                     elsif
                     Name = "generic_renaming_decl_rule"
                  then
                     return 56;
                     elsif
                     Name = "generic_instantiation_rule"
                  then
                     return 57;
                     elsif
                     Name = "exception_decl_rule"
                  then
                     return 58;
                     elsif
                     Name = "basic_decls_rule"
                  then
                     return 59;
                     elsif
                     Name = "package_renaming_decl_rule"
                  then
                     return 60;
                     elsif
                     Name = "package_decl_rule"
                  then
                     return 61;
                     elsif
                     Name = "basic_decl_rule"
                  then
                     return 62;
                     elsif
                     Name = "object_decl_rule"
                  then
                     return 63;
                     elsif
                     Name = "sub_object_decl_rule"
                  then
                     return 64;
                     elsif
                     Name = "no_type_object_renaming_decl_rule"
                  then
                     return 65;
                     elsif
                     Name = "ext_ret_stmt_object_decl_rule"
                  then
                     return 66;
                     elsif
                     Name = "defining_id_list_rule"
                  then
                     return 67;
                     elsif
                     Name = "number_decl_rule"
                  then
                     return 68;
                     elsif
                     Name = "contract_case_assoc_rule"
                  then
                     return 69;
                     elsif
                     Name = "contract_cases_expr_rule"
                  then
                     return 70;
                     elsif
                     Name = "abstract_state_decl_rule"
                  then
                     return 71;
                     elsif
                     Name = "multi_abstract_state_decl_rule"
                  then
                     return 72;
                     elsif
                     Name = "aspect_assoc_rule"
                  then
                     return 73;
                     elsif
                     Name = "aspect_spec_rule"
                  then
                     return 74;
                     elsif
                     Name = "single_task_decl_rule"
                  then
                     return 75;
                     elsif
                     Name = "overriding_indicator_rule"
                  then
                     return 76;
                     elsif
                     Name = "entry_decl_rule"
                  then
                     return 77;
                     elsif
                     Name = "component_clause_rule"
                  then
                     return 78;
                     elsif
                     Name = "aspect_clause_rule"
                  then
                     return 79;
                     elsif
                     Name = "param_spec_rule"
                  then
                     return 80;
                     elsif
                     Name = "param_specs_rule"
                  then
                     return 81;
                     elsif
                     Name = "subp_spec_rule"
                  then
                     return 82;
                     elsif
                     Name = "expr_fn_rule"
                  then
                     return 83;
                     elsif
                     Name = "null_subp_decl_rule"
                  then
                     return 84;
                     elsif
                     Name = "abstract_subp_decl_rule"
                  then
                     return 85;
                     elsif
                     Name = "subp_renaming_decl_rule"
                  then
                     return 86;
                     elsif
                     Name = "simple_subp_decl_rule"
                  then
                     return 87;
                     elsif
                     Name = "subp_decl_rule"
                  then
                     return 88;
                     elsif
                     Name = "with_clause_rule"
                  then
                     return 89;
                     elsif
                     Name = "context_item_rule"
                  then
                     return 90;
                     elsif
                     Name = "use_clause_rule"
                  then
                     return 91;
                     elsif
                     Name = "use_package_clause_rule"
                  then
                     return 92;
                     elsif
                     Name = "use_type_clause_rule"
                  then
                     return 93;
                     elsif
                     Name = "subtype_indication_rule"
                  then
                     return 94;
                     elsif
                     Name = "discrete_subtype_indication_rule"
                  then
                     return 95;
                     elsif
                     Name = "constrained_subtype_indication_rule"
                  then
                     return 96;
                     elsif
                     Name = "type_expr_rule"
                  then
                     return 97;
                     elsif
                     Name = "anonymous_type_rule"
                  then
                     return 98;
                     elsif
                     Name = "mode_rule"
                  then
                     return 99;
                     elsif
                     Name = "pragma_argument_rule"
                  then
                     return 100;
                     elsif
                     Name = "pragma_rule"
                  then
                     return 101;
                     elsif
                     Name = "subunit_rule"
                  then
                     return 102;
                     elsif
                     Name = "library_unit_body_rule"
                  then
                     return 103;
                     elsif
                     Name = "library_unit_renaming_decl_rule"
                  then
                     return 104;
                     elsif
                     Name = "library_item_rule"
                  then
                     return 105;
                     elsif
                     Name = "compilation_unit_rule"
                  then
                     return 106;
                     elsif
                     Name = "compilation_rule"
                  then
                     return 107;
                     elsif
                     Name = "decl_part_rule"
                  then
                     return 108;
                     elsif
                     Name = "entry_body_rule"
                  then
                     return 109;
                     elsif
                     Name = "protected_body_rule"
                  then
                     return 110;
                     elsif
                     Name = "protected_body_stub_rule"
                  then
                     return 111;
                     elsif
                     Name = "task_body_rule"
                  then
                     return 112;
                     elsif
                     Name = "task_body_stub_rule"
                  then
                     return 113;
                     elsif
                     Name = "package_body_stub_rule"
                  then
                     return 114;
                     elsif
                     Name = "package_body_rule"
                  then
                     return 115;
                     elsif
                     Name = "terminate_alternative_rule"
                  then
                     return 116;
                     elsif
                     Name = "select_stmt_rule"
                  then
                     return 117;
                     elsif
                     Name = "accept_stmt_rule"
                  then
                     return 118;
                     elsif
                     Name = "case_alt_rule"
                  then
                     return 119;
                     elsif
                     Name = "case_stmt_rule"
                  then
                     return 120;
                     elsif
                     Name = "ext_return_stmt_rule"
                  then
                     return 121;
                     elsif
                     Name = "iblock_stmt_rule"
                  then
                     return 122;
                     elsif
                     Name = "block_stmt_rule"
                  then
                     return 123;
                     elsif
                     Name = "while_loop_spec_rule"
                  then
                     return 124;
                     elsif
                     Name = "iloop_stmt_rule"
                  then
                     return 125;
                     elsif
                     Name = "loop_stmt_rule"
                  then
                     return 126;
                     elsif
                     Name = "compound_stmt_rule"
                  then
                     return 127;
                     elsif
                     Name = "elsif_part_rule"
                  then
                     return 128;
                     elsif
                     Name = "if_stmt_rule"
                  then
                     return 129;
                     elsif
                     Name = "raise_stmt_rule"
                  then
                     return 130;
                     elsif
                     Name = "delay_stmt_rule"
                  then
                     return 131;
                     elsif
                     Name = "abort_stmt_rule"
                  then
                     return 132;
                     elsif
                     Name = "body_rule"
                  then
                     return 133;
                     elsif
                     Name = "body_stub_rule"
                  then
                     return 134;
                     elsif
                     Name = "subp_body_stub_rule"
                  then
                     return 135;
                     elsif
                     Name = "recov_decl_part_rule"
                  then
                     return 136;
                     elsif
                     Name = "subp_body_rule"
                  then
                     return 137;
                     elsif
                     Name = "handled_stmts_rule"
                  then
                     return 138;
                     elsif
                     Name = "exception_handler_rule"
                  then
                     return 139;
                     elsif
                     Name = "stmts_rule"
                  then
                     return 140;
                     elsif
                     Name = "label_rule"
                  then
                     return 141;
                     elsif
                     Name = "stmt_rule"
                  then
                     return 142;
                     elsif
                     Name = "call_stmt_rule"
                  then
                     return 143;
                     elsif
                     Name = "simple_stmt_rule"
                  then
                     return 144;
                     elsif
                     Name = "null_stmt_rule"
                  then
                     return 145;
                     elsif
                     Name = "assignment_stmt_rule"
                  then
                     return 146;
                     elsif
                     Name = "goto_stmt_rule"
                  then
                     return 147;
                     elsif
                     Name = "exit_stmt_rule"
                  then
                     return 148;
                     elsif
                     Name = "return_stmt_rule"
                  then
                     return 149;
                     elsif
                     Name = "requeue_stmt_rule"
                  then
                     return 150;
                     elsif
                     Name = "identifier_rule"
                  then
                     return 151;
                     elsif
                     Name = "char_literal_rule"
                  then
                     return 152;
                     elsif
                     Name = "string_literal_rule"
                  then
                     return 153;
                     elsif
                     Name = "defining_id_rule"
                  then
                     return 154;
                     elsif
                     Name = "dec_literal_rule"
                  then
                     return 155;
                     elsif
                     Name = "int_literal_rule"
                  then
                     return 156;
                     elsif
                     Name = "num_literal_rule"
                  then
                     return 157;
                     elsif
                     Name = "null_literal_rule"
                  then
                     return 158;
                     elsif
                     Name = "allocator_rule"
                  then
                     return 159;
                     elsif
                     Name = "for_loop_param_spec_rule"
                  then
                     return 160;
                     elsif
                     Name = "quantified_expr_rule"
                  then
                     return 161;
                     elsif
                     Name = "case_expr_rule"
                  then
                     return 162;
                     elsif
                     Name = "case_expr_alt_rule"
                  then
                     return 163;
                     elsif
                     Name = "raise_expr_rule"
                  then
                     return 164;
                     elsif
                     Name = "if_expr_rule"
                  then
                     return 165;
                     elsif
                     Name = "conditional_expr_rule"
                  then
                     return 166;
                     elsif
                     Name = "box_expr_rule"
                  then
                     return 167;
                     elsif
                     Name = "others_designator_rule"
                  then
                     return 168;
                     elsif
                     Name = "iterated_assoc_rule"
                  then
                     return 169;
                     elsif
                     Name = "aggregate_assoc_rule"
                  then
                     return 170;
                     elsif
                     Name = "regular_aggregate_rule"
                  then
                     return 171;
                     elsif
                     Name = "bracket_aggregate_rule"
                  then
                     return 172;
                     elsif
                     Name = "aggregate_rule"
                  then
                     return 173;
                     elsif
                     Name = "direct_name_rule"
                  then
                     return 174;
                     elsif
                     Name = "param_assoc_rule"
                  then
                     return 175;
                     elsif
                     Name = "call_suffix_rule"
                  then
                     return 176;
                     elsif
                     Name = "attr_suffix_rule"
                  then
                     return 177;
                     elsif
                     Name = "qualified_name_rule"
                  then
                     return 178;
                     elsif
                     Name = "qual_name_internal_rule"
                  then
                     return 179;
                     elsif
                     Name = "value_sequence_rule"
                  then
                     return 180;
                     elsif
                     Name = "name_rule"
                  then
                     return 181;
                     elsif
                     Name = "defining_name_rule"
                  then
                     return 182;
                     elsif
                     Name = "direct_name_or_target_name_rule"
                  then
                     return 183;
                     elsif
                     Name = "target_name_rule"
                  then
                     return 184;
                     elsif
                     Name = "update_attr_aggregate_rule"
                  then
                     return 185;
                     elsif
                     Name = "update_attr_content_rule"
                  then
                     return 186;
                     elsif
                     Name = "multidim_array_assoc_rule"
                  then
                     return 187;
                     elsif
                     Name = "subtype_name_rule"
                  then
                     return 188;
                     elsif
                     Name = "static_name_rule"
                  then
                     return 189;
                     elsif
                     Name = "primary_rule"
                  then
                     return 190;
                     elsif
                     Name = "paren_expr_rule"
                  then
                     return 191;
                     elsif
                     Name = "declare_expr_rule"
                  then
                     return 192;
                     elsif
                     Name = "factor_rule"
                  then
                     return 193;
                     elsif
                     Name = "term_rule"
                  then
                     return 194;
                     elsif
                     Name = "unop_term_rule"
                  then
                     return 195;
                     elsif
                     Name = "simple_expr_rule"
                  then
                     return 196;
                     elsif
                     Name = "boolean_op_rule"
                  then
                     return 197;
                     elsif
                     Name = "discrete_range_rule"
                  then
                     return 198;
                     elsif
                     Name = "choice_rule"
                  then
                     return 199;
                     elsif
                     Name = "choice_list_rule"
                  then
                     return 200;
                     elsif
                     Name = "rel_op_rule"
                  then
                     return 201;
                     elsif
                     Name = "membership_choice_rule"
                  then
                     return 202;
                     elsif
                     Name = "membership_choice_list_rule"
                  then
                     return 203;
                     elsif
                     Name = "relation_rule"
                  then
                     return 204;
                     elsif
                     Name = "expr_rule"
                  then
                     return 205;
                     elsif
                     Name = "pp_directive_rule"
                  then
                     return 206;
                     elsif
                     Name = "pp_then_rule"
                  then
                     return 207;
                     elsif
                     Name = "pp_expr_rule"
                  then
                     return 208;
                     elsif
                     Name = "pp_term_rule"
                  then
                     return 209;
               end if;
      end case;

      return No_Enum_Value_Index;
   end Lookup_Enum_Value;

   -----------------
   -- Create_Enum --
   -----------------

   function Create_Enum
     (Kind : Enum_Value_Kind; Index : Enum_Value_Index) return Value_Type is
   begin
      case Kind is
            when Analysis_Unit_Kind_Value =>
               case Index is
                     when 1 =>
                        return Create_Analysis_Unit_Kind (Unit_Specification);
                     when 2 =>
                        return Create_Analysis_Unit_Kind (Unit_Body);

                  when others => null;
               end case;
            when Lookup_Kind_Value =>
               case Index is
                     when 1 =>
                        return Create_Lookup_Kind (Recursive);
                     when 2 =>
                        return Create_Lookup_Kind (Flat);
                     when 3 =>
                        return Create_Lookup_Kind (Minimal);

                  when others => null;
               end case;
            when Designated_Env_Kind_Value =>
               case Index is
                     when 1 =>
                        return Create_Designated_Env_Kind (None);
                     when 2 =>
                        return Create_Designated_Env_Kind (Current_Env);
                     when 3 =>
                        return Create_Designated_Env_Kind (Named_Env);
                     when 4 =>
                        return Create_Designated_Env_Kind (Direct_Env);

                  when others => null;
               end case;
            when Ref_Result_Kind_Value =>
               case Index is
                     when 1 =>
                        return Create_Ref_Result_Kind (No_Ref);
                     when 2 =>
                        return Create_Ref_Result_Kind (Precise);
                     when 3 =>
                        return Create_Ref_Result_Kind (Imprecise);
                     when 4 =>
                        return Create_Ref_Result_Kind (Error);

                  when others => null;
               end case;
            when Call_Expr_Kind_Value =>
               case Index is
                     when 1 =>
                        return Create_Call_Expr_Kind (Call);
                     when 2 =>
                        return Create_Call_Expr_Kind (Array_Slice);
                     when 3 =>
                        return Create_Call_Expr_Kind (Array_Index);
                     when 4 =>
                        return Create_Call_Expr_Kind (Type_Conversion);

                  when others => null;
               end case;
            when Grammar_Rule_Value =>
               case Index is
                     when 1 =>
                        return Create_Grammar_Rule (Parent_List_Rule);
                     when 2 =>
                        return Create_Grammar_Rule (Protected_Type_Decl_Rule);
                     when 3 =>
                        return Create_Grammar_Rule (Protected_Op_Rule);
                     when 4 =>
                        return Create_Grammar_Rule (Protected_El_Rule);
                     when 5 =>
                        return Create_Grammar_Rule (Protected_Def_Rule);
                     when 6 =>
                        return Create_Grammar_Rule (Protected_Decl_Rule);
                     when 7 =>
                        return Create_Grammar_Rule (Task_Item_Rule);
                     when 8 =>
                        return Create_Grammar_Rule (Task_Def_Rule);
                     when 9 =>
                        return Create_Grammar_Rule (Task_Type_Decl_Rule);
                     when 10 =>
                        return Create_Grammar_Rule (Subtype_Decl_Rule);
                     when 11 =>
                        return Create_Grammar_Rule (Interface_Type_Def_Rule);
                     when 12 =>
                        return Create_Grammar_Rule (Unconstrained_Index_Rule);
                     when 13 =>
                        return Create_Grammar_Rule (Array_Type_Def_Rule);
                     when 14 =>
                        return Create_Grammar_Rule (Discrete_Subtype_Definition_Rule);
                     when 15 =>
                        return Create_Grammar_Rule (Constraint_List_Rule);
                     when 16 =>
                        return Create_Grammar_Rule (Signed_Int_Type_Def_Rule);
                     when 17 =>
                        return Create_Grammar_Rule (Mod_Int_Type_Def_Rule);
                     when 18 =>
                        return Create_Grammar_Rule (Derived_Type_Def_Rule);
                     when 19 =>
                        return Create_Grammar_Rule (Composite_Constraint_Assoc_Rule);
                     when 20 =>
                        return Create_Grammar_Rule (Composite_Constraint_Rule);
                     when 21 =>
                        return Create_Grammar_Rule (Digits_Constraint_Rule);
                     when 22 =>
                        return Create_Grammar_Rule (Delta_Constraint_Rule);
                     when 23 =>
                        return Create_Grammar_Rule (Range_Constraint_Rule);
                     when 24 =>
                        return Create_Grammar_Rule (Constraint_Rule);
                     when 25 =>
                        return Create_Grammar_Rule (Discriminant_Spec_Rule);
                     when 26 =>
                        return Create_Grammar_Rule (Discr_Spec_List_Rule);
                     when 27 =>
                        return Create_Grammar_Rule (Discriminant_Part_Rule);
                     when 28 =>
                        return Create_Grammar_Rule (Enum_Literal_Decl_Rule);
                     when 29 =>
                        return Create_Grammar_Rule (Formal_Discrete_Type_Def_Rule);
                     when 30 =>
                        return Create_Grammar_Rule (Record_Def_Rule);
                     when 31 =>
                        return Create_Grammar_Rule (Range_Spec_Rule);
                     when 32 =>
                        return Create_Grammar_Rule (Real_Type_Def_Rule);
                     when 33 =>
                        return Create_Grammar_Rule (Sexpr_Or_Box_Rule);
                     when 34 =>
                        return Create_Grammar_Rule (Ordinary_Fixed_Point_Def_Rule);
                     when 35 =>
                        return Create_Grammar_Rule (Decimal_Fixed_Point_Def_Rule);
                     when 36 =>
                        return Create_Grammar_Rule (Floating_Point_Def_Rule);
                     when 37 =>
                        return Create_Grammar_Rule (Record_Type_Def_Rule);
                     when 38 =>
                        return Create_Grammar_Rule (Access_Def_Rule);
                     when 39 =>
                        return Create_Grammar_Rule (Enum_Type_Def_Rule);
                     when 40 =>
                        return Create_Grammar_Rule (Type_Def_Rule);
                     when 41 =>
                        return Create_Grammar_Rule (Variant_Rule);
                     when 42 =>
                        return Create_Grammar_Rule (Anonymous_Type_Decl_Rule);
                     when 43 =>
                        return Create_Grammar_Rule (Incomplete_Type_Decl_Rule);
                     when 44 =>
                        return Create_Grammar_Rule (Type_Decl_Rule);
                     when 45 =>
                        return Create_Grammar_Rule (Variant_Part_Rule);
                     when 46 =>
                        return Create_Grammar_Rule (Component_Def_Rule);
                     when 47 =>
                        return Create_Grammar_Rule (Component_Item_Rule);
                     when 48 =>
                        return Create_Grammar_Rule (Component_Decl_Rule);
                     when 49 =>
                        return Create_Grammar_Rule (Component_List_Rule);
                     when 50 =>
                        return Create_Grammar_Rule (Generic_Decl_Rule);
                     when 51 =>
                        return Create_Grammar_Rule (Generic_Formal_Part_Rule);
                     when 52 =>
                        return Create_Grammar_Rule (Generic_Formal_Decl_Rule);
                     when 53 =>
                        return Create_Grammar_Rule (Formal_Type_Decl_Rule);
                     when 54 =>
                        return Create_Grammar_Rule (Formal_Subp_Decl_Rule);
                     when 55 =>
                        return Create_Grammar_Rule (Renaming_Clause_Rule);
                     when 56 =>
                        return Create_Grammar_Rule (Generic_Renaming_Decl_Rule);
                     when 57 =>
                        return Create_Grammar_Rule (Generic_Instantiation_Rule);
                     when 58 =>
                        return Create_Grammar_Rule (Exception_Decl_Rule);
                     when 59 =>
                        return Create_Grammar_Rule (Basic_Decls_Rule);
                     when 60 =>
                        return Create_Grammar_Rule (Package_Renaming_Decl_Rule);
                     when 61 =>
                        return Create_Grammar_Rule (Package_Decl_Rule);
                     when 62 =>
                        return Create_Grammar_Rule (Basic_Decl_Rule);
                     when 63 =>
                        return Create_Grammar_Rule (Object_Decl_Rule);
                     when 64 =>
                        return Create_Grammar_Rule (Sub_Object_Decl_Rule);
                     when 65 =>
                        return Create_Grammar_Rule (No_Type_Object_Renaming_Decl_Rule);
                     when 66 =>
                        return Create_Grammar_Rule (Ext_Ret_Stmt_Object_Decl_Rule);
                     when 67 =>
                        return Create_Grammar_Rule (Defining_Id_List_Rule);
                     when 68 =>
                        return Create_Grammar_Rule (Number_Decl_Rule);
                     when 69 =>
                        return Create_Grammar_Rule (Contract_Case_Assoc_Rule);
                     when 70 =>
                        return Create_Grammar_Rule (Contract_Cases_Expr_Rule);
                     when 71 =>
                        return Create_Grammar_Rule (Abstract_State_Decl_Rule);
                     when 72 =>
                        return Create_Grammar_Rule (Multi_Abstract_State_Decl_Rule);
                     when 73 =>
                        return Create_Grammar_Rule (Aspect_Assoc_Rule);
                     when 74 =>
                        return Create_Grammar_Rule (Aspect_Spec_Rule);
                     when 75 =>
                        return Create_Grammar_Rule (Single_Task_Decl_Rule);
                     when 76 =>
                        return Create_Grammar_Rule (Overriding_Indicator_Rule);
                     when 77 =>
                        return Create_Grammar_Rule (Entry_Decl_Rule);
                     when 78 =>
                        return Create_Grammar_Rule (Component_Clause_Rule);
                     when 79 =>
                        return Create_Grammar_Rule (Aspect_Clause_Rule);
                     when 80 =>
                        return Create_Grammar_Rule (Param_Spec_Rule);
                     when 81 =>
                        return Create_Grammar_Rule (Param_Specs_Rule);
                     when 82 =>
                        return Create_Grammar_Rule (Subp_Spec_Rule);
                     when 83 =>
                        return Create_Grammar_Rule (Expr_Fn_Rule);
                     when 84 =>
                        return Create_Grammar_Rule (Null_Subp_Decl_Rule);
                     when 85 =>
                        return Create_Grammar_Rule (Abstract_Subp_Decl_Rule);
                     when 86 =>
                        return Create_Grammar_Rule (Subp_Renaming_Decl_Rule);
                     when 87 =>
                        return Create_Grammar_Rule (Simple_Subp_Decl_Rule);
                     when 88 =>
                        return Create_Grammar_Rule (Subp_Decl_Rule);
                     when 89 =>
                        return Create_Grammar_Rule (With_Clause_Rule);
                     when 90 =>
                        return Create_Grammar_Rule (Context_Item_Rule);
                     when 91 =>
                        return Create_Grammar_Rule (Use_Clause_Rule);
                     when 92 =>
                        return Create_Grammar_Rule (Use_Package_Clause_Rule);
                     when 93 =>
                        return Create_Grammar_Rule (Use_Type_Clause_Rule);
                     when 94 =>
                        return Create_Grammar_Rule (Subtype_Indication_Rule);
                     when 95 =>
                        return Create_Grammar_Rule (Discrete_Subtype_Indication_Rule);
                     when 96 =>
                        return Create_Grammar_Rule (Constrained_Subtype_Indication_Rule);
                     when 97 =>
                        return Create_Grammar_Rule (Type_Expr_Rule);
                     when 98 =>
                        return Create_Grammar_Rule (Anonymous_Type_Rule);
                     when 99 =>
                        return Create_Grammar_Rule (Mode_Rule);
                     when 100 =>
                        return Create_Grammar_Rule (Pragma_Argument_Rule);
                     when 101 =>
                        return Create_Grammar_Rule (Pragma_Rule);
                     when 102 =>
                        return Create_Grammar_Rule (Subunit_Rule);
                     when 103 =>
                        return Create_Grammar_Rule (Library_Unit_Body_Rule);
                     when 104 =>
                        return Create_Grammar_Rule (Library_Unit_Renaming_Decl_Rule);
                     when 105 =>
                        return Create_Grammar_Rule (Library_Item_Rule);
                     when 106 =>
                        return Create_Grammar_Rule (Compilation_Unit_Rule);
                     when 107 =>
                        return Create_Grammar_Rule (Compilation_Rule);
                     when 108 =>
                        return Create_Grammar_Rule (Decl_Part_Rule);
                     when 109 =>
                        return Create_Grammar_Rule (Entry_Body_Rule);
                     when 110 =>
                        return Create_Grammar_Rule (Protected_Body_Rule);
                     when 111 =>
                        return Create_Grammar_Rule (Protected_Body_Stub_Rule);
                     when 112 =>
                        return Create_Grammar_Rule (Task_Body_Rule);
                     when 113 =>
                        return Create_Grammar_Rule (Task_Body_Stub_Rule);
                     when 114 =>
                        return Create_Grammar_Rule (Package_Body_Stub_Rule);
                     when 115 =>
                        return Create_Grammar_Rule (Package_Body_Rule);
                     when 116 =>
                        return Create_Grammar_Rule (Terminate_Alternative_Rule);
                     when 117 =>
                        return Create_Grammar_Rule (Select_Stmt_Rule);
                     when 118 =>
                        return Create_Grammar_Rule (Accept_Stmt_Rule);
                     when 119 =>
                        return Create_Grammar_Rule (Case_Alt_Rule);
                     when 120 =>
                        return Create_Grammar_Rule (Case_Stmt_Rule);
                     when 121 =>
                        return Create_Grammar_Rule (Ext_Return_Stmt_Rule);
                     when 122 =>
                        return Create_Grammar_Rule (Iblock_Stmt_Rule);
                     when 123 =>
                        return Create_Grammar_Rule (Block_Stmt_Rule);
                     when 124 =>
                        return Create_Grammar_Rule (While_Loop_Spec_Rule);
                     when 125 =>
                        return Create_Grammar_Rule (Iloop_Stmt_Rule);
                     when 126 =>
                        return Create_Grammar_Rule (Loop_Stmt_Rule);
                     when 127 =>
                        return Create_Grammar_Rule (Compound_Stmt_Rule);
                     when 128 =>
                        return Create_Grammar_Rule (Elsif_Part_Rule);
                     when 129 =>
                        return Create_Grammar_Rule (If_Stmt_Rule);
                     when 130 =>
                        return Create_Grammar_Rule (Raise_Stmt_Rule);
                     when 131 =>
                        return Create_Grammar_Rule (Delay_Stmt_Rule);
                     when 132 =>
                        return Create_Grammar_Rule (Abort_Stmt_Rule);
                     when 133 =>
                        return Create_Grammar_Rule (Body_Rule);
                     when 134 =>
                        return Create_Grammar_Rule (Body_Stub_Rule);
                     when 135 =>
                        return Create_Grammar_Rule (Subp_Body_Stub_Rule);
                     when 136 =>
                        return Create_Grammar_Rule (Recov_Decl_Part_Rule);
                     when 137 =>
                        return Create_Grammar_Rule (Subp_Body_Rule);
                     when 138 =>
                        return Create_Grammar_Rule (Handled_Stmts_Rule);
                     when 139 =>
                        return Create_Grammar_Rule (Exception_Handler_Rule);
                     when 140 =>
                        return Create_Grammar_Rule (Stmts_Rule);
                     when 141 =>
                        return Create_Grammar_Rule (Label_Rule);
                     when 142 =>
                        return Create_Grammar_Rule (Stmt_Rule);
                     when 143 =>
                        return Create_Grammar_Rule (Call_Stmt_Rule);
                     when 144 =>
                        return Create_Grammar_Rule (Simple_Stmt_Rule);
                     when 145 =>
                        return Create_Grammar_Rule (Null_Stmt_Rule);
                     when 146 =>
                        return Create_Grammar_Rule (Assignment_Stmt_Rule);
                     when 147 =>
                        return Create_Grammar_Rule (Goto_Stmt_Rule);
                     when 148 =>
                        return Create_Grammar_Rule (Exit_Stmt_Rule);
                     when 149 =>
                        return Create_Grammar_Rule (Return_Stmt_Rule);
                     when 150 =>
                        return Create_Grammar_Rule (Requeue_Stmt_Rule);
                     when 151 =>
                        return Create_Grammar_Rule (Identifier_Rule);
                     when 152 =>
                        return Create_Grammar_Rule (Char_Literal_Rule);
                     when 153 =>
                        return Create_Grammar_Rule (String_Literal_Rule);
                     when 154 =>
                        return Create_Grammar_Rule (Defining_Id_Rule);
                     when 155 =>
                        return Create_Grammar_Rule (Dec_Literal_Rule);
                     when 156 =>
                        return Create_Grammar_Rule (Int_Literal_Rule);
                     when 157 =>
                        return Create_Grammar_Rule (Num_Literal_Rule);
                     when 158 =>
                        return Create_Grammar_Rule (Null_Literal_Rule);
                     when 159 =>
                        return Create_Grammar_Rule (Allocator_Rule);
                     when 160 =>
                        return Create_Grammar_Rule (For_Loop_Param_Spec_Rule);
                     when 161 =>
                        return Create_Grammar_Rule (Quantified_Expr_Rule);
                     when 162 =>
                        return Create_Grammar_Rule (Case_Expr_Rule);
                     when 163 =>
                        return Create_Grammar_Rule (Case_Expr_Alt_Rule);
                     when 164 =>
                        return Create_Grammar_Rule (Raise_Expr_Rule);
                     when 165 =>
                        return Create_Grammar_Rule (If_Expr_Rule);
                     when 166 =>
                        return Create_Grammar_Rule (Conditional_Expr_Rule);
                     when 167 =>
                        return Create_Grammar_Rule (Box_Expr_Rule);
                     when 168 =>
                        return Create_Grammar_Rule (Others_Designator_Rule);
                     when 169 =>
                        return Create_Grammar_Rule (Iterated_Assoc_Rule);
                     when 170 =>
                        return Create_Grammar_Rule (Aggregate_Assoc_Rule);
                     when 171 =>
                        return Create_Grammar_Rule (Regular_Aggregate_Rule);
                     when 172 =>
                        return Create_Grammar_Rule (Bracket_Aggregate_Rule);
                     when 173 =>
                        return Create_Grammar_Rule (Aggregate_Rule);
                     when 174 =>
                        return Create_Grammar_Rule (Direct_Name_Rule);
                     when 175 =>
                        return Create_Grammar_Rule (Param_Assoc_Rule);
                     when 176 =>
                        return Create_Grammar_Rule (Call_Suffix_Rule);
                     when 177 =>
                        return Create_Grammar_Rule (Attr_Suffix_Rule);
                     when 178 =>
                        return Create_Grammar_Rule (Qualified_Name_Rule);
                     when 179 =>
                        return Create_Grammar_Rule (Qual_Name_Internal_Rule);
                     when 180 =>
                        return Create_Grammar_Rule (Value_Sequence_Rule);
                     when 181 =>
                        return Create_Grammar_Rule (Name_Rule);
                     when 182 =>
                        return Create_Grammar_Rule (Defining_Name_Rule);
                     when 183 =>
                        return Create_Grammar_Rule (Direct_Name_Or_Target_Name_Rule);
                     when 184 =>
                        return Create_Grammar_Rule (Target_Name_Rule);
                     when 185 =>
                        return Create_Grammar_Rule (Update_Attr_Aggregate_Rule);
                     when 186 =>
                        return Create_Grammar_Rule (Update_Attr_Content_Rule);
                     when 187 =>
                        return Create_Grammar_Rule (Multidim_Array_Assoc_Rule);
                     when 188 =>
                        return Create_Grammar_Rule (Subtype_Name_Rule);
                     when 189 =>
                        return Create_Grammar_Rule (Static_Name_Rule);
                     when 190 =>
                        return Create_Grammar_Rule (Primary_Rule);
                     when 191 =>
                        return Create_Grammar_Rule (Paren_Expr_Rule);
                     when 192 =>
                        return Create_Grammar_Rule (Declare_Expr_Rule);
                     when 193 =>
                        return Create_Grammar_Rule (Factor_Rule);
                     when 194 =>
                        return Create_Grammar_Rule (Term_Rule);
                     when 195 =>
                        return Create_Grammar_Rule (Unop_Term_Rule);
                     when 196 =>
                        return Create_Grammar_Rule (Simple_Expr_Rule);
                     when 197 =>
                        return Create_Grammar_Rule (Boolean_Op_Rule);
                     when 198 =>
                        return Create_Grammar_Rule (Discrete_Range_Rule);
                     when 199 =>
                        return Create_Grammar_Rule (Choice_Rule);
                     when 200 =>
                        return Create_Grammar_Rule (Choice_List_Rule);
                     when 201 =>
                        return Create_Grammar_Rule (Rel_Op_Rule);
                     when 202 =>
                        return Create_Grammar_Rule (Membership_Choice_Rule);
                     when 203 =>
                        return Create_Grammar_Rule (Membership_Choice_List_Rule);
                     when 204 =>
                        return Create_Grammar_Rule (Relation_Rule);
                     when 205 =>
                        return Create_Grammar_Rule (Expr_Rule);
                     when 206 =>
                        return Create_Grammar_Rule (Pp_Directive_Rule);
                     when 207 =>
                        return Create_Grammar_Rule (Pp_Then_Rule);
                     when 208 =>
                        return Create_Grammar_Rule (Pp_Expr_Rule);
                     when 209 =>
                        return Create_Grammar_Rule (Pp_Term_Rule);

                  when others => null;
               end case;
      end case;

      return (raise Out_Of_Bounds_Error with "out of bounds enum value index");
   end Create_Enum;

   ----------------
   -- Enum_Index --
   ----------------

   function Enum_Index (Value : Value_Type) return Enum_Value_Index is
   begin
      case Kind (Value) is
            when Analysis_Unit_Kind_Value =>
               return Analysis_Unit_Kind'Pos (As_Analysis_Unit_Kind (Value)) + 1;
            when Lookup_Kind_Value =>
               return Lookup_Kind'Pos (As_Lookup_Kind (Value)) + 1;
            when Designated_Env_Kind_Value =>
               return Designated_Env_Kind'Pos (As_Designated_Env_Kind (Value)) + 1;
            when Ref_Result_Kind_Value =>
               return Ref_Result_Kind'Pos (As_Ref_Result_Kind (Value)) + 1;
            when Call_Expr_Kind_Value =>
               return Call_Expr_Kind'Pos (As_Call_Expr_Kind (Value)) + 1;
            when Grammar_Rule_Value =>
               return Grammar_Rule'Pos (As_Grammar_Rule (Value)) + 1;

         when others =>
            return (raise Bad_Type_Error with "not an enum value");
      end case;
   end Enum_Index;

   

   ------------------------------
   -- Array_Element_Constraint --
   ------------------------------

   function Array_Element_Constraint
     (Kind : Array_Value_Kind) return Type_Constraint is
   begin
      case Kind is
            
            when Discriminant_Values_Array_Value =>
                  return (Kind => Discriminant_Values_Value);
            
            when Doc_Annotation_Array_Value =>
                  return (Kind => Doc_Annotation_Value);
            
            when Accept_Stmt_Array_Value =>
                  
                  return (Kind      => Node_Value,
                          Node_Type => Common.Accept_Stmt_Type_Id);
            
            when Ada_Node_Array_Value =>
                  
                  return (Kind      => Node_Value,
                          Node_Type => Common.Ada_Node_Type_Id);
            
            when Base_Formal_Param_Decl_Array_Value =>
                  
                  return (Kind      => Node_Value,
                          Node_Type => Common.Base_Formal_Param_Decl_Type_Id);
            
            when Base_Type_Decl_Array_Value =>
                  
                  return (Kind      => Node_Value,
                          Node_Type => Common.Base_Type_Decl_Type_Id);
            
            when Basic_Decl_Array_Value =>
                  
                  return (Kind      => Node_Value,
                          Node_Type => Common.Basic_Decl_Type_Id);
            
            when Compilation_Unit_Array_Value =>
                  
                  return (Kind      => Node_Value,
                          Node_Type => Common.Compilation_Unit_Type_Id);
            
            when Defining_Name_Array_Value =>
                  
                  return (Kind      => Node_Value,
                          Node_Type => Common.Defining_Name_Type_Id);
            
            when Expr_Array_Value =>
                  
                  return (Kind      => Node_Value,
                          Node_Type => Common.Expr_Type_Id);
            
            when Generic_Instantiation_Array_Value =>
                  
                  return (Kind      => Node_Value,
                          Node_Type => Common.Generic_Instantiation_Type_Id);
            
            when Param_Spec_Array_Value =>
                  
                  return (Kind      => Node_Value,
                          Node_Type => Common.Param_Spec_Type_Id);
            
            when Pragma_Node_Array_Value =>
                  
                  return (Kind      => Node_Value,
                          Node_Type => Common.Pragma_Node_Type_Id);
            
            when Type_Decl_Array_Value =>
                  
                  return (Kind      => Node_Value,
                          Node_Type => Common.Type_Decl_Type_Id);
            
            when Param_Actual_Array_Value =>
                  return (Kind => Param_Actual_Value);
            
            when Ref_Result_Array_Value =>
                  return (Kind => Ref_Result_Value);
            
            when Shape_Array_Value =>
                  return (Kind => Shape_Value);
            
            when Substitution_Array_Value =>
                  return (Kind => Substitution_Value);
            
            when Analysis_Unit_Array_Value =>
                  return (Kind => Analysis_Unit_Value);
            
            when Unbounded_Text_Type_Array_Value =>
                  return (Kind => Unbounded_Text_Value);
      end case;
   end Array_Element_Constraint;

   ------------------
   -- Array_Length --
   ------------------

   function Array_Length (Self : Value_Type) return Natural is
   begin
      case Kind (Self) is
            when Discriminant_Values_Array_Value =>
               return Self.Value.Value.Discriminant_Values_Array_Value.all'Length;
            when Doc_Annotation_Array_Value =>
               return Self.Value.Value.Doc_Annotation_Array_Value.all'Length;
            when Accept_Stmt_Array_Value =>
               return Self.Value.Value.Accept_Stmt_Array_Value.all'Length;
            when Ada_Node_Array_Value =>
               return Self.Value.Value.Ada_Node_Array_Value.all'Length;
            when Base_Formal_Param_Decl_Array_Value =>
               return Self.Value.Value.Base_Formal_Param_Decl_Array_Value.all'Length;
            when Base_Type_Decl_Array_Value =>
               return Self.Value.Value.Base_Type_Decl_Array_Value.all'Length;
            when Basic_Decl_Array_Value =>
               return Self.Value.Value.Basic_Decl_Array_Value.all'Length;
            when Compilation_Unit_Array_Value =>
               return Self.Value.Value.Compilation_Unit_Array_Value.all'Length;
            when Defining_Name_Array_Value =>
               return Self.Value.Value.Defining_Name_Array_Value.all'Length;
            when Expr_Array_Value =>
               return Self.Value.Value.Expr_Array_Value.all'Length;
            when Generic_Instantiation_Array_Value =>
               return Self.Value.Value.Generic_Instantiation_Array_Value.all'Length;
            when Param_Spec_Array_Value =>
               return Self.Value.Value.Param_Spec_Array_Value.all'Length;
            when Pragma_Node_Array_Value =>
               return Self.Value.Value.Pragma_Node_Array_Value.all'Length;
            when Type_Decl_Array_Value =>
               return Self.Value.Value.Type_Decl_Array_Value.all'Length;
            when Param_Actual_Array_Value =>
               return Self.Value.Value.Param_Actual_Array_Value.all'Length;
            when Ref_Result_Array_Value =>
               return Self.Value.Value.Ref_Result_Array_Value.all'Length;
            when Shape_Array_Value =>
               return Self.Value.Value.Shape_Array_Value.all'Length;
            when Substitution_Array_Value =>
               return Self.Value.Value.Substitution_Array_Value.all'Length;
            when Analysis_Unit_Array_Value =>
               return Self.Value.Value.Analysis_Unit_Array_Value.all'Length;
            when Unbounded_Text_Type_Array_Value =>
               return Self.Value.Value.Unbounded_Text_Type_Array_Value.all'Length;

         when others =>
            return (raise Bad_Type_Error with "input value is not an array");
      end case;
   end Array_Length;

   -------------------
   -- Array_Element --
   -------------------

   function Array_Element
     (Self : Value_Type; Index : Positive) return Value_Type is
   begin
      case Kind (Self) is
            when Discriminant_Values_Array_Value =>
               declare
                  A : Discriminant_Values_Array renames
                     Self.Value.Value.Discriminant_Values_Array_Value.all;
               begin
                  if Index not in A'Range then
                     raise Out_Of_Bounds_Error with "index of array bounds";
                  end if;
                  return Create_Discriminant_Values
                    (A (Index));
               end;
            when Doc_Annotation_Array_Value =>
               declare
                  A : Doc_Annotation_Array renames
                     Self.Value.Value.Doc_Annotation_Array_Value.all;
               begin
                  if Index not in A'Range then
                     raise Out_Of_Bounds_Error with "index of array bounds";
                  end if;
                  return Create_Doc_Annotation
                    (A (Index));
               end;
            when Accept_Stmt_Array_Value =>
               declare
                  A : Accept_Stmt_Array renames
                     Self.Value.Value.Accept_Stmt_Array_Value.all;
               begin
                  if Index not in A'Range then
                     raise Out_Of_Bounds_Error with "index of array bounds";
                  end if;
                  return Create_Node
                    (A (Index));
               end;
            when Ada_Node_Array_Value =>
               declare
                  A : Ada_Node_Array renames
                     Self.Value.Value.Ada_Node_Array_Value.all;
               begin
                  if Index not in A'Range then
                     raise Out_Of_Bounds_Error with "index of array bounds";
                  end if;
                  return Create_Node
                    (A (Index));
               end;
            when Base_Formal_Param_Decl_Array_Value =>
               declare
                  A : Base_Formal_Param_Decl_Array renames
                     Self.Value.Value.Base_Formal_Param_Decl_Array_Value.all;
               begin
                  if Index not in A'Range then
                     raise Out_Of_Bounds_Error with "index of array bounds";
                  end if;
                  return Create_Node
                    (A (Index));
               end;
            when Base_Type_Decl_Array_Value =>
               declare
                  A : Base_Type_Decl_Array renames
                     Self.Value.Value.Base_Type_Decl_Array_Value.all;
               begin
                  if Index not in A'Range then
                     raise Out_Of_Bounds_Error with "index of array bounds";
                  end if;
                  return Create_Node
                    (A (Index));
               end;
            when Basic_Decl_Array_Value =>
               declare
                  A : Basic_Decl_Array renames
                     Self.Value.Value.Basic_Decl_Array_Value.all;
               begin
                  if Index not in A'Range then
                     raise Out_Of_Bounds_Error with "index of array bounds";
                  end if;
                  return Create_Node
                    (A (Index));
               end;
            when Compilation_Unit_Array_Value =>
               declare
                  A : Compilation_Unit_Array renames
                     Self.Value.Value.Compilation_Unit_Array_Value.all;
               begin
                  if Index not in A'Range then
                     raise Out_Of_Bounds_Error with "index of array bounds";
                  end if;
                  return Create_Node
                    (A (Index));
               end;
            when Defining_Name_Array_Value =>
               declare
                  A : Defining_Name_Array renames
                     Self.Value.Value.Defining_Name_Array_Value.all;
               begin
                  if Index not in A'Range then
                     raise Out_Of_Bounds_Error with "index of array bounds";
                  end if;
                  return Create_Node
                    (A (Index));
               end;
            when Expr_Array_Value =>
               declare
                  A : Expr_Array renames
                     Self.Value.Value.Expr_Array_Value.all;
               begin
                  if Index not in A'Range then
                     raise Out_Of_Bounds_Error with "index of array bounds";
                  end if;
                  return Create_Node
                    (A (Index));
               end;
            when Generic_Instantiation_Array_Value =>
               declare
                  A : Generic_Instantiation_Array renames
                     Self.Value.Value.Generic_Instantiation_Array_Value.all;
               begin
                  if Index not in A'Range then
                     raise Out_Of_Bounds_Error with "index of array bounds";
                  end if;
                  return Create_Node
                    (A (Index));
               end;
            when Param_Spec_Array_Value =>
               declare
                  A : Param_Spec_Array renames
                     Self.Value.Value.Param_Spec_Array_Value.all;
               begin
                  if Index not in A'Range then
                     raise Out_Of_Bounds_Error with "index of array bounds";
                  end if;
                  return Create_Node
                    (A (Index));
               end;
            when Pragma_Node_Array_Value =>
               declare
                  A : Pragma_Node_Array renames
                     Self.Value.Value.Pragma_Node_Array_Value.all;
               begin
                  if Index not in A'Range then
                     raise Out_Of_Bounds_Error with "index of array bounds";
                  end if;
                  return Create_Node
                    (A (Index));
               end;
            when Type_Decl_Array_Value =>
               declare
                  A : Type_Decl_Array renames
                     Self.Value.Value.Type_Decl_Array_Value.all;
               begin
                  if Index not in A'Range then
                     raise Out_Of_Bounds_Error with "index of array bounds";
                  end if;
                  return Create_Node
                    (A (Index));
               end;
            when Param_Actual_Array_Value =>
               declare
                  A : Param_Actual_Array renames
                     Self.Value.Value.Param_Actual_Array_Value.all;
               begin
                  if Index not in A'Range then
                     raise Out_Of_Bounds_Error with "index of array bounds";
                  end if;
                  return Create_Param_Actual
                    (A (Index));
               end;
            when Ref_Result_Array_Value =>
               declare
                  A : Ref_Result_Array renames
                     Self.Value.Value.Ref_Result_Array_Value.all;
               begin
                  if Index not in A'Range then
                     raise Out_Of_Bounds_Error with "index of array bounds";
                  end if;
                  return Create_Ref_Result
                    (A (Index));
               end;
            when Shape_Array_Value =>
               declare
                  A : Shape_Array renames
                     Self.Value.Value.Shape_Array_Value.all;
               begin
                  if Index not in A'Range then
                     raise Out_Of_Bounds_Error with "index of array bounds";
                  end if;
                  return Create_Shape
                    (A (Index));
               end;
            when Substitution_Array_Value =>
               declare
                  A : Substitution_Array renames
                     Self.Value.Value.Substitution_Array_Value.all;
               begin
                  if Index not in A'Range then
                     raise Out_Of_Bounds_Error with "index of array bounds";
                  end if;
                  return Create_Substitution
                    (A (Index));
               end;
            when Analysis_Unit_Array_Value =>
               declare
                  A : Analysis_Unit_Array renames
                     Self.Value.Value.Analysis_Unit_Array_Value.all;
               begin
                  if Index not in A'Range then
                     raise Out_Of_Bounds_Error with "index of array bounds";
                  end if;
                  return Create_Analysis_Unit
                    (A (Index));
               end;
            when Unbounded_Text_Type_Array_Value =>
               declare
                  A : Unbounded_Text_Type_Array renames
                     Self.Value.Value.Unbounded_Text_Type_Array_Value.all;
               begin
                  if Index not in A'Range then
                     raise Out_Of_Bounds_Error with "index of array bounds";
                  end if;
                  return Create_Unbounded_Text
                    (A (Index));
               end;

         when others =>
            return (raise Bad_Type_Error with "input value is not an array");
      end case;
   end Array_Element;

   ------------------
   -- Create_Array --
   ------------------

   function Create_Array
     (Kind : Array_Value_Kind; Values : Value_Array) return Value_Type
   is
      Elt_Cons : constant Type_Constraint := Array_Element_Constraint (Kind);
   begin
      --  First check that all input values have the expected type

      for I in Values'Range loop
         if not Satisfies (Values (I), Elt_Cons) then
            raise Bad_Type_Error with "invalid value at index " & I'Image;
         end if;
      end loop;

      --  Then create the array to return

      case Kind is
            
            when Discriminant_Values_Array_Value =>
               declare
                  A : Discriminant_Values_Array (1 .. Values'Length);
               begin
                  for I in Values'Range loop
                        A (I) := As_Discriminant_Values (Values (I));
                  end loop;
                  return Create_Discriminant_Values_Array (A);
               end;
            
            when Doc_Annotation_Array_Value =>
               declare
                  A : Doc_Annotation_Array (1 .. Values'Length);
               begin
                  for I in Values'Range loop
                        A (I) := As_Doc_Annotation (Values (I));
                  end loop;
                  return Create_Doc_Annotation_Array (A);
               end;
            
            when Accept_Stmt_Array_Value =>
               declare
                  A : Accept_Stmt_Array (1 .. Values'Length);
               begin
                  for I in Values'Range loop
                        
                        A (I) :=
                           As_Node (Values (I))
                              .As_Accept_Stmt
                        ;

                  end loop;
                  return Create_Accept_Stmt_Array (A);
               end;
            
            when Ada_Node_Array_Value =>
               declare
                  A : Ada_Node_Array (1 .. Values'Length);
               begin
                  for I in Values'Range loop
                        
                        A (I) :=
                           As_Node (Values (I))
                        ;

                  end loop;
                  return Create_Ada_Node_Array (A);
               end;
            
            when Base_Formal_Param_Decl_Array_Value =>
               declare
                  A : Base_Formal_Param_Decl_Array (1 .. Values'Length);
               begin
                  for I in Values'Range loop
                        
                        A (I) :=
                           As_Node (Values (I))
                              .As_Base_Formal_Param_Decl
                        ;

                  end loop;
                  return Create_Base_Formal_Param_Decl_Array (A);
               end;
            
            when Base_Type_Decl_Array_Value =>
               declare
                  A : Base_Type_Decl_Array (1 .. Values'Length);
               begin
                  for I in Values'Range loop
                        
                        A (I) :=
                           As_Node (Values (I))
                              .As_Base_Type_Decl
                        ;

                  end loop;
                  return Create_Base_Type_Decl_Array (A);
               end;
            
            when Basic_Decl_Array_Value =>
               declare
                  A : Basic_Decl_Array (1 .. Values'Length);
               begin
                  for I in Values'Range loop
                        
                        A (I) :=
                           As_Node (Values (I))
                              .As_Basic_Decl
                        ;

                  end loop;
                  return Create_Basic_Decl_Array (A);
               end;
            
            when Compilation_Unit_Array_Value =>
               declare
                  A : Compilation_Unit_Array (1 .. Values'Length);
               begin
                  for I in Values'Range loop
                        
                        A (I) :=
                           As_Node (Values (I))
                              .As_Compilation_Unit
                        ;

                  end loop;
                  return Create_Compilation_Unit_Array (A);
               end;
            
            when Defining_Name_Array_Value =>
               declare
                  A : Defining_Name_Array (1 .. Values'Length);
               begin
                  for I in Values'Range loop
                        
                        A (I) :=
                           As_Node (Values (I))
                              .As_Defining_Name
                        ;

                  end loop;
                  return Create_Defining_Name_Array (A);
               end;
            
            when Expr_Array_Value =>
               declare
                  A : Expr_Array (1 .. Values'Length);
               begin
                  for I in Values'Range loop
                        
                        A (I) :=
                           As_Node (Values (I))
                              .As_Expr
                        ;

                  end loop;
                  return Create_Expr_Array (A);
               end;
            
            when Generic_Instantiation_Array_Value =>
               declare
                  A : Generic_Instantiation_Array (1 .. Values'Length);
               begin
                  for I in Values'Range loop
                        
                        A (I) :=
                           As_Node (Values (I))
                              .As_Generic_Instantiation
                        ;

                  end loop;
                  return Create_Generic_Instantiation_Array (A);
               end;
            
            when Param_Spec_Array_Value =>
               declare
                  A : Param_Spec_Array (1 .. Values'Length);
               begin
                  for I in Values'Range loop
                        
                        A (I) :=
                           As_Node (Values (I))
                              .As_Param_Spec
                        ;

                  end loop;
                  return Create_Param_Spec_Array (A);
               end;
            
            when Pragma_Node_Array_Value =>
               declare
                  A : Pragma_Node_Array (1 .. Values'Length);
               begin
                  for I in Values'Range loop
                        
                        A (I) :=
                           As_Node (Values (I))
                              .As_Pragma_Node
                        ;

                  end loop;
                  return Create_Pragma_Node_Array (A);
               end;
            
            when Type_Decl_Array_Value =>
               declare
                  A : Type_Decl_Array (1 .. Values'Length);
               begin
                  for I in Values'Range loop
                        
                        A (I) :=
                           As_Node (Values (I))
                              .As_Type_Decl
                        ;

                  end loop;
                  return Create_Type_Decl_Array (A);
               end;
            
            when Param_Actual_Array_Value =>
               declare
                  A : Param_Actual_Array (1 .. Values'Length);
               begin
                  for I in Values'Range loop
                        A (I) := As_Param_Actual (Values (I));
                  end loop;
                  return Create_Param_Actual_Array (A);
               end;
            
            when Ref_Result_Array_Value =>
               declare
                  A : Ref_Result_Array (1 .. Values'Length);
               begin
                  for I in Values'Range loop
                        A (I) := As_Ref_Result (Values (I));
                  end loop;
                  return Create_Ref_Result_Array (A);
               end;
            
            when Shape_Array_Value =>
               declare
                  A : Shape_Array (1 .. Values'Length);
               begin
                  for I in Values'Range loop
                        A (I) := As_Shape (Values (I));
                  end loop;
                  return Create_Shape_Array (A);
               end;
            
            when Substitution_Array_Value =>
               declare
                  A : Substitution_Array (1 .. Values'Length);
               begin
                  for I in Values'Range loop
                        A (I) := As_Substitution (Values (I));
                  end loop;
                  return Create_Substitution_Array (A);
               end;
            
            when Analysis_Unit_Array_Value =>
               declare
                  A : Analysis_Unit_Array (1 .. Values'Length);
               begin
                  for I in Values'Range loop
                        A (I) := As_Analysis_Unit (Values (I));
                  end loop;
                  return Create_Analysis_Unit_Array (A);
               end;
            
            when Unbounded_Text_Type_Array_Value =>
               declare
                  A : Unbounded_Text_Type_Array (1 .. Values'Length);
               begin
                  for I in Values'Range loop
                        A (I) := As_Unbounded_Text (Values (I));
                  end loop;
                  return Create_Unbounded_Text_Type_Array (A);
               end;
      end case;
   end Create_Array;

   -------------------
   -- Struct_Fields --
   -------------------

   function Struct_Fields
     (Kind : Struct_Value_Kind) return Struct_Field_Reference_Array is
   begin
      pragma Warnings (Off, "value not in range of type");
      return Impl.Struct_Fields (Kind);
      pragma Warnings (On, "value not in range of type");
   end Struct_Fields;

   -------------------
   -- Create_Struct --
   -------------------

   pragma Warnings (Off, "referenced");
   function Create_Struct
     (Kind : Struct_Value_Kind; Values : Value_Array) return Value_Type
   is
      pragma Warnings (On, "referenced");
   begin
         --  First check that input values have the expected format
         declare
            Fields : constant Struct_Field_Reference_Array :=
               Struct_Fields (Kind);
         begin
            if Fields'Length /= Values'Length then
               raise Bad_Type_Error with "unexpected number of values";
            end if;

            for I in Fields'Range loop
               declare
                  F : constant Struct_Field_Reference := Fields (I);
                  T : constant Type_Constraint := Member_Type (F);
                  V : Value_Type renames Values (I + Values'First - 1);
               begin
                  if not Satisfies (V, T) then
                     raise Bad_Type_Error with
                        "type mismatch for " & Image (Member_Name (F));
                  end if;
               end;
            end loop;
         end;

         --  Only then, use input values to create the struct
         case Kind is
            when Aspect_Value =>
               
               declare
                     
                     F_Exists : constant Boolean := As_Boolean (Values (Values'First + 0));
                     
                     F_Node : constant Ada_Node := As_Node (Values (Values'First + 1));
                     
                     F_Value : constant Expr := As_Node (Values (Values'First + 2)).As_Expr;
                  Result : constant Aspect :=
                     Analysis.Create_Aspect
                       (F_Exists, F_Node, F_Value);
               begin
                  return Introspection.Create_Aspect (Result);
               end;
            when Completion_Item_Value =>
               
               declare
                     
                     F_Decl : constant Basic_Decl := As_Node (Values (Values'First + 0)).As_Basic_Decl;
                     
                     F_Is_Dot_Call : constant Boolean := As_Boolean (Values (Values'First + 1));
                     
                     F_Is_Visible : constant Boolean := As_Boolean (Values (Values'First + 2));
                     
                     F_Weight : constant Integer := As_Integer (Values (Values'First + 3));
                  Result : constant Completion_Item :=
                     Analysis.Create_Completion_Item
                       (F_Decl, F_Is_Dot_Call, F_Is_Visible, F_Weight);
               begin
                  return Introspection.Create_Completion_Item (Result);
               end;
            when Discrete_Range_Value =>
               
               declare
                     
                     F_Low_Bound : constant Expr := As_Node (Values (Values'First + 0)).As_Expr;
                     
                     F_High_Bound : constant Expr := As_Node (Values (Values'First + 1)).As_Expr;
                  Result : constant Discrete_Range :=
                     Analysis.Create_Discrete_Range
                       (F_Low_Bound, F_High_Bound);
               begin
                  return Introspection.Create_Discrete_Range (Result);
               end;
            when Discriminant_Values_Value =>
               
               declare
                     
                     F_Discriminant : constant Identifier := As_Node (Values (Values'First + 0)).As_Identifier;
                     
                     F_Values : constant Alternatives_List := As_Node (Values (Values'First + 1)).As_Alternatives_List;
                  Result : constant Discriminant_Values :=
                     Analysis.Create_Discriminant_Values
                       (F_Discriminant, F_Values);
               begin
                  return Introspection.Create_Discriminant_Values (Result);
               end;
            when Doc_Annotation_Value =>
               
               declare
                     
                     F_Key : constant Text_Type := As_String (Values (Values'First + 0));
                     
                     F_Value : constant Text_Type := As_String (Values (Values'First + 1));
                  Result : constant Doc_Annotation :=
                     Analysis.Create_Doc_Annotation
                       (F_Key, F_Value);
               begin
                  return Introspection.Create_Doc_Annotation (Result);
               end;
            when Param_Actual_Value =>
               
               declare
                     
                     F_Param : constant Defining_Name := As_Node (Values (Values'First + 0)).As_Defining_Name;
                     
                     F_Actual : constant Expr := As_Node (Values (Values'First + 1)).As_Expr;
                  Result : constant Param_Actual :=
                     Analysis.Create_Param_Actual
                       (F_Param, F_Actual);
               begin
                  return Introspection.Create_Param_Actual (Result);
               end;
            when Ref_Result_Value =>
               
               declare
                     
                     F_Ref : constant Base_Id := As_Node (Values (Values'First + 0)).As_Base_Id;
                     
                     F_Kind : constant Ref_Result_Kind := As_Ref_Result_Kind (Values (Values'First + 1));
                  Result : constant Ref_Result :=
                     Analysis.Create_Ref_Result
                       (F_Ref, F_Kind);
               begin
                  return Introspection.Create_Ref_Result (Result);
               end;
            when Refd_Decl_Value =>
               
               declare
                     
                     F_Decl : constant Basic_Decl := As_Node (Values (Values'First + 0)).As_Basic_Decl;
                     
                     F_Kind : constant Ref_Result_Kind := As_Ref_Result_Kind (Values (Values'First + 1));
                  Result : constant Refd_Decl :=
                     Analysis.Create_Refd_Decl
                       (F_Decl, F_Kind);
               begin
                  return Introspection.Create_Refd_Decl (Result);
               end;
            when Refd_Def_Value =>
               
               declare
                     
                     F_Def_Name : constant Defining_Name := As_Node (Values (Values'First + 0)).As_Defining_Name;
                     
                     F_Kind : constant Ref_Result_Kind := As_Ref_Result_Kind (Values (Values'First + 1));
                  Result : constant Refd_Def :=
                     Analysis.Create_Refd_Def
                       (F_Def_Name, F_Kind);
               begin
                  return Introspection.Create_Refd_Def (Result);
               end;
            when Shape_Value =>
               
               declare
                     
                     F_Components : constant Base_Formal_Param_Decl_Array := As_Base_Formal_Param_Decl_Array (Values (Values'First + 0));
                     
                     F_Discriminants_Values : constant Discriminant_Values_Array := As_Discriminant_Values_Array (Values (Values'First + 1));
                  Result : constant Shape :=
                     Analysis.Create_Shape
                       (F_Components, F_Discriminants_Values);
               begin
                  return Introspection.Create_Shape (Result);
               end;
            when Substitution_Value =>
               
               declare
                     
                     F_From_Decl : constant Basic_Decl := As_Node (Values (Values'First + 0)).As_Basic_Decl;
                     
                     F_To_Value : constant Big_Integer := As_Big_Integer (Values (Values'First + 1));
                     
                     F_Value_Type : constant Base_Type_Decl := As_Node (Values (Values'First + 2)).As_Base_Type_Decl;
                  Result : constant Substitution :=
                     Analysis.Create_Substitution
                       (F_From_Decl, F_To_Value, F_Value_Type);
               begin
                  return Introspection.Create_Substitution (Result);
               end;
         end case;

   end Create_Struct;

   -----------------
   -- Member_Name --
   -----------------

   function Member_Name (Member : Member_Reference) return Text_Type is
   begin
      return Impl.Member_Name (Member);
   end Member_Name;

   -----------------
   -- Member_Type --
   -----------------

   function Member_Type (Member : Member_Reference) return Type_Constraint is
   begin
      return Impl.Member_Type (Member);
   end Member_Type;

   function Eval_Member
     (Prefix    : Value_Type;
      Member    : Member_Reference;
      Arguments : Value_Array) return Value_Type
   is
      Prefix_Val : Value_Record renames Prefix.Value.Value.all;
   begin
      case Prefix_Val.Kind is
      when Struct_Value_Kind =>
         if Member not in Struct_Field_Reference then
            return (raise Bad_Type_Error with "no such member");
         elsif Arguments'Length /= 0 then
            return (raise Bad_Type_Error
                    with "struct fields take no argument");
         else
            pragma Warnings (Off, "value not in range of type");
            return Eval_Member (Prefix, Member);
            pragma Warnings (On, "value not in range of type");
         end if;

      when Node_Value =>
         return Eval_Member (Prefix_Val.Node_Value, Member, Arguments);

      when others =>
         return (raise Bad_Type_Error with "invalid prefix type");
      end case;
   end Eval_Member;

   -----------------
   -- Eval_Member --
   -----------------

   pragma Warnings (Off, "referenced");
   function Eval_Member
     (Prefix : Value_Type; Field : Struct_Field_Reference) return Value_Type
   is
      pragma Warnings (On, "referenced");
      Prefix_Val : Value_Record renames Prefix.Value.Value.all;
   begin
      case Prefix_Val.Kind is

         when Aspect_Value =>
            case Field is
               when Aspect_F_Exists =>
                  declare
                     
                  begin
                     return Create_Boolean (Analysis.Exists (Prefix_Val.Aspect_Value));
                  end;
               when Aspect_F_Node =>
                  declare
                     
                  begin
                     return Create_Node (Analysis.Node (Prefix_Val.Aspect_Value));
                  end;
               when Aspect_F_Value =>
                  declare
                     
                  begin
                     return Create_Node (Analysis.Value (Prefix_Val.Aspect_Value));
                  end;

            when others => null;
            end case;
         when Completion_Item_Value =>
            case Field is
               when Completion_Item_F_Decl =>
                  declare
                     
                  begin
                     return Create_Node (Analysis.Decl (Prefix_Val.Completion_Item_Value));
                  end;
               when Completion_Item_F_Is_Dot_Call =>
                  declare
                     
                  begin
                     return Create_Boolean (Analysis.Is_Dot_Call (Prefix_Val.Completion_Item_Value));
                  end;
               when Completion_Item_F_Is_Visible =>
                  declare
                     
                  begin
                     return Create_Boolean (Analysis.Is_Visible (Prefix_Val.Completion_Item_Value));
                  end;
               when Completion_Item_F_Weight =>
                  declare
                     
                  begin
                     return Create_Integer (Analysis.Weight (Prefix_Val.Completion_Item_Value));
                  end;

            when others => null;
            end case;
         when Discrete_Range_Value =>
            case Field is
               when Discrete_Range_F_Low_Bound =>
                  declare
                     
                  begin
                     return Create_Node (Analysis.Low_Bound (Prefix_Val.Discrete_Range_Value));
                  end;
               when Discrete_Range_F_High_Bound =>
                  declare
                     
                  begin
                     return Create_Node (Analysis.High_Bound (Prefix_Val.Discrete_Range_Value));
                  end;

            when others => null;
            end case;
         when Discriminant_Values_Value =>
            case Field is
               when Discriminant_Values_F_Discriminant =>
                  declare
                     
                  begin
                     return Create_Node (Analysis.Discriminant (Prefix_Val.Discriminant_Values_Value));
                  end;
               when Discriminant_Values_F_Values =>
                  declare
                     
                  begin
                     return Create_Node (Analysis.Values (Prefix_Val.Discriminant_Values_Value));
                  end;

            when others => null;
            end case;
         when Doc_Annotation_Value =>
            case Field is
               when Doc_Annotation_F_Key =>
                  declare
                     
                  begin
                     return Create_String (Analysis.Key (Prefix_Val.Doc_Annotation_Value));
                  end;
               when Doc_Annotation_F_Value =>
                  declare
                     
                  begin
                     return Create_String (Analysis.Value (Prefix_Val.Doc_Annotation_Value));
                  end;

            when others => null;
            end case;
         when Param_Actual_Value =>
            case Field is
               when Param_Actual_F_Param =>
                  declare
                     
                  begin
                     return Create_Node (Analysis.Param (Prefix_Val.Param_Actual_Value));
                  end;
               when Param_Actual_F_Actual =>
                  declare
                     
                  begin
                     return Create_Node (Analysis.Actual (Prefix_Val.Param_Actual_Value));
                  end;

            when others => null;
            end case;
         when Ref_Result_Value =>
            case Field is
               when Ref_Result_F_Ref =>
                  declare
                     
                  begin
                     return Create_Node (Analysis.Ref (Prefix_Val.Ref_Result_Value));
                  end;
               when Ref_Result_F_Kind =>
                  declare
                     
                  begin
                     return Create_Ref_Result_Kind (Analysis.Kind (Prefix_Val.Ref_Result_Value));
                  end;

            when others => null;
            end case;
         when Refd_Decl_Value =>
            case Field is
               when Refd_Decl_F_Decl =>
                  declare
                     
                  begin
                     return Create_Node (Analysis.Decl (Prefix_Val.Refd_Decl_Value));
                  end;
               when Refd_Decl_F_Kind =>
                  declare
                     
                  begin
                     return Create_Ref_Result_Kind (Analysis.Kind (Prefix_Val.Refd_Decl_Value));
                  end;

            when others => null;
            end case;
         when Refd_Def_Value =>
            case Field is
               when Refd_Def_F_Def_Name =>
                  declare
                     
                  begin
                     return Create_Node (Analysis.Def_Name (Prefix_Val.Refd_Def_Value));
                  end;
               when Refd_Def_F_Kind =>
                  declare
                     
                  begin
                     return Create_Ref_Result_Kind (Analysis.Kind (Prefix_Val.Refd_Def_Value));
                  end;

            when others => null;
            end case;
         when Shape_Value =>
            case Field is
               when Shape_F_Components =>
                  declare
                     
                  begin
                     return Create_Base_Formal_Param_Decl_Array (Analysis.Components (Prefix_Val.Shape_Value));
                  end;
               when Shape_F_Discriminants_Values =>
                  declare
                     
                  begin
                     return Create_Discriminant_Values_Array (Analysis.Discriminants_Values (Prefix_Val.Shape_Value));
                  end;

            when others => null;
            end case;
         when Substitution_Value =>
            case Field is
               when Substitution_F_From_Decl =>
                  declare
                     
                  begin
                     return Create_Node (Analysis.From_Decl (Prefix_Val.Substitution_Value));
                  end;
               when Substitution_F_To_Value =>
                  declare
                     
                  begin
                     return Create_Big_Integer (Analysis.To_Value (Prefix_Val.Substitution_Value));
                  end;
               when Substitution_F_Value_Type =>
                  declare
                     
                  begin
                     return Create_Node (Analysis.Value_Type (Prefix_Val.Substitution_Value));
                  end;

            when others => null;
            end case;

      when others =>
         return (raise Program_Error);
      end case;

         return (raise Bad_Type_Error with "no such member");
   end Eval_Member;

   -----------------
   -- Eval_Member --
   -----------------

   function Eval_Member
     (Node      : Ada_Node'Class;
      Member    : Node_Member_Reference;
      Arguments : Value_Array) return Value_Type is
   begin
      case Member is
         when Syntax_Field_Reference =>
            if Arguments'Length > 0 then
               raise Bad_Type_Error with "fields take no argument";
            end if;
            pragma Warnings (Off, "value not in range of type");
            return Create_Node (Eval_Syntax_Field (Node, Member));
            pragma Warnings (On, "value not in range of type");

         when Property_Reference =>
            return Eval_Property (Node, Member, Arguments);
      end case;
   end Eval_Member;

   -------------------
   -- Lookup_Member --
   -------------------

   function Lookup_Member
     (Prefix : Value_Type;
      Name   : Text_Type) return Any_Member_Reference
   is
      Prefix_Val : Value_Record renames Prefix.Value.Value.all;
   begin
      case Prefix_Val.Kind is
      when Struct_Value_Kind =>
         pragma Warnings (Off, "value not in range of type");
         return Impl.Lookup_Member_Struct (Prefix_Val.Kind, Name);
         pragma Warnings (On, "value not in range of type");

      when Node_Value =>
         declare
            Node : constant Ada_Node := Prefix_Val.Node_Value;
         begin
            if Node.Is_Null then
               raise Bad_Type_Error with "invalid null prefix node";
            end if;
            return Impl.Lookup_Member_Node (Impl.Id_For_Kind (Node.Kind), Name);
         end;

      when others =>
         return (raise Bad_Type_Error with "invalid prefix type");
      end case;
   end Lookup_Member;

   function Lookup_Member
     (Id   : Node_Type_Id;
      Name : Text_Type) return Any_Member_Reference is
   begin
      return Impl.Lookup_Member_Node (Id, Name);
   end Lookup_Member;

   -----------------------
   -- Eval_Syntax_Field --
   -----------------------

   function Eval_Syntax_Field
     (Node  : Ada_Node'Class;
      Field : Syntax_Field_Reference) return Ada_Node
   is
      Ent : constant Internal_Entity := Unwrap_Entity (Node);

      pragma Warnings (Off, "value not in range of type");
      Result : constant Bare_Ada_Node :=
         Impl.Eval_Syntax_Field (Ent.Node, Field);
      pragma Warnings (On, "value not in range of type");
   begin
      return Wrap_Node (Result, Ent.Info);
   end Eval_Syntax_Field;

   -----------
   -- Index --
   -----------

   function Index
     (Kind : Ada_Node_Kind_Type; Field : Syntax_Field_Reference) return Positive is
   begin
      pragma Warnings (Off, "value not in range of type");
      return Impl.Index (Kind, Field);
      pragma Warnings (On, "value not in range of type");
   end Index;

   ---------------------------------------
   -- Syntax_Field_Reference_From_Index --
   ---------------------------------------

   function Syntax_Field_Reference_From_Index
     (Kind : Ada_Node_Kind_Type; Index : Positive) return Syntax_Field_Reference is
   begin
      pragma Warnings (Off, "value not in range of type");
      return Impl.Syntax_Field_Reference_From_Index (Kind, Index);
      pragma Warnings (On, "value not in range of type");
   end Syntax_Field_Reference_From_Index;

   -------------------
   -- Syntax_Fields --
   -------------------

   function Syntax_Fields
     (Kind : Ada_Node_Kind_Type) return Syntax_Field_Reference_Array is
   begin
      return Impl.Syntax_Fields (Kind);
   end Syntax_Fields;

   -------------------
   -- Syntax_Fields --
   -------------------

   function Syntax_Fields
     (Id : Node_Type_Id) return Syntax_Field_Reference_Array is
   begin
      return Impl.Syntax_Fields (Id);
   end Syntax_Fields;

   -----------------------------
   -- Property_Argument_Types --
   -----------------------------

   function Property_Argument_Types
     (Property : Property_Reference) return Type_Constraint_Array is
   begin
      return Impl.Property_Argument_Types (Property);
   end Property_Argument_Types;

   ----------------------------
   -- Property_Argument_Name --
   ----------------------------

   function Property_Argument_Name
     (Property        : Property_Reference;
      Argument_Number : Positive) return Text_Type is
   begin
      return Impl.Property_Argument_Name (Property, Argument_Number);
   end Property_Argument_Name;

   -------------------------------------
   -- Property_Argument_Default_Value --
   -------------------------------------

   function Property_Argument_Default_Value
     (Property        : Property_Reference;
      Argument_Number : Positive) return Any_Value_Type
   is
      Desc : Impl.Property_Descriptor renames
         Impl.Property_Descriptors (Property).all;
   begin
      Impl.Check_Argument_Number (Desc, Argument_Number);
      return From_Internal_Value
        (Desc.Argument_Default_Values (Argument_Number));
   end Property_Argument_Default_Value;

   -------------------
   -- Eval_Property --
   -------------------

   function Eval_Property
     (Node      : Ada_Node'Class;
      Property  : Property_Reference;
      Arguments : Value_Array) return Value_Type
   is
      Kind   : constant Ada_Node_Kind_Type := Node.Kind;
      Desc   : Impl.Property_Descriptor renames
         Impl.Property_Descriptors (Property).all;
      Result : Any_Value_Type := No_Value;
   begin
      --  First, check that arguments match the property signature

      if Arguments'Length /= Desc.Arity then
         raise Bad_Type_Error with "invalid number of arguments";
      end if;

      for I in Desc.Argument_Types'Range loop
         declare
            Arg : Value_Type renames Arguments (I - 1 + Arguments'First);
         begin
            if not Satisfies (Arg, Desc.Argument_Types (I)) then
               raise Bad_Type_Error with
                  "invalid type for argument " & Desc.Argument_Names (I).all;
            end if;
         end;
      end loop;

      --  Now, we can proceed with the property evaluation

      
      case Property is
when Ada_Node_P_Declarative_Scope =>
Result := Create_Node (Node.P_Declarative_Scope);
when Ada_Node_P_Enclosing_Compilation_Unit =>
Result := Create_Node (Node.P_Enclosing_Compilation_Unit);
when Ada_Node_P_Get_Uninstantiated_Node =>
Result := Create_Node (Node.P_Get_Uninstantiated_Node);
when Ada_Node_P_Complete =>
Result := Create_Completion_Item_Iterator (Node.P_Complete);
when Ada_Node_P_Valid_Keywords =>
Result := Create_Unbounded_Text_Type_Array (Node.P_Valid_Keywords);
when Ada_Node_P_Generic_Instantiations =>
Result := Create_Generic_Instantiation_Array (Node.P_Generic_Instantiations);
when Ada_Node_P_Semantic_Parent =>
Result := Create_Node (Node.P_Semantic_Parent);
when Ada_Node_P_Parent_Basic_Decl =>
Result := Create_Node (Node.P_Parent_Basic_Decl);
when Ada_Node_P_Filter_Is_Imported_By =>
declare
Units : constant Analysis_Unit_Array :=
As_Analysis_Unit_Array (Arguments (Arguments'First + 0));
Transitive : constant Boolean :=
As_Boolean (Arguments (Arguments'First + 1));
begin
Result := Create_Analysis_Unit_Array (Node.P_Filter_Is_Imported_By (Units, Transitive));
end;
when Ada_Node_P_Xref_Entry_Point =>
Result := Create_Boolean (Node.P_Xref_Entry_Point);
when Ada_Node_P_Resolve_Names =>
Result := Create_Boolean (Node.P_Resolve_Names);
when Ada_Node_P_Standard_Unit =>
Result := Create_Analysis_Unit (Node.P_Standard_Unit);
when Ada_Node_P_Std_Entity =>
declare
Sym : constant Unbounded_Text_Type :=
As_Unbounded_Text (Arguments (Arguments'First + 0));
begin
Result := Create_Node (Node.P_Std_Entity (Sym));
end;
when Ada_Node_P_Bool_Type =>
Result := Create_Node (Node.P_Bool_Type);
when Ada_Node_P_Int_Type =>
Result := Create_Node (Node.P_Int_Type);
when Ada_Node_P_Universal_Int_Type =>
Result := Create_Node (Node.P_Universal_Int_Type);
when Ada_Node_P_Universal_Real_Type =>
Result := Create_Node (Node.P_Universal_Real_Type);
when Ada_Node_P_Std_Char_Type =>
Result := Create_Node (Node.P_Std_Char_Type);
when Ada_Node_P_Std_Wide_Char_Type =>
Result := Create_Node (Node.P_Std_Wide_Char_Type);
when Ada_Node_P_Std_Wide_Wide_Char_Type =>
Result := Create_Node (Node.P_Std_Wide_Wide_Char_Type);
when Ada_Node_P_Top_Level_Decl =>
declare
Unit : constant Analysis_Unit :=
As_Analysis_Unit (Arguments (Arguments'First + 0));
begin
Result := Create_Node (Node.P_Top_Level_Decl (Unit));
end;
when Ada_Node_P_Choice_Match =>
declare
Value : constant Big_Integer :=
As_Big_Integer (Arguments (Arguments'First + 0));
begin
Result := Create_Boolean (Node.P_Choice_Match (Value));
end;
when Ada_Node_P_Gnat_Xref =>
declare
Imprecise_Fallback : constant Boolean :=
As_Boolean (Arguments (Arguments'First + 0));
begin
Result := Create_Node (Node.P_Gnat_Xref (Imprecise_Fallback));
end;
when Ada_Node_Parent =>
Result := Create_Node (Node.Parent);
when Ada_Node_Parents =>
declare
With_Self : constant Boolean :=
As_Boolean (Arguments (Arguments'First + 0));
begin
Result := Create_Ada_Node_Array (Node.Parents (With_Self));
end;
when Ada_Node_Children =>
Result := Create_Ada_Node_Array (Node.Children);
when Ada_Node_Token_Start =>
Result := Create_Token (Node.Token_Start);
when Ada_Node_Token_End =>
Result := Create_Token (Node.Token_End);
when Ada_Node_Child_Index =>
Result := Create_Integer (Node.Child_Index);
when Ada_Node_Previous_Sibling =>
Result := Create_Node (Node.Previous_Sibling);
when Ada_Node_Next_Sibling =>
Result := Create_Node (Node.Next_Sibling);
when Ada_Node_Unit =>
Result := Create_Analysis_Unit (Node.Unit);
when Ada_Node_Is_Ghost =>
Result := Create_Boolean (Node.Is_Ghost);
when Ada_Node_Full_Sloc_Image =>
Result := Create_String (Node.Full_Sloc_Image);
when others => null;
end case;
case Ada_Ada_Node (Kind) is
when Ada_Abort_Node =>
declare
N_Bare_Abort_Node : constant Analysis.Abort_Node := Node.As_Abort_Node;
begin
case Property is
when Abort_Node_P_As_Bool =>
Result := Create_Boolean (N_Bare_Abort_Node.P_As_Bool);
when others => null;
end case;
end;
when Ada_Abstract_Node =>
declare
N_Bare_Abstract_Node : constant Analysis.Abstract_Node := Node.As_Abstract_Node;
begin
case Property is
when Abstract_Node_P_As_Bool =>
Result := Create_Boolean (N_Bare_Abstract_Node.P_As_Bool);
when others => null;
end case;
end;
when Ada_Assoc_List_Range =>
declare
N_Bare_Assoc_List : constant Analysis.Assoc_List := Node.As_Assoc_List;
begin
case Property is
when Assoc_List_P_Zip_With_Params =>
declare
Imprecise_Fallback : constant Boolean :=
As_Boolean (Arguments (Arguments'First + 0));
begin
Result := Create_Param_Actual_Array (N_Bare_Assoc_List.P_Zip_With_Params (Imprecise_Fallback));
end;
when others => null;
end case;
end;
when Ada_Aliased_Node =>
declare
N_Bare_Aliased_Node : constant Analysis.Aliased_Node := Node.As_Aliased_Node;
begin
case Property is
when Aliased_Node_P_As_Bool =>
Result := Create_Boolean (N_Bare_Aliased_Node.P_As_Bool);
when others => null;
end case;
end;
when Ada_All_Node =>
declare
N_Bare_All_Node : constant Analysis.All_Node := Node.As_All_Node;
begin
case Property is
when All_Node_P_As_Bool =>
Result := Create_Boolean (N_Bare_All_Node.P_As_Bool);
when others => null;
end case;
end;
when Ada_Aspect_Assoc_Range =>
declare
N_Bare_Aspect_Assoc : constant Analysis.Aspect_Assoc := Node.As_Aspect_Assoc;
begin
case Property is
when Aspect_Assoc_P_Is_Ghost_Code =>
Result := Create_Boolean (N_Bare_Aspect_Assoc.P_Is_Ghost_Code);
when others => null;
end case;
end;
when Ada_Enum_Rep_Clause_Range =>
declare
N_Bare_Enum_Rep_Clause : constant Analysis.Enum_Rep_Clause := Node.As_Enum_Rep_Clause;
begin
case Property is
when Enum_Rep_Clause_P_Params =>
Result := Create_Param_Actual_Array (N_Bare_Enum_Rep_Clause.P_Params);
when others => null;
end case;
end;
when Ada_Base_Assoc =>
declare
N_Bare_Base_Assoc : constant Analysis.Base_Assoc := Node.As_Base_Assoc;
begin
case Property is
when Base_Assoc_P_Assoc_Expr =>
Result := Create_Node (N_Bare_Base_Assoc.P_Assoc_Expr);
when others => null;
end case;
end;
when Ada_Base_Formal_Param_Holder =>
declare
N_Bare_Base_Formal_Param_Holder : constant Analysis.Base_Formal_Param_Holder := Node.As_Base_Formal_Param_Holder;
begin
case Property is
when Base_Formal_Param_Holder_P_Abstract_Formal_Params =>
Result := Create_Base_Formal_Param_Decl_Array (N_Bare_Base_Formal_Param_Holder.P_Abstract_Formal_Params);
when Base_Formal_Param_Holder_P_Formal_Params =>
Result := Create_Defining_Name_Array (N_Bare_Base_Formal_Param_Holder.P_Formal_Params);
when Base_Formal_Param_Holder_P_Nb_Min_Params =>
Result := Create_Integer (N_Bare_Base_Formal_Param_Holder.P_Nb_Min_Params);
when Base_Formal_Param_Holder_P_Nb_Max_Params =>
Result := Create_Integer (N_Bare_Base_Formal_Param_Holder.P_Nb_Max_Params);
when Base_Formal_Param_Holder_P_Param_Types =>
declare
Origin : constant Ada_Node :=
As_Node (Arguments (Arguments'First + 0));
begin
Result := Create_Base_Type_Decl_Array (N_Bare_Base_Formal_Param_Holder.P_Param_Types (Origin));
end;
when others => null;
end case;
case Ada_Base_Formal_Param_Holder (Kind) is
when Ada_Base_Subp_Spec =>
declare
N_Bare_Base_Subp_Spec : constant Analysis.Base_Subp_Spec := N_Bare_Base_Formal_Param_Holder.As_Base_Subp_Spec;
begin
case Property is
when Base_Subp_Spec_P_Returns =>
Result := Create_Node (N_Bare_Base_Subp_Spec.P_Returns);
when Base_Subp_Spec_P_Params =>
Result := Create_Param_Spec_Array (N_Bare_Base_Subp_Spec.P_Params);
when Base_Subp_Spec_P_Primitive_Subp_Types =>
declare
Imprecise_Fallback : constant Boolean :=
As_Boolean (Arguments (Arguments'First + 0));
begin
Result := Create_Base_Type_Decl_Array (N_Bare_Base_Subp_Spec.P_Primitive_Subp_Types (Imprecise_Fallback));
end;
when Base_Subp_Spec_P_Primitive_Subp_First_Type =>
declare
Imprecise_Fallback : constant Boolean :=
As_Boolean (Arguments (Arguments'First + 0));
begin
Result := Create_Node (N_Bare_Base_Subp_Spec.P_Primitive_Subp_First_Type (Imprecise_Fallback));
end;
when Base_Subp_Spec_P_Primitive_Subp_Tagged_Type =>
declare
Imprecise_Fallback : constant Boolean :=
As_Boolean (Arguments (Arguments'First + 0));
begin
Result := Create_Node (N_Bare_Base_Subp_Spec.P_Primitive_Subp_Tagged_Type (Imprecise_Fallback));
end;
when Base_Subp_Spec_P_Return_Type =>
declare
Origin : constant Ada_Node :=
As_Node (Arguments (Arguments'First + 0));
begin
Result := Create_Node (N_Bare_Base_Subp_Spec.P_Return_Type (Origin));
end;
when others => null;
end case;
end;
when others => null;
end case;
end;
when Ada_Basic_Assoc =>
declare
N_Bare_Basic_Assoc : constant Analysis.Basic_Assoc := Node.As_Basic_Assoc;
begin
case Property is
when Basic_Assoc_P_Get_Params =>
declare
Imprecise_Fallback : constant Boolean :=
As_Boolean (Arguments (Arguments'First + 0));
begin
Result := Create_Defining_Name_Array (N_Bare_Basic_Assoc.P_Get_Params (Imprecise_Fallback));
end;
when others => null;
end case;
end;
when Ada_Basic_Decl =>
declare
N_Bare_Basic_Decl : constant Analysis.Basic_Decl := Node.As_Basic_Decl;
begin
case Property is
when Basic_Decl_P_Is_Formal =>
Result := Create_Boolean (N_Bare_Basic_Decl.P_Is_Formal);
when Basic_Decl_P_Doc_Annotations =>
Result := Create_Doc_Annotation_Array (N_Bare_Basic_Decl.P_Doc_Annotations);
when Basic_Decl_P_Doc =>
Result := Create_String (N_Bare_Basic_Decl.P_Doc);
when Basic_Decl_P_Previous_Part_For_Decl =>
declare
Imprecise_Fallback : constant Boolean :=
As_Boolean (Arguments (Arguments'First + 0));
begin
Result := Create_Node (N_Bare_Basic_Decl.P_Previous_Part_For_Decl (Imprecise_Fallback));
end;
when Basic_Decl_P_Canonical_Part =>
declare
Imprecise_Fallback : constant Boolean :=
As_Boolean (Arguments (Arguments'First + 0));
begin
Result := Create_Node (N_Bare_Basic_Decl.P_Canonical_Part (Imprecise_Fallback));
end;
when Basic_Decl_P_All_Parts =>
declare
Imprecise_Fallback : constant Boolean :=
As_Boolean (Arguments (Arguments'First + 0));
begin
Result := Create_Basic_Decl_Array (N_Bare_Basic_Decl.P_All_Parts (Imprecise_Fallback));
end;
when Basic_Decl_P_Is_Static_Decl =>
declare
Imprecise_Fallback : constant Boolean :=
As_Boolean (Arguments (Arguments'First + 0));
begin
Result := Create_Boolean (N_Bare_Basic_Decl.P_Is_Static_Decl (Imprecise_Fallback));
end;
when Basic_Decl_P_Get_Aspect_Assoc =>
declare
Name : constant Unbounded_Text_Type :=
As_Unbounded_Text (Arguments (Arguments'First + 0));
begin
Result := Create_Node (N_Bare_Basic_Decl.P_Get_Aspect_Assoc (Name));
end;
when Basic_Decl_P_Get_Aspect_Spec_Expr =>
declare
Name : constant Unbounded_Text_Type :=
As_Unbounded_Text (Arguments (Arguments'First + 0));
begin
Result := Create_Node (N_Bare_Basic_Decl.P_Get_Aspect_Spec_Expr (Name));
end;
when Basic_Decl_P_Get_Aspect =>
declare
Name : constant Unbounded_Text_Type :=
As_Unbounded_Text (Arguments (Arguments'First + 0));
Imprecise_Fallback : constant Boolean :=
As_Boolean (Arguments (Arguments'First + 1));
begin
Result := Create_Aspect (N_Bare_Basic_Decl.P_Get_Aspect (Name, Imprecise_Fallback));
end;
when Basic_Decl_P_Has_Aspect =>
declare
Name : constant Unbounded_Text_Type :=
As_Unbounded_Text (Arguments (Arguments'First + 0));
Imprecise_Fallback : constant Boolean :=
As_Boolean (Arguments (Arguments'First + 1));
begin
Result := Create_Boolean (N_Bare_Basic_Decl.P_Has_Aspect (Name, Imprecise_Fallback));
end;
when Basic_Decl_P_Get_Pragma =>
declare
Name : constant Unbounded_Text_Type :=
As_Unbounded_Text (Arguments (Arguments'First + 0));
begin
Result := Create_Node (N_Bare_Basic_Decl.P_Get_Pragma (Name));
end;
when Basic_Decl_P_Get_Representation_Clause =>
declare
Name : constant Unbounded_Text_Type :=
As_Unbounded_Text (Arguments (Arguments'First + 0));
Imprecise_Fallback : constant Boolean :=
As_Boolean (Arguments (Arguments'First + 1));
begin
Result := Create_Node (N_Bare_Basic_Decl.P_Get_Representation_Clause (Name, Imprecise_Fallback));
end;
when Basic_Decl_P_Get_At_Clause =>
declare
Imprecise_Fallback : constant Boolean :=
As_Boolean (Arguments (Arguments'First + 0));
begin
Result := Create_Node (N_Bare_Basic_Decl.P_Get_At_Clause (Imprecise_Fallback));
end;
when Basic_Decl_P_Is_Imported =>
Result := Create_Boolean (N_Bare_Basic_Decl.P_Is_Imported);
when Basic_Decl_P_Is_Ghost_Code =>
Result := Create_Boolean (N_Bare_Basic_Decl.P_Is_Ghost_Code);
when Basic_Decl_P_Is_Compilation_Unit_Root =>
Result := Create_Boolean (N_Bare_Basic_Decl.P_Is_Compilation_Unit_Root);
when Basic_Decl_P_Is_Visible =>
declare
From_Node : constant Ada_Node :=
As_Node (Arguments (Arguments'First + 0));
begin
Result := Create_Boolean (N_Bare_Basic_Decl.P_Is_Visible (From_Node));
end;
when Basic_Decl_P_Base_Subp_Declarations =>
declare
Imprecise_Fallback : constant Boolean :=
As_Boolean (Arguments (Arguments'First + 0));
begin
Result := Create_Basic_Decl_Array (N_Bare_Basic_Decl.P_Base_Subp_Declarations (Imprecise_Fallback));
end;
when Basic_Decl_P_Root_Subp_Declarations =>
declare
Origin : constant Ada_Node :=
As_Node (Arguments (Arguments'First + 0));
Imprecise_Fallback : constant Boolean :=
As_Boolean (Arguments (Arguments'First + 1));
begin
Result := Create_Basic_Decl_Array (N_Bare_Basic_Decl.P_Root_Subp_Declarations (Origin, Imprecise_Fallback));
end;
when Basic_Decl_P_Find_All_Overrides =>
declare
Units : constant Analysis_Unit_Array :=
As_Analysis_Unit_Array (Arguments (Arguments'First + 0));
Imprecise_Fallback : constant Boolean :=
As_Boolean (Arguments (Arguments'First + 1));
begin
Result := Create_Basic_Decl_Array (N_Bare_Basic_Decl.P_Find_All_Overrides (Units, Imprecise_Fallback));
end;
when Basic_Decl_P_Defining_Names =>
Result := Create_Defining_Name_Array (N_Bare_Basic_Decl.P_Defining_Names);
when Basic_Decl_P_Defining_Name =>
Result := Create_Node (N_Bare_Basic_Decl.P_Defining_Name);
when Basic_Decl_P_Type_Expression =>
Result := Create_Node (N_Bare_Basic_Decl.P_Type_Expression);
when Basic_Decl_P_Subp_Spec_Or_Null =>
declare
Follow_Generic : constant Boolean :=
As_Boolean (Arguments (Arguments'First + 0));
begin
Result := Create_Node (N_Bare_Basic_Decl.P_Subp_Spec_Or_Null (Follow_Generic));
end;
when Basic_Decl_P_Is_Subprogram =>
Result := Create_Boolean (N_Bare_Basic_Decl.P_Is_Subprogram);
when Basic_Decl_P_Relative_Name =>
Result := Create_Node (N_Bare_Basic_Decl.P_Relative_Name);
when Basic_Decl_P_Relative_Name_Text =>
Result := Create_Unbounded_Text (N_Bare_Basic_Decl.P_Relative_Name_Text);
when Basic_Decl_P_Next_Part_For_Decl =>
declare
Imprecise_Fallback : constant Boolean :=
As_Boolean (Arguments (Arguments'First + 0));
begin
Result := Create_Node (N_Bare_Basic_Decl.P_Next_Part_For_Decl (Imprecise_Fallback));
end;
when Basic_Decl_P_Body_Part_For_Decl =>
declare
Imprecise_Fallback : constant Boolean :=
As_Boolean (Arguments (Arguments'First + 0));
begin
Result := Create_Node (N_Bare_Basic_Decl.P_Body_Part_For_Decl (Imprecise_Fallback));
end;
when Basic_Decl_P_Most_Visible_Part =>
declare
Origin : constant Ada_Node :=
As_Node (Arguments (Arguments'First + 0));
Imprecise_Fallback : constant Boolean :=
As_Boolean (Arguments (Arguments'First + 1));
begin
Result := Create_Node (N_Bare_Basic_Decl.P_Most_Visible_Part (Origin, Imprecise_Fallback));
end;
when Basic_Decl_P_Fully_Qualified_Name_Array =>
declare
Include_Profile : constant Boolean :=
As_Boolean (Arguments (Arguments'First + 0));
begin
Result := Create_Unbounded_Text_Type_Array (N_Bare_Basic_Decl.P_Fully_Qualified_Name_Array (Include_Profile));
end;
when Basic_Decl_P_Fully_Qualified_Name =>
Result := Create_String (N_Bare_Basic_Decl.P_Fully_Qualified_Name);
when Basic_Decl_P_Canonical_Fully_Qualified_Name =>
Result := Create_String (N_Bare_Basic_Decl.P_Canonical_Fully_Qualified_Name);
when Basic_Decl_P_Unique_Identifying_Name =>
Result := Create_String (N_Bare_Basic_Decl.P_Unique_Identifying_Name);
when Basic_Decl_P_Is_Constant_Object =>
Result := Create_Boolean (N_Bare_Basic_Decl.P_Is_Constant_Object);
when others => null;
end case;
case Ada_Basic_Decl (Kind) is
when Ada_Anonymous_Expr_Decl_Range =>
declare
N_Bare_Anonymous_Expr_Decl : constant Analysis.Anonymous_Expr_Decl := N_Bare_Basic_Decl.As_Anonymous_Expr_Decl;
begin
case Property is
when Anonymous_Expr_Decl_P_Get_Formal =>
declare
Imprecise_Fallback : constant Boolean :=
As_Boolean (Arguments (Arguments'First + 0));
begin
Result := Create_Node (N_Bare_Anonymous_Expr_Decl.P_Get_Formal (Imprecise_Fallback));
end;
when others => null;
end case;
end;
when Ada_Base_Formal_Param_Decl =>
declare
N_Bare_Base_Formal_Param_Decl : constant Analysis.Base_Formal_Param_Decl := N_Bare_Basic_Decl.As_Base_Formal_Param_Decl;
begin
case Property is
when Base_Formal_Param_Decl_P_Formal_Type =>
declare
Origin : constant Ada_Node :=
As_Node (Arguments (Arguments'First + 0));
begin
Result := Create_Node (N_Bare_Base_Formal_Param_Decl.P_Formal_Type (Origin));
end;
when others => null;
end case;
end;
when Ada_Base_Package_Decl =>
declare
N_Bare_Base_Package_Decl : constant Analysis.Base_Package_Decl := N_Bare_Basic_Decl.As_Base_Package_Decl;
begin
case Property is
when Base_Package_Decl_P_Body_Part =>
Result := Create_Node (N_Bare_Base_Package_Decl.P_Body_Part);
when others => null;
end case;
end;
when Ada_Base_Type_Decl =>
declare
N_Bare_Base_Type_Decl : constant Analysis.Base_Type_Decl := N_Bare_Basic_Decl.As_Base_Type_Decl;
begin
case Property is
when Base_Type_Decl_P_Base_Subtype =>
declare
Origin : constant Ada_Node :=
As_Node (Arguments (Arguments'First + 0));
begin
Result := Create_Node (N_Bare_Base_Type_Decl.P_Base_Subtype (Origin));
end;
when Base_Type_Decl_P_Private_Completion =>
Result := Create_Node (N_Bare_Base_Type_Decl.P_Private_Completion);
when Base_Type_Decl_P_Is_Inherited_Primitive =>
declare
P : constant Basic_Decl :=
As_Node (Arguments (Arguments'First + 0)).As_Basic_Decl;
begin
Result := Create_Boolean (N_Bare_Base_Type_Decl.P_Is_Inherited_Primitive (P));
end;
when Base_Type_Decl_P_Get_Record_Representation_Clause =>
declare
Imprecise_Fallback : constant Boolean :=
As_Boolean (Arguments (Arguments'First + 0));
begin
Result := Create_Node (N_Bare_Base_Type_Decl.P_Get_Record_Representation_Clause (Imprecise_Fallback));
end;
when Base_Type_Decl_P_Get_Enum_Representation_Clause =>
declare
Imprecise_Fallback : constant Boolean :=
As_Boolean (Arguments (Arguments'First + 0));
begin
Result := Create_Node (N_Bare_Base_Type_Decl.P_Get_Enum_Representation_Clause (Imprecise_Fallback));
end;
when Base_Type_Decl_P_Is_Record_Type =>
declare
Origin : constant Ada_Node :=
As_Node (Arguments (Arguments'First + 0));
begin
Result := Create_Boolean (N_Bare_Base_Type_Decl.P_Is_Record_Type (Origin));
end;
when Base_Type_Decl_P_Is_Array_Type =>
declare
Origin : constant Ada_Node :=
As_Node (Arguments (Arguments'First + 0));
begin
Result := Create_Boolean (N_Bare_Base_Type_Decl.P_Is_Array_Type (Origin));
end;
when Base_Type_Decl_P_Find_Derived_Types =>
declare
Root : constant Ada_Node :=
As_Node (Arguments (Arguments'First + 0));
Origin : constant Ada_Node :=
As_Node (Arguments (Arguments'First + 1));
Imprecise_Fallback : constant Boolean :=
As_Boolean (Arguments (Arguments'First + 2));
begin
Result := Create_Type_Decl_Array (N_Bare_Base_Type_Decl.P_Find_Derived_Types (Root, Origin, Imprecise_Fallback));
end;
when Base_Type_Decl_P_Is_Real_Type =>
declare
Origin : constant Ada_Node :=
As_Node (Arguments (Arguments'First + 0));
begin
Result := Create_Boolean (N_Bare_Base_Type_Decl.P_Is_Real_Type (Origin));
end;
when Base_Type_Decl_P_Is_Float_Type =>
declare
Origin : constant Ada_Node :=
As_Node (Arguments (Arguments'First + 0));
begin
Result := Create_Boolean (N_Bare_Base_Type_Decl.P_Is_Float_Type (Origin));
end;
when Base_Type_Decl_P_Is_Fixed_Point =>
declare
Origin : constant Ada_Node :=
As_Node (Arguments (Arguments'First + 0));
begin
Result := Create_Boolean (N_Bare_Base_Type_Decl.P_Is_Fixed_Point (Origin));
end;
when Base_Type_Decl_P_Is_Enum_Type =>
declare
Origin : constant Ada_Node :=
As_Node (Arguments (Arguments'First + 0));
begin
Result := Create_Boolean (N_Bare_Base_Type_Decl.P_Is_Enum_Type (Origin));
end;
when Base_Type_Decl_P_Is_Access_Type =>
declare
Origin : constant Ada_Node :=
As_Node (Arguments (Arguments'First + 0));
begin
Result := Create_Boolean (N_Bare_Base_Type_Decl.P_Is_Access_Type (Origin));
end;
when Base_Type_Decl_P_Is_Char_Type =>
declare
Origin : constant Ada_Node :=
As_Node (Arguments (Arguments'First + 0));
begin
Result := Create_Boolean (N_Bare_Base_Type_Decl.P_Is_Char_Type (Origin));
end;
when Base_Type_Decl_P_Discrete_Range =>
Result := Create_Discrete_Range (N_Bare_Base_Type_Decl.P_Discrete_Range);
when Base_Type_Decl_P_Is_Discrete_Type =>
declare
Origin : constant Ada_Node :=
As_Node (Arguments (Arguments'First + 0));
begin
Result := Create_Boolean (N_Bare_Base_Type_Decl.P_Is_Discrete_Type (Origin));
end;
when Base_Type_Decl_P_Is_Int_Type =>
declare
Origin : constant Ada_Node :=
As_Node (Arguments (Arguments'First + 0));
begin
Result := Create_Boolean (N_Bare_Base_Type_Decl.P_Is_Int_Type (Origin));
end;
when Base_Type_Decl_P_Accessed_Type =>
declare
Origin : constant Ada_Node :=
As_Node (Arguments (Arguments'First + 0));
begin
Result := Create_Node (N_Bare_Base_Type_Decl.P_Accessed_Type (Origin));
end;
when Base_Type_Decl_P_Is_Tagged_Type =>
declare
Origin : constant Ada_Node :=
As_Node (Arguments (Arguments'First + 0));
begin
Result := Create_Boolean (N_Bare_Base_Type_Decl.P_Is_Tagged_Type (Origin));
end;
when Base_Type_Decl_P_Base_Type =>
declare
Origin : constant Ada_Node :=
As_Node (Arguments (Arguments'First + 0));
begin
Result := Create_Node (N_Bare_Base_Type_Decl.P_Base_Type (Origin));
end;
when Base_Type_Decl_P_Base_Types =>
declare
Origin : constant Ada_Node :=
As_Node (Arguments (Arguments'First + 0));
begin
Result := Create_Base_Type_Decl_Array (N_Bare_Base_Type_Decl.P_Base_Types (Origin));
end;
when Base_Type_Decl_P_Find_All_Derived_Types =>
declare
Units : constant Analysis_Unit_Array :=
As_Analysis_Unit_Array (Arguments (Arguments'First + 0));
Imprecise_Fallback : constant Boolean :=
As_Boolean (Arguments (Arguments'First + 1));
begin
Result := Create_Type_Decl_Array (N_Bare_Base_Type_Decl.P_Find_All_Derived_Types (Units, Imprecise_Fallback));
end;
when Base_Type_Decl_P_Comp_Type =>
declare
Is_Subscript : constant Boolean :=
As_Boolean (Arguments (Arguments'First + 0));
Origin : constant Ada_Node :=
As_Node (Arguments (Arguments'First + 1));
begin
Result := Create_Node (N_Bare_Base_Type_Decl.P_Comp_Type (Is_Subscript, Origin));
end;
when Base_Type_Decl_P_Index_Type =>
declare
Dim : constant Integer :=
As_Integer (Arguments (Arguments'First + 0));
Origin : constant Ada_Node :=
As_Node (Arguments (Arguments'First + 1));
begin
Result := Create_Node (N_Bare_Base_Type_Decl.P_Index_Type (Dim, Origin));
end;
when Base_Type_Decl_P_Is_Derived_Type =>
declare
Other_Type : constant Base_Type_Decl :=
As_Node (Arguments (Arguments'First + 0)).As_Base_Type_Decl;
Origin : constant Ada_Node :=
As_Node (Arguments (Arguments'First + 1));
begin
Result := Create_Boolean (N_Bare_Base_Type_Decl.P_Is_Derived_Type (Other_Type, Origin));
end;
when Base_Type_Decl_P_Is_Interface_Type =>
declare
Origin : constant Ada_Node :=
As_Node (Arguments (Arguments'First + 0));
begin
Result := Create_Boolean (N_Bare_Base_Type_Decl.P_Is_Interface_Type (Origin));
end;
when Base_Type_Decl_P_Matching_Type =>
declare
Expected_Type : constant Base_Type_Decl :=
As_Node (Arguments (Arguments'First + 0)).As_Base_Type_Decl;
Origin : constant Ada_Node :=
As_Node (Arguments (Arguments'First + 1));
begin
Result := Create_Boolean (N_Bare_Base_Type_Decl.P_Matching_Type (Expected_Type, Origin));
end;
when Base_Type_Decl_P_Canonical_Type =>
declare
Origin : constant Ada_Node :=
As_Node (Arguments (Arguments'First + 0));
begin
Result := Create_Node (N_Bare_Base_Type_Decl.P_Canonical_Type (Origin));
end;
when Base_Type_Decl_P_Previous_Part =>
declare
Go_To_Incomplete : constant Boolean :=
As_Boolean (Arguments (Arguments'First + 0));
begin
Result := Create_Node (N_Bare_Base_Type_Decl.P_Previous_Part (Go_To_Incomplete));
end;
when Base_Type_Decl_P_Next_Part =>
Result := Create_Node (N_Bare_Base_Type_Decl.P_Next_Part);
when Base_Type_Decl_P_Full_View =>
Result := Create_Node (N_Bare_Base_Type_Decl.P_Full_View);
when Base_Type_Decl_P_Is_Definite_Subtype =>
declare
Origin : constant Ada_Node :=
As_Node (Arguments (Arguments'First + 0));
begin
Result := Create_Boolean (N_Bare_Base_Type_Decl.P_Is_Definite_Subtype (Origin));
end;
when Base_Type_Decl_P_Is_Private =>
Result := Create_Boolean (N_Bare_Base_Type_Decl.P_Is_Private);
when Base_Type_Decl_P_Discriminants_List =>
declare
Origin : constant Ada_Node :=
As_Node (Arguments (Arguments'First + 0));
begin
Result := Create_Base_Formal_Param_Decl_Array (N_Bare_Base_Type_Decl.P_Discriminants_List (Origin));
end;
when Base_Type_Decl_P_Root_Type =>
declare
Origin : constant Ada_Node :=
As_Node (Arguments (Arguments'First + 0));
begin
Result := Create_Node (N_Bare_Base_Type_Decl.P_Root_Type (Origin));
end;
when Base_Type_Decl_P_Shapes =>
declare
Include_Discriminants : constant Boolean :=
As_Boolean (Arguments (Arguments'First + 0));
Origin : constant Ada_Node :=
As_Node (Arguments (Arguments'First + 1));
begin
Result := Create_Shape_Array (N_Bare_Base_Type_Decl.P_Shapes (Include_Discriminants, Origin));
end;
when others => null;
end case;
case Ada_Base_Type_Decl (Kind) is
when Ada_Base_Subtype_Decl =>
declare
N_Bare_Base_Subtype_Decl : constant Analysis.Base_Subtype_Decl := N_Bare_Base_Type_Decl.As_Base_Subtype_Decl;
begin
case Property is
when Base_Subtype_Decl_P_Get_Type =>
declare
Origin : constant Ada_Node :=
As_Node (Arguments (Arguments'First + 0));
begin
Result := Create_Node (N_Bare_Base_Subtype_Decl.P_Get_Type (Origin));
end;
when others => null;
end case;
end;
when Ada_Type_Decl =>
declare
N_Bare_Type_Decl : constant Analysis.Type_Decl := N_Bare_Base_Type_Decl.As_Type_Decl;
begin
case Property is
when Type_Decl_P_Get_Primitives =>
declare
Only_Inherited : constant Boolean :=
As_Boolean (Arguments (Arguments'First + 0));
Include_Predefined_Operators : constant Boolean :=
As_Boolean (Arguments (Arguments'First + 1));
begin
Result := Create_Basic_Decl_Array (N_Bare_Type_Decl.P_Get_Primitives (Only_Inherited, Include_Predefined_Operators));
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
case Property is
when Basic_Subp_Decl_P_Subp_Decl_Spec =>
Result := Create_Node (N_Bare_Basic_Subp_Decl.P_Subp_Decl_Spec);
when others => null;
end case;
case Ada_Basic_Subp_Decl (Kind) is
when Ada_Classic_Subp_Decl =>
declare
N_Bare_Classic_Subp_Decl : constant Analysis.Classic_Subp_Decl := N_Bare_Basic_Subp_Decl.As_Classic_Subp_Decl;
begin
case Property is
when Classic_Subp_Decl_P_Body_Part =>
declare
Imprecise_Fallback : constant Boolean :=
As_Boolean (Arguments (Arguments'First + 0));
begin
Result := Create_Node (N_Bare_Classic_Subp_Decl.P_Body_Part (Imprecise_Fallback));
end;
when others => null;
end case;
end;
when Ada_Entry_Decl_Range =>
declare
N_Bare_Entry_Decl : constant Analysis.Entry_Decl := N_Bare_Basic_Subp_Decl.As_Entry_Decl;
begin
case Property is
when Entry_Decl_P_Body_Part =>
declare
Imprecise_Fallback : constant Boolean :=
As_Boolean (Arguments (Arguments'First + 0));
begin
Result := Create_Node (N_Bare_Entry_Decl.P_Body_Part (Imprecise_Fallback));
end;
when Entry_Decl_P_Accept_Stmts =>
Result := Create_Accept_Stmt_Array (N_Bare_Entry_Decl.P_Accept_Stmts);
when others => null;
end case;
end;
when Ada_Enum_Literal_Decl_Range =>
declare
N_Bare_Enum_Literal_Decl : constant Analysis.Enum_Literal_Decl := N_Bare_Basic_Subp_Decl.As_Enum_Literal_Decl;
begin
case Property is
when Enum_Literal_Decl_P_Enum_Type =>
Result := Create_Node (N_Bare_Enum_Literal_Decl.P_Enum_Type);
when others => null;
end case;
case Ada_Enum_Literal_Decl_Range (Kind) is
when Ada_Synthetic_Char_Enum_Lit_Range =>
declare
N_Bare_Synthetic_Char_Enum_Lit : constant Analysis.Synthetic_Char_Enum_Lit := N_Bare_Enum_Literal_Decl.As_Synthetic_Char_Enum_Lit;
begin
case Property is
when Synthetic_Char_Enum_Lit_P_Expr =>
Result := Create_Node (N_Bare_Synthetic_Char_Enum_Lit.P_Expr);
when others => null;
end case;
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
case Property is
when Body_Node_P_Previous_Part =>
declare
Imprecise_Fallback : constant Boolean :=
As_Boolean (Arguments (Arguments'First + 0));
begin
Result := Create_Node (N_Bare_Body_Node.P_Previous_Part (Imprecise_Fallback));
end;
when Body_Node_P_Decl_Part =>
declare
Imprecise_Fallback : constant Boolean :=
As_Boolean (Arguments (Arguments'First + 0));
begin
Result := Create_Node (N_Bare_Body_Node.P_Decl_Part (Imprecise_Fallback));
end;
when Body_Node_P_Subunit_Root =>
Result := Create_Node (N_Bare_Body_Node.P_Subunit_Root);
when others => null;
end case;
case Ada_Body_Node (Kind) is
when Ada_Body_Stub =>
declare
N_Bare_Body_Stub : constant Analysis.Body_Stub := N_Bare_Body_Node.As_Body_Stub;
begin
case Property is
when Body_Stub_P_Syntactic_Fully_Qualified_Name =>
Result := Create_Unbounded_Text_Type_Array (N_Bare_Body_Stub.P_Syntactic_Fully_Qualified_Name);
when others => null;
end case;
end;
when others => null;
end case;
end;
when Ada_Generic_Package_Decl_Range =>
declare
N_Bare_Generic_Package_Decl : constant Analysis.Generic_Package_Decl := N_Bare_Basic_Decl.As_Generic_Package_Decl;
begin
case Property is
when Generic_Package_Decl_P_Body_Part =>
Result := Create_Node (N_Bare_Generic_Package_Decl.P_Body_Part);
when others => null;
end case;
end;
when Ada_Generic_Subp_Decl_Range =>
declare
N_Bare_Generic_Subp_Decl : constant Analysis.Generic_Subp_Decl := N_Bare_Basic_Decl.As_Generic_Subp_Decl;
begin
case Property is
when Generic_Subp_Decl_P_Body_Part =>
declare
Imprecise_Fallback : constant Boolean :=
As_Boolean (Arguments (Arguments'First + 0));
begin
Result := Create_Node (N_Bare_Generic_Subp_Decl.P_Body_Part (Imprecise_Fallback));
end;
when others => null;
end case;
end;
when Ada_Generic_Instantiation =>
declare
N_Bare_Generic_Instantiation : constant Analysis.Generic_Instantiation := N_Bare_Basic_Decl.As_Generic_Instantiation;
begin
case Property is
when Generic_Instantiation_P_Designated_Generic_Decl =>
Result := Create_Node (N_Bare_Generic_Instantiation.P_Designated_Generic_Decl);
when Generic_Instantiation_P_Inst_Params =>
Result := Create_Param_Actual_Array (N_Bare_Generic_Instantiation.P_Inst_Params);
when others => null;
end case;
case Ada_Generic_Instantiation (Kind) is
when Ada_Generic_Subp_Instantiation_Range =>
declare
N_Bare_Generic_Subp_Instantiation : constant Analysis.Generic_Subp_Instantiation := N_Bare_Generic_Instantiation.As_Generic_Subp_Instantiation;
begin
case Property is
when Generic_Subp_Instantiation_P_Designated_Subp =>
Result := Create_Node (N_Bare_Generic_Subp_Instantiation.P_Designated_Subp);
when others => null;
end case;
end;
when others => null;
end case;
end;
when Ada_Object_Decl_Range =>
declare
N_Bare_Object_Decl : constant Analysis.Object_Decl := N_Bare_Basic_Decl.As_Object_Decl;
begin
case Property is
when Object_Decl_P_Private_Part_Decl =>
Result := Create_Node (N_Bare_Object_Decl.P_Private_Part_Decl);
when Object_Decl_P_Public_Part_Decl =>
Result := Create_Node (N_Bare_Object_Decl.P_Public_Part_Decl);
when others => null;
end case;
end;
when Ada_Package_Renaming_Decl_Range =>
declare
N_Bare_Package_Renaming_Decl : constant Analysis.Package_Renaming_Decl := N_Bare_Basic_Decl.As_Package_Renaming_Decl;
begin
case Property is
when Package_Renaming_Decl_P_Renamed_Package =>
Result := Create_Node (N_Bare_Package_Renaming_Decl.P_Renamed_Package);
when Package_Renaming_Decl_P_Final_Renamed_Package =>
Result := Create_Node (N_Bare_Package_Renaming_Decl.P_Final_Renamed_Package);
when others => null;
end case;
end;
when others => null;
end case;
end;
when Ada_Compilation_Unit_Range =>
declare
N_Bare_Compilation_Unit : constant Analysis.Compilation_Unit := Node.As_Compilation_Unit;
begin
case Property is
when Compilation_Unit_P_Syntactic_Fully_Qualified_Name =>
Result := Create_Unbounded_Text_Type_Array (N_Bare_Compilation_Unit.P_Syntactic_Fully_Qualified_Name);
when Compilation_Unit_P_Unit_Kind =>
Result := Create_Analysis_Unit_Kind (N_Bare_Compilation_Unit.P_Unit_Kind);
when Compilation_Unit_P_Withed_Units =>
Result := Create_Compilation_Unit_Array (N_Bare_Compilation_Unit.P_Withed_Units);
when Compilation_Unit_P_Imported_Units =>
Result := Create_Compilation_Unit_Array (N_Bare_Compilation_Unit.P_Imported_Units);
when Compilation_Unit_P_Unit_Dependencies =>
Result := Create_Compilation_Unit_Array (N_Bare_Compilation_Unit.P_Unit_Dependencies);
when Compilation_Unit_P_Decl =>
Result := Create_Node (N_Bare_Compilation_Unit.P_Decl);
when Compilation_Unit_P_Is_Preelaborable =>
declare
Imprecise_Fallback : constant Boolean :=
As_Boolean (Arguments (Arguments'First + 0));
begin
Result := Create_Boolean (N_Bare_Compilation_Unit.P_Is_Preelaborable (Imprecise_Fallback));
end;
when Compilation_Unit_P_Other_Part =>
Result := Create_Node (N_Bare_Compilation_Unit.P_Other_Part);
when Compilation_Unit_P_Has_Restriction =>
declare
Name : constant Unbounded_Text_Type :=
As_Unbounded_Text (Arguments (Arguments'First + 0));
begin
Result := Create_Boolean (N_Bare_Compilation_Unit.P_Has_Restriction (Name));
end;
when Compilation_Unit_P_All_Config_Pragmas =>
Result := Create_Pragma_Node_Array (N_Bare_Compilation_Unit.P_All_Config_Pragmas);
when Compilation_Unit_P_Config_Pragmas =>
declare
Name : constant Unbounded_Text_Type :=
As_Unbounded_Text (Arguments (Arguments'First + 0));
begin
Result := Create_Pragma_Node_Array (N_Bare_Compilation_Unit.P_Config_Pragmas (Name));
end;
when others => null;
end case;
end;
when Ada_Constant_Node =>
declare
N_Bare_Constant_Node : constant Analysis.Constant_Node := Node.As_Constant_Node;
begin
case Property is
when Constant_Node_P_As_Bool =>
Result := Create_Boolean (N_Bare_Constant_Node.P_As_Bool);
when others => null;
end case;
end;
when Ada_Composite_Constraint_Range =>
declare
N_Bare_Composite_Constraint : constant Analysis.Composite_Constraint := Node.As_Composite_Constraint;
begin
case Property is
when Composite_Constraint_P_Is_Index_Constraint =>
Result := Create_Boolean (N_Bare_Composite_Constraint.P_Is_Index_Constraint);
when Composite_Constraint_P_Is_Discriminant_Constraint =>
Result := Create_Boolean (N_Bare_Composite_Constraint.P_Is_Discriminant_Constraint);
when others => null;
end case;
end;
when Ada_Expr =>
declare
N_Bare_Expr : constant Analysis.Expr := Node.As_Expr;
begin
case Property is
when Expr_P_Expression_Type =>
Result := Create_Node (N_Bare_Expr.P_Expression_Type);
when Expr_P_Expected_Expression_Type =>
Result := Create_Node (N_Bare_Expr.P_Expected_Expression_Type);
when Expr_P_Is_Dynamically_Tagged =>
declare
Imprecise_Fallback : constant Boolean :=
As_Boolean (Arguments (Arguments'First + 0));
begin
Result := Create_Boolean (N_Bare_Expr.P_Is_Dynamically_Tagged (Imprecise_Fallback));
end;
when Expr_P_Is_Dispatching_Call =>
declare
Imprecise_Fallback : constant Boolean :=
As_Boolean (Arguments (Arguments'First + 0));
begin
Result := Create_Boolean (N_Bare_Expr.P_Is_Dispatching_Call (Imprecise_Fallback));
end;
when Expr_P_Is_Static_Expr =>
declare
Imprecise_Fallback : constant Boolean :=
As_Boolean (Arguments (Arguments'First + 0));
begin
Result := Create_Boolean (N_Bare_Expr.P_Is_Static_Expr (Imprecise_Fallback));
end;
when Expr_P_First_Corresponding_Decl =>
Result := Create_Node (N_Bare_Expr.P_First_Corresponding_Decl);
when Expr_P_Eval_As_Int =>
Result := Create_Big_Integer (N_Bare_Expr.P_Eval_As_Int);
when Expr_P_Eval_As_Int_In_Env =>
declare
Env : constant Substitution_Array :=
As_Substitution_Array (Arguments (Arguments'First + 0));
begin
Result := Create_Big_Integer (N_Bare_Expr.P_Eval_As_Int_In_Env (Env));
end;
when Expr_P_Eval_As_String =>
Result := Create_String (N_Bare_Expr.P_Eval_As_String);
when Expr_P_Eval_As_String_In_Env =>
declare
Env : constant Substitution_Array :=
As_Substitution_Array (Arguments (Arguments'First + 0));
begin
Result := Create_String (N_Bare_Expr.P_Eval_As_String_In_Env (Env));
end;
when Expr_P_Matching_Nodes =>
Result := Create_Ada_Node_Array (N_Bare_Expr.P_Matching_Nodes);
when others => null;
end case;
case Ada_Expr (Kind) is
when Ada_Allocator_Range =>
declare
N_Bare_Allocator : constant Analysis.Allocator := N_Bare_Expr.As_Allocator;
begin
case Property is
when Allocator_P_Get_Allocated_Type =>
Result := Create_Node (N_Bare_Allocator.P_Get_Allocated_Type);
when others => null;
end case;
end;
when Ada_Base_Aggregate =>
declare
N_Bare_Base_Aggregate : constant Analysis.Base_Aggregate := N_Bare_Expr.As_Base_Aggregate;
begin
case Property is
when Base_Aggregate_P_Aggregate_Params =>
Result := Create_Param_Actual_Array (N_Bare_Base_Aggregate.P_Aggregate_Params);
when Base_Aggregate_P_Is_Subaggregate =>
Result := Create_Boolean (N_Bare_Base_Aggregate.P_Is_Subaggregate);
when others => null;
end case;
end;
when Ada_Concat_Op_Range =>
declare
N_Bare_Concat_Op : constant Analysis.Concat_Op := N_Bare_Expr.As_Concat_Op;
begin
case Property is
when Concat_Op_P_Operands =>
Result := Create_Expr_Array (N_Bare_Concat_Op.P_Operands);
when others => null;
end case;
end;
when Ada_Cond_Expr =>
declare
N_Bare_Cond_Expr : constant Analysis.Cond_Expr := N_Bare_Expr.As_Cond_Expr;
begin
case Property is
when Cond_Expr_P_Dependent_Exprs =>
Result := Create_Expr_Array (N_Bare_Cond_Expr.P_Dependent_Exprs);
when others => null;
end case;
end;
when Ada_Name =>
declare
N_Bare_Name : constant Analysis.Name := N_Bare_Expr.As_Name;
begin
case Property is
when Name_P_Enclosing_Defining_Name =>
Result := Create_Node (N_Bare_Name.P_Enclosing_Defining_Name);
when Name_P_Is_Defining =>
Result := Create_Boolean (N_Bare_Name.P_Is_Defining);
when Name_P_Name_Is =>
declare
Sym : constant Unbounded_Text_Type :=
As_Unbounded_Text (Arguments (Arguments'First + 0));
begin
Result := Create_Boolean (N_Bare_Name.P_Name_Is (Sym));
end;
when Name_P_Is_Direct_Call =>
Result := Create_Boolean (N_Bare_Name.P_Is_Direct_Call);
when Name_P_Is_Access_Call =>
Result := Create_Boolean (N_Bare_Name.P_Is_Access_Call);
when Name_P_Is_Call =>
Result := Create_Boolean (N_Bare_Name.P_Is_Call);
when Name_P_Is_Dot_Call =>
declare
Imprecise_Fallback : constant Boolean :=
As_Boolean (Arguments (Arguments'First + 0));
begin
Result := Create_Boolean (N_Bare_Name.P_Is_Dot_Call (Imprecise_Fallback));
end;
when Name_P_Failsafe_Referenced_Def_Name =>
declare
Imprecise_Fallback : constant Boolean :=
As_Boolean (Arguments (Arguments'First + 0));
begin
Result := Create_Refd_Def (N_Bare_Name.P_Failsafe_Referenced_Def_Name (Imprecise_Fallback));
end;
when Name_P_Referenced_Defining_Name =>
declare
Imprecise_Fallback : constant Boolean :=
As_Boolean (Arguments (Arguments'First + 0));
begin
Result := Create_Node (N_Bare_Name.P_Referenced_Defining_Name (Imprecise_Fallback));
end;
when Name_P_All_Env_Elements =>
declare
Seq : constant Boolean :=
As_Boolean (Arguments (Arguments'First + 0));
Seq_From : constant Ada_Node :=
As_Node (Arguments (Arguments'First + 1));
begin
Result := Create_Ada_Node_Array (N_Bare_Name.P_All_Env_Elements (Seq, Seq_From));
end;
when Name_P_Called_Subp_Spec =>
Result := Create_Node (N_Bare_Name.P_Called_Subp_Spec);
when Name_P_Referenced_Decl =>
declare
Imprecise_Fallback : constant Boolean :=
As_Boolean (Arguments (Arguments'First + 0));
begin
Result := Create_Node (N_Bare_Name.P_Referenced_Decl (Imprecise_Fallback));
end;
when Name_P_Failsafe_Referenced_Decl =>
declare
Imprecise_Fallback : constant Boolean :=
As_Boolean (Arguments (Arguments'First + 0));
begin
Result := Create_Refd_Decl (N_Bare_Name.P_Failsafe_Referenced_Decl (Imprecise_Fallback));
end;
when Name_P_Referenced_Decl_Internal =>
declare
Imprecise_Fallback : constant Boolean :=
As_Boolean (Arguments (Arguments'First + 0));
begin
Result := Create_Refd_Decl (N_Bare_Name.P_Referenced_Decl_Internal (Imprecise_Fallback));
end;
when Name_P_Name_Designated_Type =>
Result := Create_Node (N_Bare_Name.P_Name_Designated_Type);
when Name_P_Is_Static_Subtype =>
declare
Imprecise_Fallback : constant Boolean :=
As_Boolean (Arguments (Arguments'First + 0));
begin
Result := Create_Boolean (N_Bare_Name.P_Is_Static_Subtype (Imprecise_Fallback));
end;
when Name_P_Name_Matches =>
declare
N : constant Name :=
As_Node (Arguments (Arguments'First + 0)).As_Name;
begin
Result := Create_Boolean (N_Bare_Name.P_Name_Matches (N));
end;
when Name_P_Relative_Name =>
Result := Create_Node (N_Bare_Name.P_Relative_Name);
when Name_P_Is_Operator_Name =>
Result := Create_Boolean (N_Bare_Name.P_Is_Operator_Name);
when Name_P_Is_Write_Reference =>
declare
Imprecise_Fallback : constant Boolean :=
As_Boolean (Arguments (Arguments'First + 0));
begin
Result := Create_Boolean (N_Bare_Name.P_Is_Write_Reference (Imprecise_Fallback));
end;
when Name_P_Is_Static_Call =>
declare
Imprecise_Fallback : constant Boolean :=
As_Boolean (Arguments (Arguments'First + 0));
begin
Result := Create_Boolean (N_Bare_Name.P_Is_Static_Call (Imprecise_Fallback));
end;
when Name_P_As_Symbol_Array =>
Result := Create_Unbounded_Text_Type_Array (N_Bare_Name.P_As_Symbol_Array);
when Name_P_Canonical_Text =>
Result := Create_Unbounded_Text (N_Bare_Name.P_Canonical_Text);
when Name_P_Is_Constant =>
Result := Create_Boolean (N_Bare_Name.P_Is_Constant);
when Name_P_Call_Params =>
Result := Create_Param_Actual_Array (N_Bare_Name.P_Call_Params);
when others => null;
end case;
case Ada_Name (Kind) is
when Ada_Call_Expr_Range =>
declare
N_Bare_Call_Expr : constant Analysis.Call_Expr := N_Bare_Name.As_Call_Expr;
begin
case Property is
when Call_Expr_P_Kind =>
Result := Create_Call_Expr_Kind (N_Bare_Call_Expr.P_Kind);
when Call_Expr_P_Is_Array_Slice =>
Result := Create_Boolean (N_Bare_Call_Expr.P_Is_Array_Slice);
when others => null;
end case;
end;
when Ada_Defining_Name_Range =>
declare
N_Bare_Defining_Name : constant Analysis.Defining_Name := N_Bare_Name.As_Defining_Name;
begin
case Property is
when Defining_Name_P_Canonical_Fully_Qualified_Name =>
Result := Create_String (N_Bare_Defining_Name.P_Canonical_Fully_Qualified_Name);
when Defining_Name_P_Unique_Identifying_Name =>
Result := Create_String (N_Bare_Defining_Name.P_Unique_Identifying_Name);
when Defining_Name_P_Fully_Qualified_Name_Array =>
Result := Create_Unbounded_Text_Type_Array (N_Bare_Defining_Name.P_Fully_Qualified_Name_Array);
when Defining_Name_P_Fully_Qualified_Name =>
Result := Create_String (N_Bare_Defining_Name.P_Fully_Qualified_Name);
when Defining_Name_P_Basic_Decl =>
Result := Create_Node (N_Bare_Defining_Name.P_Basic_Decl);
when Defining_Name_P_Find_Refs =>
declare
Root : constant Ada_Node :=
As_Node (Arguments (Arguments'First + 0));
Imprecise_Fallback : constant Boolean :=
As_Boolean (Arguments (Arguments'First + 1));
begin
Result := Create_Ref_Result_Array (N_Bare_Defining_Name.P_Find_Refs (Root, Imprecise_Fallback));
end;
when Defining_Name_P_Find_All_References =>
declare
Units : constant Analysis_Unit_Array :=
As_Analysis_Unit_Array (Arguments (Arguments'First + 0));
Follow_Renamings : constant Boolean :=
As_Boolean (Arguments (Arguments'First + 1));
Imprecise_Fallback : constant Boolean :=
As_Boolean (Arguments (Arguments'First + 2));
begin
Result := Create_Ref_Result_Array (N_Bare_Defining_Name.P_Find_All_References (Units, Follow_Renamings, Imprecise_Fallback));
end;
when Defining_Name_P_Find_All_Calls =>
declare
Units : constant Analysis_Unit_Array :=
As_Analysis_Unit_Array (Arguments (Arguments'First + 0));
Follow_Renamings : constant Boolean :=
As_Boolean (Arguments (Arguments'First + 1));
Imprecise_Fallback : constant Boolean :=
As_Boolean (Arguments (Arguments'First + 2));
begin
Result := Create_Ref_Result_Array (N_Bare_Defining_Name.P_Find_All_Calls (Units, Follow_Renamings, Imprecise_Fallback));
end;
when Defining_Name_P_Next_Part =>
declare
Imprecise_Fallback : constant Boolean :=
As_Boolean (Arguments (Arguments'First + 0));
begin
Result := Create_Node (N_Bare_Defining_Name.P_Next_Part (Imprecise_Fallback));
end;
when Defining_Name_P_Previous_Part =>
declare
Imprecise_Fallback : constant Boolean :=
As_Boolean (Arguments (Arguments'First + 0));
begin
Result := Create_Node (N_Bare_Defining_Name.P_Previous_Part (Imprecise_Fallback));
end;
when Defining_Name_P_Canonical_Part =>
declare
Imprecise_Fallback : constant Boolean :=
As_Boolean (Arguments (Arguments'First + 0));
begin
Result := Create_Node (N_Bare_Defining_Name.P_Canonical_Part (Imprecise_Fallback));
end;
when Defining_Name_P_Most_Visible_Part =>
declare
Origin : constant Ada_Node :=
As_Node (Arguments (Arguments'First + 0));
Imprecise_Fallback : constant Boolean :=
As_Boolean (Arguments (Arguments'First + 1));
begin
Result := Create_Node (N_Bare_Defining_Name.P_Most_Visible_Part (Origin, Imprecise_Fallback));
end;
when Defining_Name_P_All_Parts =>
declare
Imprecise_Fallback : constant Boolean :=
As_Boolean (Arguments (Arguments'First + 0));
begin
Result := Create_Defining_Name_Array (N_Bare_Defining_Name.P_All_Parts (Imprecise_Fallback));
end;
when Defining_Name_P_Get_Aspect =>
declare
Name : constant Unbounded_Text_Type :=
As_Unbounded_Text (Arguments (Arguments'First + 0));
Imprecise_Fallback : constant Boolean :=
As_Boolean (Arguments (Arguments'First + 1));
begin
Result := Create_Aspect (N_Bare_Defining_Name.P_Get_Aspect (Name, Imprecise_Fallback));
end;
when Defining_Name_P_Has_Aspect =>
declare
Name : constant Unbounded_Text_Type :=
As_Unbounded_Text (Arguments (Arguments'First + 0));
Imprecise_Fallback : constant Boolean :=
As_Boolean (Arguments (Arguments'First + 1));
begin
Result := Create_Boolean (N_Bare_Defining_Name.P_Has_Aspect (Name, Imprecise_Fallback));
end;
when Defining_Name_P_Get_Pragma =>
declare
Name : constant Unbounded_Text_Type :=
As_Unbounded_Text (Arguments (Arguments'First + 0));
begin
Result := Create_Node (N_Bare_Defining_Name.P_Get_Pragma (Name));
end;
when Defining_Name_P_Get_Representation_Clause =>
declare
Name : constant Unbounded_Text_Type :=
As_Unbounded_Text (Arguments (Arguments'First + 0));
Imprecise_Fallback : constant Boolean :=
As_Boolean (Arguments (Arguments'First + 1));
begin
Result := Create_Node (N_Bare_Defining_Name.P_Get_Representation_Clause (Name, Imprecise_Fallback));
end;
when Defining_Name_P_Get_At_Clause =>
declare
Imprecise_Fallback : constant Boolean :=
As_Boolean (Arguments (Arguments'First + 0));
begin
Result := Create_Node (N_Bare_Defining_Name.P_Get_At_Clause (Imprecise_Fallback));
end;
when Defining_Name_P_Is_Imported =>
Result := Create_Boolean (N_Bare_Defining_Name.P_Is_Imported);
when Defining_Name_P_Is_Ghost_Code =>
Result := Create_Boolean (N_Bare_Defining_Name.P_Is_Ghost_Code);
when others => null;
end case;
end;
when Ada_End_Name_Range =>
declare
N_Bare_End_Name : constant Analysis.End_Name := N_Bare_Name.As_End_Name;
begin
case Property is
when End_Name_P_Basic_Decl =>
Result := Create_Node (N_Bare_End_Name.P_Basic_Decl);
when others => null;
end case;
end;
when Ada_Char_Literal_Range =>
declare
N_Bare_Char_Literal : constant Analysis.Char_Literal := N_Bare_Name.As_Char_Literal;
begin
case Property is
when Char_Literal_P_Denoted_Value =>
Result := Create_Character (N_Bare_Char_Literal.P_Denoted_Value);
when others => null;
end case;
end;
when Ada_String_Literal_Range =>
declare
N_Bare_String_Literal : constant Analysis.String_Literal := N_Bare_Name.As_String_Literal;
begin
case Property is
when String_Literal_P_Denoted_Value =>
Result := Create_String (N_Bare_String_Literal.P_Denoted_Value);
when others => null;
end case;
end;
when Ada_Int_Literal_Range =>
declare
N_Bare_Int_Literal : constant Analysis.Int_Literal := N_Bare_Name.As_Int_Literal;
begin
case Property is
when Int_Literal_P_Denoted_Value =>
Result := Create_Big_Integer (N_Bare_Int_Literal.P_Denoted_Value);
when others => null;
end case;
end;
when others => null;
end case;
end;
when others => null;
end case;
end;
when Ada_Limited_Node =>
declare
N_Bare_Limited_Node : constant Analysis.Limited_Node := Node.As_Limited_Node;
begin
case Property is
when Limited_Node_P_As_Bool =>
Result := Create_Boolean (N_Bare_Limited_Node.P_As_Bool);
when others => null;
end case;
end;
when Ada_Not_Null =>
declare
N_Bare_Not_Null : constant Analysis.Not_Null := Node.As_Not_Null;
begin
case Property is
when Not_Null_P_As_Bool =>
Result := Create_Boolean (N_Bare_Not_Null.P_As_Bool);
when others => null;
end case;
end;
when Ada_Pragma_Node_Range =>
declare
N_Bare_Pragma_Node : constant Analysis.Pragma_Node := Node.As_Pragma_Node;
begin
case Property is
when Pragma_Node_P_Is_Ghost_Code =>
Result := Create_Boolean (N_Bare_Pragma_Node.P_Is_Ghost_Code);
when Pragma_Node_P_Associated_Entities =>
Result := Create_Defining_Name_Array (N_Bare_Pragma_Node.P_Associated_Entities);
when others => null;
end case;
end;
when Ada_Private_Node =>
declare
N_Bare_Private_Node : constant Analysis.Private_Node := Node.As_Private_Node;
begin
case Property is
when Private_Node_P_As_Bool =>
Result := Create_Boolean (N_Bare_Private_Node.P_As_Bool);
when others => null;
end case;
end;
when Ada_Protected_Node =>
declare
N_Bare_Protected_Node : constant Analysis.Protected_Node := Node.As_Protected_Node;
begin
case Property is
when Protected_Node_P_As_Bool =>
Result := Create_Boolean (N_Bare_Protected_Node.P_As_Bool);
when others => null;
end case;
end;
when Ada_Reverse_Node =>
declare
N_Bare_Reverse_Node : constant Analysis.Reverse_Node := Node.As_Reverse_Node;
begin
case Property is
when Reverse_Node_P_As_Bool =>
Result := Create_Boolean (N_Bare_Reverse_Node.P_As_Bool);
when others => null;
end case;
end;
when Ada_Stmt =>
declare
N_Bare_Stmt : constant Analysis.Stmt := Node.As_Stmt;
begin
case Property is
when Stmt_P_Is_Ghost_Code =>
Result := Create_Boolean (N_Bare_Stmt.P_Is_Ghost_Code);
when others => null;
end case;
case Ada_Stmt (Kind) is
when Ada_Accept_Stmt_Range =>
declare
N_Bare_Accept_Stmt : constant Analysis.Accept_Stmt := N_Bare_Stmt.As_Accept_Stmt;
begin
case Property is
when Accept_Stmt_P_Corresponding_Entry =>
declare
Origin : constant Ada_Node :=
As_Node (Arguments (Arguments'First + 0));
begin
Result := Create_Node (N_Bare_Accept_Stmt.P_Corresponding_Entry (Origin));
end;
when others => null;
end case;
end;
when others => null;
end case;
end;
when Ada_Subunit_Range =>
declare
N_Bare_Subunit : constant Analysis.Subunit := Node.As_Subunit;
begin
case Property is
when Subunit_P_Body_Root =>
Result := Create_Node (N_Bare_Subunit.P_Body_Root);
when others => null;
end case;
end;
when Ada_Synchronized_Node =>
declare
N_Bare_Synchronized_Node : constant Analysis.Synchronized_Node := Node.As_Synchronized_Node;
begin
case Property is
when Synchronized_Node_P_As_Bool =>
Result := Create_Boolean (N_Bare_Synchronized_Node.P_As_Bool);
when others => null;
end case;
end;
when Ada_Tagged_Node =>
declare
N_Bare_Tagged_Node : constant Analysis.Tagged_Node := Node.As_Tagged_Node;
begin
case Property is
when Tagged_Node_P_As_Bool =>
Result := Create_Boolean (N_Bare_Tagged_Node.P_As_Bool);
when others => null;
end case;
end;
when Ada_Type_Expr =>
declare
N_Bare_Type_Expr : constant Analysis.Type_Expr := Node.As_Type_Expr;
begin
case Property is
when Type_Expr_P_Type_Name =>
Result := Create_Node (N_Bare_Type_Expr.P_Type_Name);
when Type_Expr_P_Designated_Type_Decl =>
Result := Create_Node (N_Bare_Type_Expr.P_Designated_Type_Decl);
when Type_Expr_P_Designated_Type_Decl_From =>
declare
Origin_Node : constant Ada_Node :=
As_Node (Arguments (Arguments'First + 0));
begin
Result := Create_Node (N_Bare_Type_Expr.P_Designated_Type_Decl_From (Origin_Node));
end;
when others => null;
end case;
case Ada_Type_Expr (Kind) is
when Ada_Subtype_Indication_Range =>
declare
N_Bare_Subtype_Indication : constant Analysis.Subtype_Indication := N_Bare_Type_Expr.As_Subtype_Indication;
begin
case Property is
when Subtype_Indication_P_Subtype_Constraints =>
Result := Create_Param_Actual_Array (N_Bare_Subtype_Indication.P_Subtype_Constraints);
when Subtype_Indication_P_Is_Static_Subtype =>
declare
Imprecise_Fallback : constant Boolean :=
As_Boolean (Arguments (Arguments'First + 0));
begin
Result := Create_Boolean (N_Bare_Subtype_Indication.P_Is_Static_Subtype (Imprecise_Fallback));
end;
when others => null;
end case;
end;
when others => null;
end case;
end;
when Ada_Until_Node =>
declare
N_Bare_Until_Node : constant Analysis.Until_Node := Node.As_Until_Node;
begin
case Property is
when Until_Node_P_As_Bool =>
Result := Create_Boolean (N_Bare_Until_Node.P_As_Bool);
when others => null;
end case;
end;
when Ada_With_Private =>
declare
N_Bare_With_Private : constant Analysis.With_Private := Node.As_With_Private;
begin
case Property is
when With_Private_P_As_Bool =>
Result := Create_Boolean (N_Bare_With_Private.P_As_Bool);
when others => null;
end case;
end;
when others => null;
end case;

      if Result = No_Value then
         raise Bad_Type_Error with "no such field on this node";
      end if;
      return Result;
   end Eval_Property;

   ----------------
   -- Properties --
   ----------------

   function Properties (Kind : Ada_Node_Kind_Type) return Property_Reference_Array
   is
   begin
      return Impl.Properties (Kind);
   end Properties;

   ----------------
   -- Properties --
   ----------------

   function Properties (Id : Node_Type_Id) return Property_Reference_Array is
   begin
      return Impl.Properties (Id);
   end Properties;

   ---------------------
   -- Token_Node_Kind --
   ---------------------

   function Token_Node_Kind (Kind : Ada_Node_Kind_Type) return Token_Kind is
   begin
      return Impl.Token_Node_Kind (Kind);
   end Token_Node_Kind;

end Libadalang.Introspection;
