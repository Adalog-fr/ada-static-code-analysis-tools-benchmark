--
--  Copyright (C) 2014-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Unchecked_Conversion;

with Libadalang.Common;

with Libadalang.Public_Converters; use Libadalang.Public_Converters;
with Libadalang.Rewriting_Implementation;

package body Libadalang.Rewriting is

   package Impl renames Libadalang.Rewriting_Implementation;

   pragma Warnings (Off, "possible aliasing problem for type");
   function Unwrap_RH is new Ada.Unchecked_Conversion
     (Rewriting_Handle, Impl.Rewriting_Handle);
   function Wrap_RH is new Ada.Unchecked_Conversion
     (Impl.Rewriting_Handle, Rewriting_Handle);

   function Unwrap_Node_RH is new Ada.Unchecked_Conversion
     (Node_Rewriting_Handle, Impl.Node_Rewriting_Handle);
   function Wrap_Node_RH is new Ada.Unchecked_Conversion
     (Impl.Node_Rewriting_Handle, Node_Rewriting_Handle);

   function Unwrap_Unit_RH is new Ada.Unchecked_Conversion
     (Unit_Rewriting_Handle, Impl.Unit_Rewriting_Handle);
   function Wrap_Unit_RH is new Ada.Unchecked_Conversion
     (Impl.Unit_Rewriting_Handle, Unit_Rewriting_Handle);
   pragma Warnings (On, "possible aliasing problem for type");

   function Wrap_Apply_Result
     (Res : Impl.Apply_Result) return Apply_Result;

   function Wrap_Unit_RH_Array
     (Arr : Impl.Unit_Rewriting_Handle_Array)
      return Unit_Rewriting_Handle_Array;

   function Unwrap_Node_RH_Array
     (Arr : Node_Rewriting_Handle_Array)
      return Impl.Node_Rewriting_Handle_Array;

   function Wrap_Apply_Result
     (Res : Impl.Apply_Result) return Apply_Result is
   begin
      if Res.Success then
         return (Success => True);
      else
         return
           (Success     => False,
            Unit        => Wrap_Unit (Res.Unit),
            Diagnostics => Res.Diagnostics);
      end if;
   end Wrap_Apply_Result;

   ------------------------
   -- Wrap_Unit_RH_Array --
   ------------------------

   function Wrap_Unit_RH_Array
     (Arr : Impl.Unit_Rewriting_Handle_Array)
      return Unit_Rewriting_Handle_Array
   is
      Res : Unit_Rewriting_Handle_Array (Arr'Range);
   begin
      for I in Arr'Range loop
         Res (I) := Wrap_Unit_RH (Arr (I));
      end loop;
      return Res;
   end Wrap_Unit_RH_Array;

   --------------------------
   -- Unwrap_Node_RH_Array --
   --------------------------

   function Unwrap_Node_RH_Array
     (Arr : Node_Rewriting_Handle_Array)
      return Impl.Node_Rewriting_Handle_Array
   is
      Res : Impl.Node_Rewriting_Handle_Array (Arr'Range);
   begin
      for I in Arr'Range loop
         Res (I) := Unwrap_Node_RH (Arr (I));
      end loop;
      return Res;
   end Unwrap_Node_RH_Array;

   ------------
   -- Handle --
   ------------

   function Handle (Context : Analysis_Context) return Rewriting_Handle is
   begin
      return Wrap_RH (Impl.Handle (Unwrap_Context (Context)));
   end Handle;

   -------------
   -- Context --
   -------------

   function Context (Handle : Rewriting_Handle) return Analysis_Context is
   begin
      return Wrap_Context (Impl.Context (Unwrap_RH (Handle)));
   end Context;

   ---------------------
   -- Start_Rewriting --
   ---------------------

   function Start_Rewriting
     (Context : Analysis_Context) return Rewriting_Handle is
   begin
      return Wrap_RH (Impl.Start_Rewriting (Unwrap_Context (Context)));
   end Start_Rewriting;

   ---------------------
   -- Abort_Rewriting --
   ---------------------

   procedure Abort_Rewriting
     (Handle          : in out Rewriting_Handle)
   is
      Internal_Handle : Impl.Rewriting_Handle := Unwrap_RH (Handle);
   begin
      Impl.Abort_Rewriting (Internal_Handle);
      Handle := Wrap_RH (Internal_Handle);
   end Abort_Rewriting;

   -----------
   -- Apply --
   -----------

   function Apply (Handle : in out Rewriting_Handle) return Apply_Result is
      Internal_Handle : Impl.Rewriting_Handle := Unwrap_RH (Handle);
      Res             : Impl.Apply_Result := Impl.Apply (Internal_Handle);
   begin
      Handle := Wrap_RH (Internal_Handle);
      return Wrap_Apply_Result (Res);
   end Apply;

   ------------------
   -- Unit_Handles --
   ------------------

   function Unit_Handles
     (Handle : Rewriting_Handle) return Unit_Rewriting_Handle_Array is
   begin
      return Wrap_Unit_RH_Array (Impl.Unit_Handles (Unwrap_RH (Handle)));
   end Unit_Handles;

   ------------
   -- Handle --
   ------------

   function Handle (Unit : Analysis_Unit) return Unit_Rewriting_Handle is
   begin
      return Wrap_Unit_RH (Impl.Handle (Unwrap_Unit (Unit)));
   end Handle;

   ----------
   -- Unit --
   ----------

   function Unit (Handle : Unit_Rewriting_Handle) return Analysis_Unit is
   begin
      return Wrap_Unit (Impl.Unit (Unwrap_Unit_RH (Handle)));
   end Unit;

   ----------
   -- Root --
   ----------

   function Root (Handle : Unit_Rewriting_Handle) return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH (Impl.Root (Unwrap_Unit_RH (Handle)));
   end Root;

   --------------
   -- Set_Root --
   --------------

   procedure Set_Root
     (Handle : Unit_Rewriting_Handle;
      Root   : Node_Rewriting_Handle) is
   begin
      Impl.Set_Root (Unwrap_Unit_RH (Handle), Unwrap_Node_RH (Root));
   end Set_Root;

   -------------
   -- Unparse --
   -------------

   function Unparse
     (Handle : Unit_Rewriting_Handle) return Unbounded_Text_Type is
   begin
      return Impl.Unparse (Unwrap_Unit_RH (Handle));
   end Unparse;

   ------------
   -- Handle --
   ------------

   function Handle
     (Node : Ada_Node'Class) return Node_Rewriting_Handle is
   begin
      return Wrap_Node_RH (Impl.Handle (Unwrap_Node (Node)));
   end Handle;

   ----------
   -- Node --
   ----------

   function Node
     (Handle : Node_Rewriting_Handle) return Ada_Node is
   begin
      return Wrap_Node (Impl.Node (Unwrap_Node_RH (Handle)));
   end Node;

   -------------
   -- Context --
   -------------

   function Context (Handle : Node_Rewriting_Handle) return Rewriting_Handle is
   begin
      return Wrap_RH (Impl.Context (Unwrap_Node_RH (Handle)));
   end Context;

   -------------
   -- Unparse --
   -------------

   function Unparse (Handle : Node_Rewriting_Handle) return Text_Type is
   begin
      return Impl.Unparse (Unwrap_Node_RH (Handle));
   end Unparse;

   ----------
   -- Kind --
   ----------

   function Kind (Handle : Node_Rewriting_Handle) return Ada_Node_Kind_Type is
   begin
      return Impl.Kind (Unwrap_Node_RH (Handle));
   end Kind;

   ----------
   -- Tied --
   ----------

   function Tied (Handle : Node_Rewriting_Handle) return Boolean is
   begin
      return Impl.Tied (Unwrap_Node_RH (Handle));
   end Tied;

   ------------
   -- Parent --
   ------------

   function Parent
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle is
   begin
      return Wrap_Node_RH (Impl.Parent (Unwrap_Node_RH (Handle)));
   end Parent;

   --------------------
   -- Children_Count --
   --------------------

   function Children_Count (Handle : Node_Rewriting_Handle) return Natural is
   begin
      return Impl.Children_Count (Unwrap_Node_RH (Handle));
   end Children_Count;

   -----------
   -- Child --
   -----------

   function Child
     (Handle : Node_Rewriting_Handle;
      Index  : Positive) return Node_Rewriting_Handle is
   begin
      return Wrap_Node_RH (Impl.Child (Unwrap_Node_RH (Handle), Index));
   end Child;

   ---------------
   -- Set_Child --
   ---------------

   procedure Set_Child
     (Handle : Node_Rewriting_Handle;
      Index  : Positive;
      Child  : Node_Rewriting_Handle)
   is
   begin
      Impl.Set_Child (Unwrap_Node_RH (Handle), Index, Unwrap_Node_RH (Child));
   end Set_Child;

   ----------
   -- Text --
   ----------

   function Text (Handle : Node_Rewriting_Handle) return Text_Type is
   begin
      return Impl.Text (Unwrap_Node_RH (Handle));
   end Text;

   --------------
   -- Set_Text --
   --------------

   procedure Set_Text (Handle : Node_Rewriting_Handle; Text : Text_Type) is
   begin
      Impl.Set_Text (Unwrap_Node_RH (Handle), Text);
   end Set_Text;

   -------------
   -- Replace --
   -------------

   procedure Replace (Handle, New_Node : Node_Rewriting_Handle) is
   begin
      Impl.Replace (Unwrap_Node_RH (Handle), Unwrap_Node_RH (New_Node));
   end Replace;

   ------------------
   -- Insert_Child --
   ------------------

   procedure Insert_Child
     (Handle : Node_Rewriting_Handle;
      Index  : Positive;
      Child  : Node_Rewriting_Handle) is
   begin
      Impl.Insert_Child
        (Unwrap_Node_RH (Handle), Index, Unwrap_Node_RH (Child));
   end Insert_Child;

   ------------------
   -- Append_Child --
   ------------------

   procedure Append_Child
     (Handle : Node_Rewriting_Handle;
      Child  : Node_Rewriting_Handle) is
   begin
      Impl.Append_Child (Unwrap_Node_RH (Handle), Unwrap_Node_RH (Child));
   end Append_Child;

   ------------------
   -- Remove_Child --
   ------------------

   procedure Remove_Child
     (Handle : Node_Rewriting_Handle;
      Index  : Positive) is
   begin
      Impl.Remove_Child (Unwrap_Node_RH (Handle), Index);
   end Remove_Child;

   -----------
   -- Clone --
   -----------

   function Clone
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle
   is
   begin
      return Wrap_Node_RH (Impl.Clone (Unwrap_Node_RH (Handle)));
   end Clone;

   -----------------
   -- Create_Node --
   -----------------

   function Create_Node
     (Handle : Rewriting_Handle;
      Kind   : Ada_Node_Kind_Type) return Node_Rewriting_Handle is
   begin
      return Wrap_Node_RH (Impl.Create_Node (Unwrap_RH (Handle), Kind));
   end Create_Node;

   -----------------------
   -- Create_Token_Node --
   -----------------------

   function Create_Token_Node
     (Handle : Rewriting_Handle;
      Kind   : Ada_Node_Kind_Type;
      Text   : Text_Type) return Node_Rewriting_Handle is
   begin
      return Wrap_Node_RH
        (Impl.Create_Token_Node (Unwrap_RH (Handle), Kind, Text));
   end Create_Token_Node;

   -------------------------
   -- Create_Regular_Node --
   -------------------------

   function Create_Regular_Node
     (Handle   : Rewriting_Handle;
      Kind     : Ada_Node_Kind_Type;
      Children : Node_Rewriting_Handle_Array) return Node_Rewriting_Handle is
   begin
      return Wrap_Node_RH (Impl.Create_Regular_Node
        (Unwrap_RH (Handle), Kind, Unwrap_Node_RH_Array (Children)));
   end Create_Regular_Node;

   --------------------------
   -- Create_From_Template --
   --------------------------

   function Create_From_Template
     (Handle    : Rewriting_Handle;
      Template  : Text_Type;
      Arguments : Node_Rewriting_Handle_Array;
      Rule      : Grammar_Rule) return Node_Rewriting_Handle is
   begin
      return Wrap_Node_RH (Impl.Create_From_Template
        (Unwrap_RH (Handle),
         Template,
         Unwrap_Node_RH_Array (Arguments),
         Rule));
   end Create_From_Template;


         function Create_Constrained_Array_Indices
           (Handle : Rewriting_Handle
               ; F_List : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Constrained_Array_Indices
               (Unwrap_RH (Handle),
                Constrained_Array_Indices_F_List => Unwrap_Node_RH (F_List)));
         end;


         function Create_Unconstrained_Array_Indices
           (Handle : Rewriting_Handle
               ; F_Types : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Unconstrained_Array_Indices
               (Unwrap_RH (Handle),
                Unconstrained_Array_Indices_F_Types => Unwrap_Node_RH (F_Types)));
         end;


         function Create_Aspect_Assoc
           (Handle : Rewriting_Handle
               ; F_Id : Node_Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Aspect_Assoc
               (Unwrap_RH (Handle),
                Aspect_Assoc_F_Id => Unwrap_Node_RH (F_Id), Aspect_Assoc_F_Expr => Unwrap_Node_RH (F_Expr)));
         end;


         function Create_At_Clause
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_At_Clause
               (Unwrap_RH (Handle),
                At_Clause_F_Name => Unwrap_Node_RH (F_Name), At_Clause_F_Expr => Unwrap_Node_RH (F_Expr)));
         end;


         function Create_Attribute_Def_Clause
           (Handle : Rewriting_Handle
               ; F_Attribute_Expr : Node_Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Attribute_Def_Clause
               (Unwrap_RH (Handle),
                Attribute_Def_Clause_F_Attribute_Expr => Unwrap_Node_RH (F_Attribute_Expr), Attribute_Def_Clause_F_Expr => Unwrap_Node_RH (F_Expr)));
         end;


         function Create_Enum_Rep_Clause
           (Handle : Rewriting_Handle
               ; F_Type_Name : Node_Rewriting_Handle
               ; F_Aggregate : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Enum_Rep_Clause
               (Unwrap_RH (Handle),
                Enum_Rep_Clause_F_Type_Name => Unwrap_Node_RH (F_Type_Name), Enum_Rep_Clause_F_Aggregate => Unwrap_Node_RH (F_Aggregate)));
         end;


         function Create_Record_Rep_Clause
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_At_Expr : Node_Rewriting_Handle
               ; F_Components : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Record_Rep_Clause
               (Unwrap_RH (Handle),
                Record_Rep_Clause_F_Name => Unwrap_Node_RH (F_Name), Record_Rep_Clause_F_At_Expr => Unwrap_Node_RH (F_At_Expr), Record_Rep_Clause_F_Components => Unwrap_Node_RH (F_Components)));
         end;


         function Create_Aspect_Spec
           (Handle : Rewriting_Handle
               ; F_Aspect_Assocs : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Aspect_Spec
               (Unwrap_RH (Handle),
                Aspect_Spec_F_Aspect_Assocs => Unwrap_Node_RH (F_Aspect_Assocs)));
         end;


         function Create_Contract_Case_Assoc
           (Handle : Rewriting_Handle
               ; F_Guard : Node_Rewriting_Handle
               ; F_Consequence : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Contract_Case_Assoc
               (Unwrap_RH (Handle),
                Contract_Case_Assoc_F_Guard => Unwrap_Node_RH (F_Guard), Contract_Case_Assoc_F_Consequence => Unwrap_Node_RH (F_Consequence)));
         end;


         function Create_Pragma_Argument_Assoc
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Pragma_Argument_Assoc
               (Unwrap_RH (Handle),
                Pragma_Argument_Assoc_F_Name => Unwrap_Node_RH (F_Name), Pragma_Argument_Assoc_F_Expr => Unwrap_Node_RH (F_Expr)));
         end;


         function Create_Entry_Spec
           (Handle : Rewriting_Handle
               ; F_Entry_Name : Node_Rewriting_Handle
               ; F_Family_Type : Node_Rewriting_Handle
               ; F_Entry_Params : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Entry_Spec
               (Unwrap_RH (Handle),
                Entry_Spec_F_Entry_Name => Unwrap_Node_RH (F_Entry_Name), Entry_Spec_F_Family_Type => Unwrap_Node_RH (F_Family_Type), Entry_Spec_F_Entry_Params => Unwrap_Node_RH (F_Entry_Params)));
         end;


         function Create_Subp_Spec
           (Handle : Rewriting_Handle
               ; F_Subp_Kind : Node_Rewriting_Handle
               ; F_Subp_Name : Node_Rewriting_Handle
               ; F_Subp_Params : Node_Rewriting_Handle
               ; F_Subp_Returns : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Subp_Spec
               (Unwrap_RH (Handle),
                Subp_Spec_F_Subp_Kind => Unwrap_Node_RH (F_Subp_Kind), Subp_Spec_F_Subp_Name => Unwrap_Node_RH (F_Subp_Name), Subp_Spec_F_Subp_Params => Unwrap_Node_RH (F_Subp_Params), Subp_Spec_F_Subp_Returns => Unwrap_Node_RH (F_Subp_Returns)));
         end;


         function Create_Synthetic_Binary_Spec
           (Handle : Rewriting_Handle
               ; F_Left_Param : Node_Rewriting_Handle
               ; F_Right_Param : Node_Rewriting_Handle
               ; F_Return_Type_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Synthetic_Binary_Spec
               (Unwrap_RH (Handle),
                Synthetic_Binary_Spec_F_Left_Param => Unwrap_Node_RH (F_Left_Param), Synthetic_Binary_Spec_F_Right_Param => Unwrap_Node_RH (F_Right_Param), Synthetic_Binary_Spec_F_Return_Type_Expr => Unwrap_Node_RH (F_Return_Type_Expr)));
         end;


         function Create_Synthetic_Unary_Spec
           (Handle : Rewriting_Handle
               ; F_Right_Param : Node_Rewriting_Handle
               ; F_Return_Type_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Synthetic_Unary_Spec
               (Unwrap_RH (Handle),
                Synthetic_Unary_Spec_F_Right_Param => Unwrap_Node_RH (F_Right_Param), Synthetic_Unary_Spec_F_Return_Type_Expr => Unwrap_Node_RH (F_Return_Type_Expr)));
         end;


         function Create_Component_List
           (Handle : Rewriting_Handle
               ; F_Components : Node_Rewriting_Handle
               ; F_Variant_Part : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Component_List
               (Unwrap_RH (Handle),
                Component_List_F_Components => Unwrap_Node_RH (F_Components), Component_List_F_Variant_Part => Unwrap_Node_RH (F_Variant_Part)));
         end;


         function Create_Known_Discriminant_Part
           (Handle : Rewriting_Handle
               ; F_Discr_Specs : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Known_Discriminant_Part
               (Unwrap_RH (Handle),
                Known_Discriminant_Part_F_Discr_Specs => Unwrap_Node_RH (F_Discr_Specs)));
         end;


         function Create_Entry_Completion_Formal_Params
           (Handle : Rewriting_Handle
               ; F_Params : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Entry_Completion_Formal_Params
               (Unwrap_RH (Handle),
                Entry_Completion_Formal_Params_F_Params => Unwrap_Node_RH (F_Params)));
         end;


         function Create_Generic_Formal_Part
           (Handle : Rewriting_Handle
               ; F_Decls : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Generic_Formal_Part
               (Unwrap_RH (Handle),
                Generic_Formal_Part_F_Decls => Unwrap_Node_RH (F_Decls)));
         end;


         function Create_Null_Record_Def
           (Handle : Rewriting_Handle
               ; F_Components : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Null_Record_Def
               (Unwrap_RH (Handle),
                Base_Record_Def_F_Components => Unwrap_Node_RH (F_Components)));
         end;


         function Create_Record_Def
           (Handle : Rewriting_Handle
               ; F_Components : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Record_Def
               (Unwrap_RH (Handle),
                Base_Record_Def_F_Components => Unwrap_Node_RH (F_Components)));
         end;


         function Create_Aggregate_Assoc
           (Handle : Rewriting_Handle
               ; F_Designators : Node_Rewriting_Handle
               ; F_R_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Aggregate_Assoc
               (Unwrap_RH (Handle),
                Aggregate_Assoc_F_Designators => Unwrap_Node_RH (F_Designators), Aggregate_Assoc_F_R_Expr => Unwrap_Node_RH (F_R_Expr)));
         end;


         function Create_Multi_Dim_Array_Assoc
           (Handle : Rewriting_Handle
               ; F_Designators : Node_Rewriting_Handle
               ; F_R_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Multi_Dim_Array_Assoc
               (Unwrap_RH (Handle),
                Aggregate_Assoc_F_Designators => Unwrap_Node_RH (F_Designators), Aggregate_Assoc_F_R_Expr => Unwrap_Node_RH (F_R_Expr)));
         end;


         function Create_Composite_Constraint_Assoc
           (Handle : Rewriting_Handle
               ; F_Ids : Node_Rewriting_Handle
               ; F_Constraint_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Composite_Constraint_Assoc
               (Unwrap_RH (Handle),
                Composite_Constraint_Assoc_F_Ids => Unwrap_Node_RH (F_Ids), Composite_Constraint_Assoc_F_Constraint_Expr => Unwrap_Node_RH (F_Constraint_Expr)));
         end;


         function Create_Iterated_Assoc
           (Handle : Rewriting_Handle
               ; F_Spec : Node_Rewriting_Handle
               ; F_R_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Iterated_Assoc
               (Unwrap_RH (Handle),
                Iterated_Assoc_F_Spec => Unwrap_Node_RH (F_Spec), Iterated_Assoc_F_R_Expr => Unwrap_Node_RH (F_R_Expr)));
         end;


         function Create_Param_Assoc
           (Handle : Rewriting_Handle
               ; F_Designator : Node_Rewriting_Handle
               ; F_R_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Param_Assoc
               (Unwrap_RH (Handle),
                Param_Assoc_F_Designator => Unwrap_Node_RH (F_Designator), Param_Assoc_F_R_Expr => Unwrap_Node_RH (F_R_Expr)));
         end;


         function Create_Abstract_State_Decl
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Abstract_State_Decl
               (Unwrap_RH (Handle),
                Abstract_State_Decl_F_Name => Unwrap_Node_RH (F_Name), Abstract_State_Decl_F_Aspects => Unwrap_Node_RH (F_Aspects)));
         end;


         function Create_Anonymous_Expr_Decl
           (Handle : Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Anonymous_Expr_Decl
               (Unwrap_RH (Handle),
                Anonymous_Expr_Decl_F_Expr => Unwrap_Node_RH (F_Expr), Anonymous_Expr_Decl_F_Aspects => Unwrap_Node_RH (F_Aspects)));
         end;


         function Create_Component_Decl
           (Handle : Rewriting_Handle
               ; F_Ids : Node_Rewriting_Handle
               ; F_Component_Def : Node_Rewriting_Handle
               ; F_Default_Expr : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Component_Decl
               (Unwrap_RH (Handle),
                Component_Decl_F_Ids => Unwrap_Node_RH (F_Ids), Component_Decl_F_Component_Def => Unwrap_Node_RH (F_Component_Def), Component_Decl_F_Default_Expr => Unwrap_Node_RH (F_Default_Expr), Component_Decl_F_Aspects => Unwrap_Node_RH (F_Aspects)));
         end;


         function Create_Discriminant_Spec
           (Handle : Rewriting_Handle
               ; F_Ids : Node_Rewriting_Handle
               ; F_Type_Expr : Node_Rewriting_Handle
               ; F_Default_Expr : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Discriminant_Spec
               (Unwrap_RH (Handle),
                Discriminant_Spec_F_Ids => Unwrap_Node_RH (F_Ids), Discriminant_Spec_F_Type_Expr => Unwrap_Node_RH (F_Type_Expr), Discriminant_Spec_F_Default_Expr => Unwrap_Node_RH (F_Default_Expr), Discriminant_Spec_F_Aspects => Unwrap_Node_RH (F_Aspects)));
         end;


         function Create_Generic_Formal_Obj_Decl
           (Handle : Rewriting_Handle
               ; F_Decl : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Generic_Formal_Obj_Decl
               (Unwrap_RH (Handle),
                Generic_Formal_F_Decl => Unwrap_Node_RH (F_Decl), Generic_Formal_F_Aspects => Unwrap_Node_RH (F_Aspects)));
         end;


         function Create_Generic_Formal_Package
           (Handle : Rewriting_Handle
               ; F_Decl : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Generic_Formal_Package
               (Unwrap_RH (Handle),
                Generic_Formal_F_Decl => Unwrap_Node_RH (F_Decl), Generic_Formal_F_Aspects => Unwrap_Node_RH (F_Aspects)));
         end;


         function Create_Generic_Formal_Subp_Decl
           (Handle : Rewriting_Handle
               ; F_Decl : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Generic_Formal_Subp_Decl
               (Unwrap_RH (Handle),
                Generic_Formal_F_Decl => Unwrap_Node_RH (F_Decl), Generic_Formal_F_Aspects => Unwrap_Node_RH (F_Aspects)));
         end;


         function Create_Generic_Formal_Type_Decl
           (Handle : Rewriting_Handle
               ; F_Decl : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Generic_Formal_Type_Decl
               (Unwrap_RH (Handle),
                Generic_Formal_F_Decl => Unwrap_Node_RH (F_Decl), Generic_Formal_F_Aspects => Unwrap_Node_RH (F_Aspects)));
         end;


         function Create_Param_Spec
           (Handle : Rewriting_Handle
               ; F_Ids : Node_Rewriting_Handle
               ; F_Has_Aliased : Node_Rewriting_Handle
               ; F_Mode : Node_Rewriting_Handle
               ; F_Type_Expr : Node_Rewriting_Handle
               ; F_Default_Expr : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Param_Spec
               (Unwrap_RH (Handle),
                Param_Spec_F_Ids => Unwrap_Node_RH (F_Ids), Param_Spec_F_Has_Aliased => Unwrap_Node_RH (F_Has_Aliased), Param_Spec_F_Mode => Unwrap_Node_RH (F_Mode), Param_Spec_F_Type_Expr => Unwrap_Node_RH (F_Type_Expr), Param_Spec_F_Default_Expr => Unwrap_Node_RH (F_Default_Expr), Param_Spec_F_Aspects => Unwrap_Node_RH (F_Aspects)));
         end;


         function Create_Synthetic_Formal_Param_Decl
           (Handle : Rewriting_Handle
               ; F_Param_Type : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Synthetic_Formal_Param_Decl
               (Unwrap_RH (Handle),
                Synthetic_Formal_Param_Decl_F_Param_Type => Unwrap_Node_RH (F_Param_Type), Synthetic_Formal_Param_Decl_F_Aspects => Unwrap_Node_RH (F_Aspects)));
         end;


         function Create_Generic_Package_Internal
           (Handle : Rewriting_Handle
               ; F_Package_Name : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
               ; F_Public_Part : Node_Rewriting_Handle
               ; F_Private_Part : Node_Rewriting_Handle
               ; F_End_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Generic_Package_Internal
               (Unwrap_RH (Handle),
                Base_Package_Decl_F_Package_Name => Unwrap_Node_RH (F_Package_Name), Base_Package_Decl_F_Aspects => Unwrap_Node_RH (F_Aspects), Base_Package_Decl_F_Public_Part => Unwrap_Node_RH (F_Public_Part), Base_Package_Decl_F_Private_Part => Unwrap_Node_RH (F_Private_Part), Base_Package_Decl_F_End_Name => Unwrap_Node_RH (F_End_Name)));
         end;


         function Create_Package_Decl
           (Handle : Rewriting_Handle
               ; F_Package_Name : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
               ; F_Public_Part : Node_Rewriting_Handle
               ; F_Private_Part : Node_Rewriting_Handle
               ; F_End_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Package_Decl
               (Unwrap_RH (Handle),
                Base_Package_Decl_F_Package_Name => Unwrap_Node_RH (F_Package_Name), Base_Package_Decl_F_Aspects => Unwrap_Node_RH (F_Aspects), Base_Package_Decl_F_Public_Part => Unwrap_Node_RH (F_Public_Part), Base_Package_Decl_F_Private_Part => Unwrap_Node_RH (F_Private_Part), Base_Package_Decl_F_End_Name => Unwrap_Node_RH (F_End_Name)));
         end;


         function Create_Discrete_Base_Subtype_Decl
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Discrete_Base_Subtype_Decl
               (Unwrap_RH (Handle),
                Base_Type_Decl_F_Name => Unwrap_Node_RH (F_Name), Discrete_Base_Subtype_Decl_F_Aspects => Unwrap_Node_RH (F_Aspects)));
         end;


         function Create_Subtype_Decl
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Subtype : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Subtype_Decl
               (Unwrap_RH (Handle),
                Base_Type_Decl_F_Name => Unwrap_Node_RH (F_Name), Subtype_Decl_F_Subtype => Unwrap_Node_RH (F_Subtype), Subtype_Decl_F_Aspects => Unwrap_Node_RH (F_Aspects)));
         end;


         function Create_Classwide_Type_Decl
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Classwide_Type_Decl
               (Unwrap_RH (Handle),
                Base_Type_Decl_F_Name => Unwrap_Node_RH (F_Name), Classwide_Type_Decl_F_Aspects => Unwrap_Node_RH (F_Aspects)));
         end;


         function Create_Incomplete_Type_Decl
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Discriminants : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Incomplete_Type_Decl
               (Unwrap_RH (Handle),
                Base_Type_Decl_F_Name => Unwrap_Node_RH (F_Name), Incomplete_Type_Decl_F_Discriminants => Unwrap_Node_RH (F_Discriminants), Incomplete_Type_Decl_F_Aspects => Unwrap_Node_RH (F_Aspects)));
         end;


         function Create_Incomplete_Formal_Type_Decl
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Discriminants : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
               ; F_Is_Tagged : Node_Rewriting_Handle
               ; F_Default_Type : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Incomplete_Formal_Type_Decl
               (Unwrap_RH (Handle),
                Base_Type_Decl_F_Name => Unwrap_Node_RH (F_Name), Incomplete_Type_Decl_F_Discriminants => Unwrap_Node_RH (F_Discriminants), Incomplete_Type_Decl_F_Aspects => Unwrap_Node_RH (F_Aspects), Incomplete_Formal_Type_Decl_F_Is_Tagged => Unwrap_Node_RH (F_Is_Tagged), Incomplete_Formal_Type_Decl_F_Default_Type => Unwrap_Node_RH (F_Default_Type)));
         end;


         function Create_Incomplete_Tagged_Type_Decl
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Discriminants : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
               ; F_Has_Abstract : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Incomplete_Tagged_Type_Decl
               (Unwrap_RH (Handle),
                Base_Type_Decl_F_Name => Unwrap_Node_RH (F_Name), Incomplete_Type_Decl_F_Discriminants => Unwrap_Node_RH (F_Discriminants), Incomplete_Type_Decl_F_Aspects => Unwrap_Node_RH (F_Aspects), Incomplete_Tagged_Type_Decl_F_Has_Abstract => Unwrap_Node_RH (F_Has_Abstract)));
         end;


         function Create_Protected_Type_Decl
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Discriminants : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
               ; F_Interfaces : Node_Rewriting_Handle
               ; F_Definition : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Protected_Type_Decl
               (Unwrap_RH (Handle),
                Base_Type_Decl_F_Name => Unwrap_Node_RH (F_Name), Protected_Type_Decl_F_Discriminants => Unwrap_Node_RH (F_Discriminants), Protected_Type_Decl_F_Aspects => Unwrap_Node_RH (F_Aspects), Protected_Type_Decl_F_Interfaces => Unwrap_Node_RH (F_Interfaces), Protected_Type_Decl_F_Definition => Unwrap_Node_RH (F_Definition)));
         end;


         function Create_Task_Type_Decl
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Discriminants : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
               ; F_Definition : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Task_Type_Decl
               (Unwrap_RH (Handle),
                Base_Type_Decl_F_Name => Unwrap_Node_RH (F_Name), Task_Type_Decl_F_Discriminants => Unwrap_Node_RH (F_Discriminants), Task_Type_Decl_F_Aspects => Unwrap_Node_RH (F_Aspects), Task_Type_Decl_F_Definition => Unwrap_Node_RH (F_Definition)));
         end;


         function Create_Single_Task_Type_Decl
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Discriminants : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
               ; F_Definition : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Single_Task_Type_Decl
               (Unwrap_RH (Handle),
                Base_Type_Decl_F_Name => Unwrap_Node_RH (F_Name), Task_Type_Decl_F_Discriminants => Unwrap_Node_RH (F_Discriminants), Task_Type_Decl_F_Aspects => Unwrap_Node_RH (F_Aspects), Task_Type_Decl_F_Definition => Unwrap_Node_RH (F_Definition)));
         end;


         function Create_Anonymous_Type_Decl
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Discriminants : Node_Rewriting_Handle
               ; F_Type_Def : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Anonymous_Type_Decl
               (Unwrap_RH (Handle),
                Base_Type_Decl_F_Name => Unwrap_Node_RH (F_Name), Type_Decl_F_Discriminants => Unwrap_Node_RH (F_Discriminants), Type_Decl_F_Type_Def => Unwrap_Node_RH (F_Type_Def), Anonymous_Type_Decl_F_Aspects => Unwrap_Node_RH (F_Aspects)));
         end;


         function Create_Synth_Anonymous_Type_Decl
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Discriminants : Node_Rewriting_Handle
               ; F_Type_Def : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Synth_Anonymous_Type_Decl
               (Unwrap_RH (Handle),
                Base_Type_Decl_F_Name => Unwrap_Node_RH (F_Name), Type_Decl_F_Discriminants => Unwrap_Node_RH (F_Discriminants), Type_Decl_F_Type_Def => Unwrap_Node_RH (F_Type_Def), Anonymous_Type_Decl_F_Aspects => Unwrap_Node_RH (F_Aspects)));
         end;


         function Create_Concrete_Type_Decl
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Discriminants : Node_Rewriting_Handle
               ; F_Type_Def : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Concrete_Type_Decl
               (Unwrap_RH (Handle),
                Base_Type_Decl_F_Name => Unwrap_Node_RH (F_Name), Type_Decl_F_Discriminants => Unwrap_Node_RH (F_Discriminants), Type_Decl_F_Type_Def => Unwrap_Node_RH (F_Type_Def), Concrete_Type_Decl_F_Aspects => Unwrap_Node_RH (F_Aspects)));
         end;


         function Create_Formal_Type_Decl
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Discriminants : Node_Rewriting_Handle
               ; F_Type_Def : Node_Rewriting_Handle
               ; F_Default_Type : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Formal_Type_Decl
               (Unwrap_RH (Handle),
                Base_Type_Decl_F_Name => Unwrap_Node_RH (F_Name), Type_Decl_F_Discriminants => Unwrap_Node_RH (F_Discriminants), Type_Decl_F_Type_Def => Unwrap_Node_RH (F_Type_Def), Formal_Type_Decl_F_Default_Type => Unwrap_Node_RH (F_Default_Type), Formal_Type_Decl_F_Aspects => Unwrap_Node_RH (F_Aspects)));
         end;


         function Create_Abstract_Subp_Decl
           (Handle : Rewriting_Handle
               ; F_Overriding : Node_Rewriting_Handle
               ; F_Subp_Spec : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Abstract_Subp_Decl
               (Unwrap_RH (Handle),
                Classic_Subp_Decl_F_Overriding => Unwrap_Node_RH (F_Overriding), Classic_Subp_Decl_F_Subp_Spec => Unwrap_Node_RH (F_Subp_Spec), Abstract_Subp_Decl_F_Aspects => Unwrap_Node_RH (F_Aspects)));
         end;


         function Create_Abstract_Formal_Subp_Decl
           (Handle : Rewriting_Handle
               ; F_Overriding : Node_Rewriting_Handle
               ; F_Subp_Spec : Node_Rewriting_Handle
               ; F_Default_Expr : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Abstract_Formal_Subp_Decl
               (Unwrap_RH (Handle),
                Classic_Subp_Decl_F_Overriding => Unwrap_Node_RH (F_Overriding), Classic_Subp_Decl_F_Subp_Spec => Unwrap_Node_RH (F_Subp_Spec), Formal_Subp_Decl_F_Default_Expr => Unwrap_Node_RH (F_Default_Expr), Formal_Subp_Decl_F_Aspects => Unwrap_Node_RH (F_Aspects)));
         end;


         function Create_Concrete_Formal_Subp_Decl
           (Handle : Rewriting_Handle
               ; F_Overriding : Node_Rewriting_Handle
               ; F_Subp_Spec : Node_Rewriting_Handle
               ; F_Default_Expr : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Concrete_Formal_Subp_Decl
               (Unwrap_RH (Handle),
                Classic_Subp_Decl_F_Overriding => Unwrap_Node_RH (F_Overriding), Classic_Subp_Decl_F_Subp_Spec => Unwrap_Node_RH (F_Subp_Spec), Formal_Subp_Decl_F_Default_Expr => Unwrap_Node_RH (F_Default_Expr), Formal_Subp_Decl_F_Aspects => Unwrap_Node_RH (F_Aspects)));
         end;


         function Create_Subp_Decl
           (Handle : Rewriting_Handle
               ; F_Overriding : Node_Rewriting_Handle
               ; F_Subp_Spec : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Subp_Decl
               (Unwrap_RH (Handle),
                Classic_Subp_Decl_F_Overriding => Unwrap_Node_RH (F_Overriding), Classic_Subp_Decl_F_Subp_Spec => Unwrap_Node_RH (F_Subp_Spec), Subp_Decl_F_Aspects => Unwrap_Node_RH (F_Aspects)));
         end;


         function Create_Entry_Decl
           (Handle : Rewriting_Handle
               ; F_Overriding : Node_Rewriting_Handle
               ; F_Spec : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Entry_Decl
               (Unwrap_RH (Handle),
                Entry_Decl_F_Overriding => Unwrap_Node_RH (F_Overriding), Entry_Decl_F_Spec => Unwrap_Node_RH (F_Spec), Entry_Decl_F_Aspects => Unwrap_Node_RH (F_Aspects)));
         end;


         function Create_Enum_Literal_Decl
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Enum_Literal_Decl
               (Unwrap_RH (Handle),
                Enum_Literal_Decl_F_Name => Unwrap_Node_RH (F_Name), Enum_Literal_Decl_F_Aspects => Unwrap_Node_RH (F_Aspects)));
         end;


         function Create_Synthetic_Char_Enum_Lit
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Synthetic_Char_Enum_Lit
               (Unwrap_RH (Handle),
                Enum_Literal_Decl_F_Name => Unwrap_Node_RH (F_Name), Enum_Literal_Decl_F_Aspects => Unwrap_Node_RH (F_Aspects)));
         end;


         function Create_Generic_Subp_Internal
           (Handle : Rewriting_Handle
               ; F_Subp_Spec : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Generic_Subp_Internal
               (Unwrap_RH (Handle),
                Generic_Subp_Internal_F_Subp_Spec => Unwrap_Node_RH (F_Subp_Spec), Generic_Subp_Internal_F_Aspects => Unwrap_Node_RH (F_Aspects)));
         end;


         function Create_Synthetic_Subp_Decl
           (Handle : Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
               ; F_Spec : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Synthetic_Subp_Decl
               (Unwrap_RH (Handle),
                Synthetic_Subp_Decl_F_Aspects => Unwrap_Node_RH (F_Aspects), Synthetic_Subp_Decl_F_Spec => Unwrap_Node_RH (F_Spec)));
         end;


         function Create_Expr_Function
           (Handle : Rewriting_Handle
               ; F_Overriding : Node_Rewriting_Handle
               ; F_Subp_Spec : Node_Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Expr_Function
               (Unwrap_RH (Handle),
                Base_Subp_Body_F_Overriding => Unwrap_Node_RH (F_Overriding), Base_Subp_Body_F_Subp_Spec => Unwrap_Node_RH (F_Subp_Spec), Expr_Function_F_Expr => Unwrap_Node_RH (F_Expr), Expr_Function_F_Aspects => Unwrap_Node_RH (F_Aspects)));
         end;


         function Create_Null_Subp_Decl
           (Handle : Rewriting_Handle
               ; F_Overriding : Node_Rewriting_Handle
               ; F_Subp_Spec : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Null_Subp_Decl
               (Unwrap_RH (Handle),
                Base_Subp_Body_F_Overriding => Unwrap_Node_RH (F_Overriding), Base_Subp_Body_F_Subp_Spec => Unwrap_Node_RH (F_Subp_Spec), Null_Subp_Decl_F_Aspects => Unwrap_Node_RH (F_Aspects)));
         end;


         function Create_Subp_Body
           (Handle : Rewriting_Handle
               ; F_Overriding : Node_Rewriting_Handle
               ; F_Subp_Spec : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
               ; F_Decls : Node_Rewriting_Handle
               ; F_Stmts : Node_Rewriting_Handle
               ; F_End_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Subp_Body
               (Unwrap_RH (Handle),
                Base_Subp_Body_F_Overriding => Unwrap_Node_RH (F_Overriding), Base_Subp_Body_F_Subp_Spec => Unwrap_Node_RH (F_Subp_Spec), Subp_Body_F_Aspects => Unwrap_Node_RH (F_Aspects), Subp_Body_F_Decls => Unwrap_Node_RH (F_Decls), Subp_Body_F_Stmts => Unwrap_Node_RH (F_Stmts), Subp_Body_F_End_Name => Unwrap_Node_RH (F_End_Name)));
         end;


         function Create_Subp_Renaming_Decl
           (Handle : Rewriting_Handle
               ; F_Overriding : Node_Rewriting_Handle
               ; F_Subp_Spec : Node_Rewriting_Handle
               ; F_Renames : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Subp_Renaming_Decl
               (Unwrap_RH (Handle),
                Base_Subp_Body_F_Overriding => Unwrap_Node_RH (F_Overriding), Base_Subp_Body_F_Subp_Spec => Unwrap_Node_RH (F_Subp_Spec), Subp_Renaming_Decl_F_Renames => Unwrap_Node_RH (F_Renames), Subp_Renaming_Decl_F_Aspects => Unwrap_Node_RH (F_Aspects)));
         end;


         function Create_Package_Body_Stub
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Package_Body_Stub
               (Unwrap_RH (Handle),
                Package_Body_Stub_F_Name => Unwrap_Node_RH (F_Name), Package_Body_Stub_F_Aspects => Unwrap_Node_RH (F_Aspects)));
         end;


         function Create_Protected_Body_Stub
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Protected_Body_Stub
               (Unwrap_RH (Handle),
                Protected_Body_Stub_F_Name => Unwrap_Node_RH (F_Name), Protected_Body_Stub_F_Aspects => Unwrap_Node_RH (F_Aspects)));
         end;


         function Create_Subp_Body_Stub
           (Handle : Rewriting_Handle
               ; F_Overriding : Node_Rewriting_Handle
               ; F_Subp_Spec : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Subp_Body_Stub
               (Unwrap_RH (Handle),
                Subp_Body_Stub_F_Overriding => Unwrap_Node_RH (F_Overriding), Subp_Body_Stub_F_Subp_Spec => Unwrap_Node_RH (F_Subp_Spec), Subp_Body_Stub_F_Aspects => Unwrap_Node_RH (F_Aspects)));
         end;


         function Create_Task_Body_Stub
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Task_Body_Stub
               (Unwrap_RH (Handle),
                Task_Body_Stub_F_Name => Unwrap_Node_RH (F_Name), Task_Body_Stub_F_Aspects => Unwrap_Node_RH (F_Aspects)));
         end;


         function Create_Entry_Body
           (Handle : Rewriting_Handle
               ; F_Entry_Name : Node_Rewriting_Handle
               ; F_Index_Spec : Node_Rewriting_Handle
               ; F_Params : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
               ; F_Barrier : Node_Rewriting_Handle
               ; F_Decls : Node_Rewriting_Handle
               ; F_Stmts : Node_Rewriting_Handle
               ; F_End_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Entry_Body
               (Unwrap_RH (Handle),
                Entry_Body_F_Entry_Name => Unwrap_Node_RH (F_Entry_Name), Entry_Body_F_Index_Spec => Unwrap_Node_RH (F_Index_Spec), Entry_Body_F_Params => Unwrap_Node_RH (F_Params), Entry_Body_F_Aspects => Unwrap_Node_RH (F_Aspects), Entry_Body_F_Barrier => Unwrap_Node_RH (F_Barrier), Entry_Body_F_Decls => Unwrap_Node_RH (F_Decls), Entry_Body_F_Stmts => Unwrap_Node_RH (F_Stmts), Entry_Body_F_End_Name => Unwrap_Node_RH (F_End_Name)));
         end;


         function Create_Package_Body
           (Handle : Rewriting_Handle
               ; F_Package_Name : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
               ; F_Decls : Node_Rewriting_Handle
               ; F_Stmts : Node_Rewriting_Handle
               ; F_End_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Package_Body
               (Unwrap_RH (Handle),
                Package_Body_F_Package_Name => Unwrap_Node_RH (F_Package_Name), Package_Body_F_Aspects => Unwrap_Node_RH (F_Aspects), Package_Body_F_Decls => Unwrap_Node_RH (F_Decls), Package_Body_F_Stmts => Unwrap_Node_RH (F_Stmts), Package_Body_F_End_Name => Unwrap_Node_RH (F_End_Name)));
         end;


         function Create_Protected_Body
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
               ; F_Decls : Node_Rewriting_Handle
               ; F_End_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Protected_Body
               (Unwrap_RH (Handle),
                Protected_Body_F_Name => Unwrap_Node_RH (F_Name), Protected_Body_F_Aspects => Unwrap_Node_RH (F_Aspects), Protected_Body_F_Decls => Unwrap_Node_RH (F_Decls), Protected_Body_F_End_Name => Unwrap_Node_RH (F_End_Name)));
         end;


         function Create_Task_Body
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
               ; F_Decls : Node_Rewriting_Handle
               ; F_Stmts : Node_Rewriting_Handle
               ; F_End_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Task_Body
               (Unwrap_RH (Handle),
                Task_Body_F_Name => Unwrap_Node_RH (F_Name), Task_Body_F_Aspects => Unwrap_Node_RH (F_Aspects), Task_Body_F_Decls => Unwrap_Node_RH (F_Decls), Task_Body_F_Stmts => Unwrap_Node_RH (F_Stmts), Task_Body_F_End_Name => Unwrap_Node_RH (F_End_Name)));
         end;


         function Create_Entry_Index_Spec
           (Handle : Rewriting_Handle
               ; F_Id : Node_Rewriting_Handle
               ; F_Subtype : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Entry_Index_Spec
               (Unwrap_RH (Handle),
                Entry_Index_Spec_F_Id => Unwrap_Node_RH (F_Id), Entry_Index_Spec_F_Subtype => Unwrap_Node_RH (F_Subtype), Entry_Index_Spec_F_Aspects => Unwrap_Node_RH (F_Aspects)));
         end;


         function Create_Error_Decl
           (Handle : Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Error_Decl
               (Unwrap_RH (Handle),
                Error_Decl_F_Aspects => Unwrap_Node_RH (F_Aspects)));
         end;


         function Create_Exception_Decl
           (Handle : Rewriting_Handle
               ; F_Ids : Node_Rewriting_Handle
               ; F_Renames : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Exception_Decl
               (Unwrap_RH (Handle),
                Exception_Decl_F_Ids => Unwrap_Node_RH (F_Ids), Exception_Decl_F_Renames => Unwrap_Node_RH (F_Renames), Exception_Decl_F_Aspects => Unwrap_Node_RH (F_Aspects)));
         end;


         function Create_Exception_Handler
           (Handle : Rewriting_Handle
               ; F_Exception_Name : Node_Rewriting_Handle
               ; F_Handled_Exceptions : Node_Rewriting_Handle
               ; F_Stmts : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Exception_Handler
               (Unwrap_RH (Handle),
                Exception_Handler_F_Exception_Name => Unwrap_Node_RH (F_Exception_Name), Exception_Handler_F_Handled_Exceptions => Unwrap_Node_RH (F_Handled_Exceptions), Exception_Handler_F_Stmts => Unwrap_Node_RH (F_Stmts), Exception_Handler_F_Aspects => Unwrap_Node_RH (F_Aspects)));
         end;


         function Create_For_Loop_Var_Decl
           (Handle : Rewriting_Handle
               ; F_Id : Node_Rewriting_Handle
               ; F_Id_Type : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_For_Loop_Var_Decl
               (Unwrap_RH (Handle),
                For_Loop_Var_Decl_F_Id => Unwrap_Node_RH (F_Id), For_Loop_Var_Decl_F_Id_Type => Unwrap_Node_RH (F_Id_Type), For_Loop_Var_Decl_F_Aspects => Unwrap_Node_RH (F_Aspects)));
         end;


         function Create_Generic_Package_Decl
           (Handle : Rewriting_Handle
               ; F_Formal_Part : Node_Rewriting_Handle
               ; F_Package_Decl : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Generic_Package_Decl
               (Unwrap_RH (Handle),
                Generic_Decl_F_Formal_Part => Unwrap_Node_RH (F_Formal_Part), Generic_Package_Decl_F_Package_Decl => Unwrap_Node_RH (F_Package_Decl), Generic_Package_Decl_F_Aspects => Unwrap_Node_RH (F_Aspects)));
         end;


         function Create_Generic_Subp_Decl
           (Handle : Rewriting_Handle
               ; F_Formal_Part : Node_Rewriting_Handle
               ; F_Subp_Decl : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Generic_Subp_Decl
               (Unwrap_RH (Handle),
                Generic_Decl_F_Formal_Part => Unwrap_Node_RH (F_Formal_Part), Generic_Subp_Decl_F_Subp_Decl => Unwrap_Node_RH (F_Subp_Decl), Generic_Subp_Decl_F_Aspects => Unwrap_Node_RH (F_Aspects)));
         end;


         function Create_Generic_Package_Instantiation
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Generic_Pkg_Name : Node_Rewriting_Handle
               ; F_Params : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Generic_Package_Instantiation
               (Unwrap_RH (Handle),
                Generic_Package_Instantiation_F_Name => Unwrap_Node_RH (F_Name), Generic_Package_Instantiation_F_Generic_Pkg_Name => Unwrap_Node_RH (F_Generic_Pkg_Name), Generic_Package_Instantiation_F_Params => Unwrap_Node_RH (F_Params), Generic_Package_Instantiation_F_Aspects => Unwrap_Node_RH (F_Aspects)));
         end;


         function Create_Generic_Subp_Instantiation
           (Handle : Rewriting_Handle
               ; F_Overriding : Node_Rewriting_Handle
               ; F_Kind : Node_Rewriting_Handle
               ; F_Subp_Name : Node_Rewriting_Handle
               ; F_Generic_Subp_Name : Node_Rewriting_Handle
               ; F_Params : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Generic_Subp_Instantiation
               (Unwrap_RH (Handle),
                Generic_Subp_Instantiation_F_Overriding => Unwrap_Node_RH (F_Overriding), Generic_Subp_Instantiation_F_Kind => Unwrap_Node_RH (F_Kind), Generic_Subp_Instantiation_F_Subp_Name => Unwrap_Node_RH (F_Subp_Name), Generic_Subp_Instantiation_F_Generic_Subp_Name => Unwrap_Node_RH (F_Generic_Subp_Name), Generic_Subp_Instantiation_F_Params => Unwrap_Node_RH (F_Params), Generic_Subp_Instantiation_F_Aspects => Unwrap_Node_RH (F_Aspects)));
         end;


         function Create_Generic_Package_Renaming_Decl
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Renames : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Generic_Package_Renaming_Decl
               (Unwrap_RH (Handle),
                Generic_Package_Renaming_Decl_F_Name => Unwrap_Node_RH (F_Name), Generic_Package_Renaming_Decl_F_Renames => Unwrap_Node_RH (F_Renames), Generic_Package_Renaming_Decl_F_Aspects => Unwrap_Node_RH (F_Aspects)));
         end;


         function Create_Generic_Subp_Renaming_Decl
           (Handle : Rewriting_Handle
               ; F_Kind : Node_Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Renames : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Generic_Subp_Renaming_Decl
               (Unwrap_RH (Handle),
                Generic_Subp_Renaming_Decl_F_Kind => Unwrap_Node_RH (F_Kind), Generic_Subp_Renaming_Decl_F_Name => Unwrap_Node_RH (F_Name), Generic_Subp_Renaming_Decl_F_Renames => Unwrap_Node_RH (F_Renames), Generic_Subp_Renaming_Decl_F_Aspects => Unwrap_Node_RH (F_Aspects)));
         end;


         function Create_Label_Decl
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Label_Decl
               (Unwrap_RH (Handle),
                Label_Decl_F_Name => Unwrap_Node_RH (F_Name), Label_Decl_F_Aspects => Unwrap_Node_RH (F_Aspects)));
         end;


         function Create_Named_Stmt_Decl
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Named_Stmt_Decl
               (Unwrap_RH (Handle),
                Named_Stmt_Decl_F_Name => Unwrap_Node_RH (F_Name), Named_Stmt_Decl_F_Aspects => Unwrap_Node_RH (F_Aspects)));
         end;


         function Create_Number_Decl
           (Handle : Rewriting_Handle
               ; F_Ids : Node_Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Number_Decl
               (Unwrap_RH (Handle),
                Number_Decl_F_Ids => Unwrap_Node_RH (F_Ids), Number_Decl_F_Expr => Unwrap_Node_RH (F_Expr), Number_Decl_F_Aspects => Unwrap_Node_RH (F_Aspects)));
         end;


         function Create_Object_Decl
           (Handle : Rewriting_Handle
               ; F_Ids : Node_Rewriting_Handle
               ; F_Has_Aliased : Node_Rewriting_Handle
               ; F_Has_Constant : Node_Rewriting_Handle
               ; F_Mode : Node_Rewriting_Handle
               ; F_Type_Expr : Node_Rewriting_Handle
               ; F_Default_Expr : Node_Rewriting_Handle
               ; F_Renaming_Clause : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Object_Decl
               (Unwrap_RH (Handle),
                Object_Decl_F_Ids => Unwrap_Node_RH (F_Ids), Object_Decl_F_Has_Aliased => Unwrap_Node_RH (F_Has_Aliased), Object_Decl_F_Has_Constant => Unwrap_Node_RH (F_Has_Constant), Object_Decl_F_Mode => Unwrap_Node_RH (F_Mode), Object_Decl_F_Type_Expr => Unwrap_Node_RH (F_Type_Expr), Object_Decl_F_Default_Expr => Unwrap_Node_RH (F_Default_Expr), Object_Decl_F_Renaming_Clause => Unwrap_Node_RH (F_Renaming_Clause), Object_Decl_F_Aspects => Unwrap_Node_RH (F_Aspects)));
         end;


         function Create_Extended_Return_Stmt_Object_Decl
           (Handle : Rewriting_Handle
               ; F_Ids : Node_Rewriting_Handle
               ; F_Has_Aliased : Node_Rewriting_Handle
               ; F_Has_Constant : Node_Rewriting_Handle
               ; F_Mode : Node_Rewriting_Handle
               ; F_Type_Expr : Node_Rewriting_Handle
               ; F_Default_Expr : Node_Rewriting_Handle
               ; F_Renaming_Clause : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Extended_Return_Stmt_Object_Decl
               (Unwrap_RH (Handle),
                Object_Decl_F_Ids => Unwrap_Node_RH (F_Ids), Object_Decl_F_Has_Aliased => Unwrap_Node_RH (F_Has_Aliased), Object_Decl_F_Has_Constant => Unwrap_Node_RH (F_Has_Constant), Object_Decl_F_Mode => Unwrap_Node_RH (F_Mode), Object_Decl_F_Type_Expr => Unwrap_Node_RH (F_Type_Expr), Object_Decl_F_Default_Expr => Unwrap_Node_RH (F_Default_Expr), Object_Decl_F_Renaming_Clause => Unwrap_Node_RH (F_Renaming_Clause), Object_Decl_F_Aspects => Unwrap_Node_RH (F_Aspects)));
         end;


         function Create_No_Type_Object_Renaming_Decl
           (Handle : Rewriting_Handle
               ; F_Ids : Node_Rewriting_Handle
               ; F_Has_Aliased : Node_Rewriting_Handle
               ; F_Has_Constant : Node_Rewriting_Handle
               ; F_Mode : Node_Rewriting_Handle
               ; F_Type_Expr : Node_Rewriting_Handle
               ; F_Default_Expr : Node_Rewriting_Handle
               ; F_Renaming_Clause : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_No_Type_Object_Renaming_Decl
               (Unwrap_RH (Handle),
                Object_Decl_F_Ids => Unwrap_Node_RH (F_Ids), Object_Decl_F_Has_Aliased => Unwrap_Node_RH (F_Has_Aliased), Object_Decl_F_Has_Constant => Unwrap_Node_RH (F_Has_Constant), Object_Decl_F_Mode => Unwrap_Node_RH (F_Mode), Object_Decl_F_Type_Expr => Unwrap_Node_RH (F_Type_Expr), Object_Decl_F_Default_Expr => Unwrap_Node_RH (F_Default_Expr), Object_Decl_F_Renaming_Clause => Unwrap_Node_RH (F_Renaming_Clause), Object_Decl_F_Aspects => Unwrap_Node_RH (F_Aspects)));
         end;


         function Create_Package_Renaming_Decl
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Renames : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Package_Renaming_Decl
               (Unwrap_RH (Handle),
                Package_Renaming_Decl_F_Name => Unwrap_Node_RH (F_Name), Package_Renaming_Decl_F_Renames => Unwrap_Node_RH (F_Renames), Package_Renaming_Decl_F_Aspects => Unwrap_Node_RH (F_Aspects)));
         end;


         function Create_Single_Protected_Decl
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
               ; F_Interfaces : Node_Rewriting_Handle
               ; F_Definition : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Single_Protected_Decl
               (Unwrap_RH (Handle),
                Single_Protected_Decl_F_Name => Unwrap_Node_RH (F_Name), Single_Protected_Decl_F_Aspects => Unwrap_Node_RH (F_Aspects), Single_Protected_Decl_F_Interfaces => Unwrap_Node_RH (F_Interfaces), Single_Protected_Decl_F_Definition => Unwrap_Node_RH (F_Definition)));
         end;


         function Create_Single_Task_Decl
           (Handle : Rewriting_Handle
               ; F_Task_Type : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Single_Task_Decl
               (Unwrap_RH (Handle),
                Single_Task_Decl_F_Task_Type => Unwrap_Node_RH (F_Task_Type), Single_Task_Decl_F_Aspects => Unwrap_Node_RH (F_Aspects)));
         end;


         function Create_Case_Stmt_Alternative
           (Handle : Rewriting_Handle
               ; F_Choices : Node_Rewriting_Handle
               ; F_Stmts : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Case_Stmt_Alternative
               (Unwrap_RH (Handle),
                Case_Stmt_Alternative_F_Choices => Unwrap_Node_RH (F_Choices), Case_Stmt_Alternative_F_Stmts => Unwrap_Node_RH (F_Stmts)));
         end;


         function Create_Compilation_Unit
           (Handle : Rewriting_Handle
               ; F_Prelude : Node_Rewriting_Handle
               ; F_Body : Node_Rewriting_Handle
               ; F_Pragmas : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Compilation_Unit
               (Unwrap_RH (Handle),
                Compilation_Unit_F_Prelude => Unwrap_Node_RH (F_Prelude), Compilation_Unit_F_Body => Unwrap_Node_RH (F_Body), Compilation_Unit_F_Pragmas => Unwrap_Node_RH (F_Pragmas)));
         end;


         function Create_Component_Clause
           (Handle : Rewriting_Handle
               ; F_Id : Node_Rewriting_Handle
               ; F_Position : Node_Rewriting_Handle
               ; F_Range : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Component_Clause
               (Unwrap_RH (Handle),
                Component_Clause_F_Id => Unwrap_Node_RH (F_Id), Component_Clause_F_Position => Unwrap_Node_RH (F_Position), Component_Clause_F_Range => Unwrap_Node_RH (F_Range)));
         end;


         function Create_Component_Def
           (Handle : Rewriting_Handle
               ; F_Has_Aliased : Node_Rewriting_Handle
               ; F_Has_Constant : Node_Rewriting_Handle
               ; F_Type_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Component_Def
               (Unwrap_RH (Handle),
                Component_Def_F_Has_Aliased => Unwrap_Node_RH (F_Has_Aliased), Component_Def_F_Has_Constant => Unwrap_Node_RH (F_Has_Constant), Component_Def_F_Type_Expr => Unwrap_Node_RH (F_Type_Expr)));
         end;


         function Create_Composite_Constraint
           (Handle : Rewriting_Handle
               ; F_Constraints : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Composite_Constraint
               (Unwrap_RH (Handle),
                Composite_Constraint_F_Constraints => Unwrap_Node_RH (F_Constraints)));
         end;


         function Create_Delta_Constraint
           (Handle : Rewriting_Handle
               ; F_Digits : Node_Rewriting_Handle
               ; F_Range : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Delta_Constraint
               (Unwrap_RH (Handle),
                Delta_Constraint_F_Digits => Unwrap_Node_RH (F_Digits), Delta_Constraint_F_Range => Unwrap_Node_RH (F_Range)));
         end;


         function Create_Digits_Constraint
           (Handle : Rewriting_Handle
               ; F_Digits : Node_Rewriting_Handle
               ; F_Range : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Digits_Constraint
               (Unwrap_RH (Handle),
                Digits_Constraint_F_Digits => Unwrap_Node_RH (F_Digits), Digits_Constraint_F_Range => Unwrap_Node_RH (F_Range)));
         end;


         function Create_Range_Constraint
           (Handle : Rewriting_Handle
               ; F_Range : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Range_Constraint
               (Unwrap_RH (Handle),
                Range_Constraint_F_Range => Unwrap_Node_RH (F_Range)));
         end;


         function Create_Declarative_Part
           (Handle : Rewriting_Handle
               ; F_Decls : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Declarative_Part
               (Unwrap_RH (Handle),
                Declarative_Part_F_Decls => Unwrap_Node_RH (F_Decls)));
         end;


         function Create_Private_Part
           (Handle : Rewriting_Handle
               ; F_Decls : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Private_Part
               (Unwrap_RH (Handle),
                Declarative_Part_F_Decls => Unwrap_Node_RH (F_Decls)));
         end;


         function Create_Public_Part
           (Handle : Rewriting_Handle
               ; F_Decls : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Public_Part
               (Unwrap_RH (Handle),
                Declarative_Part_F_Decls => Unwrap_Node_RH (F_Decls)));
         end;


         function Create_Elsif_Expr_Part
           (Handle : Rewriting_Handle
               ; F_Cond_Expr : Node_Rewriting_Handle
               ; F_Then_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Elsif_Expr_Part
               (Unwrap_RH (Handle),
                Elsif_Expr_Part_F_Cond_Expr => Unwrap_Node_RH (F_Cond_Expr), Elsif_Expr_Part_F_Then_Expr => Unwrap_Node_RH (F_Then_Expr)));
         end;


         function Create_Elsif_Stmt_Part
           (Handle : Rewriting_Handle
               ; F_Cond_Expr : Node_Rewriting_Handle
               ; F_Stmts : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Elsif_Stmt_Part
               (Unwrap_RH (Handle),
                Elsif_Stmt_Part_F_Cond_Expr => Unwrap_Node_RH (F_Cond_Expr), Elsif_Stmt_Part_F_Stmts => Unwrap_Node_RH (F_Stmts)));
         end;


         function Create_Abstract_State_Decl_Expr
           (Handle : Rewriting_Handle
               ; F_State_Decl : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Abstract_State_Decl_Expr
               (Unwrap_RH (Handle),
                Abstract_State_Decl_Expr_F_State_Decl => Unwrap_Node_RH (F_State_Decl)));
         end;


         function Create_Allocator
           (Handle : Rewriting_Handle
               ; F_Subpool : Node_Rewriting_Handle
               ; F_Type_Or_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Allocator
               (Unwrap_RH (Handle),
                Allocator_F_Subpool => Unwrap_Node_RH (F_Subpool), Allocator_F_Type_Or_Expr => Unwrap_Node_RH (F_Type_Or_Expr)));
         end;


         function Create_Aggregate
           (Handle : Rewriting_Handle
               ; F_Ancestor_Expr : Node_Rewriting_Handle
               ; F_Assocs : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Aggregate
               (Unwrap_RH (Handle),
                Base_Aggregate_F_Ancestor_Expr => Unwrap_Node_RH (F_Ancestor_Expr), Base_Aggregate_F_Assocs => Unwrap_Node_RH (F_Assocs)));
         end;


         function Create_Bracket_Aggregate
           (Handle : Rewriting_Handle
               ; F_Ancestor_Expr : Node_Rewriting_Handle
               ; F_Assocs : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Bracket_Aggregate
               (Unwrap_RH (Handle),
                Base_Aggregate_F_Ancestor_Expr => Unwrap_Node_RH (F_Ancestor_Expr), Base_Aggregate_F_Assocs => Unwrap_Node_RH (F_Assocs)));
         end;


         function Create_Delta_Aggregate
           (Handle : Rewriting_Handle
               ; F_Ancestor_Expr : Node_Rewriting_Handle
               ; F_Assocs : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Delta_Aggregate
               (Unwrap_RH (Handle),
                Base_Aggregate_F_Ancestor_Expr => Unwrap_Node_RH (F_Ancestor_Expr), Base_Aggregate_F_Assocs => Unwrap_Node_RH (F_Assocs)));
         end;


         function Create_Bracket_Delta_Aggregate
           (Handle : Rewriting_Handle
               ; F_Ancestor_Expr : Node_Rewriting_Handle
               ; F_Assocs : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Bracket_Delta_Aggregate
               (Unwrap_RH (Handle),
                Base_Aggregate_F_Ancestor_Expr => Unwrap_Node_RH (F_Ancestor_Expr), Base_Aggregate_F_Assocs => Unwrap_Node_RH (F_Assocs)));
         end;


         function Create_Null_Record_Aggregate
           (Handle : Rewriting_Handle
               ; F_Ancestor_Expr : Node_Rewriting_Handle
               ; F_Assocs : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Null_Record_Aggregate
               (Unwrap_RH (Handle),
                Base_Aggregate_F_Ancestor_Expr => Unwrap_Node_RH (F_Ancestor_Expr), Base_Aggregate_F_Assocs => Unwrap_Node_RH (F_Assocs)));
         end;


         function Create_Bin_Op
           (Handle : Rewriting_Handle
               ; F_Left : Node_Rewriting_Handle
               ; F_Op : Node_Rewriting_Handle
               ; F_Right : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Bin_Op
               (Unwrap_RH (Handle),
                Bin_Op_F_Left => Unwrap_Node_RH (F_Left), Bin_Op_F_Op => Unwrap_Node_RH (F_Op), Bin_Op_F_Right => Unwrap_Node_RH (F_Right)));
         end;


         function Create_Relation_Op
           (Handle : Rewriting_Handle
               ; F_Left : Node_Rewriting_Handle
               ; F_Op : Node_Rewriting_Handle
               ; F_Right : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Relation_Op
               (Unwrap_RH (Handle),
                Bin_Op_F_Left => Unwrap_Node_RH (F_Left), Bin_Op_F_Op => Unwrap_Node_RH (F_Op), Bin_Op_F_Right => Unwrap_Node_RH (F_Right)));
         end;


         function Create_Case_Expr_Alternative
           (Handle : Rewriting_Handle
               ; F_Choices : Node_Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Case_Expr_Alternative
               (Unwrap_RH (Handle),
                Case_Expr_Alternative_F_Choices => Unwrap_Node_RH (F_Choices), Case_Expr_Alternative_F_Expr => Unwrap_Node_RH (F_Expr)));
         end;


         function Create_Concat_Op
           (Handle : Rewriting_Handle
               ; F_First_Operand : Node_Rewriting_Handle
               ; F_Other_Operands : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Concat_Op
               (Unwrap_RH (Handle),
                Concat_Op_F_First_Operand => Unwrap_Node_RH (F_First_Operand), Concat_Op_F_Other_Operands => Unwrap_Node_RH (F_Other_Operands)));
         end;


         function Create_Concat_Operand
           (Handle : Rewriting_Handle
               ; F_Operator : Node_Rewriting_Handle
               ; F_Operand : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Concat_Operand
               (Unwrap_RH (Handle),
                Concat_Operand_F_Operator => Unwrap_Node_RH (F_Operator), Concat_Operand_F_Operand => Unwrap_Node_RH (F_Operand)));
         end;


         function Create_Case_Expr
           (Handle : Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
               ; F_Cases : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Case_Expr
               (Unwrap_RH (Handle),
                Case_Expr_F_Expr => Unwrap_Node_RH (F_Expr), Case_Expr_F_Cases => Unwrap_Node_RH (F_Cases)));
         end;


         function Create_If_Expr
           (Handle : Rewriting_Handle
               ; F_Cond_Expr : Node_Rewriting_Handle
               ; F_Then_Expr : Node_Rewriting_Handle
               ; F_Alternatives : Node_Rewriting_Handle
               ; F_Else_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_If_Expr
               (Unwrap_RH (Handle),
                If_Expr_F_Cond_Expr => Unwrap_Node_RH (F_Cond_Expr), If_Expr_F_Then_Expr => Unwrap_Node_RH (F_Then_Expr), If_Expr_F_Alternatives => Unwrap_Node_RH (F_Alternatives), If_Expr_F_Else_Expr => Unwrap_Node_RH (F_Else_Expr)));
         end;


         function Create_Contract_Cases
           (Handle : Rewriting_Handle
               ; F_Contract_Cases : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Contract_Cases
               (Unwrap_RH (Handle),
                Contract_Cases_F_Contract_Cases => Unwrap_Node_RH (F_Contract_Cases)));
         end;


         function Create_Decl_Expr
           (Handle : Rewriting_Handle
               ; F_Decls : Node_Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Decl_Expr
               (Unwrap_RH (Handle),
                Decl_Expr_F_Decls => Unwrap_Node_RH (F_Decls), Decl_Expr_F_Expr => Unwrap_Node_RH (F_Expr)));
         end;


         function Create_Membership_Expr
           (Handle : Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
               ; F_Op : Node_Rewriting_Handle
               ; F_Membership_Exprs : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Membership_Expr
               (Unwrap_RH (Handle),
                Membership_Expr_F_Expr => Unwrap_Node_RH (F_Expr), Membership_Expr_F_Op => Unwrap_Node_RH (F_Op), Membership_Expr_F_Membership_Exprs => Unwrap_Node_RH (F_Membership_Exprs)));
         end;


         function Create_Attribute_Ref
           (Handle : Rewriting_Handle
               ; F_Prefix : Node_Rewriting_Handle
               ; F_Attribute : Node_Rewriting_Handle
               ; F_Args : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Attribute_Ref
               (Unwrap_RH (Handle),
                Attribute_Ref_F_Prefix => Unwrap_Node_RH (F_Prefix), Attribute_Ref_F_Attribute => Unwrap_Node_RH (F_Attribute), Attribute_Ref_F_Args => Unwrap_Node_RH (F_Args)));
         end;


         function Create_Call_Expr
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Suffix : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Call_Expr
               (Unwrap_RH (Handle),
                Call_Expr_F_Name => Unwrap_Node_RH (F_Name), Call_Expr_F_Suffix => Unwrap_Node_RH (F_Suffix)));
         end;


         function Create_Defining_Name
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Defining_Name
               (Unwrap_RH (Handle),
                Defining_Name_F_Name => Unwrap_Node_RH (F_Name)));
         end;


         function Create_Synthetic_Defining_Name
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Synthetic_Defining_Name
               (Unwrap_RH (Handle),
                Defining_Name_F_Name => Unwrap_Node_RH (F_Name)));
         end;


         function Create_Discrete_Subtype_Name
           (Handle : Rewriting_Handle
               ; F_Subtype : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Discrete_Subtype_Name
               (Unwrap_RH (Handle),
                Discrete_Subtype_Name_F_Subtype => Unwrap_Node_RH (F_Subtype)));
         end;


         function Create_Dotted_Name
           (Handle : Rewriting_Handle
               ; F_Prefix : Node_Rewriting_Handle
               ; F_Suffix : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Dotted_Name
               (Unwrap_RH (Handle),
                Dotted_Name_F_Prefix => Unwrap_Node_RH (F_Prefix), Dotted_Name_F_Suffix => Unwrap_Node_RH (F_Suffix)));
         end;


         function Create_End_Name
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_End_Name
               (Unwrap_RH (Handle),
                End_Name_F_Name => Unwrap_Node_RH (F_Name)));
         end;


         function Create_Explicit_Deref
           (Handle : Rewriting_Handle
               ; F_Prefix : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Explicit_Deref
               (Unwrap_RH (Handle),
                Explicit_Deref_F_Prefix => Unwrap_Node_RH (F_Prefix)));
         end;


         function Create_Qual_Expr
           (Handle : Rewriting_Handle
               ; F_Prefix : Node_Rewriting_Handle
               ; F_Suffix : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Qual_Expr
               (Unwrap_RH (Handle),
                Qual_Expr_F_Prefix => Unwrap_Node_RH (F_Prefix), Qual_Expr_F_Suffix => Unwrap_Node_RH (F_Suffix)));
         end;


         function Create_Reduce_Attribute_Ref
           (Handle : Rewriting_Handle
               ; F_Prefix : Node_Rewriting_Handle
               ; F_Attribute : Node_Rewriting_Handle
               ; F_Args : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Reduce_Attribute_Ref
               (Unwrap_RH (Handle),
                Reduce_Attribute_Ref_F_Prefix => Unwrap_Node_RH (F_Prefix), Reduce_Attribute_Ref_F_Attribute => Unwrap_Node_RH (F_Attribute), Reduce_Attribute_Ref_F_Args => Unwrap_Node_RH (F_Args)));
         end;


         function Create_Update_Attribute_Ref
           (Handle : Rewriting_Handle
               ; F_Prefix : Node_Rewriting_Handle
               ; F_Attribute : Node_Rewriting_Handle
               ; F_Values : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Update_Attribute_Ref
               (Unwrap_RH (Handle),
                Update_Attribute_Ref_F_Prefix => Unwrap_Node_RH (F_Prefix), Update_Attribute_Ref_F_Attribute => Unwrap_Node_RH (F_Attribute), Update_Attribute_Ref_F_Values => Unwrap_Node_RH (F_Values)));
         end;


         function Create_Paren_Expr
           (Handle : Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Paren_Expr
               (Unwrap_RH (Handle),
                Paren_Expr_F_Expr => Unwrap_Node_RH (F_Expr)));
         end;


         function Create_Quantified_Expr
           (Handle : Rewriting_Handle
               ; F_Quantifier : Node_Rewriting_Handle
               ; F_Loop_Spec : Node_Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Quantified_Expr
               (Unwrap_RH (Handle),
                Quantified_Expr_F_Quantifier => Unwrap_Node_RH (F_Quantifier), Quantified_Expr_F_Loop_Spec => Unwrap_Node_RH (F_Loop_Spec), Quantified_Expr_F_Expr => Unwrap_Node_RH (F_Expr)));
         end;


         function Create_Raise_Expr
           (Handle : Rewriting_Handle
               ; F_Exception_Name : Node_Rewriting_Handle
               ; F_Error_Message : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Raise_Expr
               (Unwrap_RH (Handle),
                Raise_Expr_F_Exception_Name => Unwrap_Node_RH (F_Exception_Name), Raise_Expr_F_Error_Message => Unwrap_Node_RH (F_Error_Message)));
         end;


         function Create_Un_Op
           (Handle : Rewriting_Handle
               ; F_Op : Node_Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Un_Op
               (Unwrap_RH (Handle),
                Un_Op_F_Op => Unwrap_Node_RH (F_Op), Un_Op_F_Expr => Unwrap_Node_RH (F_Expr)));
         end;


         function Create_Handled_Stmts
           (Handle : Rewriting_Handle
               ; F_Stmts : Node_Rewriting_Handle
               ; F_Exceptions : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Handled_Stmts
               (Unwrap_RH (Handle),
                Handled_Stmts_F_Stmts => Unwrap_Node_RH (F_Stmts), Handled_Stmts_F_Exceptions => Unwrap_Node_RH (F_Exceptions)));
         end;


         function Create_Library_Item
           (Handle : Rewriting_Handle
               ; F_Has_Private : Node_Rewriting_Handle
               ; F_Item : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Library_Item
               (Unwrap_RH (Handle),
                Library_Item_F_Has_Private => Unwrap_Node_RH (F_Has_Private), Library_Item_F_Item => Unwrap_Node_RH (F_Item)));
         end;


         function Create_For_Loop_Spec
           (Handle : Rewriting_Handle
               ; F_Var_Decl : Node_Rewriting_Handle
               ; F_Loop_Type : Node_Rewriting_Handle
               ; F_Has_Reverse : Node_Rewriting_Handle
               ; F_Iter_Expr : Node_Rewriting_Handle
               ; F_Iter_Filter : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_For_Loop_Spec
               (Unwrap_RH (Handle),
                For_Loop_Spec_F_Var_Decl => Unwrap_Node_RH (F_Var_Decl), For_Loop_Spec_F_Loop_Type => Unwrap_Node_RH (F_Loop_Type), For_Loop_Spec_F_Has_Reverse => Unwrap_Node_RH (F_Has_Reverse), For_Loop_Spec_F_Iter_Expr => Unwrap_Node_RH (F_Iter_Expr), For_Loop_Spec_F_Iter_Filter => Unwrap_Node_RH (F_Iter_Filter)));
         end;


         function Create_While_Loop_Spec
           (Handle : Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_While_Loop_Spec
               (Unwrap_RH (Handle),
                While_Loop_Spec_F_Expr => Unwrap_Node_RH (F_Expr)));
         end;


         function Create_Multi_Abstract_State_Decl
           (Handle : Rewriting_Handle
               ; F_Decls : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Multi_Abstract_State_Decl
               (Unwrap_RH (Handle),
                Multi_Abstract_State_Decl_F_Decls => Unwrap_Node_RH (F_Decls)));
         end;


         function Create_Params
           (Handle : Rewriting_Handle
               ; F_Params : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Params
               (Unwrap_RH (Handle),
                Params_F_Params => Unwrap_Node_RH (F_Params)));
         end;


         function Create_Paren_Abstract_State_Decl
           (Handle : Rewriting_Handle
               ; F_Decl : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Paren_Abstract_State_Decl
               (Unwrap_RH (Handle),
                Paren_Abstract_State_Decl_F_Decl => Unwrap_Node_RH (F_Decl)));
         end;


         function Create_Pp_Elsif_Directive
           (Handle : Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
               ; F_Then_Kw : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Pp_Elsif_Directive
               (Unwrap_RH (Handle),
                Pp_Elsif_Directive_F_Expr => Unwrap_Node_RH (F_Expr), Pp_Elsif_Directive_F_Then_Kw => Unwrap_Node_RH (F_Then_Kw)));
         end;


         function Create_Pp_If_Directive
           (Handle : Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
               ; F_Then_Kw : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Pp_If_Directive
               (Unwrap_RH (Handle),
                Pp_If_Directive_F_Expr => Unwrap_Node_RH (F_Expr), Pp_If_Directive_F_Then_Kw => Unwrap_Node_RH (F_Then_Kw)));
         end;


         function Create_Pragma_Node
           (Handle : Rewriting_Handle
               ; F_Id : Node_Rewriting_Handle
               ; F_Args : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Pragma_Node
               (Unwrap_RH (Handle),
                Pragma_Node_F_Id => Unwrap_Node_RH (F_Id), Pragma_Node_F_Args => Unwrap_Node_RH (F_Args)));
         end;


         function Create_Protected_Def
           (Handle : Rewriting_Handle
               ; F_Public_Part : Node_Rewriting_Handle
               ; F_Private_Part : Node_Rewriting_Handle
               ; F_End_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Protected_Def
               (Unwrap_RH (Handle),
                Protected_Def_F_Public_Part => Unwrap_Node_RH (F_Public_Part), Protected_Def_F_Private_Part => Unwrap_Node_RH (F_Private_Part), Protected_Def_F_End_Name => Unwrap_Node_RH (F_End_Name)));
         end;


         function Create_Range_Spec
           (Handle : Rewriting_Handle
               ; F_Range : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Range_Spec
               (Unwrap_RH (Handle),
                Range_Spec_F_Range => Unwrap_Node_RH (F_Range)));
         end;


         function Create_Renaming_Clause
           (Handle : Rewriting_Handle
               ; F_Renamed_Object : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Renaming_Clause
               (Unwrap_RH (Handle),
                Renaming_Clause_F_Renamed_Object => Unwrap_Node_RH (F_Renamed_Object)));
         end;


         function Create_Synthetic_Renaming_Clause
           (Handle : Rewriting_Handle
               ; F_Renamed_Object : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Synthetic_Renaming_Clause
               (Unwrap_RH (Handle),
                Renaming_Clause_F_Renamed_Object => Unwrap_Node_RH (F_Renamed_Object)));
         end;


         function Create_Select_When_Part
           (Handle : Rewriting_Handle
               ; F_Cond_Expr : Node_Rewriting_Handle
               ; F_Stmts : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Select_When_Part
               (Unwrap_RH (Handle),
                Select_When_Part_F_Cond_Expr => Unwrap_Node_RH (F_Cond_Expr), Select_When_Part_F_Stmts => Unwrap_Node_RH (F_Stmts)));
         end;


         function Create_Accept_Stmt
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Entry_Index_Expr : Node_Rewriting_Handle
               ; F_Params : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Accept_Stmt
               (Unwrap_RH (Handle),
                Accept_Stmt_F_Name => Unwrap_Node_RH (F_Name), Accept_Stmt_F_Entry_Index_Expr => Unwrap_Node_RH (F_Entry_Index_Expr), Accept_Stmt_F_Params => Unwrap_Node_RH (F_Params)));
         end;


         function Create_Accept_Stmt_With_Stmts
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Entry_Index_Expr : Node_Rewriting_Handle
               ; F_Params : Node_Rewriting_Handle
               ; F_Stmts : Node_Rewriting_Handle
               ; F_End_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Accept_Stmt_With_Stmts
               (Unwrap_RH (Handle),
                Accept_Stmt_F_Name => Unwrap_Node_RH (F_Name), Accept_Stmt_F_Entry_Index_Expr => Unwrap_Node_RH (F_Entry_Index_Expr), Accept_Stmt_F_Params => Unwrap_Node_RH (F_Params), Accept_Stmt_With_Stmts_F_Stmts => Unwrap_Node_RH (F_Stmts), Accept_Stmt_With_Stmts_F_End_Name => Unwrap_Node_RH (F_End_Name)));
         end;


         function Create_For_Loop_Stmt
           (Handle : Rewriting_Handle
               ; F_Spec : Node_Rewriting_Handle
               ; F_Stmts : Node_Rewriting_Handle
               ; F_End_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_For_Loop_Stmt
               (Unwrap_RH (Handle),
                Base_Loop_Stmt_F_Spec => Unwrap_Node_RH (F_Spec), Base_Loop_Stmt_F_Stmts => Unwrap_Node_RH (F_Stmts), Base_Loop_Stmt_F_End_Name => Unwrap_Node_RH (F_End_Name)));
         end;


         function Create_Loop_Stmt
           (Handle : Rewriting_Handle
               ; F_Spec : Node_Rewriting_Handle
               ; F_Stmts : Node_Rewriting_Handle
               ; F_End_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Loop_Stmt
               (Unwrap_RH (Handle),
                Base_Loop_Stmt_F_Spec => Unwrap_Node_RH (F_Spec), Base_Loop_Stmt_F_Stmts => Unwrap_Node_RH (F_Stmts), Base_Loop_Stmt_F_End_Name => Unwrap_Node_RH (F_End_Name)));
         end;


         function Create_While_Loop_Stmt
           (Handle : Rewriting_Handle
               ; F_Spec : Node_Rewriting_Handle
               ; F_Stmts : Node_Rewriting_Handle
               ; F_End_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_While_Loop_Stmt
               (Unwrap_RH (Handle),
                Base_Loop_Stmt_F_Spec => Unwrap_Node_RH (F_Spec), Base_Loop_Stmt_F_Stmts => Unwrap_Node_RH (F_Stmts), Base_Loop_Stmt_F_End_Name => Unwrap_Node_RH (F_End_Name)));
         end;


         function Create_Begin_Block
           (Handle : Rewriting_Handle
               ; F_Stmts : Node_Rewriting_Handle
               ; F_End_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Begin_Block
               (Unwrap_RH (Handle),
                Begin_Block_F_Stmts => Unwrap_Node_RH (F_Stmts), Begin_Block_F_End_Name => Unwrap_Node_RH (F_End_Name)));
         end;


         function Create_Decl_Block
           (Handle : Rewriting_Handle
               ; F_Decls : Node_Rewriting_Handle
               ; F_Stmts : Node_Rewriting_Handle
               ; F_End_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Decl_Block
               (Unwrap_RH (Handle),
                Decl_Block_F_Decls => Unwrap_Node_RH (F_Decls), Decl_Block_F_Stmts => Unwrap_Node_RH (F_Stmts), Decl_Block_F_End_Name => Unwrap_Node_RH (F_End_Name)));
         end;


         function Create_Case_Stmt
           (Handle : Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
               ; F_Pragmas : Node_Rewriting_Handle
               ; F_Alternatives : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Case_Stmt
               (Unwrap_RH (Handle),
                Case_Stmt_F_Expr => Unwrap_Node_RH (F_Expr), Case_Stmt_F_Pragmas => Unwrap_Node_RH (F_Pragmas), Case_Stmt_F_Alternatives => Unwrap_Node_RH (F_Alternatives)));
         end;


         function Create_Extended_Return_Stmt
           (Handle : Rewriting_Handle
               ; F_Decl : Node_Rewriting_Handle
               ; F_Stmts : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Extended_Return_Stmt
               (Unwrap_RH (Handle),
                Extended_Return_Stmt_F_Decl => Unwrap_Node_RH (F_Decl), Extended_Return_Stmt_F_Stmts => Unwrap_Node_RH (F_Stmts)));
         end;


         function Create_If_Stmt
           (Handle : Rewriting_Handle
               ; F_Cond_Expr : Node_Rewriting_Handle
               ; F_Then_Stmts : Node_Rewriting_Handle
               ; F_Alternatives : Node_Rewriting_Handle
               ; F_Else_Stmts : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_If_Stmt
               (Unwrap_RH (Handle),
                If_Stmt_F_Cond_Expr => Unwrap_Node_RH (F_Cond_Expr), If_Stmt_F_Then_Stmts => Unwrap_Node_RH (F_Then_Stmts), If_Stmt_F_Alternatives => Unwrap_Node_RH (F_Alternatives), If_Stmt_F_Else_Stmts => Unwrap_Node_RH (F_Else_Stmts)));
         end;


         function Create_Named_Stmt
           (Handle : Rewriting_Handle
               ; F_Decl : Node_Rewriting_Handle
               ; F_Stmt : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Named_Stmt
               (Unwrap_RH (Handle),
                Named_Stmt_F_Decl => Unwrap_Node_RH (F_Decl), Named_Stmt_F_Stmt => Unwrap_Node_RH (F_Stmt)));
         end;


         function Create_Select_Stmt
           (Handle : Rewriting_Handle
               ; F_Guards : Node_Rewriting_Handle
               ; F_Else_Stmts : Node_Rewriting_Handle
               ; F_Abort_Stmts : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Select_Stmt
               (Unwrap_RH (Handle),
                Select_Stmt_F_Guards => Unwrap_Node_RH (F_Guards), Select_Stmt_F_Else_Stmts => Unwrap_Node_RH (F_Else_Stmts), Select_Stmt_F_Abort_Stmts => Unwrap_Node_RH (F_Abort_Stmts)));
         end;


         function Create_Abort_Stmt
           (Handle : Rewriting_Handle
               ; F_Names : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Abort_Stmt
               (Unwrap_RH (Handle),
                Abort_Stmt_F_Names => Unwrap_Node_RH (F_Names)));
         end;


         function Create_Assign_Stmt
           (Handle : Rewriting_Handle
               ; F_Dest : Node_Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Assign_Stmt
               (Unwrap_RH (Handle),
                Assign_Stmt_F_Dest => Unwrap_Node_RH (F_Dest), Assign_Stmt_F_Expr => Unwrap_Node_RH (F_Expr)));
         end;


         function Create_Call_Stmt
           (Handle : Rewriting_Handle
               ; F_Call : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Call_Stmt
               (Unwrap_RH (Handle),
                Call_Stmt_F_Call => Unwrap_Node_RH (F_Call)));
         end;


         function Create_Delay_Stmt
           (Handle : Rewriting_Handle
               ; F_Has_Until : Node_Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Delay_Stmt
               (Unwrap_RH (Handle),
                Delay_Stmt_F_Has_Until => Unwrap_Node_RH (F_Has_Until), Delay_Stmt_F_Expr => Unwrap_Node_RH (F_Expr)));
         end;


         function Create_Exit_Stmt
           (Handle : Rewriting_Handle
               ; F_Loop_Name : Node_Rewriting_Handle
               ; F_Cond_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Exit_Stmt
               (Unwrap_RH (Handle),
                Exit_Stmt_F_Loop_Name => Unwrap_Node_RH (F_Loop_Name), Exit_Stmt_F_Cond_Expr => Unwrap_Node_RH (F_Cond_Expr)));
         end;


         function Create_Goto_Stmt
           (Handle : Rewriting_Handle
               ; F_Label_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Goto_Stmt
               (Unwrap_RH (Handle),
                Goto_Stmt_F_Label_Name => Unwrap_Node_RH (F_Label_Name)));
         end;


         function Create_Label
           (Handle : Rewriting_Handle
               ; F_Decl : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Label
               (Unwrap_RH (Handle),
                Label_F_Decl => Unwrap_Node_RH (F_Decl)));
         end;


         function Create_Raise_Stmt
           (Handle : Rewriting_Handle
               ; F_Exception_Name : Node_Rewriting_Handle
               ; F_Error_Message : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Raise_Stmt
               (Unwrap_RH (Handle),
                Raise_Stmt_F_Exception_Name => Unwrap_Node_RH (F_Exception_Name), Raise_Stmt_F_Error_Message => Unwrap_Node_RH (F_Error_Message)));
         end;


         function Create_Requeue_Stmt
           (Handle : Rewriting_Handle
               ; F_Call_Name : Node_Rewriting_Handle
               ; F_Has_Abort : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Requeue_Stmt
               (Unwrap_RH (Handle),
                Requeue_Stmt_F_Call_Name => Unwrap_Node_RH (F_Call_Name), Requeue_Stmt_F_Has_Abort => Unwrap_Node_RH (F_Has_Abort)));
         end;


         function Create_Return_Stmt
           (Handle : Rewriting_Handle
               ; F_Return_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Return_Stmt
               (Unwrap_RH (Handle),
                Return_Stmt_F_Return_Expr => Unwrap_Node_RH (F_Return_Expr)));
         end;


         function Create_Subunit
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Body : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Subunit
               (Unwrap_RH (Handle),
                Subunit_F_Name => Unwrap_Node_RH (F_Name), Subunit_F_Body => Unwrap_Node_RH (F_Body)));
         end;


         function Create_Task_Def
           (Handle : Rewriting_Handle
               ; F_Interfaces : Node_Rewriting_Handle
               ; F_Public_Part : Node_Rewriting_Handle
               ; F_Private_Part : Node_Rewriting_Handle
               ; F_End_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Task_Def
               (Unwrap_RH (Handle),
                Task_Def_F_Interfaces => Unwrap_Node_RH (F_Interfaces), Task_Def_F_Public_Part => Unwrap_Node_RH (F_Public_Part), Task_Def_F_Private_Part => Unwrap_Node_RH (F_Private_Part), Task_Def_F_End_Name => Unwrap_Node_RH (F_End_Name)));
         end;


         function Create_Access_To_Subp_Def
           (Handle : Rewriting_Handle
               ; F_Has_Not_Null : Node_Rewriting_Handle
               ; F_Has_Protected : Node_Rewriting_Handle
               ; F_Subp_Spec : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Access_To_Subp_Def
               (Unwrap_RH (Handle),
                Access_Def_F_Has_Not_Null => Unwrap_Node_RH (F_Has_Not_Null), Access_To_Subp_Def_F_Has_Protected => Unwrap_Node_RH (F_Has_Protected), Access_To_Subp_Def_F_Subp_Spec => Unwrap_Node_RH (F_Subp_Spec)));
         end;


         function Create_Anonymous_Type_Access_Def
           (Handle : Rewriting_Handle
               ; F_Has_Not_Null : Node_Rewriting_Handle
               ; F_Type_Decl : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Anonymous_Type_Access_Def
               (Unwrap_RH (Handle),
                Access_Def_F_Has_Not_Null => Unwrap_Node_RH (F_Has_Not_Null), Anonymous_Type_Access_Def_F_Type_Decl => Unwrap_Node_RH (F_Type_Decl)));
         end;


         function Create_Type_Access_Def
           (Handle : Rewriting_Handle
               ; F_Has_Not_Null : Node_Rewriting_Handle
               ; F_Has_All : Node_Rewriting_Handle
               ; F_Has_Constant : Node_Rewriting_Handle
               ; F_Subtype_Indication : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Type_Access_Def
               (Unwrap_RH (Handle),
                Access_Def_F_Has_Not_Null => Unwrap_Node_RH (F_Has_Not_Null), Type_Access_Def_F_Has_All => Unwrap_Node_RH (F_Has_All), Type_Access_Def_F_Has_Constant => Unwrap_Node_RH (F_Has_Constant), Type_Access_Def_F_Subtype_Indication => Unwrap_Node_RH (F_Subtype_Indication)));
         end;


         function Create_Array_Type_Def
           (Handle : Rewriting_Handle
               ; F_Indices : Node_Rewriting_Handle
               ; F_Component_Type : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Array_Type_Def
               (Unwrap_RH (Handle),
                Array_Type_Def_F_Indices => Unwrap_Node_RH (F_Indices), Array_Type_Def_F_Component_Type => Unwrap_Node_RH (F_Component_Type)));
         end;


         function Create_Derived_Type_Def
           (Handle : Rewriting_Handle
               ; F_Has_Abstract : Node_Rewriting_Handle
               ; F_Has_Limited : Node_Rewriting_Handle
               ; F_Has_Synchronized : Node_Rewriting_Handle
               ; F_Subtype_Indication : Node_Rewriting_Handle
               ; F_Interfaces : Node_Rewriting_Handle
               ; F_Record_Extension : Node_Rewriting_Handle
               ; F_Has_With_Private : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Derived_Type_Def
               (Unwrap_RH (Handle),
                Derived_Type_Def_F_Has_Abstract => Unwrap_Node_RH (F_Has_Abstract), Derived_Type_Def_F_Has_Limited => Unwrap_Node_RH (F_Has_Limited), Derived_Type_Def_F_Has_Synchronized => Unwrap_Node_RH (F_Has_Synchronized), Derived_Type_Def_F_Subtype_Indication => Unwrap_Node_RH (F_Subtype_Indication), Derived_Type_Def_F_Interfaces => Unwrap_Node_RH (F_Interfaces), Derived_Type_Def_F_Record_Extension => Unwrap_Node_RH (F_Record_Extension), Derived_Type_Def_F_Has_With_Private => Unwrap_Node_RH (F_Has_With_Private)));
         end;


         function Create_Enum_Type_Def
           (Handle : Rewriting_Handle
               ; F_Enum_Literals : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Enum_Type_Def
               (Unwrap_RH (Handle),
                Enum_Type_Def_F_Enum_Literals => Unwrap_Node_RH (F_Enum_Literals)));
         end;


         function Create_Interface_Type_Def
           (Handle : Rewriting_Handle
               ; F_Interface_Kind : Node_Rewriting_Handle
               ; F_Interfaces : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Interface_Type_Def
               (Unwrap_RH (Handle),
                Interface_Type_Def_F_Interface_Kind => Unwrap_Node_RH (F_Interface_Kind), Interface_Type_Def_F_Interfaces => Unwrap_Node_RH (F_Interfaces)));
         end;


         function Create_Mod_Int_Type_Def
           (Handle : Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Mod_Int_Type_Def
               (Unwrap_RH (Handle),
                Mod_Int_Type_Def_F_Expr => Unwrap_Node_RH (F_Expr)));
         end;


         function Create_Private_Type_Def
           (Handle : Rewriting_Handle
               ; F_Has_Abstract : Node_Rewriting_Handle
               ; F_Has_Tagged : Node_Rewriting_Handle
               ; F_Has_Limited : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Private_Type_Def
               (Unwrap_RH (Handle),
                Private_Type_Def_F_Has_Abstract => Unwrap_Node_RH (F_Has_Abstract), Private_Type_Def_F_Has_Tagged => Unwrap_Node_RH (F_Has_Tagged), Private_Type_Def_F_Has_Limited => Unwrap_Node_RH (F_Has_Limited)));
         end;


         function Create_Decimal_Fixed_Point_Def
           (Handle : Rewriting_Handle
               ; F_Delta : Node_Rewriting_Handle
               ; F_Digits : Node_Rewriting_Handle
               ; F_Range : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Decimal_Fixed_Point_Def
               (Unwrap_RH (Handle),
                Decimal_Fixed_Point_Def_F_Delta => Unwrap_Node_RH (F_Delta), Decimal_Fixed_Point_Def_F_Digits => Unwrap_Node_RH (F_Digits), Decimal_Fixed_Point_Def_F_Range => Unwrap_Node_RH (F_Range)));
         end;


         function Create_Floating_Point_Def
           (Handle : Rewriting_Handle
               ; F_Num_Digits : Node_Rewriting_Handle
               ; F_Range : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Floating_Point_Def
               (Unwrap_RH (Handle),
                Floating_Point_Def_F_Num_Digits => Unwrap_Node_RH (F_Num_Digits), Floating_Point_Def_F_Range => Unwrap_Node_RH (F_Range)));
         end;


         function Create_Ordinary_Fixed_Point_Def
           (Handle : Rewriting_Handle
               ; F_Delta : Node_Rewriting_Handle
               ; F_Range : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Ordinary_Fixed_Point_Def
               (Unwrap_RH (Handle),
                Ordinary_Fixed_Point_Def_F_Delta => Unwrap_Node_RH (F_Delta), Ordinary_Fixed_Point_Def_F_Range => Unwrap_Node_RH (F_Range)));
         end;


         function Create_Record_Type_Def
           (Handle : Rewriting_Handle
               ; F_Has_Abstract : Node_Rewriting_Handle
               ; F_Has_Tagged : Node_Rewriting_Handle
               ; F_Has_Limited : Node_Rewriting_Handle
               ; F_Record_Def : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Record_Type_Def
               (Unwrap_RH (Handle),
                Record_Type_Def_F_Has_Abstract => Unwrap_Node_RH (F_Has_Abstract), Record_Type_Def_F_Has_Tagged => Unwrap_Node_RH (F_Has_Tagged), Record_Type_Def_F_Has_Limited => Unwrap_Node_RH (F_Has_Limited), Record_Type_Def_F_Record_Def => Unwrap_Node_RH (F_Record_Def)));
         end;


         function Create_Signed_Int_Type_Def
           (Handle : Rewriting_Handle
               ; F_Range : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Signed_Int_Type_Def
               (Unwrap_RH (Handle),
                Signed_Int_Type_Def_F_Range => Unwrap_Node_RH (F_Range)));
         end;


         function Create_Anonymous_Type
           (Handle : Rewriting_Handle
               ; F_Type_Decl : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Anonymous_Type
               (Unwrap_RH (Handle),
                Anonymous_Type_F_Type_Decl => Unwrap_Node_RH (F_Type_Decl)));
         end;


         function Create_Subtype_Indication
           (Handle : Rewriting_Handle
               ; F_Has_Not_Null : Node_Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Constraint : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Subtype_Indication
               (Unwrap_RH (Handle),
                Subtype_Indication_F_Has_Not_Null => Unwrap_Node_RH (F_Has_Not_Null), Subtype_Indication_F_Name => Unwrap_Node_RH (F_Name), Subtype_Indication_F_Constraint => Unwrap_Node_RH (F_Constraint)));
         end;


         function Create_Constrained_Subtype_Indication
           (Handle : Rewriting_Handle
               ; F_Has_Not_Null : Node_Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Constraint : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Constrained_Subtype_Indication
               (Unwrap_RH (Handle),
                Subtype_Indication_F_Has_Not_Null => Unwrap_Node_RH (F_Has_Not_Null), Subtype_Indication_F_Name => Unwrap_Node_RH (F_Name), Subtype_Indication_F_Constraint => Unwrap_Node_RH (F_Constraint)));
         end;


         function Create_Discrete_Subtype_Indication
           (Handle : Rewriting_Handle
               ; F_Has_Not_Null : Node_Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Constraint : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Discrete_Subtype_Indication
               (Unwrap_RH (Handle),
                Subtype_Indication_F_Has_Not_Null => Unwrap_Node_RH (F_Has_Not_Null), Subtype_Indication_F_Name => Unwrap_Node_RH (F_Name), Subtype_Indication_F_Constraint => Unwrap_Node_RH (F_Constraint)));
         end;


         function Create_Synthetic_Type_Expr
           (Handle : Rewriting_Handle
               ; F_Target_Type : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Synthetic_Type_Expr
               (Unwrap_RH (Handle),
                Synthetic_Type_Expr_F_Target_Type => Unwrap_Node_RH (F_Target_Type)));
         end;


         function Create_Unconstrained_Array_Index
           (Handle : Rewriting_Handle
               ; F_Subtype_Indication : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Unconstrained_Array_Index
               (Unwrap_RH (Handle),
                Unconstrained_Array_Index_F_Subtype_Indication => Unwrap_Node_RH (F_Subtype_Indication)));
         end;


         function Create_Use_Package_Clause
           (Handle : Rewriting_Handle
               ; F_Packages : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Use_Package_Clause
               (Unwrap_RH (Handle),
                Use_Package_Clause_F_Packages => Unwrap_Node_RH (F_Packages)));
         end;


         function Create_Use_Type_Clause
           (Handle : Rewriting_Handle
               ; F_Has_All : Node_Rewriting_Handle
               ; F_Types : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Use_Type_Clause
               (Unwrap_RH (Handle),
                Use_Type_Clause_F_Has_All => Unwrap_Node_RH (F_Has_All), Use_Type_Clause_F_Types => Unwrap_Node_RH (F_Types)));
         end;


         function Create_Value_Sequence
           (Handle : Rewriting_Handle
               ; F_Iter_Assoc : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Value_Sequence
               (Unwrap_RH (Handle),
                Value_Sequence_F_Iter_Assoc => Unwrap_Node_RH (F_Iter_Assoc)));
         end;


         function Create_Variant
           (Handle : Rewriting_Handle
               ; F_Choices : Node_Rewriting_Handle
               ; F_Components : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Variant
               (Unwrap_RH (Handle),
                Variant_F_Choices => Unwrap_Node_RH (F_Choices), Variant_F_Components => Unwrap_Node_RH (F_Components)));
         end;


         function Create_Variant_Part
           (Handle : Rewriting_Handle
               ; F_Discr_Name : Node_Rewriting_Handle
               ; F_Variant : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_Variant_Part
               (Unwrap_RH (Handle),
                Variant_Part_F_Discr_Name => Unwrap_Node_RH (F_Discr_Name), Variant_Part_F_Variant => Unwrap_Node_RH (F_Variant)));
         end;


         function Create_With_Clause
           (Handle : Rewriting_Handle
               ; F_Has_Limited : Node_Rewriting_Handle
               ; F_Has_Private : Node_Rewriting_Handle
               ; F_Packages : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle is
         begin
            return Wrap_Node_RH (Impl.Create_With_Clause
               (Unwrap_RH (Handle),
                With_Clause_F_Has_Limited => Unwrap_Node_RH (F_Has_Limited), With_Clause_F_Has_Private => Unwrap_Node_RH (F_Has_Private), With_Clause_F_Packages => Unwrap_Node_RH (F_Packages)));
         end;


end Libadalang.Rewriting;
