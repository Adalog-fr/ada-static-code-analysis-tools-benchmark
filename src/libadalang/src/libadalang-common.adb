--
--  Copyright (C) 2014-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Unchecked_Conversion;

with System;

with GNATCOLL.Iconv;
with GNATCOLL.VFS; use GNATCOLL.VFS;

with Langkit_Support.Generic_API; use Langkit_Support.Generic_API;
with Langkit_Support.Generic_API.Analysis;
use Langkit_Support.Generic_API.Analysis;
with Langkit_Support.Internal.Analysis;
with Langkit_Support.Internal.Conversions;

with Libadalang.Generic_API;
with Libadalang.Implementation; use Libadalang.Implementation;
with Libadalang.Lexer_Implementation;
use Libadalang.Lexer_Implementation;
with Libadalang.Private_Converters;


package body Libadalang.Common is

   Is_Token_Node_Kind : constant array (Ada_Node_Kind_Type) of Boolean :=
     (Ada_Abort_Absent => False, Ada_Abort_Present => False, Ada_Abstract_Absent => False, Ada_Abstract_Present => False, Ada_Ada_Node_List => False, Ada_Abstract_State_Decl_List => False, Ada_Alternatives_List => False, Ada_Constraint_List => False, Ada_Decl_List => False, Ada_Stmt_List => False, Ada_Aspect_Assoc_List => False, Ada_Base_Assoc_List => False, Ada_Assoc_List => False, Ada_Basic_Decl_List => False, Ada_Case_Expr_Alternative_List => False, Ada_Case_Stmt_Alternative_List => False, Ada_Compilation_Unit_List => False, Ada_Concat_Operand_List => False, Ada_Contract_Case_Assoc_List => False, Ada_Defining_Name_List => False, Ada_Discriminant_Spec_List => False, Ada_Elsif_Expr_Part_List => False, Ada_Elsif_Stmt_Part_List => False, Ada_Enum_Literal_Decl_List => False, Ada_Expr_Alternatives_List => False, Ada_Discriminant_Choice_List => False, Ada_Name_List => False, Ada_Parent_List => False, Ada_Param_Spec_List => False, Ada_Pragma_Node_List => False, Ada_Select_When_Part_List => False, Ada_Unconstrained_Array_Index_List => False, Ada_Variant_List => False, Ada_Aliased_Absent => False, Ada_Aliased_Present => False, Ada_All_Absent => False, Ada_All_Present => False, Ada_Constrained_Array_Indices => False, Ada_Unconstrained_Array_Indices => False, Ada_Aspect_Assoc => False, Ada_At_Clause => False, Ada_Attribute_Def_Clause => False, Ada_Enum_Rep_Clause => False, Ada_Record_Rep_Clause => False, Ada_Aspect_Spec => False, Ada_Contract_Case_Assoc => False, Ada_Pragma_Argument_Assoc => False, Ada_Entry_Spec => False, Ada_Enum_Subp_Spec => False, Ada_Subp_Spec => False, Ada_Synthetic_Binary_Spec => False, Ada_Synthetic_Unary_Spec => False, Ada_Component_List => False, Ada_Known_Discriminant_Part => False, Ada_Unknown_Discriminant_Part => False, Ada_Entry_Completion_Formal_Params => False, Ada_Generic_Formal_Part => False, Ada_Null_Record_Def => False, Ada_Record_Def => False, Ada_Aggregate_Assoc => False, Ada_Multi_Dim_Array_Assoc => False, Ada_Composite_Constraint_Assoc => False, Ada_Iterated_Assoc => False, Ada_Param_Assoc => False, Ada_Abstract_State_Decl => False, Ada_Anonymous_Expr_Decl => False, Ada_Component_Decl => False, Ada_Discriminant_Spec => False, Ada_Generic_Formal_Obj_Decl => False, Ada_Generic_Formal_Package => False, Ada_Generic_Formal_Subp_Decl => False, Ada_Generic_Formal_Type_Decl => False, Ada_Param_Spec => False, Ada_Synthetic_Formal_Param_Decl => False, Ada_Generic_Package_Internal => False, Ada_Package_Decl => False, Ada_Discrete_Base_Subtype_Decl => False, Ada_Subtype_Decl => False, Ada_Classwide_Type_Decl => False, Ada_Incomplete_Type_Decl => False, Ada_Incomplete_Formal_Type_Decl => False, Ada_Incomplete_Tagged_Type_Decl => False, Ada_Protected_Type_Decl => False, Ada_Task_Type_Decl => False, Ada_Single_Task_Type_Decl => False, Ada_Anonymous_Type_Decl => False, Ada_Synth_Anonymous_Type_Decl => False, Ada_Concrete_Type_Decl => False, Ada_Formal_Type_Decl => False, Ada_Abstract_Subp_Decl => False, Ada_Abstract_Formal_Subp_Decl => False, Ada_Concrete_Formal_Subp_Decl => False, Ada_Subp_Decl => False, Ada_Entry_Decl => False, Ada_Enum_Literal_Decl => False, Ada_Synthetic_Char_Enum_Lit => False, Ada_Generic_Subp_Internal => False, Ada_Synthetic_Subp_Decl => False, Ada_Expr_Function => False, Ada_Null_Subp_Decl => False, Ada_Subp_Body => False, Ada_Subp_Renaming_Decl => False, Ada_Package_Body_Stub => False, Ada_Protected_Body_Stub => False, Ada_Subp_Body_Stub => False, Ada_Task_Body_Stub => False, Ada_Entry_Body => False, Ada_Package_Body => False, Ada_Protected_Body => False, Ada_Task_Body => False, Ada_Entry_Index_Spec => False, Ada_Error_Decl => False, Ada_Exception_Decl => False, Ada_Exception_Handler => False, Ada_For_Loop_Var_Decl => False, Ada_Generic_Package_Decl => False, Ada_Generic_Subp_Decl => False, Ada_Generic_Package_Instantiation => False, Ada_Generic_Subp_Instantiation => False, Ada_Generic_Package_Renaming_Decl => False, Ada_Generic_Subp_Renaming_Decl => False, Ada_Label_Decl => False, Ada_Named_Stmt_Decl => False, Ada_Number_Decl => False, Ada_Object_Decl => False, Ada_Extended_Return_Stmt_Object_Decl => False, Ada_No_Type_Object_Renaming_Decl => False, Ada_Package_Renaming_Decl => False, Ada_Single_Protected_Decl => False, Ada_Single_Task_Decl => False, Ada_Case_Stmt_Alternative => False, Ada_Compilation_Unit => False, Ada_Component_Clause => False, Ada_Component_Def => False, Ada_Constant_Absent => False, Ada_Constant_Present => False, Ada_Composite_Constraint => False, Ada_Delta_Constraint => False, Ada_Digits_Constraint => False, Ada_Range_Constraint => False, Ada_Declarative_Part => False, Ada_Private_Part => False, Ada_Public_Part => False, Ada_Elsif_Expr_Part => False, Ada_Elsif_Stmt_Part => False, Ada_Abstract_State_Decl_Expr => False, Ada_Allocator => False, Ada_Aggregate => False, Ada_Bracket_Aggregate => False, Ada_Delta_Aggregate => False, Ada_Bracket_Delta_Aggregate => False, Ada_Null_Record_Aggregate => False, Ada_Bin_Op => False, Ada_Relation_Op => False, Ada_Box_Expr => False, Ada_Case_Expr_Alternative => False, Ada_Concat_Op => False, Ada_Concat_Operand => False, Ada_Case_Expr => False, Ada_If_Expr => False, Ada_Contract_Cases => False, Ada_Decl_Expr => False, Ada_Membership_Expr => False, Ada_Attribute_Ref => False, Ada_Call_Expr => False, Ada_Defining_Name => False, Ada_Synthetic_Defining_Name => False, Ada_Discrete_Subtype_Name => False, Ada_Dotted_Name => False, Ada_End_Name => False, Ada_Explicit_Deref => False, Ada_Qual_Expr => False, Ada_Reduce_Attribute_Ref => False, Ada_Char_Literal => True, Ada_Identifier => True, Ada_Op_Abs => False, Ada_Op_And => False, Ada_Op_And_Then => False, Ada_Op_Concat => False, Ada_Op_Div => False, Ada_Op_Double_Dot => False, Ada_Op_Eq => False, Ada_Op_Gt => False, Ada_Op_Gte => False, Ada_Op_In => False, Ada_Op_Lt => False, Ada_Op_Lte => False, Ada_Op_Minus => False, Ada_Op_Mod => False, Ada_Op_Mult => False, Ada_Op_Neq => False, Ada_Op_Not => False, Ada_Op_Not_In => False, Ada_Op_Or => False, Ada_Op_Or_Else => False, Ada_Op_Plus => False, Ada_Op_Pow => False, Ada_Op_Rem => False, Ada_Op_Xor => False, Ada_String_Literal => True, Ada_Null_Literal => True, Ada_Int_Literal => True, Ada_Real_Literal => True, Ada_Synthetic_Identifier => False, Ada_Target_Name => False, Ada_Update_Attribute_Ref => False, Ada_Paren_Expr => False, Ada_Quantified_Expr => False, Ada_Raise_Expr => False, Ada_Un_Op => False, Ada_Handled_Stmts => False, Ada_Interface_Kind_Limited => False, Ada_Interface_Kind_Protected => False, Ada_Interface_Kind_Synchronized => False, Ada_Interface_Kind_Task => False, Ada_Iter_Type_In => False, Ada_Iter_Type_Of => False, Ada_Library_Item => False, Ada_Limited_Absent => False, Ada_Limited_Present => False, Ada_For_Loop_Spec => False, Ada_While_Loop_Spec => False, Ada_Mode_Default => False, Ada_Mode_In => False, Ada_Mode_In_Out => False, Ada_Mode_Out => False, Ada_Multi_Abstract_State_Decl => False, Ada_Not_Null_Absent => False, Ada_Not_Null_Present => False, Ada_Null_Component_Decl => False, Ada_Others_Designator => False, Ada_Overriding_Not_Overriding => False, Ada_Overriding_Overriding => False, Ada_Overriding_Unspecified => False, Ada_Params => False, Ada_Paren_Abstract_State_Decl => False, Ada_Pp_Else_Directive => False, Ada_Pp_Elsif_Directive => False, Ada_Pp_End_If_Directive => False, Ada_Pp_If_Directive => False, Ada_Pp_Then_Kw => False, Ada_Pragma_Node => False, Ada_Private_Absent => False, Ada_Private_Present => False, Ada_Protected_Def => False, Ada_Protected_Absent => False, Ada_Protected_Present => False, Ada_Quantifier_All => False, Ada_Quantifier_Some => False, Ada_Range_Spec => False, Ada_Renaming_Clause => False, Ada_Synthetic_Renaming_Clause => False, Ada_Reverse_Absent => False, Ada_Reverse_Present => False, Ada_Select_When_Part => False, Ada_Accept_Stmt => False, Ada_Accept_Stmt_With_Stmts => False, Ada_For_Loop_Stmt => False, Ada_Loop_Stmt => False, Ada_While_Loop_Stmt => False, Ada_Begin_Block => False, Ada_Decl_Block => False, Ada_Case_Stmt => False, Ada_Extended_Return_Stmt => False, Ada_If_Stmt => False, Ada_Named_Stmt => False, Ada_Select_Stmt => False, Ada_Error_Stmt => False, Ada_Abort_Stmt => False, Ada_Assign_Stmt => False, Ada_Call_Stmt => False, Ada_Delay_Stmt => False, Ada_Exit_Stmt => False, Ada_Goto_Stmt => False, Ada_Label => False, Ada_Null_Stmt => False, Ada_Raise_Stmt => False, Ada_Requeue_Stmt => False, Ada_Return_Stmt => False, Ada_Terminate_Alternative => False, Ada_Subp_Kind_Function => False, Ada_Subp_Kind_Procedure => False, Ada_Subunit => False, Ada_Synchronized_Absent => False, Ada_Synchronized_Present => False, Ada_Tagged_Absent => False, Ada_Tagged_Present => False, Ada_Task_Def => False, Ada_Type_Attributes_Repository => False, Ada_Access_To_Subp_Def => False, Ada_Anonymous_Type_Access_Def => False, Ada_Type_Access_Def => False, Ada_Array_Type_Def => False, Ada_Derived_Type_Def => False, Ada_Enum_Type_Def => False, Ada_Formal_Discrete_Type_Def => False, Ada_Interface_Type_Def => False, Ada_Mod_Int_Type_Def => False, Ada_Private_Type_Def => False, Ada_Decimal_Fixed_Point_Def => False, Ada_Floating_Point_Def => False, Ada_Ordinary_Fixed_Point_Def => False, Ada_Record_Type_Def => False, Ada_Signed_Int_Type_Def => False, Ada_Anonymous_Type => False, Ada_Enum_Lit_Synth_Type_Expr => False, Ada_Subtype_Indication => False, Ada_Constrained_Subtype_Indication => False, Ada_Discrete_Subtype_Indication => False, Ada_Synthetic_Type_Expr => False, Ada_Unconstrained_Array_Index => False, Ada_Until_Absent => False, Ada_Until_Present => False, Ada_Use_Package_Clause => False, Ada_Use_Type_Clause => False, Ada_Value_Sequence => False, Ada_Variant => False, Ada_Variant_Part => False, Ada_With_Clause => False, Ada_With_Private_Absent => False, Ada_With_Private_Present => False);
   --  For each node kind, return whether it is a node that contains only a
   --  single token.

   Is_Error_Node_Kind : constant array (Ada_Node_Kind_Type) of Boolean :=
     (Ada_Abort_Absent => False, Ada_Abort_Present => False, Ada_Abstract_Absent => False, Ada_Abstract_Present => False, Ada_Ada_Node_List => False, Ada_Abstract_State_Decl_List => False, Ada_Alternatives_List => False, Ada_Constraint_List => False, Ada_Decl_List => False, Ada_Stmt_List => False, Ada_Aspect_Assoc_List => False, Ada_Base_Assoc_List => False, Ada_Assoc_List => False, Ada_Basic_Decl_List => False, Ada_Case_Expr_Alternative_List => False, Ada_Case_Stmt_Alternative_List => False, Ada_Compilation_Unit_List => False, Ada_Concat_Operand_List => False, Ada_Contract_Case_Assoc_List => False, Ada_Defining_Name_List => False, Ada_Discriminant_Spec_List => False, Ada_Elsif_Expr_Part_List => False, Ada_Elsif_Stmt_Part_List => False, Ada_Enum_Literal_Decl_List => False, Ada_Expr_Alternatives_List => False, Ada_Discriminant_Choice_List => False, Ada_Name_List => False, Ada_Parent_List => False, Ada_Param_Spec_List => False, Ada_Pragma_Node_List => False, Ada_Select_When_Part_List => False, Ada_Unconstrained_Array_Index_List => False, Ada_Variant_List => False, Ada_Aliased_Absent => False, Ada_Aliased_Present => False, Ada_All_Absent => False, Ada_All_Present => False, Ada_Constrained_Array_Indices => False, Ada_Unconstrained_Array_Indices => False, Ada_Aspect_Assoc => False, Ada_At_Clause => False, Ada_Attribute_Def_Clause => False, Ada_Enum_Rep_Clause => False, Ada_Record_Rep_Clause => False, Ada_Aspect_Spec => False, Ada_Contract_Case_Assoc => False, Ada_Pragma_Argument_Assoc => False, Ada_Entry_Spec => False, Ada_Enum_Subp_Spec => False, Ada_Subp_Spec => False, Ada_Synthetic_Binary_Spec => False, Ada_Synthetic_Unary_Spec => False, Ada_Component_List => False, Ada_Known_Discriminant_Part => False, Ada_Unknown_Discriminant_Part => False, Ada_Entry_Completion_Formal_Params => False, Ada_Generic_Formal_Part => False, Ada_Null_Record_Def => False, Ada_Record_Def => False, Ada_Aggregate_Assoc => False, Ada_Multi_Dim_Array_Assoc => False, Ada_Composite_Constraint_Assoc => False, Ada_Iterated_Assoc => False, Ada_Param_Assoc => False, Ada_Abstract_State_Decl => False, Ada_Anonymous_Expr_Decl => False, Ada_Component_Decl => False, Ada_Discriminant_Spec => False, Ada_Generic_Formal_Obj_Decl => False, Ada_Generic_Formal_Package => False, Ada_Generic_Formal_Subp_Decl => False, Ada_Generic_Formal_Type_Decl => False, Ada_Param_Spec => False, Ada_Synthetic_Formal_Param_Decl => False, Ada_Generic_Package_Internal => False, Ada_Package_Decl => False, Ada_Discrete_Base_Subtype_Decl => False, Ada_Subtype_Decl => False, Ada_Classwide_Type_Decl => False, Ada_Incomplete_Type_Decl => False, Ada_Incomplete_Formal_Type_Decl => False, Ada_Incomplete_Tagged_Type_Decl => False, Ada_Protected_Type_Decl => False, Ada_Task_Type_Decl => False, Ada_Single_Task_Type_Decl => False, Ada_Anonymous_Type_Decl => False, Ada_Synth_Anonymous_Type_Decl => False, Ada_Concrete_Type_Decl => False, Ada_Formal_Type_Decl => False, Ada_Abstract_Subp_Decl => False, Ada_Abstract_Formal_Subp_Decl => False, Ada_Concrete_Formal_Subp_Decl => False, Ada_Subp_Decl => False, Ada_Entry_Decl => False, Ada_Enum_Literal_Decl => False, Ada_Synthetic_Char_Enum_Lit => False, Ada_Generic_Subp_Internal => False, Ada_Synthetic_Subp_Decl => False, Ada_Expr_Function => False, Ada_Null_Subp_Decl => False, Ada_Subp_Body => False, Ada_Subp_Renaming_Decl => False, Ada_Package_Body_Stub => False, Ada_Protected_Body_Stub => False, Ada_Subp_Body_Stub => False, Ada_Task_Body_Stub => False, Ada_Entry_Body => False, Ada_Package_Body => False, Ada_Protected_Body => False, Ada_Task_Body => False, Ada_Entry_Index_Spec => False, Ada_Error_Decl => True, Ada_Exception_Decl => False, Ada_Exception_Handler => False, Ada_For_Loop_Var_Decl => False, Ada_Generic_Package_Decl => False, Ada_Generic_Subp_Decl => False, Ada_Generic_Package_Instantiation => False, Ada_Generic_Subp_Instantiation => False, Ada_Generic_Package_Renaming_Decl => False, Ada_Generic_Subp_Renaming_Decl => False, Ada_Label_Decl => False, Ada_Named_Stmt_Decl => False, Ada_Number_Decl => False, Ada_Object_Decl => False, Ada_Extended_Return_Stmt_Object_Decl => False, Ada_No_Type_Object_Renaming_Decl => False, Ada_Package_Renaming_Decl => False, Ada_Single_Protected_Decl => False, Ada_Single_Task_Decl => False, Ada_Case_Stmt_Alternative => False, Ada_Compilation_Unit => False, Ada_Component_Clause => False, Ada_Component_Def => False, Ada_Constant_Absent => False, Ada_Constant_Present => False, Ada_Composite_Constraint => False, Ada_Delta_Constraint => False, Ada_Digits_Constraint => False, Ada_Range_Constraint => False, Ada_Declarative_Part => False, Ada_Private_Part => False, Ada_Public_Part => False, Ada_Elsif_Expr_Part => False, Ada_Elsif_Stmt_Part => False, Ada_Abstract_State_Decl_Expr => False, Ada_Allocator => False, Ada_Aggregate => False, Ada_Bracket_Aggregate => False, Ada_Delta_Aggregate => False, Ada_Bracket_Delta_Aggregate => False, Ada_Null_Record_Aggregate => False, Ada_Bin_Op => False, Ada_Relation_Op => False, Ada_Box_Expr => False, Ada_Case_Expr_Alternative => False, Ada_Concat_Op => False, Ada_Concat_Operand => False, Ada_Case_Expr => False, Ada_If_Expr => False, Ada_Contract_Cases => False, Ada_Decl_Expr => False, Ada_Membership_Expr => False, Ada_Attribute_Ref => False, Ada_Call_Expr => False, Ada_Defining_Name => False, Ada_Synthetic_Defining_Name => False, Ada_Discrete_Subtype_Name => False, Ada_Dotted_Name => False, Ada_End_Name => False, Ada_Explicit_Deref => False, Ada_Qual_Expr => False, Ada_Reduce_Attribute_Ref => False, Ada_Char_Literal => False, Ada_Identifier => False, Ada_Op_Abs => False, Ada_Op_And => False, Ada_Op_And_Then => False, Ada_Op_Concat => False, Ada_Op_Div => False, Ada_Op_Double_Dot => False, Ada_Op_Eq => False, Ada_Op_Gt => False, Ada_Op_Gte => False, Ada_Op_In => False, Ada_Op_Lt => False, Ada_Op_Lte => False, Ada_Op_Minus => False, Ada_Op_Mod => False, Ada_Op_Mult => False, Ada_Op_Neq => False, Ada_Op_Not => False, Ada_Op_Not_In => False, Ada_Op_Or => False, Ada_Op_Or_Else => False, Ada_Op_Plus => False, Ada_Op_Pow => False, Ada_Op_Rem => False, Ada_Op_Xor => False, Ada_String_Literal => False, Ada_Null_Literal => False, Ada_Int_Literal => False, Ada_Real_Literal => False, Ada_Synthetic_Identifier => False, Ada_Target_Name => False, Ada_Update_Attribute_Ref => False, Ada_Paren_Expr => False, Ada_Quantified_Expr => False, Ada_Raise_Expr => False, Ada_Un_Op => False, Ada_Handled_Stmts => False, Ada_Interface_Kind_Limited => False, Ada_Interface_Kind_Protected => False, Ada_Interface_Kind_Synchronized => False, Ada_Interface_Kind_Task => False, Ada_Iter_Type_In => False, Ada_Iter_Type_Of => False, Ada_Library_Item => False, Ada_Limited_Absent => False, Ada_Limited_Present => False, Ada_For_Loop_Spec => False, Ada_While_Loop_Spec => False, Ada_Mode_Default => False, Ada_Mode_In => False, Ada_Mode_In_Out => False, Ada_Mode_Out => False, Ada_Multi_Abstract_State_Decl => False, Ada_Not_Null_Absent => False, Ada_Not_Null_Present => False, Ada_Null_Component_Decl => False, Ada_Others_Designator => False, Ada_Overriding_Not_Overriding => False, Ada_Overriding_Overriding => False, Ada_Overriding_Unspecified => False, Ada_Params => False, Ada_Paren_Abstract_State_Decl => False, Ada_Pp_Else_Directive => False, Ada_Pp_Elsif_Directive => False, Ada_Pp_End_If_Directive => False, Ada_Pp_If_Directive => False, Ada_Pp_Then_Kw => False, Ada_Pragma_Node => False, Ada_Private_Absent => False, Ada_Private_Present => False, Ada_Protected_Def => False, Ada_Protected_Absent => False, Ada_Protected_Present => False, Ada_Quantifier_All => False, Ada_Quantifier_Some => False, Ada_Range_Spec => False, Ada_Renaming_Clause => False, Ada_Synthetic_Renaming_Clause => False, Ada_Reverse_Absent => False, Ada_Reverse_Present => False, Ada_Select_When_Part => False, Ada_Accept_Stmt => False, Ada_Accept_Stmt_With_Stmts => False, Ada_For_Loop_Stmt => False, Ada_Loop_Stmt => False, Ada_While_Loop_Stmt => False, Ada_Begin_Block => False, Ada_Decl_Block => False, Ada_Case_Stmt => False, Ada_Extended_Return_Stmt => False, Ada_If_Stmt => False, Ada_Named_Stmt => False, Ada_Select_Stmt => False, Ada_Error_Stmt => True, Ada_Abort_Stmt => False, Ada_Assign_Stmt => False, Ada_Call_Stmt => False, Ada_Delay_Stmt => False, Ada_Exit_Stmt => False, Ada_Goto_Stmt => False, Ada_Label => False, Ada_Null_Stmt => False, Ada_Raise_Stmt => False, Ada_Requeue_Stmt => False, Ada_Return_Stmt => False, Ada_Terminate_Alternative => False, Ada_Subp_Kind_Function => False, Ada_Subp_Kind_Procedure => False, Ada_Subunit => False, Ada_Synchronized_Absent => False, Ada_Synchronized_Present => False, Ada_Tagged_Absent => False, Ada_Tagged_Present => False, Ada_Task_Def => False, Ada_Type_Attributes_Repository => False, Ada_Access_To_Subp_Def => False, Ada_Anonymous_Type_Access_Def => False, Ada_Type_Access_Def => False, Ada_Array_Type_Def => False, Ada_Derived_Type_Def => False, Ada_Enum_Type_Def => False, Ada_Formal_Discrete_Type_Def => False, Ada_Interface_Type_Def => False, Ada_Mod_Int_Type_Def => False, Ada_Private_Type_Def => False, Ada_Decimal_Fixed_Point_Def => False, Ada_Floating_Point_Def => False, Ada_Ordinary_Fixed_Point_Def => False, Ada_Record_Type_Def => False, Ada_Signed_Int_Type_Def => False, Ada_Anonymous_Type => False, Ada_Enum_Lit_Synth_Type_Expr => False, Ada_Subtype_Indication => False, Ada_Constrained_Subtype_Indication => False, Ada_Discrete_Subtype_Indication => False, Ada_Synthetic_Type_Expr => False, Ada_Unconstrained_Array_Index => False, Ada_Until_Absent => False, Ada_Until_Present => False, Ada_Use_Package_Clause => False, Ada_Use_Type_Clause => False, Ada_Value_Sequence => False, Ada_Variant => False, Ada_Variant_Part => False, Ada_With_Clause => False, Ada_With_Private_Absent => False, Ada_With_Private_Present => False);
   --  For each node kind, return whether it is an error node

   function Wrap_Token_Reference
     (Context : Internal_Context;
      TDH     : Token_Data_Handler_Access;
      Index   : Token_Or_Trivia_Index) return Token_Reference;
   function Get_Token_Context (Token : Token_Reference) return Internal_Context;
   function Get_Token_Unit (Token : Token_Reference) return Internal_Unit;
   function Get_Token_TDH
     (Token : Token_Reference) return Token_Data_Handler_Access;
   function Get_Token_Index
     (Token : Token_Reference) return Token_Or_Trivia_Index;
   procedure Extract_Token_Text
     (Token         : Token_Data_Type;
      Source_Buffer : out Text_Cst_Access;
      First         : out Positive;
      Last          : out Natural);
   --  Implementations for converters soft-links

   function From_Generic (Token : Lk_Token) return Common.Token_Reference
     with Export, External_Name => "Libadalang__from_generic_token";
   function To_Generic (Token : Common.Token_Reference) return Lk_Token
     with Export, External_Name => "Libadalang__to_generic_token";
   --  Implementation for converters hard-links in Private_Converters

   pragma Warnings (Off, "possible aliasing problem for type");
   function "+" is new Ada.Unchecked_Conversion
     (Langkit_Support.Internal.Analysis.Internal_Context, Internal_Context);
   function "+" is new Ada.Unchecked_Conversion
     (Internal_Context, Langkit_Support.Internal.Analysis.Internal_Context);
   pragma Warnings (On, "possible aliasing problem for type");

   function Rewrap_Token
     (Origin : Token_Reference;
      Index  : Token_Or_Trivia_Index) return Token_Reference;
   --  Create a token reference for ``Index`` using the token data handler
   --  reference from ``Origin``.

   Token_Kind_To_Literals : constant array (Token_Kind) of Text_Access := (
   

         Ada_Access => new Text_Type'("access"),
         
         Ada_Range => new Text_Type'("range"),
         
         Ada_Digits => new Text_Type'("digits"),
         
         Ada_Delta => new Text_Type'("delta"),
         
         Ada_Mod => new Text_Type'("mod"),
         
         Ada_Abort => new Text_Type'("abort"),
         
         Ada_Else => new Text_Type'("else"),
         
         Ada_New => new Text_Type'("new"),
         
         Ada_Return => new Text_Type'("return"),
         
         Ada_Abs => new Text_Type'("abs"),
         
         Ada_Elsif => new Text_Type'("elsif"),
         
         Ada_Not => new Text_Type'("not"),
         
         Ada_Reverse => new Text_Type'("reverse"),
         
         Ada_End => new Text_Type'("end"),
         
         Ada_Null => new Text_Type'("null"),
         
         Ada_Accept => new Text_Type'("accept"),
         
         Ada_Entry => new Text_Type'("entry"),
         
         Ada_Select => new Text_Type'("select"),
         
         Ada_Exception => new Text_Type'("exception"),
         
         Ada_Of => new Text_Type'("of"),
         
         Ada_Separate => new Text_Type'("separate"),
         
         Ada_Exit => new Text_Type'("exit"),
         
         Ada_Or => new Text_Type'("or"),
         
         Ada_All => new Text_Type'("all"),
         
         Ada_Others => new Text_Type'("others"),
         
         Ada_Subtype => new Text_Type'("subtype"),
         
         Ada_And => new Text_Type'("and"),
         
         Ada_For => new Text_Type'("for"),
         
         Ada_Out => new Text_Type'("out"),
         
         Ada_Array => new Text_Type'("array"),
         
         Ada_Function => new Text_Type'("function"),
         
         Ada_At => new Text_Type'("at"),
         
         Ada_Generic => new Text_Type'("generic"),
         
         Ada_Package => new Text_Type'("package"),
         
         Ada_Task => new Text_Type'("task"),
         
         Ada_Begin => new Text_Type'("begin"),
         
         Ada_Goto => new Text_Type'("goto"),
         
         Ada_Pragma => new Text_Type'("pragma"),
         
         Ada_Terminate => new Text_Type'("terminate"),
         
         Ada_Body => new Text_Type'("body"),
         
         Ada_Private => new Text_Type'("private"),
         
         Ada_Then => new Text_Type'("then"),
         
         Ada_If => new Text_Type'("if"),
         
         Ada_Procedure => new Text_Type'("procedure"),
         
         Ada_Type => new Text_Type'("type"),
         
         Ada_Case => new Text_Type'("case"),
         
         Ada_In => new Text_Type'("in"),
         
         Ada_Constant => new Text_Type'("constant"),
         
         Ada_Is => new Text_Type'("is"),
         
         Ada_Raise => new Text_Type'("raise"),
         
         Ada_Use => new Text_Type'("use"),
         
         Ada_Declare => new Text_Type'("declare"),
         
         Ada_Delay => new Text_Type'("delay"),
         
         Ada_Limited => new Text_Type'("limited"),
         
         Ada_Record => new Text_Type'("record"),
         
         Ada_When => new Text_Type'("when"),
         
         Ada_Loop => new Text_Type'("loop"),
         
         Ada_Rem => new Text_Type'("rem"),
         
         Ada_While => new Text_Type'("while"),
         
         Ada_Renames => new Text_Type'("renames"),
         
         Ada_With => new Text_Type'("with"),
         
         Ada_Do => new Text_Type'("do"),
         
         Ada_Xor => new Text_Type'("xor"),
         
         Ada_Par_Open => new Text_Type'("("),
         
         Ada_Par_Close => new Text_Type'(")"),
         
         Ada_Brack_Open => new Text_Type'("["),
         
         Ada_Brack_Close => new Text_Type'("]"),
         
         Ada_Semicolon => new Text_Type'(";"),
         
         Ada_Colon => new Text_Type'(":"),
         
         Ada_Comma => new Text_Type'(","),
         
         Ada_Doubledot => new Text_Type'(".."),
         
         Ada_Assign => new Text_Type'(":="),
         
         Ada_Dot => new Text_Type'("."),
         
         Ada_Diamond => new Text_Type'("<>"),
         
         Ada_Lte => new Text_Type'("<="),
         
         Ada_Gte => new Text_Type'(">="),
         
         Ada_Arrow => new Text_Type'("=>"),
         
         Ada_Equal => new Text_Type'("="),
         
         Ada_Lt => new Text_Type'("<"),
         
         Ada_Gt => new Text_Type'(">"),
         
         Ada_Plus => new Text_Type'("+"),
         
         Ada_Minus => new Text_Type'("-"),
         
         Ada_Power => new Text_Type'("**"),
         
         Ada_Mult => new Text_Type'("*"),
         
         Ada_Amp => new Text_Type'("&"),
         
         Ada_Notequal => new Text_Type'("/="),
         
         Ada_Divide => new Text_Type'("/"),
         
         Ada_Tick => new Text_Type'("'"),
         
         Ada_Pipe => new Text_Type'("|"),
         
         Ada_Label_Start => new Text_Type'("<<"),
         
         Ada_Label_End => new Text_Type'(">>"),
         
         Ada_Target => new Text_Type'("@"),
         
      others => new Text_Type'("")
   );

   Token_Kind_Names : constant array (Token_Kind) of String_Access := (
          Ada_Identifier =>
             new String'("Identifier")
              ,
          Ada_All =>
             new String'("All")
              ,
          Ada_Abort =>
             new String'("Abort")
              ,
          Ada_Else =>
             new String'("Else")
              ,
          Ada_New =>
             new String'("New")
              ,
          Ada_Return =>
             new String'("Return")
              ,
          Ada_Abs =>
             new String'("Abs")
              ,
          Ada_Elsif =>
             new String'("Elsif")
              ,
          Ada_Not =>
             new String'("Not")
              ,
          Ada_Reverse =>
             new String'("Reverse")
              ,
          Ada_End =>
             new String'("End")
              ,
          Ada_Null =>
             new String'("Null")
              ,
          Ada_Accept =>
             new String'("Accept")
              ,
          Ada_Entry =>
             new String'("Entry")
              ,
          Ada_Select =>
             new String'("Select")
              ,
          Ada_Access =>
             new String'("Access")
              ,
          Ada_Exception =>
             new String'("Exception")
              ,
          Ada_Of =>
             new String'("Of")
              ,
          Ada_Separate =>
             new String'("Separate")
              ,
          Ada_Exit =>
             new String'("Exit")
              ,
          Ada_Or =>
             new String'("Or")
              ,
          Ada_Others =>
             new String'("Others")
              ,
          Ada_Subtype =>
             new String'("Subtype")
              ,
          Ada_And =>
             new String'("And")
              ,
          Ada_For =>
             new String'("For")
              ,
          Ada_Out =>
             new String'("Out")
              ,
          Ada_Array =>
             new String'("Array")
              ,
          Ada_Function =>
             new String'("Function")
              ,
          Ada_At =>
             new String'("At")
              ,
          Ada_Generic =>
             new String'("Generic")
              ,
          Ada_Package =>
             new String'("Package")
              ,
          Ada_Task =>
             new String'("Task")
              ,
          Ada_Begin =>
             new String'("Begin")
              ,
          Ada_Goto =>
             new String'("Goto")
              ,
          Ada_Pragma =>
             new String'("Pragma")
              ,
          Ada_Terminate =>
             new String'("Terminate")
              ,
          Ada_Body =>
             new String'("Body")
              ,
          Ada_Private =>
             new String'("Private")
              ,
          Ada_Then =>
             new String'("Then")
              ,
          Ada_If =>
             new String'("If")
              ,
          Ada_Procedure =>
             new String'("Procedure")
              ,
          Ada_Type =>
             new String'("Type")
              ,
          Ada_Case =>
             new String'("Case")
              ,
          Ada_In =>
             new String'("In")
              ,
          Ada_Constant =>
             new String'("Constant")
              ,
          Ada_Is =>
             new String'("Is")
              ,
          Ada_Raise =>
             new String'("Raise")
              ,
          Ada_Use =>
             new String'("Use")
              ,
          Ada_Declare =>
             new String'("Declare")
              ,
          Ada_Range =>
             new String'("Range")
              ,
          Ada_Delay =>
             new String'("Delay")
              ,
          Ada_Limited =>
             new String'("Limited")
              ,
          Ada_Record =>
             new String'("Record")
              ,
          Ada_When =>
             new String'("When")
              ,
          Ada_Delta =>
             new String'("Delta")
              ,
          Ada_Loop =>
             new String'("Loop")
              ,
          Ada_Rem =>
             new String'("Rem")
              ,
          Ada_While =>
             new String'("While")
              ,
          Ada_Digits =>
             new String'("Digits")
              ,
          Ada_Renames =>
             new String'("Renames")
              ,
          Ada_Do =>
             new String'("Do")
              ,
          Ada_Mod =>
             new String'("Mod")
              ,
          Ada_Xor =>
             new String'("Xor")
              ,
          Ada_Par_Close =>
             new String'("Par_Close")
              ,
          Ada_Par_Open =>
             new String'("Par_Open")
              ,
          Ada_Brack_Close =>
             new String'("Brack_Close")
              ,
          Ada_Brack_Open =>
             new String'("Brack_Open")
              ,
          Ada_Semicolon =>
             new String'("Semicolon")
              ,
          Ada_Colon =>
             new String'("Colon")
              ,
          Ada_Comma =>
             new String'("Comma")
              ,
          Ada_Doubledot =>
             new String'("Doubledot")
              ,
          Ada_Dot =>
             new String'("Dot")
              ,
          Ada_Diamond =>
             new String'("Diamond")
              ,
          Ada_Lte =>
             new String'("Lte")
              ,
          Ada_Gte =>
             new String'("Gte")
              ,
          Ada_Arrow =>
             new String'("Arrow")
              ,
          Ada_Equal =>
             new String'("Equal")
              ,
          Ada_Lt =>
             new String'("Lt")
              ,
          Ada_Gt =>
             new String'("Gt")
              ,
          Ada_Plus =>
             new String'("Plus")
              ,
          Ada_Minus =>
             new String'("Minus")
              ,
          Ada_Power =>
             new String'("Power")
              ,
          Ada_Mult =>
             new String'("Mult")
              ,
          Ada_Amp =>
             new String'("Amp")
              ,
          Ada_Notequal =>
             new String'("Notequal")
              ,
          Ada_Divide =>
             new String'("Divide")
              ,
          Ada_Tick =>
             new String'("Tick")
              ,
          Ada_Pipe =>
             new String'("Pipe")
              ,
          Ada_Assign =>
             new String'("Assign")
              ,
          Ada_Label_Start =>
             new String'("Label_Start")
              ,
          Ada_Label_End =>
             new String'("Label_End")
              ,
          Ada_Target =>
             new String'("Target")
              ,
          Ada_String =>
             new String'("String")
              ,
          Ada_Char =>
             new String'("Char")
              ,
          Ada_With =>
             new String'("With")
              ,
          Ada_Decimal =>
             new String'("Decimal")
              ,
          Ada_Integer =>
             new String'("Integer")
              ,
          Ada_Comment =>
             new String'("Comment")
              ,
          Ada_Prep_Line =>
             new String'("Prep_Line")
              ,
          Ada_Whitespace =>
             new String'("Whitespace")
              ,
          Ada_Termination =>
             new String'("Termination")
              ,
          Ada_Lexing_Failure =>
             new String'("Lexing_Failure")
   );

   ---------------------
   -- Token_Kind_Name --
   ---------------------

   function Token_Kind_Name (Token_Id : Token_Kind) return String is
     (Token_Kind_Names (Token_Id).all);

   ------------------------
   -- Token_Kind_Literal --
   ------------------------

   function Token_Kind_Literal (Token_Id : Token_Kind) return Text_Type is
     (Token_Kind_To_Literals (Token_Id).all);

   -----------------------
   -- Token_Error_Image --
   -----------------------

   function Token_Error_Image (Token_Id : Token_Kind) return String is
      Literal : constant Text_Type := Token_Kind_Literal (Token_Id);
   begin
      return (if Literal /= ""
              then "'" & Image (Literal) & "'"
              else Token_Kind_Name (Token_Id));
   end Token_Error_Image;

   function To_Token_Kind (Raw : Raw_Token_Kind) return Token_Kind
   is (Token_Kind'Val (Raw));

   function From_Token_Kind (Kind : Token_Kind) return Raw_Token_Kind
   is (Token_Kind'Pos (Kind));

   -------------------
   -- Is_Token_Node --
   -------------------

   function Is_Token_Node (Kind : Ada_Node_Kind_Type) return Boolean is
   begin
      return Is_Token_Node_Kind (Kind);
   end Is_Token_Node;

   -------------------
   -- Is_Error_Node --
   -------------------

   function Is_Error_Node (Kind : Ada_Node_Kind_Type) return Boolean is
   begin
      return Is_Error_Node_Kind (Kind);
   end Is_Error_Node;

   ------------------
   -- Is_List_Node --
   ------------------

   function Is_List_Node (Kind : Ada_Node_Kind_Type) return Boolean is
   begin
         return Kind in Ada_Ada_List;
   end Is_List_Node;

   ------------------
   -- Rewrap_Token --
   ------------------

   function Rewrap_Token
     (Origin : Token_Reference;
      Index  : Token_Or_Trivia_Index) return Token_Reference is
   begin
      return (if Index = No_Token_Or_Trivia_Index
              then No_Token
              else (Origin.TDH, Index, Origin.Safety_Net));
   end Rewrap_Token;

   ----------------------
   -- Check_Safety_Net --
   ----------------------

   procedure Check_Safety_Net (Self : Token_Reference) is
      SN  : Token_Safety_Net renames Self.Safety_Net;
      Ctx : constant Internal_Context := +SN.Context;
   begin
      if Self.TDH /= null
         and then (Ctx.Serial_Number /= SN.Context_Version
                   or else Self.TDH.Version /= SN.TDH_Version)
      then
         raise Stale_Reference_Error;
      end if;
   end Check_Safety_Net;

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Token_Reference) return Boolean is
      pragma Assert (Left.TDH = Right.TDH);
   begin
      Check_Safety_Net (Left);
      Check_Safety_Net (Right);
      if Left.Index.Token < Right.Index.Token then
         return True;

      elsif Left.Index.Token = Right.Index.Token then
         return Left.Index.Trivia < Right.Index.Trivia;

      else
         return False;
      end if;
   end "<";

   ----------
   -- Next --
   ----------

   function Next
     (Token          : Token_Reference;
      Exclude_Trivia : Boolean := False) return Token_Reference is
   begin
      Check_Safety_Net (Token);
      return (if Token.TDH = null
              then No_Token
              else Rewrap_Token (Token,
                                 Next (Token.Index, Token.TDH.all,
                                       Exclude_Trivia)));
   end Next;

   --------------
   -- Previous --
   --------------

   function Previous
     (Token          : Token_Reference;
      Exclude_Trivia : Boolean := False) return Token_Reference is
   begin
      Check_Safety_Net (Token);
      return (if Token.TDH = null
              then No_Token
              else Rewrap_Token (Token,
                                 Previous (Token.Index, Token.TDH.all,
                                           Exclude_Trivia)));
   end Previous;

   ----------------
   -- Get_Symbol --
   ----------------

   function Get_Symbol (Token : Token_Reference) return Symbol_Type is
   begin
      Check_Safety_Net (Token);
      if Token.TDH = null then
         raise Precondition_Failure with "null token argument";
      end if;
      return Get_Symbol (Token.Index, Token.TDH.all);
   end Get_Symbol;

   ----------
   -- Data --
   ----------

   function Data (Token : Token_Reference) return Token_Data_Type is
   begin
      Check_Safety_Net (Token);
      if Token.TDH = null then
         raise Precondition_Failure with "null token argument";
      end if;
      return Convert (Token.TDH.all, Token, Raw_Data (Token));
   end Data;

   ----------
   -- Text --
   ----------

   function Text (Token : Token_Reference) return Text_Type is
      RD : constant Stored_Token_Data := Raw_Data (Token);
   begin
      Check_Safety_Net (Token);
      if Token.TDH = null then
         raise Precondition_Failure with "null token argument";
      end if;
      return Token.TDH.Source_Buffer (RD.Source_First .. RD.Source_Last);
   end Text;

   ----------
   -- Text --
   ----------

   function Text (First, Last : Token_Reference) return Text_Type is
      FD, LD : Token_Data_Type;
   begin
      Check_Safety_Net (First);
      Check_Safety_Net (Last);
      if First.TDH = null then
         raise Precondition_Failure with "null token argument";
      end if;
      if First.TDH /= Last.TDH then
         raise Precondition_Failure with
            "token arguments must belong to the same source";
      end if;
      FD := Data (First);
      LD := Data (Last);
      return FD.Source_Buffer.all (FD.Source_First .. LD.Source_Last);
   end Text;

   ----------
   -- Kind --
   ----------

   function Kind (Token_Data : Token_Data_Type) return Token_Kind is
   begin
      return Token_Data.Kind;
   end Kind;

   ---------------
   -- Is_Trivia --
   ---------------

   function Is_Trivia (Token : Token_Reference) return Boolean is
   begin
      Check_Safety_Net (Token);
      return Token.Index.Trivia /= No_Token_Index;
   end Is_Trivia;

   ---------------
   -- Is_Trivia --
   ---------------

   function Is_Trivia (Token_Data : Token_Data_Type) return Boolean is
   begin
      return Token_Data.Is_Trivia;
   end Is_Trivia;

   -----------
   -- Index --
   -----------

   function Index (Token : Token_Reference) return Token_Index is
   begin
      Check_Safety_Net (Token);
      return (if Token.Index.Trivia = No_Token_Index
              then Token.Index.Token
              else Token.Index.Trivia);
   end Index;

   -----------
   -- Index --
   -----------

   function Index (Token_Data : Token_Data_Type) return Token_Index is
   begin
      return Token_Data.Index;
   end Index;

   ----------------
   -- Sloc_Range --
   ----------------

   function Sloc_Range
     (Token_Data : Token_Data_Type) return Source_Location_Range
   is
   begin
      return Token_Data.Sloc_Range;
   end Sloc_Range;

   ---------------------
   -- Origin_Filename --
   ---------------------

   function Origin_Filename (Token : Token_Reference) return String is
   begin
      Check_Safety_Net (Token);
      if Token.TDH = null then
         raise Precondition_Failure with "null token argument";
      end if;
      return +Token.TDH.Filename.Full_Name;
   end Origin_Filename;

   --------------------
   -- Origin_Charset --
   --------------------

   function Origin_Charset (Token : Token_Reference) return String is
   begin
      Check_Safety_Net (Token);
      if Token.TDH = null then
         raise Precondition_Failure with "null token argument";
      end if;
      return To_String (Token.TDH.Charset);
   end Origin_Charset;

   -------------------
   -- Is_Equivalent --
   -------------------

   function Is_Equivalent (L, R : Token_Reference) return Boolean is
      DL : constant Token_Data_Type := Data (L);
      DR : constant Token_Data_Type := Data (R);
      TL : constant Text_Type := Text (L);
      TR : constant Text_Type := Text (R);
   begin
      return DL.Kind = DR.Kind and then TL = TR;
   end Is_Equivalent;

   -----------
   -- Image --
   -----------

   function Image (Token : Token_Reference) return String is
      D : constant Token_Data_Type := Data (Token);
   begin
      return ("<Token Kind=" & Token_Kind_Name (D.Kind) &
              " Text=" & Image (Text (Token), With_Quotes => True) & ">");
   end Image;

   --------------
   -- Raw_Data --
   --------------

   function Raw_Data (T : Token_Reference) return Stored_Token_Data is
   begin
      Check_Safety_Net (T);
      if T.TDH = null then
         raise Precondition_Failure with "null token argument";
      end if;
      return
        (if T.Index.Trivia = No_Token_Index
         then Token_Vectors.Get (T.TDH.Tokens, Natural (T.Index.Token))
         else Trivia_Vectors.Get (T.TDH.Trivias, Natural (T.Index.Trivia)).T);
   end Raw_Data;

   -------------
   -- Convert --
   -------------

   function Convert
     (TDH      : Token_Data_Handler;
      Token    : Token_Reference;
      Raw_Data : Stored_Token_Data) return Token_Data_Type is
   begin
      Check_Safety_Net (Token);
      return (Kind          => To_Token_Kind (Raw_Data.Kind),
              Is_Trivia     => Token.Index.Trivia /= No_Token_Index,
              Index         => (if Token.Index.Trivia = No_Token_Index
                                then Token.Index.Token
                                else Token.Index.Trivia),
              Source_Buffer => Text_Cst_Access (TDH.Source_Buffer),
              Source_First  => Raw_Data.Source_First,
              Source_Last   => Raw_Data.Source_Last,
              Sloc_Range    => Sloc_Range (TDH, Raw_Data));
   end Convert;


   ------------------
   -- From_Generic --
   ------------------

   function From_Generic (Token : Lk_Token) return Common.Token_Reference is
      use Langkit_Support.Internal.Conversions;
      Id         : Any_Language_Id;
      Data       : Langkit_Support.Internal.Analysis.Internal_Token;
      Safety_Net : Langkit_Support.Internal.Analysis.Token_Safety_Net;
   begin
      Unwrap_Token (Token, Id, Data, Safety_Net);
      pragma Assert (Id = Generic_API.Self_Id);
      return (Data.TDH,
              Data.Index,
              (Safety_Net.Context,
               Safety_Net.Context_Version,
               Safety_Net.TDH_Version));
   end From_Generic;

   ----------------
   -- To_Generic --
   ----------------

   function To_Generic (Token : Common.Token_Reference) return Lk_Token is
      use Langkit_Support.Internal.Conversions;
   begin
      return Wrap_Token
        (Generic_API.Self_Id,
         (Token.TDH, Token.Index),
         (Token.Safety_Net.Context,
          Token.Safety_Net.Context_Version,
          Token.Safety_Net.TDH_Version));
   end To_Generic;


   --------------------------
   -- Wrap_Token_Reference --
   --------------------------

   function Wrap_Token_Reference
     (Context : Internal_Context;
      TDH     : Token_Data_Handler_Access;
      Index   : Token_Or_Trivia_Index) return Token_Reference is
   begin
      if Index = No_Token_Or_Trivia_Index then
         return No_Token;
      end if;

      declare
         SN : constant Token_Safety_Net :=
           (Context         => +Context,
            Context_Version => Context.Serial_Number,
            TDH_Version     => TDH.Version);
      begin
        return (TDH, Index, SN);
      end;
   end Wrap_Token_Reference;

   --------------------
   -- Get_Token_Unit --
   --------------------

   function Get_Token_Unit (Token : Token_Reference) return Internal_Unit is
      function "+" is new Ada.Unchecked_Conversion
        (System.Address, Internal_Unit);
   begin
      if Token = No_Token then
         raise Precondition_Failure with "null token argument";
      end if;
      Check_Safety_Net (Token);
      return +Token.TDH.Owner;
   end Get_Token_Unit;

   -----------------------
   -- Get_Token_Context --
   -----------------------

   function Get_Token_Context
     (Token : Token_Reference) return Internal_Context is
   begin
      return +Token.Safety_Net.Context;
   end Get_Token_Context;

   -------------------
   -- Get_Token_TDH --
   -------------------

   function Get_Token_TDH
     (Token : Token_Reference) return Token_Data_Handler_Access is
   begin
      return Token.TDH;
   end Get_Token_TDH;

   ---------------------
   -- Get_Token_Index --
   ---------------------

   function Get_Token_Index
     (Token : Token_Reference) return Token_Or_Trivia_Index is
   begin
      return Token.Index;
   end Get_Token_Index;

   ------------------------
   -- Extract_Token_Text --
   ------------------------

   procedure Extract_Token_Text
     (Token         : Token_Data_Type;
      Source_Buffer : out Text_Cst_Access;
      First         : out Positive;
      Last          : out Natural) is
   begin
      Source_Buffer := Token.Source_Buffer;
      First := Token.Source_First;
      Last := Token.Source_Last;
   end Extract_Token_Text;


begin
   --  Check that we actually have full Libiconv support: as nothing works
   --  without it, we explicitly check support here instead of letting
   --  user-unfriendly errors happen during lexing.

   if not GNATCOLL.Iconv.Has_Iconv then
      raise Program_Error with "Libiconv is not available";
   end if;


   Private_Converters.Wrap_Token_Reference := Wrap_Token_Reference'Access;
   Private_Converters.Get_Token_Context := Get_Token_Context'Access;
   Private_Converters.Get_Token_Unit := Get_Token_Unit'Access;
   Private_Converters.Get_Token_TDH := Get_Token_TDH'Access;
   Private_Converters.Get_Token_Index := Get_Token_Index'Access;
   Private_Converters.Extract_Token_Text := Extract_Token_Text'Access;
end Libadalang.Common;
