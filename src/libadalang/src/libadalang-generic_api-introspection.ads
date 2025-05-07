--
--  Copyright (C) 2014-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

--  This package provides contants to refer to Libadalang types and struct
--  members in the generic introspection API
--  (``Langkit_Support.Generic_API.Introspection``).

with Langkit_Support.Generic_API.Introspection;

package Libadalang.Generic_API.Introspection is

   package G renames Langkit_Support.Generic_API.Introspection;

   ---------------------
   -- Type references --
   ---------------------

   package Type_Refs is
         Analysis_Unit : constant G.Type_Ref :=
           G.From_Index (Self_Id, 1);
         Big_Integer : constant G.Type_Ref :=
           G.From_Index (Self_Id, 2);
         Boolean : constant G.Type_Ref :=
           G.From_Index (Self_Id, 3);
         Character_Type : constant G.Type_Ref :=
           G.From_Index (Self_Id, 4);
         Integer : constant G.Type_Ref :=
           G.From_Index (Self_Id, 5);
         Source_Location_Range : constant G.Type_Ref :=
           G.From_Index (Self_Id, 6);
         Text_Type : constant G.Type_Ref :=
           G.From_Index (Self_Id, 7);
         Token_Reference : constant G.Type_Ref :=
           G.From_Index (Self_Id, 8);
         Unbounded_Text_Type : constant G.Type_Ref :=
           G.From_Index (Self_Id, 9);
         Analysis_Unit_Kind : constant G.Type_Ref :=
           G.From_Index (Self_Id, 10);
         Lookup_Kind : constant G.Type_Ref :=
           G.From_Index (Self_Id, 11);
         Designated_Env_Kind : constant G.Type_Ref :=
           G.From_Index (Self_Id, 12);
         Ref_Result_Kind : constant G.Type_Ref :=
           G.From_Index (Self_Id, 13);
         Call_Expr_Kind : constant G.Type_Ref :=
           G.From_Index (Self_Id, 14);
         Grammar_Rule : constant G.Type_Ref :=
           G.From_Index (Self_Id, 15);
         Discriminant_Values_Array : constant G.Type_Ref :=
           G.From_Index (Self_Id, 16);
         Doc_Annotation_Array : constant G.Type_Ref :=
           G.From_Index (Self_Id, 17);
         Accept_Stmt_Array : constant G.Type_Ref :=
           G.From_Index (Self_Id, 18);
         Ada_Node_Array : constant G.Type_Ref :=
           G.From_Index (Self_Id, 19);
         Base_Formal_Param_Decl_Array : constant G.Type_Ref :=
           G.From_Index (Self_Id, 20);
         Base_Type_Decl_Array : constant G.Type_Ref :=
           G.From_Index (Self_Id, 21);
         Basic_Decl_Array : constant G.Type_Ref :=
           G.From_Index (Self_Id, 22);
         Compilation_Unit_Array : constant G.Type_Ref :=
           G.From_Index (Self_Id, 23);
         Defining_Name_Array : constant G.Type_Ref :=
           G.From_Index (Self_Id, 24);
         Expr_Array : constant G.Type_Ref :=
           G.From_Index (Self_Id, 25);
         Generic_Instantiation_Array : constant G.Type_Ref :=
           G.From_Index (Self_Id, 26);
         Param_Spec_Array : constant G.Type_Ref :=
           G.From_Index (Self_Id, 27);
         Pragma_Node_Array : constant G.Type_Ref :=
           G.From_Index (Self_Id, 28);
         Type_Decl_Array : constant G.Type_Ref :=
           G.From_Index (Self_Id, 29);
         Param_Actual_Array : constant G.Type_Ref :=
           G.From_Index (Self_Id, 30);
         Ref_Result_Array : constant G.Type_Ref :=
           G.From_Index (Self_Id, 31);
         Shape_Array : constant G.Type_Ref :=
           G.From_Index (Self_Id, 32);
         Substitution_Array : constant G.Type_Ref :=
           G.From_Index (Self_Id, 33);
         Analysis_Unit_Array : constant G.Type_Ref :=
           G.From_Index (Self_Id, 34);
         Unbounded_Text_Type_Array : constant G.Type_Ref :=
           G.From_Index (Self_Id, 35);
         Completion_Item_Iterator : constant G.Type_Ref :=
           G.From_Index (Self_Id, 36);
         Aspect : constant G.Type_Ref :=
           G.From_Index (Self_Id, 37);
         Completion_Item : constant G.Type_Ref :=
           G.From_Index (Self_Id, 38);
         Discrete_Range : constant G.Type_Ref :=
           G.From_Index (Self_Id, 39);
         Discriminant_Values : constant G.Type_Ref :=
           G.From_Index (Self_Id, 40);
         Doc_Annotation : constant G.Type_Ref :=
           G.From_Index (Self_Id, 41);
         Param_Actual : constant G.Type_Ref :=
           G.From_Index (Self_Id, 42);
         Ref_Result : constant G.Type_Ref :=
           G.From_Index (Self_Id, 43);
         Refd_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 44);
         Refd_Def : constant G.Type_Ref :=
           G.From_Index (Self_Id, 45);
         Shape : constant G.Type_Ref :=
           G.From_Index (Self_Id, 46);
         Substitution : constant G.Type_Ref :=
           G.From_Index (Self_Id, 47);
         Ada_Node : constant G.Type_Ref :=
           G.From_Index (Self_Id, 48);
         Abort_Node : constant G.Type_Ref :=
           G.From_Index (Self_Id, 49);
         Abort_Absent : constant G.Type_Ref :=
           G.From_Index (Self_Id, 50);
         Abort_Present : constant G.Type_Ref :=
           G.From_Index (Self_Id, 51);
         Abstract_Node : constant G.Type_Ref :=
           G.From_Index (Self_Id, 52);
         Abstract_Absent : constant G.Type_Ref :=
           G.From_Index (Self_Id, 53);
         Abstract_Present : constant G.Type_Ref :=
           G.From_Index (Self_Id, 54);
         Ada_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 55);
         Ada_Node_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 56);
         Abstract_State_Decl_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 57);
         Alternatives_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 58);
         Constraint_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 59);
         Decl_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 60);
         Stmt_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 61);
         Aspect_Assoc_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 62);
         Base_Assoc_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 63);
         Basic_Assoc_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 64);
         Assoc_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 65);
         Basic_Decl_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 66);
         Case_Expr_Alternative_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 67);
         Case_Stmt_Alternative_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 68);
         Compilation_Unit_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 69);
         Concat_Operand_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 70);
         Contract_Case_Assoc_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 71);
         Defining_Name_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 72);
         Discriminant_Spec_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 73);
         Elsif_Expr_Part_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 74);
         Elsif_Stmt_Part_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 75);
         Enum_Literal_Decl_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 76);
         Expr_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 77);
         Expr_Alternatives_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 78);
         Identifier_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 79);
         Discriminant_Choice_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 80);
         Name_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 81);
         Parent_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 82);
         Param_Spec_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 83);
         Pragma_Node_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 84);
         Select_When_Part_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 85);
         Unconstrained_Array_Index_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 86);
         Variant_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 87);
         Aliased_Node : constant G.Type_Ref :=
           G.From_Index (Self_Id, 88);
         Aliased_Absent : constant G.Type_Ref :=
           G.From_Index (Self_Id, 89);
         Aliased_Present : constant G.Type_Ref :=
           G.From_Index (Self_Id, 90);
         All_Node : constant G.Type_Ref :=
           G.From_Index (Self_Id, 91);
         All_Absent : constant G.Type_Ref :=
           G.From_Index (Self_Id, 92);
         All_Present : constant G.Type_Ref :=
           G.From_Index (Self_Id, 93);
         Array_Indices : constant G.Type_Ref :=
           G.From_Index (Self_Id, 94);
         Constrained_Array_Indices : constant G.Type_Ref :=
           G.From_Index (Self_Id, 95);
         Unconstrained_Array_Indices : constant G.Type_Ref :=
           G.From_Index (Self_Id, 96);
         Aspect_Assoc : constant G.Type_Ref :=
           G.From_Index (Self_Id, 97);
         Aspect_Clause : constant G.Type_Ref :=
           G.From_Index (Self_Id, 98);
         At_Clause : constant G.Type_Ref :=
           G.From_Index (Self_Id, 99);
         Attribute_Def_Clause : constant G.Type_Ref :=
           G.From_Index (Self_Id, 100);
         Enum_Rep_Clause : constant G.Type_Ref :=
           G.From_Index (Self_Id, 101);
         Record_Rep_Clause : constant G.Type_Ref :=
           G.From_Index (Self_Id, 102);
         Aspect_Spec : constant G.Type_Ref :=
           G.From_Index (Self_Id, 103);
         Base_Assoc : constant G.Type_Ref :=
           G.From_Index (Self_Id, 104);
         Contract_Case_Assoc : constant G.Type_Ref :=
           G.From_Index (Self_Id, 105);
         Pragma_Argument_Assoc : constant G.Type_Ref :=
           G.From_Index (Self_Id, 106);
         Base_Formal_Param_Holder : constant G.Type_Ref :=
           G.From_Index (Self_Id, 107);
         Base_Subp_Spec : constant G.Type_Ref :=
           G.From_Index (Self_Id, 108);
         Entry_Spec : constant G.Type_Ref :=
           G.From_Index (Self_Id, 109);
         Enum_Subp_Spec : constant G.Type_Ref :=
           G.From_Index (Self_Id, 110);
         Subp_Spec : constant G.Type_Ref :=
           G.From_Index (Self_Id, 111);
         Synthetic_Binary_Spec : constant G.Type_Ref :=
           G.From_Index (Self_Id, 112);
         Synthetic_Unary_Spec : constant G.Type_Ref :=
           G.From_Index (Self_Id, 113);
         Component_List : constant G.Type_Ref :=
           G.From_Index (Self_Id, 114);
         Discriminant_Part : constant G.Type_Ref :=
           G.From_Index (Self_Id, 115);
         Known_Discriminant_Part : constant G.Type_Ref :=
           G.From_Index (Self_Id, 116);
         Unknown_Discriminant_Part : constant G.Type_Ref :=
           G.From_Index (Self_Id, 117);
         Entry_Completion_Formal_Params : constant G.Type_Ref :=
           G.From_Index (Self_Id, 118);
         Generic_Formal_Part : constant G.Type_Ref :=
           G.From_Index (Self_Id, 119);
         Base_Record_Def : constant G.Type_Ref :=
           G.From_Index (Self_Id, 120);
         Null_Record_Def : constant G.Type_Ref :=
           G.From_Index (Self_Id, 121);
         Record_Def : constant G.Type_Ref :=
           G.From_Index (Self_Id, 122);
         Basic_Assoc : constant G.Type_Ref :=
           G.From_Index (Self_Id, 123);
         Aggregate_Assoc : constant G.Type_Ref :=
           G.From_Index (Self_Id, 124);
         Multi_Dim_Array_Assoc : constant G.Type_Ref :=
           G.From_Index (Self_Id, 125);
         Composite_Constraint_Assoc : constant G.Type_Ref :=
           G.From_Index (Self_Id, 126);
         Iterated_Assoc : constant G.Type_Ref :=
           G.From_Index (Self_Id, 127);
         Param_Assoc : constant G.Type_Ref :=
           G.From_Index (Self_Id, 128);
         Basic_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 129);
         Abstract_State_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 130);
         Anonymous_Expr_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 131);
         Base_Formal_Param_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 132);
         Component_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 133);
         Discriminant_Spec : constant G.Type_Ref :=
           G.From_Index (Self_Id, 134);
         Generic_Formal : constant G.Type_Ref :=
           G.From_Index (Self_Id, 135);
         Generic_Formal_Obj_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 136);
         Generic_Formal_Package : constant G.Type_Ref :=
           G.From_Index (Self_Id, 137);
         Generic_Formal_Subp_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 138);
         Generic_Formal_Type_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 139);
         Param_Spec : constant G.Type_Ref :=
           G.From_Index (Self_Id, 140);
         Synthetic_Formal_Param_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 141);
         Base_Package_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 142);
         Generic_Package_Internal : constant G.Type_Ref :=
           G.From_Index (Self_Id, 143);
         Package_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 144);
         Base_Type_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 145);
         Base_Subtype_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 146);
         Discrete_Base_Subtype_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 147);
         Subtype_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 148);
         Classwide_Type_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 149);
         Incomplete_Type_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 150);
         Incomplete_Formal_Type_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 151);
         Incomplete_Tagged_Type_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 152);
         Protected_Type_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 153);
         Task_Type_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 154);
         Single_Task_Type_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 155);
         Type_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 156);
         Anonymous_Type_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 157);
         Synth_Anonymous_Type_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 158);
         Concrete_Type_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 159);
         Formal_Type_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 160);
         Basic_Subp_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 161);
         Classic_Subp_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 162);
         Abstract_Subp_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 163);
         Formal_Subp_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 164);
         Abstract_Formal_Subp_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 165);
         Concrete_Formal_Subp_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 166);
         Subp_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 167);
         Entry_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 168);
         Enum_Literal_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 169);
         Synthetic_Char_Enum_Lit : constant G.Type_Ref :=
           G.From_Index (Self_Id, 170);
         Generic_Subp_Internal : constant G.Type_Ref :=
           G.From_Index (Self_Id, 171);
         Synthetic_Subp_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 172);
         Body_Node : constant G.Type_Ref :=
           G.From_Index (Self_Id, 173);
         Base_Subp_Body : constant G.Type_Ref :=
           G.From_Index (Self_Id, 174);
         Expr_Function : constant G.Type_Ref :=
           G.From_Index (Self_Id, 175);
         Null_Subp_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 176);
         Subp_Body : constant G.Type_Ref :=
           G.From_Index (Self_Id, 177);
         Subp_Renaming_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 178);
         Body_Stub : constant G.Type_Ref :=
           G.From_Index (Self_Id, 179);
         Package_Body_Stub : constant G.Type_Ref :=
           G.From_Index (Self_Id, 180);
         Protected_Body_Stub : constant G.Type_Ref :=
           G.From_Index (Self_Id, 181);
         Subp_Body_Stub : constant G.Type_Ref :=
           G.From_Index (Self_Id, 182);
         Task_Body_Stub : constant G.Type_Ref :=
           G.From_Index (Self_Id, 183);
         Entry_Body : constant G.Type_Ref :=
           G.From_Index (Self_Id, 184);
         Package_Body : constant G.Type_Ref :=
           G.From_Index (Self_Id, 185);
         Protected_Body : constant G.Type_Ref :=
           G.From_Index (Self_Id, 186);
         Task_Body : constant G.Type_Ref :=
           G.From_Index (Self_Id, 187);
         Entry_Index_Spec : constant G.Type_Ref :=
           G.From_Index (Self_Id, 188);
         Error_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 189);
         Exception_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 190);
         Exception_Handler : constant G.Type_Ref :=
           G.From_Index (Self_Id, 191);
         For_Loop_Var_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 192);
         Generic_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 193);
         Generic_Package_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 194);
         Generic_Subp_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 195);
         Generic_Instantiation : constant G.Type_Ref :=
           G.From_Index (Self_Id, 196);
         Generic_Package_Instantiation : constant G.Type_Ref :=
           G.From_Index (Self_Id, 197);
         Generic_Subp_Instantiation : constant G.Type_Ref :=
           G.From_Index (Self_Id, 198);
         Generic_Renaming_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 199);
         Generic_Package_Renaming_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 200);
         Generic_Subp_Renaming_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 201);
         Label_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 202);
         Named_Stmt_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 203);
         Number_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 204);
         Object_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 205);
         Extended_Return_Stmt_Object_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 206);
         No_Type_Object_Renaming_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 207);
         Package_Renaming_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 208);
         Single_Protected_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 209);
         Single_Task_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 210);
         Case_Stmt_Alternative : constant G.Type_Ref :=
           G.From_Index (Self_Id, 211);
         Compilation_Unit : constant G.Type_Ref :=
           G.From_Index (Self_Id, 212);
         Component_Clause : constant G.Type_Ref :=
           G.From_Index (Self_Id, 213);
         Component_Def : constant G.Type_Ref :=
           G.From_Index (Self_Id, 214);
         Constant_Node : constant G.Type_Ref :=
           G.From_Index (Self_Id, 215);
         Constant_Absent : constant G.Type_Ref :=
           G.From_Index (Self_Id, 216);
         Constant_Present : constant G.Type_Ref :=
           G.From_Index (Self_Id, 217);
         Constraint : constant G.Type_Ref :=
           G.From_Index (Self_Id, 218);
         Composite_Constraint : constant G.Type_Ref :=
           G.From_Index (Self_Id, 219);
         Delta_Constraint : constant G.Type_Ref :=
           G.From_Index (Self_Id, 220);
         Digits_Constraint : constant G.Type_Ref :=
           G.From_Index (Self_Id, 221);
         Range_Constraint : constant G.Type_Ref :=
           G.From_Index (Self_Id, 222);
         Declarative_Part : constant G.Type_Ref :=
           G.From_Index (Self_Id, 223);
         Private_Part : constant G.Type_Ref :=
           G.From_Index (Self_Id, 224);
         Public_Part : constant G.Type_Ref :=
           G.From_Index (Self_Id, 225);
         Elsif_Expr_Part : constant G.Type_Ref :=
           G.From_Index (Self_Id, 226);
         Elsif_Stmt_Part : constant G.Type_Ref :=
           G.From_Index (Self_Id, 227);
         Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 228);
         Abstract_State_Decl_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 229);
         Allocator : constant G.Type_Ref :=
           G.From_Index (Self_Id, 230);
         Base_Aggregate : constant G.Type_Ref :=
           G.From_Index (Self_Id, 231);
         Aggregate : constant G.Type_Ref :=
           G.From_Index (Self_Id, 232);
         Bracket_Aggregate : constant G.Type_Ref :=
           G.From_Index (Self_Id, 233);
         Delta_Aggregate : constant G.Type_Ref :=
           G.From_Index (Self_Id, 234);
         Bracket_Delta_Aggregate : constant G.Type_Ref :=
           G.From_Index (Self_Id, 235);
         Null_Record_Aggregate : constant G.Type_Ref :=
           G.From_Index (Self_Id, 236);
         Bin_Op : constant G.Type_Ref :=
           G.From_Index (Self_Id, 237);
         Relation_Op : constant G.Type_Ref :=
           G.From_Index (Self_Id, 238);
         Box_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 239);
         Case_Expr_Alternative : constant G.Type_Ref :=
           G.From_Index (Self_Id, 240);
         Concat_Op : constant G.Type_Ref :=
           G.From_Index (Self_Id, 241);
         Concat_Operand : constant G.Type_Ref :=
           G.From_Index (Self_Id, 242);
         Cond_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 243);
         Case_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 244);
         If_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 245);
         Contract_Cases : constant G.Type_Ref :=
           G.From_Index (Self_Id, 246);
         Decl_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 247);
         Membership_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 248);
         Name : constant G.Type_Ref :=
           G.From_Index (Self_Id, 249);
         Attribute_Ref : constant G.Type_Ref :=
           G.From_Index (Self_Id, 250);
         Call_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 251);
         Defining_Name : constant G.Type_Ref :=
           G.From_Index (Self_Id, 252);
         Synthetic_Defining_Name : constant G.Type_Ref :=
           G.From_Index (Self_Id, 253);
         Discrete_Subtype_Name : constant G.Type_Ref :=
           G.From_Index (Self_Id, 254);
         Dotted_Name : constant G.Type_Ref :=
           G.From_Index (Self_Id, 255);
         End_Name : constant G.Type_Ref :=
           G.From_Index (Self_Id, 256);
         Explicit_Deref : constant G.Type_Ref :=
           G.From_Index (Self_Id, 257);
         Qual_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 258);
         Reduce_Attribute_Ref : constant G.Type_Ref :=
           G.From_Index (Self_Id, 259);
         Single_Tok_Node : constant G.Type_Ref :=
           G.From_Index (Self_Id, 260);
         Base_Id : constant G.Type_Ref :=
           G.From_Index (Self_Id, 261);
         Char_Literal : constant G.Type_Ref :=
           G.From_Index (Self_Id, 262);
         Identifier : constant G.Type_Ref :=
           G.From_Index (Self_Id, 263);
         Op : constant G.Type_Ref :=
           G.From_Index (Self_Id, 264);
         Op_Abs : constant G.Type_Ref :=
           G.From_Index (Self_Id, 265);
         Op_And : constant G.Type_Ref :=
           G.From_Index (Self_Id, 266);
         Op_And_Then : constant G.Type_Ref :=
           G.From_Index (Self_Id, 267);
         Op_Concat : constant G.Type_Ref :=
           G.From_Index (Self_Id, 268);
         Op_Div : constant G.Type_Ref :=
           G.From_Index (Self_Id, 269);
         Op_Double_Dot : constant G.Type_Ref :=
           G.From_Index (Self_Id, 270);
         Op_Eq : constant G.Type_Ref :=
           G.From_Index (Self_Id, 271);
         Op_Gt : constant G.Type_Ref :=
           G.From_Index (Self_Id, 272);
         Op_Gte : constant G.Type_Ref :=
           G.From_Index (Self_Id, 273);
         Op_In : constant G.Type_Ref :=
           G.From_Index (Self_Id, 274);
         Op_Lt : constant G.Type_Ref :=
           G.From_Index (Self_Id, 275);
         Op_Lte : constant G.Type_Ref :=
           G.From_Index (Self_Id, 276);
         Op_Minus : constant G.Type_Ref :=
           G.From_Index (Self_Id, 277);
         Op_Mod : constant G.Type_Ref :=
           G.From_Index (Self_Id, 278);
         Op_Mult : constant G.Type_Ref :=
           G.From_Index (Self_Id, 279);
         Op_Neq : constant G.Type_Ref :=
           G.From_Index (Self_Id, 280);
         Op_Not : constant G.Type_Ref :=
           G.From_Index (Self_Id, 281);
         Op_Not_In : constant G.Type_Ref :=
           G.From_Index (Self_Id, 282);
         Op_Or : constant G.Type_Ref :=
           G.From_Index (Self_Id, 283);
         Op_Or_Else : constant G.Type_Ref :=
           G.From_Index (Self_Id, 284);
         Op_Plus : constant G.Type_Ref :=
           G.From_Index (Self_Id, 285);
         Op_Pow : constant G.Type_Ref :=
           G.From_Index (Self_Id, 286);
         Op_Rem : constant G.Type_Ref :=
           G.From_Index (Self_Id, 287);
         Op_Xor : constant G.Type_Ref :=
           G.From_Index (Self_Id, 288);
         String_Literal : constant G.Type_Ref :=
           G.From_Index (Self_Id, 289);
         Null_Literal : constant G.Type_Ref :=
           G.From_Index (Self_Id, 290);
         Num_Literal : constant G.Type_Ref :=
           G.From_Index (Self_Id, 291);
         Int_Literal : constant G.Type_Ref :=
           G.From_Index (Self_Id, 292);
         Real_Literal : constant G.Type_Ref :=
           G.From_Index (Self_Id, 293);
         Synthetic_Identifier : constant G.Type_Ref :=
           G.From_Index (Self_Id, 294);
         Target_Name : constant G.Type_Ref :=
           G.From_Index (Self_Id, 295);
         Update_Attribute_Ref : constant G.Type_Ref :=
           G.From_Index (Self_Id, 296);
         Paren_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 297);
         Quantified_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 298);
         Raise_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 299);
         Un_Op : constant G.Type_Ref :=
           G.From_Index (Self_Id, 300);
         Handled_Stmts : constant G.Type_Ref :=
           G.From_Index (Self_Id, 301);
         Interface_Kind : constant G.Type_Ref :=
           G.From_Index (Self_Id, 302);
         Interface_Kind_Limited : constant G.Type_Ref :=
           G.From_Index (Self_Id, 303);
         Interface_Kind_Protected : constant G.Type_Ref :=
           G.From_Index (Self_Id, 304);
         Interface_Kind_Synchronized : constant G.Type_Ref :=
           G.From_Index (Self_Id, 305);
         Interface_Kind_Task : constant G.Type_Ref :=
           G.From_Index (Self_Id, 306);
         Iter_Type : constant G.Type_Ref :=
           G.From_Index (Self_Id, 307);
         Iter_Type_In : constant G.Type_Ref :=
           G.From_Index (Self_Id, 308);
         Iter_Type_Of : constant G.Type_Ref :=
           G.From_Index (Self_Id, 309);
         Library_Item : constant G.Type_Ref :=
           G.From_Index (Self_Id, 310);
         Limited_Node : constant G.Type_Ref :=
           G.From_Index (Self_Id, 311);
         Limited_Absent : constant G.Type_Ref :=
           G.From_Index (Self_Id, 312);
         Limited_Present : constant G.Type_Ref :=
           G.From_Index (Self_Id, 313);
         Loop_Spec : constant G.Type_Ref :=
           G.From_Index (Self_Id, 314);
         For_Loop_Spec : constant G.Type_Ref :=
           G.From_Index (Self_Id, 315);
         While_Loop_Spec : constant G.Type_Ref :=
           G.From_Index (Self_Id, 316);
         Mode : constant G.Type_Ref :=
           G.From_Index (Self_Id, 317);
         Mode_Default : constant G.Type_Ref :=
           G.From_Index (Self_Id, 318);
         Mode_In : constant G.Type_Ref :=
           G.From_Index (Self_Id, 319);
         Mode_In_Out : constant G.Type_Ref :=
           G.From_Index (Self_Id, 320);
         Mode_Out : constant G.Type_Ref :=
           G.From_Index (Self_Id, 321);
         Multi_Abstract_State_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 322);
         Not_Null : constant G.Type_Ref :=
           G.From_Index (Self_Id, 323);
         Not_Null_Absent : constant G.Type_Ref :=
           G.From_Index (Self_Id, 324);
         Not_Null_Present : constant G.Type_Ref :=
           G.From_Index (Self_Id, 325);
         Null_Component_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 326);
         Others_Designator : constant G.Type_Ref :=
           G.From_Index (Self_Id, 327);
         Overriding_Node : constant G.Type_Ref :=
           G.From_Index (Self_Id, 328);
         Overriding_Not_Overriding : constant G.Type_Ref :=
           G.From_Index (Self_Id, 329);
         Overriding_Overriding : constant G.Type_Ref :=
           G.From_Index (Self_Id, 330);
         Overriding_Unspecified : constant G.Type_Ref :=
           G.From_Index (Self_Id, 331);
         Params : constant G.Type_Ref :=
           G.From_Index (Self_Id, 332);
         Paren_Abstract_State_Decl : constant G.Type_Ref :=
           G.From_Index (Self_Id, 333);
         Pp_Directive : constant G.Type_Ref :=
           G.From_Index (Self_Id, 334);
         Pp_Else_Directive : constant G.Type_Ref :=
           G.From_Index (Self_Id, 335);
         Pp_Elsif_Directive : constant G.Type_Ref :=
           G.From_Index (Self_Id, 336);
         Pp_End_If_Directive : constant G.Type_Ref :=
           G.From_Index (Self_Id, 337);
         Pp_If_Directive : constant G.Type_Ref :=
           G.From_Index (Self_Id, 338);
         Pp_Then_Kw : constant G.Type_Ref :=
           G.From_Index (Self_Id, 339);
         Pragma_Node : constant G.Type_Ref :=
           G.From_Index (Self_Id, 340);
         Private_Node : constant G.Type_Ref :=
           G.From_Index (Self_Id, 341);
         Private_Absent : constant G.Type_Ref :=
           G.From_Index (Self_Id, 342);
         Private_Present : constant G.Type_Ref :=
           G.From_Index (Self_Id, 343);
         Protected_Def : constant G.Type_Ref :=
           G.From_Index (Self_Id, 344);
         Protected_Node : constant G.Type_Ref :=
           G.From_Index (Self_Id, 345);
         Protected_Absent : constant G.Type_Ref :=
           G.From_Index (Self_Id, 346);
         Protected_Present : constant G.Type_Ref :=
           G.From_Index (Self_Id, 347);
         Quantifier : constant G.Type_Ref :=
           G.From_Index (Self_Id, 348);
         Quantifier_All : constant G.Type_Ref :=
           G.From_Index (Self_Id, 349);
         Quantifier_Some : constant G.Type_Ref :=
           G.From_Index (Self_Id, 350);
         Range_Spec : constant G.Type_Ref :=
           G.From_Index (Self_Id, 351);
         Renaming_Clause : constant G.Type_Ref :=
           G.From_Index (Self_Id, 352);
         Synthetic_Renaming_Clause : constant G.Type_Ref :=
           G.From_Index (Self_Id, 353);
         Reverse_Node : constant G.Type_Ref :=
           G.From_Index (Self_Id, 354);
         Reverse_Absent : constant G.Type_Ref :=
           G.From_Index (Self_Id, 355);
         Reverse_Present : constant G.Type_Ref :=
           G.From_Index (Self_Id, 356);
         Select_When_Part : constant G.Type_Ref :=
           G.From_Index (Self_Id, 357);
         Stmt : constant G.Type_Ref :=
           G.From_Index (Self_Id, 358);
         Composite_Stmt : constant G.Type_Ref :=
           G.From_Index (Self_Id, 359);
         Accept_Stmt : constant G.Type_Ref :=
           G.From_Index (Self_Id, 360);
         Accept_Stmt_With_Stmts : constant G.Type_Ref :=
           G.From_Index (Self_Id, 361);
         Base_Loop_Stmt : constant G.Type_Ref :=
           G.From_Index (Self_Id, 362);
         For_Loop_Stmt : constant G.Type_Ref :=
           G.From_Index (Self_Id, 363);
         Loop_Stmt : constant G.Type_Ref :=
           G.From_Index (Self_Id, 364);
         While_Loop_Stmt : constant G.Type_Ref :=
           G.From_Index (Self_Id, 365);
         Block_Stmt : constant G.Type_Ref :=
           G.From_Index (Self_Id, 366);
         Begin_Block : constant G.Type_Ref :=
           G.From_Index (Self_Id, 367);
         Decl_Block : constant G.Type_Ref :=
           G.From_Index (Self_Id, 368);
         Case_Stmt : constant G.Type_Ref :=
           G.From_Index (Self_Id, 369);
         Extended_Return_Stmt : constant G.Type_Ref :=
           G.From_Index (Self_Id, 370);
         If_Stmt : constant G.Type_Ref :=
           G.From_Index (Self_Id, 371);
         Named_Stmt : constant G.Type_Ref :=
           G.From_Index (Self_Id, 372);
         Select_Stmt : constant G.Type_Ref :=
           G.From_Index (Self_Id, 373);
         Error_Stmt : constant G.Type_Ref :=
           G.From_Index (Self_Id, 374);
         Simple_Stmt : constant G.Type_Ref :=
           G.From_Index (Self_Id, 375);
         Abort_Stmt : constant G.Type_Ref :=
           G.From_Index (Self_Id, 376);
         Assign_Stmt : constant G.Type_Ref :=
           G.From_Index (Self_Id, 377);
         Call_Stmt : constant G.Type_Ref :=
           G.From_Index (Self_Id, 378);
         Delay_Stmt : constant G.Type_Ref :=
           G.From_Index (Self_Id, 379);
         Exit_Stmt : constant G.Type_Ref :=
           G.From_Index (Self_Id, 380);
         Goto_Stmt : constant G.Type_Ref :=
           G.From_Index (Self_Id, 381);
         Label : constant G.Type_Ref :=
           G.From_Index (Self_Id, 382);
         Null_Stmt : constant G.Type_Ref :=
           G.From_Index (Self_Id, 383);
         Raise_Stmt : constant G.Type_Ref :=
           G.From_Index (Self_Id, 384);
         Requeue_Stmt : constant G.Type_Ref :=
           G.From_Index (Self_Id, 385);
         Return_Stmt : constant G.Type_Ref :=
           G.From_Index (Self_Id, 386);
         Terminate_Alternative : constant G.Type_Ref :=
           G.From_Index (Self_Id, 387);
         Subp_Kind : constant G.Type_Ref :=
           G.From_Index (Self_Id, 388);
         Subp_Kind_Function : constant G.Type_Ref :=
           G.From_Index (Self_Id, 389);
         Subp_Kind_Procedure : constant G.Type_Ref :=
           G.From_Index (Self_Id, 390);
         Subunit : constant G.Type_Ref :=
           G.From_Index (Self_Id, 391);
         Synchronized_Node : constant G.Type_Ref :=
           G.From_Index (Self_Id, 392);
         Synchronized_Absent : constant G.Type_Ref :=
           G.From_Index (Self_Id, 393);
         Synchronized_Present : constant G.Type_Ref :=
           G.From_Index (Self_Id, 394);
         Tagged_Node : constant G.Type_Ref :=
           G.From_Index (Self_Id, 395);
         Tagged_Absent : constant G.Type_Ref :=
           G.From_Index (Self_Id, 396);
         Tagged_Present : constant G.Type_Ref :=
           G.From_Index (Self_Id, 397);
         Task_Def : constant G.Type_Ref :=
           G.From_Index (Self_Id, 398);
         Type_Attributes_Repository : constant G.Type_Ref :=
           G.From_Index (Self_Id, 399);
         Type_Def : constant G.Type_Ref :=
           G.From_Index (Self_Id, 400);
         Access_Def : constant G.Type_Ref :=
           G.From_Index (Self_Id, 401);
         Access_To_Subp_Def : constant G.Type_Ref :=
           G.From_Index (Self_Id, 402);
         Base_Type_Access_Def : constant G.Type_Ref :=
           G.From_Index (Self_Id, 403);
         Anonymous_Type_Access_Def : constant G.Type_Ref :=
           G.From_Index (Self_Id, 404);
         Type_Access_Def : constant G.Type_Ref :=
           G.From_Index (Self_Id, 405);
         Array_Type_Def : constant G.Type_Ref :=
           G.From_Index (Self_Id, 406);
         Derived_Type_Def : constant G.Type_Ref :=
           G.From_Index (Self_Id, 407);
         Enum_Type_Def : constant G.Type_Ref :=
           G.From_Index (Self_Id, 408);
         Formal_Discrete_Type_Def : constant G.Type_Ref :=
           G.From_Index (Self_Id, 409);
         Interface_Type_Def : constant G.Type_Ref :=
           G.From_Index (Self_Id, 410);
         Mod_Int_Type_Def : constant G.Type_Ref :=
           G.From_Index (Self_Id, 411);
         Private_Type_Def : constant G.Type_Ref :=
           G.From_Index (Self_Id, 412);
         Real_Type_Def : constant G.Type_Ref :=
           G.From_Index (Self_Id, 413);
         Decimal_Fixed_Point_Def : constant G.Type_Ref :=
           G.From_Index (Self_Id, 414);
         Floating_Point_Def : constant G.Type_Ref :=
           G.From_Index (Self_Id, 415);
         Ordinary_Fixed_Point_Def : constant G.Type_Ref :=
           G.From_Index (Self_Id, 416);
         Record_Type_Def : constant G.Type_Ref :=
           G.From_Index (Self_Id, 417);
         Signed_Int_Type_Def : constant G.Type_Ref :=
           G.From_Index (Self_Id, 418);
         Type_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 419);
         Anonymous_Type : constant G.Type_Ref :=
           G.From_Index (Self_Id, 420);
         Enum_Lit_Synth_Type_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 421);
         Subtype_Indication : constant G.Type_Ref :=
           G.From_Index (Self_Id, 422);
         Constrained_Subtype_Indication : constant G.Type_Ref :=
           G.From_Index (Self_Id, 423);
         Discrete_Subtype_Indication : constant G.Type_Ref :=
           G.From_Index (Self_Id, 424);
         Synthetic_Type_Expr : constant G.Type_Ref :=
           G.From_Index (Self_Id, 425);
         Unconstrained_Array_Index : constant G.Type_Ref :=
           G.From_Index (Self_Id, 426);
         Until_Node : constant G.Type_Ref :=
           G.From_Index (Self_Id, 427);
         Until_Absent : constant G.Type_Ref :=
           G.From_Index (Self_Id, 428);
         Until_Present : constant G.Type_Ref :=
           G.From_Index (Self_Id, 429);
         Use_Clause : constant G.Type_Ref :=
           G.From_Index (Self_Id, 430);
         Use_Package_Clause : constant G.Type_Ref :=
           G.From_Index (Self_Id, 431);
         Use_Type_Clause : constant G.Type_Ref :=
           G.From_Index (Self_Id, 432);
         Value_Sequence : constant G.Type_Ref :=
           G.From_Index (Self_Id, 433);
         Variant : constant G.Type_Ref :=
           G.From_Index (Self_Id, 434);
         Variant_Part : constant G.Type_Ref :=
           G.From_Index (Self_Id, 435);
         With_Clause : constant G.Type_Ref :=
           G.From_Index (Self_Id, 436);
         With_Private : constant G.Type_Ref :=
           G.From_Index (Self_Id, 437);
         With_Private_Absent : constant G.Type_Ref :=
           G.From_Index (Self_Id, 438);
         With_Private_Present : constant G.Type_Ref :=
           G.From_Index (Self_Id, 439);
   end Type_Refs;

   -----------------------
   -- Member references --
   -----------------------

   package Member_Refs is
         Aspect_Exists : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 1);
         Aspect_Node : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 2);
         Aspect_Value : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 3);
         Completion_Item_Decl : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 4);
         Completion_Item_Is_Dot_Call : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 5);
         Completion_Item_Is_Visible : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 6);
         Completion_Item_Weight : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 7);
         Discrete_Range_Low_Bound : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 8);
         Discrete_Range_High_Bound : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 9);
         Discriminant_Values_Discriminant : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 10);
         Discriminant_Values_Values : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 11);
         Doc_Annotation_Key : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 12);
         Doc_Annotation_Value : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 13);
         Param_Actual_Param : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 14);
         Param_Actual_Actual : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 15);
         Ref_Result_Ref : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 16);
         Ref_Result_Kind : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 17);
         Refd_Decl_Decl : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 18);
         Refd_Decl_Kind : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 19);
         Refd_Def_Def_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 20);
         Refd_Def_Kind : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 21);
         Shape_Components : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 22);
         Shape_Discriminants_Values : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 23);
         Substitution_From_Decl : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 24);
         Substitution_To_Value : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 25);
         Substitution_Value_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 26);
         Constrained_Array_Indices_F_List : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 27);
         Unconstrained_Array_Indices_F_Types : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 28);
         Aspect_Assoc_F_Id : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 29);
         Aspect_Assoc_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 30);
         At_Clause_F_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 31);
         At_Clause_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 32);
         Attribute_Def_Clause_F_Attribute_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 33);
         Attribute_Def_Clause_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 34);
         Enum_Rep_Clause_F_Type_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 35);
         Enum_Rep_Clause_F_Aggregate : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 36);
         Record_Rep_Clause_F_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 37);
         Record_Rep_Clause_F_At_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 38);
         Record_Rep_Clause_F_Components : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 39);
         Aspect_Spec_F_Aspect_Assocs : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 40);
         Contract_Case_Assoc_F_Guard : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 41);
         Contract_Case_Assoc_F_Consequence : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 42);
         Pragma_Argument_Assoc_F_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 43);
         Pragma_Argument_Assoc_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 44);
         Entry_Spec_F_Entry_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 45);
         Entry_Spec_F_Family_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 46);
         Entry_Spec_F_Entry_Params : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 47);
         Subp_Spec_F_Subp_Kind : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 48);
         Subp_Spec_F_Subp_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 49);
         Subp_Spec_F_Subp_Params : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 50);
         Subp_Spec_F_Subp_Returns : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 51);
         Synthetic_Binary_Spec_F_Left_Param : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 52);
         Synthetic_Binary_Spec_F_Right_Param : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 53);
         Synthetic_Binary_Spec_F_Return_Type_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 54);
         Synthetic_Unary_Spec_F_Right_Param : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 55);
         Synthetic_Unary_Spec_F_Return_Type_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 56);
         Component_List_F_Components : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 57);
         Component_List_F_Variant_Part : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 58);
         Known_Discriminant_Part_F_Discr_Specs : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 59);
         Entry_Completion_Formal_Params_F_Params : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 60);
         Generic_Formal_Part_F_Decls : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 61);
         Base_Record_Def_F_Components : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 62);
         Aggregate_Assoc_F_Designators : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 63);
         Aggregate_Assoc_F_R_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 64);
         Composite_Constraint_Assoc_F_Ids : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 65);
         Composite_Constraint_Assoc_F_Constraint_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 66);
         Iterated_Assoc_F_Spec : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 67);
         Iterated_Assoc_F_R_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 68);
         Param_Assoc_F_Designator : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 69);
         Param_Assoc_F_R_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 70);
         Basic_Decl_F_Aspects : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 71);
         Abstract_State_Decl_F_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 72);
         Anonymous_Expr_Decl_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 73);
         Component_Decl_F_Ids : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 74);
         Component_Decl_F_Component_Def : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 75);
         Component_Decl_F_Default_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 76);
         Discriminant_Spec_F_Ids : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 77);
         Discriminant_Spec_F_Type_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 78);
         Discriminant_Spec_F_Default_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 79);
         Generic_Formal_F_Decl : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 80);
         Param_Spec_F_Ids : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 81);
         Param_Spec_F_Has_Aliased : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 82);
         Param_Spec_F_Mode : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 83);
         Param_Spec_F_Type_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 84);
         Param_Spec_F_Default_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 85);
         Synthetic_Formal_Param_Decl_F_Param_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 86);
         Base_Package_Decl_F_Package_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 87);
         Base_Package_Decl_F_Public_Part : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 88);
         Base_Package_Decl_F_Private_Part : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 89);
         Base_Package_Decl_F_End_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 90);
         Base_Type_Decl_F_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 91);
         Subtype_Decl_F_Subtype : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 92);
         Incomplete_Type_Decl_F_Discriminants : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 93);
         Incomplete_Formal_Type_Decl_F_Is_Tagged : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 94);
         Incomplete_Formal_Type_Decl_F_Default_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 95);
         Incomplete_Tagged_Type_Decl_F_Has_Abstract : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 96);
         Protected_Type_Decl_F_Discriminants : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 97);
         Protected_Type_Decl_F_Interfaces : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 98);
         Protected_Type_Decl_F_Definition : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 99);
         Task_Type_Decl_F_Discriminants : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 100);
         Task_Type_Decl_F_Definition : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 101);
         Type_Decl_F_Discriminants : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 102);
         Type_Decl_F_Type_Def : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 103);
         Formal_Type_Decl_F_Default_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 104);
         Classic_Subp_Decl_F_Overriding : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 105);
         Classic_Subp_Decl_F_Subp_Spec : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 106);
         Formal_Subp_Decl_F_Default_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 107);
         Entry_Decl_F_Overriding : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 108);
         Entry_Decl_F_Spec : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 109);
         Enum_Literal_Decl_F_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 110);
         Generic_Subp_Internal_F_Subp_Spec : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 111);
         Synthetic_Subp_Decl_F_Spec : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 112);
         Base_Subp_Body_F_Overriding : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 113);
         Base_Subp_Body_F_Subp_Spec : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 114);
         Expr_Function_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 115);
         Subp_Body_F_Decls : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 116);
         Subp_Body_F_Stmts : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 117);
         Subp_Body_F_End_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 118);
         Subp_Renaming_Decl_F_Renames : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 119);
         Package_Body_Stub_F_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 120);
         Protected_Body_Stub_F_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 121);
         Subp_Body_Stub_F_Overriding : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 122);
         Subp_Body_Stub_F_Subp_Spec : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 123);
         Task_Body_Stub_F_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 124);
         Entry_Body_F_Entry_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 125);
         Entry_Body_F_Index_Spec : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 126);
         Entry_Body_F_Params : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 127);
         Entry_Body_F_Barrier : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 128);
         Entry_Body_F_Decls : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 129);
         Entry_Body_F_Stmts : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 130);
         Entry_Body_F_End_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 131);
         Package_Body_F_Package_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 132);
         Package_Body_F_Decls : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 133);
         Package_Body_F_Stmts : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 134);
         Package_Body_F_End_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 135);
         Protected_Body_F_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 136);
         Protected_Body_F_Decls : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 137);
         Protected_Body_F_End_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 138);
         Task_Body_F_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 139);
         Task_Body_F_Decls : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 140);
         Task_Body_F_Stmts : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 141);
         Task_Body_F_End_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 142);
         Entry_Index_Spec_F_Id : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 143);
         Entry_Index_Spec_F_Subtype : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 144);
         Exception_Decl_F_Ids : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 145);
         Exception_Decl_F_Renames : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 146);
         Exception_Handler_F_Exception_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 147);
         Exception_Handler_F_Handled_Exceptions : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 148);
         Exception_Handler_F_Stmts : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 149);
         For_Loop_Var_Decl_F_Id : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 150);
         For_Loop_Var_Decl_F_Id_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 151);
         Generic_Decl_F_Formal_Part : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 152);
         Generic_Package_Decl_F_Package_Decl : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 153);
         Generic_Subp_Decl_F_Subp_Decl : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 154);
         Generic_Package_Instantiation_F_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 155);
         Generic_Package_Instantiation_F_Generic_Pkg_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 156);
         Generic_Package_Instantiation_F_Params : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 157);
         Generic_Subp_Instantiation_F_Overriding : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 158);
         Generic_Subp_Instantiation_F_Kind : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 159);
         Generic_Subp_Instantiation_F_Subp_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 160);
         Generic_Subp_Instantiation_F_Generic_Subp_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 161);
         Generic_Subp_Instantiation_F_Params : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 162);
         Generic_Package_Renaming_Decl_F_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 163);
         Generic_Package_Renaming_Decl_F_Renames : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 164);
         Generic_Subp_Renaming_Decl_F_Kind : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 165);
         Generic_Subp_Renaming_Decl_F_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 166);
         Generic_Subp_Renaming_Decl_F_Renames : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 167);
         Label_Decl_F_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 168);
         Named_Stmt_Decl_F_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 169);
         Number_Decl_F_Ids : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 170);
         Number_Decl_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 171);
         Object_Decl_F_Ids : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 172);
         Object_Decl_F_Has_Aliased : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 173);
         Object_Decl_F_Has_Constant : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 174);
         Object_Decl_F_Mode : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 175);
         Object_Decl_F_Type_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 176);
         Object_Decl_F_Default_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 177);
         Object_Decl_F_Renaming_Clause : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 178);
         Package_Renaming_Decl_F_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 179);
         Package_Renaming_Decl_F_Renames : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 180);
         Single_Protected_Decl_F_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 181);
         Single_Protected_Decl_F_Interfaces : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 182);
         Single_Protected_Decl_F_Definition : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 183);
         Single_Task_Decl_F_Task_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 184);
         Case_Stmt_Alternative_F_Choices : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 185);
         Case_Stmt_Alternative_F_Stmts : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 186);
         Compilation_Unit_F_Prelude : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 187);
         Compilation_Unit_F_Body : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 188);
         Compilation_Unit_F_Pragmas : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 189);
         Component_Clause_F_Id : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 190);
         Component_Clause_F_Position : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 191);
         Component_Clause_F_Range : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 192);
         Component_Def_F_Has_Aliased : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 193);
         Component_Def_F_Has_Constant : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 194);
         Component_Def_F_Type_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 195);
         Composite_Constraint_F_Constraints : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 196);
         Delta_Constraint_F_Digits : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 197);
         Delta_Constraint_F_Range : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 198);
         Digits_Constraint_F_Digits : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 199);
         Digits_Constraint_F_Range : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 200);
         Range_Constraint_F_Range : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 201);
         Declarative_Part_F_Decls : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 202);
         Elsif_Expr_Part_F_Cond_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 203);
         Elsif_Expr_Part_F_Then_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 204);
         Elsif_Stmt_Part_F_Cond_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 205);
         Elsif_Stmt_Part_F_Stmts : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 206);
         Abstract_State_Decl_Expr_F_State_Decl : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 207);
         Allocator_F_Subpool : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 208);
         Allocator_F_Type_Or_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 209);
         Base_Aggregate_F_Ancestor_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 210);
         Base_Aggregate_F_Assocs : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 211);
         Bin_Op_F_Left : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 212);
         Bin_Op_F_Op : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 213);
         Bin_Op_F_Right : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 214);
         Case_Expr_Alternative_F_Choices : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 215);
         Case_Expr_Alternative_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 216);
         Concat_Op_F_First_Operand : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 217);
         Concat_Op_F_Other_Operands : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 218);
         Concat_Operand_F_Operator : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 219);
         Concat_Operand_F_Operand : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 220);
         Case_Expr_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 221);
         Case_Expr_F_Cases : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 222);
         If_Expr_F_Cond_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 223);
         If_Expr_F_Then_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 224);
         If_Expr_F_Alternatives : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 225);
         If_Expr_F_Else_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 226);
         Contract_Cases_F_Contract_Cases : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 227);
         Decl_Expr_F_Decls : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 228);
         Decl_Expr_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 229);
         Membership_Expr_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 230);
         Membership_Expr_F_Op : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 231);
         Membership_Expr_F_Membership_Exprs : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 232);
         Attribute_Ref_F_Prefix : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 233);
         Attribute_Ref_F_Attribute : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 234);
         Attribute_Ref_F_Args : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 235);
         Call_Expr_F_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 236);
         Call_Expr_F_Suffix : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 237);
         Defining_Name_F_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 238);
         Discrete_Subtype_Name_F_Subtype : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 239);
         Dotted_Name_F_Prefix : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 240);
         Dotted_Name_F_Suffix : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 241);
         End_Name_F_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 242);
         Explicit_Deref_F_Prefix : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 243);
         Qual_Expr_F_Prefix : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 244);
         Qual_Expr_F_Suffix : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 245);
         Reduce_Attribute_Ref_F_Prefix : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 246);
         Reduce_Attribute_Ref_F_Attribute : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 247);
         Reduce_Attribute_Ref_F_Args : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 248);
         Update_Attribute_Ref_F_Prefix : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 249);
         Update_Attribute_Ref_F_Attribute : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 250);
         Update_Attribute_Ref_F_Values : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 251);
         Paren_Expr_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 252);
         Quantified_Expr_F_Quantifier : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 253);
         Quantified_Expr_F_Loop_Spec : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 254);
         Quantified_Expr_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 255);
         Raise_Expr_F_Exception_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 256);
         Raise_Expr_F_Error_Message : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 257);
         Un_Op_F_Op : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 258);
         Un_Op_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 259);
         Handled_Stmts_F_Stmts : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 260);
         Handled_Stmts_F_Exceptions : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 261);
         Library_Item_F_Has_Private : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 262);
         Library_Item_F_Item : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 263);
         For_Loop_Spec_F_Var_Decl : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 264);
         For_Loop_Spec_F_Loop_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 265);
         For_Loop_Spec_F_Has_Reverse : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 266);
         For_Loop_Spec_F_Iter_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 267);
         For_Loop_Spec_F_Iter_Filter : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 268);
         While_Loop_Spec_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 269);
         Multi_Abstract_State_Decl_F_Decls : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 270);
         Params_F_Params : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 271);
         Paren_Abstract_State_Decl_F_Decl : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 272);
         Pp_Elsif_Directive_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 273);
         Pp_Elsif_Directive_F_Then_Kw : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 274);
         Pp_If_Directive_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 275);
         Pp_If_Directive_F_Then_Kw : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 276);
         Pragma_Node_F_Id : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 277);
         Pragma_Node_F_Args : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 278);
         Protected_Def_F_Public_Part : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 279);
         Protected_Def_F_Private_Part : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 280);
         Protected_Def_F_End_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 281);
         Range_Spec_F_Range : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 282);
         Renaming_Clause_F_Renamed_Object : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 283);
         Select_When_Part_F_Cond_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 284);
         Select_When_Part_F_Stmts : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 285);
         Accept_Stmt_F_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 286);
         Accept_Stmt_F_Entry_Index_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 287);
         Accept_Stmt_F_Params : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 288);
         Accept_Stmt_With_Stmts_F_Stmts : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 289);
         Accept_Stmt_With_Stmts_F_End_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 290);
         Base_Loop_Stmt_F_Spec : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 291);
         Base_Loop_Stmt_F_Stmts : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 292);
         Base_Loop_Stmt_F_End_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 293);
         Begin_Block_F_Stmts : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 294);
         Begin_Block_F_End_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 295);
         Decl_Block_F_Decls : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 296);
         Decl_Block_F_Stmts : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 297);
         Decl_Block_F_End_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 298);
         Case_Stmt_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 299);
         Case_Stmt_F_Pragmas : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 300);
         Case_Stmt_F_Alternatives : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 301);
         Extended_Return_Stmt_F_Decl : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 302);
         Extended_Return_Stmt_F_Stmts : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 303);
         If_Stmt_F_Cond_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 304);
         If_Stmt_F_Then_Stmts : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 305);
         If_Stmt_F_Alternatives : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 306);
         If_Stmt_F_Else_Stmts : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 307);
         Named_Stmt_F_Decl : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 308);
         Named_Stmt_F_Stmt : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 309);
         Select_Stmt_F_Guards : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 310);
         Select_Stmt_F_Else_Stmts : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 311);
         Select_Stmt_F_Abort_Stmts : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 312);
         Abort_Stmt_F_Names : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 313);
         Assign_Stmt_F_Dest : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 314);
         Assign_Stmt_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 315);
         Call_Stmt_F_Call : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 316);
         Delay_Stmt_F_Has_Until : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 317);
         Delay_Stmt_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 318);
         Exit_Stmt_F_Loop_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 319);
         Exit_Stmt_F_Cond_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 320);
         Goto_Stmt_F_Label_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 321);
         Label_F_Decl : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 322);
         Raise_Stmt_F_Exception_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 323);
         Raise_Stmt_F_Error_Message : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 324);
         Requeue_Stmt_F_Call_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 325);
         Requeue_Stmt_F_Has_Abort : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 326);
         Return_Stmt_F_Return_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 327);
         Subunit_F_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 328);
         Subunit_F_Body : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 329);
         Task_Def_F_Interfaces : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 330);
         Task_Def_F_Public_Part : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 331);
         Task_Def_F_Private_Part : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 332);
         Task_Def_F_End_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 333);
         Access_Def_F_Has_Not_Null : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 334);
         Access_To_Subp_Def_F_Has_Protected : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 335);
         Access_To_Subp_Def_F_Subp_Spec : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 336);
         Anonymous_Type_Access_Def_F_Type_Decl : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 337);
         Type_Access_Def_F_Has_All : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 338);
         Type_Access_Def_F_Has_Constant : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 339);
         Type_Access_Def_F_Subtype_Indication : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 340);
         Array_Type_Def_F_Indices : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 341);
         Array_Type_Def_F_Component_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 342);
         Derived_Type_Def_F_Has_Abstract : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 343);
         Derived_Type_Def_F_Has_Limited : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 344);
         Derived_Type_Def_F_Has_Synchronized : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 345);
         Derived_Type_Def_F_Subtype_Indication : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 346);
         Derived_Type_Def_F_Interfaces : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 347);
         Derived_Type_Def_F_Record_Extension : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 348);
         Derived_Type_Def_F_Has_With_Private : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 349);
         Enum_Type_Def_F_Enum_Literals : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 350);
         Interface_Type_Def_F_Interface_Kind : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 351);
         Interface_Type_Def_F_Interfaces : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 352);
         Mod_Int_Type_Def_F_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 353);
         Private_Type_Def_F_Has_Abstract : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 354);
         Private_Type_Def_F_Has_Tagged : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 355);
         Private_Type_Def_F_Has_Limited : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 356);
         Decimal_Fixed_Point_Def_F_Delta : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 357);
         Decimal_Fixed_Point_Def_F_Digits : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 358);
         Decimal_Fixed_Point_Def_F_Range : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 359);
         Floating_Point_Def_F_Num_Digits : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 360);
         Floating_Point_Def_F_Range : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 361);
         Ordinary_Fixed_Point_Def_F_Delta : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 362);
         Ordinary_Fixed_Point_Def_F_Range : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 363);
         Record_Type_Def_F_Has_Abstract : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 364);
         Record_Type_Def_F_Has_Tagged : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 365);
         Record_Type_Def_F_Has_Limited : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 366);
         Record_Type_Def_F_Record_Def : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 367);
         Signed_Int_Type_Def_F_Range : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 368);
         Anonymous_Type_F_Type_Decl : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 369);
         Subtype_Indication_F_Has_Not_Null : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 370);
         Subtype_Indication_F_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 371);
         Subtype_Indication_F_Constraint : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 372);
         Synthetic_Type_Expr_F_Target_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 373);
         Unconstrained_Array_Index_F_Subtype_Indication : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 374);
         Use_Package_Clause_F_Packages : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 375);
         Use_Type_Clause_F_Has_All : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 376);
         Use_Type_Clause_F_Types : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 377);
         Value_Sequence_F_Iter_Assoc : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 378);
         Variant_F_Choices : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 379);
         Variant_F_Components : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 380);
         Variant_Part_F_Discr_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 381);
         Variant_Part_F_Variant : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 382);
         With_Clause_F_Has_Limited : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 383);
         With_Clause_F_Has_Private : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 384);
         With_Clause_F_Packages : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 385);
         Ada_Node_P_Declarative_Scope : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 386);
         Ada_Node_P_Enclosing_Compilation_Unit : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 387);
         Ada_Node_P_Get_Uninstantiated_Node : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 388);
         Ada_Node_P_Complete : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 389);
         Ada_Node_P_Valid_Keywords : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 390);
         Ada_Node_P_Generic_Instantiations : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 391);
         Ada_Node_P_Semantic_Parent : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 392);
         Ada_Node_P_Parent_Basic_Decl : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 393);
         Ada_Node_P_Filter_Is_Imported_By : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 394);
         Ada_Node_P_Xref_Entry_Point : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 395);
         Ada_Node_P_Resolve_Names : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 396);
         Ada_Node_P_Standard_Unit : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 397);
         Ada_Node_P_Std_Entity : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 398);
         Ada_Node_P_Bool_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 399);
         Ada_Node_P_Int_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 400);
         Ada_Node_P_Universal_Int_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 401);
         Ada_Node_P_Universal_Real_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 402);
         Ada_Node_P_Std_Char_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 403);
         Ada_Node_P_Std_Wide_Char_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 404);
         Ada_Node_P_Std_Wide_Wide_Char_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 405);
         Ada_Node_P_Top_Level_Decl : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 406);
         Ada_Node_P_Choice_Match : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 407);
         Ada_Node_P_Gnat_Xref : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 408);
         Parent : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 409);
         Parents : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 410);
         Children : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 411);
         Token_Start : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 412);
         Token_End : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 413);
         Child_Index : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 414);
         Previous_Sibling : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 415);
         Next_Sibling : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 416);
         Unit : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 417);
         Is_Ghost : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 418);
         Full_Sloc_Image : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 419);
         Abort_Node_P_As_Bool : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 420);
         Abstract_Node_P_As_Bool : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 421);
         Assoc_List_P_Zip_With_Params : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 422);
         Aliased_Node_P_As_Bool : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 423);
         All_Node_P_As_Bool : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 424);
         Aspect_Assoc_P_Is_Ghost_Code : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 425);
         Enum_Rep_Clause_P_Params : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 426);
         Base_Assoc_P_Assoc_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 427);
         Base_Formal_Param_Holder_P_Abstract_Formal_Params : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 428);
         Base_Formal_Param_Holder_P_Formal_Params : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 429);
         Base_Formal_Param_Holder_P_Nb_Min_Params : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 430);
         Base_Formal_Param_Holder_P_Nb_Max_Params : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 431);
         Base_Formal_Param_Holder_P_Param_Types : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 432);
         Base_Subp_Spec_P_Returns : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 433);
         Base_Subp_Spec_P_Params : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 434);
         Base_Subp_Spec_P_Primitive_Subp_Types : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 435);
         Base_Subp_Spec_P_Primitive_Subp_First_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 436);
         Base_Subp_Spec_P_Primitive_Subp_Tagged_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 437);
         Base_Subp_Spec_P_Return_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 438);
         Basic_Assoc_P_Get_Params : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 439);
         Basic_Decl_P_Is_Formal : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 440);
         Basic_Decl_P_Doc_Annotations : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 441);
         Basic_Decl_P_Doc : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 442);
         Basic_Decl_P_Previous_Part_For_Decl : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 443);
         Basic_Decl_P_Canonical_Part : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 444);
         Basic_Decl_P_All_Parts : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 445);
         Basic_Decl_P_Is_Static_Decl : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 446);
         Basic_Decl_P_Get_Aspect_Assoc : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 447);
         Basic_Decl_P_Get_Aspect_Spec_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 448);
         Basic_Decl_P_Get_Aspect : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 449);
         Basic_Decl_P_Has_Aspect : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 450);
         Basic_Decl_P_Get_Pragma : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 451);
         Basic_Decl_P_Get_Representation_Clause : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 452);
         Basic_Decl_P_Get_At_Clause : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 453);
         Basic_Decl_P_Is_Imported : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 454);
         Basic_Decl_P_Is_Ghost_Code : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 455);
         Basic_Decl_P_Is_Compilation_Unit_Root : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 456);
         Basic_Decl_P_Is_Visible : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 457);
         Basic_Decl_P_Base_Subp_Declarations : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 458);
         Basic_Decl_P_Root_Subp_Declarations : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 459);
         Basic_Decl_P_Find_All_Overrides : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 460);
         Basic_Decl_P_Defining_Names : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 461);
         Basic_Decl_P_Defining_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 462);
         Basic_Decl_P_Type_Expression : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 463);
         Basic_Decl_P_Subp_Spec_Or_Null : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 464);
         Basic_Decl_P_Is_Subprogram : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 465);
         Basic_Decl_P_Relative_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 466);
         Basic_Decl_P_Relative_Name_Text : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 467);
         Basic_Decl_P_Next_Part_For_Decl : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 468);
         Basic_Decl_P_Body_Part_For_Decl : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 469);
         Basic_Decl_P_Most_Visible_Part : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 470);
         Basic_Decl_P_Fully_Qualified_Name_Array : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 471);
         Basic_Decl_P_Fully_Qualified_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 472);
         Basic_Decl_P_Canonical_Fully_Qualified_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 473);
         Basic_Decl_P_Unique_Identifying_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 474);
         Basic_Decl_P_Is_Constant_Object : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 475);
         Anonymous_Expr_Decl_P_Get_Formal : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 476);
         Base_Formal_Param_Decl_P_Formal_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 477);
         Base_Package_Decl_P_Body_Part : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 478);
         Base_Type_Decl_P_Base_Subtype : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 479);
         Base_Type_Decl_P_Private_Completion : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 480);
         Base_Type_Decl_P_Is_Inherited_Primitive : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 481);
         Base_Type_Decl_P_Get_Record_Representation_Clause : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 482);
         Base_Type_Decl_P_Get_Enum_Representation_Clause : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 483);
         Base_Type_Decl_P_Is_Record_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 484);
         Base_Type_Decl_P_Is_Array_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 485);
         Base_Type_Decl_P_Find_Derived_Types : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 486);
         Base_Type_Decl_P_Is_Real_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 487);
         Base_Type_Decl_P_Is_Float_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 488);
         Base_Type_Decl_P_Is_Fixed_Point : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 489);
         Base_Type_Decl_P_Is_Enum_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 490);
         Base_Type_Decl_P_Is_Access_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 491);
         Base_Type_Decl_P_Is_Char_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 492);
         Base_Type_Decl_P_Discrete_Range : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 493);
         Base_Type_Decl_P_Is_Discrete_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 494);
         Base_Type_Decl_P_Is_Int_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 495);
         Base_Type_Decl_P_Accessed_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 496);
         Base_Type_Decl_P_Is_Tagged_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 497);
         Base_Type_Decl_P_Base_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 498);
         Base_Type_Decl_P_Base_Types : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 499);
         Base_Type_Decl_P_Find_All_Derived_Types : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 500);
         Base_Type_Decl_P_Comp_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 501);
         Base_Type_Decl_P_Index_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 502);
         Base_Type_Decl_P_Is_Derived_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 503);
         Base_Type_Decl_P_Is_Interface_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 504);
         Base_Type_Decl_P_Matching_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 505);
         Base_Type_Decl_P_Canonical_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 506);
         Base_Type_Decl_P_Previous_Part : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 507);
         Base_Type_Decl_P_Next_Part : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 508);
         Base_Type_Decl_P_Full_View : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 509);
         Base_Type_Decl_P_Is_Definite_Subtype : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 510);
         Base_Type_Decl_P_Is_Private : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 511);
         Base_Type_Decl_P_Discriminants_List : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 512);
         Base_Type_Decl_P_Root_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 513);
         Base_Type_Decl_P_Shapes : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 514);
         Base_Subtype_Decl_P_Get_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 515);
         Type_Decl_P_Get_Primitives : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 516);
         Basic_Subp_Decl_P_Subp_Decl_Spec : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 517);
         Classic_Subp_Decl_P_Body_Part : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 518);
         Entry_Decl_P_Body_Part : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 519);
         Entry_Decl_P_Accept_Stmts : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 520);
         Enum_Literal_Decl_P_Enum_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 521);
         Synthetic_Char_Enum_Lit_P_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 522);
         Body_Node_P_Previous_Part : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 523);
         Body_Node_P_Decl_Part : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 524);
         Body_Node_P_Subunit_Root : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 525);
         Body_Stub_P_Syntactic_Fully_Qualified_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 526);
         Generic_Package_Decl_P_Body_Part : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 527);
         Generic_Subp_Decl_P_Body_Part : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 528);
         Generic_Instantiation_P_Designated_Generic_Decl : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 529);
         Generic_Instantiation_P_Inst_Params : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 530);
         Generic_Subp_Instantiation_P_Designated_Subp : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 531);
         Object_Decl_P_Private_Part_Decl : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 532);
         Object_Decl_P_Public_Part_Decl : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 533);
         Package_Renaming_Decl_P_Renamed_Package : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 534);
         Package_Renaming_Decl_P_Final_Renamed_Package : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 535);
         Compilation_Unit_P_Syntactic_Fully_Qualified_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 536);
         Compilation_Unit_P_Unit_Kind : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 537);
         Compilation_Unit_P_Withed_Units : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 538);
         Compilation_Unit_P_Imported_Units : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 539);
         Compilation_Unit_P_Unit_Dependencies : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 540);
         Compilation_Unit_P_Decl : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 541);
         Compilation_Unit_P_Is_Preelaborable : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 542);
         Compilation_Unit_P_Other_Part : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 543);
         Compilation_Unit_P_Has_Restriction : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 544);
         Compilation_Unit_P_All_Config_Pragmas : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 545);
         Compilation_Unit_P_Config_Pragmas : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 546);
         Constant_Node_P_As_Bool : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 547);
         Composite_Constraint_P_Is_Index_Constraint : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 548);
         Composite_Constraint_P_Is_Discriminant_Constraint : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 549);
         Expr_P_Expression_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 550);
         Expr_P_Expected_Expression_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 551);
         Expr_P_Is_Dynamically_Tagged : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 552);
         Expr_P_Is_Dispatching_Call : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 553);
         Expr_P_Is_Static_Expr : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 554);
         Expr_P_First_Corresponding_Decl : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 555);
         Expr_P_Eval_As_Int : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 556);
         Expr_P_Eval_As_Int_In_Env : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 557);
         Expr_P_Eval_As_String : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 558);
         Expr_P_Eval_As_String_In_Env : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 559);
         Expr_P_Matching_Nodes : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 560);
         Allocator_P_Get_Allocated_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 561);
         Base_Aggregate_P_Aggregate_Params : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 562);
         Base_Aggregate_P_Is_Subaggregate : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 563);
         Concat_Op_P_Operands : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 564);
         Cond_Expr_P_Dependent_Exprs : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 565);
         Name_P_Enclosing_Defining_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 566);
         Name_P_Is_Defining : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 567);
         Name_P_Name_Is : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 568);
         Name_P_Is_Direct_Call : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 569);
         Name_P_Is_Access_Call : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 570);
         Name_P_Is_Call : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 571);
         Name_P_Is_Dot_Call : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 572);
         Name_P_Failsafe_Referenced_Def_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 573);
         Name_P_Referenced_Defining_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 574);
         Name_P_All_Env_Elements : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 575);
         Name_P_Called_Subp_Spec : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 576);
         Name_P_Referenced_Decl : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 577);
         Name_P_Failsafe_Referenced_Decl : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 578);
         Name_P_Referenced_Decl_Internal : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 579);
         Name_P_Name_Designated_Type : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 580);
         Name_P_Is_Static_Subtype : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 581);
         Name_P_Name_Matches : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 582);
         Name_P_Relative_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 583);
         Name_P_Is_Operator_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 584);
         Name_P_Is_Write_Reference : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 585);
         Name_P_Is_Static_Call : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 586);
         Name_P_As_Symbol_Array : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 587);
         Name_P_Canonical_Text : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 588);
         Name_P_Is_Constant : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 589);
         Name_P_Call_Params : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 590);
         Call_Expr_P_Kind : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 591);
         Call_Expr_P_Is_Array_Slice : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 592);
         Defining_Name_P_Canonical_Fully_Qualified_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 593);
         Defining_Name_P_Unique_Identifying_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 594);
         Defining_Name_P_Fully_Qualified_Name_Array : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 595);
         Defining_Name_P_Fully_Qualified_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 596);
         Defining_Name_P_Basic_Decl : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 597);
         Defining_Name_P_Find_Refs : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 598);
         Defining_Name_P_Find_All_References : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 599);
         Defining_Name_P_Find_All_Calls : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 600);
         Defining_Name_P_Next_Part : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 601);
         Defining_Name_P_Previous_Part : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 602);
         Defining_Name_P_Canonical_Part : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 603);
         Defining_Name_P_Most_Visible_Part : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 604);
         Defining_Name_P_All_Parts : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 605);
         Defining_Name_P_Get_Aspect : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 606);
         Defining_Name_P_Has_Aspect : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 607);
         Defining_Name_P_Get_Pragma : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 608);
         Defining_Name_P_Get_Representation_Clause : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 609);
         Defining_Name_P_Get_At_Clause : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 610);
         Defining_Name_P_Is_Imported : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 611);
         Defining_Name_P_Is_Ghost_Code : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 612);
         End_Name_P_Basic_Decl : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 613);
         Char_Literal_P_Denoted_Value : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 614);
         String_Literal_P_Denoted_Value : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 615);
         Int_Literal_P_Denoted_Value : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 616);
         Limited_Node_P_As_Bool : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 617);
         Not_Null_P_As_Bool : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 618);
         Pragma_Node_P_Is_Ghost_Code : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 619);
         Pragma_Node_P_Associated_Entities : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 620);
         Private_Node_P_As_Bool : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 621);
         Protected_Node_P_As_Bool : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 622);
         Reverse_Node_P_As_Bool : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 623);
         Stmt_P_Is_Ghost_Code : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 624);
         Accept_Stmt_P_Corresponding_Entry : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 625);
         Subunit_P_Body_Root : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 626);
         Synchronized_Node_P_As_Bool : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 627);
         Tagged_Node_P_As_Bool : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 628);
         Type_Expr_P_Type_Name : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 629);
         Type_Expr_P_Designated_Type_Decl : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 630);
         Type_Expr_P_Designated_Type_Decl_From : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 631);
         Subtype_Indication_P_Subtype_Constraints : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 632);
         Subtype_Indication_P_Is_Static_Subtype : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 633);
         Until_Node_P_As_Bool : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 634);
         With_Private_P_As_Bool : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, 635);
   end Member_Refs;

end Libadalang.Generic_API.Introspection;
