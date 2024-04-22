
import sys


sys.path.append('/home/chouteau/src/github/langkit')


import langkit.gdb



langkit.gdb.setup(
    lib_name='gpr_parser',
    astnode_names=['Gpr_Node', 'Ada_Prelude_Node', 'Ada_Access_Subp', 'Ada_Context_Clause', 'Ada_Pragma', 'Ada_Use', 'Ada_With', 'Ada_Entity_Kind', 'Ada_Entity_Kind_Function', 'Ada_Entity_Kind_Package', 'Ada_Entity_Kind_Procedure', 'Ada_Generic', 'Ada_Library_Item', 'Ada_Main', 'Ada_Pkg', 'Ada_Pkg_Body', 'Ada_Subp', 'Ada_Prelude', 'Ada_Separate', 'Ada_Skip', 'Ada_With_Formal', 'All_Qualifier', 'All_Qualifier_Absent', 'All_Qualifier_Present', 'Attribute_Decl', 'Attribute_Reference', 'Base_List', 'Ada_Context_Clause_List', 'Ada_Prelude_Node_List', 'Ada_Skip_List', 'Case_Item_List', 'Expr_List', 'Gpr_Node_List', 'Choices', 'Term_List', 'Identifier_List', 'String_Literal_List', 'Term_List_List', 'With_Decl_List', 'Builtin_Function_Call', 'Case_Construction', 'Case_Item', 'Compilation_Unit', 'Empty_Decl', 'Expr', 'Prefix', 'Single_Tok_Node', 'Identifier', 'Num_Literal', 'String_Literal', 'Limited_Node', 'Limited_Absent', 'Limited_Present', 'Others_Designator', 'Package_Decl', 'Package_Extension', 'Package_Renaming', 'Package_Spec', 'Private_Node', 'Private_Absent', 'Private_Present', 'Project', 'Project_Declaration', 'Project_Extension', 'Project_Qualifier', 'Project_Qualifier_Abstract', 'Project_Qualifier_Aggregate', 'Project_Qualifier_Aggregate_Library', 'Project_Qualifier_Configuration', 'Project_Qualifier_Library', 'Project_Qualifier_Standard', 'Project_Reference', 'String_Literal_At', 'Terms', 'Type_Reference', 'Typed_String_Decl', 'Variable_Decl', 'Variable_Reference', 'With_Decl'],
    astnode_kinds={1: 'Ada_Access_Subp', 2: 'Ada_Pragma', 3: 'Ada_Use', 4: 'Ada_With', 5: 'Ada_Entity_Kind_Function', 6: 'Ada_Entity_Kind_Package', 7: 'Ada_Entity_Kind_Procedure', 8: 'Ada_Generic', 9: 'Ada_Library_Item', 10: 'Ada_Pkg', 11: 'Ada_Pkg_Body', 12: 'Ada_Subp', 13: 'Ada_Prelude', 14: 'Ada_Separate', 15: 'Ada_Skip', 16: 'Ada_With_Formal', 17: 'All_Qualifier_Absent', 18: 'All_Qualifier_Present', 19: 'Attribute_Decl', 20: 'Attribute_Reference', 21: 'Ada_Context_Clause_List', 22: 'Ada_Prelude_Node_List', 23: 'Ada_Skip_List', 24: 'Case_Item_List', 25: 'Expr_List', 26: 'Gpr_Node_List', 27: 'Choices', 28: 'Term_List', 29: 'Identifier_List', 30: 'String_Literal_List', 31: 'Term_List_List', 32: 'With_Decl_List', 33: 'Builtin_Function_Call', 34: 'Case_Construction', 35: 'Case_Item', 36: 'Compilation_Unit', 37: 'Empty_Decl', 38: 'Prefix', 39: 'Identifier', 40: 'Num_Literal', 41: 'String_Literal', 42: 'Limited_Absent', 43: 'Limited_Present', 44: 'Others_Designator', 45: 'Package_Decl', 46: 'Package_Extension', 47: 'Package_Renaming', 48: 'Package_Spec', 49: 'Private_Absent', 50: 'Private_Present', 51: 'Project', 52: 'Project_Declaration', 53: 'Project_Extension', 54: 'Project_Qualifier_Abstract', 55: 'Project_Qualifier_Aggregate', 56: 'Project_Qualifier_Aggregate_Library', 57: 'Project_Qualifier_Configuration', 58: 'Project_Qualifier_Library', 59: 'Project_Qualifier_Standard', 60: 'Project_Reference', 61: 'String_Literal_At', 62: 'Terms', 63: 'Type_Reference', 64: 'Typed_String_Decl', 65: 'Variable_Decl', 66: 'Variable_Reference', 67: 'With_Decl'},
    prefix='gpr_parser'
)
