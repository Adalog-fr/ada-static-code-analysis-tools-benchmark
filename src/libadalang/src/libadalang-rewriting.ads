--
--  Copyright (C) 2014-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

--  This package provides support for tree-based source code rewriting.
--
--  .. ATTENTION:: This is an experimental feature, so even if it is exposed to
--  allow experiments, it is totally unsupported and the API is very likely to
--  change in the future.

with System;

with Libadalang.Analysis; use Libadalang.Analysis;
with Libadalang.Common;   use Libadalang.Common;

package Libadalang.Rewriting is

   use Support.Diagnostics, Support.Text;

   type Rewriting_Handle is private;
   --  Handle for an analysis context rewriting session

   type Unit_Rewriting_Handle is private;
   --  Handle for the process of rewriting an analysis unit. Such handles are
   --  owned by a Rewriting_Handle instance.

   type Node_Rewriting_Handle is private;
   --  Handle for the process of rewriting an AST node. Such handles are owned
   --  by a Rewriting_Handle instance.

   No_Rewriting_Handle      : constant Rewriting_Handle;
   No_Unit_Rewriting_Handle : constant Unit_Rewriting_Handle;
   No_Node_Rewriting_Handle : constant Node_Rewriting_Handle;

   type Unit_Rewriting_Handle_Array is
      array (Positive range <>) of Unit_Rewriting_Handle;

   type Node_Rewriting_Handle_Array is
      array (Positive range <>) of Node_Rewriting_Handle;

   -----------------------
   -- Context rewriting --
   -----------------------

   function Handle (Context : Analysis_Context) return Rewriting_Handle;
   --  Return the rewriting handle associated to Context, or
   --  No_Rewriting_Handle if Context is not being rewritten.

   function Context (Handle : Rewriting_Handle) return Analysis_Context;
   --  Return the analysis context associated to Handle

   function Start_Rewriting
     (Context : Analysis_Context) return Rewriting_Handle;
   --  Start a rewriting session for Context.
   --
   --  This handle will keep track of all changes to do on Context's analysis
   --  units. Once the set of changes is complete, call the Apply procedure to
   --  actually update Context. This makes it possible to inspect the "old"
   --  Context state while creating the list of changes.
   --
   --  There can be only one rewriting session per analysis context, so this
   --  will raise an Existing_Rewriting_Handle_Error exception if Context
   --  already has a living rewriting session.

   procedure Abort_Rewriting (Handle : in out Rewriting_Handle);
   --  Discard all modifications registered in Handle and close Handle

   type Apply_Result (Success : Boolean := True) is record
      case Success is
         when False =>
            Unit : Analysis_Unit;
            --  Reference to the analysis unit that was being processed when
            --  the error occurred.

            Diagnostics : Diagnostics_Vectors.Vector;
            --  Corresponding list of error messages
         when True => null;
      end case;
   end record;

   function Apply (Handle : in out Rewriting_Handle) return Apply_Result;
   --  Apply all modifications to Handle's analysis context. If that worked,
   --  close Handle and return (Success => True). Otherwise, reparsing did not
   --  work, so keep Handle and its Context unchanged and return details about
   --  the error that happened.

   function Unit_Handles
     (Handle : Rewriting_Handle) return Unit_Rewriting_Handle_Array;
   --  Return the list of unit rewriting handles in the given context handle
   --  for units that the Apply primitive will modify.

   --------------------
   -- Unit rewriting --
   --------------------

   function Handle (Unit : Analysis_Unit) return Unit_Rewriting_Handle;
   --  Return the rewriting handle corresponding to Unit

   function Unit (Handle : Unit_Rewriting_Handle) return Analysis_Unit;
   --  Return the unit corresponding to Handle

   function Root (Handle : Unit_Rewriting_Handle) return Node_Rewriting_Handle;
   --  Return the node handle corresponding to the root of the unit which
   --  Handle designates.

   procedure Set_Root
     (Handle : Unit_Rewriting_Handle;
      Root   : Node_Rewriting_Handle);
   --  Set the root node for the unit Handle to Root. This unties the previous
   --  root handle. If Root is not No_Node_Rewriting_Handle, this also ties
   --  Root to Handle.
   --
   --  Root must not already be tied to another analysis unit handle.

   function Unparse (Handle : Unit_Rewriting_Handle) return Unbounded_Text_Type;
   --  Return the text associated to the given unit.

   --------------------
   -- Node rewriting --
   --------------------

   function Handle
     (Node : Ada_Node'Class) return Node_Rewriting_Handle;
   --  Return the rewriting handle corresponding to Node.
   --
   --  The owning unit of Node must be free of diagnostics.

   function Node
     (Handle : Node_Rewriting_Handle) return Ada_Node;
   --  Return the node which the given rewriting Handle relates to. This can be
   --  the null entity if this handle designates a new node.

   function Context (Handle : Node_Rewriting_Handle) return Rewriting_Handle;
   --  Return a handle for the rewriting context to which Handle belongs

   function Unparse (Handle : Node_Rewriting_Handle) return Text_Type;
   --  Turn the given rewritten node Handles designates into text. This is the
   --  text that is used in Apply in order to re-create an analysis unit.

   function Kind (Handle : Node_Rewriting_Handle) return Ada_Node_Kind_Type;
   --  Return the kind corresponding to Handle's node

   function Tied (Handle : Node_Rewriting_Handle) return Boolean;
   --  Return whether this node handle is tied to an analysis unit. If it is
   --  not, it can be passed as the Child parameter to Set_Child.

   function Parent
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle;
   --  Return a handle for the node that is the parent of Handle's node. This
   --  is ``No_Rewriting_Handle`` for a node that is not tied to any tree yet.

   function Children_Count (Handle : Node_Rewriting_Handle) return Natural;
   --  Return the number of children the node represented by Handle has

   function Child
     (Handle : Node_Rewriting_Handle;
      Index  : Positive) return Node_Rewriting_Handle;
   --  Return a handle corresponding to the Index'th child of the node that
   --  Handle represents. Index is 1-based.

   procedure Set_Child
     (Handle : Node_Rewriting_Handle;
      Index  : Positive;
      Child  : Node_Rewriting_Handle);
   --  If Child is ``No_Rewriting_Node``, untie the Handle's ``Index``'th child
   --  to this tree, so it can be attached to another one. Otherwise, Child
   --  must have no parent as it will be tied to ``Handle``'s tree.

   function Text (Handle : Node_Rewriting_Handle) return Text_Type;
   --  Return the text associated to the given token node.

   procedure Set_Text (Handle : Node_Rewriting_Handle; Text : Text_Type);
   --  Override text associated to the given token node.

   procedure Replace (Handle, New_Node : Node_Rewriting_Handle);
   --  If Handle is the root of an analysis unit, untie it and set New_Node as
   --  its new root. Otherwise, replace Handle with New_Node in Handle's parent
   --  node.
   --
   --  Note that: * Handle must be tied to an existing analysis unit handle. *
   --  New_Node must not already be tied to another analysis unit handle.

   -------------------------
   -- List node rewriting --
   -------------------------

   procedure Insert_Child
     (Handle : Node_Rewriting_Handle;
      Index  : Positive;
      Child  : Node_Rewriting_Handle);
   --  Assuming Handle refers to a list node, insert the given Child node to be
   --  in the children list at the given index.
   --
   --  The given Child node must not be tied to any analysis unit.

   procedure Append_Child
     (Handle : Node_Rewriting_Handle;
      Child  : Node_Rewriting_Handle);
   --  Assuming Handle refers to a list node, append the given Child node to
   --  the children list.
   --
   --  The given Child node must not be tied to any analysis unit.

   procedure Remove_Child
     (Handle : Node_Rewriting_Handle;
      Index  : Positive);
   --  Assuming Handle refers to a list node, remove the child at the given
   --  Index from the children list.

   -------------------
   -- Node creation --
   -------------------

   function Clone
     (Handle : Node_Rewriting_Handle) return Node_Rewriting_Handle;
   --  Create a clone of the Handle node tree. The result is not tied to any
   --  analysis unit tree.

   function Create_Node
     (Handle : Rewriting_Handle;
      Kind   : Ada_Node_Kind_Type) return Node_Rewriting_Handle;
   --  Create a new node of the given Kind, with empty text (for token nodes)
   --  or children (for regular nodes).

   function Create_Token_Node
     (Handle : Rewriting_Handle;
      Kind   : Ada_Node_Kind_Type;
      Text   : Text_Type) return Node_Rewriting_Handle;
   --  Create a new token node with the given Kind and Text

   function Create_Regular_Node
     (Handle   : Rewriting_Handle;
      Kind     : Ada_Node_Kind_Type;
      Children : Node_Rewriting_Handle_Array) return Node_Rewriting_Handle;
   --  Create a new regular node of the given Kind and assign it the given
   --  Children.
   --
   --  Except for lists, which can have any number of children, the size of
   --  Children must match the number of children associated to the given Kind.
   --  Besides, all given children must not be tied.

   ---------------
   -- Templates --
   ---------------

   --  Templating is a way to create trees of node rewriting handles. It is
   --  intended to be more convenient than calling node constructors for each
   --  individual node in a tree.
   --
   --  A template is text that represents source code, including zero or
   --  multiple placeholders (stray "{}").
   --
   --  Create a tree of new nodes from a template is called instantiating a
   --  template: just call Create_From_Template, passing to it the template
   --  itself, a sequence of nodes (the template arguments) to fill the
   --  template placeholders and a grammar rule to parse the resulting source
   --  code. This will unparse given nodes to replace placeholders in the
   --  template text, and then parse the resulting source code in order to
   --  create a tree of node rewriting handles.
   --
   --  In order not to interfer with the template DSL, stray "{" and "}"
   --  characters in the source code must be doubled: for instance "{{"
   --  represent "{" in the source code to be parsed.

   function Create_From_Template
     (Handle    : Rewriting_Handle;
      Template  : Text_Type;
      Arguments : Node_Rewriting_Handle_Array;
      Rule      : Grammar_Rule) return Node_Rewriting_Handle;
   --  Create a tree of new nodes from the given Template string, replacing
   --  placeholders with nodes in Arguments and parsed according to the given
   --  grammar Rule.

   -----------------------------
   -- Node creation shortcuts --
   -----------------------------



         function Create_Constrained_Array_Indices
           (Handle : Rewriting_Handle
               ; F_List : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Unconstrained_Array_Indices
           (Handle : Rewriting_Handle
               ; F_Types : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Aspect_Assoc
           (Handle : Rewriting_Handle
               ; F_Id : Node_Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_At_Clause
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Attribute_Def_Clause
           (Handle : Rewriting_Handle
               ; F_Attribute_Expr : Node_Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Enum_Rep_Clause
           (Handle : Rewriting_Handle
               ; F_Type_Name : Node_Rewriting_Handle
               ; F_Aggregate : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Record_Rep_Clause
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_At_Expr : Node_Rewriting_Handle
               ; F_Components : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Aspect_Spec
           (Handle : Rewriting_Handle
               ; F_Aspect_Assocs : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Contract_Case_Assoc
           (Handle : Rewriting_Handle
               ; F_Guard : Node_Rewriting_Handle
               ; F_Consequence : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Pragma_Argument_Assoc
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Entry_Spec
           (Handle : Rewriting_Handle
               ; F_Entry_Name : Node_Rewriting_Handle
               ; F_Family_Type : Node_Rewriting_Handle
               ; F_Entry_Params : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Subp_Spec
           (Handle : Rewriting_Handle
               ; F_Subp_Kind : Node_Rewriting_Handle
               ; F_Subp_Name : Node_Rewriting_Handle
               ; F_Subp_Params : Node_Rewriting_Handle
               ; F_Subp_Returns : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Synthetic_Binary_Spec
           (Handle : Rewriting_Handle
               ; F_Left_Param : Node_Rewriting_Handle
               ; F_Right_Param : Node_Rewriting_Handle
               ; F_Return_Type_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Synthetic_Unary_Spec
           (Handle : Rewriting_Handle
               ; F_Right_Param : Node_Rewriting_Handle
               ; F_Return_Type_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Component_List
           (Handle : Rewriting_Handle
               ; F_Components : Node_Rewriting_Handle
               ; F_Variant_Part : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Known_Discriminant_Part
           (Handle : Rewriting_Handle
               ; F_Discr_Specs : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Entry_Completion_Formal_Params
           (Handle : Rewriting_Handle
               ; F_Params : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Generic_Formal_Part
           (Handle : Rewriting_Handle
               ; F_Decls : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Null_Record_Def
           (Handle : Rewriting_Handle
               ; F_Components : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Record_Def
           (Handle : Rewriting_Handle
               ; F_Components : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Aggregate_Assoc
           (Handle : Rewriting_Handle
               ; F_Designators : Node_Rewriting_Handle
               ; F_R_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Multi_Dim_Array_Assoc
           (Handle : Rewriting_Handle
               ; F_Designators : Node_Rewriting_Handle
               ; F_R_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Composite_Constraint_Assoc
           (Handle : Rewriting_Handle
               ; F_Ids : Node_Rewriting_Handle
               ; F_Constraint_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Iterated_Assoc
           (Handle : Rewriting_Handle
               ; F_Spec : Node_Rewriting_Handle
               ; F_R_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Param_Assoc
           (Handle : Rewriting_Handle
               ; F_Designator : Node_Rewriting_Handle
               ; F_R_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Abstract_State_Decl
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Anonymous_Expr_Decl
           (Handle : Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Component_Decl
           (Handle : Rewriting_Handle
               ; F_Ids : Node_Rewriting_Handle
               ; F_Component_Def : Node_Rewriting_Handle
               ; F_Default_Expr : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Discriminant_Spec
           (Handle : Rewriting_Handle
               ; F_Ids : Node_Rewriting_Handle
               ; F_Type_Expr : Node_Rewriting_Handle
               ; F_Default_Expr : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Generic_Formal_Obj_Decl
           (Handle : Rewriting_Handle
               ; F_Decl : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Generic_Formal_Package
           (Handle : Rewriting_Handle
               ; F_Decl : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Generic_Formal_Subp_Decl
           (Handle : Rewriting_Handle
               ; F_Decl : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Generic_Formal_Type_Decl
           (Handle : Rewriting_Handle
               ; F_Decl : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Param_Spec
           (Handle : Rewriting_Handle
               ; F_Ids : Node_Rewriting_Handle
               ; F_Has_Aliased : Node_Rewriting_Handle
               ; F_Mode : Node_Rewriting_Handle
               ; F_Type_Expr : Node_Rewriting_Handle
               ; F_Default_Expr : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Synthetic_Formal_Param_Decl
           (Handle : Rewriting_Handle
               ; F_Param_Type : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Generic_Package_Internal
           (Handle : Rewriting_Handle
               ; F_Package_Name : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
               ; F_Public_Part : Node_Rewriting_Handle
               ; F_Private_Part : Node_Rewriting_Handle
               ; F_End_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Package_Decl
           (Handle : Rewriting_Handle
               ; F_Package_Name : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
               ; F_Public_Part : Node_Rewriting_Handle
               ; F_Private_Part : Node_Rewriting_Handle
               ; F_End_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Discrete_Base_Subtype_Decl
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Subtype_Decl
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Subtype : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Classwide_Type_Decl
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Incomplete_Type_Decl
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Discriminants : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Incomplete_Formal_Type_Decl
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Discriminants : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
               ; F_Is_Tagged : Node_Rewriting_Handle
               ; F_Default_Type : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Incomplete_Tagged_Type_Decl
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Discriminants : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
               ; F_Has_Abstract : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Protected_Type_Decl
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Discriminants : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
               ; F_Interfaces : Node_Rewriting_Handle
               ; F_Definition : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Task_Type_Decl
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Discriminants : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
               ; F_Definition : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Single_Task_Type_Decl
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Discriminants : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
               ; F_Definition : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Anonymous_Type_Decl
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Discriminants : Node_Rewriting_Handle
               ; F_Type_Def : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Synth_Anonymous_Type_Decl
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Discriminants : Node_Rewriting_Handle
               ; F_Type_Def : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Concrete_Type_Decl
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Discriminants : Node_Rewriting_Handle
               ; F_Type_Def : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Formal_Type_Decl
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Discriminants : Node_Rewriting_Handle
               ; F_Type_Def : Node_Rewriting_Handle
               ; F_Default_Type : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Abstract_Subp_Decl
           (Handle : Rewriting_Handle
               ; F_Overriding : Node_Rewriting_Handle
               ; F_Subp_Spec : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Abstract_Formal_Subp_Decl
           (Handle : Rewriting_Handle
               ; F_Overriding : Node_Rewriting_Handle
               ; F_Subp_Spec : Node_Rewriting_Handle
               ; F_Default_Expr : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Concrete_Formal_Subp_Decl
           (Handle : Rewriting_Handle
               ; F_Overriding : Node_Rewriting_Handle
               ; F_Subp_Spec : Node_Rewriting_Handle
               ; F_Default_Expr : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Subp_Decl
           (Handle : Rewriting_Handle
               ; F_Overriding : Node_Rewriting_Handle
               ; F_Subp_Spec : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Entry_Decl
           (Handle : Rewriting_Handle
               ; F_Overriding : Node_Rewriting_Handle
               ; F_Spec : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Enum_Literal_Decl
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Synthetic_Char_Enum_Lit
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Generic_Subp_Internal
           (Handle : Rewriting_Handle
               ; F_Subp_Spec : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Synthetic_Subp_Decl
           (Handle : Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
               ; F_Spec : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Expr_Function
           (Handle : Rewriting_Handle
               ; F_Overriding : Node_Rewriting_Handle
               ; F_Subp_Spec : Node_Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Null_Subp_Decl
           (Handle : Rewriting_Handle
               ; F_Overriding : Node_Rewriting_Handle
               ; F_Subp_Spec : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Subp_Body
           (Handle : Rewriting_Handle
               ; F_Overriding : Node_Rewriting_Handle
               ; F_Subp_Spec : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
               ; F_Decls : Node_Rewriting_Handle
               ; F_Stmts : Node_Rewriting_Handle
               ; F_End_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Subp_Renaming_Decl
           (Handle : Rewriting_Handle
               ; F_Overriding : Node_Rewriting_Handle
               ; F_Subp_Spec : Node_Rewriting_Handle
               ; F_Renames : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Package_Body_Stub
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Protected_Body_Stub
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Subp_Body_Stub
           (Handle : Rewriting_Handle
               ; F_Overriding : Node_Rewriting_Handle
               ; F_Subp_Spec : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Task_Body_Stub
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


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
            ) return Node_Rewriting_Handle;


         function Create_Package_Body
           (Handle : Rewriting_Handle
               ; F_Package_Name : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
               ; F_Decls : Node_Rewriting_Handle
               ; F_Stmts : Node_Rewriting_Handle
               ; F_End_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Protected_Body
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
               ; F_Decls : Node_Rewriting_Handle
               ; F_End_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Task_Body
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
               ; F_Decls : Node_Rewriting_Handle
               ; F_Stmts : Node_Rewriting_Handle
               ; F_End_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Entry_Index_Spec
           (Handle : Rewriting_Handle
               ; F_Id : Node_Rewriting_Handle
               ; F_Subtype : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Error_Decl
           (Handle : Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Exception_Decl
           (Handle : Rewriting_Handle
               ; F_Ids : Node_Rewriting_Handle
               ; F_Renames : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Exception_Handler
           (Handle : Rewriting_Handle
               ; F_Exception_Name : Node_Rewriting_Handle
               ; F_Handled_Exceptions : Node_Rewriting_Handle
               ; F_Stmts : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_For_Loop_Var_Decl
           (Handle : Rewriting_Handle
               ; F_Id : Node_Rewriting_Handle
               ; F_Id_Type : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Generic_Package_Decl
           (Handle : Rewriting_Handle
               ; F_Formal_Part : Node_Rewriting_Handle
               ; F_Package_Decl : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Generic_Subp_Decl
           (Handle : Rewriting_Handle
               ; F_Formal_Part : Node_Rewriting_Handle
               ; F_Subp_Decl : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Generic_Package_Instantiation
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Generic_Pkg_Name : Node_Rewriting_Handle
               ; F_Params : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Generic_Subp_Instantiation
           (Handle : Rewriting_Handle
               ; F_Overriding : Node_Rewriting_Handle
               ; F_Kind : Node_Rewriting_Handle
               ; F_Subp_Name : Node_Rewriting_Handle
               ; F_Generic_Subp_Name : Node_Rewriting_Handle
               ; F_Params : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Generic_Package_Renaming_Decl
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Renames : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Generic_Subp_Renaming_Decl
           (Handle : Rewriting_Handle
               ; F_Kind : Node_Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Renames : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Label_Decl
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Named_Stmt_Decl
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Number_Decl
           (Handle : Rewriting_Handle
               ; F_Ids : Node_Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


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
            ) return Node_Rewriting_Handle;


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
            ) return Node_Rewriting_Handle;


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
            ) return Node_Rewriting_Handle;


         function Create_Package_Renaming_Decl
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Renames : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Single_Protected_Decl
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
               ; F_Interfaces : Node_Rewriting_Handle
               ; F_Definition : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Single_Task_Decl
           (Handle : Rewriting_Handle
               ; F_Task_Type : Node_Rewriting_Handle
               ; F_Aspects : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Case_Stmt_Alternative
           (Handle : Rewriting_Handle
               ; F_Choices : Node_Rewriting_Handle
               ; F_Stmts : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Compilation_Unit
           (Handle : Rewriting_Handle
               ; F_Prelude : Node_Rewriting_Handle
               ; F_Body : Node_Rewriting_Handle
               ; F_Pragmas : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Component_Clause
           (Handle : Rewriting_Handle
               ; F_Id : Node_Rewriting_Handle
               ; F_Position : Node_Rewriting_Handle
               ; F_Range : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Component_Def
           (Handle : Rewriting_Handle
               ; F_Has_Aliased : Node_Rewriting_Handle
               ; F_Has_Constant : Node_Rewriting_Handle
               ; F_Type_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Composite_Constraint
           (Handle : Rewriting_Handle
               ; F_Constraints : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Delta_Constraint
           (Handle : Rewriting_Handle
               ; F_Digits : Node_Rewriting_Handle
               ; F_Range : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Digits_Constraint
           (Handle : Rewriting_Handle
               ; F_Digits : Node_Rewriting_Handle
               ; F_Range : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Range_Constraint
           (Handle : Rewriting_Handle
               ; F_Range : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Declarative_Part
           (Handle : Rewriting_Handle
               ; F_Decls : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Private_Part
           (Handle : Rewriting_Handle
               ; F_Decls : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Public_Part
           (Handle : Rewriting_Handle
               ; F_Decls : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Elsif_Expr_Part
           (Handle : Rewriting_Handle
               ; F_Cond_Expr : Node_Rewriting_Handle
               ; F_Then_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Elsif_Stmt_Part
           (Handle : Rewriting_Handle
               ; F_Cond_Expr : Node_Rewriting_Handle
               ; F_Stmts : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Abstract_State_Decl_Expr
           (Handle : Rewriting_Handle
               ; F_State_Decl : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Allocator
           (Handle : Rewriting_Handle
               ; F_Subpool : Node_Rewriting_Handle
               ; F_Type_Or_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Aggregate
           (Handle : Rewriting_Handle
               ; F_Ancestor_Expr : Node_Rewriting_Handle
               ; F_Assocs : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Bracket_Aggregate
           (Handle : Rewriting_Handle
               ; F_Ancestor_Expr : Node_Rewriting_Handle
               ; F_Assocs : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Delta_Aggregate
           (Handle : Rewriting_Handle
               ; F_Ancestor_Expr : Node_Rewriting_Handle
               ; F_Assocs : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Bracket_Delta_Aggregate
           (Handle : Rewriting_Handle
               ; F_Ancestor_Expr : Node_Rewriting_Handle
               ; F_Assocs : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Null_Record_Aggregate
           (Handle : Rewriting_Handle
               ; F_Ancestor_Expr : Node_Rewriting_Handle
               ; F_Assocs : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Bin_Op
           (Handle : Rewriting_Handle
               ; F_Left : Node_Rewriting_Handle
               ; F_Op : Node_Rewriting_Handle
               ; F_Right : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Relation_Op
           (Handle : Rewriting_Handle
               ; F_Left : Node_Rewriting_Handle
               ; F_Op : Node_Rewriting_Handle
               ; F_Right : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Case_Expr_Alternative
           (Handle : Rewriting_Handle
               ; F_Choices : Node_Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Concat_Op
           (Handle : Rewriting_Handle
               ; F_First_Operand : Node_Rewriting_Handle
               ; F_Other_Operands : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Concat_Operand
           (Handle : Rewriting_Handle
               ; F_Operator : Node_Rewriting_Handle
               ; F_Operand : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Case_Expr
           (Handle : Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
               ; F_Cases : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_If_Expr
           (Handle : Rewriting_Handle
               ; F_Cond_Expr : Node_Rewriting_Handle
               ; F_Then_Expr : Node_Rewriting_Handle
               ; F_Alternatives : Node_Rewriting_Handle
               ; F_Else_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Contract_Cases
           (Handle : Rewriting_Handle
               ; F_Contract_Cases : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Decl_Expr
           (Handle : Rewriting_Handle
               ; F_Decls : Node_Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Membership_Expr
           (Handle : Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
               ; F_Op : Node_Rewriting_Handle
               ; F_Membership_Exprs : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Attribute_Ref
           (Handle : Rewriting_Handle
               ; F_Prefix : Node_Rewriting_Handle
               ; F_Attribute : Node_Rewriting_Handle
               ; F_Args : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Call_Expr
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Suffix : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Defining_Name
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Synthetic_Defining_Name
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Discrete_Subtype_Name
           (Handle : Rewriting_Handle
               ; F_Subtype : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Dotted_Name
           (Handle : Rewriting_Handle
               ; F_Prefix : Node_Rewriting_Handle
               ; F_Suffix : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_End_Name
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Explicit_Deref
           (Handle : Rewriting_Handle
               ; F_Prefix : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Qual_Expr
           (Handle : Rewriting_Handle
               ; F_Prefix : Node_Rewriting_Handle
               ; F_Suffix : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Reduce_Attribute_Ref
           (Handle : Rewriting_Handle
               ; F_Prefix : Node_Rewriting_Handle
               ; F_Attribute : Node_Rewriting_Handle
               ; F_Args : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Update_Attribute_Ref
           (Handle : Rewriting_Handle
               ; F_Prefix : Node_Rewriting_Handle
               ; F_Attribute : Node_Rewriting_Handle
               ; F_Values : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Paren_Expr
           (Handle : Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Quantified_Expr
           (Handle : Rewriting_Handle
               ; F_Quantifier : Node_Rewriting_Handle
               ; F_Loop_Spec : Node_Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Raise_Expr
           (Handle : Rewriting_Handle
               ; F_Exception_Name : Node_Rewriting_Handle
               ; F_Error_Message : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Un_Op
           (Handle : Rewriting_Handle
               ; F_Op : Node_Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Handled_Stmts
           (Handle : Rewriting_Handle
               ; F_Stmts : Node_Rewriting_Handle
               ; F_Exceptions : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Library_Item
           (Handle : Rewriting_Handle
               ; F_Has_Private : Node_Rewriting_Handle
               ; F_Item : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_For_Loop_Spec
           (Handle : Rewriting_Handle
               ; F_Var_Decl : Node_Rewriting_Handle
               ; F_Loop_Type : Node_Rewriting_Handle
               ; F_Has_Reverse : Node_Rewriting_Handle
               ; F_Iter_Expr : Node_Rewriting_Handle
               ; F_Iter_Filter : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_While_Loop_Spec
           (Handle : Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Multi_Abstract_State_Decl
           (Handle : Rewriting_Handle
               ; F_Decls : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Params
           (Handle : Rewriting_Handle
               ; F_Params : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Paren_Abstract_State_Decl
           (Handle : Rewriting_Handle
               ; F_Decl : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Pp_Elsif_Directive
           (Handle : Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
               ; F_Then_Kw : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Pp_If_Directive
           (Handle : Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
               ; F_Then_Kw : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Pragma_Node
           (Handle : Rewriting_Handle
               ; F_Id : Node_Rewriting_Handle
               ; F_Args : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Protected_Def
           (Handle : Rewriting_Handle
               ; F_Public_Part : Node_Rewriting_Handle
               ; F_Private_Part : Node_Rewriting_Handle
               ; F_End_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Range_Spec
           (Handle : Rewriting_Handle
               ; F_Range : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Renaming_Clause
           (Handle : Rewriting_Handle
               ; F_Renamed_Object : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Synthetic_Renaming_Clause
           (Handle : Rewriting_Handle
               ; F_Renamed_Object : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Select_When_Part
           (Handle : Rewriting_Handle
               ; F_Cond_Expr : Node_Rewriting_Handle
               ; F_Stmts : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Accept_Stmt
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Entry_Index_Expr : Node_Rewriting_Handle
               ; F_Params : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Accept_Stmt_With_Stmts
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Entry_Index_Expr : Node_Rewriting_Handle
               ; F_Params : Node_Rewriting_Handle
               ; F_Stmts : Node_Rewriting_Handle
               ; F_End_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_For_Loop_Stmt
           (Handle : Rewriting_Handle
               ; F_Spec : Node_Rewriting_Handle
               ; F_Stmts : Node_Rewriting_Handle
               ; F_End_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Loop_Stmt
           (Handle : Rewriting_Handle
               ; F_Spec : Node_Rewriting_Handle
               ; F_Stmts : Node_Rewriting_Handle
               ; F_End_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_While_Loop_Stmt
           (Handle : Rewriting_Handle
               ; F_Spec : Node_Rewriting_Handle
               ; F_Stmts : Node_Rewriting_Handle
               ; F_End_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Begin_Block
           (Handle : Rewriting_Handle
               ; F_Stmts : Node_Rewriting_Handle
               ; F_End_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Decl_Block
           (Handle : Rewriting_Handle
               ; F_Decls : Node_Rewriting_Handle
               ; F_Stmts : Node_Rewriting_Handle
               ; F_End_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Case_Stmt
           (Handle : Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
               ; F_Pragmas : Node_Rewriting_Handle
               ; F_Alternatives : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Extended_Return_Stmt
           (Handle : Rewriting_Handle
               ; F_Decl : Node_Rewriting_Handle
               ; F_Stmts : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_If_Stmt
           (Handle : Rewriting_Handle
               ; F_Cond_Expr : Node_Rewriting_Handle
               ; F_Then_Stmts : Node_Rewriting_Handle
               ; F_Alternatives : Node_Rewriting_Handle
               ; F_Else_Stmts : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Named_Stmt
           (Handle : Rewriting_Handle
               ; F_Decl : Node_Rewriting_Handle
               ; F_Stmt : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Select_Stmt
           (Handle : Rewriting_Handle
               ; F_Guards : Node_Rewriting_Handle
               ; F_Else_Stmts : Node_Rewriting_Handle
               ; F_Abort_Stmts : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Abort_Stmt
           (Handle : Rewriting_Handle
               ; F_Names : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Assign_Stmt
           (Handle : Rewriting_Handle
               ; F_Dest : Node_Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Call_Stmt
           (Handle : Rewriting_Handle
               ; F_Call : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Delay_Stmt
           (Handle : Rewriting_Handle
               ; F_Has_Until : Node_Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Exit_Stmt
           (Handle : Rewriting_Handle
               ; F_Loop_Name : Node_Rewriting_Handle
               ; F_Cond_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Goto_Stmt
           (Handle : Rewriting_Handle
               ; F_Label_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Label
           (Handle : Rewriting_Handle
               ; F_Decl : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Raise_Stmt
           (Handle : Rewriting_Handle
               ; F_Exception_Name : Node_Rewriting_Handle
               ; F_Error_Message : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Requeue_Stmt
           (Handle : Rewriting_Handle
               ; F_Call_Name : Node_Rewriting_Handle
               ; F_Has_Abort : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Return_Stmt
           (Handle : Rewriting_Handle
               ; F_Return_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Subunit
           (Handle : Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Body : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Task_Def
           (Handle : Rewriting_Handle
               ; F_Interfaces : Node_Rewriting_Handle
               ; F_Public_Part : Node_Rewriting_Handle
               ; F_Private_Part : Node_Rewriting_Handle
               ; F_End_Name : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Access_To_Subp_Def
           (Handle : Rewriting_Handle
               ; F_Has_Not_Null : Node_Rewriting_Handle
               ; F_Has_Protected : Node_Rewriting_Handle
               ; F_Subp_Spec : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Anonymous_Type_Access_Def
           (Handle : Rewriting_Handle
               ; F_Has_Not_Null : Node_Rewriting_Handle
               ; F_Type_Decl : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Type_Access_Def
           (Handle : Rewriting_Handle
               ; F_Has_Not_Null : Node_Rewriting_Handle
               ; F_Has_All : Node_Rewriting_Handle
               ; F_Has_Constant : Node_Rewriting_Handle
               ; F_Subtype_Indication : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Array_Type_Def
           (Handle : Rewriting_Handle
               ; F_Indices : Node_Rewriting_Handle
               ; F_Component_Type : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Derived_Type_Def
           (Handle : Rewriting_Handle
               ; F_Has_Abstract : Node_Rewriting_Handle
               ; F_Has_Limited : Node_Rewriting_Handle
               ; F_Has_Synchronized : Node_Rewriting_Handle
               ; F_Subtype_Indication : Node_Rewriting_Handle
               ; F_Interfaces : Node_Rewriting_Handle
               ; F_Record_Extension : Node_Rewriting_Handle
               ; F_Has_With_Private : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Enum_Type_Def
           (Handle : Rewriting_Handle
               ; F_Enum_Literals : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Interface_Type_Def
           (Handle : Rewriting_Handle
               ; F_Interface_Kind : Node_Rewriting_Handle
               ; F_Interfaces : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Mod_Int_Type_Def
           (Handle : Rewriting_Handle
               ; F_Expr : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Private_Type_Def
           (Handle : Rewriting_Handle
               ; F_Has_Abstract : Node_Rewriting_Handle
               ; F_Has_Tagged : Node_Rewriting_Handle
               ; F_Has_Limited : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Decimal_Fixed_Point_Def
           (Handle : Rewriting_Handle
               ; F_Delta : Node_Rewriting_Handle
               ; F_Digits : Node_Rewriting_Handle
               ; F_Range : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Floating_Point_Def
           (Handle : Rewriting_Handle
               ; F_Num_Digits : Node_Rewriting_Handle
               ; F_Range : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Ordinary_Fixed_Point_Def
           (Handle : Rewriting_Handle
               ; F_Delta : Node_Rewriting_Handle
               ; F_Range : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Record_Type_Def
           (Handle : Rewriting_Handle
               ; F_Has_Abstract : Node_Rewriting_Handle
               ; F_Has_Tagged : Node_Rewriting_Handle
               ; F_Has_Limited : Node_Rewriting_Handle
               ; F_Record_Def : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Signed_Int_Type_Def
           (Handle : Rewriting_Handle
               ; F_Range : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Anonymous_Type
           (Handle : Rewriting_Handle
               ; F_Type_Decl : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Subtype_Indication
           (Handle : Rewriting_Handle
               ; F_Has_Not_Null : Node_Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Constraint : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Constrained_Subtype_Indication
           (Handle : Rewriting_Handle
               ; F_Has_Not_Null : Node_Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Constraint : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Discrete_Subtype_Indication
           (Handle : Rewriting_Handle
               ; F_Has_Not_Null : Node_Rewriting_Handle
               ; F_Name : Node_Rewriting_Handle
               ; F_Constraint : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Synthetic_Type_Expr
           (Handle : Rewriting_Handle
               ; F_Target_Type : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Unconstrained_Array_Index
           (Handle : Rewriting_Handle
               ; F_Subtype_Indication : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Use_Package_Clause
           (Handle : Rewriting_Handle
               ; F_Packages : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Use_Type_Clause
           (Handle : Rewriting_Handle
               ; F_Has_All : Node_Rewriting_Handle
               ; F_Types : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Value_Sequence
           (Handle : Rewriting_Handle
               ; F_Iter_Assoc : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Variant
           (Handle : Rewriting_Handle
               ; F_Choices : Node_Rewriting_Handle
               ; F_Components : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_Variant_Part
           (Handle : Rewriting_Handle
               ; F_Discr_Name : Node_Rewriting_Handle
               ; F_Variant : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


         function Create_With_Clause
           (Handle : Rewriting_Handle
               ; F_Has_Limited : Node_Rewriting_Handle
               ; F_Has_Private : Node_Rewriting_Handle
               ; F_Packages : Node_Rewriting_Handle
            ) return Node_Rewriting_Handle;


private

   --  Workaround S114-026 by not deriving from Impl.Rewriting_Handle directly.
   --  TODO: Cleanup once S114-026 is fixed.
   type Rewriting_Handle is new System.Address;
   type Unit_Rewriting_Handle is new System.Address;
   type Node_Rewriting_Handle is new System.Address;

   No_Rewriting_Handle : constant Rewriting_Handle :=
      Rewriting_Handle (System.Null_Address);
   No_Unit_Rewriting_Handle : constant Unit_Rewriting_Handle :=
      Unit_Rewriting_Handle (System.Null_Address);
   No_Node_Rewriting_Handle : constant Node_Rewriting_Handle :=
      Node_Rewriting_Handle (System.Null_Address);

end Libadalang.Rewriting;
