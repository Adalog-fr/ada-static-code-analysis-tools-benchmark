--
--  Copyright (C) 2014-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

private with Ada.Finalization;
private with Ada.Unchecked_Deallocation;

with Libadalang.Analysis; use Libadalang.Analysis;
with Libadalang.Common;   use Libadalang.Common;

--  This package provides primitives to inspect the structure of parse trees.
--  It answers questions such as: what is the index of a syntax field in a
--  node? or conversely, to what syntax field does a couple (index, node kind)
--  correspond?
--
--  For instance, the following code snippet prints the name of the first
--  syntax field in ``Node``:
--
--  .. code-block:: ada
--
--     declare
--        Field_Ref : constant Syntax_Field_Reference :=
--           Syntax_Field_Reference_From_Index (Node.Kind, 1);
--     begin
--        Ada.Text_IO.Put_Line (Field_Name (Field_Ref));
--     end;

package Libadalang.Introspection is

   use Support.Text;

   ----------------
   -- Node types --
   ----------------

   function DSL_Name (Id : Node_Type_Id) return Text_Type;
   --  Return the name corresponding to ``Id`` in the Langkit DSL

   function Lookup_DSL_Name (Name : Text_Type) return Any_Node_Type_Id;
   --  Look for the node type for which Name is in the Lankgit DSL. Return it
   --  if found, otherwise return None.

   function Is_Abstract (Id : Node_Type_Id) return Boolean;
   --  Return whether ``Id`` designates an abstract node

   function Is_Concrete (Id : Node_Type_Id) return Boolean
   is (not Is_Abstract (Id));

   function Kind_For (Id : Node_Type_Id) return Ada_Node_Kind_Type;
   --  Return the node kind corresponding to ``Id``. This raises a
   --  ``Bad_Type_Error`` if ``Id`` designates an abstract node.

   function First_Kind_For (Id : Node_Type_Id) return Ada_Node_Kind_Type;
   function Last_Kind_For (Id : Node_Type_Id) return Ada_Node_Kind_Type;
   --  Return the node kinds corresponding to respectively the first and
   --  last concrete nodes that derive from ``Id`` (included).

   function Id_For_Kind (Kind : Ada_Node_Kind_Type) return Node_Type_Id;
   --  Return the node type corresponding to the given node ``Kind``

   function Is_Root_Node (Id : Node_Type_Id) return Boolean;
   --  Return whether ``Id`` is a reference for the root node type

   function Base_Type (Id : Node_Type_Id) return Node_Type_Id;
   --  If Id is the root node type, raise a ``Bad_Type_Error``. Otherwise,
   --  return a reference to ``Id``'s base type.

   function Derived_Types (Id : Node_Type_Id) return Node_Type_Id_Array;
   --  Return type references for all direct derivations for ``Id``

   function Is_Derived_From (Id, Parent : Node_Type_Id) return Boolean;
   --  Return whether the type that ``Id`` represents is derives (directly or
   --  indirectly) from the type that ``Parent`` represents.

   ------------------------
   -- Polymorphic values --
   ------------------------

   type Any_Value_Type is private;
   --  Polymorphic value to contain ``Kind`` values. This type has by-reference
   --  semantics, so copying it is cheap.

   No_Value : constant Any_Value_Type;
   --  Special ``Any_Value_Type`` to mean: no reference to a value

   subtype Value_Type is Any_Value_Type
      with Dynamic_Predicate => Value_Type /= No_Value;

   function Kind (Self : Value_Type) return Value_Kind;
   --  Return the kind of values that ``Value`` holds

   --  Accessors and constructors for inner value

   function As_Boolean (Self : Value_Type) return Boolean
      with Pre => Kind (Self) = Boolean_Value;
   function Create_Boolean (Value : Boolean) return Value_Type;

   function As_Integer (Self : Value_Type) return Integer
      with Pre => Kind (Self) = Integer_Value;
   function Create_Integer (Value : Integer) return Value_Type;

   function As_Big_Integer (Self : Value_Type) return Big_Integer
      with Pre => Kind (Self) = Big_Integer_Value;
   function Create_Big_Integer (Value : Big_Integer) return Value_Type;

   function As_Character (Self : Value_Type) return Character_Type
      with Pre => Kind (Self) = Character_Value;
   function Create_Character (Value : Character_Type) return Value_Type;

   function As_String (Self : Value_Type) return Text_Type
      with Pre => Kind (Self) = String_Value;
   function Create_String (Value : Text_Type) return Value_Type;

   function As_Token (Self : Value_Type) return Token_Reference
      with Pre => Kind (Self) = Token_Value;
   function Create_Token (Value : Token_Reference) return Value_Type;

   function As_Unbounded_Text (Self : Value_Type) return Unbounded_Text_Type
      with Pre => Kind (Self) = Unbounded_Text_Value;
   function Create_Unbounded_Text
     (Value : Unbounded_Text_Type) return Value_Type;

   function As_Analysis_Unit (Self : Value_Type) return Analysis_Unit
      with Pre => Kind (Self) = Analysis_Unit_Value;
   function Create_Analysis_Unit (Value : Analysis_Unit) return Value_Type;

   function As_Node (Self : Value_Type) return Ada_Node
      with Pre => Kind (Self) = Node_Value;
   function Create_Node
     (Value : Ada_Node'Class) return Value_Type;

      function As_Analysis_Unit_Kind
        (Self : Value_Type) return Analysis_Unit_Kind
         with Pre => Kind (Self) = Analysis_Unit_Kind_Value;
      function Create_Analysis_Unit_Kind
        (Value : Analysis_Unit_Kind) return Value_Type;
      function As_Lookup_Kind
        (Self : Value_Type) return Lookup_Kind
         with Pre => Kind (Self) = Lookup_Kind_Value;
      function Create_Lookup_Kind
        (Value : Lookup_Kind) return Value_Type;
      function As_Designated_Env_Kind
        (Self : Value_Type) return Designated_Env_Kind
         with Pre => Kind (Self) = Designated_Env_Kind_Value;
      function Create_Designated_Env_Kind
        (Value : Designated_Env_Kind) return Value_Type;
      function As_Ref_Result_Kind
        (Self : Value_Type) return Ref_Result_Kind
         with Pre => Kind (Self) = Ref_Result_Kind_Value;
      function Create_Ref_Result_Kind
        (Value : Ref_Result_Kind) return Value_Type;
      function As_Call_Expr_Kind
        (Self : Value_Type) return Call_Expr_Kind
         with Pre => Kind (Self) = Call_Expr_Kind_Value;
      function Create_Call_Expr_Kind
        (Value : Call_Expr_Kind) return Value_Type;
      function As_Grammar_Rule
        (Self : Value_Type) return Grammar_Rule
         with Pre => Kind (Self) = Grammar_Rule_Value;
      function Create_Grammar_Rule
        (Value : Grammar_Rule) return Value_Type;

         function As_Aspect (Self : Value_Type) return Aspect
            with Pre => Kind (Self) = Aspect_Value;
         function Create_Aspect
           (Value : Aspect) return Value_Type;
         function As_Completion_Item (Self : Value_Type) return Completion_Item
            with Pre => Kind (Self) = Completion_Item_Value;
         function Create_Completion_Item
           (Value : Completion_Item) return Value_Type;
         function As_Completion_Item_Iterator (Self : Value_Type) return Completion_Item_Iterator
            with Pre => Kind (Self) = Completion_Item_Iterator_Value;
         function Create_Completion_Item_Iterator
           (Value : Completion_Item_Iterator) return Value_Type;
         function As_Discrete_Range (Self : Value_Type) return Discrete_Range
            with Pre => Kind (Self) = Discrete_Range_Value;
         function Create_Discrete_Range
           (Value : Discrete_Range) return Value_Type;
         function As_Discriminant_Values (Self : Value_Type) return Discriminant_Values
            with Pre => Kind (Self) = Discriminant_Values_Value;
         function Create_Discriminant_Values
           (Value : Discriminant_Values) return Value_Type;
         function As_Discriminant_Values_Array (Self : Value_Type) return Discriminant_Values_Array
            with Pre => Kind (Self) = Discriminant_Values_Array_Value;
         function Create_Discriminant_Values_Array
           (Value : Discriminant_Values_Array) return Value_Type;
         function As_Doc_Annotation (Self : Value_Type) return Doc_Annotation
            with Pre => Kind (Self) = Doc_Annotation_Value;
         function Create_Doc_Annotation
           (Value : Doc_Annotation) return Value_Type;
         function As_Doc_Annotation_Array (Self : Value_Type) return Doc_Annotation_Array
            with Pre => Kind (Self) = Doc_Annotation_Array_Value;
         function Create_Doc_Annotation_Array
           (Value : Doc_Annotation_Array) return Value_Type;
         function As_Accept_Stmt_Array (Self : Value_Type) return Accept_Stmt_Array
            with Pre => Kind (Self) = Accept_Stmt_Array_Value;
         function Create_Accept_Stmt_Array
           (Value : Accept_Stmt_Array) return Value_Type;
         function As_Ada_Node_Array (Self : Value_Type) return Ada_Node_Array
            with Pre => Kind (Self) = Ada_Node_Array_Value;
         function Create_Ada_Node_Array
           (Value : Ada_Node_Array) return Value_Type;
         function As_Base_Formal_Param_Decl_Array (Self : Value_Type) return Base_Formal_Param_Decl_Array
            with Pre => Kind (Self) = Base_Formal_Param_Decl_Array_Value;
         function Create_Base_Formal_Param_Decl_Array
           (Value : Base_Formal_Param_Decl_Array) return Value_Type;
         function As_Base_Type_Decl_Array (Self : Value_Type) return Base_Type_Decl_Array
            with Pre => Kind (Self) = Base_Type_Decl_Array_Value;
         function Create_Base_Type_Decl_Array
           (Value : Base_Type_Decl_Array) return Value_Type;
         function As_Basic_Decl_Array (Self : Value_Type) return Basic_Decl_Array
            with Pre => Kind (Self) = Basic_Decl_Array_Value;
         function Create_Basic_Decl_Array
           (Value : Basic_Decl_Array) return Value_Type;
         function As_Compilation_Unit_Array (Self : Value_Type) return Compilation_Unit_Array
            with Pre => Kind (Self) = Compilation_Unit_Array_Value;
         function Create_Compilation_Unit_Array
           (Value : Compilation_Unit_Array) return Value_Type;
         function As_Defining_Name_Array (Self : Value_Type) return Defining_Name_Array
            with Pre => Kind (Self) = Defining_Name_Array_Value;
         function Create_Defining_Name_Array
           (Value : Defining_Name_Array) return Value_Type;
         function As_Expr_Array (Self : Value_Type) return Expr_Array
            with Pre => Kind (Self) = Expr_Array_Value;
         function Create_Expr_Array
           (Value : Expr_Array) return Value_Type;
         function As_Generic_Instantiation_Array (Self : Value_Type) return Generic_Instantiation_Array
            with Pre => Kind (Self) = Generic_Instantiation_Array_Value;
         function Create_Generic_Instantiation_Array
           (Value : Generic_Instantiation_Array) return Value_Type;
         function As_Param_Spec_Array (Self : Value_Type) return Param_Spec_Array
            with Pre => Kind (Self) = Param_Spec_Array_Value;
         function Create_Param_Spec_Array
           (Value : Param_Spec_Array) return Value_Type;
         function As_Pragma_Node_Array (Self : Value_Type) return Pragma_Node_Array
            with Pre => Kind (Self) = Pragma_Node_Array_Value;
         function Create_Pragma_Node_Array
           (Value : Pragma_Node_Array) return Value_Type;
         function As_Type_Decl_Array (Self : Value_Type) return Type_Decl_Array
            with Pre => Kind (Self) = Type_Decl_Array_Value;
         function Create_Type_Decl_Array
           (Value : Type_Decl_Array) return Value_Type;
         function As_Param_Actual (Self : Value_Type) return Param_Actual
            with Pre => Kind (Self) = Param_Actual_Value;
         function Create_Param_Actual
           (Value : Param_Actual) return Value_Type;
         function As_Param_Actual_Array (Self : Value_Type) return Param_Actual_Array
            with Pre => Kind (Self) = Param_Actual_Array_Value;
         function Create_Param_Actual_Array
           (Value : Param_Actual_Array) return Value_Type;
         function As_Ref_Result (Self : Value_Type) return Ref_Result
            with Pre => Kind (Self) = Ref_Result_Value;
         function Create_Ref_Result
           (Value : Ref_Result) return Value_Type;
         function As_Ref_Result_Array (Self : Value_Type) return Ref_Result_Array
            with Pre => Kind (Self) = Ref_Result_Array_Value;
         function Create_Ref_Result_Array
           (Value : Ref_Result_Array) return Value_Type;
         function As_Refd_Decl (Self : Value_Type) return Refd_Decl
            with Pre => Kind (Self) = Refd_Decl_Value;
         function Create_Refd_Decl
           (Value : Refd_Decl) return Value_Type;
         function As_Refd_Def (Self : Value_Type) return Refd_Def
            with Pre => Kind (Self) = Refd_Def_Value;
         function Create_Refd_Def
           (Value : Refd_Def) return Value_Type;
         function As_Shape (Self : Value_Type) return Shape
            with Pre => Kind (Self) = Shape_Value;
         function Create_Shape
           (Value : Shape) return Value_Type;
         function As_Shape_Array (Self : Value_Type) return Shape_Array
            with Pre => Kind (Self) = Shape_Array_Value;
         function Create_Shape_Array
           (Value : Shape_Array) return Value_Type;
         function As_Substitution (Self : Value_Type) return Substitution
            with Pre => Kind (Self) = Substitution_Value;
         function Create_Substitution
           (Value : Substitution) return Value_Type;
         function As_Substitution_Array (Self : Value_Type) return Substitution_Array
            with Pre => Kind (Self) = Substitution_Array_Value;
         function Create_Substitution_Array
           (Value : Substitution_Array) return Value_Type;
         function As_Analysis_Unit_Array (Self : Value_Type) return Analysis_Unit_Array
            with Pre => Kind (Self) = Analysis_Unit_Array_Value;
         function Create_Analysis_Unit_Array
           (Value : Analysis_Unit_Array) return Value_Type;
         function As_Unbounded_Text_Type_Array (Self : Value_Type) return Unbounded_Text_Type_Array
            with Pre => Kind (Self) = Unbounded_Text_Type_Array_Value;
         function Create_Unbounded_Text_Type_Array
           (Value : Unbounded_Text_Type_Array) return Value_Type;

   type Value_Array is array (Positive range <>) of Value_Type;
   type Any_Value_Array is array (Positive range <>) of Any_Value_Type;

   function DSL_Name (Constraint : Type_Constraint) return Text_Type;
   --  Return the name corresponding to ``Constraint`` in the Langkit DSL

   function Satisfies
     (Value : Value_Type; Constraint : Type_Constraint) return Boolean;
   --  Return whether the given ``Value`` satisfy the given ``Constraint``

   -----------
   -- Enums --
   -----------

   type Any_Enum_Value_Index is new Natural;
   subtype Enum_Value_Index is Any_Enum_Value_Index
      range 0 ..  Any_Enum_Value_Index'Last;
   --  Index of an enum value for a given enum type

   No_Enum_Value_Index : constant Any_Enum_Value_Index := 0;

   function Enum_Last_Value (Kind : Enum_Value_Kind) return Enum_Value_Index;
   --  Return the index of the last enum value for the given ``Kind`` enum type

   function Enum_Default_Value
     (Kind : Enum_Value_Kind) return Any_Enum_Value_Index;
   --  Return the index of the default enum value for the given ``Kind`` enum
   --  type, or No_Enum_Value_Index if this type does not have a default value.

   function Enum_Value_Name
     (Kind : Enum_Value_Kind; Index : Enum_Value_Index) return Text_Type;
   --  Return the name corresponding to the ``Index``th value for the ``Kind``
   --  enum type. This raises a ``Out_Of_Bounds_Error`` if ``Index`` is too big
   --  for this enum type.

   function Lookup_Enum_Value
     (Kind : Enum_Value_Kind; Name : Text_Type) return Any_Enum_Value_Index;
   --  Return the index for the enumeration value in the ``Kind`` enum type
   --  whose name is ``Name``. Return ``No_Enum_Value_Index`` if no value in
   --  that enum type has such a name.

   function Create_Enum
     (Kind : Enum_Value_Kind; Index : Enum_Value_Index) return Value_Type;
   --  Return the enum value corresponding to the given ``Index`` and ``Kind``
   --  enum type. This raises a ``Out_Of_Bounds_Error`` if ``Index`` is too big
   --  for this enum type.

   function Enum_Index (Value : Value_Type) return Enum_Value_Index;
   --  Return the index for the given enum value, relative to its type. This
   --  raises a ``Bad_Type_Error`` if ``Value`` is not an enum value.

   ------------
   -- Arrays --
   ------------

   function Array_Element_Constraint
     (Kind : Array_Value_Kind) return Type_Constraint;
   --  Return the constraint for elements of ``Kind`` arrays

   function Array_Length (Self : Value_Type) return Natural;
   --  Assuming that ``Self`` is an array (regardless of its element type),
   --  return the number of elements it contains.
   --
   --  This raises a ``Bad_Type_Error`` if Value is not an array.

   function Array_Element
     (Self : Value_Type; Index : Positive) return Value_Type;
   --  Assuming that ``Self`` is an array (regardless of its element type),
   --  return the value at the given 1-based index.
   --
   --  This raises a ``Bad_Type_Error`` if ``Value`` is not an array, and an
   --  ``Out_Of_Bounds_Error`` if ``Index`` is out of ``Value``'s bounds.

   function Create_Array
     (Kind : Array_Value_Kind; Values : Value_Array) return Value_Type;
   --  Return an array of the given kind that contains the given values.
   --
   --  This raises a ``Bad_Type_Error`` if a value in ``Values`` does not have
   --  the type that ``Kind`` implies.

   -------------
   -- Structs --
   -------------

   function Struct_Fields
     (Kind : Struct_Value_Kind) return Struct_Field_Reference_Array;
   --  Return the list of fields for ``Kind`` structs

   function Create_Struct
     (Kind : Struct_Value_Kind; Values : Value_Array) return Value_Type;
   --  Return a struct of the given kind and assign the given values to each
   --  field.
   --
   --  ``Struct_Fields (Kind)`` describes what ``Values`` should be: both
   --  arrays must have the same length and the type of each value in
   --  ``Values`` must satisfy the corresponding field type.
   --
   --  This raises a ``Bad_Type_Error`` if ``Values`` does not have the
   --  expected size or if the values it contains do not have the expected
   --  type.

   -------------
   -- Members --
   -------------

   function Member_Name (Member : Member_Reference) return Text_Type;
   --  Return a lower-case name for ``Member``

   function Member_Type (Member : Member_Reference) return Type_Constraint;
   --  Return the constraint associated with ``Member``'s type (or its return
   --  type).

   function Eval_Member
     (Prefix    : Value_Type;
      Member    : Member_Reference;
      Arguments : Value_Array) return Value_Type;
   --  Evaluate ``Member`` on the given ``Prefix`` value and the given
   --  ``Arguments`` and return the result of this evaluation.
   --
   --  This raises a ``Property_Error`` If ``Member`` is a node property and
   --  that its execution raises a ``Property_Error``.
   --
   --  This raises a ``Bad_Type_Error`` if ``Prefix`` is not a struct and not a
   --  node, if ``Prefix`` has no such member or if the provided arguments are
   --  invalid for that member.

   function Eval_Member
     (Prefix : Value_Type; Field : Struct_Field_Reference) return Value_Type;
   --  Evaluate ``Field`` on the given ``Prefix`` struct value and return the
   --  result of this evaluation.
   --
   --  This raises a ``Bad_Type_Error`` if ``Prefix`` is not a struct or if
   --  ``Prefix`` has no such field.

   function Eval_Member
     (Node      : Ada_Node'Class;
      Member    : Node_Member_Reference;
      Arguments : Value_Array) return Value_Type;
   --  Evaluate ``Member on`` the given ``Node`` and the given ``Arguments``.
   --  If the evaluation raises a ``Property_Error``, forward it. Otherwise,
   --  return its result.
   --
   --  This raises a ``Bad_Type_Error`` if ``Node`` has no such member or if
   --  the provided arguments are invalid for it.

   function Lookup_Member
     (Prefix : Value_Type;
      Name   : Text_Type) return Any_Member_Reference;
   --  Look for the member corresponding to the given ``Name`` (lower-case
   --  name) in the given ``Prefix`` value. Return it if found, otherwise
   --  return None.
   --
   --  This raises a ``Bad_Type_Error`` if ``Prefix`` is not a struct and not a
   --  node, if ``Prefix`` is a null node, if ``Prefix`` has no such member or
   --  if the provided arguments are invalid for that member.

   function Lookup_Member
     (Id   : Node_Type_Id;
      Name : Text_Type) return Any_Member_Reference;
   --  Look for the member corresponding to the given ``Name`` (lower-case
   --  name) in the given node type reference (``Id``). Return it if found,
   --  otherwise return None.

   -------------------------------
   -- Syntax fields (for nodes) --
   -------------------------------

   function Eval_Syntax_Field
     (Node  : Ada_Node'Class;
      Field : Syntax_Field_Reference) return Ada_Node;
   --  Evaluate ``Field`` on the given ``Node``. Return the corresponding
   --  children ``Node``.
   --
   --  This raises a Bad_Type_Error if ``Node`` has no such field.

   function Index
     (Kind : Ada_Node_Kind_Type; Field : Syntax_Field_Reference) return Positive;
   --  Return the index in nodes to access the given ``Field`` considering the
   --  given ``Kind`` of node.
   --
   --  This raises an ``Bad_Type_Error`` exception if ``Kind`` nodes do not
   --  have the given ``Field``.

   function Syntax_Field_Reference_From_Index
     (Kind : Ada_Node_Kind_Type; Index : Positive) return Syntax_Field_Reference;
   --  Return the field reference corresponding to the given ``Index`` in nodes
   --  of the given ``Kind``. Raise an ``Bad_Type_Error`` exception if there is
   --  no field corresponding to this index.

   function Syntax_Fields
     (Kind : Ada_Node_Kind_Type) return Syntax_Field_Reference_Array;
   --  Return the list of fields that nodes of the given ``Kind`` have. This
   --  returns an empty array for list nodes.

   function Syntax_Fields
     (Id : Node_Type_Id) return Syntax_Field_Reference_Array;
   --  Likewise, but taking a reference to a node type instead

   ----------------------------
   -- Properties (for nodes) --
   ----------------------------

   function Property_Argument_Types
     (Property : Property_Reference) return Type_Constraint_Array
      with Post => Property_Argument_Types'Result'Length = 0
                   or else Property_Argument_Types'Result'First = 1;
   --  Return the type constraints for ``Property``'s arguments

   function Property_Argument_Name
     (Property        : Property_Reference;
      Argument_Number : Positive) return Text_Type;
   --  Return the lower-cased name for ``Property``'s argument whose index is
   --  ``Argument_Number``. This raises a ``Property_Error`` if ``Property``
   --  has no such argument.

   function Property_Argument_Default_Value
     (Property        : Property_Reference;
      Argument_Number : Positive) return Any_Value_Type;
   --  If the argument corresponding to ``Argument_Number`` of the given
   --  ``Property`` has a default value, return it. Return ``No_Value``
   --  otherwise.  This raises a ``Property_Error`` if Property has no such
   --  argument.

   function Eval_Property
     (Node      : Ada_Node'Class;
      Property  : Property_Reference;
      Arguments : Value_Array) return Value_Type;
   --  Evaluate ``Property`` on the given ``Node`` and the given arguments. If
   --  the property raises a ``Property_Error``, forward it, otherwise return
   --  its result.
   --
   --  This raises a ``Bad_Type_Error`` if ``Node`` has no such property or if
   --  the provided arguments are invalid for this property.

   function Properties (Kind : Ada_Node_Kind_Type) return Property_Reference_Array;
   --  Return the list of properties that nodes of the given ``Kind`` have

   function Properties (Id : Node_Type_Id) return Property_Reference_Array;
   --  Likewise, but taking a reference to a node type instead

   ------------
   -- Tokens --
   ------------

   function Token_Node_Kind (Kind : Ada_Node_Kind_Type) return Token_Kind
      with Pre => Is_Token_Node (Kind);
   --  Return the token kind corresponding to the given token node kind

private

   type Value_Record;
   type Value_Access is access all Value_Record;

   --  In order to avoid ``Any_Value_Type`` to be tagged (which makes all its
   --  primitives dispatching, which has awful consequences, such as making
   --  some code patterns illegal, or making GNAT slow, wrap the access in a
   --  dedicated controlled object and make ``Any_Value_Type`` contain this
   --  wrapper.

   type Value_Access_Wrapper is new Ada.Finalization.Controlled with record
      Value : Value_Access;
   end record;

   overriding procedure Adjust (Self : in out Value_Access_Wrapper);
   overriding procedure Finalize (Self : in out Value_Access_Wrapper);

   type Any_Value_Type is record
      Value : Value_Access_Wrapper;
   end record;

   No_Value : constant Any_Value_Type :=
     (Value => (Ada.Finalization.Controlled with Value => null));

         type Discriminant_Values_Array_Access is access all Discriminant_Values_Array;
         procedure Free is new Ada.Unchecked_Deallocation
           (Discriminant_Values_Array, Discriminant_Values_Array_Access);
         type Doc_Annotation_Array_Access is access all Doc_Annotation_Array;
         procedure Free is new Ada.Unchecked_Deallocation
           (Doc_Annotation_Array, Doc_Annotation_Array_Access);
         type Accept_Stmt_Array_Access is access all Accept_Stmt_Array;
         procedure Free is new Ada.Unchecked_Deallocation
           (Accept_Stmt_Array, Accept_Stmt_Array_Access);
         type Ada_Node_Array_Access is access all Ada_Node_Array;
         procedure Free is new Ada.Unchecked_Deallocation
           (Ada_Node_Array, Ada_Node_Array_Access);
         type Base_Formal_Param_Decl_Array_Access is access all Base_Formal_Param_Decl_Array;
         procedure Free is new Ada.Unchecked_Deallocation
           (Base_Formal_Param_Decl_Array, Base_Formal_Param_Decl_Array_Access);
         type Base_Type_Decl_Array_Access is access all Base_Type_Decl_Array;
         procedure Free is new Ada.Unchecked_Deallocation
           (Base_Type_Decl_Array, Base_Type_Decl_Array_Access);
         type Basic_Decl_Array_Access is access all Basic_Decl_Array;
         procedure Free is new Ada.Unchecked_Deallocation
           (Basic_Decl_Array, Basic_Decl_Array_Access);
         type Compilation_Unit_Array_Access is access all Compilation_Unit_Array;
         procedure Free is new Ada.Unchecked_Deallocation
           (Compilation_Unit_Array, Compilation_Unit_Array_Access);
         type Defining_Name_Array_Access is access all Defining_Name_Array;
         procedure Free is new Ada.Unchecked_Deallocation
           (Defining_Name_Array, Defining_Name_Array_Access);
         type Expr_Array_Access is access all Expr_Array;
         procedure Free is new Ada.Unchecked_Deallocation
           (Expr_Array, Expr_Array_Access);
         type Generic_Instantiation_Array_Access is access all Generic_Instantiation_Array;
         procedure Free is new Ada.Unchecked_Deallocation
           (Generic_Instantiation_Array, Generic_Instantiation_Array_Access);
         type Param_Spec_Array_Access is access all Param_Spec_Array;
         procedure Free is new Ada.Unchecked_Deallocation
           (Param_Spec_Array, Param_Spec_Array_Access);
         type Pragma_Node_Array_Access is access all Pragma_Node_Array;
         procedure Free is new Ada.Unchecked_Deallocation
           (Pragma_Node_Array, Pragma_Node_Array_Access);
         type Type_Decl_Array_Access is access all Type_Decl_Array;
         procedure Free is new Ada.Unchecked_Deallocation
           (Type_Decl_Array, Type_Decl_Array_Access);
         type Param_Actual_Array_Access is access all Param_Actual_Array;
         procedure Free is new Ada.Unchecked_Deallocation
           (Param_Actual_Array, Param_Actual_Array_Access);
         type Ref_Result_Array_Access is access all Ref_Result_Array;
         procedure Free is new Ada.Unchecked_Deallocation
           (Ref_Result_Array, Ref_Result_Array_Access);
         type Shape_Array_Access is access all Shape_Array;
         procedure Free is new Ada.Unchecked_Deallocation
           (Shape_Array, Shape_Array_Access);
         type Substitution_Array_Access is access all Substitution_Array;
         procedure Free is new Ada.Unchecked_Deallocation
           (Substitution_Array, Substitution_Array_Access);
         type Analysis_Unit_Array_Access is access all Analysis_Unit_Array;
         procedure Free is new Ada.Unchecked_Deallocation
           (Analysis_Unit_Array, Analysis_Unit_Array_Access);
         type Unbounded_Text_Type_Array_Access is access all Unbounded_Text_Type_Array;
         procedure Free is new Ada.Unchecked_Deallocation
           (Unbounded_Text_Type_Array, Unbounded_Text_Type_Array_Access);

   type Value_Record (Kind : Value_Kind := Value_Kind'First) is
   limited record
      Ref_Count : Natural;

      case Kind is
         when Boolean_Value =>
            Boolean_Value : Boolean;

         when Integer_Value =>
            Integer_Value : Integer;

         when Big_Integer_Value =>
            Big_Integer_Value : Big_Integer;

         when Character_Value =>
            Character_Value : Character_Type;

         when String_Value =>
            String_Value : Unbounded_Text_Type;

         when Token_Value =>
            Token_Value : Token_Reference;

         when Unbounded_Text_Value =>
            Unbounded_Text_Value : Unbounded_Text_Type;

         when Analysis_Unit_Value =>
            Analysis_Unit_Value : Analysis_Unit;

         when Node_Value =>
            Node_Value : Ada_Node;

         when Analysis_Unit_Kind_Value =>
            Analysis_Unit_Kind_Value : Analysis_Unit_Kind;
         when Lookup_Kind_Value =>
            Lookup_Kind_Value : Lookup_Kind;
         when Designated_Env_Kind_Value =>
            Designated_Env_Kind_Value : Designated_Env_Kind;
         when Ref_Result_Kind_Value =>
            Ref_Result_Kind_Value : Ref_Result_Kind;
         when Call_Expr_Kind_Value =>
            Call_Expr_Kind_Value : Call_Expr_Kind;
         when Grammar_Rule_Value =>
            Grammar_Rule_Value : Grammar_Rule;


         when Aspect_Value =>
            Aspect_Value :
                  Aspect;
         when Completion_Item_Value =>
            Completion_Item_Value :
                  Completion_Item;
         when Completion_Item_Iterator_Value =>
            Completion_Item_Iterator_Value :
                  Completion_Item_Iterator;
         when Discrete_Range_Value =>
            Discrete_Range_Value :
                  Discrete_Range;
         when Discriminant_Values_Value =>
            Discriminant_Values_Value :
                  Discriminant_Values;
         when Discriminant_Values_Array_Value =>
            Discriminant_Values_Array_Value :
                  Discriminant_Values_Array_Access;
         when Doc_Annotation_Value =>
            Doc_Annotation_Value :
                  Doc_Annotation;
         when Doc_Annotation_Array_Value =>
            Doc_Annotation_Array_Value :
                  Doc_Annotation_Array_Access;
         when Accept_Stmt_Array_Value =>
            Accept_Stmt_Array_Value :
                  Accept_Stmt_Array_Access;
         when Ada_Node_Array_Value =>
            Ada_Node_Array_Value :
                  Ada_Node_Array_Access;
         when Base_Formal_Param_Decl_Array_Value =>
            Base_Formal_Param_Decl_Array_Value :
                  Base_Formal_Param_Decl_Array_Access;
         when Base_Type_Decl_Array_Value =>
            Base_Type_Decl_Array_Value :
                  Base_Type_Decl_Array_Access;
         when Basic_Decl_Array_Value =>
            Basic_Decl_Array_Value :
                  Basic_Decl_Array_Access;
         when Compilation_Unit_Array_Value =>
            Compilation_Unit_Array_Value :
                  Compilation_Unit_Array_Access;
         when Defining_Name_Array_Value =>
            Defining_Name_Array_Value :
                  Defining_Name_Array_Access;
         when Expr_Array_Value =>
            Expr_Array_Value :
                  Expr_Array_Access;
         when Generic_Instantiation_Array_Value =>
            Generic_Instantiation_Array_Value :
                  Generic_Instantiation_Array_Access;
         when Param_Spec_Array_Value =>
            Param_Spec_Array_Value :
                  Param_Spec_Array_Access;
         when Pragma_Node_Array_Value =>
            Pragma_Node_Array_Value :
                  Pragma_Node_Array_Access;
         when Type_Decl_Array_Value =>
            Type_Decl_Array_Value :
                  Type_Decl_Array_Access;
         when Param_Actual_Value =>
            Param_Actual_Value :
                  Param_Actual;
         when Param_Actual_Array_Value =>
            Param_Actual_Array_Value :
                  Param_Actual_Array_Access;
         when Ref_Result_Value =>
            Ref_Result_Value :
                  Ref_Result;
         when Ref_Result_Array_Value =>
            Ref_Result_Array_Value :
                  Ref_Result_Array_Access;
         when Refd_Decl_Value =>
            Refd_Decl_Value :
                  Refd_Decl;
         when Refd_Def_Value =>
            Refd_Def_Value :
                  Refd_Def;
         when Shape_Value =>
            Shape_Value :
                  Shape;
         when Shape_Array_Value =>
            Shape_Array_Value :
                  Shape_Array_Access;
         when Substitution_Value =>
            Substitution_Value :
                  Substitution;
         when Substitution_Array_Value =>
            Substitution_Array_Value :
                  Substitution_Array_Access;
         when Analysis_Unit_Array_Value =>
            Analysis_Unit_Array_Value :
                  Analysis_Unit_Array_Access;
         when Unbounded_Text_Type_Array_Value =>
            Unbounded_Text_Type_Array_Value :
                  Unbounded_Text_Type_Array_Access;
      end case;
   end record;

   procedure Free is new Ada.Unchecked_Deallocation
     (Value_Record, Value_Access);

end Libadalang.Introspection;
