--
--  Copyright (C) 2014-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Containers;            use Ada.Containers;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
pragma Warnings (Off, "is an internal GNAT unit");
with Ada.Strings.Unbounded.Aux; use Ada.Strings.Unbounded.Aux;
pragma Warnings (On, "is an internal GNAT unit");

with GNATCOLL.Traces;

pragma Warnings (Off, "referenced");
with Langkit_Support.Symbols; use Langkit_Support.Symbols;
pragma Warnings (On, "referenced");

with Langkit_Support.Types;        use Langkit_Support.Types;

with Libadalang.Common;
with Libadalang.Private_Converters;
use Libadalang.Private_Converters;
with Libadalang.Public_Converters; use Libadalang.Public_Converters;


          with Libadalang.Implementation.Extensions;
            use Libadalang.Implementation.Extensions;
          with Libadalang.Lexer;


package body Libadalang.Analysis is

   use Libadalang.Implementation;
   use AST_Envs;

      


      

      


      

      


      

      


      

      


      

      


      

      


      

      


      

      


      

      


      

      
      function To_Public_Discriminant_Values_Array
         (Value : Internal_Discriminant_Values_Array_Access) return Discriminant_Values_Array;


      

      
      function To_Public_Doc_Annotation_Array
         (Value : Internal_Doc_Annotation_Array_Access) return Doc_Annotation_Array;


      

      
      function To_Public_Accept_Stmt_Array
         (Value : Internal_Entity_Accept_Stmt_Array_Access) return Accept_Stmt_Array;


      

      
      function To_Public_Ada_Node_Array
         (Value : Internal_Entity_Array_Access) return Ada_Node_Array;


      

      


      

      
      function To_Public_Base_Formal_Param_Decl_Array
         (Value : Internal_Entity_Base_Formal_Param_Decl_Array_Access) return Base_Formal_Param_Decl_Array;


      

      
      function To_Public_Base_Type_Decl_Array
         (Value : Internal_Entity_Base_Type_Decl_Array_Access) return Base_Type_Decl_Array;


      

      


      

      
      function To_Public_Basic_Decl_Array
         (Value : Internal_Entity_Basic_Decl_Array_Access) return Basic_Decl_Array;


      

      
      function To_Public_Compilation_Unit_Array
         (Value : Internal_Entity_Compilation_Unit_Array_Access) return Compilation_Unit_Array;


      

      


      

      
      function To_Public_Defining_Name_Array
         (Value : Internal_Entity_Defining_Name_Array_Access) return Defining_Name_Array;


      

      
      function To_Public_Expr_Array
         (Value : Internal_Entity_Expr_Array_Access) return Expr_Array;


      

      
      function To_Public_Generic_Instantiation_Array
         (Value : Internal_Entity_Generic_Instantiation_Array_Access) return Generic_Instantiation_Array;


      

      


      

      


      

      


      

      
      function To_Public_Param_Spec_Array
         (Value : Internal_Entity_Param_Spec_Array_Access) return Param_Spec_Array;


      

      
      function To_Public_Pragma_Node_Array
         (Value : Internal_Entity_Pragma_Node_Array_Access) return Pragma_Node_Array;


      

      
      function To_Public_Type_Decl_Array
         (Value : Internal_Entity_Type_Decl_Array_Access) return Type_Decl_Array;


      

      


      

      


      

      


      

      


      

      
      function To_Public_Param_Actual_Array
         (Value : Internal_Param_Actual_Array_Access) return Param_Actual_Array;


      

      


      

      
      function To_Public_Ref_Result_Array
         (Value : Internal_Ref_Result_Array_Access) return Ref_Result_Array;


      

      
      function To_Public_Shape_Array
         (Value : Internal_Shape_Array_Access) return Shape_Array;


      

      


      

      

      function To_Internal_Substitution_Array
         (Value : Substitution_Array
          ) return Internal_Substitution_Array_Access;

      

      
      function To_Public_Analysis_Unit_Array
         (Value : Internal_Unit_Array_Access) return Analysis_Unit_Array;

      function To_Internal_Analysis_Unit_Array
         (Value : Analysis_Unit_Array
          ) return Internal_Unit_Array_Access;

      

      


      

      


      

      


      

      
      function To_Public_Unbounded_Text_Type_Array
         (Value : Symbol_Type_Array_Access) return Unbounded_Text_Type_Array;


      


      

      

      

      

      

      

      

      

      

      
      function To_Public_Completion_Item_Iterator
        (Value : Internal_Completion_Item_Iterator_Access) return Completion_Item_Iterator;

      

      

      

      

      

      

      

      

      

      

      

      

      

      

      

      

      

      

      

      

      

      

      

      

      

      

      

      

      

      

      

      

      

      

      


         
      function To_Public_Aspect
        (Value : Internal_Aspect) return Aspect;


         
      function To_Public_Completion_Item
        (Value : Internal_Completion_Item) return Completion_Item;


         
      function To_Public_Discrete_Range
        (Value : Internal_Discrete_Range) return Discrete_Range;


         
      function To_Public_Discriminant_Values
        (Value : Internal_Discriminant_Values) return Discriminant_Values;


         
      function To_Public_Doc_Annotation
        (Value : Internal_Doc_Annotation) return Doc_Annotation;


         
      function To_Public_Param_Actual
        (Value : Internal_Param_Actual) return Param_Actual;


         
      function To_Public_Ref_Result
        (Value : Internal_Ref_Result) return Ref_Result;


         
      function To_Public_Refd_Decl
        (Value : Internal_Refd_Decl) return Refd_Decl;


         
      function To_Public_Refd_Def
        (Value : Internal_Refd_Def) return Refd_Def;


         
      function To_Public_Shape
        (Value : Internal_Shape) return Shape;


         

      function To_Internal_Substitution
        (Value : Substitution) return Internal_Substitution;


   ----------------
   -- Do_Release --
   ----------------

   procedure Do_Release (Self : in out Event_Handler_Interface'Class) is
   begin
      Self.Release;
   end Do_Release;

   ----------------
   -- Do_Release --
   ----------------

   procedure Do_Release (Provider : in out Unit_Provider_Interface'Class) is
   begin
      Provider.Release;
   end Do_Release;

   ------------------------------------
   -- Create_Unit_Provider_Reference --
   ------------------------------------

   function Create_Unit_Provider_Reference
     (Provider : Unit_Provider_Interface'Class) return Unit_Provider_Reference
   is
   begin
      return Result : Unit_Provider_Reference do
         Result.Set (Provider);
      end return;
   end Create_Unit_Provider_Reference;

   ------------------------------------
   -- Create_Event_Handler_Reference --
   ------------------------------------

   function Create_Event_Handler_Reference
     (Handler : Event_Handler_Interface'Class) return Event_Handler_Reference
   is
   begin
      return Result : Event_Handler_Reference do
         Result.Set (Handler);
      end return;
   end Create_Event_Handler_Reference;

   --------------------
   -- Create_Context --
   --------------------

   function Create_Context
     (Charset       : String := Default_Charset;
      File_Reader   : File_Reader_Reference := No_File_Reader_Reference;
      Unit_Provider : Unit_Provider_Reference := No_Unit_Provider_Reference;
      Event_Handler : Event_Handler_Reference := No_Event_Handler_Ref;
      With_Trivia   : Boolean := True;
      Tab_Stop      : Positive := 8)
      return Analysis_Context
   is
      use Unit_Provider_References;

      FR     : Internal_File_Reader_Access :=
         Wrap_Public_File_Reader (File_Reader);
      UP     : Internal_Unit_Provider_Access :=
         Wrap_Public_Provider (Unit_Provider);
      EH     : Internal_Event_Handler_Access :=
         Wrap_Public_Event_Handler (Event_Handler);
      Result : Internal_Context := Create_Context
        (Charset, FR, UP, EH, With_Trivia, Tab_Stop);
   begin
      --  Create_Context created ownership shares for itself, so don't forget
      --  to remove the shares on FR and UP.
      Dec_Ref (FR);
      Dec_Ref (UP);
      Dec_Ref (EH);

      return Context : constant Analysis_Context := Wrap_Context (Result)
      do
         --  Result has one ownership share and the call to Wrap_Context
         --  creates a new one, so don't forget to dec-ref before returning.
         Dec_Ref (Result);
      end return;
   end Create_Context;

   --------------
   -- Has_Unit --
   --------------

   function Has_Unit
     (Context       : Analysis_Context'Class;
      Unit_Filename : String) return Boolean is
   begin
      if Context.Internal = null then
         raise Precondition_Failure with "null context argument";
      end if;
      return Has_Unit (Unwrap_Context (Context), Unit_Filename);
   end Has_Unit;

   -------------------
   -- Get_From_File --
   -------------------

   function Get_From_File
     (Context  : Analysis_Context'Class;
      Filename : String;
      Charset  : String := "";
      Reparse  : Boolean := False;
      Rule     : Grammar_Rule := Default_Grammar_Rule) return Analysis_Unit is
   begin
      if Context.Internal = null then
         raise Precondition_Failure with "null context argument";
      end if;
      return Wrap_Unit
        (Get_From_File (Unwrap_Context (Context), Filename, Charset,
                        Reparse, Rule));
   end Get_From_File;

   ---------------------
   -- Get_From_Buffer --
   ---------------------

   function Get_From_Buffer
     (Context  : Analysis_Context'Class;
      Filename : String;
      Charset  : String := "";
      Buffer   : String;
      Rule     : Grammar_Rule := Default_Grammar_Rule) return Analysis_Unit is
   begin
      if Context.Internal = null then
         raise Precondition_Failure with "null context argument";
      end if;
      return Wrap_Unit
        (Get_From_Buffer (Unwrap_Context (Context), Filename, Charset,
                          Buffer, Rule));
   end Get_From_Buffer;

   ---------------------
   -- Get_From_Buffer --
   ---------------------

   function Get_From_Buffer
     (Context  : Analysis_Context'Class;
      Filename : String;
      Charset  : String := "";
      Buffer   : Ada.Strings.Unbounded.Unbounded_String;
      Rule     : Grammar_Rule := Default_Grammar_Rule) return Analysis_Unit
   is
      Bytes       : Big_String_Access;
      Bytes_Count : Natural;
   begin
      if Context.Internal = null then
         raise Precondition_Failure with "null context argument";
      end if;
      Get_String (Buffer, Bytes, Bytes_Count);
      return Wrap_Unit
        (Get_From_Buffer (Unwrap_Context (Context), Filename, Charset,
                          Bytes (1 .. Bytes_Count), Rule));
   end Get_From_Buffer;

   --------------------
   -- Get_With_Error --
   --------------------

   function Get_With_Error
     (Context  : Analysis_Context'Class;
      Filename : String;
      Error    : Text_Type;
      Charset  : String := "";
      Rule     : Grammar_Rule := Default_Grammar_Rule) return Analysis_Unit
   is
      Result : Internal_Unit;
   begin
      if Context.Internal = null then
         raise Precondition_Failure with "null context argument";
      end if;
      Result := Implementation.Get_With_Error
        (Unwrap_Context (Context), Filename, Error, Charset, Rule);
      return Wrap_Unit (Result);
   end Get_With_Error;


   -----------------------
   -- Get_From_Provider --
   -----------------------

   function Get_From_Provider
     (Context : Analysis_Context'Class;
      Name    : Text_Type;
      Kind    : Analysis_Unit_Kind;
      Charset : String := "";
      Reparse : Boolean := False) return Analysis_Unit is
   begin
      if Context.Internal = null then
         raise Precondition_Failure with "null context argument";
      end if;
      return Wrap_Unit
        (Get_From_Provider (Unwrap_Context (Context), Name, Kind,
                            Charset, Reparse));
   end Get_From_Provider;


   -------------------
   -- Unit_Provider --
   -------------------

   function Unit_Provider
     (Context : Analysis_Context'Class) return Unit_Provider_Reference
   is
      Provider : Internal_Unit_Provider_Access;
   begin
      if Context.Internal = null then
         raise Precondition_Failure with "null context argument";
      end if;

      --  By design, Unit_Provider_Wrapper is supposed to be the only
      --  implementation of the Internal_Unit_Provider interface.
      Provider := Unit_Provider (Unwrap_Context (Context));
      if Provider.all not in Unit_Provider_Wrapper'Class then
         raise Program_Error;
      end if;

      return Unit_Provider_Wrapper (Provider.all).Internal;
   end Unit_Provider;

   ----------
   -- Hash --
   ----------

   function Hash (Context : Analysis_Context) return Ada.Containers.Hash_Type
   is
   begin
      return Hash (Unwrap_Context (Context));
   end Hash;

   ---------------------
   -- Has_With_Trivia --
   ---------------------

   function Has_With_Trivia (Context : Analysis_Context'Class) return Boolean
   is
   begin
      if Context.Internal = null then
         raise Precondition_Failure with "null context argument";
      end if;

      return Has_With_Trivia (Unwrap_Context (Context));
   end Has_With_Trivia;

   --------------------------------------------
   -- Discard_Errors_In_Populate_Lexical_Env --
   --------------------------------------------

   procedure Discard_Errors_In_Populate_Lexical_Env
     (Context : Analysis_Context'Class; Discard : Boolean) is
   begin
      if Context.Internal = null then
         raise Precondition_Failure with "null context argument";
      end if;

      Discard_Errors_In_Populate_Lexical_Env
        (Unwrap_Context (Context), Discard);
   end Discard_Errors_In_Populate_Lexical_Env;

   ----------------------------------
   -- Set_Logic_Resolution_Timeout --
   ----------------------------------

   procedure Set_Logic_Resolution_Timeout
     (Context : Analysis_Context'Class; Timeout : Natural) is
   begin
      if Context.Internal = null then
         raise Precondition_Failure with "null context argument";
      end if;

      Set_Logic_Resolution_Timeout (Unwrap_Context (Context), Timeout);
   end Set_Logic_Resolution_Timeout;

   ---------------------------
   -- Set_Lookup_Cache_Mode --
   ---------------------------

   procedure Set_Lookup_Cache_Mode (Mode : Lookup_Cache_Kind) is
   begin
      Lookup_Cache_Mode := Mode;
   end Set_Lookup_Cache_Mode;

   --------------------------
   -- Has_Rewriting_Handle --
   --------------------------

   function Has_Rewriting_Handle
     (Context : Analysis_Context'Class) return Boolean is
   begin
      if Context.Internal = null then
         raise Precondition_Failure with "null context argument";
      end if;

      return Has_Rewriting_Handle (Unwrap_Context (Context));
   end Has_Rewriting_Handle;

   ----------------------
   -- Get_Symbol_Table --
   ----------------------

   function Get_Symbol_Table
     (Context : Analysis_Context'Class) return Symbol_Table
   is
   begin
      return Context.Internal.Symbols;
   end Get_Symbol_Table;

   -------------
   -- Context --
   -------------

   function Context (Unit : Analysis_Unit'Class) return Analysis_Context is
   begin
      if Unit.Internal = null then
         raise Precondition_Failure with "null unit argument";
      end if;

      return Wrap_Context (Context (Unwrap_Unit (Unit)));
   end Context;

   ----------
   -- Hash --
   ----------

   function Hash (Unit : Analysis_Unit) return Ada.Containers.Hash_Type
   is
   begin
      return Hash (Unwrap_Unit (Unit));
   end Hash;

   -------------
   -- Reparse --
   -------------

   procedure Reparse (Unit : Analysis_Unit'Class; Charset : String := "") is
   begin
      if Unit.Internal = null then
         raise Precondition_Failure with "null unit argument";
      end if;

      Reparse (Unwrap_Unit (Unit), Charset);
   end Reparse;

   -------------
   -- Reparse --
   -------------

   procedure Reparse
     (Unit : Analysis_Unit'Class; Charset : String := ""; Buffer  : String) is
   begin
      if Unit.Internal = null then
         raise Precondition_Failure with "null unit argument";
      end if;

      Reparse (Unwrap_Unit (Unit), Charset, Buffer);
   end Reparse;

   --------------------------
   -- Populate_Lexical_Env --
   --------------------------

   procedure Populate_Lexical_Env (Unit : Analysis_Unit'Class) is
   begin
      if Unit.Internal = null then
         raise Precondition_Failure with "null unit argument";
      end if;

      Populate_Lexical_Env (Unwrap_Unit (Unit));
   end Populate_Lexical_Env;

   ------------------
   -- Get_Filename --
   ------------------

   function Get_Filename (Unit : Analysis_Unit'Class) return String is
   begin
      if Unit.Internal = null then
         raise Precondition_Failure with "null unit argument";
      end if;

      return Get_Filename (Unwrap_Unit (Unit));
   end Get_Filename;

   -----------------
   -- Get_Charset --
   -----------------

   function Get_Charset (Unit : Analysis_Unit'Class) return String is
   begin
      if Unit.Internal = null then
         raise Precondition_Failure with "null unit argument";
      end if;

      return Get_Charset (Unwrap_Unit (Unit));
   end Get_Charset;

   ---------------------
   -- Has_Diagnostics --
   ---------------------

   function Has_Diagnostics (Unit : Analysis_Unit'Class) return Boolean is
   begin
      if Unit.Internal = null then
         raise Precondition_Failure with "null unit argument";
      end if;

      return Has_Diagnostics (Unwrap_Unit (Unit));
   end Has_Diagnostics;

   -----------------
   -- Diagnostics --
   -----------------

   function Diagnostics (Unit : Analysis_Unit'Class) return Diagnostics_Array
   is
   begin
      if Unit.Internal = null then
         raise Precondition_Failure with "null unit argument";
      end if;

      return Implementation.Diagnostics (Unwrap_Unit (Unit));
   end Diagnostics;

   ---------------------------
   -- Format_GNU_Diagnostic --
   ---------------------------

   function Format_GNU_Diagnostic
     (Unit : Analysis_Unit'Class; D : Diagnostic) return String is
   begin
      if Unit.Internal = null then
         raise Precondition_Failure with "null unit argument";
      end if;

      return Format_GNU_Diagnostic (Unwrap_Unit (Unit), D);
   end Format_GNU_Diagnostic;

   ----------
   -- Root --
   ----------

   function Root (Unit : Analysis_Unit'Class) return Ada_Node is
   begin
      if Unit.Internal = null then
         raise Precondition_Failure with "null unit argument";
      end if;

      return Wrap_Node (Root (Unwrap_Unit (Unit)));
   end Root;

   -----------------
   -- First_Token --
   -----------------

   function First_Token (Unit : Analysis_Unit'Class) return Token_Reference is
   begin
      if Unit.Internal = null then
         raise Precondition_Failure with "null unit argument";
      end if;

      return First_Token (Unwrap_Unit (Unit));
   end First_Token;

   ----------------
   -- Last_Token --
   ----------------

   function Last_Token (Unit : Analysis_Unit'Class) return Token_Reference is
   begin
      if Unit.Internal = null then
         raise Precondition_Failure with "null unit argument";
      end if;

      return Last_Token (Unwrap_Unit (Unit));
   end Last_Token;

   -----------------
   -- Token_Count --
   -----------------

   function Token_Count (Unit : Analysis_Unit'Class) return Natural is
   begin
      if Unit.Internal = null then
         raise Precondition_Failure with "null unit argument";
      end if;

      return Token_Count (Unwrap_Unit (Unit));
   end Token_Count;

   ------------------
   -- Trivia_Count --
   ------------------

   function Trivia_Count (Unit : Analysis_Unit'Class) return Natural is
   begin
      if Unit.Internal = null then
         raise Precondition_Failure with "null unit argument";
      end if;

      return Trivia_Count (Unwrap_Unit (Unit));
   end Trivia_Count;

   ----------
   -- Unit --
   ----------

   function Unit (Token : Token_Reference) return Analysis_Unit is
   begin
      return Wrap_Unit (Get_Token_Unit (Token));
   end Unit;

   ----------
   -- Text --
   ----------

   function Text (Unit : Analysis_Unit'Class) return Text_Type is
   begin
      if Unit.Internal = null then
         raise Precondition_Failure with "null unit argument";
      end if;

      return Implementation.Text (Unwrap_Unit (Unit));
   end Text;

   ------------------
   -- Lookup_Token --
   ------------------

   function Lookup_Token
     (Unit : Analysis_Unit'Class; Sloc : Source_Location)
      return Token_Reference is
   begin
      if Unit.Internal = null then
         raise Precondition_Failure with "null unit argument";
      end if;

      return Lookup_Token (Unwrap_Unit (Unit), Sloc);
   end Lookup_Token;

   --------------
   -- Get_Line --
   --------------

   function Get_Line
     (Unit : Analysis_Unit; Line_Number : Positive) return Text_Type is
   begin
      if Unit.Internal = null then
         raise Precondition_Failure with "null unit argument";
      end if;

      return Get_Line (Unwrap_Unit (Unit), Line_Number);
   end Get_Line;

   ----------------------
   -- Dump_Lexical_Env --
   ----------------------

   procedure Dump_Lexical_Env (Unit : Analysis_Unit'Class) is
   begin
      if Unit.Internal = null then
         raise Precondition_Failure with "null unit argument";
      end if;

      Dump_Lexical_Env (Unwrap_Unit (Unit));
   end Dump_Lexical_Env;

   ------------------------
   -- Trigger_Envs_Debug --
   ------------------------

   procedure Trigger_Envs_Debug (Is_Active : Boolean) is
   begin
      Langkit_Support.Lexical_Envs.Me.Set_Active (Is_Active);
   end Trigger_Envs_Debug;

   -----------
   -- Print --
   -----------

   procedure Print (Unit : Analysis_Unit'Class; Show_Slocs : Boolean := True)
   is
   begin
      if Unit.Internal = null then
         raise Precondition_Failure with "null unit argument";
      end if;

      Print (Unwrap_Unit (Unit), Show_Slocs);
   end Print;

   ---------------
   -- PP_Trivia --
   ---------------

   procedure PP_Trivia (Unit : Analysis_Unit'Class) is
   begin
      if Unit.Internal = null then
         raise Precondition_Failure with "null unit argument";
      end if;

      PP_Trivia (Unwrap_Unit (Unit));
   end PP_Trivia;

   -------------
   -- Is_Null --
   -------------

   function Is_Null (Node : Ada_Node'Class) return Boolean is
     (Node.Internal.Node = null);

   -------------------
   -- Is_Token_Node --
   -------------------

   function Is_Token_Node (Node : Ada_Node'Class) return Boolean
   is
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      return Is_Token_Node (Node.Internal.Node);
   end Is_Token_Node;

   ------------------
   -- Is_Synthetic --
   ------------------

   function Is_Synthetic (Node : Ada_Node'Class) return Boolean
   is
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      return Is_Synthetic (Node.Internal.Node);
   end Is_Synthetic;

   ---------
   -- "=" --
   ---------

   function "=" (L, R : Ada_Node'Class) return Boolean is
   begin
      Check_Safety_Net (L);
      Check_Safety_Net (R);
      return Compare_Entity (L.Internal, R.Internal);
   end "=";

   -----------
   -- Image --
   -----------

   function Image (Node : Ada_Node'Class) return String is
   begin
      Check_Safety_Net (Node);
      return Image (Node.Internal);
   end Image;

   -----------------------
   -- Entity converters --
   -----------------------

      function As_Ada_Node
        (Node : Ada_Node'Class) return Ada_Node
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Ada_Node;
         end if;

         Check_Safety_Net (Node);

         
         

            
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         

      end;
      function As_Expr
        (Node : Ada_Node'Class) return Expr
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Expr;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Expr then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to Expr";
         
            end if;
      end;
      function As_Basic_Decl
        (Node : Ada_Node'Class) return Basic_Decl
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Basic_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Basic_Decl then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to BasicDecl";
         
            end if;
      end;
      function As_Ada_List
        (Node : Ada_Node'Class) return Ada_List
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Ada_List;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Ada_List then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to AdaList";
         
            end if;
      end;
      function As_Ada_Node_List
        (Node : Ada_Node'Class) return Ada_Node_List
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Ada_Node_List;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Ada_Node_List_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to AdaNode.list";
         
            end if;
      end;
      function As_Alternatives_List
        (Node : Ada_Node'Class) return Alternatives_List
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Alternatives_List;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Alternatives_List_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to AlternativesList";
         
            end if;
      end;
      function As_Name
        (Node : Ada_Node'Class) return Name
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Name;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Name then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to Name";
         
            end if;
      end;
      function As_Single_Tok_Node
        (Node : Ada_Node'Class) return Single_Tok_Node
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Single_Tok_Node;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Single_Tok_Node then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to SingleTokNode";
         
            end if;
      end;
      function As_Base_Id
        (Node : Ada_Node'Class) return Base_Id
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Base_Id;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Base_Id then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to BaseId";
         
            end if;
      end;
      function As_Identifier
        (Node : Ada_Node'Class) return Identifier
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Identifier;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Identifier_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to Identifier";
         
            end if;
      end;
      function As_Abort_Node
        (Node : Ada_Node'Class) return Abort_Node
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Abort_Node;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Abort_Node then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to Abort";
         
            end if;
      end;
      function As_Abort_Absent
        (Node : Ada_Node'Class) return Abort_Absent
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Abort_Absent;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Abort_Absent_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to Abort.Absent";
         
            end if;
      end;
      function As_Abort_Present
        (Node : Ada_Node'Class) return Abort_Present
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Abort_Present;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Abort_Present_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to Abort.Present";
         
            end if;
      end;
      function As_Stmt
        (Node : Ada_Node'Class) return Stmt
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Stmt;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Stmt then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to Stmt";
         
            end if;
      end;
      function As_Simple_Stmt
        (Node : Ada_Node'Class) return Simple_Stmt
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Simple_Stmt;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Simple_Stmt then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to SimpleStmt";
         
            end if;
      end;
      function As_Abort_Stmt
        (Node : Ada_Node'Class) return Abort_Stmt
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Abort_Stmt;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Abort_Stmt_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to AbortStmt";
         
            end if;
      end;
      function As_Abstract_Node
        (Node : Ada_Node'Class) return Abstract_Node
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Abstract_Node;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Abstract_Node then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to Abstract";
         
            end if;
      end;
      function As_Abstract_Absent
        (Node : Ada_Node'Class) return Abstract_Absent
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Abstract_Absent;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Abstract_Absent_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to Abstract.Absent";
         
            end if;
      end;
      function As_Basic_Subp_Decl
        (Node : Ada_Node'Class) return Basic_Subp_Decl
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Basic_Subp_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Basic_Subp_Decl then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to BasicSubpDecl";
         
            end if;
      end;
      function As_Classic_Subp_Decl
        (Node : Ada_Node'Class) return Classic_Subp_Decl
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Classic_Subp_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Classic_Subp_Decl then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to ClassicSubpDecl";
         
            end if;
      end;
      function As_Formal_Subp_Decl
        (Node : Ada_Node'Class) return Formal_Subp_Decl
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Formal_Subp_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Formal_Subp_Decl then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to FormalSubpDecl";
         
            end if;
      end;
      function As_Abstract_Formal_Subp_Decl
        (Node : Ada_Node'Class) return Abstract_Formal_Subp_Decl
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Abstract_Formal_Subp_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Abstract_Formal_Subp_Decl_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to AbstractFormalSubpDecl";
         
            end if;
      end;
      function As_Abstract_Present
        (Node : Ada_Node'Class) return Abstract_Present
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Abstract_Present;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Abstract_Present_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to Abstract.Present";
         
            end if;
      end;
      function As_Abstract_State_Decl
        (Node : Ada_Node'Class) return Abstract_State_Decl
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Abstract_State_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Abstract_State_Decl_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to AbstractStateDecl";
         
            end if;
      end;
      function As_Abstract_State_Decl_Expr
        (Node : Ada_Node'Class) return Abstract_State_Decl_Expr
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Abstract_State_Decl_Expr;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Abstract_State_Decl_Expr_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to AbstractStateDeclExpr";
         
            end if;
      end;
      function As_Abstract_State_Decl_List
        (Node : Ada_Node'Class) return Abstract_State_Decl_List
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Abstract_State_Decl_List;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Abstract_State_Decl_List_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to AbstractStateDeclList";
         
            end if;
      end;
      function As_Abstract_Subp_Decl
        (Node : Ada_Node'Class) return Abstract_Subp_Decl
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Abstract_Subp_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Abstract_Subp_Decl_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to AbstractSubpDecl";
         
            end if;
      end;
      function As_Composite_Stmt
        (Node : Ada_Node'Class) return Composite_Stmt
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Composite_Stmt;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Composite_Stmt then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to CompositeStmt";
         
            end if;
      end;
      function As_Accept_Stmt
        (Node : Ada_Node'Class) return Accept_Stmt
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Accept_Stmt;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Accept_Stmt_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to AcceptStmt";
         
            end if;
      end;
      function As_Accept_Stmt_With_Stmts
        (Node : Ada_Node'Class) return Accept_Stmt_With_Stmts
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Accept_Stmt_With_Stmts;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Accept_Stmt_With_Stmts_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to AcceptStmtWithStmts";
         
            end if;
      end;
      function As_Type_Def
        (Node : Ada_Node'Class) return Type_Def
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Type_Def;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Type_Def then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to TypeDef";
         
            end if;
      end;
      function As_Access_Def
        (Node : Ada_Node'Class) return Access_Def
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Access_Def;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Access_Def then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to AccessDef";
         
            end if;
      end;
      function As_Access_To_Subp_Def
        (Node : Ada_Node'Class) return Access_To_Subp_Def
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Access_To_Subp_Def;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Access_To_Subp_Def_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to AccessToSubpDef";
         
            end if;
      end;
      function As_Base_Aggregate
        (Node : Ada_Node'Class) return Base_Aggregate
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Base_Aggregate;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Base_Aggregate then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to BaseAggregate";
         
            end if;
      end;
      function As_Aggregate
        (Node : Ada_Node'Class) return Aggregate
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Aggregate;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Aggregate_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to Aggregate";
         
            end if;
      end;
      function As_Basic_Assoc
        (Node : Ada_Node'Class) return Basic_Assoc
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Basic_Assoc;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Basic_Assoc then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to BasicAssoc";
         
            end if;
      end;
      function As_Aggregate_Assoc
        (Node : Ada_Node'Class) return Aggregate_Assoc
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Aggregate_Assoc;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Aggregate_Assoc_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to AggregateAssoc";
         
            end if;
      end;
      function As_Aliased_Node
        (Node : Ada_Node'Class) return Aliased_Node
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Aliased_Node;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Aliased_Node then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to Aliased";
         
            end if;
      end;
      function As_Aliased_Absent
        (Node : Ada_Node'Class) return Aliased_Absent
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Aliased_Absent;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Aliased_Absent_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to Aliased.Absent";
         
            end if;
      end;
      function As_Aliased_Present
        (Node : Ada_Node'Class) return Aliased_Present
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Aliased_Present;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Aliased_Present_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to Aliased.Present";
         
            end if;
      end;
      function As_All_Node
        (Node : Ada_Node'Class) return All_Node
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_All_Node;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_All_Node then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to All";
         
            end if;
      end;
      function As_All_Absent
        (Node : Ada_Node'Class) return All_Absent
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_All_Absent;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_All_Absent_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to All.Absent";
         
            end if;
      end;
      function As_All_Present
        (Node : Ada_Node'Class) return All_Present
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_All_Present;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_All_Present_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to All.Present";
         
            end if;
      end;
      function As_Allocator
        (Node : Ada_Node'Class) return Allocator
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Allocator;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Allocator_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to Allocator";
         
            end if;
      end;
      function As_Anonymous_Expr_Decl
        (Node : Ada_Node'Class) return Anonymous_Expr_Decl
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Anonymous_Expr_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Anonymous_Expr_Decl_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to AnonymousExprDecl";
         
            end if;
      end;
      function As_Type_Expr
        (Node : Ada_Node'Class) return Type_Expr
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Type_Expr;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Type_Expr then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to TypeExpr";
         
            end if;
      end;
      function As_Anonymous_Type
        (Node : Ada_Node'Class) return Anonymous_Type
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Anonymous_Type;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Anonymous_Type_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to AnonymousType";
         
            end if;
      end;
      function As_Base_Type_Access_Def
        (Node : Ada_Node'Class) return Base_Type_Access_Def
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Base_Type_Access_Def;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Base_Type_Access_Def then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to BaseTypeAccessDef";
         
            end if;
      end;
      function As_Anonymous_Type_Access_Def
        (Node : Ada_Node'Class) return Anonymous_Type_Access_Def
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Anonymous_Type_Access_Def;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Anonymous_Type_Access_Def_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to AnonymousTypeAccessDef";
         
            end if;
      end;
      function As_Base_Type_Decl
        (Node : Ada_Node'Class) return Base_Type_Decl
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Base_Type_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Base_Type_Decl then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to BaseTypeDecl";
         
            end if;
      end;
      function As_Type_Decl
        (Node : Ada_Node'Class) return Type_Decl
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Type_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Type_Decl then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to TypeDecl";
         
            end if;
      end;
      function As_Anonymous_Type_Decl
        (Node : Ada_Node'Class) return Anonymous_Type_Decl
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Anonymous_Type_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Anonymous_Type_Decl_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to AnonymousTypeDecl";
         
            end if;
      end;
      function As_Array_Indices
        (Node : Ada_Node'Class) return Array_Indices
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Array_Indices;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Array_Indices then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to ArrayIndices";
         
            end if;
      end;
      function As_Array_Type_Def
        (Node : Ada_Node'Class) return Array_Type_Def
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Array_Type_Def;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Array_Type_Def_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to ArrayTypeDef";
         
            end if;
      end;
      function As_Aspect_Assoc
        (Node : Ada_Node'Class) return Aspect_Assoc
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Aspect_Assoc;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Aspect_Assoc_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to AspectAssoc";
         
            end if;
      end;
      function As_Aspect_Assoc_List
        (Node : Ada_Node'Class) return Aspect_Assoc_List
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Aspect_Assoc_List;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Aspect_Assoc_List_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to AspectAssoc.list";
         
            end if;
      end;
      function As_Aspect_Clause
        (Node : Ada_Node'Class) return Aspect_Clause
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Aspect_Clause;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Aspect_Clause then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to AspectClause";
         
            end if;
      end;
      function As_Aspect_Spec
        (Node : Ada_Node'Class) return Aspect_Spec
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Aspect_Spec;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Aspect_Spec_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to AspectSpec";
         
            end if;
      end;
      function As_Assign_Stmt
        (Node : Ada_Node'Class) return Assign_Stmt
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Assign_Stmt;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Assign_Stmt_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to AssignStmt";
         
            end if;
      end;
      function As_Basic_Assoc_List
        (Node : Ada_Node'Class) return Basic_Assoc_List
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Basic_Assoc_List;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Basic_Assoc_List then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to BasicAssoc.list";
         
            end if;
      end;
      function As_Assoc_List
        (Node : Ada_Node'Class) return Assoc_List
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Assoc_List;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Assoc_List_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to AssocList";
         
            end if;
      end;
      function As_At_Clause
        (Node : Ada_Node'Class) return At_Clause
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_At_Clause;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_At_Clause_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to AtClause";
         
            end if;
      end;
      function As_Attribute_Def_Clause
        (Node : Ada_Node'Class) return Attribute_Def_Clause
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Attribute_Def_Clause;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Attribute_Def_Clause_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to AttributeDefClause";
         
            end if;
      end;
      function As_Attribute_Ref
        (Node : Ada_Node'Class) return Attribute_Ref
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Attribute_Ref;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Attribute_Ref_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to AttributeRef";
         
            end if;
      end;
      function As_Base_Assoc
        (Node : Ada_Node'Class) return Base_Assoc
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Base_Assoc;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Base_Assoc then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to BaseAssoc";
         
            end if;
      end;
      function As_Base_Assoc_List
        (Node : Ada_Node'Class) return Base_Assoc_List
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Base_Assoc_List;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Base_Assoc_List_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to BaseAssoc.list";
         
            end if;
      end;
      function As_Base_Formal_Param_Decl
        (Node : Ada_Node'Class) return Base_Formal_Param_Decl
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Base_Formal_Param_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Base_Formal_Param_Decl then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to BaseFormalParamDecl";
         
            end if;
      end;
      function As_Base_Formal_Param_Holder
        (Node : Ada_Node'Class) return Base_Formal_Param_Holder
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Base_Formal_Param_Holder;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Base_Formal_Param_Holder then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to BaseFormalParamHolder";
         
            end if;
      end;
      function As_Base_Loop_Stmt
        (Node : Ada_Node'Class) return Base_Loop_Stmt
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Base_Loop_Stmt;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Base_Loop_Stmt then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to BaseLoopStmt";
         
            end if;
      end;
      function As_Base_Package_Decl
        (Node : Ada_Node'Class) return Base_Package_Decl
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Base_Package_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Base_Package_Decl then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to BasePackageDecl";
         
            end if;
      end;
      function As_Base_Record_Def
        (Node : Ada_Node'Class) return Base_Record_Def
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Base_Record_Def;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Base_Record_Def then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to BaseRecordDef";
         
            end if;
      end;
      function As_Body_Node
        (Node : Ada_Node'Class) return Body_Node
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Body_Node;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Body_Node then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to Body";
         
            end if;
      end;
      function As_Base_Subp_Body
        (Node : Ada_Node'Class) return Base_Subp_Body
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Base_Subp_Body;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Base_Subp_Body then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to BaseSubpBody";
         
            end if;
      end;
      function As_Base_Subp_Spec
        (Node : Ada_Node'Class) return Base_Subp_Spec
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Base_Subp_Spec;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Base_Subp_Spec then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to BaseSubpSpec";
         
            end if;
      end;
      function As_Base_Subtype_Decl
        (Node : Ada_Node'Class) return Base_Subtype_Decl
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Base_Subtype_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Base_Subtype_Decl then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to BaseSubtypeDecl";
         
            end if;
      end;
      function As_Basic_Decl_List
        (Node : Ada_Node'Class) return Basic_Decl_List
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Basic_Decl_List;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Basic_Decl_List_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to BasicDecl.list";
         
            end if;
      end;
      function As_Block_Stmt
        (Node : Ada_Node'Class) return Block_Stmt
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Block_Stmt;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Block_Stmt then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to BlockStmt";
         
            end if;
      end;
      function As_Begin_Block
        (Node : Ada_Node'Class) return Begin_Block
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Begin_Block;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Begin_Block_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to BeginBlock";
         
            end if;
      end;
      function As_Bin_Op
        (Node : Ada_Node'Class) return Bin_Op
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Bin_Op;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Bin_Op_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to BinOp";
         
            end if;
      end;
      function As_Body_Stub
        (Node : Ada_Node'Class) return Body_Stub
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Body_Stub;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Body_Stub then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to BodyStub";
         
            end if;
      end;
      function As_Box_Expr
        (Node : Ada_Node'Class) return Box_Expr
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Box_Expr;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Box_Expr_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to BoxExpr";
         
            end if;
      end;
      function As_Bracket_Aggregate
        (Node : Ada_Node'Class) return Bracket_Aggregate
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Bracket_Aggregate;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Bracket_Aggregate_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to BracketAggregate";
         
            end if;
      end;
      function As_Delta_Aggregate
        (Node : Ada_Node'Class) return Delta_Aggregate
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Delta_Aggregate;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Delta_Aggregate_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to DeltaAggregate";
         
            end if;
      end;
      function As_Bracket_Delta_Aggregate
        (Node : Ada_Node'Class) return Bracket_Delta_Aggregate
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Bracket_Delta_Aggregate;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Bracket_Delta_Aggregate_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to BracketDeltaAggregate";
         
            end if;
      end;
      function As_Call_Expr
        (Node : Ada_Node'Class) return Call_Expr
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Call_Expr;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Call_Expr_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to CallExpr";
         
            end if;
      end;
      function As_Call_Stmt
        (Node : Ada_Node'Class) return Call_Stmt
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Call_Stmt;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Call_Stmt_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to CallStmt";
         
            end if;
      end;
      function As_Cond_Expr
        (Node : Ada_Node'Class) return Cond_Expr
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Cond_Expr;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Cond_Expr then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to CondExpr";
         
            end if;
      end;
      function As_Case_Expr
        (Node : Ada_Node'Class) return Case_Expr
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Case_Expr;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Case_Expr_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to CaseExpr";
         
            end if;
      end;
      function As_Case_Expr_Alternative
        (Node : Ada_Node'Class) return Case_Expr_Alternative
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Case_Expr_Alternative;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Case_Expr_Alternative_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to CaseExprAlternative";
         
            end if;
      end;
      function As_Case_Expr_Alternative_List
        (Node : Ada_Node'Class) return Case_Expr_Alternative_List
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Case_Expr_Alternative_List;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Case_Expr_Alternative_List_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to CaseExprAlternative.list";
         
            end if;
      end;
      function As_Case_Stmt
        (Node : Ada_Node'Class) return Case_Stmt
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Case_Stmt;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Case_Stmt_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to CaseStmt";
         
            end if;
      end;
      function As_Case_Stmt_Alternative
        (Node : Ada_Node'Class) return Case_Stmt_Alternative
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Case_Stmt_Alternative;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Case_Stmt_Alternative_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to CaseStmtAlternative";
         
            end if;
      end;
      function As_Case_Stmt_Alternative_List
        (Node : Ada_Node'Class) return Case_Stmt_Alternative_List
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Case_Stmt_Alternative_List;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Case_Stmt_Alternative_List_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to CaseStmtAlternative.list";
         
            end if;
      end;
      function As_Char_Literal
        (Node : Ada_Node'Class) return Char_Literal
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Char_Literal;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Char_Literal_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to CharLiteral";
         
            end if;
      end;
      function As_Classwide_Type_Decl
        (Node : Ada_Node'Class) return Classwide_Type_Decl
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Classwide_Type_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Classwide_Type_Decl_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to ClasswideTypeDecl";
         
            end if;
      end;
      function As_Compilation_Unit
        (Node : Ada_Node'Class) return Compilation_Unit
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Compilation_Unit;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Compilation_Unit_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to CompilationUnit";
         
            end if;
      end;
      function As_Compilation_Unit_List
        (Node : Ada_Node'Class) return Compilation_Unit_List
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Compilation_Unit_List;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Compilation_Unit_List_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to CompilationUnit.list";
         
            end if;
      end;
      function As_Component_Clause
        (Node : Ada_Node'Class) return Component_Clause
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Component_Clause;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Component_Clause_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to ComponentClause";
         
            end if;
      end;
      function As_Component_Decl
        (Node : Ada_Node'Class) return Component_Decl
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Component_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Component_Decl_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to ComponentDecl";
         
            end if;
      end;
      function As_Component_Def
        (Node : Ada_Node'Class) return Component_Def
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Component_Def;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Component_Def_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to ComponentDef";
         
            end if;
      end;
      function As_Component_List
        (Node : Ada_Node'Class) return Component_List
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Component_List;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Component_List_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to ComponentList";
         
            end if;
      end;
      function As_Constraint
        (Node : Ada_Node'Class) return Constraint
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Constraint;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Constraint then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to Constraint";
         
            end if;
      end;
      function As_Composite_Constraint
        (Node : Ada_Node'Class) return Composite_Constraint
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Composite_Constraint;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Composite_Constraint_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to CompositeConstraint";
         
            end if;
      end;
      function As_Composite_Constraint_Assoc
        (Node : Ada_Node'Class) return Composite_Constraint_Assoc
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Composite_Constraint_Assoc;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Composite_Constraint_Assoc_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to CompositeConstraintAssoc";
         
            end if;
      end;
      function As_Concat_Op
        (Node : Ada_Node'Class) return Concat_Op
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Concat_Op;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Concat_Op_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to ConcatOp";
         
            end if;
      end;
      function As_Concat_Operand
        (Node : Ada_Node'Class) return Concat_Operand
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Concat_Operand;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Concat_Operand_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to ConcatOperand";
         
            end if;
      end;
      function As_Concat_Operand_List
        (Node : Ada_Node'Class) return Concat_Operand_List
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Concat_Operand_List;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Concat_Operand_List_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to ConcatOperand.list";
         
            end if;
      end;
      function As_Concrete_Formal_Subp_Decl
        (Node : Ada_Node'Class) return Concrete_Formal_Subp_Decl
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Concrete_Formal_Subp_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Concrete_Formal_Subp_Decl_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to ConcreteFormalSubpDecl";
         
            end if;
      end;
      function As_Concrete_Type_Decl
        (Node : Ada_Node'Class) return Concrete_Type_Decl
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Concrete_Type_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Concrete_Type_Decl_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to ConcreteTypeDecl";
         
            end if;
      end;
      function As_Constant_Node
        (Node : Ada_Node'Class) return Constant_Node
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Constant_Node;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Constant_Node then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to Constant";
         
            end if;
      end;
      function As_Constant_Absent
        (Node : Ada_Node'Class) return Constant_Absent
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Constant_Absent;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Constant_Absent_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to Constant.Absent";
         
            end if;
      end;
      function As_Constant_Present
        (Node : Ada_Node'Class) return Constant_Present
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Constant_Present;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Constant_Present_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to Constant.Present";
         
            end if;
      end;
      function As_Constrained_Array_Indices
        (Node : Ada_Node'Class) return Constrained_Array_Indices
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Constrained_Array_Indices;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Constrained_Array_Indices_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to ConstrainedArrayIndices";
         
            end if;
      end;
      function As_Subtype_Indication
        (Node : Ada_Node'Class) return Subtype_Indication
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Subtype_Indication;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Subtype_Indication_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to SubtypeIndication";
         
            end if;
      end;
      function As_Constrained_Subtype_Indication
        (Node : Ada_Node'Class) return Constrained_Subtype_Indication
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Constrained_Subtype_Indication;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Constrained_Subtype_Indication_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to ConstrainedSubtypeIndication";
         
            end if;
      end;
      function As_Constraint_List
        (Node : Ada_Node'Class) return Constraint_List
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Constraint_List;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Constraint_List_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to ConstraintList";
         
            end if;
      end;
      function As_Contract_Case_Assoc
        (Node : Ada_Node'Class) return Contract_Case_Assoc
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Contract_Case_Assoc;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Contract_Case_Assoc_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to ContractCaseAssoc";
         
            end if;
      end;
      function As_Contract_Case_Assoc_List
        (Node : Ada_Node'Class) return Contract_Case_Assoc_List
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Contract_Case_Assoc_List;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Contract_Case_Assoc_List_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to ContractCaseAssoc.list";
         
            end if;
      end;
      function As_Contract_Cases
        (Node : Ada_Node'Class) return Contract_Cases
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Contract_Cases;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Contract_Cases_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to ContractCases";
         
            end if;
      end;
      function As_Real_Type_Def
        (Node : Ada_Node'Class) return Real_Type_Def
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Real_Type_Def;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Real_Type_Def then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to RealTypeDef";
         
            end if;
      end;
      function As_Decimal_Fixed_Point_Def
        (Node : Ada_Node'Class) return Decimal_Fixed_Point_Def
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Decimal_Fixed_Point_Def;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Decimal_Fixed_Point_Def_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to DecimalFixedPointDef";
         
            end if;
      end;
      function As_Decl_Block
        (Node : Ada_Node'Class) return Decl_Block
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Decl_Block;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Decl_Block_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to DeclBlock";
         
            end if;
      end;
      function As_Decl_Expr
        (Node : Ada_Node'Class) return Decl_Expr
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Decl_Expr;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Decl_Expr_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to DeclExpr";
         
            end if;
      end;
      function As_Decl_List
        (Node : Ada_Node'Class) return Decl_List
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Decl_List;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Decl_List_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to DeclList";
         
            end if;
      end;
      function As_Declarative_Part
        (Node : Ada_Node'Class) return Declarative_Part
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Declarative_Part;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Declarative_Part_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to DeclarativePart";
         
            end if;
      end;
      function As_Defining_Name
        (Node : Ada_Node'Class) return Defining_Name
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Defining_Name;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Defining_Name_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to DefiningName";
         
            end if;
      end;
      function As_Defining_Name_List
        (Node : Ada_Node'Class) return Defining_Name_List
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Defining_Name_List;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Defining_Name_List_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to DefiningName.list";
         
            end if;
      end;
      function As_Delay_Stmt
        (Node : Ada_Node'Class) return Delay_Stmt
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Delay_Stmt;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Delay_Stmt_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to DelayStmt";
         
            end if;
      end;
      function As_Delta_Constraint
        (Node : Ada_Node'Class) return Delta_Constraint
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Delta_Constraint;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Delta_Constraint_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to DeltaConstraint";
         
            end if;
      end;
      function As_Derived_Type_Def
        (Node : Ada_Node'Class) return Derived_Type_Def
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Derived_Type_Def;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Derived_Type_Def_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to DerivedTypeDef";
         
            end if;
      end;
      function As_Digits_Constraint
        (Node : Ada_Node'Class) return Digits_Constraint
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Digits_Constraint;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Digits_Constraint_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to DigitsConstraint";
         
            end if;
      end;
      function As_Discrete_Base_Subtype_Decl
        (Node : Ada_Node'Class) return Discrete_Base_Subtype_Decl
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Discrete_Base_Subtype_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Discrete_Base_Subtype_Decl_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to DiscreteBaseSubtypeDecl";
         
            end if;
      end;
      function As_Discrete_Subtype_Indication
        (Node : Ada_Node'Class) return Discrete_Subtype_Indication
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Discrete_Subtype_Indication;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Discrete_Subtype_Indication_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to DiscreteSubtypeIndication";
         
            end if;
      end;
      function As_Discrete_Subtype_Name
        (Node : Ada_Node'Class) return Discrete_Subtype_Name
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Discrete_Subtype_Name;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Discrete_Subtype_Name_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to DiscreteSubtypeName";
         
            end if;
      end;
      function As_Identifier_List
        (Node : Ada_Node'Class) return Identifier_List
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Identifier_List;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Identifier_List then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to Identifier.list";
         
            end if;
      end;
      function As_Discriminant_Choice_List
        (Node : Ada_Node'Class) return Discriminant_Choice_List
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Discriminant_Choice_List;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Discriminant_Choice_List_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to DiscriminantChoiceList";
         
            end if;
      end;
      function As_Discriminant_Part
        (Node : Ada_Node'Class) return Discriminant_Part
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Discriminant_Part;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Discriminant_Part then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to DiscriminantPart";
         
            end if;
      end;
      function As_Discriminant_Spec
        (Node : Ada_Node'Class) return Discriminant_Spec
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Discriminant_Spec;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Discriminant_Spec_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to DiscriminantSpec";
         
            end if;
      end;
      function As_Discriminant_Spec_List
        (Node : Ada_Node'Class) return Discriminant_Spec_List
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Discriminant_Spec_List;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Discriminant_Spec_List_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to DiscriminantSpec.list";
         
            end if;
      end;
      function As_Dotted_Name
        (Node : Ada_Node'Class) return Dotted_Name
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Dotted_Name;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Dotted_Name_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to DottedName";
         
            end if;
      end;
      function As_Elsif_Expr_Part
        (Node : Ada_Node'Class) return Elsif_Expr_Part
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Elsif_Expr_Part;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Elsif_Expr_Part_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to ElsifExprPart";
         
            end if;
      end;
      function As_Elsif_Expr_Part_List
        (Node : Ada_Node'Class) return Elsif_Expr_Part_List
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Elsif_Expr_Part_List;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Elsif_Expr_Part_List_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to ElsifExprPart.list";
         
            end if;
      end;
      function As_Elsif_Stmt_Part
        (Node : Ada_Node'Class) return Elsif_Stmt_Part
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Elsif_Stmt_Part;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Elsif_Stmt_Part_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to ElsifStmtPart";
         
            end if;
      end;
      function As_Elsif_Stmt_Part_List
        (Node : Ada_Node'Class) return Elsif_Stmt_Part_List
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Elsif_Stmt_Part_List;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Elsif_Stmt_Part_List_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to ElsifStmtPart.list";
         
            end if;
      end;
      function As_End_Name
        (Node : Ada_Node'Class) return End_Name
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_End_Name;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_End_Name_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to EndName";
         
            end if;
      end;
      function As_Entry_Body
        (Node : Ada_Node'Class) return Entry_Body
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Entry_Body;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Entry_Body_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to EntryBody";
         
            end if;
      end;
      function As_Entry_Completion_Formal_Params
        (Node : Ada_Node'Class) return Entry_Completion_Formal_Params
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Entry_Completion_Formal_Params;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Entry_Completion_Formal_Params_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to EntryCompletionFormalParams";
         
            end if;
      end;
      function As_Entry_Decl
        (Node : Ada_Node'Class) return Entry_Decl
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Entry_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Entry_Decl_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to EntryDecl";
         
            end if;
      end;
      function As_Entry_Index_Spec
        (Node : Ada_Node'Class) return Entry_Index_Spec
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Entry_Index_Spec;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Entry_Index_Spec_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to EntryIndexSpec";
         
            end if;
      end;
      function As_Entry_Spec
        (Node : Ada_Node'Class) return Entry_Spec
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Entry_Spec;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Entry_Spec_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to EntrySpec";
         
            end if;
      end;
      function As_Enum_Lit_Synth_Type_Expr
        (Node : Ada_Node'Class) return Enum_Lit_Synth_Type_Expr
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Enum_Lit_Synth_Type_Expr;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Enum_Lit_Synth_Type_Expr_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to EnumLitSynthTypeExpr";
         
            end if;
      end;
      function As_Enum_Literal_Decl
        (Node : Ada_Node'Class) return Enum_Literal_Decl
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Enum_Literal_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Enum_Literal_Decl_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to EnumLiteralDecl";
         
            end if;
      end;
      function As_Enum_Literal_Decl_List
        (Node : Ada_Node'Class) return Enum_Literal_Decl_List
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Enum_Literal_Decl_List;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Enum_Literal_Decl_List_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to EnumLiteralDecl.list";
         
            end if;
      end;
      function As_Enum_Rep_Clause
        (Node : Ada_Node'Class) return Enum_Rep_Clause
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Enum_Rep_Clause;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Enum_Rep_Clause_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to EnumRepClause";
         
            end if;
      end;
      function As_Enum_Subp_Spec
        (Node : Ada_Node'Class) return Enum_Subp_Spec
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Enum_Subp_Spec;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Enum_Subp_Spec_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to EnumSubpSpec";
         
            end if;
      end;
      function As_Enum_Type_Def
        (Node : Ada_Node'Class) return Enum_Type_Def
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Enum_Type_Def;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Enum_Type_Def_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to EnumTypeDef";
         
            end if;
      end;
      function As_Error_Decl
        (Node : Ada_Node'Class) return Error_Decl
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Error_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Error_Decl_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to ErrorDecl";
         
            end if;
      end;
      function As_Error_Stmt
        (Node : Ada_Node'Class) return Error_Stmt
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Error_Stmt;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Error_Stmt_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to ErrorStmt";
         
            end if;
      end;
      function As_Exception_Decl
        (Node : Ada_Node'Class) return Exception_Decl
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Exception_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Exception_Decl_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to ExceptionDecl";
         
            end if;
      end;
      function As_Exception_Handler
        (Node : Ada_Node'Class) return Exception_Handler
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Exception_Handler;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Exception_Handler_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to ExceptionHandler";
         
            end if;
      end;
      function As_Exit_Stmt
        (Node : Ada_Node'Class) return Exit_Stmt
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Exit_Stmt;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Exit_Stmt_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to ExitStmt";
         
            end if;
      end;
      function As_Explicit_Deref
        (Node : Ada_Node'Class) return Explicit_Deref
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Explicit_Deref;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Explicit_Deref_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to ExplicitDeref";
         
            end if;
      end;
      function As_Expr_List
        (Node : Ada_Node'Class) return Expr_List
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Expr_List;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Expr_List then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to Expr.list";
         
            end if;
      end;
      function As_Expr_Alternatives_List
        (Node : Ada_Node'Class) return Expr_Alternatives_List
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Expr_Alternatives_List;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Expr_Alternatives_List_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to ExprAlternativesList";
         
            end if;
      end;
      function As_Expr_Function
        (Node : Ada_Node'Class) return Expr_Function
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Expr_Function;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Expr_Function_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to ExprFunction";
         
            end if;
      end;
      function As_Extended_Return_Stmt
        (Node : Ada_Node'Class) return Extended_Return_Stmt
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Extended_Return_Stmt;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Extended_Return_Stmt_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to ExtendedReturnStmt";
         
            end if;
      end;
      function As_Object_Decl
        (Node : Ada_Node'Class) return Object_Decl
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Object_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Object_Decl_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to ObjectDecl";
         
            end if;
      end;
      function As_Extended_Return_Stmt_Object_Decl
        (Node : Ada_Node'Class) return Extended_Return_Stmt_Object_Decl
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Extended_Return_Stmt_Object_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Extended_Return_Stmt_Object_Decl_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to ExtendedReturnStmtObjectDecl";
         
            end if;
      end;
      function As_Floating_Point_Def
        (Node : Ada_Node'Class) return Floating_Point_Def
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Floating_Point_Def;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Floating_Point_Def_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to FloatingPointDef";
         
            end if;
      end;
      function As_Loop_Spec
        (Node : Ada_Node'Class) return Loop_Spec
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Loop_Spec;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Loop_Spec then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to LoopSpec";
         
            end if;
      end;
      function As_For_Loop_Spec
        (Node : Ada_Node'Class) return For_Loop_Spec
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_For_Loop_Spec;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_For_Loop_Spec_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to ForLoopSpec";
         
            end if;
      end;
      function As_For_Loop_Stmt
        (Node : Ada_Node'Class) return For_Loop_Stmt
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_For_Loop_Stmt;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_For_Loop_Stmt_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to ForLoopStmt";
         
            end if;
      end;
      function As_For_Loop_Var_Decl
        (Node : Ada_Node'Class) return For_Loop_Var_Decl
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_For_Loop_Var_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_For_Loop_Var_Decl_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to ForLoopVarDecl";
         
            end if;
      end;
      function As_Formal_Discrete_Type_Def
        (Node : Ada_Node'Class) return Formal_Discrete_Type_Def
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Formal_Discrete_Type_Def;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Formal_Discrete_Type_Def_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to FormalDiscreteTypeDef";
         
            end if;
      end;
      function As_Formal_Type_Decl
        (Node : Ada_Node'Class) return Formal_Type_Decl
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Formal_Type_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Formal_Type_Decl_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to FormalTypeDecl";
         
            end if;
      end;
      function As_Generic_Decl
        (Node : Ada_Node'Class) return Generic_Decl
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Generic_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Generic_Decl then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to GenericDecl";
         
            end if;
      end;
      function As_Generic_Formal
        (Node : Ada_Node'Class) return Generic_Formal
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Generic_Formal;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Generic_Formal then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to GenericFormal";
         
            end if;
      end;
      function As_Generic_Formal_Obj_Decl
        (Node : Ada_Node'Class) return Generic_Formal_Obj_Decl
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Generic_Formal_Obj_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Generic_Formal_Obj_Decl_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to GenericFormalObjDecl";
         
            end if;
      end;
      function As_Generic_Formal_Package
        (Node : Ada_Node'Class) return Generic_Formal_Package
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Generic_Formal_Package;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Generic_Formal_Package_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to GenericFormalPackage";
         
            end if;
      end;
      function As_Generic_Formal_Part
        (Node : Ada_Node'Class) return Generic_Formal_Part
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Generic_Formal_Part;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Generic_Formal_Part_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to GenericFormalPart";
         
            end if;
      end;
      function As_Generic_Formal_Subp_Decl
        (Node : Ada_Node'Class) return Generic_Formal_Subp_Decl
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Generic_Formal_Subp_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Generic_Formal_Subp_Decl_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to GenericFormalSubpDecl";
         
            end if;
      end;
      function As_Generic_Formal_Type_Decl
        (Node : Ada_Node'Class) return Generic_Formal_Type_Decl
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Generic_Formal_Type_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Generic_Formal_Type_Decl_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to GenericFormalTypeDecl";
         
            end if;
      end;
      function As_Generic_Instantiation
        (Node : Ada_Node'Class) return Generic_Instantiation
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Generic_Instantiation;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Generic_Instantiation then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to GenericInstantiation";
         
            end if;
      end;
      function As_Generic_Package_Decl
        (Node : Ada_Node'Class) return Generic_Package_Decl
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Generic_Package_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Generic_Package_Decl_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to GenericPackageDecl";
         
            end if;
      end;
      function As_Generic_Package_Instantiation
        (Node : Ada_Node'Class) return Generic_Package_Instantiation
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Generic_Package_Instantiation;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Generic_Package_Instantiation_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to GenericPackageInstantiation";
         
            end if;
      end;
      function As_Generic_Package_Internal
        (Node : Ada_Node'Class) return Generic_Package_Internal
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Generic_Package_Internal;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Generic_Package_Internal_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to GenericPackageInternal";
         
            end if;
      end;
      function As_Generic_Renaming_Decl
        (Node : Ada_Node'Class) return Generic_Renaming_Decl
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Generic_Renaming_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Generic_Renaming_Decl then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to GenericRenamingDecl";
         
            end if;
      end;
      function As_Generic_Package_Renaming_Decl
        (Node : Ada_Node'Class) return Generic_Package_Renaming_Decl
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Generic_Package_Renaming_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Generic_Package_Renaming_Decl_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to GenericPackageRenamingDecl";
         
            end if;
      end;
      function As_Generic_Subp_Decl
        (Node : Ada_Node'Class) return Generic_Subp_Decl
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Generic_Subp_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Generic_Subp_Decl_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to GenericSubpDecl";
         
            end if;
      end;
      function As_Generic_Subp_Instantiation
        (Node : Ada_Node'Class) return Generic_Subp_Instantiation
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Generic_Subp_Instantiation;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Generic_Subp_Instantiation_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to GenericSubpInstantiation";
         
            end if;
      end;
      function As_Generic_Subp_Internal
        (Node : Ada_Node'Class) return Generic_Subp_Internal
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Generic_Subp_Internal;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Generic_Subp_Internal_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to GenericSubpInternal";
         
            end if;
      end;
      function As_Generic_Subp_Renaming_Decl
        (Node : Ada_Node'Class) return Generic_Subp_Renaming_Decl
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Generic_Subp_Renaming_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Generic_Subp_Renaming_Decl_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to GenericSubpRenamingDecl";
         
            end if;
      end;
      function As_Goto_Stmt
        (Node : Ada_Node'Class) return Goto_Stmt
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Goto_Stmt;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Goto_Stmt_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to GotoStmt";
         
            end if;
      end;
      function As_Handled_Stmts
        (Node : Ada_Node'Class) return Handled_Stmts
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Handled_Stmts;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Handled_Stmts_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to HandledStmts";
         
            end if;
      end;
      function As_If_Expr
        (Node : Ada_Node'Class) return If_Expr
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_If_Expr;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_If_Expr_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to IfExpr";
         
            end if;
      end;
      function As_If_Stmt
        (Node : Ada_Node'Class) return If_Stmt
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_If_Stmt;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_If_Stmt_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to IfStmt";
         
            end if;
      end;
      function As_Incomplete_Type_Decl
        (Node : Ada_Node'Class) return Incomplete_Type_Decl
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Incomplete_Type_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Incomplete_Type_Decl_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to IncompleteTypeDecl";
         
            end if;
      end;
      function As_Incomplete_Formal_Type_Decl
        (Node : Ada_Node'Class) return Incomplete_Formal_Type_Decl
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Incomplete_Formal_Type_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Incomplete_Formal_Type_Decl_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to IncompleteFormalTypeDecl";
         
            end if;
      end;
      function As_Incomplete_Tagged_Type_Decl
        (Node : Ada_Node'Class) return Incomplete_Tagged_Type_Decl
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Incomplete_Tagged_Type_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Incomplete_Tagged_Type_Decl_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to IncompleteTaggedTypeDecl";
         
            end if;
      end;
      function As_Num_Literal
        (Node : Ada_Node'Class) return Num_Literal
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Num_Literal;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Num_Literal then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to NumLiteral";
         
            end if;
      end;
      function As_Int_Literal
        (Node : Ada_Node'Class) return Int_Literal
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Int_Literal;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Int_Literal_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to IntLiteral";
         
            end if;
      end;
      function As_Interface_Kind
        (Node : Ada_Node'Class) return Interface_Kind
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Interface_Kind;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Interface_Kind then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to InterfaceKind";
         
            end if;
      end;
      function As_Interface_Kind_Limited
        (Node : Ada_Node'Class) return Interface_Kind_Limited
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Interface_Kind_Limited;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Interface_Kind_Limited_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to InterfaceKind.Limited";
         
            end if;
      end;
      function As_Interface_Kind_Protected
        (Node : Ada_Node'Class) return Interface_Kind_Protected
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Interface_Kind_Protected;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Interface_Kind_Protected_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to InterfaceKind.Protected";
         
            end if;
      end;
      function As_Interface_Kind_Synchronized
        (Node : Ada_Node'Class) return Interface_Kind_Synchronized
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Interface_Kind_Synchronized;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Interface_Kind_Synchronized_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to InterfaceKind.Synchronized";
         
            end if;
      end;
      function As_Interface_Kind_Task
        (Node : Ada_Node'Class) return Interface_Kind_Task
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Interface_Kind_Task;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Interface_Kind_Task_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to InterfaceKind.Task";
         
            end if;
      end;
      function As_Interface_Type_Def
        (Node : Ada_Node'Class) return Interface_Type_Def
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Interface_Type_Def;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Interface_Type_Def_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to InterfaceTypeDef";
         
            end if;
      end;
      function As_Iter_Type
        (Node : Ada_Node'Class) return Iter_Type
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Iter_Type;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Iter_Type then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to IterType";
         
            end if;
      end;
      function As_Iter_Type_In
        (Node : Ada_Node'Class) return Iter_Type_In
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Iter_Type_In;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Iter_Type_In_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to IterType.In";
         
            end if;
      end;
      function As_Iter_Type_Of
        (Node : Ada_Node'Class) return Iter_Type_Of
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Iter_Type_Of;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Iter_Type_Of_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to IterType.Of";
         
            end if;
      end;
      function As_Iterated_Assoc
        (Node : Ada_Node'Class) return Iterated_Assoc
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Iterated_Assoc;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Iterated_Assoc_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to IteratedAssoc";
         
            end if;
      end;
      function As_Known_Discriminant_Part
        (Node : Ada_Node'Class) return Known_Discriminant_Part
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Known_Discriminant_Part;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Known_Discriminant_Part_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to KnownDiscriminantPart";
         
            end if;
      end;
      function As_Label
        (Node : Ada_Node'Class) return Label
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Label;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Label_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to Label";
         
            end if;
      end;
      function As_Label_Decl
        (Node : Ada_Node'Class) return Label_Decl
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Label_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Label_Decl_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to LabelDecl";
         
            end if;
      end;
      function As_Library_Item
        (Node : Ada_Node'Class) return Library_Item
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Library_Item;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Library_Item_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to LibraryItem";
         
            end if;
      end;
      function As_Limited_Node
        (Node : Ada_Node'Class) return Limited_Node
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Limited_Node;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Limited_Node then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to Limited";
         
            end if;
      end;
      function As_Limited_Absent
        (Node : Ada_Node'Class) return Limited_Absent
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Limited_Absent;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Limited_Absent_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to Limited.Absent";
         
            end if;
      end;
      function As_Limited_Present
        (Node : Ada_Node'Class) return Limited_Present
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Limited_Present;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Limited_Present_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to Limited.Present";
         
            end if;
      end;
      function As_Loop_Stmt
        (Node : Ada_Node'Class) return Loop_Stmt
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Loop_Stmt;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Loop_Stmt_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to LoopStmt";
         
            end if;
      end;
      function As_Membership_Expr
        (Node : Ada_Node'Class) return Membership_Expr
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Membership_Expr;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Membership_Expr_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to MembershipExpr";
         
            end if;
      end;
      function As_Mod_Int_Type_Def
        (Node : Ada_Node'Class) return Mod_Int_Type_Def
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Mod_Int_Type_Def;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Mod_Int_Type_Def_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to ModIntTypeDef";
         
            end if;
      end;
      function As_Mode
        (Node : Ada_Node'Class) return Mode
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Mode;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Mode then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to Mode";
         
            end if;
      end;
      function As_Mode_Default
        (Node : Ada_Node'Class) return Mode_Default
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Mode_Default;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Mode_Default_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to Mode.Default";
         
            end if;
      end;
      function As_Mode_In
        (Node : Ada_Node'Class) return Mode_In
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Mode_In;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Mode_In_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to Mode.In";
         
            end if;
      end;
      function As_Mode_In_Out
        (Node : Ada_Node'Class) return Mode_In_Out
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Mode_In_Out;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Mode_In_Out_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to Mode.InOut";
         
            end if;
      end;
      function As_Mode_Out
        (Node : Ada_Node'Class) return Mode_Out
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Mode_Out;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Mode_Out_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to Mode.Out";
         
            end if;
      end;
      function As_Multi_Abstract_State_Decl
        (Node : Ada_Node'Class) return Multi_Abstract_State_Decl
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Multi_Abstract_State_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Multi_Abstract_State_Decl_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to MultiAbstractStateDecl";
         
            end if;
      end;
      function As_Multi_Dim_Array_Assoc
        (Node : Ada_Node'Class) return Multi_Dim_Array_Assoc
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Multi_Dim_Array_Assoc;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Multi_Dim_Array_Assoc_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to MultiDimArrayAssoc";
         
            end if;
      end;
      function As_Name_List
        (Node : Ada_Node'Class) return Name_List
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Name_List;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Name_List_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to Name.list";
         
            end if;
      end;
      function As_Named_Stmt
        (Node : Ada_Node'Class) return Named_Stmt
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Named_Stmt;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Named_Stmt_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to NamedStmt";
         
            end if;
      end;
      function As_Named_Stmt_Decl
        (Node : Ada_Node'Class) return Named_Stmt_Decl
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Named_Stmt_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Named_Stmt_Decl_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to NamedStmtDecl";
         
            end if;
      end;
      function As_No_Type_Object_Renaming_Decl
        (Node : Ada_Node'Class) return No_Type_Object_Renaming_Decl
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_No_Type_Object_Renaming_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_No_Type_Object_Renaming_Decl_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to NoTypeObjectRenamingDecl";
         
            end if;
      end;
      function As_Not_Null
        (Node : Ada_Node'Class) return Not_Null
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Not_Null;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Not_Null then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to NotNull";
         
            end if;
      end;
      function As_Not_Null_Absent
        (Node : Ada_Node'Class) return Not_Null_Absent
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Not_Null_Absent;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Not_Null_Absent_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to NotNull.Absent";
         
            end if;
      end;
      function As_Not_Null_Present
        (Node : Ada_Node'Class) return Not_Null_Present
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Not_Null_Present;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Not_Null_Present_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to NotNull.Present";
         
            end if;
      end;
      function As_Null_Component_Decl
        (Node : Ada_Node'Class) return Null_Component_Decl
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Null_Component_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Null_Component_Decl_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to NullComponentDecl";
         
            end if;
      end;
      function As_Null_Literal
        (Node : Ada_Node'Class) return Null_Literal
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Null_Literal;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Null_Literal_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to NullLiteral";
         
            end if;
      end;
      function As_Null_Record_Aggregate
        (Node : Ada_Node'Class) return Null_Record_Aggregate
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Null_Record_Aggregate;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Null_Record_Aggregate_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to NullRecordAggregate";
         
            end if;
      end;
      function As_Null_Record_Def
        (Node : Ada_Node'Class) return Null_Record_Def
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Null_Record_Def;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Null_Record_Def_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to NullRecordDef";
         
            end if;
      end;
      function As_Null_Stmt
        (Node : Ada_Node'Class) return Null_Stmt
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Null_Stmt;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Null_Stmt_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to NullStmt";
         
            end if;
      end;
      function As_Null_Subp_Decl
        (Node : Ada_Node'Class) return Null_Subp_Decl
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Null_Subp_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Null_Subp_Decl_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to NullSubpDecl";
         
            end if;
      end;
      function As_Number_Decl
        (Node : Ada_Node'Class) return Number_Decl
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Number_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Number_Decl_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to NumberDecl";
         
            end if;
      end;
      function As_Op
        (Node : Ada_Node'Class) return Op
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Op;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Op then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to Op";
         
            end if;
      end;
      function As_Op_Abs
        (Node : Ada_Node'Class) return Op_Abs
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Op_Abs;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Op_Abs_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to Op.Abs";
         
            end if;
      end;
      function As_Op_And
        (Node : Ada_Node'Class) return Op_And
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Op_And;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Op_And_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to Op.And";
         
            end if;
      end;
      function As_Op_And_Then
        (Node : Ada_Node'Class) return Op_And_Then
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Op_And_Then;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Op_And_Then_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to Op.AndThen";
         
            end if;
      end;
      function As_Op_Concat
        (Node : Ada_Node'Class) return Op_Concat
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Op_Concat;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Op_Concat_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to Op.Concat";
         
            end if;
      end;
      function As_Op_Div
        (Node : Ada_Node'Class) return Op_Div
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Op_Div;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Op_Div_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to Op.Div";
         
            end if;
      end;
      function As_Op_Double_Dot
        (Node : Ada_Node'Class) return Op_Double_Dot
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Op_Double_Dot;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Op_Double_Dot_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to Op.DoubleDot";
         
            end if;
      end;
      function As_Op_Eq
        (Node : Ada_Node'Class) return Op_Eq
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Op_Eq;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Op_Eq_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to Op.Eq";
         
            end if;
      end;
      function As_Op_Gt
        (Node : Ada_Node'Class) return Op_Gt
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Op_Gt;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Op_Gt_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to Op.Gt";
         
            end if;
      end;
      function As_Op_Gte
        (Node : Ada_Node'Class) return Op_Gte
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Op_Gte;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Op_Gte_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to Op.Gte";
         
            end if;
      end;
      function As_Op_In
        (Node : Ada_Node'Class) return Op_In
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Op_In;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Op_In_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to Op.In";
         
            end if;
      end;
      function As_Op_Lt
        (Node : Ada_Node'Class) return Op_Lt
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Op_Lt;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Op_Lt_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to Op.Lt";
         
            end if;
      end;
      function As_Op_Lte
        (Node : Ada_Node'Class) return Op_Lte
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Op_Lte;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Op_Lte_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to Op.Lte";
         
            end if;
      end;
      function As_Op_Minus
        (Node : Ada_Node'Class) return Op_Minus
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Op_Minus;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Op_Minus_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to Op.Minus";
         
            end if;
      end;
      function As_Op_Mod
        (Node : Ada_Node'Class) return Op_Mod
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Op_Mod;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Op_Mod_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to Op.Mod";
         
            end if;
      end;
      function As_Op_Mult
        (Node : Ada_Node'Class) return Op_Mult
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Op_Mult;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Op_Mult_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to Op.Mult";
         
            end if;
      end;
      function As_Op_Neq
        (Node : Ada_Node'Class) return Op_Neq
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Op_Neq;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Op_Neq_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to Op.Neq";
         
            end if;
      end;
      function As_Op_Not
        (Node : Ada_Node'Class) return Op_Not
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Op_Not;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Op_Not_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to Op.Not";
         
            end if;
      end;
      function As_Op_Not_In
        (Node : Ada_Node'Class) return Op_Not_In
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Op_Not_In;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Op_Not_In_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to Op.NotIn";
         
            end if;
      end;
      function As_Op_Or
        (Node : Ada_Node'Class) return Op_Or
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Op_Or;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Op_Or_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to Op.Or";
         
            end if;
      end;
      function As_Op_Or_Else
        (Node : Ada_Node'Class) return Op_Or_Else
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Op_Or_Else;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Op_Or_Else_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to Op.OrElse";
         
            end if;
      end;
      function As_Op_Plus
        (Node : Ada_Node'Class) return Op_Plus
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Op_Plus;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Op_Plus_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to Op.Plus";
         
            end if;
      end;
      function As_Op_Pow
        (Node : Ada_Node'Class) return Op_Pow
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Op_Pow;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Op_Pow_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to Op.Pow";
         
            end if;
      end;
      function As_Op_Rem
        (Node : Ada_Node'Class) return Op_Rem
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Op_Rem;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Op_Rem_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to Op.Rem";
         
            end if;
      end;
      function As_Op_Xor
        (Node : Ada_Node'Class) return Op_Xor
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Op_Xor;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Op_Xor_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to Op.Xor";
         
            end if;
      end;
      function As_Ordinary_Fixed_Point_Def
        (Node : Ada_Node'Class) return Ordinary_Fixed_Point_Def
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Ordinary_Fixed_Point_Def;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Ordinary_Fixed_Point_Def_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to OrdinaryFixedPointDef";
         
            end if;
      end;
      function As_Others_Designator
        (Node : Ada_Node'Class) return Others_Designator
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Others_Designator;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Others_Designator_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to OthersDesignator";
         
            end if;
      end;
      function As_Overriding_Node
        (Node : Ada_Node'Class) return Overriding_Node
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Overriding_Node;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Overriding_Node then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to Overriding";
         
            end if;
      end;
      function As_Overriding_Not_Overriding
        (Node : Ada_Node'Class) return Overriding_Not_Overriding
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Overriding_Not_Overriding;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Overriding_Not_Overriding_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to Overriding.NotOverriding";
         
            end if;
      end;
      function As_Overriding_Overriding
        (Node : Ada_Node'Class) return Overriding_Overriding
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Overriding_Overriding;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Overriding_Overriding_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to Overriding.Overriding";
         
            end if;
      end;
      function As_Overriding_Unspecified
        (Node : Ada_Node'Class) return Overriding_Unspecified
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Overriding_Unspecified;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Overriding_Unspecified_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to Overriding.Unspecified";
         
            end if;
      end;
      function As_Package_Body
        (Node : Ada_Node'Class) return Package_Body
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Package_Body;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Package_Body_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to PackageBody";
         
            end if;
      end;
      function As_Package_Body_Stub
        (Node : Ada_Node'Class) return Package_Body_Stub
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Package_Body_Stub;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Package_Body_Stub_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to PackageBodyStub";
         
            end if;
      end;
      function As_Package_Decl
        (Node : Ada_Node'Class) return Package_Decl
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Package_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Package_Decl_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to PackageDecl";
         
            end if;
      end;
      function As_Package_Renaming_Decl
        (Node : Ada_Node'Class) return Package_Renaming_Decl
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Package_Renaming_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Package_Renaming_Decl_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to PackageRenamingDecl";
         
            end if;
      end;
      function As_Param_Assoc
        (Node : Ada_Node'Class) return Param_Assoc
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Param_Assoc;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Param_Assoc_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to ParamAssoc";
         
            end if;
      end;
      function As_Param_Spec
        (Node : Ada_Node'Class) return Param_Spec
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Param_Spec;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Param_Spec_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to ParamSpec";
         
            end if;
      end;
      function As_Param_Spec_List
        (Node : Ada_Node'Class) return Param_Spec_List
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Param_Spec_List;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Param_Spec_List_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to ParamSpec.list";
         
            end if;
      end;
      function As_Params
        (Node : Ada_Node'Class) return Params
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Params;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Params_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to Params";
         
            end if;
      end;
      function As_Paren_Abstract_State_Decl
        (Node : Ada_Node'Class) return Paren_Abstract_State_Decl
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Paren_Abstract_State_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Paren_Abstract_State_Decl_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to ParenAbstractStateDecl";
         
            end if;
      end;
      function As_Paren_Expr
        (Node : Ada_Node'Class) return Paren_Expr
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Paren_Expr;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Paren_Expr_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to ParenExpr";
         
            end if;
      end;
      function As_Parent_List
        (Node : Ada_Node'Class) return Parent_List
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Parent_List;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Parent_List_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to ParentList";
         
            end if;
      end;
      function As_Pp_Directive
        (Node : Ada_Node'Class) return Pp_Directive
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Pp_Directive;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Pp_Directive then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to PpDirective";
         
            end if;
      end;
      function As_Pp_Else_Directive
        (Node : Ada_Node'Class) return Pp_Else_Directive
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Pp_Else_Directive;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Pp_Else_Directive_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to PpElseDirective";
         
            end if;
      end;
      function As_Pp_Elsif_Directive
        (Node : Ada_Node'Class) return Pp_Elsif_Directive
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Pp_Elsif_Directive;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Pp_Elsif_Directive_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to PpElsifDirective";
         
            end if;
      end;
      function As_Pp_End_If_Directive
        (Node : Ada_Node'Class) return Pp_End_If_Directive
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Pp_End_If_Directive;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Pp_End_If_Directive_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to PpEndIfDirective";
         
            end if;
      end;
      function As_Pp_If_Directive
        (Node : Ada_Node'Class) return Pp_If_Directive
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Pp_If_Directive;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Pp_If_Directive_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to PpIfDirective";
         
            end if;
      end;
      function As_Pp_Then_Kw
        (Node : Ada_Node'Class) return Pp_Then_Kw
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Pp_Then_Kw;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Pp_Then_Kw_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to PpThenKw";
         
            end if;
      end;
      function As_Pragma_Argument_Assoc
        (Node : Ada_Node'Class) return Pragma_Argument_Assoc
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Pragma_Argument_Assoc;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Pragma_Argument_Assoc_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to PragmaArgumentAssoc";
         
            end if;
      end;
      function As_Pragma_Node
        (Node : Ada_Node'Class) return Pragma_Node
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Pragma_Node;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Pragma_Node_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to Pragma";
         
            end if;
      end;
      function As_Pragma_Node_List
        (Node : Ada_Node'Class) return Pragma_Node_List
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Pragma_Node_List;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Pragma_Node_List_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to Pragma.list";
         
            end if;
      end;
      function As_Private_Node
        (Node : Ada_Node'Class) return Private_Node
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Private_Node;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Private_Node then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to Private";
         
            end if;
      end;
      function As_Private_Absent
        (Node : Ada_Node'Class) return Private_Absent
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Private_Absent;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Private_Absent_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to Private.Absent";
         
            end if;
      end;
      function As_Private_Part
        (Node : Ada_Node'Class) return Private_Part
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Private_Part;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Private_Part_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to PrivatePart";
         
            end if;
      end;
      function As_Private_Present
        (Node : Ada_Node'Class) return Private_Present
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Private_Present;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Private_Present_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to Private.Present";
         
            end if;
      end;
      function As_Private_Type_Def
        (Node : Ada_Node'Class) return Private_Type_Def
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Private_Type_Def;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Private_Type_Def_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to PrivateTypeDef";
         
            end if;
      end;
      function As_Protected_Node
        (Node : Ada_Node'Class) return Protected_Node
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Protected_Node;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Protected_Node then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to Protected";
         
            end if;
      end;
      function As_Protected_Absent
        (Node : Ada_Node'Class) return Protected_Absent
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Protected_Absent;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Protected_Absent_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to Protected.Absent";
         
            end if;
      end;
      function As_Protected_Body
        (Node : Ada_Node'Class) return Protected_Body
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Protected_Body;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Protected_Body_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to ProtectedBody";
         
            end if;
      end;
      function As_Protected_Body_Stub
        (Node : Ada_Node'Class) return Protected_Body_Stub
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Protected_Body_Stub;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Protected_Body_Stub_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to ProtectedBodyStub";
         
            end if;
      end;
      function As_Protected_Def
        (Node : Ada_Node'Class) return Protected_Def
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Protected_Def;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Protected_Def_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to ProtectedDef";
         
            end if;
      end;
      function As_Protected_Present
        (Node : Ada_Node'Class) return Protected_Present
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Protected_Present;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Protected_Present_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to Protected.Present";
         
            end if;
      end;
      function As_Protected_Type_Decl
        (Node : Ada_Node'Class) return Protected_Type_Decl
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Protected_Type_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Protected_Type_Decl_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to ProtectedTypeDecl";
         
            end if;
      end;
      function As_Public_Part
        (Node : Ada_Node'Class) return Public_Part
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Public_Part;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Public_Part_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to PublicPart";
         
            end if;
      end;
      function As_Qual_Expr
        (Node : Ada_Node'Class) return Qual_Expr
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Qual_Expr;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Qual_Expr_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to QualExpr";
         
            end if;
      end;
      function As_Quantified_Expr
        (Node : Ada_Node'Class) return Quantified_Expr
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Quantified_Expr;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Quantified_Expr_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to QuantifiedExpr";
         
            end if;
      end;
      function As_Quantifier
        (Node : Ada_Node'Class) return Quantifier
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Quantifier;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Quantifier then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to Quantifier";
         
            end if;
      end;
      function As_Quantifier_All
        (Node : Ada_Node'Class) return Quantifier_All
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Quantifier_All;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Quantifier_All_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to Quantifier.All";
         
            end if;
      end;
      function As_Quantifier_Some
        (Node : Ada_Node'Class) return Quantifier_Some
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Quantifier_Some;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Quantifier_Some_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to Quantifier.Some";
         
            end if;
      end;
      function As_Raise_Expr
        (Node : Ada_Node'Class) return Raise_Expr
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Raise_Expr;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Raise_Expr_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to RaiseExpr";
         
            end if;
      end;
      function As_Raise_Stmt
        (Node : Ada_Node'Class) return Raise_Stmt
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Raise_Stmt;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Raise_Stmt_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to RaiseStmt";
         
            end if;
      end;
      function As_Range_Constraint
        (Node : Ada_Node'Class) return Range_Constraint
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Range_Constraint;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Range_Constraint_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to RangeConstraint";
         
            end if;
      end;
      function As_Range_Spec
        (Node : Ada_Node'Class) return Range_Spec
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Range_Spec;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Range_Spec_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to RangeSpec";
         
            end if;
      end;
      function As_Real_Literal
        (Node : Ada_Node'Class) return Real_Literal
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Real_Literal;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Real_Literal_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to RealLiteral";
         
            end if;
      end;
      function As_Record_Def
        (Node : Ada_Node'Class) return Record_Def
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Record_Def;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Record_Def_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to RecordDef";
         
            end if;
      end;
      function As_Record_Rep_Clause
        (Node : Ada_Node'Class) return Record_Rep_Clause
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Record_Rep_Clause;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Record_Rep_Clause_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to RecordRepClause";
         
            end if;
      end;
      function As_Record_Type_Def
        (Node : Ada_Node'Class) return Record_Type_Def
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Record_Type_Def;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Record_Type_Def_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to RecordTypeDef";
         
            end if;
      end;
      function As_Reduce_Attribute_Ref
        (Node : Ada_Node'Class) return Reduce_Attribute_Ref
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Reduce_Attribute_Ref;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Reduce_Attribute_Ref_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to ReduceAttributeRef";
         
            end if;
      end;
      function As_Relation_Op
        (Node : Ada_Node'Class) return Relation_Op
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Relation_Op;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Relation_Op_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to RelationOp";
         
            end if;
      end;
      function As_Renaming_Clause
        (Node : Ada_Node'Class) return Renaming_Clause
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Renaming_Clause;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Renaming_Clause_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to RenamingClause";
         
            end if;
      end;
      function As_Requeue_Stmt
        (Node : Ada_Node'Class) return Requeue_Stmt
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Requeue_Stmt;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Requeue_Stmt_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to RequeueStmt";
         
            end if;
      end;
      function As_Return_Stmt
        (Node : Ada_Node'Class) return Return_Stmt
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Return_Stmt;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Return_Stmt_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to ReturnStmt";
         
            end if;
      end;
      function As_Reverse_Node
        (Node : Ada_Node'Class) return Reverse_Node
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Reverse_Node;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Reverse_Node then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to Reverse";
         
            end if;
      end;
      function As_Reverse_Absent
        (Node : Ada_Node'Class) return Reverse_Absent
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Reverse_Absent;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Reverse_Absent_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to Reverse.Absent";
         
            end if;
      end;
      function As_Reverse_Present
        (Node : Ada_Node'Class) return Reverse_Present
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Reverse_Present;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Reverse_Present_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to Reverse.Present";
         
            end if;
      end;
      function As_Select_Stmt
        (Node : Ada_Node'Class) return Select_Stmt
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Select_Stmt;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Select_Stmt_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to SelectStmt";
         
            end if;
      end;
      function As_Select_When_Part
        (Node : Ada_Node'Class) return Select_When_Part
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Select_When_Part;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Select_When_Part_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to SelectWhenPart";
         
            end if;
      end;
      function As_Select_When_Part_List
        (Node : Ada_Node'Class) return Select_When_Part_List
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Select_When_Part_List;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Select_When_Part_List_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to SelectWhenPart.list";
         
            end if;
      end;
      function As_Signed_Int_Type_Def
        (Node : Ada_Node'Class) return Signed_Int_Type_Def
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Signed_Int_Type_Def;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Signed_Int_Type_Def_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to SignedIntTypeDef";
         
            end if;
      end;
      function As_Single_Protected_Decl
        (Node : Ada_Node'Class) return Single_Protected_Decl
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Single_Protected_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Single_Protected_Decl_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to SingleProtectedDecl";
         
            end if;
      end;
      function As_Single_Task_Decl
        (Node : Ada_Node'Class) return Single_Task_Decl
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Single_Task_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Single_Task_Decl_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to SingleTaskDecl";
         
            end if;
      end;
      function As_Task_Type_Decl
        (Node : Ada_Node'Class) return Task_Type_Decl
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Task_Type_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Task_Type_Decl_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to TaskTypeDecl";
         
            end if;
      end;
      function As_Single_Task_Type_Decl
        (Node : Ada_Node'Class) return Single_Task_Type_Decl
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Single_Task_Type_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Single_Task_Type_Decl_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to SingleTaskTypeDecl";
         
            end if;
      end;
      function As_Stmt_List
        (Node : Ada_Node'Class) return Stmt_List
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Stmt_List;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Stmt_List_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to StmtList";
         
            end if;
      end;
      function As_String_Literal
        (Node : Ada_Node'Class) return String_Literal
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_String_Literal;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_String_Literal_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to StringLiteral";
         
            end if;
      end;
      function As_Subp_Body
        (Node : Ada_Node'Class) return Subp_Body
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Subp_Body;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Subp_Body_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to SubpBody";
         
            end if;
      end;
      function As_Subp_Body_Stub
        (Node : Ada_Node'Class) return Subp_Body_Stub
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Subp_Body_Stub;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Subp_Body_Stub_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to SubpBodyStub";
         
            end if;
      end;
      function As_Subp_Decl
        (Node : Ada_Node'Class) return Subp_Decl
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Subp_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Subp_Decl_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to SubpDecl";
         
            end if;
      end;
      function As_Subp_Kind
        (Node : Ada_Node'Class) return Subp_Kind
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Subp_Kind;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Subp_Kind then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to SubpKind";
         
            end if;
      end;
      function As_Subp_Kind_Function
        (Node : Ada_Node'Class) return Subp_Kind_Function
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Subp_Kind_Function;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Subp_Kind_Function_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to SubpKind.Function";
         
            end if;
      end;
      function As_Subp_Kind_Procedure
        (Node : Ada_Node'Class) return Subp_Kind_Procedure
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Subp_Kind_Procedure;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Subp_Kind_Procedure_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to SubpKind.Procedure";
         
            end if;
      end;
      function As_Subp_Renaming_Decl
        (Node : Ada_Node'Class) return Subp_Renaming_Decl
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Subp_Renaming_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Subp_Renaming_Decl_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to SubpRenamingDecl";
         
            end if;
      end;
      function As_Subp_Spec
        (Node : Ada_Node'Class) return Subp_Spec
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Subp_Spec;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Subp_Spec_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to SubpSpec";
         
            end if;
      end;
      function As_Subtype_Decl
        (Node : Ada_Node'Class) return Subtype_Decl
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Subtype_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Subtype_Decl_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to SubtypeDecl";
         
            end if;
      end;
      function As_Subunit
        (Node : Ada_Node'Class) return Subunit
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Subunit;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Subunit_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to Subunit";
         
            end if;
      end;
      function As_Synchronized_Node
        (Node : Ada_Node'Class) return Synchronized_Node
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Synchronized_Node;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Synchronized_Node then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to Synchronized";
         
            end if;
      end;
      function As_Synchronized_Absent
        (Node : Ada_Node'Class) return Synchronized_Absent
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Synchronized_Absent;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Synchronized_Absent_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to Synchronized.Absent";
         
            end if;
      end;
      function As_Synchronized_Present
        (Node : Ada_Node'Class) return Synchronized_Present
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Synchronized_Present;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Synchronized_Present_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to Synchronized.Present";
         
            end if;
      end;
      function As_Synth_Anonymous_Type_Decl
        (Node : Ada_Node'Class) return Synth_Anonymous_Type_Decl
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Synth_Anonymous_Type_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Synth_Anonymous_Type_Decl_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to SynthAnonymousTypeDecl";
         
            end if;
      end;
      function As_Synthetic_Binary_Spec
        (Node : Ada_Node'Class) return Synthetic_Binary_Spec
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Synthetic_Binary_Spec;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Synthetic_Binary_Spec_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to SyntheticBinarySpec";
         
            end if;
      end;
      function As_Synthetic_Char_Enum_Lit
        (Node : Ada_Node'Class) return Synthetic_Char_Enum_Lit
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Synthetic_Char_Enum_Lit;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Synthetic_Char_Enum_Lit_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to SyntheticCharEnumLit";
         
            end if;
      end;
      function As_Synthetic_Defining_Name
        (Node : Ada_Node'Class) return Synthetic_Defining_Name
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Synthetic_Defining_Name;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Synthetic_Defining_Name_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to SyntheticDefiningName";
         
            end if;
      end;
      function As_Synthetic_Formal_Param_Decl
        (Node : Ada_Node'Class) return Synthetic_Formal_Param_Decl
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Synthetic_Formal_Param_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Synthetic_Formal_Param_Decl_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to SyntheticFormalParamDecl";
         
            end if;
      end;
      function As_Synthetic_Identifier
        (Node : Ada_Node'Class) return Synthetic_Identifier
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Synthetic_Identifier;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Synthetic_Identifier_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to SyntheticIdentifier";
         
            end if;
      end;
      function As_Synthetic_Renaming_Clause
        (Node : Ada_Node'Class) return Synthetic_Renaming_Clause
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Synthetic_Renaming_Clause;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Synthetic_Renaming_Clause_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to SyntheticRenamingClause";
         
            end if;
      end;
      function As_Synthetic_Subp_Decl
        (Node : Ada_Node'Class) return Synthetic_Subp_Decl
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Synthetic_Subp_Decl;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Synthetic_Subp_Decl_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to SyntheticSubpDecl";
         
            end if;
      end;
      function As_Synthetic_Type_Expr
        (Node : Ada_Node'Class) return Synthetic_Type_Expr
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Synthetic_Type_Expr;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Synthetic_Type_Expr_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to SyntheticTypeExpr";
         
            end if;
      end;
      function As_Synthetic_Unary_Spec
        (Node : Ada_Node'Class) return Synthetic_Unary_Spec
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Synthetic_Unary_Spec;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Synthetic_Unary_Spec_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to SyntheticUnarySpec";
         
            end if;
      end;
      function As_Tagged_Node
        (Node : Ada_Node'Class) return Tagged_Node
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Tagged_Node;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Tagged_Node then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to Tagged";
         
            end if;
      end;
      function As_Tagged_Absent
        (Node : Ada_Node'Class) return Tagged_Absent
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Tagged_Absent;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Tagged_Absent_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to Tagged.Absent";
         
            end if;
      end;
      function As_Tagged_Present
        (Node : Ada_Node'Class) return Tagged_Present
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Tagged_Present;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Tagged_Present_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to Tagged.Present";
         
            end if;
      end;
      function As_Target_Name
        (Node : Ada_Node'Class) return Target_Name
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Target_Name;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Target_Name_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to TargetName";
         
            end if;
      end;
      function As_Task_Body
        (Node : Ada_Node'Class) return Task_Body
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Task_Body;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Task_Body_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to TaskBody";
         
            end if;
      end;
      function As_Task_Body_Stub
        (Node : Ada_Node'Class) return Task_Body_Stub
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Task_Body_Stub;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Task_Body_Stub_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to TaskBodyStub";
         
            end if;
      end;
      function As_Task_Def
        (Node : Ada_Node'Class) return Task_Def
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Task_Def;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Task_Def_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to TaskDef";
         
            end if;
      end;
      function As_Terminate_Alternative
        (Node : Ada_Node'Class) return Terminate_Alternative
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Terminate_Alternative;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Terminate_Alternative_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to TerminateAlternative";
         
            end if;
      end;
      function As_Type_Access_Def
        (Node : Ada_Node'Class) return Type_Access_Def
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Type_Access_Def;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Type_Access_Def_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to TypeAccessDef";
         
            end if;
      end;
      function As_Type_Attributes_Repository
        (Node : Ada_Node'Class) return Type_Attributes_Repository
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Type_Attributes_Repository;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Type_Attributes_Repository_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to TypeAttributesRepository";
         
            end if;
      end;
      function As_Un_Op
        (Node : Ada_Node'Class) return Un_Op
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Un_Op;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Un_Op_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to UnOp";
         
            end if;
      end;
      function As_Unconstrained_Array_Index
        (Node : Ada_Node'Class) return Unconstrained_Array_Index
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Unconstrained_Array_Index;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Unconstrained_Array_Index_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to UnconstrainedArrayIndex";
         
            end if;
      end;
      function As_Unconstrained_Array_Index_List
        (Node : Ada_Node'Class) return Unconstrained_Array_Index_List
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Unconstrained_Array_Index_List;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Unconstrained_Array_Index_List_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to UnconstrainedArrayIndex.list";
         
            end if;
      end;
      function As_Unconstrained_Array_Indices
        (Node : Ada_Node'Class) return Unconstrained_Array_Indices
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Unconstrained_Array_Indices;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Unconstrained_Array_Indices_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to UnconstrainedArrayIndices";
         
            end if;
      end;
      function As_Unknown_Discriminant_Part
        (Node : Ada_Node'Class) return Unknown_Discriminant_Part
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Unknown_Discriminant_Part;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Unknown_Discriminant_Part_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to UnknownDiscriminantPart";
         
            end if;
      end;
      function As_Until_Node
        (Node : Ada_Node'Class) return Until_Node
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Until_Node;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Until_Node then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to Until";
         
            end if;
      end;
      function As_Until_Absent
        (Node : Ada_Node'Class) return Until_Absent
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Until_Absent;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Until_Absent_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to Until.Absent";
         
            end if;
      end;
      function As_Until_Present
        (Node : Ada_Node'Class) return Until_Present
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Until_Present;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Until_Present_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to Until.Present";
         
            end if;
      end;
      function As_Update_Attribute_Ref
        (Node : Ada_Node'Class) return Update_Attribute_Ref
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Update_Attribute_Ref;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Update_Attribute_Ref_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to UpdateAttributeRef";
         
            end if;
      end;
      function As_Use_Clause
        (Node : Ada_Node'Class) return Use_Clause
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Use_Clause;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Use_Clause then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to UseClause";
         
            end if;
      end;
      function As_Use_Package_Clause
        (Node : Ada_Node'Class) return Use_Package_Clause
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Use_Package_Clause;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Use_Package_Clause_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to UsePackageClause";
         
            end if;
      end;
      function As_Use_Type_Clause
        (Node : Ada_Node'Class) return Use_Type_Clause
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Use_Type_Clause;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Use_Type_Clause_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to UseTypeClause";
         
            end if;
      end;
      function As_Value_Sequence
        (Node : Ada_Node'Class) return Value_Sequence
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Value_Sequence;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Value_Sequence_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to ValueSequence";
         
            end if;
      end;
      function As_Variant
        (Node : Ada_Node'Class) return Variant
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Variant;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Variant_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to Variant";
         
            end if;
      end;
      function As_Variant_List
        (Node : Ada_Node'Class) return Variant_List
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Variant_List;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Variant_List_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to Variant.list";
         
            end if;
      end;
      function As_Variant_Part
        (Node : Ada_Node'Class) return Variant_Part
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_Variant_Part;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_Variant_Part_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to VariantPart";
         
            end if;
      end;
      function As_While_Loop_Spec
        (Node : Ada_Node'Class) return While_Loop_Spec
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_While_Loop_Spec;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_While_Loop_Spec_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to WhileLoopSpec";
         
            end if;
      end;
      function As_While_Loop_Stmt
        (Node : Ada_Node'Class) return While_Loop_Stmt
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_While_Loop_Stmt;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_While_Loop_Stmt_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to WhileLoopStmt";
         
            end if;
      end;
      function As_With_Clause
        (Node : Ada_Node'Class) return With_Clause
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_With_Clause;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_With_Clause_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to WithClause";
         
            end if;
      end;
      function As_With_Private
        (Node : Ada_Node'Class) return With_Private
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_With_Private;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_With_Private then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to WithPrivate";
         
            end if;
      end;
      function As_With_Private_Absent
        (Node : Ada_Node'Class) return With_Private_Absent
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_With_Private_Absent;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_With_Private_Absent_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to WithPrivate.Absent";
         
            end if;
      end;
      function As_With_Private_Present
        (Node : Ada_Node'Class) return With_Private_Present
      is
         N : constant Bare_Ada_Node := Node.Internal.Node;
      begin
         if N = null then
            return No_With_Private_Present;
         end if;

         Check_Safety_Net (Node);

         
         

            if N.Kind in Ada_With_Private_Present_Range then
               
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         
            else
               
            raise Constraint_Error with
              "Libadalang: invalid type conversion from "
              & Node.Kind_Name
              & " to WithPrivate.Present";
         
            end if;
      end;

   -----------------------
   -- Entity primitives --
   -----------------------

   ----------
   -- Hash --
   ----------

   function Hash
     (Node : Ada_Node) return Ada.Containers.Hash_Type is
   begin
      Check_Safety_Net (Node);
      return Hash_Entity (Node.Internal);
   end Hash;

   ----------
   -- Kind --
   ----------

   function Kind (Node : Ada_Node'Class) return Ada_Node_Kind_Type
   is
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      return Node.Internal.Node.Kind;
   end Kind;

   ---------------
   -- Kind_Name --
   ---------------

   function Kind_Name (Node : Ada_Node'Class) return String is
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      return Kind_Name (Node.Internal.Node);
   end Kind_Name;

      


      


      


      


      


      


      


      


      


      


      
      function To_Public_Discriminant_Values_Array
         (Value : Internal_Discriminant_Values_Array_Access) return Discriminant_Values_Array is
      begin
         return Result : Discriminant_Values_Array (1 .. Value.N) do
            for I in Result'Range loop
               
               Result (I - Value.Items'First + Result'First)
                     := To_Public_Discriminant_Values (Value.Items (I))
               ;
            end loop;
         end return;
      end;


      
      function To_Public_Doc_Annotation_Array
         (Value : Internal_Doc_Annotation_Array_Access) return Doc_Annotation_Array is
      begin
         return Result : Doc_Annotation_Array (1 .. Value.N) do
            for I in Result'Range loop
               
               Result (I - Value.Items'First + Result'First)
                     := To_Public_Doc_Annotation (Value.Items (I))
               ;
            end loop;
         end return;
      end;


      
      function To_Public_Accept_Stmt_Array
         (Value : Internal_Entity_Accept_Stmt_Array_Access) return Accept_Stmt_Array is
      begin
         return Result : Accept_Stmt_Array (1 .. Value.N) do
            for I in Result'Range loop
               
               Result (I - Value.Items'First + Result'First)
                     := Wrap_Node (Value.Items (I).Node, Value.Items (I).Info).As_Accept_Stmt
               ;
            end loop;
         end return;
      end;


      
      function To_Public_Ada_Node_Array
         (Value : Internal_Entity_Array_Access) return Ada_Node_Array is
      begin
         return Result : Ada_Node_Array (1 .. Value.N) do
            for I in Result'Range loop
               
               Result (I - Value.Items'First + Result'First)
                     := Wrap_Node (Value.Items (I).Node, Value.Items (I).Info)
               ;
            end loop;
         end return;
      end;


      


      
      function To_Public_Base_Formal_Param_Decl_Array
         (Value : Internal_Entity_Base_Formal_Param_Decl_Array_Access) return Base_Formal_Param_Decl_Array is
      begin
         return Result : Base_Formal_Param_Decl_Array (1 .. Value.N) do
            for I in Result'Range loop
               
               Result (I - Value.Items'First + Result'First)
                     := Wrap_Node (Value.Items (I).Node, Value.Items (I).Info).As_Base_Formal_Param_Decl
               ;
            end loop;
         end return;
      end;


      
      function To_Public_Base_Type_Decl_Array
         (Value : Internal_Entity_Base_Type_Decl_Array_Access) return Base_Type_Decl_Array is
      begin
         return Result : Base_Type_Decl_Array (1 .. Value.N) do
            for I in Result'Range loop
               
               Result (I - Value.Items'First + Result'First)
                     := Wrap_Node (Value.Items (I).Node, Value.Items (I).Info).As_Base_Type_Decl
               ;
            end loop;
         end return;
      end;


      


      
      function To_Public_Basic_Decl_Array
         (Value : Internal_Entity_Basic_Decl_Array_Access) return Basic_Decl_Array is
      begin
         return Result : Basic_Decl_Array (1 .. Value.N) do
            for I in Result'Range loop
               
               Result (I - Value.Items'First + Result'First)
                     := Wrap_Node (Value.Items (I).Node, Value.Items (I).Info).As_Basic_Decl
               ;
            end loop;
         end return;
      end;


      
      function To_Public_Compilation_Unit_Array
         (Value : Internal_Entity_Compilation_Unit_Array_Access) return Compilation_Unit_Array is
      begin
         return Result : Compilation_Unit_Array (1 .. Value.N) do
            for I in Result'Range loop
               
               Result (I - Value.Items'First + Result'First)
                     := Wrap_Node (Value.Items (I).Node, Value.Items (I).Info).As_Compilation_Unit
               ;
            end loop;
         end return;
      end;


      


      
      function To_Public_Defining_Name_Array
         (Value : Internal_Entity_Defining_Name_Array_Access) return Defining_Name_Array is
      begin
         return Result : Defining_Name_Array (1 .. Value.N) do
            for I in Result'Range loop
               
               Result (I - Value.Items'First + Result'First)
                     := Wrap_Node (Value.Items (I).Node, Value.Items (I).Info).As_Defining_Name
               ;
            end loop;
         end return;
      end;


      
      function To_Public_Expr_Array
         (Value : Internal_Entity_Expr_Array_Access) return Expr_Array is
      begin
         return Result : Expr_Array (1 .. Value.N) do
            for I in Result'Range loop
               
               Result (I - Value.Items'First + Result'First)
                     := Wrap_Node (Value.Items (I).Node, Value.Items (I).Info).As_Expr
               ;
            end loop;
         end return;
      end;


      
      function To_Public_Generic_Instantiation_Array
         (Value : Internal_Entity_Generic_Instantiation_Array_Access) return Generic_Instantiation_Array is
      begin
         return Result : Generic_Instantiation_Array (1 .. Value.N) do
            for I in Result'Range loop
               
               Result (I - Value.Items'First + Result'First)
                     := Wrap_Node (Value.Items (I).Node, Value.Items (I).Info).As_Generic_Instantiation
               ;
            end loop;
         end return;
      end;


      


      


      


      
      function To_Public_Param_Spec_Array
         (Value : Internal_Entity_Param_Spec_Array_Access) return Param_Spec_Array is
      begin
         return Result : Param_Spec_Array (1 .. Value.N) do
            for I in Result'Range loop
               
               Result (I - Value.Items'First + Result'First)
                     := Wrap_Node (Value.Items (I).Node, Value.Items (I).Info).As_Param_Spec
               ;
            end loop;
         end return;
      end;


      
      function To_Public_Pragma_Node_Array
         (Value : Internal_Entity_Pragma_Node_Array_Access) return Pragma_Node_Array is
      begin
         return Result : Pragma_Node_Array (1 .. Value.N) do
            for I in Result'Range loop
               
               Result (I - Value.Items'First + Result'First)
                     := Wrap_Node (Value.Items (I).Node, Value.Items (I).Info).As_Pragma_Node
               ;
            end loop;
         end return;
      end;


      
      function To_Public_Type_Decl_Array
         (Value : Internal_Entity_Type_Decl_Array_Access) return Type_Decl_Array is
      begin
         return Result : Type_Decl_Array (1 .. Value.N) do
            for I in Result'Range loop
               
               Result (I - Value.Items'First + Result'First)
                     := Wrap_Node (Value.Items (I).Node, Value.Items (I).Info).As_Type_Decl
               ;
            end loop;
         end return;
      end;


      


      


      


      


      
      function To_Public_Param_Actual_Array
         (Value : Internal_Param_Actual_Array_Access) return Param_Actual_Array is
      begin
         return Result : Param_Actual_Array (1 .. Value.N) do
            for I in Result'Range loop
               
               Result (I - Value.Items'First + Result'First)
                     := To_Public_Param_Actual (Value.Items (I))
               ;
            end loop;
         end return;
      end;


      


      
      function To_Public_Ref_Result_Array
         (Value : Internal_Ref_Result_Array_Access) return Ref_Result_Array is
      begin
         return Result : Ref_Result_Array (1 .. Value.N) do
            for I in Result'Range loop
               
               Result (I - Value.Items'First + Result'First)
                     := To_Public_Ref_Result (Value.Items (I))
               ;
            end loop;
         end return;
      end;


      
      function To_Public_Shape_Array
         (Value : Internal_Shape_Array_Access) return Shape_Array is
      begin
         return Result : Shape_Array (1 .. Value.N) do
            for I in Result'Range loop
               
               Result (I - Value.Items'First + Result'First)
                     := To_Public_Shape (Value.Items (I))
               ;
            end loop;
         end return;
      end;


      


      

      function To_Internal_Substitution_Array
         (Value : Substitution_Array
          ) return Internal_Substitution_Array_Access
      is
         Result : constant Internal_Substitution_Array_Access :=
            Create_Internal_Substitution_Array (Value'Length);
      begin
         for I in Value'Range loop
            Result.Items (I - Value'First + Result.Items'First) :=
               To_Internal_Substitution (Value (I));
         end loop;
         return Result;
      end;

      
      function To_Public_Analysis_Unit_Array
         (Value : Internal_Unit_Array_Access) return Analysis_Unit_Array is
      begin
         return Result : Analysis_Unit_Array (1 .. Value.N) do
            for I in Result'Range loop
               
               Result (I - Value.Items'First + Result'First)
                     := Wrap_Unit (Value.Items (I))
               ;
            end loop;
         end return;
      end;

      function To_Internal_Analysis_Unit_Array
         (Value : Analysis_Unit_Array
          ) return Internal_Unit_Array_Access
      is
         Result : constant Internal_Unit_Array_Access :=
            Create_Internal_Unit_Array (Value'Length);
      begin
         for I in Value'Range loop
            Result.Items (I - Value'First + Result.Items'First) :=
               Unwrap_Unit (Value (I));
         end loop;
         return Result;
      end;

      


      


      


      
      function To_Public_Unbounded_Text_Type_Array
         (Value : Symbol_Type_Array_Access) return Unbounded_Text_Type_Array is
      begin
         return Result : Unbounded_Text_Type_Array (1 .. Value.N) do
            for I in Result'Range loop
               
               Result (I - Value.Items'First + Result'First)
                     := To_Unbounded_Text (Image (Value.Items (I)))
               ;
            end loop;
         end return;
      end;



         
   

      function To_Public_Completion_Item_Iterator
         (Value : Internal_Completion_Item_Iterator_Access) return Completion_Item_Iterator is
      begin
         Inc_Ref (Value);
         return (Controlled => (Ada.Finalization.Controlled with
                                Internal_Iterator => Value));
      end;


   ----------
   -- Next --
   ----------

   function Next
     (Self : Completion_Item_Iterator;
      Item : out Completion_Item) return Boolean
   is
      Internal_Iter    : constant Internal_Completion_Item_Iterator_Access :=
         Self.Controlled.Internal_Iterator;
      Internal_Element : Internal_Completion_Item;
      Result           : Boolean;
   begin
      if Internal_Iter = null then
         raise Precondition_Failure with "null iterator argument";
      end if;

      Result := Next (Internal_Iter, Internal_Element);
      if Result then
      
         Item := To_Public_Completion_Item (Internal_Element);
      end if;
      return Result;
   end Next;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : in out Completion_Item_Iterator_Controlled) is
   begin
      Self.Internal_Iterator := null;
   end Initialize;

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (Self : in out Completion_Item_Iterator_Controlled) is
   begin
      Inc_Ref (Self.Internal_Iterator);
   end Adjust;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Self : in out Completion_Item_Iterator_Controlled) is
   begin
      Dec_Ref (Self.Internal_Iterator);
   end Finalize;


         

      
   function Exists
     (Self : Aspect)
      return Boolean
 is
         Record_Ref : constant Boxed_Aspect.Element_Access :=
            Internal_Access (Self);
      begin
            return Record_Ref.Internal_Exists
            ;
      end;
      
   function Node
     (Self : Aspect)
      return Ada_Node'Class
 is
         Record_Ref : constant Boxed_Aspect.Element_Access :=
            Internal_Access (Self);
      begin
            return Record_Ref.Internal_Node
            ;
      end;
      
   function Value
     (Self : Aspect)
      return Expr'Class
 is
         Record_Ref : constant Boxed_Aspect.Element_Access :=
            Internal_Access (Self);
      begin
            return Record_Ref.Internal_Value
            ;
      end;

   --------------
   -- Refcount --
   --------------

   function Refcount (Self : Internal_Aspect_Record) return Positive
   is (Self.Refcount);

   ------------------
   -- Set_Refcount --
   ------------------

   procedure Set_Refcount
     (Self : in out Internal_Aspect_Record; Count : Positive) is
   begin
      Self.Refcount := Count;
   end Set_Refcount;


      function To_Public_Aspect
        (Value : Internal_Aspect) return Aspect
      is
         Result : constant Aspect :=
            Aspect (Boxed_Aspect.Create_Element);
         Record_Ref : constant Boxed_Aspect.Element_Access :=
            Internal_Access (Result);
      begin
            
               Record_Ref.Internal_Exists := Value.Exists;
            
               Record_Ref.Internal_Node := Wrap_Node (Value.Node.Node, Value.Node.Info);
            
               Record_Ref.Internal_Value := Wrap_Node (Value.Value.Node, Value.Value.Info).As_Expr;
         return Result;
      end;


   
   function Create_Aspect (Exists : Boolean; Node : Ada_Node'Class; Value : Expr'Class) return Aspect
 is
      Result     : constant Aspect :=
         Aspect (Boxed_Aspect.Create_Element);
      Record_Def : constant Boxed_Aspect.Element_Access :=
         Internal_Access (Result);
   begin
         
            Record_Def.Internal_Exists := Exists;
         
            Record_Def.Internal_Node := Node.As_Ada_Node;
         
            Record_Def.Internal_Value := Value.As_Expr;
      return Result;
   end;

         

      
   function Decl
     (Self : Completion_Item)
      return Basic_Decl'Class
 is
         Record_Ref : constant Boxed_Completion_Item.Element_Access :=
            Internal_Access (Self);
      begin
            return Record_Ref.Internal_Decl
            ;
      end;
      
   function Is_Dot_Call
     (Self : Completion_Item)
      return Boolean
 is
         Record_Ref : constant Boxed_Completion_Item.Element_Access :=
            Internal_Access (Self);
      begin
            return Record_Ref.Internal_Is_Dot_Call
            ;
      end;
      
   function Is_Visible
     (Self : Completion_Item)
      return Boolean
 is
         Record_Ref : constant Boxed_Completion_Item.Element_Access :=
            Internal_Access (Self);
      begin
            return Record_Ref.Internal_Is_Visible
            ;
      end;
      
   function Weight
     (Self : Completion_Item)
      return Integer
 is
         Record_Ref : constant Boxed_Completion_Item.Element_Access :=
            Internal_Access (Self);
      begin
            return Record_Ref.Internal_Weight
            ;
      end;

   --------------
   -- Refcount --
   --------------

   function Refcount (Self : Internal_Completion_Item_Record) return Positive
   is (Self.Refcount);

   ------------------
   -- Set_Refcount --
   ------------------

   procedure Set_Refcount
     (Self : in out Internal_Completion_Item_Record; Count : Positive) is
   begin
      Self.Refcount := Count;
   end Set_Refcount;


      function To_Public_Completion_Item
        (Value : Internal_Completion_Item) return Completion_Item
      is
         Result : constant Completion_Item :=
            Completion_Item (Boxed_Completion_Item.Create_Element);
         Record_Ref : constant Boxed_Completion_Item.Element_Access :=
            Internal_Access (Result);
      begin
            
               Record_Ref.Internal_Decl := Wrap_Node (Value.Decl.Node, Value.Decl.Info).As_Basic_Decl;
            
               Record_Ref.Internal_Is_Dot_Call := Value.Is_Dot_Call;
            
               Record_Ref.Internal_Is_Visible := Value.Is_Visible;
            
               Record_Ref.Internal_Weight := Value.Weight;
         return Result;
      end;


   
   function Create_Completion_Item (Decl : Basic_Decl'Class; Is_Dot_Call : Boolean; Is_Visible : Boolean; Weight : Integer) return Completion_Item
 is
      Result     : constant Completion_Item :=
         Completion_Item (Boxed_Completion_Item.Create_Element);
      Record_Def : constant Boxed_Completion_Item.Element_Access :=
         Internal_Access (Result);
   begin
         
            Record_Def.Internal_Decl := Decl.As_Basic_Decl;
         
            Record_Def.Internal_Is_Dot_Call := Is_Dot_Call;
         
            Record_Def.Internal_Is_Visible := Is_Visible;
         
            Record_Def.Internal_Weight := Weight;
      return Result;
   end;

         

      
   function Low_Bound
     (Self : Discrete_Range)
      return Expr'Class
 is
         Record_Ref : constant Boxed_Discrete_Range.Element_Access :=
            Internal_Access (Self);
      begin
            return Record_Ref.Internal_Low_Bound
            ;
      end;
      
   function High_Bound
     (Self : Discrete_Range)
      return Expr'Class
 is
         Record_Ref : constant Boxed_Discrete_Range.Element_Access :=
            Internal_Access (Self);
      begin
            return Record_Ref.Internal_High_Bound
            ;
      end;

   --------------
   -- Refcount --
   --------------

   function Refcount (Self : Internal_Discrete_Range_Record) return Positive
   is (Self.Refcount);

   ------------------
   -- Set_Refcount --
   ------------------

   procedure Set_Refcount
     (Self : in out Internal_Discrete_Range_Record; Count : Positive) is
   begin
      Self.Refcount := Count;
   end Set_Refcount;


      function To_Public_Discrete_Range
        (Value : Internal_Discrete_Range) return Discrete_Range
      is
         Result : constant Discrete_Range :=
            Discrete_Range (Boxed_Discrete_Range.Create_Element);
         Record_Ref : constant Boxed_Discrete_Range.Element_Access :=
            Internal_Access (Result);
      begin
            
               Record_Ref.Internal_Low_Bound := Wrap_Node (Value.Low_Bound.Node, Value.Low_Bound.Info).As_Expr;
            
               Record_Ref.Internal_High_Bound := Wrap_Node (Value.High_Bound.Node, Value.High_Bound.Info).As_Expr;
         return Result;
      end;


   
   function Create_Discrete_Range (Low_Bound : Expr'Class; High_Bound : Expr'Class) return Discrete_Range
 is
      Result     : constant Discrete_Range :=
         Discrete_Range (Boxed_Discrete_Range.Create_Element);
      Record_Def : constant Boxed_Discrete_Range.Element_Access :=
         Internal_Access (Result);
   begin
         
            Record_Def.Internal_Low_Bound := Low_Bound.As_Expr;
         
            Record_Def.Internal_High_Bound := High_Bound.As_Expr;
      return Result;
   end;

         

      
   function Discriminant
     (Self : Discriminant_Values)
      return Identifier'Class
 is
         Record_Ref : constant Boxed_Discriminant_Values.Element_Access :=
            Internal_Access (Self);
      begin
            return Record_Ref.Internal_Discriminant
            ;
      end;
      
   function Values
     (Self : Discriminant_Values)
      return Alternatives_List'Class
 is
         Record_Ref : constant Boxed_Discriminant_Values.Element_Access :=
            Internal_Access (Self);
      begin
            return Record_Ref.Internal_Values
            ;
      end;

   --------------
   -- Refcount --
   --------------

   function Refcount (Self : Internal_Discriminant_Values_Record) return Positive
   is (Self.Refcount);

   ------------------
   -- Set_Refcount --
   ------------------

   procedure Set_Refcount
     (Self : in out Internal_Discriminant_Values_Record; Count : Positive) is
   begin
      Self.Refcount := Count;
   end Set_Refcount;


      function To_Public_Discriminant_Values
        (Value : Internal_Discriminant_Values) return Discriminant_Values
      is
         Result : constant Discriminant_Values :=
            Discriminant_Values (Boxed_Discriminant_Values.Create_Element);
         Record_Ref : constant Boxed_Discriminant_Values.Element_Access :=
            Internal_Access (Result);
      begin
            
               Record_Ref.Internal_Discriminant := Wrap_Node (Value.Discriminant.Node, Value.Discriminant.Info).As_Identifier;
            
               Record_Ref.Internal_Values := Wrap_Node (Value.Values.Node, Value.Values.Info).As_Alternatives_List;
         return Result;
      end;


   
   function Create_Discriminant_Values (Discriminant : Identifier'Class; Values : Alternatives_List'Class) return Discriminant_Values
 is
      Result     : constant Discriminant_Values :=
         Discriminant_Values (Boxed_Discriminant_Values.Create_Element);
      Record_Def : constant Boxed_Discriminant_Values.Element_Access :=
         Internal_Access (Result);
   begin
         
            Record_Def.Internal_Discriminant := Discriminant.As_Identifier;
         
            Record_Def.Internal_Values := Values.As_Alternatives_List;
      return Result;
   end;

         

      
   function Key
     (Self : Doc_Annotation)
      return Text_Type
 is
         Record_Ref : constant Boxed_Doc_Annotation.Element_Access :=
            Internal_Access (Self);
      begin
            return Record_Ref.Internal_Key
                  .all
            ;
      end;
      
   function Value
     (Self : Doc_Annotation)
      return Text_Type
 is
         Record_Ref : constant Boxed_Doc_Annotation.Element_Access :=
            Internal_Access (Self);
      begin
            return Record_Ref.Internal_Value
                  .all
            ;
      end;

   --------------
   -- Refcount --
   --------------

   function Refcount (Self : Internal_Doc_Annotation_Record) return Positive
   is (Self.Refcount);

   ------------------
   -- Set_Refcount --
   ------------------

   procedure Set_Refcount
     (Self : in out Internal_Doc_Annotation_Record; Count : Positive) is
   begin
      Self.Refcount := Count;
   end Set_Refcount;

      -------------
      -- Release --
      -------------

      procedure Release (Self : in out Internal_Doc_Annotation_Record) is
         
      begin
            Free (Self.Internal_Key);
            Free (Self.Internal_Value);
      end Release;

      function To_Public_Doc_Annotation
        (Value : Internal_Doc_Annotation) return Doc_Annotation
      is
         Result : constant Doc_Annotation :=
            Doc_Annotation (Boxed_Doc_Annotation.Create_Element);
         Record_Ref : constant Boxed_Doc_Annotation.Element_Access :=
            Internal_Access (Result);
      begin
            
               Record_Ref.Internal_Key := new Text_Type'(Value.Key.Content);
            
               Record_Ref.Internal_Value := new Text_Type'(Value.Value.Content);
         return Result;
      end;


   
   function Create_Doc_Annotation (Key : Text_Type; Value : Text_Type) return Doc_Annotation
 is
      Result     : constant Doc_Annotation :=
         Doc_Annotation (Boxed_Doc_Annotation.Create_Element);
      Record_Def : constant Boxed_Doc_Annotation.Element_Access :=
         Internal_Access (Result);
   begin
         
            Record_Def.Internal_Key := new Text_Type'(Key);
         
            Record_Def.Internal_Value := new Text_Type'(Value);
      return Result;
   end;

         

      
   function Param
     (Self : Param_Actual)
      return Defining_Name'Class
 is
         Record_Ref : constant Boxed_Param_Actual.Element_Access :=
            Internal_Access (Self);
      begin
            return Record_Ref.Internal_Param
            ;
      end;
      
   function Actual
     (Self : Param_Actual)
      return Expr'Class
 is
         Record_Ref : constant Boxed_Param_Actual.Element_Access :=
            Internal_Access (Self);
      begin
            return Record_Ref.Internal_Actual
            ;
      end;

   --------------
   -- Refcount --
   --------------

   function Refcount (Self : Internal_Param_Actual_Record) return Positive
   is (Self.Refcount);

   ------------------
   -- Set_Refcount --
   ------------------

   procedure Set_Refcount
     (Self : in out Internal_Param_Actual_Record; Count : Positive) is
   begin
      Self.Refcount := Count;
   end Set_Refcount;


      function To_Public_Param_Actual
        (Value : Internal_Param_Actual) return Param_Actual
      is
         Result : constant Param_Actual :=
            Param_Actual (Boxed_Param_Actual.Create_Element);
         Record_Ref : constant Boxed_Param_Actual.Element_Access :=
            Internal_Access (Result);
      begin
            
               Record_Ref.Internal_Param := Wrap_Node (Value.Param.Node, Value.Param.Info).As_Defining_Name;
            
               Record_Ref.Internal_Actual := Wrap_Node (Value.Actual.Node, Value.Actual.Info).As_Expr;
         return Result;
      end;


   
   function Create_Param_Actual (Param : Defining_Name'Class; Actual : Expr'Class) return Param_Actual
 is
      Result     : constant Param_Actual :=
         Param_Actual (Boxed_Param_Actual.Create_Element);
      Record_Def : constant Boxed_Param_Actual.Element_Access :=
         Internal_Access (Result);
   begin
         
            Record_Def.Internal_Param := Param.As_Defining_Name;
         
            Record_Def.Internal_Actual := Actual.As_Expr;
      return Result;
   end;

         

      
   function Ref
     (Self : Ref_Result)
      return Base_Id'Class
 is
         Record_Ref : constant Boxed_Ref_Result.Element_Access :=
            Internal_Access (Self);
      begin
            return Record_Ref.Internal_Ref
            ;
      end;
      
   function Kind
     (Self : Ref_Result)
      return Ref_Result_Kind
 is
         Record_Ref : constant Boxed_Ref_Result.Element_Access :=
            Internal_Access (Self);
      begin
            return Record_Ref.Internal_Kind
            ;
      end;

   --------------
   -- Refcount --
   --------------

   function Refcount (Self : Internal_Ref_Result_Record) return Positive
   is (Self.Refcount);

   ------------------
   -- Set_Refcount --
   ------------------

   procedure Set_Refcount
     (Self : in out Internal_Ref_Result_Record; Count : Positive) is
   begin
      Self.Refcount := Count;
   end Set_Refcount;


      function To_Public_Ref_Result
        (Value : Internal_Ref_Result) return Ref_Result
      is
         Result : constant Ref_Result :=
            Ref_Result (Boxed_Ref_Result.Create_Element);
         Record_Ref : constant Boxed_Ref_Result.Element_Access :=
            Internal_Access (Result);
      begin
            
               Record_Ref.Internal_Ref := Wrap_Node (Value.Ref.Node, Value.Ref.Info).As_Base_Id;
            
               Record_Ref.Internal_Kind := Value.Kind;
         return Result;
      end;


   
   function Create_Ref_Result (Ref : Base_Id'Class; Kind : Ref_Result_Kind) return Ref_Result
 is
      Result     : constant Ref_Result :=
         Ref_Result (Boxed_Ref_Result.Create_Element);
      Record_Def : constant Boxed_Ref_Result.Element_Access :=
         Internal_Access (Result);
   begin
         
            Record_Def.Internal_Ref := Ref.As_Base_Id;
         
            Record_Def.Internal_Kind := Kind;
      return Result;
   end;

         

      
   function Decl
     (Self : Refd_Decl)
      return Basic_Decl'Class
 is
         Record_Ref : constant Boxed_Refd_Decl.Element_Access :=
            Internal_Access (Self);
      begin
            return Record_Ref.Internal_Decl
            ;
      end;
      
   function Kind
     (Self : Refd_Decl)
      return Ref_Result_Kind
 is
         Record_Ref : constant Boxed_Refd_Decl.Element_Access :=
            Internal_Access (Self);
      begin
            return Record_Ref.Internal_Kind
            ;
      end;

   --------------
   -- Refcount --
   --------------

   function Refcount (Self : Internal_Refd_Decl_Record) return Positive
   is (Self.Refcount);

   ------------------
   -- Set_Refcount --
   ------------------

   procedure Set_Refcount
     (Self : in out Internal_Refd_Decl_Record; Count : Positive) is
   begin
      Self.Refcount := Count;
   end Set_Refcount;


      function To_Public_Refd_Decl
        (Value : Internal_Refd_Decl) return Refd_Decl
      is
         Result : constant Refd_Decl :=
            Refd_Decl (Boxed_Refd_Decl.Create_Element);
         Record_Ref : constant Boxed_Refd_Decl.Element_Access :=
            Internal_Access (Result);
      begin
            
               Record_Ref.Internal_Decl := Wrap_Node (Value.Decl.Node, Value.Decl.Info).As_Basic_Decl;
            
               Record_Ref.Internal_Kind := Value.Kind;
         return Result;
      end;


   
   function Create_Refd_Decl (Decl : Basic_Decl'Class; Kind : Ref_Result_Kind) return Refd_Decl
 is
      Result     : constant Refd_Decl :=
         Refd_Decl (Boxed_Refd_Decl.Create_Element);
      Record_Def : constant Boxed_Refd_Decl.Element_Access :=
         Internal_Access (Result);
   begin
         
            Record_Def.Internal_Decl := Decl.As_Basic_Decl;
         
            Record_Def.Internal_Kind := Kind;
      return Result;
   end;

         

      
   function Def_Name
     (Self : Refd_Def)
      return Defining_Name'Class
 is
         Record_Ref : constant Boxed_Refd_Def.Element_Access :=
            Internal_Access (Self);
      begin
            return Record_Ref.Internal_Def_Name
            ;
      end;
      
   function Kind
     (Self : Refd_Def)
      return Ref_Result_Kind
 is
         Record_Ref : constant Boxed_Refd_Def.Element_Access :=
            Internal_Access (Self);
      begin
            return Record_Ref.Internal_Kind
            ;
      end;

   --------------
   -- Refcount --
   --------------

   function Refcount (Self : Internal_Refd_Def_Record) return Positive
   is (Self.Refcount);

   ------------------
   -- Set_Refcount --
   ------------------

   procedure Set_Refcount
     (Self : in out Internal_Refd_Def_Record; Count : Positive) is
   begin
      Self.Refcount := Count;
   end Set_Refcount;


      function To_Public_Refd_Def
        (Value : Internal_Refd_Def) return Refd_Def
      is
         Result : constant Refd_Def :=
            Refd_Def (Boxed_Refd_Def.Create_Element);
         Record_Ref : constant Boxed_Refd_Def.Element_Access :=
            Internal_Access (Result);
      begin
            
               Record_Ref.Internal_Def_Name := Wrap_Node (Value.Def_Name.Node, Value.Def_Name.Info).As_Defining_Name;
            
               Record_Ref.Internal_Kind := Value.Kind;
         return Result;
      end;


   
   function Create_Refd_Def (Def_Name : Defining_Name'Class; Kind : Ref_Result_Kind) return Refd_Def
 is
      Result     : constant Refd_Def :=
         Refd_Def (Boxed_Refd_Def.Create_Element);
      Record_Def : constant Boxed_Refd_Def.Element_Access :=
         Internal_Access (Result);
   begin
         
            Record_Def.Internal_Def_Name := Def_Name.As_Defining_Name;
         
            Record_Def.Internal_Kind := Kind;
      return Result;
   end;

         

      
   function Components
     (Self : Shape)
      return Base_Formal_Param_Decl_Array
 is
         Record_Ref : constant Boxed_Shape.Element_Access :=
            Internal_Access (Self);
      begin
            return Record_Ref.Internal_Components
                  .all
            ;
      end;
      
   function Discriminants_Values
     (Self : Shape)
      return Discriminant_Values_Array
 is
         Record_Ref : constant Boxed_Shape.Element_Access :=
            Internal_Access (Self);
      begin
            return Record_Ref.Internal_Discriminants_Values
                  .all
            ;
      end;

   --------------
   -- Refcount --
   --------------

   function Refcount (Self : Internal_Shape_Record) return Positive
   is (Self.Refcount);

   ------------------
   -- Set_Refcount --
   ------------------

   procedure Set_Refcount
     (Self : in out Internal_Shape_Record; Count : Positive) is
   begin
      Self.Refcount := Count;
   end Set_Refcount;

      -------------
      -- Release --
      -------------

      procedure Release (Self : in out Internal_Shape_Record) is
         
      begin
            Free (Self.Internal_Components);
            Free (Self.Internal_Discriminants_Values);
      end Release;

      function To_Public_Shape
        (Value : Internal_Shape) return Shape
      is
         Result : constant Shape :=
            Shape (Boxed_Shape.Create_Element);
         Record_Ref : constant Boxed_Shape.Element_Access :=
            Internal_Access (Result);
      begin
            
               Record_Ref.Internal_Components := new Base_Formal_Param_Decl_Array'(To_Public_Base_Formal_Param_Decl_Array (Value.Components));
            
               Record_Ref.Internal_Discriminants_Values := new Discriminant_Values_Array'(To_Public_Discriminant_Values_Array (Value.Discriminants_Values));
         return Result;
      end;


   
   function Create_Shape (Components : Base_Formal_Param_Decl_Array; Discriminants_Values : Discriminant_Values_Array) return Shape
 is
      Result     : constant Shape :=
         Shape (Boxed_Shape.Create_Element);
      Record_Def : constant Boxed_Shape.Element_Access :=
         Internal_Access (Result);
   begin
         
            Record_Def.Internal_Components := new Base_Formal_Param_Decl_Array'(Components);
         
            Record_Def.Internal_Discriminants_Values := new Discriminant_Values_Array'(Discriminants_Values);
      return Result;
   end;

         

      
   function From_Decl
     (Self : Substitution)
      return Basic_Decl'Class
 is
         Record_Ref : constant Boxed_Substitution.Element_Access :=
            Internal_Access (Self);
      begin
            return Record_Ref.Internal_From_Decl
            ;
      end;
      
   function To_Value
     (Self : Substitution)
      return Big_Integer
 is
         Record_Ref : constant Boxed_Substitution.Element_Access :=
            Internal_Access (Self);
      begin
            return Result : Big_Integer do
               Result.Set (Record_Ref.Internal_To_Value);
            end return;
      end;
      
   function Value_Type
     (Self : Substitution)
      return Base_Type_Decl'Class
 is
         Record_Ref : constant Boxed_Substitution.Element_Access :=
            Internal_Access (Self);
      begin
            return Record_Ref.Internal_Value_Type
            ;
      end;

   --------------
   -- Refcount --
   --------------

   function Refcount (Self : Internal_Substitution_Record) return Positive
   is (Self.Refcount);

   ------------------
   -- Set_Refcount --
   ------------------

   procedure Set_Refcount
     (Self : in out Internal_Substitution_Record; Count : Positive) is
   begin
      Self.Refcount := Count;
   end Set_Refcount;



      function To_Internal_Substitution
        (Value : Substitution) return Internal_Substitution
      is
         Record_Ref : constant Boxed_Substitution.Element_Access :=
            Internal_Access (Value);
         Result     : Internal_Substitution;
      begin
            
            Result.From_Decl := (Record_Ref.Internal_From_Decl.Internal.Node, Record_Ref.Internal_From_Decl.Internal.Info);
            
            Result.To_Value := Create_Big_Integer (Record_Ref.Internal_To_Value);
            
            Result.Value_Type := (Record_Ref.Internal_Value_Type.Internal.Node, Record_Ref.Internal_Value_Type.Internal.Info);
         return Result;
      end;

   
   function Create_Substitution (From_Decl : Basic_Decl'Class; To_Value : Big_Integer; Value_Type : Base_Type_Decl'Class) return Substitution
 is
      Result     : constant Substitution :=
         Substitution (Boxed_Substitution.Create_Element);
      Record_Def : constant Boxed_Substitution.Element_Access :=
         Internal_Access (Result);
   begin
         
            Record_Def.Internal_From_Decl := From_Decl.As_Basic_Decl;
         
            Record_Def.Internal_To_Value.Set (To_Value);
         
            Record_Def.Internal_Value_Type := Value_Type.As_Base_Type_Decl;
      return Result;
   end;






         
   function P_Declarative_Scope
     (Node : Ada_Node'Class) return Declarative_Part is
      


      Property_Result : Bare_Declarative_Part;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Ada_Node_P_Declarative_Scope
            (Bare_Ada_Node (Node.Internal.Node));

         return Wrap_Node (Property_Result, No_Entity_Info).As_Declarative_Part;

   end;

         
   function P_Enclosing_Compilation_Unit
     (Node : Ada_Node'Class) return Compilation_Unit is
      


      Property_Result : Bare_Compilation_Unit;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Ada_Node_P_Enclosing_Compilation_Unit
            (Bare_Ada_Node (Node.Internal.Node));

         return Wrap_Node (Property_Result, No_Entity_Info).As_Compilation_Unit;

   end;

         
   function P_Get_Uninstantiated_Node
     (Node : Ada_Node'Class) return Ada_Node is
      


      Property_Result : Internal_Entity;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Ada_Node_P_Get_Uninstantiated_Node
            (Bare_Ada_Node (Node.Internal.Node));

         return Wrap_Node (Property_Result.Node, Property_Result.Info);

   end;

         
   function P_Complete
     (Node : Ada_Node'Class) return Completion_Item_Iterator is
      


      Property_Result : Internal_Completion_Item_Iterator_Access;

         procedure Free_Internal;
         --  Dec-ref all internal arguments and the property result, when
         --  applicable.

         -------------------
         -- Free_Internal --
         -------------------

         procedure Free_Internal is
         begin
               Dec_Ref (Property_Result);
         end Free_Internal;

   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Ada_Node_P_Complete
            (Bare_Ada_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return Result : constant Completion_Item_Iterator :=
            To_Public_Completion_Item_Iterator (Property_Result)
         do
            Free_Internal;
         end return;

      exception
         when Precondition_Failure | Property_Error =>
            Free_Internal;
            raise;
   end;

         
   function P_Valid_Keywords
     (Node : Ada_Node'Class) return Unbounded_Text_Type_Array is
      


      Property_Result : Symbol_Type_Array_Access;

         procedure Free_Internal;
         --  Dec-ref all internal arguments and the property result, when
         --  applicable.

         -------------------
         -- Free_Internal --
         -------------------

         procedure Free_Internal is
         begin
               Dec_Ref (Property_Result);
         end Free_Internal;

   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Ada_Node_P_Valid_Keywords
            (Bare_Ada_Node (Node.Internal.Node));

         return Result : constant Unbounded_Text_Type_Array :=
            To_Public_Unbounded_Text_Type_Array (Property_Result)
         do
            Free_Internal;
         end return;

      exception
         when Precondition_Failure | Property_Error =>
            Free_Internal;
            raise;
   end;

         
   function P_Generic_Instantiations
     (Node : Ada_Node'Class) return Generic_Instantiation_Array is
      


      Property_Result : Internal_Entity_Generic_Instantiation_Array_Access;

         procedure Free_Internal;
         --  Dec-ref all internal arguments and the property result, when
         --  applicable.

         -------------------
         -- Free_Internal --
         -------------------

         procedure Free_Internal is
         begin
               Dec_Ref (Property_Result);
         end Free_Internal;

   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Ada_Node_P_Generic_Instantiations
            (Bare_Ada_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return Result : constant Generic_Instantiation_Array :=
            To_Public_Generic_Instantiation_Array (Property_Result)
         do
            Free_Internal;
         end return;

      exception
         when Precondition_Failure | Property_Error =>
            Free_Internal;
            raise;
   end;

         
   function P_Semantic_Parent
     (Node : Ada_Node'Class) return Ada_Node is
      


      Property_Result : Internal_Entity;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Ada_Node_P_Semantic_Parent
            (Bare_Ada_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return Wrap_Node (Property_Result.Node, Property_Result.Info);

   end;

         
   function P_Parent_Basic_Decl
     (Node : Ada_Node'Class) return Basic_Decl is
      


      Property_Result : Internal_Entity_Basic_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Ada_Node_P_Parent_Basic_Decl
            (Bare_Ada_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return Wrap_Node (Property_Result.Node, Property_Result.Info).As_Basic_Decl;

   end;

         
   function P_Filter_Is_Imported_By
     (Node : Ada_Node'Class;
      Units : Analysis_Unit_Array;
      Transitive : Boolean) return Analysis_Unit_Array is
      


         Internal_Arg_Units : Internal_Unit_Array_Access;
         Internal_Arg_Transitive : Boolean;
      Property_Result : Internal_Unit_Array_Access;

         procedure Free_Internal;
         --  Dec-ref all internal arguments and the property result, when
         --  applicable.

         -------------------
         -- Free_Internal --
         -------------------

         procedure Free_Internal is
         begin
               Dec_Ref (Internal_Arg_Units);
               Dec_Ref (Property_Result);
         end Free_Internal;

   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Units :=
            To_Internal_Analysis_Unit_Array (Units);
         Internal_Arg_Transitive :=
            Transitive;

      
      Property_Result :=
         Libadalang.Implementation.Extensions.Ada_Node_P_Filter_Is_Imported_By
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Units, Internal_Arg_Transitive);

         return Result : constant Analysis_Unit_Array :=
            To_Public_Analysis_Unit_Array (Property_Result)
         do
            Free_Internal;
         end return;

      exception
         when Precondition_Failure | Property_Error =>
            Free_Internal;
            raise;
   end;

         
   function P_Xref_Entry_Point
     (Node : Ada_Node'Class) return Boolean is
      


      Property_Result : Boolean;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Dispatcher_Ada_Node_P_Xref_Entry_Point
            (Bare_Ada_Node (Node.Internal.Node));

         return Property_Result;

   end;

         
   function P_Resolve_Names
     (Node : Ada_Node'Class) return Boolean is
      


      Property_Result : Boolean;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Ada_Node_P_Resolve_Names
            (Bare_Ada_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return Property_Result;

   end;

         
   function P_Standard_Unit
     (Node : Ada_Node'Class) return Analysis_Unit is
      


      Property_Result : Internal_Unit;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Extensions.Ada_Node_P_Standard_Unit
            (Bare_Ada_Node (Node.Internal.Node));

         return Wrap_Unit (Property_Result);

   end;

         
   function P_Std_Entity
     (Node : Ada_Node'Class;
      Sym : Unbounded_Text_Type) return Ada_Node is
      


         Internal_Arg_Sym : Symbol_Type;
      Property_Result : Internal_Entity;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Sym :=
            Lookup_Symbol (Node.Internal.Node.Unit.Context, To_Text (Sym));

      
      Property_Result :=
         Libadalang.Implementation.Ada_Node_P_Std_Entity
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Sym);

         return Wrap_Node (Property_Result.Node, Property_Result.Info);

   end;

         
   function P_Bool_Type
     (Node : Ada_Node'Class) return Base_Type_Decl is
      


      Property_Result : Internal_Entity_Base_Type_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Ada_Node_P_Bool_Type
            (Bare_Ada_Node (Node.Internal.Node));

         return Wrap_Node (Property_Result.Node, Property_Result.Info).As_Base_Type_Decl;

   end;

         
   function P_Int_Type
     (Node : Ada_Node'Class) return Base_Type_Decl is
      


      Property_Result : Internal_Entity_Base_Type_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Ada_Node_P_Int_Type
            (Bare_Ada_Node (Node.Internal.Node));

         return Wrap_Node (Property_Result.Node, Property_Result.Info).As_Base_Type_Decl;

   end;

         
   function P_Universal_Int_Type
     (Node : Ada_Node'Class) return Ada_Node is
      


      Property_Result : Internal_Entity;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Ada_Node_P_Universal_Int_Type
            (Bare_Ada_Node (Node.Internal.Node));

         return Wrap_Node (Property_Result.Node, Property_Result.Info);

   end;

         
   function P_Universal_Real_Type
     (Node : Ada_Node'Class) return Ada_Node is
      


      Property_Result : Internal_Entity;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Ada_Node_P_Universal_Real_Type
            (Bare_Ada_Node (Node.Internal.Node));

         return Wrap_Node (Property_Result.Node, Property_Result.Info);

   end;

         
   function P_Std_Char_Type
     (Node : Ada_Node'Class) return Base_Type_Decl is
      


      Property_Result : Internal_Entity_Base_Type_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Ada_Node_P_Std_Char_Type
            (Bare_Ada_Node (Node.Internal.Node));

         return Wrap_Node (Property_Result.Node, Property_Result.Info).As_Base_Type_Decl;

   end;

         
   function P_Std_Wide_Char_Type
     (Node : Ada_Node'Class) return Base_Type_Decl is
      


      Property_Result : Internal_Entity_Base_Type_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Ada_Node_P_Std_Wide_Char_Type
            (Bare_Ada_Node (Node.Internal.Node));

         return Wrap_Node (Property_Result.Node, Property_Result.Info).As_Base_Type_Decl;

   end;

         
   function P_Std_Wide_Wide_Char_Type
     (Node : Ada_Node'Class) return Base_Type_Decl is
      


      Property_Result : Internal_Entity_Base_Type_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Ada_Node_P_Std_Wide_Wide_Char_Type
            (Bare_Ada_Node (Node.Internal.Node));

         return Wrap_Node (Property_Result.Node, Property_Result.Info).As_Base_Type_Decl;

   end;

         
   function P_Top_Level_Decl
     (Node : Ada_Node'Class;
      Unit : Analysis_Unit'Class) return Basic_Decl is
      


         Internal_Arg_Unit : Internal_Unit;
      Property_Result : Bare_Basic_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Unit :=
            Unwrap_Unit (Unit);

      
      Property_Result :=
         Libadalang.Implementation.Ada_Node_P_Top_Level_Decl
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Unit);

         return Wrap_Node (Property_Result, No_Entity_Info).As_Basic_Decl;

   end;

         
   function P_Choice_Match
     (Node : Ada_Node'Class;
      Value : Big_Integer) return Boolean is
      


         Internal_Arg_Value : Big_Integer_Type;
      Property_Result : Boolean;

         procedure Free_Internal;
         --  Dec-ref all internal arguments and the property result, when
         --  applicable.

         -------------------
         -- Free_Internal --
         -------------------

         procedure Free_Internal is
         begin
               Dec_Ref (Internal_Arg_Value);
         end Free_Internal;

   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Value :=
            Create_Big_Integer (Value);

      
      Property_Result :=
         Libadalang.Implementation.Ada_Node_P_Choice_Match
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Value, E_Info => Node.Internal.Info);

         return Result : constant Boolean :=
            Property_Result
         do
            Free_Internal;
         end return;

      exception
         when Precondition_Failure | Property_Error =>
            Free_Internal;
            raise;
   end;

         
   function P_Gnat_Xref
     (Node : Ada_Node'Class;
      Imprecise_Fallback : Boolean := False) return Defining_Name is
      


         Internal_Arg_Imprecise_Fallback : Boolean;
      Property_Result : Internal_Entity_Defining_Name;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Imprecise_Fallback :=
            Imprecise_Fallback;

      
      Property_Result :=
         Libadalang.Implementation.Ada_Node_P_Gnat_Xref
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Imprecise_Fallback, E_Info => Node.Internal.Info);

         return Wrap_Node (Property_Result.Node, Property_Result.Info).As_Defining_Name;

   end;

         
   function Parent
     (Node : Ada_Node'Class) return Ada_Node is
      


      Property_Result : Internal_Entity;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Parent
            (Bare_Ada_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return Wrap_Node (Property_Result.Node, Property_Result.Info);

   end;

         
   function Parents
     (Node : Ada_Node'Class;
      With_Self : Boolean := True) return Ada_Node_Array is
      


         Internal_Arg_With_Self : Boolean;
      Property_Result : Internal_Entity_Array_Access;

         procedure Free_Internal;
         --  Dec-ref all internal arguments and the property result, when
         --  applicable.

         -------------------
         -- Free_Internal --
         -------------------

         procedure Free_Internal is
         begin
               Dec_Ref (Property_Result);
         end Free_Internal;

   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_With_Self :=
            With_Self;

      
      Property_Result :=
         Libadalang.Implementation.Parents
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_With_Self, E_Info => Node.Internal.Info);

         return Result : constant Ada_Node_Array :=
            To_Public_Ada_Node_Array (Property_Result)
         do
            Free_Internal;
         end return;

      exception
         when Precondition_Failure | Property_Error =>
            Free_Internal;
            raise;
   end;

         
   function Children
     (Node : Ada_Node'Class) return Ada_Node_Array is
      


      Property_Result : Internal_Entity_Array_Access;

         procedure Free_Internal;
         --  Dec-ref all internal arguments and the property result, when
         --  applicable.

         -------------------
         -- Free_Internal --
         -------------------

         procedure Free_Internal is
         begin
               Dec_Ref (Property_Result);
         end Free_Internal;

   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Children
            (Bare_Ada_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return Result : constant Ada_Node_Array :=
            To_Public_Ada_Node_Array (Property_Result)
         do
            Free_Internal;
         end return;

      exception
         when Precondition_Failure | Property_Error =>
            Free_Internal;
            raise;
   end;

         
   function Token_Start
     (Node : Ada_Node'Class) return Token_Reference is
      


      Property_Result : Token_Reference;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Token_Start
            (Bare_Ada_Node (Node.Internal.Node));

         return Property_Result;

   end;

         
   function Token_End
     (Node : Ada_Node'Class) return Token_Reference is
      


      Property_Result : Token_Reference;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Token_End
            (Bare_Ada_Node (Node.Internal.Node));

         return Property_Result;

   end;

         
   function Child_Index
     (Node : Ada_Node'Class) return Integer is
      


      Property_Result : Integer;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Child_Index
            (Bare_Ada_Node (Node.Internal.Node));

         return Property_Result;

   end;

         
   function Previous_Sibling
     (Node : Ada_Node'Class) return Ada_Node is
      


      Property_Result : Internal_Entity;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Previous_Sibling
            (Bare_Ada_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return Wrap_Node (Property_Result.Node, Property_Result.Info);

   end;

         
   function Next_Sibling
     (Node : Ada_Node'Class) return Ada_Node is
      


      Property_Result : Internal_Entity;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Next_Sibling
            (Bare_Ada_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return Wrap_Node (Property_Result.Node, Property_Result.Info);

   end;

         
   function Unit
     (Node : Ada_Node'Class) return Analysis_Unit is
      


      Property_Result : Internal_Unit;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Unit
            (Bare_Ada_Node (Node.Internal.Node));

         return Wrap_Unit (Property_Result);

   end;

         
   function Is_Ghost
     (Node : Ada_Node'Class) return Boolean is
      


      Property_Result : Boolean;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Is_Ghost
            (Bare_Ada_Node (Node.Internal.Node));

         return Property_Result;

   end;

         
   function Full_Sloc_Image
     (Node : Ada_Node'Class) return Text_Type is
      


      Property_Result : String_Type;

         procedure Free_Internal;
         --  Dec-ref all internal arguments and the property result, when
         --  applicable.

         -------------------
         -- Free_Internal --
         -------------------

         procedure Free_Internal is
         begin
               Dec_Ref (Property_Result);
         end Free_Internal;

   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Full_Sloc_Image
            (Bare_Ada_Node (Node.Internal.Node));

         return Result : constant Text_Type :=
            Property_Result.Content
         do
            Free_Internal;
         end return;

      exception
         when Precondition_Failure | Property_Error =>
            Free_Internal;
            raise;
   end;






         
   function P_Expression_Type
     (Node : Expr'Class) return Base_Type_Decl is
      


      Property_Result : Internal_Entity_Base_Type_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Expr_P_Expression_Type
            (Bare_Ada_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return Wrap_Node (Property_Result.Node, Property_Result.Info).As_Base_Type_Decl;

   end;

         
   function P_Expected_Expression_Type
     (Node : Expr'Class) return Base_Type_Decl is
      


      Property_Result : Internal_Entity_Base_Type_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Expr_P_Expected_Expression_Type
            (Bare_Ada_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return Wrap_Node (Property_Result.Node, Property_Result.Info).As_Base_Type_Decl;

   end;

         
   function P_Is_Dynamically_Tagged
     (Node : Expr'Class;
      Imprecise_Fallback : Boolean := False) return Boolean is
      


         Internal_Arg_Imprecise_Fallback : Boolean;
      Property_Result : Boolean;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Imprecise_Fallback :=
            Imprecise_Fallback;

      
      Property_Result :=
         Libadalang.Implementation.Expr_P_Is_Dynamically_Tagged
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Imprecise_Fallback, E_Info => Node.Internal.Info);

         return Property_Result;

   end;

         
   function P_Is_Dispatching_Call
     (Node : Expr'Class;
      Imprecise_Fallback : Boolean := False) return Boolean is
      


         Internal_Arg_Imprecise_Fallback : Boolean;
      Property_Result : Boolean;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Imprecise_Fallback :=
            Imprecise_Fallback;

      
      Property_Result :=
         Libadalang.Implementation.Dispatcher_Expr_P_Is_Dispatching_Call
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Imprecise_Fallback, E_Info => Node.Internal.Info);

         return Property_Result;

   end;

         
   function P_Is_Static_Expr
     (Node : Expr'Class;
      Imprecise_Fallback : Boolean := False) return Boolean is
      


         Internal_Arg_Imprecise_Fallback : Boolean;
      Property_Result : Boolean;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Imprecise_Fallback :=
            Imprecise_Fallback;

      
      Property_Result :=
         Libadalang.Implementation.Expr_P_Is_Static_Expr
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Imprecise_Fallback, E_Info => Node.Internal.Info);

         return Property_Result;

   end;

         
   function P_First_Corresponding_Decl
     (Node : Expr'Class) return Basic_Decl is
      


      Property_Result : Internal_Entity_Basic_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Dispatcher_Expr_P_First_Corresponding_Decl
            (Bare_Ada_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return Wrap_Node (Property_Result.Node, Property_Result.Info).As_Basic_Decl;

   end;

         
   function P_Eval_As_Int
     (Node : Expr'Class) return Big_Integer is
      


      Property_Result : Big_Integer_Type;

         procedure Free_Internal;
         --  Dec-ref all internal arguments and the property result, when
         --  applicable.

         -------------------
         -- Free_Internal --
         -------------------

         procedure Free_Internal is
         begin
               Dec_Ref (Property_Result);
         end Free_Internal;

   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Expr_P_Eval_As_Int
            (Bare_Ada_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return Result : constant Big_Integer :=
            Create_Public_Big_Integer (Property_Result)
         do
            Free_Internal;
         end return;

      exception
         when Precondition_Failure | Property_Error =>
            Free_Internal;
            raise;
   end;

         
   function P_Eval_As_Int_In_Env
     (Node : Expr'Class;
      Env : Substitution_Array) return Big_Integer is
      


         Internal_Arg_Env : Internal_Substitution_Array_Access;
      Property_Result : Big_Integer_Type;

         procedure Free_Internal;
         --  Dec-ref all internal arguments and the property result, when
         --  applicable.

         -------------------
         -- Free_Internal --
         -------------------

         procedure Free_Internal is
         begin
               Dec_Ref (Internal_Arg_Env);
               Dec_Ref (Property_Result);
         end Free_Internal;

   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Env :=
            To_Internal_Substitution_Array (Env);

      
      Property_Result :=
         Libadalang.Implementation.Extensions.Expr_P_Eval_As_Int_In_Env
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Env, E_Info => Node.Internal.Info);

         return Result : constant Big_Integer :=
            Create_Public_Big_Integer (Property_Result)
         do
            Free_Internal;
         end return;

      exception
         when Precondition_Failure | Property_Error =>
            Free_Internal;
            raise;
   end;

         
   function P_Eval_As_String
     (Node : Expr'Class) return Text_Type is
      


      Property_Result : String_Type;

         procedure Free_Internal;
         --  Dec-ref all internal arguments and the property result, when
         --  applicable.

         -------------------
         -- Free_Internal --
         -------------------

         procedure Free_Internal is
         begin
               Dec_Ref (Property_Result);
         end Free_Internal;

   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Expr_P_Eval_As_String
            (Bare_Ada_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return Result : constant Text_Type :=
            Property_Result.Content
         do
            Free_Internal;
         end return;

      exception
         when Precondition_Failure | Property_Error =>
            Free_Internal;
            raise;
   end;

         
   function P_Eval_As_String_In_Env
     (Node : Expr'Class;
      Env : Substitution_Array) return Text_Type is
      


         Internal_Arg_Env : Internal_Substitution_Array_Access;
      Property_Result : String_Type;

         procedure Free_Internal;
         --  Dec-ref all internal arguments and the property result, when
         --  applicable.

         -------------------
         -- Free_Internal --
         -------------------

         procedure Free_Internal is
         begin
               Dec_Ref (Internal_Arg_Env);
               Dec_Ref (Property_Result);
         end Free_Internal;

   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Env :=
            To_Internal_Substitution_Array (Env);

      
      Property_Result :=
         Libadalang.Implementation.Extensions.Expr_P_Eval_As_String_In_Env
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Env, E_Info => Node.Internal.Info);

         return Result : constant Text_Type :=
            Property_Result.Content
         do
            Free_Internal;
         end return;

      exception
         when Precondition_Failure | Property_Error =>
            Free_Internal;
            raise;
   end;

         
   function P_Matching_Nodes
     (Node : Expr'Class) return Ada_Node_Array is
      


      Property_Result : Internal_Entity_Array_Access;

         procedure Free_Internal;
         --  Dec-ref all internal arguments and the property result, when
         --  applicable.

         -------------------
         -- Free_Internal --
         -------------------

         procedure Free_Internal is
         begin
               Dec_Ref (Property_Result);
         end Free_Internal;

   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Expr_P_Matching_Nodes
            (Bare_Ada_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return Result : constant Ada_Node_Array :=
            To_Public_Ada_Node_Array (Property_Result)
         do
            Free_Internal;
         end return;

      exception
         when Precondition_Failure | Property_Error =>
            Free_Internal;
            raise;
   end;





         
   

   function F_Aspects
     (Node : Basic_Decl'Class) return Aspect_Spec
   is
      Result : Bare_Aspect_Spec;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Basic_Decl_F_Aspects (Node.Internal.Node);
         if Result = null then
            return No_Aspect_Spec;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Aspects;



         
   function P_Is_Formal
     (Node : Basic_Decl'Class) return Boolean is
      


      Property_Result : Boolean;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Basic_Decl_P_Is_Formal
            (Bare_Ada_Node (Node.Internal.Node));

         return Property_Result;

   end;

         
   function P_Doc_Annotations
     (Node : Basic_Decl'Class) return Doc_Annotation_Array is
      


      Property_Result : Internal_Doc_Annotation_Array_Access;

         procedure Free_Internal;
         --  Dec-ref all internal arguments and the property result, when
         --  applicable.

         -------------------
         -- Free_Internal --
         -------------------

         procedure Free_Internal is
         begin
               Dec_Ref (Property_Result);
         end Free_Internal;

   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Extensions.Basic_Decl_P_Doc_Annotations
            (Bare_Ada_Node (Node.Internal.Node));

         return Result : constant Doc_Annotation_Array :=
            To_Public_Doc_Annotation_Array (Property_Result)
         do
            Free_Internal;
         end return;

      exception
         when Precondition_Failure | Property_Error =>
            Free_Internal;
            raise;
   end;

         
   function P_Doc
     (Node : Basic_Decl'Class) return Text_Type is
      


      Property_Result : String_Type;

         procedure Free_Internal;
         --  Dec-ref all internal arguments and the property result, when
         --  applicable.

         -------------------
         -- Free_Internal --
         -------------------

         procedure Free_Internal is
         begin
               Dec_Ref (Property_Result);
         end Free_Internal;

   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Extensions.Basic_Decl_P_Doc
            (Bare_Ada_Node (Node.Internal.Node));

         return Result : constant Text_Type :=
            Property_Result.Content
         do
            Free_Internal;
         end return;

      exception
         when Precondition_Failure | Property_Error =>
            Free_Internal;
            raise;
   end;

         
   function P_Previous_Part_For_Decl
     (Node : Basic_Decl'Class;
      Imprecise_Fallback : Boolean := False) return Basic_Decl is
      


         Internal_Arg_Imprecise_Fallback : Boolean;
      Property_Result : Internal_Entity_Basic_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Imprecise_Fallback :=
            Imprecise_Fallback;

      
      Property_Result :=
         Libadalang.Implementation.Dispatcher_Basic_Decl_P_Previous_Part_For_Decl
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Imprecise_Fallback, E_Info => Node.Internal.Info);

         return Wrap_Node (Property_Result.Node, Property_Result.Info).As_Basic_Decl;

   end;

         
   function P_Canonical_Part
     (Node : Basic_Decl'Class;
      Imprecise_Fallback : Boolean := False) return Basic_Decl is
      


         Internal_Arg_Imprecise_Fallback : Boolean;
      Property_Result : Internal_Entity_Basic_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Imprecise_Fallback :=
            Imprecise_Fallback;

      
      Property_Result :=
         Libadalang.Implementation.Basic_Decl_P_Canonical_Part
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Imprecise_Fallback, E_Info => Node.Internal.Info);

         return Wrap_Node (Property_Result.Node, Property_Result.Info).As_Basic_Decl;

   end;

         
   function P_All_Parts
     (Node : Basic_Decl'Class;
      Imprecise_Fallback : Boolean := False) return Basic_Decl_Array is
      


         Internal_Arg_Imprecise_Fallback : Boolean;
      Property_Result : Internal_Entity_Basic_Decl_Array_Access;

         procedure Free_Internal;
         --  Dec-ref all internal arguments and the property result, when
         --  applicable.

         -------------------
         -- Free_Internal --
         -------------------

         procedure Free_Internal is
         begin
               Dec_Ref (Property_Result);
         end Free_Internal;

   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Imprecise_Fallback :=
            Imprecise_Fallback;

      
      Property_Result :=
         Libadalang.Implementation.Basic_Decl_P_All_Parts
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Imprecise_Fallback, E_Info => Node.Internal.Info);

         return Result : constant Basic_Decl_Array :=
            To_Public_Basic_Decl_Array (Property_Result)
         do
            Free_Internal;
         end return;

      exception
         when Precondition_Failure | Property_Error =>
            Free_Internal;
            raise;
   end;

         
   function P_Is_Static_Decl
     (Node : Basic_Decl'Class;
      Imprecise_Fallback : Boolean := False) return Boolean is
      


         Internal_Arg_Imprecise_Fallback : Boolean;
      Property_Result : Boolean;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Imprecise_Fallback :=
            Imprecise_Fallback;

      
      Property_Result :=
         Libadalang.Implementation.Dispatcher_Basic_Decl_P_Is_Static_Decl
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Imprecise_Fallback, E_Info => Node.Internal.Info);

         return Property_Result;

   end;

         
   function P_Get_Aspect_Assoc
     (Node : Basic_Decl'Class;
      Name : Unbounded_Text_Type) return Aspect_Assoc is
      


         Internal_Arg_Name : Symbol_Type;
      Property_Result : Internal_Entity_Aspect_Assoc;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Name :=
            Lookup_Symbol (Node.Internal.Node.Unit.Context, To_Text (Name));

      
      Property_Result :=
         Libadalang.Implementation.Dispatcher_Basic_Decl_P_Get_Aspect_Assoc
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Name, E_Info => Node.Internal.Info);

         return Wrap_Node (Property_Result.Node, Property_Result.Info).As_Aspect_Assoc;

   end;

         
   function P_Get_Aspect_Spec_Expr
     (Node : Basic_Decl'Class;
      Name : Unbounded_Text_Type) return Expr is
      


         Internal_Arg_Name : Symbol_Type;
      Property_Result : Internal_Entity_Expr;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Name :=
            Lookup_Symbol (Node.Internal.Node.Unit.Context, To_Text (Name));

      
      Property_Result :=
         Libadalang.Implementation.Basic_Decl_P_Get_Aspect_Spec_Expr
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Name, E_Info => Node.Internal.Info);

         return Wrap_Node (Property_Result.Node, Property_Result.Info).As_Expr;

   end;

         
   function P_Get_Aspect
     (Node : Basic_Decl'Class;
      Name : Unbounded_Text_Type;
      Imprecise_Fallback : Boolean := False) return Aspect is
      


         Internal_Arg_Name : Symbol_Type;
         Internal_Arg_Imprecise_Fallback : Boolean;
      Property_Result : Internal_Aspect;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Name :=
            Lookup_Symbol (Node.Internal.Node.Unit.Context, To_Text (Name));
         Internal_Arg_Imprecise_Fallback :=
            Imprecise_Fallback;

      
      Property_Result :=
         Libadalang.Implementation.Basic_Decl_P_Get_Aspect
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Name, Internal_Arg_Imprecise_Fallback, E_Info => Node.Internal.Info);

         return To_Public_Aspect (Property_Result);

   end;

         
   function P_Has_Aspect
     (Node : Basic_Decl'Class;
      Name : Unbounded_Text_Type;
      Imprecise_Fallback : Boolean := False) return Boolean is
      


         Internal_Arg_Name : Symbol_Type;
         Internal_Arg_Imprecise_Fallback : Boolean;
      Property_Result : Boolean;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Name :=
            Lookup_Symbol (Node.Internal.Node.Unit.Context, To_Text (Name));
         Internal_Arg_Imprecise_Fallback :=
            Imprecise_Fallback;

      
      Property_Result :=
         Libadalang.Implementation.Basic_Decl_P_Has_Aspect
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Name, Internal_Arg_Imprecise_Fallback, E_Info => Node.Internal.Info);

         return Property_Result;

   end;

         
   function P_Get_Pragma
     (Node : Basic_Decl'Class;
      Name : Unbounded_Text_Type) return Pragma_Node is
      


         Internal_Arg_Name : Symbol_Type;
      Property_Result : Internal_Entity_Pragma_Node;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Name :=
            Lookup_Symbol (Node.Internal.Node.Unit.Context, To_Text (Name));

      
      Property_Result :=
         Libadalang.Implementation.Basic_Decl_P_Get_Pragma
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Name, E_Info => Node.Internal.Info);

         return Wrap_Node (Property_Result.Node, Property_Result.Info).As_Pragma_Node;

   end;

         
   function P_Get_Representation_Clause
     (Node : Basic_Decl'Class;
      Name : Unbounded_Text_Type;
      Imprecise_Fallback : Boolean := False) return Attribute_Def_Clause is
      


         Internal_Arg_Name : Symbol_Type;
         Internal_Arg_Imprecise_Fallback : Boolean;
      Property_Result : Internal_Entity_Attribute_Def_Clause;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Name :=
            Lookup_Symbol (Node.Internal.Node.Unit.Context, To_Text (Name));
         Internal_Arg_Imprecise_Fallback :=
            Imprecise_Fallback;

      
      Property_Result :=
         Libadalang.Implementation.Basic_Decl_P_Get_Representation_Clause
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Name, Internal_Arg_Imprecise_Fallback, E_Info => Node.Internal.Info);

         return Wrap_Node (Property_Result.Node, Property_Result.Info).As_Attribute_Def_Clause;

   end;

         
   function P_Get_At_Clause
     (Node : Basic_Decl'Class;
      Imprecise_Fallback : Boolean := False) return At_Clause is
      


         Internal_Arg_Imprecise_Fallback : Boolean;
      Property_Result : Internal_Entity_At_Clause;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Imprecise_Fallback :=
            Imprecise_Fallback;

      
      Property_Result :=
         Libadalang.Implementation.Basic_Decl_P_Get_At_Clause
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Imprecise_Fallback, E_Info => Node.Internal.Info);

         return Wrap_Node (Property_Result.Node, Property_Result.Info).As_At_Clause;

   end;

         
   function P_Is_Imported
     (Node : Basic_Decl'Class) return Boolean is
      


      Property_Result : Boolean;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Dispatcher_Basic_Decl_P_Is_Imported
            (Bare_Ada_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return Property_Result;

   end;

         
   function P_Is_Ghost_Code
     (Node : Basic_Decl'Class) return Boolean is
      


      Property_Result : Boolean;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Basic_Decl_P_Is_Ghost_Code
            (Bare_Ada_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return Property_Result;

   end;

         
   function P_Is_Compilation_Unit_Root
     (Node : Basic_Decl'Class) return Boolean is
      


      Property_Result : Boolean;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Basic_Decl_P_Is_Compilation_Unit_Root
            (Bare_Ada_Node (Node.Internal.Node));

         return Property_Result;

   end;

         
   function P_Is_Visible
     (Node : Basic_Decl'Class;
      From_Node : Ada_Node'Class) return Boolean is
      


         Internal_Arg_From_Node : Internal_Entity;
      Property_Result : Boolean;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_From_Node :=
            (From_Node.Internal.Node, From_Node.Internal.Info);

      
      Property_Result :=
         Libadalang.Implementation.Basic_Decl_P_Is_Visible
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_From_Node, E_Info => Node.Internal.Info);

         return Property_Result;

   end;

         
   function P_Base_Subp_Declarations
     (Node : Basic_Decl'Class;
      Imprecise_Fallback : Boolean := False) return Basic_Decl_Array is
      


         Internal_Arg_Imprecise_Fallback : Boolean;
      Property_Result : Internal_Entity_Basic_Decl_Array_Access;

         procedure Free_Internal;
         --  Dec-ref all internal arguments and the property result, when
         --  applicable.

         -------------------
         -- Free_Internal --
         -------------------

         procedure Free_Internal is
         begin
               Dec_Ref (Property_Result);
         end Free_Internal;

   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Imprecise_Fallback :=
            Imprecise_Fallback;

      
      Property_Result :=
         Libadalang.Implementation.Basic_Decl_P_Base_Subp_Declarations
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Imprecise_Fallback, E_Info => Node.Internal.Info);

         return Result : constant Basic_Decl_Array :=
            To_Public_Basic_Decl_Array (Property_Result)
         do
            Free_Internal;
         end return;

      exception
         when Precondition_Failure | Property_Error =>
            Free_Internal;
            raise;
   end;

         
   function P_Root_Subp_Declarations
     (Node : Basic_Decl'Class;
      Origin : Ada_Node'Class := No_Ada_Node;
      Imprecise_Fallback : Boolean := False) return Basic_Decl_Array is
      


         Internal_Arg_Origin : Bare_Ada_Node;
         Internal_Arg_Imprecise_Fallback : Boolean;
      Property_Result : Internal_Entity_Basic_Decl_Array_Access;

         procedure Free_Internal;
         --  Dec-ref all internal arguments and the property result, when
         --  applicable.

         -------------------
         -- Free_Internal --
         -------------------

         procedure Free_Internal is
         begin
               Dec_Ref (Property_Result);
         end Free_Internal;

   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Origin :=
            Origin.Internal.Node;
         Internal_Arg_Imprecise_Fallback :=
            Imprecise_Fallback;

      
      Property_Result :=
         Libadalang.Implementation.Basic_Decl_P_Root_Subp_Declarations
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Origin, Internal_Arg_Imprecise_Fallback, E_Info => Node.Internal.Info);

         return Result : constant Basic_Decl_Array :=
            To_Public_Basic_Decl_Array (Property_Result)
         do
            Free_Internal;
         end return;

      exception
         when Precondition_Failure | Property_Error =>
            Free_Internal;
            raise;
   end;

         
   function P_Find_All_Overrides
     (Node : Basic_Decl'Class;
      Units : Analysis_Unit_Array;
      Imprecise_Fallback : Boolean := False) return Basic_Decl_Array is
      


         Internal_Arg_Units : Internal_Unit_Array_Access;
         Internal_Arg_Imprecise_Fallback : Boolean;
      Property_Result : Internal_Entity_Basic_Decl_Array_Access;

         procedure Free_Internal;
         --  Dec-ref all internal arguments and the property result, when
         --  applicable.

         -------------------
         -- Free_Internal --
         -------------------

         procedure Free_Internal is
         begin
               Dec_Ref (Internal_Arg_Units);
               Dec_Ref (Property_Result);
         end Free_Internal;

   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Units :=
            To_Internal_Analysis_Unit_Array (Units);
         Internal_Arg_Imprecise_Fallback :=
            Imprecise_Fallback;

      
      Property_Result :=
         Libadalang.Implementation.Basic_Decl_P_Find_All_Overrides
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Units, Internal_Arg_Imprecise_Fallback, E_Info => Node.Internal.Info);

         return Result : constant Basic_Decl_Array :=
            To_Public_Basic_Decl_Array (Property_Result)
         do
            Free_Internal;
         end return;

      exception
         when Precondition_Failure | Property_Error =>
            Free_Internal;
            raise;
   end;

         
   function P_Defining_Names
     (Node : Basic_Decl'Class) return Defining_Name_Array is
      


      Property_Result : Internal_Entity_Defining_Name_Array_Access;

         procedure Free_Internal;
         --  Dec-ref all internal arguments and the property result, when
         --  applicable.

         -------------------
         -- Free_Internal --
         -------------------

         procedure Free_Internal is
         begin
               Dec_Ref (Property_Result);
         end Free_Internal;

   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Dispatcher_Basic_Decl_P_Defining_Names
            (Bare_Ada_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return Result : constant Defining_Name_Array :=
            To_Public_Defining_Name_Array (Property_Result)
         do
            Free_Internal;
         end return;

      exception
         when Precondition_Failure | Property_Error =>
            Free_Internal;
            raise;
   end;

         
   function P_Defining_Name
     (Node : Basic_Decl'Class) return Defining_Name is
      


      Property_Result : Internal_Entity_Defining_Name;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Basic_Decl_P_Defining_Name
            (Bare_Ada_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return Wrap_Node (Property_Result.Node, Property_Result.Info).As_Defining_Name;

   end;

         
   function P_Type_Expression
     (Node : Basic_Decl'Class) return Type_Expr is
      


      Property_Result : Internal_Entity_Type_Expr;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Dispatcher_Basic_Decl_P_Type_Expression
            (Bare_Ada_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return Wrap_Node (Property_Result.Node, Property_Result.Info).As_Type_Expr;

   end;

         
   function P_Subp_Spec_Or_Null
     (Node : Basic_Decl'Class;
      Follow_Generic : Boolean := True) return Base_Subp_Spec is
      


         Internal_Arg_Follow_Generic : Boolean;
      Property_Result : Internal_Entity_Base_Subp_Spec;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Follow_Generic :=
            Follow_Generic;

      
      Property_Result :=
         Libadalang.Implementation.Basic_Decl_P_Subp_Spec_Or_Null
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Follow_Generic, E_Info => Node.Internal.Info);

         return Wrap_Node (Property_Result.Node, Property_Result.Info).As_Base_Subp_Spec;

   end;

         
   function P_Is_Subprogram
     (Node : Basic_Decl'Class) return Boolean is
      


      Property_Result : Boolean;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Basic_Decl_P_Is_Subprogram
            (Bare_Ada_Node (Node.Internal.Node));

         return Property_Result;

   end;

         
   function P_Relative_Name
     (Node : Basic_Decl'Class) return Single_Tok_Node is
      


      Property_Result : Internal_Entity_Single_Tok_Node;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Basic_Decl_P_Relative_Name
            (Bare_Ada_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return Wrap_Node (Property_Result.Node, Property_Result.Info).As_Single_Tok_Node;

   end;

         
   function P_Relative_Name_Text
     (Node : Basic_Decl'Class) return Unbounded_Text_Type is
      


      Property_Result : Symbol_Type;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Basic_Decl_P_Relative_Name_Text
            (Bare_Ada_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return To_Unbounded_Text (Image (Property_Result));

   end;

         
   function P_Next_Part_For_Decl
     (Node : Basic_Decl'Class;
      Imprecise_Fallback : Boolean := False) return Basic_Decl is
      


         Internal_Arg_Imprecise_Fallback : Boolean;
      Property_Result : Internal_Entity_Basic_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Imprecise_Fallback :=
            Imprecise_Fallback;

      
      Property_Result :=
         Libadalang.Implementation.Dispatcher_Basic_Decl_P_Next_Part_For_Decl
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Imprecise_Fallback, E_Info => Node.Internal.Info);

         return Wrap_Node (Property_Result.Node, Property_Result.Info).As_Basic_Decl;

   end;

         
   function P_Body_Part_For_Decl
     (Node : Basic_Decl'Class;
      Imprecise_Fallback : Boolean := False) return Body_Node is
      


         Internal_Arg_Imprecise_Fallback : Boolean;
      Property_Result : Internal_Entity_Body_Node;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Imprecise_Fallback :=
            Imprecise_Fallback;

      
      Property_Result :=
         Libadalang.Implementation.Basic_Decl_P_Body_Part_For_Decl
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Imprecise_Fallback, E_Info => Node.Internal.Info);

         return Wrap_Node (Property_Result.Node, Property_Result.Info).As_Body_Node;

   end;

         
   function P_Most_Visible_Part
     (Node : Basic_Decl'Class;
      Origin : Ada_Node'Class;
      Imprecise_Fallback : Boolean := False) return Basic_Decl is
      


         Internal_Arg_Origin : Bare_Ada_Node;
         Internal_Arg_Imprecise_Fallback : Boolean;
      Property_Result : Internal_Entity_Basic_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Origin :=
            Origin.Internal.Node;
         Internal_Arg_Imprecise_Fallback :=
            Imprecise_Fallback;

      
      Property_Result :=
         Libadalang.Implementation.Basic_Decl_P_Most_Visible_Part
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Origin, Internal_Arg_Imprecise_Fallback, E_Info => Node.Internal.Info);

         return Wrap_Node (Property_Result.Node, Property_Result.Info).As_Basic_Decl;

   end;

         
   function P_Fully_Qualified_Name_Array
     (Node : Basic_Decl'Class;
      Include_Profile : Boolean := False) return Unbounded_Text_Type_Array is
      


         Internal_Arg_Include_Profile : Boolean;
      Property_Result : Symbol_Type_Array_Access;

         procedure Free_Internal;
         --  Dec-ref all internal arguments and the property result, when
         --  applicable.

         -------------------
         -- Free_Internal --
         -------------------

         procedure Free_Internal is
         begin
               Dec_Ref (Property_Result);
         end Free_Internal;

   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Include_Profile :=
            Include_Profile;

      
      Property_Result :=
         Libadalang.Implementation.Basic_Decl_P_Fully_Qualified_Name_Array
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Include_Profile, E_Info => Node.Internal.Info);

         return Result : constant Unbounded_Text_Type_Array :=
            To_Public_Unbounded_Text_Type_Array (Property_Result)
         do
            Free_Internal;
         end return;

      exception
         when Precondition_Failure | Property_Error =>
            Free_Internal;
            raise;
   end;

         
   function P_Fully_Qualified_Name
     (Node : Basic_Decl'Class) return Text_Type is
      


      Property_Result : String_Type;

         procedure Free_Internal;
         --  Dec-ref all internal arguments and the property result, when
         --  applicable.

         -------------------
         -- Free_Internal --
         -------------------

         procedure Free_Internal is
         begin
               Dec_Ref (Property_Result);
         end Free_Internal;

   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Basic_Decl_P_Fully_Qualified_Name
            (Bare_Ada_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return Result : constant Text_Type :=
            Property_Result.Content
         do
            Free_Internal;
         end return;

      exception
         when Precondition_Failure | Property_Error =>
            Free_Internal;
            raise;
   end;

         
   function P_Canonical_Fully_Qualified_Name
     (Node : Basic_Decl'Class) return Text_Type is
      


      Property_Result : String_Type;

         procedure Free_Internal;
         --  Dec-ref all internal arguments and the property result, when
         --  applicable.

         -------------------
         -- Free_Internal --
         -------------------

         procedure Free_Internal is
         begin
               Dec_Ref (Property_Result);
         end Free_Internal;

   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Basic_Decl_P_Canonical_Fully_Qualified_Name
            (Bare_Ada_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return Result : constant Text_Type :=
            Property_Result.Content
         do
            Free_Internal;
         end return;

      exception
         when Precondition_Failure | Property_Error =>
            Free_Internal;
            raise;
   end;

         
   function P_Unique_Identifying_Name
     (Node : Basic_Decl'Class) return Text_Type is
      


      Property_Result : String_Type;

         procedure Free_Internal;
         --  Dec-ref all internal arguments and the property result, when
         --  applicable.

         -------------------
         -- Free_Internal --
         -------------------

         procedure Free_Internal is
         begin
               Dec_Ref (Property_Result);
         end Free_Internal;

   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Basic_Decl_P_Unique_Identifying_Name
            (Bare_Ada_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return Result : constant Text_Type :=
            Property_Result.Content
         do
            Free_Internal;
         end return;

      exception
         when Precondition_Failure | Property_Error =>
            Free_Internal;
            raise;
   end;

         
   function P_Is_Constant_Object
     (Node : Basic_Decl'Class) return Boolean is
      


      Property_Result : Boolean;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Dispatcher_Basic_Decl_P_Is_Constant_Object
            (Bare_Ada_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return Property_Result;

   end;









         

         function Ada_Node_List_First (Node : Ada_Node_List) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return 1;
         end;

         function Ada_Node_List_Next
           (Node : Ada_Node_List; Cursor : Positive) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor + 1;
         end;

         function Ada_Node_List_Has_Element
           (Node : Ada_Node_List; Cursor : Positive) return Boolean is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor in 1 .. Node.Children_Count;
         end;

         function Ada_Node_List_Element
           (Node : Ada_Node_List; Cursor : Positive) return Ada_Node'Class
         is
            Child : Ada_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Child := Node.Child (Cursor);
            return Ada_Node'(Child.As_Ada_Node);
         end;












         
   function P_Enclosing_Defining_Name
     (Node : Name'Class) return Defining_Name is
      


      Property_Result : Internal_Entity_Defining_Name;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Name_P_Enclosing_Defining_Name
            (Bare_Ada_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return Wrap_Node (Property_Result.Node, Property_Result.Info).As_Defining_Name;

   end;

         
   function P_Is_Defining
     (Node : Name'Class) return Boolean is
      


      Property_Result : Boolean;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Name_P_Is_Defining
            (Bare_Ada_Node (Node.Internal.Node));

         return Property_Result;

   end;

         
   function P_Name_Is
     (Node : Name'Class;
      Sym : Unbounded_Text_Type) return Boolean is
      


         Internal_Arg_Sym : Symbol_Type;
      Property_Result : Boolean;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Sym :=
            Lookup_Symbol (Node.Internal.Node.Unit.Context, To_Text (Sym));

      
      Property_Result :=
         Libadalang.Implementation.Name_P_Name_Is
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Sym);

         return Property_Result;

   end;

         
   function P_Is_Direct_Call
     (Node : Name'Class) return Boolean is
      


      Property_Result : Boolean;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Name_P_Is_Direct_Call
            (Bare_Ada_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return Property_Result;

   end;

         
   function P_Is_Access_Call
     (Node : Name'Class) return Boolean is
      


      Property_Result : Boolean;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Name_P_Is_Access_Call
            (Bare_Ada_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return Property_Result;

   end;

         
   function P_Is_Call
     (Node : Name'Class) return Boolean is
      


      Property_Result : Boolean;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Name_P_Is_Call
            (Bare_Ada_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return Property_Result;

   end;

         
   function P_Is_Dot_Call
     (Node : Name'Class;
      Imprecise_Fallback : Boolean := False) return Boolean is
      


         Internal_Arg_Imprecise_Fallback : Boolean;
      Property_Result : Boolean;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Imprecise_Fallback :=
            Imprecise_Fallback;

      
      Property_Result :=
         Libadalang.Implementation.Name_P_Is_Dot_Call
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Imprecise_Fallback, E_Info => Node.Internal.Info);

         return Property_Result;

   end;

         
   function P_Failsafe_Referenced_Def_Name
     (Node : Name'Class;
      Imprecise_Fallback : Boolean := False) return Refd_Def is
      


         Internal_Arg_Imprecise_Fallback : Boolean;
      Property_Result : Internal_Refd_Def;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Imprecise_Fallback :=
            Imprecise_Fallback;

      
      Property_Result :=
         Libadalang.Implementation.Name_P_Failsafe_Referenced_Def_Name
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Imprecise_Fallback, E_Info => Node.Internal.Info);

         return To_Public_Refd_Def (Property_Result);

   end;

         
   function P_Referenced_Defining_Name
     (Node : Name'Class;
      Imprecise_Fallback : Boolean := False) return Defining_Name is
      


         Internal_Arg_Imprecise_Fallback : Boolean;
      Property_Result : Internal_Entity_Defining_Name;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Imprecise_Fallback :=
            Imprecise_Fallback;

      
      Property_Result :=
         Libadalang.Implementation.Name_P_Referenced_Defining_Name
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Imprecise_Fallback, E_Info => Node.Internal.Info);

         return Wrap_Node (Property_Result.Node, Property_Result.Info).As_Defining_Name;

   end;

         
   function P_All_Env_Elements
     (Node : Name'Class;
      Seq : Boolean := True;
      Seq_From : Ada_Node'Class := No_Ada_Node) return Ada_Node_Array is
      


         Internal_Arg_Seq : Boolean;
         Internal_Arg_Seq_From : Bare_Ada_Node;
      Property_Result : Internal_Entity_Array_Access;

         procedure Free_Internal;
         --  Dec-ref all internal arguments and the property result, when
         --  applicable.

         -------------------
         -- Free_Internal --
         -------------------

         procedure Free_Internal is
         begin
               Dec_Ref (Property_Result);
         end Free_Internal;

   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Seq :=
            Seq;
         Internal_Arg_Seq_From :=
            Seq_From.Internal.Node;

      
      Property_Result :=
         Libadalang.Implementation.Name_P_All_Env_Elements
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Seq, Internal_Arg_Seq_From, E_Info => Node.Internal.Info);

         return Result : constant Ada_Node_Array :=
            To_Public_Ada_Node_Array (Property_Result)
         do
            Free_Internal;
         end return;

      exception
         when Precondition_Failure | Property_Error =>
            Free_Internal;
            raise;
   end;

         
   function P_Called_Subp_Spec
     (Node : Name'Class) return Base_Formal_Param_Holder is
      


      Property_Result : Internal_Entity_Base_Formal_Param_Holder;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Name_P_Called_Subp_Spec
            (Bare_Ada_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return Wrap_Node (Property_Result.Node, Property_Result.Info).As_Base_Formal_Param_Holder;

   end;

         
   function P_Referenced_Decl
     (Node : Name'Class;
      Imprecise_Fallback : Boolean := False) return Basic_Decl is
      


         Internal_Arg_Imprecise_Fallback : Boolean;
      Property_Result : Internal_Entity_Basic_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Imprecise_Fallback :=
            Imprecise_Fallback;

      
      Property_Result :=
         Libadalang.Implementation.Name_P_Referenced_Decl
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Imprecise_Fallback, E_Info => Node.Internal.Info);

         return Wrap_Node (Property_Result.Node, Property_Result.Info).As_Basic_Decl;

   end;

         
   function P_Failsafe_Referenced_Decl
     (Node : Name'Class;
      Imprecise_Fallback : Boolean := False) return Refd_Decl is
      


         Internal_Arg_Imprecise_Fallback : Boolean;
      Property_Result : Internal_Refd_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Imprecise_Fallback :=
            Imprecise_Fallback;

      
      Property_Result :=
         Libadalang.Implementation.Name_P_Failsafe_Referenced_Decl
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Imprecise_Fallback, E_Info => Node.Internal.Info);

         return To_Public_Refd_Decl (Property_Result);

   end;

         
   function P_Referenced_Decl_Internal
     (Node : Name'Class;
      Imprecise_Fallback : Boolean := False) return Refd_Decl is
      


         Internal_Arg_Imprecise_Fallback : Boolean;
      Property_Result : Internal_Refd_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Imprecise_Fallback :=
            Imprecise_Fallback;

      
      Property_Result :=
         Libadalang.Implementation.Name_P_Referenced_Decl_Internal
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Imprecise_Fallback, E_Info => Node.Internal.Info);

         return To_Public_Refd_Decl (Property_Result);

   end;

         
   function P_Name_Designated_Type
     (Node : Name'Class) return Base_Type_Decl is
      


      Property_Result : Internal_Entity_Base_Type_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Name_P_Name_Designated_Type
            (Bare_Ada_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return Wrap_Node (Property_Result.Node, Property_Result.Info).As_Base_Type_Decl;

   end;

         
   function P_Is_Static_Subtype
     (Node : Name'Class;
      Imprecise_Fallback : Boolean := False) return Boolean is
      


         Internal_Arg_Imprecise_Fallback : Boolean;
      Property_Result : Boolean;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Imprecise_Fallback :=
            Imprecise_Fallback;

      
      Property_Result :=
         Libadalang.Implementation.Name_P_Is_Static_Subtype
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Imprecise_Fallback, E_Info => Node.Internal.Info);

         return Property_Result;

   end;

         
   function P_Name_Matches
     (Node : Name'Class;
      N : Name'Class) return Boolean is
      


         Internal_Arg_N : Internal_Entity_Name;
      Property_Result : Boolean;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_N :=
            (N.Internal.Node, N.Internal.Info);

      
      Property_Result :=
         Libadalang.Implementation.Name_P_Name_Matches
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_N);

         return Property_Result;

   end;

         
   function P_Relative_Name
     (Node : Name'Class) return Single_Tok_Node is
      


      Property_Result : Internal_Entity_Single_Tok_Node;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Dispatcher_Name_P_Relative_Name
            (Bare_Ada_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return Wrap_Node (Property_Result.Node, Property_Result.Info).As_Single_Tok_Node;

   end;

         
   function P_Is_Operator_Name
     (Node : Name'Class) return Boolean is
      


      Property_Result : Boolean;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Name_P_Is_Operator_Name
            (Bare_Ada_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return Property_Result;

   end;

         
   function P_Is_Write_Reference
     (Node : Name'Class;
      Imprecise_Fallback : Boolean := False) return Boolean is
      


         Internal_Arg_Imprecise_Fallback : Boolean;
      Property_Result : Boolean;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Imprecise_Fallback :=
            Imprecise_Fallback;

      
      Property_Result :=
         Libadalang.Implementation.Name_P_Is_Write_Reference
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Imprecise_Fallback, E_Info => Node.Internal.Info);

         return Property_Result;

   end;

         
   function P_Is_Static_Call
     (Node : Name'Class;
      Imprecise_Fallback : Boolean := False) return Boolean is
      


         Internal_Arg_Imprecise_Fallback : Boolean;
      Property_Result : Boolean;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Imprecise_Fallback :=
            Imprecise_Fallback;

      
      Property_Result :=
         Libadalang.Implementation.Name_P_Is_Static_Call
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Imprecise_Fallback, E_Info => Node.Internal.Info);

         return Property_Result;

   end;

         
   function P_As_Symbol_Array
     (Node : Name'Class) return Unbounded_Text_Type_Array is
      


      Property_Result : Symbol_Type_Array_Access;

         procedure Free_Internal;
         --  Dec-ref all internal arguments and the property result, when
         --  applicable.

         -------------------
         -- Free_Internal --
         -------------------

         procedure Free_Internal is
         begin
               Dec_Ref (Property_Result);
         end Free_Internal;

   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Dispatcher_Name_P_As_Symbol_Array
            (Bare_Ada_Node (Node.Internal.Node));

         return Result : constant Unbounded_Text_Type_Array :=
            To_Public_Unbounded_Text_Type_Array (Property_Result)
         do
            Free_Internal;
         end return;

      exception
         when Precondition_Failure | Property_Error =>
            Free_Internal;
            raise;
   end;

         
   function P_Canonical_Text
     (Node : Name'Class) return Unbounded_Text_Type is
      


      Property_Result : Symbol_Type;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Dispatcher_Name_P_Canonical_Text
            (Bare_Ada_Node (Node.Internal.Node));

         return To_Unbounded_Text (Image (Property_Result));

   end;

         
   function P_Is_Constant
     (Node : Name'Class) return Boolean is
      


      Property_Result : Boolean;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Dispatcher_Name_P_Is_Constant
            (Bare_Ada_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return Property_Result;

   end;

         
   function P_Call_Params
     (Node : Name'Class) return Param_Actual_Array is
      


      Property_Result : Internal_Param_Actual_Array_Access;

         procedure Free_Internal;
         --  Dec-ref all internal arguments and the property result, when
         --  applicable.

         -------------------
         -- Free_Internal --
         -------------------

         procedure Free_Internal is
         begin
               Dec_Ref (Property_Result);
         end Free_Internal;

   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Name_P_Call_Params
            (Bare_Ada_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return Result : constant Param_Actual_Array :=
            To_Public_Param_Actual_Array (Property_Result)
         do
            Free_Internal;
         end return;

      exception
         when Precondition_Failure | Property_Error =>
            Free_Internal;
            raise;
   end;





















         
   function P_As_Bool
     (Node : Abort_Node'Class) return Boolean is
      


      Property_Result : Boolean;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Dispatcher_Abort_Node_P_As_Bool
            (Bare_Ada_Node (Node.Internal.Node));

         return Property_Result;

   end;
















         
   function P_Is_Ghost_Code
     (Node : Stmt'Class) return Boolean is
      


      Property_Result : Boolean;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Stmt_P_Is_Ghost_Code
            (Bare_Ada_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return Property_Result;

   end;










         
   

   function F_Names
     (Node : Abort_Stmt'Class) return Name_List
   is
      Result : Bare_Name_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Abort_Stmt_F_Names (Node.Internal.Node);
         if Result = null then
            return No_Name_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Names;








         
   function P_As_Bool
     (Node : Abstract_Node'Class) return Boolean is
      


      Property_Result : Boolean;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Dispatcher_Abstract_Node_P_As_Bool
            (Bare_Ada_Node (Node.Internal.Node));

         return Property_Result;

   end;











         
   function P_Subp_Decl_Spec
     (Node : Basic_Subp_Decl'Class) return Base_Subp_Spec is
      


      Property_Result : Internal_Entity_Base_Subp_Spec;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Dispatcher_Basic_Subp_Decl_P_Subp_Decl_Spec
            (Bare_Ada_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return Wrap_Node (Property_Result.Node, Property_Result.Info).As_Base_Subp_Spec;

   end;





         
   

   function F_Overriding
     (Node : Classic_Subp_Decl'Class) return Overriding_Node
   is
      Result : Bare_Overriding_Node;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Classic_Subp_Decl_F_Overriding (Node.Internal.Node);
         if Result = null then
            return No_Overriding_Node;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Overriding;

         function F_Overriding
           (Node : Classic_Subp_Decl'Class) return Ada_Overriding_Node
         is (Overriding_Node'(Node.F_Overriding).Kind);

         
   

   function F_Subp_Spec
     (Node : Classic_Subp_Decl'Class) return Subp_Spec
   is
      Result : Bare_Subp_Spec;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Classic_Subp_Decl_F_Subp_Spec (Node.Internal.Node);
         if Result = null then
            return No_Subp_Spec;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Subp_Spec;



         
   function P_Body_Part
     (Node : Classic_Subp_Decl'Class;
      Imprecise_Fallback : Boolean := False) return Base_Subp_Body is
      


         Internal_Arg_Imprecise_Fallback : Boolean;
      Property_Result : Internal_Entity_Base_Subp_Body;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Imprecise_Fallback :=
            Imprecise_Fallback;

      
      Property_Result :=
         Libadalang.Implementation.Classic_Subp_Decl_P_Body_Part
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Imprecise_Fallback, E_Info => Node.Internal.Info);

         return Wrap_Node (Property_Result.Node, Property_Result.Info).As_Base_Subp_Body;

   end;





         
   

   function F_Default_Expr
     (Node : Formal_Subp_Decl'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Formal_Subp_Decl_F_Default_Expr (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Default_Expr;

















         
   

   function F_Name
     (Node : Abstract_State_Decl'Class) return Defining_Name
   is
      Result : Bare_Defining_Name;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Abstract_State_Decl_F_Name (Node.Internal.Node);
         if Result = null then
            return No_Defining_Name;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Name;







         
   

   function F_State_Decl
     (Node : Abstract_State_Decl_Expr'Class) return Ada_Node
   is
      Result : Bare_Ada_Node;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Abstract_State_Decl_Expr_F_State_Decl (Node.Internal.Node);
         if Result = null then
            return No_Ada_Node;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_State_Decl;






















         
   

   function F_Name
     (Node : Accept_Stmt'Class) return Identifier
   is
      Result : Bare_Identifier;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Accept_Stmt_F_Name (Node.Internal.Node);
         if Result = null then
            return No_Identifier;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Name;


         
   

   function F_Entry_Index_Expr
     (Node : Accept_Stmt'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Accept_Stmt_F_Entry_Index_Expr (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Entry_Index_Expr;


         
   

   function F_Params
     (Node : Accept_Stmt'Class) return Entry_Completion_Formal_Params
   is
      Result : Bare_Entry_Completion_Formal_Params;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Accept_Stmt_F_Params (Node.Internal.Node);
         if Result = null then
            return No_Entry_Completion_Formal_Params;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Params;



         
   function P_Corresponding_Entry
     (Node : Accept_Stmt'Class;
      Origin : Ada_Node'Class := No_Ada_Node) return Entry_Decl is
      


         Internal_Arg_Origin : Bare_Ada_Node;
      Property_Result : Internal_Entity_Entry_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Origin :=
            Origin.Internal.Node;

      
      Property_Result :=
         Libadalang.Implementation.Accept_Stmt_P_Corresponding_Entry
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Origin, E_Info => Node.Internal.Info);

         return Wrap_Node (Property_Result.Node, Property_Result.Info).As_Entry_Decl;

   end;





         
   

   function F_Stmts
     (Node : Accept_Stmt_With_Stmts'Class) return Handled_Stmts
   is
      Result : Bare_Handled_Stmts;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Accept_Stmt_With_Stmts_F_Stmts (Node.Internal.Node);
         if Result = null then
            return No_Handled_Stmts;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Stmts;


         
   

   function F_End_Name
     (Node : Accept_Stmt_With_Stmts'Class) return End_Name
   is
      Result : Bare_End_Name;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Accept_Stmt_With_Stmts_F_End_Name (Node.Internal.Node);
         if Result = null then
            return No_End_Name;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_End_Name;












         
   

   function F_Has_Not_Null
     (Node : Access_Def'Class) return Not_Null
   is
      Result : Bare_Not_Null;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Access_Def_F_Has_Not_Null (Node.Internal.Node);
         if Result = null then
            return No_Not_Null;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Has_Not_Null;

         function F_Has_Not_Null (Node : Access_Def'Class) return Boolean
         is (Not_Null'(Node.F_Has_Not_Null).Kind
             = Ada_Not_Null_Present);







         
   

   function F_Has_Protected
     (Node : Access_To_Subp_Def'Class) return Protected_Node
   is
      Result : Bare_Protected_Node;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Access_To_Subp_Def_F_Has_Protected (Node.Internal.Node);
         if Result = null then
            return No_Protected_Node;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Has_Protected;

         function F_Has_Protected (Node : Access_To_Subp_Def'Class) return Boolean
         is (Protected_Node'(Node.F_Has_Protected).Kind
             = Ada_Protected_Present);


         
   

   function F_Subp_Spec
     (Node : Access_To_Subp_Def'Class) return Subp_Spec
   is
      Result : Bare_Subp_Spec;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Access_To_Subp_Def_F_Subp_Spec (Node.Internal.Node);
         if Result = null then
            return No_Subp_Spec;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Subp_Spec;







         
   

   function F_Ancestor_Expr
     (Node : Base_Aggregate'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Base_Aggregate_F_Ancestor_Expr (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Ancestor_Expr;


         
   

   function F_Assocs
     (Node : Base_Aggregate'Class) return Assoc_List
   is
      Result : Bare_Assoc_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Base_Aggregate_F_Assocs (Node.Internal.Node);
         if Result = null then
            return No_Assoc_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Assocs;



         
   function P_Aggregate_Params
     (Node : Base_Aggregate'Class) return Param_Actual_Array is
      


      Property_Result : Internal_Param_Actual_Array_Access;

         procedure Free_Internal;
         --  Dec-ref all internal arguments and the property result, when
         --  applicable.

         -------------------
         -- Free_Internal --
         -------------------

         procedure Free_Internal is
         begin
               Dec_Ref (Property_Result);
         end Free_Internal;

   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Base_Aggregate_P_Aggregate_Params
            (Bare_Ada_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return Result : constant Param_Actual_Array :=
            To_Public_Param_Actual_Array (Property_Result)
         do
            Free_Internal;
         end return;

      exception
         when Precondition_Failure | Property_Error =>
            Free_Internal;
            raise;
   end;

         
   function P_Is_Subaggregate
     (Node : Base_Aggregate'Class) return Boolean is
      


      Property_Result : Boolean;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Base_Aggregate_P_Is_Subaggregate
            (Bare_Ada_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return Property_Result;

   end;











         
   function P_Get_Params
     (Node : Basic_Assoc'Class;
      Imprecise_Fallback : Boolean := False) return Defining_Name_Array is
      


         Internal_Arg_Imprecise_Fallback : Boolean;
      Property_Result : Internal_Entity_Defining_Name_Array_Access;

         procedure Free_Internal;
         --  Dec-ref all internal arguments and the property result, when
         --  applicable.

         -------------------
         -- Free_Internal --
         -------------------

         procedure Free_Internal is
         begin
               Dec_Ref (Property_Result);
         end Free_Internal;

   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Imprecise_Fallback :=
            Imprecise_Fallback;

      
      Property_Result :=
         Libadalang.Implementation.Basic_Assoc_P_Get_Params
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Imprecise_Fallback, E_Info => Node.Internal.Info);

         return Result : constant Defining_Name_Array :=
            To_Public_Defining_Name_Array (Property_Result)
         do
            Free_Internal;
         end return;

      exception
         when Precondition_Failure | Property_Error =>
            Free_Internal;
            raise;
   end;





         
   

   function F_Designators
     (Node : Aggregate_Assoc'Class) return Alternatives_List
   is
      Result : Bare_Alternatives_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Aggregate_Assoc_F_Designators (Node.Internal.Node);
         if Result = null then
            return No_Alternatives_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Designators;


         
   

   function F_R_Expr
     (Node : Aggregate_Assoc'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Aggregate_Assoc_F_R_Expr (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_R_Expr;








         
   function P_As_Bool
     (Node : Aliased_Node'Class) return Boolean is
      


      Property_Result : Boolean;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Dispatcher_Aliased_Node_P_As_Bool
            (Bare_Ada_Node (Node.Internal.Node));

         return Property_Result;

   end;
















         
   function P_As_Bool
     (Node : All_Node'Class) return Boolean is
      


      Property_Result : Boolean;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Dispatcher_All_Node_P_As_Bool
            (Bare_Ada_Node (Node.Internal.Node));

         return Property_Result;

   end;















         
   

   function F_Subpool
     (Node : Allocator'Class) return Name
   is
      Result : Bare_Name;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Allocator_F_Subpool (Node.Internal.Node);
         if Result = null then
            return No_Name;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Subpool;


         
   

   function F_Type_Or_Expr
     (Node : Allocator'Class) return Ada_Node
   is
      Result : Bare_Ada_Node;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Allocator_F_Type_Or_Expr (Node.Internal.Node);
         if Result = null then
            return No_Ada_Node;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Type_Or_Expr;



         
   function P_Get_Allocated_Type
     (Node : Allocator'Class) return Base_Type_Decl is
      


      Property_Result : Internal_Entity_Base_Type_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Allocator_P_Get_Allocated_Type
            (Bare_Ada_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return Wrap_Node (Property_Result.Node, Property_Result.Info).As_Base_Type_Decl;

   end;





         
   

   function F_Expr
     (Node : Anonymous_Expr_Decl'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Anonymous_Expr_Decl_F_Expr (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Expr;



         
   function P_Get_Formal
     (Node : Anonymous_Expr_Decl'Class;
      Imprecise_Fallback : Boolean := False) return Defining_Name is
      


         Internal_Arg_Imprecise_Fallback : Boolean;
      Property_Result : Internal_Entity_Defining_Name;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Imprecise_Fallback :=
            Imprecise_Fallback;

      
      Property_Result :=
         Libadalang.Implementation.Anonymous_Expr_Decl_P_Get_Formal
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Imprecise_Fallback, E_Info => Node.Internal.Info);

         return Wrap_Node (Property_Result.Node, Property_Result.Info).As_Defining_Name;

   end;






         
   function P_Type_Name
     (Node : Type_Expr'Class) return Name is
      


      Property_Result : Internal_Entity_Name;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Type_Expr_P_Type_Name
            (Bare_Ada_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return Wrap_Node (Property_Result.Node, Property_Result.Info).As_Name;

   end;

         
   function P_Designated_Type_Decl
     (Node : Type_Expr'Class) return Base_Type_Decl is
      


      Property_Result : Internal_Entity_Base_Type_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Type_Expr_P_Designated_Type_Decl
            (Bare_Ada_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return Wrap_Node (Property_Result.Node, Property_Result.Info).As_Base_Type_Decl;

   end;

         
   function P_Designated_Type_Decl_From
     (Node : Type_Expr'Class;
      Origin_Node : Ada_Node'Class) return Base_Type_Decl is
      


         Internal_Arg_Origin_Node : Internal_Entity;
      Property_Result : Internal_Entity_Base_Type_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Origin_Node :=
            (Origin_Node.Internal.Node, Origin_Node.Internal.Info);

      
      Property_Result :=
         Libadalang.Implementation.Type_Expr_P_Designated_Type_Decl_From
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Origin_Node, E_Info => Node.Internal.Info);

         return Wrap_Node (Property_Result.Node, Property_Result.Info).As_Base_Type_Decl;

   end;





         
   

   function F_Type_Decl
     (Node : Anonymous_Type'Class) return Anonymous_Type_Decl
   is
      Result : Bare_Anonymous_Type_Decl;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Anonymous_Type_F_Type_Decl (Node.Internal.Node);
         if Result = null then
            return No_Anonymous_Type_Decl;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Type_Decl;












         
   

   function F_Type_Decl
     (Node : Anonymous_Type_Access_Def'Class) return Base_Type_Decl
   is
      Result : Bare_Base_Type_Decl;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Anonymous_Type_Access_Def_F_Type_Decl (Node.Internal.Node);
         if Result = null then
            return No_Base_Type_Decl;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Type_Decl;







         
   

   function F_Name
     (Node : Base_Type_Decl'Class) return Defining_Name
   is
      Result : Bare_Defining_Name;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Base_Type_Decl_F_Name (Node.Internal.Node);
         if Result = null then
            return No_Defining_Name;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Name;



         
   function P_Base_Subtype
     (Node : Base_Type_Decl'Class;
      Origin : Ada_Node'Class := No_Ada_Node) return Base_Type_Decl is
      


         Internal_Arg_Origin : Bare_Ada_Node;
      Property_Result : Internal_Entity_Base_Type_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Origin :=
            Origin.Internal.Node;

      
      Property_Result :=
         Libadalang.Implementation.Base_Type_Decl_P_Base_Subtype
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Origin, E_Info => Node.Internal.Info);

         return Wrap_Node (Property_Result.Node, Property_Result.Info).As_Base_Type_Decl;

   end;

         
   function P_Private_Completion
     (Node : Base_Type_Decl'Class) return Base_Type_Decl is
      


      Property_Result : Internal_Entity_Base_Type_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Base_Type_Decl_P_Private_Completion
            (Bare_Ada_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return Wrap_Node (Property_Result.Node, Property_Result.Info).As_Base_Type_Decl;

   end;

         
   function P_Is_Inherited_Primitive
     (Node : Base_Type_Decl'Class;
      P : Basic_Decl'Class) return Boolean is
      


         Internal_Arg_P : Internal_Entity_Basic_Decl;
      Property_Result : Boolean;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_P :=
            (P.Internal.Node, P.Internal.Info);

      
      Property_Result :=
         Libadalang.Implementation.Base_Type_Decl_P_Is_Inherited_Primitive
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_P, E_Info => Node.Internal.Info);

         return Property_Result;

   end;

         
   function P_Get_Record_Representation_Clause
     (Node : Base_Type_Decl'Class;
      Imprecise_Fallback : Boolean := False) return Record_Rep_Clause is
      


         Internal_Arg_Imprecise_Fallback : Boolean;
      Property_Result : Internal_Entity_Record_Rep_Clause;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Imprecise_Fallback :=
            Imprecise_Fallback;

      
      Property_Result :=
         Libadalang.Implementation.Base_Type_Decl_P_Get_Record_Representation_Clause
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Imprecise_Fallback, E_Info => Node.Internal.Info);

         return Wrap_Node (Property_Result.Node, Property_Result.Info).As_Record_Rep_Clause;

   end;

         
   function P_Get_Enum_Representation_Clause
     (Node : Base_Type_Decl'Class;
      Imprecise_Fallback : Boolean := False) return Enum_Rep_Clause is
      


         Internal_Arg_Imprecise_Fallback : Boolean;
      Property_Result : Internal_Entity_Enum_Rep_Clause;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Imprecise_Fallback :=
            Imprecise_Fallback;

      
      Property_Result :=
         Libadalang.Implementation.Base_Type_Decl_P_Get_Enum_Representation_Clause
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Imprecise_Fallback, E_Info => Node.Internal.Info);

         return Wrap_Node (Property_Result.Node, Property_Result.Info).As_Enum_Rep_Clause;

   end;

         
   function P_Is_Record_Type
     (Node : Base_Type_Decl'Class;
      Origin : Ada_Node'Class := No_Ada_Node) return Boolean is
      


         Internal_Arg_Origin : Bare_Ada_Node;
      Property_Result : Boolean;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Origin :=
            Origin.Internal.Node;

      
      Property_Result :=
         Libadalang.Implementation.Dispatcher_Base_Type_Decl_P_Is_Record_Type
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Origin, E_Info => Node.Internal.Info);

         return Property_Result;

   end;

         
   function P_Is_Array_Type
     (Node : Base_Type_Decl'Class;
      Origin : Ada_Node'Class := No_Ada_Node) return Boolean is
      


         Internal_Arg_Origin : Bare_Ada_Node;
      Property_Result : Boolean;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Origin :=
            Origin.Internal.Node;

      
      Property_Result :=
         Libadalang.Implementation.Base_Type_Decl_P_Is_Array_Type
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Origin, E_Info => Node.Internal.Info);

         return Property_Result;

   end;

         
   function P_Find_Derived_Types
     (Node : Base_Type_Decl'Class;
      Root : Ada_Node'Class;
      Origin : Ada_Node'Class;
      Imprecise_Fallback : Boolean := False) return Type_Decl_Array is
      


         Internal_Arg_Root : Internal_Entity;
         Internal_Arg_Origin : Bare_Ada_Node;
         Internal_Arg_Imprecise_Fallback : Boolean;
      Property_Result : Internal_Entity_Type_Decl_Array_Access;

         procedure Free_Internal;
         --  Dec-ref all internal arguments and the property result, when
         --  applicable.

         -------------------
         -- Free_Internal --
         -------------------

         procedure Free_Internal is
         begin
               Dec_Ref (Property_Result);
         end Free_Internal;

   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Root :=
            (Root.Internal.Node, Root.Internal.Info);
         Internal_Arg_Origin :=
            Origin.Internal.Node;
         Internal_Arg_Imprecise_Fallback :=
            Imprecise_Fallback;

      
      Property_Result :=
         Libadalang.Implementation.Base_Type_Decl_P_Find_Derived_Types
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Root, Internal_Arg_Origin, Internal_Arg_Imprecise_Fallback, E_Info => Node.Internal.Info);

         return Result : constant Type_Decl_Array :=
            To_Public_Type_Decl_Array (Property_Result)
         do
            Free_Internal;
         end return;

      exception
         when Precondition_Failure | Property_Error =>
            Free_Internal;
            raise;
   end;

         
   function P_Is_Real_Type
     (Node : Base_Type_Decl'Class;
      Origin : Ada_Node'Class := No_Ada_Node) return Boolean is
      


         Internal_Arg_Origin : Bare_Ada_Node;
      Property_Result : Boolean;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Origin :=
            Origin.Internal.Node;

      
      Property_Result :=
         Libadalang.Implementation.Dispatcher_Base_Type_Decl_P_Is_Real_Type
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Origin, E_Info => Node.Internal.Info);

         return Property_Result;

   end;

         
   function P_Is_Float_Type
     (Node : Base_Type_Decl'Class;
      Origin : Ada_Node'Class := No_Ada_Node) return Boolean is
      


         Internal_Arg_Origin : Bare_Ada_Node;
      Property_Result : Boolean;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Origin :=
            Origin.Internal.Node;

      
      Property_Result :=
         Libadalang.Implementation.Dispatcher_Base_Type_Decl_P_Is_Float_Type
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Origin, E_Info => Node.Internal.Info);

         return Property_Result;

   end;

         
   function P_Is_Fixed_Point
     (Node : Base_Type_Decl'Class;
      Origin : Ada_Node'Class := No_Ada_Node) return Boolean is
      


         Internal_Arg_Origin : Bare_Ada_Node;
      Property_Result : Boolean;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Origin :=
            Origin.Internal.Node;

      
      Property_Result :=
         Libadalang.Implementation.Dispatcher_Base_Type_Decl_P_Is_Fixed_Point
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Origin, E_Info => Node.Internal.Info);

         return Property_Result;

   end;

         
   function P_Is_Enum_Type
     (Node : Base_Type_Decl'Class;
      Origin : Ada_Node'Class := No_Ada_Node) return Boolean is
      


         Internal_Arg_Origin : Bare_Ada_Node;
      Property_Result : Boolean;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Origin :=
            Origin.Internal.Node;

      
      Property_Result :=
         Libadalang.Implementation.Dispatcher_Base_Type_Decl_P_Is_Enum_Type
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Origin, E_Info => Node.Internal.Info);

         return Property_Result;

   end;

         
   function P_Is_Access_Type
     (Node : Base_Type_Decl'Class;
      Origin : Ada_Node'Class := No_Ada_Node) return Boolean is
      


         Internal_Arg_Origin : Bare_Ada_Node;
      Property_Result : Boolean;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Origin :=
            Origin.Internal.Node;

      
      Property_Result :=
         Libadalang.Implementation.Dispatcher_Base_Type_Decl_P_Is_Access_Type
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Origin, E_Info => Node.Internal.Info);

         return Property_Result;

   end;

         
   function P_Is_Char_Type
     (Node : Base_Type_Decl'Class;
      Origin : Ada_Node'Class := No_Ada_Node) return Boolean is
      


         Internal_Arg_Origin : Bare_Ada_Node;
      Property_Result : Boolean;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Origin :=
            Origin.Internal.Node;

      
      Property_Result :=
         Libadalang.Implementation.Dispatcher_Base_Type_Decl_P_Is_Char_Type
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Origin, E_Info => Node.Internal.Info);

         return Property_Result;

   end;

         
   function P_Discrete_Range
     (Node : Base_Type_Decl'Class) return Discrete_Range is
      


      Property_Result : Internal_Discrete_Range;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Dispatcher_Base_Type_Decl_P_Discrete_Range
            (Bare_Ada_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return To_Public_Discrete_Range (Property_Result);

   end;

         
   function P_Is_Discrete_Type
     (Node : Base_Type_Decl'Class;
      Origin : Ada_Node'Class := No_Ada_Node) return Boolean is
      


         Internal_Arg_Origin : Bare_Ada_Node;
      Property_Result : Boolean;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Origin :=
            Origin.Internal.Node;

      
      Property_Result :=
         Libadalang.Implementation.Dispatcher_Base_Type_Decl_P_Is_Discrete_Type
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Origin, E_Info => Node.Internal.Info);

         return Property_Result;

   end;

         
   function P_Is_Int_Type
     (Node : Base_Type_Decl'Class;
      Origin : Ada_Node'Class := No_Ada_Node) return Boolean is
      


         Internal_Arg_Origin : Bare_Ada_Node;
      Property_Result : Boolean;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Origin :=
            Origin.Internal.Node;

      
      Property_Result :=
         Libadalang.Implementation.Dispatcher_Base_Type_Decl_P_Is_Int_Type
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Origin, E_Info => Node.Internal.Info);

         return Property_Result;

   end;

         
   function P_Accessed_Type
     (Node : Base_Type_Decl'Class;
      Origin : Ada_Node'Class := No_Ada_Node) return Base_Type_Decl is
      


         Internal_Arg_Origin : Bare_Ada_Node;
      Property_Result : Internal_Entity_Base_Type_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Origin :=
            Origin.Internal.Node;

      
      Property_Result :=
         Libadalang.Implementation.Dispatcher_Base_Type_Decl_P_Accessed_Type
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Origin, E_Info => Node.Internal.Info);

         return Wrap_Node (Property_Result.Node, Property_Result.Info).As_Base_Type_Decl;

   end;

         
   function P_Is_Tagged_Type
     (Node : Base_Type_Decl'Class;
      Origin : Ada_Node'Class := No_Ada_Node) return Boolean is
      


         Internal_Arg_Origin : Bare_Ada_Node;
      Property_Result : Boolean;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Origin :=
            Origin.Internal.Node;

      
      Property_Result :=
         Libadalang.Implementation.Dispatcher_Base_Type_Decl_P_Is_Tagged_Type
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Origin, E_Info => Node.Internal.Info);

         return Property_Result;

   end;

         
   function P_Base_Type
     (Node : Base_Type_Decl'Class;
      Origin : Ada_Node'Class := No_Ada_Node) return Base_Type_Decl is
      


         Internal_Arg_Origin : Bare_Ada_Node;
      Property_Result : Internal_Entity_Base_Type_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Origin :=
            Origin.Internal.Node;

      
      Property_Result :=
         Libadalang.Implementation.Dispatcher_Base_Type_Decl_P_Base_Type
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Origin, E_Info => Node.Internal.Info);

         return Wrap_Node (Property_Result.Node, Property_Result.Info).As_Base_Type_Decl;

   end;

         
   function P_Base_Types
     (Node : Base_Type_Decl'Class;
      Origin : Ada_Node'Class := No_Ada_Node) return Base_Type_Decl_Array is
      


         Internal_Arg_Origin : Bare_Ada_Node;
      Property_Result : Internal_Entity_Base_Type_Decl_Array_Access;

         procedure Free_Internal;
         --  Dec-ref all internal arguments and the property result, when
         --  applicable.

         -------------------
         -- Free_Internal --
         -------------------

         procedure Free_Internal is
         begin
               Dec_Ref (Property_Result);
         end Free_Internal;

   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Origin :=
            Origin.Internal.Node;

      
      Property_Result :=
         Libadalang.Implementation.Dispatcher_Base_Type_Decl_P_Base_Types
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Origin, E_Info => Node.Internal.Info);

         return Result : constant Base_Type_Decl_Array :=
            To_Public_Base_Type_Decl_Array (Property_Result)
         do
            Free_Internal;
         end return;

      exception
         when Precondition_Failure | Property_Error =>
            Free_Internal;
            raise;
   end;

         
   function P_Find_All_Derived_Types
     (Node : Base_Type_Decl'Class;
      Units : Analysis_Unit_Array;
      Imprecise_Fallback : Boolean := False) return Type_Decl_Array is
      


         Internal_Arg_Units : Internal_Unit_Array_Access;
         Internal_Arg_Imprecise_Fallback : Boolean;
      Property_Result : Internal_Entity_Type_Decl_Array_Access;

         procedure Free_Internal;
         --  Dec-ref all internal arguments and the property result, when
         --  applicable.

         -------------------
         -- Free_Internal --
         -------------------

         procedure Free_Internal is
         begin
               Dec_Ref (Internal_Arg_Units);
               Dec_Ref (Property_Result);
         end Free_Internal;

   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Units :=
            To_Internal_Analysis_Unit_Array (Units);
         Internal_Arg_Imprecise_Fallback :=
            Imprecise_Fallback;

      
      Property_Result :=
         Libadalang.Implementation.Base_Type_Decl_P_Find_All_Derived_Types
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Units, Internal_Arg_Imprecise_Fallback, E_Info => Node.Internal.Info);

         return Result : constant Type_Decl_Array :=
            To_Public_Type_Decl_Array (Property_Result)
         do
            Free_Internal;
         end return;

      exception
         when Precondition_Failure | Property_Error =>
            Free_Internal;
            raise;
   end;

         
   function P_Comp_Type
     (Node : Base_Type_Decl'Class;
      Is_Subscript : Boolean := False;
      Origin : Ada_Node'Class := No_Ada_Node) return Base_Type_Decl is
      


         Internal_Arg_Is_Subscript : Boolean;
         Internal_Arg_Origin : Bare_Ada_Node;
      Property_Result : Internal_Entity_Base_Type_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Is_Subscript :=
            Is_Subscript;
         Internal_Arg_Origin :=
            Origin.Internal.Node;

      
      Property_Result :=
         Libadalang.Implementation.Base_Type_Decl_P_Comp_Type
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Is_Subscript, Internal_Arg_Origin, E_Info => Node.Internal.Info);

         return Wrap_Node (Property_Result.Node, Property_Result.Info).As_Base_Type_Decl;

   end;

         
   function P_Index_Type
     (Node : Base_Type_Decl'Class;
      Dim : Integer;
      Origin : Ada_Node'Class := No_Ada_Node) return Base_Type_Decl is
      


         Internal_Arg_Dim : Integer;
         Internal_Arg_Origin : Bare_Ada_Node;
      Property_Result : Internal_Entity_Base_Type_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Dim :=
            Dim;
         Internal_Arg_Origin :=
            Origin.Internal.Node;

      
      Property_Result :=
         Libadalang.Implementation.Base_Type_Decl_P_Index_Type
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Dim, Internal_Arg_Origin, E_Info => Node.Internal.Info);

         return Wrap_Node (Property_Result.Node, Property_Result.Info).As_Base_Type_Decl;

   end;

         
   function P_Is_Derived_Type
     (Node : Base_Type_Decl'Class;
      Other_Type : Base_Type_Decl'Class;
      Origin : Ada_Node'Class := No_Ada_Node) return Boolean is
      


         Internal_Arg_Other_Type : Internal_Entity_Base_Type_Decl;
         Internal_Arg_Origin : Bare_Ada_Node;
      Property_Result : Boolean;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Other_Type :=
            (Other_Type.Internal.Node, Other_Type.Internal.Info);
         Internal_Arg_Origin :=
            Origin.Internal.Node;

      
      Property_Result :=
         Libadalang.Implementation.Base_Type_Decl_P_Is_Derived_Type
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Other_Type, Internal_Arg_Origin, E_Info => Node.Internal.Info);

         return Property_Result;

   end;

         
   function P_Is_Interface_Type
     (Node : Base_Type_Decl'Class;
      Origin : Ada_Node'Class := No_Ada_Node) return Boolean is
      


         Internal_Arg_Origin : Bare_Ada_Node;
      Property_Result : Boolean;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Origin :=
            Origin.Internal.Node;

      
      Property_Result :=
         Libadalang.Implementation.Dispatcher_Base_Type_Decl_P_Is_Interface_Type
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Origin, E_Info => Node.Internal.Info);

         return Property_Result;

   end;

         
   function P_Matching_Type
     (Node : Base_Type_Decl'Class;
      Expected_Type : Base_Type_Decl'Class;
      Origin : Ada_Node'Class := No_Ada_Node) return Boolean is
      


         Internal_Arg_Expected_Type : Internal_Entity_Base_Type_Decl;
         Internal_Arg_Origin : Bare_Ada_Node;
      Property_Result : Boolean;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Expected_Type :=
            (Expected_Type.Internal.Node, Expected_Type.Internal.Info);
         Internal_Arg_Origin :=
            Origin.Internal.Node;

      
      Property_Result :=
         Libadalang.Implementation.Base_Type_Decl_P_Matching_Type
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Expected_Type, Internal_Arg_Origin, E_Info => Node.Internal.Info);

         return Property_Result;

   end;

         
   function P_Canonical_Type
     (Node : Base_Type_Decl'Class;
      Origin : Ada_Node'Class := No_Ada_Node) return Base_Type_Decl is
      


         Internal_Arg_Origin : Bare_Ada_Node;
      Property_Result : Internal_Entity_Base_Type_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Origin :=
            Origin.Internal.Node;

      
      Property_Result :=
         Libadalang.Implementation.Dispatcher_Base_Type_Decl_P_Canonical_Type
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Origin, E_Info => Node.Internal.Info);

         return Wrap_Node (Property_Result.Node, Property_Result.Info).As_Base_Type_Decl;

   end;

         
   function P_Previous_Part
     (Node : Base_Type_Decl'Class;
      Go_To_Incomplete : Boolean := True) return Base_Type_Decl is
      


         Internal_Arg_Go_To_Incomplete : Boolean;
      Property_Result : Internal_Entity_Base_Type_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Go_To_Incomplete :=
            Go_To_Incomplete;

      
      Property_Result :=
         Libadalang.Implementation.Dispatcher_Base_Type_Decl_P_Previous_Part
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Go_To_Incomplete, E_Info => Node.Internal.Info);

         return Wrap_Node (Property_Result.Node, Property_Result.Info).As_Base_Type_Decl;

   end;

         
   function P_Next_Part
     (Node : Base_Type_Decl'Class) return Base_Type_Decl is
      


      Property_Result : Internal_Entity_Base_Type_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Base_Type_Decl_P_Next_Part
            (Bare_Ada_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return Wrap_Node (Property_Result.Node, Property_Result.Info).As_Base_Type_Decl;

   end;

         
   function P_Full_View
     (Node : Base_Type_Decl'Class) return Base_Type_Decl is
      


      Property_Result : Internal_Entity_Base_Type_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Base_Type_Decl_P_Full_View
            (Bare_Ada_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return Wrap_Node (Property_Result.Node, Property_Result.Info).As_Base_Type_Decl;

   end;

         
   function P_Is_Definite_Subtype
     (Node : Base_Type_Decl'Class;
      Origin : Ada_Node'Class := No_Ada_Node) return Boolean is
      


         Internal_Arg_Origin : Bare_Ada_Node;
      Property_Result : Boolean;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Origin :=
            Origin.Internal.Node;

      
      Property_Result :=
         Libadalang.Implementation.Base_Type_Decl_P_Is_Definite_Subtype
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Origin, E_Info => Node.Internal.Info);

         return Property_Result;

   end;

         
   function P_Is_Private
     (Node : Base_Type_Decl'Class) return Boolean is
      


      Property_Result : Boolean;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Dispatcher_Base_Type_Decl_P_Is_Private
            (Bare_Ada_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return Property_Result;

   end;

         
   function P_Discriminants_List
     (Node : Base_Type_Decl'Class;
      Origin : Ada_Node'Class := No_Ada_Node) return Base_Formal_Param_Decl_Array is
      


         Internal_Arg_Origin : Bare_Ada_Node;
      Property_Result : Internal_Entity_Base_Formal_Param_Decl_Array_Access;

         procedure Free_Internal;
         --  Dec-ref all internal arguments and the property result, when
         --  applicable.

         -------------------
         -- Free_Internal --
         -------------------

         procedure Free_Internal is
         begin
               Dec_Ref (Property_Result);
         end Free_Internal;

   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Origin :=
            Origin.Internal.Node;

      
      Property_Result :=
         Libadalang.Implementation.Dispatcher_Base_Type_Decl_P_Discriminants_List
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Origin, E_Info => Node.Internal.Info);

         return Result : constant Base_Formal_Param_Decl_Array :=
            To_Public_Base_Formal_Param_Decl_Array (Property_Result)
         do
            Free_Internal;
         end return;

      exception
         when Precondition_Failure | Property_Error =>
            Free_Internal;
            raise;
   end;

         
   function P_Root_Type
     (Node : Base_Type_Decl'Class;
      Origin : Ada_Node'Class := No_Ada_Node) return Base_Type_Decl is
      


         Internal_Arg_Origin : Bare_Ada_Node;
      Property_Result : Internal_Entity_Base_Type_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Origin :=
            Origin.Internal.Node;

      
      Property_Result :=
         Libadalang.Implementation.Dispatcher_Base_Type_Decl_P_Root_Type
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Origin, E_Info => Node.Internal.Info);

         return Wrap_Node (Property_Result.Node, Property_Result.Info).As_Base_Type_Decl;

   end;

         
   function P_Shapes
     (Node : Base_Type_Decl'Class;
      Include_Discriminants : Boolean := True;
      Origin : Ada_Node'Class := No_Ada_Node) return Shape_Array is
      


         Internal_Arg_Include_Discriminants : Boolean;
         Internal_Arg_Origin : Bare_Ada_Node;
      Property_Result : Internal_Shape_Array_Access;

         procedure Free_Internal;
         --  Dec-ref all internal arguments and the property result, when
         --  applicable.

         -------------------
         -- Free_Internal --
         -------------------

         procedure Free_Internal is
         begin
               Dec_Ref (Property_Result);
         end Free_Internal;

   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Include_Discriminants :=
            Include_Discriminants;
         Internal_Arg_Origin :=
            Origin.Internal.Node;

      
      Property_Result :=
         Libadalang.Implementation.Base_Type_Decl_P_Shapes
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Include_Discriminants, Internal_Arg_Origin, E_Info => Node.Internal.Info);

         return Result : constant Shape_Array :=
            To_Public_Shape_Array (Property_Result)
         do
            Free_Internal;
         end return;

      exception
         when Precondition_Failure | Property_Error =>
            Free_Internal;
            raise;
   end;





         
   

   function F_Discriminants
     (Node : Type_Decl'Class) return Discriminant_Part
   is
      Result : Bare_Discriminant_Part;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Type_Decl_F_Discriminants (Node.Internal.Node);
         if Result = null then
            return No_Discriminant_Part;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Discriminants;


         
   

   function F_Type_Def
     (Node : Type_Decl'Class) return Type_Def
   is
      Result : Bare_Type_Def;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Type_Decl_F_Type_Def (Node.Internal.Node);
         if Result = null then
            return No_Type_Def;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Type_Def;



         
   function P_Get_Primitives
     (Node : Type_Decl'Class;
      Only_Inherited : Boolean := False;
      Include_Predefined_Operators : Boolean := False) return Basic_Decl_Array is
      


         Internal_Arg_Only_Inherited : Boolean;
         Internal_Arg_Include_Predefined_Operators : Boolean;
      Property_Result : Internal_Entity_Basic_Decl_Array_Access;

         procedure Free_Internal;
         --  Dec-ref all internal arguments and the property result, when
         --  applicable.

         -------------------
         -- Free_Internal --
         -------------------

         procedure Free_Internal is
         begin
               Dec_Ref (Property_Result);
         end Free_Internal;

   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Only_Inherited :=
            Only_Inherited;
         Internal_Arg_Include_Predefined_Operators :=
            Include_Predefined_Operators;

      
      Property_Result :=
         Libadalang.Implementation.Type_Decl_P_Get_Primitives
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Only_Inherited, Internal_Arg_Include_Predefined_Operators, E_Info => Node.Internal.Info);

         return Result : constant Basic_Decl_Array :=
            To_Public_Basic_Decl_Array (Property_Result)
         do
            Free_Internal;
         end return;

      exception
         when Precondition_Failure | Property_Error =>
            Free_Internal;
            raise;
   end;















         
   

   function F_Indices
     (Node : Array_Type_Def'Class) return Array_Indices
   is
      Result : Bare_Array_Indices;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Array_Type_Def_F_Indices (Node.Internal.Node);
         if Result = null then
            return No_Array_Indices;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Indices;


         
   

   function F_Component_Type
     (Node : Array_Type_Def'Class) return Component_Def
   is
      Result : Bare_Component_Def;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Array_Type_Def_F_Component_Type (Node.Internal.Node);
         if Result = null then
            return No_Component_Def;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Component_Type;







         
   

   function F_Id
     (Node : Aspect_Assoc'Class) return Name
   is
      Result : Bare_Name;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Aspect_Assoc_F_Id (Node.Internal.Node);
         if Result = null then
            return No_Name;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Id;


         
   

   function F_Expr
     (Node : Aspect_Assoc'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Aspect_Assoc_F_Expr (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Expr;



         
   function P_Is_Ghost_Code
     (Node : Aspect_Assoc'Class) return Boolean is
      


      Property_Result : Boolean;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Aspect_Assoc_P_Is_Ghost_Code
            (Bare_Ada_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return Property_Result;

   end;



         
         ----------------
         -- List_Child --
         ----------------

         function List_Child
           (Node : Aspect_Assoc_List'Class; Index : Positive) return Aspect_Assoc
         is
            Result : Ada_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Result := Node.Child (Index);
            return Result.As_Aspect_Assoc;
         end List_Child;

         

         function Aspect_Assoc_List_First (Node : Aspect_Assoc_List) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return 1;
         end;

         function Aspect_Assoc_List_Next
           (Node : Aspect_Assoc_List; Cursor : Positive) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor + 1;
         end;

         function Aspect_Assoc_List_Has_Element
           (Node : Aspect_Assoc_List; Cursor : Positive) return Boolean is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor in 1 .. Node.Children_Count;
         end;

         function Aspect_Assoc_List_Element
           (Node : Aspect_Assoc_List; Cursor : Positive) return Aspect_Assoc'Class
         is
            Child : Ada_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Child := Node.Child (Cursor);
            return Aspect_Assoc'(Child.As_Aspect_Assoc);
         end;











         
   

   function F_Aspect_Assocs
     (Node : Aspect_Spec'Class) return Aspect_Assoc_List
   is
      Result : Bare_Aspect_Assoc_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Aspect_Spec_F_Aspect_Assocs (Node.Internal.Node);
         if Result = null then
            return No_Aspect_Assoc_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Aspect_Assocs;







         
   

   function F_Dest
     (Node : Assign_Stmt'Class) return Name
   is
      Result : Bare_Name;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Assign_Stmt_F_Dest (Node.Internal.Node);
         if Result = null then
            return No_Name;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Dest;


         
   

   function F_Expr
     (Node : Assign_Stmt'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Assign_Stmt_F_Expr (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Expr;





         
         ----------------
         -- List_Child --
         ----------------

         function List_Child
           (Node : Basic_Assoc_List'Class; Index : Positive) return Basic_Assoc
         is
            Result : Ada_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Result := Node.Child (Index);
            return Result.As_Basic_Assoc;
         end List_Child;

         

         function Basic_Assoc_List_First (Node : Basic_Assoc_List) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return 1;
         end;

         function Basic_Assoc_List_Next
           (Node : Basic_Assoc_List; Cursor : Positive) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor + 1;
         end;

         function Basic_Assoc_List_Has_Element
           (Node : Basic_Assoc_List; Cursor : Positive) return Boolean is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor in 1 .. Node.Children_Count;
         end;

         function Basic_Assoc_List_Element
           (Node : Basic_Assoc_List; Cursor : Positive) return Basic_Assoc'Class
         is
            Child : Ada_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Child := Node.Child (Cursor);
            return Basic_Assoc'(Child.As_Basic_Assoc);
         end;







         
   function P_Zip_With_Params
     (Node : Assoc_List'Class;
      Imprecise_Fallback : Boolean := False) return Param_Actual_Array is
      


         Internal_Arg_Imprecise_Fallback : Boolean;
      Property_Result : Internal_Param_Actual_Array_Access;

         procedure Free_Internal;
         --  Dec-ref all internal arguments and the property result, when
         --  applicable.

         -------------------
         -- Free_Internal --
         -------------------

         procedure Free_Internal is
         begin
               Dec_Ref (Property_Result);
         end Free_Internal;

   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Imprecise_Fallback :=
            Imprecise_Fallback;

      
      Property_Result :=
         Libadalang.Implementation.Assoc_List_P_Zip_With_Params
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Imprecise_Fallback, E_Info => Node.Internal.Info);

         return Result : constant Param_Actual_Array :=
            To_Public_Param_Actual_Array (Property_Result)
         do
            Free_Internal;
         end return;

      exception
         when Precondition_Failure | Property_Error =>
            Free_Internal;
            raise;
   end;





         
   

   function F_Name
     (Node : At_Clause'Class) return Base_Id
   is
      Result : Bare_Base_Id;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.At_Clause_F_Name (Node.Internal.Node);
         if Result = null then
            return No_Base_Id;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Name;


         
   

   function F_Expr
     (Node : At_Clause'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.At_Clause_F_Expr (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Expr;







         
   

   function F_Attribute_Expr
     (Node : Attribute_Def_Clause'Class) return Name
   is
      Result : Bare_Name;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Attribute_Def_Clause_F_Attribute_Expr (Node.Internal.Node);
         if Result = null then
            return No_Name;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Attribute_Expr;


         
   

   function F_Expr
     (Node : Attribute_Def_Clause'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Attribute_Def_Clause_F_Expr (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Expr;







         
   

   function F_Prefix
     (Node : Attribute_Ref'Class) return Name
   is
      Result : Bare_Name;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Attribute_Ref_F_Prefix (Node.Internal.Node);
         if Result = null then
            return No_Name;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Prefix;


         
   

   function F_Attribute
     (Node : Attribute_Ref'Class) return Identifier
   is
      Result : Bare_Identifier;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Attribute_Ref_F_Attribute (Node.Internal.Node);
         if Result = null then
            return No_Identifier;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Attribute;


         
   

   function F_Args
     (Node : Attribute_Ref'Class) return Assoc_List
   is
      Result : Bare_Assoc_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Attribute_Ref_F_Args (Node.Internal.Node);
         if Result = null then
            return No_Assoc_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Args;








         
   function P_Assoc_Expr
     (Node : Base_Assoc'Class) return Expr is
      


      Property_Result : Internal_Entity_Expr;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Dispatcher_Base_Assoc_P_Assoc_Expr
            (Bare_Ada_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return Wrap_Node (Property_Result.Node, Property_Result.Info).As_Expr;

   end;



         
         ----------------
         -- List_Child --
         ----------------

         function List_Child
           (Node : Base_Assoc_List'Class; Index : Positive) return Base_Assoc
         is
            Result : Ada_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Result := Node.Child (Index);
            return Result.As_Base_Assoc;
         end List_Child;

         

         function Base_Assoc_List_First (Node : Base_Assoc_List) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return 1;
         end;

         function Base_Assoc_List_Next
           (Node : Base_Assoc_List; Cursor : Positive) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor + 1;
         end;

         function Base_Assoc_List_Has_Element
           (Node : Base_Assoc_List; Cursor : Positive) return Boolean is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor in 1 .. Node.Children_Count;
         end;

         function Base_Assoc_List_Element
           (Node : Base_Assoc_List; Cursor : Positive) return Base_Assoc'Class
         is
            Child : Ada_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Child := Node.Child (Cursor);
            return Base_Assoc'(Child.As_Base_Assoc);
         end;







         
   function P_Formal_Type
     (Node : Base_Formal_Param_Decl'Class;
      Origin : Ada_Node'Class := No_Ada_Node) return Base_Type_Decl is
      


         Internal_Arg_Origin : Bare_Ada_Node;
      Property_Result : Internal_Entity_Base_Type_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Origin :=
            Origin.Internal.Node;

      
      Property_Result :=
         Libadalang.Implementation.Base_Formal_Param_Decl_P_Formal_Type
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Origin, E_Info => Node.Internal.Info);

         return Wrap_Node (Property_Result.Node, Property_Result.Info).As_Base_Type_Decl;

   end;






         
   function P_Abstract_Formal_Params
     (Node : Base_Formal_Param_Holder'Class) return Base_Formal_Param_Decl_Array is
      


      Property_Result : Internal_Entity_Base_Formal_Param_Decl_Array_Access;

         procedure Free_Internal;
         --  Dec-ref all internal arguments and the property result, when
         --  applicable.

         -------------------
         -- Free_Internal --
         -------------------

         procedure Free_Internal is
         begin
               Dec_Ref (Property_Result);
         end Free_Internal;

   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Dispatcher_Base_Formal_Param_Holder_P_Abstract_Formal_Params
            (Bare_Ada_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return Result : constant Base_Formal_Param_Decl_Array :=
            To_Public_Base_Formal_Param_Decl_Array (Property_Result)
         do
            Free_Internal;
         end return;

      exception
         when Precondition_Failure | Property_Error =>
            Free_Internal;
            raise;
   end;

         
   function P_Formal_Params
     (Node : Base_Formal_Param_Holder'Class) return Defining_Name_Array is
      


      Property_Result : Internal_Entity_Defining_Name_Array_Access;

         procedure Free_Internal;
         --  Dec-ref all internal arguments and the property result, when
         --  applicable.

         -------------------
         -- Free_Internal --
         -------------------

         procedure Free_Internal is
         begin
               Dec_Ref (Property_Result);
         end Free_Internal;

   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Base_Formal_Param_Holder_P_Formal_Params
            (Bare_Ada_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return Result : constant Defining_Name_Array :=
            To_Public_Defining_Name_Array (Property_Result)
         do
            Free_Internal;
         end return;

      exception
         when Precondition_Failure | Property_Error =>
            Free_Internal;
            raise;
   end;

         
   function P_Nb_Min_Params
     (Node : Base_Formal_Param_Holder'Class) return Integer is
      


      Property_Result : Integer;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Base_Formal_Param_Holder_P_Nb_Min_Params
            (Bare_Ada_Node (Node.Internal.Node));

         return Property_Result;

   end;

         
   function P_Nb_Max_Params
     (Node : Base_Formal_Param_Holder'Class) return Integer is
      


      Property_Result : Integer;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Base_Formal_Param_Holder_P_Nb_Max_Params
            (Bare_Ada_Node (Node.Internal.Node));

         return Property_Result;

   end;

         
   function P_Param_Types
     (Node : Base_Formal_Param_Holder'Class;
      Origin : Ada_Node'Class := No_Ada_Node) return Base_Type_Decl_Array is
      


         Internal_Arg_Origin : Bare_Ada_Node;
      Property_Result : Internal_Entity_Base_Type_Decl_Array_Access;

         procedure Free_Internal;
         --  Dec-ref all internal arguments and the property result, when
         --  applicable.

         -------------------
         -- Free_Internal --
         -------------------

         procedure Free_Internal is
         begin
               Dec_Ref (Property_Result);
         end Free_Internal;

   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Origin :=
            Origin.Internal.Node;

      
      Property_Result :=
         Libadalang.Implementation.Base_Formal_Param_Holder_P_Param_Types
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Origin, E_Info => Node.Internal.Info);

         return Result : constant Base_Type_Decl_Array :=
            To_Public_Base_Type_Decl_Array (Property_Result)
         do
            Free_Internal;
         end return;

      exception
         when Precondition_Failure | Property_Error =>
            Free_Internal;
            raise;
   end;





         
   

   function F_Spec
     (Node : Base_Loop_Stmt'Class) return Loop_Spec
   is
      Result : Bare_Loop_Spec;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Base_Loop_Stmt_F_Spec (Node.Internal.Node);
         if Result = null then
            return No_Loop_Spec;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Spec;


         
   

   function F_Stmts
     (Node : Base_Loop_Stmt'Class) return Stmt_List
   is
      Result : Bare_Stmt_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Base_Loop_Stmt_F_Stmts (Node.Internal.Node);
         if Result = null then
            return No_Stmt_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Stmts;


         
   

   function F_End_Name
     (Node : Base_Loop_Stmt'Class) return End_Name
   is
      Result : Bare_End_Name;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Base_Loop_Stmt_F_End_Name (Node.Internal.Node);
         if Result = null then
            return No_End_Name;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_End_Name;







         
   

   function F_Package_Name
     (Node : Base_Package_Decl'Class) return Defining_Name
   is
      Result : Bare_Defining_Name;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Base_Package_Decl_F_Package_Name (Node.Internal.Node);
         if Result = null then
            return No_Defining_Name;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Package_Name;


         
   

   function F_Public_Part
     (Node : Base_Package_Decl'Class) return Public_Part
   is
      Result : Bare_Public_Part;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Base_Package_Decl_F_Public_Part (Node.Internal.Node);
         if Result = null then
            return No_Public_Part;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Public_Part;


         
   

   function F_Private_Part
     (Node : Base_Package_Decl'Class) return Private_Part
   is
      Result : Bare_Private_Part;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Base_Package_Decl_F_Private_Part (Node.Internal.Node);
         if Result = null then
            return No_Private_Part;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Private_Part;


         
   

   function F_End_Name
     (Node : Base_Package_Decl'Class) return End_Name
   is
      Result : Bare_End_Name;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Base_Package_Decl_F_End_Name (Node.Internal.Node);
         if Result = null then
            return No_End_Name;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_End_Name;



         
   function P_Body_Part
     (Node : Base_Package_Decl'Class) return Package_Body is
      


      Property_Result : Internal_Entity_Package_Body;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Base_Package_Decl_P_Body_Part
            (Bare_Ada_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return Wrap_Node (Property_Result.Node, Property_Result.Info).As_Package_Body;

   end;





         
   

   function F_Components
     (Node : Base_Record_Def'Class) return Component_List
   is
      Result : Bare_Component_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Base_Record_Def_F_Components (Node.Internal.Node);
         if Result = null then
            return No_Component_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Components;








         
   function P_Previous_Part
     (Node : Body_Node'Class;
      Imprecise_Fallback : Boolean := False) return Basic_Decl is
      


         Internal_Arg_Imprecise_Fallback : Boolean;
      Property_Result : Internal_Entity_Basic_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Imprecise_Fallback :=
            Imprecise_Fallback;

      
      Property_Result :=
         Libadalang.Implementation.Body_Node_P_Previous_Part
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Imprecise_Fallback, E_Info => Node.Internal.Info);

         return Wrap_Node (Property_Result.Node, Property_Result.Info).As_Basic_Decl;

   end;

         
   function P_Decl_Part
     (Node : Body_Node'Class;
      Imprecise_Fallback : Boolean := False) return Basic_Decl is
      


         Internal_Arg_Imprecise_Fallback : Boolean;
      Property_Result : Internal_Entity_Basic_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Imprecise_Fallback :=
            Imprecise_Fallback;

      
      Property_Result :=
         Libadalang.Implementation.Body_Node_P_Decl_Part
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Imprecise_Fallback, E_Info => Node.Internal.Info);

         return Wrap_Node (Property_Result.Node, Property_Result.Info).As_Basic_Decl;

   end;

         
   function P_Subunit_Root
     (Node : Body_Node'Class) return Basic_Decl is
      


      Property_Result : Internal_Entity_Basic_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Body_Node_P_Subunit_Root
            (Bare_Ada_Node (Node.Internal.Node));

         return Wrap_Node (Property_Result.Node, Property_Result.Info).As_Basic_Decl;

   end;





         
   

   function F_Overriding
     (Node : Base_Subp_Body'Class) return Overriding_Node
   is
      Result : Bare_Overriding_Node;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Base_Subp_Body_F_Overriding (Node.Internal.Node);
         if Result = null then
            return No_Overriding_Node;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Overriding;

         function F_Overriding
           (Node : Base_Subp_Body'Class) return Ada_Overriding_Node
         is (Overriding_Node'(Node.F_Overriding).Kind);

         
   

   function F_Subp_Spec
     (Node : Base_Subp_Body'Class) return Subp_Spec
   is
      Result : Bare_Subp_Spec;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Base_Subp_Body_F_Subp_Spec (Node.Internal.Node);
         if Result = null then
            return No_Subp_Spec;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Subp_Spec;








         
   function P_Returns
     (Node : Base_Subp_Spec'Class) return Type_Expr is
      


      Property_Result : Internal_Entity_Type_Expr;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Dispatcher_Base_Subp_Spec_P_Returns
            (Bare_Ada_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return Wrap_Node (Property_Result.Node, Property_Result.Info).As_Type_Expr;

   end;

         
   function P_Params
     (Node : Base_Subp_Spec'Class) return Param_Spec_Array is
      


      Property_Result : Internal_Entity_Param_Spec_Array_Access;

         procedure Free_Internal;
         --  Dec-ref all internal arguments and the property result, when
         --  applicable.

         -------------------
         -- Free_Internal --
         -------------------

         procedure Free_Internal is
         begin
               Dec_Ref (Property_Result);
         end Free_Internal;

   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Dispatcher_Base_Subp_Spec_P_Params
            (Bare_Ada_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return Result : constant Param_Spec_Array :=
            To_Public_Param_Spec_Array (Property_Result)
         do
            Free_Internal;
         end return;

      exception
         when Precondition_Failure | Property_Error =>
            Free_Internal;
            raise;
   end;

         
   function P_Primitive_Subp_Types
     (Node : Base_Subp_Spec'Class;
      Imprecise_Fallback : Boolean := False) return Base_Type_Decl_Array is
      


         Internal_Arg_Imprecise_Fallback : Boolean;
      Property_Result : Internal_Entity_Base_Type_Decl_Array_Access;

         procedure Free_Internal;
         --  Dec-ref all internal arguments and the property result, when
         --  applicable.

         -------------------
         -- Free_Internal --
         -------------------

         procedure Free_Internal is
         begin
               Dec_Ref (Property_Result);
         end Free_Internal;

   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Imprecise_Fallback :=
            Imprecise_Fallback;

      
      Property_Result :=
         Libadalang.Implementation.Base_Subp_Spec_P_Primitive_Subp_Types
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Imprecise_Fallback, E_Info => Node.Internal.Info);

         return Result : constant Base_Type_Decl_Array :=
            To_Public_Base_Type_Decl_Array (Property_Result)
         do
            Free_Internal;
         end return;

      exception
         when Precondition_Failure | Property_Error =>
            Free_Internal;
            raise;
   end;

         
   function P_Primitive_Subp_First_Type
     (Node : Base_Subp_Spec'Class;
      Imprecise_Fallback : Boolean := False) return Base_Type_Decl is
      


         Internal_Arg_Imprecise_Fallback : Boolean;
      Property_Result : Internal_Entity_Base_Type_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Imprecise_Fallback :=
            Imprecise_Fallback;

      
      Property_Result :=
         Libadalang.Implementation.Base_Subp_Spec_P_Primitive_Subp_First_Type
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Imprecise_Fallback, E_Info => Node.Internal.Info);

         return Wrap_Node (Property_Result.Node, Property_Result.Info).As_Base_Type_Decl;

   end;

         
   function P_Primitive_Subp_Tagged_Type
     (Node : Base_Subp_Spec'Class;
      Imprecise_Fallback : Boolean := False) return Base_Type_Decl is
      


         Internal_Arg_Imprecise_Fallback : Boolean;
      Property_Result : Internal_Entity_Base_Type_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Imprecise_Fallback :=
            Imprecise_Fallback;

      
      Property_Result :=
         Libadalang.Implementation.Base_Subp_Spec_P_Primitive_Subp_Tagged_Type
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Imprecise_Fallback, E_Info => Node.Internal.Info);

         return Wrap_Node (Property_Result.Node, Property_Result.Info).As_Base_Type_Decl;

   end;

         
   function P_Return_Type
     (Node : Base_Subp_Spec'Class;
      Origin : Ada_Node'Class := No_Ada_Node) return Base_Type_Decl is
      


         Internal_Arg_Origin : Bare_Ada_Node;
      Property_Result : Internal_Entity_Base_Type_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Origin :=
            Origin.Internal.Node;

      
      Property_Result :=
         Libadalang.Implementation.Base_Subp_Spec_P_Return_Type
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Origin, E_Info => Node.Internal.Info);

         return Wrap_Node (Property_Result.Node, Property_Result.Info).As_Base_Type_Decl;

   end;






         
   function P_Get_Type
     (Node : Base_Subtype_Decl'Class;
      Origin : Ada_Node'Class := No_Ada_Node) return Base_Type_Decl is
      


         Internal_Arg_Origin : Bare_Ada_Node;
      Property_Result : Internal_Entity_Base_Type_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Origin :=
            Origin.Internal.Node;

      
      Property_Result :=
         Libadalang.Implementation.Dispatcher_Base_Subtype_Decl_P_Get_Type
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Origin, E_Info => Node.Internal.Info);

         return Wrap_Node (Property_Result.Node, Property_Result.Info).As_Base_Type_Decl;

   end;



         
         ----------------
         -- List_Child --
         ----------------

         function List_Child
           (Node : Basic_Decl_List'Class; Index : Positive) return Basic_Decl
         is
            Result : Ada_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Result := Node.Child (Index);
            return Result.As_Basic_Decl;
         end List_Child;

         

         function Basic_Decl_List_First (Node : Basic_Decl_List) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return 1;
         end;

         function Basic_Decl_List_Next
           (Node : Basic_Decl_List; Cursor : Positive) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor + 1;
         end;

         function Basic_Decl_List_Has_Element
           (Node : Basic_Decl_List; Cursor : Positive) return Boolean is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor in 1 .. Node.Children_Count;
         end;

         function Basic_Decl_List_Element
           (Node : Basic_Decl_List; Cursor : Positive) return Basic_Decl'Class
         is
            Child : Ada_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Child := Node.Child (Cursor);
            return Basic_Decl'(Child.As_Basic_Decl);
         end;











         
   

   function F_Stmts
     (Node : Begin_Block'Class) return Handled_Stmts
   is
      Result : Bare_Handled_Stmts;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Begin_Block_F_Stmts (Node.Internal.Node);
         if Result = null then
            return No_Handled_Stmts;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Stmts;


         
   

   function F_End_Name
     (Node : Begin_Block'Class) return End_Name
   is
      Result : Bare_End_Name;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Begin_Block_F_End_Name (Node.Internal.Node);
         if Result = null then
            return No_End_Name;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_End_Name;







         
   

   function F_Left
     (Node : Bin_Op'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Bin_Op_F_Left (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Left;


         
   

   function F_Op
     (Node : Bin_Op'Class) return Op
   is
      Result : Bare_Op;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Bin_Op_F_Op (Node.Internal.Node);
         if Result = null then
            return No_Op;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Op;

         function F_Op
           (Node : Bin_Op'Class) return Ada_Op
         is (Op'(Node.F_Op).Kind);

         
   

   function F_Right
     (Node : Bin_Op'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Bin_Op_F_Right (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Right;








         
   function P_Syntactic_Fully_Qualified_Name
     (Node : Body_Stub'Class) return Unbounded_Text_Type_Array is
      


      Property_Result : Symbol_Type_Array_Access;

         procedure Free_Internal;
         --  Dec-ref all internal arguments and the property result, when
         --  applicable.

         -------------------
         -- Free_Internal --
         -------------------

         procedure Free_Internal is
         begin
               Dec_Ref (Property_Result);
         end Free_Internal;

   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Body_Stub_P_Syntactic_Fully_Qualified_Name
            (Bare_Ada_Node (Node.Internal.Node));

         return Result : constant Unbounded_Text_Type_Array :=
            To_Public_Unbounded_Text_Type_Array (Property_Result)
         do
            Free_Internal;
         end return;

      exception
         when Precondition_Failure | Property_Error =>
            Free_Internal;
            raise;
   end;

























         
   

   function F_Name
     (Node : Call_Expr'Class) return Name
   is
      Result : Bare_Name;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Call_Expr_F_Name (Node.Internal.Node);
         if Result = null then
            return No_Name;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Name;


         
   

   function F_Suffix
     (Node : Call_Expr'Class) return Ada_Node
   is
      Result : Bare_Ada_Node;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Call_Expr_F_Suffix (Node.Internal.Node);
         if Result = null then
            return No_Ada_Node;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Suffix;



         
   function P_Kind
     (Node : Call_Expr'Class) return Call_Expr_Kind is
      


      Property_Result : Call_Expr_Kind;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Call_Expr_P_Kind
            (Bare_Ada_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return Property_Result;

   end;

         
   function P_Is_Array_Slice
     (Node : Call_Expr'Class) return Boolean is
      


      Property_Result : Boolean;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Call_Expr_P_Is_Array_Slice
            (Bare_Ada_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return Property_Result;

   end;





         
   

   function F_Call
     (Node : Call_Stmt'Class) return Name
   is
      Result : Bare_Name;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Call_Stmt_F_Call (Node.Internal.Node);
         if Result = null then
            return No_Name;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Call;








         
   function P_Dependent_Exprs
     (Node : Cond_Expr'Class) return Expr_Array is
      


      Property_Result : Internal_Entity_Expr_Array_Access;

         procedure Free_Internal;
         --  Dec-ref all internal arguments and the property result, when
         --  applicable.

         -------------------
         -- Free_Internal --
         -------------------

         procedure Free_Internal is
         begin
               Dec_Ref (Property_Result);
         end Free_Internal;

   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Dispatcher_Cond_Expr_P_Dependent_Exprs
            (Bare_Ada_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return Result : constant Expr_Array :=
            To_Public_Expr_Array (Property_Result)
         do
            Free_Internal;
         end return;

      exception
         when Precondition_Failure | Property_Error =>
            Free_Internal;
            raise;
   end;





         
   

   function F_Expr
     (Node : Case_Expr'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Case_Expr_F_Expr (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Expr;


         
   

   function F_Cases
     (Node : Case_Expr'Class) return Case_Expr_Alternative_List
   is
      Result : Bare_Case_Expr_Alternative_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Case_Expr_F_Cases (Node.Internal.Node);
         if Result = null then
            return No_Case_Expr_Alternative_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Cases;







         
   

   function F_Choices
     (Node : Case_Expr_Alternative'Class) return Alternatives_List
   is
      Result : Bare_Alternatives_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Case_Expr_Alternative_F_Choices (Node.Internal.Node);
         if Result = null then
            return No_Alternatives_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Choices;


         
   

   function F_Expr
     (Node : Case_Expr_Alternative'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Case_Expr_Alternative_F_Expr (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Expr;





         
         ----------------
         -- List_Child --
         ----------------

         function List_Child
           (Node : Case_Expr_Alternative_List'Class; Index : Positive) return Case_Expr_Alternative
         is
            Result : Ada_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Result := Node.Child (Index);
            return Result.As_Case_Expr_Alternative;
         end List_Child;

         

         function Case_Expr_Alternative_List_First (Node : Case_Expr_Alternative_List) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return 1;
         end;

         function Case_Expr_Alternative_List_Next
           (Node : Case_Expr_Alternative_List; Cursor : Positive) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor + 1;
         end;

         function Case_Expr_Alternative_List_Has_Element
           (Node : Case_Expr_Alternative_List; Cursor : Positive) return Boolean is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor in 1 .. Node.Children_Count;
         end;

         function Case_Expr_Alternative_List_Element
           (Node : Case_Expr_Alternative_List; Cursor : Positive) return Case_Expr_Alternative'Class
         is
            Child : Ada_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Child := Node.Child (Cursor);
            return Case_Expr_Alternative'(Child.As_Case_Expr_Alternative);
         end;






         
   

   function F_Expr
     (Node : Case_Stmt'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Case_Stmt_F_Expr (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Expr;


         
   

   function F_Pragmas
     (Node : Case_Stmt'Class) return Pragma_Node_List
   is
      Result : Bare_Pragma_Node_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Case_Stmt_F_Pragmas (Node.Internal.Node);
         if Result = null then
            return No_Pragma_Node_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Pragmas;


         
   

   function F_Alternatives
     (Node : Case_Stmt'Class) return Case_Stmt_Alternative_List
   is
      Result : Bare_Case_Stmt_Alternative_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Case_Stmt_F_Alternatives (Node.Internal.Node);
         if Result = null then
            return No_Case_Stmt_Alternative_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Alternatives;







         
   

   function F_Choices
     (Node : Case_Stmt_Alternative'Class) return Alternatives_List
   is
      Result : Bare_Alternatives_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Case_Stmt_Alternative_F_Choices (Node.Internal.Node);
         if Result = null then
            return No_Alternatives_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Choices;


         
   

   function F_Stmts
     (Node : Case_Stmt_Alternative'Class) return Stmt_List
   is
      Result : Bare_Stmt_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Case_Stmt_Alternative_F_Stmts (Node.Internal.Node);
         if Result = null then
            return No_Stmt_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Stmts;





         
         ----------------
         -- List_Child --
         ----------------

         function List_Child
           (Node : Case_Stmt_Alternative_List'Class; Index : Positive) return Case_Stmt_Alternative
         is
            Result : Ada_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Result := Node.Child (Index);
            return Result.As_Case_Stmt_Alternative;
         end List_Child;

         

         function Case_Stmt_Alternative_List_First (Node : Case_Stmt_Alternative_List) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return 1;
         end;

         function Case_Stmt_Alternative_List_Next
           (Node : Case_Stmt_Alternative_List; Cursor : Positive) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor + 1;
         end;

         function Case_Stmt_Alternative_List_Has_Element
           (Node : Case_Stmt_Alternative_List; Cursor : Positive) return Boolean is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor in 1 .. Node.Children_Count;
         end;

         function Case_Stmt_Alternative_List_Element
           (Node : Case_Stmt_Alternative_List; Cursor : Positive) return Case_Stmt_Alternative'Class
         is
            Child : Ada_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Child := Node.Child (Cursor);
            return Case_Stmt_Alternative'(Child.As_Case_Stmt_Alternative);
         end;







         
   function P_Denoted_Value
     (Node : Char_Literal'Class) return Character_Type is
      


      Property_Result : Character_Type;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Extensions.Char_Literal_P_Denoted_Value
            (Bare_Ada_Node (Node.Internal.Node));

         return Property_Result;

   end;










         
   

   function F_Prelude
     (Node : Compilation_Unit'Class) return Ada_Node_List
   is
      Result : Bare_Ada_Node_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Compilation_Unit_F_Prelude (Node.Internal.Node);
         if Result = null then
            return No_Ada_Node_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Prelude;


         
   

   function F_Body
     (Node : Compilation_Unit'Class) return Ada_Node
   is
      Result : Bare_Ada_Node;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Compilation_Unit_F_Body (Node.Internal.Node);
         if Result = null then
            return No_Ada_Node;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Body;


         
   

   function F_Pragmas
     (Node : Compilation_Unit'Class) return Pragma_Node_List
   is
      Result : Bare_Pragma_Node_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Compilation_Unit_F_Pragmas (Node.Internal.Node);
         if Result = null then
            return No_Pragma_Node_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Pragmas;



         
   function P_Syntactic_Fully_Qualified_Name
     (Node : Compilation_Unit'Class) return Unbounded_Text_Type_Array is
      


      Property_Result : Symbol_Type_Array_Access;

         procedure Free_Internal;
         --  Dec-ref all internal arguments and the property result, when
         --  applicable.

         -------------------
         -- Free_Internal --
         -------------------

         procedure Free_Internal is
         begin
               Dec_Ref (Property_Result);
         end Free_Internal;

   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Compilation_Unit_P_Syntactic_Fully_Qualified_Name
            (Bare_Ada_Node (Node.Internal.Node));

         return Result : constant Unbounded_Text_Type_Array :=
            To_Public_Unbounded_Text_Type_Array (Property_Result)
         do
            Free_Internal;
         end return;

      exception
         when Precondition_Failure | Property_Error =>
            Free_Internal;
            raise;
   end;

         
   function P_Unit_Kind
     (Node : Compilation_Unit'Class) return Analysis_Unit_Kind is
      


      Property_Result : Analysis_Unit_Kind;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Compilation_Unit_P_Unit_Kind
            (Bare_Ada_Node (Node.Internal.Node));

         return Property_Result;

   end;

         
   function P_Withed_Units
     (Node : Compilation_Unit'Class) return Compilation_Unit_Array is
      


      Property_Result : Internal_Entity_Compilation_Unit_Array_Access;

         procedure Free_Internal;
         --  Dec-ref all internal arguments and the property result, when
         --  applicable.

         -------------------
         -- Free_Internal --
         -------------------

         procedure Free_Internal is
         begin
               Dec_Ref (Property_Result);
         end Free_Internal;

   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Compilation_Unit_P_Withed_Units
            (Bare_Ada_Node (Node.Internal.Node));

         return Result : constant Compilation_Unit_Array :=
            To_Public_Compilation_Unit_Array (Property_Result)
         do
            Free_Internal;
         end return;

      exception
         when Precondition_Failure | Property_Error =>
            Free_Internal;
            raise;
   end;

         
   function P_Imported_Units
     (Node : Compilation_Unit'Class) return Compilation_Unit_Array is
      


      Property_Result : Internal_Entity_Compilation_Unit_Array_Access;

         procedure Free_Internal;
         --  Dec-ref all internal arguments and the property result, when
         --  applicable.

         -------------------
         -- Free_Internal --
         -------------------

         procedure Free_Internal is
         begin
               Dec_Ref (Property_Result);
         end Free_Internal;

   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Compilation_Unit_P_Imported_Units
            (Bare_Ada_Node (Node.Internal.Node));

         return Result : constant Compilation_Unit_Array :=
            To_Public_Compilation_Unit_Array (Property_Result)
         do
            Free_Internal;
         end return;

      exception
         when Precondition_Failure | Property_Error =>
            Free_Internal;
            raise;
   end;

         
   function P_Unit_Dependencies
     (Node : Compilation_Unit'Class) return Compilation_Unit_Array is
      


      Property_Result : Internal_Entity_Compilation_Unit_Array_Access;

         procedure Free_Internal;
         --  Dec-ref all internal arguments and the property result, when
         --  applicable.

         -------------------
         -- Free_Internal --
         -------------------

         procedure Free_Internal is
         begin
               Dec_Ref (Property_Result);
         end Free_Internal;

   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Compilation_Unit_P_Unit_Dependencies
            (Bare_Ada_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return Result : constant Compilation_Unit_Array :=
            To_Public_Compilation_Unit_Array (Property_Result)
         do
            Free_Internal;
         end return;

      exception
         when Precondition_Failure | Property_Error =>
            Free_Internal;
            raise;
   end;

         
   function P_Decl
     (Node : Compilation_Unit'Class) return Basic_Decl is
      


      Property_Result : Bare_Basic_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Compilation_Unit_P_Decl
            (Bare_Ada_Node (Node.Internal.Node));

         return Wrap_Node (Property_Result, No_Entity_Info).As_Basic_Decl;

   end;

         
   function P_Is_Preelaborable
     (Node : Compilation_Unit'Class;
      Imprecise_Fallback : Boolean := False) return Boolean is
      


         Internal_Arg_Imprecise_Fallback : Boolean;
      Property_Result : Boolean;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Imprecise_Fallback :=
            Imprecise_Fallback;

      
      Property_Result :=
         Libadalang.Implementation.Compilation_Unit_P_Is_Preelaborable
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Imprecise_Fallback, E_Info => Node.Internal.Info);

         return Property_Result;

   end;

         
   function P_Other_Part
     (Node : Compilation_Unit'Class) return Compilation_Unit is
      


      Property_Result : Internal_Entity_Compilation_Unit;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Compilation_Unit_P_Other_Part
            (Bare_Ada_Node (Node.Internal.Node));

         return Wrap_Node (Property_Result.Node, Property_Result.Info).As_Compilation_Unit;

   end;

         
   function P_Has_Restriction
     (Node : Compilation_Unit'Class;
      Name : Unbounded_Text_Type) return Boolean is
      


         Internal_Arg_Name : Symbol_Type;
      Property_Result : Boolean;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Name :=
            Lookup_Symbol (Node.Internal.Node.Unit.Context, To_Text (Name));

      
      Property_Result :=
         Libadalang.Implementation.Compilation_Unit_P_Has_Restriction
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Name);

         return Property_Result;

   end;

         
   function P_All_Config_Pragmas
     (Node : Compilation_Unit'Class) return Pragma_Node_Array is
      


      Property_Result : Internal_Entity_Pragma_Node_Array_Access;

         procedure Free_Internal;
         --  Dec-ref all internal arguments and the property result, when
         --  applicable.

         -------------------
         -- Free_Internal --
         -------------------

         procedure Free_Internal is
         begin
               Dec_Ref (Property_Result);
         end Free_Internal;

   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Compilation_Unit_P_All_Config_Pragmas
            (Bare_Ada_Node (Node.Internal.Node));

         return Result : constant Pragma_Node_Array :=
            To_Public_Pragma_Node_Array (Property_Result)
         do
            Free_Internal;
         end return;

      exception
         when Precondition_Failure | Property_Error =>
            Free_Internal;
            raise;
   end;

         
   function P_Config_Pragmas
     (Node : Compilation_Unit'Class;
      Name : Unbounded_Text_Type) return Pragma_Node_Array is
      


         Internal_Arg_Name : Symbol_Type;
      Property_Result : Internal_Entity_Pragma_Node_Array_Access;

         procedure Free_Internal;
         --  Dec-ref all internal arguments and the property result, when
         --  applicable.

         -------------------
         -- Free_Internal --
         -------------------

         procedure Free_Internal is
         begin
               Dec_Ref (Property_Result);
         end Free_Internal;

   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Name :=
            Lookup_Symbol (Node.Internal.Node.Unit.Context, To_Text (Name));

      
      Property_Result :=
         Libadalang.Implementation.Compilation_Unit_P_Config_Pragmas
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Name);

         return Result : constant Pragma_Node_Array :=
            To_Public_Pragma_Node_Array (Property_Result)
         do
            Free_Internal;
         end return;

      exception
         when Precondition_Failure | Property_Error =>
            Free_Internal;
            raise;
   end;



         
         ----------------
         -- List_Child --
         ----------------

         function List_Child
           (Node : Compilation_Unit_List'Class; Index : Positive) return Compilation_Unit
         is
            Result : Ada_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Result := Node.Child (Index);
            return Result.As_Compilation_Unit;
         end List_Child;

         

         function Compilation_Unit_List_First (Node : Compilation_Unit_List) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return 1;
         end;

         function Compilation_Unit_List_Next
           (Node : Compilation_Unit_List; Cursor : Positive) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor + 1;
         end;

         function Compilation_Unit_List_Has_Element
           (Node : Compilation_Unit_List; Cursor : Positive) return Boolean is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor in 1 .. Node.Children_Count;
         end;

         function Compilation_Unit_List_Element
           (Node : Compilation_Unit_List; Cursor : Positive) return Compilation_Unit'Class
         is
            Child : Ada_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Child := Node.Child (Cursor);
            return Compilation_Unit'(Child.As_Compilation_Unit);
         end;






         
   

   function F_Id
     (Node : Component_Clause'Class) return Identifier
   is
      Result : Bare_Identifier;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Component_Clause_F_Id (Node.Internal.Node);
         if Result = null then
            return No_Identifier;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Id;


         
   

   function F_Position
     (Node : Component_Clause'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Component_Clause_F_Position (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Position;


         
   

   function F_Range
     (Node : Component_Clause'Class) return Range_Spec
   is
      Result : Bare_Range_Spec;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Component_Clause_F_Range (Node.Internal.Node);
         if Result = null then
            return No_Range_Spec;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Range;







         
   

   function F_Ids
     (Node : Component_Decl'Class) return Defining_Name_List
   is
      Result : Bare_Defining_Name_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Component_Decl_F_Ids (Node.Internal.Node);
         if Result = null then
            return No_Defining_Name_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Ids;


         
   

   function F_Component_Def
     (Node : Component_Decl'Class) return Component_Def
   is
      Result : Bare_Component_Def;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Component_Decl_F_Component_Def (Node.Internal.Node);
         if Result = null then
            return No_Component_Def;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Component_Def;


         
   

   function F_Default_Expr
     (Node : Component_Decl'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Component_Decl_F_Default_Expr (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Default_Expr;







         
   

   function F_Has_Aliased
     (Node : Component_Def'Class) return Aliased_Node
   is
      Result : Bare_Aliased_Node;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Component_Def_F_Has_Aliased (Node.Internal.Node);
         if Result = null then
            return No_Aliased_Node;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Has_Aliased;

         function F_Has_Aliased (Node : Component_Def'Class) return Boolean
         is (Aliased_Node'(Node.F_Has_Aliased).Kind
             = Ada_Aliased_Present);


         
   

   function F_Has_Constant
     (Node : Component_Def'Class) return Constant_Node
   is
      Result : Bare_Constant_Node;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Component_Def_F_Has_Constant (Node.Internal.Node);
         if Result = null then
            return No_Constant_Node;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Has_Constant;

         function F_Has_Constant (Node : Component_Def'Class) return Boolean
         is (Constant_Node'(Node.F_Has_Constant).Kind
             = Ada_Constant_Present);


         
   

   function F_Type_Expr
     (Node : Component_Def'Class) return Type_Expr
   is
      Result : Bare_Type_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Component_Def_F_Type_Expr (Node.Internal.Node);
         if Result = null then
            return No_Type_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Type_Expr;







         
   

   function F_Components
     (Node : Component_List'Class) return Ada_Node_List
   is
      Result : Bare_Ada_Node_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Component_List_F_Components (Node.Internal.Node);
         if Result = null then
            return No_Ada_Node_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Components;


         
   

   function F_Variant_Part
     (Node : Component_List'Class) return Variant_Part
   is
      Result : Bare_Variant_Part;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Component_List_F_Variant_Part (Node.Internal.Node);
         if Result = null then
            return No_Variant_Part;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Variant_Part;












         
   

   function F_Constraints
     (Node : Composite_Constraint'Class) return Assoc_List
   is
      Result : Bare_Assoc_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Composite_Constraint_F_Constraints (Node.Internal.Node);
         if Result = null then
            return No_Assoc_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Constraints;



         
   function P_Is_Index_Constraint
     (Node : Composite_Constraint'Class) return Boolean is
      


      Property_Result : Boolean;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Composite_Constraint_P_Is_Index_Constraint
            (Bare_Ada_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return Property_Result;

   end;

         
   function P_Is_Discriminant_Constraint
     (Node : Composite_Constraint'Class) return Boolean is
      


      Property_Result : Boolean;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Composite_Constraint_P_Is_Discriminant_Constraint
            (Bare_Ada_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return Property_Result;

   end;





         
   

   function F_Ids
     (Node : Composite_Constraint_Assoc'Class) return Discriminant_Choice_List
   is
      Result : Bare_Discriminant_Choice_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Composite_Constraint_Assoc_F_Ids (Node.Internal.Node);
         if Result = null then
            return No_Discriminant_Choice_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Ids;


         
   

   function F_Constraint_Expr
     (Node : Composite_Constraint_Assoc'Class) return Ada_Node
   is
      Result : Bare_Ada_Node;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Composite_Constraint_Assoc_F_Constraint_Expr (Node.Internal.Node);
         if Result = null then
            return No_Ada_Node;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Constraint_Expr;







         
   

   function F_First_Operand
     (Node : Concat_Op'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Concat_Op_F_First_Operand (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_First_Operand;


         
   

   function F_Other_Operands
     (Node : Concat_Op'Class) return Concat_Operand_List
   is
      Result : Bare_Concat_Operand_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Concat_Op_F_Other_Operands (Node.Internal.Node);
         if Result = null then
            return No_Concat_Operand_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Other_Operands;



         
   function P_Operands
     (Node : Concat_Op'Class) return Expr_Array is
      


      Property_Result : Internal_Entity_Expr_Array_Access;

         procedure Free_Internal;
         --  Dec-ref all internal arguments and the property result, when
         --  applicable.

         -------------------
         -- Free_Internal --
         -------------------

         procedure Free_Internal is
         begin
               Dec_Ref (Property_Result);
         end Free_Internal;

   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Concat_Op_P_Operands
            (Bare_Ada_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return Result : constant Expr_Array :=
            To_Public_Expr_Array (Property_Result)
         do
            Free_Internal;
         end return;

      exception
         when Precondition_Failure | Property_Error =>
            Free_Internal;
            raise;
   end;





         
   

   function F_Operator
     (Node : Concat_Operand'Class) return Op_Concat
   is
      Result : Bare_Op_Concat;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Concat_Operand_F_Operator (Node.Internal.Node);
         if Result = null then
            return No_Op_Concat;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Operator;


         
   

   function F_Operand
     (Node : Concat_Operand'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Concat_Operand_F_Operand (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Operand;





         
         ----------------
         -- List_Child --
         ----------------

         function List_Child
           (Node : Concat_Operand_List'Class; Index : Positive) return Concat_Operand
         is
            Result : Ada_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Result := Node.Child (Index);
            return Result.As_Concat_Operand;
         end List_Child;

         

         function Concat_Operand_List_First (Node : Concat_Operand_List) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return 1;
         end;

         function Concat_Operand_List_Next
           (Node : Concat_Operand_List; Cursor : Positive) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor + 1;
         end;

         function Concat_Operand_List_Has_Element
           (Node : Concat_Operand_List; Cursor : Positive) return Boolean is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor in 1 .. Node.Children_Count;
         end;

         function Concat_Operand_List_Element
           (Node : Concat_Operand_List; Cursor : Positive) return Concat_Operand'Class
         is
            Child : Ada_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Child := Node.Child (Cursor);
            return Concat_Operand'(Child.As_Concat_Operand);
         end;

















         
   function P_As_Bool
     (Node : Constant_Node'Class) return Boolean is
      


      Property_Result : Boolean;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Dispatcher_Constant_Node_P_As_Bool
            (Bare_Ada_Node (Node.Internal.Node));

         return Property_Result;

   end;















         
   

   function F_List
     (Node : Constrained_Array_Indices'Class) return Constraint_List
   is
      Result : Bare_Constraint_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Constrained_Array_Indices_F_List (Node.Internal.Node);
         if Result = null then
            return No_Constraint_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_List;







         
   

   function F_Has_Not_Null
     (Node : Subtype_Indication'Class) return Not_Null
   is
      Result : Bare_Not_Null;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Subtype_Indication_F_Has_Not_Null (Node.Internal.Node);
         if Result = null then
            return No_Not_Null;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Has_Not_Null;

         function F_Has_Not_Null (Node : Subtype_Indication'Class) return Boolean
         is (Not_Null'(Node.F_Has_Not_Null).Kind
             = Ada_Not_Null_Present);


         
   

   function F_Name
     (Node : Subtype_Indication'Class) return Name
   is
      Result : Bare_Name;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Subtype_Indication_F_Name (Node.Internal.Node);
         if Result = null then
            return No_Name;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Name;


         
   

   function F_Constraint
     (Node : Subtype_Indication'Class) return Constraint
   is
      Result : Bare_Constraint;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Subtype_Indication_F_Constraint (Node.Internal.Node);
         if Result = null then
            return No_Constraint;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Constraint;



         
   function P_Subtype_Constraints
     (Node : Subtype_Indication'Class) return Param_Actual_Array is
      


      Property_Result : Internal_Param_Actual_Array_Access;

         procedure Free_Internal;
         --  Dec-ref all internal arguments and the property result, when
         --  applicable.

         -------------------
         -- Free_Internal --
         -------------------

         procedure Free_Internal is
         begin
               Dec_Ref (Property_Result);
         end Free_Internal;

   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Subtype_Indication_P_Subtype_Constraints
            (Bare_Ada_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return Result : constant Param_Actual_Array :=
            To_Public_Param_Actual_Array (Property_Result)
         do
            Free_Internal;
         end return;

      exception
         when Precondition_Failure | Property_Error =>
            Free_Internal;
            raise;
   end;

         
   function P_Is_Static_Subtype
     (Node : Subtype_Indication'Class;
      Imprecise_Fallback : Boolean := False) return Boolean is
      


         Internal_Arg_Imprecise_Fallback : Boolean;
      Property_Result : Boolean;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Imprecise_Fallback :=
            Imprecise_Fallback;

      
      Property_Result :=
         Libadalang.Implementation.Subtype_Indication_P_Is_Static_Subtype
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Imprecise_Fallback, E_Info => Node.Internal.Info);

         return Property_Result;

   end;















         
   

   function F_Guard
     (Node : Contract_Case_Assoc'Class) return Ada_Node
   is
      Result : Bare_Ada_Node;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Contract_Case_Assoc_F_Guard (Node.Internal.Node);
         if Result = null then
            return No_Ada_Node;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Guard;


         
   

   function F_Consequence
     (Node : Contract_Case_Assoc'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Contract_Case_Assoc_F_Consequence (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Consequence;





         
         ----------------
         -- List_Child --
         ----------------

         function List_Child
           (Node : Contract_Case_Assoc_List'Class; Index : Positive) return Contract_Case_Assoc
         is
            Result : Ada_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Result := Node.Child (Index);
            return Result.As_Contract_Case_Assoc;
         end List_Child;

         

         function Contract_Case_Assoc_List_First (Node : Contract_Case_Assoc_List) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return 1;
         end;

         function Contract_Case_Assoc_List_Next
           (Node : Contract_Case_Assoc_List; Cursor : Positive) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor + 1;
         end;

         function Contract_Case_Assoc_List_Has_Element
           (Node : Contract_Case_Assoc_List; Cursor : Positive) return Boolean is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor in 1 .. Node.Children_Count;
         end;

         function Contract_Case_Assoc_List_Element
           (Node : Contract_Case_Assoc_List; Cursor : Positive) return Contract_Case_Assoc'Class
         is
            Child : Ada_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Child := Node.Child (Cursor);
            return Contract_Case_Assoc'(Child.As_Contract_Case_Assoc);
         end;






         
   

   function F_Contract_Cases
     (Node : Contract_Cases'Class) return Contract_Case_Assoc_List
   is
      Result : Bare_Contract_Case_Assoc_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Contract_Cases_F_Contract_Cases (Node.Internal.Node);
         if Result = null then
            return No_Contract_Case_Assoc_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Contract_Cases;












         
   

   function F_Delta
     (Node : Decimal_Fixed_Point_Def'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Decimal_Fixed_Point_Def_F_Delta (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Delta;


         
   

   function F_Digits
     (Node : Decimal_Fixed_Point_Def'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Decimal_Fixed_Point_Def_F_Digits (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Digits;


         
   

   function F_Range
     (Node : Decimal_Fixed_Point_Def'Class) return Range_Spec
   is
      Result : Bare_Range_Spec;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Decimal_Fixed_Point_Def_F_Range (Node.Internal.Node);
         if Result = null then
            return No_Range_Spec;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Range;







         
   

   function F_Decls
     (Node : Decl_Block'Class) return Declarative_Part
   is
      Result : Bare_Declarative_Part;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Decl_Block_F_Decls (Node.Internal.Node);
         if Result = null then
            return No_Declarative_Part;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Decls;


         
   

   function F_Stmts
     (Node : Decl_Block'Class) return Handled_Stmts
   is
      Result : Bare_Handled_Stmts;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Decl_Block_F_Stmts (Node.Internal.Node);
         if Result = null then
            return No_Handled_Stmts;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Stmts;


         
   

   function F_End_Name
     (Node : Decl_Block'Class) return End_Name
   is
      Result : Bare_End_Name;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Decl_Block_F_End_Name (Node.Internal.Node);
         if Result = null then
            return No_End_Name;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_End_Name;







         
   

   function F_Decls
     (Node : Decl_Expr'Class) return Basic_Decl_List
   is
      Result : Bare_Basic_Decl_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Decl_Expr_F_Decls (Node.Internal.Node);
         if Result = null then
            return No_Basic_Decl_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Decls;


         
   

   function F_Expr
     (Node : Decl_Expr'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Decl_Expr_F_Expr (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Expr;












         
   

   function F_Decls
     (Node : Declarative_Part'Class) return Ada_Node_List
   is
      Result : Bare_Ada_Node_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Declarative_Part_F_Decls (Node.Internal.Node);
         if Result = null then
            return No_Ada_Node_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Decls;







         
   

   function F_Name
     (Node : Defining_Name'Class) return Name
   is
      Result : Bare_Name;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Defining_Name_F_Name (Node.Internal.Node);
         if Result = null then
            return No_Name;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Name;



         
   function P_Canonical_Fully_Qualified_Name
     (Node : Defining_Name'Class) return Text_Type is
      


      Property_Result : String_Type;

         procedure Free_Internal;
         --  Dec-ref all internal arguments and the property result, when
         --  applicable.

         -------------------
         -- Free_Internal --
         -------------------

         procedure Free_Internal is
         begin
               Dec_Ref (Property_Result);
         end Free_Internal;

   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Defining_Name_P_Canonical_Fully_Qualified_Name
            (Bare_Ada_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return Result : constant Text_Type :=
            Property_Result.Content
         do
            Free_Internal;
         end return;

      exception
         when Precondition_Failure | Property_Error =>
            Free_Internal;
            raise;
   end;

         
   function P_Unique_Identifying_Name
     (Node : Defining_Name'Class) return Text_Type is
      


      Property_Result : String_Type;

         procedure Free_Internal;
         --  Dec-ref all internal arguments and the property result, when
         --  applicable.

         -------------------
         -- Free_Internal --
         -------------------

         procedure Free_Internal is
         begin
               Dec_Ref (Property_Result);
         end Free_Internal;

   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Defining_Name_P_Unique_Identifying_Name
            (Bare_Ada_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return Result : constant Text_Type :=
            Property_Result.Content
         do
            Free_Internal;
         end return;

      exception
         when Precondition_Failure | Property_Error =>
            Free_Internal;
            raise;
   end;

         
   function P_Fully_Qualified_Name_Array
     (Node : Defining_Name'Class) return Unbounded_Text_Type_Array is
      


      Property_Result : Symbol_Type_Array_Access;

         procedure Free_Internal;
         --  Dec-ref all internal arguments and the property result, when
         --  applicable.

         -------------------
         -- Free_Internal --
         -------------------

         procedure Free_Internal is
         begin
               Dec_Ref (Property_Result);
         end Free_Internal;

   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Defining_Name_P_Fully_Qualified_Name_Array
            (Bare_Ada_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return Result : constant Unbounded_Text_Type_Array :=
            To_Public_Unbounded_Text_Type_Array (Property_Result)
         do
            Free_Internal;
         end return;

      exception
         when Precondition_Failure | Property_Error =>
            Free_Internal;
            raise;
   end;

         
   function P_Fully_Qualified_Name
     (Node : Defining_Name'Class) return Text_Type is
      


      Property_Result : String_Type;

         procedure Free_Internal;
         --  Dec-ref all internal arguments and the property result, when
         --  applicable.

         -------------------
         -- Free_Internal --
         -------------------

         procedure Free_Internal is
         begin
               Dec_Ref (Property_Result);
         end Free_Internal;

   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Defining_Name_P_Fully_Qualified_Name
            (Bare_Ada_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return Result : constant Text_Type :=
            Property_Result.Content
         do
            Free_Internal;
         end return;

      exception
         when Precondition_Failure | Property_Error =>
            Free_Internal;
            raise;
   end;

         
   function P_Basic_Decl
     (Node : Defining_Name'Class) return Basic_Decl is
      


      Property_Result : Internal_Entity_Basic_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Defining_Name_P_Basic_Decl
            (Bare_Ada_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return Wrap_Node (Property_Result.Node, Property_Result.Info).As_Basic_Decl;

   end;

         
   function P_Find_Refs
     (Node : Defining_Name'Class;
      Root : Ada_Node'Class;
      Imprecise_Fallback : Boolean := False) return Ref_Result_Array is
      


         Internal_Arg_Root : Internal_Entity;
         Internal_Arg_Imprecise_Fallback : Boolean;
      Property_Result : Internal_Ref_Result_Array_Access;

         procedure Free_Internal;
         --  Dec-ref all internal arguments and the property result, when
         --  applicable.

         -------------------
         -- Free_Internal --
         -------------------

         procedure Free_Internal is
         begin
               Dec_Ref (Property_Result);
         end Free_Internal;

   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Root :=
            (Root.Internal.Node, Root.Internal.Info);
         Internal_Arg_Imprecise_Fallback :=
            Imprecise_Fallback;

      
      Property_Result :=
         Libadalang.Implementation.Defining_Name_P_Find_Refs
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Root, Internal_Arg_Imprecise_Fallback, E_Info => Node.Internal.Info);

         return Result : constant Ref_Result_Array :=
            To_Public_Ref_Result_Array (Property_Result)
         do
            Free_Internal;
         end return;

      exception
         when Precondition_Failure | Property_Error =>
            Free_Internal;
            raise;
   end;

         
   function P_Find_All_References
     (Node : Defining_Name'Class;
      Units : Analysis_Unit_Array;
      Follow_Renamings : Boolean := False;
      Imprecise_Fallback : Boolean := False) return Ref_Result_Array is
      


         Internal_Arg_Units : Internal_Unit_Array_Access;
         Internal_Arg_Follow_Renamings : Boolean;
         Internal_Arg_Imprecise_Fallback : Boolean;
      Property_Result : Internal_Ref_Result_Array_Access;

         procedure Free_Internal;
         --  Dec-ref all internal arguments and the property result, when
         --  applicable.

         -------------------
         -- Free_Internal --
         -------------------

         procedure Free_Internal is
         begin
               Dec_Ref (Internal_Arg_Units);
               Dec_Ref (Property_Result);
         end Free_Internal;

   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Units :=
            To_Internal_Analysis_Unit_Array (Units);
         Internal_Arg_Follow_Renamings :=
            Follow_Renamings;
         Internal_Arg_Imprecise_Fallback :=
            Imprecise_Fallback;

      
      Property_Result :=
         Libadalang.Implementation.Defining_Name_P_Find_All_References
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Units, Internal_Arg_Follow_Renamings, Internal_Arg_Imprecise_Fallback, E_Info => Node.Internal.Info);

         return Result : constant Ref_Result_Array :=
            To_Public_Ref_Result_Array (Property_Result)
         do
            Free_Internal;
         end return;

      exception
         when Precondition_Failure | Property_Error =>
            Free_Internal;
            raise;
   end;

         
   function P_Find_All_Calls
     (Node : Defining_Name'Class;
      Units : Analysis_Unit_Array;
      Follow_Renamings : Boolean := False;
      Imprecise_Fallback : Boolean := False) return Ref_Result_Array is
      


         Internal_Arg_Units : Internal_Unit_Array_Access;
         Internal_Arg_Follow_Renamings : Boolean;
         Internal_Arg_Imprecise_Fallback : Boolean;
      Property_Result : Internal_Ref_Result_Array_Access;

         procedure Free_Internal;
         --  Dec-ref all internal arguments and the property result, when
         --  applicable.

         -------------------
         -- Free_Internal --
         -------------------

         procedure Free_Internal is
         begin
               Dec_Ref (Internal_Arg_Units);
               Dec_Ref (Property_Result);
         end Free_Internal;

   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Units :=
            To_Internal_Analysis_Unit_Array (Units);
         Internal_Arg_Follow_Renamings :=
            Follow_Renamings;
         Internal_Arg_Imprecise_Fallback :=
            Imprecise_Fallback;

      
      Property_Result :=
         Libadalang.Implementation.Defining_Name_P_Find_All_Calls
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Units, Internal_Arg_Follow_Renamings, Internal_Arg_Imprecise_Fallback, E_Info => Node.Internal.Info);

         return Result : constant Ref_Result_Array :=
            To_Public_Ref_Result_Array (Property_Result)
         do
            Free_Internal;
         end return;

      exception
         when Precondition_Failure | Property_Error =>
            Free_Internal;
            raise;
   end;

         
   function P_Next_Part
     (Node : Defining_Name'Class;
      Imprecise_Fallback : Boolean := False) return Defining_Name is
      


         Internal_Arg_Imprecise_Fallback : Boolean;
      Property_Result : Internal_Entity_Defining_Name;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Imprecise_Fallback :=
            Imprecise_Fallback;

      
      Property_Result :=
         Libadalang.Implementation.Defining_Name_P_Next_Part
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Imprecise_Fallback, E_Info => Node.Internal.Info);

         return Wrap_Node (Property_Result.Node, Property_Result.Info).As_Defining_Name;

   end;

         
   function P_Previous_Part
     (Node : Defining_Name'Class;
      Imprecise_Fallback : Boolean := False) return Defining_Name is
      


         Internal_Arg_Imprecise_Fallback : Boolean;
      Property_Result : Internal_Entity_Defining_Name;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Imprecise_Fallback :=
            Imprecise_Fallback;

      
      Property_Result :=
         Libadalang.Implementation.Defining_Name_P_Previous_Part
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Imprecise_Fallback, E_Info => Node.Internal.Info);

         return Wrap_Node (Property_Result.Node, Property_Result.Info).As_Defining_Name;

   end;

         
   function P_Canonical_Part
     (Node : Defining_Name'Class;
      Imprecise_Fallback : Boolean := False) return Defining_Name is
      


         Internal_Arg_Imprecise_Fallback : Boolean;
      Property_Result : Internal_Entity_Defining_Name;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Imprecise_Fallback :=
            Imprecise_Fallback;

      
      Property_Result :=
         Libadalang.Implementation.Defining_Name_P_Canonical_Part
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Imprecise_Fallback, E_Info => Node.Internal.Info);

         return Wrap_Node (Property_Result.Node, Property_Result.Info).As_Defining_Name;

   end;

         
   function P_Most_Visible_Part
     (Node : Defining_Name'Class;
      Origin : Ada_Node'Class;
      Imprecise_Fallback : Boolean := False) return Defining_Name is
      


         Internal_Arg_Origin : Bare_Ada_Node;
         Internal_Arg_Imprecise_Fallback : Boolean;
      Property_Result : Internal_Entity_Defining_Name;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Origin :=
            Origin.Internal.Node;
         Internal_Arg_Imprecise_Fallback :=
            Imprecise_Fallback;

      
      Property_Result :=
         Libadalang.Implementation.Defining_Name_P_Most_Visible_Part
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Origin, Internal_Arg_Imprecise_Fallback, E_Info => Node.Internal.Info);

         return Wrap_Node (Property_Result.Node, Property_Result.Info).As_Defining_Name;

   end;

         
   function P_All_Parts
     (Node : Defining_Name'Class;
      Imprecise_Fallback : Boolean := False) return Defining_Name_Array is
      


         Internal_Arg_Imprecise_Fallback : Boolean;
      Property_Result : Internal_Entity_Defining_Name_Array_Access;

         procedure Free_Internal;
         --  Dec-ref all internal arguments and the property result, when
         --  applicable.

         -------------------
         -- Free_Internal --
         -------------------

         procedure Free_Internal is
         begin
               Dec_Ref (Property_Result);
         end Free_Internal;

   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Imprecise_Fallback :=
            Imprecise_Fallback;

      
      Property_Result :=
         Libadalang.Implementation.Defining_Name_P_All_Parts
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Imprecise_Fallback, E_Info => Node.Internal.Info);

         return Result : constant Defining_Name_Array :=
            To_Public_Defining_Name_Array (Property_Result)
         do
            Free_Internal;
         end return;

      exception
         when Precondition_Failure | Property_Error =>
            Free_Internal;
            raise;
   end;

         
   function P_Get_Aspect
     (Node : Defining_Name'Class;
      Name : Unbounded_Text_Type;
      Imprecise_Fallback : Boolean := False) return Aspect is
      


         Internal_Arg_Name : Symbol_Type;
         Internal_Arg_Imprecise_Fallback : Boolean;
      Property_Result : Internal_Aspect;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Name :=
            Lookup_Symbol (Node.Internal.Node.Unit.Context, To_Text (Name));
         Internal_Arg_Imprecise_Fallback :=
            Imprecise_Fallback;

      
      Property_Result :=
         Libadalang.Implementation.Defining_Name_P_Get_Aspect
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Name, Internal_Arg_Imprecise_Fallback, E_Info => Node.Internal.Info);

         return To_Public_Aspect (Property_Result);

   end;

         
   function P_Has_Aspect
     (Node : Defining_Name'Class;
      Name : Unbounded_Text_Type;
      Imprecise_Fallback : Boolean := False) return Boolean is
      


         Internal_Arg_Name : Symbol_Type;
         Internal_Arg_Imprecise_Fallback : Boolean;
      Property_Result : Boolean;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Name :=
            Lookup_Symbol (Node.Internal.Node.Unit.Context, To_Text (Name));
         Internal_Arg_Imprecise_Fallback :=
            Imprecise_Fallback;

      
      Property_Result :=
         Libadalang.Implementation.Defining_Name_P_Has_Aspect
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Name, Internal_Arg_Imprecise_Fallback, E_Info => Node.Internal.Info);

         return Property_Result;

   end;

         
   function P_Get_Pragma
     (Node : Defining_Name'Class;
      Name : Unbounded_Text_Type) return Pragma_Node is
      


         Internal_Arg_Name : Symbol_Type;
      Property_Result : Internal_Entity_Pragma_Node;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Name :=
            Lookup_Symbol (Node.Internal.Node.Unit.Context, To_Text (Name));

      
      Property_Result :=
         Libadalang.Implementation.Defining_Name_P_Get_Pragma
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Name, E_Info => Node.Internal.Info);

         return Wrap_Node (Property_Result.Node, Property_Result.Info).As_Pragma_Node;

   end;

         
   function P_Get_Representation_Clause
     (Node : Defining_Name'Class;
      Name : Unbounded_Text_Type;
      Imprecise_Fallback : Boolean := False) return Attribute_Def_Clause is
      


         Internal_Arg_Name : Symbol_Type;
         Internal_Arg_Imprecise_Fallback : Boolean;
      Property_Result : Internal_Entity_Attribute_Def_Clause;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Name :=
            Lookup_Symbol (Node.Internal.Node.Unit.Context, To_Text (Name));
         Internal_Arg_Imprecise_Fallback :=
            Imprecise_Fallback;

      
      Property_Result :=
         Libadalang.Implementation.Defining_Name_P_Get_Representation_Clause
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Name, Internal_Arg_Imprecise_Fallback, E_Info => Node.Internal.Info);

         return Wrap_Node (Property_Result.Node, Property_Result.Info).As_Attribute_Def_Clause;

   end;

         
   function P_Get_At_Clause
     (Node : Defining_Name'Class;
      Imprecise_Fallback : Boolean := False) return At_Clause is
      


         Internal_Arg_Imprecise_Fallback : Boolean;
      Property_Result : Internal_Entity_At_Clause;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Imprecise_Fallback :=
            Imprecise_Fallback;

      
      Property_Result :=
         Libadalang.Implementation.Defining_Name_P_Get_At_Clause
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Imprecise_Fallback, E_Info => Node.Internal.Info);

         return Wrap_Node (Property_Result.Node, Property_Result.Info).As_At_Clause;

   end;

         
   function P_Is_Imported
     (Node : Defining_Name'Class) return Boolean is
      


      Property_Result : Boolean;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Defining_Name_P_Is_Imported
            (Bare_Ada_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return Property_Result;

   end;

         
   function P_Is_Ghost_Code
     (Node : Defining_Name'Class) return Boolean is
      


      Property_Result : Boolean;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Defining_Name_P_Is_Ghost_Code
            (Bare_Ada_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return Property_Result;

   end;



         
         ----------------
         -- List_Child --
         ----------------

         function List_Child
           (Node : Defining_Name_List'Class; Index : Positive) return Defining_Name
         is
            Result : Ada_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Result := Node.Child (Index);
            return Result.As_Defining_Name;
         end List_Child;

         

         function Defining_Name_List_First (Node : Defining_Name_List) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return 1;
         end;

         function Defining_Name_List_Next
           (Node : Defining_Name_List; Cursor : Positive) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor + 1;
         end;

         function Defining_Name_List_Has_Element
           (Node : Defining_Name_List; Cursor : Positive) return Boolean is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor in 1 .. Node.Children_Count;
         end;

         function Defining_Name_List_Element
           (Node : Defining_Name_List; Cursor : Positive) return Defining_Name'Class
         is
            Child : Ada_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Child := Node.Child (Cursor);
            return Defining_Name'(Child.As_Defining_Name);
         end;






         
   

   function F_Has_Until
     (Node : Delay_Stmt'Class) return Until_Node
   is
      Result : Bare_Until_Node;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Delay_Stmt_F_Has_Until (Node.Internal.Node);
         if Result = null then
            return No_Until_Node;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Has_Until;

         function F_Has_Until (Node : Delay_Stmt'Class) return Boolean
         is (Until_Node'(Node.F_Has_Until).Kind
             = Ada_Until_Present);


         
   

   function F_Expr
     (Node : Delay_Stmt'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Delay_Stmt_F_Expr (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Expr;







         
   

   function F_Digits
     (Node : Delta_Constraint'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Delta_Constraint_F_Digits (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Digits;


         
   

   function F_Range
     (Node : Delta_Constraint'Class) return Range_Spec
   is
      Result : Bare_Range_Spec;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Delta_Constraint_F_Range (Node.Internal.Node);
         if Result = null then
            return No_Range_Spec;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Range;







         
   

   function F_Has_Abstract
     (Node : Derived_Type_Def'Class) return Abstract_Node
   is
      Result : Bare_Abstract_Node;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Derived_Type_Def_F_Has_Abstract (Node.Internal.Node);
         if Result = null then
            return No_Abstract_Node;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Has_Abstract;

         function F_Has_Abstract (Node : Derived_Type_Def'Class) return Boolean
         is (Abstract_Node'(Node.F_Has_Abstract).Kind
             = Ada_Abstract_Present);


         
   

   function F_Has_Limited
     (Node : Derived_Type_Def'Class) return Limited_Node
   is
      Result : Bare_Limited_Node;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Derived_Type_Def_F_Has_Limited (Node.Internal.Node);
         if Result = null then
            return No_Limited_Node;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Has_Limited;

         function F_Has_Limited (Node : Derived_Type_Def'Class) return Boolean
         is (Limited_Node'(Node.F_Has_Limited).Kind
             = Ada_Limited_Present);


         
   

   function F_Has_Synchronized
     (Node : Derived_Type_Def'Class) return Synchronized_Node
   is
      Result : Bare_Synchronized_Node;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Derived_Type_Def_F_Has_Synchronized (Node.Internal.Node);
         if Result = null then
            return No_Synchronized_Node;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Has_Synchronized;

         function F_Has_Synchronized (Node : Derived_Type_Def'Class) return Boolean
         is (Synchronized_Node'(Node.F_Has_Synchronized).Kind
             = Ada_Synchronized_Present);


         
   

   function F_Subtype_Indication
     (Node : Derived_Type_Def'Class) return Subtype_Indication
   is
      Result : Bare_Subtype_Indication;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Derived_Type_Def_F_Subtype_Indication (Node.Internal.Node);
         if Result = null then
            return No_Subtype_Indication;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Subtype_Indication;


         
   

   function F_Interfaces
     (Node : Derived_Type_Def'Class) return Parent_List
   is
      Result : Bare_Parent_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Derived_Type_Def_F_Interfaces (Node.Internal.Node);
         if Result = null then
            return No_Parent_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Interfaces;


         
   

   function F_Record_Extension
     (Node : Derived_Type_Def'Class) return Base_Record_Def
   is
      Result : Bare_Base_Record_Def;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Derived_Type_Def_F_Record_Extension (Node.Internal.Node);
         if Result = null then
            return No_Base_Record_Def;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Record_Extension;


         
   

   function F_Has_With_Private
     (Node : Derived_Type_Def'Class) return With_Private
   is
      Result : Bare_With_Private;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Derived_Type_Def_F_Has_With_Private (Node.Internal.Node);
         if Result = null then
            return No_With_Private;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Has_With_Private;

         function F_Has_With_Private (Node : Derived_Type_Def'Class) return Boolean
         is (With_Private'(Node.F_Has_With_Private).Kind
             = Ada_With_Private_Present);







         
   

   function F_Digits
     (Node : Digits_Constraint'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Digits_Constraint_F_Digits (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Digits;


         
   

   function F_Range
     (Node : Digits_Constraint'Class) return Range_Spec
   is
      Result : Bare_Range_Spec;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Digits_Constraint_F_Range (Node.Internal.Node);
         if Result = null then
            return No_Range_Spec;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Range;

















         
   

   function F_Subtype
     (Node : Discrete_Subtype_Name'Class) return Discrete_Subtype_Indication
   is
      Result : Bare_Discrete_Subtype_Indication;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Discrete_Subtype_Name_F_Subtype (Node.Internal.Node);
         if Result = null then
            return No_Discrete_Subtype_Indication;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Subtype;





         
         ----------------
         -- List_Child --
         ----------------

         function List_Child
           (Node : Identifier_List'Class; Index : Positive) return Identifier
         is
            Result : Ada_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Result := Node.Child (Index);
            return Result.As_Identifier;
         end List_Child;

         

         function Identifier_List_First (Node : Identifier_List) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return 1;
         end;

         function Identifier_List_Next
           (Node : Identifier_List; Cursor : Positive) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor + 1;
         end;

         function Identifier_List_Has_Element
           (Node : Identifier_List; Cursor : Positive) return Boolean is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor in 1 .. Node.Children_Count;
         end;

         function Identifier_List_Element
           (Node : Identifier_List; Cursor : Positive) return Identifier'Class
         is
            Child : Ada_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Child := Node.Child (Cursor);
            return Identifier'(Child.As_Identifier);
         end;
















         
   

   function F_Ids
     (Node : Discriminant_Spec'Class) return Defining_Name_List
   is
      Result : Bare_Defining_Name_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Discriminant_Spec_F_Ids (Node.Internal.Node);
         if Result = null then
            return No_Defining_Name_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Ids;


         
   

   function F_Type_Expr
     (Node : Discriminant_Spec'Class) return Type_Expr
   is
      Result : Bare_Type_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Discriminant_Spec_F_Type_Expr (Node.Internal.Node);
         if Result = null then
            return No_Type_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Type_Expr;


         
   

   function F_Default_Expr
     (Node : Discriminant_Spec'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Discriminant_Spec_F_Default_Expr (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Default_Expr;





         
         ----------------
         -- List_Child --
         ----------------

         function List_Child
           (Node : Discriminant_Spec_List'Class; Index : Positive) return Discriminant_Spec
         is
            Result : Ada_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Result := Node.Child (Index);
            return Result.As_Discriminant_Spec;
         end List_Child;

         

         function Discriminant_Spec_List_First (Node : Discriminant_Spec_List) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return 1;
         end;

         function Discriminant_Spec_List_Next
           (Node : Discriminant_Spec_List; Cursor : Positive) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor + 1;
         end;

         function Discriminant_Spec_List_Has_Element
           (Node : Discriminant_Spec_List; Cursor : Positive) return Boolean is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor in 1 .. Node.Children_Count;
         end;

         function Discriminant_Spec_List_Element
           (Node : Discriminant_Spec_List; Cursor : Positive) return Discriminant_Spec'Class
         is
            Child : Ada_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Child := Node.Child (Cursor);
            return Discriminant_Spec'(Child.As_Discriminant_Spec);
         end;






         
   

   function F_Prefix
     (Node : Dotted_Name'Class) return Name
   is
      Result : Bare_Name;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Dotted_Name_F_Prefix (Node.Internal.Node);
         if Result = null then
            return No_Name;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Prefix;


         
   

   function F_Suffix
     (Node : Dotted_Name'Class) return Base_Id
   is
      Result : Bare_Base_Id;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Dotted_Name_F_Suffix (Node.Internal.Node);
         if Result = null then
            return No_Base_Id;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Suffix;







         
   

   function F_Cond_Expr
     (Node : Elsif_Expr_Part'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Elsif_Expr_Part_F_Cond_Expr (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Cond_Expr;


         
   

   function F_Then_Expr
     (Node : Elsif_Expr_Part'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Elsif_Expr_Part_F_Then_Expr (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Then_Expr;





         
         ----------------
         -- List_Child --
         ----------------

         function List_Child
           (Node : Elsif_Expr_Part_List'Class; Index : Positive) return Elsif_Expr_Part
         is
            Result : Ada_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Result := Node.Child (Index);
            return Result.As_Elsif_Expr_Part;
         end List_Child;

         

         function Elsif_Expr_Part_List_First (Node : Elsif_Expr_Part_List) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return 1;
         end;

         function Elsif_Expr_Part_List_Next
           (Node : Elsif_Expr_Part_List; Cursor : Positive) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor + 1;
         end;

         function Elsif_Expr_Part_List_Has_Element
           (Node : Elsif_Expr_Part_List; Cursor : Positive) return Boolean is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor in 1 .. Node.Children_Count;
         end;

         function Elsif_Expr_Part_List_Element
           (Node : Elsif_Expr_Part_List; Cursor : Positive) return Elsif_Expr_Part'Class
         is
            Child : Ada_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Child := Node.Child (Cursor);
            return Elsif_Expr_Part'(Child.As_Elsif_Expr_Part);
         end;






         
   

   function F_Cond_Expr
     (Node : Elsif_Stmt_Part'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Elsif_Stmt_Part_F_Cond_Expr (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Cond_Expr;


         
   

   function F_Stmts
     (Node : Elsif_Stmt_Part'Class) return Stmt_List
   is
      Result : Bare_Stmt_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Elsif_Stmt_Part_F_Stmts (Node.Internal.Node);
         if Result = null then
            return No_Stmt_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Stmts;





         
         ----------------
         -- List_Child --
         ----------------

         function List_Child
           (Node : Elsif_Stmt_Part_List'Class; Index : Positive) return Elsif_Stmt_Part
         is
            Result : Ada_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Result := Node.Child (Index);
            return Result.As_Elsif_Stmt_Part;
         end List_Child;

         

         function Elsif_Stmt_Part_List_First (Node : Elsif_Stmt_Part_List) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return 1;
         end;

         function Elsif_Stmt_Part_List_Next
           (Node : Elsif_Stmt_Part_List; Cursor : Positive) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor + 1;
         end;

         function Elsif_Stmt_Part_List_Has_Element
           (Node : Elsif_Stmt_Part_List; Cursor : Positive) return Boolean is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor in 1 .. Node.Children_Count;
         end;

         function Elsif_Stmt_Part_List_Element
           (Node : Elsif_Stmt_Part_List; Cursor : Positive) return Elsif_Stmt_Part'Class
         is
            Child : Ada_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Child := Node.Child (Cursor);
            return Elsif_Stmt_Part'(Child.As_Elsif_Stmt_Part);
         end;






         
   

   function F_Name
     (Node : End_Name'Class) return Name
   is
      Result : Bare_Name;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.End_Name_F_Name (Node.Internal.Node);
         if Result = null then
            return No_Name;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Name;



         
   function P_Basic_Decl
     (Node : End_Name'Class) return Basic_Decl is
      


      Property_Result : Internal_Entity_Basic_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.End_Name_P_Basic_Decl
            (Bare_Ada_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return Wrap_Node (Property_Result.Node, Property_Result.Info).As_Basic_Decl;

   end;





         
   

   function F_Entry_Name
     (Node : Entry_Body'Class) return Defining_Name
   is
      Result : Bare_Defining_Name;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Entry_Body_F_Entry_Name (Node.Internal.Node);
         if Result = null then
            return No_Defining_Name;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Entry_Name;


         
   

   function F_Index_Spec
     (Node : Entry_Body'Class) return Entry_Index_Spec
   is
      Result : Bare_Entry_Index_Spec;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Entry_Body_F_Index_Spec (Node.Internal.Node);
         if Result = null then
            return No_Entry_Index_Spec;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Index_Spec;


         
   

   function F_Params
     (Node : Entry_Body'Class) return Entry_Completion_Formal_Params
   is
      Result : Bare_Entry_Completion_Formal_Params;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Entry_Body_F_Params (Node.Internal.Node);
         if Result = null then
            return No_Entry_Completion_Formal_Params;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Params;


         
   

   function F_Barrier
     (Node : Entry_Body'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Entry_Body_F_Barrier (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Barrier;


         
   

   function F_Decls
     (Node : Entry_Body'Class) return Declarative_Part
   is
      Result : Bare_Declarative_Part;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Entry_Body_F_Decls (Node.Internal.Node);
         if Result = null then
            return No_Declarative_Part;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Decls;


         
   

   function F_Stmts
     (Node : Entry_Body'Class) return Handled_Stmts
   is
      Result : Bare_Handled_Stmts;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Entry_Body_F_Stmts (Node.Internal.Node);
         if Result = null then
            return No_Handled_Stmts;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Stmts;


         
   

   function F_End_Name
     (Node : Entry_Body'Class) return End_Name
   is
      Result : Bare_End_Name;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Entry_Body_F_End_Name (Node.Internal.Node);
         if Result = null then
            return No_End_Name;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_End_Name;







         
   

   function F_Params
     (Node : Entry_Completion_Formal_Params'Class) return Params
   is
      Result : Bare_Params;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Entry_Completion_Formal_Params_F_Params (Node.Internal.Node);
         if Result = null then
            return No_Params;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Params;







         
   

   function F_Overriding
     (Node : Entry_Decl'Class) return Overriding_Node
   is
      Result : Bare_Overriding_Node;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Entry_Decl_F_Overriding (Node.Internal.Node);
         if Result = null then
            return No_Overriding_Node;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Overriding;

         function F_Overriding
           (Node : Entry_Decl'Class) return Ada_Overriding_Node
         is (Overriding_Node'(Node.F_Overriding).Kind);

         
   

   function F_Spec
     (Node : Entry_Decl'Class) return Entry_Spec
   is
      Result : Bare_Entry_Spec;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Entry_Decl_F_Spec (Node.Internal.Node);
         if Result = null then
            return No_Entry_Spec;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Spec;



         
   function P_Body_Part
     (Node : Entry_Decl'Class;
      Imprecise_Fallback : Boolean := False) return Entry_Body is
      


         Internal_Arg_Imprecise_Fallback : Boolean;
      Property_Result : Internal_Entity_Entry_Body;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Imprecise_Fallback :=
            Imprecise_Fallback;

      
      Property_Result :=
         Libadalang.Implementation.Entry_Decl_P_Body_Part
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Imprecise_Fallback, E_Info => Node.Internal.Info);

         return Wrap_Node (Property_Result.Node, Property_Result.Info).As_Entry_Body;

   end;

         
   function P_Accept_Stmts
     (Node : Entry_Decl'Class) return Accept_Stmt_Array is
      


      Property_Result : Internal_Entity_Accept_Stmt_Array_Access;

         procedure Free_Internal;
         --  Dec-ref all internal arguments and the property result, when
         --  applicable.

         -------------------
         -- Free_Internal --
         -------------------

         procedure Free_Internal is
         begin
               Dec_Ref (Property_Result);
         end Free_Internal;

   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Entry_Decl_P_Accept_Stmts
            (Bare_Ada_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return Result : constant Accept_Stmt_Array :=
            To_Public_Accept_Stmt_Array (Property_Result)
         do
            Free_Internal;
         end return;

      exception
         when Precondition_Failure | Property_Error =>
            Free_Internal;
            raise;
   end;





         
   

   function F_Id
     (Node : Entry_Index_Spec'Class) return Defining_Name
   is
      Result : Bare_Defining_Name;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Entry_Index_Spec_F_Id (Node.Internal.Node);
         if Result = null then
            return No_Defining_Name;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Id;


         
   

   function F_Subtype
     (Node : Entry_Index_Spec'Class) return Ada_Node
   is
      Result : Bare_Ada_Node;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Entry_Index_Spec_F_Subtype (Node.Internal.Node);
         if Result = null then
            return No_Ada_Node;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Subtype;







         
   

   function F_Entry_Name
     (Node : Entry_Spec'Class) return Defining_Name
   is
      Result : Bare_Defining_Name;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Entry_Spec_F_Entry_Name (Node.Internal.Node);
         if Result = null then
            return No_Defining_Name;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Entry_Name;


         
   

   function F_Family_Type
     (Node : Entry_Spec'Class) return Ada_Node
   is
      Result : Bare_Ada_Node;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Entry_Spec_F_Family_Type (Node.Internal.Node);
         if Result = null then
            return No_Ada_Node;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Family_Type;


         
   

   function F_Entry_Params
     (Node : Entry_Spec'Class) return Params
   is
      Result : Bare_Params;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Entry_Spec_F_Entry_Params (Node.Internal.Node);
         if Result = null then
            return No_Params;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Entry_Params;












         
   

   function F_Name
     (Node : Enum_Literal_Decl'Class) return Defining_Name
   is
      Result : Bare_Defining_Name;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Enum_Literal_Decl_F_Name (Node.Internal.Node);
         if Result = null then
            return No_Defining_Name;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Name;



         
   function P_Enum_Type
     (Node : Enum_Literal_Decl'Class) return Type_Decl is
      


      Property_Result : Internal_Entity_Type_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Dispatcher_Enum_Literal_Decl_P_Enum_Type
            (Bare_Ada_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return Wrap_Node (Property_Result.Node, Property_Result.Info).As_Type_Decl;

   end;



         
         ----------------
         -- List_Child --
         ----------------

         function List_Child
           (Node : Enum_Literal_Decl_List'Class; Index : Positive) return Enum_Literal_Decl
         is
            Result : Ada_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Result := Node.Child (Index);
            return Result.As_Enum_Literal_Decl;
         end List_Child;

         

         function Enum_Literal_Decl_List_First (Node : Enum_Literal_Decl_List) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return 1;
         end;

         function Enum_Literal_Decl_List_Next
           (Node : Enum_Literal_Decl_List; Cursor : Positive) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor + 1;
         end;

         function Enum_Literal_Decl_List_Has_Element
           (Node : Enum_Literal_Decl_List; Cursor : Positive) return Boolean is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor in 1 .. Node.Children_Count;
         end;

         function Enum_Literal_Decl_List_Element
           (Node : Enum_Literal_Decl_List; Cursor : Positive) return Enum_Literal_Decl'Class
         is
            Child : Ada_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Child := Node.Child (Cursor);
            return Enum_Literal_Decl'(Child.As_Enum_Literal_Decl);
         end;






         
   

   function F_Type_Name
     (Node : Enum_Rep_Clause'Class) return Name
   is
      Result : Bare_Name;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Enum_Rep_Clause_F_Type_Name (Node.Internal.Node);
         if Result = null then
            return No_Name;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Type_Name;


         
   

   function F_Aggregate
     (Node : Enum_Rep_Clause'Class) return Base_Aggregate
   is
      Result : Bare_Base_Aggregate;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Enum_Rep_Clause_F_Aggregate (Node.Internal.Node);
         if Result = null then
            return No_Base_Aggregate;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Aggregate;



         
   function P_Params
     (Node : Enum_Rep_Clause'Class) return Param_Actual_Array is
      


      Property_Result : Internal_Param_Actual_Array_Access;

         procedure Free_Internal;
         --  Dec-ref all internal arguments and the property result, when
         --  applicable.

         -------------------
         -- Free_Internal --
         -------------------

         procedure Free_Internal is
         begin
               Dec_Ref (Property_Result);
         end Free_Internal;

   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Enum_Rep_Clause_P_Params
            (Bare_Ada_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return Result : constant Param_Actual_Array :=
            To_Public_Param_Actual_Array (Property_Result)
         do
            Free_Internal;
         end return;

      exception
         when Precondition_Failure | Property_Error =>
            Free_Internal;
            raise;
   end;










         
   

   function F_Enum_Literals
     (Node : Enum_Type_Def'Class) return Enum_Literal_Decl_List
   is
      Result : Bare_Enum_Literal_Decl_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Enum_Type_Def_F_Enum_Literals (Node.Internal.Node);
         if Result = null then
            return No_Enum_Literal_Decl_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Enum_Literals;

















         
   

   function F_Ids
     (Node : Exception_Decl'Class) return Defining_Name_List
   is
      Result : Bare_Defining_Name_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Exception_Decl_F_Ids (Node.Internal.Node);
         if Result = null then
            return No_Defining_Name_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Ids;


         
   

   function F_Renames
     (Node : Exception_Decl'Class) return Renaming_Clause
   is
      Result : Bare_Renaming_Clause;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Exception_Decl_F_Renames (Node.Internal.Node);
         if Result = null then
            return No_Renaming_Clause;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Renames;







         
   

   function F_Exception_Name
     (Node : Exception_Handler'Class) return Defining_Name
   is
      Result : Bare_Defining_Name;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Exception_Handler_F_Exception_Name (Node.Internal.Node);
         if Result = null then
            return No_Defining_Name;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Exception_Name;


         
   

   function F_Handled_Exceptions
     (Node : Exception_Handler'Class) return Alternatives_List
   is
      Result : Bare_Alternatives_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Exception_Handler_F_Handled_Exceptions (Node.Internal.Node);
         if Result = null then
            return No_Alternatives_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Handled_Exceptions;


         
   

   function F_Stmts
     (Node : Exception_Handler'Class) return Stmt_List
   is
      Result : Bare_Stmt_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Exception_Handler_F_Stmts (Node.Internal.Node);
         if Result = null then
            return No_Stmt_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Stmts;







         
   

   function F_Loop_Name
     (Node : Exit_Stmt'Class) return Name
   is
      Result : Bare_Name;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Exit_Stmt_F_Loop_Name (Node.Internal.Node);
         if Result = null then
            return No_Name;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Loop_Name;


         
   

   function F_Cond_Expr
     (Node : Exit_Stmt'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Exit_Stmt_F_Cond_Expr (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Cond_Expr;







         
   

   function F_Prefix
     (Node : Explicit_Deref'Class) return Name
   is
      Result : Bare_Name;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Explicit_Deref_F_Prefix (Node.Internal.Node);
         if Result = null then
            return No_Name;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Prefix;





         
         ----------------
         -- List_Child --
         ----------------

         function List_Child
           (Node : Expr_List'Class; Index : Positive) return Expr
         is
            Result : Ada_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Result := Node.Child (Index);
            return Result.As_Expr;
         end List_Child;

         

         function Expr_List_First (Node : Expr_List) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return 1;
         end;

         function Expr_List_Next
           (Node : Expr_List; Cursor : Positive) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor + 1;
         end;

         function Expr_List_Has_Element
           (Node : Expr_List; Cursor : Positive) return Boolean is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor in 1 .. Node.Children_Count;
         end;

         function Expr_List_Element
           (Node : Expr_List; Cursor : Positive) return Expr'Class
         is
            Child : Ada_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Child := Node.Child (Cursor);
            return Expr'(Child.As_Expr);
         end;











         
   

   function F_Expr
     (Node : Expr_Function'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Expr_Function_F_Expr (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Expr;







         
   

   function F_Decl
     (Node : Extended_Return_Stmt'Class) return Extended_Return_Stmt_Object_Decl
   is
      Result : Bare_Extended_Return_Stmt_Object_Decl;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Extended_Return_Stmt_F_Decl (Node.Internal.Node);
         if Result = null then
            return No_Extended_Return_Stmt_Object_Decl;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Decl;


         
   

   function F_Stmts
     (Node : Extended_Return_Stmt'Class) return Handled_Stmts
   is
      Result : Bare_Handled_Stmts;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Extended_Return_Stmt_F_Stmts (Node.Internal.Node);
         if Result = null then
            return No_Handled_Stmts;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Stmts;







         
   

   function F_Ids
     (Node : Object_Decl'Class) return Defining_Name_List
   is
      Result : Bare_Defining_Name_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Object_Decl_F_Ids (Node.Internal.Node);
         if Result = null then
            return No_Defining_Name_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Ids;


         
   

   function F_Has_Aliased
     (Node : Object_Decl'Class) return Aliased_Node
   is
      Result : Bare_Aliased_Node;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Object_Decl_F_Has_Aliased (Node.Internal.Node);
         if Result = null then
            return No_Aliased_Node;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Has_Aliased;

         function F_Has_Aliased (Node : Object_Decl'Class) return Boolean
         is (Aliased_Node'(Node.F_Has_Aliased).Kind
             = Ada_Aliased_Present);


         
   

   function F_Has_Constant
     (Node : Object_Decl'Class) return Constant_Node
   is
      Result : Bare_Constant_Node;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Object_Decl_F_Has_Constant (Node.Internal.Node);
         if Result = null then
            return No_Constant_Node;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Has_Constant;

         function F_Has_Constant (Node : Object_Decl'Class) return Boolean
         is (Constant_Node'(Node.F_Has_Constant).Kind
             = Ada_Constant_Present);


         
   

   function F_Mode
     (Node : Object_Decl'Class) return Mode
   is
      Result : Bare_Mode;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Object_Decl_F_Mode (Node.Internal.Node);
         if Result = null then
            return No_Mode;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Mode;

         function F_Mode
           (Node : Object_Decl'Class) return Ada_Mode
         is (Mode'(Node.F_Mode).Kind);

         
   

   function F_Type_Expr
     (Node : Object_Decl'Class) return Type_Expr
   is
      Result : Bare_Type_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Object_Decl_F_Type_Expr (Node.Internal.Node);
         if Result = null then
            return No_Type_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Type_Expr;


         
   

   function F_Default_Expr
     (Node : Object_Decl'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Object_Decl_F_Default_Expr (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Default_Expr;


         
   

   function F_Renaming_Clause
     (Node : Object_Decl'Class) return Renaming_Clause
   is
      Result : Bare_Renaming_Clause;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Object_Decl_F_Renaming_Clause (Node.Internal.Node);
         if Result = null then
            return No_Renaming_Clause;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Renaming_Clause;



         
   function P_Private_Part_Decl
     (Node : Object_Decl'Class) return Basic_Decl is
      


      Property_Result : Internal_Entity_Basic_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Object_Decl_P_Private_Part_Decl
            (Bare_Ada_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return Wrap_Node (Property_Result.Node, Property_Result.Info).As_Basic_Decl;

   end;

         
   function P_Public_Part_Decl
     (Node : Object_Decl'Class) return Basic_Decl is
      


      Property_Result : Internal_Entity_Basic_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Object_Decl_P_Public_Part_Decl
            (Bare_Ada_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return Wrap_Node (Property_Result.Node, Property_Result.Info).As_Basic_Decl;

   end;










         
   

   function F_Num_Digits
     (Node : Floating_Point_Def'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Floating_Point_Def_F_Num_Digits (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Num_Digits;


         
   

   function F_Range
     (Node : Floating_Point_Def'Class) return Range_Spec
   is
      Result : Bare_Range_Spec;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Floating_Point_Def_F_Range (Node.Internal.Node);
         if Result = null then
            return No_Range_Spec;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Range;












         
   

   function F_Var_Decl
     (Node : For_Loop_Spec'Class) return For_Loop_Var_Decl
   is
      Result : Bare_For_Loop_Var_Decl;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.For_Loop_Spec_F_Var_Decl (Node.Internal.Node);
         if Result = null then
            return No_For_Loop_Var_Decl;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Var_Decl;


         
   

   function F_Loop_Type
     (Node : For_Loop_Spec'Class) return Iter_Type
   is
      Result : Bare_Iter_Type;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.For_Loop_Spec_F_Loop_Type (Node.Internal.Node);
         if Result = null then
            return No_Iter_Type;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Loop_Type;

         function F_Loop_Type
           (Node : For_Loop_Spec'Class) return Ada_Iter_Type
         is (Iter_Type'(Node.F_Loop_Type).Kind);

         
   

   function F_Has_Reverse
     (Node : For_Loop_Spec'Class) return Reverse_Node
   is
      Result : Bare_Reverse_Node;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.For_Loop_Spec_F_Has_Reverse (Node.Internal.Node);
         if Result = null then
            return No_Reverse_Node;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Has_Reverse;

         function F_Has_Reverse (Node : For_Loop_Spec'Class) return Boolean
         is (Reverse_Node'(Node.F_Has_Reverse).Kind
             = Ada_Reverse_Present);


         
   

   function F_Iter_Expr
     (Node : For_Loop_Spec'Class) return Ada_Node
   is
      Result : Bare_Ada_Node;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.For_Loop_Spec_F_Iter_Expr (Node.Internal.Node);
         if Result = null then
            return No_Ada_Node;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Iter_Expr;


         
   

   function F_Iter_Filter
     (Node : For_Loop_Spec'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.For_Loop_Spec_F_Iter_Filter (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Iter_Filter;












         
   

   function F_Id
     (Node : For_Loop_Var_Decl'Class) return Defining_Name
   is
      Result : Bare_Defining_Name;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.For_Loop_Var_Decl_F_Id (Node.Internal.Node);
         if Result = null then
            return No_Defining_Name;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Id;


         
   

   function F_Id_Type
     (Node : For_Loop_Var_Decl'Class) return Type_Expr
   is
      Result : Bare_Type_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.For_Loop_Var_Decl_F_Id_Type (Node.Internal.Node);
         if Result = null then
            return No_Type_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Id_Type;












         
   

   function F_Default_Type
     (Node : Formal_Type_Decl'Class) return Name
   is
      Result : Bare_Name;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Formal_Type_Decl_F_Default_Type (Node.Internal.Node);
         if Result = null then
            return No_Name;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Default_Type;







         
   

   function F_Formal_Part
     (Node : Generic_Decl'Class) return Generic_Formal_Part
   is
      Result : Bare_Generic_Formal_Part;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Generic_Decl_F_Formal_Part (Node.Internal.Node);
         if Result = null then
            return No_Generic_Formal_Part;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Formal_Part;







         
   

   function F_Decl
     (Node : Generic_Formal'Class) return Basic_Decl
   is
      Result : Bare_Basic_Decl;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Generic_Formal_F_Decl (Node.Internal.Node);
         if Result = null then
            return No_Basic_Decl;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Decl;

















         
   

   function F_Decls
     (Node : Generic_Formal_Part'Class) return Ada_Node_List
   is
      Result : Bare_Ada_Node_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Generic_Formal_Part_F_Decls (Node.Internal.Node);
         if Result = null then
            return No_Ada_Node_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Decls;


















         
   function P_Designated_Generic_Decl
     (Node : Generic_Instantiation'Class) return Basic_Decl is
      


      Property_Result : Internal_Entity_Basic_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Dispatcher_Generic_Instantiation_P_Designated_Generic_Decl
            (Bare_Ada_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return Wrap_Node (Property_Result.Node, Property_Result.Info).As_Basic_Decl;

   end;

         
   function P_Inst_Params
     (Node : Generic_Instantiation'Class) return Param_Actual_Array is
      


      Property_Result : Internal_Param_Actual_Array_Access;

         procedure Free_Internal;
         --  Dec-ref all internal arguments and the property result, when
         --  applicable.

         -------------------
         -- Free_Internal --
         -------------------

         procedure Free_Internal is
         begin
               Dec_Ref (Property_Result);
         end Free_Internal;

   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Generic_Instantiation_P_Inst_Params
            (Bare_Ada_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return Result : constant Param_Actual_Array :=
            To_Public_Param_Actual_Array (Property_Result)
         do
            Free_Internal;
         end return;

      exception
         when Precondition_Failure | Property_Error =>
            Free_Internal;
            raise;
   end;





         
   

   function F_Package_Decl
     (Node : Generic_Package_Decl'Class) return Generic_Package_Internal
   is
      Result : Bare_Generic_Package_Internal;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Generic_Package_Decl_F_Package_Decl (Node.Internal.Node);
         if Result = null then
            return No_Generic_Package_Internal;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Package_Decl;



         
   function P_Body_Part
     (Node : Generic_Package_Decl'Class) return Package_Body is
      


      Property_Result : Internal_Entity_Package_Body;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Generic_Package_Decl_P_Body_Part
            (Bare_Ada_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return Wrap_Node (Property_Result.Node, Property_Result.Info).As_Package_Body;

   end;





         
   

   function F_Name
     (Node : Generic_Package_Instantiation'Class) return Defining_Name
   is
      Result : Bare_Defining_Name;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Generic_Package_Instantiation_F_Name (Node.Internal.Node);
         if Result = null then
            return No_Defining_Name;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Name;


         
   

   function F_Generic_Pkg_Name
     (Node : Generic_Package_Instantiation'Class) return Name
   is
      Result : Bare_Name;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Generic_Package_Instantiation_F_Generic_Pkg_Name (Node.Internal.Node);
         if Result = null then
            return No_Name;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Generic_Pkg_Name;


         
   

   function F_Params
     (Node : Generic_Package_Instantiation'Class) return Assoc_List
   is
      Result : Bare_Assoc_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Generic_Package_Instantiation_F_Params (Node.Internal.Node);
         if Result = null then
            return No_Assoc_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Params;

















         
   

   function F_Name
     (Node : Generic_Package_Renaming_Decl'Class) return Defining_Name
   is
      Result : Bare_Defining_Name;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Generic_Package_Renaming_Decl_F_Name (Node.Internal.Node);
         if Result = null then
            return No_Defining_Name;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Name;


         
   

   function F_Renames
     (Node : Generic_Package_Renaming_Decl'Class) return Name
   is
      Result : Bare_Name;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Generic_Package_Renaming_Decl_F_Renames (Node.Internal.Node);
         if Result = null then
            return No_Name;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Renames;







         
   

   function F_Subp_Decl
     (Node : Generic_Subp_Decl'Class) return Generic_Subp_Internal
   is
      Result : Bare_Generic_Subp_Internal;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Generic_Subp_Decl_F_Subp_Decl (Node.Internal.Node);
         if Result = null then
            return No_Generic_Subp_Internal;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Subp_Decl;



         
   function P_Body_Part
     (Node : Generic_Subp_Decl'Class;
      Imprecise_Fallback : Boolean := False) return Base_Subp_Body is
      


         Internal_Arg_Imprecise_Fallback : Boolean;
      Property_Result : Internal_Entity_Base_Subp_Body;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

         Internal_Arg_Imprecise_Fallback :=
            Imprecise_Fallback;

      
      Property_Result :=
         Libadalang.Implementation.Generic_Subp_Decl_P_Body_Part
            (Bare_Ada_Node (Node.Internal.Node), Internal_Arg_Imprecise_Fallback, E_Info => Node.Internal.Info);

         return Wrap_Node (Property_Result.Node, Property_Result.Info).As_Base_Subp_Body;

   end;





         
   

   function F_Overriding
     (Node : Generic_Subp_Instantiation'Class) return Overriding_Node
   is
      Result : Bare_Overriding_Node;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Generic_Subp_Instantiation_F_Overriding (Node.Internal.Node);
         if Result = null then
            return No_Overriding_Node;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Overriding;

         function F_Overriding
           (Node : Generic_Subp_Instantiation'Class) return Ada_Overriding_Node
         is (Overriding_Node'(Node.F_Overriding).Kind);

         
   

   function F_Kind
     (Node : Generic_Subp_Instantiation'Class) return Subp_Kind
   is
      Result : Bare_Subp_Kind;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Generic_Subp_Instantiation_F_Kind (Node.Internal.Node);
         if Result = null then
            return No_Subp_Kind;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Kind;

         function F_Kind
           (Node : Generic_Subp_Instantiation'Class) return Ada_Subp_Kind
         is (Subp_Kind'(Node.F_Kind).Kind);

         
   

   function F_Subp_Name
     (Node : Generic_Subp_Instantiation'Class) return Defining_Name
   is
      Result : Bare_Defining_Name;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Generic_Subp_Instantiation_F_Subp_Name (Node.Internal.Node);
         if Result = null then
            return No_Defining_Name;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Subp_Name;


         
   

   function F_Generic_Subp_Name
     (Node : Generic_Subp_Instantiation'Class) return Name
   is
      Result : Bare_Name;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Generic_Subp_Instantiation_F_Generic_Subp_Name (Node.Internal.Node);
         if Result = null then
            return No_Name;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Generic_Subp_Name;


         
   

   function F_Params
     (Node : Generic_Subp_Instantiation'Class) return Assoc_List
   is
      Result : Bare_Assoc_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Generic_Subp_Instantiation_F_Params (Node.Internal.Node);
         if Result = null then
            return No_Assoc_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Params;



         
   function P_Designated_Subp
     (Node : Generic_Subp_Instantiation'Class) return Basic_Subp_Decl is
      


      Property_Result : Internal_Entity_Basic_Subp_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Generic_Subp_Instantiation_P_Designated_Subp
            (Bare_Ada_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return Wrap_Node (Property_Result.Node, Property_Result.Info).As_Basic_Subp_Decl;

   end;





         
   

   function F_Subp_Spec
     (Node : Generic_Subp_Internal'Class) return Subp_Spec
   is
      Result : Bare_Subp_Spec;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Generic_Subp_Internal_F_Subp_Spec (Node.Internal.Node);
         if Result = null then
            return No_Subp_Spec;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Subp_Spec;







         
   

   function F_Kind
     (Node : Generic_Subp_Renaming_Decl'Class) return Subp_Kind
   is
      Result : Bare_Subp_Kind;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Generic_Subp_Renaming_Decl_F_Kind (Node.Internal.Node);
         if Result = null then
            return No_Subp_Kind;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Kind;

         function F_Kind
           (Node : Generic_Subp_Renaming_Decl'Class) return Ada_Subp_Kind
         is (Subp_Kind'(Node.F_Kind).Kind);

         
   

   function F_Name
     (Node : Generic_Subp_Renaming_Decl'Class) return Defining_Name
   is
      Result : Bare_Defining_Name;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Generic_Subp_Renaming_Decl_F_Name (Node.Internal.Node);
         if Result = null then
            return No_Defining_Name;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Name;


         
   

   function F_Renames
     (Node : Generic_Subp_Renaming_Decl'Class) return Name
   is
      Result : Bare_Name;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Generic_Subp_Renaming_Decl_F_Renames (Node.Internal.Node);
         if Result = null then
            return No_Name;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Renames;







         
   

   function F_Label_Name
     (Node : Goto_Stmt'Class) return Name
   is
      Result : Bare_Name;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Goto_Stmt_F_Label_Name (Node.Internal.Node);
         if Result = null then
            return No_Name;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Label_Name;







         
   

   function F_Stmts
     (Node : Handled_Stmts'Class) return Stmt_List
   is
      Result : Bare_Stmt_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Handled_Stmts_F_Stmts (Node.Internal.Node);
         if Result = null then
            return No_Stmt_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Stmts;


         
   

   function F_Exceptions
     (Node : Handled_Stmts'Class) return Ada_Node_List
   is
      Result : Bare_Ada_Node_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Handled_Stmts_F_Exceptions (Node.Internal.Node);
         if Result = null then
            return No_Ada_Node_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Exceptions;







         
   

   function F_Cond_Expr
     (Node : If_Expr'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.If_Expr_F_Cond_Expr (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Cond_Expr;


         
   

   function F_Then_Expr
     (Node : If_Expr'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.If_Expr_F_Then_Expr (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Then_Expr;


         
   

   function F_Alternatives
     (Node : If_Expr'Class) return Elsif_Expr_Part_List
   is
      Result : Bare_Elsif_Expr_Part_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.If_Expr_F_Alternatives (Node.Internal.Node);
         if Result = null then
            return No_Elsif_Expr_Part_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Alternatives;


         
   

   function F_Else_Expr
     (Node : If_Expr'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.If_Expr_F_Else_Expr (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Else_Expr;







         
   

   function F_Cond_Expr
     (Node : If_Stmt'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.If_Stmt_F_Cond_Expr (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Cond_Expr;


         
   

   function F_Then_Stmts
     (Node : If_Stmt'Class) return Stmt_List
   is
      Result : Bare_Stmt_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.If_Stmt_F_Then_Stmts (Node.Internal.Node);
         if Result = null then
            return No_Stmt_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Then_Stmts;


         
   

   function F_Alternatives
     (Node : If_Stmt'Class) return Elsif_Stmt_Part_List
   is
      Result : Bare_Elsif_Stmt_Part_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.If_Stmt_F_Alternatives (Node.Internal.Node);
         if Result = null then
            return No_Elsif_Stmt_Part_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Alternatives;


         
   

   function F_Else_Stmts
     (Node : If_Stmt'Class) return Stmt_List
   is
      Result : Bare_Stmt_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.If_Stmt_F_Else_Stmts (Node.Internal.Node);
         if Result = null then
            return No_Stmt_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Else_Stmts;







         
   

   function F_Discriminants
     (Node : Incomplete_Type_Decl'Class) return Discriminant_Part
   is
      Result : Bare_Discriminant_Part;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Incomplete_Type_Decl_F_Discriminants (Node.Internal.Node);
         if Result = null then
            return No_Discriminant_Part;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Discriminants;







         
   

   function F_Is_Tagged
     (Node : Incomplete_Formal_Type_Decl'Class) return Tagged_Node
   is
      Result : Bare_Tagged_Node;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Incomplete_Formal_Type_Decl_F_Is_Tagged (Node.Internal.Node);
         if Result = null then
            return No_Tagged_Node;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Is_Tagged;

         function F_Is_Tagged (Node : Incomplete_Formal_Type_Decl'Class) return Boolean
         is (Tagged_Node'(Node.F_Is_Tagged).Kind
             = Ada_Tagged_Present);


         
   

   function F_Default_Type
     (Node : Incomplete_Formal_Type_Decl'Class) return Name
   is
      Result : Bare_Name;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Incomplete_Formal_Type_Decl_F_Default_Type (Node.Internal.Node);
         if Result = null then
            return No_Name;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Default_Type;







         
   

   function F_Has_Abstract
     (Node : Incomplete_Tagged_Type_Decl'Class) return Abstract_Node
   is
      Result : Bare_Abstract_Node;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Incomplete_Tagged_Type_Decl_F_Has_Abstract (Node.Internal.Node);
         if Result = null then
            return No_Abstract_Node;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Has_Abstract;

         function F_Has_Abstract (Node : Incomplete_Tagged_Type_Decl'Class) return Boolean
         is (Abstract_Node'(Node.F_Has_Abstract).Kind
             = Ada_Abstract_Present);













         
   function P_Denoted_Value
     (Node : Int_Literal'Class) return Big_Integer is
      


      Property_Result : Big_Integer_Type;

         procedure Free_Internal;
         --  Dec-ref all internal arguments and the property result, when
         --  applicable.

         -------------------
         -- Free_Internal --
         -------------------

         procedure Free_Internal is
         begin
               Dec_Ref (Property_Result);
         end Free_Internal;

   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Extensions.Int_Literal_P_Denoted_Value
            (Bare_Ada_Node (Node.Internal.Node));

         return Result : constant Big_Integer :=
            Create_Public_Big_Integer (Property_Result)
         do
            Free_Internal;
         end return;

      exception
         when Precondition_Failure | Property_Error =>
            Free_Internal;
            raise;
   end;






























         
   

   function F_Interface_Kind
     (Node : Interface_Type_Def'Class) return Interface_Kind
   is
      Result : Bare_Interface_Kind;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Interface_Type_Def_F_Interface_Kind (Node.Internal.Node);
         if Result = null then
            return No_Interface_Kind;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Interface_Kind;

         function F_Interface_Kind
           (Node : Interface_Type_Def'Class) return Ada_Interface_Kind
         is (Interface_Kind'(Node.F_Interface_Kind).Kind);

         
   

   function F_Interfaces
     (Node : Interface_Type_Def'Class) return Parent_List
   is
      Result : Bare_Parent_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Interface_Type_Def_F_Interfaces (Node.Internal.Node);
         if Result = null then
            return No_Parent_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Interfaces;






















         
   

   function F_Spec
     (Node : Iterated_Assoc'Class) return For_Loop_Spec
   is
      Result : Bare_For_Loop_Spec;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Iterated_Assoc_F_Spec (Node.Internal.Node);
         if Result = null then
            return No_For_Loop_Spec;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Spec;


         
   

   function F_R_Expr
     (Node : Iterated_Assoc'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Iterated_Assoc_F_R_Expr (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_R_Expr;







         
   

   function F_Discr_Specs
     (Node : Known_Discriminant_Part'Class) return Discriminant_Spec_List
   is
      Result : Bare_Discriminant_Spec_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Known_Discriminant_Part_F_Discr_Specs (Node.Internal.Node);
         if Result = null then
            return No_Discriminant_Spec_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Discr_Specs;







         
   

   function F_Decl
     (Node : Label'Class) return Label_Decl
   is
      Result : Bare_Label_Decl;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Label_F_Decl (Node.Internal.Node);
         if Result = null then
            return No_Label_Decl;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Decl;







         
   

   function F_Name
     (Node : Label_Decl'Class) return Defining_Name
   is
      Result : Bare_Defining_Name;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Label_Decl_F_Name (Node.Internal.Node);
         if Result = null then
            return No_Defining_Name;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Name;







         
   

   function F_Has_Private
     (Node : Library_Item'Class) return Private_Node
   is
      Result : Bare_Private_Node;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Library_Item_F_Has_Private (Node.Internal.Node);
         if Result = null then
            return No_Private_Node;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Has_Private;

         function F_Has_Private (Node : Library_Item'Class) return Boolean
         is (Private_Node'(Node.F_Has_Private).Kind
             = Ada_Private_Present);


         
   

   function F_Item
     (Node : Library_Item'Class) return Basic_Decl
   is
      Result : Bare_Basic_Decl;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Library_Item_F_Item (Node.Internal.Node);
         if Result = null then
            return No_Basic_Decl;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Item;








         
   function P_As_Bool
     (Node : Limited_Node'Class) return Boolean is
      


      Property_Result : Boolean;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Dispatcher_Limited_Node_P_As_Bool
            (Bare_Ada_Node (Node.Internal.Node));

         return Property_Result;

   end;




















         
   

   function F_Expr
     (Node : Membership_Expr'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Membership_Expr_F_Expr (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Expr;


         
   

   function F_Op
     (Node : Membership_Expr'Class) return Op
   is
      Result : Bare_Op;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Membership_Expr_F_Op (Node.Internal.Node);
         if Result = null then
            return No_Op;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Op;

         function F_Op
           (Node : Membership_Expr'Class) return Ada_Op
         is (Op'(Node.F_Op).Kind);

         
   

   function F_Membership_Exprs
     (Node : Membership_Expr'Class) return Expr_Alternatives_List
   is
      Result : Bare_Expr_Alternatives_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Membership_Expr_F_Membership_Exprs (Node.Internal.Node);
         if Result = null then
            return No_Expr_Alternatives_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Membership_Exprs;







         
   

   function F_Expr
     (Node : Mod_Int_Type_Def'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Mod_Int_Type_Def_F_Expr (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Expr;
































         
   

   function F_Decls
     (Node : Multi_Abstract_State_Decl'Class) return Abstract_State_Decl_List
   is
      Result : Bare_Abstract_State_Decl_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Multi_Abstract_State_Decl_F_Decls (Node.Internal.Node);
         if Result = null then
            return No_Abstract_State_Decl_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Decls;










         
         ----------------
         -- List_Child --
         ----------------

         function List_Child
           (Node : Name_List'Class; Index : Positive) return Name
         is
            Result : Ada_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Result := Node.Child (Index);
            return Result.As_Name;
         end List_Child;

         

         function Name_List_First (Node : Name_List) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return 1;
         end;

         function Name_List_Next
           (Node : Name_List; Cursor : Positive) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor + 1;
         end;

         function Name_List_Has_Element
           (Node : Name_List; Cursor : Positive) return Boolean is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor in 1 .. Node.Children_Count;
         end;

         function Name_List_Element
           (Node : Name_List; Cursor : Positive) return Name'Class
         is
            Child : Ada_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Child := Node.Child (Cursor);
            return Name'(Child.As_Name);
         end;






         
   

   function F_Decl
     (Node : Named_Stmt'Class) return Named_Stmt_Decl
   is
      Result : Bare_Named_Stmt_Decl;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Named_Stmt_F_Decl (Node.Internal.Node);
         if Result = null then
            return No_Named_Stmt_Decl;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Decl;


         
   

   function F_Stmt
     (Node : Named_Stmt'Class) return Composite_Stmt
   is
      Result : Bare_Composite_Stmt;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Named_Stmt_F_Stmt (Node.Internal.Node);
         if Result = null then
            return No_Composite_Stmt;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Stmt;







         
   

   function F_Name
     (Node : Named_Stmt_Decl'Class) return Defining_Name
   is
      Result : Bare_Defining_Name;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Named_Stmt_Decl_F_Name (Node.Internal.Node);
         if Result = null then
            return No_Defining_Name;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Name;













         
   function P_As_Bool
     (Node : Not_Null'Class) return Boolean is
      


      Property_Result : Boolean;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Dispatcher_Not_Null_P_As_Bool
            (Bare_Ada_Node (Node.Internal.Node));

         return Property_Result;

   end;













































         
   

   function F_Ids
     (Node : Number_Decl'Class) return Defining_Name_List
   is
      Result : Bare_Defining_Name_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Number_Decl_F_Ids (Node.Internal.Node);
         if Result = null then
            return No_Defining_Name_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Ids;


         
   

   function F_Expr
     (Node : Number_Decl'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Number_Decl_F_Expr (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Expr;




































































































































         
   

   function F_Delta
     (Node : Ordinary_Fixed_Point_Def'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Ordinary_Fixed_Point_Def_F_Delta (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Delta;


         
   

   function F_Range
     (Node : Ordinary_Fixed_Point_Def'Class) return Range_Spec
   is
      Result : Bare_Range_Spec;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Ordinary_Fixed_Point_Def_F_Range (Node.Internal.Node);
         if Result = null then
            return No_Range_Spec;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Range;
































         
   

   function F_Package_Name
     (Node : Package_Body'Class) return Defining_Name
   is
      Result : Bare_Defining_Name;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Package_Body_F_Package_Name (Node.Internal.Node);
         if Result = null then
            return No_Defining_Name;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Package_Name;


         
   

   function F_Decls
     (Node : Package_Body'Class) return Declarative_Part
   is
      Result : Bare_Declarative_Part;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Package_Body_F_Decls (Node.Internal.Node);
         if Result = null then
            return No_Declarative_Part;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Decls;


         
   

   function F_Stmts
     (Node : Package_Body'Class) return Handled_Stmts
   is
      Result : Bare_Handled_Stmts;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Package_Body_F_Stmts (Node.Internal.Node);
         if Result = null then
            return No_Handled_Stmts;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Stmts;


         
   

   function F_End_Name
     (Node : Package_Body'Class) return End_Name
   is
      Result : Bare_End_Name;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Package_Body_F_End_Name (Node.Internal.Node);
         if Result = null then
            return No_End_Name;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_End_Name;







         
   

   function F_Name
     (Node : Package_Body_Stub'Class) return Defining_Name
   is
      Result : Bare_Defining_Name;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Package_Body_Stub_F_Name (Node.Internal.Node);
         if Result = null then
            return No_Defining_Name;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Name;












         
   

   function F_Name
     (Node : Package_Renaming_Decl'Class) return Defining_Name
   is
      Result : Bare_Defining_Name;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Package_Renaming_Decl_F_Name (Node.Internal.Node);
         if Result = null then
            return No_Defining_Name;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Name;


         
   

   function F_Renames
     (Node : Package_Renaming_Decl'Class) return Renaming_Clause
   is
      Result : Bare_Renaming_Clause;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Package_Renaming_Decl_F_Renames (Node.Internal.Node);
         if Result = null then
            return No_Renaming_Clause;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Renames;



         
   function P_Renamed_Package
     (Node : Package_Renaming_Decl'Class) return Basic_Decl is
      


      Property_Result : Internal_Entity_Basic_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Package_Renaming_Decl_P_Renamed_Package
            (Bare_Ada_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return Wrap_Node (Property_Result.Node, Property_Result.Info).As_Basic_Decl;

   end;

         
   function P_Final_Renamed_Package
     (Node : Package_Renaming_Decl'Class) return Basic_Decl is
      


      Property_Result : Internal_Entity_Basic_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Package_Renaming_Decl_P_Final_Renamed_Package
            (Bare_Ada_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return Wrap_Node (Property_Result.Node, Property_Result.Info).As_Basic_Decl;

   end;





         
   

   function F_Designator
     (Node : Param_Assoc'Class) return Ada_Node
   is
      Result : Bare_Ada_Node;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Param_Assoc_F_Designator (Node.Internal.Node);
         if Result = null then
            return No_Ada_Node;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Designator;


         
   

   function F_R_Expr
     (Node : Param_Assoc'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Param_Assoc_F_R_Expr (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_R_Expr;







         
   

   function F_Ids
     (Node : Param_Spec'Class) return Defining_Name_List
   is
      Result : Bare_Defining_Name_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Param_Spec_F_Ids (Node.Internal.Node);
         if Result = null then
            return No_Defining_Name_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Ids;


         
   

   function F_Has_Aliased
     (Node : Param_Spec'Class) return Aliased_Node
   is
      Result : Bare_Aliased_Node;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Param_Spec_F_Has_Aliased (Node.Internal.Node);
         if Result = null then
            return No_Aliased_Node;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Has_Aliased;

         function F_Has_Aliased (Node : Param_Spec'Class) return Boolean
         is (Aliased_Node'(Node.F_Has_Aliased).Kind
             = Ada_Aliased_Present);


         
   

   function F_Mode
     (Node : Param_Spec'Class) return Mode
   is
      Result : Bare_Mode;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Param_Spec_F_Mode (Node.Internal.Node);
         if Result = null then
            return No_Mode;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Mode;

         function F_Mode
           (Node : Param_Spec'Class) return Ada_Mode
         is (Mode'(Node.F_Mode).Kind);

         
   

   function F_Type_Expr
     (Node : Param_Spec'Class) return Type_Expr
   is
      Result : Bare_Type_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Param_Spec_F_Type_Expr (Node.Internal.Node);
         if Result = null then
            return No_Type_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Type_Expr;


         
   

   function F_Default_Expr
     (Node : Param_Spec'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Param_Spec_F_Default_Expr (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Default_Expr;





         
         ----------------
         -- List_Child --
         ----------------

         function List_Child
           (Node : Param_Spec_List'Class; Index : Positive) return Param_Spec
         is
            Result : Ada_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Result := Node.Child (Index);
            return Result.As_Param_Spec;
         end List_Child;

         

         function Param_Spec_List_First (Node : Param_Spec_List) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return 1;
         end;

         function Param_Spec_List_Next
           (Node : Param_Spec_List; Cursor : Positive) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor + 1;
         end;

         function Param_Spec_List_Has_Element
           (Node : Param_Spec_List; Cursor : Positive) return Boolean is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor in 1 .. Node.Children_Count;
         end;

         function Param_Spec_List_Element
           (Node : Param_Spec_List; Cursor : Positive) return Param_Spec'Class
         is
            Child : Ada_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Child := Node.Child (Cursor);
            return Param_Spec'(Child.As_Param_Spec);
         end;






         
   

   function F_Params
     (Node : Params'Class) return Param_Spec_List
   is
      Result : Bare_Param_Spec_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Params_F_Params (Node.Internal.Node);
         if Result = null then
            return No_Param_Spec_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Params;







         
   

   function F_Decl
     (Node : Paren_Abstract_State_Decl'Class) return Ada_Node
   is
      Result : Bare_Ada_Node;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Paren_Abstract_State_Decl_F_Decl (Node.Internal.Node);
         if Result = null then
            return No_Ada_Node;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Decl;







         
   

   function F_Expr
     (Node : Paren_Expr'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Paren_Expr_F_Expr (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Expr;






















         
   

   function F_Expr
     (Node : Pp_Elsif_Directive'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Pp_Elsif_Directive_F_Expr (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Expr;


         
   

   function F_Then_Kw
     (Node : Pp_Elsif_Directive'Class) return Pp_Then_Kw
   is
      Result : Bare_Pp_Then_Kw;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Pp_Elsif_Directive_F_Then_Kw (Node.Internal.Node);
         if Result = null then
            return No_Pp_Then_Kw;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Then_Kw;












         
   

   function F_Expr
     (Node : Pp_If_Directive'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Pp_If_Directive_F_Expr (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Expr;


         
   

   function F_Then_Kw
     (Node : Pp_If_Directive'Class) return Pp_Then_Kw
   is
      Result : Bare_Pp_Then_Kw;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Pp_If_Directive_F_Then_Kw (Node.Internal.Node);
         if Result = null then
            return No_Pp_Then_Kw;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Then_Kw;












         
   

   function F_Name
     (Node : Pragma_Argument_Assoc'Class) return Name
   is
      Result : Bare_Name;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Pragma_Argument_Assoc_F_Name (Node.Internal.Node);
         if Result = null then
            return No_Name;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Name;


         
   

   function F_Expr
     (Node : Pragma_Argument_Assoc'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Pragma_Argument_Assoc_F_Expr (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Expr;







         
   

   function F_Id
     (Node : Pragma_Node'Class) return Identifier
   is
      Result : Bare_Identifier;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Pragma_Node_F_Id (Node.Internal.Node);
         if Result = null then
            return No_Identifier;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Id;


         
   

   function F_Args
     (Node : Pragma_Node'Class) return Base_Assoc_List
   is
      Result : Bare_Base_Assoc_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Pragma_Node_F_Args (Node.Internal.Node);
         if Result = null then
            return No_Base_Assoc_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Args;



         
   function P_Is_Ghost_Code
     (Node : Pragma_Node'Class) return Boolean is
      


      Property_Result : Boolean;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Pragma_Node_P_Is_Ghost_Code
            (Bare_Ada_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return Property_Result;

   end;

         
   function P_Associated_Entities
     (Node : Pragma_Node'Class) return Defining_Name_Array is
      


      Property_Result : Internal_Entity_Defining_Name_Array_Access;

         procedure Free_Internal;
         --  Dec-ref all internal arguments and the property result, when
         --  applicable.

         -------------------
         -- Free_Internal --
         -------------------

         procedure Free_Internal is
         begin
               Dec_Ref (Property_Result);
         end Free_Internal;

   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Pragma_Node_P_Associated_Entities
            (Bare_Ada_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return Result : constant Defining_Name_Array :=
            To_Public_Defining_Name_Array (Property_Result)
         do
            Free_Internal;
         end return;

      exception
         when Precondition_Failure | Property_Error =>
            Free_Internal;
            raise;
   end;



         
         ----------------
         -- List_Child --
         ----------------

         function List_Child
           (Node : Pragma_Node_List'Class; Index : Positive) return Pragma_Node
         is
            Result : Ada_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Result := Node.Child (Index);
            return Result.As_Pragma_Node;
         end List_Child;

         

         function Pragma_Node_List_First (Node : Pragma_Node_List) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return 1;
         end;

         function Pragma_Node_List_Next
           (Node : Pragma_Node_List; Cursor : Positive) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor + 1;
         end;

         function Pragma_Node_List_Has_Element
           (Node : Pragma_Node_List; Cursor : Positive) return Boolean is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor in 1 .. Node.Children_Count;
         end;

         function Pragma_Node_List_Element
           (Node : Pragma_Node_List; Cursor : Positive) return Pragma_Node'Class
         is
            Child : Ada_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Child := Node.Child (Cursor);
            return Pragma_Node'(Child.As_Pragma_Node);
         end;







         
   function P_As_Bool
     (Node : Private_Node'Class) return Boolean is
      


      Property_Result : Boolean;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Dispatcher_Private_Node_P_As_Bool
            (Bare_Ada_Node (Node.Internal.Node));

         return Property_Result;

   end;




















         
   

   function F_Has_Abstract
     (Node : Private_Type_Def'Class) return Abstract_Node
   is
      Result : Bare_Abstract_Node;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Private_Type_Def_F_Has_Abstract (Node.Internal.Node);
         if Result = null then
            return No_Abstract_Node;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Has_Abstract;

         function F_Has_Abstract (Node : Private_Type_Def'Class) return Boolean
         is (Abstract_Node'(Node.F_Has_Abstract).Kind
             = Ada_Abstract_Present);


         
   

   function F_Has_Tagged
     (Node : Private_Type_Def'Class) return Tagged_Node
   is
      Result : Bare_Tagged_Node;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Private_Type_Def_F_Has_Tagged (Node.Internal.Node);
         if Result = null then
            return No_Tagged_Node;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Has_Tagged;

         function F_Has_Tagged (Node : Private_Type_Def'Class) return Boolean
         is (Tagged_Node'(Node.F_Has_Tagged).Kind
             = Ada_Tagged_Present);


         
   

   function F_Has_Limited
     (Node : Private_Type_Def'Class) return Limited_Node
   is
      Result : Bare_Limited_Node;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Private_Type_Def_F_Has_Limited (Node.Internal.Node);
         if Result = null then
            return No_Limited_Node;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Has_Limited;

         function F_Has_Limited (Node : Private_Type_Def'Class) return Boolean
         is (Limited_Node'(Node.F_Has_Limited).Kind
             = Ada_Limited_Present);








         
   function P_As_Bool
     (Node : Protected_Node'Class) return Boolean is
      


      Property_Result : Boolean;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Dispatcher_Protected_Node_P_As_Bool
            (Bare_Ada_Node (Node.Internal.Node));

         return Property_Result;

   end;










         
   

   function F_Name
     (Node : Protected_Body'Class) return Defining_Name
   is
      Result : Bare_Defining_Name;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Protected_Body_F_Name (Node.Internal.Node);
         if Result = null then
            return No_Defining_Name;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Name;


         
   

   function F_Decls
     (Node : Protected_Body'Class) return Declarative_Part
   is
      Result : Bare_Declarative_Part;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Protected_Body_F_Decls (Node.Internal.Node);
         if Result = null then
            return No_Declarative_Part;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Decls;


         
   

   function F_End_Name
     (Node : Protected_Body'Class) return End_Name
   is
      Result : Bare_End_Name;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Protected_Body_F_End_Name (Node.Internal.Node);
         if Result = null then
            return No_End_Name;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_End_Name;







         
   

   function F_Name
     (Node : Protected_Body_Stub'Class) return Defining_Name
   is
      Result : Bare_Defining_Name;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Protected_Body_Stub_F_Name (Node.Internal.Node);
         if Result = null then
            return No_Defining_Name;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Name;







         
   

   function F_Public_Part
     (Node : Protected_Def'Class) return Public_Part
   is
      Result : Bare_Public_Part;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Protected_Def_F_Public_Part (Node.Internal.Node);
         if Result = null then
            return No_Public_Part;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Public_Part;


         
   

   function F_Private_Part
     (Node : Protected_Def'Class) return Private_Part
   is
      Result : Bare_Private_Part;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Protected_Def_F_Private_Part (Node.Internal.Node);
         if Result = null then
            return No_Private_Part;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Private_Part;


         
   

   function F_End_Name
     (Node : Protected_Def'Class) return End_Name
   is
      Result : Bare_End_Name;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Protected_Def_F_End_Name (Node.Internal.Node);
         if Result = null then
            return No_End_Name;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_End_Name;












         
   

   function F_Discriminants
     (Node : Protected_Type_Decl'Class) return Discriminant_Part
   is
      Result : Bare_Discriminant_Part;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Protected_Type_Decl_F_Discriminants (Node.Internal.Node);
         if Result = null then
            return No_Discriminant_Part;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Discriminants;


         
   

   function F_Interfaces
     (Node : Protected_Type_Decl'Class) return Parent_List
   is
      Result : Bare_Parent_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Protected_Type_Decl_F_Interfaces (Node.Internal.Node);
         if Result = null then
            return No_Parent_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Interfaces;


         
   

   function F_Definition
     (Node : Protected_Type_Decl'Class) return Protected_Def
   is
      Result : Bare_Protected_Def;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Protected_Type_Decl_F_Definition (Node.Internal.Node);
         if Result = null then
            return No_Protected_Def;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Definition;












         
   

   function F_Prefix
     (Node : Qual_Expr'Class) return Name
   is
      Result : Bare_Name;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Qual_Expr_F_Prefix (Node.Internal.Node);
         if Result = null then
            return No_Name;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Prefix;


         
   

   function F_Suffix
     (Node : Qual_Expr'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Qual_Expr_F_Suffix (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Suffix;







         
   

   function F_Quantifier
     (Node : Quantified_Expr'Class) return Quantifier
   is
      Result : Bare_Quantifier;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Quantified_Expr_F_Quantifier (Node.Internal.Node);
         if Result = null then
            return No_Quantifier;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Quantifier;

         function F_Quantifier
           (Node : Quantified_Expr'Class) return Ada_Quantifier
         is (Quantifier'(Node.F_Quantifier).Kind);

         
   

   function F_Loop_Spec
     (Node : Quantified_Expr'Class) return For_Loop_Spec
   is
      Result : Bare_For_Loop_Spec;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Quantified_Expr_F_Loop_Spec (Node.Internal.Node);
         if Result = null then
            return No_For_Loop_Spec;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Loop_Spec;


         
   

   function F_Expr
     (Node : Quantified_Expr'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Quantified_Expr_F_Expr (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Expr;






















         
   

   function F_Exception_Name
     (Node : Raise_Expr'Class) return Name
   is
      Result : Bare_Name;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Raise_Expr_F_Exception_Name (Node.Internal.Node);
         if Result = null then
            return No_Name;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Exception_Name;


         
   

   function F_Error_Message
     (Node : Raise_Expr'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Raise_Expr_F_Error_Message (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Error_Message;







         
   

   function F_Exception_Name
     (Node : Raise_Stmt'Class) return Name
   is
      Result : Bare_Name;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Raise_Stmt_F_Exception_Name (Node.Internal.Node);
         if Result = null then
            return No_Name;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Exception_Name;


         
   

   function F_Error_Message
     (Node : Raise_Stmt'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Raise_Stmt_F_Error_Message (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Error_Message;







         
   

   function F_Range
     (Node : Range_Constraint'Class) return Range_Spec
   is
      Result : Bare_Range_Spec;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Range_Constraint_F_Range (Node.Internal.Node);
         if Result = null then
            return No_Range_Spec;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Range;







         
   

   function F_Range
     (Node : Range_Spec'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Range_Spec_F_Range (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Range;

















         
   

   function F_Name
     (Node : Record_Rep_Clause'Class) return Name
   is
      Result : Bare_Name;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Record_Rep_Clause_F_Name (Node.Internal.Node);
         if Result = null then
            return No_Name;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Name;


         
   

   function F_At_Expr
     (Node : Record_Rep_Clause'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Record_Rep_Clause_F_At_Expr (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_At_Expr;


         
   

   function F_Components
     (Node : Record_Rep_Clause'Class) return Ada_Node_List
   is
      Result : Bare_Ada_Node_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Record_Rep_Clause_F_Components (Node.Internal.Node);
         if Result = null then
            return No_Ada_Node_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Components;







         
   

   function F_Has_Abstract
     (Node : Record_Type_Def'Class) return Abstract_Node
   is
      Result : Bare_Abstract_Node;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Record_Type_Def_F_Has_Abstract (Node.Internal.Node);
         if Result = null then
            return No_Abstract_Node;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Has_Abstract;

         function F_Has_Abstract (Node : Record_Type_Def'Class) return Boolean
         is (Abstract_Node'(Node.F_Has_Abstract).Kind
             = Ada_Abstract_Present);


         
   

   function F_Has_Tagged
     (Node : Record_Type_Def'Class) return Tagged_Node
   is
      Result : Bare_Tagged_Node;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Record_Type_Def_F_Has_Tagged (Node.Internal.Node);
         if Result = null then
            return No_Tagged_Node;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Has_Tagged;

         function F_Has_Tagged (Node : Record_Type_Def'Class) return Boolean
         is (Tagged_Node'(Node.F_Has_Tagged).Kind
             = Ada_Tagged_Present);


         
   

   function F_Has_Limited
     (Node : Record_Type_Def'Class) return Limited_Node
   is
      Result : Bare_Limited_Node;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Record_Type_Def_F_Has_Limited (Node.Internal.Node);
         if Result = null then
            return No_Limited_Node;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Has_Limited;

         function F_Has_Limited (Node : Record_Type_Def'Class) return Boolean
         is (Limited_Node'(Node.F_Has_Limited).Kind
             = Ada_Limited_Present);


         
   

   function F_Record_Def
     (Node : Record_Type_Def'Class) return Base_Record_Def
   is
      Result : Bare_Base_Record_Def;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Record_Type_Def_F_Record_Def (Node.Internal.Node);
         if Result = null then
            return No_Base_Record_Def;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Record_Def;







         
   

   function F_Prefix
     (Node : Reduce_Attribute_Ref'Class) return Ada_Node
   is
      Result : Bare_Ada_Node;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Reduce_Attribute_Ref_F_Prefix (Node.Internal.Node);
         if Result = null then
            return No_Ada_Node;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Prefix;


         
   

   function F_Attribute
     (Node : Reduce_Attribute_Ref'Class) return Identifier
   is
      Result : Bare_Identifier;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Reduce_Attribute_Ref_F_Attribute (Node.Internal.Node);
         if Result = null then
            return No_Identifier;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Attribute;


         
   

   function F_Args
     (Node : Reduce_Attribute_Ref'Class) return Assoc_List
   is
      Result : Bare_Assoc_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Reduce_Attribute_Ref_F_Args (Node.Internal.Node);
         if Result = null then
            return No_Assoc_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Args;












         
   

   function F_Renamed_Object
     (Node : Renaming_Clause'Class) return Name
   is
      Result : Bare_Name;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Renaming_Clause_F_Renamed_Object (Node.Internal.Node);
         if Result = null then
            return No_Name;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Renamed_Object;







         
   

   function F_Call_Name
     (Node : Requeue_Stmt'Class) return Name
   is
      Result : Bare_Name;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Requeue_Stmt_F_Call_Name (Node.Internal.Node);
         if Result = null then
            return No_Name;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Call_Name;


         
   

   function F_Has_Abort
     (Node : Requeue_Stmt'Class) return Abort_Node
   is
      Result : Bare_Abort_Node;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Requeue_Stmt_F_Has_Abort (Node.Internal.Node);
         if Result = null then
            return No_Abort_Node;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Has_Abort;

         function F_Has_Abort (Node : Requeue_Stmt'Class) return Boolean
         is (Abort_Node'(Node.F_Has_Abort).Kind
             = Ada_Abort_Present);







         
   

   function F_Return_Expr
     (Node : Return_Stmt'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Return_Stmt_F_Return_Expr (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Return_Expr;








         
   function P_As_Bool
     (Node : Reverse_Node'Class) return Boolean is
      


      Property_Result : Boolean;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Dispatcher_Reverse_Node_P_As_Bool
            (Bare_Ada_Node (Node.Internal.Node));

         return Property_Result;

   end;















         
   

   function F_Guards
     (Node : Select_Stmt'Class) return Select_When_Part_List
   is
      Result : Bare_Select_When_Part_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Select_Stmt_F_Guards (Node.Internal.Node);
         if Result = null then
            return No_Select_When_Part_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Guards;


         
   

   function F_Else_Stmts
     (Node : Select_Stmt'Class) return Stmt_List
   is
      Result : Bare_Stmt_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Select_Stmt_F_Else_Stmts (Node.Internal.Node);
         if Result = null then
            return No_Stmt_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Else_Stmts;


         
   

   function F_Abort_Stmts
     (Node : Select_Stmt'Class) return Stmt_List
   is
      Result : Bare_Stmt_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Select_Stmt_F_Abort_Stmts (Node.Internal.Node);
         if Result = null then
            return No_Stmt_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Abort_Stmts;







         
   

   function F_Cond_Expr
     (Node : Select_When_Part'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Select_When_Part_F_Cond_Expr (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Cond_Expr;


         
   

   function F_Stmts
     (Node : Select_When_Part'Class) return Stmt_List
   is
      Result : Bare_Stmt_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Select_When_Part_F_Stmts (Node.Internal.Node);
         if Result = null then
            return No_Stmt_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Stmts;





         
         ----------------
         -- List_Child --
         ----------------

         function List_Child
           (Node : Select_When_Part_List'Class; Index : Positive) return Select_When_Part
         is
            Result : Ada_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Result := Node.Child (Index);
            return Result.As_Select_When_Part;
         end List_Child;

         

         function Select_When_Part_List_First (Node : Select_When_Part_List) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return 1;
         end;

         function Select_When_Part_List_Next
           (Node : Select_When_Part_List; Cursor : Positive) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor + 1;
         end;

         function Select_When_Part_List_Has_Element
           (Node : Select_When_Part_List; Cursor : Positive) return Boolean is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor in 1 .. Node.Children_Count;
         end;

         function Select_When_Part_List_Element
           (Node : Select_When_Part_List; Cursor : Positive) return Select_When_Part'Class
         is
            Child : Ada_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Child := Node.Child (Cursor);
            return Select_When_Part'(Child.As_Select_When_Part);
         end;






         
   

   function F_Range
     (Node : Signed_Int_Type_Def'Class) return Range_Spec
   is
      Result : Bare_Range_Spec;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Signed_Int_Type_Def_F_Range (Node.Internal.Node);
         if Result = null then
            return No_Range_Spec;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Range;







         
   

   function F_Name
     (Node : Single_Protected_Decl'Class) return Defining_Name
   is
      Result : Bare_Defining_Name;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Single_Protected_Decl_F_Name (Node.Internal.Node);
         if Result = null then
            return No_Defining_Name;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Name;


         
   

   function F_Interfaces
     (Node : Single_Protected_Decl'Class) return Parent_List
   is
      Result : Bare_Parent_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Single_Protected_Decl_F_Interfaces (Node.Internal.Node);
         if Result = null then
            return No_Parent_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Interfaces;


         
   

   function F_Definition
     (Node : Single_Protected_Decl'Class) return Protected_Def
   is
      Result : Bare_Protected_Def;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Single_Protected_Decl_F_Definition (Node.Internal.Node);
         if Result = null then
            return No_Protected_Def;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Definition;







         
   

   function F_Task_Type
     (Node : Single_Task_Decl'Class) return Single_Task_Type_Decl
   is
      Result : Bare_Single_Task_Type_Decl;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Single_Task_Decl_F_Task_Type (Node.Internal.Node);
         if Result = null then
            return No_Single_Task_Type_Decl;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Task_Type;







         
   

   function F_Discriminants
     (Node : Task_Type_Decl'Class) return Discriminant_Part
   is
      Result : Bare_Discriminant_Part;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Task_Type_Decl_F_Discriminants (Node.Internal.Node);
         if Result = null then
            return No_Discriminant_Part;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Discriminants;


         
   

   function F_Definition
     (Node : Task_Type_Decl'Class) return Task_Def
   is
      Result : Bare_Task_Def;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Task_Type_Decl_F_Definition (Node.Internal.Node);
         if Result = null then
            return No_Task_Def;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Definition;


















         
   function P_Denoted_Value
     (Node : String_Literal'Class) return Text_Type is
      


      Property_Result : String_Type;

         procedure Free_Internal;
         --  Dec-ref all internal arguments and the property result, when
         --  applicable.

         -------------------
         -- Free_Internal --
         -------------------

         procedure Free_Internal is
         begin
               Dec_Ref (Property_Result);
         end Free_Internal;

   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Extensions.String_Literal_P_Denoted_Value
            (Bare_Ada_Node (Node.Internal.Node));

         return Result : constant Text_Type :=
            Property_Result.Content
         do
            Free_Internal;
         end return;

      exception
         when Precondition_Failure | Property_Error =>
            Free_Internal;
            raise;
   end;





         
   

   function F_Decls
     (Node : Subp_Body'Class) return Declarative_Part
   is
      Result : Bare_Declarative_Part;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Subp_Body_F_Decls (Node.Internal.Node);
         if Result = null then
            return No_Declarative_Part;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Decls;


         
   

   function F_Stmts
     (Node : Subp_Body'Class) return Handled_Stmts
   is
      Result : Bare_Handled_Stmts;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Subp_Body_F_Stmts (Node.Internal.Node);
         if Result = null then
            return No_Handled_Stmts;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Stmts;


         
   

   function F_End_Name
     (Node : Subp_Body'Class) return End_Name
   is
      Result : Bare_End_Name;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Subp_Body_F_End_Name (Node.Internal.Node);
         if Result = null then
            return No_End_Name;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_End_Name;







         
   

   function F_Overriding
     (Node : Subp_Body_Stub'Class) return Overriding_Node
   is
      Result : Bare_Overriding_Node;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Subp_Body_Stub_F_Overriding (Node.Internal.Node);
         if Result = null then
            return No_Overriding_Node;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Overriding;

         function F_Overriding
           (Node : Subp_Body_Stub'Class) return Ada_Overriding_Node
         is (Overriding_Node'(Node.F_Overriding).Kind);

         
   

   function F_Subp_Spec
     (Node : Subp_Body_Stub'Class) return Subp_Spec
   is
      Result : Bare_Subp_Spec;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Subp_Body_Stub_F_Subp_Spec (Node.Internal.Node);
         if Result = null then
            return No_Subp_Spec;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Subp_Spec;



























         
   

   function F_Renames
     (Node : Subp_Renaming_Decl'Class) return Renaming_Clause
   is
      Result : Bare_Renaming_Clause;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Subp_Renaming_Decl_F_Renames (Node.Internal.Node);
         if Result = null then
            return No_Renaming_Clause;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Renames;







         
   

   function F_Subp_Kind
     (Node : Subp_Spec'Class) return Subp_Kind
   is
      Result : Bare_Subp_Kind;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Subp_Spec_F_Subp_Kind (Node.Internal.Node);
         if Result = null then
            return No_Subp_Kind;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Subp_Kind;

         function F_Subp_Kind
           (Node : Subp_Spec'Class) return Ada_Subp_Kind
         is (Subp_Kind'(Node.F_Subp_Kind).Kind);

         
   

   function F_Subp_Name
     (Node : Subp_Spec'Class) return Defining_Name
   is
      Result : Bare_Defining_Name;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Subp_Spec_F_Subp_Name (Node.Internal.Node);
         if Result = null then
            return No_Defining_Name;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Subp_Name;


         
   

   function F_Subp_Params
     (Node : Subp_Spec'Class) return Params
   is
      Result : Bare_Params;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Subp_Spec_F_Subp_Params (Node.Internal.Node);
         if Result = null then
            return No_Params;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Subp_Params;


         
   

   function F_Subp_Returns
     (Node : Subp_Spec'Class) return Type_Expr
   is
      Result : Bare_Type_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Subp_Spec_F_Subp_Returns (Node.Internal.Node);
         if Result = null then
            return No_Type_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Subp_Returns;







         
   

   function F_Subtype
     (Node : Subtype_Decl'Class) return Subtype_Indication
   is
      Result : Bare_Subtype_Indication;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Subtype_Decl_F_Subtype (Node.Internal.Node);
         if Result = null then
            return No_Subtype_Indication;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Subtype;







         
   

   function F_Name
     (Node : Subunit'Class) return Name
   is
      Result : Bare_Name;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Subunit_F_Name (Node.Internal.Node);
         if Result = null then
            return No_Name;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Name;


         
   

   function F_Body
     (Node : Subunit'Class) return Body_Node
   is
      Result : Bare_Body_Node;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Subunit_F_Body (Node.Internal.Node);
         if Result = null then
            return No_Body_Node;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Body;



         
   function P_Body_Root
     (Node : Subunit'Class) return Basic_Decl is
      


      Property_Result : Internal_Entity_Basic_Decl;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Subunit_P_Body_Root
            (Bare_Ada_Node (Node.Internal.Node));

         return Wrap_Node (Property_Result.Node, Property_Result.Info).As_Basic_Decl;

   end;






         
   function P_As_Bool
     (Node : Synchronized_Node'Class) return Boolean is
      


      Property_Result : Boolean;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Dispatcher_Synchronized_Node_P_As_Bool
            (Bare_Ada_Node (Node.Internal.Node));

         return Property_Result;

   end;




















         
   

   function F_Left_Param
     (Node : Synthetic_Binary_Spec'Class) return Synthetic_Formal_Param_Decl
   is
      Result : Bare_Synthetic_Formal_Param_Decl;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Synthetic_Binary_Spec_F_Left_Param (Node.Internal.Node);
         if Result = null then
            return No_Synthetic_Formal_Param_Decl;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Left_Param;


         
   

   function F_Right_Param
     (Node : Synthetic_Binary_Spec'Class) return Synthetic_Formal_Param_Decl
   is
      Result : Bare_Synthetic_Formal_Param_Decl;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Synthetic_Binary_Spec_F_Right_Param (Node.Internal.Node);
         if Result = null then
            return No_Synthetic_Formal_Param_Decl;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Right_Param;


         
   

   function F_Return_Type_Expr
     (Node : Synthetic_Binary_Spec'Class) return Type_Expr
   is
      Result : Bare_Type_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Synthetic_Binary_Spec_F_Return_Type_Expr (Node.Internal.Node);
         if Result = null then
            return No_Type_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Return_Type_Expr;








         
   function P_Expr
     (Node : Synthetic_Char_Enum_Lit'Class) return Defining_Name is
      


      Property_Result : Internal_Entity_Defining_Name;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Synthetic_Char_Enum_Lit_P_Expr
            (Bare_Ada_Node (Node.Internal.Node), E_Info => Node.Internal.Info);

         return Wrap_Node (Property_Result.Node, Property_Result.Info).As_Defining_Name;

   end;










         
   

   function F_Param_Type
     (Node : Synthetic_Formal_Param_Decl'Class) return Type_Expr
   is
      Result : Bare_Type_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Synthetic_Formal_Param_Decl_F_Param_Type (Node.Internal.Node);
         if Result = null then
            return No_Type_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Param_Type;

















         
   

   function F_Spec
     (Node : Synthetic_Subp_Decl'Class) return Base_Subp_Spec
   is
      Result : Bare_Base_Subp_Spec;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Synthetic_Subp_Decl_F_Spec (Node.Internal.Node);
         if Result = null then
            return No_Base_Subp_Spec;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Spec;







         
   

   function F_Target_Type
     (Node : Synthetic_Type_Expr'Class) return Base_Type_Decl
   is
      Result : Bare_Base_Type_Decl;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Synthetic_Type_Expr_F_Target_Type (Node.Internal.Node);
         if Result = null then
            return No_Base_Type_Decl;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Target_Type;







         
   

   function F_Right_Param
     (Node : Synthetic_Unary_Spec'Class) return Synthetic_Formal_Param_Decl
   is
      Result : Bare_Synthetic_Formal_Param_Decl;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Synthetic_Unary_Spec_F_Right_Param (Node.Internal.Node);
         if Result = null then
            return No_Synthetic_Formal_Param_Decl;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Right_Param;


         
   

   function F_Return_Type_Expr
     (Node : Synthetic_Unary_Spec'Class) return Synthetic_Type_Expr
   is
      Result : Bare_Synthetic_Type_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Synthetic_Unary_Spec_F_Return_Type_Expr (Node.Internal.Node);
         if Result = null then
            return No_Synthetic_Type_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Return_Type_Expr;








         
   function P_As_Bool
     (Node : Tagged_Node'Class) return Boolean is
      


      Property_Result : Boolean;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Dispatcher_Tagged_Node_P_As_Bool
            (Bare_Ada_Node (Node.Internal.Node));

         return Property_Result;

   end;




















         
   

   function F_Name
     (Node : Task_Body'Class) return Defining_Name
   is
      Result : Bare_Defining_Name;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Task_Body_F_Name (Node.Internal.Node);
         if Result = null then
            return No_Defining_Name;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Name;


         
   

   function F_Decls
     (Node : Task_Body'Class) return Declarative_Part
   is
      Result : Bare_Declarative_Part;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Task_Body_F_Decls (Node.Internal.Node);
         if Result = null then
            return No_Declarative_Part;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Decls;


         
   

   function F_Stmts
     (Node : Task_Body'Class) return Handled_Stmts
   is
      Result : Bare_Handled_Stmts;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Task_Body_F_Stmts (Node.Internal.Node);
         if Result = null then
            return No_Handled_Stmts;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Stmts;


         
   

   function F_End_Name
     (Node : Task_Body'Class) return End_Name
   is
      Result : Bare_End_Name;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Task_Body_F_End_Name (Node.Internal.Node);
         if Result = null then
            return No_End_Name;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_End_Name;







         
   

   function F_Name
     (Node : Task_Body_Stub'Class) return Defining_Name
   is
      Result : Bare_Defining_Name;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Task_Body_Stub_F_Name (Node.Internal.Node);
         if Result = null then
            return No_Defining_Name;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Name;







         
   

   function F_Interfaces
     (Node : Task_Def'Class) return Parent_List
   is
      Result : Bare_Parent_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Task_Def_F_Interfaces (Node.Internal.Node);
         if Result = null then
            return No_Parent_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Interfaces;


         
   

   function F_Public_Part
     (Node : Task_Def'Class) return Public_Part
   is
      Result : Bare_Public_Part;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Task_Def_F_Public_Part (Node.Internal.Node);
         if Result = null then
            return No_Public_Part;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Public_Part;


         
   

   function F_Private_Part
     (Node : Task_Def'Class) return Private_Part
   is
      Result : Bare_Private_Part;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Task_Def_F_Private_Part (Node.Internal.Node);
         if Result = null then
            return No_Private_Part;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Private_Part;


         
   

   function F_End_Name
     (Node : Task_Def'Class) return End_Name
   is
      Result : Bare_End_Name;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Task_Def_F_End_Name (Node.Internal.Node);
         if Result = null then
            return No_End_Name;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_End_Name;












         
   

   function F_Has_All
     (Node : Type_Access_Def'Class) return All_Node
   is
      Result : Bare_All_Node;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Type_Access_Def_F_Has_All (Node.Internal.Node);
         if Result = null then
            return No_All_Node;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Has_All;

         function F_Has_All (Node : Type_Access_Def'Class) return Boolean
         is (All_Node'(Node.F_Has_All).Kind
             = Ada_All_Present);


         
   

   function F_Has_Constant
     (Node : Type_Access_Def'Class) return Constant_Node
   is
      Result : Bare_Constant_Node;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Type_Access_Def_F_Has_Constant (Node.Internal.Node);
         if Result = null then
            return No_Constant_Node;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Has_Constant;

         function F_Has_Constant (Node : Type_Access_Def'Class) return Boolean
         is (Constant_Node'(Node.F_Has_Constant).Kind
             = Ada_Constant_Present);


         
   

   function F_Subtype_Indication
     (Node : Type_Access_Def'Class) return Subtype_Indication
   is
      Result : Bare_Subtype_Indication;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Type_Access_Def_F_Subtype_Indication (Node.Internal.Node);
         if Result = null then
            return No_Subtype_Indication;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Subtype_Indication;












         
   

   function F_Op
     (Node : Un_Op'Class) return Op
   is
      Result : Bare_Op;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Un_Op_F_Op (Node.Internal.Node);
         if Result = null then
            return No_Op;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Op;

         function F_Op
           (Node : Un_Op'Class) return Ada_Op
         is (Op'(Node.F_Op).Kind);

         
   

   function F_Expr
     (Node : Un_Op'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Un_Op_F_Expr (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Expr;







         
   

   function F_Subtype_Indication
     (Node : Unconstrained_Array_Index'Class) return Subtype_Indication
   is
      Result : Bare_Subtype_Indication;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Unconstrained_Array_Index_F_Subtype_Indication (Node.Internal.Node);
         if Result = null then
            return No_Subtype_Indication;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Subtype_Indication;





         
         ----------------
         -- List_Child --
         ----------------

         function List_Child
           (Node : Unconstrained_Array_Index_List'Class; Index : Positive) return Unconstrained_Array_Index
         is
            Result : Ada_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Result := Node.Child (Index);
            return Result.As_Unconstrained_Array_Index;
         end List_Child;

         

         function Unconstrained_Array_Index_List_First (Node : Unconstrained_Array_Index_List) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return 1;
         end;

         function Unconstrained_Array_Index_List_Next
           (Node : Unconstrained_Array_Index_List; Cursor : Positive) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor + 1;
         end;

         function Unconstrained_Array_Index_List_Has_Element
           (Node : Unconstrained_Array_Index_List; Cursor : Positive) return Boolean is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor in 1 .. Node.Children_Count;
         end;

         function Unconstrained_Array_Index_List_Element
           (Node : Unconstrained_Array_Index_List; Cursor : Positive) return Unconstrained_Array_Index'Class
         is
            Child : Ada_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Child := Node.Child (Cursor);
            return Unconstrained_Array_Index'(Child.As_Unconstrained_Array_Index);
         end;






         
   

   function F_Types
     (Node : Unconstrained_Array_Indices'Class) return Unconstrained_Array_Index_List
   is
      Result : Bare_Unconstrained_Array_Index_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Unconstrained_Array_Indices_F_Types (Node.Internal.Node);
         if Result = null then
            return No_Unconstrained_Array_Index_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Types;













         
   function P_As_Bool
     (Node : Until_Node'Class) return Boolean is
      


      Property_Result : Boolean;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Dispatcher_Until_Node_P_As_Bool
            (Bare_Ada_Node (Node.Internal.Node));

         return Property_Result;

   end;















         
   

   function F_Prefix
     (Node : Update_Attribute_Ref'Class) return Name
   is
      Result : Bare_Name;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Update_Attribute_Ref_F_Prefix (Node.Internal.Node);
         if Result = null then
            return No_Name;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Prefix;


         
   

   function F_Attribute
     (Node : Update_Attribute_Ref'Class) return Identifier
   is
      Result : Bare_Identifier;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Update_Attribute_Ref_F_Attribute (Node.Internal.Node);
         if Result = null then
            return No_Identifier;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Attribute;


         
   

   function F_Values
     (Node : Update_Attribute_Ref'Class) return Base_Aggregate
   is
      Result : Bare_Base_Aggregate;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Update_Attribute_Ref_F_Values (Node.Internal.Node);
         if Result = null then
            return No_Base_Aggregate;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Values;












         
   

   function F_Packages
     (Node : Use_Package_Clause'Class) return Name_List
   is
      Result : Bare_Name_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Use_Package_Clause_F_Packages (Node.Internal.Node);
         if Result = null then
            return No_Name_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Packages;







         
   

   function F_Has_All
     (Node : Use_Type_Clause'Class) return All_Node
   is
      Result : Bare_All_Node;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Use_Type_Clause_F_Has_All (Node.Internal.Node);
         if Result = null then
            return No_All_Node;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Has_All;

         function F_Has_All (Node : Use_Type_Clause'Class) return Boolean
         is (All_Node'(Node.F_Has_All).Kind
             = Ada_All_Present);


         
   

   function F_Types
     (Node : Use_Type_Clause'Class) return Name_List
   is
      Result : Bare_Name_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Use_Type_Clause_F_Types (Node.Internal.Node);
         if Result = null then
            return No_Name_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Types;







         
   

   function F_Iter_Assoc
     (Node : Value_Sequence'Class) return Iterated_Assoc
   is
      Result : Bare_Iterated_Assoc;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Value_Sequence_F_Iter_Assoc (Node.Internal.Node);
         if Result = null then
            return No_Iterated_Assoc;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Iter_Assoc;







         
   

   function F_Choices
     (Node : Variant'Class) return Alternatives_List
   is
      Result : Bare_Alternatives_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Variant_F_Choices (Node.Internal.Node);
         if Result = null then
            return No_Alternatives_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Choices;


         
   

   function F_Components
     (Node : Variant'Class) return Component_List
   is
      Result : Bare_Component_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Variant_F_Components (Node.Internal.Node);
         if Result = null then
            return No_Component_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Components;





         
         ----------------
         -- List_Child --
         ----------------

         function List_Child
           (Node : Variant_List'Class; Index : Positive) return Variant
         is
            Result : Ada_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Result := Node.Child (Index);
            return Result.As_Variant;
         end List_Child;

         

         function Variant_List_First (Node : Variant_List) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return 1;
         end;

         function Variant_List_Next
           (Node : Variant_List; Cursor : Positive) return Positive is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor + 1;
         end;

         function Variant_List_Has_Element
           (Node : Variant_List; Cursor : Positive) return Boolean is
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            return Cursor in 1 .. Node.Children_Count;
         end;

         function Variant_List_Element
           (Node : Variant_List; Cursor : Positive) return Variant'Class
         is
            Child : Ada_Node;
         begin
            if Node.Internal.Node = null then
               raise Precondition_Failure with "null node argument";
            end if;

            Child := Node.Child (Cursor);
            return Variant'(Child.As_Variant);
         end;






         
   

   function F_Discr_Name
     (Node : Variant_Part'Class) return Identifier
   is
      Result : Bare_Identifier;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Variant_Part_F_Discr_Name (Node.Internal.Node);
         if Result = null then
            return No_Identifier;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Discr_Name;


         
   

   function F_Variant
     (Node : Variant_Part'Class) return Variant_List
   is
      Result : Bare_Variant_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.Variant_Part_F_Variant (Node.Internal.Node);
         if Result = null then
            return No_Variant_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Variant;







         
   

   function F_Expr
     (Node : While_Loop_Spec'Class) return Expr
   is
      Result : Bare_Expr;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.While_Loop_Spec_F_Expr (Node.Internal.Node);
         if Result = null then
            return No_Expr;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Expr;












         
   

   function F_Has_Limited
     (Node : With_Clause'Class) return Limited_Node
   is
      Result : Bare_Limited_Node;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.With_Clause_F_Has_Limited (Node.Internal.Node);
         if Result = null then
            return No_Limited_Node;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Has_Limited;

         function F_Has_Limited (Node : With_Clause'Class) return Boolean
         is (Limited_Node'(Node.F_Has_Limited).Kind
             = Ada_Limited_Present);


         
   

   function F_Has_Private
     (Node : With_Clause'Class) return Private_Node
   is
      Result : Bare_Private_Node;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.With_Clause_F_Has_Private (Node.Internal.Node);
         if Result = null then
            return No_Private_Node;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Has_Private;

         function F_Has_Private (Node : With_Clause'Class) return Boolean
         is (Private_Node'(Node.F_Has_Private).Kind
             = Ada_Private_Present);


         
   

   function F_Packages
     (Node : With_Clause'Class) return Name_List
   is
      Result : Bare_Name_List;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Result := Implementation.With_Clause_F_Packages (Node.Internal.Node);
         if Result = null then
            return No_Name_List;
         else
            return (Internal   => (Result, Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         end if;
   end F_Packages;








         
   function P_As_Bool
     (Node : With_Private'Class) return Boolean is
      


      Property_Result : Boolean;


   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);


      
      Property_Result :=
         Libadalang.Implementation.Dispatcher_With_Private_P_As_Bool
            (Bare_Ada_Node (Node.Internal.Node));

         return Property_Result;

   end;













   --------------------
   -- Children_Count --
   --------------------

   function Children_Count
     (Node : Ada_Node'Class) return Natural is
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      return Children_Count (Node.Internal.Node);
   end Children_Count;

   -----------------------
   -- First_Child_Index --
   -----------------------

   function First_Child_Index
     (Node : Ada_Node'Class) return Natural is
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      return First_Child_Index (Node.Internal.Node);
   end First_Child_Index;

   ----------------------
   -- Last_Child_Index --
   ----------------------

   function Last_Child_Index
     (Node : Ada_Node'Class) return Natural is
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      return Last_Child_Index (Node.Internal.Node);
   end Last_Child_Index;

   -----------------
   -- First_Child --
   -----------------

   function First_Child
     (Node : Ada_Node'Class) return Ada_Node is
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

      return Node.Child (First_Child_Index (Node.Internal.Node));
   end First_Child;

   ----------------
   -- Last_Child --
   ----------------

   function Last_Child
     (Node : Ada_Node'Class) return Ada_Node is
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);

      return Node.Child (Last_Child_Index (Node.Internal.Node));
   end Last_Child;

   ---------------
   -- Get_Child --
   ---------------

   procedure Get_Child
     (Node            : Ada_Node'Class;
      Index           : Positive;
      Index_In_Bounds : out Boolean;
      Result          : out Ada_Node)
   is
      N : Bare_Ada_Node;
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Get_Child (Node.Internal.Node, Index, Index_In_Bounds, N);
      Result := Wrap_Node (N, Node.Internal.Info);
   end Get_Child;

   -----------
   -- Child --
   -----------

   function Child
     (Node  : Ada_Node'Class;
      Index : Positive) return Ada_Node
   is
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      return Wrap_Node (Child (Node.Internal.Node, Index), Node.Internal.Info);
   end Child;

   ----------------
   -- Sloc_Range --
   ----------------

   function Sloc_Range
     (Node : Ada_Node'Class) return Source_Location_Range is
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      return Sloc_Range (Node.Internal.Node);
   end Sloc_Range;

   -------------
   -- Compare --
   -------------

   function Compare
     (Node : Ada_Node'Class;
      Sloc : Source_Location) return Relative_Position is
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      return Compare (Node.Internal.Node, Sloc);
   end Compare;

   ------------
   -- Lookup --
   ------------

   function Lookup
     (Node : Ada_Node'Class;
      Sloc : Source_Location) return Ada_Node is
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      return Wrap_Node (Lookup (Node.Internal.Node, Sloc));
   end Lookup;

   ----------
   -- Text --
   ----------

   function Text (Node : Ada_Node'Class) return Text_Type is
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      return Implementation.Text (Node.Internal.Node);
   end Text;

   -----------------
   -- Token_Range --
   -----------------

   function Token_Range
     (Node : Ada_Node'Class) return Token_Iterator is
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      return Token_Iterator'(Node.As_Ada_Node,
                             Node.Internal.Node.Token_End_Index);
   end Token_Range;

   -----------
   -- Print --
   -----------

   procedure Print
     (Node        : Ada_Node'Class;
      Show_Slocs  : Boolean := True;
      Line_Prefix : String := "") is
   begin
      Check_Safety_Net (Node);
      Print (Node.Internal.Node, Show_Slocs, Line_Prefix);
   end Print;

   ---------------
   -- PP_Trivia --
   ---------------

   procedure PP_Trivia
     (Node : Ada_Node'Class; Line_Prefix : String := "") is
   begin
      Check_Safety_Net (Node);
      PP_Trivia (Node.Internal.Node, Line_Prefix);
   end PP_Trivia;

   --------------
   -- Traverse --
   --------------

   function Traverse
     (Node  : Ada_Node'Class;
      Visit : access function (Node : Ada_Node'Class)
              return Visit_Status)
      return Visit_Status
   is
      Info : constant Internal_Entity_Info := Node.Internal.Info;

      -------------
      -- Wrapper --
      -------------

      function Wrapper (Node : Bare_Ada_Node) return Visit_Status
      is
         Public_Node : constant Ada_Node :=
           Wrap_Node (Bare_Ada_Node (Node), Info);
      begin
         return Visit (Public_Node);
      end Wrapper;

   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      return Traverse (Node.Internal.Node, Wrapper'Access);
   end Traverse;

   --------------
   -- Traverse --
   --------------

   procedure Traverse
     (Node  : Ada_Node'Class;
      Visit : access function (Node : Ada_Node'Class)
                               return Visit_Status)
   is
      Result_Status : Visit_Status;
      pragma Unreferenced (Result_Status);
   begin
      Result_Status := Traverse (Node, Visit);
   end Traverse;

   --------------------------------
   -- Assign_Names_To_Logic_Vars --
   --------------------------------

   procedure Assign_Names_To_Logic_Vars (Node : Ada_Node'Class)
   is
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      Assign_Names_To_Logic_Vars (Node.Internal.Node);
   end Assign_Names_To_Logic_Vars;

   -------------------------
   -- Children_And_Trivia --
   -------------------------

   function Children_And_Trivia
     (Node : Ada_Node'Class) return Children_Array
   is
   begin
      if Node.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (Node);
      declare
         Bare_Result : constant Bare_Children_Array :=
            Children_And_Trivia (Unwrap_Node (Node));
         Result      : Children_Array (Bare_Result'Range);
      begin
         for I in Bare_Result'Range loop
            declare
               BR : Bare_Child_Record renames Bare_Result (I);
               R  : Child_Record renames Result (I);
            begin
               case BR.Kind is
                  when Child =>
                     R := (Child, Wrap_Node (BR.Node));
                  when Trivia =>
                     R := (Trivia, BR.Trivia);
               end case;
            end;
         end loop;
         return Result;
      end;
   end Children_And_Trivia;

   -----------------
   -- First_Token --
   -----------------

   function First_Token (Self : Token_Iterator) return Token_Reference is
   begin
      Check_Safety_Net (Self.Node);
      return Token_Start (Self.Node);
   end First_Token;

   ----------------
   -- Next_Token --
   ----------------

   function Next_Token
     (Self : Token_Iterator; Tok : Token_Reference) return Token_Reference is
   begin
      Check_Safety_Net (Self.Node);
      return Next (Tok);
   end Next_Token;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element
     (Self : Token_Iterator; Tok : Token_Reference) return Boolean is
   begin
      Check_Safety_Net (Self.Node);
      return Get_Token_Index (Tok).Token <= Self.Last;
   end Has_Element;

   -------------
   -- Element --
   -------------

   function Element
     (Self : Token_Iterator; Tok : Token_Reference) return Token_Reference is
   begin
      Check_Safety_Net (Self.Node);
      return Tok;
   end Element;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (Context : in out Analysis_Context) is
   begin
      Context.Internal := null;
   end Initialize;

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (Context : in out Analysis_Context) is
   begin
      Inc_Ref (Unwrap_Context (Context));
   end Adjust;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Context : in out Analysis_Context) is
      Ctx : Internal_Context := Unwrap_Context (Context);
   begin
      Dec_Ref (Ctx);
      Context.Internal := null;
   end Finalize;

   ----------------------------------------------------
   -- Soft links for public/internal type converters --
   ----------------------------------------------------

   function Wrap_Context (Context : Internal_Context) return Analysis_Context;
   function Unwrap_Context
     (Context : Analysis_Context'Class) return Internal_Context;

   function Wrap_Unit (Unit : Internal_Unit) return Analysis_Unit;
   function Unwrap_Unit (Unit : Analysis_Unit'Class) return Internal_Unit;

   function Wrap_Node
     (Node : Bare_Ada_Node;
      Info : Internal_Entity_Info := No_Entity_Info)
      return Ada_Node;
   function Unwrap_Node
     (Node : Ada_Node'Class) return Bare_Ada_Node;
   function Unwrap_Entity
     (Entity : Ada_Node'Class) return Internal_Entity;

   ------------------
   -- Wrap_Context --
   ------------------

   function Wrap_Context (Context : Internal_Context) return Analysis_Context
   is
   begin
      Inc_Ref (Context);
      return (Ada.Finalization.Controlled with
              Internal => Internal_Context_Access (Context));
   end Wrap_Context;

   --------------------
   -- Unwrap_Context --
   --------------------

   function Unwrap_Context
     (Context : Analysis_Context'Class) return Internal_Context
   is (Internal_Context (Context.Internal));

   ---------------
   -- Wrap_Unit --
   ---------------

   function Wrap_Unit (Unit : Internal_Unit) return Analysis_Unit
   is (if Unit = null
       then No_Analysis_Unit
       else (Internal => Internal_Unit_Access (Unit),
             Context  => Wrap_Context (Context (Unit))));

   -----------------
   -- Unwrap_Unit --
   -----------------

   function Unwrap_Unit (Unit : Analysis_Unit'Class) return Internal_Unit
   is (Internal_Unit (Unit.Internal));

   ----------------------
   -- Check_Safety_Net --
   ----------------------

   procedure Check_Safety_Net (Self : Ada_Node'Class) is
      R  : Env_Rebindings renames Self.Internal.Info.Rebindings;
      SN : Node_Safety_Net renames Self.Safety_Net;
   begin
      if SN.Context = null then
         return;
      end if;

      --  Check that SN's context has not been released (see the Context_Pool)
      if SN.Context.Serial_Number /= SN.Context_Serial then
         raise Stale_Reference_Error with "context was released";

      --  Then check that the unit version is the same
      elsif SN.Unit.Unit_Version /= SN.Unit_Version then
         raise Stale_Reference_Error with "unit was reparsed";

      --  Then check that the R rebindings reference, if not-null, is not stale
      elsif R /= null and then R.Version /= SN.Rebindings_Version then
         raise Stale_Reference_Error with "related unit was reparsed";
      end if;
   end Check_Safety_Net;

   ---------------
   -- Wrap_Node --
   ---------------

   function Wrap_Node
     (Node : Bare_Ada_Node;
      Info : Internal_Entity_Info := No_Entity_Info)
      return Ada_Node is
   begin
      if Node = null then
         return No_Ada_Node;
      end if;

      declare
         Unit               : constant Internal_Unit := Node.Unit;
         Context            : constant Internal_Context := Unit.Context;
         Rebindings_Version : constant Version_Number :=
           (if Info.Rebindings = null
            then 0
            else Info.Rebindings.Version);
      begin
         return ((Internal   => (Node, Info),
                  Safety_Net => (Context            => Context,
                                 Context_Serial     => Context.Serial_Number,
                                 Unit               => Unit,
                                 Unit_Version       => Unit.Unit_Version,
                                 Rebindings_Version => Rebindings_Version)));
      end;
   end;

   -----------------
   -- Unwrap_Node --
   -----------------

   function Unwrap_Node
     (Node : Ada_Node'Class) return Bare_Ada_Node
   is (Node.Internal.Node);

   -------------------
   -- Unwrap_Entity --
   -------------------

   function Unwrap_Entity
     (Entity : Ada_Node'Class) return Internal_Entity
   is ((Entity.Internal));

   
      
----------------
-- Is_Keyword --
----------------

function Is_Keyword
  (Token   : Token_Reference;
   Version : Language_Version) return Boolean
is
   TDH   : constant Token_Data_Handler_Access := Get_Token_TDH (Token);
   Index : constant Token_Or_Trivia_Index := Get_Token_Index (Token);
begin
   return Libadalang.Lexer.Is_Keyword (TDH.all, Index, Version);
end Is_Keyword;



begin
   Public_Converters.Wrap_Context := Wrap_Context'Access;
   Public_Converters.Unwrap_Context := Unwrap_Context'Access;
   Public_Converters.Wrap_Unit := Wrap_Unit'Access;
   Public_Converters.Unwrap_Unit := Unwrap_Unit'Access;
   Public_Converters.Wrap_Node := Wrap_Node'Access;
   Public_Converters.Unwrap_Node := Unwrap_Node'Access;
   Public_Converters.Unwrap_Entity := Unwrap_Entity'Access;
end Libadalang.Analysis;
