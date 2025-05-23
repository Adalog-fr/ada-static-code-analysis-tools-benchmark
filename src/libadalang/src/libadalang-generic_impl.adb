--
--  Copyright (C) 2014-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Unchecked_Deallocation;

with Libadalang.Public_Converters;  use Libadalang.Public_Converters;

package body Libadalang.Generic_Impl is

   ---------
   -- "+" --
   ---------

   function "+"
     (Entity : Internal_Entity) return Implementation.Internal_Entity
   is
      Md : constant Internal_Node_Metadata_Access := +Entity.Metadata;
   begin
      return (Node => +Entity.Node,
              Info => (Md           => (if Md = null
                                        then Implementation.No_Metadata
                                        else Md.Internal),
                       Rebindings   => Entity.Rebindings,
                       From_Rebound => Entity.From_Rebound));
   end "+";

   ---------
   -- "+" --
   ---------

   function "+"
     (Entity : Implementation.Internal_Entity) return Internal_Entity
   is
      use Libadalang.Implementation;

      Md : constant Internal_Node_Metadata_Access :=
        (if Entity.Info.Md = Implementation.No_Metadata
         then null
         else new Internal_Node_Metadata_Type'
                    (Ref_Count => 1,
                     Internal  => Entity.Info.Md));
   begin
      return (Node         => +Entity.Node,
              Rebindings   => Entity.Info.Rebindings,
              From_Rebound => Entity.Info.From_Rebound,
              Metadata     => +Md);
   end "+";

   --------------------
   -- Create_Context --
   --------------------

   function Create_Context
     (Charset     : String;
      File_Reader : File_Reader_Reference;
      With_Trivia : Boolean;
      Tab_Stop    : Natural) return Internal_Context
   is
      FR : Implementation.Internal_File_Reader_Access :=
         Wrap_Public_File_Reader (File_Reader);

      Actual_Tab_Stop : constant Positive :=
        (if Tab_Stop = 0
         then 8
         else Tab_Stop);

      Result : constant Implementation.Internal_Context :=
        Implementation.Create_Context
          (Charset        => Charset,
           File_Reader    => FR,
           Event_Handler  => null,
           Unit_Provider  => null,
           With_Trivia    => With_Trivia,
           Tab_Stop       => Actual_Tab_Stop,
           Max_Call_Depth => 1000);
   begin
      return +Result;
   end Create_Context;

   ---------------------
   -- Context_Inc_Ref --
   ---------------------

   procedure Context_Inc_Ref (Context : Internal_Context) is
   begin
      Implementation.Inc_Ref (+Context);
   end Context_Inc_Ref;

   ---------------------
   -- Context_Dec_Ref --
   ---------------------

   procedure Context_Dec_Ref (Context : in out Internal_Context) is
      Ctx : Implementation.Internal_Context := +Context;
   begin
      Implementation.Dec_Ref (Ctx);
      Context := +Ctx;
   end Context_Dec_Ref;

   ---------------------
   -- Context_Version --
   ---------------------

   function Context_Version (Context : Internal_Context) return Version_Number
   is
      Ctx : constant Implementation.Internal_Context := +Context;
   begin
      return Ctx.Serial_Number;
   end Context_Version;

   ----------------------
   -- Context_Has_Unit --
   ----------------------

   function Context_Has_Unit
     (Context : Internal_Context; Unit_Filename : String) return Boolean is
   begin
      return Implementation.Has_Unit (+Context, Unit_Filename);
   end Context_Has_Unit;

   ---------------------------
   -- Context_Get_From_File --
   ---------------------------

   function Context_Get_From_File
     (Context           : Internal_Context;
      Filename, Charset : String;
      Reparse           : Boolean;
      Rule              : Grammar_Rule_Index) return Internal_Unit
   is
      Ctx : constant Implementation.Internal_Context := +Context;
   begin
      return +Implementation.Get_From_File
        (Ctx, Filename, Charset, Reparse, +Rule);
   end Context_Get_From_File;

   ------------------
   -- Unit_Context --
   ------------------

   function Unit_Context (Unit : Internal_Unit) return Internal_Context is
      U : constant Implementation.Internal_Unit := +Unit;
   begin
      return +U.Context;
   end Unit_Context;

   ------------------
   -- Unit_Version --
   ------------------

   function Unit_Version (Unit : Internal_Unit) return Version_Number is
      U : constant Implementation.Internal_Unit := +Unit;
   begin
      return U.Unit_Version;
   end Unit_Version;

   -------------------
   -- Unit_Filename --
   -------------------

   function Unit_Filename (Unit : Internal_Unit) return String is
      U : constant Implementation.Internal_Unit := +Unit;
   begin
      return Implementation.Get_Filename (U);
   end Unit_Filename;

   ---------------
   -- Unit_Root --
   ---------------

   function Unit_Root (Unit : Internal_Unit) return Internal_Node is
      U : constant Implementation.Internal_Unit := +Unit;
   begin
      return +U.Ast_Root;
   end Unit_Root;

   ----------------------
   -- Unit_First_Token --
   ----------------------

   function Unit_First_Token (Unit : Internal_Unit) return Internal_Token is
      U : constant Implementation.Internal_Unit := +Unit;
   begin
      return +Implementation.First_Token (U);
   end Unit_First_Token;

   ---------------------
   -- Unit_Last_Token --
   ---------------------

   function Unit_Last_Token (Unit : Internal_Unit) return Internal_Token is
      U : constant Implementation.Internal_Unit := +Unit;
   begin
      return +Implementation.Last_Token (U);
   end Unit_Last_Token;

   -------------------
   -- Unit_Get_Line --
   -------------------

   function Unit_Get_Line
     (Unit : Internal_Unit; Line_Number : Positive) return Text_Type
   is
      U : constant Implementation.Internal_Unit := +Unit;
   begin
      return Implementation.Get_Line (U, Line_Number);
   end Unit_Get_Line;

   ---------------------------
   -- Node_Metadata_Inc_Ref --
   ---------------------------

   procedure Node_Metadata_Inc_Ref (Metadata : Internal_Node_Metadata) is
      Md : constant Internal_Node_Metadata_Access := +Metadata;
   begin
      Md.Ref_Count := Md.Ref_Count + 1;
   end Node_Metadata_Inc_Ref;

   ---------------------------
   -- Node_Metadata_Dec_Ref --
   ---------------------------

   procedure Node_Metadata_Dec_Ref (Metadata : in out Internal_Node_Metadata)
   is
      procedure Destroy is new Ada.Unchecked_Deallocation
        (Internal_Node_Metadata_Type, Internal_Node_Metadata_Access);
      Md : Internal_Node_Metadata_Access := +Metadata;
   begin
      Md.Ref_Count := Md.Ref_Count - 1;
      if Md.Ref_Count = 0 then
         Destroy (Md);
      end if;
      Metadata := No_Internal_Node_Metadata;
   end Node_Metadata_Dec_Ref;

   ---------------
   -- Node_Unit --
   ---------------

   function Node_Unit (Node : Internal_Node) return Internal_Unit is
      N : constant Implementation.Bare_Ada_Node := +Node;
   begin
      return +N.Unit;
   end Node_Unit;

   ---------------
   -- Node_Kind --
   ---------------

   function Node_Kind (Node : Internal_Node) return Type_Index is
      N : constant Implementation.Bare_Ada_Node := +Node;
   begin
      return Node_Kinds (N.Kind);
   end Node_Kind;

   -----------------
   -- Node_Parent --
   -----------------

   function Node_Parent (Node : Internal_Entity) return Internal_Entity is
      E      : constant Implementation.Internal_Entity := +Node;
      Result : constant Implementation.Internal_Entity :=
        Implementation.Parent (E.Node, E.Info);
   begin
      return +Result;
   end Node_Parent;

   ------------------
   -- Node_Parents --
   ------------------

   function Node_Parents
     (Node : Internal_Entity; With_Self : Boolean) return Internal_Entity_Array
   is
      E       : constant Implementation.Internal_Entity := +Node;
      Parents : Implementation.Internal_Entity_Array_Access :=
        Implementation.Parents (E.Node, With_Self, E.Info);
   begin
      return Result : Internal_Entity_Array (Parents.Items'Range) do
         for I in Result'Range loop
            Result (I) := +Parents.Items (I);
         end loop;
         Implementation.Dec_Ref (Parents);
      end return;
   end Node_Parents;

   -------------------------
   -- Node_Children_Count --
   -------------------------

   function Node_Children_Count (Node : Internal_Node) return Natural is
      N : constant Implementation.Bare_Ada_Node := +Node;
   begin
      return Implementation.Children_Count (N);
   end Node_Children_Count;

   --------------------
   -- Node_Get_Child --
   --------------------

   procedure Node_Get_Child
     (Node            : Internal_Node;
      Index           : Positive;
      Index_In_Bounds : out Boolean;
      Result          : out Internal_Node)
   is
      R : Implementation.Bare_Ada_Node;
   begin
      Implementation.Get_Child (+Node, Index, Index_In_Bounds, R);
      Result := +R;
   end Node_Get_Child;

   ------------------------
   -- Node_Fetch_Sibling --
   ------------------------

   function Node_Fetch_Sibling
     (Node : Internal_Node; Offset : Integer) return Internal_Node is
   begin
      return +Implementation.Fetch_Sibling (+Node, Offset);
   end Node_Fetch_Sibling;

   -------------------
   -- Node_Is_Ghost --
   -------------------

   function Node_Is_Ghost (Node : Analysis.Internal_Node) return Boolean is
   begin
      return Implementation.Is_Ghost (+Node);
   end Node_Is_Ghost;

   ----------------------
   -- Node_Token_Start --
   ----------------------

   function Node_Token_Start (Node : Internal_Node) return Internal_Token is
   begin
      return +Implementation.Token_Start (+Node);
   end Node_Token_Start;

   --------------------
   -- Node_Token_End --
   --------------------

   function Node_Token_End (Node : Internal_Node) return Internal_Token is
   begin
      return +Implementation.Token_End (+Node);
   end Node_Token_End;

   ---------------
   -- Node_Text --
   ---------------

   function Node_Text (Node : Internal_Node) return Text_Type is
   begin
      return Implementation.Text (+Node);
   end Node_Text;

   ---------------------
   -- Node_Sloc_Range --
   ---------------------

   function Node_Sloc_Range
     (Node : Internal_Node) return Source_Location_Range is
   begin
      return Implementation.Sloc_Range (+Node);
   end Node_Sloc_Range;

   -------------------------------
   -- Node_Last_Attempted_Child --
   -------------------------------

   function Node_Last_Attempted_Child (Node : Internal_Node) return Integer is
      N : Implementation.Bare_Ada_Node := +Node;
   begin
      return N.Last_Attempted_Child;
   end Node_Last_Attempted_Child;

   ------------------
   -- Entity_Image --
   ------------------

   function Entity_Image (Entity : Internal_Entity) return String is
   begin
      return Implementation.Image (+Entity);
   end Entity_Image;

   -------------------------
   -- Token_Is_Equivalent --
   -------------------------

   function Token_Is_Equivalent
     (Left, Right       : Internal_Token;
      Left_SN, Right_SN : Token_Safety_Net) return Boolean
   is
   begin
      return Common.Is_Equivalent
        (Wrap_Token (Left_SN.Context, Left),
         Wrap_Token (Right_SN.Context, Right));
   end Token_Is_Equivalent;

end Libadalang.Generic_Impl;
