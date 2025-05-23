--
--  Copyright (C) 2014-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with Langkit_Support.Diagnostics;  use Langkit_Support.Diagnostics;
with Langkit_Support.File_Readers; use Langkit_Support.File_Readers;

with Libadalang.Analysis;       use Libadalang.Analysis;
with Libadalang.Common;         use Libadalang.Common;
with Libadalang.Implementation; use Libadalang.Implementation;

--  Internal package: provide implementation helpers to switch between public
--  types and implementation ones.

private package Libadalang.Public_Converters is

   use Support.Text;

   type Context_Wrapper is access function
     (Context : Internal_Context) return Analysis_Context;
   Wrap_Context : Context_Wrapper;

   type Context_Unwrapper is access function
     (Context : Analysis_Context'Class) return Internal_Context;
   Unwrap_Context : Context_Unwrapper;

   type Unit_Wrapper is access function
     (Unit : Internal_Unit) return Analysis_Unit;
   Wrap_Unit : Unit_Wrapper;

   type Unit_Unwrapper is access function
     (Unit : Analysis_Unit'Class) return Internal_Unit;
   Unwrap_Unit : Unit_Unwrapper;

   type Node_Wrapper is access function
     (Node : Bare_Ada_Node;
      Info : Internal_Entity_Info := No_Entity_Info)
      return Ada_Node;
   Wrap_Node : Node_Wrapper;

   type Node_Unwrapper is access function
     (Node : Ada_Node'Class) return Bare_Ada_Node;
   Unwrap_Node : Node_Unwrapper;

   type Entity_Unwrapper is access function
     (Entity : Ada_Node'Class) return Internal_Entity;
   Unwrap_Entity : Entity_Unwrapper;

   -------------------------
   -- File_Reader_Wrapper --
   -------------------------

   --  This wraps a file reader using the public API into the one that fits our
   --  internal APIs.

   type File_Reader_Wrapper is new Internal_File_Reader with record
      Ref_Count : Natural;
      Internal  : File_Reader_Reference;
   end record;

   overriding procedure Inc_Ref (Self : in out File_Reader_Wrapper);
   overriding function Dec_Ref
     (Self : in out File_Reader_Wrapper) return Boolean;

   overriding procedure Read
     (Self        : File_Reader_Wrapper;
      Filename    : String;
      Charset     : String;
      Read_BOM    : Boolean;
      Contents    : out Decoded_File_Contents;
      Diagnostics : in out Diagnostics_Vectors.Vector);

   function Wrap_Public_File_Reader
     (File_Reader : File_Reader_Reference) return Internal_File_Reader_Access;
   --  Wrap a public file reader inside an internal one. If File_Reader is a
   --  null reference, return null. Otherwise, this returns a new internal
   --  file reader allocation, with a ref-count of 1.

   ---------------------------
   -- Unit_Provider_Wrapper --
   ---------------------------

   --  This wraps a unit provider using the public API into one that fits in
   --  our internal APIs.

   type Unit_Provider_Wrapper is new Internal_Unit_Provider with record
      Ref_Count : Natural;
      Internal  : Unit_Provider_Reference;
   end record;

   overriding procedure Inc_Ref (Provider : in out Unit_Provider_Wrapper);
   overriding function Dec_Ref
     (Provider : in out Unit_Provider_Wrapper) return Boolean;

   overriding function Get_Unit_Filename
     (Provider : Unit_Provider_Wrapper;
      Name     : Text_Type;
      Kind     : Analysis_Unit_Kind) return String;
   overriding function Get_Unit
     (Provider    : Unit_Provider_Wrapper;
      Context     : Internal_Context;
      Name        : Text_Type;
      Kind        : Analysis_Unit_Kind;
      Charset     : String := "";
      Reparse     : Boolean := False) return Internal_Unit;

   function Wrap_Public_Provider
     (Provider : Unit_Provider_Reference) return Internal_Unit_Provider_Access;
   --  Wrap a public unit provider inside an internal one. If Provider is a
   --  null reference, return null. Otherwise, this returns a new internal
   --  provider allocation, with a ref-count of 1.

   ---------------------------
   -- Event_Handler_Wrapper --
   ---------------------------

   type Event_Handler_Wrapper is new Internal_Event_Handler with record
      Ref_Count : Natural;
      Internal  : Event_Handler_Reference;
   end record;

   overriding procedure Unit_Requested_Callback
     (Self               : in out Event_Handler_Wrapper;
      Context            : Internal_Context;
      Name               : Text_Type;
      From               : Internal_Unit;
      Found              : Boolean;
      Is_Not_Found_Error : Boolean);

   overriding procedure Unit_Parsed_Callback
     (Self     : in out Event_Handler_Wrapper;
      Context  : Internal_Context;
      Unit     : Internal_Unit;
      Reparsed : Boolean);

   function Wrap_Public_Event_Handler
     (Self : Event_Handler_Reference) return Internal_Event_Handler_Access;
   --  Wrap a public event inside an internal one. If Self is a
   --  null reference, return null. Otherwise, this returns a new internal
   --  handler allocation, with a ref-count of 1.

   overriding procedure Inc_Ref (Self : in out Event_Handler_Wrapper);
   overriding function Dec_Ref
     (Self : in out Event_Handler_Wrapper) return Boolean;

end Libadalang.Public_Converters;
