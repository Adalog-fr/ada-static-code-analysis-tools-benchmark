pragma Warnings (Off);
pragma Ada_95;
pragma Source_File_Name (ada_main, Spec_File_Name => "b__arenstorf.ads");
pragma Source_File_Name (ada_main, Body_File_Name => "b__arenstorf.adb");
pragma Suppress (Overflow_Check);
with Ada.Exceptions;

package body ada_main is

   E081 : Short_Integer; pragma Import (Ada, E081, "system__os_lib_E");
   E006 : Short_Integer; pragma Import (Ada, E006, "ada__exceptions_E");
   E011 : Short_Integer; pragma Import (Ada, E011, "system__soft_links_E");
   E022 : Short_Integer; pragma Import (Ada, E022, "system__exception_table_E");
   E048 : Short_Integer; pragma Import (Ada, E048, "ada__containers_E");
   E076 : Short_Integer; pragma Import (Ada, E076, "ada__io_exceptions_E");
   E029 : Short_Integer; pragma Import (Ada, E029, "ada__numerics_E");
   E063 : Short_Integer; pragma Import (Ada, E063, "ada__strings_E");
   E065 : Short_Integer; pragma Import (Ada, E065, "ada__strings__maps_E");
   E068 : Short_Integer; pragma Import (Ada, E068, "ada__strings__maps__constants_E");
   E053 : Short_Integer; pragma Import (Ada, E053, "interfaces__c_E");
   E023 : Short_Integer; pragma Import (Ada, E023, "system__exceptions_E");
   E090 : Short_Integer; pragma Import (Ada, E090, "system__object_reader_E");
   E058 : Short_Integer; pragma Import (Ada, E058, "system__dwarf_lines_E");
   E018 : Short_Integer; pragma Import (Ada, E018, "system__soft_links__initialize_E");
   E047 : Short_Integer; pragma Import (Ada, E047, "system__traceback__symbolic_E");
   E028 : Short_Integer; pragma Import (Ada, E028, "system__img_int_E");
   E071 : Short_Integer; pragma Import (Ada, E071, "system__img_uns_E");
   E111 : Short_Integer; pragma Import (Ada, E111, "ada__strings__utf_encoding_E");
   E119 : Short_Integer; pragma Import (Ada, E119, "ada__tags_E");
   E109 : Short_Integer; pragma Import (Ada, E109, "ada__strings__text_buffers_E");
   E127 : Short_Integer; pragma Import (Ada, E127, "ada__streams_E");
   E139 : Short_Integer; pragma Import (Ada, E139, "system__file_control_block_E");
   E138 : Short_Integer; pragma Import (Ada, E138, "system__finalization_root_E");
   E136 : Short_Integer; pragma Import (Ada, E136, "ada__finalization_E");
   E135 : Short_Integer; pragma Import (Ada, E135, "system__file_io_E");
   E176 : Short_Integer; pragma Import (Ada, E176, "system__storage_pools_E");
   E174 : Short_Integer; pragma Import (Ada, E174, "system__finalization_masters_E");
   E170 : Short_Integer; pragma Import (Ada, E170, "ada__calendar_E");
   E125 : Short_Integer; pragma Import (Ada, E125, "ada__text_io_E");
   E178 : Short_Integer; pragma Import (Ada, E178, "system__pool_global_E");
   E160 : Short_Integer; pragma Import (Ada, E160, "system__img_llli_E");
   E157 : Short_Integer; pragma Import (Ada, E157, "system__img_lli_E");
   E191 : Short_Integer; pragma Import (Ada, E191, "system__img_llu_E");
   E144 : Short_Integer; pragma Import (Ada, E144, "g_matrices_E");
   E146 : Short_Integer; pragma Import (Ada, E146, "graph_E");
   E152 : Short_Integer; pragma Import (Ada, E152, "graph__postscript_driver_E");

   Sec_Default_Sized_Stacks : array (1 .. 1) of aliased System.Secondary_Stack.SS_Stack (System.Parameters.Runtime_Default_Sec_Stack_Size);

   Local_Priority_Specific_Dispatching : constant String := "";
   Local_Interrupt_States : constant String := "";

   Is_Elaborated : Boolean := False;

   procedure finalize_library is
   begin
      E146 := E146 - 1;
      declare
         procedure F1;
         pragma Import (Ada, F1, "graph__finalize_spec");
      begin
         F1;
      end;
      E178 := E178 - 1;
      declare
         procedure F2;
         pragma Import (Ada, F2, "system__pool_global__finalize_spec");
      begin
         F2;
      end;
      E125 := E125 - 1;
      declare
         procedure F3;
         pragma Import (Ada, F3, "ada__text_io__finalize_spec");
      begin
         F3;
      end;
      E174 := E174 - 1;
      declare
         procedure F4;
         pragma Import (Ada, F4, "system__finalization_masters__finalize_spec");
      begin
         F4;
      end;
      declare
         procedure F5;
         pragma Import (Ada, F5, "system__file_io__finalize_body");
      begin
         E135 := E135 - 1;
         F5;
      end;
      declare
         procedure Reraise_Library_Exception_If_Any;
            pragma Import (Ada, Reraise_Library_Exception_If_Any, "__gnat_reraise_library_exception_if_any");
      begin
         Reraise_Library_Exception_If_Any;
      end;
   end finalize_library;

   procedure adafinal is
      procedure s_stalib_adafinal;
      pragma Import (Ada, s_stalib_adafinal, "system__standard_library__adafinal");

      procedure Runtime_Finalize;
      pragma Import (C, Runtime_Finalize, "__gnat_runtime_finalize");

   begin
      if not Is_Elaborated then
         return;
      end if;
      Is_Elaborated := False;
      Runtime_Finalize;
      s_stalib_adafinal;
   end adafinal;

   type No_Param_Proc is access procedure;
   pragma Favor_Top_Level (No_Param_Proc);

   procedure adainit is
      Main_Priority : Integer;
      pragma Import (C, Main_Priority, "__gl_main_priority");
      Time_Slice_Value : Integer;
      pragma Import (C, Time_Slice_Value, "__gl_time_slice_val");
      WC_Encoding : Character;
      pragma Import (C, WC_Encoding, "__gl_wc_encoding");
      Locking_Policy : Character;
      pragma Import (C, Locking_Policy, "__gl_locking_policy");
      Queuing_Policy : Character;
      pragma Import (C, Queuing_Policy, "__gl_queuing_policy");
      Task_Dispatching_Policy : Character;
      pragma Import (C, Task_Dispatching_Policy, "__gl_task_dispatching_policy");
      Priority_Specific_Dispatching : System.Address;
      pragma Import (C, Priority_Specific_Dispatching, "__gl_priority_specific_dispatching");
      Num_Specific_Dispatching : Integer;
      pragma Import (C, Num_Specific_Dispatching, "__gl_num_specific_dispatching");
      Main_CPU : Integer;
      pragma Import (C, Main_CPU, "__gl_main_cpu");
      Interrupt_States : System.Address;
      pragma Import (C, Interrupt_States, "__gl_interrupt_states");
      Num_Interrupt_States : Integer;
      pragma Import (C, Num_Interrupt_States, "__gl_num_interrupt_states");
      Unreserve_All_Interrupts : Integer;
      pragma Import (C, Unreserve_All_Interrupts, "__gl_unreserve_all_interrupts");
      Exception_Tracebacks : Integer;
      pragma Import (C, Exception_Tracebacks, "__gl_exception_tracebacks");
      Exception_Tracebacks_Symbolic : Integer;
      pragma Import (C, Exception_Tracebacks_Symbolic, "__gl_exception_tracebacks_symbolic");
      Detect_Blocking : Integer;
      pragma Import (C, Detect_Blocking, "__gl_detect_blocking");
      Default_Stack_Size : Integer;
      pragma Import (C, Default_Stack_Size, "__gl_default_stack_size");
      Default_Secondary_Stack_Size : System.Parameters.Size_Type;
      pragma Import (C, Default_Secondary_Stack_Size, "__gnat_default_ss_size");
      Bind_Env_Addr : System.Address;
      pragma Import (C, Bind_Env_Addr, "__gl_bind_env_addr");

      procedure Runtime_Initialize (Install_Handler : Integer);
      pragma Import (C, Runtime_Initialize, "__gnat_runtime_initialize");

      Finalize_Library_Objects : No_Param_Proc;
      pragma Import (C, Finalize_Library_Objects, "__gnat_finalize_library_objects");
      Binder_Sec_Stacks_Count : Natural;
      pragma Import (Ada, Binder_Sec_Stacks_Count, "__gnat_binder_ss_count");
      Default_Sized_SS_Pool : System.Address;
      pragma Import (Ada, Default_Sized_SS_Pool, "__gnat_default_ss_pool");

   begin
      if Is_Elaborated then
         return;
      end if;
      Is_Elaborated := True;
      Main_Priority := -1;
      Time_Slice_Value := -1;
      WC_Encoding := 'b';
      Locking_Policy := ' ';
      Queuing_Policy := ' ';
      Task_Dispatching_Policy := ' ';
      Priority_Specific_Dispatching :=
        Local_Priority_Specific_Dispatching'Address;
      Num_Specific_Dispatching := 0;
      Main_CPU := -1;
      Interrupt_States := Local_Interrupt_States'Address;
      Num_Interrupt_States := 0;
      Unreserve_All_Interrupts := 0;
      Exception_Tracebacks := 1;
      Exception_Tracebacks_Symbolic := 1;
      Detect_Blocking := 0;
      Default_Stack_Size := -1;

      ada_main'Elab_Body;
      Default_Secondary_Stack_Size := System.Parameters.Runtime_Default_Sec_Stack_Size;
      Binder_Sec_Stacks_Count := 1;
      Default_Sized_SS_Pool := Sec_Default_Sized_Stacks'Address;

      Runtime_Initialize (1);

      Finalize_Library_Objects := finalize_library'access;

      Ada.Exceptions'Elab_Spec;
      System.Soft_Links'Elab_Spec;
      System.Exception_Table'Elab_Body;
      E022 := E022 + 1;
      Ada.Containers'Elab_Spec;
      E048 := E048 + 1;
      Ada.Io_Exceptions'Elab_Spec;
      E076 := E076 + 1;
      Ada.Numerics'Elab_Spec;
      E029 := E029 + 1;
      Ada.Strings'Elab_Spec;
      E063 := E063 + 1;
      Ada.Strings.Maps'Elab_Spec;
      E065 := E065 + 1;
      Ada.Strings.Maps.Constants'Elab_Spec;
      E068 := E068 + 1;
      Interfaces.C'Elab_Spec;
      E053 := E053 + 1;
      System.Exceptions'Elab_Spec;
      E023 := E023 + 1;
      System.Object_Reader'Elab_Spec;
      E090 := E090 + 1;
      System.Dwarf_Lines'Elab_Spec;
      System.Os_Lib'Elab_Body;
      E081 := E081 + 1;
      System.Soft_Links.Initialize'Elab_Body;
      E018 := E018 + 1;
      E011 := E011 + 1;
      System.Traceback.Symbolic'Elab_Body;
      E047 := E047 + 1;
      System.Img_Int'Elab_Spec;
      E028 := E028 + 1;
      E006 := E006 + 1;
      System.Img_Uns'Elab_Spec;
      E071 := E071 + 1;
      E058 := E058 + 1;
      Ada.Strings.Utf_Encoding'Elab_Spec;
      E111 := E111 + 1;
      Ada.Tags'Elab_Spec;
      Ada.Tags'Elab_Body;
      E119 := E119 + 1;
      Ada.Strings.Text_Buffers'Elab_Spec;
      E109 := E109 + 1;
      Ada.Streams'Elab_Spec;
      E127 := E127 + 1;
      System.File_Control_Block'Elab_Spec;
      E139 := E139 + 1;
      System.Finalization_Root'Elab_Spec;
      E138 := E138 + 1;
      Ada.Finalization'Elab_Spec;
      E136 := E136 + 1;
      System.File_Io'Elab_Body;
      E135 := E135 + 1;
      System.Storage_Pools'Elab_Spec;
      E176 := E176 + 1;
      System.Finalization_Masters'Elab_Spec;
      System.Finalization_Masters'Elab_Body;
      E174 := E174 + 1;
      Ada.Calendar'Elab_Spec;
      Ada.Calendar'Elab_Body;
      E170 := E170 + 1;
      Ada.Text_Io'Elab_Spec;
      Ada.Text_Io'Elab_Body;
      E125 := E125 + 1;
      System.Pool_Global'Elab_Spec;
      System.Pool_Global'Elab_Body;
      E178 := E178 + 1;
      System.Img_Llli'Elab_Spec;
      E160 := E160 + 1;
      System.Img_Lli'Elab_Spec;
      E157 := E157 + 1;
      System.Img_Llu'Elab_Spec;
      E191 := E191 + 1;
      E144 := E144 + 1;
      Graph'Elab_Spec;
      E152 := E152 + 1;
      Graph'Elab_Body;
      E146 := E146 + 1;
   end adainit;

   procedure Ada_Main_Program;
   pragma Import (Ada, Ada_Main_Program, "_ada_arenstorf");

   function main
     (argc : Integer;
      argv : System.Address;
      envp : System.Address)
      return Integer
   is
      procedure Initialize (Addr : System.Address);
      pragma Import (C, Initialize, "__gnat_initialize");

      procedure Finalize;
      pragma Import (C, Finalize, "__gnat_finalize");
      SEH : aliased array (1 .. 2) of Integer;

      Ensure_Reference : aliased System.Address := Ada_Main_Program_Name'Address;
      pragma Volatile (Ensure_Reference);

   begin
      if gnat_argc = 0 then
         gnat_argc := argc;
         gnat_argv := argv;
      end if;
      gnat_envp := envp;

      Initialize (SEH'Address);
      adainit;
      Ada_Main_Program;
      adafinal;
      Finalize;
      return (gnat_exit_status);
   end;

--  BEGIN Object file/option list
   --   /workspaces/bench-source/src/mathpaqs/obj_fast/dormand_prince_8.o
   --   /workspaces/bench-source/src/mathpaqs/obj_fast/g_matrices.o
   --   /workspaces/bench-source/src/mathpaqs/obj_fast/time_display.o
   --   /workspaces/bench-source/src/mathpaqs/obj_fast/graph-postscript_driver.o
   --   /workspaces/bench-source/src/mathpaqs/obj_fast/graph.o
   --   /workspaces/bench-source/src/mathpaqs/obj_fast/arenstorf.o
   --   -L/workspaces/bench-source/src/mathpaqs/obj_fast/
   --   -L/workspaces/bench-source/src/mathpaqs/obj_fast/
   --   -L/usr/gnat/lib/gcc/x86_64-pc-linux-gnu/12.2.1/adalib/
   --   -static
   --   -lgnat
   --   -lm
   --   -ldl
--  END Object file/option list   

end ada_main;