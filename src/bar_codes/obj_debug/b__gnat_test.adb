pragma Warnings (Off);
pragma Ada_95;
pragma Source_File_Name (ada_main, Spec_File_Name => "b__gnat_test.ads");
pragma Source_File_Name (ada_main, Body_File_Name => "b__gnat_test.adb");
pragma Suppress (Overflow_Check);
with Ada.Exceptions;

package body ada_main is

   E082 : Short_Integer; pragma Import (Ada, E082, "system__os_lib_E");
   E007 : Short_Integer; pragma Import (Ada, E007, "ada__exceptions_E");
   E012 : Short_Integer; pragma Import (Ada, E012, "system__soft_links_E");
   E023 : Short_Integer; pragma Import (Ada, E023, "system__exception_table_E");
   E049 : Short_Integer; pragma Import (Ada, E049, "ada__containers_E");
   E077 : Short_Integer; pragma Import (Ada, E077, "ada__io_exceptions_E");
   E030 : Short_Integer; pragma Import (Ada, E030, "ada__numerics_E");
   E064 : Short_Integer; pragma Import (Ada, E064, "ada__strings_E");
   E066 : Short_Integer; pragma Import (Ada, E066, "ada__strings__maps_E");
   E069 : Short_Integer; pragma Import (Ada, E069, "ada__strings__maps__constants_E");
   E054 : Short_Integer; pragma Import (Ada, E054, "interfaces__c_E");
   E024 : Short_Integer; pragma Import (Ada, E024, "system__exceptions_E");
   E091 : Short_Integer; pragma Import (Ada, E091, "system__object_reader_E");
   E059 : Short_Integer; pragma Import (Ada, E059, "system__dwarf_lines_E");
   E019 : Short_Integer; pragma Import (Ada, E019, "system__soft_links__initialize_E");
   E048 : Short_Integer; pragma Import (Ada, E048, "system__traceback__symbolic_E");
   E029 : Short_Integer; pragma Import (Ada, E029, "system__img_int_E");
   E072 : Short_Integer; pragma Import (Ada, E072, "system__img_uns_E");
   E107 : Short_Integer; pragma Import (Ada, E107, "ada__strings__utf_encoding_E");
   E115 : Short_Integer; pragma Import (Ada, E115, "ada__tags_E");
   E105 : Short_Integer; pragma Import (Ada, E105, "ada__strings__text_buffers_E");
   E197 : Short_Integer; pragma Import (Ada, E197, "gnat_E");
   E123 : Short_Integer; pragma Import (Ada, E123, "ada__streams_E");
   E135 : Short_Integer; pragma Import (Ada, E135, "system__file_control_block_E");
   E134 : Short_Integer; pragma Import (Ada, E134, "system__finalization_root_E");
   E132 : Short_Integer; pragma Import (Ada, E132, "ada__finalization_E");
   E131 : Short_Integer; pragma Import (Ada, E131, "system__file_io_E");
   E183 : Short_Integer; pragma Import (Ada, E183, "ada__strings__unbounded_E");
   E144 : Short_Integer; pragma Import (Ada, E144, "ada__calendar_E");
   E121 : Short_Integer; pragma Import (Ada, E121, "ada__text_io_E");
   E142 : Short_Integer; pragma Import (Ada, E142, "system__random_seed_E");
   E165 : Short_Integer; pragma Import (Ada, E165, "system__img_llu_E");
   E148 : Short_Integer; pragma Import (Ada, E148, "bar_codes_E");
   E152 : Short_Integer; pragma Import (Ada, E152, "bar_codes__encode_code_128_E");
   E155 : Short_Integer; pragma Import (Ada, E155, "bar_codes__encode_qr_E");
   E181 : Short_Integer; pragma Import (Ada, E181, "bar_codes__impl_E");
   E202 : Short_Integer; pragma Import (Ada, E202, "tb_wrap_E");

   Sec_Default_Sized_Stacks : array (1 .. 1) of aliased System.Secondary_Stack.SS_Stack (System.Parameters.Runtime_Default_Sec_Stack_Size);

   Local_Priority_Specific_Dispatching : constant String := "";
   Local_Interrupt_States : constant String := "";

   Is_Elaborated : Boolean := False;

   procedure finalize_library is
   begin
      E121 := E121 - 1;
      declare
         procedure F1;
         pragma Import (Ada, F1, "ada__text_io__finalize_spec");
      begin
         F1;
      end;
      E183 := E183 - 1;
      declare
         procedure F2;
         pragma Import (Ada, F2, "ada__strings__unbounded__finalize_spec");
      begin
         F2;
      end;
      declare
         procedure F3;
         pragma Import (Ada, F3, "system__file_io__finalize_body");
      begin
         E131 := E131 - 1;
         F3;
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
      Detect_Blocking := 0;
      Default_Stack_Size := -1;

      ada_main'Elab_Body;
      Default_Secondary_Stack_Size := System.Parameters.Runtime_Default_Sec_Stack_Size;
      Binder_Sec_Stacks_Count := 1;
      Default_Sized_SS_Pool := Sec_Default_Sized_Stacks'Address;

      Runtime_Initialize (1);

      System.Scalar_Values.Initialize ('I', 'N');

      Finalize_Library_Objects := finalize_library'access;

      Ada.Exceptions'Elab_Spec;
      System.Soft_Links'Elab_Spec;
      System.Exception_Table'Elab_Body;
      E023 := E023 + 1;
      Ada.Containers'Elab_Spec;
      E049 := E049 + 1;
      Ada.Io_Exceptions'Elab_Spec;
      E077 := E077 + 1;
      Ada.Numerics'Elab_Spec;
      E030 := E030 + 1;
      Ada.Strings'Elab_Spec;
      E064 := E064 + 1;
      Ada.Strings.Maps'Elab_Spec;
      E066 := E066 + 1;
      Ada.Strings.Maps.Constants'Elab_Spec;
      E069 := E069 + 1;
      Interfaces.C'Elab_Spec;
      E054 := E054 + 1;
      System.Exceptions'Elab_Spec;
      E024 := E024 + 1;
      System.Object_Reader'Elab_Spec;
      E091 := E091 + 1;
      System.Dwarf_Lines'Elab_Spec;
      System.Os_Lib'Elab_Body;
      E082 := E082 + 1;
      System.Soft_Links.Initialize'Elab_Body;
      E019 := E019 + 1;
      E012 := E012 + 1;
      System.Traceback.Symbolic'Elab_Body;
      E048 := E048 + 1;
      System.Img_Int'Elab_Spec;
      E029 := E029 + 1;
      E007 := E007 + 1;
      System.Img_Uns'Elab_Spec;
      E072 := E072 + 1;
      E059 := E059 + 1;
      Ada.Strings.Utf_Encoding'Elab_Spec;
      E107 := E107 + 1;
      Ada.Tags'Elab_Spec;
      Ada.Tags'Elab_Body;
      E115 := E115 + 1;
      Ada.Strings.Text_Buffers'Elab_Spec;
      E105 := E105 + 1;
      Gnat'Elab_Spec;
      E197 := E197 + 1;
      Ada.Streams'Elab_Spec;
      E123 := E123 + 1;
      System.File_Control_Block'Elab_Spec;
      E135 := E135 + 1;
      System.Finalization_Root'Elab_Spec;
      E134 := E134 + 1;
      Ada.Finalization'Elab_Spec;
      E132 := E132 + 1;
      System.File_Io'Elab_Body;
      E131 := E131 + 1;
      Ada.Strings.Unbounded'Elab_Spec;
      E183 := E183 + 1;
      Ada.Calendar'Elab_Spec;
      Ada.Calendar'Elab_Body;
      E144 := E144 + 1;
      Ada.Text_Io'Elab_Spec;
      Ada.Text_Io'Elab_Body;
      E121 := E121 + 1;
      System.Random_Seed'Elab_Body;
      E142 := E142 + 1;
      System.Img_Llu'Elab_Spec;
      E165 := E165 + 1;
      Bar_Codes'Elab_Spec;
      E152 := E152 + 1;
      Bar_Codes.Encode_Qr'Elab_Body;
      E155 := E155 + 1;
      Bar_Codes'Elab_Body;
      E148 := E148 + 1;
      E181 := E181 + 1;
      E202 := E202 + 1;
   end adainit;

   procedure Ada_Main_Program;
   pragma Import (Ada, Ada_Main_Program, "_ada_gnat_test");

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
   --   /workspaces/bench-source/src/bar_codes/obj_debug/bar_codes-encode_code_128.o
   --   /workspaces/bench-source/src/bar_codes/obj_debug/bar_codes-encode_qr.o
   --   /workspaces/bench-source/src/bar_codes/obj_debug/bar_codes.o
   --   /workspaces/bench-source/src/bar_codes/obj_debug/bar_codes-impl.o
   --   /workspaces/bench-source/src/bar_codes/obj_debug/bar_codes_test.o
   --   /workspaces/bench-source/src/bar_codes/obj_debug/tb_wrap.o
   --   /workspaces/bench-source/src/bar_codes/obj_debug/gnat_test.o
   --   -L/workspaces/bench-source/src/bar_codes/obj_debug/
   --   -L/workspaces/bench-source/src/bar_codes/obj_debug/
   --   -L/usr/gnat/lib/gcc/x86_64-pc-linux-gnu/12.2.1/adalib/
   --   -static
   --   -lgnat
   --   -ldl
--  END Object file/option list   

end ada_main;
