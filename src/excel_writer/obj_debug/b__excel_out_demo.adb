pragma Warnings (Off);
pragma Ada_95;
pragma Source_File_Name (ada_main, Spec_File_Name => "b__excel_out_demo.ads");
pragma Source_File_Name (ada_main, Body_File_Name => "b__excel_out_demo.adb");
pragma Suppress (Overflow_Check);
with Ada.Exceptions;

package body ada_main is

   E083 : Short_Integer; pragma Import (Ada, E083, "system__os_lib_E");
   E008 : Short_Integer; pragma Import (Ada, E008, "ada__exceptions_E");
   E013 : Short_Integer; pragma Import (Ada, E013, "system__soft_links_E");
   E024 : Short_Integer; pragma Import (Ada, E024, "system__exception_table_E");
   E050 : Short_Integer; pragma Import (Ada, E050, "ada__containers_E");
   E078 : Short_Integer; pragma Import (Ada, E078, "ada__io_exceptions_E");
   E031 : Short_Integer; pragma Import (Ada, E031, "ada__numerics_E");
   E065 : Short_Integer; pragma Import (Ada, E065, "ada__strings_E");
   E067 : Short_Integer; pragma Import (Ada, E067, "ada__strings__maps_E");
   E070 : Short_Integer; pragma Import (Ada, E070, "ada__strings__maps__constants_E");
   E055 : Short_Integer; pragma Import (Ada, E055, "interfaces__c_E");
   E025 : Short_Integer; pragma Import (Ada, E025, "system__exceptions_E");
   E092 : Short_Integer; pragma Import (Ada, E092, "system__object_reader_E");
   E060 : Short_Integer; pragma Import (Ada, E060, "system__dwarf_lines_E");
   E020 : Short_Integer; pragma Import (Ada, E020, "system__soft_links__initialize_E");
   E049 : Short_Integer; pragma Import (Ada, E049, "system__traceback__symbolic_E");
   E030 : Short_Integer; pragma Import (Ada, E030, "system__img_int_E");
   E073 : Short_Integer; pragma Import (Ada, E073, "system__img_uns_E");
   E122 : Short_Integer; pragma Import (Ada, E122, "ada__strings__utf_encoding_E");
   E112 : Short_Integer; pragma Import (Ada, E112, "ada__tags_E");
   E120 : Short_Integer; pragma Import (Ada, E120, "ada__strings__text_buffers_E");
   E130 : Short_Integer; pragma Import (Ada, E130, "ada__streams_E");
   E146 : Short_Integer; pragma Import (Ada, E146, "system__file_control_block_E");
   E145 : Short_Integer; pragma Import (Ada, E145, "system__finalization_root_E");
   E143 : Short_Integer; pragma Import (Ada, E143, "ada__finalization_E");
   E142 : Short_Integer; pragma Import (Ada, E142, "system__file_io_E");
   E136 : Short_Integer; pragma Import (Ada, E136, "ada__streams__stream_io_E");
   E174 : Short_Integer; pragma Import (Ada, E174, "system__storage_pools_E");
   E178 : Short_Integer; pragma Import (Ada, E178, "system__finalization_masters_E");
   E176 : Short_Integer; pragma Import (Ada, E176, "system__storage_pools__subpools_E");
   E188 : Short_Integer; pragma Import (Ada, E188, "ada__strings__unbounded_E");
   E006 : Short_Integer; pragma Import (Ada, E006, "ada__calendar_E");
   E148 : Short_Integer; pragma Import (Ada, E148, "ada__text_io_E");
   E197 : Short_Integer; pragma Import (Ada, E197, "system__pool_global_E");
   E118 : Short_Integer; pragma Import (Ada, E118, "system__random_seed_E");
   E166 : Short_Integer; pragma Import (Ada, E166, "system__img_llli_E");
   E163 : Short_Integer; pragma Import (Ada, E163, "system__img_lli_E");
   E157 : Short_Integer; pragma Import (Ada, E157, "ieee_754_E");
   E150 : Short_Integer; pragma Import (Ada, E150, "excel_out_E");

   Sec_Default_Sized_Stacks : array (1 .. 1) of aliased System.Secondary_Stack.SS_Stack (System.Parameters.Runtime_Default_Sec_Stack_Size);

   Local_Priority_Specific_Dispatching : constant String := "";
   Local_Interrupt_States : constant String := "";

   Is_Elaborated : Boolean := False;

   procedure finalize_library is
   begin
      E150 := E150 - 1;
      declare
         procedure F1;
         pragma Import (Ada, F1, "excel_out__finalize_spec");
      begin
         F1;
      end;
      E197 := E197 - 1;
      declare
         procedure F2;
         pragma Import (Ada, F2, "system__pool_global__finalize_spec");
      begin
         F2;
      end;
      E148 := E148 - 1;
      declare
         procedure F3;
         pragma Import (Ada, F3, "ada__text_io__finalize_spec");
      begin
         F3;
      end;
      E188 := E188 - 1;
      declare
         procedure F4;
         pragma Import (Ada, F4, "ada__strings__unbounded__finalize_spec");
      begin
         F4;
      end;
      E176 := E176 - 1;
      declare
         procedure F5;
         pragma Import (Ada, F5, "system__storage_pools__subpools__finalize_spec");
      begin
         F5;
      end;
      E178 := E178 - 1;
      declare
         procedure F6;
         pragma Import (Ada, F6, "system__finalization_masters__finalize_spec");
      begin
         F6;
      end;
      E136 := E136 - 1;
      declare
         procedure F7;
         pragma Import (Ada, F7, "ada__streams__stream_io__finalize_spec");
      begin
         F7;
      end;
      declare
         procedure F8;
         pragma Import (Ada, F8, "system__file_io__finalize_body");
      begin
         E142 := E142 - 1;
         F8;
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
      E024 := E024 + 1;
      Ada.Containers'Elab_Spec;
      E050 := E050 + 1;
      Ada.Io_Exceptions'Elab_Spec;
      E078 := E078 + 1;
      Ada.Numerics'Elab_Spec;
      E031 := E031 + 1;
      Ada.Strings'Elab_Spec;
      E065 := E065 + 1;
      Ada.Strings.Maps'Elab_Spec;
      E067 := E067 + 1;
      Ada.Strings.Maps.Constants'Elab_Spec;
      E070 := E070 + 1;
      Interfaces.C'Elab_Spec;
      E055 := E055 + 1;
      System.Exceptions'Elab_Spec;
      E025 := E025 + 1;
      System.Object_Reader'Elab_Spec;
      E092 := E092 + 1;
      System.Dwarf_Lines'Elab_Spec;
      System.Os_Lib'Elab_Body;
      E083 := E083 + 1;
      System.Soft_Links.Initialize'Elab_Body;
      E020 := E020 + 1;
      E013 := E013 + 1;
      System.Traceback.Symbolic'Elab_Body;
      E049 := E049 + 1;
      System.Img_Int'Elab_Spec;
      E030 := E030 + 1;
      E008 := E008 + 1;
      System.Img_Uns'Elab_Spec;
      E073 := E073 + 1;
      E060 := E060 + 1;
      Ada.Strings.Utf_Encoding'Elab_Spec;
      E122 := E122 + 1;
      Ada.Tags'Elab_Spec;
      Ada.Tags'Elab_Body;
      E112 := E112 + 1;
      Ada.Strings.Text_Buffers'Elab_Spec;
      E120 := E120 + 1;
      Ada.Streams'Elab_Spec;
      E130 := E130 + 1;
      System.File_Control_Block'Elab_Spec;
      E146 := E146 + 1;
      System.Finalization_Root'Elab_Spec;
      E145 := E145 + 1;
      Ada.Finalization'Elab_Spec;
      E143 := E143 + 1;
      System.File_Io'Elab_Body;
      E142 := E142 + 1;
      Ada.Streams.Stream_Io'Elab_Spec;
      E136 := E136 + 1;
      System.Storage_Pools'Elab_Spec;
      E174 := E174 + 1;
      System.Finalization_Masters'Elab_Spec;
      System.Finalization_Masters'Elab_Body;
      E178 := E178 + 1;
      System.Storage_Pools.Subpools'Elab_Spec;
      E176 := E176 + 1;
      Ada.Strings.Unbounded'Elab_Spec;
      E188 := E188 + 1;
      Ada.Calendar'Elab_Spec;
      Ada.Calendar'Elab_Body;
      E006 := E006 + 1;
      Ada.Text_Io'Elab_Spec;
      Ada.Text_Io'Elab_Body;
      E148 := E148 + 1;
      System.Pool_Global'Elab_Spec;
      System.Pool_Global'Elab_Body;
      E197 := E197 + 1;
      System.Random_Seed'Elab_Body;
      E118 := E118 + 1;
      System.Img_Llli'Elab_Spec;
      E166 := E166 + 1;
      System.Img_Lli'Elab_Spec;
      E163 := E163 + 1;
      IEEE_754'ELAB_SPEC;
      E157 := E157 + 1;
      Excel_Out'Elab_Spec;
      Excel_Out'Elab_Body;
      E150 := E150 + 1;
   end adainit;

   procedure Ada_Main_Program;
   pragma Import (Ada, Ada_Main_Program, "_ada_excel_out_demo");

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
   --   /workspaces/bench-source/src/excel_writer/obj_debug/ieee_754.o
   --   /workspaces/bench-source/src/excel_writer/obj_debug/ieee_754-generic_double_precision.o
   --   /workspaces/bench-source/src/excel_writer/obj_debug/excel_out.o
   --   /workspaces/bench-source/src/excel_writer/obj_debug/excel_out_demo.o
   --   -L/workspaces/bench-source/src/excel_writer/obj_debug/
   --   -L/workspaces/bench-source/src/excel_writer/obj_debug/
   --   -L/usr/gnat/lib/gcc/x86_64-pc-linux-gnu/12.2.1/adalib/
   --   -static
   --   -lgnat
   --   -ldl
--  END Object file/option list   

end ada_main;