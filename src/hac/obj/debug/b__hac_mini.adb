pragma Warnings (Off);
pragma Ada_95;
pragma Source_File_Name (ada_main, Spec_File_Name => "b__hac_mini.ads");
pragma Source_File_Name (ada_main, Body_File_Name => "b__hac_mini.adb");
pragma Suppress (Overflow_Check);
with Ada.Exceptions;

package body ada_main is

   E083 : Short_Integer; pragma Import (Ada, E083, "system__os_lib_E");
   E011 : Short_Integer; pragma Import (Ada, E011, "ada__exceptions_E");
   E015 : Short_Integer; pragma Import (Ada, E015, "system__soft_links_E");
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
   E017 : Short_Integer; pragma Import (Ada, E017, "system__soft_links__initialize_E");
   E049 : Short_Integer; pragma Import (Ada, E049, "system__traceback__symbolic_E");
   E030 : Short_Integer; pragma Import (Ada, E030, "system__img_int_E");
   E073 : Short_Integer; pragma Import (Ada, E073, "system__img_uns_E");
   E219 : Short_Integer; pragma Import (Ada, E219, "ada__assertions_E");
   E108 : Short_Integer; pragma Import (Ada, E108, "ada__strings__utf_encoding_E");
   E116 : Short_Integer; pragma Import (Ada, E116, "ada__tags_E");
   E106 : Short_Integer; pragma Import (Ada, E106, "ada__strings__text_buffers_E");
   E268 : Short_Integer; pragma Import (Ada, E268, "interfaces__c__strings_E");
   E124 : Short_Integer; pragma Import (Ada, E124, "ada__streams_E");
   E136 : Short_Integer; pragma Import (Ada, E136, "system__file_control_block_E");
   E135 : Short_Integer; pragma Import (Ada, E135, "system__finalization_root_E");
   E133 : Short_Integer; pragma Import (Ada, E133, "ada__finalization_E");
   E132 : Short_Integer; pragma Import (Ada, E132, "system__file_io_E");
   E141 : Short_Integer; pragma Import (Ada, E141, "ada__streams__stream_io_E");
   E232 : Short_Integer; pragma Import (Ada, E232, "system__storage_pools_E");
   E230 : Short_Integer; pragma Import (Ada, E230, "system__finalization_masters_E");
   E249 : Short_Integer; pragma Import (Ada, E249, "system__storage_pools__subpools_E");
   E199 : Short_Integer; pragma Import (Ada, E199, "ada__strings__unbounded_E");
   E195 : Short_Integer; pragma Import (Ada, E195, "ada__calendar_E");
   E343 : Short_Integer; pragma Import (Ada, E343, "ada__calendar__delays_E");
   E122 : Short_Integer; pragma Import (Ada, E122, "ada__text_io_E");
   E215 : Short_Integer; pragma Import (Ada, E215, "ada__text_io__text_streams_E");
   E245 : Short_Integer; pragma Import (Ada, E245, "system__pool_global_E");
   E193 : Short_Integer; pragma Import (Ada, E193, "system__random_seed_E");
   E264 : Short_Integer; pragma Import (Ada, E264, "system__regexp_E");
   E256 : Short_Integer; pragma Import (Ada, E256, "ada__directories_E");
   E155 : Short_Integer; pragma Import (Ada, E155, "system__img_llli_E");
   E152 : Short_Integer; pragma Import (Ada, E152, "system__img_lli_E");
   E242 : Short_Integer; pragma Import (Ada, E242, "system__img_llu_E");
   E175 : Short_Integer; pragma Import (Ada, E175, "hat_E");
   E173 : Short_Integer; pragma Import (Ada, E173, "hac_sys__defs_E");
   E325 : Short_Integer; pragma Import (Ada, E325, "hac_sys__multi_precision_integers_E");
   E290 : Short_Integer; pragma Import (Ada, E290, "hac_sys__pcode_E");
   E284 : Short_Integer; pragma Import (Ada, E284, "hac_sys__co_defs_E");
   E171 : Short_Integer; pragma Import (Ada, E171, "hac_sys__errors_E");
   E299 : Short_Integer; pragma Import (Ada, E299, "hac_sys__scanner_E");
   E337 : Short_Integer; pragma Import (Ada, E337, "hac_sys__librarian_E");
   E145 : Short_Integer; pragma Import (Ada, E145, "hac_sys__compiler_E");
   E169 : Short_Integer; pragma Import (Ada, E169, "hac_sys__compiler__pcode_emit_E");
   E339 : Short_Integer; pragma Import (Ada, E339, "hac_sys__librarian__built_in_packages_E");
   E293 : Short_Integer; pragma Import (Ada, E293, "hac_sys__parser_E");
   E306 : Short_Integer; pragma Import (Ada, E306, "hac_sys__parser__attributes_E");
   E317 : Short_Integer; pragma Import (Ada, E317, "hac_sys__parser__calls_E");
   E295 : Short_Integer; pragma Import (Ada, E295, "hac_sys__parser__const_var_E");
   E297 : Short_Integer; pragma Import (Ada, E297, "hac_sys__parser__enter_def_E");
   E304 : Short_Integer; pragma Import (Ada, E304, "hac_sys__parser__expressions_E");
   E308 : Short_Integer; pragma Import (Ada, E308, "hac_sys__parser__helpers_E");
   E335 : Short_Integer; pragma Import (Ada, E335, "hac_sys__parser__modularity_E");
   E331 : Short_Integer; pragma Import (Ada, E331, "hac_sys__parser__packages_E");
   E314 : Short_Integer; pragma Import (Ada, E314, "hac_sys__parser__ranges_E");
   E319 : Short_Integer; pragma Import (Ada, E319, "hac_sys__parser__standard_functions_E");
   E327 : Short_Integer; pragma Import (Ada, E327, "hac_sys__parser__standard_procedures_E");
   E323 : Short_Integer; pragma Import (Ada, E323, "hac_sys__parser__statements_E");
   E333 : Short_Integer; pragma Import (Ada, E333, "hac_sys__parser__tasking_E");
   E321 : Short_Integer; pragma Import (Ada, E321, "hac_sys__parser__type_conversion_E");
   E329 : Short_Integer; pragma Import (Ada, E329, "hac_sys__parser__type_def_E");
   E139 : Short_Integer; pragma Import (Ada, E139, "hac_sys__builder_E");
   E341 : Short_Integer; pragma Import (Ada, E341, "hac_sys__pcode__interpreter_E");
   E347 : Short_Integer; pragma Import (Ada, E347, "hac_sys__pcode__interpreter__in_defs_E");
   E345 : Short_Integer; pragma Import (Ada, E345, "hac_sys__interfacing_E");
   E349 : Short_Integer; pragma Import (Ada, E349, "hac_sys__pcode__interpreter__calls_E");
   E355 : Short_Integer; pragma Import (Ada, E355, "hac_sys__pcode__interpreter__composite_data_E");
   E351 : Short_Integer; pragma Import (Ada, E351, "hac_sys__pcode__interpreter__exceptions_E");
   E357 : Short_Integer; pragma Import (Ada, E357, "hac_sys__pcode__interpreter__multi_statement_E");
   E359 : Short_Integer; pragma Import (Ada, E359, "hac_sys__pcode__interpreter__operators_E");
   E353 : Short_Integer; pragma Import (Ada, E353, "hac_sys__pcode__interpreter__tasking_E");

   Sec_Default_Sized_Stacks : array (1 .. 1) of aliased System.Secondary_Stack.SS_Stack (System.Parameters.Runtime_Default_Sec_Stack_Size);

   Local_Priority_Specific_Dispatching : constant String := "";
   Local_Interrupt_States : constant String := "";

   Is_Elaborated : Boolean := False;

   procedure finalize_library is
   begin
      E341 := E341 - 1;
      E347 := E347 - 1;
      declare
         procedure F1;
         pragma Import (Ada, F1, "hac_sys__pcode__interpreter__in_defs__finalize_spec");
      begin
         F1;
      end;
      declare
         procedure F2;
         pragma Import (Ada, F2, "hac_sys__pcode__interpreter__finalize_spec");
      begin
         F2;
      end;
      E139 := E139 - 1;
      declare
         procedure F3;
         pragma Import (Ada, F3, "hac_sys__builder__finalize_spec");
      begin
         F3;
      end;
      declare
         procedure F4;
         pragma Import (Ada, F4, "hac_sys__librarian__finalize_body");
      begin
         E337 := E337 - 1;
         F4;
      end;
      declare
         procedure F5;
         pragma Import (Ada, F5, "hac_sys__librarian__finalize_spec");
      begin
         F5;
      end;
      E284 := E284 - 1;
      declare
         procedure F6;
         pragma Import (Ada, F6, "hac_sys__co_defs__finalize_spec");
      begin
         F6;
      end;
      E173 := E173 - 1;
      declare
         procedure F7;
         pragma Import (Ada, F7, "hac_sys__defs__finalize_spec");
      begin
         F7;
      end;
      declare
         procedure F8;
         pragma Import (Ada, F8, "hat__finalize_body");
      begin
         E175 := E175 - 1;
         F8;
      end;
      declare
         procedure F9;
         pragma Import (Ada, F9, "ada__directories__finalize_body");
      begin
         E256 := E256 - 1;
         F9;
      end;
      declare
         procedure F10;
         pragma Import (Ada, F10, "ada__directories__finalize_spec");
      begin
         F10;
      end;
      E264 := E264 - 1;
      declare
         procedure F11;
         pragma Import (Ada, F11, "system__regexp__finalize_spec");
      begin
         F11;
      end;
      E245 := E245 - 1;
      declare
         procedure F12;
         pragma Import (Ada, F12, "system__pool_global__finalize_spec");
      begin
         F12;
      end;
      E122 := E122 - 1;
      declare
         procedure F13;
         pragma Import (Ada, F13, "ada__text_io__finalize_spec");
      begin
         F13;
      end;
      E199 := E199 - 1;
      declare
         procedure F14;
         pragma Import (Ada, F14, "ada__strings__unbounded__finalize_spec");
      begin
         F14;
      end;
      E249 := E249 - 1;
      declare
         procedure F15;
         pragma Import (Ada, F15, "system__storage_pools__subpools__finalize_spec");
      begin
         F15;
      end;
      E230 := E230 - 1;
      declare
         procedure F16;
         pragma Import (Ada, F16, "system__finalization_masters__finalize_spec");
      begin
         F16;
      end;
      E141 := E141 - 1;
      declare
         procedure F17;
         pragma Import (Ada, F17, "ada__streams__stream_io__finalize_spec");
      begin
         F17;
      end;
      declare
         procedure F18;
         pragma Import (Ada, F18, "system__file_io__finalize_body");
      begin
         E132 := E132 - 1;
         F18;
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
      E017 := E017 + 1;
      E015 := E015 + 1;
      System.Traceback.Symbolic'Elab_Body;
      E049 := E049 + 1;
      System.Img_Int'Elab_Spec;
      E030 := E030 + 1;
      E011 := E011 + 1;
      System.Img_Uns'Elab_Spec;
      E073 := E073 + 1;
      E060 := E060 + 1;
      Ada.Assertions'Elab_Spec;
      E219 := E219 + 1;
      Ada.Strings.Utf_Encoding'Elab_Spec;
      E108 := E108 + 1;
      Ada.Tags'Elab_Spec;
      Ada.Tags'Elab_Body;
      E116 := E116 + 1;
      Ada.Strings.Text_Buffers'Elab_Spec;
      E106 := E106 + 1;
      Interfaces.C.Strings'Elab_Spec;
      E268 := E268 + 1;
      Ada.Streams'Elab_Spec;
      E124 := E124 + 1;
      System.File_Control_Block'Elab_Spec;
      E136 := E136 + 1;
      System.Finalization_Root'Elab_Spec;
      E135 := E135 + 1;
      Ada.Finalization'Elab_Spec;
      E133 := E133 + 1;
      System.File_Io'Elab_Body;
      E132 := E132 + 1;
      Ada.Streams.Stream_Io'Elab_Spec;
      E141 := E141 + 1;
      System.Storage_Pools'Elab_Spec;
      E232 := E232 + 1;
      System.Finalization_Masters'Elab_Spec;
      System.Finalization_Masters'Elab_Body;
      E230 := E230 + 1;
      System.Storage_Pools.Subpools'Elab_Spec;
      E249 := E249 + 1;
      Ada.Strings.Unbounded'Elab_Spec;
      E199 := E199 + 1;
      Ada.Calendar'Elab_Spec;
      Ada.Calendar'Elab_Body;
      E195 := E195 + 1;
      Ada.Calendar.Delays'Elab_Body;
      E343 := E343 + 1;
      Ada.Text_Io'Elab_Spec;
      Ada.Text_Io'Elab_Body;
      E122 := E122 + 1;
      Ada.Text_Io.Text_Streams'Elab_Spec;
      E215 := E215 + 1;
      System.Pool_Global'Elab_Spec;
      System.Pool_Global'Elab_Body;
      E245 := E245 + 1;
      System.Random_Seed'Elab_Body;
      E193 := E193 + 1;
      System.Regexp'Elab_Spec;
      System.Regexp'Elab_Body;
      E264 := E264 + 1;
      Ada.Directories'Elab_Spec;
      Ada.Directories'Elab_Body;
      E256 := E256 + 1;
      System.Img_Llli'Elab_Spec;
      E155 := E155 + 1;
      System.Img_Lli'Elab_Spec;
      E152 := E152 + 1;
      System.Img_Llu'Elab_Spec;
      E242 := E242 + 1;
      HAT'ELAB_BODY;
      E175 := E175 + 1;
      HAC_SYS.DEFS'ELAB_SPEC;
      E173 := E173 + 1;
      Hac_Sys.Multi_Precision_Integers'Elab_Spec;
      Hac_Sys.Multi_Precision_Integers'Elab_Body;
      E325 := E325 + 1;
      Hac_Sys.Co_Defs'Elab_Spec;
      Hac_Sys.Co_Defs'Elab_Body;
      E284 := E284 + 1;
      Hac_Sys.Errors'Elab_Spec;
      Hac_Sys.Errors'Elab_Body;
      E171 := E171 + 1;
      E290 := E290 + 1;
      Hac_Sys.Scanner'Elab_Body;
      E299 := E299 + 1;
      Hac_Sys.Librarian'Elab_Spec;
      E169 := E169 + 1;
      E297 := E297 + 1;
      Hac_Sys.Parser.Helpers'Elab_Spec;
      Hac_Sys.Parser.Helpers'Elab_Body;
      E308 := E308 + 1;
      E317 := E317 + 1;
      E145 := E145 + 1;
      Hac_Sys.Librarian'Elab_Body;
      E337 := E337 + 1;
      E339 := E339 + 1;
      E335 := E335 + 1;
      E314 := E314 + 1;
      E306 := E306 + 1;
      E319 := E319 + 1;
      Hac_Sys.Parser.Standard_Procedures'Elab_Body;
      E327 := E327 + 1;
      E323 := E323 + 1;
      E333 := E333 + 1;
      E321 := E321 + 1;
      Hac_Sys.Parser.Expressions'Elab_Body;
      E304 := E304 + 1;
      E329 := E329 + 1;
      E293 := E293 + 1;
      E295 := E295 + 1;
      E331 := E331 + 1;
      Hac_Sys.Builder'Elab_Spec;
      Hac_Sys.Builder'Elab_Body;
      E139 := E139 + 1;
      Hac_Sys.Pcode.Interpreter'Elab_Spec;
      Hac_Sys.Pcode.Interpreter.In_Defs'Elab_Spec;
      E347 := E347 + 1;
      Hac_Sys.Interfacing'Elab_Spec;
      E345 := E345 + 1;
      E355 := E355 + 1;
      E351 := E351 + 1;
      E357 := E357 + 1;
      E359 := E359 + 1;
      E353 := E353 + 1;
      E341 := E341 + 1;
      E349 := E349 + 1;
   end adainit;

   procedure Ada_Main_Program;
   pragma Import (Ada, Ada_Main_Program, "_ada_hac_mini");

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
   --   /workspaces/bench-source/src/hac/obj/debug/hac_sys.o
   --   /workspaces/bench-source/src/hac/obj/debug/hat.o
   --   /workspaces/bench-source/src/hac/obj/debug/hac_sys-defs.o
   --   /workspaces/bench-source/src/hac/obj/debug/hac_sys-multi_precision_integers.o
   --   /workspaces/bench-source/src/hac/obj/debug/hac_sys-co_defs.o
   --   /workspaces/bench-source/src/hac/obj/debug/hac_sys-errors.o
   --   /workspaces/bench-source/src/hac/obj/debug/hac_sys-pcode.o
   --   /workspaces/bench-source/src/hac/obj/debug/hac_sys-scanner.o
   --   /workspaces/bench-source/src/hac/obj/debug/hac_sys-compiler-pcode_emit.o
   --   /workspaces/bench-source/src/hac/obj/debug/hac_sys-parser-enter_def.o
   --   /workspaces/bench-source/src/hac/obj/debug/hac_sys-parser-helpers.o
   --   /workspaces/bench-source/src/hac/obj/debug/hac_sys-parser-calls.o
   --   /workspaces/bench-source/src/hac/obj/debug/hac_sys-compiler.o
   --   /workspaces/bench-source/src/hac/obj/debug/hac_sys-librarian.o
   --   /workspaces/bench-source/src/hac/obj/debug/hac_sys-librarian-built_in_packages.o
   --   /workspaces/bench-source/src/hac/obj/debug/hac_sys-parser-modularity.o
   --   /workspaces/bench-source/src/hac/obj/debug/hac_sys-parser-ranges.o
   --   /workspaces/bench-source/src/hac/obj/debug/hac_sys-parser-attributes.o
   --   /workspaces/bench-source/src/hac/obj/debug/hac_sys-parser-standard_functions.o
   --   /workspaces/bench-source/src/hac/obj/debug/hac_sys-parser-standard_procedures.o
   --   /workspaces/bench-source/src/hac/obj/debug/hac_sys-parser-statements.o
   --   /workspaces/bench-source/src/hac/obj/debug/hac_sys-parser-tasking.o
   --   /workspaces/bench-source/src/hac/obj/debug/hac_sys-parser-type_conversion.o
   --   /workspaces/bench-source/src/hac/obj/debug/hac_sys-parser-expressions.o
   --   /workspaces/bench-source/src/hac/obj/debug/hac_sys-parser-type_def.o
   --   /workspaces/bench-source/src/hac/obj/debug/hac_sys-parser.o
   --   /workspaces/bench-source/src/hac/obj/debug/hac_sys-parser-const_var.o
   --   /workspaces/bench-source/src/hac/obj/debug/hac_sys-parser-packages.o
   --   /workspaces/bench-source/src/hac/obj/debug/hac_sys-builder.o
   --   /workspaces/bench-source/src/hac/obj/debug/hac_sys-pcode-interpreter-in_defs.o
   --   /workspaces/bench-source/src/hac/obj/debug/hac_sys-interfacing.o
   --   /workspaces/bench-source/src/hac/obj/debug/hac_sys-pcode-interpreter-composite_data.o
   --   /workspaces/bench-source/src/hac/obj/debug/hac_sys-pcode-interpreter-exceptions.o
   --   /workspaces/bench-source/src/hac/obj/debug/hac_sys-pcode-interpreter-multi_statement.o
   --   /workspaces/bench-source/src/hac/obj/debug/hac_sys-pcode-interpreter-operators.o
   --   /workspaces/bench-source/src/hac/obj/debug/hac_sys-pcode-interpreter-tasking.o
   --   /workspaces/bench-source/src/hac/obj/debug/hac_sys-pcode-interpreter.o
   --   /workspaces/bench-source/src/hac/obj/debug/hac_sys-pcode-interpreter-calls.o
   --   /workspaces/bench-source/src/hac/obj/debug/hac_mini.o
   --   -L/workspaces/bench-source/src/hac/obj/debug/
   --   -L/workspaces/bench-source/src/hac/obj/debug/
   --   -L/usr/gnat/lib/gcc/x86_64-pc-linux-gnu/12.2.1/adalib/
   --   -static
   --   -lgnat
   --   -lm
   --   -ldl
--  END Object file/option list   

end ada_main;
