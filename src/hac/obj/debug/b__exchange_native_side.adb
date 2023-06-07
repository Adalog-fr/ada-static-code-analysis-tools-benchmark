pragma Warnings (Off);
pragma Ada_95;
pragma Source_File_Name (ada_main, Spec_File_Name => "b__exchange_native_side.ads");
pragma Source_File_Name (ada_main, Body_File_Name => "b__exchange_native_side.adb");
pragma Suppress (Overflow_Check);
with Ada.Exceptions;

package body ada_main is

   E085 : Short_Integer; pragma Import (Ada, E085, "system__os_lib_E");
   E010 : Short_Integer; pragma Import (Ada, E010, "ada__exceptions_E");
   E015 : Short_Integer; pragma Import (Ada, E015, "system__soft_links_E");
   E026 : Short_Integer; pragma Import (Ada, E026, "system__exception_table_E");
   E052 : Short_Integer; pragma Import (Ada, E052, "ada__containers_E");
   E080 : Short_Integer; pragma Import (Ada, E080, "ada__io_exceptions_E");
   E033 : Short_Integer; pragma Import (Ada, E033, "ada__numerics_E");
   E067 : Short_Integer; pragma Import (Ada, E067, "ada__strings_E");
   E069 : Short_Integer; pragma Import (Ada, E069, "ada__strings__maps_E");
   E072 : Short_Integer; pragma Import (Ada, E072, "ada__strings__maps__constants_E");
   E057 : Short_Integer; pragma Import (Ada, E057, "interfaces__c_E");
   E027 : Short_Integer; pragma Import (Ada, E027, "system__exceptions_E");
   E094 : Short_Integer; pragma Import (Ada, E094, "system__object_reader_E");
   E062 : Short_Integer; pragma Import (Ada, E062, "system__dwarf_lines_E");
   E022 : Short_Integer; pragma Import (Ada, E022, "system__soft_links__initialize_E");
   E051 : Short_Integer; pragma Import (Ada, E051, "system__traceback__symbolic_E");
   E032 : Short_Integer; pragma Import (Ada, E032, "system__img_int_E");
   E075 : Short_Integer; pragma Import (Ada, E075, "system__img_uns_E");
   E223 : Short_Integer; pragma Import (Ada, E223, "ada__assertions_E");
   E117 : Short_Integer; pragma Import (Ada, E117, "ada__strings__utf_encoding_E");
   E125 : Short_Integer; pragma Import (Ada, E125, "ada__tags_E");
   E115 : Short_Integer; pragma Import (Ada, E115, "ada__strings__text_buffers_E");
   E258 : Short_Integer; pragma Import (Ada, E258, "interfaces__c__strings_E");
   E113 : Short_Integer; pragma Import (Ada, E113, "ada__streams_E");
   E166 : Short_Integer; pragma Import (Ada, E166, "system__file_control_block_E");
   E135 : Short_Integer; pragma Import (Ada, E135, "system__finalization_root_E");
   E111 : Short_Integer; pragma Import (Ada, E111, "ada__finalization_E");
   E163 : Short_Integer; pragma Import (Ada, E163, "system__file_io_E");
   E213 : Short_Integer; pragma Import (Ada, E213, "ada__streams__stream_io_E");
   E170 : Short_Integer; pragma Import (Ada, E170, "system__storage_pools_E");
   E168 : Short_Integer; pragma Import (Ada, E168, "system__finalization_masters_E");
   E174 : Short_Integer; pragma Import (Ada, E174, "system__storage_pools__subpools_E");
   E147 : Short_Integer; pragma Import (Ada, E147, "ada__strings__unbounded_E");
   E008 : Short_Integer; pragma Import (Ada, E008, "ada__calendar_E");
   E354 : Short_Integer; pragma Import (Ada, E354, "ada__calendar__delays_E");
   E178 : Short_Integer; pragma Import (Ada, E178, "ada__text_io_E");
   E219 : Short_Integer; pragma Import (Ada, E219, "ada__text_io__text_streams_E");
   E247 : Short_Integer; pragma Import (Ada, E247, "system__pool_global_E");
   E211 : Short_Integer; pragma Import (Ada, E211, "system__random_seed_E");
   E172 : Short_Integer; pragma Import (Ada, E172, "system__regexp_E");
   E006 : Short_Integer; pragma Import (Ada, E006, "ada__directories_E");
   E268 : Short_Integer; pragma Import (Ada, E268, "system__img_llli_E");
   E245 : Short_Integer; pragma Import (Ada, E245, "system__img_lli_E");
   E242 : Short_Integer; pragma Import (Ada, E242, "system__img_llu_E");
   E197 : Short_Integer; pragma Import (Ada, E197, "hat_E");
   E195 : Short_Integer; pragma Import (Ada, E195, "hac_sys__defs_E");
   E336 : Short_Integer; pragma Import (Ada, E336, "hac_sys__multi_precision_integers_E");
   E301 : Short_Integer; pragma Import (Ada, E301, "hac_sys__pcode_E");
   E295 : Short_Integer; pragma Import (Ada, E295, "hac_sys__co_defs_E");
   E289 : Short_Integer; pragma Import (Ada, E289, "hac_sys__errors_E");
   E310 : Short_Integer; pragma Import (Ada, E310, "hac_sys__scanner_E");
   E348 : Short_Integer; pragma Import (Ada, E348, "hac_sys__librarian_E");
   E283 : Short_Integer; pragma Import (Ada, E283, "hac_sys__compiler_E");
   E287 : Short_Integer; pragma Import (Ada, E287, "hac_sys__compiler__pcode_emit_E");
   E350 : Short_Integer; pragma Import (Ada, E350, "hac_sys__librarian__built_in_packages_E");
   E304 : Short_Integer; pragma Import (Ada, E304, "hac_sys__parser_E");
   E317 : Short_Integer; pragma Import (Ada, E317, "hac_sys__parser__attributes_E");
   E328 : Short_Integer; pragma Import (Ada, E328, "hac_sys__parser__calls_E");
   E306 : Short_Integer; pragma Import (Ada, E306, "hac_sys__parser__const_var_E");
   E308 : Short_Integer; pragma Import (Ada, E308, "hac_sys__parser__enter_def_E");
   E315 : Short_Integer; pragma Import (Ada, E315, "hac_sys__parser__expressions_E");
   E319 : Short_Integer; pragma Import (Ada, E319, "hac_sys__parser__helpers_E");
   E346 : Short_Integer; pragma Import (Ada, E346, "hac_sys__parser__modularity_E");
   E342 : Short_Integer; pragma Import (Ada, E342, "hac_sys__parser__packages_E");
   E325 : Short_Integer; pragma Import (Ada, E325, "hac_sys__parser__ranges_E");
   E330 : Short_Integer; pragma Import (Ada, E330, "hac_sys__parser__standard_functions_E");
   E338 : Short_Integer; pragma Import (Ada, E338, "hac_sys__parser__standard_procedures_E");
   E334 : Short_Integer; pragma Import (Ada, E334, "hac_sys__parser__statements_E");
   E344 : Short_Integer; pragma Import (Ada, E344, "hac_sys__parser__tasking_E");
   E332 : Short_Integer; pragma Import (Ada, E332, "hac_sys__parser__type_conversion_E");
   E340 : Short_Integer; pragma Import (Ada, E340, "hac_sys__parser__type_def_E");
   E281 : Short_Integer; pragma Import (Ada, E281, "hac_sys__builder_E");
   E352 : Short_Integer; pragma Import (Ada, E352, "hac_sys__pcode__interpreter_E");
   E360 : Short_Integer; pragma Import (Ada, E360, "hac_sys__pcode__interpreter__in_defs_E");
   E193 : Short_Integer; pragma Import (Ada, E193, "hac_sys__interfacing_E");
   E356 : Short_Integer; pragma Import (Ada, E356, "hac_sys__pcode__interpreter__calls_E");
   E364 : Short_Integer; pragma Import (Ada, E364, "hac_sys__pcode__interpreter__composite_data_E");
   E358 : Short_Integer; pragma Import (Ada, E358, "hac_sys__pcode__interpreter__exceptions_E");
   E366 : Short_Integer; pragma Import (Ada, E366, "hac_sys__pcode__interpreter__multi_statement_E");
   E368 : Short_Integer; pragma Import (Ada, E368, "hac_sys__pcode__interpreter__operators_E");
   E362 : Short_Integer; pragma Import (Ada, E362, "hac_sys__pcode__interpreter__tasking_E");
   E180 : Short_Integer; pragma Import (Ada, E180, "exchange_native_side_pkg_E");

   Sec_Default_Sized_Stacks : array (1 .. 1) of aliased System.Secondary_Stack.SS_Stack (System.Parameters.Runtime_Default_Sec_Stack_Size);

   Local_Priority_Specific_Dispatching : constant String := "";
   Local_Interrupt_States : constant String := "";

   Is_Elaborated : Boolean := False;

   procedure finalize_library is
   begin
      E352 := E352 - 1;
      E360 := E360 - 1;
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
      E281 := E281 - 1;
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
         E348 := E348 - 1;
         F4;
      end;
      declare
         procedure F5;
         pragma Import (Ada, F5, "hac_sys__librarian__finalize_spec");
      begin
         F5;
      end;
      E295 := E295 - 1;
      declare
         procedure F6;
         pragma Import (Ada, F6, "hac_sys__co_defs__finalize_spec");
      begin
         F6;
      end;
      E195 := E195 - 1;
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
         E197 := E197 - 1;
         F8;
      end;
      declare
         procedure F9;
         pragma Import (Ada, F9, "ada__directories__finalize_body");
      begin
         E006 := E006 - 1;
         F9;
      end;
      declare
         procedure F10;
         pragma Import (Ada, F10, "ada__directories__finalize_spec");
      begin
         F10;
      end;
      E172 := E172 - 1;
      declare
         procedure F11;
         pragma Import (Ada, F11, "system__regexp__finalize_spec");
      begin
         F11;
      end;
      E247 := E247 - 1;
      declare
         procedure F12;
         pragma Import (Ada, F12, "system__pool_global__finalize_spec");
      begin
         F12;
      end;
      E178 := E178 - 1;
      declare
         procedure F13;
         pragma Import (Ada, F13, "ada__text_io__finalize_spec");
      begin
         F13;
      end;
      E147 := E147 - 1;
      declare
         procedure F14;
         pragma Import (Ada, F14, "ada__strings__unbounded__finalize_spec");
      begin
         F14;
      end;
      E174 := E174 - 1;
      declare
         procedure F15;
         pragma Import (Ada, F15, "system__storage_pools__subpools__finalize_spec");
      begin
         F15;
      end;
      E168 := E168 - 1;
      declare
         procedure F16;
         pragma Import (Ada, F16, "system__finalization_masters__finalize_spec");
      begin
         F16;
      end;
      E213 := E213 - 1;
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
         E163 := E163 - 1;
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
      E026 := E026 + 1;
      Ada.Containers'Elab_Spec;
      E052 := E052 + 1;
      Ada.Io_Exceptions'Elab_Spec;
      E080 := E080 + 1;
      Ada.Numerics'Elab_Spec;
      E033 := E033 + 1;
      Ada.Strings'Elab_Spec;
      E067 := E067 + 1;
      Ada.Strings.Maps'Elab_Spec;
      E069 := E069 + 1;
      Ada.Strings.Maps.Constants'Elab_Spec;
      E072 := E072 + 1;
      Interfaces.C'Elab_Spec;
      E057 := E057 + 1;
      System.Exceptions'Elab_Spec;
      E027 := E027 + 1;
      System.Object_Reader'Elab_Spec;
      E094 := E094 + 1;
      System.Dwarf_Lines'Elab_Spec;
      System.Os_Lib'Elab_Body;
      E085 := E085 + 1;
      System.Soft_Links.Initialize'Elab_Body;
      E022 := E022 + 1;
      E015 := E015 + 1;
      System.Traceback.Symbolic'Elab_Body;
      E051 := E051 + 1;
      System.Img_Int'Elab_Spec;
      E032 := E032 + 1;
      E010 := E010 + 1;
      System.Img_Uns'Elab_Spec;
      E075 := E075 + 1;
      E062 := E062 + 1;
      Ada.Assertions'Elab_Spec;
      E223 := E223 + 1;
      Ada.Strings.Utf_Encoding'Elab_Spec;
      E117 := E117 + 1;
      Ada.Tags'Elab_Spec;
      Ada.Tags'Elab_Body;
      E125 := E125 + 1;
      Ada.Strings.Text_Buffers'Elab_Spec;
      E115 := E115 + 1;
      Interfaces.C.Strings'Elab_Spec;
      E258 := E258 + 1;
      Ada.Streams'Elab_Spec;
      E113 := E113 + 1;
      System.File_Control_Block'Elab_Spec;
      E166 := E166 + 1;
      System.Finalization_Root'Elab_Spec;
      E135 := E135 + 1;
      Ada.Finalization'Elab_Spec;
      E111 := E111 + 1;
      System.File_Io'Elab_Body;
      E163 := E163 + 1;
      Ada.Streams.Stream_Io'Elab_Spec;
      E213 := E213 + 1;
      System.Storage_Pools'Elab_Spec;
      E170 := E170 + 1;
      System.Finalization_Masters'Elab_Spec;
      System.Finalization_Masters'Elab_Body;
      E168 := E168 + 1;
      System.Storage_Pools.Subpools'Elab_Spec;
      E174 := E174 + 1;
      Ada.Strings.Unbounded'Elab_Spec;
      E147 := E147 + 1;
      Ada.Calendar'Elab_Spec;
      Ada.Calendar'Elab_Body;
      E008 := E008 + 1;
      Ada.Calendar.Delays'Elab_Body;
      E354 := E354 + 1;
      Ada.Text_Io'Elab_Spec;
      Ada.Text_Io'Elab_Body;
      E178 := E178 + 1;
      Ada.Text_Io.Text_Streams'Elab_Spec;
      E219 := E219 + 1;
      System.Pool_Global'Elab_Spec;
      System.Pool_Global'Elab_Body;
      E247 := E247 + 1;
      System.Random_Seed'Elab_Body;
      E211 := E211 + 1;
      System.Regexp'Elab_Spec;
      System.Regexp'Elab_Body;
      E172 := E172 + 1;
      Ada.Directories'Elab_Spec;
      Ada.Directories'Elab_Body;
      E006 := E006 + 1;
      System.Img_Llli'Elab_Spec;
      E268 := E268 + 1;
      System.Img_Lli'Elab_Spec;
      E245 := E245 + 1;
      System.Img_Llu'Elab_Spec;
      E242 := E242 + 1;
      HAT'ELAB_BODY;
      E197 := E197 + 1;
      HAC_SYS.DEFS'ELAB_SPEC;
      E195 := E195 + 1;
      Hac_Sys.Multi_Precision_Integers'Elab_Spec;
      Hac_Sys.Multi_Precision_Integers'Elab_Body;
      E336 := E336 + 1;
      Hac_Sys.Co_Defs'Elab_Spec;
      Hac_Sys.Co_Defs'Elab_Body;
      E295 := E295 + 1;
      Hac_Sys.Errors'Elab_Spec;
      Hac_Sys.Errors'Elab_Body;
      E289 := E289 + 1;
      E301 := E301 + 1;
      Hac_Sys.Scanner'Elab_Body;
      E310 := E310 + 1;
      Hac_Sys.Librarian'Elab_Spec;
      E287 := E287 + 1;
      E308 := E308 + 1;
      Hac_Sys.Parser.Helpers'Elab_Spec;
      Hac_Sys.Parser.Helpers'Elab_Body;
      E319 := E319 + 1;
      E328 := E328 + 1;
      E283 := E283 + 1;
      Hac_Sys.Librarian'Elab_Body;
      E348 := E348 + 1;
      E350 := E350 + 1;
      E346 := E346 + 1;
      E325 := E325 + 1;
      E317 := E317 + 1;
      E330 := E330 + 1;
      Hac_Sys.Parser.Standard_Procedures'Elab_Body;
      E338 := E338 + 1;
      E334 := E334 + 1;
      E344 := E344 + 1;
      E332 := E332 + 1;
      Hac_Sys.Parser.Expressions'Elab_Body;
      E315 := E315 + 1;
      E340 := E340 + 1;
      E304 := E304 + 1;
      E306 := E306 + 1;
      E342 := E342 + 1;
      Hac_Sys.Builder'Elab_Spec;
      Hac_Sys.Builder'Elab_Body;
      E281 := E281 + 1;
      Hac_Sys.Pcode.Interpreter'Elab_Spec;
      Hac_Sys.Pcode.Interpreter.In_Defs'Elab_Spec;
      E360 := E360 + 1;
      Hac_Sys.Interfacing'Elab_Spec;
      E193 := E193 + 1;
      E364 := E364 + 1;
      E358 := E358 + 1;
      E366 := E366 + 1;
      E368 := E368 + 1;
      E362 := E362 + 1;
      E352 := E352 + 1;
      E356 := E356 + 1;
      E180 := E180 + 1;
   end adainit;

   procedure Ada_Main_Program;
   pragma Import (Ada, Ada_Main_Program, "_ada_exchange_native_side");

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
   --   /workspaces/bench-source/src/hac/obj/debug/exchange_common.o
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
   --   /workspaces/bench-source/src/hac/obj/debug/exchange_native_side_pkg.o
   --   /workspaces/bench-source/src/hac/obj/debug/exchange_native_side.o
   --   -L/workspaces/bench-source/src/hac/obj/debug/
   --   -L/workspaces/bench-source/src/hac/obj/debug/
   --   -L/usr/gnat/lib/gcc/x86_64-pc-linux-gnu/12.2.1/adalib/
   --   -static
   --   -lgnat
   --   -lm
   --   -ldl
--  END Object file/option list   

end ada_main;
