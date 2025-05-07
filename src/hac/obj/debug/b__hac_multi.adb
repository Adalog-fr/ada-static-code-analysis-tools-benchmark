pragma Warnings (Off);
pragma Ada_95;
pragma Source_File_Name (ada_main, Spec_File_Name => "b__hac_multi.ads");
pragma Source_File_Name (ada_main, Body_File_Name => "b__hac_multi.adb");
pragma Suppress (Overflow_Check);

with System.Restrictions;
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
   E246 : Short_Integer; pragma Import (Ada, E246, "ada__assertions_E");
   E126 : Short_Integer; pragma Import (Ada, E126, "ada__strings__utf_encoding_E");
   E116 : Short_Integer; pragma Import (Ada, E116, "ada__tags_E");
   E124 : Short_Integer; pragma Import (Ada, E124, "ada__strings__text_buffers_E");
   E290 : Short_Integer; pragma Import (Ada, E290, "interfaces__c__strings_E");
   E161 : Short_Integer; pragma Import (Ada, E161, "ada__streams_E");
   E177 : Short_Integer; pragma Import (Ada, E177, "system__file_control_block_E");
   E176 : Short_Integer; pragma Import (Ada, E176, "system__finalization_root_E");
   E174 : Short_Integer; pragma Import (Ada, E174, "ada__finalization_E");
   E173 : Short_Integer; pragma Import (Ada, E173, "system__file_io_E");
   E167 : Short_Integer; pragma Import (Ada, E167, "ada__streams__stream_io_E");
   E255 : Short_Integer; pragma Import (Ada, E255, "system__storage_pools_E");
   E253 : Short_Integer; pragma Import (Ada, E253, "system__finalization_masters_E");
   E272 : Short_Integer; pragma Import (Ada, E272, "system__storage_pools__subpools_E");
   E226 : Short_Integer; pragma Import (Ada, E226, "ada__strings__unbounded_E");
   E149 : Short_Integer; pragma Import (Ada, E149, "system__task_info_E");
   E006 : Short_Integer; pragma Import (Ada, E006, "ada__calendar_E");
   E108 : Short_Integer; pragma Import (Ada, E108, "ada__calendar__delays_E");
   E179 : Short_Integer; pragma Import (Ada, E179, "ada__text_io_E");
   E242 : Short_Integer; pragma Import (Ada, E242, "ada__text_io__text_streams_E");
   E268 : Short_Integer; pragma Import (Ada, E268, "system__pool_global_E");
   E122 : Short_Integer; pragma Import (Ada, E122, "system__random_seed_E");
   E286 : Short_Integer; pragma Import (Ada, E286, "system__regexp_E");
   E279 : Short_Integer; pragma Import (Ada, E279, "ada__directories_E");
   E192 : Short_Integer; pragma Import (Ada, E192, "system__img_llli_E");
   E156 : Short_Integer; pragma Import (Ada, E156, "system__img_lli_E");
   E143 : Short_Integer; pragma Import (Ada, E143, "system__task_primitives__operations_E");
   E134 : Short_Integer; pragma Import (Ada, E134, "ada__real_time_E");
   E265 : Short_Integer; pragma Import (Ada, E265, "system__img_llu_E");
   E390 : Short_Integer; pragma Import (Ada, E390, "system__tasking__initialization_E");
   E398 : Short_Integer; pragma Import (Ada, E398, "system__tasking__protected_objects_E");
   E400 : Short_Integer; pragma Import (Ada, E400, "system__tasking__protected_objects__entries_E");
   E404 : Short_Integer; pragma Import (Ada, E404, "system__tasking__queuing_E");
   E408 : Short_Integer; pragma Import (Ada, E408, "system__tasking__stages_E");
   E212 : Short_Integer; pragma Import (Ada, E212, "hat_E");
   E210 : Short_Integer; pragma Import (Ada, E210, "hac_sys__defs_E");
   E347 : Short_Integer; pragma Import (Ada, E347, "hac_sys__multi_precision_integers_E");
   E312 : Short_Integer; pragma Import (Ada, E312, "hac_sys__pcode_E");
   E306 : Short_Integer; pragma Import (Ada, E306, "hac_sys__co_defs_E");
   E208 : Short_Integer; pragma Import (Ada, E208, "hac_sys__errors_E");
   E321 : Short_Integer; pragma Import (Ada, E321, "hac_sys__scanner_E");
   E359 : Short_Integer; pragma Import (Ada, E359, "hac_sys__librarian_E");
   E184 : Short_Integer; pragma Import (Ada, E184, "hac_sys__compiler_E");
   E206 : Short_Integer; pragma Import (Ada, E206, "hac_sys__compiler__pcode_emit_E");
   E361 : Short_Integer; pragma Import (Ada, E361, "hac_sys__librarian__built_in_packages_E");
   E315 : Short_Integer; pragma Import (Ada, E315, "hac_sys__parser_E");
   E328 : Short_Integer; pragma Import (Ada, E328, "hac_sys__parser__attributes_E");
   E339 : Short_Integer; pragma Import (Ada, E339, "hac_sys__parser__calls_E");
   E317 : Short_Integer; pragma Import (Ada, E317, "hac_sys__parser__const_var_E");
   E319 : Short_Integer; pragma Import (Ada, E319, "hac_sys__parser__enter_def_E");
   E326 : Short_Integer; pragma Import (Ada, E326, "hac_sys__parser__expressions_E");
   E330 : Short_Integer; pragma Import (Ada, E330, "hac_sys__parser__helpers_E");
   E357 : Short_Integer; pragma Import (Ada, E357, "hac_sys__parser__modularity_E");
   E353 : Short_Integer; pragma Import (Ada, E353, "hac_sys__parser__packages_E");
   E336 : Short_Integer; pragma Import (Ada, E336, "hac_sys__parser__ranges_E");
   E341 : Short_Integer; pragma Import (Ada, E341, "hac_sys__parser__standard_functions_E");
   E349 : Short_Integer; pragma Import (Ada, E349, "hac_sys__parser__standard_procedures_E");
   E345 : Short_Integer; pragma Import (Ada, E345, "hac_sys__parser__statements_E");
   E355 : Short_Integer; pragma Import (Ada, E355, "hac_sys__parser__tasking_E");
   E343 : Short_Integer; pragma Import (Ada, E343, "hac_sys__parser__type_conversion_E");
   E351 : Short_Integer; pragma Import (Ada, E351, "hac_sys__parser__type_def_E");
   E182 : Short_Integer; pragma Import (Ada, E182, "hac_sys__builder_E");
   E363 : Short_Integer; pragma Import (Ada, E363, "hac_sys__pcode__interpreter_E");
   E367 : Short_Integer; pragma Import (Ada, E367, "hac_sys__pcode__interpreter__in_defs_E");
   E365 : Short_Integer; pragma Import (Ada, E365, "hac_sys__interfacing_E");
   E369 : Short_Integer; pragma Import (Ada, E369, "hac_sys__pcode__interpreter__calls_E");
   E375 : Short_Integer; pragma Import (Ada, E375, "hac_sys__pcode__interpreter__composite_data_E");
   E371 : Short_Integer; pragma Import (Ada, E371, "hac_sys__pcode__interpreter__exceptions_E");
   E377 : Short_Integer; pragma Import (Ada, E377, "hac_sys__pcode__interpreter__multi_statement_E");
   E379 : Short_Integer; pragma Import (Ada, E379, "hac_sys__pcode__interpreter__operators_E");
   E373 : Short_Integer; pragma Import (Ada, E373, "hac_sys__pcode__interpreter__tasking_E");

   Sec_Default_Sized_Stacks : array (1 .. 1) of aliased System.Secondary_Stack.SS_Stack (System.Parameters.Runtime_Default_Sec_Stack_Size);

   Local_Priority_Specific_Dispatching : constant String := "";
   Local_Interrupt_States : constant String := "";

   Is_Elaborated : Boolean := False;

   procedure finalize_library is
   begin
      E363 := E363 - 1;
      E367 := E367 - 1;
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
      E182 := E182 - 1;
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
         E359 := E359 - 1;
         F4;
      end;
      declare
         procedure F5;
         pragma Import (Ada, F5, "hac_sys__librarian__finalize_spec");
      begin
         F5;
      end;
      E306 := E306 - 1;
      declare
         procedure F6;
         pragma Import (Ada, F6, "hac_sys__co_defs__finalize_spec");
      begin
         F6;
      end;
      E210 := E210 - 1;
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
         E212 := E212 - 1;
         F8;
      end;
      E400 := E400 - 1;
      declare
         procedure F9;
         pragma Import (Ada, F9, "system__tasking__protected_objects__entries__finalize_spec");
      begin
         F9;
      end;
      declare
         procedure F10;
         pragma Import (Ada, F10, "ada__directories__finalize_body");
      begin
         E279 := E279 - 1;
         F10;
      end;
      declare
         procedure F11;
         pragma Import (Ada, F11, "ada__directories__finalize_spec");
      begin
         F11;
      end;
      E286 := E286 - 1;
      declare
         procedure F12;
         pragma Import (Ada, F12, "system__regexp__finalize_spec");
      begin
         F12;
      end;
      E268 := E268 - 1;
      declare
         procedure F13;
         pragma Import (Ada, F13, "system__pool_global__finalize_spec");
      begin
         F13;
      end;
      E179 := E179 - 1;
      declare
         procedure F14;
         pragma Import (Ada, F14, "ada__text_io__finalize_spec");
      begin
         F14;
      end;
      E226 := E226 - 1;
      declare
         procedure F15;
         pragma Import (Ada, F15, "ada__strings__unbounded__finalize_spec");
      begin
         F15;
      end;
      E272 := E272 - 1;
      declare
         procedure F16;
         pragma Import (Ada, F16, "system__storage_pools__subpools__finalize_spec");
      begin
         F16;
      end;
      E253 := E253 - 1;
      declare
         procedure F17;
         pragma Import (Ada, F17, "system__finalization_masters__finalize_spec");
      begin
         F17;
      end;
      E167 := E167 - 1;
      declare
         procedure F18;
         pragma Import (Ada, F18, "ada__streams__stream_io__finalize_spec");
      begin
         F18;
      end;
      declare
         procedure F19;
         pragma Import (Ada, F19, "system__file_io__finalize_body");
      begin
         E173 := E173 - 1;
         F19;
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
      System.Restrictions.Run_Time_Restrictions :=
        (Set =>
          (False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, True, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False),
         Value => (0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
         Violated =>
          (False, False, False, False, True, True, False, False, 
           True, False, False, True, True, True, True, False, 
           False, False, False, True, False, False, True, True, 
           False, True, True, False, True, True, True, True, 
           False, True, False, False, True, False, True, False, 
           False, True, False, True, False, False, True, False, 
           True, False, True, False, False, True, True, False, 
           True, False, False, False, False, True, False, False, 
           False, True, False, True, True, True, False, False, 
           True, False, True, True, True, False, True, True, 
           False, True, True, True, True, False, False, False, 
           False, False, False, False, False, False, False, True, 
           True, False, False, False),
         Count => (0, 0, 0, 0, 0, 1, 1, 0, 0, 0),
         Unknown => (False, False, False, False, False, False, True, False, False, False));
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
      Ada.Assertions'Elab_Spec;
      E246 := E246 + 1;
      Ada.Strings.Utf_Encoding'Elab_Spec;
      E126 := E126 + 1;
      Ada.Tags'Elab_Spec;
      Ada.Tags'Elab_Body;
      E116 := E116 + 1;
      Ada.Strings.Text_Buffers'Elab_Spec;
      E124 := E124 + 1;
      Interfaces.C.Strings'Elab_Spec;
      E290 := E290 + 1;
      Ada.Streams'Elab_Spec;
      E161 := E161 + 1;
      System.File_Control_Block'Elab_Spec;
      E177 := E177 + 1;
      System.Finalization_Root'Elab_Spec;
      E176 := E176 + 1;
      Ada.Finalization'Elab_Spec;
      E174 := E174 + 1;
      System.File_Io'Elab_Body;
      E173 := E173 + 1;
      Ada.Streams.Stream_Io'Elab_Spec;
      E167 := E167 + 1;
      System.Storage_Pools'Elab_Spec;
      E255 := E255 + 1;
      System.Finalization_Masters'Elab_Spec;
      System.Finalization_Masters'Elab_Body;
      E253 := E253 + 1;
      System.Storage_Pools.Subpools'Elab_Spec;
      E272 := E272 + 1;
      Ada.Strings.Unbounded'Elab_Spec;
      E226 := E226 + 1;
      System.Task_Info'Elab_Spec;
      E149 := E149 + 1;
      Ada.Calendar'Elab_Spec;
      Ada.Calendar'Elab_Body;
      E006 := E006 + 1;
      Ada.Calendar.Delays'Elab_Body;
      E108 := E108 + 1;
      Ada.Text_Io'Elab_Spec;
      Ada.Text_Io'Elab_Body;
      E179 := E179 + 1;
      Ada.Text_Io.Text_Streams'Elab_Spec;
      E242 := E242 + 1;
      System.Pool_Global'Elab_Spec;
      System.Pool_Global'Elab_Body;
      E268 := E268 + 1;
      System.Random_Seed'Elab_Body;
      E122 := E122 + 1;
      System.Regexp'Elab_Spec;
      System.Regexp'Elab_Body;
      E286 := E286 + 1;
      Ada.Directories'Elab_Spec;
      Ada.Directories'Elab_Body;
      E279 := E279 + 1;
      System.Img_Llli'Elab_Spec;
      E192 := E192 + 1;
      System.Img_Lli'Elab_Spec;
      E156 := E156 + 1;
      System.Task_Primitives.Operations'Elab_Body;
      E143 := E143 + 1;
      Ada.Real_Time'Elab_Spec;
      Ada.Real_Time'Elab_Body;
      E134 := E134 + 1;
      System.Img_Llu'Elab_Spec;
      E265 := E265 + 1;
      System.Tasking.Initialization'Elab_Body;
      E390 := E390 + 1;
      System.Tasking.Protected_Objects'Elab_Body;
      E398 := E398 + 1;
      System.Tasking.Protected_Objects.Entries'Elab_Spec;
      E400 := E400 + 1;
      System.Tasking.Queuing'Elab_Body;
      E404 := E404 + 1;
      System.Tasking.Stages'Elab_Body;
      E408 := E408 + 1;
      HAT'ELAB_BODY;
      E212 := E212 + 1;
      HAC_SYS.DEFS'ELAB_SPEC;
      E210 := E210 + 1;
      Hac_Sys.Multi_Precision_Integers'Elab_Spec;
      Hac_Sys.Multi_Precision_Integers'Elab_Body;
      E347 := E347 + 1;
      Hac_Sys.Co_Defs'Elab_Spec;
      Hac_Sys.Co_Defs'Elab_Body;
      E306 := E306 + 1;
      Hac_Sys.Errors'Elab_Spec;
      Hac_Sys.Errors'Elab_Body;
      E208 := E208 + 1;
      E312 := E312 + 1;
      Hac_Sys.Scanner'Elab_Body;
      E321 := E321 + 1;
      Hac_Sys.Librarian'Elab_Spec;
      E206 := E206 + 1;
      E319 := E319 + 1;
      Hac_Sys.Parser.Helpers'Elab_Spec;
      Hac_Sys.Parser.Helpers'Elab_Body;
      E330 := E330 + 1;
      E339 := E339 + 1;
      E184 := E184 + 1;
      Hac_Sys.Librarian'Elab_Body;
      E359 := E359 + 1;
      E361 := E361 + 1;
      E357 := E357 + 1;
      E336 := E336 + 1;
      E328 := E328 + 1;
      E341 := E341 + 1;
      Hac_Sys.Parser.Standard_Procedures'Elab_Body;
      E349 := E349 + 1;
      E345 := E345 + 1;
      E355 := E355 + 1;
      E343 := E343 + 1;
      Hac_Sys.Parser.Expressions'Elab_Body;
      E326 := E326 + 1;
      E351 := E351 + 1;
      E315 := E315 + 1;
      E317 := E317 + 1;
      E353 := E353 + 1;
      Hac_Sys.Builder'Elab_Spec;
      Hac_Sys.Builder'Elab_Body;
      E182 := E182 + 1;
      Hac_Sys.Pcode.Interpreter'Elab_Spec;
      Hac_Sys.Pcode.Interpreter.In_Defs'Elab_Spec;
      E367 := E367 + 1;
      Hac_Sys.Interfacing'Elab_Spec;
      E365 := E365 + 1;
      E375 := E375 + 1;
      E371 := E371 + 1;
      E377 := E377 + 1;
      E379 := E379 + 1;
      E373 := E373 + 1;
      E363 := E363 + 1;
      E369 := E369 + 1;
   end adainit;

   procedure Ada_Main_Program;
   pragma Import (Ada, Ada_Main_Program, "_ada_hac_multi");

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
   --   /workspaces/bench-source/src/hac/obj/debug/hac_multi.o
   --   -L/workspaces/bench-source/src/hac/obj/debug/
   --   -L/workspaces/bench-source/src/hac/obj/debug/
   --   -L/usr/gnat/lib/gcc/x86_64-pc-linux-gnu/12.2.1/adalib/
   --   -static
   --   -lgnarl
   --   -lgnat
   --   -lrt
   --   -lpthread
   --   -lm
   --   -ldl
--  END Object file/option list   

end ada_main;
