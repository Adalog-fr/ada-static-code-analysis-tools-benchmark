pragma Warnings (Off);
pragma Ada_95;
pragma Source_File_Name (langkit_supportmain, Spec_File_Name => "b__langkit_support.ads");
pragma Source_File_Name (langkit_supportmain, Body_File_Name => "b__langkit_support.adb");
pragma Suppress (Overflow_Check);
with Ada.Exceptions;

package body langkit_supportmain is

   E156 : Short_Integer; pragma Import (Ada, E156, "system__os_lib_E");
   E093 : Short_Integer; pragma Import (Ada, E093, "ada__exceptions_E");
   E089 : Short_Integer; pragma Import (Ada, E089, "system__soft_links_E");
   E087 : Short_Integer; pragma Import (Ada, E087, "system__exception_table_E");
   E122 : Short_Integer; pragma Import (Ada, E122, "ada__containers_E");
   E151 : Short_Integer; pragma Import (Ada, E151, "ada__io_exceptions_E");
   E102 : Short_Integer; pragma Import (Ada, E102, "ada__numerics_E");
   E084 : Short_Integer; pragma Import (Ada, E084, "ada__strings_E");
   E140 : Short_Integer; pragma Import (Ada, E140, "ada__strings__maps_E");
   E143 : Short_Integer; pragma Import (Ada, E143, "ada__strings__maps__constants_E");
   E127 : Short_Integer; pragma Import (Ada, E127, "interfaces__c_E");
   E096 : Short_Integer; pragma Import (Ada, E096, "system__exceptions_E");
   E165 : Short_Integer; pragma Import (Ada, E165, "system__object_reader_E");
   E134 : Short_Integer; pragma Import (Ada, E134, "system__dwarf_lines_E");
   E179 : Short_Integer; pragma Import (Ada, E179, "system__soft_links__initialize_E");
   E121 : Short_Integer; pragma Import (Ada, E121, "system__traceback__symbolic_E");
   E101 : Short_Integer; pragma Import (Ada, E101, "system__img_int_E");
   E146 : Short_Integer; pragma Import (Ada, E146, "system__img_uns_E");
   E221 : Short_Integer; pragma Import (Ada, E221, "ada__assertions_E");
   E183 : Short_Integer; pragma Import (Ada, E183, "ada__strings__utf_encoding_E");
   E191 : Short_Integer; pragma Import (Ada, E191, "ada__tags_E");
   E083 : Short_Integer; pragma Import (Ada, E083, "ada__strings__text_buffers_E");
   E246 : Short_Integer; pragma Import (Ada, E246, "gnat_E");
   E217 : Short_Integer; pragma Import (Ada, E217, "interfaces__c__strings_E");
   E199 : Short_Integer; pragma Import (Ada, E199, "ada__streams_E");
   E211 : Short_Integer; pragma Import (Ada, E211, "system__file_control_block_E");
   E210 : Short_Integer; pragma Import (Ada, E210, "system__finalization_root_E");
   E208 : Short_Integer; pragma Import (Ada, E208, "ada__finalization_E");
   E207 : Short_Integer; pragma Import (Ada, E207, "system__file_io_E");
   E309 : Short_Integer; pragma Import (Ada, E309, "system__regpat_E");
   E263 : Short_Integer; pragma Import (Ada, E263, "system__storage_pools_E");
   E265 : Short_Integer; pragma Import (Ada, E265, "system__finalization_masters_E");
   E285 : Short_Integer; pragma Import (Ada, E285, "system__storage_pools__subpools_E");
   E231 : Short_Integer; pragma Import (Ada, E231, "ada__strings__unbounded_E");
   E411 : Short_Integer; pragma Import (Ada, E411, "ada__strings__wide_wide_maps_E");
   E407 : Short_Integer; pragma Import (Ada, E407, "ada__strings__wide_wide_unbounded_E");
   E223 : Short_Integer; pragma Import (Ada, E223, "ada__calendar_E");
   E396 : Short_Integer; pragma Import (Ada, E396, "ada__calendar__delays_E");
   E293 : Short_Integer; pragma Import (Ada, E293, "ada__calendar__time_zones_E");
   E197 : Short_Integer; pragma Import (Ada, E197, "ada__text_io_E");
   E388 : Short_Integer; pragma Import (Ada, E388, "ada__wide_wide_text_io_E");
   E392 : Short_Integer; pragma Import (Ada, E392, "gnat__byte_order_mark_E");
   E298 : Short_Integer; pragma Import (Ada, E298, "gnat__calendar_E");
   E362 : Short_Integer; pragma Import (Ada, E362, "gnat__directory_operations_E");
   E326 : Short_Integer; pragma Import (Ada, E326, "system__checked_pools_E");
   E267 : Short_Integer; pragma Import (Ada, E267, "system__pool_global_E");
   E344 : Short_Integer; pragma Import (Ada, E344, "gnat__expect_E");
   E386 : Short_Integer; pragma Import (Ada, E386, "system__random_seed_E");
   E373 : Short_Integer; pragma Import (Ada, E373, "system__regexp_E");
   E366 : Short_Integer; pragma Import (Ada, E366, "ada__directories_E");
   E303 : Short_Integer; pragma Import (Ada, E303, "system__img_lli_E");
   E305 : Short_Integer; pragma Import (Ada, E305, "system__img_llu_E");
   E301 : Short_Integer; pragma Import (Ada, E301, "gnat__calendar__time_io_E");
   E315 : Short_Integer; pragma Import (Ada, E315, "gnat__debug_pools_E");
   E404 : Short_Integer; pragma Import (Ada, E404, "gnatcoll__gmp__integers_E");
   E028 : Short_Integer; pragma Import (Ada, E028, "langkit_support__errors_E");
   E254 : Short_Integer; pragma Import (Ada, E254, "gnatcoll__atomic_E");
   E313 : Short_Integer; pragma Import (Ada, E313, "gnatcoll__memory_E");
   E340 : Short_Integer; pragma Import (Ada, E340, "gnatcoll__os_E");
   E260 : Short_Integer; pragma Import (Ada, E260, "gnatcoll__storage_pools__headers_E");
   E258 : Short_Integer; pragma Import (Ada, E258, "gnatcoll__refcount_E");
   E339 : Short_Integer; pragma Import (Ada, E339, "gnatcoll__string_builders_E");
   E256 : Short_Integer; pragma Import (Ada, E256, "gnatcoll__strings_impl_E");
   E252 : Short_Integer; pragma Import (Ada, E252, "gnatcoll__strings_E");
   E251 : Short_Integer; pragma Import (Ada, E251, "gnatcoll__strings_E");
   E328 : Short_Integer; pragma Import (Ada, E328, "gnatcoll__mmap_E");
   E330 : Short_Integer; pragma Import (Ada, E330, "gnatcoll__mmap__system_E");
   E333 : Short_Integer; pragma Import (Ada, E333, "gnatcoll__templates_E");
   E335 : Short_Integer; pragma Import (Ada, E335, "gnatcoll__terminal_E");
   E337 : Short_Integer; pragma Import (Ada, E337, "gnatcoll__utils_E");
   E357 : Short_Integer; pragma Import (Ada, E357, "gnatcoll__io_E");
   E375 : Short_Integer; pragma Import (Ada, E375, "gnatcoll__path_E");
   E364 : Short_Integer; pragma Import (Ada, E364, "gnatcoll__io__native_E");
   E378 : Short_Integer; pragma Import (Ada, E378, "gnatcoll__remote_E");
   E382 : Short_Integer; pragma Import (Ada, E382, "gnatcoll__remote__db_E");
   E360 : Short_Integer; pragma Import (Ada, E360, "gnatcoll__io__remote_E");
   E377 : Short_Integer; pragma Import (Ada, E377, "gnatcoll__io__remote__unix_E");
   E380 : Short_Integer; pragma Import (Ada, E380, "gnatcoll__io__remote__windows_E");
   E349 : Short_Integer; pragma Import (Ada, E349, "gnatcoll__vfs_E");
   E289 : Short_Integer; pragma Import (Ada, E289, "gnatcoll__traces_E");
   E394 : Short_Integer; pragma Import (Ada, E394, "gnatcoll__iconv_E");
   E013 : Short_Integer; pragma Import (Ada, E013, "langkit_support__adalog_E");
   E002 : Short_Integer; pragma Import (Ada, E002, "langkit_support__adalog__debug_E");
   E006 : Short_Integer; pragma Import (Ada, E006, "langkit_support__adalog__logic_var_E");
   E012 : Short_Integer; pragma Import (Ada, E012, "langkit_support__adalog__solver_interface_E");
   E015 : Short_Integer; pragma Import (Ada, E015, "langkit_support__array_utils_E");
   E017 : Short_Integer; pragma Import (Ada, E017, "langkit_support__boxes_E");
   E040 : Short_Integer; pragma Import (Ada, E040, "langkit_support__hashes_E");
   E042 : Short_Integer; pragma Import (Ada, E042, "langkit_support__images_E");
   E051 : Short_Integer; pragma Import (Ada, E051, "langkit_support__iterators_E");
   E060 : Short_Integer; pragma Import (Ada, E060, "langkit_support__packrat_E");
   E062 : Short_Integer; pragma Import (Ada, E062, "langkit_support__relative_get_E");
   E070 : Short_Integer; pragma Import (Ada, E070, "langkit_support__text_E");
   E058 : Short_Integer; pragma Import (Ada, E058, "langkit_support__names_E");
   E056 : Short_Integer; pragma Import (Ada, E056, "langkit_support__names__maps_E");
   E064 : Short_Integer; pragma Import (Ada, E064, "langkit_support__slocs_E");
   E027 : Short_Integer; pragma Import (Ada, E027, "langkit_support__diagnostics_E");
   E025 : Short_Integer; pragma Import (Ada, E025, "langkit_support__diagnostics__output_E");
   E030 : Short_Integer; pragma Import (Ada, E030, "langkit_support__file_readers_E");
   E077 : Short_Integer; pragma Import (Ada, E077, "langkit_support__vectors_E");
   E010 : Short_Integer; pragma Import (Ada, E010, "langkit_support__adalog__solver_E");
   E004 : Short_Integer; pragma Import (Ada, E004, "langkit_support__adalog__generic_main_support_E");
   E008 : Short_Integer; pragma Import (Ada, E008, "langkit_support__adalog__main_support_E");
   E023 : Short_Integer; pragma Import (Ada, E023, "langkit_support__cheap_sets_E");
   E038 : Short_Integer; pragma Import (Ada, E038, "langkit_support__generic_bump_ptr_E");
   E019 : Short_Integer; pragma Import (Ada, E019, "langkit_support__bump_ptr_E");
   E021 : Short_Integer; pragma Import (Ada, E021, "langkit_support__bump_ptr_vectors_E");
   E052 : Short_Integer; pragma Import (Ada, E052, "langkit_support__lexical_envs_E");
   E068 : Short_Integer; pragma Import (Ada, E068, "langkit_support__symbols_E");
   E054 : Short_Integer; pragma Import (Ada, E054, "langkit_support__lexical_envs_impl_E");
   E066 : Short_Integer; pragma Import (Ada, E066, "langkit_support__symbols__precomputed_E");
   E072 : Short_Integer; pragma Import (Ada, E072, "langkit_support__token_data_handlers_E");
   E036 : Short_Integer; pragma Import (Ada, E036, "langkit_support__generic_api_E");
   E044 : Short_Integer; pragma Import (Ada, E044, "langkit_support__internal__analysis_E");
   E032 : Short_Integer; pragma Import (Ada, E032, "langkit_support__generic_api__analysis_E");
   E034 : Short_Integer; pragma Import (Ada, E034, "langkit_support__generic_api__introspection_E");
   E048 : Short_Integer; pragma Import (Ada, E048, "langkit_support__internal__introspection_E");
   E074 : Short_Integer; pragma Import (Ada, E074, "langkit_support__tree_traversal_iterator_E");

   Sec_Default_Sized_Stacks : array (1 .. 1) of aliased System.Secondary_Stack.SS_Stack (System.Parameters.Runtime_Default_Sec_Stack_Size);

   Local_Priority_Specific_Dispatching : constant String := "";
   Local_Interrupt_States : constant String := "";

   Is_Elaborated : Boolean := False;

   procedure finalize_library is
   begin
      E048 := E048 - 1;
      E034 := E034 - 1;
      E032 := E032 - 1;
      declare
         procedure F1;
         pragma Import (Ada, F1, "langkit_support__internal__introspection__finalize_spec");
      begin
         if E048 = 0 then
            F1;
         end if;
      end;
      declare
         procedure F2;
         pragma Import (Ada, F2, "langkit_support__generic_api__introspection__finalize_spec");
      begin
         if E034 = 0 then
            F2;
         end if;
      end;
      declare
         procedure F3;
         pragma Import (Ada, F3, "langkit_support__generic_api__analysis__finalize_spec");
      begin
         if E032 = 0 then
            F3;
         end if;
      end;
      E072 := E072 - 1;
      declare
         procedure F4;
         pragma Import (Ada, F4, "langkit_support__token_data_handlers__finalize_spec");
      begin
         if E072 = 0 then
            F4;
         end if;
      end;
      E068 := E068 - 1;
      declare
         procedure F5;
         pragma Import (Ada, F5, "langkit_support__symbols__finalize_spec");
      begin
         if E068 = 0 then
            F5;
         end if;
      end;
      declare
         procedure F6;
         pragma Import (Ada, F6, "langkit_support__lexical_envs__finalize_spec");
      begin
         E052 := E052 - 1;
         if E052 = 0 then
            F6;
         end if;
      end;
      E019 := E019 - 1;
      declare
         procedure F7;
         pragma Import (Ada, F7, "langkit_support__bump_ptr__finalize_spec");
      begin
         if E019 = 0 then
            F7;
         end if;
      end;
      E008 := E008 - 1;
      declare
         procedure F8;
         pragma Import (Ada, F8, "langkit_support__adalog__main_support__finalize_spec");
      begin
         if E008 = 0 then
            F8;
         end if;
      end;
      E030 := E030 - 1;
      declare
         procedure F9;
         pragma Import (Ada, F9, "langkit_support__file_readers__finalize_spec");
      begin
         if E030 = 0 then
            F9;
         end if;
      end;
      E027 := E027 - 1;
      declare
         procedure F10;
         pragma Import (Ada, F10, "langkit_support__diagnostics__finalize_spec");
      begin
         if E027 = 0 then
            F10;
         end if;
      end;
      declare
         procedure F11;
         pragma Import (Ada, F11, "gnatcoll__traces__finalize_body");
      begin
         E289 := E289 - 1;
         if E289 = 0 then
            F11;
         end if;
      end;
      declare
         procedure F12;
         pragma Import (Ada, F12, "gnatcoll__traces__finalize_spec");
      begin
         if E289 = 0 then
            F12;
         end if;
      end;
      E349 := E349 - 1;
      declare
         procedure F13;
         pragma Import (Ada, F13, "gnatcoll__vfs__finalize_spec");
      begin
         if E349 = 0 then
            F13;
         end if;
      end;
      E360 := E360 - 1;
      declare
         procedure F14;
         pragma Import (Ada, F14, "gnatcoll__io__remote__finalize_spec");
      begin
         if E360 = 0 then
            F14;
         end if;
      end;
      declare
         procedure F15;
         pragma Import (Ada, F15, "gnatcoll__remote__finalize_spec");
      begin
         E378 := E378 - 1;
         if E378 = 0 then
            F15;
         end if;
      end;
      E364 := E364 - 1;
      declare
         procedure F16;
         pragma Import (Ada, F16, "gnatcoll__io__native__finalize_spec");
      begin
         if E364 = 0 then
            F16;
         end if;
      end;
      E357 := E357 - 1;
      declare
         procedure F17;
         pragma Import (Ada, F17, "gnatcoll__io__finalize_spec");
      begin
         if E357 = 0 then
            F17;
         end if;
      end;
      E335 := E335 - 1;
      declare
         procedure F18;
         pragma Import (Ada, F18, "gnatcoll__terminal__finalize_spec");
      begin
         if E335 = 0 then
            F18;
         end if;
      end;
      E252 := E252 - 1;
      declare
         procedure F19;
         pragma Import (Ada, F19, "gnatcoll__strings__finalize_spec");
      begin
         if E252 = 0 then
            F19;
         end if;
      end;
      E258 := E258 - 1;
      declare
         procedure F20;
         pragma Import (Ada, F20, "gnatcoll__refcount__finalize_spec");
      begin
         if E258 = 0 then
            F20;
         end if;
      end;
      declare
         procedure F21;
         pragma Import (Ada, F21, "gnatcoll__memory__finalize_body");
      begin
         E313 := E313 - 1;
         if E313 = 0 then
            F21;
         end if;
      end;
      E404 := E404 - 1;
      declare
         procedure F22;
         pragma Import (Ada, F22, "gnatcoll__gmp__integers__finalize_spec");
      begin
         if E404 = 0 then
            F22;
         end if;
      end;
      declare
         procedure F23;
         pragma Import (Ada, F23, "gnat__debug_pools__finalize_body");
      begin
         E315 := E315 - 1;
         if E315 = 0 then
            F23;
         end if;
      end;
      declare
         procedure F24;
         pragma Import (Ada, F24, "gnat__debug_pools__finalize_spec");
      begin
         if E315 = 0 then
            F24;
         end if;
      end;
      declare
         procedure F25;
         pragma Import (Ada, F25, "ada__directories__finalize_body");
      begin
         E366 := E366 - 1;
         if E366 = 0 then
            F25;
         end if;
      end;
      declare
         procedure F26;
         pragma Import (Ada, F26, "ada__directories__finalize_spec");
      begin
         if E366 = 0 then
            F26;
         end if;
      end;
      E373 := E373 - 1;
      declare
         procedure F27;
         pragma Import (Ada, F27, "system__regexp__finalize_spec");
      begin
         if E373 = 0 then
            F27;
         end if;
      end;
      E344 := E344 - 1;
      declare
         procedure F28;
         pragma Import (Ada, F28, "gnat__expect__finalize_spec");
      begin
         if E344 = 0 then
            F28;
         end if;
      end;
      E267 := E267 - 1;
      declare
         procedure F29;
         pragma Import (Ada, F29, "system__pool_global__finalize_spec");
      begin
         if E267 = 0 then
            F29;
         end if;
      end;
      E388 := E388 - 1;
      declare
         procedure F30;
         pragma Import (Ada, F30, "ada__wide_wide_text_io__finalize_spec");
      begin
         if E388 = 0 then
            F30;
         end if;
      end;
      E197 := E197 - 1;
      declare
         procedure F31;
         pragma Import (Ada, F31, "ada__text_io__finalize_spec");
      begin
         if E197 = 0 then
            F31;
         end if;
      end;
      E407 := E407 - 1;
      declare
         procedure F32;
         pragma Import (Ada, F32, "ada__strings__wide_wide_unbounded__finalize_spec");
      begin
         if E407 = 0 then
            F32;
         end if;
      end;
      E411 := E411 - 1;
      declare
         procedure F33;
         pragma Import (Ada, F33, "ada__strings__wide_wide_maps__finalize_spec");
      begin
         if E411 = 0 then
            F33;
         end if;
      end;
      E231 := E231 - 1;
      declare
         procedure F34;
         pragma Import (Ada, F34, "ada__strings__unbounded__finalize_spec");
      begin
         if E231 = 0 then
            F34;
         end if;
      end;
      E285 := E285 - 1;
      declare
         procedure F35;
         pragma Import (Ada, F35, "system__storage_pools__subpools__finalize_spec");
      begin
         if E285 = 0 then
            F35;
         end if;
      end;
      E265 := E265 - 1;
      declare
         procedure F36;
         pragma Import (Ada, F36, "system__finalization_masters__finalize_spec");
      begin
         if E265 = 0 then
            F36;
         end if;
      end;
      declare
         procedure F37;
         pragma Import (Ada, F37, "system__file_io__finalize_body");
      begin
         E207 := E207 - 1;
         if E207 = 0 then
            F37;
         end if;
      end;
      declare
         procedure Reraise_Library_Exception_If_Any;
            pragma Import (Ada, Reraise_Library_Exception_If_Any, "__gnat_reraise_library_exception_if_any");
      begin
         Reraise_Library_Exception_If_Any;
      end;
   end finalize_library;

   procedure langkit_supportfinal is

      procedure Runtime_Finalize;
      pragma Import (C, Runtime_Finalize, "__gnat_runtime_finalize");

   begin
      if not Is_Elaborated then
         return;
      end if;
      Is_Elaborated := False;
      Runtime_Finalize;
      finalize_library;
   end langkit_supportfinal;

   type No_Param_Proc is access procedure;
   pragma Favor_Top_Level (No_Param_Proc);

   procedure langkit_supportinit is
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

      langkit_supportmain'Elab_Body;
      Default_Secondary_Stack_Size := System.Parameters.Runtime_Default_Sec_Stack_Size;
      Binder_Sec_Stacks_Count := 1;
      Default_Sized_SS_Pool := Sec_Default_Sized_Stacks'Address;

      Runtime_Initialize (1);

      if E093 = 0 then
         Ada.Exceptions'Elab_Spec;
      end if;
      if E089 = 0 then
         System.Soft_Links'Elab_Spec;
      end if;
      if E087 = 0 then
         System.Exception_Table'Elab_Body;
      end if;
      E087 := E087 + 1;
      if E122 = 0 then
         Ada.Containers'Elab_Spec;
      end if;
      E122 := E122 + 1;
      if E151 = 0 then
         Ada.Io_Exceptions'Elab_Spec;
      end if;
      E151 := E151 + 1;
      if E102 = 0 then
         Ada.Numerics'Elab_Spec;
      end if;
      E102 := E102 + 1;
      if E084 = 0 then
         Ada.Strings'Elab_Spec;
      end if;
      E084 := E084 + 1;
      if E140 = 0 then
         Ada.Strings.Maps'Elab_Spec;
      end if;
      E140 := E140 + 1;
      if E143 = 0 then
         Ada.Strings.Maps.Constants'Elab_Spec;
      end if;
      E143 := E143 + 1;
      if E127 = 0 then
         Interfaces.C'Elab_Spec;
      end if;
      E127 := E127 + 1;
      if E096 = 0 then
         System.Exceptions'Elab_Spec;
      end if;
      E096 := E096 + 1;
      if E165 = 0 then
         System.Object_Reader'Elab_Spec;
      end if;
      E165 := E165 + 1;
      if E134 = 0 then
         System.Dwarf_Lines'Elab_Spec;
      end if;
      if E156 = 0 then
         System.Os_Lib'Elab_Body;
      end if;
      E156 := E156 + 1;
      if E179 = 0 then
         System.Soft_Links.Initialize'Elab_Body;
      end if;
      E179 := E179 + 1;
      E089 := E089 + 1;
      if E121 = 0 then
         System.Traceback.Symbolic'Elab_Body;
      end if;
      E121 := E121 + 1;
      if E101 = 0 then
         System.Img_Int'Elab_Spec;
      end if;
      E101 := E101 + 1;
      E093 := E093 + 1;
      if E146 = 0 then
         System.Img_Uns'Elab_Spec;
      end if;
      E146 := E146 + 1;
      E134 := E134 + 1;
      if E221 = 0 then
         Ada.Assertions'Elab_Spec;
      end if;
      E221 := E221 + 1;
      if E183 = 0 then
         Ada.Strings.Utf_Encoding'Elab_Spec;
      end if;
      E183 := E183 + 1;
      if E191 = 0 then
         Ada.Tags'Elab_Spec;
      end if;
      if E191 = 0 then
         Ada.Tags'Elab_Body;
      end if;
      E191 := E191 + 1;
      if E083 = 0 then
         Ada.Strings.Text_Buffers'Elab_Spec;
      end if;
      E083 := E083 + 1;
      if E246 = 0 then
         Gnat'Elab_Spec;
      end if;
      E246 := E246 + 1;
      if E217 = 0 then
         Interfaces.C.Strings'Elab_Spec;
      end if;
      E217 := E217 + 1;
      if E199 = 0 then
         Ada.Streams'Elab_Spec;
      end if;
      E199 := E199 + 1;
      if E211 = 0 then
         System.File_Control_Block'Elab_Spec;
      end if;
      E211 := E211 + 1;
      if E210 = 0 then
         System.Finalization_Root'Elab_Spec;
      end if;
      E210 := E210 + 1;
      if E208 = 0 then
         Ada.Finalization'Elab_Spec;
      end if;
      E208 := E208 + 1;
      if E207 = 0 then
         System.File_Io'Elab_Body;
      end if;
      E207 := E207 + 1;
      if E309 = 0 then
         System.Regpat'Elab_Spec;
      end if;
      E309 := E309 + 1;
      if E263 = 0 then
         System.Storage_Pools'Elab_Spec;
      end if;
      E263 := E263 + 1;
      if E265 = 0 then
         System.Finalization_Masters'Elab_Spec;
      end if;
      if E265 = 0 then
         System.Finalization_Masters'Elab_Body;
      end if;
      E265 := E265 + 1;
      if E285 = 0 then
         System.Storage_Pools.Subpools'Elab_Spec;
      end if;
      E285 := E285 + 1;
      if E231 = 0 then
         Ada.Strings.Unbounded'Elab_Spec;
      end if;
      E231 := E231 + 1;
      if E411 = 0 then
         Ada.Strings.Wide_Wide_Maps'Elab_Spec;
      end if;
      E411 := E411 + 1;
      if E407 = 0 then
         Ada.Strings.Wide_Wide_Unbounded'Elab_Spec;
      end if;
      E407 := E407 + 1;
      if E223 = 0 then
         Ada.Calendar'Elab_Spec;
      end if;
      if E223 = 0 then
         Ada.Calendar'Elab_Body;
      end if;
      E223 := E223 + 1;
      if E396 = 0 then
         Ada.Calendar.Delays'Elab_Body;
      end if;
      E396 := E396 + 1;
      if E293 = 0 then
         Ada.Calendar.Time_Zones'Elab_Spec;
      end if;
      E293 := E293 + 1;
      if E197 = 0 then
         Ada.Text_Io'Elab_Spec;
      end if;
      if E197 = 0 then
         Ada.Text_Io'Elab_Body;
      end if;
      E197 := E197 + 1;
      if E388 = 0 then
         Ada.Wide_Wide_Text_Io'Elab_Spec;
      end if;
      if E388 = 0 then
         Ada.Wide_Wide_Text_Io'Elab_Body;
      end if;
      E388 := E388 + 1;
      E392 := E392 + 1;
      if E298 = 0 then
         Gnat.Calendar'Elab_Spec;
      end if;
      E298 := E298 + 1;
      if E362 = 0 then
         Gnat.Directory_Operations'Elab_Spec;
      end if;
      if E362 = 0 then
         Gnat.Directory_Operations'Elab_Body;
      end if;
      E362 := E362 + 1;
      if E326 = 0 then
         System.Checked_Pools'Elab_Spec;
      end if;
      E326 := E326 + 1;
      if E267 = 0 then
         System.Pool_Global'Elab_Spec;
      end if;
      if E267 = 0 then
         System.Pool_Global'Elab_Body;
      end if;
      E267 := E267 + 1;
      if E344 = 0 then
         Gnat.Expect'Elab_Spec;
      end if;
      if E344 = 0 then
         Gnat.Expect'Elab_Body;
      end if;
      E344 := E344 + 1;
      if E386 = 0 then
         System.Random_Seed'Elab_Body;
      end if;
      E386 := E386 + 1;
      if E373 = 0 then
         System.Regexp'Elab_Spec;
      end if;
      if E373 = 0 then
         System.Regexp'Elab_Body;
      end if;
      E373 := E373 + 1;
      if E366 = 0 then
         Ada.Directories'Elab_Spec;
      end if;
      if E366 = 0 then
         Ada.Directories'Elab_Body;
      end if;
      E366 := E366 + 1;
      if E303 = 0 then
         System.Img_Lli'Elab_Spec;
      end if;
      E303 := E303 + 1;
      if E305 = 0 then
         System.Img_Llu'Elab_Spec;
      end if;
      E305 := E305 + 1;
      if E301 = 0 then
         Gnat.Calendar.Time_Io'Elab_Spec;
      end if;
      E301 := E301 + 1;
      if E315 = 0 then
         Gnat.Debug_Pools'Elab_Spec;
      end if;
      if E315 = 0 then
         Gnat.Debug_Pools'Elab_Body;
      end if;
      E315 := E315 + 1;
      if E404 = 0 then
         GNATCOLL.GMP.INTEGERS'ELAB_SPEC;
      end if;
      E404 := E404 + 1;
      if E028 = 0 then
         Langkit_Support.Errors'Elab_Spec;
      end if;
      E028 := E028 + 1;
      E254 := E254 + 1;
      if E313 = 0 then
         GNATCOLL.MEMORY'ELAB_BODY;
      end if;
      E313 := E313 + 1;
      if E340 = 0 then
         GNATCOLL.OS'ELAB_SPEC;
      end if;
      E340 := E340 + 1;
      E260 := E260 + 1;
      if E258 = 0 then
         GNATCOLL.REFCOUNT'ELAB_SPEC;
      end if;
      E258 := E258 + 1;
      E339 := E339 + 1;
      E256 := E256 + 1;
      if E252 = 0 then
         GNATCOLL.STRINGS'ELAB_SPEC;
      end if;
      if E252 = 0 then
         GNATCOLL.STRINGS'ELAB_BODY;
      end if;
      E252 := E252 + 1;
      if E328 = 0 then
         GNATCOLL.MMAP'ELAB_SPEC;
      end if;
      E330 := E330 + 1;
      E328 := E328 + 1;
      if E333 = 0 then
         GNATCOLL.TEMPLATES'ELAB_SPEC;
      end if;
      E333 := E333 + 1;
      if E335 = 0 then
         GNATCOLL.TERMINAL'ELAB_SPEC;
      end if;
      if E335 = 0 then
         GNATCOLL.TERMINAL'ELAB_BODY;
      end if;
      E335 := E335 + 1;
      E337 := E337 + 1;
      if E357 = 0 then
         GNATCOLL.IO'ELAB_SPEC;
      end if;
      if E357 = 0 then
         GNATCOLL.IO'ELAB_BODY;
      end if;
      E357 := E357 + 1;
      if E375 = 0 then
         GNATCOLL.PATH'ELAB_SPEC;
      end if;
      if E375 = 0 then
         GNATCOLL.PATH'ELAB_BODY;
      end if;
      E375 := E375 + 1;
      if E364 = 0 then
         GNATCOLL.IO.NATIVE'ELAB_SPEC;
      end if;
      if E364 = 0 then
         GNATCOLL.IO.NATIVE'ELAB_BODY;
      end if;
      E364 := E364 + 1;
      if E378 = 0 then
         GNATCOLL.REMOTE'ELAB_SPEC;
      end if;
      E378 := E378 + 1;
      if E382 = 0 then
         GNATCOLL.REMOTE.DB'ELAB_SPEC;
      end if;
      E382 := E382 + 1;
      if E360 = 0 then
         GNATCOLL.IO.REMOTE'ELAB_SPEC;
      end if;
      E377 := E377 + 1;
      E380 := E380 + 1;
      if E360 = 0 then
         GNATCOLL.IO.REMOTE'ELAB_BODY;
      end if;
      E360 := E360 + 1;
      if E349 = 0 then
         GNATCOLL.VFS'ELAB_SPEC;
      end if;
      if E349 = 0 then
         GNATCOLL.VFS'ELAB_BODY;
      end if;
      E349 := E349 + 1;
      if E289 = 0 then
         GNATCOLL.TRACES'ELAB_SPEC;
      end if;
      if E289 = 0 then
         GNATCOLL.TRACES'ELAB_BODY;
      end if;
      E289 := E289 + 1;
      if E394 = 0 then
         GNATCOLL.ICONV'ELAB_SPEC;
      end if;
      if E394 = 0 then
         GNATCOLL.ICONV'ELAB_BODY;
      end if;
      E394 := E394 + 1;
      if E013 = 0 then
         Langkit_Support.Adalog'Elab_Spec;
      end if;
      E013 := E013 + 1;
      E002 := E002 + 1;
      E006 := E006 + 1;
      E012 := E012 + 1;
      E015 := E015 + 1;
      E017 := E017 + 1;
      E040 := E040 + 1;
      E042 := E042 + 1;
      E051 := E051 + 1;
      E060 := E060 + 1;
      E062 := E062 + 1;
      if E070 = 0 then
         Langkit_Support.Text'Elab_Spec;
      end if;
      E070 := E070 + 1;
      if E058 = 0 then
         Langkit_Support.Names'Elab_Spec;
      end if;
      E058 := E058 + 1;
      E056 := E056 + 1;
      E064 := E064 + 1;
      if E027 = 0 then
         Langkit_Support.Diagnostics'Elab_Spec;
      end if;
      E027 := E027 + 1;
      if E025 = 0 then
         Langkit_Support.Diagnostics.Output'Elab_Spec;
      end if;
      if E025 = 0 then
         Langkit_Support.Diagnostics.Output'Elab_Body;
      end if;
      E025 := E025 + 1;
      if E030 = 0 then
         Langkit_Support.File_Readers'Elab_Spec;
      end if;
      E030 := E030 + 1;
      E077 := E077 + 1;
      E010 := E010 + 1;
      E004 := E004 + 1;
      if E008 = 0 then
         Langkit_Support.Adalog.Main_Support'Elab_Spec;
      end if;
      E008 := E008 + 1;
      E023 := E023 + 1;
      E038 := E038 + 1;
      if E019 = 0 then
         Langkit_Support.Bump_Ptr'Elab_Spec;
      end if;
      E019 := E019 + 1;
      E021 := E021 + 1;
      if E052 = 0 then
         Langkit_Support.Lexical_Envs'Elab_Spec;
      end if;
      E052 := E052 + 1;
      if E068 = 0 then
         Langkit_Support.Symbols'Elab_Spec;
      end if;
      E068 := E068 + 1;
      E054 := E054 + 1;
      E066 := E066 + 1;
      if E072 = 0 then
         Langkit_Support.Token_Data_Handlers'Elab_Spec;
      end if;
      E072 := E072 + 1;
      if E036 = 0 then
         Langkit_Support.Generic_Api'Elab_Spec;
      end if;
      if E032 = 0 then
         Langkit_Support.Generic_Api.Analysis'Elab_Spec;
      end if;
      if E034 = 0 then
         Langkit_Support.Generic_Api.Introspection'Elab_Spec;
      end if;
      if E048 = 0 then
         Langkit_Support.Internal.Introspection'Elab_Spec;
      end if;
      E036 := E036 + 1;
      if E032 = 0 then
         Langkit_Support.Generic_Api.Analysis'Elab_Body;
      end if;
      E032 := E032 + 1;
      if E034 = 0 then
         Langkit_Support.Generic_Api.Introspection'Elab_Body;
      end if;
      E034 := E034 + 1;
      E044 := E044 + 1;
      if E048 = 0 then
         Langkit_Support.Internal.Introspection'Elab_Body;
      end if;
      E048 := E048 + 1;
      E074 := E074 + 1;
   end langkit_supportinit;

--  BEGIN Object file/option list
   --   /workspaces/bench-source/src/langkit_support/obj/dev/langkit_support.o
   --   /workspaces/bench-source/src/langkit_support/obj/dev/langkit_support-errors.o
   --   /workspaces/bench-source/src/langkit_support/obj/dev/langkit_support-adalog.o
   --   /workspaces/bench-source/src/langkit_support/obj/dev/langkit_support-adalog-debug.o
   --   /workspaces/bench-source/src/langkit_support/obj/dev/langkit_support-adalog-logic_var.o
   --   /workspaces/bench-source/src/langkit_support/obj/dev/langkit_support-adalog-solver_interface.o
   --   /workspaces/bench-source/src/langkit_support/obj/dev/langkit_support-array_utils.o
   --   /workspaces/bench-source/src/langkit_support/obj/dev/langkit_support-boxes.o
   --   /workspaces/bench-source/src/langkit_support/obj/dev/langkit_support-hashes.o
   --   /workspaces/bench-source/src/langkit_support/obj/dev/langkit_support-images.o
   --   /workspaces/bench-source/src/langkit_support/obj/dev/langkit_support-iterators.o
   --   /workspaces/bench-source/src/langkit_support/obj/dev/langkit_support-packrat.o
   --   /workspaces/bench-source/src/langkit_support/obj/dev/langkit_support-relative_get.o
   --   /workspaces/bench-source/src/langkit_support/obj/dev/langkit_support-text.o
   --   /workspaces/bench-source/src/langkit_support/obj/dev/langkit_support-names.o
   --   /workspaces/bench-source/src/langkit_support/obj/dev/langkit_support-names-maps.o
   --   /workspaces/bench-source/src/langkit_support/obj/dev/langkit_support-slocs.o
   --   /workspaces/bench-source/src/langkit_support/obj/dev/langkit_support-diagnostics.o
   --   /workspaces/bench-source/src/langkit_support/obj/dev/langkit_support-diagnostics-output.o
   --   /workspaces/bench-source/src/langkit_support/obj/dev/langkit_support-file_readers.o
   --   /workspaces/bench-source/src/langkit_support/obj/dev/langkit_support-types.o
   --   /workspaces/bench-source/src/langkit_support/obj/dev/langkit_support-vectors.o
   --   /workspaces/bench-source/src/langkit_support/obj/dev/langkit_support-adalog-solver.o
   --   /workspaces/bench-source/src/langkit_support/obj/dev/langkit_support-adalog-generic_main_support.o
   --   /workspaces/bench-source/src/langkit_support/obj/dev/langkit_support-adalog-main_support.o
   --   /workspaces/bench-source/src/langkit_support/obj/dev/langkit_support-cheap_sets.o
   --   /workspaces/bench-source/src/langkit_support/obj/dev/langkit_support-generic_bump_ptr.o
   --   /workspaces/bench-source/src/langkit_support/obj/dev/langkit_support-bump_ptr.o
   --   /workspaces/bench-source/src/langkit_support/obj/dev/langkit_support-bump_ptr_vectors.o
   --   /workspaces/bench-source/src/langkit_support/obj/dev/langkit_support-lexical_envs.o
   --   /workspaces/bench-source/src/langkit_support/obj/dev/langkit_support-symbols.o
   --   /workspaces/bench-source/src/langkit_support/obj/dev/langkit_support-lexical_envs_impl.o
   --   /workspaces/bench-source/src/langkit_support/obj/dev/langkit_support-symbols-precomputed.o
   --   /workspaces/bench-source/src/langkit_support/obj/dev/langkit_support-token_data_handlers.o
   --   /workspaces/bench-source/src/langkit_support/obj/dev/langkit_support-internal.o
   --   /workspaces/bench-source/src/langkit_support/obj/dev/langkit_support-internal-conversions.o
   --   /workspaces/bench-source/src/langkit_support/obj/dev/langkit_support-internal-descriptor.o
   --   /workspaces/bench-source/src/langkit_support/obj/dev/langkit_support-generic_api.o
   --   /workspaces/bench-source/src/langkit_support/obj/dev/langkit_support-generic_api-analysis.o
   --   /workspaces/bench-source/src/langkit_support/obj/dev/langkit_support-generic_api-introspection.o
   --   /workspaces/bench-source/src/langkit_support/obj/dev/langkit_support-internal-analysis.o
   --   /workspaces/bench-source/src/langkit_support/obj/dev/langkit_support-internal-introspection.o
   --   /workspaces/bench-source/src/langkit_support/obj/dev/langkit_support-tree_traversal_iterator.o
   --   -L/workspaces/bench-source/src/langkit_support/obj/dev/
   --   -L/workspaces/bench-source/src/gnatcoll-bindings/iconv/lib/static/
   --   -L/workspaces/bench-source/src/gnatcoll-bindings/gmp/lib/static/
   --   -L/workspaces/bench-source/src/xmlada/schema/lib/static/
   --   -L/workspaces/bench-source/src/xmlada/dom/lib/static/
   --   -L/workspaces/bench-source/src/xmlada/input_sources/lib/static/
   --   -L/workspaces/bench-source/src/xmlada/sax/lib/static/
   --   -L/workspaces/bench-source/src/xmlada/unicode/lib/static/
   --   -L/workspaces/bench-source/src/gprbuild/gpr/lib/production/static/
   --   -L/workspaces/bench-source/src/gnatcoll-core/lib/gnatcoll/static/
   --   -L/usr/gnat/lib/gcc/x86_64-pc-linux-gnu/12.2.1/adalib/
   --   -static
   --   -lgnat
   --   -ldl
--  END Object file/option list   

end langkit_supportmain;
