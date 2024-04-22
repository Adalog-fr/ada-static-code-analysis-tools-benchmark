pragma Warnings (Off);
pragma Ada_95;
pragma Source_File_Name (ada_main, Spec_File_Name => "b__find_zip.ads");
pragma Source_File_Name (ada_main, Body_File_Name => "b__find_zip.adb");
pragma Suppress (Overflow_Check);
with Ada.Exceptions;

package body ada_main is

   E072 : Short_Integer; pragma Import (Ada, E072, "system__os_lib_E");
   E018 : Short_Integer; pragma Import (Ada, E018, "ada__exceptions_E");
   E014 : Short_Integer; pragma Import (Ada, E014, "system__soft_links_E");
   E012 : Short_Integer; pragma Import (Ada, E012, "system__exception_table_E");
   E047 : Short_Integer; pragma Import (Ada, E047, "ada__containers_E");
   E067 : Short_Integer; pragma Import (Ada, E067, "ada__io_exceptions_E");
   E027 : Short_Integer; pragma Import (Ada, E027, "ada__numerics_E");
   E009 : Short_Integer; pragma Import (Ada, E009, "ada__strings_E");
   E099 : Short_Integer; pragma Import (Ada, E099, "ada__strings__maps_E");
   E102 : Short_Integer; pragma Import (Ada, E102, "ada__strings__maps__constants_E");
   E052 : Short_Integer; pragma Import (Ada, E052, "interfaces__c_E");
   E021 : Short_Integer; pragma Import (Ada, E021, "system__exceptions_E");
   E081 : Short_Integer; pragma Import (Ada, E081, "system__object_reader_E");
   E059 : Short_Integer; pragma Import (Ada, E059, "system__dwarf_lines_E");
   E095 : Short_Integer; pragma Import (Ada, E095, "system__soft_links__initialize_E");
   E046 : Short_Integer; pragma Import (Ada, E046, "system__traceback__symbolic_E");
   E026 : Short_Integer; pragma Import (Ada, E026, "system__img_int_E");
   E062 : Short_Integer; pragma Import (Ada, E062, "system__img_uns_E");
   E114 : Short_Integer; pragma Import (Ada, E114, "ada__strings__utf_encoding_E");
   E122 : Short_Integer; pragma Import (Ada, E122, "ada__tags_E");
   E112 : Short_Integer; pragma Import (Ada, E112, "ada__strings__text_buffers_E");
   E110 : Short_Integer; pragma Import (Ada, E110, "ada__streams_E");
   E138 : Short_Integer; pragma Import (Ada, E138, "system__file_control_block_E");
   E137 : Short_Integer; pragma Import (Ada, E137, "system__finalization_root_E");
   E135 : Short_Integer; pragma Import (Ada, E135, "ada__finalization_E");
   E134 : Short_Integer; pragma Import (Ada, E134, "system__file_io_E");
   E172 : Short_Integer; pragma Import (Ada, E172, "ada__streams__stream_io_E");
   E210 : Short_Integer; pragma Import (Ada, E210, "system__storage_pools_E");
   E208 : Short_Integer; pragma Import (Ada, E208, "system__finalization_masters_E");
   E220 : Short_Integer; pragma Import (Ada, E220, "system__storage_pools__subpools_E");
   E190 : Short_Integer; pragma Import (Ada, E190, "ada__strings__unbounded_E");
   E204 : Short_Integer; pragma Import (Ada, E204, "ada__calendar_E");
   E108 : Short_Integer; pragma Import (Ada, E108, "ada__text_io_E");
   E181 : Short_Integer; pragma Import (Ada, E181, "system__direct_io_E");
   E212 : Short_Integer; pragma Import (Ada, E212, "system__pool_global_E");
   E146 : Short_Integer; pragma Import (Ada, E146, "system__img_llli_E");
   E198 : Short_Integer; pragma Import (Ada, E198, "system__img_lllu_E");
   E143 : Short_Integer; pragma Import (Ada, E143, "system__img_lli_E");
   E184 : Short_Integer; pragma Import (Ada, E184, "system__img_llu_E");
   E176 : Short_Integer; pragma Import (Ada, E176, "bzip2_E");
   E178 : Short_Integer; pragma Import (Ada, E178, "bzip2__decoding_E");
   E183 : Short_Integer; pragma Import (Ada, E183, "lzma__decoding_E");
   E202 : Short_Integer; pragma Import (Ada, E202, "zip_streams_E");
   E188 : Short_Integer; pragma Import (Ada, E188, "zip_E");
   E200 : Short_Integer; pragma Import (Ada, E200, "zip__headers_E");
   E216 : Short_Integer; pragma Import (Ada, E216, "zip__crc_crypto_E");
   E166 : Short_Integer; pragma Import (Ada, E166, "unzip_E");
   E170 : Short_Integer; pragma Import (Ada, E170, "unzip__decompress_E");
   E186 : Short_Integer; pragma Import (Ada, E186, "unzip__decompress__huffman_E");
   E218 : Short_Integer; pragma Import (Ada, E218, "unzip__streams_E");

   Sec_Default_Sized_Stacks : array (1 .. 1) of aliased System.Secondary_Stack.SS_Stack (System.Parameters.Runtime_Default_Sec_Stack_Size);

   Local_Priority_Specific_Dispatching : constant String := "";
   Local_Interrupt_States : constant String := "";

   Is_Elaborated : Boolean := False;

   procedure finalize_library is
   begin
      E218 := E218 - 1;
      declare
         procedure F1;
         pragma Import (Ada, F1, "unzip__streams__finalize_spec");
      begin
         F1;
      end;
      E166 := E166 - 1;
      declare
         procedure F2;
         pragma Import (Ada, F2, "unzip__finalize_spec");
      begin
         F2;
      end;
      E188 := E188 - 1;
      declare
         procedure F3;
         pragma Import (Ada, F3, "zip__finalize_spec");
      begin
         F3;
      end;
      E202 := E202 - 1;
      declare
         procedure F4;
         pragma Import (Ada, F4, "zip_streams__finalize_spec");
      begin
         F4;
      end;
      E212 := E212 - 1;
      declare
         procedure F5;
         pragma Import (Ada, F5, "system__pool_global__finalize_spec");
      begin
         F5;
      end;
      E181 := E181 - 1;
      declare
         procedure F6;
         pragma Import (Ada, F6, "system__direct_io__finalize_spec");
      begin
         F6;
      end;
      E108 := E108 - 1;
      declare
         procedure F7;
         pragma Import (Ada, F7, "ada__text_io__finalize_spec");
      begin
         F7;
      end;
      E190 := E190 - 1;
      declare
         procedure F8;
         pragma Import (Ada, F8, "ada__strings__unbounded__finalize_spec");
      begin
         F8;
      end;
      E220 := E220 - 1;
      declare
         procedure F9;
         pragma Import (Ada, F9, "system__storage_pools__subpools__finalize_spec");
      begin
         F9;
      end;
      E208 := E208 - 1;
      declare
         procedure F10;
         pragma Import (Ada, F10, "system__finalization_masters__finalize_spec");
      begin
         F10;
      end;
      E172 := E172 - 1;
      declare
         procedure F11;
         pragma Import (Ada, F11, "ada__streams__stream_io__finalize_spec");
      begin
         F11;
      end;
      declare
         procedure F12;
         pragma Import (Ada, F12, "system__file_io__finalize_body");
      begin
         E134 := E134 - 1;
         F12;
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
      E012 := E012 + 1;
      Ada.Containers'Elab_Spec;
      E047 := E047 + 1;
      Ada.Io_Exceptions'Elab_Spec;
      E067 := E067 + 1;
      Ada.Numerics'Elab_Spec;
      E027 := E027 + 1;
      Ada.Strings'Elab_Spec;
      E009 := E009 + 1;
      Ada.Strings.Maps'Elab_Spec;
      E099 := E099 + 1;
      Ada.Strings.Maps.Constants'Elab_Spec;
      E102 := E102 + 1;
      Interfaces.C'Elab_Spec;
      E052 := E052 + 1;
      System.Exceptions'Elab_Spec;
      E021 := E021 + 1;
      System.Object_Reader'Elab_Spec;
      E081 := E081 + 1;
      System.Dwarf_Lines'Elab_Spec;
      System.Os_Lib'Elab_Body;
      E072 := E072 + 1;
      System.Soft_Links.Initialize'Elab_Body;
      E095 := E095 + 1;
      E014 := E014 + 1;
      System.Traceback.Symbolic'Elab_Body;
      E046 := E046 + 1;
      System.Img_Int'Elab_Spec;
      E026 := E026 + 1;
      E018 := E018 + 1;
      System.Img_Uns'Elab_Spec;
      E062 := E062 + 1;
      E059 := E059 + 1;
      Ada.Strings.Utf_Encoding'Elab_Spec;
      E114 := E114 + 1;
      Ada.Tags'Elab_Spec;
      Ada.Tags'Elab_Body;
      E122 := E122 + 1;
      Ada.Strings.Text_Buffers'Elab_Spec;
      E112 := E112 + 1;
      Ada.Streams'Elab_Spec;
      E110 := E110 + 1;
      System.File_Control_Block'Elab_Spec;
      E138 := E138 + 1;
      System.Finalization_Root'Elab_Spec;
      E137 := E137 + 1;
      Ada.Finalization'Elab_Spec;
      E135 := E135 + 1;
      System.File_Io'Elab_Body;
      E134 := E134 + 1;
      Ada.Streams.Stream_Io'Elab_Spec;
      E172 := E172 + 1;
      System.Storage_Pools'Elab_Spec;
      E210 := E210 + 1;
      System.Finalization_Masters'Elab_Spec;
      System.Finalization_Masters'Elab_Body;
      E208 := E208 + 1;
      System.Storage_Pools.Subpools'Elab_Spec;
      E220 := E220 + 1;
      Ada.Strings.Unbounded'Elab_Spec;
      E190 := E190 + 1;
      Ada.Calendar'Elab_Spec;
      Ada.Calendar'Elab_Body;
      E204 := E204 + 1;
      Ada.Text_Io'Elab_Spec;
      Ada.Text_Io'Elab_Body;
      E108 := E108 + 1;
      System.Direct_Io'Elab_Spec;
      System.Direct_Io'Elab_Body;
      E181 := E181 + 1;
      System.Pool_Global'Elab_Spec;
      System.Pool_Global'Elab_Body;
      E212 := E212 + 1;
      System.Img_Llli'Elab_Spec;
      E146 := E146 + 1;
      System.Img_Lllu'Elab_Spec;
      E198 := E198 + 1;
      System.Img_Lli'Elab_Spec;
      E143 := E143 + 1;
      System.Img_Llu'Elab_Spec;
      E184 := E184 + 1;
      E176 := E176 + 1;
      E178 := E178 + 1;
      E183 := E183 + 1;
      Zip_Streams'Elab_Spec;
      E202 := E202 + 1;
      Zip'Elab_Spec;
      Zip.Headers'Elab_Spec;
      E200 := E200 + 1;
      E188 := E188 + 1;
      E216 := E216 + 1;
      Unzip'Elab_Spec;
      E166 := E166 + 1;
      Unzip.Decompress.Huffman'Elab_Spec;
      E186 := E186 + 1;
      E170 := E170 + 1;
      Unzip.Streams'Elab_Spec;
      E218 := E218 + 1;
   end adainit;

   procedure Ada_Main_Program;
   pragma Import (Ada, Ada_Main_Program, "_ada_find_zip");

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
   --   /workspaces/bench-source/src/zipada/obj_opt/bzip2.o
   --   /workspaces/bench-source/src/zipada/obj_opt/bzip2-decoding.o
   --   /workspaces/bench-source/src/zipada/obj_opt/lzma.o
   --   /workspaces/bench-source/src/zipada/obj_opt/lzma-decoding.o
   --   /workspaces/bench-source/src/zipada/obj_opt/show_license.o
   --   /workspaces/bench-source/src/zipada/obj_opt/zip_streams.o
   --   /workspaces/bench-source/src/zipada/obj_opt/zip-headers.o
   --   /workspaces/bench-source/src/zipada/obj_opt/zip.o
   --   /workspaces/bench-source/src/zipada/obj_opt/zip-crc_crypto.o
   --   /workspaces/bench-source/src/zipada/obj_opt/unzip.o
   --   /workspaces/bench-source/src/zipada/obj_opt/unzip-decompress-huffman.o
   --   /workspaces/bench-source/src/zipada/obj_opt/unzip-decompress.o
   --   /workspaces/bench-source/src/zipada/obj_opt/unzip-streams.o
   --   /workspaces/bench-source/src/zipada/obj_opt/find_zip.o
   --   -L/workspaces/bench-source/src/zipada/obj_opt/
   --   -L/workspaces/bench-source/src/zipada/obj_opt/
   --   -L/usr/gnat/lib/gcc/x86_64-pc-linux-gnu/12.2.1/adalib/
   --   -static
   --   -lgnat
   --   -ldl
--  END Object file/option list   

end ada_main;
