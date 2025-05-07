pragma Warnings (Off);
pragma Ada_95;
pragma Source_File_Name (ada_main, Spec_File_Name => "b__unzipada.ads");
pragma Source_File_Name (ada_main, Body_File_Name => "b__unzipada.adb");
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
   E119 : Short_Integer; pragma Import (Ada, E119, "ada__strings__utf_encoding_E");
   E127 : Short_Integer; pragma Import (Ada, E127, "ada__tags_E");
   E117 : Short_Integer; pragma Import (Ada, E117, "ada__strings__text_buffers_E");
   E115 : Short_Integer; pragma Import (Ada, E115, "ada__streams_E");
   E168 : Short_Integer; pragma Import (Ada, E168, "system__file_control_block_E");
   E137 : Short_Integer; pragma Import (Ada, E137, "system__finalization_root_E");
   E113 : Short_Integer; pragma Import (Ada, E113, "ada__finalization_E");
   E165 : Short_Integer; pragma Import (Ada, E165, "system__file_io_E");
   E223 : Short_Integer; pragma Import (Ada, E223, "ada__streams__stream_io_E");
   E172 : Short_Integer; pragma Import (Ada, E172, "system__storage_pools_E");
   E170 : Short_Integer; pragma Import (Ada, E170, "system__finalization_masters_E");
   E176 : Short_Integer; pragma Import (Ada, E176, "system__storage_pools__subpools_E");
   E149 : Short_Integer; pragma Import (Ada, E149, "ada__strings__unbounded_E");
   E006 : Short_Integer; pragma Import (Ada, E006, "ada__calendar_E");
   E182 : Short_Integer; pragma Import (Ada, E182, "ada__text_io_E");
   E232 : Short_Integer; pragma Import (Ada, E232, "system__direct_io_E");
   E244 : Short_Integer; pragma Import (Ada, E244, "system__pool_global_E");
   E174 : Short_Integer; pragma Import (Ada, E174, "system__regexp_E");
   E110 : Short_Integer; pragma Import (Ada, E110, "ada__directories_E");
   E208 : Short_Integer; pragma Import (Ada, E208, "system__img_llli_E");
   E212 : Short_Integer; pragma Import (Ada, E212, "system__img_lllu_E");
   E206 : Short_Integer; pragma Import (Ada, E206, "system__img_lli_E");
   E192 : Short_Integer; pragma Import (Ada, E192, "system__img_llu_E");
   E227 : Short_Integer; pragma Import (Ada, E227, "bzip2_E");
   E229 : Short_Integer; pragma Import (Ada, E229, "bzip2__decoding_E");
   E234 : Short_Integer; pragma Import (Ada, E234, "lzma__decoding_E");
   E242 : Short_Integer; pragma Import (Ada, E242, "zip_streams_E");
   E238 : Short_Integer; pragma Import (Ada, E238, "zip_E");
   E240 : Short_Integer; pragma Import (Ada, E240, "zip__headers_E");
   E248 : Short_Integer; pragma Import (Ada, E248, "zip__crc_crypto_E");
   E217 : Short_Integer; pragma Import (Ada, E217, "unzip_E");
   E221 : Short_Integer; pragma Import (Ada, E221, "unzip__decompress_E");
   E236 : Short_Integer; pragma Import (Ada, E236, "unzip__decompress__huffman_E");
   E250 : Short_Integer; pragma Import (Ada, E250, "zip_console_io_E");

   Sec_Default_Sized_Stacks : array (1 .. 1) of aliased System.Secondary_Stack.SS_Stack (System.Parameters.Runtime_Default_Sec_Stack_Size);

   Local_Priority_Specific_Dispatching : constant String := "";
   Local_Interrupt_States : constant String := "";

   Is_Elaborated : Boolean := False;

   procedure finalize_library is
   begin
      E217 := E217 - 1;
      declare
         procedure F1;
         pragma Import (Ada, F1, "unzip__finalize_spec");
      begin
         F1;
      end;
      E238 := E238 - 1;
      declare
         procedure F2;
         pragma Import (Ada, F2, "zip__finalize_spec");
      begin
         F2;
      end;
      E242 := E242 - 1;
      declare
         procedure F3;
         pragma Import (Ada, F3, "zip_streams__finalize_spec");
      begin
         F3;
      end;
      declare
         procedure F4;
         pragma Import (Ada, F4, "ada__directories__finalize_body");
      begin
         E110 := E110 - 1;
         F4;
      end;
      declare
         procedure F5;
         pragma Import (Ada, F5, "ada__directories__finalize_spec");
      begin
         F5;
      end;
      E174 := E174 - 1;
      declare
         procedure F6;
         pragma Import (Ada, F6, "system__regexp__finalize_spec");
      begin
         F6;
      end;
      E244 := E244 - 1;
      declare
         procedure F7;
         pragma Import (Ada, F7, "system__pool_global__finalize_spec");
      begin
         F7;
      end;
      E232 := E232 - 1;
      declare
         procedure F8;
         pragma Import (Ada, F8, "system__direct_io__finalize_spec");
      begin
         F8;
      end;
      E182 := E182 - 1;
      declare
         procedure F9;
         pragma Import (Ada, F9, "ada__text_io__finalize_spec");
      begin
         F9;
      end;
      E149 := E149 - 1;
      declare
         procedure F10;
         pragma Import (Ada, F10, "ada__strings__unbounded__finalize_spec");
      begin
         F10;
      end;
      E176 := E176 - 1;
      declare
         procedure F11;
         pragma Import (Ada, F11, "system__storage_pools__subpools__finalize_spec");
      begin
         F11;
      end;
      E170 := E170 - 1;
      declare
         procedure F12;
         pragma Import (Ada, F12, "system__finalization_masters__finalize_spec");
      begin
         F12;
      end;
      E223 := E223 - 1;
      declare
         procedure F13;
         pragma Import (Ada, F13, "ada__streams__stream_io__finalize_spec");
      begin
         F13;
      end;
      declare
         procedure F14;
         pragma Import (Ada, F14, "system__file_io__finalize_body");
      begin
         E165 := E165 - 1;
         F14;
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
      E119 := E119 + 1;
      Ada.Tags'Elab_Spec;
      Ada.Tags'Elab_Body;
      E127 := E127 + 1;
      Ada.Strings.Text_Buffers'Elab_Spec;
      E117 := E117 + 1;
      Ada.Streams'Elab_Spec;
      E115 := E115 + 1;
      System.File_Control_Block'Elab_Spec;
      E168 := E168 + 1;
      System.Finalization_Root'Elab_Spec;
      E137 := E137 + 1;
      Ada.Finalization'Elab_Spec;
      E113 := E113 + 1;
      System.File_Io'Elab_Body;
      E165 := E165 + 1;
      Ada.Streams.Stream_Io'Elab_Spec;
      E223 := E223 + 1;
      System.Storage_Pools'Elab_Spec;
      E172 := E172 + 1;
      System.Finalization_Masters'Elab_Spec;
      System.Finalization_Masters'Elab_Body;
      E170 := E170 + 1;
      System.Storage_Pools.Subpools'Elab_Spec;
      E176 := E176 + 1;
      Ada.Strings.Unbounded'Elab_Spec;
      E149 := E149 + 1;
      Ada.Calendar'Elab_Spec;
      Ada.Calendar'Elab_Body;
      E006 := E006 + 1;
      Ada.Text_Io'Elab_Spec;
      Ada.Text_Io'Elab_Body;
      E182 := E182 + 1;
      System.Direct_Io'Elab_Spec;
      System.Direct_Io'Elab_Body;
      E232 := E232 + 1;
      System.Pool_Global'Elab_Spec;
      System.Pool_Global'Elab_Body;
      E244 := E244 + 1;
      System.Regexp'Elab_Spec;
      System.Regexp'Elab_Body;
      E174 := E174 + 1;
      Ada.Directories'Elab_Spec;
      Ada.Directories'Elab_Body;
      E110 := E110 + 1;
      System.Img_Llli'Elab_Spec;
      E208 := E208 + 1;
      System.Img_Lllu'Elab_Spec;
      E212 := E212 + 1;
      System.Img_Lli'Elab_Spec;
      E206 := E206 + 1;
      System.Img_Llu'Elab_Spec;
      E192 := E192 + 1;
      E227 := E227 + 1;
      E229 := E229 + 1;
      E234 := E234 + 1;
      Zip_Streams'Elab_Spec;
      E242 := E242 + 1;
      Zip'Elab_Spec;
      Zip.Headers'Elab_Spec;
      E240 := E240 + 1;
      E238 := E238 + 1;
      E248 := E248 + 1;
      Unzip'Elab_Spec;
      E217 := E217 + 1;
      Unzip.Decompress.Huffman'Elab_Spec;
      E236 := E236 + 1;
      E221 := E221 + 1;
      E250 := E250 + 1;
   end adainit;

   procedure Ada_Main_Program;
   pragma Import (Ada, Ada_Main_Program, "_ada_unzipada");

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
   --   /workspaces/bench-source/src/zipada/obj_opt/zip_console_io.o
   --   /workspaces/bench-source/src/zipada/obj_opt/unzipada.o
   --   -L/workspaces/bench-source/src/zipada/obj_opt/
   --   -L/workspaces/bench-source/src/zipada/obj_opt/
   --   -L/usr/gnat/lib/gcc/x86_64-pc-linux-gnu/12.2.1/adalib/
   --   -static
   --   -lgnat
   --   -ldl
--  END Object file/option list   

end ada_main;
