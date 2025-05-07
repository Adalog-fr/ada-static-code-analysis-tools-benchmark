pragma Warnings (Off);
pragma Ada_95;
pragma Source_File_Name (ada_main, Spec_File_Name => "b__rezip.ads");
pragma Source_File_Name (ada_main, Body_File_Name => "b__rezip.adb");
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
   E108 : Short_Integer; pragma Import (Ada, E108, "ada__strings__utf_encoding_E");
   E116 : Short_Integer; pragma Import (Ada, E116, "ada__tags_E");
   E106 : Short_Integer; pragma Import (Ada, E106, "ada__strings__text_buffers_E");
   E297 : Short_Integer; pragma Import (Ada, E297, "gnat_E");
   E136 : Short_Integer; pragma Import (Ada, E136, "ada__streams_E");
   E154 : Short_Integer; pragma Import (Ada, E154, "system__file_control_block_E");
   E138 : Short_Integer; pragma Import (Ada, E138, "system__finalization_root_E");
   E134 : Short_Integer; pragma Import (Ada, E134, "ada__finalization_E");
   E153 : Short_Integer; pragma Import (Ada, E153, "system__file_io_E");
   E183 : Short_Integer; pragma Import (Ada, E183, "ada__streams__stream_io_E");
   E175 : Short_Integer; pragma Import (Ada, E175, "system__storage_pools_E");
   E173 : Short_Integer; pragma Import (Ada, E173, "system__finalization_masters_E");
   E220 : Short_Integer; pragma Import (Ada, E220, "system__storage_pools__subpools_E");
   E122 : Short_Integer; pragma Import (Ada, E122, "ada__strings__unbounded_E");
   E208 : Short_Integer; pragma Import (Ada, E208, "ada__calendar_E");
   E149 : Short_Integer; pragma Import (Ada, E149, "ada__text_io_E");
   E192 : Short_Integer; pragma Import (Ada, E192, "system__direct_io_E");
   E212 : Short_Integer; pragma Import (Ada, E212, "system__pool_global_E");
   E267 : Short_Integer; pragma Import (Ada, E267, "system__random_seed_E");
   E236 : Short_Integer; pragma Import (Ada, E236, "system__regexp_E");
   E226 : Short_Integer; pragma Import (Ada, E226, "ada__directories_E");
   E165 : Short_Integer; pragma Import (Ada, E165, "system__img_llli_E");
   E202 : Short_Integer; pragma Import (Ada, E202, "system__img_lllu_E");
   E162 : Short_Integer; pragma Import (Ada, E162, "system__img_lli_E");
   E195 : Short_Integer; pragma Import (Ada, E195, "system__img_llu_E");
   E187 : Short_Integer; pragma Import (Ada, E187, "bzip2_E");
   E189 : Short_Integer; pragma Import (Ada, E189, "bzip2__decoding_E");
   E269 : Short_Integer; pragma Import (Ada, E269, "dual_io_E");
   E296 : Short_Integer; pragma Import (Ada, E296, "flexible_temp_files_E");
   E304 : Short_Integer; pragma Import (Ada, E304, "length_limited_huffman_code_lengths_E");
   E306 : Short_Integer; pragma Import (Ada, E306, "lz77_E");
   E194 : Short_Integer; pragma Import (Ada, E194, "lzma__decoding_E");
   E310 : Short_Integer; pragma Import (Ada, E310, "lzma__encoding_E");
   E206 : Short_Integer; pragma Import (Ada, E206, "zip_streams_E");
   E199 : Short_Integer; pragma Import (Ada, E199, "zip_E");
   E204 : Short_Integer; pragma Import (Ada, E204, "zip__headers_E");
   E216 : Short_Integer; pragma Import (Ada, E216, "zip__crc_crypto_E");
   E177 : Short_Integer; pragma Import (Ada, E177, "unzip_E");
   E181 : Short_Integer; pragma Import (Ada, E181, "unzip__decompress_E");
   E197 : Short_Integer; pragma Import (Ada, E197, "unzip__decompress__huffman_E");
   E218 : Short_Integer; pragma Import (Ada, E218, "unzip__streams_E");
   E300 : Short_Integer; pragma Import (Ada, E300, "zip__compress_E");
   E302 : Short_Integer; pragma Import (Ada, E302, "zip__compress__deflate_E");
   E308 : Short_Integer; pragma Import (Ada, E308, "zip__compress__lzma_e_E");
   E312 : Short_Integer; pragma Import (Ada, E312, "zip__compress__reduce_E");
   E314 : Short_Integer; pragma Import (Ada, E314, "zip__compress__shrink_E");
   E316 : Short_Integer; pragma Import (Ada, E316, "zip__create_E");
   E323 : Short_Integer; pragma Import (Ada, E323, "zip_console_io_E");
   E224 : Short_Integer; pragma Import (Ada, E224, "rezip_lib_E");

   Sec_Default_Sized_Stacks : array (1 .. 1) of aliased System.Secondary_Stack.SS_Stack (System.Parameters.Runtime_Default_Sec_Stack_Size);

   Local_Priority_Specific_Dispatching : constant String := "";
   Local_Interrupt_States : constant String := "";

   Is_Elaborated : Boolean := False;

   procedure finalize_library is
   begin
      E316 := E316 - 1;
      declare
         procedure F1;
         pragma Import (Ada, F1, "zip__create__finalize_spec");
      begin
         F1;
      end;
      E218 := E218 - 1;
      declare
         procedure F2;
         pragma Import (Ada, F2, "unzip__streams__finalize_spec");
      begin
         F2;
      end;
      E177 := E177 - 1;
      declare
         procedure F3;
         pragma Import (Ada, F3, "unzip__finalize_spec");
      begin
         F3;
      end;
      E199 := E199 - 1;
      declare
         procedure F4;
         pragma Import (Ada, F4, "zip__finalize_spec");
      begin
         F4;
      end;
      E206 := E206 - 1;
      declare
         procedure F5;
         pragma Import (Ada, F5, "zip_streams__finalize_spec");
      begin
         F5;
      end;
      declare
         procedure F6;
         pragma Import (Ada, F6, "ada__directories__finalize_body");
      begin
         E226 := E226 - 1;
         F6;
      end;
      declare
         procedure F7;
         pragma Import (Ada, F7, "ada__directories__finalize_spec");
      begin
         F7;
      end;
      E236 := E236 - 1;
      declare
         procedure F8;
         pragma Import (Ada, F8, "system__regexp__finalize_spec");
      begin
         F8;
      end;
      E212 := E212 - 1;
      declare
         procedure F9;
         pragma Import (Ada, F9, "system__pool_global__finalize_spec");
      begin
         F9;
      end;
      E192 := E192 - 1;
      declare
         procedure F10;
         pragma Import (Ada, F10, "system__direct_io__finalize_spec");
      begin
         F10;
      end;
      E149 := E149 - 1;
      declare
         procedure F11;
         pragma Import (Ada, F11, "ada__text_io__finalize_spec");
      begin
         F11;
      end;
      E122 := E122 - 1;
      declare
         procedure F12;
         pragma Import (Ada, F12, "ada__strings__unbounded__finalize_spec");
      begin
         F12;
      end;
      E220 := E220 - 1;
      declare
         procedure F13;
         pragma Import (Ada, F13, "system__storage_pools__subpools__finalize_spec");
      begin
         F13;
      end;
      E173 := E173 - 1;
      declare
         procedure F14;
         pragma Import (Ada, F14, "system__finalization_masters__finalize_spec");
      begin
         F14;
      end;
      E183 := E183 - 1;
      declare
         procedure F15;
         pragma Import (Ada, F15, "ada__streams__stream_io__finalize_spec");
      begin
         F15;
      end;
      declare
         procedure F16;
         pragma Import (Ada, F16, "system__file_io__finalize_body");
      begin
         E153 := E153 - 1;
         F16;
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
      E108 := E108 + 1;
      Ada.Tags'Elab_Spec;
      Ada.Tags'Elab_Body;
      E116 := E116 + 1;
      Ada.Strings.Text_Buffers'Elab_Spec;
      E106 := E106 + 1;
      Gnat'Elab_Spec;
      E297 := E297 + 1;
      Ada.Streams'Elab_Spec;
      E136 := E136 + 1;
      System.File_Control_Block'Elab_Spec;
      E154 := E154 + 1;
      System.Finalization_Root'Elab_Spec;
      E138 := E138 + 1;
      Ada.Finalization'Elab_Spec;
      E134 := E134 + 1;
      System.File_Io'Elab_Body;
      E153 := E153 + 1;
      Ada.Streams.Stream_Io'Elab_Spec;
      E183 := E183 + 1;
      System.Storage_Pools'Elab_Spec;
      E175 := E175 + 1;
      System.Finalization_Masters'Elab_Spec;
      System.Finalization_Masters'Elab_Body;
      E173 := E173 + 1;
      System.Storage_Pools.Subpools'Elab_Spec;
      E220 := E220 + 1;
      Ada.Strings.Unbounded'Elab_Spec;
      E122 := E122 + 1;
      Ada.Calendar'Elab_Spec;
      Ada.Calendar'Elab_Body;
      E208 := E208 + 1;
      Ada.Text_Io'Elab_Spec;
      Ada.Text_Io'Elab_Body;
      E149 := E149 + 1;
      System.Direct_Io'Elab_Spec;
      System.Direct_Io'Elab_Body;
      E192 := E192 + 1;
      System.Pool_Global'Elab_Spec;
      System.Pool_Global'Elab_Body;
      E212 := E212 + 1;
      System.Random_Seed'Elab_Body;
      E267 := E267 + 1;
      System.Regexp'Elab_Spec;
      System.Regexp'Elab_Body;
      E236 := E236 + 1;
      Ada.Directories'Elab_Spec;
      Ada.Directories'Elab_Body;
      E226 := E226 + 1;
      System.Img_Llli'Elab_Spec;
      E165 := E165 + 1;
      System.Img_Lllu'Elab_Spec;
      E202 := E202 + 1;
      System.Img_Lli'Elab_Spec;
      E162 := E162 + 1;
      System.Img_Llu'Elab_Spec;
      E195 := E195 + 1;
      E187 := E187 + 1;
      E189 := E189 + 1;
      Dual_Io'Elab_Spec;
      E269 := E269 + 1;
      E296 := E296 + 1;
      E304 := E304 + 1;
      E306 := E306 + 1;
      E194 := E194 + 1;
      E310 := E310 + 1;
      Zip_Streams'Elab_Spec;
      E206 := E206 + 1;
      Zip'Elab_Spec;
      Zip.Headers'Elab_Spec;
      E204 := E204 + 1;
      E199 := E199 + 1;
      E216 := E216 + 1;
      Unzip'Elab_Spec;
      E177 := E177 + 1;
      Unzip.Decompress.Huffman'Elab_Spec;
      E197 := E197 + 1;
      E181 := E181 + 1;
      Unzip.Streams'Elab_Spec;
      E218 := E218 + 1;
      Zip.Compress'Elab_Spec;
      E302 := E302 + 1;
      E308 := E308 + 1;
      E312 := E312 + 1;
      E314 := E314 + 1;
      Zip.Create'Elab_Spec;
      E316 := E316 + 1;
      E300 := E300 + 1;
      E323 := E323 + 1;
      Rezip_Lib'Elab_Body;
      E224 := E224 + 1;
   end adainit;

   procedure Ada_Main_Program;
   pragma Import (Ada, Ada_Main_Program, "_ada_rezip");

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
   --   /workspaces/bench-source/src/zipada/obj_opt/dual_io.o
   --   /workspaces/bench-source/src/zipada/obj_opt/flexible_temp_files.o
   --   /workspaces/bench-source/src/zipada/obj_opt/length_limited_huffman_code_lengths.o
   --   /workspaces/bench-source/src/zipada/obj_opt/lz77.o
   --   /workspaces/bench-source/src/zipada/obj_opt/lzma.o
   --   /workspaces/bench-source/src/zipada/obj_opt/lzma-decoding.o
   --   /workspaces/bench-source/src/zipada/obj_opt/lzma-encoding.o
   --   /workspaces/bench-source/src/zipada/obj_opt/show_license.o
   --   /workspaces/bench-source/src/zipada/obj_opt/zip_streams.o
   --   /workspaces/bench-source/src/zipada/obj_opt/zip-headers.o
   --   /workspaces/bench-source/src/zipada/obj_opt/zip.o
   --   /workspaces/bench-source/src/zipada/obj_opt/zip-crc_crypto.o
   --   /workspaces/bench-source/src/zipada/obj_opt/unzip.o
   --   /workspaces/bench-source/src/zipada/obj_opt/unzip-decompress-huffman.o
   --   /workspaces/bench-source/src/zipada/obj_opt/unzip-decompress.o
   --   /workspaces/bench-source/src/zipada/obj_opt/unzip-streams.o
   --   /workspaces/bench-source/src/zipada/obj_opt/comp_zip_prc.o
   --   /workspaces/bench-source/src/zipada/obj_opt/zip-compress-deflate.o
   --   /workspaces/bench-source/src/zipada/obj_opt/zip-compress-lzma_e.o
   --   /workspaces/bench-source/src/zipada/obj_opt/zip-compress-reduce.o
   --   /workspaces/bench-source/src/zipada/obj_opt/zip-compress-shrink.o
   --   /workspaces/bench-source/src/zipada/obj_opt/zip-create.o
   --   /workspaces/bench-source/src/zipada/obj_opt/zip-compress.o
   --   /workspaces/bench-source/src/zipada/obj_opt/zip_console_io.o
   --   /workspaces/bench-source/src/zipada/obj_opt/rezip_lib.o
   --   /workspaces/bench-source/src/zipada/obj_opt/rezip.o
   --   -L/workspaces/bench-source/src/zipada/obj_opt/
   --   -L/workspaces/bench-source/src/zipada/obj_opt/
   --   -L/usr/gnat/lib/gcc/x86_64-pc-linux-gnu/12.2.1/adalib/
   --   -static
   --   -lgnat
   --   -lm
   --   -ldl
--  END Object file/option list   

end ada_main;
