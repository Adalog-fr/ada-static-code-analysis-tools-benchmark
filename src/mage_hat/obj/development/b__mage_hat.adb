pragma Warnings (Off);
pragma Ada_95;
pragma Source_File_Name (ada_main, Spec_File_Name => "b__mage_hat.ads");
pragma Source_File_Name (ada_main, Body_File_Name => "b__mage_hat.adb");
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
   E224 : Short_Integer; pragma Import (Ada, E224, "ada__assertions_E");
   E137 : Short_Integer; pragma Import (Ada, E137, "ada__strings__utf_encoding_E");
   E145 : Short_Integer; pragma Import (Ada, E145, "ada__tags_E");
   E135 : Short_Integer; pragma Import (Ada, E135, "ada__strings__text_buffers_E");
   E191 : Short_Integer; pragma Import (Ada, E191, "interfaces__c__strings_E");
   E153 : Short_Integer; pragma Import (Ada, E153, "ada__streams_E");
   E165 : Short_Integer; pragma Import (Ada, E165, "system__file_control_block_E");
   E164 : Short_Integer; pragma Import (Ada, E164, "system__finalization_root_E");
   E162 : Short_Integer; pragma Import (Ada, E162, "ada__finalization_E");
   E161 : Short_Integer; pragma Import (Ada, E161, "system__file_io_E");
   E199 : Short_Integer; pragma Import (Ada, E199, "system__storage_pools_E");
   E197 : Short_Integer; pragma Import (Ada, E197, "system__finalization_masters_E");
   E201 : Short_Integer; pragma Import (Ada, E201, "system__storage_pools__subpools_E");
   E169 : Short_Integer; pragma Import (Ada, E169, "ada__strings__unbounded_E");
   E121 : Short_Integer; pragma Import (Ada, E121, "system__task_info_E");
   E151 : Short_Integer; pragma Import (Ada, E151, "ada__text_io_E");
   E205 : Short_Integer; pragma Import (Ada, E205, "system__pool_global_E");
   E128 : Short_Integer; pragma Import (Ada, E128, "system__img_lli_E");
   E113 : Short_Integer; pragma Import (Ada, E113, "system__task_primitives__operations_E");
   E006 : Short_Integer; pragma Import (Ada, E006, "ada__real_time_E");
   E189 : Short_Integer; pragma Import (Ada, E189, "sdl__video_E");
   E195 : Short_Integer; pragma Import (Ada, E195, "sdl__video__palettes_E");
   E220 : Short_Integer; pragma Import (Ada, E220, "sdl__video__pixel_formats_E");
   E225 : Short_Integer; pragma Import (Ada, E225, "sdl__video__pixels_E");
   E213 : Short_Integer; pragma Import (Ada, E213, "sdl__video__rectangles_E");
   E231 : Short_Integer; pragma Import (Ada, E231, "sdl__video__surfaces_E");
   E218 : Short_Integer; pragma Import (Ada, E218, "sdl__video__textures_E");
   E227 : Short_Integer; pragma Import (Ada, E227, "sdl__video__windows_E");
   E249 : Short_Integer; pragma Import (Ada, E249, "sdl__events__events_E");
   E215 : Short_Integer; pragma Import (Ada, E215, "sdl__video__renderers_E");
   E167 : Short_Integer; pragma Import (Ada, E167, "mage_E");
   E209 : Short_Integer; pragma Import (Ada, E209, "mage__draw_E");
   E244 : Short_Integer; pragma Import (Ada, E244, "mage__event_E");

   Sec_Default_Sized_Stacks : array (1 .. 1) of aliased System.Secondary_Stack.SS_Stack (System.Parameters.Runtime_Default_Sec_Stack_Size);

   Local_Priority_Specific_Dispatching : constant String := "";
   Local_Interrupt_States : constant String := "";

   Is_Elaborated : Boolean := False;

   procedure finalize_library is
   begin
      declare
         procedure F1;
         pragma Import (Ada, F1, "mage__event__finalize_body");
      begin
         E244 := E244 - 1;
         F1;
      end;
      declare
         procedure F2;
         pragma Import (Ada, F2, "mage__draw__finalize_body");
      begin
         E209 := E209 - 1;
         F2;
      end;
      E215 := E215 - 1;
      declare
         procedure F3;
         pragma Import (Ada, F3, "sdl__video__renderers__finalize_spec");
      begin
         F3;
      end;
      E227 := E227 - 1;
      declare
         procedure F4;
         pragma Import (Ada, F4, "sdl__video__windows__finalize_spec");
      begin
         F4;
      end;
      E218 := E218 - 1;
      declare
         procedure F5;
         pragma Import (Ada, F5, "sdl__video__textures__finalize_spec");
      begin
         F5;
      end;
      E231 := E231 - 1;
      declare
         procedure F6;
         pragma Import (Ada, F6, "sdl__video__surfaces__finalize_spec");
      begin
         F6;
      end;
      declare
         procedure F7;
         pragma Import (Ada, F7, "sdl__video__palettes__finalize_body");
      begin
         E195 := E195 - 1;
         F7;
      end;
      declare
         procedure F8;
         pragma Import (Ada, F8, "sdl__video__palettes__finalize_spec");
      begin
         F8;
      end;
      E205 := E205 - 1;
      declare
         procedure F9;
         pragma Import (Ada, F9, "system__pool_global__finalize_spec");
      begin
         F9;
      end;
      E151 := E151 - 1;
      declare
         procedure F10;
         pragma Import (Ada, F10, "ada__text_io__finalize_spec");
      begin
         F10;
      end;
      E169 := E169 - 1;
      declare
         procedure F11;
         pragma Import (Ada, F11, "ada__strings__unbounded__finalize_spec");
      begin
         F11;
      end;
      E201 := E201 - 1;
      declare
         procedure F12;
         pragma Import (Ada, F12, "system__storage_pools__subpools__finalize_spec");
      begin
         F12;
      end;
      E197 := E197 - 1;
      declare
         procedure F13;
         pragma Import (Ada, F13, "system__finalization_masters__finalize_spec");
      begin
         F13;
      end;
      declare
         procedure F14;
         pragma Import (Ada, F14, "system__file_io__finalize_body");
      begin
         E161 := E161 - 1;
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
      Ada.Assertions'Elab_Spec;
      E224 := E224 + 1;
      Ada.Strings.Utf_Encoding'Elab_Spec;
      E137 := E137 + 1;
      Ada.Tags'Elab_Spec;
      Ada.Tags'Elab_Body;
      E145 := E145 + 1;
      Ada.Strings.Text_Buffers'Elab_Spec;
      E135 := E135 + 1;
      Interfaces.C.Strings'Elab_Spec;
      E191 := E191 + 1;
      Ada.Streams'Elab_Spec;
      E153 := E153 + 1;
      System.File_Control_Block'Elab_Spec;
      E165 := E165 + 1;
      System.Finalization_Root'Elab_Spec;
      E164 := E164 + 1;
      Ada.Finalization'Elab_Spec;
      E162 := E162 + 1;
      System.File_Io'Elab_Body;
      E161 := E161 + 1;
      System.Storage_Pools'Elab_Spec;
      E199 := E199 + 1;
      System.Finalization_Masters'Elab_Spec;
      System.Finalization_Masters'Elab_Body;
      E197 := E197 + 1;
      System.Storage_Pools.Subpools'Elab_Spec;
      E201 := E201 + 1;
      Ada.Strings.Unbounded'Elab_Spec;
      E169 := E169 + 1;
      System.Task_Info'Elab_Spec;
      E121 := E121 + 1;
      Ada.Text_Io'Elab_Spec;
      Ada.Text_Io'Elab_Body;
      E151 := E151 + 1;
      System.Pool_Global'Elab_Spec;
      System.Pool_Global'Elab_Body;
      E205 := E205 + 1;
      System.Img_Lli'Elab_Spec;
      E128 := E128 + 1;
      System.Task_Primitives.Operations'Elab_Body;
      E113 := E113 + 1;
      Ada.Real_Time'Elab_Spec;
      Ada.Real_Time'Elab_Body;
      E006 := E006 + 1;
      SDL.VIDEO'ELAB_SPEC;
      E189 := E189 + 1;
      SDL.VIDEO.PALETTES'ELAB_SPEC;
      SDL.VIDEO.PALETTES'ELAB_BODY;
      E195 := E195 + 1;
      SDL.VIDEO.PIXEL_FORMATS'ELAB_SPEC;
      E220 := E220 + 1;
      SDL.VIDEO.PIXELS'ELAB_SPEC;
      E225 := E225 + 1;
      SDL.VIDEO.RECTANGLES'ELAB_SPEC;
      E213 := E213 + 1;
      SDL.VIDEO.SURFACES'ELAB_SPEC;
      E231 := E231 + 1;
      SDL.VIDEO.TEXTURES'ELAB_SPEC;
      E218 := E218 + 1;
      SDL.VIDEO.WINDOWS'ELAB_SPEC;
      E227 := E227 + 1;
      SDL.EVENTS.EVENTS'ELAB_SPEC;
      E249 := E249 + 1;
      SDL.VIDEO.RENDERERS'ELAB_SPEC;
      E215 := E215 + 1;
      Mage'Elab_Spec;
      Mage'Elab_Body;
      E167 := E167 + 1;
      Mage.Draw'Elab_Spec;
      Mage.Draw'Elab_Body;
      E209 := E209 + 1;
      Mage.Event'Elab_Body;
      E244 := E244 + 1;
   end adainit;

   procedure Ada_Main_Program;
   pragma Import (Ada, Ada_Main_Program, "_ada_mage_hat");

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
   --   /workspaces/bench-source/src/mage_hat/obj/development/mage_hat.o
   --   -L/workspaces/bench-source/src/mage_hat/obj/development/
   --   -L/workspaces/bench-source/src/mage_hat/obj/development/
   --   -L/workspaces/bench-source/src/mage/lib/
   --   -L/workspaces/bench-source/src/sdlada/build/gnat/gen/debug/lib/
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
