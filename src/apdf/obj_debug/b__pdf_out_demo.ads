pragma Warnings (Off);
pragma Ada_95;
with System;
with System.Scalar_Values;
with System.Parameters;
with System.Secondary_Stack;
package ada_main is

   gnat_argc : Integer;
   gnat_argv : System.Address;
   gnat_envp : System.Address;

   pragma Import (C, gnat_argc);
   pragma Import (C, gnat_argv);
   pragma Import (C, gnat_envp);

   gnat_exit_status : Integer;
   pragma Import (C, gnat_exit_status);

   GNAT_Version : constant String :=
                    "GNAT Version: Pro 24.0w (20230301-122)" & ASCII.NUL;
   pragma Export (C, GNAT_Version, "__gnat_version");

   GNAT_Version_Address : constant System.Address := GNAT_Version'Address;
   pragma Export (C, GNAT_Version_Address, "__gnat_version_address");

   Ada_Main_Program_Name : constant String := "_ada_pdf_out_demo" & ASCII.NUL;
   pragma Export (C, Ada_Main_Program_Name, "__gnat_ada_main_program_name");

   procedure adainit;
   pragma Export (C, adainit, "adainit");

   procedure adafinal;
   pragma Export (C, adafinal, "adafinal");

   function main
     (argc : Integer;
      argv : System.Address;
      envp : System.Address)
      return Integer;
   pragma Export (C, main, "main");

   type Version_32 is mod 2 ** 32;
   u00001 : constant Version_32 := 16#7b2fce52#;
   pragma Export (C, u00001, "pdf_out_demoB");
   u00002 : constant Version_32 := 16#7320ff5f#;
   pragma Export (C, u00002, "system__standard_libraryB");
   u00003 : constant Version_32 := 16#50630821#;
   pragma Export (C, u00003, "system__standard_libraryS");
   u00004 : constant Version_32 := 16#76789da1#;
   pragma Export (C, u00004, "adaS");
   u00005 : constant Version_32 := 16#e9d77c55#;
   pragma Export (C, u00005, "ada__exceptionsB");
   u00006 : constant Version_32 := 16#1e7524b5#;
   pragma Export (C, u00006, "ada__exceptionsS");
   u00007 : constant Version_32 := 16#0740df23#;
   pragma Export (C, u00007, "ada__exceptions__last_chance_handlerB");
   u00008 : constant Version_32 := 16#6dc27684#;
   pragma Export (C, u00008, "ada__exceptions__last_chance_handlerS");
   u00009 : constant Version_32 := 16#426dafb8#;
   pragma Export (C, u00009, "systemS");
   u00010 : constant Version_32 := 16#fd5f5f4c#;
   pragma Export (C, u00010, "system__soft_linksB");
   u00011 : constant Version_32 := 16#3ff0395b#;
   pragma Export (C, u00011, "system__soft_linksS");
   u00012 : constant Version_32 := 16#49d1641b#;
   pragma Export (C, u00012, "system__secondary_stackB");
   u00013 : constant Version_32 := 16#f738e7ab#;
   pragma Export (C, u00013, "system__secondary_stackS");
   u00014 : constant Version_32 := 16#821dff88#;
   pragma Export (C, u00014, "system__parametersB");
   u00015 : constant Version_32 := 16#96f90b1e#;
   pragma Export (C, u00015, "system__parametersS");
   u00016 : constant Version_32 := 16#742e3af8#;
   pragma Export (C, u00016, "system__storage_elementsS");
   u00017 : constant Version_32 := 16#0286ce9f#;
   pragma Export (C, u00017, "system__soft_links__initializeB");
   u00018 : constant Version_32 := 16#2ed17187#;
   pragma Export (C, u00018, "system__soft_links__initializeS");
   u00019 : constant Version_32 := 16#8599b27b#;
   pragma Export (C, u00019, "system__stack_checkingB");
   u00020 : constant Version_32 := 16#7faffb06#;
   pragma Export (C, u00020, "system__stack_checkingS");
   u00021 : constant Version_32 := 16#c71e6c8a#;
   pragma Export (C, u00021, "system__exception_tableB");
   u00022 : constant Version_32 := 16#cf46d9a1#;
   pragma Export (C, u00022, "system__exception_tableS");
   u00023 : constant Version_32 := 16#70c8108a#;
   pragma Export (C, u00023, "system__exceptionsS");
   u00024 : constant Version_32 := 16#69416224#;
   pragma Export (C, u00024, "system__exceptions__machineB");
   u00025 : constant Version_32 := 16#8bdfdbe3#;
   pragma Export (C, u00025, "system__exceptions__machineS");
   u00026 : constant Version_32 := 16#7706238d#;
   pragma Export (C, u00026, "system__exceptions_debugB");
   u00027 : constant Version_32 := 16#7263f7eb#;
   pragma Export (C, u00027, "system__exceptions_debugS");
   u00028 : constant Version_32 := 16#32ee70d0#;
   pragma Export (C, u00028, "system__img_intS");
   u00029 : constant Version_32 := 16#f2c63a02#;
   pragma Export (C, u00029, "ada__numericsS");
   u00030 : constant Version_32 := 16#174f5472#;
   pragma Export (C, u00030, "ada__numerics__big_numbersS");
   u00031 : constant Version_32 := 16#b847d0e1#;
   pragma Export (C, u00031, "system__unsigned_typesS");
   u00032 : constant Version_32 := 16#5e8f37b6#;
   pragma Export (C, u00032, "system__val_intS");
   u00033 : constant Version_32 := 16#48912782#;
   pragma Export (C, u00033, "system__val_unsS");
   u00034 : constant Version_32 := 16#119c6c25#;
   pragma Export (C, u00034, "system__sparkS");
   u00035 : constant Version_32 := 16#812db2df#;
   pragma Export (C, u00035, "system__spark__cut_operationsB");
   u00036 : constant Version_32 := 16#46c019b4#;
   pragma Export (C, u00036, "system__spark__cut_operationsS");
   u00037 : constant Version_32 := 16#96e09402#;
   pragma Export (C, u00037, "system__val_utilB");
   u00038 : constant Version_32 := 16#71a87b35#;
   pragma Export (C, u00038, "system__val_utilS");
   u00039 : constant Version_32 := 16#b98923bf#;
   pragma Export (C, u00039, "system__case_utilB");
   u00040 : constant Version_32 := 16#8d7e78ed#;
   pragma Export (C, u00040, "system__case_utilS");
   u00041 : constant Version_32 := 16#8d029d03#;
   pragma Export (C, u00041, "system__wid_unsS");
   u00042 : constant Version_32 := 16#5c7d9c20#;
   pragma Export (C, u00042, "system__tracebackB");
   u00043 : constant Version_32 := 16#c4f75b05#;
   pragma Export (C, u00043, "system__tracebackS");
   u00044 : constant Version_32 := 16#5f6b6486#;
   pragma Export (C, u00044, "system__traceback_entriesB");
   u00045 : constant Version_32 := 16#8a711034#;
   pragma Export (C, u00045, "system__traceback_entriesS");
   u00046 : constant Version_32 := 16#4e92775e#;
   pragma Export (C, u00046, "system__traceback__symbolicB");
   u00047 : constant Version_32 := 16#d9e66ad1#;
   pragma Export (C, u00047, "system__traceback__symbolicS");
   u00048 : constant Version_32 := 16#179d7d28#;
   pragma Export (C, u00048, "ada__containersS");
   u00049 : constant Version_32 := 16#701f9d88#;
   pragma Export (C, u00049, "ada__exceptions__tracebackB");
   u00050 : constant Version_32 := 16#eb07882c#;
   pragma Export (C, u00050, "ada__exceptions__tracebackS");
   u00051 : constant Version_32 := 16#15f799c2#;
   pragma Export (C, u00051, "interfacesS");
   u00052 : constant Version_32 := 16#f50995ab#;
   pragma Export (C, u00052, "interfaces__cB");
   u00053 : constant Version_32 := 16#9d395173#;
   pragma Export (C, u00053, "interfaces__cS");
   u00054 : constant Version_32 := 16#f3e539c5#;
   pragma Export (C, u00054, "system__bounded_stringsB");
   u00055 : constant Version_32 := 16#35908ea1#;
   pragma Export (C, u00055, "system__bounded_stringsS");
   u00056 : constant Version_32 := 16#1cff99e6#;
   pragma Export (C, u00056, "system__crtlS");
   u00057 : constant Version_32 := 16#c8ee63e7#;
   pragma Export (C, u00057, "system__dwarf_linesB");
   u00058 : constant Version_32 := 16#a5cb9aae#;
   pragma Export (C, u00058, "system__dwarf_linesS");
   u00059 : constant Version_32 := 16#5b4659fa#;
   pragma Export (C, u00059, "ada__charactersS");
   u00060 : constant Version_32 := 16#f70a517e#;
   pragma Export (C, u00060, "ada__characters__handlingB");
   u00061 : constant Version_32 := 16#ea6baced#;
   pragma Export (C, u00061, "ada__characters__handlingS");
   u00062 : constant Version_32 := 16#cde9ea2d#;
   pragma Export (C, u00062, "ada__characters__latin_1S");
   u00063 : constant Version_32 := 16#e6d4fa36#;
   pragma Export (C, u00063, "ada__stringsS");
   u00064 : constant Version_32 := 16#37fac31e#;
   pragma Export (C, u00064, "ada__strings__mapsB");
   u00065 : constant Version_32 := 16#9df1863a#;
   pragma Export (C, u00065, "ada__strings__mapsS");
   u00066 : constant Version_32 := 16#96b40646#;
   pragma Export (C, u00066, "system__bit_opsB");
   u00067 : constant Version_32 := 16#8f9e0384#;
   pragma Export (C, u00067, "system__bit_opsS");
   u00068 : constant Version_32 := 16#4642cba6#;
   pragma Export (C, u00068, "ada__strings__maps__constantsS");
   u00069 : constant Version_32 := 16#a0d3d22b#;
   pragma Export (C, u00069, "system__address_imageB");
   u00070 : constant Version_32 := 16#e3813282#;
   pragma Export (C, u00070, "system__address_imageS");
   u00071 : constant Version_32 := 16#cdf7317a#;
   pragma Export (C, u00071, "system__img_unsS");
   u00072 : constant Version_32 := 16#20ec7aa3#;
   pragma Export (C, u00072, "system__ioB");
   u00073 : constant Version_32 := 16#dc2f58f7#;
   pragma Export (C, u00073, "system__ioS");
   u00074 : constant Version_32 := 16#e15ca368#;
   pragma Export (C, u00074, "system__mmapB");
   u00075 : constant Version_32 := 16#7a46ab42#;
   pragma Export (C, u00075, "system__mmapS");
   u00076 : constant Version_32 := 16#367911c4#;
   pragma Export (C, u00076, "ada__io_exceptionsS");
   u00077 : constant Version_32 := 16#dd82c35a#;
   pragma Export (C, u00077, "system__mmap__os_interfaceB");
   u00078 : constant Version_32 := 16#37fd3b64#;
   pragma Export (C, u00078, "system__mmap__os_interfaceS");
   u00079 : constant Version_32 := 16#3e3920c1#;
   pragma Export (C, u00079, "system__mmap__unixS");
   u00080 : constant Version_32 := 16#1d7382c4#;
   pragma Export (C, u00080, "system__os_libB");
   u00081 : constant Version_32 := 16#b8017fe7#;
   pragma Export (C, u00081, "system__os_libS");
   u00082 : constant Version_32 := 16#6e5d049a#;
   pragma Export (C, u00082, "system__atomic_operations__test_and_setB");
   u00083 : constant Version_32 := 16#57acee8e#;
   pragma Export (C, u00083, "system__atomic_operations__test_and_setS");
   u00084 : constant Version_32 := 16#850ed59d#;
   pragma Export (C, u00084, "system__atomic_operationsS");
   u00085 : constant Version_32 := 16#29cc6115#;
   pragma Export (C, u00085, "system__atomic_primitivesB");
   u00086 : constant Version_32 := 16#0524e799#;
   pragma Export (C, u00086, "system__atomic_primitivesS");
   u00087 : constant Version_32 := 16#256dbbe5#;
   pragma Export (C, u00087, "system__stringsB");
   u00088 : constant Version_32 := 16#d9efafa0#;
   pragma Export (C, u00088, "system__stringsS");
   u00089 : constant Version_32 := 16#2fdbc40e#;
   pragma Export (C, u00089, "system__object_readerB");
   u00090 : constant Version_32 := 16#55f4bbb3#;
   pragma Export (C, u00090, "system__object_readerS");
   u00091 : constant Version_32 := 16#d7e08022#;
   pragma Export (C, u00091, "system__val_lliS");
   u00092 : constant Version_32 := 16#6a5ef568#;
   pragma Export (C, u00092, "system__val_lluS");
   u00093 : constant Version_32 := 16#bad10b33#;
   pragma Export (C, u00093, "system__exception_tracesB");
   u00094 : constant Version_32 := 16#aef5c6de#;
   pragma Export (C, u00094, "system__exception_tracesS");
   u00095 : constant Version_32 := 16#fd158a37#;
   pragma Export (C, u00095, "system__wch_conB");
   u00096 : constant Version_32 := 16#9b6e8cdb#;
   pragma Export (C, u00096, "system__wch_conS");
   u00097 : constant Version_32 := 16#5c289972#;
   pragma Export (C, u00097, "system__wch_stwB");
   u00098 : constant Version_32 := 16#b67fa0da#;
   pragma Export (C, u00098, "system__wch_stwS");
   u00099 : constant Version_32 := 16#f8305de6#;
   pragma Export (C, u00099, "system__wch_cnvB");
   u00100 : constant Version_32 := 16#9dae46ab#;
   pragma Export (C, u00100, "system__wch_cnvS");
   u00101 : constant Version_32 := 16#e538de43#;
   pragma Export (C, u00101, "system__wch_jisB");
   u00102 : constant Version_32 := 16#28192481#;
   pragma Export (C, u00102, "system__wch_jisS");
   u00103 : constant Version_32 := 16#a201b8c5#;
   pragma Export (C, u00103, "ada__strings__text_buffersB");
   u00104 : constant Version_32 := 16#a7cfd09b#;
   pragma Export (C, u00104, "ada__strings__text_buffersS");
   u00105 : constant Version_32 := 16#8b7604c4#;
   pragma Export (C, u00105, "ada__strings__utf_encodingB");
   u00106 : constant Version_32 := 16#4d0e0994#;
   pragma Export (C, u00106, "ada__strings__utf_encodingS");
   u00107 : constant Version_32 := 16#bb780f45#;
   pragma Export (C, u00107, "ada__strings__utf_encoding__stringsB");
   u00108 : constant Version_32 := 16#b85ff4b6#;
   pragma Export (C, u00108, "ada__strings__utf_encoding__stringsS");
   u00109 : constant Version_32 := 16#d1d1ed0b#;
   pragma Export (C, u00109, "ada__strings__utf_encoding__wide_stringsB");
   u00110 : constant Version_32 := 16#5678478f#;
   pragma Export (C, u00110, "ada__strings__utf_encoding__wide_stringsS");
   u00111 : constant Version_32 := 16#c2b98963#;
   pragma Export (C, u00111, "ada__strings__utf_encoding__wide_wide_stringsB");
   u00112 : constant Version_32 := 16#d7af3358#;
   pragma Export (C, u00112, "ada__strings__utf_encoding__wide_wide_stringsS");
   u00113 : constant Version_32 := 16#6e9a9dee#;
   pragma Export (C, u00113, "ada__tagsB");
   u00114 : constant Version_32 := 16#d00a1748#;
   pragma Export (C, u00114, "ada__tagsS");
   u00115 : constant Version_32 := 16#3548d972#;
   pragma Export (C, u00115, "system__htableB");
   u00116 : constant Version_32 := 16#c3b4f753#;
   pragma Export (C, u00116, "system__htableS");
   u00117 : constant Version_32 := 16#1f1abe38#;
   pragma Export (C, u00117, "system__string_hashB");
   u00118 : constant Version_32 := 16#64f1772c#;
   pragma Export (C, u00118, "system__string_hashS");
   u00119 : constant Version_32 := 16#67eb6d5a#;
   pragma Export (C, u00119, "ada__text_ioB");
   u00120 : constant Version_32 := 16#3cf1122b#;
   pragma Export (C, u00120, "ada__text_ioS");
   u00121 : constant Version_32 := 16#b4f41810#;
   pragma Export (C, u00121, "ada__streamsB");
   u00122 : constant Version_32 := 16#67e31212#;
   pragma Export (C, u00122, "ada__streamsS");
   u00123 : constant Version_32 := 16#abd3c34b#;
   pragma Export (C, u00123, "system__put_imagesB");
   u00124 : constant Version_32 := 16#5ec3a8a7#;
   pragma Export (C, u00124, "system__put_imagesS");
   u00125 : constant Version_32 := 16#22b9eb9f#;
   pragma Export (C, u00125, "ada__strings__text_buffers__utilsB");
   u00126 : constant Version_32 := 16#89062ac3#;
   pragma Export (C, u00126, "ada__strings__text_buffers__utilsS");
   u00127 : constant Version_32 := 16#73d2d764#;
   pragma Export (C, u00127, "interfaces__c_streamsB");
   u00128 : constant Version_32 := 16#7acc80b4#;
   pragma Export (C, u00128, "interfaces__c_streamsS");
   u00129 : constant Version_32 := 16#1aa716c1#;
   pragma Export (C, u00129, "system__file_ioB");
   u00130 : constant Version_32 := 16#3ecf6aed#;
   pragma Export (C, u00130, "system__file_ioS");
   u00131 : constant Version_32 := 16#86c56e5a#;
   pragma Export (C, u00131, "ada__finalizationS");
   u00132 : constant Version_32 := 16#95817ed8#;
   pragma Export (C, u00132, "system__finalization_rootB");
   u00133 : constant Version_32 := 16#0d9fdc28#;
   pragma Export (C, u00133, "system__finalization_rootS");
   u00134 : constant Version_32 := 16#e09c58a9#;
   pragma Export (C, u00134, "system__file_control_blockS");
   u00135 : constant Version_32 := 16#d85d1393#;
   pragma Export (C, u00135, "bar_codeS");
   u00136 : constant Version_32 := 16#0629f7ab#;
   pragma Export (C, u00136, "fancy_pageB");
   u00137 : constant Version_32 := 16#e8e3d663#;
   pragma Export (C, u00137, "fancy_pageS");
   u00138 : constant Version_32 := 16#bcc987d2#;
   pragma Export (C, u00138, "system__concat_4B");
   u00139 : constant Version_32 := 16#7195f086#;
   pragma Export (C, u00139, "system__concat_4S");
   u00140 : constant Version_32 := 16#ddbdd733#;
   pragma Export (C, u00140, "system__fat_llfS");
   u00141 : constant Version_32 := 16#a17cca23#;
   pragma Export (C, u00141, "pdf_outB");
   u00142 : constant Version_32 := 16#949381dd#;
   pragma Export (C, u00142, "pdf_outS");
   u00143 : constant Version_32 := 16#b98a4d98#;
   pragma Export (C, u00143, "ada__strings__fixedB");
   u00144 : constant Version_32 := 16#1bd3eed0#;
   pragma Export (C, u00144, "ada__strings__fixedS");
   u00145 : constant Version_32 := 16#292c204f#;
   pragma Export (C, u00145, "ada__strings__searchB");
   u00146 : constant Version_32 := 16#20765109#;
   pragma Export (C, u00146, "ada__strings__searchS");
   u00147 : constant Version_32 := 16#44bc8f6a#;
   pragma Export (C, u00147, "ada__text_io__generic_auxB");
   u00148 : constant Version_32 := 16#ba6faca0#;
   pragma Export (C, u00148, "ada__text_io__generic_auxS");
   u00149 : constant Version_32 := 16#2bcafb82#;
   pragma Export (C, u00149, "gidB");
   u00150 : constant Version_32 := 16#cc9e089d#;
   pragma Export (C, u00150, "gidS");
   u00151 : constant Version_32 := 16#0c07b229#;
   pragma Export (C, u00151, "gid__decoding_bmpB");
   u00152 : constant Version_32 := 16#f83d5c30#;
   pragma Export (C, u00152, "gid__decoding_bmpS");
   u00153 : constant Version_32 := 16#968d21a9#;
   pragma Export (C, u00153, "gid__bufferingB");
   u00154 : constant Version_32 := 16#cfa0328e#;
   pragma Export (C, u00154, "gid__bufferingS");
   u00155 : constant Version_32 := 16#0c67333d#;
   pragma Export (C, u00155, "gid__decoding_gifB");
   u00156 : constant Version_32 := 16#95cfdbb5#;
   pragma Export (C, u00156, "gid__decoding_gifS");
   u00157 : constant Version_32 := 16#041f516b#;
   pragma Export (C, u00157, "gid__color_tablesB");
   u00158 : constant Version_32 := 16#fb59515d#;
   pragma Export (C, u00158, "gid__color_tablesS");
   u00159 : constant Version_32 := 16#ca878138#;
   pragma Export (C, u00159, "system__concat_2B");
   u00160 : constant Version_32 := 16#f796dc4f#;
   pragma Export (C, u00160, "system__concat_2S");
   u00161 : constant Version_32 := 16#8356fb7a#;
   pragma Export (C, u00161, "system__stream_attributesB");
   u00162 : constant Version_32 := 16#085a4f55#;
   pragma Export (C, u00162, "system__stream_attributesS");
   u00163 : constant Version_32 := 16#4ea7f13e#;
   pragma Export (C, u00163, "system__stream_attributes__xdrB");
   u00164 : constant Version_32 := 16#14c199f1#;
   pragma Export (C, u00164, "system__stream_attributes__xdrS");
   u00165 : constant Version_32 := 16#815f70d4#;
   pragma Export (C, u00165, "system__fat_fltS");
   u00166 : constant Version_32 := 16#a76d79d9#;
   pragma Export (C, u00166, "system__fat_lfltS");
   u00167 : constant Version_32 := 16#022a3ef5#;
   pragma Export (C, u00167, "gid__decoding_jpgB");
   u00168 : constant Version_32 := 16#c9ed4a94#;
   pragma Export (C, u00168, "gid__decoding_jpgS");
   u00169 : constant Version_32 := 16#f64b89a4#;
   pragma Export (C, u00169, "ada__integer_text_ioB");
   u00170 : constant Version_32 := 16#dc1f7556#;
   pragma Export (C, u00170, "ada__integer_text_ioS");
   u00171 : constant Version_32 := 16#8b9a2c46#;
   pragma Export (C, u00171, "system__img_biuS");
   u00172 : constant Version_32 := 16#c6c4eb98#;
   pragma Export (C, u00172, "system__img_llbS");
   u00173 : constant Version_32 := 16#602bc0ef#;
   pragma Export (C, u00173, "system__img_lliS");
   u00174 : constant Version_32 := 16#ad0ace1a#;
   pragma Export (C, u00174, "system__wid_lluS");
   u00175 : constant Version_32 := 16#b1351eea#;
   pragma Export (C, u00175, "system__img_lllbS");
   u00176 : constant Version_32 := 16#868c229b#;
   pragma Export (C, u00176, "system__img_llliS");
   u00177 : constant Version_32 := 16#eaaea841#;
   pragma Export (C, u00177, "system__val_llliS");
   u00178 : constant Version_32 := 16#5ce094b2#;
   pragma Export (C, u00178, "system__val_llluS");
   u00179 : constant Version_32 := 16#ceb71b59#;
   pragma Export (C, u00179, "system__wid_llluS");
   u00180 : constant Version_32 := 16#bb4107e6#;
   pragma Export (C, u00180, "system__img_lllwS");
   u00181 : constant Version_32 := 16#9af69e93#;
   pragma Export (C, u00181, "system__img_llwS");
   u00182 : constant Version_32 := 16#b4409774#;
   pragma Export (C, u00182, "system__img_wiuS");
   u00183 : constant Version_32 := 16#752a67ed#;
   pragma Export (C, u00183, "system__concat_3B");
   u00184 : constant Version_32 := 16#c817b61a#;
   pragma Export (C, u00184, "system__concat_3S");
   u00185 : constant Version_32 := 16#7f5b11f9#;
   pragma Export (C, u00185, "gid__decoding_pngB");
   u00186 : constant Version_32 := 16#02e7cb31#;
   pragma Export (C, u00186, "gid__decoding_pngS");
   u00187 : constant Version_32 := 16#e6e0265c#;
   pragma Export (C, u00187, "gid__decoding_png__huffmanB");
   u00188 : constant Version_32 := 16#39874414#;
   pragma Export (C, u00188, "gid__decoding_png__huffmanS");
   u00189 : constant Version_32 := 16#3124cfea#;
   pragma Export (C, u00189, "system__concat_8B");
   u00190 : constant Version_32 := 16#cfe2ff79#;
   pragma Export (C, u00190, "system__concat_8S");
   u00191 : constant Version_32 := 16#0e0e78c8#;
   pragma Export (C, u00191, "system__val_enum_16S");
   u00192 : constant Version_32 := 16#64082409#;
   pragma Export (C, u00192, "gid__decoding_pnmB");
   u00193 : constant Version_32 := 16#16bfc7c5#;
   pragma Export (C, u00193, "gid__decoding_pnmS");
   u00194 : constant Version_32 := 16#a5571fec#;
   pragma Export (C, u00194, "ada__strings__unboundedB");
   u00195 : constant Version_32 := 16#efe6e98c#;
   pragma Export (C, u00195, "ada__strings__unboundedS");
   u00196 : constant Version_32 := 16#21641b9a#;
   pragma Export (C, u00196, "system__compare_array_unsigned_8B");
   u00197 : constant Version_32 := 16#d28b31db#;
   pragma Export (C, u00197, "system__compare_array_unsigned_8S");
   u00198 : constant Version_32 := 16#74e358eb#;
   pragma Export (C, u00198, "system__address_operationsB");
   u00199 : constant Version_32 := 16#3c598318#;
   pragma Export (C, u00199, "system__address_operationsS");
   u00200 : constant Version_32 := 16#7b453c33#;
   pragma Export (C, u00200, "system__return_stackS");
   u00201 : constant Version_32 := 16#a8ed4e2b#;
   pragma Export (C, u00201, "system__atomic_countersB");
   u00202 : constant Version_32 := 16#9e75407b#;
   pragma Export (C, u00202, "system__atomic_countersS");
   u00203 : constant Version_32 := 16#049b8b2c#;
   pragma Export (C, u00203, "gid__decoding_qoiB");
   u00204 : constant Version_32 := 16#530c377f#;
   pragma Export (C, u00204, "gid__decoding_qoiS");
   u00205 : constant Version_32 := 16#94538365#;
   pragma Export (C, u00205, "gid__decoding_tgaB");
   u00206 : constant Version_32 := 16#34f9aa55#;
   pragma Export (C, u00206, "gid__decoding_tgaS");
   u00207 : constant Version_32 := 16#a8f80fe0#;
   pragma Export (C, u00207, "gid__headersB");
   u00208 : constant Version_32 := 16#4a51dbbc#;
   pragma Export (C, u00208, "gid__headersS");
   u00209 : constant Version_32 := 16#bf56f131#;
   pragma Export (C, u00209, "system__strings__stream_opsB");
   u00210 : constant Version_32 := 16#ba9b6df2#;
   pragma Export (C, u00210, "system__strings__stream_opsS");
   u00211 : constant Version_32 := 16#a440ec49#;
   pragma Export (C, u00211, "ada__calendarB");
   u00212 : constant Version_32 := 16#8324cd02#;
   pragma Export (C, u00212, "ada__calendarS");
   u00213 : constant Version_32 := 16#307180be#;
   pragma Export (C, u00213, "system__os_primitivesB");
   u00214 : constant Version_32 := 16#4590ca4e#;
   pragma Export (C, u00214, "system__os_primitivesS");
   u00215 : constant Version_32 := 16#965ad48e#;
   pragma Export (C, u00215, "ada__strings__boundedB");
   u00216 : constant Version_32 := 16#59838791#;
   pragma Export (C, u00216, "ada__strings__boundedS");
   u00217 : constant Version_32 := 16#4e4ea8d2#;
   pragma Export (C, u00217, "ada__strings__superboundedB");
   u00218 : constant Version_32 := 16#7401a22f#;
   pragma Export (C, u00218, "ada__strings__superboundedS");
   u00219 : constant Version_32 := 16#b59f703c#;
   pragma Export (C, u00219, "system__finalization_mastersB");
   u00220 : constant Version_32 := 16#9ff3107f#;
   pragma Export (C, u00220, "system__finalization_mastersS");
   u00221 : constant Version_32 := 16#35d6ef80#;
   pragma Export (C, u00221, "system__storage_poolsB");
   u00222 : constant Version_32 := 16#229b974b#;
   pragma Export (C, u00222, "system__storage_poolsS");
   u00223 : constant Version_32 := 16#7c78c3c5#;
   pragma Export (C, u00223, "system__pool_globalB");
   u00224 : constant Version_32 := 16#0ca49a01#;
   pragma Export (C, u00224, "system__pool_globalS");
   u00225 : constant Version_32 := 16#1982dcd0#;
   pragma Export (C, u00225, "system__memoryB");
   u00226 : constant Version_32 := 16#19e99d68#;
   pragma Export (C, u00226, "system__memoryS");
   u00227 : constant Version_32 := 16#ad98c70a#;
   pragma Export (C, u00227, "pdf_out__fontsB");
   u00228 : constant Version_32 := 16#6130267c#;
   pragma Export (C, u00228, "pdf_out__fontsS");
   u00229 : constant Version_32 := 16#02cecc7b#;
   pragma Export (C, u00229, "system__concat_6B");
   u00230 : constant Version_32 := 16#c03f5eb0#;
   pragma Export (C, u00230, "system__concat_6S");
   u00231 : constant Version_32 := 16#4bb24b3e#;
   pragma Export (C, u00231, "pdf_out__imagesB");
   u00232 : constant Version_32 := 16#f31a3b3b#;
   pragma Export (C, u00232, "pdf_out__imagesS");
   u00233 : constant Version_32 := 16#63bad2e6#;
   pragma Export (C, u00233, "system__concat_9B");
   u00234 : constant Version_32 := 16#168267d3#;
   pragma Export (C, u00234, "system__concat_9S");
   u00235 : constant Version_32 := 16#f1d549fe#;
   pragma Export (C, u00235, "system__img_fltS");
   u00236 : constant Version_32 := 16#1b28662b#;
   pragma Export (C, u00236, "system__float_controlB");
   u00237 : constant Version_32 := 16#a291ec84#;
   pragma Export (C, u00237, "system__float_controlS");
   u00238 : constant Version_32 := 16#4f0058da#;
   pragma Export (C, u00238, "system__img_utilB");
   u00239 : constant Version_32 := 16#35740b01#;
   pragma Export (C, u00239, "system__img_utilS");
   u00240 : constant Version_32 := 16#e7771600#;
   pragma Export (C, u00240, "system__powten_fltS");
   u00241 : constant Version_32 := 16#2a9b675e#;
   pragma Export (C, u00241, "system__img_lfltS");
   u00242 : constant Version_32 := 16#585f81ee#;
   pragma Export (C, u00242, "system__img_lluS");
   u00243 : constant Version_32 := 16#ee65fd70#;
   pragma Export (C, u00243, "system__powten_lfltS");
   u00244 : constant Version_32 := 16#614b4032#;
   pragma Export (C, u00244, "system__img_llfS");
   u00245 : constant Version_32 := 16#d9f447fb#;
   pragma Export (C, u00245, "system__powten_llfS");
   u00246 : constant Version_32 := 16#298df3f9#;
   pragma Export (C, u00246, "system__storage_pools__subpoolsB");
   u00247 : constant Version_32 := 16#3fcf28a8#;
   pragma Export (C, u00247, "system__storage_pools__subpoolsS");
   u00248 : constant Version_32 := 16#b0df1928#;
   pragma Export (C, u00248, "system__storage_pools__subpools__finalizationB");
   u00249 : constant Version_32 := 16#562129f7#;
   pragma Export (C, u00249, "system__storage_pools__subpools__finalizationS");
   u00250 : constant Version_32 := 16#95f8767f#;
   pragma Export (C, u00250, "system__val_fltS");
   u00251 : constant Version_32 := 16#e77d8041#;
   pragma Export (C, u00251, "system__exn_fltS");
   u00252 : constant Version_32 := 16#7054388e#;
   pragma Export (C, u00252, "system__val_lfltS");
   u00253 : constant Version_32 := 16#593c6198#;
   pragma Export (C, u00253, "system__exn_lfltS");
   u00254 : constant Version_32 := 16#d0838ac3#;
   pragma Export (C, u00254, "system__val_llfS");
   u00255 : constant Version_32 := 16#7492a1e8#;
   pragma Export (C, u00255, "system__exn_llfS");
   u00256 : constant Version_32 := 16#5d191d0e#;
   pragma Export (C, u00256, "ada__streams__stream_ioB");
   u00257 : constant Version_32 := 16#5b183aea#;
   pragma Export (C, u00257, "ada__streams__stream_ioS");
   u00258 : constant Version_32 := 16#5de653db#;
   pragma Export (C, u00258, "system__communicationB");
   u00259 : constant Version_32 := 16#dfb0b3e3#;
   pragma Export (C, u00259, "system__communicationS");
   u00260 : constant Version_32 := 16#0c3d2ef8#;
   pragma Export (C, u00260, "insert_mascotB");
   u00261 : constant Version_32 := 16#804c154d#;
   pragma Export (C, u00261, "qr_codeS");

   --  BEGIN ELABORATION ORDER
   --  ada%s
   --  ada.characters%s
   --  ada.characters.latin_1%s
   --  interfaces%s
   --  system%s
   --  system.address_operations%s
   --  system.address_operations%b
   --  system.atomic_operations%s
   --  system.float_control%s
   --  system.float_control%b
   --  system.io%s
   --  system.io%b
   --  system.parameters%s
   --  system.parameters%b
   --  system.crtl%s
   --  interfaces.c_streams%s
   --  interfaces.c_streams%b
   --  system.os_primitives%s
   --  system.os_primitives%b
   --  system.powten_flt%s
   --  system.powten_lflt%s
   --  system.powten_llf%s
   --  system.spark%s
   --  system.spark.cut_operations%s
   --  system.spark.cut_operations%b
   --  system.storage_elements%s
   --  system.return_stack%s
   --  system.stack_checking%s
   --  system.stack_checking%b
   --  system.string_hash%s
   --  system.string_hash%b
   --  system.htable%s
   --  system.htable%b
   --  system.strings%s
   --  system.strings%b
   --  system.traceback_entries%s
   --  system.traceback_entries%b
   --  system.unsigned_types%s
   --  system.img_biu%s
   --  system.img_llb%s
   --  system.img_lllb%s
   --  system.img_lllw%s
   --  system.img_llw%s
   --  system.img_wiu%s
   --  system.wch_con%s
   --  system.wch_con%b
   --  system.wch_jis%s
   --  system.wch_jis%b
   --  system.wch_cnv%s
   --  system.wch_cnv%b
   --  system.compare_array_unsigned_8%s
   --  system.compare_array_unsigned_8%b
   --  system.concat_2%s
   --  system.concat_2%b
   --  system.concat_3%s
   --  system.concat_3%b
   --  system.concat_4%s
   --  system.concat_4%b
   --  system.concat_6%s
   --  system.concat_6%b
   --  system.concat_8%s
   --  system.concat_8%b
   --  system.concat_9%s
   --  system.concat_9%b
   --  system.exn_flt%s
   --  system.exn_lflt%s
   --  system.exn_llf%s
   --  system.traceback%s
   --  system.traceback%b
   --  ada.characters.handling%s
   --  system.atomic_operations.test_and_set%s
   --  system.case_util%s
   --  system.os_lib%s
   --  system.secondary_stack%s
   --  system.standard_library%s
   --  ada.exceptions%s
   --  system.exceptions_debug%s
   --  system.exceptions_debug%b
   --  system.soft_links%s
   --  system.val_llu%s
   --  system.val_lli%s
   --  system.val_uns%s
   --  system.val_int%s
   --  system.val_util%s
   --  system.val_util%b
   --  system.wch_stw%s
   --  system.wch_stw%b
   --  ada.exceptions.last_chance_handler%s
   --  ada.exceptions.last_chance_handler%b
   --  ada.exceptions.traceback%s
   --  ada.exceptions.traceback%b
   --  system.address_image%s
   --  system.address_image%b
   --  system.bit_ops%s
   --  system.bit_ops%b
   --  system.bounded_strings%s
   --  system.bounded_strings%b
   --  system.case_util%b
   --  system.exception_table%s
   --  system.exception_table%b
   --  ada.containers%s
   --  ada.io_exceptions%s
   --  ada.numerics%s
   --  ada.numerics.big_numbers%s
   --  ada.strings%s
   --  ada.strings.maps%s
   --  ada.strings.maps%b
   --  ada.strings.maps.constants%s
   --  interfaces.c%s
   --  interfaces.c%b
   --  system.atomic_primitives%s
   --  system.atomic_primitives%b
   --  system.exceptions%s
   --  system.exceptions.machine%s
   --  system.exceptions.machine%b
   --  ada.characters.handling%b
   --  system.atomic_operations.test_and_set%b
   --  system.exception_traces%s
   --  system.exception_traces%b
   --  system.memory%s
   --  system.memory%b
   --  system.mmap%s
   --  system.mmap.os_interface%s
   --  system.mmap%b
   --  system.mmap.unix%s
   --  system.mmap.os_interface%b
   --  system.object_reader%s
   --  system.object_reader%b
   --  system.dwarf_lines%s
   --  system.os_lib%b
   --  system.secondary_stack%b
   --  system.soft_links.initialize%s
   --  system.soft_links.initialize%b
   --  system.soft_links%b
   --  system.standard_library%b
   --  system.traceback.symbolic%s
   --  system.traceback.symbolic%b
   --  system.wid_uns%s
   --  system.img_int%s
   --  ada.exceptions%b
   --  system.img_uns%s
   --  system.dwarf_lines%b
   --  ada.strings.search%s
   --  ada.strings.search%b
   --  ada.strings.fixed%s
   --  ada.strings.fixed%b
   --  ada.strings.utf_encoding%s
   --  ada.strings.utf_encoding%b
   --  ada.strings.utf_encoding.strings%s
   --  ada.strings.utf_encoding.strings%b
   --  ada.strings.utf_encoding.wide_strings%s
   --  ada.strings.utf_encoding.wide_strings%b
   --  ada.strings.utf_encoding.wide_wide_strings%s
   --  ada.strings.utf_encoding.wide_wide_strings%b
   --  ada.tags%s
   --  ada.tags%b
   --  ada.strings.text_buffers%s
   --  ada.strings.text_buffers%b
   --  ada.strings.text_buffers.utils%s
   --  ada.strings.text_buffers.utils%b
   --  system.atomic_counters%s
   --  system.atomic_counters%b
   --  system.fat_flt%s
   --  system.fat_lflt%s
   --  system.fat_llf%s
   --  system.put_images%s
   --  system.put_images%b
   --  ada.streams%s
   --  ada.streams%b
   --  ada.strings.superbounded%s
   --  ada.strings.superbounded%b
   --  ada.strings.bounded%s
   --  ada.strings.bounded%b
   --  system.communication%s
   --  system.communication%b
   --  system.file_control_block%s
   --  system.finalization_root%s
   --  system.finalization_root%b
   --  ada.finalization%s
   --  system.file_io%s
   --  system.file_io%b
   --  ada.streams.stream_io%s
   --  ada.streams.stream_io%b
   --  system.storage_pools%s
   --  system.storage_pools%b
   --  system.finalization_masters%s
   --  system.finalization_masters%b
   --  system.storage_pools.subpools%s
   --  system.storage_pools.subpools.finalization%s
   --  system.storage_pools.subpools.finalization%b
   --  system.storage_pools.subpools%b
   --  system.stream_attributes%s
   --  system.stream_attributes.xdr%s
   --  system.stream_attributes.xdr%b
   --  system.stream_attributes%b
   --  ada.strings.unbounded%s
   --  ada.strings.unbounded%b
   --  system.val_enum_16%s
   --  system.val_flt%s
   --  system.val_lflt%s
   --  system.val_llf%s
   --  system.val_lllu%s
   --  system.val_llli%s
   --  ada.calendar%s
   --  ada.calendar%b
   --  ada.text_io%s
   --  ada.text_io%b
   --  ada.text_io.generic_aux%s
   --  ada.text_io.generic_aux%b
   --  system.img_util%s
   --  system.img_util%b
   --  system.img_flt%s
   --  system.pool_global%s
   --  system.pool_global%b
   --  system.strings.stream_ops%s
   --  system.strings.stream_ops%b
   --  system.wid_lllu%s
   --  system.img_llli%s
   --  system.wid_llu%s
   --  system.img_lli%s
   --  ada.integer_text_io%s
   --  ada.integer_text_io%b
   --  system.img_llu%s
   --  system.img_lflt%s
   --  system.img_llf%s
   --  bar_code%s
   --  gid%s
   --  gid.buffering%s
   --  gid.buffering%b
   --  gid.color_tables%s
   --  gid.color_tables%b
   --  gid.decoding_bmp%s
   --  gid.decoding_bmp%b
   --  gid.decoding_gif%s
   --  gid.decoding_gif%b
   --  gid.decoding_jpg%s
   --  gid.decoding_jpg%b
   --  gid.decoding_png%s
   --  gid.decoding_png.huffman%s
   --  gid.decoding_png.huffman%b
   --  gid.decoding_png%b
   --  gid.decoding_pnm%s
   --  gid.decoding_pnm%b
   --  gid.decoding_qoi%s
   --  gid.decoding_qoi%b
   --  gid.decoding_tga%s
   --  gid.decoding_tga%b
   --  gid.headers%s
   --  gid.headers%b
   --  gid%b
   --  pdf_out%s
   --  pdf_out.fonts%s
   --  pdf_out.fonts%b
   --  pdf_out.images%s
   --  pdf_out.images%b
   --  pdf_out%b
   --  fancy_page%s
   --  fancy_page%b
   --  insert_mascot%b
   --  qr_code%s
   --  pdf_out_demo%b
   --  END ELABORATION ORDER

end ada_main;