pragma Warnings (Off);
pragma Ada_95;
with System;
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

   Ada_Main_Program_Name : constant String := "_ada_mage_hat" & ASCII.NUL;
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
   u00001 : constant Version_32 := 16#aa90dc5f#;
   pragma Export (C, u00001, "mage_hatB");
   u00002 : constant Version_32 := 16#7320ff5f#;
   pragma Export (C, u00002, "system__standard_libraryB");
   u00003 : constant Version_32 := 16#50630821#;
   pragma Export (C, u00003, "system__standard_libraryS");
   u00004 : constant Version_32 := 16#76789da1#;
   pragma Export (C, u00004, "adaS");
   u00005 : constant Version_32 := 16#a8e42c2f#;
   pragma Export (C, u00005, "ada__real_timeB");
   u00006 : constant Version_32 := 16#a00d3370#;
   pragma Export (C, u00006, "ada__real_timeS");
   u00007 : constant Version_32 := 16#e9d77c55#;
   pragma Export (C, u00007, "ada__exceptionsB");
   u00008 : constant Version_32 := 16#1e7524b5#;
   pragma Export (C, u00008, "ada__exceptionsS");
   u00009 : constant Version_32 := 16#0740df23#;
   pragma Export (C, u00009, "ada__exceptions__last_chance_handlerB");
   u00010 : constant Version_32 := 16#6dc27684#;
   pragma Export (C, u00010, "ada__exceptions__last_chance_handlerS");
   u00011 : constant Version_32 := 16#426dafb8#;
   pragma Export (C, u00011, "systemS");
   u00012 : constant Version_32 := 16#fd5f5f4c#;
   pragma Export (C, u00012, "system__soft_linksB");
   u00013 : constant Version_32 := 16#3ff0395b#;
   pragma Export (C, u00013, "system__soft_linksS");
   u00014 : constant Version_32 := 16#49d1641b#;
   pragma Export (C, u00014, "system__secondary_stackB");
   u00015 : constant Version_32 := 16#f738e7ab#;
   pragma Export (C, u00015, "system__secondary_stackS");
   u00016 : constant Version_32 := 16#821dff88#;
   pragma Export (C, u00016, "system__parametersB");
   u00017 : constant Version_32 := 16#96f90b1e#;
   pragma Export (C, u00017, "system__parametersS");
   u00018 : constant Version_32 := 16#742e3af8#;
   pragma Export (C, u00018, "system__storage_elementsS");
   u00019 : constant Version_32 := 16#0286ce9f#;
   pragma Export (C, u00019, "system__soft_links__initializeB");
   u00020 : constant Version_32 := 16#2ed17187#;
   pragma Export (C, u00020, "system__soft_links__initializeS");
   u00021 : constant Version_32 := 16#8599b27b#;
   pragma Export (C, u00021, "system__stack_checkingB");
   u00022 : constant Version_32 := 16#7faffb06#;
   pragma Export (C, u00022, "system__stack_checkingS");
   u00023 : constant Version_32 := 16#c71e6c8a#;
   pragma Export (C, u00023, "system__exception_tableB");
   u00024 : constant Version_32 := 16#cf46d9a1#;
   pragma Export (C, u00024, "system__exception_tableS");
   u00025 : constant Version_32 := 16#70c8108a#;
   pragma Export (C, u00025, "system__exceptionsS");
   u00026 : constant Version_32 := 16#69416224#;
   pragma Export (C, u00026, "system__exceptions__machineB");
   u00027 : constant Version_32 := 16#8bdfdbe3#;
   pragma Export (C, u00027, "system__exceptions__machineS");
   u00028 : constant Version_32 := 16#7706238d#;
   pragma Export (C, u00028, "system__exceptions_debugB");
   u00029 : constant Version_32 := 16#7263f7eb#;
   pragma Export (C, u00029, "system__exceptions_debugS");
   u00030 : constant Version_32 := 16#32ee70d0#;
   pragma Export (C, u00030, "system__img_intS");
   u00031 : constant Version_32 := 16#f2c63a02#;
   pragma Export (C, u00031, "ada__numericsS");
   u00032 : constant Version_32 := 16#174f5472#;
   pragma Export (C, u00032, "ada__numerics__big_numbersS");
   u00033 : constant Version_32 := 16#b847d0e1#;
   pragma Export (C, u00033, "system__unsigned_typesS");
   u00034 : constant Version_32 := 16#5e8f37b6#;
   pragma Export (C, u00034, "system__val_intS");
   u00035 : constant Version_32 := 16#48912782#;
   pragma Export (C, u00035, "system__val_unsS");
   u00036 : constant Version_32 := 16#119c6c25#;
   pragma Export (C, u00036, "system__sparkS");
   u00037 : constant Version_32 := 16#812db2df#;
   pragma Export (C, u00037, "system__spark__cut_operationsB");
   u00038 : constant Version_32 := 16#46c019b4#;
   pragma Export (C, u00038, "system__spark__cut_operationsS");
   u00039 : constant Version_32 := 16#96e09402#;
   pragma Export (C, u00039, "system__val_utilB");
   u00040 : constant Version_32 := 16#71a87b35#;
   pragma Export (C, u00040, "system__val_utilS");
   u00041 : constant Version_32 := 16#b98923bf#;
   pragma Export (C, u00041, "system__case_utilB");
   u00042 : constant Version_32 := 16#8d7e78ed#;
   pragma Export (C, u00042, "system__case_utilS");
   u00043 : constant Version_32 := 16#8d029d03#;
   pragma Export (C, u00043, "system__wid_unsS");
   u00044 : constant Version_32 := 16#5c7d9c20#;
   pragma Export (C, u00044, "system__tracebackB");
   u00045 : constant Version_32 := 16#c4f75b05#;
   pragma Export (C, u00045, "system__tracebackS");
   u00046 : constant Version_32 := 16#5f6b6486#;
   pragma Export (C, u00046, "system__traceback_entriesB");
   u00047 : constant Version_32 := 16#8a711034#;
   pragma Export (C, u00047, "system__traceback_entriesS");
   u00048 : constant Version_32 := 16#4e92775e#;
   pragma Export (C, u00048, "system__traceback__symbolicB");
   u00049 : constant Version_32 := 16#d9e66ad1#;
   pragma Export (C, u00049, "system__traceback__symbolicS");
   u00050 : constant Version_32 := 16#179d7d28#;
   pragma Export (C, u00050, "ada__containersS");
   u00051 : constant Version_32 := 16#701f9d88#;
   pragma Export (C, u00051, "ada__exceptions__tracebackB");
   u00052 : constant Version_32 := 16#eb07882c#;
   pragma Export (C, u00052, "ada__exceptions__tracebackS");
   u00053 : constant Version_32 := 16#15f799c2#;
   pragma Export (C, u00053, "interfacesS");
   u00054 : constant Version_32 := 16#f50995ab#;
   pragma Export (C, u00054, "interfaces__cB");
   u00055 : constant Version_32 := 16#9d395173#;
   pragma Export (C, u00055, "interfaces__cS");
   u00056 : constant Version_32 := 16#f3e539c5#;
   pragma Export (C, u00056, "system__bounded_stringsB");
   u00057 : constant Version_32 := 16#35908ea1#;
   pragma Export (C, u00057, "system__bounded_stringsS");
   u00058 : constant Version_32 := 16#1cff99e6#;
   pragma Export (C, u00058, "system__crtlS");
   u00059 : constant Version_32 := 16#c8ee63e7#;
   pragma Export (C, u00059, "system__dwarf_linesB");
   u00060 : constant Version_32 := 16#a5cb9aae#;
   pragma Export (C, u00060, "system__dwarf_linesS");
   u00061 : constant Version_32 := 16#5b4659fa#;
   pragma Export (C, u00061, "ada__charactersS");
   u00062 : constant Version_32 := 16#f70a517e#;
   pragma Export (C, u00062, "ada__characters__handlingB");
   u00063 : constant Version_32 := 16#ea6baced#;
   pragma Export (C, u00063, "ada__characters__handlingS");
   u00064 : constant Version_32 := 16#cde9ea2d#;
   pragma Export (C, u00064, "ada__characters__latin_1S");
   u00065 : constant Version_32 := 16#e6d4fa36#;
   pragma Export (C, u00065, "ada__stringsS");
   u00066 : constant Version_32 := 16#37fac31e#;
   pragma Export (C, u00066, "ada__strings__mapsB");
   u00067 : constant Version_32 := 16#9df1863a#;
   pragma Export (C, u00067, "ada__strings__mapsS");
   u00068 : constant Version_32 := 16#96b40646#;
   pragma Export (C, u00068, "system__bit_opsB");
   u00069 : constant Version_32 := 16#8f9e0384#;
   pragma Export (C, u00069, "system__bit_opsS");
   u00070 : constant Version_32 := 16#4642cba6#;
   pragma Export (C, u00070, "ada__strings__maps__constantsS");
   u00071 : constant Version_32 := 16#a0d3d22b#;
   pragma Export (C, u00071, "system__address_imageB");
   u00072 : constant Version_32 := 16#e3813282#;
   pragma Export (C, u00072, "system__address_imageS");
   u00073 : constant Version_32 := 16#cdf7317a#;
   pragma Export (C, u00073, "system__img_unsS");
   u00074 : constant Version_32 := 16#20ec7aa3#;
   pragma Export (C, u00074, "system__ioB");
   u00075 : constant Version_32 := 16#dc2f58f7#;
   pragma Export (C, u00075, "system__ioS");
   u00076 : constant Version_32 := 16#e15ca368#;
   pragma Export (C, u00076, "system__mmapB");
   u00077 : constant Version_32 := 16#7a46ab42#;
   pragma Export (C, u00077, "system__mmapS");
   u00078 : constant Version_32 := 16#367911c4#;
   pragma Export (C, u00078, "ada__io_exceptionsS");
   u00079 : constant Version_32 := 16#dd82c35a#;
   pragma Export (C, u00079, "system__mmap__os_interfaceB");
   u00080 : constant Version_32 := 16#37fd3b64#;
   pragma Export (C, u00080, "system__mmap__os_interfaceS");
   u00081 : constant Version_32 := 16#3e3920c1#;
   pragma Export (C, u00081, "system__mmap__unixS");
   u00082 : constant Version_32 := 16#1d7382c4#;
   pragma Export (C, u00082, "system__os_libB");
   u00083 : constant Version_32 := 16#b8017fe7#;
   pragma Export (C, u00083, "system__os_libS");
   u00084 : constant Version_32 := 16#6e5d049a#;
   pragma Export (C, u00084, "system__atomic_operations__test_and_setB");
   u00085 : constant Version_32 := 16#57acee8e#;
   pragma Export (C, u00085, "system__atomic_operations__test_and_setS");
   u00086 : constant Version_32 := 16#850ed59d#;
   pragma Export (C, u00086, "system__atomic_operationsS");
   u00087 : constant Version_32 := 16#29cc6115#;
   pragma Export (C, u00087, "system__atomic_primitivesB");
   u00088 : constant Version_32 := 16#0524e799#;
   pragma Export (C, u00088, "system__atomic_primitivesS");
   u00089 : constant Version_32 := 16#256dbbe5#;
   pragma Export (C, u00089, "system__stringsB");
   u00090 : constant Version_32 := 16#d9efafa0#;
   pragma Export (C, u00090, "system__stringsS");
   u00091 : constant Version_32 := 16#2fdbc40e#;
   pragma Export (C, u00091, "system__object_readerB");
   u00092 : constant Version_32 := 16#55f4bbb3#;
   pragma Export (C, u00092, "system__object_readerS");
   u00093 : constant Version_32 := 16#d7e08022#;
   pragma Export (C, u00093, "system__val_lliS");
   u00094 : constant Version_32 := 16#6a5ef568#;
   pragma Export (C, u00094, "system__val_lluS");
   u00095 : constant Version_32 := 16#bad10b33#;
   pragma Export (C, u00095, "system__exception_tracesB");
   u00096 : constant Version_32 := 16#aef5c6de#;
   pragma Export (C, u00096, "system__exception_tracesS");
   u00097 : constant Version_32 := 16#fd158a37#;
   pragma Export (C, u00097, "system__wch_conB");
   u00098 : constant Version_32 := 16#9b6e8cdb#;
   pragma Export (C, u00098, "system__wch_conS");
   u00099 : constant Version_32 := 16#5c289972#;
   pragma Export (C, u00099, "system__wch_stwB");
   u00100 : constant Version_32 := 16#b67fa0da#;
   pragma Export (C, u00100, "system__wch_stwS");
   u00101 : constant Version_32 := 16#f8305de6#;
   pragma Export (C, u00101, "system__wch_cnvB");
   u00102 : constant Version_32 := 16#9dae46ab#;
   pragma Export (C, u00102, "system__wch_cnvS");
   u00103 : constant Version_32 := 16#e538de43#;
   pragma Export (C, u00103, "system__wch_jisB");
   u00104 : constant Version_32 := 16#28192481#;
   pragma Export (C, u00104, "system__wch_jisS");
   u00105 : constant Version_32 := 16#c892ee09#;
   pragma Export (C, u00105, "system__taskingB");
   u00106 : constant Version_32 := 16#f9a2cde3#;
   pragma Export (C, u00106, "system__taskingS");
   u00107 : constant Version_32 := 16#e850091f#;
   pragma Export (C, u00107, "system__task_primitivesS");
   u00108 : constant Version_32 := 16#848a1fe0#;
   pragma Export (C, u00108, "system__os_interfaceB");
   u00109 : constant Version_32 := 16#1952b102#;
   pragma Export (C, u00109, "system__os_interfaceS");
   u00110 : constant Version_32 := 16#fe266d85#;
   pragma Export (C, u00110, "system__linuxS");
   u00111 : constant Version_32 := 16#c7b9aba1#;
   pragma Export (C, u00111, "system__os_constantsS");
   u00112 : constant Version_32 := 16#021ce70a#;
   pragma Export (C, u00112, "system__task_primitives__operationsB");
   u00113 : constant Version_32 := 16#fb8251ad#;
   pragma Export (C, u00113, "system__task_primitives__operationsS");
   u00114 : constant Version_32 := 16#9ebeb40e#;
   pragma Export (C, u00114, "system__interrupt_managementB");
   u00115 : constant Version_32 := 16#50dc425b#;
   pragma Export (C, u00115, "system__interrupt_managementS");
   u00116 : constant Version_32 := 16#fe2ee843#;
   pragma Export (C, u00116, "system__multiprocessorsB");
   u00117 : constant Version_32 := 16#7ac130cb#;
   pragma Export (C, u00117, "system__multiprocessorsS");
   u00118 : constant Version_32 := 16#307180be#;
   pragma Export (C, u00118, "system__os_primitivesB");
   u00119 : constant Version_32 := 16#4590ca4e#;
   pragma Export (C, u00119, "system__os_primitivesS");
   u00120 : constant Version_32 := 16#4ee862d1#;
   pragma Export (C, u00120, "system__task_infoB");
   u00121 : constant Version_32 := 16#f415468c#;
   pragma Export (C, u00121, "system__task_infoS");
   u00122 : constant Version_32 := 16#c07cb956#;
   pragma Export (C, u00122, "system__tasking__debugB");
   u00123 : constant Version_32 := 16#aeb4df49#;
   pragma Export (C, u00123, "system__tasking__debugS");
   u00124 : constant Version_32 := 16#ca878138#;
   pragma Export (C, u00124, "system__concat_2B");
   u00125 : constant Version_32 := 16#f796dc4f#;
   pragma Export (C, u00125, "system__concat_2S");
   u00126 : constant Version_32 := 16#752a67ed#;
   pragma Export (C, u00126, "system__concat_3B");
   u00127 : constant Version_32 := 16#c817b61a#;
   pragma Export (C, u00127, "system__concat_3S");
   u00128 : constant Version_32 := 16#602bc0ef#;
   pragma Export (C, u00128, "system__img_lliS");
   u00129 : constant Version_32 := 16#ad0ace1a#;
   pragma Export (C, u00129, "system__wid_lluS");
   u00130 : constant Version_32 := 16#e5d09b61#;
   pragma Export (C, u00130, "system__stack_usageB");
   u00131 : constant Version_32 := 16#aa0de253#;
   pragma Export (C, u00131, "system__stack_usageS");
   u00132 : constant Version_32 := 16#e1f81eab#;
   pragma Export (C, u00132, "ada__real_time__delaysB");
   u00133 : constant Version_32 := 16#b086ca67#;
   pragma Export (C, u00133, "ada__real_time__delaysS");
   u00134 : constant Version_32 := 16#a201b8c5#;
   pragma Export (C, u00134, "ada__strings__text_buffersB");
   u00135 : constant Version_32 := 16#a7cfd09b#;
   pragma Export (C, u00135, "ada__strings__text_buffersS");
   u00136 : constant Version_32 := 16#8b7604c4#;
   pragma Export (C, u00136, "ada__strings__utf_encodingB");
   u00137 : constant Version_32 := 16#4d0e0994#;
   pragma Export (C, u00137, "ada__strings__utf_encodingS");
   u00138 : constant Version_32 := 16#bb780f45#;
   pragma Export (C, u00138, "ada__strings__utf_encoding__stringsB");
   u00139 : constant Version_32 := 16#b85ff4b6#;
   pragma Export (C, u00139, "ada__strings__utf_encoding__stringsS");
   u00140 : constant Version_32 := 16#d1d1ed0b#;
   pragma Export (C, u00140, "ada__strings__utf_encoding__wide_stringsB");
   u00141 : constant Version_32 := 16#5678478f#;
   pragma Export (C, u00141, "ada__strings__utf_encoding__wide_stringsS");
   u00142 : constant Version_32 := 16#c2b98963#;
   pragma Export (C, u00142, "ada__strings__utf_encoding__wide_wide_stringsB");
   u00143 : constant Version_32 := 16#d7af3358#;
   pragma Export (C, u00143, "ada__strings__utf_encoding__wide_wide_stringsS");
   u00144 : constant Version_32 := 16#6e9a9dee#;
   pragma Export (C, u00144, "ada__tagsB");
   u00145 : constant Version_32 := 16#d00a1748#;
   pragma Export (C, u00145, "ada__tagsS");
   u00146 : constant Version_32 := 16#3548d972#;
   pragma Export (C, u00146, "system__htableB");
   u00147 : constant Version_32 := 16#c3b4f753#;
   pragma Export (C, u00147, "system__htableS");
   u00148 : constant Version_32 := 16#1f1abe38#;
   pragma Export (C, u00148, "system__string_hashB");
   u00149 : constant Version_32 := 16#64f1772c#;
   pragma Export (C, u00149, "system__string_hashS");
   u00150 : constant Version_32 := 16#67eb6d5a#;
   pragma Export (C, u00150, "ada__text_ioB");
   u00151 : constant Version_32 := 16#3cf1122b#;
   pragma Export (C, u00151, "ada__text_ioS");
   u00152 : constant Version_32 := 16#b4f41810#;
   pragma Export (C, u00152, "ada__streamsB");
   u00153 : constant Version_32 := 16#67e31212#;
   pragma Export (C, u00153, "ada__streamsS");
   u00154 : constant Version_32 := 16#abd3c34b#;
   pragma Export (C, u00154, "system__put_imagesB");
   u00155 : constant Version_32 := 16#5ec3a8a7#;
   pragma Export (C, u00155, "system__put_imagesS");
   u00156 : constant Version_32 := 16#22b9eb9f#;
   pragma Export (C, u00156, "ada__strings__text_buffers__utilsB");
   u00157 : constant Version_32 := 16#89062ac3#;
   pragma Export (C, u00157, "ada__strings__text_buffers__utilsS");
   u00158 : constant Version_32 := 16#73d2d764#;
   pragma Export (C, u00158, "interfaces__c_streamsB");
   u00159 : constant Version_32 := 16#7acc80b4#;
   pragma Export (C, u00159, "interfaces__c_streamsS");
   u00160 : constant Version_32 := 16#1aa716c1#;
   pragma Export (C, u00160, "system__file_ioB");
   u00161 : constant Version_32 := 16#3ecf6aed#;
   pragma Export (C, u00161, "system__file_ioS");
   u00162 : constant Version_32 := 16#86c56e5a#;
   pragma Export (C, u00162, "ada__finalizationS");
   u00163 : constant Version_32 := 16#95817ed8#;
   pragma Export (C, u00163, "system__finalization_rootB");
   u00164 : constant Version_32 := 16#0d9fdc28#;
   pragma Export (C, u00164, "system__finalization_rootS");
   u00165 : constant Version_32 := 16#e09c58a9#;
   pragma Export (C, u00165, "system__file_control_blockS");
   u00166 : constant Version_32 := 16#91c38924#;
   pragma Export (C, u00166, "mageB");
   u00167 : constant Version_32 := 16#a67bd364#;
   pragma Export (C, u00167, "mageS");
   u00168 : constant Version_32 := 16#a5571fec#;
   pragma Export (C, u00168, "ada__strings__unboundedB");
   u00169 : constant Version_32 := 16#efe6e98c#;
   pragma Export (C, u00169, "ada__strings__unboundedS");
   u00170 : constant Version_32 := 16#292c204f#;
   pragma Export (C, u00170, "ada__strings__searchB");
   u00171 : constant Version_32 := 16#20765109#;
   pragma Export (C, u00171, "ada__strings__searchS");
   u00172 : constant Version_32 := 16#21641b9a#;
   pragma Export (C, u00172, "system__compare_array_unsigned_8B");
   u00173 : constant Version_32 := 16#d28b31db#;
   pragma Export (C, u00173, "system__compare_array_unsigned_8S");
   u00174 : constant Version_32 := 16#74e358eb#;
   pragma Export (C, u00174, "system__address_operationsB");
   u00175 : constant Version_32 := 16#3c598318#;
   pragma Export (C, u00175, "system__address_operationsS");
   u00176 : constant Version_32 := 16#7b453c33#;
   pragma Export (C, u00176, "system__return_stackS");
   u00177 : constant Version_32 := 16#a8ed4e2b#;
   pragma Export (C, u00177, "system__atomic_countersB");
   u00178 : constant Version_32 := 16#9e75407b#;
   pragma Export (C, u00178, "system__atomic_countersS");
   u00179 : constant Version_32 := 16#8356fb7a#;
   pragma Export (C, u00179, "system__stream_attributesB");
   u00180 : constant Version_32 := 16#085a4f55#;
   pragma Export (C, u00180, "system__stream_attributesS");
   u00181 : constant Version_32 := 16#4ea7f13e#;
   pragma Export (C, u00181, "system__stream_attributes__xdrB");
   u00182 : constant Version_32 := 16#14c199f1#;
   pragma Export (C, u00182, "system__stream_attributes__xdrS");
   u00183 : constant Version_32 := 16#815f70d4#;
   pragma Export (C, u00183, "system__fat_fltS");
   u00184 : constant Version_32 := 16#a76d79d9#;
   pragma Export (C, u00184, "system__fat_lfltS");
   u00185 : constant Version_32 := 16#ddbdd733#;
   pragma Export (C, u00185, "system__fat_llfS");
   u00186 : constant Version_32 := 16#3a58b449#;
   pragma Export (C, u00186, "sdlB");
   u00187 : constant Version_32 := 16#4e32b3c4#;
   pragma Export (C, u00187, "sdlS");
   u00188 : constant Version_32 := 16#dff8c4d8#;
   pragma Export (C, u00188, "sdl__videoB");
   u00189 : constant Version_32 := 16#acb4a960#;
   pragma Export (C, u00189, "sdl__videoS");
   u00190 : constant Version_32 := 16#ddee7ff6#;
   pragma Export (C, u00190, "interfaces__c__stringsB");
   u00191 : constant Version_32 := 16#a856e1a3#;
   pragma Export (C, u00191, "interfaces__c__stringsS");
   u00192 : constant Version_32 := 16#9f72c855#;
   pragma Export (C, u00192, "sdl__errorB");
   u00193 : constant Version_32 := 16#dbb853e3#;
   pragma Export (C, u00193, "sdl__errorS");
   u00194 : constant Version_32 := 16#5aa0c2ac#;
   pragma Export (C, u00194, "sdl__video__palettesB");
   u00195 : constant Version_32 := 16#02a22597#;
   pragma Export (C, u00195, "sdl__video__palettesS");
   u00196 : constant Version_32 := 16#b59f703c#;
   pragma Export (C, u00196, "system__finalization_mastersB");
   u00197 : constant Version_32 := 16#9ff3107f#;
   pragma Export (C, u00197, "system__finalization_mastersS");
   u00198 : constant Version_32 := 16#35d6ef80#;
   pragma Export (C, u00198, "system__storage_poolsB");
   u00199 : constant Version_32 := 16#229b974b#;
   pragma Export (C, u00199, "system__storage_poolsS");
   u00200 : constant Version_32 := 16#298df3f9#;
   pragma Export (C, u00200, "system__storage_pools__subpoolsB");
   u00201 : constant Version_32 := 16#3fcf28a8#;
   pragma Export (C, u00201, "system__storage_pools__subpoolsS");
   u00202 : constant Version_32 := 16#b0df1928#;
   pragma Export (C, u00202, "system__storage_pools__subpools__finalizationB");
   u00203 : constant Version_32 := 16#562129f7#;
   pragma Export (C, u00203, "system__storage_pools__subpools__finalizationS");
   u00204 : constant Version_32 := 16#7c78c3c5#;
   pragma Export (C, u00204, "system__pool_globalB");
   u00205 : constant Version_32 := 16#0ca49a01#;
   pragma Export (C, u00205, "system__pool_globalS");
   u00206 : constant Version_32 := 16#1982dcd0#;
   pragma Export (C, u00206, "system__memoryB");
   u00207 : constant Version_32 := 16#19e99d68#;
   pragma Export (C, u00207, "system__memoryS");
   u00208 : constant Version_32 := 16#6d9b535d#;
   pragma Export (C, u00208, "mage__drawB");
   u00209 : constant Version_32 := 16#21de07f3#;
   pragma Export (C, u00209, "mage__drawS");
   u00210 : constant Version_32 := 16#d3aa20c0#;
   pragma Export (C, u00210, "mage_configS");
   u00211 : constant Version_32 := 16#cde2c49d#;
   pragma Export (C, u00211, "mage_config__hardwareS");
   u00212 : constant Version_32 := 16#ef09c5ab#;
   pragma Export (C, u00212, "sdl__video__rectanglesB");
   u00213 : constant Version_32 := 16#773e1129#;
   pragma Export (C, u00213, "sdl__video__rectanglesS");
   u00214 : constant Version_32 := 16#790ae0ae#;
   pragma Export (C, u00214, "sdl__video__renderersB");
   u00215 : constant Version_32 := 16#a0ad70f6#;
   pragma Export (C, u00215, "sdl__video__renderersS");
   u00216 : constant Version_32 := 16#79bd78c4#;
   pragma Export (C, u00216, "sdl__c_pointersS");
   u00217 : constant Version_32 := 16#580ccfdc#;
   pragma Export (C, u00217, "sdl__video__texturesB");
   u00218 : constant Version_32 := 16#d1ef6f2c#;
   pragma Export (C, u00218, "sdl__video__texturesS");
   u00219 : constant Version_32 := 16#45135534#;
   pragma Export (C, u00219, "sdl__video__pixel_formatsB");
   u00220 : constant Version_32 := 16#b0d0440e#;
   pragma Export (C, u00220, "sdl__video__pixel_formatsS");
   u00221 : constant Version_32 := 16#2fb34529#;
   pragma Export (C, u00221, "system__assertionsB");
   u00222 : constant Version_32 := 16#646ed023#;
   pragma Export (C, u00222, "system__assertionsS");
   u00223 : constant Version_32 := 16#8b2c6428#;
   pragma Export (C, u00223, "ada__assertionsB");
   u00224 : constant Version_32 := 16#cc3ec2fd#;
   pragma Export (C, u00224, "ada__assertionsS");
   u00225 : constant Version_32 := 16#71581158#;
   pragma Export (C, u00225, "sdl__video__pixelsS");
   u00226 : constant Version_32 := 16#c55fea3f#;
   pragma Export (C, u00226, "sdl__video__windowsB");
   u00227 : constant Version_32 := 16#1ee45ff5#;
   pragma Export (C, u00227, "sdl__video__windowsS");
   u00228 : constant Version_32 := 16#6adf51a0#;
   pragma Export (C, u00228, "sdl__video__displaysB");
   u00229 : constant Version_32 := 16#b367c99f#;
   pragma Export (C, u00229, "sdl__video__displaysS");
   u00230 : constant Version_32 := 16#7266431e#;
   pragma Export (C, u00230, "sdl__video__surfacesB");
   u00231 : constant Version_32 := 16#e4e1d37b#;
   pragma Export (C, u00231, "sdl__video__surfacesS");
   u00232 : constant Version_32 := 16#010b3ca4#;
   pragma Export (C, u00232, "sdl__video__renderers__makersB");
   u00233 : constant Version_32 := 16#6fbb9340#;
   pragma Export (C, u00233, "sdl__video__renderers__makersS");
   u00234 : constant Version_32 := 16#6a50b071#;
   pragma Export (C, u00234, "sdl__video__windows__makersB");
   u00235 : constant Version_32 := 16#49567878#;
   pragma Export (C, u00235, "sdl__video__windows__makersS");
   u00236 : constant Version_32 := 16#6d6869cd#;
   pragma Export (C, u00236, "mage__modelS");
   u00237 : constant Version_32 := 16#3c1a89cd#;
   pragma Export (C, u00237, "ada__numerics__aux_floatS");
   u00238 : constant Version_32 := 16#effcb9fc#;
   pragma Export (C, u00238, "ada__numerics__aux_linker_optionsS");
   u00239 : constant Version_32 := 16#3935e87c#;
   pragma Export (C, u00239, "ada__numerics__aux_long_floatS");
   u00240 : constant Version_32 := 16#8333dc5f#;
   pragma Export (C, u00240, "ada__numerics__aux_long_long_floatS");
   u00241 : constant Version_32 := 16#e2164369#;
   pragma Export (C, u00241, "ada__numerics__aux_short_floatS");
   u00242 : constant Version_32 := 16#e77d8041#;
   pragma Export (C, u00242, "system__exn_fltS");
   u00243 : constant Version_32 := 16#f843af17#;
   pragma Export (C, u00243, "mage__eventB");
   u00244 : constant Version_32 := 16#d8de444a#;
   pragma Export (C, u00244, "mage__eventS");
   u00245 : constant Version_32 := 16#c3b32edd#;
   pragma Export (C, u00245, "ada__containers__helpersB");
   u00246 : constant Version_32 := 16#444c93c2#;
   pragma Export (C, u00246, "ada__containers__helpersS");
   u00247 : constant Version_32 := 16#27b838e3#;
   pragma Export (C, u00247, "sdl__eventsS");
   u00248 : constant Version_32 := 16#5948638f#;
   pragma Export (C, u00248, "sdl__events__eventsB");
   u00249 : constant Version_32 := 16#5e73368b#;
   pragma Export (C, u00249, "sdl__events__eventsS");
   u00250 : constant Version_32 := 16#f5c36f20#;
   pragma Export (C, u00250, "sdl__events__controllersS");
   u00251 : constant Version_32 := 16#ae71a01b#;
   pragma Export (C, u00251, "sdl__events__joysticksB");
   u00252 : constant Version_32 := 16#e7c77fad#;
   pragma Export (C, u00252, "sdl__events__joysticksS");
   u00253 : constant Version_32 := 16#b782aba3#;
   pragma Export (C, u00253, "sdl__events__filesS");
   u00254 : constant Version_32 := 16#1d599977#;
   pragma Export (C, u00254, "sdl__events__keyboardsB");
   u00255 : constant Version_32 := 16#9106f4b1#;
   pragma Export (C, u00255, "sdl__events__keyboardsS");
   u00256 : constant Version_32 := 16#66035241#;
   pragma Export (C, u00256, "sdl__events__miceS");
   u00257 : constant Version_32 := 16#446ed451#;
   pragma Export (C, u00257, "sdl__events__touchesS");
   u00258 : constant Version_32 := 16#713bfc83#;
   pragma Export (C, u00258, "sdl__events__windowsS");

   --  BEGIN ELABORATION ORDER
   --  ada%s
   --  ada.characters%s
   --  ada.characters.latin_1%s
   --  interfaces%s
   --  system%s
   --  system.address_operations%s
   --  system.address_operations%b
   --  system.atomic_operations%s
   --  system.io%s
   --  system.io%b
   --  system.parameters%s
   --  system.parameters%b
   --  system.crtl%s
   --  interfaces.c_streams%s
   --  interfaces.c_streams%b
   --  system.os_primitives%s
   --  system.os_primitives%b
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
   --  system.exn_flt%s
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
   --  ada.assertions%s
   --  ada.assertions%b
   --  ada.numerics.aux_linker_options%s
   --  ada.numerics.aux_float%s
   --  ada.numerics.aux_long_float%s
   --  ada.numerics.aux_long_long_float%s
   --  ada.numerics.aux_short_float%s
   --  ada.strings.search%s
   --  ada.strings.search%b
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
   --  interfaces.c.strings%s
   --  interfaces.c.strings%b
   --  system.atomic_counters%s
   --  system.atomic_counters%b
   --  system.fat_flt%s
   --  system.fat_lflt%s
   --  system.fat_llf%s
   --  system.linux%s
   --  system.multiprocessors%s
   --  system.multiprocessors%b
   --  system.os_constants%s
   --  system.os_interface%s
   --  system.os_interface%b
   --  system.put_images%s
   --  system.put_images%b
   --  ada.streams%s
   --  ada.streams%b
   --  system.file_control_block%s
   --  system.finalization_root%s
   --  system.finalization_root%b
   --  ada.finalization%s
   --  ada.containers.helpers%s
   --  ada.containers.helpers%b
   --  system.file_io%s
   --  system.file_io%b
   --  system.stack_usage%s
   --  system.stack_usage%b
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
   --  system.task_info%s
   --  system.task_info%b
   --  system.task_primitives%s
   --  system.interrupt_management%s
   --  system.interrupt_management%b
   --  ada.text_io%s
   --  ada.text_io%b
   --  system.assertions%s
   --  system.assertions%b
   --  system.pool_global%s
   --  system.pool_global%b
   --  system.wid_llu%s
   --  system.img_lli%s
   --  system.tasking%s
   --  system.task_primitives.operations%s
   --  system.tasking.debug%s
   --  system.tasking.debug%b
   --  system.task_primitives.operations%b
   --  system.tasking%b
   --  ada.real_time%s
   --  ada.real_time%b
   --  ada.real_time.delays%s
   --  ada.real_time.delays%b
   --  mage_config%s
   --  sdl%s
   --  sdl%b
   --  sdl.c_pointers%s
   --  sdl.error%s
   --  sdl.error%b
   --  sdl.events%s
   --  sdl.events.files%s
   --  sdl.events.joysticks%s
   --  sdl.events.joysticks%b
   --  sdl.events.controllers%s
   --  sdl.events.touches%s
   --  sdl.video%s
   --  sdl.video%b
   --  sdl.video.palettes%s
   --  sdl.video.palettes%b
   --  sdl.video.pixel_formats%s
   --  sdl.video.pixel_formats%b
   --  sdl.video.pixels%s
   --  sdl.video.rectangles%s
   --  sdl.video.rectangles%b
   --  sdl.video.displays%s
   --  sdl.video.displays%b
   --  sdl.video.surfaces%s
   --  sdl.video.surfaces%b
   --  sdl.video.textures%s
   --  sdl.video.textures%b
   --  sdl.video.windows%s
   --  sdl.video.windows%b
   --  sdl.events.keyboards%s
   --  sdl.events.keyboards%b
   --  sdl.events.mice%s
   --  sdl.events.windows%s
   --  sdl.events.events%s
   --  sdl.events.events%b
   --  sdl.video.renderers%s
   --  sdl.video.renderers%b
   --  sdl.video.renderers.makers%s
   --  sdl.video.renderers.makers%b
   --  sdl.video.windows.makers%s
   --  sdl.video.windows.makers%b
   --  mage%s
   --  mage%b
   --  mage.model%s
   --  mage_config.hardware%s
   --  mage.draw%s
   --  mage.draw%b
   --  mage.event%s
   --  mage.event%b
   --  mage_hat%b
   --  END ELABORATION ORDER

end ada_main;
