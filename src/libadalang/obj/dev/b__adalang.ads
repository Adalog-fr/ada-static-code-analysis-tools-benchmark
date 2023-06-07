pragma Warnings (Off);
pragma Ada_95;
with System;
with System.Parameters;
with System.Secondary_Stack;
package adalangmain is

   procedure adalanginit;
   pragma Export (C, adalanginit, "adalanginit");
   pragma Linker_Constructor (adalanginit);

   procedure adalangfinal;
   pragma Export (C, adalangfinal, "adalangfinal");
   pragma Linker_Destructor (adalangfinal);

   type Version_32 is mod 2 ** 32;
   u00001 : constant Version_32 := 16#943b5c9d#;
   pragma Export (C, u00001, "libadalang__analysisB");
   u00002 : constant Version_32 := 16#4f8ac4b6#;
   pragma Export (C, u00002, "libadalang__analysisS");
   u00003 : constant Version_32 := 16#bdff3d20#;
   pragma Export (C, u00003, "libadalang__auto_providerB");
   u00004 : constant Version_32 := 16#262054cb#;
   pragma Export (C, u00004, "libadalang__auto_providerS");
   u00005 : constant Version_32 := 16#f126c9f2#;
   pragma Export (C, u00005, "libadalang__cB");
   u00006 : constant Version_32 := 16#fc0e4808#;
   pragma Export (C, u00006, "libadalang__cS");
   u00007 : constant Version_32 := 16#ccb2a2d0#;
   pragma Export (C, u00007, "libadalang__commonB");
   u00008 : constant Version_32 := 16#9559944a#;
   pragma Export (C, u00008, "libadalang__commonS");
   u00009 : constant Version_32 := 16#e4566af2#;
   pragma Export (C, u00009, "libadalang__config_pragmasB");
   u00010 : constant Version_32 := 16#2e737e36#;
   pragma Export (C, u00010, "libadalang__config_pragmasS");
   u00011 : constant Version_32 := 16#dd6e2ff4#;
   pragma Export (C, u00011, "libadalang__config_pragmas_implB");
   u00012 : constant Version_32 := 16#d6650e2b#;
   pragma Export (C, u00012, "libadalang__config_pragmas_implS");
   u00013 : constant Version_32 := 16#4cf00ee4#;
   pragma Export (C, u00013, "libadalang__debugB");
   u00014 : constant Version_32 := 16#70fd3ef0#;
   pragma Export (C, u00014, "libadalang__debugS");
   u00015 : constant Version_32 := 16#1187abbd#;
   pragma Export (C, u00015, "libadalang__doc_utilsB");
   u00016 : constant Version_32 := 16#d0e6bad6#;
   pragma Export (C, u00016, "libadalang__doc_utilsS");
   u00017 : constant Version_32 := 16#0e1216c4#;
   pragma Export (C, u00017, "libadalang__env_hooksB");
   u00018 : constant Version_32 := 16#16ac6624#;
   pragma Export (C, u00018, "libadalang__env_hooksS");
   u00019 : constant Version_32 := 16#b0154d7a#;
   pragma Export (C, u00019, "libadalang__expr_evalB");
   u00020 : constant Version_32 := 16#3f3cb846#;
   pragma Export (C, u00020, "libadalang__expr_evalS");
   u00021 : constant Version_32 := 16#cac5dff1#;
   pragma Export (C, u00021, "libadalang__generic_api__introspectionS");
   u00022 : constant Version_32 := 16#48283050#;
   pragma Export (C, u00022, "libadalang__generic_apiB");
   u00023 : constant Version_32 := 16#6f1cf3d3#;
   pragma Export (C, u00023, "libadalang__generic_apiS");
   u00024 : constant Version_32 := 16#ae3b04ac#;
   pragma Export (C, u00024, "libadalang__generic_implB");
   u00025 : constant Version_32 := 16#a8b43f08#;
   pragma Export (C, u00025, "libadalang__generic_implS");
   u00026 : constant Version_32 := 16#0e85eb5a#;
   pragma Export (C, u00026, "libadalang__generic_introspectionB");
   u00027 : constant Version_32 := 16#136b8692#;
   pragma Export (C, u00027, "libadalang__generic_introspectionS");
   u00028 : constant Version_32 := 16#4b657878#;
   pragma Export (C, u00028, "libadalang__helpersB");
   u00029 : constant Version_32 := 16#5d11477a#;
   pragma Export (C, u00029, "libadalang__helpersS");
   u00030 : constant Version_32 := 16#228f1f1e#;
   pragma Export (C, u00030, "libadalang__implementation__c__extensionsB");
   u00031 : constant Version_32 := 16#3f487424#;
   pragma Export (C, u00031, "libadalang__implementation__c__extensionsS");
   u00032 : constant Version_32 := 16#49ab6b0c#;
   pragma Export (C, u00032, "libadalang__implementation__cB");
   u00033 : constant Version_32 := 16#1ba2aeaf#;
   pragma Export (C, u00033, "libadalang__implementation__cS");
   u00034 : constant Version_32 := 16#cc6e86a3#;
   pragma Export (C, u00034, "libadalang__implementation__extensionsB");
   u00035 : constant Version_32 := 16#b64ba2bc#;
   pragma Export (C, u00035, "libadalang__implementation__extensionsS");
   u00036 : constant Version_32 := 16#fee4d93a#;
   pragma Export (C, u00036, "libadalang__implementationB");
   u00037 : constant Version_32 := 16#4e4b382b#;
   pragma Export (C, u00037, "libadalang__implementationS");
   u00038 : constant Version_32 := 16#e66e0368#;
   pragma Export (C, u00038, "libadalang__internal_default_providerB");
   u00039 : constant Version_32 := 16#5cd9eabf#;
   pragma Export (C, u00039, "libadalang__internal_default_providerS");
   u00040 : constant Version_32 := 16#9154e819#;
   pragma Export (C, u00040, "libadalang__introspectionB");
   u00041 : constant Version_32 := 16#9759010f#;
   pragma Export (C, u00041, "libadalang__introspectionS");
   u00042 : constant Version_32 := 16#49a5beac#;
   pragma Export (C, u00042, "libadalang__introspection_implementationB");
   u00043 : constant Version_32 := 16#f6ca1a63#;
   pragma Export (C, u00043, "libadalang__introspection_implementationS");
   u00044 : constant Version_32 := 16#f46e90e6#;
   pragma Export (C, u00044, "libadalang__iterators__extensionsB");
   u00045 : constant Version_32 := 16#00f63d67#;
   pragma Export (C, u00045, "libadalang__iterators__extensionsS");
   u00046 : constant Version_32 := 16#2ce4650f#;
   pragma Export (C, u00046, "libadalang__iteratorsB");
   u00047 : constant Version_32 := 16#fc02a2dc#;
   pragma Export (C, u00047, "libadalang__iteratorsS");
   u00048 : constant Version_32 := 16#dc0041e8#;
   pragma Export (C, u00048, "libadalang__lexerB");
   u00049 : constant Version_32 := 16#2cdbd8cf#;
   pragma Export (C, u00049, "libadalang__lexerS");
   u00050 : constant Version_32 := 16#2b7eeaef#;
   pragma Export (C, u00050, "libadalang__lexer_implementationB");
   u00051 : constant Version_32 := 16#61390384#;
   pragma Export (C, u00051, "libadalang__lexer_implementationS");
   u00052 : constant Version_32 := 16#d670f460#;
   pragma Export (C, u00052, "libadalang__lexer_state_machineB");
   u00053 : constant Version_32 := 16#a17e749c#;
   pragma Export (C, u00053, "libadalang__lexer_state_machineS");
   u00054 : constant Version_32 := 16#ec0a75cb#;
   pragma Export (C, u00054, "libadalang__parsersB");
   u00055 : constant Version_32 := 16#8ebce2e3#;
   pragma Export (C, u00055, "libadalang__parsersS");
   u00056 : constant Version_32 := 16#2c1d524a#;
   pragma Export (C, u00056, "libadalang__pp_implB");
   u00057 : constant Version_32 := 16#e8561db0#;
   pragma Export (C, u00057, "libadalang__pp_implS");
   u00058 : constant Version_32 := 16#a95c1978#;
   pragma Export (C, u00058, "libadalang__pp_lexerB");
   u00059 : constant Version_32 := 16#c84e8cd3#;
   pragma Export (C, u00059, "libadalang__pp_lexerS");
   u00060 : constant Version_32 := 16#740ebad4#;
   pragma Export (C, u00060, "libadalang__preprocessingB");
   u00061 : constant Version_32 := 16#4faa7cfa#;
   pragma Export (C, u00061, "libadalang__preprocessingS");
   u00062 : constant Version_32 := 16#b6585703#;
   pragma Export (C, u00062, "libadalang__private_convertersS");
   u00063 : constant Version_32 := 16#e54c96dd#;
   pragma Export (C, u00063, "libadalang__project_providerB");
   u00064 : constant Version_32 := 16#371787a5#;
   pragma Export (C, u00064, "libadalang__project_providerS");
   u00065 : constant Version_32 := 16#35d6e76e#;
   pragma Export (C, u00065, "libadalang__public_convertersB");
   u00066 : constant Version_32 := 16#8b7d6778#;
   pragma Export (C, u00066, "libadalang__public_convertersS");
   u00067 : constant Version_32 := 16#59ab8a9c#;
   pragma Export (C, u00067, "libadalang__rewritingB");
   u00068 : constant Version_32 := 16#42705f47#;
   pragma Export (C, u00068, "libadalang__rewritingS");
   u00069 : constant Version_32 := 16#fe426be4#;
   pragma Export (C, u00069, "libadalang__rewriting_implementationB");
   u00070 : constant Version_32 := 16#487b888b#;
   pragma Export (C, u00070, "libadalang__rewriting_implementationS");
   u00071 : constant Version_32 := 16#a65ea0b3#;
   pragma Export (C, u00071, "libadalang__sourcesB");
   u00072 : constant Version_32 := 16#7b2a24ef#;
   pragma Export (C, u00072, "libadalang__sourcesS");
   u00073 : constant Version_32 := 16#f4ac860a#;
   pragma Export (C, u00073, "libadalang__unit_filesB");
   u00074 : constant Version_32 := 16#889261e2#;
   pragma Export (C, u00074, "libadalang__unit_filesS");
   u00075 : constant Version_32 := 16#c223db45#;
   pragma Export (C, u00075, "libadalang__unparsingB");
   u00076 : constant Version_32 := 16#bddad1b2#;
   pragma Export (C, u00076, "libadalang__unparsingS");
   u00077 : constant Version_32 := 16#c0409377#;
   pragma Export (C, u00077, "libadalang__unparsing_implementationB");
   u00078 : constant Version_32 := 16#96fd8ff7#;
   pragma Export (C, u00078, "libadalang__unparsing_implementationS");
   u00079 : constant Version_32 := 16#82e75d58#;
   pragma Export (C, u00079, "libadalangS");

   --  BEGIN ELABORATION ORDER
   --  ada%s
   --  ada.characters%s
   --  ada.characters.latin_1%s
   --  ada.wide_characters%s
   --  ada.wide_wide_characters%s
   --  interfaces%s
   --  system%s
   --  system.address_operations%s
   --  system.address_operations%b
   --  system.atomic_operations%s
   --  system.byte_swapping%s
   --  system.float_control%s
   --  system.float_control%b
   --  system.img_char%s
   --  system.img_char%b
   --  system.io%s
   --  system.io%b
   --  system.parameters%s
   --  system.parameters%b
   --  system.crtl%s
   --  interfaces.c_streams%s
   --  interfaces.c_streams%b
   --  system.os_primitives%s
   --  system.os_primitives%b
   --  system.powten_llf%s
   --  system.restrictions%s
   --  system.restrictions%b
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
   --  system.utf_32%s
   --  system.utf_32%b
   --  ada.wide_characters.unicode%s
   --  ada.wide_characters.unicode%b
   --  ada.wide_wide_characters.unicode%s
   --  ada.wide_wide_characters.unicode%b
   --  system.wch_con%s
   --  system.wch_con%b
   --  system.wch_jis%s
   --  system.wch_jis%b
   --  system.wch_cnv%s
   --  system.wch_cnv%b
   --  system.compare_array_unsigned_32%s
   --  system.compare_array_unsigned_32%b
   --  system.compare_array_unsigned_8%s
   --  system.compare_array_unsigned_8%b
   --  system.concat_2%s
   --  system.concat_2%b
   --  system.concat_3%s
   --  system.concat_3%b
   --  system.concat_4%s
   --  system.concat_4%b
   --  system.concat_5%s
   --  system.concat_5%b
   --  system.concat_6%s
   --  system.concat_6%b
   --  system.concat_9%s
   --  system.concat_9%b
   --  system.crc32%s
   --  system.crc32%b
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
   --  ada.assertions%s
   --  ada.assertions%b
   --  ada.characters.conversions%s
   --  ada.characters.conversions%b
   --  ada.command_line%s
   --  ada.command_line%b
   --  ada.containers.prime_numbers%s
   --  ada.containers.prime_numbers%b
   --  ada.containers.stable_sorting%s
   --  ada.containers.stable_sorting%b
   --  ada.exceptions.is_null_occurrence%s
   --  ada.exceptions.is_null_occurrence%b
   --  ada.strings.equal_case_insensitive%s
   --  ada.strings.equal_case_insensitive%b
   --  ada.strings.hash%s
   --  ada.strings.hash%b
   --  ada.strings.hash_case_insensitive%s
   --  ada.strings.hash_case_insensitive%b
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
   --  ada.strings.wide_wide_hash%s
   --  ada.strings.wide_wide_hash%b
   --  ada.tags%s
   --  ada.tags%b
   --  ada.strings.text_buffers%s
   --  ada.strings.text_buffers%b
   --  ada.strings.text_buffers.utils%s
   --  ada.strings.text_buffers.utils%b
   --  ada.wide_wide_characters.handling%s
   --  ada.wide_wide_characters.handling%b
   --  gnat%s
   --  gnat.byte_swapping%s
   --  gnat.byte_swapping%b
   --  gnat.case_util%s
   --  gnat.debug_utilities%s
   --  gnat.debug_utilities%b
   --  gnat.heap_sort%s
   --  gnat.heap_sort%b
   --  gnat.htable%s
   --  gnat.htable%b
   --  gnat.io%s
   --  gnat.io%b
   --  gnat.os_lib%s
   --  gnat.source_info%s
   --  gnat.strings%s
   --  interfaces.c.extensions%s
   --  interfaces.c.strings%s
   --  interfaces.c.strings%b
   --  ada.environment_variables%s
   --  ada.environment_variables%b
   --  system.arith_32%s
   --  system.arith_32%b
   --  system.arith_64%s
   --  system.arith_64%b
   --  system.atomic_counters%s
   --  system.atomic_counters%b
   --  system.aux_dec%s
   --  system.aux_dec%b
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
   --  system.communication%s
   --  system.communication%b
   --  system.file_control_block%s
   --  system.finalization_root%s
   --  system.finalization_root%b
   --  ada.finalization%s
   --  ada.containers.helpers%s
   --  ada.containers.helpers%b
   --  ada.containers.hash_tables%s
   --  ada.containers.red_black_trees%s
   --  system.file_io%s
   --  system.file_io%b
   --  ada.streams.stream_io%s
   --  ada.streams.stream_io%b
   --  system.regpat%s
   --  system.regpat%b
   --  gnat.regpat%s
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
   --  ada.strings.unbounded.aux%s
   --  ada.strings.unbounded.aux%b
   --  ada.strings.unbounded.hash%s
   --  ada.strings.unbounded.hash%b
   --  ada.strings.wide_wide_maps%s
   --  ada.strings.wide_wide_maps%b
   --  ada.strings.wide_wide_search%s
   --  ada.strings.wide_wide_search%b
   --  ada.strings.wide_wide_unbounded%s
   --  ada.strings.wide_wide_unbounded%b
   --  ada.strings.wide_wide_unbounded.aux%s
   --  ada.strings.wide_wide_unbounded.aux%b
   --  system.task_info%s
   --  system.task_info%b
   --  system.task_lock%s
   --  system.task_lock%b
   --  gnat.task_lock%s
   --  system.task_primitives%s
   --  system.interrupt_management%s
   --  system.interrupt_management%b
   --  system.val_bool%s
   --  system.val_bool%b
   --  system.val_enum_8%s
   --  system.val_fixed_64%s
   --  system.val_llf%s
   --  system.val_lllu%s
   --  system.val_llli%s
   --  ada.calendar%s
   --  ada.calendar%b
   --  ada.calendar.delays%s
   --  ada.calendar.delays%b
   --  ada.calendar.time_zones%s
   --  ada.calendar.time_zones%b
   --  ada.calendar.formatting%s
   --  ada.calendar.formatting%b
   --  ada.text_io%s
   --  ada.text_io%b
   --  ada.text_io.text_streams%s
   --  ada.text_io.text_streams%b
   --  gnat.byte_order_mark%s
   --  gnat.byte_order_mark%b
   --  gnat.calendar%s
   --  gnat.calendar%b
   --  gnat.directory_operations%s
   --  gnat.directory_operations%b
   --  gnat.dynamic_htables%s
   --  gnat.dynamic_htables%b
   --  gnat.secure_hashes%s
   --  gnat.secure_hashes%b
   --  gnat.secure_hashes.md5%s
   --  gnat.secure_hashes.md5%b
   --  gnat.md5%s
   --  gnat.md5%b
   --  gnat.string_split%s
   --  gnat.string_split%b
   --  gnat.traceback%s
   --  gnat.traceback%b
   --  gnat.traceback.symbolic%s
   --  gnat.tty%s
   --  gnat.tty%b
   --  system.assertions%s
   --  system.assertions%b
   --  system.checked_pools%s
   --  system.exn_int%s
   --  system.exn_lli%s
   --  system.file_attributes%s
   --  system.img_util%s
   --  system.img_util%b
   --  system.img_fixed_32%s
   --  system.img_fixed_64%s
   --  system.pool_global%s
   --  system.pool_global%b
   --  gnat.expect%s
   --  gnat.expect%b
   --  gnat.expect.tty%s
   --  gnat.expect.tty%b
   --  gnat.sockets%s
   --  gnat.sockets.linker_options%s
   --  gnat.sockets.poll%s
   --  gnat.sockets.thin_common%s
   --  gnat.sockets.thin_common%b
   --  gnat.sockets.thin%s
   --  gnat.sockets.thin%b
   --  gnat.sockets%b
   --  gnat.sockets.poll%b
   --  system.random_seed%s
   --  system.random_seed%b
   --  system.random_numbers%s
   --  system.random_numbers%b
   --  system.regexp%s
   --  system.regexp%b
   --  ada.directories%s
   --  ada.directories.hierarchical_file_names%s
   --  ada.directories.validity%s
   --  ada.directories.validity%b
   --  ada.directories%b
   --  ada.directories.hierarchical_file_names%b
   --  gnat.regexp%s
   --  system.strings.stream_ops%s
   --  system.strings.stream_ops%b
   --  system.wid_lllu%s
   --  system.img_llli%s
   --  system.wid_llu%s
   --  system.img_lli%s
   --  system.tasking%s
   --  system.task_primitives.operations%s
   --  system.tasking.debug%s
   --  system.tasking.debug%b
   --  system.task_primitives.operations%b
   --  system.tasking%b
   --  system.img_llu%s
   --  gnat.calendar.time_io%s
   --  gnat.calendar.time_io%b
   --  gnat.debug_pools%s
   --  gnat.debug_pools%b
   --  system.img_llf%s
   --  system.img_real%s
   --  system.soft_links.tasking%s
   --  system.soft_links.tasking%b
   --  system.tasking.initialization%s
   --  system.tasking.task_attributes%s
   --  system.tasking.task_attributes%b
   --  system.tasking.initialization%b
   --  system.tasking.protected_objects%s
   --  system.tasking.protected_objects%b
   --  system.tasking.protected_objects.entries%s
   --  system.tasking.protected_objects.entries%b
   --  system.tasking.queuing%s
   --  system.tasking.queuing%b
   --  system.tasking.utilities%s
   --  system.tasking.utilities%b
   --  system.tasking.entry_calls%s
   --  system.tasking.rendezvous%s
   --  system.tasking.protected_objects.operations%s
   --  system.tasking.protected_objects.operations%b
   --  system.tasking.entry_calls%b
   --  system.tasking.rendezvous%b
   --  gnat.semaphores%s
   --  gnat.semaphores%b
   --  gnatcoll%s
   --  gnatcoll.gmp%s
   --  gnatcoll.gmp.lib%s
   --  gnatcoll.gmp.integers%s
   --  gnatcoll.gmp.integers%b
   --  gnatcoll.gmp.integers.misc%s
   --  gnatcoll.gmp.integers.misc%b
   --  gnatcoll.storage_pools%s
   --  unicode%s
   --  unicode.names%s
   --  unicode.names.basic_latin%s
   --  unicode%b
   --  unicode.names.currency_symbols%s
   --  unicode.names.cyrillic%s
   --  unicode.names.general_punctuation%s
   --  unicode.names.latin_1_supplement%s
   --  unicode.names.latin_extended_a%s
   --  unicode.names.latin_extended_b%s
   --  unicode.names.letterlike_symbols%s
   --  unicode.names.spacing_modifier_letters%s
   --  dom%s
   --  gnatcoll.atomic%s
   --  gnatcoll.atomic%b
   --  gnatcoll.gmp.rational_numbers%s
   --  gnatcoll.gmp.rational_numbers%b
   --  gnatcoll.locks%s
   --  gnatcoll.locks%b
   --  gnatcoll.memory%s
   --  gnatcoll.memory%b
   --  gnatcoll.os%s
   --  gnatcoll.storage_pools.headers%s
   --  gnatcoll.storage_pools.headers%b
   --  gnatcoll.refcount%s
   --  gnatcoll.refcount%b
   --  gnatcoll.string_builders%s
   --  gnatcoll.string_builders%b
   --  gnatcoll.strings_impl%s
   --  gnatcoll.strings_impl%b
   --  gnatcoll.strings%s
   --  gnatcoll.strings%b
   --  gnatcoll.mmap%s
   --  gnatcoll.mmap.system%s
   --  gnatcoll.mmap.system%b
   --  gnatcoll.mmap%b
   --  gnatcoll.templates%s
   --  gnatcoll.templates%b
   --  gnatcoll.terminal%s
   --  gnatcoll.terminal%b
   --  gnatcoll.utils%s
   --  gnatcoll.utils%b
   --  gnatcoll.vfs_types%s
   --  gnatcoll.io%s
   --  gnatcoll.io%b
   --  gnatcoll.path%s
   --  gnatcoll.path%b
   --  gnatcoll.io.native%s
   --  gnatcoll.io.native%b
   --  gnatcoll.remote%s
   --  gnatcoll.remote.db%s
   --  gnatcoll.remote.db%b
   --  gnatcoll.io.remote%s
   --  gnatcoll.io.remote.unix%s
   --  gnatcoll.io.remote.unix%b
   --  gnatcoll.io.remote.windows%s
   --  gnatcoll.io.remote.windows%b
   --  gnatcoll.io.remote%b
   --  gnatcoll.vfs%s
   --  gnatcoll.vfs%b
   --  gnatcoll.file_paths%s
   --  gnatcoll.file_paths%b
   --  gnatcoll.opt_parse%s
   --  gnatcoll.opt_parse%b
   --  gnatcoll.traces%s
   --  gnatcoll.traces%b
   --  gnatcoll.iconv%s
   --  gnatcoll.iconv%b
   --  gnatcoll.vfs_utils%s
   --  gnatcoll.vfs_utils%b
   --  gpr%s
   --  gpr.attr%s
   --  gpr.cset%s
   --  gpr.cset%b
   --  gpr.debug%s
   --  gpr.debug%b
   --  gpr.err%s
   --  gpr.names%s
   --  gpr.opt%s
   --  gpr.osint%s
   --  gpr.erroutc%s
   --  gpr.output%s
   --  gpr.output%b
   --  gpr.names%b
   --  gpr.osint%b
   --  gpr.scans%s
   --  gpr.scans%b
   --  gpr.sinput%s
   --  gpr.sinput%b
   --  gpr.erroutc%b
   --  gpr.snames%s
   --  gpr.snames%b
   --  gpr%b
   --  gpr.attr%b
   --  gpr.err%b
   --  gpr.ali%s
   --  gpr.ali%b
   --  gpr.attr.pm%s
   --  gpr.attr.pm%b
   --  gpr.com%s
   --  gpr.ext%s
   --  gpr.ext%b
   --  gpr.tempdir%s
   --  gpr.tempdir%b
   --  libadalang%s
   --  libadalang.pp_lexer%s
   --  libadalang.pp_lexer%b
   --  libadalang.sources%s
   --  libadalang.sources%b
   --  sax%s
   --  sax.htable%s
   --  sax.htable%b
   --  sax.pointers%s
   --  sax.pointers%b
   --  sax.state_machines%s
   --  sax.state_machines%b
   --  schema%s
   --  schema%b
   --  unicode.ccs%s
   --  unicode.ccs%b
   --  unicode.ccs.iso_8859_1%s
   --  unicode.ccs.iso_8859_1%b
   --  unicode.ccs.iso_8859_15%s
   --  unicode.ccs.iso_8859_15%b
   --  unicode.ccs.iso_8859_2%s
   --  unicode.ccs.iso_8859_2%b
   --  unicode.ccs.iso_8859_3%s
   --  unicode.ccs.iso_8859_3%b
   --  unicode.ccs.iso_8859_4%s
   --  unicode.ccs.iso_8859_4%b
   --  unicode.ccs.windows_1251%s
   --  unicode.ccs.windows_1251%b
   --  unicode.ccs.windows_1252%s
   --  unicode.ccs.windows_1252%b
   --  unicode.ces%s
   --  unicode.ces%b
   --  sax.symbols%s
   --  sax.symbols%b
   --  sax.locators%s
   --  sax.locators%b
   --  sax.exceptions%s
   --  sax.exceptions%b
   --  unicode.ces.utf32%s
   --  unicode.ces.utf32%b
   --  unicode.ces.basic_8bit%s
   --  unicode.ces.basic_8bit%b
   --  unicode.ces.utf16%s
   --  unicode.ces.utf16%b
   --  unicode.ces.utf8%s
   --  unicode.ces.utf8%b
   --  sax.encodings%s
   --  sax.models%s
   --  sax.models%b
   --  sax.attributes%s
   --  sax.attributes%b
   --  sax.utils%s
   --  sax.utils%b
   --  dom.core%s
   --  dom.core%b
   --  schema.date_time%s
   --  schema.date_time%b
   --  schema.decimal%s
   --  schema.decimal%b
   --  schema.simple_types%s
   --  schema.simple_types%b
   --  unicode.encodings%s
   --  unicode.encodings%b
   --  dom.core.nodes%s
   --  dom.core.nodes%b
   --  dom.core.attrs%s
   --  dom.core.attrs%b
   --  dom.core.character_datas%s
   --  dom.core.character_datas%b
   --  dom.core.documents%s
   --  dom.core.elements%s
   --  dom.core.elements%b
   --  dom.core.documents%b
   --  input_sources%s
   --  input_sources%b
   --  input_sources.file%s
   --  input_sources.file%b
   --  input_sources.strings%s
   --  input_sources.strings%b
   --  sax.readers%s
   --  sax.readers%b
   --  schema.validators%s
   --  schema.readers%s
   --  schema.schema_readers%s
   --  schema.schema_readers%b
   --  schema.readers%b
   --  schema.validators.xsd_grammar%s
   --  schema.validators.xsd_grammar%b
   --  schema.validators%b
   --  schema.dom_readers%s
   --  schema.dom_readers%b
   --  gpr.knowledge%s
   --  gpr.sdefault%s
   --  gpr.strt%s
   --  gpr.util%s
   --  gpr.env%s
   --  gpr.env%b
   --  gpr.knowledge%b
   --  gpr.sdefault%b
   --  gpr.tree%s
   --  gpr.tree%b
   --  gpr.dect%s
   --  gpr.dect%b
   --  gpr.nmsc%s
   --  gpr.nmsc%b
   --  gpr.part%s
   --  gpr.part%b
   --  gpr.proc%s
   --  gpr.proc%b
   --  gpr.conf%s
   --  gpr.conf%b
   --  gpr.strt%b
   --  gpr.version%s
   --  gpr.version%b
   --  gpr_build_util%s
   --  gpr_build_util%b
   --  gpr.util%b
   --  gpr.pp%s
   --  gpr.pp%b
   --  gnatcoll.projects%s
   --  gnatcoll.projects.krunch%s
   --  gnatcoll.projects.krunch%b
   --  gnatcoll.projects.normalize%s
   --  gnatcoll.projects.normalize%b
   --  gnatcoll.projects%b
   --  libadalang.common%s
   --  libadalang.config_pragmas_impl%s
   --  libadalang.lexer%s
   --  libadalang.lexer_implementation%s
   --  libadalang.lexer_state_machine%s
   --  libadalang.lexer_state_machine%b
   --  libadalang.parsers%s
   --  libadalang.implementation%s
   --  libadalang.config_pragmas_impl%b
   --  libadalang.debug%s
   --  libadalang.debug%b
   --  libadalang.analysis%s
   --  libadalang.doc_utils%s
   --  libadalang.doc_utils%b
   --  libadalang.env_hooks%s
   --  libadalang.expr_eval%s
   --  libadalang.expr_eval%b
   --  libadalang.generic_api%s
   --  libadalang.generic_introspection%s
   --  libadalang.implementation.extensions%s
   --  libadalang.internal_default_provider%s
   --  libadalang.introspection_implementation%s
   --  libadalang.introspection_implementation%b
   --  libadalang.lexer%b
   --  libadalang.lexer_implementation%b
   --  libadalang.private_converters%s
   --  libadalang.common%b
   --  libadalang.generic_impl%s
   --  libadalang.implementation%b
   --  libadalang.parsers%b
   --  libadalang.project_provider%s
   --  libadalang.public_converters%s
   --  libadalang.public_converters%b
   --  libadalang.analysis%b
   --  libadalang.env_hooks%b
   --  libadalang.generic_api%b
   --  libadalang.generic_impl%b
   --  libadalang.generic_introspection%b
   --  libadalang.unit_files%s
   --  libadalang.unit_files%b
   --  libadalang.implementation.extensions%b
   --  libadalang.internal_default_provider%b
   --  libadalang.project_provider%b
   --  libadalang.auto_provider%s
   --  libadalang.auto_provider%b
   --  libadalang.c%s
   --  libadalang.c%b
   --  libadalang.config_pragmas%s
   --  libadalang.config_pragmas%b
   --  libadalang.generic_api.introspection%s
   --  libadalang.implementation.c%s
   --  libadalang.implementation.c%b
   --  libadalang.introspection%s
   --  libadalang.introspection%b
   --  libadalang.iterators%s
   --  libadalang.iterators.extensions%s
   --  libadalang.iterators.extensions%b
   --  libadalang.iterators%b
   --  libadalang.preprocessing%s
   --  libadalang.pp_impl%s
   --  libadalang.pp_impl%b
   --  libadalang.preprocessing%b
   --  libadalang.helpers%s
   --  libadalang.helpers%b
   --  libadalang.implementation.c.extensions%s
   --  libadalang.implementation.c.extensions%b
   --  libadalang.rewriting_implementation%s
   --  libadalang.unparsing_implementation%s
   --  libadalang.unparsing_implementation%b
   --  libadalang.rewriting_implementation%b
   --  libadalang.rewriting%s
   --  libadalang.rewriting%b
   --  libadalang.unparsing%s
   --  libadalang.unparsing%b
   --  END ELABORATION ORDER

end adalangmain;
