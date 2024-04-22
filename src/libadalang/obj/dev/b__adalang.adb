pragma Warnings (Off);
pragma Ada_95;
pragma Source_File_Name (adalangmain, Spec_File_Name => "b__adalang.ads");
pragma Source_File_Name (adalangmain, Body_File_Name => "b__adalang.adb");
pragma Suppress (Overflow_Check);

with System.Restrictions;
with Ada.Exceptions;

package body adalangmain is

   E155 : Short_Integer; pragma Import (Ada, E155, "system__os_lib_E");
   E092 : Short_Integer; pragma Import (Ada, E092, "ada__exceptions_E");
   E088 : Short_Integer; pragma Import (Ada, E088, "system__soft_links_E");
   E086 : Short_Integer; pragma Import (Ada, E086, "system__exception_table_E");
   E083 : Short_Integer; pragma Import (Ada, E083, "ada__containers_E");
   E150 : Short_Integer; pragma Import (Ada, E150, "ada__io_exceptions_E");
   E101 : Short_Integer; pragma Import (Ada, E101, "ada__numerics_E");
   E137 : Short_Integer; pragma Import (Ada, E137, "ada__strings_E");
   E139 : Short_Integer; pragma Import (Ada, E139, "ada__strings__maps_E");
   E142 : Short_Integer; pragma Import (Ada, E142, "ada__strings__maps__constants_E");
   E125 : Short_Integer; pragma Import (Ada, E125, "interfaces__c_E");
   E095 : Short_Integer; pragma Import (Ada, E095, "system__exceptions_E");
   E164 : Short_Integer; pragma Import (Ada, E164, "system__object_reader_E");
   E132 : Short_Integer; pragma Import (Ada, E132, "system__dwarf_lines_E");
   E178 : Short_Integer; pragma Import (Ada, E178, "system__soft_links__initialize_E");
   E120 : Short_Integer; pragma Import (Ada, E120, "system__traceback__symbolic_E");
   E100 : Short_Integer; pragma Import (Ada, E100, "system__img_int_E");
   E145 : Short_Integer; pragma Import (Ada, E145, "system__img_uns_E");
   E337 : Short_Integer; pragma Import (Ada, E337, "ada__assertions_E");
   E184 : Short_Integer; pragma Import (Ada, E184, "ada__strings__utf_encoding_E");
   E192 : Short_Integer; pragma Import (Ada, E192, "ada__tags_E");
   E182 : Short_Integer; pragma Import (Ada, E182, "ada__strings__text_buffers_E");
   E261 : Short_Integer; pragma Import (Ada, E261, "gnat_E");
   E258 : Short_Integer; pragma Import (Ada, E258, "interfaces__c__strings_E");
   E737 : Short_Integer; pragma Import (Ada, E737, "system__aux_dec_E");
   E212 : Short_Integer; pragma Import (Ada, E212, "ada__streams_E");
   E273 : Short_Integer; pragma Import (Ada, E273, "system__file_control_block_E");
   E214 : Short_Integer; pragma Import (Ada, E214, "system__finalization_root_E");
   E210 : Short_Integer; pragma Import (Ada, E210, "ada__finalization_E");
   E272 : Short_Integer; pragma Import (Ada, E272, "system__file_io_E");
   E561 : Short_Integer; pragma Import (Ada, E561, "ada__streams__stream_io_E");
   E281 : Short_Integer; pragma Import (Ada, E281, "system__regpat_E");
   E237 : Short_Integer; pragma Import (Ada, E237, "system__storage_pools_E");
   E239 : Short_Integer; pragma Import (Ada, E239, "system__finalization_masters_E");
   E331 : Short_Integer; pragma Import (Ada, E331, "system__storage_pools__subpools_E");
   E198 : Short_Integer; pragma Import (Ada, E198, "ada__strings__unbounded_E");
   E403 : Short_Integer; pragma Import (Ada, E403, "ada__strings__wide_wide_maps_E");
   E399 : Short_Integer; pragma Import (Ada, E399, "ada__strings__wide_wide_unbounded_E");
   E701 : Short_Integer; pragma Import (Ada, E701, "system__task_info_E");
   E245 : Short_Integer; pragma Import (Ada, E245, "ada__calendar_E");
   E390 : Short_Integer; pragma Import (Ada, E390, "ada__calendar__delays_E");
   E251 : Short_Integer; pragma Import (Ada, E251, "ada__calendar__time_zones_E");
   E268 : Short_Integer; pragma Import (Ada, E268, "ada__text_io_E");
   E479 : Short_Integer; pragma Import (Ada, E479, "ada__text_io__text_streams_E");
   E538 : Short_Integer; pragma Import (Ada, E538, "gnat__byte_order_mark_E");
   E263 : Short_Integer; pragma Import (Ada, E263, "gnat__calendar_E");
   E354 : Short_Integer; pragma Import (Ada, E354, "gnat__directory_operations_E");
   E548 : Short_Integer; pragma Import (Ada, E548, "gnat__dynamic_htables_E");
   E657 : Short_Integer; pragma Import (Ada, E657, "gnat__secure_hashes_E");
   E659 : Short_Integer; pragma Import (Ada, E659, "gnat__secure_hashes__md5_E");
   E655 : Short_Integer; pragma Import (Ada, E655, "gnat__md5_E");
   E555 : Short_Integer; pragma Import (Ada, E555, "gnat__string_split_E");
   E511 : Short_Integer; pragma Import (Ada, E511, "gnat__tty_E");
   E302 : Short_Integer; pragma Import (Ada, E302, "system__checked_pools_E");
   E241 : Short_Integer; pragma Import (Ada, E241, "system__pool_global_E");
   E328 : Short_Integer; pragma Import (Ada, E328, "gnat__expect_E");
   E509 : Short_Integer; pragma Import (Ada, E509, "gnat__expect__tty_E");
   E565 : Short_Integer; pragma Import (Ada, E565, "gnat__sockets_E");
   E568 : Short_Integer; pragma Import (Ada, E568, "gnat__sockets__poll_E");
   E572 : Short_Integer; pragma Import (Ada, E572, "gnat__sockets__thin_common_E");
   E570 : Short_Integer; pragma Import (Ada, E570, "gnat__sockets__thin_E");
   E380 : Short_Integer; pragma Import (Ada, E380, "system__random_seed_E");
   E367 : Short_Integer; pragma Import (Ada, E367, "system__regexp_E");
   E358 : Short_Integer; pragma Import (Ada, E358, "ada__directories_E");
   E640 : Short_Integer; pragma Import (Ada, E640, "system__img_llli_E");
   E275 : Short_Integer; pragma Import (Ada, E275, "system__img_lli_E");
   E697 : Short_Integer; pragma Import (Ada, E697, "system__task_primitives__operations_E");
   E277 : Short_Integer; pragma Import (Ada, E277, "system__img_llu_E");
   E266 : Short_Integer; pragma Import (Ada, E266, "gnat__calendar__time_io_E");
   E289 : Short_Integer; pragma Import (Ada, E289, "gnat__debug_pools_E");
   E717 : Short_Integer; pragma Import (Ada, E717, "system__tasking__initialization_E");
   E707 : Short_Integer; pragma Import (Ada, E707, "system__tasking__protected_objects_E");
   E713 : Short_Integer; pragma Import (Ada, E713, "system__tasking__protected_objects__entries_E");
   E725 : Short_Integer; pragma Import (Ada, E725, "system__tasking__queuing_E");
   E735 : Short_Integer; pragma Import (Ada, E735, "gnat__semaphores_E");
   E431 : Short_Integer; pragma Import (Ada, E431, "gnatcoll__gmp__integers_E");
   E445 : Short_Integer; pragma Import (Ada, E445, "unicode_E");
   E230 : Short_Integer; pragma Import (Ada, E230, "gnatcoll__atomic_E");
   E673 : Short_Integer; pragma Import (Ada, E673, "gnatcoll__gmp__rational_numbers_E");
   E733 : Short_Integer; pragma Import (Ada, E733, "gnatcoll__locks_E");
   E287 : Short_Integer; pragma Import (Ada, E287, "gnatcoll__memory_E");
   E324 : Short_Integer; pragma Import (Ada, E324, "gnatcoll__os_E");
   E232 : Short_Integer; pragma Import (Ada, E232, "gnatcoll__storage_pools__headers_E");
   E228 : Short_Integer; pragma Import (Ada, E228, "gnatcoll__refcount_E");
   E323 : Short_Integer; pragma Import (Ada, E323, "gnatcoll__string_builders_E");
   E310 : Short_Integer; pragma Import (Ada, E310, "gnatcoll__strings_impl_E");
   E308 : Short_Integer; pragma Import (Ada, E308, "gnatcoll__strings_E");
   E307 : Short_Integer; pragma Import (Ada, E307, "gnatcoll__strings_E");
   E304 : Short_Integer; pragma Import (Ada, E304, "gnatcoll__mmap_E");
   E306 : Short_Integer; pragma Import (Ada, E306, "gnatcoll__mmap__system_E");
   E315 : Short_Integer; pragma Import (Ada, E315, "gnatcoll__templates_E");
   E317 : Short_Integer; pragma Import (Ada, E317, "gnatcoll__terminal_E");
   E319 : Short_Integer; pragma Import (Ada, E319, "gnatcoll__utils_E");
   E349 : Short_Integer; pragma Import (Ada, E349, "gnatcoll__io_E");
   E369 : Short_Integer; pragma Import (Ada, E369, "gnatcoll__path_E");
   E356 : Short_Integer; pragma Import (Ada, E356, "gnatcoll__io__native_E");
   E372 : Short_Integer; pragma Import (Ada, E372, "gnatcoll__remote_E");
   E376 : Short_Integer; pragma Import (Ada, E376, "gnatcoll__remote__db_E");
   E352 : Short_Integer; pragma Import (Ada, E352, "gnatcoll__io__remote_E");
   E371 : Short_Integer; pragma Import (Ada, E371, "gnatcoll__io__remote__unix_E");
   E374 : Short_Integer; pragma Import (Ada, E374, "gnatcoll__io__remote__windows_E");
   E341 : Short_Integer; pragma Import (Ada, E341, "gnatcoll__vfs_E");
   E687 : Short_Integer; pragma Import (Ada, E687, "gnatcoll__file_paths_E");
   E731 : Short_Integer; pragma Import (Ada, E731, "gnatcoll__opt_parse_E");
   E243 : Short_Integer; pragma Import (Ada, E243, "gnatcoll__traces_E");
   E418 : Short_Integer; pragma Import (Ada, E418, "gnatcoll__iconv_E");
   E517 : Short_Integer; pragma Import (Ada, E517, "gnatcoll__vfs_utils_E");
   E519 : Short_Integer; pragma Import (Ada, E519, "gpr_E");
   E521 : Short_Integer; pragma Import (Ada, E521, "gpr__attr_E");
   E525 : Short_Integer; pragma Import (Ada, E525, "gpr__cset_E");
   E531 : Short_Integer; pragma Import (Ada, E531, "gpr__debug_E");
   E523 : Short_Integer; pragma Import (Ada, E523, "gpr__err_E");
   E529 : Short_Integer; pragma Import (Ada, E529, "gpr__names_E");
   E540 : Short_Integer; pragma Import (Ada, E540, "gpr__osint_E");
   E527 : Short_Integer; pragma Import (Ada, E527, "gpr__erroutc_E");
   E533 : Short_Integer; pragma Import (Ada, E533, "gpr__output_E");
   E542 : Short_Integer; pragma Import (Ada, E542, "gpr__scans_E");
   E536 : Short_Integer; pragma Import (Ada, E536, "gpr__sinput_E");
   E544 : Short_Integer; pragma Import (Ada, E544, "gpr__snames_E");
   E574 : Short_Integer; pragma Import (Ada, E574, "gpr__ali_E");
   E588 : Short_Integer; pragma Import (Ada, E588, "gpr__attr__pm_E");
   E594 : Short_Integer; pragma Import (Ada, E594, "gpr__ext_E");
   E557 : Short_Integer; pragma Import (Ada, E557, "gpr__tempdir_E");
   E059 : Short_Integer; pragma Import (Ada, E059, "libadalang__pp_lexer_E");
   E072 : Short_Integer; pragma Import (Ada, E072, "libadalang__sources_E");
   E460 : Short_Integer; pragma Import (Ada, E460, "sax__htable_E");
   E467 : Short_Integer; pragma Import (Ada, E467, "sax__pointers_E");
   E647 : Short_Integer; pragma Import (Ada, E647, "sax__state_machines_E");
   E616 : Short_Integer; pragma Import (Ada, E616, "schema_E");
   E456 : Short_Integer; pragma Import (Ada, E456, "unicode__ccs_E");
   E483 : Short_Integer; pragma Import (Ada, E483, "unicode__ccs__iso_8859_1_E");
   E485 : Short_Integer; pragma Import (Ada, E485, "unicode__ccs__iso_8859_15_E");
   E490 : Short_Integer; pragma Import (Ada, E490, "unicode__ccs__iso_8859_2_E");
   E493 : Short_Integer; pragma Import (Ada, E493, "unicode__ccs__iso_8859_3_E");
   E495 : Short_Integer; pragma Import (Ada, E495, "unicode__ccs__iso_8859_4_E");
   E497 : Short_Integer; pragma Import (Ada, E497, "unicode__ccs__windows_1251_E");
   E502 : Short_Integer; pragma Import (Ada, E502, "unicode__ccs__windows_1252_E");
   E452 : Short_Integer; pragma Import (Ada, E452, "unicode__ces_E");
   E462 : Short_Integer; pragma Import (Ada, E462, "sax__symbols_E");
   E614 : Short_Integer; pragma Import (Ada, E614, "sax__locators_E");
   E612 : Short_Integer; pragma Import (Ada, E612, "sax__exceptions_E");
   E454 : Short_Integer; pragma Import (Ada, E454, "unicode__ces__utf32_E");
   E505 : Short_Integer; pragma Import (Ada, E505, "unicode__ces__basic_8bit_E");
   E507 : Short_Integer; pragma Import (Ada, E507, "unicode__ces__utf16_E");
   E458 : Short_Integer; pragma Import (Ada, E458, "unicode__ces__utf8_E");
   E610 : Short_Integer; pragma Import (Ada, E610, "sax__models_E");
   E608 : Short_Integer; pragma Import (Ada, E608, "sax__attributes_E");
   E469 : Short_Integer; pragma Import (Ada, E469, "sax__utils_E");
   E441 : Short_Integer; pragma Import (Ada, E441, "dom__core_E");
   E628 : Short_Integer; pragma Import (Ada, E628, "schema__date_time_E");
   E632 : Short_Integer; pragma Import (Ada, E632, "schema__decimal_E");
   E626 : Short_Integer; pragma Import (Ada, E626, "schema__simple_types_E");
   E481 : Short_Integer; pragma Import (Ada, E481, "unicode__encodings_E");
   E477 : Short_Integer; pragma Import (Ada, E477, "dom__core__nodes_E");
   E475 : Short_Integer; pragma Import (Ada, E475, "dom__core__attrs_E");
   E620 : Short_Integer; pragma Import (Ada, E620, "dom__core__character_datas_E");
   E471 : Short_Integer; pragma Import (Ada, E471, "dom__core__documents_E");
   E473 : Short_Integer; pragma Import (Ada, E473, "dom__core__elements_E");
   E600 : Short_Integer; pragma Import (Ada, E600, "input_sources_E");
   E602 : Short_Integer; pragma Import (Ada, E602, "input_sources__file_E");
   E606 : Short_Integer; pragma Import (Ada, E606, "input_sources__strings_E");
   E604 : Short_Integer; pragma Import (Ada, E604, "sax__readers_E");
   E645 : Short_Integer; pragma Import (Ada, E645, "schema__validators_E");
   E622 : Short_Integer; pragma Import (Ada, E622, "schema__readers_E");
   E624 : Short_Integer; pragma Import (Ada, E624, "schema__schema_readers_E");
   E649 : Short_Integer; pragma Import (Ada, E649, "schema__validators__xsd_grammar_E");
   E618 : Short_Integer; pragma Import (Ada, E618, "schema__dom_readers_E");
   E596 : Short_Integer; pragma Import (Ada, E596, "gpr__knowledge_E");
   E598 : Short_Integer; pragma Import (Ada, E598, "gpr__sdefault_E");
   E590 : Short_Integer; pragma Import (Ada, E590, "gpr__strt_E");
   E559 : Short_Integer; pragma Import (Ada, E559, "gpr__util_E");
   E553 : Short_Integer; pragma Import (Ada, E553, "gpr__env_E");
   E551 : Short_Integer; pragma Import (Ada, E551, "gpr__tree_E");
   E586 : Short_Integer; pragma Import (Ada, E586, "gpr__dect_E");
   E578 : Short_Integer; pragma Import (Ada, E578, "gpr__nmsc_E");
   E584 : Short_Integer; pragma Import (Ada, E584, "gpr__part_E");
   E592 : Short_Integer; pragma Import (Ada, E592, "gpr__proc_E");
   E576 : Short_Integer; pragma Import (Ada, E576, "gpr__conf_E");
   E651 : Short_Integer; pragma Import (Ada, E651, "gpr__version_E");
   E653 : Short_Integer; pragma Import (Ada, E653, "gpr_build_util_E");
   E666 : Short_Integer; pragma Import (Ada, E666, "gpr__pp_E");
   E435 : Short_Integer; pragma Import (Ada, E435, "gnatcoll__projects_E");
   E513 : Short_Integer; pragma Import (Ada, E513, "gnatcoll__projects__krunch_E");
   E515 : Short_Integer; pragma Import (Ada, E515, "gnatcoll__projects__normalize_E");
   E008 : Short_Integer; pragma Import (Ada, E008, "libadalang__common_E");
   E012 : Short_Integer; pragma Import (Ada, E012, "libadalang__config_pragmas_impl_E");
   E049 : Short_Integer; pragma Import (Ada, E049, "libadalang__lexer_E");
   E051 : Short_Integer; pragma Import (Ada, E051, "libadalang__lexer_implementation_E");
   E053 : Short_Integer; pragma Import (Ada, E053, "libadalang__lexer_state_machine_E");
   E055 : Short_Integer; pragma Import (Ada, E055, "libadalang__parsers_E");
   E037 : Short_Integer; pragma Import (Ada, E037, "libadalang__implementation_E");
   E014 : Short_Integer; pragma Import (Ada, E014, "libadalang__debug_E");
   E002 : Short_Integer; pragma Import (Ada, E002, "libadalang__analysis_E");
   E016 : Short_Integer; pragma Import (Ada, E016, "libadalang__doc_utils_E");
   E018 : Short_Integer; pragma Import (Ada, E018, "libadalang__env_hooks_E");
   E020 : Short_Integer; pragma Import (Ada, E020, "libadalang__expr_eval_E");
   E023 : Short_Integer; pragma Import (Ada, E023, "libadalang__generic_api_E");
   E027 : Short_Integer; pragma Import (Ada, E027, "libadalang__generic_introspection_E");
   E035 : Short_Integer; pragma Import (Ada, E035, "libadalang__implementation__extensions_E");
   E039 : Short_Integer; pragma Import (Ada, E039, "libadalang__internal_default_provider_E");
   E043 : Short_Integer; pragma Import (Ada, E043, "libadalang__introspection_implementation_E");
   E025 : Short_Integer; pragma Import (Ada, E025, "libadalang__generic_impl_E");
   E064 : Short_Integer; pragma Import (Ada, E064, "libadalang__project_provider_E");
   E066 : Short_Integer; pragma Import (Ada, E066, "libadalang__public_converters_E");
   E074 : Short_Integer; pragma Import (Ada, E074, "libadalang__unit_files_E");
   E004 : Short_Integer; pragma Import (Ada, E004, "libadalang__auto_provider_E");
   E006 : Short_Integer; pragma Import (Ada, E006, "libadalang__c_E");
   E010 : Short_Integer; pragma Import (Ada, E010, "libadalang__config_pragmas_E");
   E021 : Short_Integer; pragma Import (Ada, E021, "libadalang__generic_api__introspection_E");
   E033 : Short_Integer; pragma Import (Ada, E033, "libadalang__implementation__c_E");
   E041 : Short_Integer; pragma Import (Ada, E041, "libadalang__introspection_E");
   E047 : Short_Integer; pragma Import (Ada, E047, "libadalang__iterators_E");
   E045 : Short_Integer; pragma Import (Ada, E045, "libadalang__iterators__extensions_E");
   E061 : Short_Integer; pragma Import (Ada, E061, "libadalang__preprocessing_E");
   E057 : Short_Integer; pragma Import (Ada, E057, "libadalang__pp_impl_E");
   E029 : Short_Integer; pragma Import (Ada, E029, "libadalang__helpers_E");
   E031 : Short_Integer; pragma Import (Ada, E031, "libadalang__implementation__c__extensions_E");
   E070 : Short_Integer; pragma Import (Ada, E070, "libadalang__rewriting_implementation_E");
   E078 : Short_Integer; pragma Import (Ada, E078, "libadalang__unparsing_implementation_E");
   E068 : Short_Integer; pragma Import (Ada, E068, "libadalang__rewriting_E");
   E076 : Short_Integer; pragma Import (Ada, E076, "libadalang__unparsing_E");

   Sec_Default_Sized_Stacks : array (1 .. 1) of aliased System.Secondary_Stack.SS_Stack (System.Parameters.Runtime_Default_Sec_Stack_Size);

   Local_Priority_Specific_Dispatching : constant String := "";
   Local_Interrupt_States : constant String := "";

   Is_Elaborated : Boolean := False;

   procedure finalize_library is
   begin
      E070 := E070 - 1;
      declare
         procedure F1;
         pragma Import (Ada, F1, "libadalang__rewriting_implementation__finalize_spec");
      begin
         if E070 = 0 then
            F1;
         end if;
      end;
      declare
         procedure F2;
         pragma Import (Ada, F2, "libadalang__helpers__finalize_body");
      begin
         E029 := E029 - 1;
         if E029 = 0 then
            F2;
         end if;
      end;
      declare
         procedure F3;
         pragma Import (Ada, F3, "libadalang__helpers__finalize_spec");
      begin
         if E029 = 0 then
            F3;
         end if;
      end;
      declare
         procedure F4;
         pragma Import (Ada, F4, "libadalang__preprocessing__finalize_body");
      begin
         E061 := E061 - 1;
         if E061 = 0 then
            F4;
         end if;
      end;
      declare
         procedure F5;
         pragma Import (Ada, F5, "libadalang__pp_impl__finalize_body");
      begin
         E057 := E057 - 1;
         if E057 = 0 then
            F5;
         end if;
      end;
      declare
         procedure F6;
         pragma Import (Ada, F6, "libadalang__preprocessing__finalize_spec");
      begin
         if E061 = 0 then
            F6;
         end if;
      end;
      declare
         procedure F7;
         pragma Import (Ada, F7, "libadalang__iterators__finalize_body");
      begin
         E047 := E047 - 1;
         if E047 = 0 then
            F7;
         end if;
      end;
      E045 := E045 - 1;
      declare
         procedure F8;
         pragma Import (Ada, F8, "libadalang__iterators__extensions__finalize_spec");
      begin
         if E045 = 0 then
            F8;
         end if;
      end;
      declare
         procedure F9;
         pragma Import (Ada, F9, "libadalang__iterators__finalize_spec");
      begin
         if E047 = 0 then
            F9;
         end if;
      end;
      E041 := E041 - 1;
      declare
         procedure F10;
         pragma Import (Ada, F10, "libadalang__introspection__finalize_spec");
      begin
         if E041 = 0 then
            F10;
         end if;
      end;
      declare
         procedure F11;
         pragma Import (Ada, F11, "libadalang__implementation__c__finalize_body");
      begin
         E033 := E033 - 1;
         if E033 = 0 then
            F11;
         end if;
      end;
      E010 := E010 - 1;
      declare
         procedure F12;
         pragma Import (Ada, F12, "libadalang__config_pragmas__finalize_spec");
      begin
         if E010 = 0 then
            F12;
         end if;
      end;
      E004 := E004 - 1;
      declare
         procedure F13;
         pragma Import (Ada, F13, "libadalang__auto_provider__finalize_spec");
      begin
         if E004 = 0 then
            F13;
         end if;
      end;
      declare
         procedure F14;
         pragma Import (Ada, F14, "libadalang__project_provider__finalize_body");
      begin
         E064 := E064 - 1;
         if E064 = 0 then
            F14;
         end if;
      end;
      E027 := E027 - 1;
      E002 := E002 - 1;
      declare
         procedure F15;
         pragma Import (Ada, F15, "libadalang__public_converters__finalize_body");
      begin
         E066 := E066 - 1;
         if E066 = 0 then
            F15;
         end if;
      end;
      declare
         procedure F16;
         pragma Import (Ada, F16, "libadalang__public_converters__finalize_spec");
      begin
         if E066 = 0 then
            F16;
         end if;
      end;
      declare
         procedure F17;
         pragma Import (Ada, F17, "libadalang__project_provider__finalize_spec");
      begin
         if E064 = 0 then
            F17;
         end if;
      end;
      declare
         procedure F18;
         pragma Import (Ada, F18, "libadalang__parsers__finalize_body");
      begin
         E055 := E055 - 1;
         if E055 = 0 then
            F18;
         end if;
      end;
      declare
         procedure F19;
         pragma Import (Ada, F19, "libadalang__implementation__finalize_body");
      begin
         E037 := E037 - 1;
         if E037 = 0 then
            F19;
         end if;
      end;
      E043 := E043 - 1;
      declare
         procedure F20;
         pragma Import (Ada, F20, "libadalang__introspection_implementation__finalize_spec");
      begin
         if E043 = 0 then
            F20;
         end if;
      end;
      declare
         procedure F21;
         pragma Import (Ada, F21, "libadalang__generic_introspection__finalize_spec");
      begin
         if E027 = 0 then
            F21;
         end if;
      end;
      E016 := E016 - 1;
      declare
         procedure F22;
         pragma Import (Ada, F22, "libadalang__doc_utils__finalize_spec");
      begin
         if E016 = 0 then
            F22;
         end if;
      end;
      declare
         procedure F23;
         pragma Import (Ada, F23, "libadalang__analysis__finalize_spec");
      begin
         if E002 = 0 then
            F23;
         end if;
      end;
      E012 := E012 - 1;
      declare
         procedure F24;
         pragma Import (Ada, F24, "libadalang__implementation__finalize_spec");
      begin
         if E037 = 0 then
            F24;
         end if;
      end;
      declare
         procedure F25;
         pragma Import (Ada, F25, "libadalang__config_pragmas_impl__finalize_spec");
      begin
         if E012 = 0 then
            F25;
         end if;
      end;
      declare
         procedure F26;
         pragma Import (Ada, F26, "gnatcoll__projects__finalize_body");
      begin
         E435 := E435 - 1;
         if E435 = 0 then
            F26;
         end if;
      end;
      declare
         procedure F27;
         pragma Import (Ada, F27, "gnatcoll__projects__finalize_spec");
      begin
         if E435 = 0 then
            F27;
         end if;
      end;
      declare
         procedure F28;
         pragma Import (Ada, F28, "gpr__util__finalize_body");
      begin
         E559 := E559 - 1;
         if E559 = 0 then
            F28;
         end if;
      end;
      declare
         procedure F29;
         pragma Import (Ada, F29, "gpr_build_util__finalize_body");
      begin
         E653 := E653 - 1;
         if E653 = 0 then
            F29;
         end if;
      end;
      declare
         procedure F30;
         pragma Import (Ada, F30, "gpr_build_util__finalize_spec");
      begin
         if E653 = 0 then
            F30;
         end if;
      end;
      declare
         procedure F31;
         pragma Import (Ada, F31, "gpr__conf__finalize_body");
      begin
         E576 := E576 - 1;
         if E576 = 0 then
            F31;
         end if;
      end;
      declare
         procedure F32;
         pragma Import (Ada, F32, "gpr__proc__finalize_body");
      begin
         E592 := E592 - 1;
         if E592 = 0 then
            F32;
         end if;
      end;
      declare
         procedure F33;
         pragma Import (Ada, F33, "gpr__nmsc__finalize_body");
      begin
         E578 := E578 - 1;
         if E578 = 0 then
            F33;
         end if;
      end;
      declare
         procedure F34;
         pragma Import (Ada, F34, "gpr__knowledge__finalize_body");
      begin
         E596 := E596 - 1;
         if E596 = 0 then
            F34;
         end if;
      end;
      E553 := E553 - 1;
      declare
         procedure F35;
         pragma Import (Ada, F35, "gpr__env__finalize_spec");
      begin
         if E553 = 0 then
            F35;
         end if;
      end;
      declare
         procedure F36;
         pragma Import (Ada, F36, "gpr__util__finalize_spec");
      begin
         if E559 = 0 then
            F36;
         end if;
      end;
      declare
         procedure F37;
         pragma Import (Ada, F37, "gpr__knowledge__finalize_spec");
      begin
         if E596 = 0 then
            F37;
         end if;
      end;
      E618 := E618 - 1;
      declare
         procedure F38;
         pragma Import (Ada, F38, "schema__dom_readers__finalize_spec");
      begin
         if E618 = 0 then
            F38;
         end if;
      end;
      E645 := E645 - 1;
      E622 := E622 - 1;
      E624 := E624 - 1;
      declare
         procedure F39;
         pragma Import (Ada, F39, "schema__schema_readers__finalize_spec");
      begin
         if E624 = 0 then
            F39;
         end if;
      end;
      declare
         procedure F40;
         pragma Import (Ada, F40, "schema__readers__finalize_spec");
      begin
         if E622 = 0 then
            F40;
         end if;
      end;
      declare
         procedure F41;
         pragma Import (Ada, F41, "schema__validators__finalize_spec");
      begin
         if E645 = 0 then
            F41;
         end if;
      end;
      E604 := E604 - 1;
      declare
         procedure F42;
         pragma Import (Ada, F42, "sax__readers__finalize_spec");
      begin
         if E604 = 0 then
            F42;
         end if;
      end;
      E606 := E606 - 1;
      declare
         procedure F43;
         pragma Import (Ada, F43, "input_sources__strings__finalize_spec");
      begin
         if E606 = 0 then
            F43;
         end if;
      end;
      E602 := E602 - 1;
      declare
         procedure F44;
         pragma Import (Ada, F44, "input_sources__file__finalize_spec");
      begin
         if E602 = 0 then
            F44;
         end if;
      end;
      E600 := E600 - 1;
      declare
         procedure F45;
         pragma Import (Ada, F45, "input_sources__finalize_spec");
      begin
         if E600 = 0 then
            F45;
         end if;
      end;
      E441 := E441 - 1;
      declare
         procedure F46;
         pragma Import (Ada, F46, "dom__core__finalize_spec");
      begin
         if E441 = 0 then
            F46;
         end if;
      end;
      E469 := E469 - 1;
      declare
         procedure F47;
         pragma Import (Ada, F47, "sax__utils__finalize_spec");
      begin
         if E469 = 0 then
            F47;
         end if;
      end;
      E608 := E608 - 1;
      declare
         procedure F48;
         pragma Import (Ada, F48, "sax__attributes__finalize_spec");
      begin
         if E608 = 0 then
            F48;
         end if;
      end;
      E612 := E612 - 1;
      declare
         procedure F49;
         pragma Import (Ada, F49, "sax__exceptions__finalize_spec");
      begin
         if E612 = 0 then
            F49;
         end if;
      end;
      E462 := E462 - 1;
      declare
         procedure F50;
         pragma Import (Ada, F50, "sax__symbols__finalize_spec");
      begin
         if E462 = 0 then
            F50;
         end if;
      end;
      E467 := E467 - 1;
      declare
         procedure F51;
         pragma Import (Ada, F51, "sax__pointers__finalize_spec");
      begin
         if E467 = 0 then
            F51;
         end if;
      end;
      E059 := E059 - 1;
      declare
         procedure F52;
         pragma Import (Ada, F52, "libadalang__pp_lexer__finalize_spec");
      begin
         if E059 = 0 then
            F52;
         end if;
      end;
      E594 := E594 - 1;
      declare
         procedure F53;
         pragma Import (Ada, F53, "gpr__ext__finalize_spec");
      begin
         if E594 = 0 then
            F53;
         end if;
      end;
      E519 := E519 - 1;
      declare
         procedure F54;
         pragma Import (Ada, F54, "gpr__sinput__finalize_body");
      begin
         E536 := E536 - 1;
         if E536 = 0 then
            F54;
         end if;
      end;
      declare
         procedure F55;
         pragma Import (Ada, F55, "gpr__names__finalize_body");
      begin
         E529 := E529 - 1;
         if E529 = 0 then
            F55;
         end if;
      end;
      declare
         procedure F56;
         pragma Import (Ada, F56, "gpr__finalize_spec");
      begin
         if E519 = 0 then
            F56;
         end if;
      end;
      declare
         procedure F57;
         pragma Import (Ada, F57, "gnatcoll__traces__finalize_body");
      begin
         E243 := E243 - 1;
         if E243 = 0 then
            F57;
         end if;
      end;
      declare
         procedure F58;
         pragma Import (Ada, F58, "gnatcoll__traces__finalize_spec");
      begin
         if E243 = 0 then
            F58;
         end if;
      end;
      declare
         procedure F59;
         pragma Import (Ada, F59, "gnatcoll__opt_parse__finalize_body");
      begin
         E731 := E731 - 1;
         if E731 = 0 then
            F59;
         end if;
      end;
      declare
         procedure F60;
         pragma Import (Ada, F60, "gnatcoll__opt_parse__finalize_spec");
      begin
         if E731 = 0 then
            F60;
         end if;
      end;
      E687 := E687 - 1;
      declare
         procedure F61;
         pragma Import (Ada, F61, "gnatcoll__file_paths__finalize_spec");
      begin
         if E687 = 0 then
            F61;
         end if;
      end;
      E341 := E341 - 1;
      declare
         procedure F62;
         pragma Import (Ada, F62, "gnatcoll__vfs__finalize_spec");
      begin
         if E341 = 0 then
            F62;
         end if;
      end;
      E352 := E352 - 1;
      declare
         procedure F63;
         pragma Import (Ada, F63, "gnatcoll__io__remote__finalize_spec");
      begin
         if E352 = 0 then
            F63;
         end if;
      end;
      declare
         procedure F64;
         pragma Import (Ada, F64, "gnatcoll__remote__finalize_spec");
      begin
         E372 := E372 - 1;
         if E372 = 0 then
            F64;
         end if;
      end;
      E356 := E356 - 1;
      declare
         procedure F65;
         pragma Import (Ada, F65, "gnatcoll__io__native__finalize_spec");
      begin
         if E356 = 0 then
            F65;
         end if;
      end;
      E349 := E349 - 1;
      declare
         procedure F66;
         pragma Import (Ada, F66, "gnatcoll__io__finalize_spec");
      begin
         if E349 = 0 then
            F66;
         end if;
      end;
      E317 := E317 - 1;
      declare
         procedure F67;
         pragma Import (Ada, F67, "gnatcoll__terminal__finalize_spec");
      begin
         if E317 = 0 then
            F67;
         end if;
      end;
      E308 := E308 - 1;
      declare
         procedure F68;
         pragma Import (Ada, F68, "gnatcoll__strings__finalize_spec");
      begin
         if E308 = 0 then
            F68;
         end if;
      end;
      E228 := E228 - 1;
      declare
         procedure F69;
         pragma Import (Ada, F69, "gnatcoll__refcount__finalize_spec");
      begin
         if E228 = 0 then
            F69;
         end if;
      end;
      declare
         procedure F70;
         pragma Import (Ada, F70, "gnatcoll__memory__finalize_body");
      begin
         E287 := E287 - 1;
         if E287 = 0 then
            F70;
         end if;
      end;
      E733 := E733 - 1;
      declare
         procedure F71;
         pragma Import (Ada, F71, "gnatcoll__locks__finalize_spec");
      begin
         if E733 = 0 then
            F71;
         end if;
      end;
      E673 := E673 - 1;
      declare
         procedure F72;
         pragma Import (Ada, F72, "gnatcoll__gmp__rational_numbers__finalize_spec");
      begin
         if E673 = 0 then
            F72;
         end if;
      end;
      E431 := E431 - 1;
      declare
         procedure F73;
         pragma Import (Ada, F73, "gnatcoll__gmp__integers__finalize_spec");
      begin
         if E431 = 0 then
            F73;
         end if;
      end;
      E713 := E713 - 1;
      declare
         procedure F74;
         pragma Import (Ada, F74, "system__tasking__protected_objects__entries__finalize_spec");
      begin
         if E713 = 0 then
            F74;
         end if;
      end;
      declare
         procedure F75;
         pragma Import (Ada, F75, "gnat__debug_pools__finalize_body");
      begin
         E289 := E289 - 1;
         if E289 = 0 then
            F75;
         end if;
      end;
      declare
         procedure F76;
         pragma Import (Ada, F76, "gnat__debug_pools__finalize_spec");
      begin
         if E289 = 0 then
            F76;
         end if;
      end;
      declare
         procedure F77;
         pragma Import (Ada, F77, "ada__directories__finalize_body");
      begin
         E358 := E358 - 1;
         if E358 = 0 then
            F77;
         end if;
      end;
      declare
         procedure F78;
         pragma Import (Ada, F78, "ada__directories__finalize_spec");
      begin
         if E358 = 0 then
            F78;
         end if;
      end;
      E367 := E367 - 1;
      declare
         procedure F79;
         pragma Import (Ada, F79, "system__regexp__finalize_spec");
      begin
         if E367 = 0 then
            F79;
         end if;
      end;
      declare
         procedure F80;
         pragma Import (Ada, F80, "gnat__sockets__finalize_body");
      begin
         E565 := E565 - 1;
         if E565 = 0 then
            F80;
         end if;
      end;
      declare
         procedure F81;
         pragma Import (Ada, F81, "gnat__sockets__finalize_spec");
      begin
         if E565 = 0 then
            F81;
         end if;
      end;
      E509 := E509 - 1;
      declare
         procedure F82;
         pragma Import (Ada, F82, "gnat__expect__tty__finalize_spec");
      begin
         if E509 = 0 then
            F82;
         end if;
      end;
      E328 := E328 - 1;
      declare
         procedure F83;
         pragma Import (Ada, F83, "gnat__expect__finalize_spec");
      begin
         if E328 = 0 then
            F83;
         end if;
      end;
      E241 := E241 - 1;
      declare
         procedure F84;
         pragma Import (Ada, F84, "system__pool_global__finalize_spec");
      begin
         if E241 = 0 then
            F84;
         end if;
      end;
      E555 := E555 - 1;
      declare
         procedure F85;
         pragma Import (Ada, F85, "gnat__string_split__finalize_spec");
      begin
         if E555 = 0 then
            F85;
         end if;
      end;
      E655 := E655 - 1;
      declare
         procedure F86;
         pragma Import (Ada, F86, "gnat__md5__finalize_spec");
      begin
         if E655 = 0 then
            F86;
         end if;
      end;
      E268 := E268 - 1;
      declare
         procedure F87;
         pragma Import (Ada, F87, "ada__text_io__finalize_spec");
      begin
         if E268 = 0 then
            F87;
         end if;
      end;
      E399 := E399 - 1;
      declare
         procedure F88;
         pragma Import (Ada, F88, "ada__strings__wide_wide_unbounded__finalize_spec");
      begin
         if E399 = 0 then
            F88;
         end if;
      end;
      E403 := E403 - 1;
      declare
         procedure F89;
         pragma Import (Ada, F89, "ada__strings__wide_wide_maps__finalize_spec");
      begin
         if E403 = 0 then
            F89;
         end if;
      end;
      E198 := E198 - 1;
      declare
         procedure F90;
         pragma Import (Ada, F90, "ada__strings__unbounded__finalize_spec");
      begin
         if E198 = 0 then
            F90;
         end if;
      end;
      E331 := E331 - 1;
      declare
         procedure F91;
         pragma Import (Ada, F91, "system__storage_pools__subpools__finalize_spec");
      begin
         if E331 = 0 then
            F91;
         end if;
      end;
      E239 := E239 - 1;
      declare
         procedure F92;
         pragma Import (Ada, F92, "system__finalization_masters__finalize_spec");
      begin
         if E239 = 0 then
            F92;
         end if;
      end;
      E561 := E561 - 1;
      declare
         procedure F93;
         pragma Import (Ada, F93, "ada__streams__stream_io__finalize_spec");
      begin
         if E561 = 0 then
            F93;
         end if;
      end;
      declare
         procedure F94;
         pragma Import (Ada, F94, "system__file_io__finalize_body");
      begin
         E272 := E272 - 1;
         if E272 = 0 then
            F94;
         end if;
      end;
      declare
         procedure Reraise_Library_Exception_If_Any;
            pragma Import (Ada, Reraise_Library_Exception_If_Any, "__gnat_reraise_library_exception_if_any");
      begin
         Reraise_Library_Exception_If_Any;
      end;
   end finalize_library;

   procedure adalangfinal is

      procedure Runtime_Finalize;
      pragma Import (C, Runtime_Finalize, "__gnat_runtime_finalize");

   begin
      if not Is_Elaborated then
         return;
      end if;
      Is_Elaborated := False;
      Runtime_Finalize;
      finalize_library;
   end adalangfinal;

   type No_Param_Proc is access procedure;
   pragma Favor_Top_Level (No_Param_Proc);

   procedure adalanginit is
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
          (True, True, False, False, True, True, False, False, 
           True, False, False, True, True, True, True, False, 
           False, False, False, True, False, False, True, True, 
           False, True, True, False, True, True, True, True, 
           False, True, False, False, False, True, True, False, 
           True, True, False, True, True, True, True, False, 
           True, False, True, True, False, False, True, False, 
           True, False, False, False, False, False, False, False, 
           False, True, False, True, True, True, True, False, 
           True, False, True, True, True, False, True, True, 
           False, True, True, True, True, False, False, False, 
           True, False, False, False, False, True, False, True, 
           False, False, True, False),
         Count => (0, 0, 0, 2, 0, 2, 0, 0, 2, 0),
         Unknown => (False, False, False, False, False, False, False, False, True, False));
      Priority_Specific_Dispatching :=
        Local_Priority_Specific_Dispatching'Address;
      Num_Specific_Dispatching := 0;
      Main_CPU := -1;
      Interrupt_States := Local_Interrupt_States'Address;
      Num_Interrupt_States := 0;
      Unreserve_All_Interrupts := 0;
      Detect_Blocking := 0;
      Default_Stack_Size := -1;

      adalangmain'Elab_Body;
      Default_Secondary_Stack_Size := System.Parameters.Runtime_Default_Sec_Stack_Size;
      Binder_Sec_Stacks_Count := 1;
      Default_Sized_SS_Pool := Sec_Default_Sized_Stacks'Address;

      Runtime_Initialize (1);

      if E092 = 0 then
         Ada.Exceptions'Elab_Spec;
      end if;
      if E088 = 0 then
         System.Soft_Links'Elab_Spec;
      end if;
      if E086 = 0 then
         System.Exception_Table'Elab_Body;
      end if;
      E086 := E086 + 1;
      if E083 = 0 then
         Ada.Containers'Elab_Spec;
      end if;
      E083 := E083 + 1;
      if E150 = 0 then
         Ada.Io_Exceptions'Elab_Spec;
      end if;
      E150 := E150 + 1;
      if E101 = 0 then
         Ada.Numerics'Elab_Spec;
      end if;
      E101 := E101 + 1;
      if E137 = 0 then
         Ada.Strings'Elab_Spec;
      end if;
      E137 := E137 + 1;
      if E139 = 0 then
         Ada.Strings.Maps'Elab_Spec;
      end if;
      E139 := E139 + 1;
      if E142 = 0 then
         Ada.Strings.Maps.Constants'Elab_Spec;
      end if;
      E142 := E142 + 1;
      if E125 = 0 then
         Interfaces.C'Elab_Spec;
      end if;
      E125 := E125 + 1;
      if E095 = 0 then
         System.Exceptions'Elab_Spec;
      end if;
      E095 := E095 + 1;
      if E164 = 0 then
         System.Object_Reader'Elab_Spec;
      end if;
      E164 := E164 + 1;
      if E132 = 0 then
         System.Dwarf_Lines'Elab_Spec;
      end if;
      if E155 = 0 then
         System.Os_Lib'Elab_Body;
      end if;
      E155 := E155 + 1;
      if E178 = 0 then
         System.Soft_Links.Initialize'Elab_Body;
      end if;
      E178 := E178 + 1;
      E088 := E088 + 1;
      if E120 = 0 then
         System.Traceback.Symbolic'Elab_Body;
      end if;
      E120 := E120 + 1;
      if E100 = 0 then
         System.Img_Int'Elab_Spec;
      end if;
      E100 := E100 + 1;
      E092 := E092 + 1;
      if E145 = 0 then
         System.Img_Uns'Elab_Spec;
      end if;
      E145 := E145 + 1;
      E132 := E132 + 1;
      if E337 = 0 then
         Ada.Assertions'Elab_Spec;
      end if;
      E337 := E337 + 1;
      if E184 = 0 then
         Ada.Strings.Utf_Encoding'Elab_Spec;
      end if;
      E184 := E184 + 1;
      if E192 = 0 then
         Ada.Tags'Elab_Spec;
      end if;
      if E192 = 0 then
         Ada.Tags'Elab_Body;
      end if;
      E192 := E192 + 1;
      if E182 = 0 then
         Ada.Strings.Text_Buffers'Elab_Spec;
      end if;
      E182 := E182 + 1;
      if E261 = 0 then
         Gnat'Elab_Spec;
      end if;
      E261 := E261 + 1;
      if E258 = 0 then
         Interfaces.C.Strings'Elab_Spec;
      end if;
      E258 := E258 + 1;
      if E737 = 0 then
         System.Aux_Dec'Elab_Spec;
      end if;
      E737 := E737 + 1;
      if E212 = 0 then
         Ada.Streams'Elab_Spec;
      end if;
      E212 := E212 + 1;
      if E273 = 0 then
         System.File_Control_Block'Elab_Spec;
      end if;
      E273 := E273 + 1;
      if E214 = 0 then
         System.Finalization_Root'Elab_Spec;
      end if;
      E214 := E214 + 1;
      if E210 = 0 then
         Ada.Finalization'Elab_Spec;
      end if;
      E210 := E210 + 1;
      if E272 = 0 then
         System.File_Io'Elab_Body;
      end if;
      E272 := E272 + 1;
      if E561 = 0 then
         Ada.Streams.Stream_Io'Elab_Spec;
      end if;
      E561 := E561 + 1;
      if E281 = 0 then
         System.Regpat'Elab_Spec;
      end if;
      E281 := E281 + 1;
      if E237 = 0 then
         System.Storage_Pools'Elab_Spec;
      end if;
      E237 := E237 + 1;
      if E239 = 0 then
         System.Finalization_Masters'Elab_Spec;
      end if;
      if E239 = 0 then
         System.Finalization_Masters'Elab_Body;
      end if;
      E239 := E239 + 1;
      if E331 = 0 then
         System.Storage_Pools.Subpools'Elab_Spec;
      end if;
      E331 := E331 + 1;
      if E198 = 0 then
         Ada.Strings.Unbounded'Elab_Spec;
      end if;
      E198 := E198 + 1;
      if E403 = 0 then
         Ada.Strings.Wide_Wide_Maps'Elab_Spec;
      end if;
      E403 := E403 + 1;
      if E399 = 0 then
         Ada.Strings.Wide_Wide_Unbounded'Elab_Spec;
      end if;
      E399 := E399 + 1;
      if E701 = 0 then
         System.Task_Info'Elab_Spec;
      end if;
      E701 := E701 + 1;
      if E245 = 0 then
         Ada.Calendar'Elab_Spec;
      end if;
      if E245 = 0 then
         Ada.Calendar'Elab_Body;
      end if;
      E245 := E245 + 1;
      if E390 = 0 then
         Ada.Calendar.Delays'Elab_Body;
      end if;
      E390 := E390 + 1;
      if E251 = 0 then
         Ada.Calendar.Time_Zones'Elab_Spec;
      end if;
      E251 := E251 + 1;
      if E268 = 0 then
         Ada.Text_Io'Elab_Spec;
      end if;
      if E268 = 0 then
         Ada.Text_Io'Elab_Body;
      end if;
      E268 := E268 + 1;
      if E479 = 0 then
         Ada.Text_Io.Text_Streams'Elab_Spec;
      end if;
      E479 := E479 + 1;
      E538 := E538 + 1;
      if E263 = 0 then
         Gnat.Calendar'Elab_Spec;
      end if;
      E263 := E263 + 1;
      if E354 = 0 then
         Gnat.Directory_Operations'Elab_Spec;
      end if;
      if E354 = 0 then
         Gnat.Directory_Operations'Elab_Body;
      end if;
      E354 := E354 + 1;
      E548 := E548 + 1;
      E657 := E657 + 1;
      E659 := E659 + 1;
      if E655 = 0 then
         Gnat.Md5'Elab_Spec;
      end if;
      E655 := E655 + 1;
      if E555 = 0 then
         Gnat.String_Split'Elab_Spec;
      end if;
      E555 := E555 + 1;
      E511 := E511 + 1;
      if E302 = 0 then
         System.Checked_Pools'Elab_Spec;
      end if;
      E302 := E302 + 1;
      if E241 = 0 then
         System.Pool_Global'Elab_Spec;
      end if;
      if E241 = 0 then
         System.Pool_Global'Elab_Body;
      end if;
      E241 := E241 + 1;
      if E328 = 0 then
         Gnat.Expect'Elab_Spec;
      end if;
      if E328 = 0 then
         Gnat.Expect'Elab_Body;
      end if;
      E328 := E328 + 1;
      if E509 = 0 then
         Gnat.Expect.Tty'Elab_Spec;
      end if;
      if E509 = 0 then
         Gnat.Expect.Tty'Elab_Body;
      end if;
      E509 := E509 + 1;
      if E565 = 0 then
         Gnat.Sockets'Elab_Spec;
      end if;
      if E572 = 0 then
         Gnat.Sockets.Thin_Common'Elab_Spec;
      end if;
      E572 := E572 + 1;
      E570 := E570 + 1;
      if E565 = 0 then
         Gnat.Sockets'Elab_Body;
      end if;
      E565 := E565 + 1;
      E568 := E568 + 1;
      if E380 = 0 then
         System.Random_Seed'Elab_Body;
      end if;
      E380 := E380 + 1;
      if E367 = 0 then
         System.Regexp'Elab_Spec;
      end if;
      if E367 = 0 then
         System.Regexp'Elab_Body;
      end if;
      E367 := E367 + 1;
      if E358 = 0 then
         Ada.Directories'Elab_Spec;
      end if;
      if E358 = 0 then
         Ada.Directories'Elab_Body;
      end if;
      E358 := E358 + 1;
      if E640 = 0 then
         System.Img_Llli'Elab_Spec;
      end if;
      E640 := E640 + 1;
      if E275 = 0 then
         System.Img_Lli'Elab_Spec;
      end if;
      E275 := E275 + 1;
      if E697 = 0 then
         System.Task_Primitives.Operations'Elab_Body;
      end if;
      E697 := E697 + 1;
      if E277 = 0 then
         System.Img_Llu'Elab_Spec;
      end if;
      E277 := E277 + 1;
      if E266 = 0 then
         Gnat.Calendar.Time_Io'Elab_Spec;
      end if;
      E266 := E266 + 1;
      if E289 = 0 then
         Gnat.Debug_Pools'Elab_Spec;
      end if;
      if E289 = 0 then
         Gnat.Debug_Pools'Elab_Body;
      end if;
      E289 := E289 + 1;
      if E717 = 0 then
         System.Tasking.Initialization'Elab_Body;
      end if;
      E717 := E717 + 1;
      if E707 = 0 then
         System.Tasking.Protected_Objects'Elab_Body;
      end if;
      E707 := E707 + 1;
      if E713 = 0 then
         System.Tasking.Protected_Objects.Entries'Elab_Spec;
      end if;
      E713 := E713 + 1;
      if E725 = 0 then
         System.Tasking.Queuing'Elab_Body;
      end if;
      E725 := E725 + 1;
      E735 := E735 + 1;
      if E431 = 0 then
         GNATCOLL.GMP.INTEGERS'ELAB_SPEC;
      end if;
      E431 := E431 + 1;
      if E445 = 0 then
         Unicode'Elab_Body;
      end if;
      E445 := E445 + 1;
      E230 := E230 + 1;
      if E673 = 0 then
         GNATCOLL.GMP.RATIONAL_NUMBERS'ELAB_SPEC;
      end if;
      if E673 = 0 then
         GNATCOLL.GMP.RATIONAL_NUMBERS'ELAB_BODY;
      end if;
      E673 := E673 + 1;
      if E733 = 0 then
         GNATCOLL.LOCKS'ELAB_SPEC;
      end if;
      E733 := E733 + 1;
      if E287 = 0 then
         GNATCOLL.MEMORY'ELAB_BODY;
      end if;
      E287 := E287 + 1;
      if E324 = 0 then
         GNATCOLL.OS'ELAB_SPEC;
      end if;
      E324 := E324 + 1;
      E232 := E232 + 1;
      if E228 = 0 then
         GNATCOLL.REFCOUNT'ELAB_SPEC;
      end if;
      E228 := E228 + 1;
      E323 := E323 + 1;
      E310 := E310 + 1;
      if E308 = 0 then
         GNATCOLL.STRINGS'ELAB_SPEC;
      end if;
      if E308 = 0 then
         GNATCOLL.STRINGS'ELAB_BODY;
      end if;
      E308 := E308 + 1;
      if E304 = 0 then
         GNATCOLL.MMAP'ELAB_SPEC;
      end if;
      E306 := E306 + 1;
      E304 := E304 + 1;
      if E315 = 0 then
         GNATCOLL.TEMPLATES'ELAB_SPEC;
      end if;
      E315 := E315 + 1;
      if E317 = 0 then
         GNATCOLL.TERMINAL'ELAB_SPEC;
      end if;
      if E317 = 0 then
         GNATCOLL.TERMINAL'ELAB_BODY;
      end if;
      E317 := E317 + 1;
      E319 := E319 + 1;
      if E349 = 0 then
         GNATCOLL.IO'ELAB_SPEC;
      end if;
      if E349 = 0 then
         GNATCOLL.IO'ELAB_BODY;
      end if;
      E349 := E349 + 1;
      if E369 = 0 then
         GNATCOLL.PATH'ELAB_SPEC;
      end if;
      if E369 = 0 then
         GNATCOLL.PATH'ELAB_BODY;
      end if;
      E369 := E369 + 1;
      if E356 = 0 then
         GNATCOLL.IO.NATIVE'ELAB_SPEC;
      end if;
      if E356 = 0 then
         GNATCOLL.IO.NATIVE'ELAB_BODY;
      end if;
      E356 := E356 + 1;
      if E372 = 0 then
         GNATCOLL.REMOTE'ELAB_SPEC;
      end if;
      E372 := E372 + 1;
      if E376 = 0 then
         GNATCOLL.REMOTE.DB'ELAB_SPEC;
      end if;
      E376 := E376 + 1;
      if E352 = 0 then
         GNATCOLL.IO.REMOTE'ELAB_SPEC;
      end if;
      E371 := E371 + 1;
      E374 := E374 + 1;
      if E352 = 0 then
         GNATCOLL.IO.REMOTE'ELAB_BODY;
      end if;
      E352 := E352 + 1;
      if E341 = 0 then
         GNATCOLL.VFS'ELAB_SPEC;
      end if;
      if E341 = 0 then
         GNATCOLL.VFS'ELAB_BODY;
      end if;
      E341 := E341 + 1;
      if E687 = 0 then
         GNATCOLL.FILE_PATHS'ELAB_SPEC;
      end if;
      E687 := E687 + 1;
      if E731 = 0 then
         GNATCOLL.OPT_PARSE'ELAB_SPEC;
      end if;
      if E731 = 0 then
         GNATCOLL.OPT_PARSE'ELAB_BODY;
      end if;
      E731 := E731 + 1;
      if E243 = 0 then
         GNATCOLL.TRACES'ELAB_SPEC;
      end if;
      if E243 = 0 then
         GNATCOLL.TRACES'ELAB_BODY;
      end if;
      E243 := E243 + 1;
      if E418 = 0 then
         GNATCOLL.ICONV'ELAB_SPEC;
      end if;
      if E418 = 0 then
         GNATCOLL.ICONV'ELAB_BODY;
      end if;
      E418 := E418 + 1;
      if E517 = 0 then
         GNATCOLL.VFS_UTILS'ELAB_SPEC;
      end if;
      E517 := E517 + 1;
      if E519 = 0 then
         GPR'ELAB_SPEC;
      end if;
      if E521 = 0 then
         GPR.ATTR'ELAB_SPEC;
      end if;
      if E525 = 0 then
         GPR.CSET'ELAB_BODY;
      end if;
      E525 := E525 + 1;
      E531 := E531 + 1;
      if E540 = 0 then
         GPR.OSINT'ELAB_SPEC;
      end if;
      if E527 = 0 then
         GPR.ERROUTC'ELAB_SPEC;
      end if;
      if E533 = 0 then
         GPR.OUTPUT'ELAB_BODY;
      end if;
      E533 := E533 + 1;
      if E529 = 0 then
         GPR.NAMES'ELAB_BODY;
      end if;
      E529 := E529 + 1;
      if E540 = 0 then
         GPR.OSINT'ELAB_BODY;
      end if;
      E540 := E540 + 1;
      E542 := E542 + 1;
      if E536 = 0 then
         GPR.SINPUT'ELAB_SPEC;
      end if;
      if E536 = 0 then
         GPR.SINPUT'ELAB_BODY;
      end if;
      E536 := E536 + 1;
      E527 := E527 + 1;
      E544 := E544 + 1;
      if E519 = 0 then
         GPR'ELAB_BODY;
      end if;
      E519 := E519 + 1;
      if E521 = 0 then
         GPR.ATTR'ELAB_BODY;
      end if;
      E521 := E521 + 1;
      E523 := E523 + 1;
      if E574 = 0 then
         GPR.ALI'ELAB_SPEC;
      end if;
      E574 := E574 + 1;
      E588 := E588 + 1;
      if E594 = 0 then
         GPR.EXT'ELAB_SPEC;
      end if;
      E594 := E594 + 1;
      if E557 = 0 then
         GPR.TEMPDIR'ELAB_BODY;
      end if;
      E557 := E557 + 1;
      if E059 = 0 then
         Libadalang.Pp_Lexer'Elab_Spec;
      end if;
      if E059 = 0 then
         Libadalang.Pp_Lexer'Elab_Body;
      end if;
      E059 := E059 + 1;
      E072 := E072 + 1;
      E460 := E460 + 1;
      if E467 = 0 then
         Sax.Pointers'Elab_Spec;
      end if;
      if E467 = 0 then
         Sax.Pointers'Elab_Body;
      end if;
      E467 := E467 + 1;
      E647 := E647 + 1;
      if E616 = 0 then
         Schema'Elab_Spec;
      end if;
      E616 := E616 + 1;
      if E456 = 0 then
         Unicode.Ccs'Elab_Spec;
      end if;
      E456 := E456 + 1;
      E483 := E483 + 1;
      E485 := E485 + 1;
      E490 := E490 + 1;
      E493 := E493 + 1;
      E495 := E495 + 1;
      E497 := E497 + 1;
      E502 := E502 + 1;
      if E452 = 0 then
         Unicode.Ces'Elab_Spec;
      end if;
      E452 := E452 + 1;
      if E462 = 0 then
         Sax.Symbols'Elab_Spec;
      end if;
      if E462 = 0 then
         Sax.Symbols'Elab_Body;
      end if;
      E462 := E462 + 1;
      E614 := E614 + 1;
      if E612 = 0 then
         Sax.Exceptions'Elab_Spec;
      end if;
      if E612 = 0 then
         Sax.Exceptions'Elab_Body;
      end if;
      E612 := E612 + 1;
      E454 := E454 + 1;
      E505 := E505 + 1;
      E507 := E507 + 1;
      E458 := E458 + 1;
      if E610 = 0 then
         Sax.Models'Elab_Spec;
      end if;
      E610 := E610 + 1;
      if E608 = 0 then
         Sax.Attributes'Elab_Spec;
      end if;
      if E608 = 0 then
         Sax.Attributes'Elab_Body;
      end if;
      E608 := E608 + 1;
      if E469 = 0 then
         Sax.Utils'Elab_Spec;
      end if;
      if E469 = 0 then
         Sax.Utils'Elab_Body;
      end if;
      E469 := E469 + 1;
      if E441 = 0 then
         DOM.CORE'ELAB_SPEC;
      end if;
      E441 := E441 + 1;
      if E628 = 0 then
         Schema.Date_Time'Elab_Spec;
      end if;
      E628 := E628 + 1;
      E632 := E632 + 1;
      if E626 = 0 then
         Schema.Simple_Types'Elab_Spec;
      end if;
      E626 := E626 + 1;
      E481 := E481 + 1;
      E477 := E477 + 1;
      E475 := E475 + 1;
      E620 := E620 + 1;
      E473 := E473 + 1;
      E471 := E471 + 1;
      if E600 = 0 then
         Input_Sources'Elab_Spec;
      end if;
      if E600 = 0 then
         Input_Sources'Elab_Body;
      end if;
      E600 := E600 + 1;
      if E602 = 0 then
         Input_Sources.File'Elab_Spec;
      end if;
      if E602 = 0 then
         Input_Sources.File'Elab_Body;
      end if;
      E602 := E602 + 1;
      if E606 = 0 then
         Input_Sources.Strings'Elab_Spec;
      end if;
      if E606 = 0 then
         Input_Sources.Strings'Elab_Body;
      end if;
      E606 := E606 + 1;
      if E604 = 0 then
         Sax.Readers'Elab_Spec;
      end if;
      if E604 = 0 then
         Sax.Readers'Elab_Body;
      end if;
      E604 := E604 + 1;
      if E645 = 0 then
         Schema.Validators'Elab_Spec;
      end if;
      if E622 = 0 then
         Schema.Readers'Elab_Spec;
      end if;
      if E624 = 0 then
         Schema.Schema_Readers'Elab_Spec;
      end if;
      if E624 = 0 then
         Schema.Schema_Readers'Elab_Body;
      end if;
      E624 := E624 + 1;
      if E622 = 0 then
         Schema.Readers'Elab_Body;
      end if;
      E622 := E622 + 1;
      E649 := E649 + 1;
      if E645 = 0 then
         Schema.Validators'Elab_Body;
      end if;
      E645 := E645 + 1;
      if E618 = 0 then
         Schema.Dom_Readers'Elab_Spec;
      end if;
      if E618 = 0 then
         Schema.Dom_Readers'Elab_Body;
      end if;
      E618 := E618 + 1;
      if E596 = 0 then
         GPR.KNOWLEDGE'ELAB_SPEC;
      end if;
      if E559 = 0 then
         GPR.UTIL'ELAB_SPEC;
      end if;
      if E553 = 0 then
         GPR.ENV'ELAB_SPEC;
      end if;
      if E553 = 0 then
         GPR.ENV'ELAB_BODY;
      end if;
      E553 := E553 + 1;
      if E596 = 0 then
         GPR.KNOWLEDGE'ELAB_BODY;
      end if;
      E596 := E596 + 1;
      if E598 = 0 then
         GPR.SDEFAULT'ELAB_BODY;
      end if;
      E598 := E598 + 1;
      if E551 = 0 then
         GPR.TREE'ELAB_SPEC;
      end if;
      if E551 = 0 then
         GPR.TREE'ELAB_BODY;
      end if;
      E551 := E551 + 1;
      E586 := E586 + 1;
      if E578 = 0 then
         GPR.NMSC'ELAB_BODY;
      end if;
      E578 := E578 + 1;
      if E584 = 0 then
         GPR.PART'ELAB_BODY;
      end if;
      E584 := E584 + 1;
      if E592 = 0 then
         GPR.PROC'ELAB_BODY;
      end if;
      E592 := E592 + 1;
      if E576 = 0 then
         GPR.CONF'ELAB_SPEC;
      end if;
      if E576 = 0 then
         GPR.CONF'ELAB_BODY;
      end if;
      E576 := E576 + 1;
      if E590 = 0 then
         GPR.STRT'ELAB_BODY;
      end if;
      E590 := E590 + 1;
      E651 := E651 + 1;
      if E653 = 0 then
         Gpr_Build_Util'Elab_Spec;
      end if;
      if E653 = 0 then
         Gpr_Build_Util'Elab_Body;
      end if;
      E653 := E653 + 1;
      if E559 = 0 then
         GPR.UTIL'ELAB_BODY;
      end if;
      E559 := E559 + 1;
      E666 := E666 + 1;
      if E435 = 0 then
         GNATCOLL.PROJECTS'ELAB_SPEC;
      end if;
      E513 := E513 + 1;
      if E515 = 0 then
         GNATCOLL.PROJECTS.NORMALIZE'ELAB_SPEC;
      end if;
      if E515 = 0 then
         GNATCOLL.PROJECTS.NORMALIZE'ELAB_BODY;
      end if;
      E515 := E515 + 1;
      if E435 = 0 then
         GNATCOLL.PROJECTS'ELAB_BODY;
      end if;
      E435 := E435 + 1;
      if E012 = 0 then
         Libadalang.Config_Pragmas_Impl'Elab_Spec;
      end if;
      E053 := E053 + 1;
      if E055 = 0 then
         Libadalang.Parsers'Elab_Spec;
      end if;
      if E037 = 0 then
         Libadalang.Implementation'Elab_Spec;
      end if;
      E012 := E012 + 1;
      E014 := E014 + 1;
      if E002 = 0 then
         Libadalang.Analysis'Elab_Spec;
      end if;
      if E016 = 0 then
         Libadalang.Doc_Utils'Elab_Spec;
      end if;
      E016 := E016 + 1;
      E020 := E020 + 1;
      if E027 = 0 then
         Libadalang.Generic_Introspection'Elab_Spec;
      end if;
      if E043 = 0 then
         Libadalang.Introspection_Implementation'Elab_Spec;
      end if;
      if E043 = 0 then
         Libadalang.Introspection_Implementation'Elab_Body;
      end if;
      E043 := E043 + 1;
      E049 := E049 + 1;
      E051 := E051 + 1;
      if E008 = 0 then
         Libadalang.Common'Elab_Body;
      end if;
      E008 := E008 + 1;
      if E025 = 0 then
         Libadalang.Generic_Impl'Elab_Spec;
      end if;
      if E037 = 0 then
         Libadalang.Implementation'Elab_Body;
      end if;
      E037 := E037 + 1;
      if E055 = 0 then
         Libadalang.Parsers'Elab_Body;
      end if;
      E055 := E055 + 1;
      if E064 = 0 then
         Libadalang.Project_Provider'Elab_Spec;
      end if;
      if E066 = 0 then
         Libadalang.Public_Converters'Elab_Spec;
      end if;
      if E066 = 0 then
         Libadalang.Public_Converters'Elab_Body;
      end if;
      E066 := E066 + 1;
      if E002 = 0 then
         Libadalang.Analysis'Elab_Body;
      end if;
      E002 := E002 + 1;
      E018 := E018 + 1;
      E023 := E023 + 1;
      E025 := E025 + 1;
      if E027 = 0 then
         Libadalang.Generic_Introspection'Elab_Body;
      end if;
      E027 := E027 + 1;
      E074 := E074 + 1;
      E035 := E035 + 1;
      E039 := E039 + 1;
      if E064 = 0 then
         Libadalang.Project_Provider'Elab_Body;
      end if;
      E064 := E064 + 1;
      if E004 = 0 then
         Libadalang.Auto_Provider'Elab_Spec;
      end if;
      if E004 = 0 then
         Libadalang.Auto_Provider'Elab_Body;
      end if;
      E004 := E004 + 1;
      E006 := E006 + 1;
      if E010 = 0 then
         Libadalang.Config_Pragmas'Elab_Spec;
      end if;
      E010 := E010 + 1;
      if E021 = 0 then
         Libadalang.Generic_Api.Introspection'Elab_Spec;
      end if;
      E021 := E021 + 1;
      if E033 = 0 then
         Libadalang.Implementation.C'Elab_Body;
      end if;
      E033 := E033 + 1;
      if E041 = 0 then
         Libadalang.Introspection'Elab_Spec;
      end if;
      E041 := E041 + 1;
      if E047 = 0 then
         Libadalang.Iterators'Elab_Spec;
      end if;
      if E045 = 0 then
         Libadalang.Iterators.Extensions'Elab_Spec;
      end if;
      if E045 = 0 then
         Libadalang.Iterators.Extensions'Elab_Body;
      end if;
      E045 := E045 + 1;
      if E047 = 0 then
         Libadalang.Iterators'Elab_Body;
      end if;
      E047 := E047 + 1;
      if E061 = 0 then
         Libadalang.Preprocessing'Elab_Spec;
      end if;
      if E057 = 0 then
         Libadalang.Pp_Impl'Elab_Body;
      end if;
      E057 := E057 + 1;
      if E061 = 0 then
         Libadalang.Preprocessing'Elab_Body;
      end if;
      E061 := E061 + 1;
      if E029 = 0 then
         Libadalang.Helpers'Elab_Spec;
      end if;
      if E029 = 0 then
         Libadalang.Helpers'Elab_Body;
      end if;
      E029 := E029 + 1;
      E031 := E031 + 1;
      if E070 = 0 then
         Libadalang.Rewriting_Implementation'Elab_Spec;
      end if;
      if E078 = 0 then
         Libadalang.Unparsing_Implementation'Elab_Spec;
      end if;
      if E078 = 0 then
         Libadalang.Unparsing_Implementation'Elab_Body;
      end if;
      E078 := E078 + 1;
      E070 := E070 + 1;
      E068 := E068 + 1;
      E076 := E076 + 1;
   end adalanginit;

--  BEGIN Object file/option list
   --   /workspaces/bench-source/src/libadalang/obj/dev/libadalang.o
   --   /workspaces/bench-source/src/libadalang/obj/dev/libadalang-pp_lexer.o
   --   /workspaces/bench-source/src/libadalang/obj/dev/libadalang-sources.o
   --   /workspaces/bench-source/src/libadalang/obj/dev/libadalang-lexer_state_machine.o
   --   /workspaces/bench-source/src/libadalang/obj/dev/libadalang-config_pragmas_impl.o
   --   /workspaces/bench-source/src/libadalang/obj/dev/libadalang-debug.o
   --   /workspaces/bench-source/src/libadalang/obj/dev/libadalang-doc_utils.o
   --   /workspaces/bench-source/src/libadalang/obj/dev/libadalang-expr_eval.o
   --   /workspaces/bench-source/src/libadalang/obj/dev/libadalang-introspection_implementation.o
   --   /workspaces/bench-source/src/libadalang/obj/dev/libadalang-lexer.o
   --   /workspaces/bench-source/src/libadalang/obj/dev/libadalang-lexer_implementation.o
   --   /workspaces/bench-source/src/libadalang/obj/dev/libadalang-private_converters.o
   --   /workspaces/bench-source/src/libadalang/obj/dev/libadalang-common.o
   --   /workspaces/bench-source/src/libadalang/obj/dev/libadalang-implementation.o
   --   /workspaces/bench-source/src/libadalang/obj/dev/libadalang-parsers.o
   --   /workspaces/bench-source/src/libadalang/obj/dev/libadalang-public_converters.o
   --   /workspaces/bench-source/src/libadalang/obj/dev/libadalang-analysis.o
   --   /workspaces/bench-source/src/libadalang/obj/dev/libadalang-env_hooks.o
   --   /workspaces/bench-source/src/libadalang/obj/dev/libadalang-generic_api.o
   --   /workspaces/bench-source/src/libadalang/obj/dev/libadalang-generic_impl.o
   --   /workspaces/bench-source/src/libadalang/obj/dev/libadalang-generic_introspection.o
   --   /workspaces/bench-source/src/libadalang/obj/dev/libadalang-unit_files.o
   --   /workspaces/bench-source/src/libadalang/obj/dev/libadalang-implementation-extensions.o
   --   /workspaces/bench-source/src/libadalang/obj/dev/libadalang-internal_default_provider.o
   --   /workspaces/bench-source/src/libadalang/obj/dev/libadalang-project_provider.o
   --   /workspaces/bench-source/src/libadalang/obj/dev/libadalang-auto_provider.o
   --   /workspaces/bench-source/src/libadalang/obj/dev/libadalang-c.o
   --   /workspaces/bench-source/src/libadalang/obj/dev/libadalang-config_pragmas.o
   --   /workspaces/bench-source/src/libadalang/obj/dev/libadalang-generic_api-introspection.o
   --   /workspaces/bench-source/src/libadalang/obj/dev/libadalang-implementation-c.o
   --   /workspaces/bench-source/src/libadalang/obj/dev/libadalang-introspection.o
   --   /workspaces/bench-source/src/libadalang/obj/dev/libadalang-iterators-extensions.o
   --   /workspaces/bench-source/src/libadalang/obj/dev/libadalang-iterators.o
   --   /workspaces/bench-source/src/libadalang/obj/dev/libadalang-pp_impl.o
   --   /workspaces/bench-source/src/libadalang/obj/dev/libadalang-preprocessing.o
   --   /workspaces/bench-source/src/libadalang/obj/dev/libadalang-helpers.o
   --   /workspaces/bench-source/src/libadalang/obj/dev/libadalang-implementation-c-extensions.o
   --   /workspaces/bench-source/src/libadalang/obj/dev/libadalang-unparsing_implementation.o
   --   /workspaces/bench-source/src/libadalang/obj/dev/libadalang-rewriting_implementation.o
   --   /workspaces/bench-source/src/libadalang/obj/dev/libadalang-rewriting.o
   --   /workspaces/bench-source/src/libadalang/obj/dev/libadalang-unparsing.o
   --   -L/workspaces/bench-source/src/libadalang/obj/dev/
   --   -L/workspaces/bench-source/src/langkit_support/lib/static/dev/
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
   --   -lgnarl
   --   -lgnat
   --   -lutil
   --   -lrt
   --   -lpthread
   --   -ldl
--  END Object file/option list   

end adalangmain;
