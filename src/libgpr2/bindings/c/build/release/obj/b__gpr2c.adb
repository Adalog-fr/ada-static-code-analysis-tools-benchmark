pragma Warnings (Off);
pragma Ada_95;
pragma Source_File_Name (gpr2cmain, Spec_File_Name => "b__gpr2c.ads");
pragma Source_File_Name (gpr2cmain, Body_File_Name => "b__gpr2c.adb");
pragma Suppress (Overflow_Check);
with Ada.Exceptions;

package body gpr2cmain is

   E096 : Short_Integer; pragma Import (Ada, E096, "system__os_lib_E");
   E021 : Short_Integer; pragma Import (Ada, E021, "ada__exceptions_E");
   E026 : Short_Integer; pragma Import (Ada, E026, "system__soft_links_E");
   E037 : Short_Integer; pragma Import (Ada, E037, "system__exception_table_E");
   E063 : Short_Integer; pragma Import (Ada, E063, "ada__containers_E");
   E091 : Short_Integer; pragma Import (Ada, E091, "ada__io_exceptions_E");
   E044 : Short_Integer; pragma Import (Ada, E044, "ada__numerics_E");
   E078 : Short_Integer; pragma Import (Ada, E078, "ada__strings_E");
   E080 : Short_Integer; pragma Import (Ada, E080, "ada__strings__maps_E");
   E083 : Short_Integer; pragma Import (Ada, E083, "ada__strings__maps__constants_E");
   E068 : Short_Integer; pragma Import (Ada, E068, "interfaces__c_E");
   E038 : Short_Integer; pragma Import (Ada, E038, "system__exceptions_E");
   E105 : Short_Integer; pragma Import (Ada, E105, "system__object_reader_E");
   E073 : Short_Integer; pragma Import (Ada, E073, "system__dwarf_lines_E");
   E033 : Short_Integer; pragma Import (Ada, E033, "system__soft_links__initialize_E");
   E062 : Short_Integer; pragma Import (Ada, E062, "system__traceback__symbolic_E");
   E043 : Short_Integer; pragma Import (Ada, E043, "system__img_int_E");
   E086 : Short_Integer; pragma Import (Ada, E086, "system__img_uns_E");
   E457 : Short_Integer; pragma Import (Ada, E457, "ada__assertions_E");
   E125 : Short_Integer; pragma Import (Ada, E125, "ada__strings__utf_encoding_E");
   E133 : Short_Integer; pragma Import (Ada, E133, "ada__tags_E");
   E123 : Short_Integer; pragma Import (Ada, E123, "ada__strings__text_buffers_E");
   E196 : Short_Integer; pragma Import (Ada, E196, "gnat_E");
   E223 : Short_Integer; pragma Import (Ada, E223, "interfaces__c__strings_E");
   E146 : Short_Integer; pragma Import (Ada, E146, "ada__streams_E");
   E183 : Short_Integer; pragma Import (Ada, E183, "system__file_control_block_E");
   E152 : Short_Integer; pragma Import (Ada, E152, "system__finalization_root_E");
   E144 : Short_Integer; pragma Import (Ada, E144, "ada__finalization_E");
   E180 : Short_Integer; pragma Import (Ada, E180, "system__file_io_E");
   E277 : Short_Integer; pragma Import (Ada, E277, "ada__streams__stream_io_E");
   E247 : Short_Integer; pragma Import (Ada, E247, "system__regpat_E");
   E187 : Short_Integer; pragma Import (Ada, E187, "system__storage_pools_E");
   E185 : Short_Integer; pragma Import (Ada, E185, "system__finalization_masters_E");
   E191 : Short_Integer; pragma Import (Ada, E191, "system__storage_pools__subpools_E");
   E164 : Short_Integer; pragma Import (Ada, E164, "ada__strings__unbounded_E");
   E590 : Short_Integer; pragma Import (Ada, E590, "ada__strings__wide_wide_maps_E");
   E586 : Short_Integer; pragma Import (Ada, E586, "ada__strings__wide_wide_unbounded_E");
   E623 : Short_Integer; pragma Import (Ada, E623, "system__task_info_E");
   E019 : Short_Integer; pragma Import (Ada, E019, "ada__calendar_E");
   E329 : Short_Integer; pragma Import (Ada, E329, "ada__calendar__delays_E");
   E214 : Short_Integer; pragma Import (Ada, E214, "ada__calendar__time_zones_E");
   E230 : Short_Integer; pragma Import (Ada, E230, "ada__text_io_E");
   E505 : Short_Integer; pragma Import (Ada, E505, "ada__text_io__text_streams_E");
   E687 : Short_Integer; pragma Import (Ada, E687, "gnat__byte_order_mark_E");
   E225 : Short_Integer; pragma Import (Ada, E225, "gnat__calendar_E");
   E303 : Short_Integer; pragma Import (Ada, E303, "gnat__directory_operations_E");
   E568 : Short_Integer; pragma Import (Ada, E568, "gnat__dynamic_htables_E");
   E261 : Short_Integer; pragma Import (Ada, E261, "gnat__secure_hashes_E");
   E263 : Short_Integer; pragma Import (Ada, E263, "gnat__secure_hashes__md5_E");
   E259 : Short_Integer; pragma Import (Ada, E259, "gnat__md5_E");
   E270 : Short_Integer; pragma Import (Ada, E270, "gnat__string_split_E");
   E380 : Short_Integer; pragma Import (Ada, E380, "system__checked_pools_E");
   E251 : Short_Integer; pragma Import (Ada, E251, "system__pool_global_E");
   E242 : Short_Integer; pragma Import (Ada, E242, "gnat__expect_E");
   E322 : Short_Integer; pragma Import (Ada, E322, "system__random_seed_E");
   E189 : Short_Integer; pragma Import (Ada, E189, "system__regexp_E");
   E141 : Short_Integer; pragma Import (Ada, E141, "ada__directories_E");
   E416 : Short_Integer; pragma Import (Ada, E416, "system__img_llli_E");
   E232 : Short_Integer; pragma Import (Ada, E232, "system__img_lli_E");
   E617 : Short_Integer; pragma Import (Ada, E617, "system__task_primitives__operations_E");
   E234 : Short_Integer; pragma Import (Ada, E234, "system__img_llu_E");
   E228 : Short_Integer; pragma Import (Ada, E228, "gnat__calendar__time_io_E");
   E367 : Short_Integer; pragma Import (Ada, E367, "gnat__debug_pools_E");
   E392 : Short_Integer; pragma Import (Ada, E392, "gnat__formatted_string_E");
   E631 : Short_Integer; pragma Import (Ada, E631, "system__tasking__protected_objects_E");
   E712 : Short_Integer; pragma Import (Ada, E712, "gnatcoll__gmp__integers_E");
   E681 : Short_Integer; pragma Import (Ada, E681, "gpr_parser_support__errors_E");
   E472 : Short_Integer; pragma Import (Ada, E472, "unicode_E");
   E291 : Short_Integer; pragma Import (Ada, E291, "gnatcoll__atomic_E");
   E365 : Short_Integer; pragma Import (Ada, E365, "gnatcoll__memory_E");
   E238 : Short_Integer; pragma Import (Ada, E238, "gnatcoll__os_E");
   E297 : Short_Integer; pragma Import (Ada, E297, "gnatcoll__storage_pools__headers_E");
   E295 : Short_Integer; pragma Import (Ada, E295, "gnatcoll__refcount_E");
   E237 : Short_Integer; pragma Import (Ada, E237, "gnatcoll__string_builders_E");
   E382 : Short_Integer; pragma Import (Ada, E382, "gnatcoll__os__fs_E");
   E387 : Short_Integer; pragma Import (Ada, E387, "gnatcoll__os__stat_E");
   E361 : Short_Integer; pragma Import (Ada, E361, "gnatcoll__os__dir_E");
   E447 : Short_Integer; pragma Import (Ada, E447, "gnatcoll__string_list_builders_E");
   E445 : Short_Integer; pragma Import (Ada, E445, "gnatcoll__os__process_types_E");
   E442 : Short_Integer; pragma Import (Ada, E442, "gnatcoll__os__process_E");
   E293 : Short_Integer; pragma Import (Ada, E293, "gnatcoll__strings_impl_E");
   E289 : Short_Integer; pragma Import (Ada, E289, "gnatcoll__strings_E");
   E288 : Short_Integer; pragma Import (Ada, E288, "gnatcoll__strings_E");
   E801 : Short_Integer; pragma Import (Ada, E801, "gnatcoll__json_E");
   E803 : Short_Integer; pragma Import (Ada, E803, "gnatcoll__json__utility_E");
   E307 : Short_Integer; pragma Import (Ada, E307, "gnatcoll__mmap_E");
   E309 : Short_Integer; pragma Import (Ada, E309, "gnatcoll__mmap__system_E");
   E451 : Short_Integer; pragma Import (Ada, E451, "gnatcoll__templates_E");
   E453 : Short_Integer; pragma Import (Ada, E453, "gnatcoll__terminal_E");
   E331 : Short_Integer; pragma Import (Ada, E331, "gnatcoll__tribooleans_E");
   E210 : Short_Integer; pragma Import (Ada, E210, "gnatcoll__utils_E");
   E287 : Short_Integer; pragma Import (Ada, E287, "gnatcoll__io_E");
   E311 : Short_Integer; pragma Import (Ada, E311, "gnatcoll__path_E");
   E305 : Short_Integer; pragma Import (Ada, E305, "gnatcoll__io__native_E");
   E314 : Short_Integer; pragma Import (Ada, E314, "gnatcoll__remote_E");
   E318 : Short_Integer; pragma Import (Ada, E318, "gnatcoll__remote__db_E");
   E301 : Short_Integer; pragma Import (Ada, E301, "gnatcoll__io__remote_E");
   E313 : Short_Integer; pragma Import (Ada, E313, "gnatcoll__io__remote__unix_E");
   E316 : Short_Integer; pragma Import (Ada, E316, "gnatcoll__io__remote__windows_E");
   E283 : Short_Integer; pragma Import (Ada, E283, "gnatcoll__vfs_E");
   E449 : Short_Integer; pragma Import (Ada, E449, "gnatcoll__traces_E");
   E654 : Short_Integer; pragma Import (Ada, E654, "gnatcoll__iconv_E");
   E461 : Short_Integer; pragma Import (Ada, E461, "gnatcoll__vfs_utils_E");
   E139 : Short_Integer; pragma Import (Ada, E139, "gpr2_E");
   E344 : Short_Integer; pragma Import (Ada, E344, "gpr2__fnmatch_E");
   E275 : Short_Integer; pragma Import (Ada, E275, "gpr2__path_name_E");
   E335 : Short_Integer; pragma Import (Ada, E335, "gpr2__path_name__set_E");
   E273 : Short_Integer; pragma Import (Ada, E273, "gpr2__source_reference_E");
   E390 : Short_Integer; pragma Import (Ada, E390, "gpr2__message_E");
   E354 : Short_Integer; pragma Import (Ada, E354, "gpr2__source_reference__attribute_E");
   E607 : Short_Integer; pragma Import (Ada, E607, "gpr2__source_reference__pack_E");
   E325 : Short_Integer; pragma Import (Ada, E325, "gpr2__source_reference__text_value_E");
   E603 : Short_Integer; pragma Import (Ada, E603, "gpr2__source_reference__identifier_E");
   E776 : Short_Integer; pragma Import (Ada, E776, "gpr2__source_reference__identifier__set_E");
   E323 : Short_Integer; pragma Import (Ada, E323, "gpr2__source_reference__value_E");
   E268 : Short_Integer; pragma Import (Ada, E268, "gpr2__containers_E");
   E257 : Short_Integer; pragma Import (Ada, E257, "gpr2__context_E");
   E594 : Short_Integer; pragma Import (Ada, E594, "gpr2__builtin_E");
   E578 : Short_Integer; pragma Import (Ada, E578, "gpr2__log_E");
   E327 : Short_Integer; pragma Import (Ada, E327, "gpr2__project_E");
   E357 : Short_Integer; pragma Import (Ada, E357, "gpr2__project__attribute_index_E");
   E752 : Short_Integer; pragma Import (Ada, E752, "gpr2__project__import_E");
   E754 : Short_Integer; pragma Import (Ada, E754, "gpr2__project__import__set_E");
   E436 : Short_Integer; pragma Import (Ada, E436, "gpr2__project__registry__pack_E");
   E775 : Short_Integer; pragma Import (Ada, E775, "gpr2__unit_E");
   E773 : Short_Integer; pragma Import (Ada, E773, "gpr2__project__unit_info_E");
   E778 : Short_Integer; pragma Import (Ada, E778, "gpr2__project__unit_info__set_E");
   E784 : Short_Integer; pragma Import (Ada, E784, "gpr2__unit__list_E");
   E782 : Short_Integer; pragma Import (Ada, E782, "gpr2__source_info_E");
   E780 : Short_Integer; pragma Import (Ada, E780, "gpr2__source_E");
   E786 : Short_Integer; pragma Import (Ada, E786, "gpr2__source_info__parser_E");
   E582 : Short_Integer; pragma Import (Ada, E582, "gpr2__view_ids_E");
   E760 : Short_Integer; pragma Import (Ada, E760, "gpr2__view_ids__set_E");
   E761 : Short_Integer; pragma Import (Ada, E761, "gpr2__view_ids__vector_E");
   E798 : Short_Integer; pragma Import (Ada, E798, "gpr2__view_ids__dags_E");
   E714 : Short_Integer; pragma Import (Ada, E714, "gpr_parser_support__adalog_E");
   E716 : Short_Integer; pragma Import (Ada, E716, "gpr_parser_support__adalog__debug_E");
   E718 : Short_Integer; pragma Import (Ada, E718, "gpr_parser_support__adalog__logic_var_E");
   E724 : Short_Integer; pragma Import (Ada, E724, "gpr_parser_support__adalog__solver_interface_E");
   E726 : Short_Integer; pragma Import (Ada, E726, "gpr_parser_support__array_utils_E");
   E693 : Short_Integer; pragma Import (Ada, E693, "gpr_parser_support__hashes_E");
   E722 : Short_Integer; pragma Import (Ada, E722, "gpr_parser_support__images_E");
   E742 : Short_Integer; pragma Import (Ada, E742, "gpr_parser_support__packrat_E");
   E732 : Short_Integer; pragma Import (Ada, E732, "gpr_parser_support__relative_get_E");
   E647 : Short_Integer; pragma Import (Ada, E647, "gpr_parser_support__text_E");
   E707 : Short_Integer; pragma Import (Ada, E707, "gpr_parser_support__names_E");
   E641 : Short_Integer; pragma Import (Ada, E641, "gpr_parser_support__slocs_E");
   E639 : Short_Integer; pragma Import (Ada, E639, "gpr_parser_support__diagnostics_E");
   E685 : Short_Integer; pragma Import (Ada, E685, "gpr_parser_support__file_readers_E");
   E763 : Short_Integer; pragma Import (Ada, E763, "gpr2__file_readers_E");
   E697 : Short_Integer; pragma Import (Ada, E697, "gpr_parser_support__vectors_E");
   E720 : Short_Integer; pragma Import (Ada, E720, "gpr_parser_support__adalog__solver_E");
   E728 : Short_Integer; pragma Import (Ada, E728, "gpr_parser_support__cheap_sets_E");
   E746 : Short_Integer; pragma Import (Ada, E746, "gpr_parser_support__generic_bump_ptr_E");
   E744 : Short_Integer; pragma Import (Ada, E744, "gpr_parser_support__bump_ptr_E");
   E694 : Short_Integer; pragma Import (Ada, E694, "gpr_parser_support__lexical_envs_E");
   E701 : Short_Integer; pragma Import (Ada, E701, "gpr_parser_support__symbols_E");
   E730 : Short_Integer; pragma Import (Ada, E730, "gpr_parser_support__lexical_envs_impl_E");
   E734 : Short_Integer; pragma Import (Ada, E734, "gpr_parser_support__symbols__precomputed_E");
   E699 : Short_Integer; pragma Import (Ada, E699, "gpr_parser_support__token_data_handlers_E");
   E680 : Short_Integer; pragma Import (Ada, E680, "gpr_parser_support__generic_api_E");
   E691 : Short_Integer; pragma Import (Ada, E691, "gpr_parser_support__internal__analysis_E");
   E705 : Short_Integer; pragma Import (Ada, E705, "gpr_parser_support__generic_api__analysis_E");
   E689 : Short_Integer; pragma Import (Ada, E689, "gpr_parser_support__generic_api__introspection_E");
   E709 : Short_Integer; pragma Import (Ada, E709, "gpr_parser_support__internal__introspection_E");
   E664 : Short_Integer; pragma Import (Ada, E664, "gpr_parser__common_E");
   E736 : Short_Integer; pragma Import (Ada, E736, "gpr_parser__lexer_implementation_E");
   E738 : Short_Integer; pragma Import (Ada, E738, "gpr_parser__lexer_state_machine_E");
   E740 : Short_Integer; pragma Import (Ada, E740, "gpr_parser__parsers_E");
   E672 : Short_Integer; pragma Import (Ada, E672, "gpr_parser__implementation_E");
   E750 : Short_Integer; pragma Import (Ada, E750, "gpr_parser__debug_E");
   E660 : Short_Integer; pragma Import (Ada, E660, "gpr_parser__analysis_E");
   E666 : Short_Integer; pragma Import (Ada, E666, "gpr_parser__generic_api_E");
   E670 : Short_Integer; pragma Import (Ada, E670, "gpr_parser__generic_introspection_E");
   E677 : Short_Integer; pragma Import (Ada, E677, "gpr_parser__introspection_implementation_E");
   E668 : Short_Integer; pragma Import (Ada, E668, "gpr_parser__generic_impl_E");
   E748 : Short_Integer; pragma Import (Ada, E748, "gpr_parser__public_converters_E");
   E489 : Short_Integer; pragma Import (Ada, E489, "sax__htable_E");
   E493 : Short_Integer; pragma Import (Ada, E493, "sax__pointers_E");
   E572 : Short_Integer; pragma Import (Ada, E572, "sax__state_machines_E");
   E551 : Short_Integer; pragma Import (Ada, E551, "schema_E");
   E485 : Short_Integer; pragma Import (Ada, E485, "unicode__ccs_E");
   E509 : Short_Integer; pragma Import (Ada, E509, "unicode__ccs__iso_8859_1_E");
   E511 : Short_Integer; pragma Import (Ada, E511, "unicode__ccs__iso_8859_15_E");
   E516 : Short_Integer; pragma Import (Ada, E516, "unicode__ccs__iso_8859_2_E");
   E519 : Short_Integer; pragma Import (Ada, E519, "unicode__ccs__iso_8859_3_E");
   E521 : Short_Integer; pragma Import (Ada, E521, "unicode__ccs__iso_8859_4_E");
   E523 : Short_Integer; pragma Import (Ada, E523, "unicode__ccs__windows_1251_E");
   E528 : Short_Integer; pragma Import (Ada, E528, "unicode__ccs__windows_1252_E");
   E481 : Short_Integer; pragma Import (Ada, E481, "unicode__ces_E");
   E491 : Short_Integer; pragma Import (Ada, E491, "sax__symbols_E");
   E549 : Short_Integer; pragma Import (Ada, E549, "sax__locators_E");
   E547 : Short_Integer; pragma Import (Ada, E547, "sax__exceptions_E");
   E483 : Short_Integer; pragma Import (Ada, E483, "unicode__ces__utf32_E");
   E531 : Short_Integer; pragma Import (Ada, E531, "unicode__ces__basic_8bit_E");
   E533 : Short_Integer; pragma Import (Ada, E533, "unicode__ces__utf16_E");
   E487 : Short_Integer; pragma Import (Ada, E487, "unicode__ces__utf8_E");
   E545 : Short_Integer; pragma Import (Ada, E545, "sax__models_E");
   E543 : Short_Integer; pragma Import (Ada, E543, "sax__attributes_E");
   E495 : Short_Integer; pragma Import (Ada, E495, "sax__utils_E");
   E468 : Short_Integer; pragma Import (Ada, E468, "dom__core_E");
   E563 : Short_Integer; pragma Import (Ada, E563, "schema__date_time_E");
   E565 : Short_Integer; pragma Import (Ada, E565, "schema__decimal_E");
   E561 : Short_Integer; pragma Import (Ada, E561, "schema__simple_types_E");
   E507 : Short_Integer; pragma Import (Ada, E507, "unicode__encodings_E");
   E503 : Short_Integer; pragma Import (Ada, E503, "dom__core__nodes_E");
   E501 : Short_Integer; pragma Import (Ada, E501, "dom__core__attrs_E");
   E555 : Short_Integer; pragma Import (Ada, E555, "dom__core__character_datas_E");
   E497 : Short_Integer; pragma Import (Ada, E497, "dom__core__documents_E");
   E499 : Short_Integer; pragma Import (Ada, E499, "dom__core__elements_E");
   E535 : Short_Integer; pragma Import (Ada, E535, "input_sources_E");
   E537 : Short_Integer; pragma Import (Ada, E537, "input_sources__file_E");
   E539 : Short_Integer; pragma Import (Ada, E539, "input_sources__strings_E");
   E541 : Short_Integer; pragma Import (Ada, E541, "sax__readers_E");
   E570 : Short_Integer; pragma Import (Ada, E570, "schema__validators_E");
   E557 : Short_Integer; pragma Import (Ada, E557, "schema__readers_E");
   E559 : Short_Integer; pragma Import (Ada, E559, "schema__schema_readers_E");
   E574 : Short_Integer; pragma Import (Ada, E574, "schema__validators__xsd_grammar_E");
   E553 : Short_Integer; pragma Import (Ada, E553, "schema__dom_readers_E");
   E340 : Short_Integer; pragma Import (Ada, E340, "gpr2__project__registry__attribute_E");
   E353 : Short_Integer; pragma Import (Ada, E353, "gpr2__project__attr_values_E");
   E351 : Short_Integer; pragma Import (Ada, E351, "gpr2__project__attribute_E");
   E596 : Short_Integer; pragma Import (Ada, E596, "gpr2__project__attribute__set_E");
   E346 : Short_Integer; pragma Import (Ada, E346, "gpr2__project__attribute_cache_E");
   E601 : Short_Integer; pragma Import (Ada, E601, "gpr2__project__name_values_E");
   E605 : Short_Integer; pragma Import (Ada, E605, "gpr2__project__typ_E");
   E755 : Short_Integer; pragma Import (Ada, E755, "gpr2__project__typ__set_E");
   E599 : Short_Integer; pragma Import (Ada, E599, "gpr2__project__variable_E");
   E606 : Short_Integer; pragma Import (Ada, E606, "gpr2__project__variable__set_E");
   E597 : Short_Integer; pragma Import (Ada, E597, "gpr2__project__pack_E");
   E342 : Short_Integer; pragma Import (Ada, E342, "gpr2__project__view_E");
   E584 : Short_Integer; pragma Import (Ada, E584, "gpr2__project__parser_E");
   E580 : Short_Integer; pragma Import (Ada, E580, "gpr2__project__configuration_E");
   E440 : Short_Integer; pragma Import (Ada, E440, "gpr2__kb_E");
   E463 : Short_Integer; pragma Import (Ada, E463, "gpr2__kb__compiler_iterator_E");
   E465 : Short_Integer; pragma Import (Ada, E465, "gpr2__kb__parsing_E");
   E757 : Short_Integer; pragma Import (Ada, E757, "gpr2__project__parser__create_E");
   E609 : Short_Integer; pragma Import (Ada, E609, "gpr2__project__parser__registry_E");
   E799 : Short_Integer; pragma Import (Ada, E799, "gpr2__project__parser__set_E");
   E795 : Short_Integer; pragma Import (Ada, E795, "gpr2__project__view__set_E");
   E765 : Short_Integer; pragma Import (Ada, E765, "gpr2__project__source_E");
   E767 : Short_Integer; pragma Import (Ada, E767, "gpr2__project__source__artifact_E");
   E769 : Short_Integer; pragma Import (Ada, E769, "gpr2__project__source__part_set_E");
   E771 : Short_Integer; pragma Import (Ada, E771, "gpr2__project__source__set_E");
   E359 : Short_Integer; pragma Import (Ada, E359, "gpr2__project__definition_E");
   E796 : Short_Integer; pragma Import (Ada, E796, "gpr2__project__view__vector_E");
   E438 : Short_Integer; pragma Import (Ada, E438, "gpr2__project__tree_E");
   E759 : Short_Integer; pragma Import (Ada, E759, "gpr2__project__tree__view_builder_E");
   E790 : Short_Integer; pragma Import (Ada, E790, "gpr2__source_info__parser__ada_language_E");
   E792 : Short_Integer; pragma Import (Ada, E792, "gpr2__source_info__parser__ali_E");
   E794 : Short_Integer; pragma Import (Ada, E794, "gpr2__source_info__parser__d_E");
   E788 : Short_Integer; pragma Import (Ada, E788, "gpr2__source_info__parser__registry_E");
   E014 : Short_Integer; pragma Import (Ada, E014, "gpr2__c_E");
   E010 : Short_Integer; pragma Import (Ada, E010, "gpr2__c__utils_E");
   E004 : Short_Integer; pragma Import (Ada, E004, "gpr2__c__json_E");
   E002 : Short_Integer; pragma Import (Ada, E002, "gpr2__c__json__encoders_E");
   E006 : Short_Integer; pragma Import (Ada, E006, "gpr2__c__source_E");
   E008 : Short_Integer; pragma Import (Ada, E008, "gpr2__c__tree_E");
   E012 : Short_Integer; pragma Import (Ada, E012, "gpr2__c__view_E");

   Sec_Default_Sized_Stacks : array (1 .. 1) of aliased System.Secondary_Stack.SS_Stack (System.Parameters.Runtime_Default_Sec_Stack_Size);

   Local_Priority_Specific_Dispatching : constant String := "";
   Local_Interrupt_States : constant String := "";

   Is_Elaborated : Boolean := False;

   procedure finalize_library is
   begin
      E004 := E004 - 1;
      declare
         procedure F1;
         pragma Import (Ada, F1, "gpr2__c__json__finalize_spec");
      begin
         if E004 = 0 then
            F1;
         end if;
      end;
      E010 := E010 - 1;
      declare
         procedure F2;
         pragma Import (Ada, F2, "gpr2__c__utils__finalize_spec");
      begin
         if E010 = 0 then
            F2;
         end if;
      end;
      E794 := E794 - 1;
      declare
         procedure F3;
         pragma Import (Ada, F3, "gpr2__source_info__parser__ali__finalize_body");
      begin
         E792 := E792 - 1;
         if E792 = 0 then
            F3;
         end if;
      end;
      E790 := E790 - 1;
      declare
         procedure F4;
         pragma Import (Ada, F4, "gpr2__project__source__finalize_body");
      begin
         E765 := E765 - 1;
         if E765 = 0 then
            F4;
         end if;
      end;
      declare
         procedure F5;
         pragma Import (Ada, F5, "gpr2__project__definition__finalize_body");
      begin
         E359 := E359 - 1;
         if E359 = 0 then
            F5;
         end if;
      end;
      declare
         procedure F6;
         pragma Import (Ada, F6, "gpr2__source_info__parser__registry__finalize_body");
      begin
         E788 := E788 - 1;
         if E788 = 0 then
            F6;
         end if;
      end;
      declare
         procedure F7;
         pragma Import (Ada, F7, "gpr2__source_info__parser__d__finalize_spec");
      begin
         if E794 = 0 then
            F7;
         end if;
      end;
      declare
         procedure F8;
         pragma Import (Ada, F8, "gpr2__source_info__parser__ali__finalize_spec");
      begin
         if E792 = 0 then
            F8;
         end if;
      end;
      declare
         procedure F9;
         pragma Import (Ada, F9, "gpr2__source_info__parser__ada_language__finalize_spec");
      begin
         if E790 = 0 then
            F9;
         end if;
      end;
      E342 := E342 - 1;
      declare
         procedure F10;
         pragma Import (Ada, F10, "gpr2__project__tree__finalize_body");
      begin
         E438 := E438 - 1;
         if E438 = 0 then
            F10;
         end if;
      end;
      E759 := E759 - 1;
      declare
         procedure F11;
         pragma Import (Ada, F11, "gpr2__project__tree__view_builder__finalize_spec");
      begin
         if E759 = 0 then
            F11;
         end if;
      end;
      E767 := E767 - 1;
      E584 := E584 - 1;
      declare
         procedure F12;
         pragma Import (Ada, F12, "gpr2__project__tree__finalize_spec");
      begin
         if E438 = 0 then
            F12;
         end if;
      end;
      declare
         procedure F13;
         pragma Import (Ada, F13, "gpr2__project__view__vector__finalize_spec");
      begin
         E796 := E796 - 1;
         if E796 = 0 then
            F13;
         end if;
      end;
      E580 := E580 - 1;
      declare
         procedure F14;
         pragma Import (Ada, F14, "gpr2__project__definition__finalize_spec");
      begin
         if E359 = 0 then
            F14;
         end if;
      end;
      declare
         procedure F15;
         pragma Import (Ada, F15, "gpr2__project__source__set__finalize_body");
      begin
         E771 := E771 - 1;
         if E771 = 0 then
            F15;
         end if;
      end;
      declare
         procedure F16;
         pragma Import (Ada, F16, "gpr2__project__source__set__finalize_spec");
      begin
         if E771 = 0 then
            F16;
         end if;
      end;
      E769 := E769 - 1;
      declare
         procedure F17;
         pragma Import (Ada, F17, "gpr2__project__source__part_set__finalize_spec");
      begin
         if E769 = 0 then
            F17;
         end if;
      end;
      declare
         procedure F18;
         pragma Import (Ada, F18, "gpr2__project__source__artifact__finalize_spec");
      begin
         if E767 = 0 then
            F18;
         end if;
      end;
      declare
         procedure F19;
         pragma Import (Ada, F19, "gpr2__project__source__finalize_spec");
      begin
         if E765 = 0 then
            F19;
         end if;
      end;
      declare
         procedure F20;
         pragma Import (Ada, F20, "gpr2__project__view__set__finalize_spec");
      begin
         E795 := E795 - 1;
         if E795 = 0 then
            F20;
         end if;
      end;
      declare
         procedure F21;
         pragma Import (Ada, F21, "gpr2__project__registry__attribute__finalize_body");
      begin
         E340 := E340 - 1;
         if E340 = 0 then
            F21;
         end if;
      end;
      declare
         procedure F22;
         pragma Import (Ada, F22, "gpr2__project__parser__set__finalize_spec");
      begin
         E799 := E799 - 1;
         if E799 = 0 then
            F22;
         end if;
      end;
      declare
         procedure F23;
         pragma Import (Ada, F23, "gpr2__project__parser__registry__finalize_body");
      begin
         E609 := E609 - 1;
         if E609 = 0 then
            F23;
         end if;
      end;
      declare
         procedure F24;
         pragma Import (Ada, F24, "gpr2__kb__finalize_body");
      begin
         E440 := E440 - 1;
         if E440 = 0 then
            F24;
         end if;
      end;
      declare
         procedure F25;
         pragma Import (Ada, F25, "gpr2__kb__compiler_iterator__finalize_body");
      begin
         E463 := E463 - 1;
         if E463 = 0 then
            F25;
         end if;
      end;
      declare
         procedure F26;
         pragma Import (Ada, F26, "gpr2__kb__finalize_spec");
      begin
         if E440 = 0 then
            F26;
         end if;
      end;
      declare
         procedure F27;
         pragma Import (Ada, F27, "gpr2__project__configuration__finalize_spec");
      begin
         if E580 = 0 then
            F27;
         end if;
      end;
      declare
         procedure F28;
         pragma Import (Ada, F28, "gpr2__project__parser__finalize_spec");
      begin
         if E584 = 0 then
            F28;
         end if;
      end;
      declare
         procedure F29;
         pragma Import (Ada, F29, "gpr2__project__view__finalize_spec");
      begin
         if E342 = 0 then
            F29;
         end if;
      end;
      declare
         procedure F30;
         pragma Import (Ada, F30, "gpr2__project__pack__finalize_spec");
      begin
         E597 := E597 - 1;
         if E597 = 0 then
            F30;
         end if;
      end;
      declare
         procedure F31;
         pragma Import (Ada, F31, "gpr2__project__variable__set__finalize_spec");
      begin
         E606 := E606 - 1;
         if E606 = 0 then
            F31;
         end if;
      end;
      E599 := E599 - 1;
      declare
         procedure F32;
         pragma Import (Ada, F32, "gpr2__project__variable__finalize_spec");
      begin
         if E599 = 0 then
            F32;
         end if;
      end;
      declare
         procedure F33;
         pragma Import (Ada, F33, "gpr2__project__typ__set__finalize_spec");
      begin
         E755 := E755 - 1;
         if E755 = 0 then
            F33;
         end if;
      end;
      E605 := E605 - 1;
      declare
         procedure F34;
         pragma Import (Ada, F34, "gpr2__project__typ__finalize_spec");
      begin
         if E605 = 0 then
            F34;
         end if;
      end;
      E601 := E601 - 1;
      declare
         procedure F35;
         pragma Import (Ada, F35, "gpr2__project__name_values__finalize_spec");
      begin
         if E601 = 0 then
            F35;
         end if;
      end;
      E346 := E346 - 1;
      declare
         procedure F36;
         pragma Import (Ada, F36, "gpr2__project__attribute_cache__finalize_spec");
      begin
         if E346 = 0 then
            F36;
         end if;
      end;
      declare
         procedure F37;
         pragma Import (Ada, F37, "gpr2__project__attribute__set__finalize_body");
      begin
         E596 := E596 - 1;
         if E596 = 0 then
            F37;
         end if;
      end;
      declare
         procedure F38;
         pragma Import (Ada, F38, "gpr2__project__attribute__set__finalize_spec");
      begin
         if E596 = 0 then
            F38;
         end if;
      end;
      E351 := E351 - 1;
      declare
         procedure F39;
         pragma Import (Ada, F39, "gpr2__project__attribute__finalize_spec");
      begin
         if E351 = 0 then
            F39;
         end if;
      end;
      E353 := E353 - 1;
      declare
         procedure F40;
         pragma Import (Ada, F40, "gpr2__project__attr_values__finalize_spec");
      begin
         if E353 = 0 then
            F40;
         end if;
      end;
      declare
         procedure F41;
         pragma Import (Ada, F41, "gpr2__project__registry__attribute__finalize_spec");
      begin
         if E340 = 0 then
            F41;
         end if;
      end;
      E553 := E553 - 1;
      declare
         procedure F42;
         pragma Import (Ada, F42, "schema__dom_readers__finalize_spec");
      begin
         if E553 = 0 then
            F42;
         end if;
      end;
      E570 := E570 - 1;
      E557 := E557 - 1;
      E559 := E559 - 1;
      declare
         procedure F43;
         pragma Import (Ada, F43, "schema__schema_readers__finalize_spec");
      begin
         if E559 = 0 then
            F43;
         end if;
      end;
      declare
         procedure F44;
         pragma Import (Ada, F44, "schema__readers__finalize_spec");
      begin
         if E557 = 0 then
            F44;
         end if;
      end;
      declare
         procedure F45;
         pragma Import (Ada, F45, "schema__validators__finalize_spec");
      begin
         if E570 = 0 then
            F45;
         end if;
      end;
      E541 := E541 - 1;
      declare
         procedure F46;
         pragma Import (Ada, F46, "sax__readers__finalize_spec");
      begin
         if E541 = 0 then
            F46;
         end if;
      end;
      E539 := E539 - 1;
      declare
         procedure F47;
         pragma Import (Ada, F47, "input_sources__strings__finalize_spec");
      begin
         if E539 = 0 then
            F47;
         end if;
      end;
      E537 := E537 - 1;
      declare
         procedure F48;
         pragma Import (Ada, F48, "input_sources__file__finalize_spec");
      begin
         if E537 = 0 then
            F48;
         end if;
      end;
      E535 := E535 - 1;
      declare
         procedure F49;
         pragma Import (Ada, F49, "input_sources__finalize_spec");
      begin
         if E535 = 0 then
            F49;
         end if;
      end;
      E468 := E468 - 1;
      declare
         procedure F50;
         pragma Import (Ada, F50, "dom__core__finalize_spec");
      begin
         if E468 = 0 then
            F50;
         end if;
      end;
      E495 := E495 - 1;
      declare
         procedure F51;
         pragma Import (Ada, F51, "sax__utils__finalize_spec");
      begin
         if E495 = 0 then
            F51;
         end if;
      end;
      E543 := E543 - 1;
      declare
         procedure F52;
         pragma Import (Ada, F52, "sax__attributes__finalize_spec");
      begin
         if E543 = 0 then
            F52;
         end if;
      end;
      E547 := E547 - 1;
      declare
         procedure F53;
         pragma Import (Ada, F53, "sax__exceptions__finalize_spec");
      begin
         if E547 = 0 then
            F53;
         end if;
      end;
      E491 := E491 - 1;
      declare
         procedure F54;
         pragma Import (Ada, F54, "sax__symbols__finalize_spec");
      begin
         if E491 = 0 then
            F54;
         end if;
      end;
      E493 := E493 - 1;
      declare
         procedure F55;
         pragma Import (Ada, F55, "sax__pointers__finalize_spec");
      begin
         if E493 = 0 then
            F55;
         end if;
      end;
      E670 := E670 - 1;
      E660 := E660 - 1;
      declare
         procedure F56;
         pragma Import (Ada, F56, "gpr_parser__public_converters__finalize_body");
      begin
         E748 := E748 - 1;
         if E748 = 0 then
            F56;
         end if;
      end;
      declare
         procedure F57;
         pragma Import (Ada, F57, "gpr_parser__public_converters__finalize_spec");
      begin
         if E748 = 0 then
            F57;
         end if;
      end;
      declare
         procedure F58;
         pragma Import (Ada, F58, "gpr_parser__parsers__finalize_body");
      begin
         E740 := E740 - 1;
         if E740 = 0 then
            F58;
         end if;
      end;
      declare
         procedure F59;
         pragma Import (Ada, F59, "gpr_parser__implementation__finalize_body");
      begin
         E672 := E672 - 1;
         if E672 = 0 then
            F59;
         end if;
      end;
      E677 := E677 - 1;
      declare
         procedure F60;
         pragma Import (Ada, F60, "gpr_parser__introspection_implementation__finalize_spec");
      begin
         if E677 = 0 then
            F60;
         end if;
      end;
      declare
         procedure F61;
         pragma Import (Ada, F61, "gpr_parser__generic_introspection__finalize_spec");
      begin
         if E670 = 0 then
            F61;
         end if;
      end;
      declare
         procedure F62;
         pragma Import (Ada, F62, "gpr_parser__analysis__finalize_spec");
      begin
         if E660 = 0 then
            F62;
         end if;
      end;
      declare
         procedure F63;
         pragma Import (Ada, F63, "gpr_parser__implementation__finalize_spec");
      begin
         if E672 = 0 then
            F63;
         end if;
      end;
      E709 := E709 - 1;
      E689 := E689 - 1;
      E705 := E705 - 1;
      declare
         procedure F64;
         pragma Import (Ada, F64, "gpr_parser_support__internal__introspection__finalize_spec");
      begin
         if E709 = 0 then
            F64;
         end if;
      end;
      declare
         procedure F65;
         pragma Import (Ada, F65, "gpr_parser_support__generic_api__introspection__finalize_spec");
      begin
         if E689 = 0 then
            F65;
         end if;
      end;
      declare
         procedure F66;
         pragma Import (Ada, F66, "gpr_parser_support__generic_api__analysis__finalize_spec");
      begin
         if E705 = 0 then
            F66;
         end if;
      end;
      E699 := E699 - 1;
      declare
         procedure F67;
         pragma Import (Ada, F67, "gpr_parser_support__token_data_handlers__finalize_spec");
      begin
         if E699 = 0 then
            F67;
         end if;
      end;
      E701 := E701 - 1;
      declare
         procedure F68;
         pragma Import (Ada, F68, "gpr_parser_support__symbols__finalize_spec");
      begin
         if E701 = 0 then
            F68;
         end if;
      end;
      declare
         procedure F69;
         pragma Import (Ada, F69, "gpr_parser_support__lexical_envs__finalize_spec");
      begin
         E694 := E694 - 1;
         if E694 = 0 then
            F69;
         end if;
      end;
      E744 := E744 - 1;
      declare
         procedure F70;
         pragma Import (Ada, F70, "gpr_parser_support__bump_ptr__finalize_spec");
      begin
         if E744 = 0 then
            F70;
         end if;
      end;
      declare
         procedure F71;
         pragma Import (Ada, F71, "gpr2__file_readers__finalize_body");
      begin
         E763 := E763 - 1;
         if E763 = 0 then
            F71;
         end if;
      end;
      declare
         procedure F72;
         pragma Import (Ada, F72, "gpr2__file_readers__finalize_spec");
      begin
         if E763 = 0 then
            F72;
         end if;
      end;
      E685 := E685 - 1;
      declare
         procedure F73;
         pragma Import (Ada, F73, "gpr_parser_support__file_readers__finalize_spec");
      begin
         if E685 = 0 then
            F73;
         end if;
      end;
      E639 := E639 - 1;
      declare
         procedure F74;
         pragma Import (Ada, F74, "gpr_parser_support__diagnostics__finalize_spec");
      begin
         if E639 = 0 then
            F74;
         end if;
      end;
      declare
         procedure F75;
         pragma Import (Ada, F75, "gpr2__view_ids__dags__finalize_body");
      begin
         E798 := E798 - 1;
         if E798 = 0 then
            F75;
         end if;
      end;
      declare
         procedure F76;
         pragma Import (Ada, F76, "gpr2__view_ids__dags__finalize_spec");
      begin
         if E798 = 0 then
            F76;
         end if;
      end;
      declare
         procedure F77;
         pragma Import (Ada, F77, "gpr2__view_ids__vector__finalize_spec");
      begin
         E761 := E761 - 1;
         if E761 = 0 then
            F77;
         end if;
      end;
      declare
         procedure F78;
         pragma Import (Ada, F78, "gpr2__view_ids__set__finalize_spec");
      begin
         E760 := E760 - 1;
         if E760 = 0 then
            F78;
         end if;
      end;
      E786 := E786 - 1;
      declare
         procedure F79;
         pragma Import (Ada, F79, "gpr2__source_info__parser__finalize_spec");
      begin
         if E786 = 0 then
            F79;
         end if;
      end;
      E780 := E780 - 1;
      declare
         procedure F80;
         pragma Import (Ada, F80, "gpr2__source__finalize_spec");
      begin
         if E780 = 0 then
            F80;
         end if;
      end;
      E782 := E782 - 1;
      declare
         procedure F81;
         pragma Import (Ada, F81, "gpr2__source_info__finalize_spec");
      begin
         if E782 = 0 then
            F81;
         end if;
      end;
      declare
         procedure F82;
         pragma Import (Ada, F82, "gpr2__unit__list__finalize_body");
      begin
         E784 := E784 - 1;
         if E784 = 0 then
            F82;
         end if;
      end;
      declare
         procedure F83;
         pragma Import (Ada, F83, "gpr2__unit__list__finalize_spec");
      begin
         if E784 = 0 then
            F83;
         end if;
      end;
      declare
         procedure F84;
         pragma Import (Ada, F84, "gpr2__project__unit_info__set__finalize_spec");
      begin
         E778 := E778 - 1;
         if E778 = 0 then
            F84;
         end if;
      end;
      E773 := E773 - 1;
      declare
         procedure F85;
         pragma Import (Ada, F85, "gpr2__project__unit_info__finalize_spec");
      begin
         if E773 = 0 then
            F85;
         end if;
      end;
      E775 := E775 - 1;
      declare
         procedure F86;
         pragma Import (Ada, F86, "gpr2__unit__finalize_spec");
      begin
         if E775 = 0 then
            F86;
         end if;
      end;
      declare
         procedure F87;
         pragma Import (Ada, F87, "gpr2__project__registry__pack__finalize_body");
      begin
         E436 := E436 - 1;
         if E436 = 0 then
            F87;
         end if;
      end;
      declare
         procedure F88;
         pragma Import (Ada, F88, "gpr2__project__import__set__finalize_body");
      begin
         E754 := E754 - 1;
         if E754 = 0 then
            F88;
         end if;
      end;
      declare
         procedure F89;
         pragma Import (Ada, F89, "gpr2__project__import__set__finalize_spec");
      begin
         if E754 = 0 then
            F89;
         end if;
      end;
      E752 := E752 - 1;
      declare
         procedure F90;
         pragma Import (Ada, F90, "gpr2__project__import__finalize_spec");
      begin
         if E752 = 0 then
            F90;
         end if;
      end;
      E357 := E357 - 1;
      declare
         procedure F91;
         pragma Import (Ada, F91, "gpr2__project__attribute_index__finalize_spec");
      begin
         if E357 = 0 then
            F91;
         end if;
      end;
      E327 := E327 - 1;
      declare
         procedure F92;
         pragma Import (Ada, F92, "gpr2__project__finalize_spec");
      begin
         if E327 = 0 then
            F92;
         end if;
      end;
      declare
         procedure F93;
         pragma Import (Ada, F93, "gpr2__log__finalize_body");
      begin
         E578 := E578 - 1;
         if E578 = 0 then
            F93;
         end if;
      end;
      declare
         procedure F94;
         pragma Import (Ada, F94, "gpr2__log__finalize_spec");
      begin
         if E578 = 0 then
            F94;
         end if;
      end;
      E257 := E257 - 1;
      declare
         procedure F95;
         pragma Import (Ada, F95, "gpr2__context__finalize_spec");
      begin
         if E257 = 0 then
            F95;
         end if;
      end;
      E268 := E268 - 1;
      declare
         procedure F96;
         pragma Import (Ada, F96, "gpr2__containers__finalize_spec");
      begin
         if E268 = 0 then
            F96;
         end if;
      end;
      declare
         procedure F97;
         pragma Import (Ada, F97, "gpr2__source_reference__value__finalize_spec");
      begin
         E323 := E323 - 1;
         if E323 = 0 then
            F97;
         end if;
      end;
      declare
         procedure F98;
         pragma Import (Ada, F98, "gpr2__source_reference__identifier__set__finalize_spec");
      begin
         E776 := E776 - 1;
         if E776 = 0 then
            F98;
         end if;
      end;
      E603 := E603 - 1;
      declare
         procedure F99;
         pragma Import (Ada, F99, "gpr2__source_reference__identifier__finalize_spec");
      begin
         if E603 = 0 then
            F99;
         end if;
      end;
      declare
         procedure F100;
         pragma Import (Ada, F100, "gpr2__source_reference__pack__finalize_spec");
      begin
         E607 := E607 - 1;
         if E607 = 0 then
            F100;
         end if;
      end;
      declare
         procedure F101;
         pragma Import (Ada, F101, "gpr2__source_reference__attribute__finalize_spec");
      begin
         E354 := E354 - 1;
         if E354 = 0 then
            F101;
         end if;
      end;
      E390 := E390 - 1;
      declare
         procedure F102;
         pragma Import (Ada, F102, "gpr2__message__finalize_spec");
      begin
         if E390 = 0 then
            F102;
         end if;
      end;
      E273 := E273 - 1;
      declare
         procedure F103;
         pragma Import (Ada, F103, "gpr2__source_reference__finalize_spec");
      begin
         if E273 = 0 then
            F103;
         end if;
      end;
      E335 := E335 - 1;
      declare
         procedure F104;
         pragma Import (Ada, F104, "gpr2__path_name__set__finalize_spec");
      begin
         if E335 = 0 then
            F104;
         end if;
      end;
      E275 := E275 - 1;
      declare
         procedure F105;
         pragma Import (Ada, F105, "gpr2__path_name__finalize_spec");
      begin
         if E275 = 0 then
            F105;
         end if;
      end;
      E139 := E139 - 1;
      declare
         procedure F106;
         pragma Import (Ada, F106, "gpr2__finalize_spec");
      begin
         if E139 = 0 then
            F106;
         end if;
      end;
      declare
         procedure F107;
         pragma Import (Ada, F107, "gnatcoll__traces__finalize_body");
      begin
         E449 := E449 - 1;
         if E449 = 0 then
            F107;
         end if;
      end;
      declare
         procedure F108;
         pragma Import (Ada, F108, "gnatcoll__traces__finalize_spec");
      begin
         if E449 = 0 then
            F108;
         end if;
      end;
      E283 := E283 - 1;
      declare
         procedure F109;
         pragma Import (Ada, F109, "gnatcoll__vfs__finalize_spec");
      begin
         if E283 = 0 then
            F109;
         end if;
      end;
      E301 := E301 - 1;
      declare
         procedure F110;
         pragma Import (Ada, F110, "gnatcoll__io__remote__finalize_spec");
      begin
         if E301 = 0 then
            F110;
         end if;
      end;
      declare
         procedure F111;
         pragma Import (Ada, F111, "gnatcoll__remote__finalize_spec");
      begin
         E314 := E314 - 1;
         if E314 = 0 then
            F111;
         end if;
      end;
      E305 := E305 - 1;
      declare
         procedure F112;
         pragma Import (Ada, F112, "gnatcoll__io__native__finalize_spec");
      begin
         if E305 = 0 then
            F112;
         end if;
      end;
      E287 := E287 - 1;
      declare
         procedure F113;
         pragma Import (Ada, F113, "gnatcoll__io__finalize_spec");
      begin
         if E287 = 0 then
            F113;
         end if;
      end;
      E453 := E453 - 1;
      declare
         procedure F114;
         pragma Import (Ada, F114, "gnatcoll__terminal__finalize_spec");
      begin
         if E453 = 0 then
            F114;
         end if;
      end;
      E801 := E801 - 1;
      declare
         procedure F115;
         pragma Import (Ada, F115, "gnatcoll__json__finalize_spec");
      begin
         if E801 = 0 then
            F115;
         end if;
      end;
      E289 := E289 - 1;
      declare
         procedure F116;
         pragma Import (Ada, F116, "gnatcoll__strings__finalize_spec");
      begin
         if E289 = 0 then
            F116;
         end if;
      end;
      E442 := E442 - 1;
      declare
         procedure F117;
         pragma Import (Ada, F117, "gnatcoll__os__process__finalize_spec");
      begin
         if E442 = 0 then
            F117;
         end if;
      end;
      E295 := E295 - 1;
      declare
         procedure F118;
         pragma Import (Ada, F118, "gnatcoll__refcount__finalize_spec");
      begin
         if E295 = 0 then
            F118;
         end if;
      end;
      declare
         procedure F119;
         pragma Import (Ada, F119, "gnatcoll__memory__finalize_body");
      begin
         E365 := E365 - 1;
         if E365 = 0 then
            F119;
         end if;
      end;
      E712 := E712 - 1;
      declare
         procedure F120;
         pragma Import (Ada, F120, "gnatcoll__gmp__integers__finalize_spec");
      begin
         if E712 = 0 then
            F120;
         end if;
      end;
      E392 := E392 - 1;
      declare
         procedure F121;
         pragma Import (Ada, F121, "gnat__formatted_string__finalize_spec");
      begin
         if E392 = 0 then
            F121;
         end if;
      end;
      declare
         procedure F122;
         pragma Import (Ada, F122, "gnat__debug_pools__finalize_body");
      begin
         E367 := E367 - 1;
         if E367 = 0 then
            F122;
         end if;
      end;
      declare
         procedure F123;
         pragma Import (Ada, F123, "gnat__debug_pools__finalize_spec");
      begin
         if E367 = 0 then
            F123;
         end if;
      end;
      declare
         procedure F124;
         pragma Import (Ada, F124, "ada__directories__finalize_body");
      begin
         E141 := E141 - 1;
         if E141 = 0 then
            F124;
         end if;
      end;
      declare
         procedure F125;
         pragma Import (Ada, F125, "ada__directories__finalize_spec");
      begin
         if E141 = 0 then
            F125;
         end if;
      end;
      E189 := E189 - 1;
      declare
         procedure F126;
         pragma Import (Ada, F126, "system__regexp__finalize_spec");
      begin
         if E189 = 0 then
            F126;
         end if;
      end;
      E242 := E242 - 1;
      declare
         procedure F127;
         pragma Import (Ada, F127, "gnat__expect__finalize_spec");
      begin
         if E242 = 0 then
            F127;
         end if;
      end;
      E251 := E251 - 1;
      declare
         procedure F128;
         pragma Import (Ada, F128, "system__pool_global__finalize_spec");
      begin
         if E251 = 0 then
            F128;
         end if;
      end;
      E270 := E270 - 1;
      declare
         procedure F129;
         pragma Import (Ada, F129, "gnat__string_split__finalize_spec");
      begin
         if E270 = 0 then
            F129;
         end if;
      end;
      E259 := E259 - 1;
      declare
         procedure F130;
         pragma Import (Ada, F130, "gnat__md5__finalize_spec");
      begin
         if E259 = 0 then
            F130;
         end if;
      end;
      E230 := E230 - 1;
      declare
         procedure F131;
         pragma Import (Ada, F131, "ada__text_io__finalize_spec");
      begin
         if E230 = 0 then
            F131;
         end if;
      end;
      E586 := E586 - 1;
      declare
         procedure F132;
         pragma Import (Ada, F132, "ada__strings__wide_wide_unbounded__finalize_spec");
      begin
         if E586 = 0 then
            F132;
         end if;
      end;
      E590 := E590 - 1;
      declare
         procedure F133;
         pragma Import (Ada, F133, "ada__strings__wide_wide_maps__finalize_spec");
      begin
         if E590 = 0 then
            F133;
         end if;
      end;
      E164 := E164 - 1;
      declare
         procedure F134;
         pragma Import (Ada, F134, "ada__strings__unbounded__finalize_spec");
      begin
         if E164 = 0 then
            F134;
         end if;
      end;
      E191 := E191 - 1;
      declare
         procedure F135;
         pragma Import (Ada, F135, "system__storage_pools__subpools__finalize_spec");
      begin
         if E191 = 0 then
            F135;
         end if;
      end;
      E185 := E185 - 1;
      declare
         procedure F136;
         pragma Import (Ada, F136, "system__finalization_masters__finalize_spec");
      begin
         if E185 = 0 then
            F136;
         end if;
      end;
      E277 := E277 - 1;
      declare
         procedure F137;
         pragma Import (Ada, F137, "ada__streams__stream_io__finalize_spec");
      begin
         if E277 = 0 then
            F137;
         end if;
      end;
      declare
         procedure F138;
         pragma Import (Ada, F138, "system__file_io__finalize_body");
      begin
         E180 := E180 - 1;
         if E180 = 0 then
            F138;
         end if;
      end;
      declare
         procedure Reraise_Library_Exception_If_Any;
            pragma Import (Ada, Reraise_Library_Exception_If_Any, "__gnat_reraise_library_exception_if_any");
      begin
         Reraise_Library_Exception_If_Any;
      end;
   end finalize_library;

   procedure gpr2cfinal is

      procedure Runtime_Finalize;
      pragma Import (C, Runtime_Finalize, "__gnat_runtime_finalize");

   begin
      if not Is_Elaborated then
         return;
      end if;
      Is_Elaborated := False;
      Runtime_Finalize;
      finalize_library;
   end gpr2cfinal;

   type No_Param_Proc is access procedure;
   pragma Favor_Top_Level (No_Param_Proc);

   procedure gpr2cinit is
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
      Detect_Blocking := 0;
      Default_Stack_Size := -1;

      gpr2cmain'Elab_Body;
      Default_Secondary_Stack_Size := System.Parameters.Runtime_Default_Sec_Stack_Size;
      Binder_Sec_Stacks_Count := 1;
      Default_Sized_SS_Pool := Sec_Default_Sized_Stacks'Address;

      Runtime_Initialize (1);

      if E021 = 0 then
         Ada.Exceptions'Elab_Spec;
      end if;
      if E026 = 0 then
         System.Soft_Links'Elab_Spec;
      end if;
      if E037 = 0 then
         System.Exception_Table'Elab_Body;
      end if;
      E037 := E037 + 1;
      if E063 = 0 then
         Ada.Containers'Elab_Spec;
      end if;
      E063 := E063 + 1;
      if E091 = 0 then
         Ada.Io_Exceptions'Elab_Spec;
      end if;
      E091 := E091 + 1;
      if E044 = 0 then
         Ada.Numerics'Elab_Spec;
      end if;
      E044 := E044 + 1;
      if E078 = 0 then
         Ada.Strings'Elab_Spec;
      end if;
      E078 := E078 + 1;
      if E080 = 0 then
         Ada.Strings.Maps'Elab_Spec;
      end if;
      E080 := E080 + 1;
      if E083 = 0 then
         Ada.Strings.Maps.Constants'Elab_Spec;
      end if;
      E083 := E083 + 1;
      if E068 = 0 then
         Interfaces.C'Elab_Spec;
      end if;
      E068 := E068 + 1;
      if E038 = 0 then
         System.Exceptions'Elab_Spec;
      end if;
      E038 := E038 + 1;
      if E105 = 0 then
         System.Object_Reader'Elab_Spec;
      end if;
      E105 := E105 + 1;
      if E073 = 0 then
         System.Dwarf_Lines'Elab_Spec;
      end if;
      if E096 = 0 then
         System.Os_Lib'Elab_Body;
      end if;
      E096 := E096 + 1;
      if E033 = 0 then
         System.Soft_Links.Initialize'Elab_Body;
      end if;
      E033 := E033 + 1;
      E026 := E026 + 1;
      if E062 = 0 then
         System.Traceback.Symbolic'Elab_Body;
      end if;
      E062 := E062 + 1;
      if E043 = 0 then
         System.Img_Int'Elab_Spec;
      end if;
      E043 := E043 + 1;
      E021 := E021 + 1;
      if E086 = 0 then
         System.Img_Uns'Elab_Spec;
      end if;
      E086 := E086 + 1;
      E073 := E073 + 1;
      if E457 = 0 then
         Ada.Assertions'Elab_Spec;
      end if;
      E457 := E457 + 1;
      if E125 = 0 then
         Ada.Strings.Utf_Encoding'Elab_Spec;
      end if;
      E125 := E125 + 1;
      if E133 = 0 then
         Ada.Tags'Elab_Spec;
      end if;
      if E133 = 0 then
         Ada.Tags'Elab_Body;
      end if;
      E133 := E133 + 1;
      if E123 = 0 then
         Ada.Strings.Text_Buffers'Elab_Spec;
      end if;
      E123 := E123 + 1;
      if E196 = 0 then
         Gnat'Elab_Spec;
      end if;
      E196 := E196 + 1;
      if E223 = 0 then
         Interfaces.C.Strings'Elab_Spec;
      end if;
      E223 := E223 + 1;
      if E146 = 0 then
         Ada.Streams'Elab_Spec;
      end if;
      E146 := E146 + 1;
      if E183 = 0 then
         System.File_Control_Block'Elab_Spec;
      end if;
      E183 := E183 + 1;
      if E152 = 0 then
         System.Finalization_Root'Elab_Spec;
      end if;
      E152 := E152 + 1;
      if E144 = 0 then
         Ada.Finalization'Elab_Spec;
      end if;
      E144 := E144 + 1;
      if E180 = 0 then
         System.File_Io'Elab_Body;
      end if;
      E180 := E180 + 1;
      if E277 = 0 then
         Ada.Streams.Stream_Io'Elab_Spec;
      end if;
      E277 := E277 + 1;
      if E247 = 0 then
         System.Regpat'Elab_Spec;
      end if;
      E247 := E247 + 1;
      if E187 = 0 then
         System.Storage_Pools'Elab_Spec;
      end if;
      E187 := E187 + 1;
      if E185 = 0 then
         System.Finalization_Masters'Elab_Spec;
      end if;
      if E185 = 0 then
         System.Finalization_Masters'Elab_Body;
      end if;
      E185 := E185 + 1;
      if E191 = 0 then
         System.Storage_Pools.Subpools'Elab_Spec;
      end if;
      E191 := E191 + 1;
      if E164 = 0 then
         Ada.Strings.Unbounded'Elab_Spec;
      end if;
      E164 := E164 + 1;
      if E590 = 0 then
         Ada.Strings.Wide_Wide_Maps'Elab_Spec;
      end if;
      E590 := E590 + 1;
      if E586 = 0 then
         Ada.Strings.Wide_Wide_Unbounded'Elab_Spec;
      end if;
      E586 := E586 + 1;
      if E623 = 0 then
         System.Task_Info'Elab_Spec;
      end if;
      E623 := E623 + 1;
      if E019 = 0 then
         Ada.Calendar'Elab_Spec;
      end if;
      if E019 = 0 then
         Ada.Calendar'Elab_Body;
      end if;
      E019 := E019 + 1;
      if E329 = 0 then
         Ada.Calendar.Delays'Elab_Body;
      end if;
      E329 := E329 + 1;
      if E214 = 0 then
         Ada.Calendar.Time_Zones'Elab_Spec;
      end if;
      E214 := E214 + 1;
      if E230 = 0 then
         Ada.Text_Io'Elab_Spec;
      end if;
      if E230 = 0 then
         Ada.Text_Io'Elab_Body;
      end if;
      E230 := E230 + 1;
      if E505 = 0 then
         Ada.Text_Io.Text_Streams'Elab_Spec;
      end if;
      E505 := E505 + 1;
      E687 := E687 + 1;
      if E225 = 0 then
         Gnat.Calendar'Elab_Spec;
      end if;
      E225 := E225 + 1;
      if E303 = 0 then
         Gnat.Directory_Operations'Elab_Spec;
      end if;
      if E303 = 0 then
         Gnat.Directory_Operations'Elab_Body;
      end if;
      E303 := E303 + 1;
      E568 := E568 + 1;
      E261 := E261 + 1;
      E263 := E263 + 1;
      if E259 = 0 then
         Gnat.Md5'Elab_Spec;
      end if;
      E259 := E259 + 1;
      if E270 = 0 then
         Gnat.String_Split'Elab_Spec;
      end if;
      E270 := E270 + 1;
      if E380 = 0 then
         System.Checked_Pools'Elab_Spec;
      end if;
      E380 := E380 + 1;
      if E251 = 0 then
         System.Pool_Global'Elab_Spec;
      end if;
      if E251 = 0 then
         System.Pool_Global'Elab_Body;
      end if;
      E251 := E251 + 1;
      if E242 = 0 then
         Gnat.Expect'Elab_Spec;
      end if;
      if E242 = 0 then
         Gnat.Expect'Elab_Body;
      end if;
      E242 := E242 + 1;
      if E322 = 0 then
         System.Random_Seed'Elab_Body;
      end if;
      E322 := E322 + 1;
      if E189 = 0 then
         System.Regexp'Elab_Spec;
      end if;
      if E189 = 0 then
         System.Regexp'Elab_Body;
      end if;
      E189 := E189 + 1;
      if E141 = 0 then
         Ada.Directories'Elab_Spec;
      end if;
      if E141 = 0 then
         Ada.Directories'Elab_Body;
      end if;
      E141 := E141 + 1;
      if E416 = 0 then
         System.Img_Llli'Elab_Spec;
      end if;
      E416 := E416 + 1;
      if E232 = 0 then
         System.Img_Lli'Elab_Spec;
      end if;
      E232 := E232 + 1;
      if E617 = 0 then
         System.Task_Primitives.Operations'Elab_Body;
      end if;
      E617 := E617 + 1;
      if E234 = 0 then
         System.Img_Llu'Elab_Spec;
      end if;
      E234 := E234 + 1;
      if E228 = 0 then
         Gnat.Calendar.Time_Io'Elab_Spec;
      end if;
      E228 := E228 + 1;
      if E367 = 0 then
         Gnat.Debug_Pools'Elab_Spec;
      end if;
      if E367 = 0 then
         Gnat.Debug_Pools'Elab_Body;
      end if;
      E367 := E367 + 1;
      if E392 = 0 then
         Gnat.Formatted_String'Elab_Spec;
      end if;
      if E392 = 0 then
         Gnat.Formatted_String'Elab_Body;
      end if;
      E392 := E392 + 1;
      if E631 = 0 then
         System.Tasking.Protected_Objects'Elab_Body;
      end if;
      E631 := E631 + 1;
      if E712 = 0 then
         GNATCOLL.GMP.INTEGERS'ELAB_SPEC;
      end if;
      E712 := E712 + 1;
      if E681 = 0 then
         Gpr_Parser_Support.Errors'Elab_Spec;
      end if;
      E681 := E681 + 1;
      if E472 = 0 then
         Unicode'Elab_Body;
      end if;
      E472 := E472 + 1;
      E291 := E291 + 1;
      if E365 = 0 then
         GNATCOLL.MEMORY'ELAB_BODY;
      end if;
      E365 := E365 + 1;
      if E238 = 0 then
         GNATCOLL.OS'ELAB_SPEC;
      end if;
      E238 := E238 + 1;
      E297 := E297 + 1;
      if E295 = 0 then
         GNATCOLL.REFCOUNT'ELAB_SPEC;
      end if;
      E295 := E295 + 1;
      E237 := E237 + 1;
      E382 := E382 + 1;
      E387 := E387 + 1;
      E361 := E361 + 1;
      E447 := E447 + 1;
      E445 := E445 + 1;
      if E442 = 0 then
         GNATCOLL.OS.PROCESS'ELAB_SPEC;
      end if;
      E442 := E442 + 1;
      E293 := E293 + 1;
      if E289 = 0 then
         GNATCOLL.STRINGS'ELAB_SPEC;
      end if;
      if E289 = 0 then
         GNATCOLL.STRINGS'ELAB_BODY;
      end if;
      E289 := E289 + 1;
      if E801 = 0 then
         GNATCOLL.JSON'ELAB_SPEC;
      end if;
      E803 := E803 + 1;
      if E801 = 0 then
         GNATCOLL.JSON'ELAB_BODY;
      end if;
      E801 := E801 + 1;
      if E307 = 0 then
         GNATCOLL.MMAP'ELAB_SPEC;
      end if;
      E309 := E309 + 1;
      E307 := E307 + 1;
      if E451 = 0 then
         GNATCOLL.TEMPLATES'ELAB_SPEC;
      end if;
      E451 := E451 + 1;
      if E453 = 0 then
         GNATCOLL.TERMINAL'ELAB_SPEC;
      end if;
      if E453 = 0 then
         GNATCOLL.TERMINAL'ELAB_BODY;
      end if;
      E453 := E453 + 1;
      E331 := E331 + 1;
      E210 := E210 + 1;
      if E287 = 0 then
         GNATCOLL.IO'ELAB_SPEC;
      end if;
      if E287 = 0 then
         GNATCOLL.IO'ELAB_BODY;
      end if;
      E287 := E287 + 1;
      if E311 = 0 then
         GNATCOLL.PATH'ELAB_SPEC;
      end if;
      if E311 = 0 then
         GNATCOLL.PATH'ELAB_BODY;
      end if;
      E311 := E311 + 1;
      if E305 = 0 then
         GNATCOLL.IO.NATIVE'ELAB_SPEC;
      end if;
      if E305 = 0 then
         GNATCOLL.IO.NATIVE'ELAB_BODY;
      end if;
      E305 := E305 + 1;
      if E314 = 0 then
         GNATCOLL.REMOTE'ELAB_SPEC;
      end if;
      E314 := E314 + 1;
      if E318 = 0 then
         GNATCOLL.REMOTE.DB'ELAB_SPEC;
      end if;
      E318 := E318 + 1;
      if E301 = 0 then
         GNATCOLL.IO.REMOTE'ELAB_SPEC;
      end if;
      E313 := E313 + 1;
      E316 := E316 + 1;
      if E301 = 0 then
         GNATCOLL.IO.REMOTE'ELAB_BODY;
      end if;
      E301 := E301 + 1;
      if E283 = 0 then
         GNATCOLL.VFS'ELAB_SPEC;
      end if;
      if E283 = 0 then
         GNATCOLL.VFS'ELAB_BODY;
      end if;
      E283 := E283 + 1;
      if E449 = 0 then
         GNATCOLL.TRACES'ELAB_SPEC;
      end if;
      if E449 = 0 then
         GNATCOLL.TRACES'ELAB_BODY;
      end if;
      E449 := E449 + 1;
      if E654 = 0 then
         GNATCOLL.ICONV'ELAB_SPEC;
      end if;
      if E654 = 0 then
         GNATCOLL.ICONV'ELAB_BODY;
      end if;
      E654 := E654 + 1;
      if E461 = 0 then
         GNATCOLL.VFS_UTILS'ELAB_SPEC;
      end if;
      E461 := E461 + 1;
      if E139 = 0 then
         GPR2'ELAB_SPEC;
      end if;
      if E139 = 0 then
         GPR2'ELAB_BODY;
      end if;
      E139 := E139 + 1;
      E344 := E344 + 1;
      if E275 = 0 then
         GPR2.PATH_NAME'ELAB_SPEC;
      end if;
      if E275 = 0 then
         GPR2.PATH_NAME'ELAB_BODY;
      end if;
      E275 := E275 + 1;
      if E335 = 0 then
         GPR2.PATH_NAME.SET'ELAB_SPEC;
      end if;
      E335 := E335 + 1;
      if E273 = 0 then
         GPR2.SOURCE_REFERENCE'ELAB_SPEC;
      end if;
      if E273 = 0 then
         GPR2.SOURCE_REFERENCE'ELAB_BODY;
      end if;
      E273 := E273 + 1;
      if E390 = 0 then
         GPR2.MESSAGE'ELAB_SPEC;
      end if;
      if E390 = 0 then
         GPR2.MESSAGE'ELAB_BODY;
      end if;
      E390 := E390 + 1;
      if E354 = 0 then
         GPR2.SOURCE_REFERENCE.ATTRIBUTE'ELAB_SPEC;
      end if;
      E354 := E354 + 1;
      if E607 = 0 then
         GPR2.SOURCE_REFERENCE.PACK'ELAB_SPEC;
      end if;
      E607 := E607 + 1;
      E325 := E325 + 1;
      if E603 = 0 then
         GPR2.SOURCE_REFERENCE.IDENTIFIER'ELAB_SPEC;
      end if;
      E603 := E603 + 1;
      if E776 = 0 then
         GPR2.SOURCE_REFERENCE.IDENTIFIER.SET'ELAB_SPEC;
      end if;
      E776 := E776 + 1;
      if E323 = 0 then
         GPR2.SOURCE_REFERENCE.VALUE'ELAB_SPEC;
      end if;
      E323 := E323 + 1;
      if E268 = 0 then
         GPR2.CONTAINERS'ELAB_SPEC;
      end if;
      E268 := E268 + 1;
      if E257 = 0 then
         GPR2.CONTEXT'ELAB_SPEC;
      end if;
      if E257 = 0 then
         GPR2.CONTEXT'ELAB_BODY;
      end if;
      E257 := E257 + 1;
      E594 := E594 + 1;
      if E578 = 0 then
         GPR2.LOG'ELAB_SPEC;
      end if;
      if E578 = 0 then
         GPR2.LOG'ELAB_BODY;
      end if;
      E578 := E578 + 1;
      if E327 = 0 then
         GPR2.PROJECT'ELAB_SPEC;
      end if;
      E327 := E327 + 1;
      if E357 = 0 then
         GPR2.PROJECT.ATTRIBUTE_INDEX'ELAB_SPEC;
      end if;
      if E357 = 0 then
         GPR2.PROJECT.ATTRIBUTE_INDEX'ELAB_BODY;
      end if;
      E357 := E357 + 1;
      if E752 = 0 then
         GPR2.PROJECT.IMPORT'ELAB_SPEC;
      end if;
      if E752 = 0 then
         GPR2.PROJECT.IMPORT'ELAB_BODY;
      end if;
      E752 := E752 + 1;
      if E754 = 0 then
         GPR2.PROJECT.IMPORT.SET'ELAB_SPEC;
      end if;
      if E754 = 0 then
         GPR2.PROJECT.IMPORT.SET'ELAB_BODY;
      end if;
      E754 := E754 + 1;
      if E436 = 0 then
         GPR2.PROJECT.REGISTRY.PACK'ELAB_SPEC;
      end if;
      if E436 = 0 then
         GPR2.PROJECT.REGISTRY.PACK'ELAB_BODY;
      end if;
      E436 := E436 + 1;
      if E775 = 0 then
         GPR2.UNIT'ELAB_SPEC;
      end if;
      if E775 = 0 then
         GPR2.UNIT'ELAB_BODY;
      end if;
      E775 := E775 + 1;
      if E773 = 0 then
         GPR2.PROJECT.UNIT_INFO'ELAB_SPEC;
      end if;
      if E773 = 0 then
         GPR2.PROJECT.UNIT_INFO'ELAB_BODY;
      end if;
      E773 := E773 + 1;
      if E778 = 0 then
         GPR2.PROJECT.UNIT_INFO.SET'ELAB_SPEC;
      end if;
      E778 := E778 + 1;
      if E784 = 0 then
         GPR2.UNIT.LIST'ELAB_SPEC;
      end if;
      if E784 = 0 then
         GPR2.UNIT.LIST'ELAB_BODY;
      end if;
      E784 := E784 + 1;
      if E782 = 0 then
         GPR2.SOURCE_INFO'ELAB_SPEC;
      end if;
      if E782 = 0 then
         GPR2.SOURCE_INFO'ELAB_BODY;
      end if;
      E782 := E782 + 1;
      if E780 = 0 then
         GPR2.SOURCE'ELAB_SPEC;
      end if;
      if E780 = 0 then
         GPR2.SOURCE'ELAB_BODY;
      end if;
      E780 := E780 + 1;
      if E786 = 0 then
         GPR2.SOURCE_INFO.PARSER'ELAB_SPEC;
      end if;
      E786 := E786 + 1;
      if E582 = 0 then
         GPR2.VIEW_IDS'ELAB_SPEC;
      end if;
      E582 := E582 + 1;
      if E760 = 0 then
         GPR2.VIEW_IDS.SET'ELAB_SPEC;
      end if;
      E760 := E760 + 1;
      if E761 = 0 then
         GPR2.VIEW_IDS.VECTOR'ELAB_SPEC;
      end if;
      E761 := E761 + 1;
      if E798 = 0 then
         GPR2.VIEW_IDS.DAGS'ELAB_SPEC;
      end if;
      if E798 = 0 then
         GPR2.VIEW_IDS.DAGS'ELAB_BODY;
      end if;
      E798 := E798 + 1;
      if E714 = 0 then
         Gpr_Parser_Support.Adalog'Elab_Spec;
      end if;
      E714 := E714 + 1;
      E716 := E716 + 1;
      E718 := E718 + 1;
      E724 := E724 + 1;
      E726 := E726 + 1;
      E693 := E693 + 1;
      E722 := E722 + 1;
      E742 := E742 + 1;
      E732 := E732 + 1;
      if E647 = 0 then
         Gpr_Parser_Support.Text'Elab_Spec;
      end if;
      E647 := E647 + 1;
      if E707 = 0 then
         Gpr_Parser_Support.Names'Elab_Spec;
      end if;
      E707 := E707 + 1;
      E641 := E641 + 1;
      if E639 = 0 then
         Gpr_Parser_Support.Diagnostics'Elab_Spec;
      end if;
      E639 := E639 + 1;
      if E685 = 0 then
         Gpr_Parser_Support.File_Readers'Elab_Spec;
      end if;
      E685 := E685 + 1;
      if E763 = 0 then
         GPR2.FILE_READERS'ELAB_SPEC;
      end if;
      if E763 = 0 then
         GPR2.FILE_READERS'ELAB_BODY;
      end if;
      E763 := E763 + 1;
      E697 := E697 + 1;
      E720 := E720 + 1;
      E728 := E728 + 1;
      E746 := E746 + 1;
      if E744 = 0 then
         Gpr_Parser_Support.Bump_Ptr'Elab_Spec;
      end if;
      E744 := E744 + 1;
      if E694 = 0 then
         Gpr_Parser_Support.Lexical_Envs'Elab_Spec;
      end if;
      E694 := E694 + 1;
      if E701 = 0 then
         Gpr_Parser_Support.Symbols'Elab_Spec;
      end if;
      E701 := E701 + 1;
      E730 := E730 + 1;
      E734 := E734 + 1;
      if E699 = 0 then
         Gpr_Parser_Support.Token_Data_Handlers'Elab_Spec;
      end if;
      E699 := E699 + 1;
      if E680 = 0 then
         Gpr_Parser_Support.Generic_Api'Elab_Spec;
      end if;
      if E705 = 0 then
         Gpr_Parser_Support.Generic_Api.Analysis'Elab_Spec;
      end if;
      if E689 = 0 then
         Gpr_Parser_Support.Generic_Api.Introspection'Elab_Spec;
      end if;
      if E709 = 0 then
         Gpr_Parser_Support.Internal.Introspection'Elab_Spec;
      end if;
      E680 := E680 + 1;
      if E705 = 0 then
         Gpr_Parser_Support.Generic_Api.Analysis'Elab_Body;
      end if;
      E705 := E705 + 1;
      if E689 = 0 then
         Gpr_Parser_Support.Generic_Api.Introspection'Elab_Body;
      end if;
      E689 := E689 + 1;
      E691 := E691 + 1;
      if E709 = 0 then
         Gpr_Parser_Support.Internal.Introspection'Elab_Body;
      end if;
      E709 := E709 + 1;
      E738 := E738 + 1;
      if E740 = 0 then
         Gpr_Parser.Parsers'Elab_Spec;
      end if;
      if E672 = 0 then
         Gpr_Parser.Implementation'Elab_Spec;
      end if;
      E750 := E750 + 1;
      if E660 = 0 then
         Gpr_Parser.Analysis'Elab_Spec;
      end if;
      if E670 = 0 then
         Gpr_Parser.Generic_Introspection'Elab_Spec;
      end if;
      if E677 = 0 then
         Gpr_Parser.Introspection_Implementation'Elab_Spec;
      end if;
      if E677 = 0 then
         Gpr_Parser.Introspection_Implementation'Elab_Body;
      end if;
      E677 := E677 + 1;
      E736 := E736 + 1;
      if E664 = 0 then
         Gpr_Parser.Common'Elab_Body;
      end if;
      E664 := E664 + 1;
      if E668 = 0 then
         Gpr_Parser.Generic_Impl'Elab_Spec;
      end if;
      if E672 = 0 then
         Gpr_Parser.Implementation'Elab_Body;
      end if;
      E672 := E672 + 1;
      if E740 = 0 then
         Gpr_Parser.Parsers'Elab_Body;
      end if;
      E740 := E740 + 1;
      if E748 = 0 then
         Gpr_Parser.Public_Converters'Elab_Spec;
      end if;
      if E748 = 0 then
         Gpr_Parser.Public_Converters'Elab_Body;
      end if;
      E748 := E748 + 1;
      if E660 = 0 then
         Gpr_Parser.Analysis'Elab_Body;
      end if;
      E660 := E660 + 1;
      E666 := E666 + 1;
      E668 := E668 + 1;
      if E670 = 0 then
         Gpr_Parser.Generic_Introspection'Elab_Body;
      end if;
      E670 := E670 + 1;
      E489 := E489 + 1;
      if E493 = 0 then
         Sax.Pointers'Elab_Spec;
      end if;
      if E493 = 0 then
         Sax.Pointers'Elab_Body;
      end if;
      E493 := E493 + 1;
      E572 := E572 + 1;
      if E551 = 0 then
         Schema'Elab_Spec;
      end if;
      E551 := E551 + 1;
      if E485 = 0 then
         Unicode.Ccs'Elab_Spec;
      end if;
      E485 := E485 + 1;
      E509 := E509 + 1;
      E511 := E511 + 1;
      E516 := E516 + 1;
      E519 := E519 + 1;
      E521 := E521 + 1;
      E523 := E523 + 1;
      E528 := E528 + 1;
      if E481 = 0 then
         Unicode.Ces'Elab_Spec;
      end if;
      E481 := E481 + 1;
      if E491 = 0 then
         Sax.Symbols'Elab_Spec;
      end if;
      if E491 = 0 then
         Sax.Symbols'Elab_Body;
      end if;
      E491 := E491 + 1;
      E549 := E549 + 1;
      if E547 = 0 then
         Sax.Exceptions'Elab_Spec;
      end if;
      if E547 = 0 then
         Sax.Exceptions'Elab_Body;
      end if;
      E547 := E547 + 1;
      E483 := E483 + 1;
      E531 := E531 + 1;
      E533 := E533 + 1;
      E487 := E487 + 1;
      if E545 = 0 then
         Sax.Models'Elab_Spec;
      end if;
      E545 := E545 + 1;
      if E543 = 0 then
         Sax.Attributes'Elab_Spec;
      end if;
      if E543 = 0 then
         Sax.Attributes'Elab_Body;
      end if;
      E543 := E543 + 1;
      if E495 = 0 then
         Sax.Utils'Elab_Spec;
      end if;
      if E495 = 0 then
         Sax.Utils'Elab_Body;
      end if;
      E495 := E495 + 1;
      if E468 = 0 then
         DOM.CORE'ELAB_SPEC;
      end if;
      E468 := E468 + 1;
      if E563 = 0 then
         Schema.Date_Time'Elab_Spec;
      end if;
      E563 := E563 + 1;
      E565 := E565 + 1;
      if E561 = 0 then
         Schema.Simple_Types'Elab_Spec;
      end if;
      E561 := E561 + 1;
      E507 := E507 + 1;
      E503 := E503 + 1;
      E501 := E501 + 1;
      E555 := E555 + 1;
      E499 := E499 + 1;
      E497 := E497 + 1;
      if E535 = 0 then
         Input_Sources'Elab_Spec;
      end if;
      if E535 = 0 then
         Input_Sources'Elab_Body;
      end if;
      E535 := E535 + 1;
      if E537 = 0 then
         Input_Sources.File'Elab_Spec;
      end if;
      if E537 = 0 then
         Input_Sources.File'Elab_Body;
      end if;
      E537 := E537 + 1;
      if E539 = 0 then
         Input_Sources.Strings'Elab_Spec;
      end if;
      if E539 = 0 then
         Input_Sources.Strings'Elab_Body;
      end if;
      E539 := E539 + 1;
      if E541 = 0 then
         Sax.Readers'Elab_Spec;
      end if;
      if E541 = 0 then
         Sax.Readers'Elab_Body;
      end if;
      E541 := E541 + 1;
      if E570 = 0 then
         Schema.Validators'Elab_Spec;
      end if;
      if E557 = 0 then
         Schema.Readers'Elab_Spec;
      end if;
      if E559 = 0 then
         Schema.Schema_Readers'Elab_Spec;
      end if;
      if E559 = 0 then
         Schema.Schema_Readers'Elab_Body;
      end if;
      E559 := E559 + 1;
      if E557 = 0 then
         Schema.Readers'Elab_Body;
      end if;
      E557 := E557 + 1;
      E574 := E574 + 1;
      if E570 = 0 then
         Schema.Validators'Elab_Body;
      end if;
      E570 := E570 + 1;
      if E553 = 0 then
         Schema.Dom_Readers'Elab_Spec;
      end if;
      if E553 = 0 then
         Schema.Dom_Readers'Elab_Body;
      end if;
      E553 := E553 + 1;
      if E340 = 0 then
         GPR2.PROJECT.REGISTRY.ATTRIBUTE'ELAB_SPEC;
      end if;
      if E353 = 0 then
         GPR2.PROJECT.ATTR_VALUES'ELAB_SPEC;
      end if;
      if E353 = 0 then
         GPR2.PROJECT.ATTR_VALUES'ELAB_BODY;
      end if;
      E353 := E353 + 1;
      if E351 = 0 then
         GPR2.PROJECT.ATTRIBUTE'ELAB_SPEC;
      end if;
      if E351 = 0 then
         GPR2.PROJECT.ATTRIBUTE'ELAB_BODY;
      end if;
      E351 := E351 + 1;
      if E596 = 0 then
         GPR2.PROJECT.ATTRIBUTE.SET'ELAB_SPEC;
      end if;
      if E596 = 0 then
         GPR2.PROJECT.ATTRIBUTE.SET'ELAB_BODY;
      end if;
      E596 := E596 + 1;
      if E346 = 0 then
         GPR2.PROJECT.ATTRIBUTE_CACHE'ELAB_SPEC;
      end if;
      if E346 = 0 then
         GPR2.PROJECT.ATTRIBUTE_CACHE'ELAB_BODY;
      end if;
      E346 := E346 + 1;
      if E601 = 0 then
         GPR2.PROJECT.NAME_VALUES'ELAB_SPEC;
      end if;
      if E601 = 0 then
         GPR2.PROJECT.NAME_VALUES'ELAB_BODY;
      end if;
      E601 := E601 + 1;
      if E605 = 0 then
         GPR2.PROJECT.TYP'ELAB_SPEC;
      end if;
      if E605 = 0 then
         GPR2.PROJECT.TYP'ELAB_BODY;
      end if;
      E605 := E605 + 1;
      if E755 = 0 then
         GPR2.PROJECT.TYP.SET'ELAB_SPEC;
      end if;
      E755 := E755 + 1;
      if E599 = 0 then
         GPR2.PROJECT.VARIABLE'ELAB_SPEC;
      end if;
      if E599 = 0 then
         GPR2.PROJECT.VARIABLE'ELAB_BODY;
      end if;
      E599 := E599 + 1;
      if E606 = 0 then
         GPR2.PROJECT.VARIABLE.SET'ELAB_SPEC;
      end if;
      E606 := E606 + 1;
      if E597 = 0 then
         GPR2.PROJECT.PACK'ELAB_SPEC;
      end if;
      E597 := E597 + 1;
      if E342 = 0 then
         GPR2.PROJECT.VIEW'ELAB_SPEC;
      end if;
      if E584 = 0 then
         GPR2.PROJECT.PARSER'ELAB_SPEC;
      end if;
      if E580 = 0 then
         GPR2.PROJECT.CONFIGURATION'ELAB_SPEC;
      end if;
      if E440 = 0 then
         GPR2.KB'ELAB_SPEC;
      end if;
      if E463 = 0 then
         GPR2.KB.COMPILER_ITERATOR'ELAB_SPEC;
      end if;
      if E463 = 0 then
         GPR2.KB.COMPILER_ITERATOR'ELAB_BODY;
      end if;
      E463 := E463 + 1;
      if E465 = 0 then
         GPR2.KB.PARSING'ELAB_BODY;
      end if;
      E465 := E465 + 1;
      if E440 = 0 then
         GPR2.KB'ELAB_BODY;
      end if;
      E440 := E440 + 1;
      E757 := E757 + 1;
      if E609 = 0 then
         GPR2.PROJECT.PARSER.REGISTRY'ELAB_BODY;
      end if;
      E609 := E609 + 1;
      if E799 = 0 then
         GPR2.PROJECT.PARSER.SET'ELAB_SPEC;
      end if;
      E799 := E799 + 1;
      if E340 = 0 then
         GPR2.PROJECT.REGISTRY.ATTRIBUTE'ELAB_BODY;
      end if;
      E340 := E340 + 1;
      if E795 = 0 then
         GPR2.PROJECT.VIEW.SET'ELAB_SPEC;
      end if;
      E795 := E795 + 1;
      if E765 = 0 then
         GPR2.PROJECT.SOURCE'ELAB_SPEC;
      end if;
      if E767 = 0 then
         GPR2.PROJECT.SOURCE.ARTIFACT'ELAB_SPEC;
      end if;
      if E769 = 0 then
         GPR2.PROJECT.SOURCE.PART_SET'ELAB_SPEC;
      end if;
      if E769 = 0 then
         GPR2.PROJECT.SOURCE.PART_SET'ELAB_BODY;
      end if;
      E769 := E769 + 1;
      if E771 = 0 then
         GPR2.PROJECT.SOURCE.SET'ELAB_SPEC;
      end if;
      if E771 = 0 then
         GPR2.PROJECT.SOURCE.SET'ELAB_BODY;
      end if;
      E771 := E771 + 1;
      if E359 = 0 then
         GPR2.PROJECT.DEFINITION'ELAB_SPEC;
      end if;
      if E580 = 0 then
         GPR2.PROJECT.CONFIGURATION'ELAB_BODY;
      end if;
      E580 := E580 + 1;
      if E796 = 0 then
         GPR2.PROJECT.VIEW.VECTOR'ELAB_SPEC;
      end if;
      E796 := E796 + 1;
      if E438 = 0 then
         GPR2.PROJECT.TREE'ELAB_SPEC;
      end if;
      if E584 = 0 then
         GPR2.PROJECT.PARSER'ELAB_BODY;
      end if;
      E584 := E584 + 1;
      if E767 = 0 then
         GPR2.PROJECT.SOURCE.ARTIFACT'ELAB_BODY;
      end if;
      E767 := E767 + 1;
      if E759 = 0 then
         GPR2.PROJECT.TREE.VIEW_BUILDER'ELAB_SPEC;
      end if;
      if E759 = 0 then
         GPR2.PROJECT.TREE.VIEW_BUILDER'ELAB_BODY;
      end if;
      E759 := E759 + 1;
      if E438 = 0 then
         GPR2.PROJECT.TREE'ELAB_BODY;
      end if;
      E438 := E438 + 1;
      if E342 = 0 then
         GPR2.PROJECT.VIEW'ELAB_BODY;
      end if;
      E342 := E342 + 1;
      if E790 = 0 then
         GPR2.SOURCE_INFO.PARSER.ADA_LANGUAGE'ELAB_SPEC;
      end if;
      if E792 = 0 then
         GPR2.SOURCE_INFO.PARSER.ALI'ELAB_SPEC;
      end if;
      if E794 = 0 then
         GPR2.SOURCE_INFO.PARSER.D'ELAB_SPEC;
      end if;
      if E788 = 0 then
         GPR2.SOURCE_INFO.PARSER.REGISTRY'ELAB_BODY;
      end if;
      E788 := E788 + 1;
      if E359 = 0 then
         GPR2.PROJECT.DEFINITION'ELAB_BODY;
      end if;
      E359 := E359 + 1;
      if E765 = 0 then
         GPR2.PROJECT.SOURCE'ELAB_BODY;
      end if;
      E765 := E765 + 1;
      if E790 = 0 then
         GPR2.SOURCE_INFO.PARSER.ADA_LANGUAGE'ELAB_BODY;
      end if;
      E790 := E790 + 1;
      if E792 = 0 then
         GPR2.SOURCE_INFO.PARSER.ALI'ELAB_BODY;
      end if;
      E792 := E792 + 1;
      if E794 = 0 then
         GPR2.SOURCE_INFO.PARSER.D'ELAB_BODY;
      end if;
      E794 := E794 + 1;
      if E014 = 0 then
         GPR2.C'ELAB_SPEC;
      end if;
      if E010 = 0 then
         GPR2.C.UTILS'ELAB_SPEC;
      end if;
      E010 := E010 + 1;
      if E004 = 0 then
         GPR2.C.JSON'ELAB_SPEC;
      end if;
      E004 := E004 + 1;
      E002 := E002 + 1;
      E006 := E006 + 1;
      E008 := E008 + 1;
      E012 := E012 + 1;
      E014 := E014 + 1;
   end gpr2cinit;

--  BEGIN Object file/option list
   --   /workspaces/bench-source/src/libgpr2/bindings/c/build/release/obj/gpr2-c-utils.o
   --   /workspaces/bench-source/src/libgpr2/bindings/c/build/release/obj/gpr2-c-json.o
   --   /workspaces/bench-source/src/libgpr2/bindings/c/build/release/obj/gpr2-c-json-encoders.o
   --   /workspaces/bench-source/src/libgpr2/bindings/c/build/release/obj/gpr2-c-source.o
   --   /workspaces/bench-source/src/libgpr2/bindings/c/build/release/obj/gpr2-c-tree.o
   --   /workspaces/bench-source/src/libgpr2/bindings/c/build/release/obj/gpr2-c-view.o
   --   /workspaces/bench-source/src/libgpr2/bindings/c/build/release/obj/gpr2-c.o
   --   -L/workspaces/bench-source/src/libgpr2/bindings/c/build/release/obj/
   --   -L/workspaces/bench-source/src/gnatcoll-bindings/gmp/lib/static-pic/
   --   -L/workspaces/bench-source/src/gnatcoll-bindings/iconv/lib/static-pic/
   --   -L/workspaces/bench-source/src/xmlada/schema/lib/static-pic/
   --   -L/workspaces/bench-source/src/xmlada/dom/lib/static-pic/
   --   -L/workspaces/bench-source/src/xmlada/input_sources/lib/static-pic/
   --   -L/workspaces/bench-source/src/xmlada/sax/lib/static-pic/
   --   -L/workspaces/bench-source/src/xmlada/unicode/lib/static-pic/
   --   -L/workspaces/bench-source/src/gprbuild/gpr/lib/production/static-pic/
   --   -L/workspaces/bench-source/src/gnatcoll-core/lib/gnatcoll/static-pic/
   --   -L/workspaces/bench-source/src/libgpr2/.build/release/lib-static-pic/
   --   -L/usr/gnat/lib/gcc/x86_64-pc-linux-gnu/12.2.1/adalib/
   --   -static
   --   -lgnarl
   --   -lgnat
   --   -lrt
   --   -lpthread
   --   -ldl
--  END Object file/option list   

end gpr2cmain;
