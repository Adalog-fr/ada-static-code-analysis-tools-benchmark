pragma Warnings (Off);
pragma Ada_95;
pragma Source_File_Name (ada_main, Spec_File_Name => "b__launch_full_demo.ads");
pragma Source_File_Name (ada_main, Body_File_Name => "b__launch_full_demo.adb");
pragma Suppress (Overflow_Check);

with System.Restrictions;
with Ada.Exceptions;

package body ada_main is

   E079 : Short_Integer; pragma Import (Ada, E079, "system__os_lib_E");
   E016 : Short_Integer; pragma Import (Ada, E016, "ada__exceptions_E");
   E012 : Short_Integer; pragma Import (Ada, E012, "system__soft_links_E");
   E010 : Short_Integer; pragma Import (Ada, E010, "system__exception_table_E");
   E045 : Short_Integer; pragma Import (Ada, E045, "ada__containers_E");
   E074 : Short_Integer; pragma Import (Ada, E074, "ada__io_exceptions_E");
   E025 : Short_Integer; pragma Import (Ada, E025, "ada__numerics_E");
   E007 : Short_Integer; pragma Import (Ada, E007, "ada__strings_E");
   E063 : Short_Integer; pragma Import (Ada, E063, "ada__strings__maps_E");
   E066 : Short_Integer; pragma Import (Ada, E066, "ada__strings__maps__constants_E");
   E050 : Short_Integer; pragma Import (Ada, E050, "interfaces__c_E");
   E019 : Short_Integer; pragma Import (Ada, E019, "system__exceptions_E");
   E088 : Short_Integer; pragma Import (Ada, E088, "system__object_reader_E");
   E057 : Short_Integer; pragma Import (Ada, E057, "system__dwarf_lines_E");
   E102 : Short_Integer; pragma Import (Ada, E102, "system__soft_links__initialize_E");
   E044 : Short_Integer; pragma Import (Ada, E044, "system__traceback__symbolic_E");
   E024 : Short_Integer; pragma Import (Ada, E024, "system__img_int_E");
   E069 : Short_Integer; pragma Import (Ada, E069, "system__img_uns_E");
   E711 : Short_Integer; pragma Import (Ada, E711, "ada__assertions_E");
   E106 : Short_Integer; pragma Import (Ada, E106, "ada__strings__utf_encoding_E");
   E114 : Short_Integer; pragma Import (Ada, E114, "ada__tags_E");
   E005 : Short_Integer; pragma Import (Ada, E005, "ada__strings__text_buffers_E");
   E337 : Short_Integer; pragma Import (Ada, E337, "gnat_E");
   E336 : Short_Integer; pragma Import (Ada, E336, "interfaces__c__strings_E");
   E169 : Short_Integer; pragma Import (Ada, E169, "ada__streams_E");
   E246 : Short_Integer; pragma Import (Ada, E246, "system__file_control_block_E");
   E245 : Short_Integer; pragma Import (Ada, E245, "system__finalization_root_E");
   E243 : Short_Integer; pragma Import (Ada, E243, "ada__finalization_E");
   E242 : Short_Integer; pragma Import (Ada, E242, "system__file_io_E");
   E440 : Short_Integer; pragma Import (Ada, E440, "ada__streams__stream_io_E");
   E256 : Short_Integer; pragma Import (Ada, E256, "system__storage_pools_E");
   E260 : Short_Integer; pragma Import (Ada, E260, "system__finalization_masters_E");
   E258 : Short_Integer; pragma Import (Ada, E258, "system__storage_pools__subpools_E");
   E446 : Short_Integer; pragma Import (Ada, E446, "ada__strings__unbounded_E");
   E219 : Short_Integer; pragma Import (Ada, E219, "system__task_info_E");
   E236 : Short_Integer; pragma Import (Ada, E236, "ada__calendar_E");
   E234 : Short_Integer; pragma Import (Ada, E234, "ada__calendar__delays_E");
   E746 : Short_Integer; pragma Import (Ada, E746, "ada__calendar__time_zones_E");
   E238 : Short_Integer; pragma Import (Ada, E238, "ada__text_io_E");
   E272 : Short_Integer; pragma Import (Ada, E272, "system__pool_global_E");
   E472 : Short_Integer; pragma Import (Ada, E472, "system__random_seed_E");
   E677 : Short_Integer; pragma Import (Ada, E677, "system__regexp_E");
   E670 : Short_Integer; pragma Import (Ada, E670, "ada__directories_E");
   E308 : Short_Integer; pragma Import (Ada, E308, "system__rpc_E");
   E306 : Short_Integer; pragma Import (Ada, E306, "system__partition_interface_E");
   E786 : Short_Integer; pragma Import (Ada, E786, "system__img_llli_E");
   E226 : Short_Integer; pragma Import (Ada, E226, "system__img_lli_E");
   E211 : Short_Integer; pragma Import (Ada, E211, "system__task_primitives__operations_E");
   E287 : Short_Integer; pragma Import (Ada, E287, "ada__real_time_E");
   E390 : Short_Integer; pragma Import (Ada, E390, "system__img_llu_E");
   E323 : Short_Integer; pragma Import (Ada, E323, "system__tasking__initialization_E");
   E200 : Short_Integer; pragma Import (Ada, E200, "system__tasking__protected_objects_E");
   E329 : Short_Integer; pragma Import (Ada, E329, "system__tasking__protected_objects__entries_E");
   E327 : Short_Integer; pragma Import (Ada, E327, "system__tasking__queuing_E");
   E349 : Short_Integer; pragma Import (Ada, E349, "system__tasking__stages_E");
   E122 : Short_Integer; pragma Import (Ada, E122, "float_math_E");
   E543 : Short_Integer; pragma Import (Ada, E543, "box2d_physics_E");
   E167 : Short_Integer; pragma Import (Ada, E167, "float_math__geometry_E");
   E186 : Short_Integer; pragma Import (Ada, E186, "float_math__geometry__d2_E");
   E575 : Short_Integer; pragma Import (Ada, E575, "bullet_physics_E");
   E362 : Short_Integer; pragma Import (Ada, E362, "freetype_E");
   E264 : Short_Integer; pragma Import (Ada, E264, "lace__any_E");
   E296 : Short_Integer; pragma Import (Ada, E296, "lace__event_E");
   E196 : Short_Integer; pragma Import (Ada, E196, "opengl_E");
   E230 : Short_Integer; pragma Import (Ada, E230, "physics_E");
   E120 : Short_Integer; pragma Import (Ada, E120, "gel_E");
   E727 : Short_Integer; pragma Import (Ada, E727, "sdl__log_E");
   E705 : Short_Integer; pragma Import (Ada, E705, "sdl__video_E");
   E718 : Short_Integer; pragma Import (Ada, E718, "sdl__video__palettes_E");
   E716 : Short_Integer; pragma Import (Ada, E716, "sdl__video__pixel_formats_E");
   E734 : Short_Integer; pragma Import (Ada, E734, "sdl__video__pixels_E");
   E720 : Short_Integer; pragma Import (Ada, E720, "sdl__video__rectangles_E");
   E722 : Short_Integer; pragma Import (Ada, E722, "sdl__video__surfaces_E");
   E733 : Short_Integer; pragma Import (Ada, E733, "sdl__video__textures_E");
   E707 : Short_Integer; pragma Import (Ada, E707, "sdl__video__windows_E");
   E695 : Short_Integer; pragma Import (Ada, E695, "sdl__events__events_E");
   E731 : Short_Integer; pragma Import (Ada, E731, "sdl__video__gl_E");
   E772 : Short_Integer; pragma Import (Ada, E772, "any_math__any_geometry__any_d3__any_modeller_E");
   E776 : Short_Integer; pragma Import (Ada, E776, "any_math__any_geometry__any_d3__any_modeller__any_forge_E");
   E566 : Short_Integer; pragma Import (Ada, E566, "c_math_c__conversion_E");
   E740 : Short_Integer; pragma Import (Ada, E740, "collada_E");
   E749 : Short_Integer; pragma Import (Ada, E749, "collada__library_E");
   E751 : Short_Integer; pragma Import (Ada, E751, "collada__library__animations_E");
   E753 : Short_Integer; pragma Import (Ada, E753, "collada__library__controllers_E");
   E755 : Short_Integer; pragma Import (Ada, E755, "collada__library__geometries_E");
   E757 : Short_Integer; pragma Import (Ada, E757, "collada__library__visual_scenes_E");
   E769 : Short_Integer; pragma Import (Ada, E769, "float_math__geometry__d3__modeller_E");
   E366 : Short_Integer; pragma Import (Ada, E366, "freetype_c__binding_E");
   E381 : Short_Integer; pragma Import (Ada, E381, "freetype__face_size_E");
   E379 : Short_Integer; pragma Import (Ada, E379, "freetype__charmap_E");
   E364 : Short_Integer; pragma Import (Ada, E364, "freetype__face_E");
   E404 : Short_Integer; pragma Import (Ada, E404, "gid_E");
   E408 : Short_Integer; pragma Import (Ada, E408, "gid__buffering_E");
   E412 : Short_Integer; pragma Import (Ada, E412, "gid__color_tables_E");
   E406 : Short_Integer; pragma Import (Ada, E406, "gid__decoding_bmp_E");
   E410 : Short_Integer; pragma Import (Ada, E410, "gid__decoding_gif_E");
   E414 : Short_Integer; pragma Import (Ada, E414, "gid__decoding_jpg_E");
   E416 : Short_Integer; pragma Import (Ada, E416, "gid__decoding_png_E");
   E418 : Short_Integer; pragma Import (Ada, E418, "gid__decoding_png__huffman_E");
   E423 : Short_Integer; pragma Import (Ada, E423, "gid__decoding_tga_E");
   E425 : Short_Integer; pragma Import (Ada, E425, "gid__headers_E");
   E334 : Short_Integer; pragma Import (Ada, E334, "gl__lean_E");
   E398 : Short_Integer; pragma Import (Ada, E398, "gl__pointers_E");
   E436 : Short_Integer; pragma Import (Ada, E436, "gl__safe_E");
   E303 : Short_Integer; pragma Import (Ada, E303, "lace__response_E");
   E301 : Short_Integer; pragma Import (Ada, E301, "lace__observer_E");
   E298 : Short_Integer; pragma Import (Ada, E298, "lace__subject_E");
   E299 : Short_Integer; pragma Import (Ada, E299, "lace__event__logger_E");
   E294 : Short_Integer; pragma Import (Ada, E294, "gel__keyboard_E");
   E290 : Short_Integer; pragma Import (Ada, E290, "gel__mouse_E");
   E594 : Short_Integer; pragma Import (Ada, E594, "lace__event__logger__text_E");
   E592 : Short_Integer; pragma Import (Ada, E592, "lace__event__utility_E");
   E598 : Short_Integer; pragma Import (Ada, E598, "lace__make_observer_E");
   E600 : Short_Integer; pragma Import (Ada, E600, "lace__make_observer__deferred_E");
   E602 : Short_Integer; pragma Import (Ada, E602, "lace__make_subject_E");
   E622 : Short_Integer; pragma Import (Ada, E622, "lace__subject__local_E");
   E620 : Short_Integer; pragma Import (Ada, E620, "gel__keyboard__local_E");
   E624 : Short_Integer; pragma Import (Ada, E624, "gel__mouse__local_E");
   E590 : Short_Integer; pragma Import (Ada, E590, "lace__subject_and_deferred_observer_E");
   E526 : Short_Integer; pragma Import (Ada, E526, "opengl__conversions_E");
   E502 : Short_Integer; pragma Import (Ada, E502, "opengl__display_E");
   E504 : Short_Integer; pragma Import (Ada, E504, "opengl__display__privvy_E");
   E355 : Short_Integer; pragma Import (Ada, E355, "opengl__frustum_E");
   E387 : Short_Integer; pragma Import (Ada, E387, "opengl__glyphimpl_E");
   E385 : Short_Integer; pragma Import (Ada, E385, "opengl__glyph_E");
   E389 : Short_Integer; pragma Import (Ada, E389, "opengl__glyph__container_E");
   E383 : Short_Integer; pragma Import (Ada, E383, "opengl__fontimpl_E");
   E361 : Short_Integer; pragma Import (Ada, E361, "opengl__font_E");
   E438 : Short_Integer; pragma Import (Ada, E438, "opengl__images_E");
   E468 : Short_Integer; pragma Import (Ada, E468, "opengl__palette_E");
   E466 : Short_Integer; pragma Import (Ada, E466, "opengl__light_E");
   E312 : Short_Integer; pragma Import (Ada, E312, "opengl__remote_model_E");
   E508 : Short_Integer; pragma Import (Ada, E508, "opengl__screen_E");
   E506 : Short_Integer; pragma Import (Ada, E506, "opengl__surface_profile_E");
   E510 : Short_Integer; pragma Import (Ada, E510, "opengl__surface_profile__privvy_E");
   E496 : Short_Integer; pragma Import (Ada, E496, "opengl__surface_E");
   E540 : Short_Integer; pragma Import (Ada, E540, "opengl__surface__privvy_E");
   E538 : Short_Integer; pragma Import (Ada, E538, "opengl__context_E");
   E317 : Short_Integer; pragma Import (Ada, E317, "opengl__tasks_E");
   E464 : Short_Integer; pragma Import (Ada, E464, "opengl__attribute_E");
   E400 : Short_Integer; pragma Import (Ada, E400, "opengl__errors_E");
   E452 : Short_Integer; pragma Import (Ada, E452, "opengl__buffer_E");
   E456 : Short_Integer; pragma Import (Ada, E456, "opengl__buffer__general_E");
   E454 : Short_Integer; pragma Import (Ada, E454, "opengl__buffer__indices_E");
   E460 : Short_Integer; pragma Import (Ada, E460, "opengl__buffer__long_indices_E");
   E804 : Short_Integer; pragma Import (Ada, E804, "opengl__buffer__short_indices_E");
   E314 : Short_Integer; pragma Import (Ada, E314, "opengl__renderer_E");
   E536 : Short_Integer; pragma Import (Ada, E536, "opengl__server_E");
   E474 : Short_Integer; pragma Import (Ada, E474, "opengl__shader_E");
   E476 : Short_Integer; pragma Import (Ada, E476, "opengl__variable_E");
   E478 : Short_Integer; pragma Import (Ada, E478, "opengl__variable__uniform_E");
   E462 : Short_Integer; pragma Import (Ada, E462, "opengl__program_E");
   E522 : Short_Integer; pragma Import (Ada, E522, "opengl__program__lit_E");
   E530 : Short_Integer; pragma Import (Ada, E530, "opengl__program__lit__colored_textured_skinned_E");
   E534 : Short_Integer; pragma Import (Ada, E534, "opengl__program__lit__textured_skinned_E");
   E444 : Short_Integer; pragma Import (Ada, E444, "opengl__viewport_E");
   E396 : Short_Integer; pragma Import (Ada, E396, "opengl__texture_E");
   E402 : Short_Integer; pragma Import (Ada, E402, "opengl__io_E");
   E767 : Short_Integer; pragma Import (Ada, E767, "opengl__io__lat_long_radius_E");
   E778 : Short_Integer; pragma Import (Ada, E778, "opengl__io__wavefront_E");
   E394 : Short_Integer; pragma Import (Ada, E394, "opengl__primitive_E");
   E450 : Short_Integer; pragma Import (Ada, E450, "opengl__primitive__indexed_E");
   E458 : Short_Integer; pragma Import (Ada, E458, "opengl__primitive__long_indexed_E");
   E392 : Short_Integer; pragma Import (Ada, E392, "opengl__geometry_E");
   E633 : Short_Integer; pragma Import (Ada, E633, "opengl__geometry__colored_E");
   E637 : Short_Integer; pragma Import (Ada, E637, "opengl__geometry__colored_textured_E");
   E651 : Short_Integer; pragma Import (Ada, E651, "opengl__geometry__lit_colored_E");
   E663 : Short_Integer; pragma Import (Ada, E663, "opengl__geometry__lit_colored_textured_E");
   E528 : Short_Integer; pragma Import (Ada, E528, "opengl__geometry__lit_colored_textured_skinned_E");
   E520 : Short_Integer; pragma Import (Ada, E520, "opengl__geometry__lit_textured_E");
   E532 : Short_Integer; pragma Import (Ada, E532, "opengl__geometry__lit_textured_skinned_E");
   E490 : Short_Integer; pragma Import (Ada, E490, "opengl__geometry__textured_E");
   E518 : Short_Integer; pragma Import (Ada, E518, "opengl__glyphimpl__texture_E");
   E516 : Short_Integer; pragma Import (Ada, E516, "opengl__glyph__texture_E");
   E514 : Short_Integer; pragma Import (Ada, E514, "opengl__fontimpl__texture_E");
   E512 : Short_Integer; pragma Import (Ada, E512, "opengl__font__texture_E");
   E359 : Short_Integer; pragma Import (Ada, E359, "opengl__model_E");
   E629 : Short_Integer; pragma Import (Ada, E629, "opengl__model__arrow_E");
   E631 : Short_Integer; pragma Import (Ada, E631, "opengl__model__arrow__colored_E");
   E486 : Short_Integer; pragma Import (Ada, E486, "opengl__model__billboard_E");
   E635 : Short_Integer; pragma Import (Ada, E635, "opengl__model__billboard__colored_textured_E");
   E488 : Short_Integer; pragma Import (Ada, E488, "opengl__model__billboard__textured_E");
   E639 : Short_Integer; pragma Import (Ada, E639, "opengl__model__box_E");
   E641 : Short_Integer; pragma Import (Ada, E641, "opengl__model__box__colored_E");
   E643 : Short_Integer; pragma Import (Ada, E643, "opengl__model__box__textured_E");
   E805 : Short_Integer; pragma Import (Ada, E805, "opengl__model__capsule_E");
   E807 : Short_Integer; pragma Import (Ada, E807, "opengl__model__capsule__textured_E");
   E644 : Short_Integer; pragma Import (Ada, E644, "opengl__model__line_E");
   E646 : Short_Integer; pragma Import (Ada, E646, "opengl__model__line__colored_E");
   E647 : Short_Integer; pragma Import (Ada, E647, "opengl__model__polygon_E");
   E649 : Short_Integer; pragma Import (Ada, E649, "opengl__model__polygon__lit_colored_E");
   E653 : Short_Integer; pragma Import (Ada, E653, "opengl__model__segment_line_E");
   E655 : Short_Integer; pragma Import (Ada, E655, "opengl__model__sphere_E");
   E657 : Short_Integer; pragma Import (Ada, E657, "opengl__model__sphere__colored_E");
   E659 : Short_Integer; pragma Import (Ada, E659, "opengl__model__sphere__lit_colored_E");
   E661 : Short_Integer; pragma Import (Ada, E661, "opengl__model__sphere__lit_colored_textured_E");
   E665 : Short_Integer; pragma Import (Ada, E665, "opengl__model__sphere__textured_E");
   E666 : Short_Integer; pragma Import (Ada, E666, "opengl__model__text_E");
   E668 : Short_Integer; pragma Import (Ada, E668, "opengl__model__text__lit_colored_E");
   E802 : Short_Integer; pragma Import (Ada, E802, "opengl__primitive__short_indexed_E");
   E811 : Short_Integer; pragma Import (Ada, E811, "opengl__texture__coordinates_E");
   E809 : Short_Integer; pragma Import (Ada, E809, "opengl__model__terrain_E");
   E357 : Short_Integer; pragma Import (Ada, E357, "opengl__visual_E");
   E484 : Short_Integer; pragma Import (Ada, E484, "opengl__impostor_E");
   E492 : Short_Integer; pragma Import (Ada, E492, "opengl__impostor__simple_E");
   E494 : Short_Integer; pragma Import (Ada, E494, "opengl__impostor__terrain_E");
   E333 : Short_Integer; pragma Import (Ada, E333, "opengl__renderer__lean_E");
   E353 : Short_Integer; pragma Import (Ada, E353, "opengl__culler_E");
   E480 : Short_Integer; pragma Import (Ada, E480, "opengl__culler__frustum_E");
   E482 : Short_Integer; pragma Import (Ada, E482, "opengl__impostorer_E");
   E341 : Short_Integer; pragma Import (Ada, E341, "opengl__camera_E");
   E616 : Short_Integer; pragma Import (Ada, E616, "opengl__renderer__lean__forge_E");
   E268 : Short_Integer; pragma Import (Ada, E268, "physics__remote__model_E");
   E311 : Short_Integer; pragma Import (Ada, E311, "gel__remote__world_E");
   E288 : Short_Integer; pragma Import (Ada, E288, "gel__events_E");
   E618 : Short_Integer; pragma Import (Ada, E618, "gel__window_E");
   E690 : Short_Integer; pragma Import (Ada, E690, "gel__window__sdl_E");
   E270 : Short_Integer; pragma Import (Ada, E270, "physics__shape_E");
   E588 : Short_Integer; pragma Import (Ada, E588, "bullet_physics__shape_E");
   E266 : Short_Integer; pragma Import (Ada, E266, "physics__model_E");
   E254 : Short_Integer; pragma Import (Ada, E254, "physics__object_E");
   E586 : Short_Integer; pragma Import (Ada, E586, "bullet_physics__object_E");
   E569 : Short_Integer; pragma Import (Ada, E569, "physics__joint_E");
   E570 : Short_Integer; pragma Import (Ada, E570, "physics__joint__ball_E");
   E571 : Short_Integer; pragma Import (Ada, E571, "physics__joint__cone_twist_E");
   E572 : Short_Integer; pragma Import (Ada, E572, "physics__joint__dof6_E");
   E573 : Short_Integer; pragma Import (Ada, E573, "physics__joint__hinge_E");
   E574 : Short_Integer; pragma Import (Ada, E574, "physics__joint__slider_E");
   E584 : Short_Integer; pragma Import (Ada, E584, "bullet_physics__joint_E");
   E568 : Short_Integer; pragma Import (Ada, E568, "physics__space_E");
   E564 : Short_Integer; pragma Import (Ada, E564, "box2d_physics__shape_E");
   E562 : Short_Integer; pragma Import (Ada, E562, "box2d_physics__object_E");
   E560 : Short_Integer; pragma Import (Ada, E560, "box2d_physics__joint_E");
   E545 : Short_Integer; pragma Import (Ada, E545, "box2d_physics__space_E");
   E577 : Short_Integer; pragma Import (Ada, E577, "bullet_physics__space_E");
   E542 : Short_Integer; pragma Import (Ada, E542, "physics__forge_E");
   E276 : Short_Integer; pragma Import (Ada, E276, "gel__joint_E");
   E250 : Short_Integer; pragma Import (Ada, E250, "gel__sprite_E");
   E252 : Short_Integer; pragma Import (Ada, E252, "gel__any_joint_E");
   E604 : Short_Integer; pragma Import (Ada, E604, "gel__hinge_joint_E");
   E278 : Short_Integer; pragma Import (Ada, E278, "gel__world_E");
   E248 : Short_Integer; pragma Import (Ada, E248, "gel__camera_E");
   E606 : Short_Integer; pragma Import (Ada, E606, "gel__camera__forge_E");
   E608 : Short_Integer; pragma Import (Ada, E608, "gel__dolly_E");
   E610 : Short_Integer; pragma Import (Ada, E610, "gel__dolly__following_E");
   E612 : Short_Integer; pragma Import (Ada, E612, "gel__dolly__simple_E");
   E681 : Short_Integer; pragma Import (Ada, E681, "gel__world__client_E");
   E687 : Short_Integer; pragma Import (Ada, E687, "gel__world__server_E");
   E614 : Short_Integer; pragma Import (Ada, E614, "gel__world__simple_E");
   E232 : Short_Integer; pragma Import (Ada, E232, "gel__applet_E");
   E679 : Short_Integer; pragma Import (Ada, E679, "gel__applet__client_world_E");
   E626 : Short_Integer; pragma Import (Ada, E626, "gel__applet__gui_and_sim_world_E");
   E683 : Short_Integer; pragma Import (Ada, E683, "gel__applet__gui_world_E");
   E685 : Short_Integer; pragma Import (Ada, E685, "gel__applet__server_world_E");
   E628 : Short_Integer; pragma Import (Ada, E628, "gel__forge_E");
   E761 : Short_Integer; pragma Import (Ada, E761, "xml_E");
   E763 : Short_Integer; pragma Import (Ada, E763, "xml__reader_E");
   E742 : Short_Integer; pragma Import (Ada, E742, "collada__document_E");
   E738 : Short_Integer; pragma Import (Ada, E738, "opengl__io__collada_E");
   E736 : Short_Integer; pragma Import (Ada, E736, "opengl__model__any_E");

   Sec_Default_Sized_Stacks : array (1 .. 1) of aliased System.Secondary_Stack.SS_Stack (System.Parameters.Runtime_Default_Sec_Stack_Size);

   Local_Priority_Specific_Dispatching : constant String := "";
   Local_Interrupt_States : constant String := "";

   Is_Elaborated : Boolean := False;

   procedure finalize_library is
   begin
      declare
         procedure F1;
         pragma Import (Ada, F1, "opengl__model__any__finalize_body");
      begin
         E736 := E736 - 1;
         F1;
      end;
      declare
         procedure F2;
         pragma Import (Ada, F2, "opengl__model__any__finalize_spec");
      begin
         F2;
      end;
      declare
         procedure F3;
         pragma Import (Ada, F3, "collada__document__finalize_body");
      begin
         E742 := E742 - 1;
         F3;
      end;
      declare
         procedure F4;
         pragma Import (Ada, F4, "collada__document__finalize_spec");
      begin
         F4;
      end;
      E761 := E761 - 1;
      declare
         procedure F5;
         pragma Import (Ada, F5, "xml__finalize_spec");
      begin
         F5;
      end;
      E685 := E685 - 1;
      declare
         procedure F6;
         pragma Import (Ada, F6, "gel__applet__server_world__finalize_spec");
      begin
         F6;
      end;
      E683 := E683 - 1;
      declare
         procedure F7;
         pragma Import (Ada, F7, "gel__applet__gui_world__finalize_spec");
      begin
         F7;
      end;
      E626 := E626 - 1;
      declare
         procedure F8;
         pragma Import (Ada, F8, "gel__applet__gui_and_sim_world__finalize_spec");
      begin
         F8;
      end;
      E679 := E679 - 1;
      declare
         procedure F9;
         pragma Import (Ada, F9, "gel__applet__client_world__finalize_spec");
      begin
         F9;
      end;
      declare
         procedure F10;
         pragma Import (Ada, F10, "gel__applet__finalize_body");
      begin
         E232 := E232 - 1;
         F10;
      end;
      declare
         procedure F11;
         pragma Import (Ada, F11, "gel__applet__finalize_spec");
      begin
         F11;
      end;
      E614 := E614 - 1;
      declare
         procedure F12;
         pragma Import (Ada, F12, "gel__world__simple__finalize_spec");
      begin
         F12;
      end;
      E687 := E687 - 1;
      declare
         procedure F13;
         pragma Import (Ada, F13, "gel__world__server__finalize_spec");
      begin
         F13;
      end;
      declare
         procedure F14;
         pragma Import (Ada, F14, "gel__world__client__finalize_body");
      begin
         E681 := E681 - 1;
         F14;
      end;
      declare
         procedure F15;
         pragma Import (Ada, F15, "gel__world__client__finalize_spec");
      begin
         F15;
      end;
      E612 := E612 - 1;
      declare
         procedure F16;
         pragma Import (Ada, F16, "gel__dolly__simple__finalize_spec");
      begin
         F16;
      end;
      E610 := E610 - 1;
      declare
         procedure F17;
         pragma Import (Ada, F17, "gel__dolly__following__finalize_spec");
      begin
         F17;
      end;
      E608 := E608 - 1;
      declare
         procedure F18;
         pragma Import (Ada, F18, "gel__dolly__finalize_spec");
      begin
         F18;
      end;
      E248 := E248 - 1;
      declare
         procedure F19;
         pragma Import (Ada, F19, "gel__camera__finalize_spec");
      begin
         F19;
      end;
      E250 := E250 - 1;
      E276 := E276 - 1;
      declare
         procedure F20;
         pragma Import (Ada, F20, "gel__world__finalize_body");
      begin
         E278 := E278 - 1;
         F20;
      end;
      declare
         procedure F21;
         pragma Import (Ada, F21, "gel__world__finalize_spec");
      begin
         F21;
      end;
      E604 := E604 - 1;
      declare
         procedure F22;
         pragma Import (Ada, F22, "gel__hinge_joint__finalize_spec");
      begin
         F22;
      end;
      E252 := E252 - 1;
      declare
         procedure F23;
         pragma Import (Ada, F23, "gel__any_joint__finalize_spec");
      begin
         F23;
      end;
      declare
         procedure F24;
         pragma Import (Ada, F24, "gel__sprite__finalize_spec");
      begin
         F24;
      end;
      declare
         procedure F25;
         pragma Import (Ada, F25, "gel__joint__finalize_spec");
      begin
         F25;
      end;
      E577 := E577 - 1;
      declare
         procedure F26;
         pragma Import (Ada, F26, "bullet_physics__space__finalize_spec");
      begin
         F26;
      end;
      declare
         procedure F27;
         pragma Import (Ada, F27, "box2d_physics__space__finalize_body");
      begin
         E545 := E545 - 1;
         F27;
      end;
      declare
         procedure F28;
         pragma Import (Ada, F28, "box2d_physics__space__finalize_spec");
      begin
         F28;
      end;
      declare
         procedure F29;
         pragma Import (Ada, F29, "box2d_physics__joint__finalize_body");
      begin
         E560 := E560 - 1;
         F29;
      end;
      declare
         procedure F30;
         pragma Import (Ada, F30, "box2d_physics__joint__finalize_spec");
      begin
         F30;
      end;
      declare
         procedure F31;
         pragma Import (Ada, F31, "box2d_physics__object__finalize_body");
      begin
         E562 := E562 - 1;
         F31;
      end;
      declare
         procedure F32;
         pragma Import (Ada, F32, "box2d_physics__object__finalize_spec");
      begin
         F32;
      end;
      E564 := E564 - 1;
      declare
         procedure F33;
         pragma Import (Ada, F33, "box2d_physics__shape__finalize_spec");
      begin
         F33;
      end;
      E568 := E568 - 1;
      declare
         procedure F34;
         pragma Import (Ada, F34, "physics__space__finalize_spec");
      begin
         F34;
      end;
      declare
         procedure F35;
         pragma Import (Ada, F35, "bullet_physics__joint__finalize_body");
      begin
         E584 := E584 - 1;
         F35;
      end;
      declare
         procedure F36;
         pragma Import (Ada, F36, "bullet_physics__joint__finalize_spec");
      begin
         F36;
      end;
      declare
         procedure F37;
         pragma Import (Ada, F37, "physics__joint__slider__finalize_spec");
      begin
         E574 := E574 - 1;
         F37;
      end;
      declare
         procedure F38;
         pragma Import (Ada, F38, "physics__joint__hinge__finalize_spec");
      begin
         E573 := E573 - 1;
         F38;
      end;
      declare
         procedure F39;
         pragma Import (Ada, F39, "physics__joint__dof6__finalize_spec");
      begin
         E572 := E572 - 1;
         F39;
      end;
      declare
         procedure F40;
         pragma Import (Ada, F40, "physics__joint__cone_twist__finalize_spec");
      begin
         E571 := E571 - 1;
         F40;
      end;
      declare
         procedure F41;
         pragma Import (Ada, F41, "physics__joint__ball__finalize_spec");
      begin
         E570 := E570 - 1;
         F41;
      end;
      declare
         procedure F42;
         pragma Import (Ada, F42, "physics__joint__finalize_spec");
      begin
         E569 := E569 - 1;
         F42;
      end;
      declare
         procedure F43;
         pragma Import (Ada, F43, "bullet_physics__object__finalize_body");
      begin
         E586 := E586 - 1;
         F43;
      end;
      declare
         procedure F44;
         pragma Import (Ada, F44, "bullet_physics__object__finalize_spec");
      begin
         F44;
      end;
      E254 := E254 - 1;
      declare
         procedure F45;
         pragma Import (Ada, F45, "physics__object__finalize_spec");
      begin
         F45;
      end;
      E266 := E266 - 1;
      declare
         procedure F46;
         pragma Import (Ada, F46, "physics__model__finalize_spec");
      begin
         F46;
      end;
      E588 := E588 - 1;
      declare
         procedure F47;
         pragma Import (Ada, F47, "bullet_physics__shape__finalize_spec");
      begin
         F47;
      end;
      E270 := E270 - 1;
      declare
         procedure F48;
         pragma Import (Ada, F48, "physics__shape__finalize_spec");
      begin
         F48;
      end;
      E690 := E690 - 1;
      declare
         procedure F49;
         pragma Import (Ada, F49, "gel__window__sdl__finalize_spec");
      begin
         F49;
      end;
      E618 := E618 - 1;
      declare
         procedure F50;
         pragma Import (Ada, F50, "gel__window__finalize_spec");
      begin
         F50;
      end;
      declare
         procedure F51;
         pragma Import (Ada, F51, "gel__events__finalize_spec");
      begin
         E288 := E288 - 1;
         F51;
      end;
      declare
         procedure F52;
         pragma Import (Ada, F52, "gel__remote__world__finalize_body");
      begin
         E311 := E311 - 1;
         F52;
      end;
      declare
         procedure F53;
         pragma Import (Ada, F53, "gel__remote__world__finalize_spec");
      begin
         F53;
      end;
      E333 := E333 - 1;
      E482 := E482 - 1;
      E494 := E494 - 1;
      E492 := E492 - 1;
      E484 := E484 - 1;
      E341 := E341 - 1;
      declare
         procedure F54;
         pragma Import (Ada, F54, "opengl__camera__finalize_spec");
      begin
         F54;
      end;
      declare
         procedure F55;
         pragma Import (Ada, F55, "opengl__impostorer__finalize_spec");
      begin
         F55;
      end;
      E480 := E480 - 1;
      declare
         procedure F56;
         pragma Import (Ada, F56, "opengl__culler__frustum__finalize_spec");
      begin
         F56;
      end;
      E353 := E353 - 1;
      declare
         procedure F57;
         pragma Import (Ada, F57, "opengl__culler__finalize_spec");
      begin
         F57;
      end;
      declare
         procedure F58;
         pragma Import (Ada, F58, "opengl__renderer__lean__finalize_spec");
      begin
         F58;
      end;
      declare
         procedure F59;
         pragma Import (Ada, F59, "opengl__impostor__terrain__finalize_spec");
      begin
         F59;
      end;
      declare
         procedure F60;
         pragma Import (Ada, F60, "opengl__impostor__simple__finalize_spec");
      begin
         F60;
      end;
      declare
         procedure F61;
         pragma Import (Ada, F61, "opengl__impostor__finalize_spec");
      begin
         F61;
      end;
      E357 := E357 - 1;
      declare
         procedure F62;
         pragma Import (Ada, F62, "opengl__visual__finalize_spec");
      begin
         F62;
      end;
      E809 := E809 - 1;
      declare
         procedure F63;
         pragma Import (Ada, F63, "opengl__model__terrain__finalize_spec");
      begin
         F63;
      end;
      E811 := E811 - 1;
      declare
         procedure F64;
         pragma Import (Ada, F64, "opengl__texture__coordinates__finalize_spec");
      begin
         F64;
      end;
      E802 := E802 - 1;
      declare
         procedure F65;
         pragma Import (Ada, F65, "opengl__primitive__short_indexed__finalize_spec");
      begin
         F65;
      end;
      E668 := E668 - 1;
      declare
         procedure F66;
         pragma Import (Ada, F66, "opengl__model__text__lit_colored__finalize_spec");
      begin
         F66;
      end;
      declare
         procedure F67;
         pragma Import (Ada, F67, "opengl__model__text__finalize_spec");
      begin
         E666 := E666 - 1;
         F67;
      end;
      E665 := E665 - 1;
      declare
         procedure F68;
         pragma Import (Ada, F68, "opengl__model__sphere__textured__finalize_spec");
      begin
         F68;
      end;
      declare
         procedure F69;
         pragma Import (Ada, F69, "opengl__model__sphere__lit_colored_textured__finalize_body");
      begin
         E661 := E661 - 1;
         F69;
      end;
      declare
         procedure F70;
         pragma Import (Ada, F70, "opengl__model__sphere__lit_colored_textured__finalize_spec");
      begin
         F70;
      end;
      E659 := E659 - 1;
      declare
         procedure F71;
         pragma Import (Ada, F71, "opengl__model__sphere__lit_colored__finalize_spec");
      begin
         F71;
      end;
      E657 := E657 - 1;
      declare
         procedure F72;
         pragma Import (Ada, F72, "opengl__model__sphere__colored__finalize_spec");
      begin
         F72;
      end;
      E655 := E655 - 1;
      declare
         procedure F73;
         pragma Import (Ada, F73, "opengl__model__sphere__finalize_spec");
      begin
         F73;
      end;
      E653 := E653 - 1;
      declare
         procedure F74;
         pragma Import (Ada, F74, "opengl__model__segment_line__finalize_spec");
      begin
         F74;
      end;
      E649 := E649 - 1;
      declare
         procedure F75;
         pragma Import (Ada, F75, "opengl__model__polygon__lit_colored__finalize_spec");
      begin
         F75;
      end;
      E646 := E646 - 1;
      declare
         procedure F76;
         pragma Import (Ada, F76, "opengl__model__line__colored__finalize_spec");
      begin
         F76;
      end;
      E807 := E807 - 1;
      declare
         procedure F77;
         pragma Import (Ada, F77, "opengl__model__capsule__textured__finalize_spec");
      begin
         F77;
      end;
      E643 := E643 - 1;
      declare
         procedure F78;
         pragma Import (Ada, F78, "opengl__model__box__textured__finalize_spec");
      begin
         F78;
      end;
      E641 := E641 - 1;
      declare
         procedure F79;
         pragma Import (Ada, F79, "opengl__model__box__colored__finalize_spec");
      begin
         F79;
      end;
      E488 := E488 - 1;
      declare
         procedure F80;
         pragma Import (Ada, F80, "opengl__model__billboard__textured__finalize_spec");
      begin
         F80;
      end;
      declare
         procedure F81;
         pragma Import (Ada, F81, "opengl__model__billboard__colored_textured__finalize_body");
      begin
         E635 := E635 - 1;
         F81;
      end;
      declare
         procedure F82;
         pragma Import (Ada, F82, "opengl__model__billboard__colored_textured__finalize_spec");
      begin
         F82;
      end;
      E631 := E631 - 1;
      declare
         procedure F83;
         pragma Import (Ada, F83, "opengl__model__arrow__colored__finalize_spec");
      begin
         F83;
      end;
      E359 := E359 - 1;
      declare
         procedure F84;
         pragma Import (Ada, F84, "opengl__model__finalize_spec");
      begin
         F84;
      end;
      E512 := E512 - 1;
      declare
         procedure F85;
         pragma Import (Ada, F85, "opengl__font__texture__finalize_spec");
      begin
         F85;
      end;
      declare
         procedure F86;
         pragma Import (Ada, F86, "opengl__fontimpl__texture__finalize_body");
      begin
         E514 := E514 - 1;
         F86;
      end;
      declare
         procedure F87;
         pragma Import (Ada, F87, "opengl__fontimpl__texture__finalize_spec");
      begin
         F87;
      end;
      declare
         procedure F88;
         pragma Import (Ada, F88, "opengl__glyph__texture__finalize_body");
      begin
         E516 := E516 - 1;
         F88;
      end;
      declare
         procedure F89;
         pragma Import (Ada, F89, "opengl__glyph__texture__finalize_spec");
      begin
         F89;
      end;
      E518 := E518 - 1;
      declare
         procedure F90;
         pragma Import (Ada, F90, "opengl__glyphimpl__texture__finalize_spec");
      begin
         F90;
      end;
      declare
         procedure F91;
         pragma Import (Ada, F91, "opengl__geometry__textured__finalize_body");
      begin
         E490 := E490 - 1;
         F91;
      end;
      declare
         procedure F92;
         pragma Import (Ada, F92, "opengl__geometry__textured__finalize_spec");
      begin
         F92;
      end;
      declare
         procedure F93;
         pragma Import (Ada, F93, "opengl__geometry__lit_textured_skinned__finalize_body");
      begin
         E532 := E532 - 1;
         F93;
      end;
      declare
         procedure F94;
         pragma Import (Ada, F94, "opengl__geometry__lit_textured_skinned__finalize_spec");
      begin
         F94;
      end;
      declare
         procedure F95;
         pragma Import (Ada, F95, "opengl__geometry__lit_textured__finalize_body");
      begin
         E520 := E520 - 1;
         F95;
      end;
      declare
         procedure F96;
         pragma Import (Ada, F96, "opengl__geometry__lit_textured__finalize_spec");
      begin
         F96;
      end;
      declare
         procedure F97;
         pragma Import (Ada, F97, "opengl__geometry__lit_colored_textured_skinned__finalize_body");
      begin
         E528 := E528 - 1;
         F97;
      end;
      declare
         procedure F98;
         pragma Import (Ada, F98, "opengl__geometry__lit_colored_textured_skinned__finalize_spec");
      begin
         F98;
      end;
      declare
         procedure F99;
         pragma Import (Ada, F99, "opengl__geometry__lit_colored_textured__finalize_body");
      begin
         E663 := E663 - 1;
         F99;
      end;
      declare
         procedure F100;
         pragma Import (Ada, F100, "opengl__geometry__lit_colored_textured__finalize_spec");
      begin
         F100;
      end;
      declare
         procedure F101;
         pragma Import (Ada, F101, "opengl__geometry__lit_colored__finalize_body");
      begin
         E651 := E651 - 1;
         F101;
      end;
      declare
         procedure F102;
         pragma Import (Ada, F102, "opengl__geometry__lit_colored__finalize_spec");
      begin
         F102;
      end;
      declare
         procedure F103;
         pragma Import (Ada, F103, "opengl__geometry__colored_textured__finalize_body");
      begin
         E637 := E637 - 1;
         F103;
      end;
      declare
         procedure F104;
         pragma Import (Ada, F104, "opengl__geometry__colored_textured__finalize_spec");
      begin
         F104;
      end;
      declare
         procedure F105;
         pragma Import (Ada, F105, "opengl__geometry__colored__finalize_body");
      begin
         E633 := E633 - 1;
         F105;
      end;
      declare
         procedure F106;
         pragma Import (Ada, F106, "opengl__geometry__colored__finalize_spec");
      begin
         F106;
      end;
      E392 := E392 - 1;
      declare
         procedure F107;
         pragma Import (Ada, F107, "opengl__geometry__finalize_spec");
      begin
         F107;
      end;
      E458 := E458 - 1;
      declare
         procedure F108;
         pragma Import (Ada, F108, "opengl__primitive__long_indexed__finalize_spec");
      begin
         F108;
      end;
      E450 := E450 - 1;
      declare
         procedure F109;
         pragma Import (Ada, F109, "opengl__primitive__indexed__finalize_spec");
      begin
         F109;
      end;
      E394 := E394 - 1;
      declare
         procedure F110;
         pragma Import (Ada, F110, "opengl__primitive__finalize_spec");
      begin
         F110;
      end;
      declare
         procedure F111;
         pragma Import (Ada, F111, "opengl__io__wavefront__finalize_body");
      begin
         E778 := E778 - 1;
         F111;
      end;
      E396 := E396 - 1;
      declare
         procedure F112;
         pragma Import (Ada, F112, "opengl__texture__finalize_spec");
      begin
         F112;
      end;
      E534 := E534 - 1;
      declare
         procedure F113;
         pragma Import (Ada, F113, "opengl__program__lit__textured_skinned__finalize_spec");
      begin
         F113;
      end;
      E530 := E530 - 1;
      declare
         procedure F114;
         pragma Import (Ada, F114, "opengl__program__lit__colored_textured_skinned__finalize_spec");
      begin
         F114;
      end;
      E522 := E522 - 1;
      declare
         procedure F115;
         pragma Import (Ada, F115, "opengl__program__lit__finalize_spec");
      begin
         F115;
      end;
      E478 := E478 - 1;
      declare
         procedure F116;
         pragma Import (Ada, F116, "opengl__program__finalize_body");
      begin
         E462 := E462 - 1;
         F116;
      end;
      declare
         procedure F117;
         pragma Import (Ada, F117, "opengl__program__finalize_spec");
      begin
         F117;
      end;
      declare
         procedure F118;
         pragma Import (Ada, F118, "opengl__variable__uniform__finalize_spec");
      begin
         F118;
      end;
      E474 := E474 - 1;
      declare
         procedure F119;
         pragma Import (Ada, F119, "opengl__shader__finalize_spec");
      begin
         F119;
      end;
      E314 := E314 - 1;
      declare
         procedure F120;
         pragma Import (Ada, F120, "opengl__renderer__finalize_spec");
      begin
         F120;
      end;
      E804 := E804 - 1;
      declare
         procedure F121;
         pragma Import (Ada, F121, "opengl__buffer__short_indices__finalize_spec");
      begin
         F121;
      end;
      E460 := E460 - 1;
      declare
         procedure F122;
         pragma Import (Ada, F122, "opengl__buffer__long_indices__finalize_spec");
      begin
         F122;
      end;
      E454 := E454 - 1;
      declare
         procedure F123;
         pragma Import (Ada, F123, "opengl__buffer__indices__finalize_spec");
      begin
         F123;
      end;
      E452 := E452 - 1;
      declare
         procedure F124;
         pragma Import (Ada, F124, "opengl__buffer__finalize_spec");
      begin
         F124;
      end;
      E464 := E464 - 1;
      declare
         procedure F125;
         pragma Import (Ada, F125, "opengl__attribute__finalize_spec");
      begin
         F125;
      end;
      E538 := E538 - 1;
      declare
         procedure F126;
         pragma Import (Ada, F126, "opengl__context__finalize_spec");
      begin
         F126;
      end;
      E496 := E496 - 1;
      declare
         procedure F127;
         pragma Import (Ada, F127, "opengl__surface__finalize_spec");
      begin
         F127;
      end;
      E506 := E506 - 1;
      declare
         procedure F128;
         pragma Import (Ada, F128, "opengl__surface_profile__finalize_spec");
      begin
         F128;
      end;
      E508 := E508 - 1;
      declare
         procedure F129;
         pragma Import (Ada, F129, "opengl__screen__finalize_spec");
      begin
         F129;
      end;
      E466 := E466 - 1;
      declare
         procedure F130;
         pragma Import (Ada, F130, "opengl__light__finalize_spec");
      begin
         F130;
      end;
      E383 := E383 - 1;
      E361 := E361 - 1;
      declare
         procedure F131;
         pragma Import (Ada, F131, "opengl__font__finalize_spec");
      begin
         F131;
      end;
      declare
         procedure F132;
         pragma Import (Ada, F132, "opengl__fontimpl__finalize_spec");
      begin
         F132;
      end;
      E389 := E389 - 1;
      declare
         procedure F133;
         pragma Import (Ada, F133, "opengl__glyph__container__finalize_spec");
      begin
         F133;
      end;
      E387 := E387 - 1;
      declare
         procedure F134;
         pragma Import (Ada, F134, "opengl__glyphimpl__finalize_spec");
      begin
         F134;
      end;
      E502 := E502 - 1;
      declare
         procedure F135;
         pragma Import (Ada, F135, "opengl__display__finalize_spec");
      begin
         F135;
      end;
      E590 := E590 - 1;
      declare
         procedure F136;
         pragma Import (Ada, F136, "lace__subject_and_deferred_observer__finalize_spec");
      begin
         F136;
      end;
      E624 := E624 - 1;
      declare
         procedure F137;
         pragma Import (Ada, F137, "gel__mouse__local__finalize_spec");
      begin
         F137;
      end;
      E620 := E620 - 1;
      declare
         procedure F138;
         pragma Import (Ada, F138, "gel__keyboard__local__finalize_spec");
      begin
         F138;
      end;
      E622 := E622 - 1;
      declare
         procedure F139;
         pragma Import (Ada, F139, "lace__subject__local__finalize_spec");
      begin
         F139;
      end;
      E594 := E594 - 1;
      declare
         procedure F140;
         pragma Import (Ada, F140, "lace__event__logger__text__finalize_spec");
      begin
         F140;
      end;
      E290 := E290 - 1;
      declare
         procedure F141;
         pragma Import (Ada, F141, "gel__mouse__finalize_spec");
      begin
         F141;
      end;
      E294 := E294 - 1;
      declare
         procedure F142;
         pragma Import (Ada, F142, "gel__keyboard__finalize_spec");
      begin
         F142;
      end;
      E298 := E298 - 1;
      E301 := E301 - 1;
      declare
         procedure F143;
         pragma Import (Ada, F143, "lace__event__logger__finalize_spec");
      begin
         E299 := E299 - 1;
         F143;
      end;
      declare
         procedure F144;
         pragma Import (Ada, F144, "lace__subject__finalize_spec");
      begin
         F144;
      end;
      declare
         procedure F145;
         pragma Import (Ada, F145, "lace__observer__finalize_spec");
      begin
         F145;
      end;
      E303 := E303 - 1;
      declare
         procedure F146;
         pragma Import (Ada, F146, "lace__response__finalize_spec");
      begin
         F146;
      end;
      E404 := E404 - 1;
      declare
         procedure F147;
         pragma Import (Ada, F147, "gid__finalize_spec");
      begin
         F147;
      end;
      E379 := E379 - 1;
      declare
         procedure F148;
         pragma Import (Ada, F148, "freetype__face__finalize_body");
      begin
         E364 := E364 - 1;
         F148;
      end;
      declare
         procedure F149;
         pragma Import (Ada, F149, "freetype__face__finalize_spec");
      begin
         F149;
      end;
      declare
         procedure F150;
         pragma Import (Ada, F150, "freetype__charmap__finalize_spec");
      begin
         F150;
      end;
      E381 := E381 - 1;
      declare
         procedure F151;
         pragma Import (Ada, F151, "freetype__face_size__finalize_spec");
      begin
         F151;
      end;
      E769 := E769 - 1;
      declare
         procedure F152;
         pragma Import (Ada, F152, "float_math__geometry__d3__modeller__finalize_spec");
      begin
         F152;
      end;
      E757 := E757 - 1;
      declare
         procedure F153;
         pragma Import (Ada, F153, "collada__library__visual_scenes__finalize_spec");
      begin
         F153;
      end;
      E753 := E753 - 1;
      declare
         procedure F154;
         pragma Import (Ada, F154, "collada__library__controllers__finalize_spec");
      begin
         F154;
      end;
      E751 := E751 - 1;
      declare
         procedure F155;
         pragma Import (Ada, F155, "collada__library__animations__finalize_spec");
      begin
         F155;
      end;
      E731 := E731 - 1;
      declare
         procedure F156;
         pragma Import (Ada, F156, "sdl__video__gl__finalize_spec");
      begin
         F156;
      end;
      E707 := E707 - 1;
      declare
         procedure F157;
         pragma Import (Ada, F157, "sdl__video__windows__finalize_spec");
      begin
         F157;
      end;
      E733 := E733 - 1;
      declare
         procedure F158;
         pragma Import (Ada, F158, "sdl__video__textures__finalize_spec");
      begin
         F158;
      end;
      E722 := E722 - 1;
      declare
         procedure F159;
         pragma Import (Ada, F159, "sdl__video__surfaces__finalize_spec");
      begin
         F159;
      end;
      declare
         procedure F160;
         pragma Import (Ada, F160, "sdl__video__palettes__finalize_body");
      begin
         E718 := E718 - 1;
         F160;
      end;
      declare
         procedure F161;
         pragma Import (Ada, F161, "sdl__video__palettes__finalize_spec");
      begin
         F161;
      end;
      E727 := E727 - 1;
      declare
         procedure F162;
         pragma Import (Ada, F162, "sdl__log__finalize_spec");
      begin
         F162;
      end;
      E196 := E196 - 1;
      declare
         procedure F163;
         pragma Import (Ada, F163, "opengl__finalize_spec");
      begin
         F163;
      end;
      E296 := E296 - 1;
      declare
         procedure F164;
         pragma Import (Ada, F164, "lace__event__finalize_spec");
      begin
         F164;
      end;
      E167 := E167 - 1;
      declare
         procedure F165;
         pragma Import (Ada, F165, "float_math__geometry__finalize_spec");
      begin
         F165;
      end;
      E329 := E329 - 1;
      declare
         procedure F166;
         pragma Import (Ada, F166, "system__tasking__protected_objects__entries__finalize_spec");
      begin
         F166;
      end;
      E306 := E306 - 1;
      declare
         procedure F167;
         pragma Import (Ada, F167, "system__partition_interface__finalize_spec");
      begin
         F167;
      end;
      E308 := E308 - 1;
      declare
         procedure F168;
         pragma Import (Ada, F168, "system__rpc__finalize_spec");
      begin
         F168;
      end;
      declare
         procedure F169;
         pragma Import (Ada, F169, "ada__directories__finalize_body");
      begin
         E670 := E670 - 1;
         F169;
      end;
      declare
         procedure F170;
         pragma Import (Ada, F170, "ada__directories__finalize_spec");
      begin
         F170;
      end;
      E677 := E677 - 1;
      declare
         procedure F171;
         pragma Import (Ada, F171, "system__regexp__finalize_spec");
      begin
         F171;
      end;
      E272 := E272 - 1;
      declare
         procedure F172;
         pragma Import (Ada, F172, "system__pool_global__finalize_spec");
      begin
         F172;
      end;
      E238 := E238 - 1;
      declare
         procedure F173;
         pragma Import (Ada, F173, "ada__text_io__finalize_spec");
      begin
         F173;
      end;
      E446 := E446 - 1;
      declare
         procedure F174;
         pragma Import (Ada, F174, "ada__strings__unbounded__finalize_spec");
      begin
         F174;
      end;
      E258 := E258 - 1;
      declare
         procedure F175;
         pragma Import (Ada, F175, "system__storage_pools__subpools__finalize_spec");
      begin
         F175;
      end;
      E260 := E260 - 1;
      declare
         procedure F176;
         pragma Import (Ada, F176, "system__finalization_masters__finalize_spec");
      begin
         F176;
      end;
      E440 := E440 - 1;
      declare
         procedure F177;
         pragma Import (Ada, F177, "ada__streams__stream_io__finalize_spec");
      begin
         F177;
      end;
      declare
         procedure F178;
         pragma Import (Ada, F178, "system__file_io__finalize_body");
      begin
         E242 := E242 - 1;
         F178;
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
          (False, False, False, False, True, True, True, False, 
           True, False, False, True, True, True, True, False, 
           False, False, False, True, False, False, True, True, 
           False, True, True, False, True, True, True, True, 
           False, True, False, False, True, False, True, False, 
           True, True, False, True, True, True, True, False, 
           True, False, True, True, False, True, True, False, 
           True, False, True, False, False, True, False, False, 
           False, True, False, True, True, True, False, False, 
           True, False, True, True, True, False, True, True, 
           False, True, True, True, True, False, False, False, 
           False, False, False, False, False, True, True, True, 
           True, False, True, False),
         Count => (0, 0, 0, 1, 2, 5, 2, 0, 11, 0),
         Unknown => (False, False, False, False, False, False, True, False, True, False));
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

      System.Scalar_Values.Initialize ('I', 'N');

      Finalize_Library_Objects := finalize_library'access;

      Ada.Exceptions'Elab_Spec;
      System.Soft_Links'Elab_Spec;
      System.Exception_Table'Elab_Body;
      E010 := E010 + 1;
      Ada.Containers'Elab_Spec;
      E045 := E045 + 1;
      Ada.Io_Exceptions'Elab_Spec;
      E074 := E074 + 1;
      Ada.Numerics'Elab_Spec;
      E025 := E025 + 1;
      Ada.Strings'Elab_Spec;
      E007 := E007 + 1;
      Ada.Strings.Maps'Elab_Spec;
      E063 := E063 + 1;
      Ada.Strings.Maps.Constants'Elab_Spec;
      E066 := E066 + 1;
      Interfaces.C'Elab_Spec;
      E050 := E050 + 1;
      System.Exceptions'Elab_Spec;
      E019 := E019 + 1;
      System.Object_Reader'Elab_Spec;
      E088 := E088 + 1;
      System.Dwarf_Lines'Elab_Spec;
      System.Os_Lib'Elab_Body;
      E079 := E079 + 1;
      System.Soft_Links.Initialize'Elab_Body;
      E102 := E102 + 1;
      E012 := E012 + 1;
      System.Traceback.Symbolic'Elab_Body;
      E044 := E044 + 1;
      System.Img_Int'Elab_Spec;
      E024 := E024 + 1;
      E016 := E016 + 1;
      System.Img_Uns'Elab_Spec;
      E069 := E069 + 1;
      E057 := E057 + 1;
      Ada.Assertions'Elab_Spec;
      E711 := E711 + 1;
      Ada.Strings.Utf_Encoding'Elab_Spec;
      E106 := E106 + 1;
      Ada.Tags'Elab_Spec;
      Ada.Tags'Elab_Body;
      E114 := E114 + 1;
      Ada.Strings.Text_Buffers'Elab_Spec;
      E005 := E005 + 1;
      Gnat'Elab_Spec;
      E337 := E337 + 1;
      Interfaces.C.Strings'Elab_Spec;
      E336 := E336 + 1;
      Ada.Streams'Elab_Spec;
      E169 := E169 + 1;
      System.File_Control_Block'Elab_Spec;
      E246 := E246 + 1;
      System.Finalization_Root'Elab_Spec;
      E245 := E245 + 1;
      Ada.Finalization'Elab_Spec;
      E243 := E243 + 1;
      System.File_Io'Elab_Body;
      E242 := E242 + 1;
      Ada.Streams.Stream_Io'Elab_Spec;
      E440 := E440 + 1;
      System.Storage_Pools'Elab_Spec;
      E256 := E256 + 1;
      System.Finalization_Masters'Elab_Spec;
      System.Finalization_Masters'Elab_Body;
      E260 := E260 + 1;
      System.Storage_Pools.Subpools'Elab_Spec;
      E258 := E258 + 1;
      Ada.Strings.Unbounded'Elab_Spec;
      E446 := E446 + 1;
      System.Task_Info'Elab_Spec;
      E219 := E219 + 1;
      Ada.Calendar'Elab_Spec;
      Ada.Calendar'Elab_Body;
      E236 := E236 + 1;
      Ada.Calendar.Delays'Elab_Body;
      E234 := E234 + 1;
      Ada.Calendar.Time_Zones'Elab_Spec;
      E746 := E746 + 1;
      Ada.Text_Io'Elab_Spec;
      Ada.Text_Io'Elab_Body;
      E238 := E238 + 1;
      System.Pool_Global'Elab_Spec;
      System.Pool_Global'Elab_Body;
      E272 := E272 + 1;
      System.Random_Seed'Elab_Body;
      E472 := E472 + 1;
      System.Regexp'Elab_Spec;
      System.Regexp'Elab_Body;
      E677 := E677 + 1;
      Ada.Directories'Elab_Spec;
      Ada.Directories'Elab_Body;
      E670 := E670 + 1;
      System.Rpc'Elab_Spec;
      System.Rpc'Elab_Body;
      E308 := E308 + 1;
      System.Partition_Interface'Elab_Spec;
      System.Partition_Interface'Elab_Body;
      E306 := E306 + 1;
      System.Img_Llli'Elab_Spec;
      E786 := E786 + 1;
      System.Img_Lli'Elab_Spec;
      E226 := E226 + 1;
      System.Task_Primitives.Operations'Elab_Body;
      E211 := E211 + 1;
      Ada.Real_Time'Elab_Spec;
      Ada.Real_Time'Elab_Body;
      E287 := E287 + 1;
      System.Img_Llu'Elab_Spec;
      E390 := E390 + 1;
      System.Tasking.Initialization'Elab_Body;
      E323 := E323 + 1;
      System.Tasking.Protected_Objects'Elab_Body;
      E200 := E200 + 1;
      System.Tasking.Protected_Objects.Entries'Elab_Spec;
      E329 := E329 + 1;
      System.Tasking.Queuing'Elab_Body;
      E327 := E327 + 1;
      System.Tasking.Stages'Elab_Body;
      E349 := E349 + 1;
      Float_Math'Elab_Spec;
      E122 := E122 + 1;
      Box2d_Physics'Elab_Spec;
      E543 := E543 + 1;
      float_math.geometry'elab_spec;
      E167 := E167 + 1;
      float_math.geometry.d2'elab_spec;
      E186 := E186 + 1;
      Bullet_Physics'Elab_Spec;
      E575 := E575 + 1;
      Freetype'Elab_Spec;
      E362 := E362 + 1;
      lace.any'elab_spec;
      E264 := E264 + 1;
      lace.event'elab_spec;
      E296 := E296 + 1;
      Opengl'Elab_Spec;
      E196 := E196 + 1;
      Physics'Elab_Spec;
      E230 := E230 + 1;
      GEL'ELAB_SPEC;
      E120 := E120 + 1;
      SDL.LOG'ELAB_SPEC;
      E727 := E727 + 1;
      SDL.VIDEO'ELAB_SPEC;
      E705 := E705 + 1;
      SDL.VIDEO.PALETTES'ELAB_SPEC;
      SDL.VIDEO.PALETTES'ELAB_BODY;
      E718 := E718 + 1;
      SDL.VIDEO.PIXEL_FORMATS'ELAB_SPEC;
      E716 := E716 + 1;
      SDL.VIDEO.PIXELS'ELAB_SPEC;
      E734 := E734 + 1;
      SDL.VIDEO.RECTANGLES'ELAB_SPEC;
      E720 := E720 + 1;
      SDL.VIDEO.SURFACES'ELAB_SPEC;
      E722 := E722 + 1;
      SDL.VIDEO.TEXTURES'ELAB_SPEC;
      E733 := E733 + 1;
      SDL.VIDEO.WINDOWS'ELAB_SPEC;
      E707 := E707 + 1;
      SDL.EVENTS.EVENTS'ELAB_SPEC;
      E695 := E695 + 1;
      SDL.VIDEO.GL'ELAB_SPEC;
      E731 := E731 + 1;
      E772 := E772 + 1;
      E776 := E776 + 1;
      E566 := E566 + 1;
      Collada'Elab_Spec;
      E740 := E740 + 1;
      collada.library'elab_spec;
      E749 := E749 + 1;
      collada.library.animations'elab_spec;
      E751 := E751 + 1;
      collada.library.controllers'elab_spec;
      E753 := E753 + 1;
      collada.library.geometries'elab_spec;
      E755 := E755 + 1;
      collada.library.visual_scenes'elab_spec;
      collada.library.visual_scenes'elab_body;
      E757 := E757 + 1;
      float_math.geometry.d3.modeller'elab_spec;
      E769 := E769 + 1;
      freetype_c.binding'elab_spec;
      E366 := E366 + 1;
      freetype.face_size'elab_spec;
      freetype.face_size'elab_body;
      E381 := E381 + 1;
      freetype.charmap'elab_spec;
      freetype.face'elab_spec;
      freetype.face'elab_body;
      E364 := E364 + 1;
      freetype.charmap'elab_body;
      E379 := E379 + 1;
      GID'ELAB_SPEC;
      E408 := E408 + 1;
      E412 := E412 + 1;
      E406 := E406 + 1;
      E410 := E410 + 1;
      E414 := E414 + 1;
      GID.DECODING_PNG.HUFFMAN'ELAB_SPEC;
      E418 := E418 + 1;
      GID.DECODING_PNG'ELAB_BODY;
      E416 := E416 + 1;
      E423 := E423 + 1;
      E425 := E425 + 1;
      GID'ELAB_BODY;
      E404 := E404 + 1;
      GL.LEAN'ELAB_SPEC;
      E334 := E334 + 1;
      E398 := E398 + 1;
      GL.SAFE'ELAB_SPEC;
      E436 := E436 + 1;
      lace.response'elab_spec;
      lace.response'elab_body;
      E303 := E303 + 1;
      lace.observer'elab_spec;
      lace.subject'elab_spec;
      lace.event.logger'elab_spec;
      E299 := E299 + 1;
      E301 := E301 + 1;
      E298 := E298 + 1;
      gel.keyboard'elab_spec;
      E294 := E294 + 1;
      gel.mouse'elab_spec;
      E290 := E290 + 1;
      lace.event.logger.text'elab_spec;
      E592 := E592 + 1;
      lace.event.logger.text'elab_body;
      E594 := E594 + 1;
      E598 := E598 + 1;
      E600 := E600 + 1;
      E602 := E602 + 1;
      lace.subject.local'elab_spec;
      lace.subject.local'elab_body;
      E622 := E622 + 1;
      gel.keyboard.local'elab_spec;
      gel.keyboard.local'elab_body;
      E620 := E620 + 1;
      gel.mouse.local'elab_spec;
      E624 := E624 + 1;
      lace.subject_and_deferred_observer'elab_spec;
      lace.subject_and_deferred_observer'elab_body;
      E590 := E590 + 1;
      E526 := E526 + 1;
      Opengl.Display'Elab_Spec;
      Opengl.Display'Elab_Body;
      E502 := E502 + 1;
      E504 := E504 + 1;
      E355 := E355 + 1;
      opengl.glyphimpl'elab_spec;
      opengl.glyphimpl'elab_body;
      E387 := E387 + 1;
      opengl.glyph'elab_spec;
      opengl.glyph'elab_body;
      E385 := E385 + 1;
      opengl.glyph.container'elab_spec;
      opengl.glyph.container'elab_body;
      E389 := E389 + 1;
      opengl.fontimpl'elab_spec;
      Opengl.Font'Elab_Spec;
      Opengl.Font'Elab_Body;
      E361 := E361 + 1;
      opengl.fontimpl'elab_body;
      E383 := E383 + 1;
      E438 := E438 + 1;
      opengl.palette'elab_spec;
      Opengl.Palette'Elab_Body;
      E468 := E468 + 1;
      Opengl.Light'Elab_Spec;
      Opengl.Light'Elab_Body;
      E466 := E466 + 1;
      Opengl.Remote_Model'Elab_Spec;
      E312 := E312 + 1;
      opengl.screen'elab_spec;
      E508 := E508 + 1;
      opengl.surface_profile'elab_spec;
      opengl.surface_profile'elab_body;
      E506 := E506 + 1;
      E510 := E510 + 1;
      opengl.surface'elab_spec;
      opengl.surface'elab_body;
      E496 := E496 + 1;
      E540 := E540 + 1;
      opengl.context'elab_spec;
      opengl.context'elab_body;
      E538 := E538 + 1;
      E317 := E317 + 1;
      opengl.attribute'elab_spec;
      opengl.attribute'elab_body;
      E464 := E464 + 1;
      E400 := E400 + 1;
      opengl.buffer'elab_spec;
      opengl.buffer'elab_body;
      E452 := E452 + 1;
      E456 := E456 + 1;
      Opengl.Buffer.Indices'Elab_Spec;
      E454 := E454 + 1;
      Opengl.Buffer.Long_Indices'Elab_Spec;
      E460 := E460 + 1;
      Opengl.Buffer.Short_Indices'Elab_Spec;
      E804 := E804 + 1;
      Opengl.Renderer'Elab_Spec;
      Opengl.Renderer'Elab_Body;
      E314 := E314 + 1;
      E536 := E536 + 1;
      opengl.shader'elab_spec;
      opengl.shader'elab_body;
      E474 := E474 + 1;
      opengl.variable'elab_spec;
      opengl.variable'elab_body;
      E476 := E476 + 1;
      opengl.variable.uniform'elab_spec;
      opengl.program'elab_spec;
      Opengl.Program'Elab_Body;
      E462 := E462 + 1;
      opengl.variable.uniform'elab_body;
      E478 := E478 + 1;
      Opengl.Program.Lit'Elab_Spec;
      Opengl.Program.Lit'Elab_Body;
      E522 := E522 + 1;
      opengl.program.lit.colored_textured_skinned'elab_spec;
      opengl.program.lit.colored_textured_skinned'elab_body;
      E530 := E530 + 1;
      opengl.program.lit.textured_skinned'elab_spec;
      opengl.program.lit.textured_skinned'elab_body;
      E534 := E534 + 1;
      E444 := E444 + 1;
      Opengl.Texture'Elab_Spec;
      opengl.io'elab_body;
      E402 := E402 + 1;
      opengl.texture'elab_body;
      E396 := E396 + 1;
      E767 := E767 + 1;
      Opengl.Io.Wavefront'Elab_Body;
      E778 := E778 + 1;
      opengl.primitive'elab_spec;
      opengl.primitive'elab_body;
      E394 := E394 + 1;
      opengl.primitive.indexed'elab_spec;
      opengl.primitive.indexed'elab_body;
      E450 := E450 + 1;
      opengl.primitive.long_indexed'elab_spec;
      opengl.primitive.long_indexed'elab_body;
      E458 := E458 + 1;
      opengl.geometry'elab_spec;
      opengl.geometry'elab_body;
      E392 := E392 + 1;
      Opengl.Geometry.Colored'Elab_Spec;
      Opengl.Geometry.Colored'Elab_Body;
      E633 := E633 + 1;
      Opengl.Geometry.Colored_Textured'Elab_Spec;
      Opengl.Geometry.Colored_Textured'Elab_Body;
      E637 := E637 + 1;
      Opengl.Geometry.Lit_Colored'Elab_Spec;
      opengl.geometry.lit_colored'elab_body;
      E651 := E651 + 1;
      Opengl.Geometry.Lit_Colored_Textured'Elab_Spec;
      opengl.geometry.lit_colored_textured'elab_body;
      E663 := E663 + 1;
      Opengl.Geometry.Lit_Colored_Textured_Skinned'Elab_Spec;
      Opengl.Geometry.Lit_Colored_Textured_Skinned'Elab_Body;
      E528 := E528 + 1;
      Opengl.Geometry.Lit_Textured'Elab_Spec;
      Opengl.Geometry.Lit_Textured'Elab_Body;
      E520 := E520 + 1;
      Opengl.Geometry.Lit_Textured_Skinned'Elab_Spec;
      Opengl.Geometry.Lit_Textured_Skinned'Elab_Body;
      E532 := E532 + 1;
      Opengl.Geometry.Textured'Elab_Spec;
      Opengl.Geometry.Textured'Elab_Body;
      E490 := E490 + 1;
      Opengl.Glyphimpl.Texture'Elab_Spec;
      Opengl.Glyphimpl.Texture'Elab_Body;
      E518 := E518 + 1;
      Opengl.Glyph.Texture'Elab_Spec;
      Opengl.Glyph.Texture'Elab_Body;
      E516 := E516 + 1;
      opengl.fontimpl.texture'elab_spec;
      opengl.fontimpl.texture'elab_body;
      E514 := E514 + 1;
      opengl.font.texture'elab_spec;
      opengl.font.texture'elab_body;
      E512 := E512 + 1;
      opengl.model'elab_spec;
      opengl.model'elab_body;
      E359 := E359 + 1;
      Opengl.Model.Arrow'Elab_Spec;
      E629 := E629 + 1;
      opengl.model.arrow.colored'elab_spec;
      Opengl.Model.Arrow.Colored'Elab_Body;
      E631 := E631 + 1;
      opengl.model.billboard'elab_spec;
      opengl.model.billboard'elab_body;
      E486 := E486 + 1;
      opengl.model.billboard.colored_textured'elab_spec;
      opengl.model.billboard.colored_textured'elab_body;
      E635 := E635 + 1;
      Opengl.Model.Billboard.Textured'Elab_Spec;
      Opengl.Model.Billboard.Textured'Elab_Body;
      E488 := E488 + 1;
      Opengl.Model.Box'Elab_Spec;
      Opengl.Model.Box'Elab_Body;
      E639 := E639 + 1;
      Opengl.Model.Box.Colored'Elab_Spec;
      Opengl.Model.Box.Colored'Elab_Body;
      E641 := E641 + 1;
      Opengl.Model.Box.Textured'Elab_Spec;
      Opengl.Model.Box.Textured'Elab_Body;
      E643 := E643 + 1;
      Opengl.Model.Capsule'Elab_Spec;
      E805 := E805 + 1;
      Opengl.Model.Capsule.Textured'Elab_Spec;
      Opengl.Model.Capsule.Textured'Elab_Body;
      E807 := E807 + 1;
      Opengl.Model.Line'Elab_Spec;
      E644 := E644 + 1;
      Opengl.Model.Line.Colored'Elab_Spec;
      Opengl.Model.Line.Colored'Elab_Body;
      E646 := E646 + 1;
      Opengl.Model.Polygon'Elab_Spec;
      E647 := E647 + 1;
      Opengl.Model.Polygon.Lit_Colored'Elab_Spec;
      Opengl.Model.Polygon.Lit_Colored'Elab_Body;
      E649 := E649 + 1;
      Opengl.Model.Segment_Line'Elab_Spec;
      Opengl.Model.Segment_Line'Elab_Body;
      E653 := E653 + 1;
      opengl.model.sphere'elab_spec;
      opengl.model.sphere'elab_body;
      E655 := E655 + 1;
      Opengl.Model.Sphere.Colored'Elab_Spec;
      Opengl.Model.Sphere.Colored'Elab_Body;
      E657 := E657 + 1;
      Opengl.Model.Sphere.Lit_Colored'Elab_Spec;
      Opengl.Model.Sphere.Lit_Colored'Elab_Body;
      E659 := E659 + 1;
      Opengl.Model.Sphere.Lit_Colored_Textured'Elab_Spec;
      Opengl.Model.Sphere.Lit_Colored_Textured'Elab_Body;
      E661 := E661 + 1;
      Opengl.Model.Sphere.Textured'Elab_Spec;
      Opengl.Model.Sphere.Textured'Elab_Body;
      E665 := E665 + 1;
      Opengl.Model.Text'Elab_Spec;
      E666 := E666 + 1;
      Opengl.Model.Text.Lit_Colored'Elab_Spec;
      Opengl.Model.Text.Lit_Colored'Elab_Body;
      E668 := E668 + 1;
      opengl.primitive.short_indexed'elab_spec;
      opengl.primitive.short_indexed'elab_body;
      E802 := E802 + 1;
      Opengl.Texture.Coordinates'Elab_Spec;
      Opengl.Texture.Coordinates'Elab_Body;
      E811 := E811 + 1;
      opengl.model.terrain'elab_spec;
      opengl.model.terrain'elab_body;
      E809 := E809 + 1;
      opengl.visual'elab_spec;
      Opengl.Visual'Elab_Body;
      E357 := E357 + 1;
      opengl.impostor'elab_spec;
      opengl.impostor.simple'elab_spec;
      opengl.impostor.terrain'elab_spec;
      opengl.renderer.lean'elab_spec;
      opengl.culler'elab_spec;
      E353 := E353 + 1;
      opengl.culler.frustum'elab_spec;
      opengl.culler.frustum'elab_body;
      E480 := E480 + 1;
      opengl.impostorer'elab_spec;
      opengl.camera'elab_spec;
      opengl.camera'elab_body;
      E341 := E341 + 1;
      opengl.impostor'elab_body;
      E484 := E484 + 1;
      opengl.impostor.simple'elab_body;
      E492 := E492 + 1;
      opengl.impostor.terrain'elab_body;
      E494 := E494 + 1;
      opengl.impostorer'elab_body;
      E482 := E482 + 1;
      opengl.renderer.lean'elab_body;
      E333 := E333 + 1;
      E616 := E616 + 1;
      physics.remote.model'elab_spec;
      E268 := E268 + 1;
      gel.remote.world'elab_spec;
      gel.remote.world'elab_body;
      E311 := E311 + 1;
      gel.events'elab_spec;
      E288 := E288 + 1;
      gel.window'elab_spec;
      gel.window'elab_body;
      E618 := E618 + 1;
      gel.window.sdl'elab_spec;
      gel.window.sdl'elab_body;
      E690 := E690 + 1;
      physics.shape'elab_spec;
      E270 := E270 + 1;
      bullet_physics.shape'elab_spec;
      bullet_physics.shape'elab_body;
      E588 := E588 + 1;
      physics.model'elab_spec;
      physics.model'elab_body;
      E266 := E266 + 1;
      physics.object'elab_spec;
      E254 := E254 + 1;
      bullet_physics.object'elab_spec;
      bullet_physics.object'elab_body;
      E586 := E586 + 1;
      physics.joint'elab_spec;
      E569 := E569 + 1;
      physics.joint.ball'elab_spec;
      E570 := E570 + 1;
      physics.joint.cone_twist'elab_spec;
      E571 := E571 + 1;
      physics.joint.dof6'elab_spec;
      E572 := E572 + 1;
      physics.joint.hinge'elab_spec;
      E573 := E573 + 1;
      physics.joint.slider'elab_spec;
      E574 := E574 + 1;
      bullet_physics.joint'elab_spec;
      bullet_physics.joint'elab_body;
      E584 := E584 + 1;
      physics.space'elab_spec;
      E568 := E568 + 1;
      box2d_physics.shape'elab_spec;
      box2d_physics.shape'elab_body;
      E564 := E564 + 1;
      box2d_physics.object'elab_spec;
      box2d_physics.object'elab_body;
      E562 := E562 + 1;
      box2d_physics.joint'elab_spec;
      box2d_physics.joint'elab_body;
      E560 := E560 + 1;
      Box2d_Physics.Space'Elab_Spec;
      box2d_physics.space'elab_body;
      E545 := E545 + 1;
      Bullet_Physics.Space'Elab_Spec;
      bullet_physics.space'elab_body;
      E577 := E577 + 1;
      E542 := E542 + 1;
      gel.joint'elab_spec;
      gel.sprite'elab_spec;
      GEL.ANY_JOINT'ELAB_SPEC;
      gel.any_joint'elab_body;
      E252 := E252 + 1;
      gel.hinge_joint'elab_spec;
      gel.hinge_joint'elab_body;
      E604 := E604 + 1;
      gel.world'elab_spec;
      gel.world'elab_body;
      E278 := E278 + 1;
      gel.joint'elab_body;
      E276 := E276 + 1;
      gel.sprite'elab_body;
      E250 := E250 + 1;
      gel.camera'elab_spec;
      gel.camera'elab_body;
      E248 := E248 + 1;
      E606 := E606 + 1;
      gel.dolly'elab_spec;
      gel.dolly'elab_body;
      E608 := E608 + 1;
      gel.dolly.following'elab_spec;
      gel.dolly.following'elab_body;
      E610 := E610 + 1;
      gel.dolly.simple'elab_spec;
      gel.dolly.simple'elab_body;
      E612 := E612 + 1;
      gel.world.client'elab_spec;
      gel.world.client'elab_body;
      E681 := E681 + 1;
      gel.world.server'elab_spec;
      gel.world.server'elab_body;
      E687 := E687 + 1;
      gel.world.simple'elab_spec;
      gel.world.simple'elab_body;
      E614 := E614 + 1;
      gel.applet'elab_spec;
      gel.applet'elab_body;
      E232 := E232 + 1;
      gel.applet.client_world'elab_spec;
      gel.applet.client_world'elab_body;
      E679 := E679 + 1;
      gel.applet.gui_and_sim_world'elab_spec;
      gel.applet.gui_and_sim_world'elab_body;
      E626 := E626 + 1;
      gel.applet.gui_world'elab_spec;
      gel.applet.gui_world'elab_body;
      E683 := E683 + 1;
      gel.applet.server_world'elab_spec;
      gel.applet.server_world'elab_body;
      E685 := E685 + 1;
      E628 := E628 + 1;
      XML'ELAB_SPEC;
      XML.READER'ELAB_SPEC;
      E763 := E763 + 1;
      XML'ELAB_BODY;
      E761 := E761 + 1;
      collada.document'elab_spec;
      collada.document'elab_body;
      E742 := E742 + 1;
      E738 := E738 + 1;
      Opengl.Model.Any'Elab_Spec;
      Opengl.Model.Any'Elab_Body;
      E736 := E736 + 1;
   end adainit;

   procedure Ada_Main_Program;
   pragma Import (Ada, Ada_Main_Program, "_ada_launch_full_demo");

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
   --   /workspaces/bench-source/src/lace/lace_math/library/build/any_math.o
   --   /workspaces/bench-source/src/lace/lace_math/library/build/any_math-any_algebra.o
   --   /workspaces/bench-source/src/lace/lace_math/library/build/any_math-any_algebra-any_linear.o
   --   /workspaces/bench-source/src/lace/lace_math/library/build/any_math-any_algebra-any_linear-any_d2.o
   --   /workspaces/bench-source/src/lace/lace_math/library/build/any_math-any_algebra-any_linear-any_d3.o
   --   /workspaces/bench-source/src/lace/lace_math/library/build/any_math-any_geometry.o
   --   /workspaces/bench-source/src/lace/lace_math/library/build/any_math-any_geometry-any_d2.o
   --   /workspaces/bench-source/src/lace/lace_math/library/build/any_math-any_geometry-any_d3.o
   --   /workspaces/bench-source/src/lace/lace_math/library/build/float_math.o
   --   /workspaces/bench-source/src/lace/lace_physics/library/build/box2d_physics.o
   --   /workspaces/bench-source/src/lace/lace_math/library/build/float_math-algebra.o
   --   /workspaces/bench-source/src/lace/lace_math/library/build/float_math-algebra-linear.o
   --   /workspaces/bench-source/src/lace/lace_math/library/build/float_math-algebra-linear-d2.o
   --   /workspaces/bench-source/src/lace/lace_math/library/build/float_math-algebra-linear-d3.o
   --   /workspaces/bench-source/src/lace/lace_math/library/build/float_math-geometry.o
   --   /workspaces/bench-source/src/lace/lace_math/library/build/float_math-geometry-d2.o
   --   /workspaces/bench-source/src/lace/lace_math/library/build/float_math-geometry-d3.o
   --   /workspaces/bench-source/src/lace/lace_physics/library/build/bullet_physics.o
   --   /workspaces/bench-source/src/lace/lace_opengl/private/freetype/library/build/freetype.o
   --   /workspaces/bench-source/src/lace/lace_opengl/private/gl/library/build/gl_types.o
   --   /workspaces/bench-source/src/lace/lace_opengl/private/gl/library/build/gl.o
   --   /workspaces/bench-source/src/lace/library/build/lace.o
   --   /workspaces/bench-source/src/lace/library/build/lace-any.o
   --   /workspaces/bench-source/src/lace/library/build/lace-event.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl.o
   --   /workspaces/bench-source/src/lace/lace_physics/library/build/physics.o
   --   /workspaces/bench-source/src/lace/lace_gel/library/build/gel.o
   --   /workspaces/bench-source/src/lace/lace_gel/library/build/gel-remote.o
   --   /workspaces/bench-source/src/lace/lace_physics/library/build/physics-remote.o
   --   /workspaces/bench-source/src/lace/lace_swig/library/build/swig.o
   --   /workspaces/bench-source/src/lace/lace_math/library/build/any_math-any_geometry-any_d3-any_modeller.o
   --   /workspaces/bench-source/src/lace/lace_math/library/build/any_math-any_geometry-any_d3-any_modeller-any_forge.o
   --   /workspaces/bench-source/src/lace_box2d/library/build/box2d_c.o
   --   /workspaces/bench-source/src/lace_box2d/library/build/box2d_c-joint_cursor.o
   --   /workspaces/bench-source/src/lace_box2d/library/build/box2d_c-pointers.o
   --   /workspaces/bench-source/src/lace_bullet/library/build/bullet_c.o
   --   /workspaces/bench-source/src/lace_bullet/library/build/bullet_c-pointers.o
   --   /workspaces/bench-source/src/lace_c_math/library/build/c_math_c.o
   --   /workspaces/bench-source/src/lace_c_math/library/build/c_math_c-pointers.o
   --   /workspaces/bench-source/src/lace_c_math/library/build/c_math_c-matrix_3x3.o
   --   /workspaces/bench-source/src/lace_c_math/library/build/c_math_c-matrix_4x4.o
   --   /workspaces/bench-source/src/lace_c_math/library/build/c_math_c-triangle.o
   --   /workspaces/bench-source/src/lace_c_math/library/build/c_math_c-vector_2.o
   --   /workspaces/bench-source/src/lace_c_math/library/build/c_math_c-vector_3.o
   --   /workspaces/bench-source/src/lace_box2d/library/build/box2d_c-b2d_contact.o
   --   /workspaces/bench-source/src/lace_box2d/library/build/box2d_c-b2d_ray_collision.o
   --   /workspaces/bench-source/src/lace_box2d/library/build/box2d_c-binding.o
   --   /workspaces/bench-source/src/lace_bullet/library/build/bullet_c-ray_collision.o
   --   /workspaces/bench-source/src/lace_bullet/library/build/bullet_c-binding.o
   --   /workspaces/bench-source/src/lace_c_math/library/build/c_math_c-conversion.o
   --   /workspaces/bench-source/src/lace/lace_collada/library/build/collada.o
   --   /workspaces/bench-source/src/lace/lace_collada/library/build/collada-asset.o
   --   /workspaces/bench-source/src/lace/lace_collada/library/build/collada-library.o
   --   /workspaces/bench-source/src/lace/lace_collada/library/build/collada-library-animations.o
   --   /workspaces/bench-source/src/lace/lace_collada/library/build/collada-library-controllers.o
   --   /workspaces/bench-source/src/lace/lace_collada/library/build/collada-library-geometries.o
   --   /workspaces/bench-source/src/lace/lace_collada/library/build/collada-library-visual_scenes.o
   --   /workspaces/bench-source/src/lace/lace_collada/library/build/collada-libraries.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/egl.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/egl-pointers.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/egl-nativedisplaytype.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/egl-binding.o
   --   /workspaces/bench-source/src/lace/lace_math/library/build/float_math-geometry-d3-modeller.o
   --   /workspaces/bench-source/src/lace/lace_math/library/build/float_math-geometry-d3-modeller-forge.o
   --   /workspaces/bench-source/src/lace/lace_opengl/private/freetype/library/build/freetype_c.o
   --   /workspaces/bench-source/src/lace/lace_opengl/private/freetype/library/build/freetype_c-ft_bbox.o
   --   /workspaces/bench-source/src/lace/lace_opengl/private/freetype/library/build/freetype_c-ft_bitmap.o
   --   /workspaces/bench-source/src/lace/lace_opengl/private/freetype/library/build/freetype_c-ft_charmaprec.o
   --   /workspaces/bench-source/src/lace/lace_opengl/private/freetype/library/build/freetype_c-ft_charmap.o
   --   /workspaces/bench-source/src/lace/lace_opengl/private/freetype/library/build/freetype_c-ft_size_metrics.o
   --   /workspaces/bench-source/src/lace/lace_opengl/private/freetype/library/build/freetype_c-ft_vector.o
   --   /workspaces/bench-source/src/lace/lace_opengl/private/freetype/library/build/freetype_c-pointers.o
   --   /workspaces/bench-source/src/lace/lace_opengl/private/freetype/library/build/freetype_c-ft_face.o
   --   /workspaces/bench-source/src/lace/lace_opengl/private/freetype/library/build/freetype_c-ft_glyphslot.o
   --   /workspaces/bench-source/src/lace/lace_opengl/private/freetype/library/build/freetype_c-ft_library.o
   --   /workspaces/bench-source/src/lace/lace_opengl/private/freetype/library/build/freetype_c-ft_size.o
   --   /workspaces/bench-source/src/lace/lace_opengl/private/freetype/library/build/freetype_c-binding.o
   --   /workspaces/bench-source/src/lace/lace_opengl/private/freetype/library/build/freetype-face_size.o
   --   /workspaces/bench-source/src/lace/lace_opengl/private/freetype/library/build/freetype-face.o
   --   /workspaces/bench-source/src/lace/lace_opengl/private/freetype/library/build/freetype-charmap.o
   --   /workspaces/bench-source/src/lace/lace_opengl/private/gid/obj_debug/gid-buffering.o
   --   /workspaces/bench-source/src/lace/lace_opengl/private/gid/obj_debug/gid-color_tables.o
   --   /workspaces/bench-source/src/lace/lace_opengl/private/gid/obj_debug/gid-decoding_bmp.o
   --   /workspaces/bench-source/src/lace/lace_opengl/private/gid/obj_debug/gid-decoding_gif.o
   --   /workspaces/bench-source/src/lace/lace_opengl/private/gid/obj_debug/gid-decoding_jpg.o
   --   /workspaces/bench-source/src/lace/lace_opengl/private/gid/obj_debug/gid-decoding_png-huffman.o
   --   /workspaces/bench-source/src/lace/lace_opengl/private/gid/obj_debug/gid-decoding_png.o
   --   /workspaces/bench-source/src/lace/lace_opengl/private/gid/obj_debug/gid-decoding_tga.o
   --   /workspaces/bench-source/src/lace/lace_opengl/private/gid/obj_debug/gid-headers.o
   --   /workspaces/bench-source/src/lace/lace_opengl/private/gid/obj_debug/gid.o
   --   /workspaces/bench-source/src/lace/lace_opengl/private/gl/library/build/gl-binding.o
   --   /workspaces/bench-source/src/lace/lace_opengl/private/gl/library/build/gl-lean.o
   --   /workspaces/bench-source/src/lace/lace_opengl/private/gl/library/build/gl-pointers.o
   --   /workspaces/bench-source/src/lace/lace_opengl/private/gl/library/build/gl-safe.o
   --   /workspaces/bench-source/src/lace/library/build/lace-response.o
   --   /workspaces/bench-source/src/lace/library/build/lace-event-logger.o
   --   /workspaces/bench-source/src/lace/library/build/lace-observer.o
   --   /workspaces/bench-source/src/lace/library/build/lace-subject.o
   --   /workspaces/bench-source/src/lace/lace_gel/library/build/gel-keyboard.o
   --   /workspaces/bench-source/src/lace/lace_gel/library/build/gel-mouse.o
   --   /workspaces/bench-source/src/lace/library/build/lace-event-utility.o
   --   /workspaces/bench-source/src/lace/library/build/lace-event-logger-text.o
   --   /workspaces/bench-source/src/lace/library/build/lace-make_observer.o
   --   /workspaces/bench-source/src/lace/library/build/lace-make_observer-deferred.o
   --   /workspaces/bench-source/src/lace/library/build/lace-make_subject.o
   --   /workspaces/bench-source/src/lace/library/build/lace-subject-local.o
   --   /workspaces/bench-source/src/lace/lace_gel/library/build/gel-keyboard-local.o
   --   /workspaces/bench-source/src/lace/lace_gel/library/build/gel-mouse-local.o
   --   /workspaces/bench-source/src/lace/library/build/lace-subject_and_deferred_observer.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-conversions.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-display.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-display-privvy.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-frustum.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-glyphimpl.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-glyph.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-glyph-container.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-font.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-fontimpl.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-images.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-palette.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-light.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-remote_model.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-screen.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-surface_profile.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-surface_profile-privvy.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-surface.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-surface-privvy.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-context.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-tasks.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-attribute.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-errors.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-buffer.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-buffer-general.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-buffer-indices.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-buffer-long_indices.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-buffer-short_indices.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-renderer.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-server.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-shader.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-variable.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-program.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-variable-uniform.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-program-lit.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-program-lit-colored_textured_skinned.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-program-lit-textured_skinned.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-viewport.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-io.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-texture.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-io-lat_long_radius.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-io-wavefront.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-primitive.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-primitive-indexed.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-primitive-long_indexed.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-geometry.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-geometry-colored.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-geometry-colored_textured.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-geometry-lit_colored.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-geometry-lit_colored_textured.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-geometry-lit_colored_textured_skinned.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-geometry-lit_textured.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-geometry-lit_textured_skinned.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-geometry-textured.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-glyphimpl-texture.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-glyph-texture.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-fontimpl-texture.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-font-texture.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-model.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-model-arrow.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-model-arrow-colored.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-model-billboard.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-model-billboard-colored_textured.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-model-billboard-textured.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-model-box.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-model-box-colored.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-model-box-textured.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-model-capsule.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-model-capsule-textured.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-model-line.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-model-line-colored.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-model-polygon.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-model-polygon-lit_colored.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-model-segment_line.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-model-sphere.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-model-sphere-colored.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-model-sphere-lit_colored.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-model-sphere-lit_colored_textured.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-model-sphere-textured.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-model-text.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-model-text-lit_colored.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-primitive-short_indexed.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-texture-coordinates.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-model-terrain.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-visual.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-culler.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-culler-frustum.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-camera.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-impostor.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-impostor-simple.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-impostor-terrain.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-impostorer.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-renderer-lean.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-renderer-lean-forge.o
   --   /workspaces/bench-source/src/lace/lace_physics/library/build/physics-remote-model.o
   --   /workspaces/bench-source/src/lace/lace_gel/library/build/gel-remote-world.o
   --   /workspaces/bench-source/src/lace/lace_gel/library/build/gel-events.o
   --   /workspaces/bench-source/src/lace/lace_gel/library/build/gel-window.o
   --   /workspaces/bench-source/src/lace/lace_gel/library/build/gel-window-sdl.o
   --   /workspaces/bench-source/src/lace/lace_gel/library/build/gel-window-setup.o
   --   /workspaces/bench-source/src/lace/lace_physics/library/build/physics-shape.o
   --   /workspaces/bench-source/src/lace/lace_physics/library/build/bullet_physics-shape.o
   --   /workspaces/bench-source/src/lace/lace_physics/library/build/physics-model.o
   --   /workspaces/bench-source/src/lace/lace_physics/library/build/physics-object.o
   --   /workspaces/bench-source/src/lace/lace_physics/library/build/bullet_physics-object.o
   --   /workspaces/bench-source/src/lace/lace_physics/library/build/physics-joint.o
   --   /workspaces/bench-source/src/lace/lace_physics/library/build/physics-joint-ball.o
   --   /workspaces/bench-source/src/lace/lace_physics/library/build/physics-joint-cone_twist.o
   --   /workspaces/bench-source/src/lace/lace_physics/library/build/physics-joint-dof6.o
   --   /workspaces/bench-source/src/lace/lace_physics/library/build/physics-joint-hinge.o
   --   /workspaces/bench-source/src/lace/lace_physics/library/build/physics-joint-slider.o
   --   /workspaces/bench-source/src/lace/lace_physics/library/build/bullet_physics-joint.o
   --   /workspaces/bench-source/src/lace/lace_physics/library/build/physics-space.o
   --   /workspaces/bench-source/src/lace/lace_physics/library/build/box2d_physics-shape.o
   --   /workspaces/bench-source/src/lace/lace_physics/library/build/box2d_physics-object.o
   --   /workspaces/bench-source/src/lace/lace_physics/library/build/box2d_physics-joint.o
   --   /workspaces/bench-source/src/lace/lace_physics/library/build/box2d_physics-space.o
   --   /workspaces/bench-source/src/lace/lace_physics/library/build/bullet_physics-space.o
   --   /workspaces/bench-source/src/lace/lace_physics/library/build/physics-forge.o
   --   /workspaces/bench-source/src/lace/lace_gel/library/build/gel-any_joint.o
   --   /workspaces/bench-source/src/lace/lace_gel/library/build/gel-hinge_joint.o
   --   /workspaces/bench-source/src/lace/lace_gel/library/build/gel-world.o
   --   /workspaces/bench-source/src/lace/lace_gel/library/build/gel-joint.o
   --   /workspaces/bench-source/src/lace/lace_gel/library/build/gel-sprite.o
   --   /workspaces/bench-source/src/lace/lace_gel/library/build/gel-camera.o
   --   /workspaces/bench-source/src/lace/lace_gel/library/build/gel-camera-forge.o
   --   /workspaces/bench-source/src/lace/lace_gel/library/build/gel-dolly.o
   --   /workspaces/bench-source/src/lace/lace_gel/library/build/gel-dolly-following.o
   --   /workspaces/bench-source/src/lace/lace_gel/library/build/gel-dolly-simple.o
   --   /workspaces/bench-source/src/lace/lace_gel/library/build/gel-world-client.o
   --   /workspaces/bench-source/src/lace/lace_gel/library/build/gel-world-server.o
   --   /workspaces/bench-source/src/lace/lace_gel/library/build/gel-world-simple.o
   --   /workspaces/bench-source/src/lace/lace_gel/library/build/gel-applet.o
   --   /workspaces/bench-source/src/lace/lace_gel/library/build/gel-applet-client_world.o
   --   /workspaces/bench-source/src/lace/lace_gel/library/build/gel-applet-gui_and_sim_world.o
   --   /workspaces/bench-source/src/lace/lace_gel/library/build/gel-applet-gui_world.o
   --   /workspaces/bench-source/src/lace/lace_gel/library/build/gel-applet-server_world.o
   --   /workspaces/bench-source/src/lace/lace_gel/library/build/gel-forge.o
   --   /workspaces/bench-source/src/lace/lace_xml/library/build/xml-reader.o
   --   /workspaces/bench-source/src/lace/lace_xml/library/build/xml.o
   --   /workspaces/bench-source/src/lace/lace_collada/library/build/collada-document.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-io-collada.o
   --   /workspaces/bench-source/src/lace/lace_opengl/library/build/opengl-model-any.o
   --   /workspaces/bench-source/src/lace_gel_full_demo/build/launch_full_demo.o
   --   -L/workspaces/bench-source/src/lace_gel_full_demo/build/
   --   -L/workspaces/bench-source/src/lace_gel_full_demo/build/
   --   -L/workspaces/bench-source/src/lace/lace_gel/library/build/
   --   -L/workspaces/bench-source/src/lace/library/build/
   --   -L/workspaces/bench-source/src/lace/lace_opengl/library/build/
   --   -L/workspaces/bench-source/src/lace/lace_opengl/private/gl/library/build/
   --   -L/workspaces/bench-source/src/lace/lace_opengl/private/gid/obj_debug/
   --   -L/workspaces/bench-source/src/lace/lace_math/library/build/
   --   -L/workspaces/bench-source/src/lace/lace_opengl/private/freetype/library/build/
   --   -L/workspaces/bench-source/src/lace/lace_collada/library/build/
   --   -L/workspaces/bench-source/src/lace/lace_xml/library/build/
   --   -L/workspaces/bench-source/src/lace/lace_physics/library/build/
   --   -L/workspaces/bench-source/src/lace_bullet/library/build/
   --   -L/workspaces/bench-source/src/lace_c_math/library/build/
   --   -L/workspaces/bench-source/src/lace/lace_swig/library/build/
   --   -L/workspaces/bench-source/src/lace_box2d/library/build/
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
