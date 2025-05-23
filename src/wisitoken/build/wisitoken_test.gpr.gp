--  Copyright (c) 2009, 2010, 2018 - 2021 Stephen Leake <stephen_leake@stephe-leake.org>
--
--  This program is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 3 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.

--  For running WisiToken tests.

with "aunit";
with "aunit_ext";
with "sal_devel";
with "wisitoken";
with "wisitoken_devel";
with "standard_common";
#if HAVE_TREE_SITTER="yes"
with "tree_sitter"; -- tree-sitter utils installed
#end if;
project WisiToken_Test is

   type Generate_Type is ("BNF_EBNF_Tree_Sitter", "BNF_EBNF", "BNF", "EBNF", "Tree_Sitter");
   Generate : Generate_Type := external
   ("GENERATE",
#if HAVE_TREE_SITTER="yes"
    "BNF_EBNF_Tree_Sitter"
#else
    "BNF_EBNF"
#end if;
   ); 

   for Source_Dirs use
     (
      ".",
      "../test",
      "../test/bnf"
     );

   for Languages use ("Ada", "C");
   
   --  These must be consistent with the same named lists in
   --  rules.make and the %generate statement in the corresponding
   --  *.wy file.
   Gen_BNF :=
     (
      "ada_lite_lalr_run.ads",
      "ada_lite_lr1_t1_run.ads",
      "ada_lite_lr1_t8_run.ads",
      "body_instantiation_conflict_lalr_run.ads",
      "body_instantiation_conflict_lr1_t1_run.ads",
      "body_instantiation_conflict_lr1_t8_run.ads",
      "body_instantiation_conflict_packrat_gen_run.ads",
      "body_instantiation_conflict_packrat_proc_run.ads",
      "case_expression_lalr_run.ads",
      "case_expression_lr1_t1_run.ads",
      "case_expression_packrat_gen_run.ads",
      "case_expression_packrat_proc_run.ads",
      "character_literal_lr1_t1_run.ads",
      "character_literal_packrat_gen_run.ads",
      "character_literal_packrat_proc_run.ads",
      "conflict_name_lalr_run.ads",
      "dragon_4_43_lalr_run.ads",
      "dragon_4_43_packrat_gen_run.ads",
      "dragon_4_43_packrat_proc_run.ads",
      "empty_production_1_lalr_run.ads",
      "empty_production_1_packrat_gen_run.ads",
      "empty_production_1_packrat_proc_run.ads",
      "empty_production_2_lalr_run.ads",
      "empty_production_2_packrat_gen_run.ads",
      "empty_production_2_packrat_proc_run.ads",
      "empty_production_3_lalr_run.ads",
      "empty_production_3_packrat_gen_run.ads",
      "empty_production_3_packrat_proc_run.ads",
      "empty_production_4_lalr_run.ads",
      "empty_production_4_packrat_gen_run.ads",
      "empty_production_4_packrat_proc_run.ads",
      "empty_production_5_lalr_run.ads",
      "empty_production_5_packrat_gen_run.ads",
      "empty_production_5_packrat_proc_run.ads",
      "empty_production_6_lalr_run.ads",
      "empty_production_6_packrat_gen_run.ads",
      "empty_production_6_packrat_proc_run.ads",
      "empty_production_7_lalr_run.ads",
      "empty_production_7_packrat_gen_run.ads",
      "empty_production_7_packrat_proc_run.ads",
      "empty_production_8_lalr_run.ads",
      "empty_production_8_packrat_gen_run.ads",
      "empty_production_8_packrat_proc_run.ads",
      "range_conflict_lalr_run.ads",
      "range_conflict_packrat_gen_run.ads",
      "range_conflict_packrat_proc_run.ads",
      "skip_to_grammar_lalr_run.ads",
      "skip_to_grammar_lr1_t1_run.ads",
      "warth_left_recurse_expr_1_lalr_run.ads",
      "warth_left_recurse_expr_1_packrat_gen_run.ads",
      "warth_left_recurse_expr_1_packrat_proc_run.ads"
     );

   Gen_EBNF :=
     (
      "ada_lite_ebnf_lalr_run.ads",
      "identifier_list_name_conflict_lalr_run.ads",
      "identifier_list_name_conflict_packrat_gen_run.ads",
      "identifier_list_name_conflict_packrat_proc_run.ads",
      "java_enum_ch19_lr1_t1_run.ads",
      "java_expressions_antlr_lr1_t1_run.ads",
      "java_expressions_ch19_lr1_t1_run.ads",
      "java_types_ch19_lr1_t1_run.ads",
      "java_types_ch19_lr1_t8_run.ads",
      "lalr_generator_bug_01_lalr_run.ads",
      "nested_ebnf_optional_lalr_run.ads",
      "three_action_conflict_lalr_run.ads"
     );

   Gen_Tree_Sitter :=
     (
      "ada_lite_ebnf_tree_sitter_run.adb",
      "ada_lite_tree_sitter_run.adb",
      "dragon_4_43_tree_sitter_run.adb"
     );
     
#if HAVE_TREE_SITTER="no"
   for Excluded_Source_Files use ("wisitoken_tree_sitter.c"); 
#end if;
     
   case Generate is
   when "BNF_EBNF_Tree_Sitter" =>
      for Main use Gen_BNF & Gen_EBNF & Gen_Tree_Sitter;
   when "BNF_EBNF" =>
      for Main use Gen_BNF & Gen_EBNF;
   when "BNF" =>
      for Main use Gen_BNF;
   when "EBNF" =>
      for Main use Gen_EBNF;
   when "Tree_Sitter" =>
      for Main use Gen_Tree_Sitter;
   end case;

   case Standard_Common.Profile is
   when "On" =>
      for Object_Dir use "obj_pro";
      for Exec_Dir use "exec_pro";

   when "Off" =>
      for Object_Dir use "obj";
      for Exec_Dir use ".";
   end case;

   package Compiler is
      for Default_Switches ("Ada") use WisiToken.Compiler'Default_Switches ("Ada");

      --  User actions are not properly indented; leave out style 'il' layout
      --  redundant with clauses; add -gnatwR
      Generated_Debug_Switches := ("-gnaty3abcefhkl120nOprtx") & Standard_Common.Compiler.Debug_Switches &
        ("-gnatwR", "-fno-var-tracking-assignments");
      Generated_Release_Switches := ("-gnaty3abcefhkl120nOprtx") & Standard_Common.Compiler.Release_Switches &
        ("-gnatwR");

      case Standard_Common.Build is
      when "Debug" =>
         for Default_Switches ("C") use Standard_Common.Compiler.Debug_Switches_C_Non_Pedantic & "-save-temps";

         for Switches ("character_literal_actions.adb") use Generated_Debug_Switches;
         for Switches ("character_literal_main.adb") use Generated_Debug_Switches;

         for Switches ("ada_lite_ebnf_lalr_main.adb") use Generated_Debug_Switches;
         for Switches ("ada_lite_lalr_main.adb") use Generated_Debug_Switches;
         for Switches ("java_expressions_antlr_lr1_main.adb") use Generated_Debug_Switches;
         for Switches ("java_expressions_ch19_lr1_main.adb") use Generated_Debug_Switches;
         for Switches ("java_types_ch19_lr1_main.adb") use Generated_Debug_Switches;

      when "Normal" =>
         for Default_Switches ("C") use Standard_Common.Compiler.Release_Switches_C_Non_Pedantic;

         for Switches ("character_literal_actions.adb") use Generated_Release_Switches;
         for Switches ("character_literal_main.adb") use Generated_Release_Switches;

         for Switches ("ada_lite_main.adb") use Generated_Release_Switches;
      end case;

   end Compiler;

   package Builder is
      case Standard_Common.Profile is
      when "On" =>
         for Default_Switches ("Ada") use Standard_Common.Builder'Default_Switches ("Ada") & ("-pg");

      when "Off" =>
         for Default_Switches ("Ada") use Standard_Common.Builder'Default_Switches ("Ada");
      end case;

      --  We use ".exe" extension even on non-Windows, to simplify the makefiles.
      for Executable_Suffix use ".exe";
   end Builder;

   package Binder is
      for Default_Switches ("Ada") use Standard_Common.Binder'Default_Switches ("Ada");
   end Binder;

end WisiToken_Test;
