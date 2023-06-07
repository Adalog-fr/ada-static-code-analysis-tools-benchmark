/*
 * Copyright (C) 2014-2022, AdaCore
 * SPDX-License-Identifier: Apache-2.0
 */

#ifndef LIBADALANG
#define LIBADALANG

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
 * This type represents a context for all source analysis. This is the first
 * type you need to create to use libadalang. It will contain the results of
 * all analysis, and is the main holder for all the data.
 *
 * You can create several analysis contexts if you need to, which enables you,
 * for example to:
 *
 * * analyze several different projects at the same time;
 *
 * * analyze different parts of the same projects in parallel.
 *
 * In the current design, contexts always keep all of their analysis units
 * allocated. If you need to get this memory released, the only option at your
 * disposal is to destroy your analysis context instance.
 *
 * This structure is partially opaque: some fields are exposed to allow direct
 * access, for performance concerns.
 */
typedef struct
{
   uint64_t serial_number;
} *ada_analysis_context;

/*
 * This type represents the analysis of a single file.
 *
 * This type has strong-reference semantics and is ref-counted. Furthermore, a
 * reference to a unit contains an implicit reference to the context that owns
 * it. This means that keeping a reference to a unit will keep the context and
 * all the unit it contains allocated.
 *
 * This structure is partially opaque: some fields are exposed to allow direct
 * access, for performance concerns.
 */
typedef struct
{
   uint64_t version_number;
} *ada_analysis_unit;

/*
 * Data type for all nodes. Nodes are assembled to make up a tree.  See the
 * node primitives below to inspect such trees.
 *
 * Unlike for contexts and units, this type has weak-reference semantics:
 * keeping a reference to a node has no effect on the decision to keep the unit
 * that it owns allocated. This means that once all references to the context
 * and units related to a node are dropped, the context and its units are
 * deallocated and the node becomes a stale reference: most operations on it
 * will raise a ``Stale_Reference_Error``.
 *
 * Note that since reparsing an analysis unit deallocates all the nodes it
 * contains, this operation makes all reference to these nodes stale as well.
 */
typedef void* ada_base_node;

/*
 * Kind of AST nodes in parse trees.
 */
typedef enum {
    

        /* ada_node (abstract)  */
        /*
         * Root node class for the Ada syntax tree.
         */
    

        /* abort_node (abstract)  */
        /*
         * Qualifier for the ``abort`` keyword.
         */
    

        /*

         */
        ada_abort_absent = 1,
    

        /*

         */
        ada_abort_present = 2,
    

        /* abstract_node (abstract)  */
        /*
         * Qualifier for the ``abstract`` keyword.
         */
    

        /*

         */
        ada_abstract_absent = 3,
    

        /*

         */
        ada_abstract_present = 4,
    

        /* ada_list (abstract)  */
        /*

         */
    

        /*
         * List of AdaNode.
         *
         * This list node can contain one of the following nodes:
         * ``ada_abstract_state_decl``, ``ada_abstract_subp_decl``,
         * ``ada_allocator``, ``ada_aspect_clause``, ``ada_attribute_ref``,
         * ``ada_base_aggregate``, ``ada_bin_op``, ``ada_body_node``,
         * ``ada_call_expr``, ``ada_char_literal``, ``ada_component_clause``,
         * ``ada_component_decl``, ``ada_concat_op``,
         * ``ada_concrete_type_decl``, ``ada_cond_expr``, ``ada_decl_expr``,
         * ``ada_dotted_name``, ``ada_entry_decl``, ``ada_error_decl``,
         * ``ada_exception_decl``, ``ada_exception_handler``,
         * ``ada_explicit_deref``, ``ada_generic_decl``,
         * ``ada_generic_formal``, ``ada_generic_instantiation``,
         * ``ada_generic_renaming_decl``, ``ada_identifier``,
         * ``ada_incomplete_type_decl``, ``ada_membership_expr``,
         * ``ada_null_component_decl``, ``ada_null_literal``,
         * ``ada_num_literal``, ``ada_number_decl``, ``ada_object_decl``,
         * ``ada_others_designator``, ``ada_package_decl``,
         * ``ada_package_renaming_decl``, ``ada_paren_abstract_state_decl``,
         * ``ada_paren_expr``, ``ada_pragma_node``,
         * ``ada_protected_type_decl``, ``ada_qual_expr``,
         * ``ada_quantified_expr``, ``ada_raise_expr``,
         * ``ada_reduce_attribute_ref``, ``ada_single_protected_decl``,
         * ``ada_single_task_decl``, ``ada_stmt``, ``ada_string_literal``,
         * ``ada_subp_decl``, ``ada_subtype_decl``, ``ada_subtype_indication``,
         * ``ada_target_name``, ``ada_task_type_decl``, ``ada_un_op``,
         * ``ada_update_attribute_ref``, ``ada_use_clause``,
         * ``ada_with_clause``
         */
        ada_ada_node_list = 5,
    

        /*
         * List of AbstractStateDecls.
         *
         * This list node can contain one of the following nodes:
         * ``ada_abstract_state_decl``, ``ada_paren_abstract_state_decl``
         */
        ada_abstract_state_decl_list = 6,
    

        /*
         * List of alternatives in a ``when ...`` clause.
         *
         * This list node can contain one of the following nodes:
         * ``ada_allocator``, ``ada_attribute_ref``, ``ada_base_aggregate``,
         * ``ada_bin_op``, ``ada_call_expr``, ``ada_char_literal``,
         * ``ada_concat_op``, ``ada_cond_expr``, ``ada_decl_expr``,
         * ``ada_discrete_subtype_indication``, ``ada_dotted_name``,
         * ``ada_explicit_deref``, ``ada_identifier``, ``ada_membership_expr``,
         * ``ada_null_literal``, ``ada_num_literal``,
         * ``ada_others_designator``, ``ada_paren_expr``, ``ada_qual_expr``,
         * ``ada_quantified_expr``, ``ada_raise_expr``,
         * ``ada_reduce_attribute_ref``, ``ada_string_literal``,
         * ``ada_target_name``, ``ada_un_op``, ``ada_update_attribute_ref``
         */
        ada_alternatives_list = 7,
    

        /*
         * List of constraints.
         *
         * This list node can contain one of the following nodes:
         * ``ada_attribute_ref``, ``ada_bin_op``, ``ada_call_expr``,
         * ``ada_char_literal``, ``ada_dotted_name``, ``ada_explicit_deref``,
         * ``ada_identifier``, ``ada_qual_expr``, ``ada_reduce_attribute_ref``,
         * ``ada_string_literal``, ``ada_subtype_indication``,
         * ``ada_target_name``, ``ada_update_attribute_ref``
         */
        ada_constraint_list = 8,
    

        /*
         * List of declarations.
         *
         * This list node can contain one of the following nodes:
         * ``ada_abstract_subp_decl``, ``ada_aspect_clause``,
         * ``ada_component_decl``, ``ada_entry_decl``, ``ada_expr_function``,
         * ``ada_null_subp_decl``, ``ada_pragma_node``, ``ada_subp_decl``,
         * ``ada_subp_renaming_decl``
         */
        ada_decl_list = 9,
    

        /*
         * List of statements.
         *
         * This list node can contain one of the following nodes:
         * ``ada_pragma_node``, ``ada_stmt``
         */
        ada_stmt_list = 10,
    

        /*
         * List of AspectAssoc.
         */
        ada_aspect_assoc_list = 11,
    

        /*
         * List of BaseAssoc.
         */
        ada_base_assoc_list = 12,
    

        /* basic_assoc_list (abstract)  */
        /*
         * List of BasicAssoc.
         */
    

        /*
         * List of associations.
         */
        ada_assoc_list = 13,
    

        /*
         * List of BasicDecl.
         *
         * This list node can contain one of the following nodes:
         * ``ada_number_decl``, ``ada_object_decl``,
         * ``ada_single_protected_decl``, ``ada_single_task_decl``
         */
        ada_basic_decl_list = 14,
    

        /*
         * List of CaseExprAlternative.
         */
        ada_case_expr_alternative_list = 15,
    

        /*
         * List of CaseStmtAlternative.
         */
        ada_case_stmt_alternative_list = 16,
    

        /*
         * List of CompilationUnit.
         */
        ada_compilation_unit_list = 17,
    

        /*
         * List of ConcatOperand.
         */
        ada_concat_operand_list = 18,
    

        /*
         * List of ContractCaseAssoc.
         */
        ada_contract_case_assoc_list = 19,
    

        /*
         * List of DefiningName.
         */
        ada_defining_name_list = 20,
    

        /*
         * List of DiscriminantSpec.
         */
        ada_discriminant_spec_list = 21,
    

        /*
         * List of ElsifExprPart.
         */
        ada_elsif_expr_part_list = 22,
    

        /*
         * List of ElsifStmtPart.
         */
        ada_elsif_stmt_part_list = 23,
    

        /*
         * List of EnumLiteralDecl.
         */
        ada_enum_literal_decl_list = 24,
    

        /* expr_list (abstract)  */
        /*
         * List of Expr.
         *
         * This list node can contain one of the following nodes:
         * ``ada_allocator``, ``ada_attribute_ref``, ``ada_base_aggregate``,
         * ``ada_bin_op``, ``ada_call_expr``, ``ada_char_literal``,
         * ``ada_concat_op``, ``ada_cond_expr``, ``ada_decl_expr``,
         * ``ada_discrete_subtype_name``, ``ada_dotted_name``,
         * ``ada_explicit_deref``, ``ada_identifier``, ``ada_null_literal``,
         * ``ada_num_literal``, ``ada_paren_expr``, ``ada_qual_expr``,
         * ``ada_quantified_expr``, ``ada_raise_expr``,
         * ``ada_reduce_attribute_ref``, ``ada_string_literal``,
         * ``ada_target_name``, ``ada_un_op``, ``ada_update_attribute_ref``
         */
    

        /*
         * List of alternatives in a membership test expression.
         *
         * This list node can contain one of the following nodes:
         * ``ada_allocator``, ``ada_attribute_ref``, ``ada_base_aggregate``,
         * ``ada_bin_op``, ``ada_call_expr``, ``ada_char_literal``,
         * ``ada_concat_op``, ``ada_cond_expr``, ``ada_decl_expr``,
         * ``ada_discrete_subtype_name``, ``ada_dotted_name``,
         * ``ada_explicit_deref``, ``ada_identifier``, ``ada_null_literal``,
         * ``ada_num_literal``, ``ada_paren_expr``, ``ada_qual_expr``,
         * ``ada_quantified_expr``, ``ada_raise_expr``,
         * ``ada_reduce_attribute_ref``, ``ada_string_literal``,
         * ``ada_target_name``, ``ada_un_op``, ``ada_update_attribute_ref``
         */
        ada_expr_alternatives_list = 25,
    

        /* identifier_list (abstract)  */
        /*
         * List of Identifier.
         */
    

        /*
         * List of discriminant associations.
         */
        ada_discriminant_choice_list = 26,
    

        /*
         * List of Name.
         *
         * This list node can contain one of the following nodes:
         * ``ada_attribute_ref``, ``ada_call_expr``, ``ada_char_literal``,
         * ``ada_dotted_name``, ``ada_explicit_deref``, ``ada_identifier``,
         * ``ada_qual_expr``, ``ada_reduce_attribute_ref``,
         * ``ada_string_literal``, ``ada_target_name``,
         * ``ada_update_attribute_ref``
         */
        ada_name_list = 27,
    

        /*
         * List of parents in a type declaration.
         *
         * This list node can contain one of the following nodes:
         * ``ada_char_literal``, ``ada_dotted_name``, ``ada_identifier``,
         * ``ada_string_literal``
         */
        ada_parent_list = 28,
    

        /*
         * List of ParamSpec.
         */
        ada_param_spec_list = 29,
    

        /*
         * List of Pragma.
         */
        ada_pragma_node_list = 30,
    

        /*
         * List of SelectWhenPart.
         */
        ada_select_when_part_list = 31,
    

        /*
         * List of UnconstrainedArrayIndex.
         */
        ada_unconstrained_array_index_list = 32,
    

        /*
         * List of Variant.
         */
        ada_variant_list = 33,
    

        /* aliased_node (abstract)  */
        /*
         * Qualifier for the ``aliased`` keyword.
         */
    

        /*

         */
        ada_aliased_absent = 34,
    

        /*

         */
        ada_aliased_present = 35,
    

        /* all_node (abstract)  */
        /*
         * Qualifier for the ``all`` keyword.
         */
    

        /*

         */
        ada_all_absent = 36,
    

        /*

         */
        ada_all_present = 37,
    

        /* array_indices (abstract)  */
        /*
         * Specification for array indexes (:rmlink:`3.6`).
         */
    

        /*
         * Constrained specification for array indexes (:rmlink:`3.6`).
         */
        ada_constrained_array_indices = 38,
    

        /*
         * Unconstrained specification for array indexes (:rmlink:`3.6`).
         */
        ada_unconstrained_array_indices = 39,
    

        /*
         * Name/expression association in an aspect.
         */
        ada_aspect_assoc = 40,
    

        /* aspect_clause (abstract)  */
        /*
         * Base class for aspect clauses.
         */
    

        /*
         * Representation clause (``for .. use at ...;``) (:rmlink:`13.5.1`).
         */
        ada_at_clause = 41,
    

        /*
         * Clause for an attribute definition (``for ...'Attribute use ...;``)
         * (:rmlink:`13.3`).
         */
        ada_attribute_def_clause = 42,
    

        /*
         * Representation clause for enumeration types (:rmlink:`13.4`).
         */
        ada_enum_rep_clause = 43,
    

        /*
         * Representation clause for a record type (:rmlink:`13.5.1`).
         */
        ada_record_rep_clause = 44,
    

        /*
         * List of aspects in a declaration (:rmlink:`13.1.1`).
         */
        ada_aspect_spec = 45,
    

        /* base_assoc (abstract)  */
        /*
         * Abstract class for a key/value association, where the value is an
         * expression.
         */
    

        /*
         * Single association for the ``Contract_Case`` aspect.
         */
        ada_contract_case_assoc = 46,
    

        /*
         * Argument assocation in a pragma.
         */
        ada_pragma_argument_assoc = 47,
    

        /* base_formal_param_holder (abstract)  */
        /*
         * Base class for lists of formal parameters. This is used in every
         * case a list of "formals" can be called or instantiated, so in all
         * the following cases:
         *
         * * Subprogram specifications (and subprogram calls).
         *
         * * Component lists (and aggregates).
         *
         * * Generic formals (and generic instantiations).
         *
         * This allows to share the parameter unpacking/matching logic.
         *
         * This is a Libadalang abstraction that has no existence in the Ada
         * reference manual.
         */
    

        /* base_subp_spec (abstract)  */
        /*
         * Base class for subprogram specifications (:rmlink:`6.1`).
         */
    

        /*
         * Entry specification.
         *
         * This node does not have ARM existence, because in the RM subprogram
         * specifications don't encompass the ad-hoc specifications that happen
         * in entry declarations. Entry declarations are described in
         * :rmlink:`9.5.2`.
         */
        ada_entry_spec = 48,
    

        /*
         * Synthetic node for the abstract subprogram spec of an enum literal.
         *
         * NOTE: This has no existence in the ARM. While enum literals are
         * functions semantically, they're not such syntactically.
         */
        ada_enum_subp_spec = 49,
    

        /*
         * Subprogram specification (:rmlink:`6.1`).
         */
        ada_subp_spec = 50,
    

        /*
         * Synthetic subprogram specification for binary operators.
         */
        ada_synthetic_binary_spec = 51,
    

        /*
         * Synthetic subprogram specification for unary operators.
         */
        ada_synthetic_unary_spec = 52,
    

        /*
         * List of component declarations (:rmlink:`3.8`).
         */
        ada_component_list = 53,
    

        /* discriminant_part (abstract)  */
        /*
         * Specification for discriminants in type declarations.
         */
    

        /*
         * Known list of discriminants in type declarations (:rmlink:`3.7`).
         */
        ada_known_discriminant_part = 54,
    

        /*
         * Unknown list of discriminants in type declarations (:rmlink:`3.7`).
         */
        ada_unknown_discriminant_part = 55,
    

        /*
         * Formal parameters for the completion of an ``EntryDecl`` (either an
         * ``EntryBody`` or an ``AcceptStmt``).
         */
        ada_entry_completion_formal_params = 56,
    

        /*
         * List of declaration for generic formals (:rmlink:`12.1`).
         */
        ada_generic_formal_part = 57,
    

        /* base_record_def (abstract)  */
        /*
         * Base class for record definitions (:rmlink:`3.8`).
         */
    

        /*
         * Record definition for ``null record``.
         */
        ada_null_record_def = 58,
    

        /*
         * Record definition that contains components (``record ... end
         * record``).
         */
        ada_record_def = 59,
    

        /* basic_assoc (abstract)  */
        /*
         * Association of one or several names to an expression.
         */
    

        /*
         * Assocation (X => Y) used for aggregates associations
         * (:rmlink:`4.3`).
         */
        ada_aggregate_assoc = 60,
    

        /*
         * Association used for multi-dimension array aggregates.
         */
        ada_multi_dim_array_assoc = 61,
    

        /*
         * Association of discriminant names to an expression
         * (:rmlink:`3.7.1`).
         */
        ada_composite_constraint_assoc = 62,
    

        /*
         * Iterated association (Ada 2020, :rmlink:`4.3.3`).
         */
        ada_iterated_assoc = 63,
    

        /*
         * Assocation (X => Y) used for parameter associations (:rmlink:`6.4`).
         */
        ada_param_assoc = 64,
    

        /* basic_decl (abstract)  */
        /*
         * Root class for an Ada declaration (:rmlink:`3.1`). A declaration
         * associates a name with a language entity, for example a type or a
         * variable.
         */
    

        /*
         * Contained (directly or indirectly) in an AbstractStateDeclExpr, and
         * is used to represent the BasicDecl associated with the abstract
         * state introduced by the Abstract_State aspect. This node is
         * necessary because all of our name resolution routines expect
         * BasicDecls as environments' values.
         *
         * The only purpose of this node is to populate the env with the
         * abstract state declared through this node, so it can be referred in
         * SPARK aspects such as Global, Depends, Refined_State, etc.
         */
        ada_abstract_state_decl = 65,
    

        /*
         * Represents a anonymous declaration that holds an expression.
         *
         * This is used to store the results of queries such as
         * ``referenced_decl`` called on references to object formals from
         * inside a instantiated generic in order to return the relevant
         * actual.
         *
         * Indeed, ``referenced_decl`` must return a ``BasicDecl``, but actuals
         * of generic instantiations are ``Expr``. This wrapper node is
         * therefore a way to both satisfy the ``BasicDecl`` interface, and
         * provide to the user the expression of the actual through the
         * ``expr`` field.
         */
        ada_anonymous_expr_decl = 66,
    

        /* base_formal_param_decl (abstract)  */
        /*
         * Base class for formal parameter declarations. This is used both for
         * records components and for subprogram parameters.
         *
         * This is a Libadalang abstraction, that has no ARM existence.
         */
    

        /*
         * Declaration for a component (:rmlink:`3.8`).
         */
        ada_component_decl = 67,
    

        /*
         * Known list of discriminants in type declarations (:rmlink:`3.7`).
         */
        ada_discriminant_spec = 68,
    

        /* generic_formal (abstract)  */
        /*
         * Enclosing declaration for a generic formal. The real declaration is
         * accessible via the ``decl`` field.
         */
    

        /*
         * Formal declaration for an object.
         */
        ada_generic_formal_obj_decl = 69,
    

        /*
         * Formal declaration for a package (:rmlink:`12.1`).
         */
        ada_generic_formal_package = 70,
    

        /*
         * Formal declaration for a subprogram (:rmlink:`12.1`).
         */
        ada_generic_formal_subp_decl = 71,
    

        /*
         * Formal declaration for a type (:rmlink:`12.1`).
         */
        ada_generic_formal_type_decl = 72,
    

        /*
         * Specification for a parameter (:rmlink:`6.1`).
         */
        ada_param_spec = 73,
    

        /*
         * Synthetic parameter declaration.
         */
        ada_synthetic_formal_param_decl = 74,
    

        /* base_package_decl (abstract)  */
        /*
         * Base class for package declarations. This will be used both for non-
         * generic package declarations (via ``ada_package_decl``) and for
         * generic ones (via ``ada_generic_package_internal``).
         */
    

        /*
         * This class denotes the internal package contained by a
         * GenericPackageDecl.
         */
        ada_generic_package_internal = 75,
    

        /*
         * Non-generic package declarations (:rmlink:`7.1`).
         */
        ada_package_decl = 76,
    

        /* base_type_decl (abstract)  */
        /*
         * Base class for type declarations. It unifies every kind of type that
         * exists in Ada, including types that have no source existence like
         * classwide types.
         */
    

        /* base_subtype_decl (abstract)  */
        /*
         * Base class for subtype declarations (:rmlink:`3.2.2`).
         */
    

        /*
         * Specific ``BaseSubtypeDecl`` synthetic subclass for the base type of
         * scalar types.
         */
        ada_discrete_base_subtype_decl = 77,
    

        /*
         * Subtype declaration (:rmlink:`3.2.2`).
         */
        ada_subtype_decl = 78,
    

        /*
         * Synthetic node (not parsed, generated from a property call). Refers
         * to the classwide type for a given tagged type (:rmlink:`3.4.1`).
         */
        ada_classwide_type_decl = 79,
    

        /*
         * Incomplete declaration for a type (:rmlink:`12.5`).
         */
        ada_incomplete_type_decl = 80,
    

        /*
         * A formal incomplete type declaration.
         */
        ada_incomplete_formal_type_decl = 81,
    

        /*
         * Incomplete declaration for a tagged type.
         */
        ada_incomplete_tagged_type_decl = 82,
    

        /*
         * Declaration for a protected type (:rmlink:`9.4`).
         */
        ada_protected_type_decl = 83,
    

        /*
         * Declaration for a task type (:rmlink:`9.1`).
         */
        ada_task_type_decl = 84,
    

        /*
         * Type declaration for a single task (:rmlink:`9.1`).
         */
        ada_single_task_type_decl = 85,
    

        /* type_decl (abstract)  */
        /*
         * Type declarations that embed a type definition node. Corresponds to
         * the ARM's full type declarations (:rmlink:`3.2.1`).
         */
    

        /*
         * Anonymous type declaration (for anonymous array or access types).
         * This class has no RM existence, and anonymous (sub)types are refered
         * to implicitly in the RM.
         */
        ada_anonymous_type_decl = 86,
    

        /*
         * Synthetic anonymous type decl. Used to generate anonymous access
         * types.
         */
        ada_synth_anonymous_type_decl = 87,
    

        /*
         * A concrete type declaration.
         */
        ada_concrete_type_decl = 88,
    

        /*
         * A formal type declaration.
         */
        ada_formal_type_decl = 89,
    

        /* basic_subp_decl (abstract)  */
        /*
         * Base class for subprogram declarations.
         */
    

        /* classic_subp_decl (abstract)  */
        /*
         * This is an intermediate abstract class for subprogram declarations
         * with a common structure: overriding indicator, ``SubpSpec``,
         * aspects, <other fields>.
         */
    

        /*
         * Declaration for an abstract subprogram (:rmlink:`3.9.3`).
         */
        ada_abstract_subp_decl = 90,
    

        /* formal_subp_decl (abstract)  */
        /*
         * Formal subprogram declarations, in generic declarations formal parts
         * (:rmlink:`12.6`).
         */
    

        /*
         * Formal declaration for an abstract subprogram (:rmlink:`12.6`).
         */
        ada_abstract_formal_subp_decl = 91,
    

        /*
         * Formal declaration for a concrete subprogram (:rmlink:`12.6`).
         */
        ada_concrete_formal_subp_decl = 92,
    

        /*
         * Regular subprogram declaration (:rmlink:`6.1`).
         */
        ada_subp_decl = 93,
    

        /*
         * Entry declaration (:rmlink:`9.4`).
         */
        ada_entry_decl = 94,
    

        /*
         * Declaration for an enumeration literal (:rmlink:`3.5.1`).
         */
        ada_enum_literal_decl = 95,
    

        /*
         * Synthetic character enum literal declaration.
         */
        ada_synthetic_char_enum_lit = 96,
    

        /*
         * Internal node for generic subprograms.
         */
        ada_generic_subp_internal = 97,
    

        /*
         * Synthetic subprogram declaration.
         *
         * Is used to represent predefined operators. This should also be
         * usable for synthesizing function attributes.
         */
        ada_synthetic_subp_decl = 98,
    

        /* body_node (abstract)  */
        /*
         * Base class for an Ada body (:rmlink:`3.11`). A body is the
         * completion of a declaration.
         */
    

        /* base_subp_body (abstract)  */
        /*
         * Base class for subprogram bodies (:rmlink:`6.3`).
         */
    

        /*
         * Expression function (:rmlink:`6.8`).
         */
        ada_expr_function = 99,
    

        /*
         * Declaration for a null subprogram (:rmlink:`6.1`).
         */
        ada_null_subp_decl = 100,
    

        /*
         * Subprogram body(:rmlink:`6.3`) .
         */
        ada_subp_body = 101,
    

        /*
         * Declaration for a subprogram renaming (:rmlink:`8.5.4`).
         */
        ada_subp_renaming_decl = 102,
    

        /* body_stub (abstract)  */
        /*
         * Base class for a body stub (:rmlink:`10.1.3`). A body stub is meant
         * to be completed by .
         */
    

        /*
         * Stub for a package body (``is separate``) (:rmlink:`10.1.3`).
         */
        ada_package_body_stub = 103,
    

        /*
         * Stub for a protected object body (``is separate``)
         * (:rmlink:`10.1.3`).
         */
        ada_protected_body_stub = 104,
    

        /*
         * Stub for a subprogram body (``is separate``) (:rmlink:`10.1.3`).
         */
        ada_subp_body_stub = 105,
    

        /*
         * Stub for a task body (``is separate``) (:rmlink:`10.1.3`).
         */
        ada_task_body_stub = 106,
    

        /*
         * Entry body (:rmlink:`9.5.2`).
         */
        ada_entry_body = 107,
    

        /*
         * Package body (:rmlink:`7.2`).
         */
        ada_package_body = 108,
    

        /*
         * Protected object body (:rmlink:`9.4`).
         */
        ada_protected_body = 109,
    

        /*
         * Task body (:rmlink:`9.1`).
         */
        ada_task_body = 110,
    

        /*
         * Index specification for an entry body (:rmlink:`9.5.2`).
         */
        ada_entry_index_spec = 111,
    

        /*
         * Placeholder node for syntax errors in lists of declarations.
         */
        ada_error_decl = 112,
    

        /*
         * Exception declarations (:rmlink:`11.1`).
         */
        ada_exception_decl = 113,
    

        /*
         * Exception handler (:rmlink:`11.2`).
         */
        ada_exception_handler = 114,
    

        /*
         * Declaration for the controlling variable in a ``for`` loop
         * (:rmlink:`5.5`).
         */
        ada_for_loop_var_decl = 115,
    

        /* generic_decl (abstract)  */
        /*
         * Base class for generic declarations (:rmlink:`12.1`).
         */
    

        /*
         * Generic package declaration (:rmlink:`12.1`).
         */
        ada_generic_package_decl = 116,
    

        /*
         * Generic subprogram declaration (:rmlink:`12.1`).
         */
        ada_generic_subp_decl = 117,
    

        /* generic_instantiation (abstract)  */
        /*
         * Instantiations of generics (:rmlink:`12.3`).
         */
    

        /*
         * Instantiations of a generic package.
         */
        ada_generic_package_instantiation = 118,
    

        /*
         * Instantiations of a generic subprogram .
         */
        ada_generic_subp_instantiation = 119,
    

        /* generic_renaming_decl (abstract)  */
        /*
         * Base node for all generic renaming declarations (:rmlink:`8.5.5`).
         */
    

        /*
         * Declaration for a generic package renaming (:rmlink:`8.5.5`).
         */
        ada_generic_package_renaming_decl = 120,
    

        /*
         * Declaration for a generic subprogram renaming.
         */
        ada_generic_subp_renaming_decl = 121,
    

        /*
         * Declaration for a code label (:rmlink:`5.1`).
         */
        ada_label_decl = 122,
    

        /*
         * BasicDecl that is always the declaration inside a named statement.
         */
        ada_named_stmt_decl = 123,
    

        /*
         * Declaration for a static constant number (:rmlink:`3.3.2`).
         */
        ada_number_decl = 124,
    

        /*
         * Base class for Ada object declarations (:rmlink:`3.3.1`). Ada object
         * declarations are variables/constants declarations that can be
         * declared in any declarative scope.
         */
        ada_object_decl = 125,
    

        /*
         * Object declaration that is part of an extended return statement
         * (:rmlink:`6.5`).
         */
        ada_extended_return_stmt_object_decl = 126,
    

        /*
         * Object declaration without subtype indication. This node has been
         * introduced to cover a special case for ``ObjectDecl``, where
         * ``type_expr`` is made optional (AI12-0275), and therefore cannot fit
         * in an ``ObjectDecl``.
         */
        ada_no_type_object_renaming_decl = 127,
    

        /*
         * Declaration for a package renaming (:rmlink:`8.5.3`).
         */
        ada_package_renaming_decl = 128,
    

        /*
         * Declaration for a single protected object (:rmlink:`9.4`).
         */
        ada_single_protected_decl = 129,
    

        /*
         * Declaration for a single task (:rmlink:`9.1`).
         */
        ada_single_task_decl = 130,
    

        /*
         * Alternative in a ``case`` statement (``when ... => ...``).
         */
        ada_case_stmt_alternative = 131,
    

        /*
         * Root node for all Ada analysis units (:rmlink:`10.1.1`).
         */
        ada_compilation_unit = 132,
    

        /*
         * Representation clause for a single component (:rmlink:`13.5.1`).
         */
        ada_component_clause = 133,
    

        /*
         * Definition for a component (:rmlink:`3.6`).
         */
        ada_component_def = 134,
    

        /* constant_node (abstract)  */
        /*
         * Qualifier for the ``constant`` keyword.
         */
    

        /*

         */
        ada_constant_absent = 135,
    

        /*

         */
        ada_constant_present = 136,
    

        /* constraint (abstract)  */
        /*
         * Base class for type constraints (:rmlink:`3.2.2`).
         */
    

        /*
         * Constraint for a composite type (:rmlink:`3.6.1`). Due to
         * ambiguities in the Ada grammar, this could be either a list of index
         * constraints, if the owning type is an array type, or a list of
         * discriminant constraints, if the owning type is a discriminated
         * record type.
         */
        ada_composite_constraint = 137,
    

        /*
         * Delta and range type constraint (:rmlink:`J.3`).
         */
        ada_delta_constraint = 138,
    

        /*
         * Digits and range type constraint (:rmlink:`3.5.9`).
         */
        ada_digits_constraint = 139,
    

        /*
         * Range-based type constraint (:rmlink:`3.5`).
         */
        ada_range_constraint = 140,
    

        /*
         * List of declarations (:rmlink:`3.11`).
         */
        ada_declarative_part = 141,
    

        /*
         * List of declarations in a private part.
         */
        ada_private_part = 142,
    

        /*
         * List of declarations in a public part.
         */
        ada_public_part = 143,
    

        /*
         * ``elsif`` block, part of an ``if`` expression.
         */
        ada_elsif_expr_part = 144,
    

        /*
         * ``elsif`` part in an ``if`` statement block.
         */
        ada_elsif_stmt_part = 145,
    

        /* expr (abstract)  */
        /*
         * Base class for expressions (:rmlink:`4.4`).
         */
    

        /*
         * Directly corresponds to the right-hand side of the Abstract_State
         * aspect. Only exists because the RHS of an AspectAssoc must be an
         * expression: the actual logic is in AbstractStateDecl.
         */
        ada_abstract_state_decl_expr = 146,
    

        /*
         * Allocator expression (``new ...``) (:rmlink:`4.8`).
         */
        ada_allocator = 147,
    

        /* base_aggregate (abstract)  */
        /*
         * Base class for aggregates (:rmlink:`4.3`).
         */
    

        /*
         * Aggregate that is not a ``null record`` aggregate (:rmlink:`4.3`).
         */
        ada_aggregate = 148,
    

        /*
         * Bracket array or container aggregate (Ada 2020, :rmlink:`4.3`).
         */
        ada_bracket_aggregate = 149,
    

        /*
         * Aggregate for delta aggregate (Ada 2022, :rmlink:`4.3`).
         */
        ada_delta_aggregate = 150,
    

        /*
         * Bracket delta aggregate (Ada 2020, :rmlink:`4.3`).
         */
        ada_bracket_delta_aggregate = 151,
    

        /*
         * Aggregate for ``null record`` (:rmlink:`4.3`).
         */
        ada_null_record_aggregate = 152,
    

        /*
         * Binary expression.
         *
         * This encompasses several ARM expressions, because it is used for
         * every binary expression in Ada, all documented in ::rmlink:`4.4`.
         */
        ada_bin_op = 153,
    

        /*
         * Binary operation that compares two value, producing a boolean
         * (:rmlink:`4.4`).
         */
        ada_relation_op = 154,
    

        /*
         * Box expression (``<>``).
         *
         * This is not an expression per-se in Ada, but treating it as one
         * helps us keep coherent types in some cases, like aggregates
         * expressions.
         */
        ada_box_expr = 155,
    

        /*
         * Alternative in a ``case`` expression (``when ... => ...``).
         */
        ada_case_expr_alternative = 156,
    

        /*
         * Concatenation expression.
         *
         * Since concatenation expression can be huge in practice, this node
         * handles them as a list of operands rather than a deep tree of binary
         * operators, in order to avoid crashes while parsing of running name
         * resolution on such huge expression.
         *
         * The purpose of this node is to replace the arbitraty too deep tree
         * of binary operators (which can lead to a stack overflow), as for
         * example with ``"A & B & C & D & E"``:
         *
         * .. code::
         *
         *    BinOp(
         *      Binop(
         *        BinOp(
         *          BinOp(A, &, B), & , C), &, D), &, E)
         *
         * by a single operator, handling a list of operands that can be
         * processed without having to perform deep recursions:
         *
         * .. code::
         *
         *    ConcatOp(A,
         *      ConcatOperand(&, B),
         *      ConcatOperand(&, C),
         *      ConcatOperand(&, D),
         *      ConcatOperand(&, E))
         */
        ada_concat_op = 157,
    

        /*
         * A concatenation operator and its RHS operand.
         *
         * This node is used to represent the tuple ("&", operand) used by the
         * ``ConcatOp`` node to store its ``other_operands`` list.
         */
        ada_concat_operand = 158,
    

        /* cond_expr (abstract)  */
        /*
         * Base class for a conditional expressions (:rmlink:`4.5.7`).
         */
    

        /*
         * ``case`` expression (:rmlink:`4.5.7`).
         */
        ada_case_expr = 159,
    

        /*
         * ``if`` expression (:rmlink`4.5.7`).
         */
        ada_if_expr = 160,
    

        /*
         * List of associations for the ``Contract_Case`` aspect.
         *
         * Contract cases is a non standard Ada extension that's mainly useful
         * in SPARK. See the SPARK RM for more details.
         */
        ada_contract_cases = 161,
    

        /*
         * Declare expression (Ada 2020, :rmlink:`4.5.9`).
         */
        ada_decl_expr = 162,
    

        /*
         * Represent a membership test (in/not in operators) (:rmlink:`4.4`).
         *
         * Note that we don't consider them as binary operators since multiple
         * expressions on the right hand side are allowed.
         */
        ada_membership_expr = 163,
    

        /* name (abstract)  */
        /*
         * Base class for names (:rmlink:`4.1`).
         */
    

        /*
         * Expression to reference an attribute (:rmlink:`4.1.4`).
         */
        ada_attribute_ref = 164,
    

        /*
         * Represent a syntactic call expression.
         *
         * At the semantic level, this can be either a subprogram call, an
         * array subcomponent access expression, an array slice or a type
         * conversion, all described in :rmlink:`4.1`, except for subprogram
         * call statements, described in :rmlink:`6.4`.
         */
        ada_call_expr = 165,
    

        /*
         * Name that defines an entity (:rmlink:`3.1`).
         */
        ada_defining_name = 166,
    

        /*
         * Synthetic DefiningName.
         */
        ada_synthetic_defining_name = 167,
    

        /*
         * Subtype name for membership test expressions (:rmlink:`3.6`).
         */
        ada_discrete_subtype_name = 168,
    

        /*
         * Name to select a suffix in a prefix (:rmlink:`4.1.3`).
         */
        ada_dotted_name = 169,
    

        /*
         * Entity name in ``end ...;`` syntactic constructs.
         */
        ada_end_name = 170,
    

        /*
         * Explicit dereference expression (``.all``) (:rmlink:`4.1`).
         */
        ada_explicit_deref = 171,
    

        /*
         * Qualified expression (``...'(...)``) .(:rmlink:`4.7`).
         */
        ada_qual_expr = 172,
    

        /*
         * Reduction expression (``Reduce`` attribute). Ada 2022, RM 4.5.10.
         */
        ada_reduce_attribute_ref = 173,
    

        /* single_tok_node (abstract)  */
        /*
         * Base class for nodes that are made up of a single token.
         */
    

        /* base_id (abstract)  */
        /*
         * Base class for identifiers.
         */
    

        /*
         * Character literal (:rmlink:`4.1`).
         */
        ada_char_literal = 174,
    

        /*
         * Regular identifier (:rmlink:`2.3`).
         */
        ada_identifier = 175,
    

        /* op (abstract)  */
        /*
         * Operation in a binary expression.
         *
         * Note that the ARM does not consider "double_dot" ("..") as a binary
         * operator, but we process it this way here anyway to keep things
         * simple.
         */
    

        /*

         */
        ada_op_abs = 176,
    

        /*

         */
        ada_op_and = 177,
    

        /*

         */
        ada_op_and_then = 178,
    

        /*

         */
        ada_op_concat = 179,
    

        /*

         */
        ada_op_div = 180,
    

        /*

         */
        ada_op_double_dot = 181,
    

        /*

         */
        ada_op_eq = 182,
    

        /*

         */
        ada_op_gt = 183,
    

        /*

         */
        ada_op_gte = 184,
    

        /*

         */
        ada_op_in = 185,
    

        /*

         */
        ada_op_lt = 186,
    

        /*

         */
        ada_op_lte = 187,
    

        /*

         */
        ada_op_minus = 188,
    

        /*

         */
        ada_op_mod = 189,
    

        /*

         */
        ada_op_mult = 190,
    

        /*

         */
        ada_op_neq = 191,
    

        /*

         */
        ada_op_not = 192,
    

        /*

         */
        ada_op_not_in = 193,
    

        /*

         */
        ada_op_or = 194,
    

        /*

         */
        ada_op_or_else = 195,
    

        /*

         */
        ada_op_plus = 196,
    

        /*

         */
        ada_op_pow = 197,
    

        /*

         */
        ada_op_rem = 198,
    

        /*

         */
        ada_op_xor = 199,
    

        /*
         * String literal (:rmlink:`2.6`).
         */
        ada_string_literal = 200,
    

        /*
         * The ``null`` literal (:rmlink:`4.4`).
         */
        ada_null_literal = 201,
    

        /* num_literal (abstract)  */
        /*
         * Base class for number literals (:rmlink:`2.4`).
         */
    

        /*
         * Literal for an integer (:rmlink:`2.4`).
         */
        ada_int_literal = 202,
    

        /*
         * Literal for a real number (:rmlink:`2.4`).
         */
        ada_real_literal = 203,
    

        /*
         * Synthetic identifier.
         */
        ada_synthetic_identifier = 204,
    

        /*
         * Name for Ada 2020 ``@`` (:rmlink:`5.2.1`).
         */
        ada_target_name = 205,
    

        /*
         * Reference to the ``Update`` attribute, which is a non standard GNAT
         * attribute.
         */
        ada_update_attribute_ref = 206,
    

        /*
         * Parenthesized expression.
         */
        ada_paren_expr = 207,
    

        /*
         * Quantified expression (:rmlink:`4.5.8`).
         */
        ada_quantified_expr = 208,
    

        /*
         * Expression to raise an exception (:rmlink:`4.4`).
         */
        ada_raise_expr = 209,
    

        /*
         * Unary expression.
         *
         * This encompasses several ARM expressions, because it is used for
         * every unary operator in Ada. Those expressions are all documented in
         * :rmlink:`4.4`.
         */
        ada_un_op = 210,
    

        /*
         * List of statements, with optional exception handlers
         * (:rmlink:`11.2`).
         */
        ada_handled_stmts = 211,
    

        /* interface_kind (abstract)  */
        /*
         * Kind of interface type.
         */
    

        /*

         */
        ada_interface_kind_limited = 212,
    

        /*

         */
        ada_interface_kind_protected = 213,
    

        /*

         */
        ada_interface_kind_synchronized = 214,
    

        /*

         */
        ada_interface_kind_task = 215,
    

        /* iter_type (abstract)  */
        /*
         * Iteration type for ``for`` loops.
         */
    

        /*

         */
        ada_iter_type_in = 216,
    

        /*

         */
        ada_iter_type_of = 217,
    

        /*
         * Library item in a compilation unit (:rmlink:`10.1.1`).
         */
        ada_library_item = 218,
    

        /* limited_node (abstract)  */
        /*
         * Qualifier for the ``limited`` keyword.
         */
    

        /*

         */
        ada_limited_absent = 219,
    

        /*

         */
        ada_limited_present = 220,
    

        /* loop_spec (abstract)  */
        /*
         * Base class for loop specifications (:rmlink:`5.5`).
         */
    

        /*
         * Specification for a ``for`` loop (:rmlink:`5.5`).
         */
        ada_for_loop_spec = 221,
    

        /*
         * Specification for a ``while`` loop (:rmlink:`5.5`).
         */
        ada_while_loop_spec = 222,
    

        /* mode (abstract)  */
        /*
         * Syntactic indicators for passing modes in formals (:rmlink:`6.1`).
         */
    

        /*

         */
        ada_mode_default = 223,
    

        /*

         */
        ada_mode_in = 224,
    

        /*

         */
        ada_mode_in_out = 225,
    

        /*

         */
        ada_mode_out = 226,
    

        /*
         * Node that holds several AbstractStateDecl nodes, which is necessary
         * when the Abstract_State aspect is associated with an aggregate in
         * order to declare a list of abstract states.
         */
        ada_multi_abstract_state_decl = 227,
    

        /* not_null (abstract)  */
        /*
         * Qualifier for the ``not null`` keywords.
         */
    

        /*

         */
        ada_not_null_absent = 228,
    

        /*

         */
        ada_not_null_present = 229,
    

        /*
         * Placeholder for the ``null`` in lists of components (:rmlink:`3.8`).
         */
        ada_null_component_decl = 230,
    

        /*
         * ``other`` designator.
         */
        ada_others_designator = 231,
    

        /* overriding_node (abstract)  */
        /*
         * Syntactic indicators for subprogram overriding modes.
         */
    

        /*

         */
        ada_overriding_not_overriding = 232,
    

        /*

         */
        ada_overriding_overriding = 233,
    

        /*

         */
        ada_overriding_unspecified = 234,
    

        /*
         * List of parameter specifications.
         */
        ada_params = 235,
    

        /*
         * Holds an AbstractStateDecl between parentheses. Needed to support
         * the syntax:
         *
         * .. code:: ada
         *
         *    package Pkg
         *        with Abstract_State => (A, (B with Some_Aspect))
         */
        ada_paren_abstract_state_decl = 236,
    

        /* pp_directive (abstract)  */
        /*
         * Base node for all preprocessor directives.
         */
    

        /*
         * ``else`` preprocessor directive.
         */
        ada_pp_else_directive = 237,
    

        /*
         * ``elsif ... [then]`` preprocessor directive.
         */
        ada_pp_elsif_directive = 238,
    

        /*
         * ``end if;`` preprocessor directive.
         */
        ada_pp_end_if_directive = 239,
    

        /*
         * ``if ... [then]`` preprocessor directive.
         */
        ada_pp_if_directive = 240,
    

        /*
         * ``then`` keyword in preprocessor directives.
         */
        ada_pp_then_kw = 241,
    

        /*
         * Class for pragmas (:rmlink:`2.8`). Pragmas are compiler directives,
         * that can be language or compiler defined.
         */
        ada_pragma_node = 242,
    

        /* private_node (abstract)  */
        /*
         * Qualifier for the ``private`` keyword.
         */
    

        /*

         */
        ada_private_absent = 243,
    

        /*

         */
        ada_private_present = 244,
    

        /*
         * Type definition for a protected object (:rmlink:`9.4`).
         */
        ada_protected_def = 245,
    

        /* protected_node (abstract)  */
        /*
         * Qualifier for the ``protected`` keyword.
         */
    

        /*

         */
        ada_protected_absent = 246,
    

        /*

         */
        ada_protected_present = 247,
    

        /* quantifier (abstract)  */
        /*
         * Type for quantified expressions.
         */
    

        /*

         */
        ada_quantifier_all = 248,
    

        /*

         */
        ada_quantifier_some = 249,
    

        /*
         * Range specification (:rmlink:`3.5.7`).
         */
        ada_range_spec = 250,
    

        /*
         * Renaming clause, used everywhere renamings are valid.
         */
        ada_renaming_clause = 251,
    

        /*
         * Synthetic renaming clause. Used to synthesize object decls with
         * renamings. (See to_anonymous_object_decl).
         */
        ada_synthetic_renaming_clause = 252,
    

        /* reverse_node (abstract)  */
        /*
         * Qualifier for the ``reverse`` keyword.
         */
    

        /*

         */
        ada_reverse_absent = 253,
    

        /*

         */
        ada_reverse_present = 254,
    

        /*
         * Alternative part in a ``select`` statements block (:rmlink:`9.7`).
         */
        ada_select_when_part = 255,
    

        /* stmt (abstract)  */
        /*
         * Bass class for statements (:rmlink:`5.1`).
         */
    

        /* composite_stmt (abstract)  */
        /*
         * Base class for composite statements (:rmlink:`5.1`).
         */
    

        /*
         * ``accept`` statement (:rmlink:`9.5.2`).
         */
        ada_accept_stmt = 256,
    

        /*
         * Extended ``accept`` statement (:rmlink:`9.5.2`).
         */
        ada_accept_stmt_with_stmts = 257,
    

        /* base_loop_stmt (abstract)  */
        /*
         * Base class for loop statements (:rmlink:`5.5`).
         */
    

        /*
         * Statement for ``for`` loops (``for ... loop ... end loop;``)
         * (:rmlink:`5.5`).
         */
        ada_for_loop_stmt = 258,
    

        /*
         * Statement for simple loops (``loop ... end loop;``) (:rmlink:`5.5`).
         */
        ada_loop_stmt = 259,
    

        /*
         * Statement for ``while`` loops (``while ... loop ... end loop;``)
         * (:rmlink:`5.5`).
         */
        ada_while_loop_stmt = 260,
    

        /* block_stmt (abstract)  */
        /*
         * Base class for statement blocks (:rmlink:`5.6`).
         */
    

        /*
         * Statement block with no declarative part (:rmlink:`5.6`).
         */
        ada_begin_block = 261,
    

        /*
         * Statement block with a declarative part (:rmlink:`5.6`).
         */
        ada_decl_block = 262,
    

        /*
         * ``case`` statement (:rmlink:`5.4`).
         */
        ada_case_stmt = 263,
    

        /*
         * Extended ``return`` statement (:rmlink:`6.5`).
         */
        ada_extended_return_stmt = 264,
    

        /*
         * ``if`` statement block (:rmlink:`5.3`).
         */
        ada_if_stmt = 265,
    

        /*
         * Wrapper class, used for composite statements that can be named
         * (declare blocks, loops). This allows to both have a BasicDecl for
         * the named entity declared, and a CompositeStmt for the statement
         * hierarchy.
         */
        ada_named_stmt = 266,
    

        /*
         * ``select`` statements block (:rmlink:`9.7`).
         */
        ada_select_stmt = 267,
    

        /*
         * Placeholder node for syntax errors in lists of statements.
         */
        ada_error_stmt = 268,
    

        /* simple_stmt (abstract)  */
        /*
         * Base class for simple statements (:rmlink:`5.1`).
         */
    

        /*
         * ``abort`` statement (:rmlink:`9.8`).
         */
        ada_abort_stmt = 269,
    

        /*
         * Statement for assignments (:rmlink:`5.2`).
         */
        ada_assign_stmt = 270,
    

        /*
         * Statement for entry or procedure calls (:rmlink:`6.4`).
         */
        ada_call_stmt = 271,
    

        /*
         * ``delay`` statement (:rmlink:`9.6`).
         */
        ada_delay_stmt = 272,
    

        /*
         * ``exit`` statement (:rmlink:`5.7`).
         */
        ada_exit_stmt = 273,
    

        /*
         * ``goto`` statement (:rmlink:`5.8`).
         */
        ada_goto_stmt = 274,
    

        /*
         * Statement to declare a code label (:rmlink:`5.1`).
         */
        ada_label = 275,
    

        /*
         * ``null;`` statement (:rmlink:`5.1`).
         */
        ada_null_stmt = 276,
    

        /*
         * ``raise`` statement (:rmlink:`11.3`).
         */
        ada_raise_stmt = 277,
    

        /*
         * ``requeue`` statement (:rmlink:`9.5.4`).
         */
        ada_requeue_stmt = 278,
    

        /*
         * ``return`` statement (:rmlink:`6.5`).
         */
        ada_return_stmt = 279,
    

        /*
         * ``terminate`` alternative in a ``select`` statement (:rmlink:`9.7`).
         */
        ada_terminate_alternative = 280,
    

        /* subp_kind (abstract)  */
        /*
         * Qualifier for a subprogram kind.
         */
    

        /*

         */
        ada_subp_kind_function = 281,
    

        /*

         */
        ada_subp_kind_procedure = 282,
    

        /*
         * Subunit (``separate``) (:rmlink:`10.1.3`).
         */
        ada_subunit = 283,
    

        /* synchronized_node (abstract)  */
        /*
         * Qualifier for the ``synchronized`` keyword.
         */
    

        /*

         */
        ada_synchronized_absent = 284,
    

        /*

         */
        ada_synchronized_present = 285,
    

        /* tagged_node (abstract)  */
        /*
         * Qualifier for the ``tagged`` keyword.
         */
    

        /*

         */
        ada_tagged_absent = 286,
    

        /*

         */
        ada_tagged_present = 287,
    

        /*
         * Type definition for a task type (:rmlink:`9.1`).
         */
        ada_task_def = 288,
    

        /*
         * Synthetic node that contains the lazy fields for the attribute
         * subprograms of a given type. The lazy fields are not directly on the
         * BaseTypeDecl node itself to minimize its size in memory: with this
         * indirection, a type for which no function attribute is ever
         * synthesized will not waste any memory.
         */
        ada_type_attributes_repository = 289,
    

        /* type_def (abstract)  */
        /*
         * Base class for type definitions (:rmlink:`3.2.1`).
         */
    

        /* access_def (abstract)  */
        /*
         * Base class for access type definitions (:rmlink:`3.10`).
         */
    

        /*
         * Type definition for accesses to subprograms (:rmlink:`3.10`).
         */
        ada_access_to_subp_def = 290,
    

        /* base_type_access_def (abstract)  */
        /*
         * Base class for access type definitions (:rmlink:`3.10`).
         */
    

        /*
         * Synthetic type access, that will directly reference a type decl. It
         * is used to generate synthetic anonymous access types.
         */
        ada_anonymous_type_access_def = 291,
    

        /*
         * Syntactic type definition for accesses.
         */
        ada_type_access_def = 292,
    

        /*
         * Type definition for an array (:rmlink:`3.6`).
         */
        ada_array_type_def = 293,
    

        /*
         * Type definition for a derived type (:rmlink:`3.4`).
         */
        ada_derived_type_def = 294,
    

        /*
         * Type definition for enumerations (:rmlink:`3.5.1`).
         */
        ada_enum_type_def = 295,
    

        /*
         * Type definition for discrete types in generic formals
         * (:rmlink:`12.5.2`).
         */
        ada_formal_discrete_type_def = 296,
    

        /*
         * Type definition for an interface (:rmlink:`3.9.4`).
         */
        ada_interface_type_def = 297,
    

        /*
         * Type definition for a modular integer type (:rmlink:`3.5.4`).
         */
        ada_mod_int_type_def = 298,
    

        /*
         * Type definition for a private type.
         *
         * Libadalang diverges from the ARM here, treating private types like
         * regular type declarations that have an embedded type definition.
         * This type definition hence corresponds to :rmlink:`7.3`.
         */
        ada_private_type_def = 299,
    

        /* real_type_def (abstract)  */
        /*
         * Type definition for real numbers (:rmlink:`3.5.6`).
         */
    

        /*
         * Type definition for decimal fixed-point numbers (:rmlink:`3.5.9`).
         */
        ada_decimal_fixed_point_def = 300,
    

        /*
         * Type definition for floating-point numbers (:rmlink:`3.5.7`).
         */
        ada_floating_point_def = 301,
    

        /*
         * Type definition for ordinary fixed-point numbers (:rmlink:`3.5.9`).
         */
        ada_ordinary_fixed_point_def = 302,
    

        /*
         * Type definition for a record (:rmlink:`3.8`).
         */
        ada_record_type_def = 303,
    

        /*
         * Type definition for a signed integer type (:rmlink:`3.5.4`).
         */
        ada_signed_int_type_def = 304,
    

        /* type_expr (abstract)  */
        /*
         * A type expression is an abstract node that embodies the concept of a
         * reference to a type.
         *
         * Since Ada has both subtype_indications and anonymous (inline) type
         * declarations, a type expression contains one or the other.
         *
         * This node has no ARM correspondence.
         */
    

        /*
         * Container for inline anonymous array and access types declarations.
         */
        ada_anonymous_type = 305,
    

        /*
         * Synthetic node. Represents the type expression for an enum literal.
         */
        ada_enum_lit_synth_type_expr = 306,
    

        /*
         * Reference to a type by name (:rmlink:`3.2.2`).
         */
        ada_subtype_indication = 307,
    

        /*
         * Reference to a type with a range constraint.
         */
        ada_constrained_subtype_indication = 308,
    

        /*
         * Reference to a type with a general constraint.
         */
        ada_discrete_subtype_indication = 309,
    

        /*
         * Synthetic type expression. The designated type is already known at
         * instantiation time and is to be given in the ``target_type`` field.
         */
        ada_synthetic_type_expr = 310,
    

        /*
         * List of unconstrained array indexes.
         */
        ada_unconstrained_array_index = 311,
    

        /* until_node (abstract)  */
        /*
         * Qualifier for the ``until`` keyword.
         */
    

        /*

         */
        ada_until_absent = 312,
    

        /*

         */
        ada_until_present = 313,
    

        /* use_clause (abstract)  */
        /*
         * Base class for use clauses (:rmlink:`10.1.2`).
         */
    

        /*
         * Use clause for packages (:rmlink:`8.4`).
         */
        ada_use_package_clause = 314,
    

        /*
         * Use clause for types (:rmlink:`8.4`).
         */
        ada_use_type_clause = 315,
    

        /*
         * The value sequence of a reduction expression (see
         * ``ReduceAttributeRef``). Ada 2022, RM 4.5.10.
         */
        ada_value_sequence = 316,
    

        /*
         * Single variant in a discriminated type record declaration.
         *
         * This corresponds to a ``when ... => ...`` section in a variant part.
         */
        ada_variant = 317,
    

        /*
         * Variant part in a discriminated type record declaration
         * (:rmlink:`3.8.1`).
         *
         * This corresponds to the whole ``case ... is ... end case;`` block.
         */
        ada_variant_part = 318,
    

        /*
         * With clause (:rmlink:`10.1.2`).
         */
        ada_with_clause = 319,
    

        /* with_private (abstract)  */
        /*
         * Qualifier for the ``private`` keyword in ``with private`` record
         * clauses.
         */
    

        /*

         */
        ada_with_private_absent = 320,
    

        /*

         */
        ada_with_private_present = 321,
} ada_node_kind_enum;

/*
 * Reference to a symbol. Symbols are owned by analysis contexts, so they must
 * not outlive them. This type exists only in the C API, and roughly wraps the
 * corresponding Ada type (an array fat pointer).
 */
typedef struct {
   void *data;
   void *bounds;
} ada_symbol_type;

/*
 * Type to contain Unicode text data.
 */
typedef struct {
   int length;
   int ref_count;
   uint32_t content[1];
} *ada_string_type;

/*
 * Data type for env rebindings. For internal use only.
 */
typedef void *ada_env_rebindings_type;

typedef uint8_t ada_bool;

/* Helper data structures for source location handling.  */

/*
 * Location in a source file. Line and column numbers are one-based.
 */
typedef struct {
    uint32_t line;
    uint16_t column;
} ada_source_location;

/*
 * Location of a span of text in a source file.
 */
typedef struct {
    ada_source_location start;
    ada_source_location end;
} ada_source_location_range;


/*
 * String encoded in UTF-32 (native endianness).
 */
typedef struct {
   /*
 * Address for the content of the string.
 */
    uint32_t *chars;
   /*
 * Size of the string (in characters).
 */
    size_t length;

    int is_allocated;
} ada_text;

/*
 * Arbitrarily large integer.
 */
typedef void *ada_big_integer;

/*
 * Kind for this token.
 */
typedef enum {
   
      
      ADA_TERMINATION = 0
      ,
      ADA_LEXING_FAILURE = 1
      ,
      ADA_IDENTIFIER = 2
      ,
      ADA_ALL = 3
      ,
      ADA_ABORT = 4
      ,
      ADA_ELSE = 5
      ,
      ADA_NEW = 6
      ,
      ADA_RETURN = 7
      ,
      ADA_ABS = 8
      ,
      ADA_ELSIF = 9
      ,
      ADA_NOT = 10
      ,
      ADA_REVERSE = 11
      ,
      ADA_END = 12
      ,
      ADA_NULL = 13
      ,
      ADA_ACCEPT = 14
      ,
      ADA_ENTRY = 15
      ,
      ADA_SELECT = 16
      ,
      ADA_ACCESS = 17
      ,
      ADA_EXCEPTION = 18
      ,
      ADA_OF = 19
      ,
      ADA_SEPARATE = 20
      ,
      ADA_EXIT = 21
      ,
      ADA_OR = 22
      ,
      ADA_OTHERS = 23
      ,
      ADA_SUBTYPE = 24
      ,
      ADA_AND = 25
      ,
      ADA_FOR = 26
      ,
      ADA_OUT = 27
      ,
      ADA_ARRAY = 28
      ,
      ADA_FUNCTION = 29
      ,
      ADA_AT = 30
      ,
      ADA_GENERIC = 31
      ,
      ADA_PACKAGE = 32
      ,
      ADA_TASK = 33
      ,
      ADA_BEGIN = 34
      ,
      ADA_GOTO = 35
      ,
      ADA_PRAGMA = 36
      ,
      ADA_TERMINATE = 37
      ,
      ADA_BODY = 38
      ,
      ADA_PRIVATE = 39
      ,
      ADA_THEN = 40
      ,
      ADA_IF = 41
      ,
      ADA_PROCEDURE = 42
      ,
      ADA_TYPE = 43
      ,
      ADA_CASE = 44
      ,
      ADA_IN = 45
      ,
      ADA_CONSTANT = 46
      ,
      ADA_IS = 47
      ,
      ADA_RAISE = 48
      ,
      ADA_USE = 49
      ,
      ADA_DECLARE = 50
      ,
      ADA_RANGE = 51
      ,
      ADA_DELAY = 52
      ,
      ADA_LIMITED = 53
      ,
      ADA_RECORD = 54
      ,
      ADA_WHEN = 55
      ,
      ADA_DELTA = 56
      ,
      ADA_LOOP = 57
      ,
      ADA_REM = 58
      ,
      ADA_WHILE = 59
      ,
      ADA_DIGITS = 60
      ,
      ADA_RENAMES = 61
      ,
      ADA_DO = 62
      ,
      ADA_MOD = 63
      ,
      ADA_XOR = 64
      ,
      ADA_PAR_CLOSE = 65
      ,
      ADA_PAR_OPEN = 66
      ,
      ADA_BRACK_CLOSE = 67
      ,
      ADA_BRACK_OPEN = 68
      ,
      ADA_SEMICOLON = 69
      ,
      ADA_COLON = 70
      ,
      ADA_COMMA = 71
      ,
      ADA_DOUBLEDOT = 72
      ,
      ADA_DOT = 73
      ,
      ADA_DIAMOND = 74
      ,
      ADA_LTE = 75
      ,
      ADA_GTE = 76
      ,
      ADA_ARROW = 77
      ,
      ADA_EQUAL = 78
      ,
      ADA_LT = 79
      ,
      ADA_GT = 80
      ,
      ADA_PLUS = 81
      ,
      ADA_MINUS = 82
      ,
      ADA_POWER = 83
      ,
      ADA_MULT = 84
      ,
      ADA_AMP = 85
      ,
      ADA_NOTEQUAL = 86
      ,
      ADA_DIVIDE = 87
      ,
      ADA_TICK = 88
      ,
      ADA_PIPE = 89
      ,
      ADA_ASSIGN = 90
      ,
      ADA_LABEL_START = 91
      ,
      ADA_LABEL_END = 92
      ,
      ADA_TARGET = 93
      ,
      ADA_STRING = 94
      ,
      ADA_CHAR = 95
      ,
      ADA_WITH = 96
      ,
      ADA_DECIMAL = 97
      ,
      ADA_INTEGER = 98
      ,
      ADA_COMMENT = 99
      ,
      ADA_PREP_LINE = 100
      ,
      ADA_WHITESPACE = 101
} ada_token_kind;

typedef struct
{
   uint64_t version;
} *ada_token_data_handler;

/*
 * Reference to a token in an analysis unit.
 */
typedef struct {
    /* Private data associated to this token, including stale reference
       checking data, or NULL if this designates no token.  */
    ada_analysis_context context;
    ada_token_data_handler token_data;

    /* Internal identifiers for this token.  */
    int token_index, trivia_index;

    ada_token_kind kind;
    ada_text text;
    ada_source_location_range sloc_range;
} ada_token;


/*
 * Diagnostic for an analysis unit: cannot open the source file, parsing error,
 * ...
 */
typedef struct {
    ada_source_location_range sloc_range;
    ada_text message;
} ada_diagnostic;

   typedef enum {
      ADA_ANALYSIS_UNIT_KIND_UNIT_SPECIFICATION, ADA_ANALYSIS_UNIT_KIND_UNIT_BODY
   } ada_analysis_unit_kind;
   /*
    * Specify a kind of analysis unit. Specification units provide an interface
    * to the outer world while body units provide an implementation for the
    * corresponding interface.
    */
   typedef enum {
      ADA_LOOKUP_KIND_RECURSIVE, ADA_LOOKUP_KIND_FLAT, ADA_LOOKUP_KIND_MINIMAL
   } ada_lookup_kind;
   /*

    */
   typedef enum {
      ADA_DESIGNATED_ENV_KIND_NONE, ADA_DESIGNATED_ENV_KIND_CURRENT_ENV, ADA_DESIGNATED_ENV_KIND_NAMED_ENV, ADA_DESIGNATED_ENV_KIND_DIRECT_ENV
   } ada_designated_env_kind;
   /*
    * Discriminant for DesignatedEnv structures.
    */
   typedef enum {
      ADA_REF_RESULT_KIND_NO_REF, ADA_REF_RESULT_KIND_PRECISE, ADA_REF_RESULT_KIND_IMPRECISE, ADA_REF_RESULT_KIND_ERROR
   } ada_ref_result_kind;
   /*
    * Kind for the result of a cross reference operation.
    *
    * * ``no_ref`` is for no reference, it is the null value for this enum.
    *
    * * ``precise`` is when the reference result is precise.
    *
    * * ``imprecise`` is when there was an error computing the precise result,
    *   and a result was gotten in an imprecise fashion.
    *
    * * ``error`` is for unrecoverable errors (either there is no imprecise
    *   path for the request you made, or the imprecise path errored out too.
    */
   typedef enum {
      ADA_CALL_EXPR_KIND_CALL, ADA_CALL_EXPR_KIND_ARRAY_SLICE, ADA_CALL_EXPR_KIND_ARRAY_INDEX, ADA_CALL_EXPR_KIND_TYPE_CONVERSION
   } ada_call_expr_kind;
   /*
    * Kind of CallExpr type.
    *
    * * ``call`` is when the CallExpr is a procedure or function call.
    *
    * * ``array_slice``, ``array_index`` is when the CallExpr is in fact an
    *   array slice or an array subcomponent access expression, respectively.
    *
    * * ``type_conversion`` is when the CallExpr is a type conversion.
    */
   typedef enum {
      ADA_GRAMMAR_RULE_PARENT_LIST_RULE, ADA_GRAMMAR_RULE_PROTECTED_TYPE_DECL_RULE, ADA_GRAMMAR_RULE_PROTECTED_OP_RULE, ADA_GRAMMAR_RULE_PROTECTED_EL_RULE, ADA_GRAMMAR_RULE_PROTECTED_DEF_RULE, ADA_GRAMMAR_RULE_PROTECTED_DECL_RULE, ADA_GRAMMAR_RULE_TASK_ITEM_RULE, ADA_GRAMMAR_RULE_TASK_DEF_RULE, ADA_GRAMMAR_RULE_TASK_TYPE_DECL_RULE, ADA_GRAMMAR_RULE_SUBTYPE_DECL_RULE, ADA_GRAMMAR_RULE_INTERFACE_TYPE_DEF_RULE, ADA_GRAMMAR_RULE_UNCONSTRAINED_INDEX_RULE, ADA_GRAMMAR_RULE_ARRAY_TYPE_DEF_RULE, ADA_GRAMMAR_RULE_DISCRETE_SUBTYPE_DEFINITION_RULE, ADA_GRAMMAR_RULE_CONSTRAINT_LIST_RULE, ADA_GRAMMAR_RULE_SIGNED_INT_TYPE_DEF_RULE, ADA_GRAMMAR_RULE_MOD_INT_TYPE_DEF_RULE, ADA_GRAMMAR_RULE_DERIVED_TYPE_DEF_RULE, ADA_GRAMMAR_RULE_COMPOSITE_CONSTRAINT_ASSOC_RULE, ADA_GRAMMAR_RULE_COMPOSITE_CONSTRAINT_RULE, ADA_GRAMMAR_RULE_DIGITS_CONSTRAINT_RULE, ADA_GRAMMAR_RULE_DELTA_CONSTRAINT_RULE, ADA_GRAMMAR_RULE_RANGE_CONSTRAINT_RULE, ADA_GRAMMAR_RULE_CONSTRAINT_RULE, ADA_GRAMMAR_RULE_DISCRIMINANT_SPEC_RULE, ADA_GRAMMAR_RULE_DISCR_SPEC_LIST_RULE, ADA_GRAMMAR_RULE_DISCRIMINANT_PART_RULE, ADA_GRAMMAR_RULE_ENUM_LITERAL_DECL_RULE, ADA_GRAMMAR_RULE_FORMAL_DISCRETE_TYPE_DEF_RULE, ADA_GRAMMAR_RULE_RECORD_DEF_RULE, ADA_GRAMMAR_RULE_RANGE_SPEC_RULE, ADA_GRAMMAR_RULE_REAL_TYPE_DEF_RULE, ADA_GRAMMAR_RULE_SEXPR_OR_BOX_RULE, ADA_GRAMMAR_RULE_ORDINARY_FIXED_POINT_DEF_RULE, ADA_GRAMMAR_RULE_DECIMAL_FIXED_POINT_DEF_RULE, ADA_GRAMMAR_RULE_FLOATING_POINT_DEF_RULE, ADA_GRAMMAR_RULE_RECORD_TYPE_DEF_RULE, ADA_GRAMMAR_RULE_ACCESS_DEF_RULE, ADA_GRAMMAR_RULE_ENUM_TYPE_DEF_RULE, ADA_GRAMMAR_RULE_TYPE_DEF_RULE, ADA_GRAMMAR_RULE_VARIANT_RULE, ADA_GRAMMAR_RULE_ANONYMOUS_TYPE_DECL_RULE, ADA_GRAMMAR_RULE_INCOMPLETE_TYPE_DECL_RULE, ADA_GRAMMAR_RULE_TYPE_DECL_RULE, ADA_GRAMMAR_RULE_VARIANT_PART_RULE, ADA_GRAMMAR_RULE_COMPONENT_DEF_RULE, ADA_GRAMMAR_RULE_COMPONENT_ITEM_RULE, ADA_GRAMMAR_RULE_COMPONENT_DECL_RULE, ADA_GRAMMAR_RULE_COMPONENT_LIST_RULE, ADA_GRAMMAR_RULE_GENERIC_DECL_RULE, ADA_GRAMMAR_RULE_GENERIC_FORMAL_PART_RULE, ADA_GRAMMAR_RULE_GENERIC_FORMAL_DECL_RULE, ADA_GRAMMAR_RULE_FORMAL_TYPE_DECL_RULE, ADA_GRAMMAR_RULE_FORMAL_SUBP_DECL_RULE, ADA_GRAMMAR_RULE_RENAMING_CLAUSE_RULE, ADA_GRAMMAR_RULE_GENERIC_RENAMING_DECL_RULE, ADA_GRAMMAR_RULE_GENERIC_INSTANTIATION_RULE, ADA_GRAMMAR_RULE_EXCEPTION_DECL_RULE, ADA_GRAMMAR_RULE_BASIC_DECLS_RULE, ADA_GRAMMAR_RULE_PACKAGE_RENAMING_DECL_RULE, ADA_GRAMMAR_RULE_PACKAGE_DECL_RULE, ADA_GRAMMAR_RULE_BASIC_DECL_RULE, ADA_GRAMMAR_RULE_OBJECT_DECL_RULE, ADA_GRAMMAR_RULE_SUB_OBJECT_DECL_RULE, ADA_GRAMMAR_RULE_NO_TYPE_OBJECT_RENAMING_DECL_RULE, ADA_GRAMMAR_RULE_EXT_RET_STMT_OBJECT_DECL_RULE, ADA_GRAMMAR_RULE_DEFINING_ID_LIST_RULE, ADA_GRAMMAR_RULE_NUMBER_DECL_RULE, ADA_GRAMMAR_RULE_CONTRACT_CASE_ASSOC_RULE, ADA_GRAMMAR_RULE_CONTRACT_CASES_EXPR_RULE, ADA_GRAMMAR_RULE_ABSTRACT_STATE_DECL_RULE, ADA_GRAMMAR_RULE_MULTI_ABSTRACT_STATE_DECL_RULE, ADA_GRAMMAR_RULE_ASPECT_ASSOC_RULE, ADA_GRAMMAR_RULE_ASPECT_SPEC_RULE, ADA_GRAMMAR_RULE_SINGLE_TASK_DECL_RULE, ADA_GRAMMAR_RULE_OVERRIDING_INDICATOR_RULE, ADA_GRAMMAR_RULE_ENTRY_DECL_RULE, ADA_GRAMMAR_RULE_COMPONENT_CLAUSE_RULE, ADA_GRAMMAR_RULE_ASPECT_CLAUSE_RULE, ADA_GRAMMAR_RULE_PARAM_SPEC_RULE, ADA_GRAMMAR_RULE_PARAM_SPECS_RULE, ADA_GRAMMAR_RULE_SUBP_SPEC_RULE, ADA_GRAMMAR_RULE_EXPR_FN_RULE, ADA_GRAMMAR_RULE_NULL_SUBP_DECL_RULE, ADA_GRAMMAR_RULE_ABSTRACT_SUBP_DECL_RULE, ADA_GRAMMAR_RULE_SUBP_RENAMING_DECL_RULE, ADA_GRAMMAR_RULE_SIMPLE_SUBP_DECL_RULE, ADA_GRAMMAR_RULE_SUBP_DECL_RULE, ADA_GRAMMAR_RULE_WITH_CLAUSE_RULE, ADA_GRAMMAR_RULE_CONTEXT_ITEM_RULE, ADA_GRAMMAR_RULE_USE_CLAUSE_RULE, ADA_GRAMMAR_RULE_USE_PACKAGE_CLAUSE_RULE, ADA_GRAMMAR_RULE_USE_TYPE_CLAUSE_RULE, ADA_GRAMMAR_RULE_SUBTYPE_INDICATION_RULE, ADA_GRAMMAR_RULE_DISCRETE_SUBTYPE_INDICATION_RULE, ADA_GRAMMAR_RULE_CONSTRAINED_SUBTYPE_INDICATION_RULE, ADA_GRAMMAR_RULE_TYPE_EXPR_RULE, ADA_GRAMMAR_RULE_ANONYMOUS_TYPE_RULE, ADA_GRAMMAR_RULE_MODE_RULE, ADA_GRAMMAR_RULE_PRAGMA_ARGUMENT_RULE, ADA_GRAMMAR_RULE_PRAGMA_RULE, ADA_GRAMMAR_RULE_SUBUNIT_RULE, ADA_GRAMMAR_RULE_LIBRARY_UNIT_BODY_RULE, ADA_GRAMMAR_RULE_LIBRARY_UNIT_RENAMING_DECL_RULE, ADA_GRAMMAR_RULE_LIBRARY_ITEM_RULE, ADA_GRAMMAR_RULE_COMPILATION_UNIT_RULE, ADA_GRAMMAR_RULE_COMPILATION_RULE, ADA_GRAMMAR_RULE_DECL_PART_RULE, ADA_GRAMMAR_RULE_ENTRY_BODY_RULE, ADA_GRAMMAR_RULE_PROTECTED_BODY_RULE, ADA_GRAMMAR_RULE_PROTECTED_BODY_STUB_RULE, ADA_GRAMMAR_RULE_TASK_BODY_RULE, ADA_GRAMMAR_RULE_TASK_BODY_STUB_RULE, ADA_GRAMMAR_RULE_PACKAGE_BODY_STUB_RULE, ADA_GRAMMAR_RULE_PACKAGE_BODY_RULE, ADA_GRAMMAR_RULE_TERMINATE_ALTERNATIVE_RULE, ADA_GRAMMAR_RULE_SELECT_STMT_RULE, ADA_GRAMMAR_RULE_ACCEPT_STMT_RULE, ADA_GRAMMAR_RULE_CASE_ALT_RULE, ADA_GRAMMAR_RULE_CASE_STMT_RULE, ADA_GRAMMAR_RULE_EXT_RETURN_STMT_RULE, ADA_GRAMMAR_RULE_IBLOCK_STMT_RULE, ADA_GRAMMAR_RULE_BLOCK_STMT_RULE, ADA_GRAMMAR_RULE_WHILE_LOOP_SPEC_RULE, ADA_GRAMMAR_RULE_ILOOP_STMT_RULE, ADA_GRAMMAR_RULE_LOOP_STMT_RULE, ADA_GRAMMAR_RULE_COMPOUND_STMT_RULE, ADA_GRAMMAR_RULE_ELSIF_PART_RULE, ADA_GRAMMAR_RULE_IF_STMT_RULE, ADA_GRAMMAR_RULE_RAISE_STMT_RULE, ADA_GRAMMAR_RULE_DELAY_STMT_RULE, ADA_GRAMMAR_RULE_ABORT_STMT_RULE, ADA_GRAMMAR_RULE_BODY_RULE, ADA_GRAMMAR_RULE_BODY_STUB_RULE, ADA_GRAMMAR_RULE_SUBP_BODY_STUB_RULE, ADA_GRAMMAR_RULE_RECOV_DECL_PART_RULE, ADA_GRAMMAR_RULE_SUBP_BODY_RULE, ADA_GRAMMAR_RULE_HANDLED_STMTS_RULE, ADA_GRAMMAR_RULE_EXCEPTION_HANDLER_RULE, ADA_GRAMMAR_RULE_STMTS_RULE, ADA_GRAMMAR_RULE_LABEL_RULE, ADA_GRAMMAR_RULE_STMT_RULE, ADA_GRAMMAR_RULE_CALL_STMT_RULE, ADA_GRAMMAR_RULE_SIMPLE_STMT_RULE, ADA_GRAMMAR_RULE_NULL_STMT_RULE, ADA_GRAMMAR_RULE_ASSIGNMENT_STMT_RULE, ADA_GRAMMAR_RULE_GOTO_STMT_RULE, ADA_GRAMMAR_RULE_EXIT_STMT_RULE, ADA_GRAMMAR_RULE_RETURN_STMT_RULE, ADA_GRAMMAR_RULE_REQUEUE_STMT_RULE, ADA_GRAMMAR_RULE_IDENTIFIER_RULE, ADA_GRAMMAR_RULE_CHAR_LITERAL_RULE, ADA_GRAMMAR_RULE_STRING_LITERAL_RULE, ADA_GRAMMAR_RULE_DEFINING_ID_RULE, ADA_GRAMMAR_RULE_DEC_LITERAL_RULE, ADA_GRAMMAR_RULE_INT_LITERAL_RULE, ADA_GRAMMAR_RULE_NUM_LITERAL_RULE, ADA_GRAMMAR_RULE_NULL_LITERAL_RULE, ADA_GRAMMAR_RULE_ALLOCATOR_RULE, ADA_GRAMMAR_RULE_FOR_LOOP_PARAM_SPEC_RULE, ADA_GRAMMAR_RULE_QUANTIFIED_EXPR_RULE, ADA_GRAMMAR_RULE_CASE_EXPR_RULE, ADA_GRAMMAR_RULE_CASE_EXPR_ALT_RULE, ADA_GRAMMAR_RULE_RAISE_EXPR_RULE, ADA_GRAMMAR_RULE_IF_EXPR_RULE, ADA_GRAMMAR_RULE_CONDITIONAL_EXPR_RULE, ADA_GRAMMAR_RULE_BOX_EXPR_RULE, ADA_GRAMMAR_RULE_OTHERS_DESIGNATOR_RULE, ADA_GRAMMAR_RULE_ITERATED_ASSOC_RULE, ADA_GRAMMAR_RULE_AGGREGATE_ASSOC_RULE, ADA_GRAMMAR_RULE_REGULAR_AGGREGATE_RULE, ADA_GRAMMAR_RULE_BRACKET_AGGREGATE_RULE, ADA_GRAMMAR_RULE_AGGREGATE_RULE, ADA_GRAMMAR_RULE_DIRECT_NAME_RULE, ADA_GRAMMAR_RULE_PARAM_ASSOC_RULE, ADA_GRAMMAR_RULE_CALL_SUFFIX_RULE, ADA_GRAMMAR_RULE_ATTR_SUFFIX_RULE, ADA_GRAMMAR_RULE_QUALIFIED_NAME_RULE, ADA_GRAMMAR_RULE_QUAL_NAME_INTERNAL_RULE, ADA_GRAMMAR_RULE_VALUE_SEQUENCE_RULE, ADA_GRAMMAR_RULE_NAME_RULE, ADA_GRAMMAR_RULE_DEFINING_NAME_RULE, ADA_GRAMMAR_RULE_DIRECT_NAME_OR_TARGET_NAME_RULE, ADA_GRAMMAR_RULE_TARGET_NAME_RULE, ADA_GRAMMAR_RULE_UPDATE_ATTR_AGGREGATE_RULE, ADA_GRAMMAR_RULE_UPDATE_ATTR_CONTENT_RULE, ADA_GRAMMAR_RULE_MULTIDIM_ARRAY_ASSOC_RULE, ADA_GRAMMAR_RULE_SUBTYPE_NAME_RULE, ADA_GRAMMAR_RULE_STATIC_NAME_RULE, ADA_GRAMMAR_RULE_PRIMARY_RULE, ADA_GRAMMAR_RULE_PAREN_EXPR_RULE, ADA_GRAMMAR_RULE_DECLARE_EXPR_RULE, ADA_GRAMMAR_RULE_FACTOR_RULE, ADA_GRAMMAR_RULE_TERM_RULE, ADA_GRAMMAR_RULE_UNOP_TERM_RULE, ADA_GRAMMAR_RULE_SIMPLE_EXPR_RULE, ADA_GRAMMAR_RULE_BOOLEAN_OP_RULE, ADA_GRAMMAR_RULE_DISCRETE_RANGE_RULE, ADA_GRAMMAR_RULE_CHOICE_RULE, ADA_GRAMMAR_RULE_CHOICE_LIST_RULE, ADA_GRAMMAR_RULE_REL_OP_RULE, ADA_GRAMMAR_RULE_MEMBERSHIP_CHOICE_RULE, ADA_GRAMMAR_RULE_MEMBERSHIP_CHOICE_LIST_RULE, ADA_GRAMMAR_RULE_RELATION_RULE, ADA_GRAMMAR_RULE_EXPR_RULE, ADA_GRAMMAR_RULE_PP_DIRECTIVE_RULE, ADA_GRAMMAR_RULE_PP_THEN_RULE, ADA_GRAMMAR_RULE_PP_EXPR_RULE, ADA_GRAMMAR_RULE_PP_TERM_RULE
   } ada_grammar_rule;
   /*
    * Gramar rule to use for parsing.
    */

const ada_grammar_rule ada_default_grammar_rule = ADA_GRAMMAR_RULE_COMPILATION_RULE;

/*
 * Enumerated type describing all possible exceptions that need to be handled
 * in the C bindings.
 */
typedef enum {
      EXCEPTION_FILE_READ_ERROR,
      EXCEPTION_BAD_TYPE_ERROR,
      EXCEPTION_OUT_OF_BOUNDS_ERROR,
      EXCEPTION_INVALID_INPUT,
      EXCEPTION_INVALID_SYMBOL_ERROR,
      EXCEPTION_INVALID_UNIT_NAME_ERROR,
      EXCEPTION_NATIVE_EXCEPTION,
      EXCEPTION_PRECONDITION_FAILURE,
      EXCEPTION_PROPERTY_ERROR,
      EXCEPTION_TEMPLATE_ARGS_ERROR,
      EXCEPTION_TEMPLATE_FORMAT_ERROR,
      EXCEPTION_TEMPLATE_INSTANTIATION_ERROR,
      EXCEPTION_STALE_REFERENCE_ERROR,
      EXCEPTION_SYNTAX_ERROR,
      EXCEPTION_UNKNOWN_CHARSET,
      EXCEPTION_INVALID_PROJECT,
      EXCEPTION_UNSUPPORTED_VIEW_ERROR,
} ada_exception_kind;

/*
 * Holder for native exceptions-related information.  Memory management for
 * this and all the fields is handled by the library: one just has to make sure
 * not to keep references to it.
 *
 * .. TODO: For the moment, this structure contains already formatted
 *    information, but depending on possible future Ada runtime improvements,
 *    this might change.
 */
typedef struct {
   /*
 * The kind of this exception.
 */
   ada_exception_kind kind;

   /*
 * Message and context information associated with this exception.
 */
   const char *information;
} ada_exception;

/*
 * Array types incomplete declarations
 */

        

typedef struct ada_discriminant_values_array_record *ada_discriminant_values_array;

        

typedef struct ada_doc_annotation_array_record *ada_doc_annotation_array;

        

typedef struct ada_ada_node_array_record *ada_ada_node_array;

        

typedef struct ada_param_actual_array_record *ada_param_actual_array;

        

typedef struct ada_ref_result_array_record *ada_ref_result_array;

        

typedef struct ada_shape_array_record *ada_shape_array;

        

typedef struct ada_substitution_array_record *ada_substitution_array;

        

typedef struct ada_analysis_unit_array_record *ada_analysis_unit_array;

        

typedef struct ada_unbounded_text_type_array_record *ada_unbounded_text_type_array;


/*
 * Iterator types incomplete declarations
 */

/*
 * An iterator provides a mean to retrieve values one-at-a-time.
 *
 * Currently, each iterator is bound to the analysis context used to create it.
 * Iterators are invalidated as soon as any unit of that analysis is reparsed.
 * Due to the nature of iterators (lazy computations), this invalidation is
 * necessary to avoid use of inconsistent state, such as an iterator trying to
 * use analysis context data that is stale.
 */



typedef void* ada_ada_node_iterator;


        

typedef void* ada_completion_item_iterator;


/*
 * Struct types declarations
 */

        



typedef struct {
        ada_bool dottable_subp;
        ada_base_node primitive;
        ada_base_node primitive_real_type;
} ada_internal_metadata;



        



typedef struct {
        ada_internal_metadata md;
        ada_env_rebindings_type rebindings;
        ada_bool from_rebound;
} ada_internal_entity_info;



        



typedef struct {
        ada_base_node node;
        ada_internal_entity_info info;
} ada_base_entity;



        



typedef struct {
        ada_bool exists;
        ada_base_entity node;
        ada_base_entity value;
} ada_internal_aspect;



        



typedef struct {
        ada_base_entity decl;
        ada_bool is_dot_call;
        ada_bool is_visible;
        int weight;
} ada_internal_completion_item;



        



typedef struct {
        ada_base_entity low_bound;
        ada_base_entity high_bound;
} ada_internal_discrete_range;



        



typedef struct {
        ada_base_entity discriminant;
        ada_base_entity values;
} ada_internal_discriminant_values;



        



typedef struct {
        ada_string_type key;
        ada_string_type value;
} ada_internal_doc_annotation;

    /* Increment the ref-count of all components in "r".  */
    extern void
    ada_internal_doc_annotation_inc_ref(ada_internal_doc_annotation *r);

    /* Decrement the ref-count of all components in "r".  */
    extern void
    ada_internal_doc_annotation_dec_ref(ada_internal_doc_annotation *r);


        



typedef struct {
        ada_base_entity param;
        ada_base_entity actual;
} ada_internal_param_actual;



        



typedef struct {
        ada_base_entity ref;
        ada_ref_result_kind kind;
} ada_internal_ref_result;



        



typedef struct {
        ada_base_entity decl;
        ada_ref_result_kind kind;
} ada_internal_refd_decl;



        



typedef struct {
        ada_base_entity def_name;
        ada_ref_result_kind kind;
} ada_internal_refd_def;



        



typedef struct {
        ada_ada_node_array components;
        ada_discriminant_values_array discriminants_values;
} ada_internal_shape;

    /* Increment the ref-count of all components in "r".  */
    extern void
    ada_internal_shape_inc_ref(ada_internal_shape *r);

    /* Decrement the ref-count of all components in "r".  */
    extern void
    ada_internal_shape_dec_ref(ada_internal_shape *r);


        



typedef struct {
        ada_base_entity from_decl;
        ada_big_integer to_value;
        ada_base_entity value_type;
} ada_internal_substitution;

    /* Increment the ref-count of all components in "r".  */
    extern void
    ada_internal_substitution_inc_ref(ada_internal_substitution *r);

    /* Decrement the ref-count of all components in "r".  */
    extern void
    ada_internal_substitution_dec_ref(ada_internal_substitution *r);



/*
 * Types for event handler
 */

/*
 * Interface to handle events sent by the analysis context.
 */
typedef void *ada_event_handler;

/*
 * Callback type for functions that are called when a unit is requested.
 *
 * ``name`` is the name of the requested unit.
 *
 * ``from`` is the unit from which the unit was requested.
 *
 * ``found`` indicates whether the requested unit was found or not.
 *
 * ``is_not_found_error`` indicates whether the fact that the unit was not
 * found is an error or not.
 *
 * .. warning:: The interface of this callback is probably subject to change,
 *    so should be treated as experimental.
 */
typedef void (*ada_event_handler_unit_requested_callback)(
   void *data,
   ada_analysis_context context,
   ada_text *name,
   ada_analysis_unit from,
   ada_bool found,
   ada_bool is_not_found_error
);

/*
 * Callback type for functions that are called when destroying an event
 * handler.
 */
typedef void (*ada_event_handler_destroy_callback)(void *data);

/*
 * Callback type for functions that are called when a unit is parsed.
 *
 * ``unit`` is the resulting unit.
 *
 * ``reparsed`` indicates whether the unit was reparsed, or whether it was the
 * first parse.
 */
typedef void (*ada_event_handler_unit_parsed_callback)(
   void *data,
   ada_analysis_context context,
   ada_analysis_unit unit,
   ada_bool reparsed
);

/*
 * Types for file readers
 */

/*
 * Interface to override how source files are fetched and decoded.
 */
typedef void *ada_file_reader;

/*
 * Callback type for functions that are called when destroying a file reader.
 */
typedef void (*ada_file_reader_destroy_callback)(void *data);

/*
 * Callback type for functions that are called to fetch the decoded source
 * buffer for a requested filename.
 */
typedef void (*ada_file_reader_read_callback)(
   void *data,
   const char *filename,
   const char *charset,
   int read_bom,
   ada_text *buffer,
   ada_diagnostic *diagnostic
);

/*
 * Types for unit providers
 */

/*
 * Interface to fetch analysis units from a name and a unit kind.
 *
 * The unit provider mechanism provides an abstraction which assumes that to
 * any couple (unit name, unit kind) we can associate at most one source file.
 * This means that several couples can be associated to the same source file,
 * but on the other hand, only one one source file can be associated to a
 * couple.
 *
 * This is used to make the semantic analysis able to switch from one analysis
 * units to another.
 *
 * See the documentation of each unit provider for the exact semantics of the
 * unit name/kind information.
 */
typedef void *ada_unit_provider;

/*
 * Callback type for functions that are called when destroying a unit file
 * provider type.
 */
typedef void (*ada_unit_provider_destroy_callback)(void *data);

/*
 * Callback type for functions that are called to turn a unit reference encoded
 * as a unit name into an analysis unit.
 */
typedef char *(*ada_unit_provider_get_unit_filename_callback)(
   void *data,
   ada_text *name,
   ada_analysis_unit_kind kind
);

/*
 * Callback type for functions that are called to turn a unit reference encoded
 * as a unit name into an analysis unit.
 */
typedef ada_analysis_unit (*ada_unit_provider_get_unit_from_name_callback)(
   void *data,
   ada_analysis_context context,
   ada_text *name,
   ada_analysis_unit_kind kind,
   const char *charset,
   int reparse
);

/* All the functions below can potentially raise an exception, so
   ada_get_last_exception must be checked after them even
   before trying to use the returned value.  */


/*
 * Array types declarations
 */

        



/*

 */
struct ada_discriminant_values_array_record {
   int n;
   int ref_count;
   ada_internal_discriminant_values items[1];
};

/* Create a length-sized array.  */
extern ada_discriminant_values_array
ada_discriminant_values_array_create(int length);

/* Increment the ref-count for "a".  */
extern void
ada_discriminant_values_array_inc_ref(ada_discriminant_values_array a);

/* Decrement the ref-count for "a". This deallocates it if the ref-count drops
   to 0.  */
extern void
ada_discriminant_values_array_dec_ref(ada_discriminant_values_array a);


        



/*

 */
struct ada_doc_annotation_array_record {
   int n;
   int ref_count;
   ada_internal_doc_annotation items[1];
};

/* Create a length-sized array.  */
extern ada_doc_annotation_array
ada_doc_annotation_array_create(int length);

/* Increment the ref-count for "a".  */
extern void
ada_doc_annotation_array_inc_ref(ada_doc_annotation_array a);

/* Decrement the ref-count for "a". This deallocates it if the ref-count drops
   to 0.  */
extern void
ada_doc_annotation_array_dec_ref(ada_doc_annotation_array a);


        



/*

 */
struct ada_ada_node_array_record {
   int n;
   int ref_count;
   ada_base_entity items[1];
};

/* Create a length-sized array.  */
extern ada_ada_node_array
ada_ada_node_array_create(int length);

/* Increment the ref-count for "a".  */
extern void
ada_ada_node_array_inc_ref(ada_ada_node_array a);

/* Decrement the ref-count for "a". This deallocates it if the ref-count drops
   to 0.  */
extern void
ada_ada_node_array_dec_ref(ada_ada_node_array a);


        



/*

 */
struct ada_param_actual_array_record {
   int n;
   int ref_count;
   ada_internal_param_actual items[1];
};

/* Create a length-sized array.  */
extern ada_param_actual_array
ada_param_actual_array_create(int length);

/* Increment the ref-count for "a".  */
extern void
ada_param_actual_array_inc_ref(ada_param_actual_array a);

/* Decrement the ref-count for "a". This deallocates it if the ref-count drops
   to 0.  */
extern void
ada_param_actual_array_dec_ref(ada_param_actual_array a);


        



/*

 */
struct ada_ref_result_array_record {
   int n;
   int ref_count;
   ada_internal_ref_result items[1];
};

/* Create a length-sized array.  */
extern ada_ref_result_array
ada_ref_result_array_create(int length);

/* Increment the ref-count for "a".  */
extern void
ada_ref_result_array_inc_ref(ada_ref_result_array a);

/* Decrement the ref-count for "a". This deallocates it if the ref-count drops
   to 0.  */
extern void
ada_ref_result_array_dec_ref(ada_ref_result_array a);


        



/*

 */
struct ada_shape_array_record {
   int n;
   int ref_count;
   ada_internal_shape items[1];
};

/* Create a length-sized array.  */
extern ada_shape_array
ada_shape_array_create(int length);

/* Increment the ref-count for "a".  */
extern void
ada_shape_array_inc_ref(ada_shape_array a);

/* Decrement the ref-count for "a". This deallocates it if the ref-count drops
   to 0.  */
extern void
ada_shape_array_dec_ref(ada_shape_array a);


        



/*

 */
struct ada_substitution_array_record {
   int n;
   int ref_count;
   ada_internal_substitution items[1];
};

/* Create a length-sized array.  */
extern ada_substitution_array
ada_substitution_array_create(int length);

/* Increment the ref-count for "a".  */
extern void
ada_substitution_array_inc_ref(ada_substitution_array a);

/* Decrement the ref-count for "a". This deallocates it if the ref-count drops
   to 0.  */
extern void
ada_substitution_array_dec_ref(ada_substitution_array a);


        



/*

 */
struct ada_analysis_unit_array_record {
   int n;
   int ref_count;
   ada_analysis_unit items[1];
};

/* Create a length-sized array.  */
extern ada_analysis_unit_array
ada_analysis_unit_array_create(int length);

/* Increment the ref-count for "a".  */
extern void
ada_analysis_unit_array_inc_ref(ada_analysis_unit_array a);

/* Decrement the ref-count for "a". This deallocates it if the ref-count drops
   to 0.  */
extern void
ada_analysis_unit_array_dec_ref(ada_analysis_unit_array a);


        



/*

 */
struct ada_unbounded_text_type_array_record {
   int n;
   int ref_count;
   ada_symbol_type items[1];
};

/* Create a length-sized array.  */
extern ada_unbounded_text_type_array
ada_unbounded_text_type_array_create(int length);

/* Increment the ref-count for "a".  */
extern void
ada_unbounded_text_type_array_inc_ref(ada_unbounded_text_type_array a);

/* Decrement the ref-count for "a". This deallocates it if the ref-count drops
   to 0.  */
extern void
ada_unbounded_text_type_array_dec_ref(ada_unbounded_text_type_array a);



/*
 * Analysis primitives
 */

/*
 * Create a new analysis context.
 *
 * ``Charset`` will be used as a default charset to decode input sources in
 * analysis units. Please see ``GNATCOLL.Iconv`` for several supported
 * charsets. Be careful: passing an unsupported charset is not guaranteed to
 * raise an error here. If no charset is provided, ``"iso-8859-1"`` is the
 * default.
 *
 * .. TODO: Passing an unsupported charset here is not guaranteed to raise an
 *    error right here, but this would be really helpful for users.
 *
 * When ``With_Trivia`` is true, the parsed analysis units will contain
 * trivias.
 *
 * If provided, ``File_Reader`` will be used to fetch the contents of source
 * files instead of the default, which is to just read it from the filesystem
 * and decode it using the regular charset rules. Note that if provided, all
 * parsing APIs that provide a buffer are forbidden, and any use of the
 * rewriting API with the returned context is rejected.
 *
 * If provided, ``Unit_Provider`` will be used to query the file name that
 * corresponds to a unit reference during semantic analysis. If it is ``NULL``,
 * the default one is used instead.
 *
 * ``Tab_Stop`` is a positive number to describe the effect of tabulation
 * characters on the column number in source files.
 */
extern ada_analysis_context
ada_create_analysis_context(
   const char *charset,
   ada_file_reader file_reader,
   ada_unit_provider unit_provider,
   ada_event_handler event_handler,
   int with_trivia,
   int tab_stop
);

/*
 * Increase the reference count to an analysis context. Return the reference
 * for convenience.
 */
extern ada_analysis_context
ada_context_incref(ada_analysis_context context);

/*
 * Decrease the reference count to an analysis context. Destruction happens
 * when the ref-count reaches 0.
 */
extern void
ada_context_decref(ada_analysis_context context);

/*
 * If the given string is a valid symbol, yield it as a symbol and return true.
 * Otherwise, return false.
 */
extern int
ada_context_symbol(ada_analysis_context context,
                                   ada_text *text,
                                   ada_symbol_type *symbol);

/*
 * Debug helper. Set whether ``Property_Error`` exceptions raised in
 * ``Populate_Lexical_Env`` should be discarded. They are by default.
 */
extern void
ada_context_discard_errors_in_populate_lexical_env(
        ada_analysis_context context,
        int discard);

/*
 * Create a new analysis unit for ``Filename`` or return the existing one if
 * any. If ``Reparse`` is true and the analysis unit already exists, reparse it
 * from ``Filename``.
 *
 * ``Rule`` controls which grammar rule is used to parse the unit.
 *
 * Use ``Charset`` in order to decode the source. If ``Charset`` is empty then
 * use the context's default charset.
 *
 * If any failure occurs, such as file opening, decoding, lexing or parsing
 * failure, return an analysis unit anyway: errors are described as diagnostics
 * of the returned analysis unit.
 */
extern ada_analysis_unit
ada_get_analysis_unit_from_file(
        ada_analysis_context context,
        const char *filename,
        const char *charset,
        int reparse,
        ada_grammar_rule rule);

/*
 * Create a new analysis unit for ``Filename`` or return the existing one if
 * any. Whether the analysis unit already exists or not, (re)parse it from the
 * source code in ``Buffer``.
 *
 * ``Rule`` controls which grammar rule is used to parse the unit.
 *
 * Use ``Charset`` in order to decode the source. If ``Charset`` is empty then
 * use the context's default charset.
 *
 * If any failure occurs, such as file opening, decoding, lexing or parsing
 * failure, return an analysis unit anyway: errors are described as diagnostics
 * of the returned analysis unit.
 */
extern ada_analysis_unit
ada_get_analysis_unit_from_buffer(
        ada_analysis_context context,
        const char *filename,
        const char *charset,
        const char *buffer,
        size_t buffer_size,
        ada_grammar_rule rule);

/*
 * Create a new analysis unit for ``Name``/``Kind`` or return the existing one
 * if any. If ``Reparse`` is true and the analysis unit already exists, reparse
 * it from the on-disk source file.
 *
 * The ``Name`` and ``Kind`` arguments are forwarded directly to query the
 * context's unit provider and get the filename for the returned unit. See the
 * documentation of the relevant unit provider for their exact semantics.
 *
 * Use ``Charset`` in order to decode the source. If ``Charset`` is empty then
 * use the context's default charset.
 *
 * If the unit name cannot be tuned into a file name, return ``NULL``. If any
 * other failure occurs, such as file opening, decoding, lexing or parsing
 * failure, return an analysis unit anyway: errors are described as diagnostics
 * of the returned analysis unit.
 */
extern ada_analysis_unit
ada_get_analysis_unit_from_provider(
        ada_analysis_context context,
        ada_text *name,
        ada_analysis_unit_kind kind,
        const char *charset,
        int reparse);

/*
 * Return the root node for this unit, or ``NULL`` if there is none.
 */
extern void
ada_unit_root(ada_analysis_unit unit,
                              ada_base_entity *result_p);

/*
 * Return a reference to the first token scanned in this unit.
 */
extern void
ada_unit_first_token(ada_analysis_unit unit,
                                     ada_token *token);

/*
 * Return a reference to the last token scanned in this unit.
 */
extern void
ada_unit_last_token(ada_analysis_unit unit,
                                    ada_token *token);

/*
 * Return the number of tokens in this unit.
 */
extern int
ada_unit_token_count(ada_analysis_unit unit);

/*
 * Return the number of trivias in this unit. This is 0 for units that were
 * parsed with trivia analysis disabled.
 */
extern int
ada_unit_trivia_count(ada_analysis_unit unit);

/*
 * Debug helper: output the lexical envs for the given analysis unit.
 */
extern void
ada_unit_dump_lexical_env(ada_analysis_unit unit);

/*
 * Return the filename this unit is associated to.
 *
 * The returned string is dynamically allocated and the caller must free it
 * when done with it.
 */
extern char *
ada_unit_filename(ada_analysis_unit unit);

/*
 * Return the number of diagnostics associated to this unit.
 */
extern unsigned
ada_unit_diagnostic_count(ada_analysis_unit unit);

/*
 * Get the Nth diagnostic in this unit and store it into ``*diagnostic_p``.
 * Return zero on failure (when N is too big).
 */
extern int
ada_unit_diagnostic(ada_analysis_unit unit,
                                    unsigned n,
                                    ada_diagnostic *diagnostic_p);

/*
 * Return the context that owns this unit.
 */
extern ada_analysis_context
ada_unit_context(ada_analysis_unit context);

/*
 * Reparse an analysis unit from the associated file.
 *
 * Use ``Charset`` in order to decode the source. If ``Charset`` is empty then
 * use the context's default charset.
 *
 * If any failure occurs, such as decoding, lexing or parsing failure,
 * diagnostic are emitted to explain what happened.
 */
extern void
ada_unit_reparse_from_file(ada_analysis_unit unit,
                                           const char *charset);

/*
 * Reparse an analysis unit from a buffer.
 *
 * Use ``Charset`` in order to decode the source. If ``Charset`` is empty then
 * use the context's default charset.
 *
 * If any failure occurs, such as decoding, lexing or parsing failure,
 * diagnostic are emitted to explain what happened.
 */
extern void
ada_unit_reparse_from_buffer (ada_analysis_unit unit,
                                              const char *charset,
                                              const char *buffer,
                                              size_t buffer_size);

/*
 * Create lexical environments for this analysis unit, according to the
 * specifications given in the language spec.
 *
 * If not done before, it will be automatically called during semantic
 * analysis. Calling it before enables one to control where the latency occurs.
 *
 * Depending on whether errors are discarded (see
 * ``Discard_Errors_In_Populate_Lexical_Env``), return ``0`` on failure and
 * ``1`` on success.
 */
extern int
ada_unit_populate_lexical_env(ada_analysis_unit unit);

/*
 * General AST node primitives
 */

/*
 * Return whether this node is a null node reference.
 */
static inline int
ada_node_is_null(ada_base_entity *node) {
    return node->node == NULL;
}

/*
 * Return the kind of this node.
 */
extern ada_node_kind_enum
ada_node_kind(ada_base_entity *node);

/*
 * Helper for textual dump: return the kind name for this node. The returned
 * string is a copy and thus must be free'd by the caller.
 */
extern void
ada_kind_name(ada_node_kind_enum kind, ada_text *result);

/*
 * Return the analysis unit that owns this node.
 */
extern int
ada_node_unit(ada_base_entity *node,
                              ada_analysis_unit *unit_p);

/*
 * Return whether this node is a node that contains only a single token.
 */
extern int
ada_node_is_token_node(ada_base_entity *node);

/*
 * Return whether this node is synthetic.
 */
extern int
ada_node_is_synthetic(ada_base_entity *node);

/*
 * Return a representation of this node as a string.
 */
extern void
ada_node_image(ada_base_entity *node,
                               ada_text *result);

/*
 * Return the source buffer slice corresponding to the text that spans between
 * the first and the last tokens of this node.
 *
 * Note that this returns the empty string for synthetic nodes.
 */
extern void
ada_node_text(ada_base_entity *node,
                              ada_text *text);

/*
 * Return the spanning source location range for this node.
 *
 * Note that this returns the sloc of the parent for synthetic nodes.
 */
extern void
ada_node_sloc_range(ada_base_entity *node,
                                    ada_source_location_range *sloc_range);

/*
 * Return the bottom-most node from in ``Node`` and its children which contains
 * ``Sloc``, or ``NULL`` if there is none.
 */
extern void
ada_lookup_in_node(ada_base_entity *node,
                                   const ada_source_location *sloc,
                                   ada_base_entity *result_p);

/*
 * Return the number of children in this node.
 */
extern unsigned
ada_node_children_count(ada_base_entity *node);

/*
 * Return the Nth child for in this node's fields and store it into
 * ``*child_p``.  Return zero on failure (when ``N`` is too big).
 */
extern int
ada_node_child(ada_base_entity *node,
                               unsigned n,
                               ada_base_entity* child_p);

/*
 * Encode some text using the current locale. The result is dynamically
 * allocated: it is up to the caller to free it when done with it.
 *
 * This is a development helper to make it quick and easy to print token and
 * diagnostic text: it ignores errors (when the locale does not support some
 * characters). Production code should use real conversion routines such as
 * libiconv's in order to deal with UTF-32 texts.
 */
extern char *
ada_text_to_locale_string(ada_text *text);

/*
 * Free dynamically allocated memory.
 *
 * This is a helper to free objects from dynamic languages.
 */
extern void
ada_free(void *address);

/*
 * If this text object owns the buffer it references, free this buffer.
 *
 * Note that even though this accepts a pointer to a text object, it does not
 * deallocates the text object itself but rather the buffer it references.
 */
extern void
ada_destroy_text(ada_text *text);

/*
 * Return the text associated to this symbol.
 */
extern void
ada_symbol_text(ada_symbol_type *symbol,
                                ada_text *text);

/*
 * Create a big integer from its string representation (in base 10).
 */
extern ada_big_integer
ada_create_big_integer(ada_text *text);

/*
 * Return the string representation (in base 10) of this big integer.
 */
extern void
ada_big_integer_text(ada_big_integer bigint,
                                     ada_text *text);

/*
 * Decrease the reference count for this big integer.
 */
extern void
ada_big_integer_decref(ada_big_integer bigint);

/*
 * Allocate strings to represent the library version number and build date and
 * put them in Version/Build_Date. Callers are expected to call free() on the
 * returned string once done.
 */
extern void
ada_get_versions(char **version, char **build_date);

/*
 * Create a string value from its content (UTF32 with native endianity).
 *
 * Note that the CONTENT buffer argument is copied: the returned value does not
 * contain a reference to it.
 */
extern ada_string_type
ada_create_string(uint32_t *content, int length);

/*
 * Decrease the reference count for this string.
 */
extern void
ada_string_dec_ref(ada_string_type self);

/*
 * Kind-specific AST node primitives
 */

/* All these primitives return their result through an OUT parameter.  They
   return a boolean telling whether the operation was successful (it can fail
   if the node does not have the proper type, for instance).  When an AST node
   is returned, its ref-count is left as-is.  */

        



/*
 * Return the scope of definition of this basic declaration.
 */
extern int ada_ada_node_p_declarative_scope(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * Return the compilation unit containing this node.
 *
 * .. note:: This returns the ``ada_compilation_unit`` node, which is different
 *    from the ``AnalysisUnit``. In particular, an analysis unit can contain
 *    multiple compilation units.
 */
extern int ada_ada_node_p_enclosing_compilation_unit(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * Assuming this node comes from an instantiated generic declaration, return
 * its non-instantiated counterpart lying in the generic declaration.
 */
extern int ada_ada_node_p_get_uninstantiated_node(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * Return possible completions at this point in the file.
 */
extern int ada_ada_node_p_complete(
    ada_base_entity *node,


    ada_completion_item_iterator *value_p
);


        



/*
 * Return the list of keywords that are valid at this point in the file.
 *
 * .. note:: This is work in progress. It will return all keywords for now,
 *    without looking at the context.
 */
extern int ada_ada_node_p_valid_keywords(
    ada_base_entity *node,


    ada_unbounded_text_type_array *value_p
);


        



/*
 * Return the potentially empty list of generic package/subprogram
 * instantiations that led to the creation of this entity. Outer-most
 * instantiations appear last.
 */
extern int ada_ada_node_p_generic_instantiations(
    ada_base_entity *node,


    ada_ada_node_array *value_p
);


        



/*
 * Return the semantic parent for this node, if applicable, null otherwise.
 *
 * .. note:: A node lying outside of a library item's declaration or subunit's
 *    body does not have a parent environment, meaning that this property will
 *    return null.
 */
extern int ada_ada_node_p_semantic_parent(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * Return the parent basic decl for this node, if applicable, null otherwise.
 *
 * .. note:: If the parent BasicDecl of the given node is a generic
 *    declaration, this call will return the instantiation from which the node
 *    was retrieved instead, if any.
 *
 * .. note:: When called on a subunit's body, this property will return the its
 *    corresponding body stub.
 *
 * .. note:: When called on a node lying outside of a library item's
 *    declaration or subunit's body this property will return null.
 */
extern int ada_ada_node_p_parent_basic_decl(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * Filters out among the list of given units those that cannot refer to the
 * unit in which this node lies. If transitive is True, the whole transitive
 * closure of imports will be used to find a reference to the unit of this
 * node.
 */
extern int ada_ada_node_p_filter_is_imported_by(
    ada_base_entity *node,

        
        ada_analysis_unit_array
        units,
        
        ada_bool
        transitive,

    ada_analysis_unit_array *value_p
);


        



/*
 * Designates entities that are entry point for the xref solving
 * infrastructure. If this returns true, then resolve_names can be called on
 * it.
 *
 * .. note:: For convenience, and unlike what is defined in the ARM wrt.
 *    complete contexts for name resolution, ``xref_entry_points`` can be
 *    nested.
 */
extern int ada_ada_node_p_xref_entry_point(
    ada_base_entity *node,


    ada_bool *value_p
);


        



/*
 * This will resolve names for this node. If the operation is successful, then
 * type_var and ref_var will be bound on appropriate subnodes of the statement.
 */
extern int ada_ada_node_p_resolve_names(
    ada_base_entity *node,


    ada_bool *value_p
);


        



/*
 * Static method. Return the analysis unit corresponding to the Standard
 * package.
 */
extern int ada_ada_node_p_standard_unit(
    ada_base_entity *node,


    ada_analysis_unit *value_p
);


        



/*
 * Static property. Return an entity from the standard package with name
 * ``sym``.
 */
extern int ada_ada_node_p_std_entity(
    ada_base_entity *node,

        
        const ada_symbol_type*
        sym,

    ada_base_entity *value_p
);


        



/*
 * Static method. Return the standard Boolean type.
 */
extern int ada_ada_node_p_bool_type(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * Static method. Return the standard Integer type.
 */
extern int ada_ada_node_p_int_type(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * Static method. Return the standard Universal Integer type.
 */
extern int ada_ada_node_p_universal_int_type(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * Static method. Return the standard Universal Real type.
 */
extern int ada_ada_node_p_universal_real_type(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * Static method. Return the standard Character type.
 */
extern int ada_ada_node_p_std_char_type(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * Static method. Return the standard Wide_Character type.
 */
extern int ada_ada_node_p_std_wide_char_type(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * Static method. Return the standard Wide_Wide_Character type.
 */
extern int ada_ada_node_p_std_wide_wide_char_type(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * Static method. Get the top-level decl in ``unit``.  This is the body of a
 * Subunit, or the item of a ``LibraryItem``.
 */
extern int ada_ada_node_p_top_level_decl(
    ada_base_entity *node,

        
        ada_analysis_unit
        unit,

    ada_base_entity *value_p
);


        



/*
 * Assuming that self is a choice expression (such as what can appear in an
 * alternative of a case statement or in the RHS of a membership expression,
 * this property returns whether the given value satisfies it.
 *
 * .. attention:: This is an experimental feature, so even if it is exposed to
 *    allow experiments, it is totally unsupported and the API and behavior are
 *    very likely to change in the future.
 */
extern int ada_ada_node_p_choice_match(
    ada_base_entity *node,

        
        const ada_big_integer*
        value,

    ada_bool *value_p
);


        



/*
 * Return a cross reference from this name to a defining identifier, trying to
 * mimic GNAT's xrefs as much as possible.
 */
extern int ada_ada_node_p_gnat_xref(
    ada_base_entity *node,

        
        ada_bool
        imprecise_fallback,

    ada_base_entity *value_p
);


        



/*
 * Return the syntactic parent for this node. Return null for the root node.
 */
extern int ada_ada_node_parent(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * Return an array that contains the lexical parents, this node included iff
 * ``with_self`` is True. Nearer parents are first in the list.
 */
extern int ada_ada_node_parents(
    ada_base_entity *node,

        
        ada_bool
        with_self,

    ada_ada_node_array *value_p
);


        



/*
 * Return an array that contains the direct lexical children.
 *
 * .. warning:: This constructs a whole array every-time you call it, and as
 *    such is less efficient than calling the ``Child`` built-in.
 */
extern int ada_ada_node_children(
    ada_base_entity *node,


    ada_ada_node_array *value_p
);


        



/*
 * Return the first token used to parse this node.
 */
extern int ada_ada_node_token_start(
    ada_base_entity *node,


    ada_token *value_p
);


        



/*
 * Return the last token used to parse this node.
 */
extern int ada_ada_node_token_end(
    ada_base_entity *node,


    ada_token *value_p
);


        



/*
 * Return the 0-based index for Node in its parent's children.
 */
extern int ada_ada_node_child_index(
    ada_base_entity *node,


    int *value_p
);


        



/*
 * Return the node's previous sibling, or null if there is no such sibling.
 */
extern int ada_ada_node_previous_sibling(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * Return the node's next sibling, or null if there is no such sibling.
 */
extern int ada_ada_node_next_sibling(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * Return the analysis unit owning this node.
 */
extern int ada_ada_node_unit(
    ada_base_entity *node,


    ada_analysis_unit *value_p
);


        



/*
 * Return whether the node is a ghost.
 *
 * Unlike regular nodes, ghost nodes cover no token in the input source: they
 * are logically located instead between two tokens. Both the ``token_start``
 * and the ``token_end`` of all ghost nodes is the token right after this
 * logical position.
 */
extern int ada_ada_node_is_ghost(
    ada_base_entity *node,


    ada_bool *value_p
);


        



/*
 * Return a string containing the filename + the sloc in GNU conformant format.
 * Useful to create diagnostics from a node.
 */
extern int ada_ada_node_full_sloc_image(
    ada_base_entity *node,


    ada_string_type *value_p
);


        



/*
 * Return whether this is an instance of AbortPresent
 */
extern int ada_abort_node_p_as_bool(
    ada_base_entity *node,


    ada_bool *value_p
);


        



/*
 * Return whether this is an instance of AbstractPresent
 */
extern int ada_abstract_node_p_as_bool(
    ada_base_entity *node,


    ada_bool *value_p
);


        



/*
 * Returns an array of pairs, associating formal parameters to actual
 * expressions. The formals to match are retrieved by resolving the call which
 * this AssocList represents the actuals of.
 */
extern int ada_assoc_list_p_zip_with_params(
    ada_base_entity *node,

        
        ada_bool
        imprecise_fallback,

    ada_param_actual_array *value_p
);


        



/*
 * Return whether this is an instance of AliasedPresent
 */
extern int ada_aliased_node_p_as_bool(
    ada_base_entity *node,


    ada_bool *value_p
);


        



/*
 * Return whether this is an instance of AllPresent
 */
extern int ada_all_node_p_as_bool(
    ada_base_entity *node,


    ada_bool *value_p
);


        



/*
 * This field contains a list that itself contains one of the following nodes:
 * ``ada_attribute_ref``, ``ada_bin_op``, ``ada_call_expr``,
 * ``ada_char_literal``, ``ada_dotted_name``, ``ada_explicit_deref``,
 * ``ada_identifier``, ``ada_qual_expr``, ``ada_reduce_attribute_ref``,
 * ``ada_string_literal``, ``ada_subtype_indication``, ``ada_target_name``,
 * ``ada_update_attribute_ref``
 */
extern int ada_constrained_array_indices_f_list(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_unconstrained_array_indices_f_types(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_attribute_ref``,
 * ``ada_call_expr``, ``ada_char_literal``, ``ada_dotted_name``,
 * ``ada_explicit_deref``, ``ada_identifier``, ``ada_qual_expr``,
 * ``ada_reduce_attribute_ref``, ``ada_string_literal``, ``ada_target_name``,
 * ``ada_update_attribute_ref``
 */
extern int ada_aspect_assoc_f_id(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes:
 * ``ada_abstract_state_decl_expr``, ``ada_allocator``, ``ada_attribute_ref``,
 * ``ada_base_aggregate``, ``ada_bin_op``, ``ada_call_expr``,
 * ``ada_char_literal``, ``ada_concat_op``, ``ada_cond_expr``,
 * ``ada_contract_cases``, ``ada_decl_expr``, ``ada_dotted_name``,
 * ``ada_explicit_deref``, ``ada_identifier``, ``ada_membership_expr``,
 * ``ada_null_literal``, ``ada_num_literal``, ``ada_paren_expr``,
 * ``ada_qual_expr``, ``ada_quantified_expr``, ``ada_raise_expr``,
 * ``ada_reduce_attribute_ref``, ``ada_string_literal``, ``ada_target_name``,
 * ``ada_un_op``, ``ada_update_attribute_ref``
 */
extern int ada_aspect_assoc_f_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * Return whether this aspect is ghost code or not. See SPARK RM 6.9.
 */
extern int ada_aspect_assoc_p_is_ghost_code(
    ada_base_entity *node,


    ada_bool *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_char_literal``,
 * ``ada_identifier``, ``ada_string_literal``
 */
extern int ada_at_clause_f_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_allocator``,
 * ``ada_attribute_ref``, ``ada_base_aggregate``, ``ada_bin_op``,
 * ``ada_call_expr``, ``ada_char_literal``, ``ada_concat_op``,
 * ``ada_cond_expr``, ``ada_decl_expr``, ``ada_dotted_name``,
 * ``ada_explicit_deref``, ``ada_identifier``, ``ada_membership_expr``,
 * ``ada_null_literal``, ``ada_num_literal``, ``ada_paren_expr``,
 * ``ada_qual_expr``, ``ada_quantified_expr``, ``ada_raise_expr``,
 * ``ada_reduce_attribute_ref``, ``ada_string_literal``, ``ada_target_name``,
 * ``ada_un_op``, ``ada_update_attribute_ref``
 */
extern int ada_at_clause_f_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_attribute_ref``,
 * ``ada_call_expr``, ``ada_char_literal``, ``ada_dotted_name``,
 * ``ada_explicit_deref``, ``ada_identifier``, ``ada_qual_expr``,
 * ``ada_reduce_attribute_ref``, ``ada_string_literal``, ``ada_target_name``,
 * ``ada_update_attribute_ref``
 */
extern int ada_attribute_def_clause_f_attribute_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_allocator``,
 * ``ada_attribute_ref``, ``ada_base_aggregate``, ``ada_bin_op``,
 * ``ada_call_expr``, ``ada_char_literal``, ``ada_concat_op``,
 * ``ada_cond_expr``, ``ada_decl_expr``, ``ada_dotted_name``,
 * ``ada_explicit_deref``, ``ada_identifier``, ``ada_membership_expr``,
 * ``ada_null_literal``, ``ada_num_literal``, ``ada_paren_expr``,
 * ``ada_qual_expr``, ``ada_quantified_expr``, ``ada_raise_expr``,
 * ``ada_reduce_attribute_ref``, ``ada_string_literal``, ``ada_target_name``,
 * ``ada_un_op``, ``ada_update_attribute_ref``
 */
extern int ada_attribute_def_clause_f_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_char_literal``,
 * ``ada_dotted_name``, ``ada_identifier``, ``ada_string_literal``
 */
extern int ada_enum_rep_clause_f_type_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_enum_rep_clause_f_aggregate(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * Returns an array of pairs, associating enum literals to representation
 * clause actuals.
 */
extern int ada_enum_rep_clause_p_params(
    ada_base_entity *node,


    ada_param_actual_array *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_char_literal``,
 * ``ada_dotted_name``, ``ada_identifier``, ``ada_string_literal``
 */
extern int ada_record_rep_clause_f_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_allocator``,
 * ``ada_attribute_ref``, ``ada_base_aggregate``, ``ada_bin_op``,
 * ``ada_call_expr``, ``ada_char_literal``, ``ada_concat_op``,
 * ``ada_cond_expr``, ``ada_decl_expr``, ``ada_dotted_name``,
 * ``ada_explicit_deref``, ``ada_identifier``, ``ada_null_literal``,
 * ``ada_num_literal``, ``ada_paren_expr``, ``ada_qual_expr``,
 * ``ada_quantified_expr``, ``ada_raise_expr``, ``ada_reduce_attribute_ref``,
 * ``ada_string_literal``, ``ada_target_name``, ``ada_un_op``,
 * ``ada_update_attribute_ref``
 */
extern int ada_record_rep_clause_f_at_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field contains a list that itself contains one of the following nodes:
 * ``ada_component_clause``, ``ada_pragma_node``
 */
extern int ada_record_rep_clause_f_components(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_aspect_spec_f_aspect_assocs(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * Returns the expression side of this assoc node.
 */
extern int ada_base_assoc_p_assoc_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_allocator``,
 * ``ada_attribute_ref``, ``ada_base_aggregate``, ``ada_bin_op``,
 * ``ada_call_expr``, ``ada_char_literal``, ``ada_concat_op``,
 * ``ada_cond_expr``, ``ada_decl_expr``, ``ada_dotted_name``,
 * ``ada_explicit_deref``, ``ada_identifier``, ``ada_membership_expr``,
 * ``ada_null_literal``, ``ada_num_literal``, ``ada_others_designator``,
 * ``ada_paren_expr``, ``ada_qual_expr``, ``ada_quantified_expr``,
 * ``ada_raise_expr``, ``ada_reduce_attribute_ref``, ``ada_string_literal``,
 * ``ada_target_name``, ``ada_un_op``, ``ada_update_attribute_ref``
 */
extern int ada_contract_case_assoc_f_guard(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_allocator``,
 * ``ada_attribute_ref``, ``ada_base_aggregate``, ``ada_bin_op``,
 * ``ada_call_expr``, ``ada_char_literal``, ``ada_concat_op``,
 * ``ada_cond_expr``, ``ada_decl_expr``, ``ada_dotted_name``,
 * ``ada_explicit_deref``, ``ada_identifier``, ``ada_membership_expr``,
 * ``ada_null_literal``, ``ada_num_literal``, ``ada_paren_expr``,
 * ``ada_qual_expr``, ``ada_quantified_expr``, ``ada_raise_expr``,
 * ``ada_reduce_attribute_ref``, ``ada_string_literal``, ``ada_target_name``,
 * ``ada_un_op``, ``ada_update_attribute_ref``
 */
extern int ada_contract_case_assoc_f_consequence(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_attribute_ref``,
 * ``ada_identifier``
 */
extern int ada_pragma_argument_assoc_f_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_allocator``,
 * ``ada_attribute_ref``, ``ada_base_aggregate``, ``ada_bin_op``,
 * ``ada_call_expr``, ``ada_char_literal``, ``ada_concat_op``,
 * ``ada_cond_expr``, ``ada_decl_expr``, ``ada_dotted_name``,
 * ``ada_explicit_deref``, ``ada_identifier``, ``ada_membership_expr``,
 * ``ada_null_literal``, ``ada_num_literal``, ``ada_paren_expr``,
 * ``ada_qual_expr``, ``ada_quantified_expr``, ``ada_raise_expr``,
 * ``ada_reduce_attribute_ref``, ``ada_string_literal``, ``ada_target_name``,
 * ``ada_un_op``, ``ada_update_attribute_ref``
 */
extern int ada_pragma_argument_assoc_f_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * Return the list of abstract formal parameters for this holder.
 */
extern int ada_base_formal_param_holder_p_abstract_formal_params(
    ada_base_entity *node,


    ada_ada_node_array *value_p
);


        



/*
 * Return all parameters as a ``DefiningName`` array. This property doesn't
 * return record discriminants nor variants when called on a record component
 * list.
 */
extern int ada_base_formal_param_holder_p_formal_params(
    ada_base_entity *node,


    ada_ada_node_array *value_p
);


        



/*
 * Return the minimum number of parameters this subprogram can be called while
 * still being a legal call.
 */
extern int ada_base_formal_param_holder_p_nb_min_params(
    ada_base_entity *node,


    int *value_p
);


        



/*
 * Return the maximum number of parameters this subprogram can be called while
 * still being a legal call.
 */
extern int ada_base_formal_param_holder_p_nb_max_params(
    ada_base_entity *node,


    int *value_p
);


        



/*
 * Returns the type of each parameter of Self.
 */
extern int ada_base_formal_param_holder_p_param_types(
    ada_base_entity *node,

        
        const ada_base_entity*
        origin,

    ada_ada_node_array *value_p
);


        



/*
 * Syntax property. Return the type expression node corresponding to the return
 * of this subprogram spec.
 */
extern int ada_base_subp_spec_p_returns(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * Returns the array of parameters specification for this subprogram spec.
 */
extern int ada_base_subp_spec_p_params(
    ada_base_entity *node,


    ada_ada_node_array *value_p
);


        



/*
 * Return the types of which this subprogram is a primitive of.
 */
extern int ada_base_subp_spec_p_primitive_subp_types(
    ada_base_entity *node,

        
        ada_bool
        imprecise_fallback,

    ada_ada_node_array *value_p
);


        



/*
 * Return the first type of which this subprogram is a primitive of.
 */
extern int ada_base_subp_spec_p_primitive_subp_first_type(
    ada_base_entity *node,

        
        ada_bool
        imprecise_fallback,

    ada_base_entity *value_p
);


        



/*
 * If this subprogram is a primitive for a tagged type, then return this type.
 */
extern int ada_base_subp_spec_p_primitive_subp_tagged_type(
    ada_base_entity *node,

        
        ada_bool
        imprecise_fallback,

    ada_base_entity *value_p
);


        



/*
 * Returns the return type of Self, if applicable (e.g. if Self is a
 * subprogram). Else, returns null.
 */
extern int ada_base_subp_spec_p_return_type(
    ada_base_entity *node,

        
        const ada_base_entity*
        origin,

    ada_base_entity *value_p
);


        



/*

 */
extern int ada_entry_spec_f_entry_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_attribute_ref``,
 * ``ada_bin_op``, ``ada_call_expr``, ``ada_char_literal``,
 * ``ada_dotted_name``, ``ada_explicit_deref``, ``ada_identifier``,
 * ``ada_qual_expr``, ``ada_reduce_attribute_ref``, ``ada_string_literal``,
 * ``ada_subtype_indication``, ``ada_target_name``,
 * ``ada_update_attribute_ref``
 */
extern int ada_entry_spec_f_family_type(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_entry_spec_f_entry_params(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_subp_spec_f_subp_kind(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_subp_spec_f_subp_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_subp_spec_f_subp_params(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_anonymous_type``,
 * ``ada_subtype_indication``
 */
extern int ada_subp_spec_f_subp_returns(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_synthetic_binary_spec_f_left_param(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_synthetic_binary_spec_f_right_param(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_synthetic_binary_spec_f_return_type_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_synthetic_unary_spec_f_right_param(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_synthetic_unary_spec_f_return_type_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field contains a list that itself contains one of the following nodes:
 * ``ada_aspect_clause``, ``ada_component_decl``, ``ada_null_component_decl``,
 * ``ada_pragma_node``
 */
extern int ada_component_list_f_components(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_component_list_f_variant_part(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_known_discriminant_part_f_discr_specs(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_entry_completion_formal_params_f_params(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field contains a list that itself contains one of the following nodes:
 * ``ada_generic_formal``, ``ada_pragma_node``, ``ada_use_clause``
 */
extern int ada_generic_formal_part_f_decls(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_base_record_def_f_components(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * Return the list of parameters that this association refers to.
 */
extern int ada_basic_assoc_p_get_params(
    ada_base_entity *node,

        
        ada_bool
        imprecise_fallback,

    ada_ada_node_array *value_p
);


        



/*
 * This field contains a list that itself contains one of the following nodes:
 * ``ada_allocator``, ``ada_attribute_ref``, ``ada_base_aggregate``,
 * ``ada_bin_op``, ``ada_call_expr``, ``ada_char_literal``, ``ada_concat_op``,
 * ``ada_cond_expr``, ``ada_decl_expr``, ``ada_discrete_subtype_indication``,
 * ``ada_dotted_name``, ``ada_explicit_deref``, ``ada_identifier``,
 * ``ada_membership_expr``, ``ada_null_literal``, ``ada_num_literal``,
 * ``ada_others_designator``, ``ada_paren_expr``, ``ada_qual_expr``,
 * ``ada_quantified_expr``, ``ada_raise_expr``, ``ada_reduce_attribute_ref``,
 * ``ada_string_literal``, ``ada_target_name``, ``ada_un_op``,
 * ``ada_update_attribute_ref``
 */
extern int ada_aggregate_assoc_f_designators(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_allocator``,
 * ``ada_attribute_ref``, ``ada_base_aggregate``, ``ada_bin_op``,
 * ``ada_box_expr``, ``ada_call_expr``, ``ada_char_literal``,
 * ``ada_concat_op``, ``ada_cond_expr``, ``ada_decl_expr``,
 * ``ada_dotted_name``, ``ada_explicit_deref``, ``ada_identifier``,
 * ``ada_membership_expr``, ``ada_null_literal``, ``ada_num_literal``,
 * ``ada_paren_expr``, ``ada_qual_expr``, ``ada_quantified_expr``,
 * ``ada_raise_expr``, ``ada_reduce_attribute_ref``, ``ada_string_literal``,
 * ``ada_target_name``, ``ada_un_op``, ``ada_update_attribute_ref``
 */
extern int ada_aggregate_assoc_f_r_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_composite_constraint_assoc_f_ids(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_allocator``,
 * ``ada_attribute_ref``, ``ada_base_aggregate``, ``ada_bin_op``,
 * ``ada_call_expr``, ``ada_char_literal``, ``ada_concat_op``,
 * ``ada_cond_expr``, ``ada_decl_expr``, ``ada_discrete_subtype_indication``,
 * ``ada_dotted_name``, ``ada_explicit_deref``, ``ada_identifier``,
 * ``ada_membership_expr``, ``ada_null_literal``, ``ada_num_literal``,
 * ``ada_paren_expr``, ``ada_qual_expr``, ``ada_quantified_expr``,
 * ``ada_raise_expr``, ``ada_reduce_attribute_ref``, ``ada_string_literal``,
 * ``ada_target_name``, ``ada_un_op``, ``ada_update_attribute_ref``
 */
extern int ada_composite_constraint_assoc_f_constraint_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_iterated_assoc_f_spec(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_allocator``,
 * ``ada_attribute_ref``, ``ada_base_aggregate``, ``ada_bin_op``,
 * ``ada_call_expr``, ``ada_char_literal``, ``ada_concat_op``,
 * ``ada_cond_expr``, ``ada_decl_expr``, ``ada_dotted_name``,
 * ``ada_explicit_deref``, ``ada_identifier``, ``ada_membership_expr``,
 * ``ada_null_literal``, ``ada_num_literal``, ``ada_paren_expr``,
 * ``ada_qual_expr``, ``ada_quantified_expr``, ``ada_raise_expr``,
 * ``ada_reduce_attribute_ref``, ``ada_string_literal``, ``ada_target_name``,
 * ``ada_un_op``, ``ada_update_attribute_ref``
 */
extern int ada_iterated_assoc_f_r_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_identifier``,
 * ``ada_others_designator``, ``ada_string_literal``
 */
extern int ada_param_assoc_f_designator(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_allocator``,
 * ``ada_attribute_ref``, ``ada_base_aggregate``, ``ada_bin_op``,
 * ``ada_box_expr``, ``ada_call_expr``, ``ada_char_literal``,
 * ``ada_concat_op``, ``ada_cond_expr``, ``ada_decl_expr``,
 * ``ada_dotted_name``, ``ada_explicit_deref``, ``ada_identifier``,
 * ``ada_membership_expr``, ``ada_null_literal``, ``ada_num_literal``,
 * ``ada_paren_expr``, ``ada_qual_expr``, ``ada_quantified_expr``,
 * ``ada_raise_expr``, ``ada_reduce_attribute_ref``, ``ada_string_literal``,
 * ``ada_target_name``, ``ada_un_op``, ``ada_update_attribute_ref``
 */
extern int ada_param_assoc_f_r_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * Whether this decl is the nested decl of a generic formal declaration.
 */
extern int ada_basic_decl_p_is_formal(
    ada_base_entity *node,


    ada_bool *value_p
);


        



/*
 * Return the documentation annotations associated with this decl. Annotations
 * are any comment line of the form:
 *
 * .. code::
 *
 *    --% [annotation_name]: [annotation]
 *
 * Raises a property error if the doc is incorrectly formatted.
 *
 * .. attention:: This is an experimental feature, so even if it is exposed to
 *    allow experiments, it is totally unsupported and the API and behavior are
 *    very likely to change in the future.
 */
extern int ada_basic_decl_p_doc_annotations(
    ada_base_entity *node,


    ada_doc_annotation_array *value_p
);


        



/*
 * Return the documentation associated with this decl. Raises a property error
 * if the doc is incorrectly formatted.
 *
 * .. attention:: This is an experimental feature, so even if it is exposed to
 *    allow experiments, it is totally unsupported and the API and behavior are
 *    very likely to change in the future.
 */
extern int ada_basic_decl_p_doc(
    ada_base_entity *node,


    ada_string_type *value_p
);


        



/*
 * Return the previous part for this decl, if applicable.
 *
 * .. note:: It is not named previous_part, because BaseTypeDecl has a more
 *    precise version of previous_part that returns a BaseTypeDecl. Probably,
 *    we want to rename the specific versions, and have the root property be
 *    named previous_part. (TODO R925-008)
 */
extern int ada_basic_decl_p_previous_part_for_decl(
    ada_base_entity *node,

        
        ada_bool
        imprecise_fallback,

    ada_base_entity *value_p
);


        



/*
 * Return the canonical part for this decl. In the case of decls composed of
 * several parts, the canonical part will be the first part.
 */
extern int ada_basic_decl_p_canonical_part(
    ada_base_entity *node,

        
        ada_bool
        imprecise_fallback,

    ada_base_entity *value_p
);


        



/*
 * Return all parts that define this entity, sorted from first part to last
 * part.
 */
extern int ada_basic_decl_p_all_parts(
    ada_base_entity *node,

        
        ada_bool
        imprecise_fallback,

    ada_ada_node_array *value_p
);


        



/*
 * Return whether this declaration is static.
 */
extern int ada_basic_decl_p_is_static_decl(
    ada_base_entity *node,

        
        ada_bool
        imprecise_fallback,

    ada_bool *value_p
);


        



/*
 * Return the list of aspects that are attached to this node.
 */
extern int ada_basic_decl_f_aspects(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * Return the aspect with name ``name`` for this entity.
 */
extern int ada_basic_decl_p_get_aspect_assoc(
    ada_base_entity *node,

        
        const ada_symbol_type*
        name,

    ada_base_entity *value_p
);


        



/*
 * Return the expression associated to the aspect with name ``name`` for this
 * entity.
 */
extern int ada_basic_decl_p_get_aspect_spec_expr(
    ada_base_entity *node,

        
        const ada_symbol_type*
        name,

    ada_base_entity *value_p
);


        



/*
 * Return the aspect with name ``name`` associated to this entity.
 *
 * Aspects are properties of entities that can be specified by the Ada program,
 * either via aspect specifications, pragmas, or attributes.
 *
 * This will return the syntactic node corresponding to attribute directly.
 *
 * Note: for some aspects (e.g. Inline), Libadalang will check if they are
 * defined on any part of the entity.
 */
extern int ada_basic_decl_p_get_aspect(
    ada_base_entity *node,

        
        const ada_symbol_type*
        name,
        
        ada_bool
        imprecise_fallback,

    ada_internal_aspect *value_p
);


        



/*
 * Returns whether the boolean aspect named ``name`` is set on the entity
 * represented by this node.
 *
 * "Aspect" is used as in RM terminology (see :rmlink:`13`).
 */
extern int ada_basic_decl_p_has_aspect(
    ada_base_entity *node,

        
        const ada_symbol_type*
        name,
        
        ada_bool
        imprecise_fallback,

    ada_bool *value_p
);


        



/*
 * Return the pragma with name ``name`` associated to this entity.
 *
 * Please use the ``p_get_aspects`` property instead if you are interested in
 * aspects, i.e. information that can be represented by either aspect
 * specification nodes, pragma nodes or attribute definition nodes.
 */
extern int ada_basic_decl_p_get_pragma(
    ada_base_entity *node,

        
        const ada_symbol_type*
        name,

    ada_base_entity *value_p
);


        



/*
 * Return the representation clause associated to this type decl that defines
 * the given attribute name.
 */
extern int ada_basic_decl_p_get_representation_clause(
    ada_base_entity *node,

        
        const ada_symbol_type*
        name,
        
        ada_bool
        imprecise_fallback,

    ada_base_entity *value_p
);


        



/*
 * Return the at clause associated to this declaration.
 */
extern int ada_basic_decl_p_get_at_clause(
    ada_base_entity *node,

        
        ada_bool
        imprecise_fallback,

    ada_base_entity *value_p
);


        



/*
 * Whether this declaration is imported from another language.
 */
extern int ada_basic_decl_p_is_imported(
    ada_base_entity *node,


    ada_bool *value_p
);


        



/*
 * Return whether this declaration is ghost code or not. See SPARK RM 6.9.
 */
extern int ada_basic_decl_p_is_ghost_code(
    ada_base_entity *node,


    ada_bool *value_p
);


        



/*
 * Whether a BasicDecl is the root decl for its unit.
 */
extern int ada_basic_decl_p_is_compilation_unit_root(
    ada_base_entity *node,


    ada_bool *value_p
);


        



/*
 * Return whether this declaration is visible from the point of view of the
 * given ``origin`` node.
 *
 * .. attention:: Only package-level (public or private) declarations are
 *    supported for now.
 */
extern int ada_basic_decl_p_is_visible(
    ada_base_entity *node,

        
        const ada_base_entity*
        from_node,

    ada_bool *value_p
);


        



/*
 * If Self declares a primitive subprogram of some tagged type T, return the
 * set of all subprogram declarations that it overrides (including itself).
 *
 * .. note:: for the moment this only works for tagged types. Remains to be
 *    seen if we need to extend it.
 */
extern int ada_basic_decl_p_base_subp_declarations(
    ada_base_entity *node,

        
        ada_bool
        imprecise_fallback,

    ada_ada_node_array *value_p
);


        



/*
 * If Self declares a primitive subprogram of some tagged type T, return the
 * root subprogram declarations that it overrides. There can be several, as in
 * the following scenario:
 *
 * * package Root defines the root tagged type T and subprogram Foo.
 *
 * * package Itf defines interface I and abstract subprogram Foo.
 *
 * * package D defines "type U is new Root.T and Itf.I" and an overriding
 *   subprogram Foo.
 *
 * Here, root_subp_declarations of Foo defined in package D will return both
 * Foo from package Root and Foo from package Itf.
 */
extern int ada_basic_decl_p_root_subp_declarations(
    ada_base_entity *node,

        
        const ada_base_entity*
        origin,
        
        ada_bool
        imprecise_fallback,

    ada_ada_node_array *value_p
);


        



/*
 * If Self is the declaration of a primitive of some type T, return the list of
 * all subprogram that override this subprogram among the given units.
 */
extern int ada_basic_decl_p_find_all_overrides(
    ada_base_entity *node,

        
        ada_analysis_unit_array
        units,
        
        ada_bool
        imprecise_fallback,

    ada_ada_node_array *value_p
);


        



/*
 * Get all the names of this basic declaration.
 */
extern int ada_basic_decl_p_defining_names(
    ada_base_entity *node,


    ada_ada_node_array *value_p
);


        



/*
 * Get the name of this declaration. If this declaration has several names, it
 * will return the first one.
 */
extern int ada_basic_decl_p_defining_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * Return the type expression for this BasicDecl if applicable, a null
 * otherwise.
 */
extern int ada_basic_decl_p_type_expression(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * If Self is a Subp, returns the specification of this subprogram.
 *
 * If ``follow_generic`` is True, will also work for instances of
 * ``GenericSubpDecl``.
 */
extern int ada_basic_decl_p_subp_spec_or_null(
    ada_base_entity *node,

        
        ada_bool
        follow_generic,

    ada_base_entity *value_p
);


        



/*
 * Return True if self is a subprogram node in the general sense (which is, an
 * entity that can be called). This includes separates and entries.
 *
 * .. attention: This is a purely syntactic query and will return True for
 *    everything that is a syntactic entity that can be called like a
 *    subprogram in some contexts, even generic formal subprograms for example.
 */
extern int ada_basic_decl_p_is_subprogram(
    ada_base_entity *node,


    ada_bool *value_p
);


        



/*
 * Return the relative name for Self. If Self's defining name is ``A.B.C``,
 * return ``C`` as a node.
 */
extern int ada_basic_decl_p_relative_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * Return the relative name for Self, as text.
 */
extern int ada_basic_decl_p_relative_name_text(
    ada_base_entity *node,


    ada_symbol_type *value_p
);


        



/*
 * Return the next part of this declaration, if applicable.
 *
 * .. note:: It is not named next_part, because BaseTypeDecl has a more precise
 *    version of next_part that returns a BaseTypeDecl. Probably, we want to
 *    rename the specific versions, and have the root property be named
 *    next_part. (TODO R925-008)
 */
extern int ada_basic_decl_p_next_part_for_decl(
    ada_base_entity *node,

        
        ada_bool
        imprecise_fallback,

    ada_base_entity *value_p
);


        



/*
 * Return the body corresponding to this declaration, if applicable.
 *
 * .. note:: It is not named body_part, subclasses have more precise versions
 *    named body_part and returning a more precise result. Probably, we want to
 *    rename the specific versions, and have the root property be named
 *    body_part. (TODO R925-008)
 */
extern int ada_basic_decl_p_body_part_for_decl(
    ada_base_entity *node,

        
        ada_bool
        imprecise_fallback,

    ada_base_entity *value_p
);


        



/*
 * Given an origin node and the entity represented by Self, this property
 * returns the most visible completion of Self that can be seen by origin,
 * according to Ada's visibility rules.
 */
extern int ada_basic_decl_p_most_visible_part(
    ada_base_entity *node,

        
        const ada_base_entity*
        origin,
        
        ada_bool
        imprecise_fallback,

    ada_base_entity *value_p
);


        



/*
 * Return the fully qualified name corresponding to this declaration, as an
 * array of symbols.
 */
extern int ada_basic_decl_p_fully_qualified_name_array(
    ada_base_entity *node,

        
        ada_bool
        include_profile,

    ada_unbounded_text_type_array *value_p
);


        



/*
 * Return the fully qualified name corresponding to this declaration.
 */
extern int ada_basic_decl_p_fully_qualified_name(
    ada_base_entity *node,


    ada_string_type *value_p
);


        



/*
 * Return a canonical representation of the fully qualified name corresponding
 * to this declaration.
 */
extern int ada_basic_decl_p_canonical_fully_qualified_name(
    ada_base_entity *node,


    ada_string_type *value_p
);


        



/*
 * Return a unique identifying name for this declaration, provided this
 * declaration is a public declaration. In the case of subprograms, this will
 * include the profile.
 *
 * .. attention:: This will only return a unique name for public declarations.
 *    Notably, anything nested in an unnamed declare block won't be handled
 *    correctly.
 */
extern int ada_basic_decl_p_unique_identifying_name(
    ada_base_entity *node,


    ada_string_type *value_p
);


        



/*
 * Return whether this object is constant or not.
 */
extern int ada_basic_decl_p_is_constant_object(
    ada_base_entity *node,


    ada_bool *value_p
);


        



/*

 */
extern int ada_abstract_state_decl_f_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * Return the expression wrapped by this declaration.
 */
extern int ada_anonymous_expr_decl_f_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * Return the generic formal object declaration corresponding to this actual.
 */
extern int ada_anonymous_expr_decl_p_get_formal(
    ada_base_entity *node,

        
        ada_bool
        imprecise_fallback,

    ada_base_entity *value_p
);


        



/*
 * Return the type for this formal.
 */
extern int ada_base_formal_param_decl_p_formal_type(
    ada_base_entity *node,

        
        const ada_base_entity*
        origin,

    ada_base_entity *value_p
);


        



/*

 */
extern int ada_component_decl_f_ids(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_component_decl_f_component_def(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_allocator``,
 * ``ada_attribute_ref``, ``ada_base_aggregate``, ``ada_bin_op``,
 * ``ada_call_expr``, ``ada_char_literal``, ``ada_concat_op``,
 * ``ada_cond_expr``, ``ada_decl_expr``, ``ada_dotted_name``,
 * ``ada_explicit_deref``, ``ada_identifier``, ``ada_membership_expr``,
 * ``ada_null_literal``, ``ada_num_literal``, ``ada_paren_expr``,
 * ``ada_qual_expr``, ``ada_quantified_expr``, ``ada_raise_expr``,
 * ``ada_reduce_attribute_ref``, ``ada_string_literal``, ``ada_target_name``,
 * ``ada_un_op``, ``ada_update_attribute_ref``
 */
extern int ada_component_decl_f_default_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_discriminant_spec_f_ids(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_anonymous_type``,
 * ``ada_subtype_indication``
 */
extern int ada_discriminant_spec_f_type_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_allocator``,
 * ``ada_attribute_ref``, ``ada_base_aggregate``, ``ada_bin_op``,
 * ``ada_call_expr``, ``ada_char_literal``, ``ada_concat_op``,
 * ``ada_cond_expr``, ``ada_decl_expr``, ``ada_dotted_name``,
 * ``ada_explicit_deref``, ``ada_identifier``, ``ada_membership_expr``,
 * ``ada_null_literal``, ``ada_num_literal``, ``ada_paren_expr``,
 * ``ada_qual_expr``, ``ada_quantified_expr``, ``ada_raise_expr``,
 * ``ada_reduce_attribute_ref``, ``ada_string_literal``, ``ada_target_name``,
 * ``ada_un_op``, ``ada_update_attribute_ref``
 */
extern int ada_discriminant_spec_f_default_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_formal_subp_decl``,
 * ``ada_formal_type_decl``, ``ada_generic_instantiation``,
 * ``ada_incomplete_formal_type_decl``, ``ada_number_decl``,
 * ``ada_object_decl``, ``ada_single_protected_decl``, ``ada_single_task_decl``
 */
extern int ada_generic_formal_f_decl(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_param_spec_f_ids(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_param_spec_f_has_aliased(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_param_spec_f_mode(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_anonymous_type``,
 * ``ada_subtype_indication``
 */
extern int ada_param_spec_f_type_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_allocator``,
 * ``ada_attribute_ref``, ``ada_base_aggregate``, ``ada_bin_op``,
 * ``ada_call_expr``, ``ada_char_literal``, ``ada_concat_op``,
 * ``ada_cond_expr``, ``ada_decl_expr``, ``ada_dotted_name``,
 * ``ada_explicit_deref``, ``ada_identifier``, ``ada_membership_expr``,
 * ``ada_null_literal``, ``ada_num_literal``, ``ada_paren_expr``,
 * ``ada_qual_expr``, ``ada_quantified_expr``, ``ada_raise_expr``,
 * ``ada_reduce_attribute_ref``, ``ada_string_literal``, ``ada_target_name``,
 * ``ada_un_op``, ``ada_update_attribute_ref``
 */
extern int ada_param_spec_f_default_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_synthetic_formal_param_decl_f_param_type(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_base_package_decl_f_package_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_base_package_decl_f_public_part(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_base_package_decl_f_private_part(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_base_package_decl_f_end_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * Return the PackageBody corresponding to this node.
 */
extern int ada_base_package_decl_p_body_part(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_base_type_decl_f_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * If this type decl is a subtype decl, return the base subtype. If not, return
 * ``Self``.
 */
extern int ada_base_type_decl_p_base_subtype(
    ada_base_entity *node,

        
        const ada_base_entity*
        origin,

    ada_base_entity *value_p
);


        



/*
 * Return the private completion for this type, if there is one.
 */
extern int ada_base_type_decl_p_private_completion(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * Assuming that P is a primitive of Self, return whether the given primitive P
 * is inherited from one of Self's parents.
 */
extern int ada_base_type_decl_p_is_inherited_primitive(
    ada_base_entity *node,

        
        const ada_base_entity*
        p,

    ada_bool *value_p
);


        



/*
 * Return the record representation clause associated to this type decl, if
 * applicable (i.e. this type decl defines a record type).
 */
extern int ada_base_type_decl_p_get_record_representation_clause(
    ada_base_entity *node,

        
        ada_bool
        imprecise_fallback,

    ada_base_entity *value_p
);


        



/*
 * Return the enum representation clause associated to this type decl, if
 * applicable (i.e. this type decl defines an enum type).
 */
extern int ada_base_type_decl_p_get_enum_representation_clause(
    ada_base_entity *node,

        
        ada_bool
        imprecise_fallback,

    ada_base_entity *value_p
);


        



/*
 * Return whether this type is a record type.
 *
 * .. attention:: Private tagged types extending public tagged records are not
 *    considered as record types.
 */
extern int ada_base_type_decl_p_is_record_type(
    ada_base_entity *node,

        
        const ada_base_entity*
        origin,

    ada_bool *value_p
);


        



/*
 * Return whether this type is an array type.
 */
extern int ada_base_type_decl_p_is_array_type(
    ada_base_entity *node,

        
        const ada_base_entity*
        origin,

    ada_bool *value_p
);


        



/*
 * Find types derived from self in the given ``root`` and its children.
 */
extern int ada_base_type_decl_p_find_derived_types(
    ada_base_entity *node,

        
        const ada_base_entity*
        root,
        
        const ada_base_entity*
        origin,
        
        ada_bool
        imprecise_fallback,

    ada_ada_node_array *value_p
);


        



/*
 * Whether type is a real type or not.
 */
extern int ada_base_type_decl_p_is_real_type(
    ada_base_entity *node,

        
        const ada_base_entity*
        origin,

    ada_bool *value_p
);


        



/*
 * Whether type is a float type or not.
 */
extern int ada_base_type_decl_p_is_float_type(
    ada_base_entity *node,

        
        const ada_base_entity*
        origin,

    ada_bool *value_p
);


        



/*
 * Whether type is a fixed point type or not.
 */
extern int ada_base_type_decl_p_is_fixed_point(
    ada_base_entity *node,

        
        const ada_base_entity*
        origin,

    ada_bool *value_p
);


        



/*
 * Whether type is an enum type
 */
extern int ada_base_type_decl_p_is_enum_type(
    ada_base_entity *node,

        
        const ada_base_entity*
        origin,

    ada_bool *value_p
);


        



/*
 * Whether Self is an access type or not
 */
extern int ada_base_type_decl_p_is_access_type(
    ada_base_entity *node,

        
        const ada_base_entity*
        origin,

    ada_bool *value_p
);


        



/*
 * Whether type is a character type or not
 */
extern int ada_base_type_decl_p_is_char_type(
    ada_base_entity *node,

        
        const ada_base_entity*
        origin,

    ada_bool *value_p
);


        



/*
 * Return the discrete range for this type decl, if applicable.
 */
extern int ada_base_type_decl_p_discrete_range(
    ada_base_entity *node,


    ada_internal_discrete_range *value_p
);


        



/*
 * Whether type is a discrete type or not.
 */
extern int ada_base_type_decl_p_is_discrete_type(
    ada_base_entity *node,

        
        const ada_base_entity*
        origin,

    ada_bool *value_p
);


        



/*
 * Whether type is an integer type or not.
 */
extern int ada_base_type_decl_p_is_int_type(
    ada_base_entity *node,

        
        const ada_base_entity*
        origin,

    ada_bool *value_p
);


        



/*
 * If this type is an access type, or a type with an Implicit_Dereference
 * aspect, return the type of a dereference of an instance of this type.
 */
extern int ada_base_type_decl_p_accessed_type(
    ada_base_entity *node,

        
        const ada_base_entity*
        origin,

    ada_base_entity *value_p
);


        



/*
 * Whether type is tagged or not
 */
extern int ada_base_type_decl_p_is_tagged_type(
    ada_base_entity *node,

        
        const ada_base_entity*
        origin,

    ada_bool *value_p
);


        



/*
 * Return the base type entity for this derived type declaration
 */
extern int ada_base_type_decl_p_base_type(
    ada_base_entity *node,

        
        const ada_base_entity*
        origin,

    ada_base_entity *value_p
);


        



/*
 * Return the list of base types for Self.
 */
extern int ada_base_type_decl_p_base_types(
    ada_base_entity *node,

        
        const ada_base_entity*
        origin,

    ada_ada_node_array *value_p
);


        



/*
 * Return the list of all types that inherit (directly or inderictly) from Self
 * among the given units.
 */
extern int ada_base_type_decl_p_find_all_derived_types(
    ada_base_entity *node,

        
        ada_analysis_unit_array
        units,
        
        ada_bool
        imprecise_fallback,

    ada_ada_node_array *value_p
);


        



/*
 * Return the component type of ``Self``, if applicable. The component type is
 * the type you'll get if you call a value whose type is ``Self``. So it can
 * either be:
 *
 * 1. The component type for an array.
 *
 * 2. The return type for an access to function.
 */
extern int ada_base_type_decl_p_comp_type(
    ada_base_entity *node,

        
        ada_bool
        is_subscript,
        
        const ada_base_entity*
        origin,

    ada_base_entity *value_p
);


        



/*
 * Return the index type for dimension ``dim`` for this type, if applicable.
 *
 * .. warning:: ``dim`` is 0-based, so the first ``index_type`` is at index 0.
 */
extern int ada_base_type_decl_p_index_type(
    ada_base_entity *node,

        
        int
        dim,
        
        const ada_base_entity*
        origin,

    ada_base_entity *value_p
);


        



/*
 * Whether Self is derived from other_type.
 */
extern int ada_base_type_decl_p_is_derived_type(
    ada_base_entity *node,

        
        const ada_base_entity*
        other_type,
        
        const ada_base_entity*
        origin,

    ada_bool *value_p
);


        



/*
 * Return True iff this type declaration is an interface definition.
 */
extern int ada_base_type_decl_p_is_interface_type(
    ada_base_entity *node,

        
        const ada_base_entity*
        origin,

    ada_bool *value_p
);


        



/*
 * Return whether ``self`` matches ``expected_type``.
 */
extern int ada_base_type_decl_p_matching_type(
    ada_base_entity *node,

        
        const ada_base_entity*
        expected_type,
        
        const ada_base_entity*
        origin,

    ada_bool *value_p
);


        



/*
 * Return the canonical type declaration for this type declaration. For
 * subtypes, it will return the base type declaration.
 */
extern int ada_base_type_decl_p_canonical_type(
    ada_base_entity *node,

        
        const ada_base_entity*
        origin,

    ada_base_entity *value_p
);


        



/*
 * Returns the previous part for this type decl.
 */
extern int ada_base_type_decl_p_previous_part(
    ada_base_entity *node,

        
        ada_bool
        go_to_incomplete,

    ada_base_entity *value_p
);


        



/*
 * Returns the next part for this type decl.
 *
 * .. note:: Since this property returns a ``BaseTypeDecl``, it cannot be used
 *    to retrieve the next part of ``TaskTypeDecl`` and ``ProtectedTypeDecl``
 *    nodes as their next part is actually a ``Body``. Use
 *    ``BasicDecl.next_part_for_decl`` for those instead.
 */
extern int ada_base_type_decl_p_next_part(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * Return the full completion of this type.
 */
extern int ada_base_type_decl_p_full_view(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * Returns whether this is a definite subtype.
 *
 * For convenience, this will return ``False`` for incomplete types, even
 * though the correct answer is more akin to "non applicable".
 */
extern int ada_base_type_decl_p_is_definite_subtype(
    ada_base_entity *node,

        
        const ada_base_entity*
        origin,

    ada_bool *value_p
);


        



/*
 * Whether node is a private view of corresponding type.
 */
extern int ada_base_type_decl_p_is_private(
    ada_base_entity *node,


    ada_bool *value_p
);


        



/*
 * Return the list of all discriminants of this type. If this type has no
 * discriminant or only unknown discriminants, an empty list is returned.
 */
extern int ada_base_type_decl_p_discriminants_list(
    ada_base_entity *node,

        
        const ada_base_entity*
        origin,

    ada_ada_node_array *value_p
);


        



/*
 * Return the type that is at the root of the derivation hierarchy (ignoring
 * secondary interfaces derivations for tagged types)
 */
extern int ada_base_type_decl_p_root_type(
    ada_base_entity *node,

        
        const ada_base_entity*
        origin,

    ada_base_entity *value_p
);


        



/*
 * Must be called on a record (sub-)type declaration. Return all the possible
 * shapes that a value of this record type can take. For example, consider the
 * following record definition:
 *
 * .. code::
 *
 *    type R (A : Integer; B : Integer) is record
 *        X : Integer;
 *        case A is
 *            when 1 .. 10 =>
 *                Y_1 : Integer;
 *                case B is
 *                    when 1 .. 10 =>
 *                        Z_1 : Integer;
 *                    when others => null;
 *                end case;
 *            when 11 .. 20 =>
 *                Y_2 : Integer;
 *                case B is
 *                    when 1 .. 10 =>
 *                        Z_2 : Integer;
 *                    when others => null;
 *                end case;
 *            when others => null;
 *        end case;
 *    end record;
 *
 * For this instance, this property will return the following results:
 *
 * .. code::
 *
 *    [
 *        [X, Y_1, Z_1],
 *        [X, Y_1],
 *        [X, Y_2, Z_2],
 *        [X, Y_2],
 *        [X]
 *    ]
 *
 * .. attention:: This property is inaccurate when called on a record extension
 *    which defines components under a certain condition C, and this same
 *    condition is used to define some components in the parent record: in that
 *    case, any feasible shape will in practice contain either both the
 *    components defined under condition C in the child record and the parent
 *    record, or none of them.However, due to the simplified algorithm we use
 *    here to compute the feasible shapes, we will also return shapes that
 *    include the components of the child record but not the parent record, and
 *    conversely.
 */
extern int ada_base_type_decl_p_shapes(
    ada_base_entity *node,

        
        ada_bool
        include_discriminants,
        
        const ada_base_entity*
        origin,

    ada_shape_array *value_p
);


        



/*
 * Get the type for this subtype.
 */
extern int ada_base_subtype_decl_p_get_type(
    ada_base_entity *node,

        
        const ada_base_entity*
        origin,

    ada_base_entity *value_p
);


        



/*

 */
extern int ada_subtype_decl_f_subtype(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_incomplete_type_decl_f_discriminants(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_incomplete_formal_type_decl_f_is_tagged(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_attribute_ref``,
 * ``ada_call_expr``, ``ada_char_literal``, ``ada_dotted_name``,
 * ``ada_explicit_deref``, ``ada_identifier``, ``ada_qual_expr``,
 * ``ada_reduce_attribute_ref``, ``ada_string_literal``, ``ada_target_name``,
 * ``ada_update_attribute_ref``
 */
extern int ada_incomplete_formal_type_decl_f_default_type(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_incomplete_tagged_type_decl_f_has_abstract(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_protected_type_decl_f_discriminants(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field contains a list that itself contains one of the following nodes:
 * ``ada_char_literal``, ``ada_dotted_name``, ``ada_identifier``,
 * ``ada_string_literal``
 */
extern int ada_protected_type_decl_f_interfaces(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_protected_type_decl_f_definition(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_task_type_decl_f_discriminants(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_task_type_decl_f_definition(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_type_decl_f_discriminants(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_type_decl_f_type_def(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * Return the list of all primitive operations that are available on this type.
 * If ``only_inherited`` is True, it will only return the primitives that are
 * implicitly inherited by this type, discarding those explicitly defined on
 * this type. Predefined operators are included in the result iff
 * ``include_predefined_operators`` is True. It defaults to False.
 */
extern int ada_type_decl_p_get_primitives(
    ada_base_entity *node,

        
        ada_bool
        only_inherited,
        
        ada_bool
        include_predefined_operators,

    ada_ada_node_array *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_attribute_ref``,
 * ``ada_call_expr``, ``ada_char_literal``, ``ada_dotted_name``,
 * ``ada_explicit_deref``, ``ada_identifier``, ``ada_qual_expr``,
 * ``ada_reduce_attribute_ref``, ``ada_string_literal``, ``ada_target_name``,
 * ``ada_update_attribute_ref``
 */
extern int ada_formal_type_decl_f_default_type(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * Return the specification for this subprogram
 */
extern int ada_basic_subp_decl_p_subp_decl_spec(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_classic_subp_decl_f_overriding(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_classic_subp_decl_f_subp_spec(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * Return the BaseSubpBody corresponding to this node.
 */
extern int ada_classic_subp_decl_p_body_part(
    ada_base_entity *node,

        
        ada_bool
        imprecise_fallback,

    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_attribute_ref``,
 * ``ada_box_expr``, ``ada_call_expr``, ``ada_char_literal``,
 * ``ada_dotted_name``, ``ada_explicit_deref``, ``ada_identifier``,
 * ``ada_null_literal``, ``ada_qual_expr``, ``ada_reduce_attribute_ref``,
 * ``ada_string_literal``, ``ada_target_name``, ``ada_update_attribute_ref``
 */
extern int ada_formal_subp_decl_f_default_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_entry_decl_f_overriding(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_entry_decl_f_spec(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * Return the entry body associated to this entry declaration.
 */
extern int ada_entry_decl_p_body_part(
    ada_base_entity *node,

        
        ada_bool
        imprecise_fallback,

    ada_base_entity *value_p
);


        



/*
 * Return an array of accept statements corresponding to this entry.
 */
extern int ada_entry_decl_p_accept_stmts(
    ada_base_entity *node,


    ada_ada_node_array *value_p
);


        



/*

 */
extern int ada_enum_literal_decl_f_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * Return the enum type corresponding to this enum literal.
 */
extern int ada_enum_literal_decl_p_enum_type(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * Return the CharLiteral expression corresponding to this enum literal.
 */
extern int ada_synthetic_char_enum_lit_p_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_generic_subp_internal_f_subp_spec(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_synthetic_subp_decl_f_spec(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * Return the previous part for this body. Might be a declaration or a body
 * stub.
 */
extern int ada_body_node_p_previous_part(
    ada_base_entity *node,

        
        ada_bool
        imprecise_fallback,

    ada_base_entity *value_p
);


        



/*
 * Return the decl corresponding to this node if applicable.
 */
extern int ada_body_node_p_decl_part(
    ada_base_entity *node,

        
        ada_bool
        imprecise_fallback,

    ada_base_entity *value_p
);


        



/*
 * If self is a subunit, return the body in which it is rooted.
 */
extern int ada_body_node_p_subunit_root(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_base_subp_body_f_overriding(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_base_subp_body_f_subp_spec(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_base_aggregate``,
 * ``ada_paren_expr``
 */
extern int ada_expr_function_f_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_subp_body_f_decls(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_subp_body_f_stmts(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_subp_body_f_end_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_subp_renaming_decl_f_renames(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * Return the syntactic fully qualified name to refer to this body.
 *
 * Note that this can raise a Property_Error when the stub is in an illegal
 * place (too nested, in a declare block, etc.).
 */
extern int ada_body_stub_p_syntactic_fully_qualified_name(
    ada_base_entity *node,


    ada_unbounded_text_type_array *value_p
);


        



/*

 */
extern int ada_package_body_stub_f_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_protected_body_stub_f_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_subp_body_stub_f_overriding(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_subp_body_stub_f_subp_spec(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_task_body_stub_f_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_entry_body_f_entry_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_entry_body_f_index_spec(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_entry_body_f_params(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_allocator``,
 * ``ada_attribute_ref``, ``ada_base_aggregate``, ``ada_bin_op``,
 * ``ada_call_expr``, ``ada_char_literal``, ``ada_concat_op``,
 * ``ada_cond_expr``, ``ada_decl_expr``, ``ada_dotted_name``,
 * ``ada_explicit_deref``, ``ada_identifier``, ``ada_membership_expr``,
 * ``ada_null_literal``, ``ada_num_literal``, ``ada_paren_expr``,
 * ``ada_qual_expr``, ``ada_quantified_expr``, ``ada_raise_expr``,
 * ``ada_reduce_attribute_ref``, ``ada_string_literal``, ``ada_target_name``,
 * ``ada_un_op``, ``ada_update_attribute_ref``
 */
extern int ada_entry_body_f_barrier(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_entry_body_f_decls(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_entry_body_f_stmts(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_entry_body_f_end_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_package_body_f_package_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_package_body_f_decls(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_package_body_f_stmts(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_package_body_f_end_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_protected_body_f_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_protected_body_f_decls(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_protected_body_f_end_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_task_body_f_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_task_body_f_decls(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_task_body_f_stmts(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_task_body_f_end_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_entry_index_spec_f_id(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_attribute_ref``,
 * ``ada_bin_op``, ``ada_call_expr``, ``ada_char_literal``,
 * ``ada_dotted_name``, ``ada_explicit_deref``, ``ada_identifier``,
 * ``ada_qual_expr``, ``ada_reduce_attribute_ref``, ``ada_string_literal``,
 * ``ada_subtype_indication``, ``ada_target_name``,
 * ``ada_update_attribute_ref``
 */
extern int ada_entry_index_spec_f_subtype(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_exception_decl_f_ids(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_exception_decl_f_renames(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_exception_handler_f_exception_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field contains a list that itself contains one of the following nodes:
 * ``ada_attribute_ref``, ``ada_call_expr``, ``ada_char_literal``,
 * ``ada_dotted_name``, ``ada_explicit_deref``, ``ada_identifier``,
 * ``ada_others_designator``, ``ada_qual_expr``, ``ada_reduce_attribute_ref``,
 * ``ada_string_literal``, ``ada_target_name``, ``ada_update_attribute_ref``
 */
extern int ada_exception_handler_f_handled_exceptions(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field contains a list that itself contains one of the following nodes:
 * ``ada_pragma_node``, ``ada_stmt``
 */
extern int ada_exception_handler_f_stmts(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_for_loop_var_decl_f_id(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_anonymous_type``,
 * ``ada_subtype_indication``
 */
extern int ada_for_loop_var_decl_f_id_type(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_generic_decl_f_formal_part(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_generic_package_decl_f_package_decl(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * Return the PackageBody corresponding to this node, or null if there is none.
 */
extern int ada_generic_package_decl_p_body_part(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_generic_subp_decl_f_subp_decl(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * Return the BaseSubpBody corresponding to this node.
 */
extern int ada_generic_subp_decl_p_body_part(
    ada_base_entity *node,

        
        ada_bool
        imprecise_fallback,

    ada_base_entity *value_p
);


        



/*
 * Return the generic decl entity designated by this instantiation, containing
 * the generic context. This is equivalent to the expanded generic unit in
 * GNAT.
 */
extern int ada_generic_instantiation_p_designated_generic_decl(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * Returns an array of pairs, associating formal parameters to actual or
 * default expressions.
 */
extern int ada_generic_instantiation_p_inst_params(
    ada_base_entity *node,


    ada_param_actual_array *value_p
);


        



/*

 */
extern int ada_generic_package_instantiation_f_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_char_literal``,
 * ``ada_dotted_name``, ``ada_identifier``, ``ada_string_literal``
 */
extern int ada_generic_package_instantiation_f_generic_pkg_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field contains a list that itself contains one of the following nodes:
 * ``ada_param_assoc``
 */
extern int ada_generic_package_instantiation_f_params(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_generic_subp_instantiation_f_overriding(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_generic_subp_instantiation_f_kind(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_generic_subp_instantiation_f_subp_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_char_literal``,
 * ``ada_dotted_name``, ``ada_identifier``, ``ada_string_literal``
 */
extern int ada_generic_subp_instantiation_f_generic_subp_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field contains a list that itself contains one of the following nodes:
 * ``ada_param_assoc``
 */
extern int ada_generic_subp_instantiation_f_params(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * Return the subprogram decl designated by this instantiation.
 */
extern int ada_generic_subp_instantiation_p_designated_subp(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_generic_package_renaming_decl_f_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_char_literal``,
 * ``ada_dotted_name``, ``ada_identifier``, ``ada_string_literal``
 */
extern int ada_generic_package_renaming_decl_f_renames(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_generic_subp_renaming_decl_f_kind(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_generic_subp_renaming_decl_f_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_char_literal``,
 * ``ada_dotted_name``, ``ada_identifier``, ``ada_string_literal``
 */
extern int ada_generic_subp_renaming_decl_f_renames(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_label_decl_f_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_named_stmt_decl_f_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_number_decl_f_ids(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_allocator``,
 * ``ada_attribute_ref``, ``ada_base_aggregate``, ``ada_bin_op``,
 * ``ada_call_expr``, ``ada_char_literal``, ``ada_concat_op``,
 * ``ada_cond_expr``, ``ada_decl_expr``, ``ada_dotted_name``,
 * ``ada_explicit_deref``, ``ada_identifier``, ``ada_null_literal``,
 * ``ada_num_literal``, ``ada_paren_expr``, ``ada_qual_expr``,
 * ``ada_quantified_expr``, ``ada_raise_expr``, ``ada_reduce_attribute_ref``,
 * ``ada_string_literal``, ``ada_target_name``, ``ada_un_op``,
 * ``ada_update_attribute_ref``
 */
extern int ada_number_decl_f_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_object_decl_f_ids(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_object_decl_f_has_aliased(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_object_decl_f_has_constant(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_object_decl_f_mode(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_anonymous_type``,
 * ``ada_subtype_indication``
 */
extern int ada_object_decl_f_type_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_allocator``,
 * ``ada_attribute_ref``, ``ada_base_aggregate``, ``ada_bin_op``,
 * ``ada_call_expr``, ``ada_char_literal``, ``ada_concat_op``,
 * ``ada_cond_expr``, ``ada_decl_expr``, ``ada_dotted_name``,
 * ``ada_explicit_deref``, ``ada_identifier``, ``ada_membership_expr``,
 * ``ada_null_literal``, ``ada_num_literal``, ``ada_paren_expr``,
 * ``ada_qual_expr``, ``ada_quantified_expr``, ``ada_raise_expr``,
 * ``ada_reduce_attribute_ref``, ``ada_string_literal``, ``ada_target_name``,
 * ``ada_un_op``, ``ada_update_attribute_ref``
 */
extern int ada_object_decl_f_default_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_object_decl_f_renaming_clause(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * If this object decl is the constant completion of an object decl in the
 * public part, return the object decl from the public part.
 */
extern int ada_object_decl_p_private_part_decl(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * If this object decl is the incomplete declaration of a constant in a public
 * part, return its completion in the private part.
 */
extern int ada_object_decl_p_public_part_decl(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_package_renaming_decl_f_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_package_renaming_decl_f_renames(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * Return the declaration of the package that is renamed by Self.
 */
extern int ada_package_renaming_decl_p_renamed_package(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * Return the declaration of the package that is ultimately renamed by Self,
 * skipping through all intermediate package renamings.
 */
extern int ada_package_renaming_decl_p_final_renamed_package(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_single_protected_decl_f_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field contains a list that itself contains one of the following nodes:
 * ``ada_char_literal``, ``ada_dotted_name``, ``ada_identifier``,
 * ``ada_string_literal``
 */
extern int ada_single_protected_decl_f_interfaces(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_single_protected_decl_f_definition(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_single_task_decl_f_task_type(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field contains a list that itself contains one of the following nodes:
 * ``ada_allocator``, ``ada_attribute_ref``, ``ada_base_aggregate``,
 * ``ada_bin_op``, ``ada_call_expr``, ``ada_char_literal``, ``ada_concat_op``,
 * ``ada_cond_expr``, ``ada_decl_expr``, ``ada_discrete_subtype_indication``,
 * ``ada_dotted_name``, ``ada_explicit_deref``, ``ada_identifier``,
 * ``ada_membership_expr``, ``ada_null_literal``, ``ada_num_literal``,
 * ``ada_others_designator``, ``ada_paren_expr``, ``ada_qual_expr``,
 * ``ada_quantified_expr``, ``ada_raise_expr``, ``ada_reduce_attribute_ref``,
 * ``ada_string_literal``, ``ada_target_name``, ``ada_un_op``,
 * ``ada_update_attribute_ref``
 */
extern int ada_case_stmt_alternative_f_choices(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field contains a list that itself contains one of the following nodes:
 * ``ada_pragma_node``, ``ada_stmt``
 */
extern int ada_case_stmt_alternative_f_stmts(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * ``with``, ``use`` or ``pragma`` statements.
 *
 * This field contains a list that itself contains one of the following nodes:
 * ``ada_pragma_node``, ``ada_use_clause``, ``ada_with_clause``
 */
extern int ada_compilation_unit_f_prelude(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_library_item``,
 * ``ada_subunit``
 */
extern int ada_compilation_unit_f_body(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_compilation_unit_f_pragmas(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * Return the syntactic fully qualified name of this compilation unit.
 */
extern int ada_compilation_unit_p_syntactic_fully_qualified_name(
    ada_base_entity *node,


    ada_unbounded_text_type_array *value_p
);


        



/*
 * Return the kind corresponding to this analysis unit.
 */
extern int ada_compilation_unit_p_unit_kind(
    ada_base_entity *node,


    ada_analysis_unit_kind *value_p
);


        



/*
 * Look for all "with" clauses at the top of this compilation unit and return
 * all the compilation units designated by them. For the complete dependencies
 * list of compilation units, see the ``unit_dependencies`` property.
 */
extern int ada_compilation_unit_p_withed_units(
    ada_base_entity *node,


    ada_ada_node_array *value_p
);


        



/*
 * Return all the compilation units that are directly imported by this one.
 * This includes "with"ed units as well as the direct parent unit.
 */
extern int ada_compilation_unit_p_imported_units(
    ada_base_entity *node,


    ada_ada_node_array *value_p
);


        



/*
 * Return the list of all the compilation units that are (direct and indirect)
 * dependencies of this one. See the ``withed_units``/``imported_units``
 * properties to only get the direct dependencies of this unit.
 */
extern int ada_compilation_unit_p_unit_dependencies(
    ada_base_entity *node,


    ada_ada_node_array *value_p
);


        



/*
 * Get the root basic decl defined in this compilation unit.
 */
extern int ada_compilation_unit_p_decl(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * Whether this compilation unit is preelaborable or not.
 */
extern int ada_compilation_unit_p_is_preelaborable(
    ada_base_entity *node,

        
        ada_bool
        imprecise_fallback,

    ada_bool *value_p
);


        



/*
 * If this compilation unit is of kind UnitSpecification, return its
 * corresponding body unit, and conversely.
 */
extern int ada_compilation_unit_p_other_part(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * Whether this compilation unit is affected by the restriction with the given
 * name.
 *
 * .. warning:: This property only supports the ``No_Elaboration_Code``
 *    restriction for now.
 */
extern int ada_compilation_unit_p_has_restriction(
    ada_base_entity *node,

        
        const ada_symbol_type*
        name,

    ada_bool *value_p
);


        



/*
 * Return the list of configuration pragmas that apply to the current unit.
 *
 * .. note:: Using this property before creating the configuration pragmas
 *    files mapping using subprograms from the ``Libadalang.Config_Pragmas``
 *    package will raise an error.
 */
extern int ada_compilation_unit_p_all_config_pragmas(
    ada_base_entity *node,


    ada_ada_node_array *value_p
);


        



/*
 * Return the list of configuration pragmas wih the given name that apply to
 * the current unit.
 *
 * .. note:: Using this property before creating the configuration pragmas
 *    files mapping using subprograms from the ``Libadalang.Config_Pragmas``
 *    package will raise an error.
 */
extern int ada_compilation_unit_p_config_pragmas(
    ada_base_entity *node,

        
        const ada_symbol_type*
        name,

    ada_ada_node_array *value_p
);


        



/*

 */
extern int ada_component_clause_f_id(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_allocator``,
 * ``ada_attribute_ref``, ``ada_base_aggregate``, ``ada_bin_op``,
 * ``ada_call_expr``, ``ada_char_literal``, ``ada_concat_op``,
 * ``ada_cond_expr``, ``ada_decl_expr``, ``ada_dotted_name``,
 * ``ada_explicit_deref``, ``ada_identifier``, ``ada_null_literal``,
 * ``ada_num_literal``, ``ada_paren_expr``, ``ada_qual_expr``,
 * ``ada_quantified_expr``, ``ada_raise_expr``, ``ada_reduce_attribute_ref``,
 * ``ada_string_literal``, ``ada_target_name``, ``ada_un_op``,
 * ``ada_update_attribute_ref``
 */
extern int ada_component_clause_f_position(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_component_clause_f_range(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_component_def_f_has_aliased(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_component_def_f_has_constant(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_anonymous_type``,
 * ``ada_subtype_indication``
 */
extern int ada_component_def_f_type_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * Return whether this is an instance of ConstantPresent
 */
extern int ada_constant_node_p_as_bool(
    ada_base_entity *node,


    ada_bool *value_p
);


        



/*
 * This field contains a list that itself contains one of the following nodes:
 * ``ada_composite_constraint_assoc``
 */
extern int ada_composite_constraint_f_constraints(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * Whether this composite constraint is an index constraint.
 */
extern int ada_composite_constraint_p_is_index_constraint(
    ada_base_entity *node,


    ada_bool *value_p
);


        



/*
 * Whether this composite constraint is a discriminant constraint.
 */
extern int ada_composite_constraint_p_is_discriminant_constraint(
    ada_base_entity *node,


    ada_bool *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_allocator``,
 * ``ada_attribute_ref``, ``ada_base_aggregate``, ``ada_bin_op``,
 * ``ada_call_expr``, ``ada_char_literal``, ``ada_concat_op``,
 * ``ada_cond_expr``, ``ada_decl_expr``, ``ada_dotted_name``,
 * ``ada_explicit_deref``, ``ada_identifier``, ``ada_null_literal``,
 * ``ada_num_literal``, ``ada_paren_expr``, ``ada_qual_expr``,
 * ``ada_quantified_expr``, ``ada_raise_expr``, ``ada_reduce_attribute_ref``,
 * ``ada_string_literal``, ``ada_target_name``, ``ada_un_op``,
 * ``ada_update_attribute_ref``
 */
extern int ada_delta_constraint_f_digits(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_delta_constraint_f_range(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_allocator``,
 * ``ada_attribute_ref``, ``ada_base_aggregate``, ``ada_bin_op``,
 * ``ada_call_expr``, ``ada_char_literal``, ``ada_concat_op``,
 * ``ada_cond_expr``, ``ada_decl_expr``, ``ada_dotted_name``,
 * ``ada_explicit_deref``, ``ada_identifier``, ``ada_null_literal``,
 * ``ada_num_literal``, ``ada_paren_expr``, ``ada_qual_expr``,
 * ``ada_quantified_expr``, ``ada_raise_expr``, ``ada_reduce_attribute_ref``,
 * ``ada_string_literal``, ``ada_target_name``, ``ada_un_op``,
 * ``ada_update_attribute_ref``
 */
extern int ada_digits_constraint_f_digits(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_digits_constraint_f_range(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_range_constraint_f_range(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field contains a list that itself contains one of the following nodes:
 * ``ada_abstract_subp_decl``, ``ada_aspect_clause``, ``ada_body_node``,
 * ``ada_component_decl``, ``ada_concrete_type_decl``, ``ada_entry_decl``,
 * ``ada_error_decl``, ``ada_exception_decl``, ``ada_generic_decl``,
 * ``ada_generic_instantiation``, ``ada_generic_renaming_decl``,
 * ``ada_incomplete_type_decl``, ``ada_number_decl``, ``ada_object_decl``,
 * ``ada_package_decl``, ``ada_package_renaming_decl``, ``ada_pragma_node``,
 * ``ada_protected_type_decl``, ``ada_single_protected_decl``,
 * ``ada_single_task_decl``, ``ada_subp_decl``, ``ada_subtype_decl``,
 * ``ada_task_type_decl``, ``ada_use_clause``
 */
extern int ada_declarative_part_f_decls(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_allocator``,
 * ``ada_attribute_ref``, ``ada_base_aggregate``, ``ada_bin_op``,
 * ``ada_call_expr``, ``ada_char_literal``, ``ada_concat_op``,
 * ``ada_cond_expr``, ``ada_decl_expr``, ``ada_dotted_name``,
 * ``ada_explicit_deref``, ``ada_identifier``, ``ada_membership_expr``,
 * ``ada_null_literal``, ``ada_num_literal``, ``ada_paren_expr``,
 * ``ada_qual_expr``, ``ada_quantified_expr``, ``ada_raise_expr``,
 * ``ada_reduce_attribute_ref``, ``ada_string_literal``, ``ada_target_name``,
 * ``ada_un_op``, ``ada_update_attribute_ref``
 */
extern int ada_elsif_expr_part_f_cond_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_allocator``,
 * ``ada_attribute_ref``, ``ada_base_aggregate``, ``ada_bin_op``,
 * ``ada_call_expr``, ``ada_char_literal``, ``ada_concat_op``,
 * ``ada_cond_expr``, ``ada_decl_expr``, ``ada_dotted_name``,
 * ``ada_explicit_deref``, ``ada_identifier``, ``ada_membership_expr``,
 * ``ada_null_literal``, ``ada_num_literal``, ``ada_paren_expr``,
 * ``ada_qual_expr``, ``ada_quantified_expr``, ``ada_raise_expr``,
 * ``ada_reduce_attribute_ref``, ``ada_string_literal``, ``ada_target_name``,
 * ``ada_un_op``, ``ada_update_attribute_ref``
 */
extern int ada_elsif_expr_part_f_then_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_allocator``,
 * ``ada_attribute_ref``, ``ada_base_aggregate``, ``ada_bin_op``,
 * ``ada_call_expr``, ``ada_char_literal``, ``ada_concat_op``,
 * ``ada_cond_expr``, ``ada_decl_expr``, ``ada_dotted_name``,
 * ``ada_explicit_deref``, ``ada_identifier``, ``ada_membership_expr``,
 * ``ada_null_literal``, ``ada_num_literal``, ``ada_paren_expr``,
 * ``ada_qual_expr``, ``ada_quantified_expr``, ``ada_raise_expr``,
 * ``ada_reduce_attribute_ref``, ``ada_string_literal``, ``ada_target_name``,
 * ``ada_un_op``, ``ada_update_attribute_ref``
 */
extern int ada_elsif_stmt_part_f_cond_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field contains a list that itself contains one of the following nodes:
 * ``ada_pragma_node``, ``ada_stmt``
 */
extern int ada_elsif_stmt_part_f_stmts(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * Return the declaration corresponding to the type of this expression after
 * name resolution.
 */
extern int ada_expr_p_expression_type(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * Return the declaration corresponding to the expected type of this expression
 * after name resolution.
 */
extern int ada_expr_p_expected_expression_type(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * Returns whether this expression is dynamically tagged (See :rmlink:`3.9.2`).
 */
extern int ada_expr_p_is_dynamically_tagged(
    ada_base_entity *node,

        
        ada_bool
        imprecise_fallback,

    ada_bool *value_p
);


        



/*
 * Returns True if this ``Name`` corresponds to a dispatching call, including:
 *
 * * Calls done through subprogram access types.
 *
 * * Calls to dispatching subprograms, in the object-oriented sense.
 *
 * .. note:: This is an experimental feature. There might be some discrepancy
 *    with the GNAT concept of "dispatching call".
 *
 * .. note:: This should only be called on a ``Name`` and ``UnOp`` or a
 *    ``BinOp``.
 */
extern int ada_expr_p_is_dispatching_call(
    ada_base_entity *node,

        
        ada_bool
        imprecise_fallback,

    ada_bool *value_p
);


        



/*
 * Return whether this expression is static according to the ARM definition of
 * static. See :rmlink:`4.9`.
 */
extern int ada_expr_p_is_static_expr(
    ada_base_entity *node,

        
        ada_bool
        imprecise_fallback,

    ada_bool *value_p
);


        



/*
 * Return the first decl that is lexically named like self in self's scope.
 */
extern int ada_expr_p_first_corresponding_decl(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * Statically evaluates self, and returns the value of the evaluation as an
 * integer.
 *
 * .. note:: In order for a call to this not to raise, the expression needs to
 *    be a static expression, as specified in :rmlink:`4.9`. You can verify
 *    whether an expression is static with the ``is_static_expr`` property.
 *
 * .. attention:: This is an experimental feature, so even if it is exposed to
 *    allow experiments, it is totally unsupported and the API and behavior are
 *    very likely to change in the future.
 */
extern int ada_expr_p_eval_as_int(
    ada_base_entity *node,


    ada_big_integer *value_p
);


        



/*
 * Statically evaluates self, and returns the value of the evaluation as an
 * integer. The given environment is used to substitute references to
 * declarations by actual values.
 *
 * .. note:: In order for a call to this not to raise, the expression needs to
 *    be a static expression, as specified in :rmlink:`4.9`. You can verify
 *    whether an expression is static with the ``is_static_expr`` property.
 *
 * .. attention:: This is an experimental feature, so even if it is exposed to
 *    allow experiments, it is totally unsupported and the API and behavior are
 *    very likely to change in the future.
 */
extern int ada_expr_p_eval_as_int_in_env(
    ada_base_entity *node,

        
        ada_substitution_array
        env,

    ada_big_integer *value_p
);


        



/*
 * Statically evaluates self, and returns the value of the evaluation as a
 * string.
 *
 * .. note:: In order for a call to this not to raise, the expression needs to
 *    be a static expression, as specified in :rmlink:`4.9`. You can verify
 *    whether an expression is static with the ``is_static_expr`` property.
 *
 * .. attention:: This is an experimental feature, so even if it is exposed to
 *    allow experiments, it is totally unsupported and the API and behavior are
 *    very likely to change in the future.
 */
extern int ada_expr_p_eval_as_string(
    ada_base_entity *node,


    ada_string_type *value_p
);


        



/*
 * Statically evaluates self, and returns the value of the evaluation as a
 * string. The given environment is used to substitute references to
 * declarations by actual values.
 *
 * .. note:: In order for a call to this not to raise, the expression needs to
 *    be a static expression, as specified in :rmlink:`4.9`. You can verify
 *    whether an expression is static with the ``is_static_expr`` property.
 *
 * .. attention:: This is an experimental feature, so even if it is exposed to
 *    allow experiments, it is totally unsupported and the API and behavior are
 *    very likely to change in the future.
 */
extern int ada_expr_p_eval_as_string_in_env(
    ada_base_entity *node,

        
        ada_substitution_array
        env,

    ada_string_type *value_p
);


        



/*
 * Return the list of AST nodes that can be a match for this expression before
 * overloading analysis.
 */
extern int ada_expr_p_matching_nodes(
    ada_base_entity *node,


    ada_ada_node_array *value_p
);


        



/*
 * This field can contain one of the following nodes:
 * ``ada_abstract_state_decl``, ``ada_multi_abstract_state_decl``,
 * ``ada_paren_abstract_state_decl``
 */
extern int ada_abstract_state_decl_expr_f_state_decl(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_attribute_ref``,
 * ``ada_call_expr``, ``ada_char_literal``, ``ada_dotted_name``,
 * ``ada_explicit_deref``, ``ada_identifier``, ``ada_qual_expr``,
 * ``ada_reduce_attribute_ref``, ``ada_string_literal``, ``ada_target_name``,
 * ``ada_update_attribute_ref``
 */
extern int ada_allocator_f_subpool(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_qual_expr``,
 * ``ada_subtype_indication``
 */
extern int ada_allocator_f_type_or_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * Return the allocated type for this allocator.
 */
extern int ada_allocator_p_get_allocated_type(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_allocator``,
 * ``ada_attribute_ref``, ``ada_base_aggregate``, ``ada_bin_op``,
 * ``ada_call_expr``, ``ada_char_literal``, ``ada_concat_op``,
 * ``ada_cond_expr``, ``ada_decl_expr``, ``ada_dotted_name``,
 * ``ada_explicit_deref``, ``ada_identifier``, ``ada_membership_expr``,
 * ``ada_null_literal``, ``ada_num_literal``, ``ada_paren_expr``,
 * ``ada_qual_expr``, ``ada_quantified_expr``, ``ada_raise_expr``,
 * ``ada_reduce_attribute_ref``, ``ada_string_literal``, ``ada_target_name``,
 * ``ada_un_op``, ``ada_update_attribute_ref``
 */
extern int ada_base_aggregate_f_ancestor_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field contains a list that itself contains one of the following nodes:
 * ``ada_aggregate_assoc``, ``ada_iterated_assoc``
 */
extern int ada_base_aggregate_f_assocs(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * Returns an array of pairs, associating formal parameters to actual
 * expressions. See ``zip_with_params``.
 */
extern int ada_base_aggregate_p_aggregate_params(
    ada_base_entity *node,


    ada_param_actual_array *value_p
);


        



/*
 * Return whether this aggregate is actually a subaggregate of a
 * multidimensional array aggregate, as described in :rmlink:`4.3.3`.
 */
extern int ada_base_aggregate_p_is_subaggregate(
    ada_base_entity *node,


    ada_bool *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_allocator``,
 * ``ada_attribute_ref``, ``ada_base_aggregate``, ``ada_bin_op``,
 * ``ada_call_expr``, ``ada_char_literal``, ``ada_concat_op``,
 * ``ada_cond_expr``, ``ada_decl_expr``, ``ada_dotted_name``,
 * ``ada_explicit_deref``, ``ada_identifier``, ``ada_membership_expr``,
 * ``ada_null_literal``, ``ada_num_literal``, ``ada_paren_expr``,
 * ``ada_qual_expr``, ``ada_quantified_expr``, ``ada_raise_expr``,
 * ``ada_reduce_attribute_ref``, ``ada_string_literal``, ``ada_target_name``,
 * ``ada_un_op``, ``ada_update_attribute_ref``
 */
extern int ada_bin_op_f_left(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_op_and_then``,
 * ``ada_op_and``, ``ada_op_div``, ``ada_op_double_dot``, ``ada_op_eq``,
 * ``ada_op_gt``, ``ada_op_gte``, ``ada_op_lt``, ``ada_op_lte``,
 * ``ada_op_minus``, ``ada_op_mod``, ``ada_op_mult``, ``ada_op_neq``,
 * ``ada_op_or_else``, ``ada_op_or``, ``ada_op_plus``, ``ada_op_pow``,
 * ``ada_op_rem``, ``ada_op_xor``
 */
extern int ada_bin_op_f_op(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_allocator``,
 * ``ada_attribute_ref``, ``ada_base_aggregate``, ``ada_bin_op``,
 * ``ada_call_expr``, ``ada_char_literal``, ``ada_concat_op``,
 * ``ada_cond_expr``, ``ada_decl_expr``, ``ada_dotted_name``,
 * ``ada_explicit_deref``, ``ada_identifier``, ``ada_membership_expr``,
 * ``ada_null_literal``, ``ada_num_literal``, ``ada_paren_expr``,
 * ``ada_qual_expr``, ``ada_quantified_expr``, ``ada_raise_expr``,
 * ``ada_reduce_attribute_ref``, ``ada_string_literal``, ``ada_target_name``,
 * ``ada_un_op``, ``ada_update_attribute_ref``
 */
extern int ada_bin_op_f_right(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field contains a list that itself contains one of the following nodes:
 * ``ada_allocator``, ``ada_attribute_ref``, ``ada_base_aggregate``,
 * ``ada_bin_op``, ``ada_call_expr``, ``ada_char_literal``, ``ada_concat_op``,
 * ``ada_cond_expr``, ``ada_decl_expr``, ``ada_discrete_subtype_indication``,
 * ``ada_dotted_name``, ``ada_explicit_deref``, ``ada_identifier``,
 * ``ada_membership_expr``, ``ada_null_literal``, ``ada_num_literal``,
 * ``ada_others_designator``, ``ada_paren_expr``, ``ada_qual_expr``,
 * ``ada_quantified_expr``, ``ada_raise_expr``, ``ada_reduce_attribute_ref``,
 * ``ada_string_literal``, ``ada_target_name``, ``ada_un_op``,
 * ``ada_update_attribute_ref``
 */
extern int ada_case_expr_alternative_f_choices(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_allocator``,
 * ``ada_attribute_ref``, ``ada_base_aggregate``, ``ada_bin_op``,
 * ``ada_call_expr``, ``ada_char_literal``, ``ada_concat_op``,
 * ``ada_cond_expr``, ``ada_decl_expr``, ``ada_dotted_name``,
 * ``ada_explicit_deref``, ``ada_identifier``, ``ada_membership_expr``,
 * ``ada_null_literal``, ``ada_num_literal``, ``ada_paren_expr``,
 * ``ada_qual_expr``, ``ada_quantified_expr``, ``ada_raise_expr``,
 * ``ada_reduce_attribute_ref``, ``ada_string_literal``, ``ada_target_name``,
 * ``ada_un_op``, ``ada_update_attribute_ref``
 */
extern int ada_case_expr_alternative_f_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_allocator``,
 * ``ada_attribute_ref``, ``ada_base_aggregate``, ``ada_bin_op``,
 * ``ada_call_expr``, ``ada_char_literal``, ``ada_cond_expr``,
 * ``ada_decl_expr``, ``ada_dotted_name``, ``ada_explicit_deref``,
 * ``ada_identifier``, ``ada_null_literal``, ``ada_num_literal``,
 * ``ada_paren_expr``, ``ada_qual_expr``, ``ada_quantified_expr``,
 * ``ada_raise_expr``, ``ada_reduce_attribute_ref``, ``ada_string_literal``,
 * ``ada_target_name``, ``ada_un_op``, ``ada_update_attribute_ref``
 */
extern int ada_concat_op_f_first_operand(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_concat_op_f_other_operands(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * Return the operands of this concatenation expression
 */
extern int ada_concat_op_p_operands(
    ada_base_entity *node,


    ada_ada_node_array *value_p
);


        



/*

 */
extern int ada_concat_operand_f_operator(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_allocator``,
 * ``ada_attribute_ref``, ``ada_base_aggregate``, ``ada_bin_op``,
 * ``ada_call_expr``, ``ada_char_literal``, ``ada_cond_expr``,
 * ``ada_decl_expr``, ``ada_dotted_name``, ``ada_explicit_deref``,
 * ``ada_identifier``, ``ada_null_literal``, ``ada_num_literal``,
 * ``ada_paren_expr``, ``ada_qual_expr``, ``ada_quantified_expr``,
 * ``ada_raise_expr``, ``ada_reduce_attribute_ref``, ``ada_string_literal``,
 * ``ada_target_name``, ``ada_un_op``, ``ada_update_attribute_ref``
 */
extern int ada_concat_operand_f_operand(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * Return the dependent expressions for this conditional expression.
 */
extern int ada_cond_expr_p_dependent_exprs(
    ada_base_entity *node,


    ada_ada_node_array *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_allocator``,
 * ``ada_attribute_ref``, ``ada_base_aggregate``, ``ada_bin_op``,
 * ``ada_call_expr``, ``ada_char_literal``, ``ada_concat_op``,
 * ``ada_cond_expr``, ``ada_decl_expr``, ``ada_dotted_name``,
 * ``ada_explicit_deref``, ``ada_identifier``, ``ada_membership_expr``,
 * ``ada_null_literal``, ``ada_num_literal``, ``ada_paren_expr``,
 * ``ada_qual_expr``, ``ada_quantified_expr``, ``ada_raise_expr``,
 * ``ada_reduce_attribute_ref``, ``ada_string_literal``, ``ada_target_name``,
 * ``ada_un_op``, ``ada_update_attribute_ref``
 */
extern int ada_case_expr_f_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_case_expr_f_cases(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_allocator``,
 * ``ada_attribute_ref``, ``ada_base_aggregate``, ``ada_bin_op``,
 * ``ada_call_expr``, ``ada_char_literal``, ``ada_concat_op``,
 * ``ada_cond_expr``, ``ada_decl_expr``, ``ada_dotted_name``,
 * ``ada_explicit_deref``, ``ada_identifier``, ``ada_membership_expr``,
 * ``ada_null_literal``, ``ada_num_literal``, ``ada_paren_expr``,
 * ``ada_qual_expr``, ``ada_quantified_expr``, ``ada_raise_expr``,
 * ``ada_reduce_attribute_ref``, ``ada_string_literal``, ``ada_target_name``,
 * ``ada_un_op``, ``ada_update_attribute_ref``
 */
extern int ada_if_expr_f_cond_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_allocator``,
 * ``ada_attribute_ref``, ``ada_base_aggregate``, ``ada_bin_op``,
 * ``ada_call_expr``, ``ada_char_literal``, ``ada_concat_op``,
 * ``ada_cond_expr``, ``ada_decl_expr``, ``ada_dotted_name``,
 * ``ada_explicit_deref``, ``ada_identifier``, ``ada_membership_expr``,
 * ``ada_null_literal``, ``ada_num_literal``, ``ada_paren_expr``,
 * ``ada_qual_expr``, ``ada_quantified_expr``, ``ada_raise_expr``,
 * ``ada_reduce_attribute_ref``, ``ada_string_literal``, ``ada_target_name``,
 * ``ada_un_op``, ``ada_update_attribute_ref``
 */
extern int ada_if_expr_f_then_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_if_expr_f_alternatives(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_allocator``,
 * ``ada_attribute_ref``, ``ada_base_aggregate``, ``ada_bin_op``,
 * ``ada_call_expr``, ``ada_char_literal``, ``ada_concat_op``,
 * ``ada_cond_expr``, ``ada_decl_expr``, ``ada_dotted_name``,
 * ``ada_explicit_deref``, ``ada_identifier``, ``ada_membership_expr``,
 * ``ada_null_literal``, ``ada_num_literal``, ``ada_paren_expr``,
 * ``ada_qual_expr``, ``ada_quantified_expr``, ``ada_raise_expr``,
 * ``ada_reduce_attribute_ref``, ``ada_string_literal``, ``ada_target_name``,
 * ``ada_un_op``, ``ada_update_attribute_ref``
 */
extern int ada_if_expr_f_else_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_contract_cases_f_contract_cases(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field contains a list that itself contains one of the following nodes:
 * ``ada_number_decl``, ``ada_object_decl``, ``ada_single_protected_decl``,
 * ``ada_single_task_decl``
 */
extern int ada_decl_expr_f_decls(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_allocator``,
 * ``ada_attribute_ref``, ``ada_base_aggregate``, ``ada_bin_op``,
 * ``ada_call_expr``, ``ada_char_literal``, ``ada_concat_op``,
 * ``ada_cond_expr``, ``ada_decl_expr``, ``ada_dotted_name``,
 * ``ada_explicit_deref``, ``ada_identifier``, ``ada_membership_expr``,
 * ``ada_null_literal``, ``ada_num_literal``, ``ada_paren_expr``,
 * ``ada_qual_expr``, ``ada_quantified_expr``, ``ada_raise_expr``,
 * ``ada_reduce_attribute_ref``, ``ada_string_literal``, ``ada_target_name``,
 * ``ada_un_op``, ``ada_update_attribute_ref``
 */
extern int ada_decl_expr_f_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_allocator``,
 * ``ada_attribute_ref``, ``ada_base_aggregate``, ``ada_bin_op``,
 * ``ada_call_expr``, ``ada_char_literal``, ``ada_concat_op``,
 * ``ada_cond_expr``, ``ada_decl_expr``, ``ada_dotted_name``,
 * ``ada_explicit_deref``, ``ada_identifier``, ``ada_membership_expr``,
 * ``ada_null_literal``, ``ada_num_literal``, ``ada_paren_expr``,
 * ``ada_qual_expr``, ``ada_quantified_expr``, ``ada_raise_expr``,
 * ``ada_reduce_attribute_ref``, ``ada_string_literal``, ``ada_target_name``,
 * ``ada_un_op``, ``ada_update_attribute_ref``
 */
extern int ada_membership_expr_f_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_op_in``,
 * ``ada_op_not_in``
 */
extern int ada_membership_expr_f_op(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field contains a list that itself contains one of the following nodes:
 * ``ada_allocator``, ``ada_attribute_ref``, ``ada_base_aggregate``,
 * ``ada_bin_op``, ``ada_call_expr``, ``ada_char_literal``, ``ada_concat_op``,
 * ``ada_cond_expr``, ``ada_decl_expr``, ``ada_discrete_subtype_name``,
 * ``ada_dotted_name``, ``ada_explicit_deref``, ``ada_identifier``,
 * ``ada_null_literal``, ``ada_num_literal``, ``ada_paren_expr``,
 * ``ada_qual_expr``, ``ada_quantified_expr``, ``ada_raise_expr``,
 * ``ada_reduce_attribute_ref``, ``ada_string_literal``, ``ada_target_name``,
 * ``ada_un_op``, ``ada_update_attribute_ref``
 */
extern int ada_membership_expr_f_membership_exprs(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * If this name is part of a defining name, return the enclosing defining name
 * node.
 */
extern int ada_name_p_enclosing_defining_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * Return True if this name is part of a defining name.
 */
extern int ada_name_p_is_defining(
    ada_base_entity *node,


    ada_bool *value_p
);


        



/*
 * Helper. Check that this name matches ``sym``.
 */
extern int ada_name_p_name_is(
    ada_base_entity *node,

        
        const ada_symbol_type*
        sym,

    ada_bool *value_p
);


        



/*
 * Return True iff this name represents a call to a subprogram which is
 * referred by its defining name. (i.e. not through a subprogram access).
 */
extern int ada_name_p_is_direct_call(
    ada_base_entity *node,


    ada_bool *value_p
);


        



/*
 * Return True iff this name represents a call to subprogram through an access
 * type.
 */
extern int ada_name_p_is_access_call(
    ada_base_entity *node,


    ada_bool *value_p
);


        



/*
 * Returns True if this Name corresponds to a call.
 */
extern int ada_name_p_is_call(
    ada_base_entity *node,


    ada_bool *value_p
);


        



/*
 * Returns True if this Name corresponds to a dot notation call.
 */
extern int ada_name_p_is_dot_call(
    ada_base_entity *node,

        
        ada_bool
        imprecise_fallback,

    ada_bool *value_p
);


        



/*
 * Failsafe version of ``referenced_defining_name``. Returns a ``RefdDef``,
 * which can be precise, imprecise, or error.
 */
extern int ada_name_p_failsafe_referenced_def_name(
    ada_base_entity *node,

        
        ada_bool
        imprecise_fallback,

    ada_internal_refd_def *value_p
);


        



/*
 * Like ``referenced_decl``, but will return the defining identifier for the
 * decl, rather than the basic declaration node itself.
 */
extern int ada_name_p_referenced_defining_name(
    ada_base_entity *node,

        
        ada_bool
        imprecise_fallback,

    ada_base_entity *value_p
);


        



/*
 * Return all elements in self's scope that are lexically named like Self.
 */
extern int ada_name_p_all_env_elements(
    ada_base_entity *node,

        
        ada_bool
        seq,
        
        const ada_base_entity*
        seq_from,

    ada_ada_node_array *value_p
);


        



/*
 * Return the subprogram specification of the subprogram or subprogram access
 * that is being called by this exact Name, if relevant.
 */
extern int ada_name_p_called_subp_spec(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * Return the declaration this node references after name resolution. If
 * imprecise_fallback is True, errors raised during resolution of the xref
 * equation are catched and a fallback mechanism is triggered, which tries to
 * find the referenced declaration in an ad-hoc way.
 */
extern int ada_name_p_referenced_decl(
    ada_base_entity *node,

        
        ada_bool
        imprecise_fallback,

    ada_base_entity *value_p
);


        



/*
 * Failsafe version of ``referenced_decl``. Returns a ``RefdDecl``, which can
 * be precise, imprecise, or error.
 */
extern int ada_name_p_failsafe_referenced_decl(
    ada_base_entity *node,

        
        ada_bool
        imprecise_fallback,

    ada_internal_refd_decl *value_p
);


        



/*
 * Return the declaration this node references. Try not to run name res if
 * already resolved.
 *
 * .. warning:: INTERNAL USE ONLY.
 */
extern int ada_name_p_referenced_decl_internal(
    ada_base_entity *node,

        
        ada_bool
        imprecise_fallback,

    ada_internal_refd_decl *value_p
);


        



/*
 * Like SubtypeIndication.designated_type, but on names, since because of Ada's
 * ambiguous grammar, some subtype indications will be parsed as names.
 */
extern int ada_name_p_name_designated_type(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * Returns whether Self denotes a static subtype or not.
 */
extern int ada_name_p_is_static_subtype(
    ada_base_entity *node,

        
        ada_bool
        imprecise_fallback,

    ada_bool *value_p
);


        



/*
 * Return whether two names match each other.
 *
 * This compares the symbol for Identifier and StringLiteral nodes. We consider
 * that there is no match for all other node kinds.
 */
extern int ada_name_p_name_matches(
    ada_base_entity *node,

        
        const ada_base_entity*
        n,

    ada_bool *value_p
);


        



/*
 * Returns the relative name of this instance. For example, for a prefix
 * ``A.B.C``, this will return ``C``.
 */
extern int ada_name_p_relative_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * Return whether the name that Self designates is an operator.
 */
extern int ada_name_p_is_operator_name(
    ada_base_entity *node,


    ada_bool *value_p
);


        



/*
 * Whether this name is a write reference.
 *
 * For example, ``X`` is a write reference in the following cases:
 *
 * 1. ``X := 2;``
 *
 * 2. ``X (2) := 2;``
 *
 * 3. ``P(F => X)`` where F is declared ``out`` or ``in out``.
 *
 * 4. ``P(F => T (X))`` where F is declared ``out`` or ``in out``
 *
 * 5. ``X'Access``.
 *
 * 6. ``X.C := 2``, ``R.X := 2``
 *
 * 7. ``X.P`` where the formal for X is declared ``out`` or ``in out``.
 *
 * .. note:: This is an experimental feature. There might be some discrepancy
 *    with the GNAT concept of "write reference".
 */
extern int ada_name_p_is_write_reference(
    ada_base_entity *node,

        
        ada_bool
        imprecise_fallback,

    ada_bool *value_p
);


        



/*
 * Returns True if this Name corresponds to a static non-dispatching call. In
 * other words, this will return True if and only if the target of the call is
 * known statically.
 *
 * .. note:: This is an experimental feature. There might be some discrepancy
 *    with the GNAT concept of "static call".
 */
extern int ada_name_p_is_static_call(
    ada_base_entity *node,

        
        ada_bool
        imprecise_fallback,

    ada_bool *value_p
);


        



/*
 * Turn this name into an array of symbols.
 *
 * For instance, a node with name ``A.B.C`` is turned into ``['A', 'B', 'C']``.
 *
 * Only simple name kinds are allowed: Identifer, DottedName and DefiningName.
 * Any other kind will trigger a PropertyError.
 */
extern int ada_name_p_as_symbol_array(
    ada_base_entity *node,


    ada_unbounded_text_type_array *value_p
);


        



/*
 * Return a canonicalized version of this name's text.
 *
 * Only simple name kinds are allowed: Identifer, DottedName and DefiningName.
 * Any other kind will trigger a PropertyError.
 */
extern int ada_name_p_canonical_text(
    ada_base_entity *node,


    ada_symbol_type *value_p
);


        



/*
 * Return whether this name denotes a constant value.
 */
extern int ada_name_p_is_constant(
    ada_base_entity *node,


    ada_bool *value_p
);


        



/*
 * Returns an array of pairs, associating formal parameters to actual or
 * default expressions.
 */
extern int ada_name_p_call_params(
    ada_base_entity *node,


    ada_param_actual_array *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_attribute_ref``,
 * ``ada_call_expr``, ``ada_char_literal``, ``ada_dotted_name``,
 * ``ada_explicit_deref``, ``ada_identifier``, ``ada_qual_expr``,
 * ``ada_reduce_attribute_ref``, ``ada_string_literal``, ``ada_target_name``,
 * ``ada_update_attribute_ref``
 */
extern int ada_attribute_ref_f_prefix(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_attribute_ref_f_attribute(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field contains a list that itself contains one of the following nodes:
 * ``ada_param_assoc``
 */
extern int ada_attribute_ref_f_args(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_attribute_ref``,
 * ``ada_call_expr``, ``ada_char_literal``, ``ada_dotted_name``,
 * ``ada_explicit_deref``, ``ada_identifier``, ``ada_qual_expr``,
 * ``ada_reduce_attribute_ref``, ``ada_string_literal``, ``ada_target_name``,
 * ``ada_update_attribute_ref``
 */
extern int ada_call_expr_f_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_attribute_ref``,
 * ``ada_basic_assoc_list``, ``ada_bin_op``, ``ada_call_expr``,
 * ``ada_char_literal``, ``ada_discrete_subtype_indication``,
 * ``ada_dotted_name``, ``ada_explicit_deref``, ``ada_identifier``,
 * ``ada_qual_expr``, ``ada_reduce_attribute_ref``, ``ada_string_literal``,
 * ``ada_target_name``, ``ada_update_attribute_ref``
 */
extern int ada_call_expr_f_suffix(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * Return whether this expression is a subprogram call, an array subcomponent
 * access expression, an array slice or a type conversion.
 */
extern int ada_call_expr_p_kind(
    ada_base_entity *node,


    ada_call_expr_kind *value_p
);


        



/*
 * Return whether this CallExpr is actually an access to a slice of the array
 * denoted by the prefix of this CallExpr.
 */
extern int ada_call_expr_p_is_array_slice(
    ada_base_entity *node,


    ada_bool *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_char_literal``,
 * ``ada_dotted_name``, ``ada_identifier``, ``ada_string_literal``,
 * ``ada_synthetic_identifier``
 */
extern int ada_defining_name_f_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * Return a canonical representation of the fully qualified name corresponding
 * to this defining name.
 */
extern int ada_defining_name_p_canonical_fully_qualified_name(
    ada_base_entity *node,


    ada_string_type *value_p
);


        



/*
 * Return a unique identifying name for this defining name, provided this
 * declaration is a public declaration. In the case of subprograms, this will
 * include the profile.
 *
 * .. attention:: This will only return a unique name for public declarations.
 *    Notably, anything nested in an unnamed declare block won't be handled
 *    correctly.
 */
extern int ada_defining_name_p_unique_identifying_name(
    ada_base_entity *node,


    ada_string_type *value_p
);


        



/*
 * Return the fully qualified name corresponding to this defining name, as an
 * array of symbols.
 */
extern int ada_defining_name_p_fully_qualified_name_array(
    ada_base_entity *node,


    ada_unbounded_text_type_array *value_p
);


        



/*
 * Return the fully qualified name corresponding to this defining name.
 */
extern int ada_defining_name_p_fully_qualified_name(
    ada_base_entity *node,


    ada_string_type *value_p
);


        



/*
 * Returns this DefiningName's basic declaration
 */
extern int ada_defining_name_p_basic_decl(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * Find all references to this defining name in the given ``root`` and its
 * children.
 */
extern int ada_defining_name_p_find_refs(
    ada_base_entity *node,

        
        const ada_base_entity*
        root,
        
        ada_bool
        imprecise_fallback,

    ada_ref_result_array *value_p
);


        



/*
 * Searches all references to this defining name in the given list of units.
 *
 * If ``follow_renamings`` is True, also this also includes references that
 * ultimately refer to this defining name, by unwinding renaming clauses.
 */
extern int ada_defining_name_p_find_all_references(
    ada_base_entity *node,

        
        ada_analysis_unit_array
        units,
        
        ada_bool
        follow_renamings,
        
        ada_bool
        imprecise_fallback,

    ada_ref_result_array *value_p
);


        



/*
 * Return the list of all possible calls to the subprogram which Self is the
 * defining name of.
 *
 * This will return the name corresponding to the call, excluding the
 * parameters if there are any. For instance, it will return ``A`` for the ``A
 * (B)`` call.
 *
 * .. note:: This does not yet support calls done inside generics.
 */
extern int ada_defining_name_p_find_all_calls(
    ada_base_entity *node,

        
        ada_analysis_unit_array
        units,
        
        ada_bool
        follow_renamings,
        
        ada_bool
        imprecise_fallback,

    ada_ref_result_array *value_p
);


        



/*
 * Like ``BasicDecl.next_part_for_decl`` on a defining name
 */
extern int ada_defining_name_p_next_part(
    ada_base_entity *node,

        
        ada_bool
        imprecise_fallback,

    ada_base_entity *value_p
);


        



/*
 * Like ``BasicDecl.previous_part_for_decl`` on a defining name
 */
extern int ada_defining_name_p_previous_part(
    ada_base_entity *node,

        
        ada_bool
        imprecise_fallback,

    ada_base_entity *value_p
);


        



/*
 * Like ``BasicDecl.canonical_part`` on a defining name
 */
extern int ada_defining_name_p_canonical_part(
    ada_base_entity *node,

        
        ada_bool
        imprecise_fallback,

    ada_base_entity *value_p
);


        



/*
 * Given an origin node and the entity represented by Self, this property
 * returns the most visible completion of Self that can be seen by origin,
 * according to Ada's visibility rules.
 */
extern int ada_defining_name_p_most_visible_part(
    ada_base_entity *node,

        
        const ada_base_entity*
        origin,
        
        ada_bool
        imprecise_fallback,

    ada_base_entity *value_p
);


        



/*
 * Return all parts that define this entity, sorted from first part to last
 * part.
 */
extern int ada_defining_name_p_all_parts(
    ada_base_entity *node,

        
        ada_bool
        imprecise_fallback,

    ada_ada_node_array *value_p
);


        



/*
 * Return the aspect with name ``name`` associated to entity that this name
 * defines.
 *
 * Aspects are properties of entities that can be specified by the Ada program,
 * either via aspect specifications, pragmas, or attributes.
 *
 * This will return the syntactic node corresponding to attribute directly.
 *
 * Note: for some aspects (e.g. ``Inline``), Libadalang will check if they are
 * defined on any part of the entity.
 */
extern int ada_defining_name_p_get_aspect(
    ada_base_entity *node,

        
        const ada_symbol_type*
        name,
        
        ada_bool
        imprecise_fallback,

    ada_internal_aspect *value_p
);


        



/*
 * Returns whether the boolean aspect named ``name`` is set on the entity
 * represented by this node.
 *
 * "Aspect" is used as in RM terminology (see :rmlink:`13.1`).
 */
extern int ada_defining_name_p_has_aspect(
    ada_base_entity *node,

        
        const ada_symbol_type*
        name,
        
        ada_bool
        imprecise_fallback,

    ada_bool *value_p
);


        



/*
 * Return the pragma with name ``name`` associated to this entity.
 *
 * Please use the ``p_get_aspects`` property instead if you are interested in
 * aspects, i.e. information that can be represented by either aspect
 * specification nodes, pragma nodes or attribute definition nodes.
 */
extern int ada_defining_name_p_get_pragma(
    ada_base_entity *node,

        
        const ada_symbol_type*
        name,

    ada_base_entity *value_p
);


        



/*
 * Return the representation clause associated to this entity that defines the
 * given attribute name.
 */
extern int ada_defining_name_p_get_representation_clause(
    ada_base_entity *node,

        
        const ada_symbol_type*
        name,
        
        ada_bool
        imprecise_fallback,

    ada_base_entity *value_p
);


        



/*
 * Return the at clause associated to this entity.
 */
extern int ada_defining_name_p_get_at_clause(
    ada_base_entity *node,

        
        ada_bool
        imprecise_fallback,

    ada_base_entity *value_p
);


        



/*
 * Whether this entity defined by this name is imported from another language.
 */
extern int ada_defining_name_p_is_imported(
    ada_base_entity *node,


    ada_bool *value_p
);


        



/*
 * Return whether the entity defined by this name is ghost or not. See SPARK RM
 * 6.9.
 */
extern int ada_defining_name_p_is_ghost_code(
    ada_base_entity *node,


    ada_bool *value_p
);


        



/*

 */
extern int ada_discrete_subtype_name_f_subtype(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_attribute_ref``,
 * ``ada_call_expr``, ``ada_char_literal``, ``ada_dotted_name``,
 * ``ada_explicit_deref``, ``ada_identifier``, ``ada_qual_expr``,
 * ``ada_reduce_attribute_ref``, ``ada_string_literal``, ``ada_target_name``,
 * ``ada_update_attribute_ref``
 */
extern int ada_dotted_name_f_prefix(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_char_literal``,
 * ``ada_identifier``, ``ada_string_literal``
 */
extern int ada_dotted_name_f_suffix(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_char_literal``,
 * ``ada_dotted_name``, ``ada_identifier``, ``ada_string_literal``
 */
extern int ada_end_name_f_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * Returns this EndName's basic declaration
 */
extern int ada_end_name_p_basic_decl(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_attribute_ref``,
 * ``ada_call_expr``, ``ada_char_literal``, ``ada_dotted_name``,
 * ``ada_explicit_deref``, ``ada_identifier``, ``ada_qual_expr``,
 * ``ada_reduce_attribute_ref``, ``ada_string_literal``, ``ada_target_name``,
 * ``ada_update_attribute_ref``
 */
extern int ada_explicit_deref_f_prefix(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_attribute_ref``,
 * ``ada_call_expr``, ``ada_char_literal``, ``ada_dotted_name``,
 * ``ada_explicit_deref``, ``ada_identifier``, ``ada_qual_expr``,
 * ``ada_reduce_attribute_ref``, ``ada_string_literal``, ``ada_target_name``,
 * ``ada_update_attribute_ref``
 */
extern int ada_qual_expr_f_prefix(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_base_aggregate``,
 * ``ada_paren_expr``
 */
extern int ada_qual_expr_f_suffix(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_attribute_ref``,
 * ``ada_call_expr``, ``ada_char_literal``, ``ada_dotted_name``,
 * ``ada_explicit_deref``, ``ada_identifier``, ``ada_qual_expr``,
 * ``ada_reduce_attribute_ref``, ``ada_string_literal``, ``ada_target_name``,
 * ``ada_update_attribute_ref``, ``ada_value_sequence``
 */
extern int ada_reduce_attribute_ref_f_prefix(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_reduce_attribute_ref_f_attribute(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field contains a list that itself contains one of the following nodes:
 * ``ada_param_assoc``
 */
extern int ada_reduce_attribute_ref_f_args(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * Return the value that this literal denotes.
 */
extern int ada_char_literal_p_denoted_value(
    ada_base_entity *node,


    uint32_t *value_p
);


        



/*
 * Return the value that this literal denotes.
 */
extern int ada_string_literal_p_denoted_value(
    ada_base_entity *node,


    ada_string_type *value_p
);


        



/*
 * Return the value that this literal denotes.
 */
extern int ada_int_literal_p_denoted_value(
    ada_base_entity *node,


    ada_big_integer *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_attribute_ref``,
 * ``ada_call_expr``, ``ada_char_literal``, ``ada_dotted_name``,
 * ``ada_explicit_deref``, ``ada_identifier``, ``ada_qual_expr``,
 * ``ada_reduce_attribute_ref``, ``ada_string_literal``, ``ada_target_name``,
 * ``ada_update_attribute_ref``
 */
extern int ada_update_attribute_ref_f_prefix(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_update_attribute_ref_f_attribute(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_update_attribute_ref_f_values(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_allocator``,
 * ``ada_attribute_ref``, ``ada_base_aggregate``, ``ada_bin_op``,
 * ``ada_call_expr``, ``ada_char_literal``, ``ada_concat_op``,
 * ``ada_cond_expr``, ``ada_decl_expr``, ``ada_dotted_name``,
 * ``ada_explicit_deref``, ``ada_identifier``, ``ada_membership_expr``,
 * ``ada_null_literal``, ``ada_num_literal``, ``ada_paren_expr``,
 * ``ada_qual_expr``, ``ada_quantified_expr``, ``ada_raise_expr``,
 * ``ada_reduce_attribute_ref``, ``ada_string_literal``, ``ada_target_name``,
 * ``ada_un_op``, ``ada_update_attribute_ref``
 */
extern int ada_paren_expr_f_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_quantified_expr_f_quantifier(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_quantified_expr_f_loop_spec(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_allocator``,
 * ``ada_attribute_ref``, ``ada_base_aggregate``, ``ada_bin_op``,
 * ``ada_call_expr``, ``ada_char_literal``, ``ada_concat_op``,
 * ``ada_cond_expr``, ``ada_decl_expr``, ``ada_dotted_name``,
 * ``ada_explicit_deref``, ``ada_identifier``, ``ada_membership_expr``,
 * ``ada_null_literal``, ``ada_num_literal``, ``ada_paren_expr``,
 * ``ada_qual_expr``, ``ada_quantified_expr``, ``ada_raise_expr``,
 * ``ada_reduce_attribute_ref``, ``ada_string_literal``, ``ada_target_name``,
 * ``ada_un_op``, ``ada_update_attribute_ref``
 */
extern int ada_quantified_expr_f_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_attribute_ref``,
 * ``ada_call_expr``, ``ada_char_literal``, ``ada_dotted_name``,
 * ``ada_explicit_deref``, ``ada_identifier``, ``ada_qual_expr``,
 * ``ada_reduce_attribute_ref``, ``ada_string_literal``, ``ada_target_name``,
 * ``ada_update_attribute_ref``
 */
extern int ada_raise_expr_f_exception_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_allocator``,
 * ``ada_attribute_ref``, ``ada_base_aggregate``, ``ada_bin_op``,
 * ``ada_call_expr``, ``ada_char_literal``, ``ada_concat_op``,
 * ``ada_cond_expr``, ``ada_decl_expr``, ``ada_dotted_name``,
 * ``ada_explicit_deref``, ``ada_identifier``, ``ada_membership_expr``,
 * ``ada_null_literal``, ``ada_num_literal``, ``ada_paren_expr``,
 * ``ada_qual_expr``, ``ada_quantified_expr``, ``ada_raise_expr``,
 * ``ada_reduce_attribute_ref``, ``ada_string_literal``, ``ada_target_name``,
 * ``ada_un_op``, ``ada_update_attribute_ref``
 */
extern int ada_raise_expr_f_error_message(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_op_abs``,
 * ``ada_op_minus``, ``ada_op_not``, ``ada_op_plus``
 */
extern int ada_un_op_f_op(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_allocator``,
 * ``ada_attribute_ref``, ``ada_base_aggregate``, ``ada_bin_op``,
 * ``ada_call_expr``, ``ada_char_literal``, ``ada_cond_expr``,
 * ``ada_decl_expr``, ``ada_dotted_name``, ``ada_explicit_deref``,
 * ``ada_identifier``, ``ada_null_literal``, ``ada_num_literal``,
 * ``ada_paren_expr``, ``ada_qual_expr``, ``ada_quantified_expr``,
 * ``ada_raise_expr``, ``ada_reduce_attribute_ref``, ``ada_string_literal``,
 * ``ada_target_name``, ``ada_un_op``, ``ada_update_attribute_ref``
 */
extern int ada_un_op_f_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field contains a list that itself contains one of the following nodes:
 * ``ada_pragma_node``, ``ada_stmt``
 */
extern int ada_handled_stmts_f_stmts(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field contains a list that itself contains one of the following nodes:
 * ``ada_exception_handler``, ``ada_pragma_node``
 */
extern int ada_handled_stmts_f_exceptions(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_library_item_f_has_private(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes:
 * ``ada_abstract_subp_decl``, ``ada_base_subp_body``, ``ada_error_decl``,
 * ``ada_generic_decl``, ``ada_generic_instantiation``,
 * ``ada_generic_renaming_decl``, ``ada_package_body``, ``ada_package_decl``,
 * ``ada_package_renaming_decl``, ``ada_subp_decl``
 */
extern int ada_library_item_f_item(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * Return whether this is an instance of LimitedPresent
 */
extern int ada_limited_node_p_as_bool(
    ada_base_entity *node,


    ada_bool *value_p
);


        



/*

 */
extern int ada_for_loop_spec_f_var_decl(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_for_loop_spec_f_loop_type(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_for_loop_spec_f_has_reverse(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_attribute_ref``,
 * ``ada_bin_op``, ``ada_call_expr``, ``ada_char_literal``,
 * ``ada_discrete_subtype_indication``, ``ada_dotted_name``,
 * ``ada_explicit_deref``, ``ada_identifier``, ``ada_qual_expr``,
 * ``ada_reduce_attribute_ref``, ``ada_string_literal``, ``ada_target_name``,
 * ``ada_update_attribute_ref``
 */
extern int ada_for_loop_spec_f_iter_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_allocator``,
 * ``ada_attribute_ref``, ``ada_base_aggregate``, ``ada_bin_op``,
 * ``ada_call_expr``, ``ada_char_literal``, ``ada_concat_op``,
 * ``ada_cond_expr``, ``ada_decl_expr``, ``ada_dotted_name``,
 * ``ada_explicit_deref``, ``ada_identifier``, ``ada_membership_expr``,
 * ``ada_null_literal``, ``ada_num_literal``, ``ada_paren_expr``,
 * ``ada_qual_expr``, ``ada_quantified_expr``, ``ada_raise_expr``,
 * ``ada_reduce_attribute_ref``, ``ada_string_literal``, ``ada_target_name``,
 * ``ada_un_op``, ``ada_update_attribute_ref``
 */
extern int ada_for_loop_spec_f_iter_filter(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_allocator``,
 * ``ada_attribute_ref``, ``ada_base_aggregate``, ``ada_bin_op``,
 * ``ada_call_expr``, ``ada_char_literal``, ``ada_concat_op``,
 * ``ada_cond_expr``, ``ada_decl_expr``, ``ada_dotted_name``,
 * ``ada_explicit_deref``, ``ada_identifier``, ``ada_membership_expr``,
 * ``ada_null_literal``, ``ada_num_literal``, ``ada_paren_expr``,
 * ``ada_qual_expr``, ``ada_quantified_expr``, ``ada_raise_expr``,
 * ``ada_reduce_attribute_ref``, ``ada_string_literal``, ``ada_target_name``,
 * ``ada_un_op``, ``ada_update_attribute_ref``
 */
extern int ada_while_loop_spec_f_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field contains a list that itself contains one of the following nodes:
 * ``ada_abstract_state_decl``, ``ada_paren_abstract_state_decl``
 */
extern int ada_multi_abstract_state_decl_f_decls(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * Return whether this is an instance of NotNullPresent
 */
extern int ada_not_null_p_as_bool(
    ada_base_entity *node,


    ada_bool *value_p
);


        



/*

 */
extern int ada_params_f_params(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes:
 * ``ada_abstract_state_decl``, ``ada_paren_abstract_state_decl``
 */
extern int ada_paren_abstract_state_decl_f_decl(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_attribute_ref``,
 * ``ada_bin_op``, ``ada_identifier``, ``ada_paren_expr``, ``ada_un_op``
 */
extern int ada_pp_elsif_directive_f_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_pp_elsif_directive_f_then_kw(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_attribute_ref``,
 * ``ada_bin_op``, ``ada_identifier``, ``ada_paren_expr``, ``ada_un_op``
 */
extern int ada_pp_if_directive_f_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_pp_if_directive_f_then_kw(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_pragma_node_f_id(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_pragma_node_f_args(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * Return whether this pragma is ghost code or not. See SPARK RM 6.9.
 */
extern int ada_pragma_node_p_is_ghost_code(
    ada_base_entity *node,


    ada_bool *value_p
);


        



/*
 * Return an array of ``BasicDecl`` instances associated with this pragma, or
 * an empty array if non applicable.
 */
extern int ada_pragma_node_p_associated_entities(
    ada_base_entity *node,


    ada_ada_node_array *value_p
);


        



/*
 * Return whether this is an instance of PrivatePresent
 */
extern int ada_private_node_p_as_bool(
    ada_base_entity *node,


    ada_bool *value_p
);


        



/*

 */
extern int ada_protected_def_f_public_part(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_protected_def_f_private_part(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_protected_def_f_end_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * Return whether this is an instance of ProtectedPresent
 */
extern int ada_protected_node_p_as_bool(
    ada_base_entity *node,


    ada_bool *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_attribute_ref``,
 * ``ada_bin_op``, ``ada_box_expr``, ``ada_call_expr``, ``ada_char_literal``,
 * ``ada_dotted_name``, ``ada_explicit_deref``, ``ada_identifier``,
 * ``ada_qual_expr``, ``ada_reduce_attribute_ref``, ``ada_string_literal``,
 * ``ada_target_name``, ``ada_update_attribute_ref``
 */
extern int ada_range_spec_f_range(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_attribute_ref``,
 * ``ada_call_expr``, ``ada_char_literal``, ``ada_dotted_name``,
 * ``ada_explicit_deref``, ``ada_identifier``, ``ada_qual_expr``,
 * ``ada_reduce_attribute_ref``, ``ada_string_literal``, ``ada_target_name``,
 * ``ada_update_attribute_ref``
 */
extern int ada_renaming_clause_f_renamed_object(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * Return whether this is an instance of ReversePresent
 */
extern int ada_reverse_node_p_as_bool(
    ada_base_entity *node,


    ada_bool *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_allocator``,
 * ``ada_attribute_ref``, ``ada_base_aggregate``, ``ada_bin_op``,
 * ``ada_call_expr``, ``ada_char_literal``, ``ada_concat_op``,
 * ``ada_cond_expr``, ``ada_decl_expr``, ``ada_dotted_name``,
 * ``ada_explicit_deref``, ``ada_identifier``, ``ada_membership_expr``,
 * ``ada_null_literal``, ``ada_num_literal``, ``ada_paren_expr``,
 * ``ada_qual_expr``, ``ada_quantified_expr``, ``ada_raise_expr``,
 * ``ada_reduce_attribute_ref``, ``ada_string_literal``, ``ada_target_name``,
 * ``ada_un_op``, ``ada_update_attribute_ref``
 */
extern int ada_select_when_part_f_cond_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field contains a list that itself contains one of the following nodes:
 * ``ada_pragma_node``, ``ada_stmt``
 */
extern int ada_select_when_part_f_stmts(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * Return whether this statement is ghost code or not. See SPARK RM 6.9.
 */
extern int ada_stmt_p_is_ghost_code(
    ada_base_entity *node,


    ada_bool *value_p
);


        



/*

 */
extern int ada_accept_stmt_f_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_allocator``,
 * ``ada_attribute_ref``, ``ada_base_aggregate``, ``ada_bin_op``,
 * ``ada_call_expr``, ``ada_char_literal``, ``ada_concat_op``,
 * ``ada_cond_expr``, ``ada_decl_expr``, ``ada_dotted_name``,
 * ``ada_explicit_deref``, ``ada_identifier``, ``ada_membership_expr``,
 * ``ada_null_literal``, ``ada_num_literal``, ``ada_paren_expr``,
 * ``ada_qual_expr``, ``ada_quantified_expr``, ``ada_raise_expr``,
 * ``ada_reduce_attribute_ref``, ``ada_string_literal``, ``ada_target_name``,
 * ``ada_un_op``, ``ada_update_attribute_ref``
 */
extern int ada_accept_stmt_f_entry_index_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_accept_stmt_f_params(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * Return the entry which corresponds to this accept statement.
 */
extern int ada_accept_stmt_p_corresponding_entry(
    ada_base_entity *node,

        
        const ada_base_entity*
        origin,

    ada_base_entity *value_p
);


        



/*

 */
extern int ada_accept_stmt_with_stmts_f_stmts(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_accept_stmt_with_stmts_f_end_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_base_loop_stmt_f_spec(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field contains a list that itself contains one of the following nodes:
 * ``ada_pragma_node``, ``ada_stmt``
 */
extern int ada_base_loop_stmt_f_stmts(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_base_loop_stmt_f_end_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_begin_block_f_stmts(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_begin_block_f_end_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_decl_block_f_decls(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_decl_block_f_stmts(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_decl_block_f_end_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_allocator``,
 * ``ada_attribute_ref``, ``ada_base_aggregate``, ``ada_bin_op``,
 * ``ada_call_expr``, ``ada_char_literal``, ``ada_concat_op``,
 * ``ada_cond_expr``, ``ada_decl_expr``, ``ada_dotted_name``,
 * ``ada_explicit_deref``, ``ada_identifier``, ``ada_membership_expr``,
 * ``ada_null_literal``, ``ada_num_literal``, ``ada_paren_expr``,
 * ``ada_qual_expr``, ``ada_quantified_expr``, ``ada_raise_expr``,
 * ``ada_reduce_attribute_ref``, ``ada_string_literal``, ``ada_target_name``,
 * ``ada_un_op``, ``ada_update_attribute_ref``
 */
extern int ada_case_stmt_f_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_case_stmt_f_pragmas(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_case_stmt_f_alternatives(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_extended_return_stmt_f_decl(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_extended_return_stmt_f_stmts(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_allocator``,
 * ``ada_attribute_ref``, ``ada_base_aggregate``, ``ada_bin_op``,
 * ``ada_call_expr``, ``ada_char_literal``, ``ada_concat_op``,
 * ``ada_cond_expr``, ``ada_decl_expr``, ``ada_dotted_name``,
 * ``ada_explicit_deref``, ``ada_identifier``, ``ada_membership_expr``,
 * ``ada_null_literal``, ``ada_num_literal``, ``ada_paren_expr``,
 * ``ada_qual_expr``, ``ada_quantified_expr``, ``ada_raise_expr``,
 * ``ada_reduce_attribute_ref``, ``ada_string_literal``, ``ada_target_name``,
 * ``ada_un_op``, ``ada_update_attribute_ref``
 */
extern int ada_if_stmt_f_cond_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field contains a list that itself contains one of the following nodes:
 * ``ada_pragma_node``, ``ada_stmt``
 */
extern int ada_if_stmt_f_then_stmts(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_if_stmt_f_alternatives(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field contains a list that itself contains one of the following nodes:
 * ``ada_pragma_node``, ``ada_stmt``
 */
extern int ada_if_stmt_f_else_stmts(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_named_stmt_f_decl(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_base_loop_stmt``,
 * ``ada_block_stmt``
 */
extern int ada_named_stmt_f_stmt(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_select_stmt_f_guards(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field contains a list that itself contains one of the following nodes:
 * ``ada_pragma_node``, ``ada_stmt``
 */
extern int ada_select_stmt_f_else_stmts(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field contains a list that itself contains one of the following nodes:
 * ``ada_pragma_node``, ``ada_stmt``
 */
extern int ada_select_stmt_f_abort_stmts(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field contains a list that itself contains one of the following nodes:
 * ``ada_attribute_ref``, ``ada_call_expr``, ``ada_char_literal``,
 * ``ada_dotted_name``, ``ada_explicit_deref``, ``ada_identifier``,
 * ``ada_qual_expr``, ``ada_reduce_attribute_ref``, ``ada_string_literal``,
 * ``ada_target_name``, ``ada_update_attribute_ref``
 */
extern int ada_abort_stmt_f_names(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_attribute_ref``,
 * ``ada_call_expr``, ``ada_char_literal``, ``ada_dotted_name``,
 * ``ada_explicit_deref``, ``ada_identifier``, ``ada_qual_expr``,
 * ``ada_reduce_attribute_ref``, ``ada_string_literal``, ``ada_target_name``,
 * ``ada_update_attribute_ref``
 */
extern int ada_assign_stmt_f_dest(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_allocator``,
 * ``ada_attribute_ref``, ``ada_base_aggregate``, ``ada_bin_op``,
 * ``ada_call_expr``, ``ada_char_literal``, ``ada_concat_op``,
 * ``ada_cond_expr``, ``ada_decl_expr``, ``ada_dotted_name``,
 * ``ada_explicit_deref``, ``ada_identifier``, ``ada_membership_expr``,
 * ``ada_null_literal``, ``ada_num_literal``, ``ada_paren_expr``,
 * ``ada_qual_expr``, ``ada_quantified_expr``, ``ada_raise_expr``,
 * ``ada_reduce_attribute_ref``, ``ada_string_literal``, ``ada_target_name``,
 * ``ada_un_op``, ``ada_update_attribute_ref``
 */
extern int ada_assign_stmt_f_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_attribute_ref``,
 * ``ada_call_expr``, ``ada_char_literal``, ``ada_dotted_name``,
 * ``ada_explicit_deref``, ``ada_identifier``, ``ada_qual_expr``,
 * ``ada_reduce_attribute_ref``, ``ada_string_literal``, ``ada_target_name``,
 * ``ada_update_attribute_ref``
 */
extern int ada_call_stmt_f_call(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_delay_stmt_f_has_until(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_allocator``,
 * ``ada_attribute_ref``, ``ada_base_aggregate``, ``ada_bin_op``,
 * ``ada_call_expr``, ``ada_char_literal``, ``ada_concat_op``,
 * ``ada_cond_expr``, ``ada_decl_expr``, ``ada_dotted_name``,
 * ``ada_explicit_deref``, ``ada_identifier``, ``ada_membership_expr``,
 * ``ada_null_literal``, ``ada_num_literal``, ``ada_paren_expr``,
 * ``ada_qual_expr``, ``ada_quantified_expr``, ``ada_raise_expr``,
 * ``ada_reduce_attribute_ref``, ``ada_string_literal``, ``ada_target_name``,
 * ``ada_un_op``, ``ada_update_attribute_ref``
 */
extern int ada_delay_stmt_f_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_char_literal``,
 * ``ada_dotted_name``, ``ada_identifier``, ``ada_string_literal``
 */
extern int ada_exit_stmt_f_loop_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_allocator``,
 * ``ada_attribute_ref``, ``ada_base_aggregate``, ``ada_bin_op``,
 * ``ada_call_expr``, ``ada_char_literal``, ``ada_concat_op``,
 * ``ada_cond_expr``, ``ada_decl_expr``, ``ada_dotted_name``,
 * ``ada_explicit_deref``, ``ada_identifier``, ``ada_membership_expr``,
 * ``ada_null_literal``, ``ada_num_literal``, ``ada_paren_expr``,
 * ``ada_qual_expr``, ``ada_quantified_expr``, ``ada_raise_expr``,
 * ``ada_reduce_attribute_ref``, ``ada_string_literal``, ``ada_target_name``,
 * ``ada_un_op``, ``ada_update_attribute_ref``
 */
extern int ada_exit_stmt_f_cond_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_char_literal``,
 * ``ada_dotted_name``, ``ada_identifier``, ``ada_string_literal``
 */
extern int ada_goto_stmt_f_label_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_label_f_decl(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_attribute_ref``,
 * ``ada_call_expr``, ``ada_char_literal``, ``ada_dotted_name``,
 * ``ada_explicit_deref``, ``ada_identifier``, ``ada_qual_expr``,
 * ``ada_reduce_attribute_ref``, ``ada_string_literal``, ``ada_target_name``,
 * ``ada_update_attribute_ref``
 */
extern int ada_raise_stmt_f_exception_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_allocator``,
 * ``ada_attribute_ref``, ``ada_base_aggregate``, ``ada_bin_op``,
 * ``ada_call_expr``, ``ada_char_literal``, ``ada_concat_op``,
 * ``ada_cond_expr``, ``ada_decl_expr``, ``ada_dotted_name``,
 * ``ada_explicit_deref``, ``ada_identifier``, ``ada_membership_expr``,
 * ``ada_null_literal``, ``ada_num_literal``, ``ada_paren_expr``,
 * ``ada_qual_expr``, ``ada_quantified_expr``, ``ada_raise_expr``,
 * ``ada_reduce_attribute_ref``, ``ada_string_literal``, ``ada_target_name``,
 * ``ada_un_op``, ``ada_update_attribute_ref``
 */
extern int ada_raise_stmt_f_error_message(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_attribute_ref``,
 * ``ada_call_expr``, ``ada_char_literal``, ``ada_dotted_name``,
 * ``ada_explicit_deref``, ``ada_identifier``, ``ada_qual_expr``,
 * ``ada_reduce_attribute_ref``, ``ada_string_literal``, ``ada_target_name``,
 * ``ada_update_attribute_ref``
 */
extern int ada_requeue_stmt_f_call_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_requeue_stmt_f_has_abort(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_allocator``,
 * ``ada_attribute_ref``, ``ada_base_aggregate``, ``ada_bin_op``,
 * ``ada_call_expr``, ``ada_char_literal``, ``ada_concat_op``,
 * ``ada_cond_expr``, ``ada_decl_expr``, ``ada_dotted_name``,
 * ``ada_explicit_deref``, ``ada_identifier``, ``ada_membership_expr``,
 * ``ada_null_literal``, ``ada_num_literal``, ``ada_paren_expr``,
 * ``ada_qual_expr``, ``ada_quantified_expr``, ``ada_raise_expr``,
 * ``ada_reduce_attribute_ref``, ``ada_string_literal``, ``ada_target_name``,
 * ``ada_un_op``, ``ada_update_attribute_ref``
 */
extern int ada_return_stmt_f_return_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_char_literal``,
 * ``ada_dotted_name``, ``ada_identifier``, ``ada_string_literal``
 */
extern int ada_subunit_f_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_package_body``,
 * ``ada_protected_body``, ``ada_subp_body``, ``ada_task_body``
 */
extern int ada_subunit_f_body(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * Return the body in which this subunit is rooted.
 */
extern int ada_subunit_p_body_root(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * Return whether this is an instance of SynchronizedPresent
 */
extern int ada_synchronized_node_p_as_bool(
    ada_base_entity *node,


    ada_bool *value_p
);


        



/*
 * Return whether this is an instance of TaggedPresent
 */
extern int ada_tagged_node_p_as_bool(
    ada_base_entity *node,


    ada_bool *value_p
);


        



/*
 * This field contains a list that itself contains one of the following nodes:
 * ``ada_char_literal``, ``ada_dotted_name``, ``ada_identifier``,
 * ``ada_string_literal``
 */
extern int ada_task_def_f_interfaces(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_task_def_f_public_part(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_task_def_f_private_part(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_task_def_f_end_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_access_def_f_has_not_null(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_access_to_subp_def_f_has_protected(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_access_to_subp_def_f_subp_spec(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_anonymous_type_access_def_f_type_decl(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_type_access_def_f_has_all(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_type_access_def_f_has_constant(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_type_access_def_f_subtype_indication(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_array_type_def_f_indices(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_array_type_def_f_component_type(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_derived_type_def_f_has_abstract(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_derived_type_def_f_has_limited(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_derived_type_def_f_has_synchronized(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_derived_type_def_f_subtype_indication(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field contains a list that itself contains one of the following nodes:
 * ``ada_char_literal``, ``ada_dotted_name``, ``ada_identifier``,
 * ``ada_string_literal``
 */
extern int ada_derived_type_def_f_interfaces(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_derived_type_def_f_record_extension(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_derived_type_def_f_has_with_private(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_enum_type_def_f_enum_literals(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_interface_type_def_f_interface_kind(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field contains a list that itself contains one of the following nodes:
 * ``ada_char_literal``, ``ada_dotted_name``, ``ada_identifier``,
 * ``ada_string_literal``
 */
extern int ada_interface_type_def_f_interfaces(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_allocator``,
 * ``ada_attribute_ref``, ``ada_base_aggregate``, ``ada_bin_op``,
 * ``ada_box_expr``, ``ada_call_expr``, ``ada_char_literal``,
 * ``ada_concat_op``, ``ada_cond_expr``, ``ada_decl_expr``,
 * ``ada_dotted_name``, ``ada_explicit_deref``, ``ada_identifier``,
 * ``ada_null_literal``, ``ada_num_literal``, ``ada_paren_expr``,
 * ``ada_qual_expr``, ``ada_quantified_expr``, ``ada_raise_expr``,
 * ``ada_reduce_attribute_ref``, ``ada_string_literal``, ``ada_target_name``,
 * ``ada_un_op``, ``ada_update_attribute_ref``
 */
extern int ada_mod_int_type_def_f_expr(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_private_type_def_f_has_abstract(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_private_type_def_f_has_tagged(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_private_type_def_f_has_limited(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_allocator``,
 * ``ada_attribute_ref``, ``ada_base_aggregate``, ``ada_bin_op``,
 * ``ada_box_expr``, ``ada_call_expr``, ``ada_char_literal``,
 * ``ada_concat_op``, ``ada_cond_expr``, ``ada_decl_expr``,
 * ``ada_dotted_name``, ``ada_explicit_deref``, ``ada_identifier``,
 * ``ada_null_literal``, ``ada_num_literal``, ``ada_paren_expr``,
 * ``ada_qual_expr``, ``ada_quantified_expr``, ``ada_raise_expr``,
 * ``ada_reduce_attribute_ref``, ``ada_string_literal``, ``ada_target_name``,
 * ``ada_un_op``, ``ada_update_attribute_ref``
 */
extern int ada_decimal_fixed_point_def_f_delta(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_allocator``,
 * ``ada_attribute_ref``, ``ada_base_aggregate``, ``ada_bin_op``,
 * ``ada_box_expr``, ``ada_call_expr``, ``ada_char_literal``,
 * ``ada_concat_op``, ``ada_cond_expr``, ``ada_decl_expr``,
 * ``ada_dotted_name``, ``ada_explicit_deref``, ``ada_identifier``,
 * ``ada_null_literal``, ``ada_num_literal``, ``ada_paren_expr``,
 * ``ada_qual_expr``, ``ada_quantified_expr``, ``ada_raise_expr``,
 * ``ada_reduce_attribute_ref``, ``ada_string_literal``, ``ada_target_name``,
 * ``ada_un_op``, ``ada_update_attribute_ref``
 */
extern int ada_decimal_fixed_point_def_f_digits(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_decimal_fixed_point_def_f_range(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_allocator``,
 * ``ada_attribute_ref``, ``ada_base_aggregate``, ``ada_bin_op``,
 * ``ada_box_expr``, ``ada_call_expr``, ``ada_char_literal``,
 * ``ada_concat_op``, ``ada_cond_expr``, ``ada_decl_expr``,
 * ``ada_dotted_name``, ``ada_explicit_deref``, ``ada_identifier``,
 * ``ada_null_literal``, ``ada_num_literal``, ``ada_paren_expr``,
 * ``ada_qual_expr``, ``ada_quantified_expr``, ``ada_raise_expr``,
 * ``ada_reduce_attribute_ref``, ``ada_string_literal``, ``ada_target_name``,
 * ``ada_un_op``, ``ada_update_attribute_ref``
 */
extern int ada_floating_point_def_f_num_digits(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_floating_point_def_f_range(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_allocator``,
 * ``ada_attribute_ref``, ``ada_base_aggregate``, ``ada_bin_op``,
 * ``ada_box_expr``, ``ada_call_expr``, ``ada_char_literal``,
 * ``ada_concat_op``, ``ada_cond_expr``, ``ada_decl_expr``,
 * ``ada_dotted_name``, ``ada_explicit_deref``, ``ada_identifier``,
 * ``ada_null_literal``, ``ada_num_literal``, ``ada_paren_expr``,
 * ``ada_qual_expr``, ``ada_quantified_expr``, ``ada_raise_expr``,
 * ``ada_reduce_attribute_ref``, ``ada_string_literal``, ``ada_target_name``,
 * ``ada_un_op``, ``ada_update_attribute_ref``
 */
extern int ada_ordinary_fixed_point_def_f_delta(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_ordinary_fixed_point_def_f_range(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_record_type_def_f_has_abstract(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_record_type_def_f_has_tagged(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_record_type_def_f_has_limited(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_record_type_def_f_record_def(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_signed_int_type_def_f_range(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * Return the name node for this type expression, if applicable, else null
 */
extern int ada_type_expr_p_type_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * Returns the type declaration designated by this type expression.
 */
extern int ada_type_expr_p_designated_type_decl(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * Return the type declaration designated by this type expression as viewed
 * from the node given by origin_node.
 */
extern int ada_type_expr_p_designated_type_decl_from(
    ada_base_entity *node,

        
        const ada_base_entity*
        origin_node,

    ada_base_entity *value_p
);


        



/*

 */
extern int ada_anonymous_type_f_type_decl(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_subtype_indication_f_has_not_null(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``ada_attribute_ref``,
 * ``ada_char_literal``, ``ada_dotted_name``, ``ada_identifier``,
 * ``ada_string_literal``
 */
extern int ada_subtype_indication_f_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_subtype_indication_f_constraint(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * Returns an array of pairs, associating formal parameters to actual or
 * default expressions.
 */
extern int ada_subtype_indication_p_subtype_constraints(
    ada_base_entity *node,


    ada_param_actual_array *value_p
);


        



/*
 * Returns whether Self denotes a static subtype or not.
 */
extern int ada_subtype_indication_p_is_static_subtype(
    ada_base_entity *node,

        
        ada_bool
        imprecise_fallback,

    ada_bool *value_p
);


        



/*

 */
extern int ada_synthetic_type_expr_f_target_type(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_unconstrained_array_index_f_subtype_indication(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * Return whether this is an instance of UntilPresent
 */
extern int ada_until_node_p_as_bool(
    ada_base_entity *node,


    ada_bool *value_p
);


        



/*
 * This field contains a list that itself contains one of the following nodes:
 * ``ada_char_literal``, ``ada_dotted_name``, ``ada_identifier``,
 * ``ada_string_literal``
 */
extern int ada_use_package_clause_f_packages(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_use_type_clause_f_has_all(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field contains a list that itself contains one of the following nodes:
 * ``ada_attribute_ref``, ``ada_call_expr``, ``ada_char_literal``,
 * ``ada_dotted_name``, ``ada_explicit_deref``, ``ada_identifier``,
 * ``ada_qual_expr``, ``ada_reduce_attribute_ref``, ``ada_string_literal``,
 * ``ada_target_name``, ``ada_update_attribute_ref``
 */
extern int ada_use_type_clause_f_types(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_value_sequence_f_iter_assoc(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field contains a list that itself contains one of the following nodes:
 * ``ada_allocator``, ``ada_attribute_ref``, ``ada_base_aggregate``,
 * ``ada_bin_op``, ``ada_call_expr``, ``ada_char_literal``, ``ada_concat_op``,
 * ``ada_cond_expr``, ``ada_decl_expr``, ``ada_discrete_subtype_indication``,
 * ``ada_dotted_name``, ``ada_explicit_deref``, ``ada_identifier``,
 * ``ada_membership_expr``, ``ada_null_literal``, ``ada_num_literal``,
 * ``ada_others_designator``, ``ada_paren_expr``, ``ada_qual_expr``,
 * ``ada_quantified_expr``, ``ada_raise_expr``, ``ada_reduce_attribute_ref``,
 * ``ada_string_literal``, ``ada_target_name``, ``ada_un_op``,
 * ``ada_update_attribute_ref``
 */
extern int ada_variant_f_choices(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_variant_f_components(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_variant_part_f_discr_name(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_variant_part_f_variant(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_with_clause_f_has_limited(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*

 */
extern int ada_with_clause_f_has_private(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * This field contains a list that itself contains one of the following nodes:
 * ``ada_char_literal``, ``ada_dotted_name``, ``ada_identifier``,
 * ``ada_string_literal``
 */
extern int ada_with_clause_f_packages(
    ada_base_entity *node,


    ada_base_entity *value_p
);


        



/*
 * Return whether this is an instance of WithPrivatePresent
 */
extern int ada_with_private_p_as_bool(
    ada_base_entity *node,


    ada_bool *value_p
);



/*
 * Event handlers
 */

/*
 * Create an event handler. When done with it, the result must be passed to
 * ``ada_dec_ref_event_handler``.
 *
 * Pass as ``data`` a pointer to hold your private data: it will be passed to
 * all callbacks below.
 *
 * ``destroy`` is a callback that is called by ``ada_dec_ref_event_handler`` to
 * leave a chance to free resources that ``data`` may hold.
 *
 * ``unit_requested`` is a callback that will be called when a unit is
 * requested.
 *
 * .. warning:: Please note that the unit requested callback can be called
 *    *many* times for the same unit, so in all likeliness, those events should
 *    be filtered if they're used to forward diagnostics to the user.
 *
 * ``unit_parsed`` is a callback that will be called when a unit is parsed.
 */
extern ada_event_handler
ada_create_event_handler(
   void *data,
   ada_event_handler_destroy_callback destroy_func,
   ada_event_handler_unit_requested_callback unit_requested_func,
   ada_event_handler_unit_parsed_callback unit_parsed_func
);

/*
 * Release an ownership share for this event handler. This destroys the event
 * handler if there are no shares left.
 */
extern void
ada_dec_ref_event_handler(ada_event_handler self);

/*
 * File readers
 */

/*
 * Create a file reader. When done with it, the result must be passed to
 * ``ada_dec_ref_file_reader``.
 *
 * Pass as ``data`` a pointer to hold your private data: it will be passed to
 * all callbacks below.
 *
 * ``destroy`` is a callback that is called by ``ada_dec_ref_file_reader`` to
 * leave a chance to free resources that ``data`` may hold.
 *
 * ``read`` is a callback. For a given filename/charset and whether to read the
 * BOM (Byte Order Mark), it tries to fetch the contents of the source file,
 * returned in ``Contents``. If there is an error, it must return it in
 * ``Diagnostic`` instead.
 */
extern ada_file_reader
ada_create_file_reader(
   void *data,
   ada_file_reader_destroy_callback destroy_func,
   ada_file_reader_read_callback read_func
);

/*
 * Release an ownership share for this file reader. This destroys the file
 * reader if there are no shares left.
 */
extern void
ada_dec_ref_file_reader(ada_file_reader self);




/*
 * Unit providers
 */

/*
 * Create a unit provider. When done with it, the result must be passed to
 * ``ada_destroy_unit_provider``.
 *
 * Pass as ``data`` a pointer to hold your private data: it will be passed to
 * all callbacks below.
 *
 * ``destroy`` is a callback that is called by ``ada_destroy_unit_provider`` to
 * leave a chance to free resources that ``data`` may hold.
 *
 * ``get_unit_from_node`` is a callback. It turns an analysis unit reference
 * represented as a node into an analysis unit. It should return ``NULL`` if
 * the node is not a valid unit name representation.
 *
 * ``get_unit_from_name`` is a callback similar to ``get_unit_from_node``
 * except it takes an analysis unit reference represented as a string.
 */
extern ada_unit_provider
ada_create_unit_provider(
   void *data,
   ada_unit_provider_destroy_callback destroy_func,
   ada_unit_provider_get_unit_filename_callback get_unit_filename_func,
   ada_unit_provider_get_unit_from_name_callback get_unit_from_name_func
);

/*
 * Release an ownership share for this unit provider. This destroys the unit
 * provider if there are no shares left.
 */
extern void
ada_dec_ref_unit_provider(void *data);


      


/*
 * Couple name/value to define a scenario variable for a project.
 */
typedef struct {
   char *name;
   char *value;
} ada_project_scenario_variable;

/*
 * Load the project file at ``Project_File`` and return a unit provider that
 * uses it.
 *
 * If ``Project`` is passed, use it to provide units, otherwise, try use the
 * whole project tree.
 *
 * As unit providers must guarantee that there exists at most one source file
 * for each couple (unit name, unit kind), aggregate projects that contains
 * several conflicting units are not supported: trying to load one will yield
 * an error (see below).
 *
 * If not ``NULL``, ``Scenario_Vars`` must point to an array of
 * ``ada_project_scenario_variable`` couples to provide scenario variables for
 * this project. The last element of this array must end with a ``{ NULL, NULL
 * }`` couple.
 *
 * If not ``NULL``, ``target`` and ``runtime`` must point to valid NULL-
 * terminated strings.
 *
 * When done with it, the result must be free'd with
 * ``ada_destroy_unit_provider``.
 *
 * If the requested project is invalid (error while opening the file, error
 * while analysing its syntax, ...), or if it is an unsupported aggregate
 * project, this returns ``NULL``.
 */
extern ada_unit_provider
ada_create_project_unit_provider(
   char *project_file,
   char *project,
   ada_project_scenario_variable *scenario_vars,
   char *target,
   char *runtime
);

/*
 * Return a unit provider that knows which compilation units are to be found in
 * the given list of source files.
 *
 * This knowledge is built trying to parse all given input files as Ada source
 * files and listing the compilation units found there. Files that cannot be
 * parsed properly are discarded. If two compilation units are found for the
 * same unit, the first that is found in the given input files is taken and the
 * other ones are discarded.
 *
 * Source files are decoded using the given charset. If it is ``NULL``, the
 * default charset (ISO-8859-1) is used.
 *
 * ``input_files`` must point to a ``NULL``-terminated array of filenames.
 * Once this function returns, this array and the strings it contains can be
 * deallocated.
 *
 * When done with it, the result must be free'd with
 * ``ada_destroy_unit_provider``.
 *
 * .. TODO: Find a way to report discarded source files/compilation units.
 */
extern ada_unit_provider
ada_create_auto_provider(
   const char **input_files,
   const char *charset
);



/*
 * Misc
 */

/*
 * Return exception information for the last error that happened in the current
 * thread. Will be automatically allocated on error and free'd on the next
 * error.
 */
extern const ada_exception *
ada_get_last_exception(void);

/*
 * Return a human-readable name for a token kind.
 *
 * The returned string is dynamically allocated and the caller must free it
 * when done with it.
 *
 * If the given kind is invalid, return ``NULL`` and set the last exception
 * accordingly.
 */
extern char *
ada_token_kind_name(ada_token_kind kind);

/*
 * Return a reference to the next token in the corresponding analysis unit.
 */
extern void
ada_token_next(ada_token *token,
                               ada_token *next_token);

/*
 * Return a reference to the previous token in the corresponding analysis unit.
 */
extern void
ada_token_previous(ada_token *token,
                                   ada_token *previous_token);

/*
 * Compute the source buffer slice corresponding to the text that spans between
 * the ``First`` and ``Last`` tokens (both included). This yields an empty
 * slice if ``Last`` actually appears before ``First``. Put the result in
 * ``RESULT``.
 *
 * This returns ``0`` if ``First`` and ``Last`` don't belong to the same
 * analysis unit. Return ``1`` if successful.
 */
extern int
ada_token_range_text(ada_token *first,
                                     ada_token *last,
                                     ada_text *result);

/*
 * Return whether ``L`` and ``R`` are structurally equivalent tokens. This
 * means that their position in the stream won't be taken into account, only
 * the kind and text of the token.
 */
extern void
ada_token_is_equivalent(ada_token *left,
                                        ada_token *right);

/*
 * Return a representation of this entity as a string.
 */
extern void
ada_entity_image(ada_base_entity ent, ada_text *result);

#ifdef __cplusplus
}
#endif

#endif
