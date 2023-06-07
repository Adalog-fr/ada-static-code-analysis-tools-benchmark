(*
 * Copyright (C) 2014-2022, AdaCore
 * SPDX-License-Identifier: Apache-2.0
 *)

(**
 * OCaml binding of the Libadalang API.
 *)

(**
 * Subprograms may raise this when they cannot open a source file. Note that
 * this does *not* concern analysis unit getters, which create diagnostic
 * vectors for such errors.
 *)
exception FileReadError of string

(**
 * Raised when introspection functions (``Libadalang.Introspection``) are
 * provided mismatching types/values.
 *)
exception BadTypeError of string

(**
 * Raised when introspection functions (``Libadalang.Introspection``) are
 * passed an out of bounds index.
 *)
exception OutOfBoundsError of string

(**
 * Raised by lexing functions (``Libadalang.Lexer``) when the input contains an
 * invalid byte sequence.
 *)
exception InvalidInput of string

(**
 * Exception raise when an invalid symbol is passed to a subprogram.
 *)
exception InvalidSymbolError of string

(**
 * Raised when an invalid unit name is provided.
 *)
exception InvalidUnitNameError of string

(**
 * Exception raised in language bindings when the underlying C API reports an
 * unexpected error that occurred in the library.
 *
 * This kind of exception is raised for internal errors: they should never
 * happen in normal situations and if they are raised at some point, it means
 * the library state is potentially corrupted.
 *
 * Nevertheless, the library does its best not to crash the program,
 * materializing internal errors using this kind of exception.
 *)
exception NativeException of string

(**
 * Exception raised when an API is called while its preconditions are not
 * satisfied.
 *)
exception PreconditionFailure of string

(**
 * Exception that is raised when an error occurs while evaluating any AST node
 * method whose name starts with ``p_``. This is the only exceptions that such
 * functions can raise.
 *)
exception PropertyError of string

(**
 * Exception raised when the provided arguments for a template don't match what
 * the template expects.
 *)
exception TemplateArgsError of string

(**
 * Exception raised when a template has an invalid syntax, such as badly
 * formatted placeholders.
 *)
exception TemplateFormatError of string

(**
 * Exception raised when the instantiation of a template cannot be parsed.
 *)
exception TemplateInstantiationError of string

(**
 * Exception raised while trying to access data that was deallocated. This
 * happens when one tries to use a node whose unit has been reparsed, for
 * instance.
 *)
exception StaleReferenceError of string

(**
 * Subprograms may raise this when they try to parse invalid syntax. Also
 * raised if a field in a parsing node is null due to a syntax error.
 *)
exception SyntaxError of string

(**
 * Raised by lexing functions (``Libadalang.Lexer``) when the input charset is
 * not supported.
 *)
exception UnknownCharset of string

(**
 * Raised when an error occurs while loading a project file.
 *)
exception InvalidProject of string

(**
 * Raised when creating a project unit provider for an unsupported project view
 * (for instance, a view with conflicting aggregated projects).
 *)
exception UnsupportedViewError of string





module AnalysisUnitKind : sig
  (**
  * Specify a kind of analysis unit. Specification units provide an interface
  * to the outer world while body units provide an implementation for the
  * corresponding interface.
  *)

  type t =
  | UnitSpecification
  | UnitBody

  val name : unit -> string
end

module LookupKind : sig
  (**

  *)

  type t =
  | Recursive
  | Flat
  | Minimal

  val name : unit -> string
end

module DesignatedEnvKind : sig
  (**
  * Discriminant for DesignatedEnv structures.
  *)

  type t =
  | None
  | CurrentEnv
  | NamedEnv
  | DirectEnv

  val name : unit -> string
end

module RefResultKind : sig
  (**
  * Kind for the result of a cross reference operation.
  *
  * * ``no_ref`` is for no reference, it is the null value for this enum.
  *
  * * ``precise`` is when the reference result is precise.
  *
  * * ``imprecise`` is when there was an error computing the precise result,
  *   and a result was gotten in an imprecise fashion.
  *
  * * ``error`` is for unrecoverable errors (either there is no imprecise path
  *   for the request you made, or the imprecise path errored out too.
  *)

  type t =
  | NoRef
  | Precise
  | Imprecise
  | Error

  val name : unit -> string
end

module CallExprKind : sig
  (**
  * Kind of CallExpr type.
  *
  * * ``call`` is when the CallExpr is a procedure or function call.
  *
  * * ``array_slice``, ``array_index`` is when the CallExpr is in fact an array
  *   slice or an array subcomponent access expression, respectively.
  *
  * * ``type_conversion`` is when the CallExpr is a type conversion.
  *)

  type t =
  | Call
  | ArraySlice
  | ArrayIndex
  | TypeConversion

  val name : unit -> string
end

module GrammarRule : sig
  (**
  * Gramar rule to use for parsing.
  *)

  type t =
  | ParentListRule
  | ProtectedTypeDeclRule
  | ProtectedOpRule
  | ProtectedElRule
  | ProtectedDefRule
  | ProtectedDeclRule
  | TaskItemRule
  | TaskDefRule
  | TaskTypeDeclRule
  | SubtypeDeclRule
  | InterfaceTypeDefRule
  | UnconstrainedIndexRule
  | ArrayTypeDefRule
  | DiscreteSubtypeDefinitionRule
  | ConstraintListRule
  | SignedIntTypeDefRule
  | ModIntTypeDefRule
  | DerivedTypeDefRule
  | CompositeConstraintAssocRule
  | CompositeConstraintRule
  | DigitsConstraintRule
  | DeltaConstraintRule
  | RangeConstraintRule
  | ConstraintRule
  | DiscriminantSpecRule
  | DiscrSpecListRule
  | DiscriminantPartRule
  | EnumLiteralDeclRule
  | FormalDiscreteTypeDefRule
  | RecordDefRule
  | RangeSpecRule
  | RealTypeDefRule
  | SexprOrBoxRule
  | OrdinaryFixedPointDefRule
  | DecimalFixedPointDefRule
  | FloatingPointDefRule
  | RecordTypeDefRule
  | AccessDefRule
  | EnumTypeDefRule
  | TypeDefRule
  | VariantRule
  | AnonymousTypeDeclRule
  | IncompleteTypeDeclRule
  | TypeDeclRule
  | VariantPartRule
  | ComponentDefRule
  | ComponentItemRule
  | ComponentDeclRule
  | ComponentListRule
  | GenericDeclRule
  | GenericFormalPartRule
  | GenericFormalDeclRule
  | FormalTypeDeclRule
  | FormalSubpDeclRule
  | RenamingClauseRule
  | GenericRenamingDeclRule
  | GenericInstantiationRule
  | ExceptionDeclRule
  | BasicDeclsRule
  | PackageRenamingDeclRule
  | PackageDeclRule
  | BasicDeclRule
  | ObjectDeclRule
  | SubObjectDeclRule
  | NoTypeObjectRenamingDeclRule
  | ExtRetStmtObjectDeclRule
  | DefiningIdListRule
  | NumberDeclRule
  | ContractCaseAssocRule
  | ContractCasesExprRule
  | AbstractStateDeclRule
  | MultiAbstractStateDeclRule
  | AspectAssocRule
  | AspectSpecRule
  | SingleTaskDeclRule
  | OverridingIndicatorRule
  | EntryDeclRule
  | ComponentClauseRule
  | AspectClauseRule
  | ParamSpecRule
  | ParamSpecsRule
  | SubpSpecRule
  | ExprFnRule
  | NullSubpDeclRule
  | AbstractSubpDeclRule
  | SubpRenamingDeclRule
  | SimpleSubpDeclRule
  | SubpDeclRule
  | WithClauseRule
  | ContextItemRule
  | UseClauseRule
  | UsePackageClauseRule
  | UseTypeClauseRule
  | SubtypeIndicationRule
  | DiscreteSubtypeIndicationRule
  | ConstrainedSubtypeIndicationRule
  | TypeExprRule
  | AnonymousTypeRule
  | ModeRule
  | PragmaArgumentRule
  | PragmaRule
  | SubunitRule
  | LibraryUnitBodyRule
  | LibraryUnitRenamingDeclRule
  | LibraryItemRule
  | CompilationUnitRule
  | CompilationRule
  | DeclPartRule
  | EntryBodyRule
  | ProtectedBodyRule
  | ProtectedBodyStubRule
  | TaskBodyRule
  | TaskBodyStubRule
  | PackageBodyStubRule
  | PackageBodyRule
  | TerminateAlternativeRule
  | SelectStmtRule
  | AcceptStmtRule
  | CaseAltRule
  | CaseStmtRule
  | ExtReturnStmtRule
  | IblockStmtRule
  | BlockStmtRule
  | WhileLoopSpecRule
  | IloopStmtRule
  | LoopStmtRule
  | CompoundStmtRule
  | ElsifPartRule
  | IfStmtRule
  | RaiseStmtRule
  | DelayStmtRule
  | AbortStmtRule
  | BodyRule
  | BodyStubRule
  | SubpBodyStubRule
  | RecovDeclPartRule
  | SubpBodyRule
  | HandledStmtsRule
  | ExceptionHandlerRule
  | StmtsRule
  | LabelRule
  | StmtRule
  | CallStmtRule
  | SimpleStmtRule
  | NullStmtRule
  | AssignmentStmtRule
  | GotoStmtRule
  | ExitStmtRule
  | ReturnStmtRule
  | RequeueStmtRule
  | IdentifierRule
  | CharLiteralRule
  | StringLiteralRule
  | DefiningIdRule
  | DecLiteralRule
  | IntLiteralRule
  | NumLiteralRule
  | NullLiteralRule
  | AllocatorRule
  | ForLoopParamSpecRule
  | QuantifiedExprRule
  | CaseExprRule
  | CaseExprAltRule
  | RaiseExprRule
  | IfExprRule
  | ConditionalExprRule
  | BoxExprRule
  | OthersDesignatorRule
  | IteratedAssocRule
  | AggregateAssocRule
  | RegularAggregateRule
  | BracketAggregateRule
  | AggregateRule
  | DirectNameRule
  | ParamAssocRule
  | CallSuffixRule
  | AttrSuffixRule
  | QualifiedNameRule
  | QualNameInternalRule
  | ValueSequenceRule
  | NameRule
  | DefiningNameRule
  | DirectNameOrTargetNameRule
  | TargetNameRule
  | UpdateAttrAggregateRule
  | UpdateAttrContentRule
  | MultidimArrayAssocRule
  | SubtypeNameRule
  | StaticNameRule
  | PrimaryRule
  | ParenExprRule
  | DeclareExprRule
  | FactorRule
  | TermRule
  | UnopTermRule
  | SimpleExprRule
  | BooleanOpRule
  | DiscreteRangeRule
  | ChoiceRule
  | ChoiceListRule
  | RelOpRule
  | MembershipChoiceRule
  | MembershipChoiceListRule
  | RelationRule
  | ExprRule
  | PpDirectiveRule
  | PpThenRule
  | PpExprRule
  | PpTermRule

  val name : unit -> string
end

val default_grammar_rule : GrammarRule.t

module Camomile : CamomileLibrary.Type

module Sloc : sig
  (**
  * Location in a source file. Line and column numbers are one-based.
  *)

  type t = {
    line : int;
    column : int;
  }
end

module SlocRange : sig
  (**
  * Location of a span of text in a source file.
  *)

  type t = {
    loc_start : Sloc.t;
    loc_end : Sloc.t;
  }

  val pp : Format.formatter -> t -> unit
end

module Diagnostic : sig
  (**
  * Diagnostic for an analysis unit: cannot open the source file, parsing
  * error, ...
  *)

  type t = {
    sloc_range : SlocRange.t;
    message : string;
  }
end

module TokenData : sig
  type t
end

module Token : sig
  (**
  * Reference to a token in an analysis unit.
  *)

  type dummy_context

  type t = {
    context : dummy_context;
    token_data : TokenData.t;
    token_index : int;
    trivia_index : int;
    kind : int;
    text : string;
    sloc_range : SlocRange.t;
  }

  val kind_name : t -> string
  (**
  * Return a human-readable name for a token kind.
  *)

  val text_range : t -> t -> string
  (**
  * Compute the source buffer slice corresponding to the text that spans
  * between the ``First`` and ``Last`` tokens (both included). This yields an
  * empty slice if ``Last`` actually appears before ``First``.
  *
  * if ``First`` and ``Last`` don't belong to the same analysis unit.
  *)

  val is_trivia : t -> bool
  (**
  * Return whether this token is a trivia. If it's not, it's a regular token.
  *)

  val index : t -> int
  (**
  * Zero-based index for this token/trivia. Tokens and trivias get their own
  * index space.
  *)

  val next : t -> t option
  (**
  * Return a reference to the first token scanned in this unit.
  *)

  val previous : t -> t option
  (**
  * Return a reference to the last token scanned in this unit.
  *)

  val compare : t -> t -> int

  val equal : t -> t -> bool

  val hash : t -> int

  val is_equivalent : t -> t -> bool
  (**
  * Return whether ``L`` and ``R`` are structurally equivalent tokens. This
  * means that their position in the stream won't be taken into account, only
  * the kind and text of the token.
  *)

  val pp : Format.formatter -> t -> unit
end

module BigInteger : sig
  type t = Z.t
end

module BareNode : sig
  type t
end

module Rebindings : sig
  type t
end

module UnitProvider : sig
  (**
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
  *)

  type t

  
        val for_project :
    ?project:string
    -> ?scenario_vars:(string * string) list
    -> ?target:string
    -> ?runtime:string
    -> string
    -> t
  (**
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
  * If not ``None``, ``Scenario_Vars`` must point to an array of
  * ``ada_project_scenario_variable`` couples to provide scenario variables for
  * this project. The last element of this array must end with a ``{ None, None
  * }`` couple.
  *
  * If not ``None``, ``target`` and ``runtime`` must point to valid NULL-
  * terminated strings.
  *
  * If the requested project is invalid (error while opening the file, error
  * while analysing its syntax, ...), or if it is an unsupported aggregate
  * project, this returns ``None``.
  *)

  val auto : string list -> t
  (**
  * Return a unit provider that knows which compilation units are to be found
  * in the given list of source files.
  *
  * This knowledge is built trying to parse all given input files as Ada source
  * files and listing the compilation units found there. Files that cannot be
  * parsed properly are discarded. If two compilation units are found for the
  * same unit, the first that is found in the given input files is taken and
  * the other ones are discarded.
  *
  * Source files are decoded using the given charset. If it is ``None``, the
  * default charset (ISO-8859-1) is used.
  *
  * .. TODO: Find a way to report discarded source files/compilation units.
  *)


end

type analysis_context

and analysis_unit

and entity


  and entity_info = {
      md :
         metadata;
      rebindings :
         Rebindings.t;
      from_rebound :
         bool;
  }



  and metadata = {
      dottable_subp :
         bool;
      primitive :
         BareNode.t;
      primitive_real_type :
         BareNode.t;
  }


  
   
  (**
    * abort_node
    * abstract_node
    * ada_list
    * aliased_node
    * all_node
    * array_indices
    * aspect_assoc
    * aspect_clause
    * aspect_spec
    * base_assoc
    * base_formal_param_holder
    * base_record_def
    * basic_assoc
    * basic_decl
    * case_stmt_alternative
    * compilation_unit
    * component_clause
    * component_def
    * constant_node
    * constraint_node
    * declarative_part
    * elsif_expr_part
    * elsif_stmt_part
    * expr
    * handled_stmts
    * interface_kind
    * iter_type
    * library_item
    * limited_node
    * loop_spec
    * mode
    * multi_abstract_state_decl
    * not_null
    * null_component_decl
    * others_designator
    * overriding_node
    * params
    * paren_abstract_state_decl
    * pp_directive
    * pp_then_kw
    * pragma_node
    * private_node
    * protected_def
    * protected_node
    * quantifier
    * range_spec
    * renaming_clause
    * reverse_node
    * select_when_part
    * stmt
    * subp_kind
    * subunit
    * synchronized_node
    * tagged_node
    * task_def
    * type_attributes_repository
    * type_def
    * type_expr
    * unconstrained_array_index
    * until_node
    * use_clause
    * value_sequence
    * variant
    * variant_part
    * with_clause
    * with_private
    *)
  and ada_node =
    [
    | `AbortAbsent
        of abort_absent_fields
    | `AbortPresent
        of abort_present_fields
    | `AbstractAbsent
        of abstract_absent_fields
    | `AbstractPresent
        of abstract_present_fields
    | `AdaNodeList
        of ada_node_list_fields
    | `AbstractStateDeclList
        of abstract_state_decl_list_fields
    | `AlternativesList
        of alternatives_list_fields
    | `ConstraintList
        of constraint_list_fields
    | `DeclList
        of decl_list_fields
    | `StmtList
        of stmt_list_fields
    | `AspectAssocList
        of aspect_assoc_list_fields
    | `BaseAssocList
        of base_assoc_list_fields
    | `AssocList
        of assoc_list_fields
    | `BasicDeclList
        of basic_decl_list_fields
    | `CaseExprAlternativeList
        of case_expr_alternative_list_fields
    | `CaseStmtAlternativeList
        of case_stmt_alternative_list_fields
    | `CompilationUnitList
        of compilation_unit_list_fields
    | `ConcatOperandList
        of concat_operand_list_fields
    | `ContractCaseAssocList
        of contract_case_assoc_list_fields
    | `DefiningNameList
        of defining_name_list_fields
    | `DiscriminantSpecList
        of discriminant_spec_list_fields
    | `ElsifExprPartList
        of elsif_expr_part_list_fields
    | `ElsifStmtPartList
        of elsif_stmt_part_list_fields
    | `EnumLiteralDeclList
        of enum_literal_decl_list_fields
    | `ExprAlternativesList
        of expr_alternatives_list_fields
    | `DiscriminantChoiceList
        of discriminant_choice_list_fields
    | `NameList
        of name_list_fields
    | `ParentList
        of parent_list_fields
    | `ParamSpecList
        of param_spec_list_fields
    | `PragmaNodeList
        of pragma_node_list_fields
    | `SelectWhenPartList
        of select_when_part_list_fields
    | `UnconstrainedArrayIndexList
        of unconstrained_array_index_list_fields
    | `VariantList
        of variant_list_fields
    | `AliasedAbsent
        of aliased_absent_fields
    | `AliasedPresent
        of aliased_present_fields
    | `AllAbsent
        of all_absent_fields
    | `AllPresent
        of all_present_fields
    | `ConstrainedArrayIndices
        of constrained_array_indices_fields
    | `UnconstrainedArrayIndices
        of unconstrained_array_indices_fields
    | `AspectAssoc
        of aspect_assoc_fields
    | `AtClause
        of at_clause_fields
    | `AttributeDefClause
        of attribute_def_clause_fields
    | `EnumRepClause
        of enum_rep_clause_fields
    | `RecordRepClause
        of record_rep_clause_fields
    | `AspectSpec
        of aspect_spec_fields
    | `ContractCaseAssoc
        of contract_case_assoc_fields
    | `PragmaArgumentAssoc
        of pragma_argument_assoc_fields
    | `EntrySpec
        of entry_spec_fields
    | `EnumSubpSpec
        of enum_subp_spec_fields
    | `SubpSpec
        of subp_spec_fields
    | `SyntheticBinarySpec
        of synthetic_binary_spec_fields
    | `SyntheticUnarySpec
        of synthetic_unary_spec_fields
    | `ComponentList
        of component_list_fields
    | `KnownDiscriminantPart
        of known_discriminant_part_fields
    | `UnknownDiscriminantPart
        of unknown_discriminant_part_fields
    | `EntryCompletionFormalParams
        of entry_completion_formal_params_fields
    | `GenericFormalPart
        of generic_formal_part_fields
    | `NullRecordDef
        of null_record_def_fields
    | `RecordDef
        of record_def_fields
    | `AggregateAssoc
        of aggregate_assoc_fields
    | `MultiDimArrayAssoc
        of multi_dim_array_assoc_fields
    | `CompositeConstraintAssoc
        of composite_constraint_assoc_fields
    | `IteratedAssoc
        of iterated_assoc_fields
    | `ParamAssoc
        of param_assoc_fields
    | `AbstractStateDecl
        of abstract_state_decl_fields
    | `AnonymousExprDecl
        of anonymous_expr_decl_fields
    | `ComponentDecl
        of component_decl_fields
    | `DiscriminantSpec
        of discriminant_spec_fields
    | `GenericFormalObjDecl
        of generic_formal_obj_decl_fields
    | `GenericFormalPackage
        of generic_formal_package_fields
    | `GenericFormalSubpDecl
        of generic_formal_subp_decl_fields
    | `GenericFormalTypeDecl
        of generic_formal_type_decl_fields
    | `ParamSpec
        of param_spec_fields
    | `SyntheticFormalParamDecl
        of synthetic_formal_param_decl_fields
    | `GenericPackageInternal
        of generic_package_internal_fields
    | `PackageDecl
        of package_decl_fields
    | `DiscreteBaseSubtypeDecl
        of discrete_base_subtype_decl_fields
    | `SubtypeDecl
        of subtype_decl_fields
    | `ClasswideTypeDecl
        of classwide_type_decl_fields
    | `IncompleteTypeDecl
        of incomplete_type_decl_fields
    | `IncompleteFormalTypeDecl
        of incomplete_formal_type_decl_fields
    | `IncompleteTaggedTypeDecl
        of incomplete_tagged_type_decl_fields
    | `ProtectedTypeDecl
        of protected_type_decl_fields
    | `TaskTypeDecl
        of task_type_decl_fields
    | `SingleTaskTypeDecl
        of single_task_type_decl_fields
    | `AnonymousTypeDecl
        of anonymous_type_decl_fields
    | `SynthAnonymousTypeDecl
        of synth_anonymous_type_decl_fields
    | `ConcreteTypeDecl
        of concrete_type_decl_fields
    | `FormalTypeDecl
        of formal_type_decl_fields
    | `AbstractSubpDecl
        of abstract_subp_decl_fields
    | `AbstractFormalSubpDecl
        of abstract_formal_subp_decl_fields
    | `ConcreteFormalSubpDecl
        of concrete_formal_subp_decl_fields
    | `SubpDecl
        of subp_decl_fields
    | `EntryDecl
        of entry_decl_fields
    | `EnumLiteralDecl
        of enum_literal_decl_fields
    | `SyntheticCharEnumLit
        of synthetic_char_enum_lit_fields
    | `GenericSubpInternal
        of generic_subp_internal_fields
    | `SyntheticSubpDecl
        of synthetic_subp_decl_fields
    | `ExprFunction
        of expr_function_fields
    | `NullSubpDecl
        of null_subp_decl_fields
    | `SubpBody
        of subp_body_fields
    | `SubpRenamingDecl
        of subp_renaming_decl_fields
    | `PackageBodyStub
        of package_body_stub_fields
    | `ProtectedBodyStub
        of protected_body_stub_fields
    | `SubpBodyStub
        of subp_body_stub_fields
    | `TaskBodyStub
        of task_body_stub_fields
    | `EntryBody
        of entry_body_fields
    | `PackageBody
        of package_body_fields
    | `ProtectedBody
        of protected_body_fields
    | `TaskBody
        of task_body_fields
    | `EntryIndexSpec
        of entry_index_spec_fields
    | `ErrorDecl
        of error_decl_fields
    | `ExceptionDecl
        of exception_decl_fields
    | `ExceptionHandler
        of exception_handler_fields
    | `ForLoopVarDecl
        of for_loop_var_decl_fields
    | `GenericPackageDecl
        of generic_package_decl_fields
    | `GenericSubpDecl
        of generic_subp_decl_fields
    | `GenericPackageInstantiation
        of generic_package_instantiation_fields
    | `GenericSubpInstantiation
        of generic_subp_instantiation_fields
    | `GenericPackageRenamingDecl
        of generic_package_renaming_decl_fields
    | `GenericSubpRenamingDecl
        of generic_subp_renaming_decl_fields
    | `LabelDecl
        of label_decl_fields
    | `NamedStmtDecl
        of named_stmt_decl_fields
    | `NumberDecl
        of number_decl_fields
    | `ObjectDecl
        of object_decl_fields
    | `ExtendedReturnStmtObjectDecl
        of extended_return_stmt_object_decl_fields
    | `NoTypeObjectRenamingDecl
        of no_type_object_renaming_decl_fields
    | `PackageRenamingDecl
        of package_renaming_decl_fields
    | `SingleProtectedDecl
        of single_protected_decl_fields
    | `SingleTaskDecl
        of single_task_decl_fields
    | `CaseStmtAlternative
        of case_stmt_alternative_fields
    | `CompilationUnit
        of compilation_unit_fields
    | `ComponentClause
        of component_clause_fields
    | `ComponentDef
        of component_def_fields
    | `ConstantAbsent
        of constant_absent_fields
    | `ConstantPresent
        of constant_present_fields
    | `CompositeConstraint
        of composite_constraint_fields
    | `DeltaConstraint
        of delta_constraint_fields
    | `DigitsConstraint
        of digits_constraint_fields
    | `RangeConstraint
        of range_constraint_fields
    | `DeclarativePart
        of declarative_part_fields
    | `PrivatePart
        of private_part_fields
    | `PublicPart
        of public_part_fields
    | `ElsifExprPart
        of elsif_expr_part_fields
    | `ElsifStmtPart
        of elsif_stmt_part_fields
    | `AbstractStateDeclExpr
        of abstract_state_decl_expr_fields
    | `Allocator
        of allocator_fields
    | `Aggregate
        of aggregate_fields
    | `BracketAggregate
        of bracket_aggregate_fields
    | `DeltaAggregate
        of delta_aggregate_fields
    | `BracketDeltaAggregate
        of bracket_delta_aggregate_fields
    | `NullRecordAggregate
        of null_record_aggregate_fields
    | `BinOp
        of bin_op_fields
    | `RelationOp
        of relation_op_fields
    | `BoxExpr
        of box_expr_fields
    | `CaseExprAlternative
        of case_expr_alternative_fields
    | `ConcatOp
        of concat_op_fields
    | `ConcatOperand
        of concat_operand_fields
    | `CaseExpr
        of case_expr_fields
    | `IfExpr
        of if_expr_fields
    | `ContractCases
        of contract_cases_fields
    | `DeclExpr
        of decl_expr_fields
    | `MembershipExpr
        of membership_expr_fields
    | `AttributeRef
        of attribute_ref_fields
    | `CallExpr
        of call_expr_fields
    | `DefiningName
        of defining_name_fields
    | `SyntheticDefiningName
        of synthetic_defining_name_fields
    | `DiscreteSubtypeName
        of discrete_subtype_name_fields
    | `DottedName
        of dotted_name_fields
    | `EndName
        of end_name_fields
    | `ExplicitDeref
        of explicit_deref_fields
    | `QualExpr
        of qual_expr_fields
    | `ReduceAttributeRef
        of reduce_attribute_ref_fields
    | `CharLiteral
        of char_literal_fields
    | `Identifier
        of identifier_fields
    | `OpAbs
        of op_abs_fields
    | `OpAnd
        of op_and_fields
    | `OpAndThen
        of op_and_then_fields
    | `OpConcat
        of op_concat_fields
    | `OpDiv
        of op_div_fields
    | `OpDoubleDot
        of op_double_dot_fields
    | `OpEq
        of op_eq_fields
    | `OpGt
        of op_gt_fields
    | `OpGte
        of op_gte_fields
    | `OpIn
        of op_in_fields
    | `OpLt
        of op_lt_fields
    | `OpLte
        of op_lte_fields
    | `OpMinus
        of op_minus_fields
    | `OpMod
        of op_mod_fields
    | `OpMult
        of op_mult_fields
    | `OpNeq
        of op_neq_fields
    | `OpNot
        of op_not_fields
    | `OpNotIn
        of op_not_in_fields
    | `OpOr
        of op_or_fields
    | `OpOrElse
        of op_or_else_fields
    | `OpPlus
        of op_plus_fields
    | `OpPow
        of op_pow_fields
    | `OpRem
        of op_rem_fields
    | `OpXor
        of op_xor_fields
    | `StringLiteral
        of string_literal_fields
    | `NullLiteral
        of null_literal_fields
    | `IntLiteral
        of int_literal_fields
    | `RealLiteral
        of real_literal_fields
    | `SyntheticIdentifier
        of synthetic_identifier_fields
    | `TargetName
        of target_name_fields
    | `UpdateAttributeRef
        of update_attribute_ref_fields
    | `ParenExpr
        of paren_expr_fields
    | `QuantifiedExpr
        of quantified_expr_fields
    | `RaiseExpr
        of raise_expr_fields
    | `UnOp
        of un_op_fields
    | `HandledStmts
        of handled_stmts_fields
    | `InterfaceKindLimited
        of interface_kind_limited_fields
    | `InterfaceKindProtected
        of interface_kind_protected_fields
    | `InterfaceKindSynchronized
        of interface_kind_synchronized_fields
    | `InterfaceKindTask
        of interface_kind_task_fields
    | `IterTypeIn
        of iter_type_in_fields
    | `IterTypeOf
        of iter_type_of_fields
    | `LibraryItem
        of library_item_fields
    | `LimitedAbsent
        of limited_absent_fields
    | `LimitedPresent
        of limited_present_fields
    | `ForLoopSpec
        of for_loop_spec_fields
    | `WhileLoopSpec
        of while_loop_spec_fields
    | `ModeDefault
        of mode_default_fields
    | `ModeIn
        of mode_in_fields
    | `ModeInOut
        of mode_in_out_fields
    | `ModeOut
        of mode_out_fields
    | `MultiAbstractStateDecl
        of multi_abstract_state_decl_fields
    | `NotNullAbsent
        of not_null_absent_fields
    | `NotNullPresent
        of not_null_present_fields
    | `NullComponentDecl
        of null_component_decl_fields
    | `OthersDesignator
        of others_designator_fields
    | `OverridingNotOverriding
        of overriding_not_overriding_fields
    | `OverridingOverriding
        of overriding_overriding_fields
    | `OverridingUnspecified
        of overriding_unspecified_fields
    | `Params
        of params_fields
    | `ParenAbstractStateDecl
        of paren_abstract_state_decl_fields
    | `PpElseDirective
        of pp_else_directive_fields
    | `PpElsifDirective
        of pp_elsif_directive_fields
    | `PpEndIfDirective
        of pp_end_if_directive_fields
    | `PpIfDirective
        of pp_if_directive_fields
    | `PpThenKw
        of pp_then_kw_fields
    | `PragmaNode
        of pragma_node_fields
    | `PrivateAbsent
        of private_absent_fields
    | `PrivatePresent
        of private_present_fields
    | `ProtectedDef
        of protected_def_fields
    | `ProtectedAbsent
        of protected_absent_fields
    | `ProtectedPresent
        of protected_present_fields
    | `QuantifierAll
        of quantifier_all_fields
    | `QuantifierSome
        of quantifier_some_fields
    | `RangeSpec
        of range_spec_fields
    | `RenamingClause
        of renaming_clause_fields
    | `SyntheticRenamingClause
        of synthetic_renaming_clause_fields
    | `ReverseAbsent
        of reverse_absent_fields
    | `ReversePresent
        of reverse_present_fields
    | `SelectWhenPart
        of select_when_part_fields
    | `AcceptStmt
        of accept_stmt_fields
    | `AcceptStmtWithStmts
        of accept_stmt_with_stmts_fields
    | `ForLoopStmt
        of for_loop_stmt_fields
    | `LoopStmt
        of loop_stmt_fields
    | `WhileLoopStmt
        of while_loop_stmt_fields
    | `BeginBlock
        of begin_block_fields
    | `DeclBlock
        of decl_block_fields
    | `CaseStmt
        of case_stmt_fields
    | `ExtendedReturnStmt
        of extended_return_stmt_fields
    | `IfStmt
        of if_stmt_fields
    | `NamedStmt
        of named_stmt_fields
    | `SelectStmt
        of select_stmt_fields
    | `ErrorStmt
        of error_stmt_fields
    | `AbortStmt
        of abort_stmt_fields
    | `AssignStmt
        of assign_stmt_fields
    | `CallStmt
        of call_stmt_fields
    | `DelayStmt
        of delay_stmt_fields
    | `ExitStmt
        of exit_stmt_fields
    | `GotoStmt
        of goto_stmt_fields
    | `Label
        of label_fields
    | `NullStmt
        of null_stmt_fields
    | `RaiseStmt
        of raise_stmt_fields
    | `RequeueStmt
        of requeue_stmt_fields
    | `ReturnStmt
        of return_stmt_fields
    | `TerminateAlternative
        of terminate_alternative_fields
    | `SubpKindFunction
        of subp_kind_function_fields
    | `SubpKindProcedure
        of subp_kind_procedure_fields
    | `Subunit
        of subunit_fields
    | `SynchronizedAbsent
        of synchronized_absent_fields
    | `SynchronizedPresent
        of synchronized_present_fields
    | `TaggedAbsent
        of tagged_absent_fields
    | `TaggedPresent
        of tagged_present_fields
    | `TaskDef
        of task_def_fields
    | `TypeAttributesRepository
        of type_attributes_repository_fields
    | `AccessToSubpDef
        of access_to_subp_def_fields
    | `AnonymousTypeAccessDef
        of anonymous_type_access_def_fields
    | `TypeAccessDef
        of type_access_def_fields
    | `ArrayTypeDef
        of array_type_def_fields
    | `DerivedTypeDef
        of derived_type_def_fields
    | `EnumTypeDef
        of enum_type_def_fields
    | `FormalDiscreteTypeDef
        of formal_discrete_type_def_fields
    | `InterfaceTypeDef
        of interface_type_def_fields
    | `ModIntTypeDef
        of mod_int_type_def_fields
    | `PrivateTypeDef
        of private_type_def_fields
    | `DecimalFixedPointDef
        of decimal_fixed_point_def_fields
    | `FloatingPointDef
        of floating_point_def_fields
    | `OrdinaryFixedPointDef
        of ordinary_fixed_point_def_fields
    | `RecordTypeDef
        of record_type_def_fields
    | `SignedIntTypeDef
        of signed_int_type_def_fields
    | `AnonymousType
        of anonymous_type_fields
    | `EnumLitSynthTypeExpr
        of enum_lit_synth_type_expr_fields
    | `SubtypeIndication
        of subtype_indication_fields
    | `ConstrainedSubtypeIndication
        of constrained_subtype_indication_fields
    | `DiscreteSubtypeIndication
        of discrete_subtype_indication_fields
    | `SyntheticTypeExpr
        of synthetic_type_expr_fields
    | `UnconstrainedArrayIndex
        of unconstrained_array_index_fields
    | `UntilAbsent
        of until_absent_fields
    | `UntilPresent
        of until_present_fields
    | `UsePackageClause
        of use_package_clause_fields
    | `UseTypeClause
        of use_type_clause_fields
    | `ValueSequence
        of value_sequence_fields
    | `Variant
        of variant_fields
    | `VariantPart
        of variant_part_fields
    | `WithClause
        of with_clause_fields
    | `WithPrivateAbsent
        of with_private_absent_fields
    | `WithPrivatePresent
        of with_private_present_fields
    ]

  
   
  and abort_node =
    [
    | `AbortAbsent
        of abort_absent_fields
    | `AbortPresent
        of abort_present_fields
    ]

  
   
  and abort_absent =
    [
    | `AbortAbsent
        of abort_absent_fields
    ]
  and abort_absent_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and abort_present =
    [
    | `AbortPresent
        of abort_present_fields
    ]
  and abort_present_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and abstract_node =
    [
    | `AbstractAbsent
        of abstract_absent_fields
    | `AbstractPresent
        of abstract_present_fields
    ]

  
   
  and abstract_absent =
    [
    | `AbstractAbsent
        of abstract_absent_fields
    ]
  and abstract_absent_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and abstract_present =
    [
    | `AbstractPresent
        of abstract_present_fields
    ]
  and abstract_present_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  (**
    * ada_node_list
    * aspect_assoc_list
    * base_assoc_list
    * basic_assoc_list
    * basic_decl_list
    * case_expr_alternative_list
    * case_stmt_alternative_list
    * compilation_unit_list
    * concat_operand_list
    * contract_case_assoc_list
    * defining_name_list
    * discriminant_spec_list
    * elsif_expr_part_list
    * elsif_stmt_part_list
    * enum_literal_decl_list
    * expr_list
    * identifier_list
    * name_list
    * param_spec_list
    * pragma_node_list
    * select_when_part_list
    * unconstrained_array_index_list
    * variant_list
    *)
  and ada_list =
    [
    | `AdaNodeList
        of ada_node_list_fields
    | `AbstractStateDeclList
        of abstract_state_decl_list_fields
    | `AlternativesList
        of alternatives_list_fields
    | `ConstraintList
        of constraint_list_fields
    | `DeclList
        of decl_list_fields
    | `StmtList
        of stmt_list_fields
    | `AspectAssocList
        of aspect_assoc_list_fields
    | `BaseAssocList
        of base_assoc_list_fields
    | `AssocList
        of assoc_list_fields
    | `BasicDeclList
        of basic_decl_list_fields
    | `CaseExprAlternativeList
        of case_expr_alternative_list_fields
    | `CaseStmtAlternativeList
        of case_stmt_alternative_list_fields
    | `CompilationUnitList
        of compilation_unit_list_fields
    | `ConcatOperandList
        of concat_operand_list_fields
    | `ContractCaseAssocList
        of contract_case_assoc_list_fields
    | `DefiningNameList
        of defining_name_list_fields
    | `DiscriminantSpecList
        of discriminant_spec_list_fields
    | `ElsifExprPartList
        of elsif_expr_part_list_fields
    | `ElsifStmtPartList
        of elsif_stmt_part_list_fields
    | `EnumLiteralDeclList
        of enum_literal_decl_list_fields
    | `ExprAlternativesList
        of expr_alternatives_list_fields
    | `DiscriminantChoiceList
        of discriminant_choice_list_fields
    | `NameList
        of name_list_fields
    | `ParentList
        of parent_list_fields
    | `ParamSpecList
        of param_spec_list_fields
    | `PragmaNodeList
        of pragma_node_list_fields
    | `SelectWhenPartList
        of select_when_part_list_fields
    | `UnconstrainedArrayIndexList
        of unconstrained_array_index_list_fields
    | `VariantList
        of variant_list_fields
    ]

  
   
  and ada_node_list =
    [
    | `AdaNodeList
        of ada_node_list_fields
    | `AbstractStateDeclList
        of abstract_state_decl_list_fields
    | `AlternativesList
        of alternatives_list_fields
    | `ConstraintList
        of constraint_list_fields
    | `DeclList
        of decl_list_fields
    | `StmtList
        of stmt_list_fields
    ]
  and ada_node_list_fields = 
  {
    list : ada_node list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and abstract_state_decl_list =
    [
    | `AbstractStateDeclList
        of abstract_state_decl_list_fields
    ]
  and abstract_state_decl_list_fields = 
  {
    list : ada_node list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and alternatives_list =
    [
    | `AlternativesList
        of alternatives_list_fields
    ]
  and alternatives_list_fields = 
  {
    list : ada_node list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and constraint_list =
    [
    | `ConstraintList
        of constraint_list_fields
    ]
  and constraint_list_fields = 
  {
    list : ada_node list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and decl_list =
    [
    | `DeclList
        of decl_list_fields
    ]
  and decl_list_fields = 
  {
    list : ada_node list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and stmt_list =
    [
    | `StmtList
        of stmt_list_fields
    ]
  and stmt_list_fields = 
  {
    list : ada_node list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and aspect_assoc_list =
    [
    | `AspectAssocList
        of aspect_assoc_list_fields
    ]
  and aspect_assoc_list_fields = 
  {
    list : aspect_assoc list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and base_assoc_list =
    [
    | `BaseAssocList
        of base_assoc_list_fields
    ]
  and base_assoc_list_fields = 
  {
    list : base_assoc list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and basic_assoc_list =
    [
    | `AssocList
        of assoc_list_fields
    ]

  
   
  and assoc_list =
    [
    | `AssocList
        of assoc_list_fields
    ]
  and assoc_list_fields = 
  {
    list : basic_assoc list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and basic_decl_list =
    [
    | `BasicDeclList
        of basic_decl_list_fields
    ]
  and basic_decl_list_fields = 
  {
    list : basic_decl list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and case_expr_alternative_list =
    [
    | `CaseExprAlternativeList
        of case_expr_alternative_list_fields
    ]
  and case_expr_alternative_list_fields = 
  {
    list : case_expr_alternative list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and case_stmt_alternative_list =
    [
    | `CaseStmtAlternativeList
        of case_stmt_alternative_list_fields
    ]
  and case_stmt_alternative_list_fields = 
  {
    list : case_stmt_alternative list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and compilation_unit_list =
    [
    | `CompilationUnitList
        of compilation_unit_list_fields
    ]
  and compilation_unit_list_fields = 
  {
    list : compilation_unit list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and concat_operand_list =
    [
    | `ConcatOperandList
        of concat_operand_list_fields
    ]
  and concat_operand_list_fields = 
  {
    list : concat_operand list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and contract_case_assoc_list =
    [
    | `ContractCaseAssocList
        of contract_case_assoc_list_fields
    ]
  and contract_case_assoc_list_fields = 
  {
    list : contract_case_assoc list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and defining_name_list =
    [
    | `DefiningNameList
        of defining_name_list_fields
    ]
  and defining_name_list_fields = 
  {
    list : defining_name list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and discriminant_spec_list =
    [
    | `DiscriminantSpecList
        of discriminant_spec_list_fields
    ]
  and discriminant_spec_list_fields = 
  {
    list : discriminant_spec list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and elsif_expr_part_list =
    [
    | `ElsifExprPartList
        of elsif_expr_part_list_fields
    ]
  and elsif_expr_part_list_fields = 
  {
    list : elsif_expr_part list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and elsif_stmt_part_list =
    [
    | `ElsifStmtPartList
        of elsif_stmt_part_list_fields
    ]
  and elsif_stmt_part_list_fields = 
  {
    list : elsif_stmt_part list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and enum_literal_decl_list =
    [
    | `EnumLiteralDeclList
        of enum_literal_decl_list_fields
    ]
  and enum_literal_decl_list_fields = 
  {
    list : enum_literal_decl list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and expr_list =
    [
    | `ExprAlternativesList
        of expr_alternatives_list_fields
    ]

  
   
  and expr_alternatives_list =
    [
    | `ExprAlternativesList
        of expr_alternatives_list_fields
    ]
  and expr_alternatives_list_fields = 
  {
    list : expr list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and identifier_list =
    [
    | `DiscriminantChoiceList
        of discriminant_choice_list_fields
    ]

  
   
  and discriminant_choice_list =
    [
    | `DiscriminantChoiceList
        of discriminant_choice_list_fields
    ]
  and discriminant_choice_list_fields = 
  {
    list : identifier list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and name_list =
    [
    | `NameList
        of name_list_fields
    | `ParentList
        of parent_list_fields
    ]
  and name_list_fields = 
  {
    list : name list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and parent_list =
    [
    | `ParentList
        of parent_list_fields
    ]
  and parent_list_fields = 
  {
    list : name list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and param_spec_list =
    [
    | `ParamSpecList
        of param_spec_list_fields
    ]
  and param_spec_list_fields = 
  {
    list : param_spec list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and pragma_node_list =
    [
    | `PragmaNodeList
        of pragma_node_list_fields
    ]
  and pragma_node_list_fields = 
  {
    list : pragma_node list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and select_when_part_list =
    [
    | `SelectWhenPartList
        of select_when_part_list_fields
    ]
  and select_when_part_list_fields = 
  {
    list : select_when_part list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and unconstrained_array_index_list =
    [
    | `UnconstrainedArrayIndexList
        of unconstrained_array_index_list_fields
    ]
  and unconstrained_array_index_list_fields = 
  {
    list : unconstrained_array_index list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and variant_list =
    [
    | `VariantList
        of variant_list_fields
    ]
  and variant_list_fields = 
  {
    list : variant list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and aliased_node =
    [
    | `AliasedAbsent
        of aliased_absent_fields
    | `AliasedPresent
        of aliased_present_fields
    ]

  
   
  and aliased_absent =
    [
    | `AliasedAbsent
        of aliased_absent_fields
    ]
  and aliased_absent_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and aliased_present =
    [
    | `AliasedPresent
        of aliased_present_fields
    ]
  and aliased_present_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and all_node =
    [
    | `AllAbsent
        of all_absent_fields
    | `AllPresent
        of all_present_fields
    ]

  
   
  and all_absent =
    [
    | `AllAbsent
        of all_absent_fields
    ]
  and all_absent_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and all_present =
    [
    | `AllPresent
        of all_present_fields
    ]
  and all_present_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and array_indices =
    [
    | `ConstrainedArrayIndices
        of constrained_array_indices_fields
    | `UnconstrainedArrayIndices
        of unconstrained_array_indices_fields
    ]

  
   
  and constrained_array_indices =
    [
    | `ConstrainedArrayIndices
        of constrained_array_indices_fields
    ]
  and constrained_array_indices_fields = 
  {
         
    f_list: constraint_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and unconstrained_array_indices =
    [
    | `UnconstrainedArrayIndices
        of unconstrained_array_indices_fields
    ]
  and unconstrained_array_indices_fields = 
  {
         
    f_types: unconstrained_array_index_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and aspect_assoc =
    [
    | `AspectAssoc
        of aspect_assoc_fields
    ]
  and aspect_assoc_fields = 
  {
         
    f_id: [
      | `AttributeRef
          of attribute_ref_fields
      | `CallExpr
          of call_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `QualExpr
          of qual_expr_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
         
    f_expr: [
      | `AbstractStateDeclExpr
          of abstract_state_decl_expr_fields
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `ContractCases
          of contract_cases_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and aspect_clause =
    [
    | `AtClause
        of at_clause_fields
    | `AttributeDefClause
        of attribute_def_clause_fields
    | `EnumRepClause
        of enum_rep_clause_fields
    | `RecordRepClause
        of record_rep_clause_fields
    ]

  
   
  and at_clause =
    [
    | `AtClause
        of at_clause_fields
    ]
  and at_clause_fields = 
  {
         
    f_name: [
      | `CharLiteral
          of char_literal_fields
      | `Identifier
          of identifier_fields
      | `StringLiteral
          of string_literal_fields
    ]
    Lazy.t;
         
    f_expr: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and attribute_def_clause =
    [
    | `AttributeDefClause
        of attribute_def_clause_fields
    ]
  and attribute_def_clause_fields = 
  {
         
    f_attribute_expr: [
      | `AttributeRef
          of attribute_ref_fields
      | `CallExpr
          of call_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `QualExpr
          of qual_expr_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
         
    f_expr: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and enum_rep_clause =
    [
    | `EnumRepClause
        of enum_rep_clause_fields
    ]
  and enum_rep_clause_fields = 
  {
         
    f_type_name: [
      | `CharLiteral
          of char_literal_fields
      | `DottedName
          of dotted_name_fields
      | `Identifier
          of identifier_fields
      | `StringLiteral
          of string_literal_fields
    ]
    Lazy.t;
         
    f_aggregate: base_aggregate
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and record_rep_clause =
    [
    | `RecordRepClause
        of record_rep_clause_fields
    ]
  and record_rep_clause_fields = 
  {
         
    f_name: [
      | `CharLiteral
          of char_literal_fields
      | `DottedName
          of dotted_name_fields
      | `Identifier
          of identifier_fields
      | `StringLiteral
          of string_literal_fields
    ]
    Lazy.t;
         
    f_at_expr: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    option
    Lazy.t;
         
    f_components: ada_node_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and aspect_spec =
    [
    | `AspectSpec
        of aspect_spec_fields
    ]
  and aspect_spec_fields = 
  {
         
    f_aspect_assocs: aspect_assoc_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and base_assoc =
    [
    | `ContractCaseAssoc
        of contract_case_assoc_fields
    | `PragmaArgumentAssoc
        of pragma_argument_assoc_fields
    ]

  
   
  and contract_case_assoc =
    [
    | `ContractCaseAssoc
        of contract_case_assoc_fields
    ]
  and contract_case_assoc_fields = 
  {
         
    f_guard: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `OthersDesignator
          of others_designator_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
         
    f_consequence: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and pragma_argument_assoc =
    [
    | `PragmaArgumentAssoc
        of pragma_argument_assoc_fields
    ]
  and pragma_argument_assoc_fields = 
  {
         
    f_name: [
      | `AttributeRef
          of attribute_ref_fields
      | `Identifier
          of identifier_fields
    ]
    option
    Lazy.t;
         
    f_expr: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  (**
    * base_subp_spec
    * component_list
    * discriminant_part
    * entry_completion_formal_params
    * generic_formal_part
    *)
  and base_formal_param_holder =
    [
    | `EntrySpec
        of entry_spec_fields
    | `EnumSubpSpec
        of enum_subp_spec_fields
    | `SubpSpec
        of subp_spec_fields
    | `SyntheticBinarySpec
        of synthetic_binary_spec_fields
    | `SyntheticUnarySpec
        of synthetic_unary_spec_fields
    | `ComponentList
        of component_list_fields
    | `KnownDiscriminantPart
        of known_discriminant_part_fields
    | `UnknownDiscriminantPart
        of unknown_discriminant_part_fields
    | `EntryCompletionFormalParams
        of entry_completion_formal_params_fields
    | `GenericFormalPart
        of generic_formal_part_fields
    ]

  
   
  and base_subp_spec =
    [
    | `EntrySpec
        of entry_spec_fields
    | `EnumSubpSpec
        of enum_subp_spec_fields
    | `SubpSpec
        of subp_spec_fields
    | `SyntheticBinarySpec
        of synthetic_binary_spec_fields
    | `SyntheticUnarySpec
        of synthetic_unary_spec_fields
    ]

  
   
  and entry_spec =
    [
    | `EntrySpec
        of entry_spec_fields
    ]
  and entry_spec_fields = 
  {
         
    f_entry_name: defining_name
    Lazy.t;
         
    f_family_type: [
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `CallExpr
          of call_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConstrainedSubtypeIndication
          of constrained_subtype_indication_fields
      | `DiscreteSubtypeIndication
          of discrete_subtype_indication_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `QualExpr
          of qual_expr_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `SubtypeIndication
          of subtype_indication_fields
      | `TargetName
          of target_name_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    option
    Lazy.t;
         
    f_entry_params: params
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and enum_subp_spec =
    [
    | `EnumSubpSpec
        of enum_subp_spec_fields
    ]
  and enum_subp_spec_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and subp_spec =
    [
    | `SubpSpec
        of subp_spec_fields
    ]
  and subp_spec_fields = 
  {
         
    f_subp_kind: subp_kind
    Lazy.t;
         
    f_subp_name: defining_name
    option
    Lazy.t;
         
    f_subp_params: params
    option
    Lazy.t;
         
    f_subp_returns: [
      | `AnonymousType
          of anonymous_type_fields
      | `ConstrainedSubtypeIndication
          of constrained_subtype_indication_fields
      | `DiscreteSubtypeIndication
          of discrete_subtype_indication_fields
      | `SubtypeIndication
          of subtype_indication_fields
    ]
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and synthetic_binary_spec =
    [
    | `SyntheticBinarySpec
        of synthetic_binary_spec_fields
    ]
  and synthetic_binary_spec_fields = 
  {
         
    f_left_param: synthetic_formal_param_decl
    Lazy.t;
         
    f_right_param: synthetic_formal_param_decl
    Lazy.t;
         
    f_return_type_expr: type_expr
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and synthetic_unary_spec =
    [
    | `SyntheticUnarySpec
        of synthetic_unary_spec_fields
    ]
  and synthetic_unary_spec_fields = 
  {
         
    f_right_param: synthetic_formal_param_decl
    Lazy.t;
         
    f_return_type_expr: synthetic_type_expr
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and component_list =
    [
    | `ComponentList
        of component_list_fields
    ]
  and component_list_fields = 
  {
         
    f_components: ada_node_list
    Lazy.t;
         
    f_variant_part: variant_part
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and discriminant_part =
    [
    | `KnownDiscriminantPart
        of known_discriminant_part_fields
    | `UnknownDiscriminantPart
        of unknown_discriminant_part_fields
    ]

  
   
  and known_discriminant_part =
    [
    | `KnownDiscriminantPart
        of known_discriminant_part_fields
    ]
  and known_discriminant_part_fields = 
  {
         
    f_discr_specs: discriminant_spec_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and unknown_discriminant_part =
    [
    | `UnknownDiscriminantPart
        of unknown_discriminant_part_fields
    ]
  and unknown_discriminant_part_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and entry_completion_formal_params =
    [
    | `EntryCompletionFormalParams
        of entry_completion_formal_params_fields
    ]
  and entry_completion_formal_params_fields = 
  {
         
    f_params: params
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and generic_formal_part =
    [
    | `GenericFormalPart
        of generic_formal_part_fields
    ]
  and generic_formal_part_fields = 
  {
         
    f_decls: ada_node_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and base_record_def =
    [
    | `NullRecordDef
        of null_record_def_fields
    | `RecordDef
        of record_def_fields
    ]

  
   
  and null_record_def =
    [
    | `NullRecordDef
        of null_record_def_fields
    ]
  and null_record_def_fields = 
  {
         
    f_components: component_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and record_def =
    [
    | `RecordDef
        of record_def_fields
    ]
  and record_def_fields = 
  {
         
    f_components: component_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  (**
    * aggregate_assoc
    * composite_constraint_assoc
    * iterated_assoc
    * param_assoc
    *)
  and basic_assoc =
    [
    | `AggregateAssoc
        of aggregate_assoc_fields
    | `MultiDimArrayAssoc
        of multi_dim_array_assoc_fields
    | `CompositeConstraintAssoc
        of composite_constraint_assoc_fields
    | `IteratedAssoc
        of iterated_assoc_fields
    | `ParamAssoc
        of param_assoc_fields
    ]

  
   
  and aggregate_assoc =
    [
    | `AggregateAssoc
        of aggregate_assoc_fields
    | `MultiDimArrayAssoc
        of multi_dim_array_assoc_fields
    ]
  and aggregate_assoc_fields = 
  {
         
    f_designators: alternatives_list
    Lazy.t;
         
    f_r_expr: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BoxExpr
          of box_expr_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and multi_dim_array_assoc =
    [
    | `MultiDimArrayAssoc
        of multi_dim_array_assoc_fields
    ]
  and multi_dim_array_assoc_fields = 
  {
         
    f_designators: alternatives_list
    Lazy.t;
         
    f_r_expr: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BoxExpr
          of box_expr_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and composite_constraint_assoc =
    [
    | `CompositeConstraintAssoc
        of composite_constraint_assoc_fields
    ]
  and composite_constraint_assoc_fields = 
  {
         
    f_ids: identifier_list
    Lazy.t;
         
    f_constraint_expr: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DiscreteSubtypeIndication
          of discrete_subtype_indication_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and iterated_assoc =
    [
    | `IteratedAssoc
        of iterated_assoc_fields
    ]
  and iterated_assoc_fields = 
  {
         
    f_spec: for_loop_spec
    Lazy.t;
         
    f_r_expr: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and param_assoc =
    [
    | `ParamAssoc
        of param_assoc_fields
    ]
  and param_assoc_fields = 
  {
         
    f_designator: [
      | `Identifier
          of identifier_fields
      | `OthersDesignator
          of others_designator_fields
      | `StringLiteral
          of string_literal_fields
    ]
    option
    Lazy.t;
         
    f_r_expr: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BoxExpr
          of box_expr_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  (**
    * abstract_state_decl
    * anonymous_expr_decl
    * base_formal_param_decl
    * base_package_decl
    * base_type_decl
    * basic_subp_decl
    * body_node
    * entry_index_spec
    * error_decl
    * exception_decl
    * exception_handler
    * for_loop_var_decl
    * generic_decl
    * generic_instantiation
    * generic_renaming_decl
    * label_decl
    * named_stmt_decl
    * number_decl
    * object_decl
    * package_renaming_decl
    * single_protected_decl
    * single_task_decl
    *)
  and basic_decl =
    [
    | `AbstractStateDecl
        of abstract_state_decl_fields
    | `AnonymousExprDecl
        of anonymous_expr_decl_fields
    | `ComponentDecl
        of component_decl_fields
    | `DiscriminantSpec
        of discriminant_spec_fields
    | `GenericFormalObjDecl
        of generic_formal_obj_decl_fields
    | `GenericFormalPackage
        of generic_formal_package_fields
    | `GenericFormalSubpDecl
        of generic_formal_subp_decl_fields
    | `GenericFormalTypeDecl
        of generic_formal_type_decl_fields
    | `ParamSpec
        of param_spec_fields
    | `SyntheticFormalParamDecl
        of synthetic_formal_param_decl_fields
    | `GenericPackageInternal
        of generic_package_internal_fields
    | `PackageDecl
        of package_decl_fields
    | `DiscreteBaseSubtypeDecl
        of discrete_base_subtype_decl_fields
    | `SubtypeDecl
        of subtype_decl_fields
    | `ClasswideTypeDecl
        of classwide_type_decl_fields
    | `IncompleteTypeDecl
        of incomplete_type_decl_fields
    | `IncompleteFormalTypeDecl
        of incomplete_formal_type_decl_fields
    | `IncompleteTaggedTypeDecl
        of incomplete_tagged_type_decl_fields
    | `ProtectedTypeDecl
        of protected_type_decl_fields
    | `TaskTypeDecl
        of task_type_decl_fields
    | `SingleTaskTypeDecl
        of single_task_type_decl_fields
    | `AnonymousTypeDecl
        of anonymous_type_decl_fields
    | `SynthAnonymousTypeDecl
        of synth_anonymous_type_decl_fields
    | `ConcreteTypeDecl
        of concrete_type_decl_fields
    | `FormalTypeDecl
        of formal_type_decl_fields
    | `AbstractSubpDecl
        of abstract_subp_decl_fields
    | `AbstractFormalSubpDecl
        of abstract_formal_subp_decl_fields
    | `ConcreteFormalSubpDecl
        of concrete_formal_subp_decl_fields
    | `SubpDecl
        of subp_decl_fields
    | `EntryDecl
        of entry_decl_fields
    | `EnumLiteralDecl
        of enum_literal_decl_fields
    | `SyntheticCharEnumLit
        of synthetic_char_enum_lit_fields
    | `GenericSubpInternal
        of generic_subp_internal_fields
    | `SyntheticSubpDecl
        of synthetic_subp_decl_fields
    | `ExprFunction
        of expr_function_fields
    | `NullSubpDecl
        of null_subp_decl_fields
    | `SubpBody
        of subp_body_fields
    | `SubpRenamingDecl
        of subp_renaming_decl_fields
    | `PackageBodyStub
        of package_body_stub_fields
    | `ProtectedBodyStub
        of protected_body_stub_fields
    | `SubpBodyStub
        of subp_body_stub_fields
    | `TaskBodyStub
        of task_body_stub_fields
    | `EntryBody
        of entry_body_fields
    | `PackageBody
        of package_body_fields
    | `ProtectedBody
        of protected_body_fields
    | `TaskBody
        of task_body_fields
    | `EntryIndexSpec
        of entry_index_spec_fields
    | `ErrorDecl
        of error_decl_fields
    | `ExceptionDecl
        of exception_decl_fields
    | `ExceptionHandler
        of exception_handler_fields
    | `ForLoopVarDecl
        of for_loop_var_decl_fields
    | `GenericPackageDecl
        of generic_package_decl_fields
    | `GenericSubpDecl
        of generic_subp_decl_fields
    | `GenericPackageInstantiation
        of generic_package_instantiation_fields
    | `GenericSubpInstantiation
        of generic_subp_instantiation_fields
    | `GenericPackageRenamingDecl
        of generic_package_renaming_decl_fields
    | `GenericSubpRenamingDecl
        of generic_subp_renaming_decl_fields
    | `LabelDecl
        of label_decl_fields
    | `NamedStmtDecl
        of named_stmt_decl_fields
    | `NumberDecl
        of number_decl_fields
    | `ObjectDecl
        of object_decl_fields
    | `ExtendedReturnStmtObjectDecl
        of extended_return_stmt_object_decl_fields
    | `NoTypeObjectRenamingDecl
        of no_type_object_renaming_decl_fields
    | `PackageRenamingDecl
        of package_renaming_decl_fields
    | `SingleProtectedDecl
        of single_protected_decl_fields
    | `SingleTaskDecl
        of single_task_decl_fields
    ]

  
   
  and abstract_state_decl =
    [
    | `AbstractStateDecl
        of abstract_state_decl_fields
    ]
  and abstract_state_decl_fields = 
  {
         
    f_name: defining_name
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and anonymous_expr_decl =
    [
    | `AnonymousExprDecl
        of anonymous_expr_decl_fields
    ]
  and anonymous_expr_decl_fields = 
  {
         
    f_expr: expr
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  (**
    * component_decl
    * discriminant_spec
    * generic_formal
    * param_spec
    * synthetic_formal_param_decl
    *)
  and base_formal_param_decl =
    [
    | `ComponentDecl
        of component_decl_fields
    | `DiscriminantSpec
        of discriminant_spec_fields
    | `GenericFormalObjDecl
        of generic_formal_obj_decl_fields
    | `GenericFormalPackage
        of generic_formal_package_fields
    | `GenericFormalSubpDecl
        of generic_formal_subp_decl_fields
    | `GenericFormalTypeDecl
        of generic_formal_type_decl_fields
    | `ParamSpec
        of param_spec_fields
    | `SyntheticFormalParamDecl
        of synthetic_formal_param_decl_fields
    ]

  
   
  and component_decl =
    [
    | `ComponentDecl
        of component_decl_fields
    ]
  and component_decl_fields = 
  {
         
    f_ids: defining_name_list
    Lazy.t;
         
    f_component_def: component_def
    Lazy.t;
         
    f_default_expr: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and discriminant_spec =
    [
    | `DiscriminantSpec
        of discriminant_spec_fields
    ]
  and discriminant_spec_fields = 
  {
         
    f_ids: defining_name_list
    Lazy.t;
         
    f_type_expr: [
      | `AnonymousType
          of anonymous_type_fields
      | `ConstrainedSubtypeIndication
          of constrained_subtype_indication_fields
      | `DiscreteSubtypeIndication
          of discrete_subtype_indication_fields
      | `SubtypeIndication
          of subtype_indication_fields
    ]
    Lazy.t;
         
    f_default_expr: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and generic_formal =
    [
    | `GenericFormalObjDecl
        of generic_formal_obj_decl_fields
    | `GenericFormalPackage
        of generic_formal_package_fields
    | `GenericFormalSubpDecl
        of generic_formal_subp_decl_fields
    | `GenericFormalTypeDecl
        of generic_formal_type_decl_fields
    ]

  
   
  and generic_formal_obj_decl =
    [
    | `GenericFormalObjDecl
        of generic_formal_obj_decl_fields
    ]
  and generic_formal_obj_decl_fields = 
  {
         
    f_decl: [
      | `AbstractFormalSubpDecl
          of abstract_formal_subp_decl_fields
      | `ConcreteFormalSubpDecl
          of concrete_formal_subp_decl_fields
      | `ExtendedReturnStmtObjectDecl
          of extended_return_stmt_object_decl_fields
      | `FormalTypeDecl
          of formal_type_decl_fields
      | `GenericPackageInstantiation
          of generic_package_instantiation_fields
      | `GenericSubpInstantiation
          of generic_subp_instantiation_fields
      | `IncompleteFormalTypeDecl
          of incomplete_formal_type_decl_fields
      | `NoTypeObjectRenamingDecl
          of no_type_object_renaming_decl_fields
      | `NumberDecl
          of number_decl_fields
      | `ObjectDecl
          of object_decl_fields
      | `SingleProtectedDecl
          of single_protected_decl_fields
      | `SingleTaskDecl
          of single_task_decl_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and generic_formal_package =
    [
    | `GenericFormalPackage
        of generic_formal_package_fields
    ]
  and generic_formal_package_fields = 
  {
         
    f_decl: [
      | `AbstractFormalSubpDecl
          of abstract_formal_subp_decl_fields
      | `ConcreteFormalSubpDecl
          of concrete_formal_subp_decl_fields
      | `ExtendedReturnStmtObjectDecl
          of extended_return_stmt_object_decl_fields
      | `FormalTypeDecl
          of formal_type_decl_fields
      | `GenericPackageInstantiation
          of generic_package_instantiation_fields
      | `GenericSubpInstantiation
          of generic_subp_instantiation_fields
      | `IncompleteFormalTypeDecl
          of incomplete_formal_type_decl_fields
      | `NoTypeObjectRenamingDecl
          of no_type_object_renaming_decl_fields
      | `NumberDecl
          of number_decl_fields
      | `ObjectDecl
          of object_decl_fields
      | `SingleProtectedDecl
          of single_protected_decl_fields
      | `SingleTaskDecl
          of single_task_decl_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and generic_formal_subp_decl =
    [
    | `GenericFormalSubpDecl
        of generic_formal_subp_decl_fields
    ]
  and generic_formal_subp_decl_fields = 
  {
         
    f_decl: [
      | `AbstractFormalSubpDecl
          of abstract_formal_subp_decl_fields
      | `ConcreteFormalSubpDecl
          of concrete_formal_subp_decl_fields
      | `ExtendedReturnStmtObjectDecl
          of extended_return_stmt_object_decl_fields
      | `FormalTypeDecl
          of formal_type_decl_fields
      | `GenericPackageInstantiation
          of generic_package_instantiation_fields
      | `GenericSubpInstantiation
          of generic_subp_instantiation_fields
      | `IncompleteFormalTypeDecl
          of incomplete_formal_type_decl_fields
      | `NoTypeObjectRenamingDecl
          of no_type_object_renaming_decl_fields
      | `NumberDecl
          of number_decl_fields
      | `ObjectDecl
          of object_decl_fields
      | `SingleProtectedDecl
          of single_protected_decl_fields
      | `SingleTaskDecl
          of single_task_decl_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and generic_formal_type_decl =
    [
    | `GenericFormalTypeDecl
        of generic_formal_type_decl_fields
    ]
  and generic_formal_type_decl_fields = 
  {
         
    f_decl: [
      | `AbstractFormalSubpDecl
          of abstract_formal_subp_decl_fields
      | `ConcreteFormalSubpDecl
          of concrete_formal_subp_decl_fields
      | `ExtendedReturnStmtObjectDecl
          of extended_return_stmt_object_decl_fields
      | `FormalTypeDecl
          of formal_type_decl_fields
      | `GenericPackageInstantiation
          of generic_package_instantiation_fields
      | `GenericSubpInstantiation
          of generic_subp_instantiation_fields
      | `IncompleteFormalTypeDecl
          of incomplete_formal_type_decl_fields
      | `NoTypeObjectRenamingDecl
          of no_type_object_renaming_decl_fields
      | `NumberDecl
          of number_decl_fields
      | `ObjectDecl
          of object_decl_fields
      | `SingleProtectedDecl
          of single_protected_decl_fields
      | `SingleTaskDecl
          of single_task_decl_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and param_spec =
    [
    | `ParamSpec
        of param_spec_fields
    ]
  and param_spec_fields = 
  {
         
    f_ids: defining_name_list
    Lazy.t;
         
    f_has_aliased: aliased_node
    Lazy.t;
         
    f_mode: mode
    option
    Lazy.t;
         
    f_type_expr: [
      | `AnonymousType
          of anonymous_type_fields
      | `ConstrainedSubtypeIndication
          of constrained_subtype_indication_fields
      | `DiscreteSubtypeIndication
          of discrete_subtype_indication_fields
      | `SubtypeIndication
          of subtype_indication_fields
    ]
    Lazy.t;
         
    f_default_expr: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and synthetic_formal_param_decl =
    [
    | `SyntheticFormalParamDecl
        of synthetic_formal_param_decl_fields
    ]
  and synthetic_formal_param_decl_fields = 
  {
         
    f_param_type: type_expr
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and base_package_decl =
    [
    | `GenericPackageInternal
        of generic_package_internal_fields
    | `PackageDecl
        of package_decl_fields
    ]

  
   
  and generic_package_internal =
    [
    | `GenericPackageInternal
        of generic_package_internal_fields
    ]
  and generic_package_internal_fields = 
  {
         
    f_package_name: defining_name
    Lazy.t;
         
    f_public_part: public_part
    Lazy.t;
         
    f_private_part: private_part
    option
    Lazy.t;
         
    f_end_name: end_name
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and package_decl =
    [
    | `PackageDecl
        of package_decl_fields
    ]
  and package_decl_fields = 
  {
         
    f_package_name: defining_name
    Lazy.t;
         
    f_public_part: public_part
    Lazy.t;
         
    f_private_part: private_part
    option
    Lazy.t;
         
    f_end_name: end_name
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  (**
    * base_subtype_decl
    * classwide_type_decl
    * incomplete_type_decl
    * protected_type_decl
    * task_type_decl
    * type_decl
    *)
  and base_type_decl =
    [
    | `DiscreteBaseSubtypeDecl
        of discrete_base_subtype_decl_fields
    | `SubtypeDecl
        of subtype_decl_fields
    | `ClasswideTypeDecl
        of classwide_type_decl_fields
    | `IncompleteTypeDecl
        of incomplete_type_decl_fields
    | `IncompleteFormalTypeDecl
        of incomplete_formal_type_decl_fields
    | `IncompleteTaggedTypeDecl
        of incomplete_tagged_type_decl_fields
    | `ProtectedTypeDecl
        of protected_type_decl_fields
    | `TaskTypeDecl
        of task_type_decl_fields
    | `SingleTaskTypeDecl
        of single_task_type_decl_fields
    | `AnonymousTypeDecl
        of anonymous_type_decl_fields
    | `SynthAnonymousTypeDecl
        of synth_anonymous_type_decl_fields
    | `ConcreteTypeDecl
        of concrete_type_decl_fields
    | `FormalTypeDecl
        of formal_type_decl_fields
    ]

  
   
  and base_subtype_decl =
    [
    | `DiscreteBaseSubtypeDecl
        of discrete_base_subtype_decl_fields
    | `SubtypeDecl
        of subtype_decl_fields
    ]

  
   
  and discrete_base_subtype_decl =
    [
    | `DiscreteBaseSubtypeDecl
        of discrete_base_subtype_decl_fields
    ]
  and discrete_base_subtype_decl_fields = 
  {
         
    f_name: defining_name
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and subtype_decl =
    [
    | `SubtypeDecl
        of subtype_decl_fields
    ]
  and subtype_decl_fields = 
  {
         
    f_name: defining_name
    option
    Lazy.t;
         
    f_subtype: subtype_indication
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and classwide_type_decl =
    [
    | `ClasswideTypeDecl
        of classwide_type_decl_fields
    ]
  and classwide_type_decl_fields = 
  {
         
    f_name: defining_name
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and incomplete_type_decl =
    [
    | `IncompleteTypeDecl
        of incomplete_type_decl_fields
    | `IncompleteFormalTypeDecl
        of incomplete_formal_type_decl_fields
    | `IncompleteTaggedTypeDecl
        of incomplete_tagged_type_decl_fields
    ]
  and incomplete_type_decl_fields = 
  {
         
    f_name: defining_name
    option
    Lazy.t;
         
    f_discriminants: discriminant_part
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and incomplete_formal_type_decl =
    [
    | `IncompleteFormalTypeDecl
        of incomplete_formal_type_decl_fields
    ]
  and incomplete_formal_type_decl_fields = 
  {
         
    f_name: defining_name
    option
    Lazy.t;
         
    f_discriminants: discriminant_part
    option
    Lazy.t;
         
    f_is_tagged: tagged_node
    option
    Lazy.t;
         
    f_default_type: [
      | `AttributeRef
          of attribute_ref_fields
      | `CallExpr
          of call_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `QualExpr
          of qual_expr_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and incomplete_tagged_type_decl =
    [
    | `IncompleteTaggedTypeDecl
        of incomplete_tagged_type_decl_fields
    ]
  and incomplete_tagged_type_decl_fields = 
  {
         
    f_name: defining_name
    option
    Lazy.t;
         
    f_discriminants: discriminant_part
    option
    Lazy.t;
         
    f_has_abstract: abstract_node
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and protected_type_decl =
    [
    | `ProtectedTypeDecl
        of protected_type_decl_fields
    ]
  and protected_type_decl_fields = 
  {
         
    f_name: defining_name
    option
    Lazy.t;
         
    f_discriminants: discriminant_part
    option
    Lazy.t;
         
    f_interfaces: parent_list
    Lazy.t;
         
    f_definition: protected_def
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and task_type_decl =
    [
    | `TaskTypeDecl
        of task_type_decl_fields
    | `SingleTaskTypeDecl
        of single_task_type_decl_fields
    ]
  and task_type_decl_fields = 
  {
         
    f_name: defining_name
    option
    Lazy.t;
         
    f_discriminants: discriminant_part
    option
    Lazy.t;
         
    f_definition: task_def
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and single_task_type_decl =
    [
    | `SingleTaskTypeDecl
        of single_task_type_decl_fields
    ]
  and single_task_type_decl_fields = 
  {
         
    f_name: defining_name
    option
    Lazy.t;
         
    f_discriminants: discriminant_part
    option
    Lazy.t;
         
    f_definition: task_def
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  (**
    * anonymous_type_decl
    * concrete_type_decl
    * formal_type_decl
    *)
  and type_decl =
    [
    | `AnonymousTypeDecl
        of anonymous_type_decl_fields
    | `SynthAnonymousTypeDecl
        of synth_anonymous_type_decl_fields
    | `ConcreteTypeDecl
        of concrete_type_decl_fields
    | `FormalTypeDecl
        of formal_type_decl_fields
    ]

  
   
  and anonymous_type_decl =
    [
    | `AnonymousTypeDecl
        of anonymous_type_decl_fields
    | `SynthAnonymousTypeDecl
        of synth_anonymous_type_decl_fields
    ]
  and anonymous_type_decl_fields = 
  {
         
    f_name: defining_name
    option
    Lazy.t;
         
    f_discriminants: discriminant_part
    option
    Lazy.t;
         
    f_type_def: type_def
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and synth_anonymous_type_decl =
    [
    | `SynthAnonymousTypeDecl
        of synth_anonymous_type_decl_fields
    ]
  and synth_anonymous_type_decl_fields = 
  {
         
    f_name: defining_name
    option
    Lazy.t;
         
    f_discriminants: discriminant_part
    option
    Lazy.t;
         
    f_type_def: type_def
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and concrete_type_decl =
    [
    | `ConcreteTypeDecl
        of concrete_type_decl_fields
    ]
  and concrete_type_decl_fields = 
  {
         
    f_name: defining_name
    option
    Lazy.t;
         
    f_discriminants: discriminant_part
    option
    Lazy.t;
         
    f_type_def: type_def
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and formal_type_decl =
    [
    | `FormalTypeDecl
        of formal_type_decl_fields
    ]
  and formal_type_decl_fields = 
  {
         
    f_name: defining_name
    option
    Lazy.t;
         
    f_discriminants: discriminant_part
    option
    Lazy.t;
         
    f_type_def: type_def
    Lazy.t;
         
    f_default_type: [
      | `AttributeRef
          of attribute_ref_fields
      | `CallExpr
          of call_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `QualExpr
          of qual_expr_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  (**
    * classic_subp_decl
    * entry_decl
    * enum_literal_decl
    * generic_subp_internal
    * synthetic_subp_decl
    *)
  and basic_subp_decl =
    [
    | `AbstractSubpDecl
        of abstract_subp_decl_fields
    | `AbstractFormalSubpDecl
        of abstract_formal_subp_decl_fields
    | `ConcreteFormalSubpDecl
        of concrete_formal_subp_decl_fields
    | `SubpDecl
        of subp_decl_fields
    | `EntryDecl
        of entry_decl_fields
    | `EnumLiteralDecl
        of enum_literal_decl_fields
    | `SyntheticCharEnumLit
        of synthetic_char_enum_lit_fields
    | `GenericSubpInternal
        of generic_subp_internal_fields
    | `SyntheticSubpDecl
        of synthetic_subp_decl_fields
    ]

  
   
  (**
    * abstract_subp_decl
    * formal_subp_decl
    * subp_decl
    *)
  and classic_subp_decl =
    [
    | `AbstractSubpDecl
        of abstract_subp_decl_fields
    | `AbstractFormalSubpDecl
        of abstract_formal_subp_decl_fields
    | `ConcreteFormalSubpDecl
        of concrete_formal_subp_decl_fields
    | `SubpDecl
        of subp_decl_fields
    ]

  
   
  and abstract_subp_decl =
    [
    | `AbstractSubpDecl
        of abstract_subp_decl_fields
    ]
  and abstract_subp_decl_fields = 
  {
         
    f_overriding: overriding_node
    Lazy.t;
         
    f_subp_spec: subp_spec
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and formal_subp_decl =
    [
    | `AbstractFormalSubpDecl
        of abstract_formal_subp_decl_fields
    | `ConcreteFormalSubpDecl
        of concrete_formal_subp_decl_fields
    ]

  
   
  and abstract_formal_subp_decl =
    [
    | `AbstractFormalSubpDecl
        of abstract_formal_subp_decl_fields
    ]
  and abstract_formal_subp_decl_fields = 
  {
         
    f_overriding: overriding_node
    Lazy.t;
         
    f_subp_spec: subp_spec
    Lazy.t;
         
    f_default_expr: [
      | `AttributeRef
          of attribute_ref_fields
      | `BoxExpr
          of box_expr_fields
      | `CallExpr
          of call_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `NullLiteral
          of null_literal_fields
      | `QualExpr
          of qual_expr_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and concrete_formal_subp_decl =
    [
    | `ConcreteFormalSubpDecl
        of concrete_formal_subp_decl_fields
    ]
  and concrete_formal_subp_decl_fields = 
  {
         
    f_overriding: overriding_node
    Lazy.t;
         
    f_subp_spec: subp_spec
    Lazy.t;
         
    f_default_expr: [
      | `AttributeRef
          of attribute_ref_fields
      | `BoxExpr
          of box_expr_fields
      | `CallExpr
          of call_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `NullLiteral
          of null_literal_fields
      | `QualExpr
          of qual_expr_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and subp_decl =
    [
    | `SubpDecl
        of subp_decl_fields
    ]
  and subp_decl_fields = 
  {
         
    f_overriding: overriding_node
    Lazy.t;
         
    f_subp_spec: subp_spec
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and entry_decl =
    [
    | `EntryDecl
        of entry_decl_fields
    ]
  and entry_decl_fields = 
  {
         
    f_overriding: overriding_node
    Lazy.t;
         
    f_spec: entry_spec
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and enum_literal_decl =
    [
    | `EnumLiteralDecl
        of enum_literal_decl_fields
    | `SyntheticCharEnumLit
        of synthetic_char_enum_lit_fields
    ]
  and enum_literal_decl_fields = 
  {
         
    f_name: defining_name
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and synthetic_char_enum_lit =
    [
    | `SyntheticCharEnumLit
        of synthetic_char_enum_lit_fields
    ]
  and synthetic_char_enum_lit_fields = 
  {
         
    f_name: defining_name
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and generic_subp_internal =
    [
    | `GenericSubpInternal
        of generic_subp_internal_fields
    ]
  and generic_subp_internal_fields = 
  {
         
    f_subp_spec: subp_spec
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and synthetic_subp_decl =
    [
    | `SyntheticSubpDecl
        of synthetic_subp_decl_fields
    ]
  and synthetic_subp_decl_fields = 
  {
         
    f_spec: [
      | `SyntheticBinarySpec
          of synthetic_binary_spec_fields
      | `SyntheticUnarySpec
          of synthetic_unary_spec_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  (**
    * base_subp_body
    * body_stub
    * entry_body
    * package_body
    * protected_body
    * task_body
    *)
  and body_node =
    [
    | `ExprFunction
        of expr_function_fields
    | `NullSubpDecl
        of null_subp_decl_fields
    | `SubpBody
        of subp_body_fields
    | `SubpRenamingDecl
        of subp_renaming_decl_fields
    | `PackageBodyStub
        of package_body_stub_fields
    | `ProtectedBodyStub
        of protected_body_stub_fields
    | `SubpBodyStub
        of subp_body_stub_fields
    | `TaskBodyStub
        of task_body_stub_fields
    | `EntryBody
        of entry_body_fields
    | `PackageBody
        of package_body_fields
    | `ProtectedBody
        of protected_body_fields
    | `TaskBody
        of task_body_fields
    ]

  
   
  and base_subp_body =
    [
    | `ExprFunction
        of expr_function_fields
    | `NullSubpDecl
        of null_subp_decl_fields
    | `SubpBody
        of subp_body_fields
    | `SubpRenamingDecl
        of subp_renaming_decl_fields
    ]

  
   
  and expr_function =
    [
    | `ExprFunction
        of expr_function_fields
    ]
  and expr_function_fields = 
  {
         
    f_overriding: overriding_node
    Lazy.t;
         
    f_subp_spec: subp_spec
    Lazy.t;
         
    f_expr: [
      | `Aggregate
          of aggregate_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and null_subp_decl =
    [
    | `NullSubpDecl
        of null_subp_decl_fields
    ]
  and null_subp_decl_fields = 
  {
         
    f_overriding: overriding_node
    Lazy.t;
         
    f_subp_spec: subp_spec
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and subp_body =
    [
    | `SubpBody
        of subp_body_fields
    ]
  and subp_body_fields = 
  {
         
    f_overriding: overriding_node
    Lazy.t;
         
    f_subp_spec: subp_spec
    Lazy.t;
         
    f_decls: declarative_part
    Lazy.t;
         
    f_stmts: handled_stmts
    Lazy.t;
         
    f_end_name: end_name
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and subp_renaming_decl =
    [
    | `SubpRenamingDecl
        of subp_renaming_decl_fields
    ]
  and subp_renaming_decl_fields = 
  {
         
    f_overriding: overriding_node
    Lazy.t;
         
    f_subp_spec: subp_spec
    Lazy.t;
         
    f_renames: renaming_clause
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and body_stub =
    [
    | `PackageBodyStub
        of package_body_stub_fields
    | `ProtectedBodyStub
        of protected_body_stub_fields
    | `SubpBodyStub
        of subp_body_stub_fields
    | `TaskBodyStub
        of task_body_stub_fields
    ]

  
   
  and package_body_stub =
    [
    | `PackageBodyStub
        of package_body_stub_fields
    ]
  and package_body_stub_fields = 
  {
         
    f_name: defining_name
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and protected_body_stub =
    [
    | `ProtectedBodyStub
        of protected_body_stub_fields
    ]
  and protected_body_stub_fields = 
  {
         
    f_name: defining_name
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and subp_body_stub =
    [
    | `SubpBodyStub
        of subp_body_stub_fields
    ]
  and subp_body_stub_fields = 
  {
         
    f_overriding: overriding_node
    Lazy.t;
         
    f_subp_spec: subp_spec
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and task_body_stub =
    [
    | `TaskBodyStub
        of task_body_stub_fields
    ]
  and task_body_stub_fields = 
  {
         
    f_name: defining_name
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and entry_body =
    [
    | `EntryBody
        of entry_body_fields
    ]
  and entry_body_fields = 
  {
         
    f_entry_name: defining_name
    Lazy.t;
         
    f_index_spec: entry_index_spec
    option
    Lazy.t;
         
    f_params: entry_completion_formal_params
    Lazy.t;
         
    f_barrier: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
         
    f_decls: declarative_part
    Lazy.t;
         
    f_stmts: handled_stmts
    Lazy.t;
         
    f_end_name: end_name
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and package_body =
    [
    | `PackageBody
        of package_body_fields
    ]
  and package_body_fields = 
  {
         
    f_package_name: defining_name
    Lazy.t;
         
    f_decls: declarative_part
    Lazy.t;
         
    f_stmts: handled_stmts
    option
    Lazy.t;
         
    f_end_name: end_name
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and protected_body =
    [
    | `ProtectedBody
        of protected_body_fields
    ]
  and protected_body_fields = 
  {
         
    f_name: defining_name
    Lazy.t;
         
    f_decls: declarative_part
    Lazy.t;
         
    f_end_name: end_name
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and task_body =
    [
    | `TaskBody
        of task_body_fields
    ]
  and task_body_fields = 
  {
         
    f_name: defining_name
    Lazy.t;
         
    f_decls: declarative_part
    Lazy.t;
         
    f_stmts: handled_stmts
    Lazy.t;
         
    f_end_name: end_name
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and entry_index_spec =
    [
    | `EntryIndexSpec
        of entry_index_spec_fields
    ]
  and entry_index_spec_fields = 
  {
         
    f_id: defining_name
    Lazy.t;
         
    f_subtype: [
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `CallExpr
          of call_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConstrainedSubtypeIndication
          of constrained_subtype_indication_fields
      | `DiscreteSubtypeIndication
          of discrete_subtype_indication_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `QualExpr
          of qual_expr_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `SubtypeIndication
          of subtype_indication_fields
      | `TargetName
          of target_name_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and error_decl =
    [
    | `ErrorDecl
        of error_decl_fields
    ]
  and error_decl_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and exception_decl =
    [
    | `ExceptionDecl
        of exception_decl_fields
    ]
  and exception_decl_fields = 
  {
         
    f_ids: defining_name_list
    Lazy.t;
         
    f_renames: renaming_clause
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and exception_handler =
    [
    | `ExceptionHandler
        of exception_handler_fields
    ]
  and exception_handler_fields = 
  {
         
    f_exception_name: defining_name
    option
    Lazy.t;
         
    f_handled_exceptions: alternatives_list
    Lazy.t;
         
    f_stmts: stmt_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and for_loop_var_decl =
    [
    | `ForLoopVarDecl
        of for_loop_var_decl_fields
    ]
  and for_loop_var_decl_fields = 
  {
         
    f_id: defining_name
    Lazy.t;
         
    f_id_type: [
      | `AnonymousType
          of anonymous_type_fields
      | `ConstrainedSubtypeIndication
          of constrained_subtype_indication_fields
      | `DiscreteSubtypeIndication
          of discrete_subtype_indication_fields
      | `SubtypeIndication
          of subtype_indication_fields
    ]
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and generic_decl =
    [
    | `GenericPackageDecl
        of generic_package_decl_fields
    | `GenericSubpDecl
        of generic_subp_decl_fields
    ]

  
   
  and generic_package_decl =
    [
    | `GenericPackageDecl
        of generic_package_decl_fields
    ]
  and generic_package_decl_fields = 
  {
         
    f_formal_part: generic_formal_part
    Lazy.t;
         
    f_package_decl: generic_package_internal
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and generic_subp_decl =
    [
    | `GenericSubpDecl
        of generic_subp_decl_fields
    ]
  and generic_subp_decl_fields = 
  {
         
    f_formal_part: generic_formal_part
    Lazy.t;
         
    f_subp_decl: generic_subp_internal
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and generic_instantiation =
    [
    | `GenericPackageInstantiation
        of generic_package_instantiation_fields
    | `GenericSubpInstantiation
        of generic_subp_instantiation_fields
    ]

  
   
  and generic_package_instantiation =
    [
    | `GenericPackageInstantiation
        of generic_package_instantiation_fields
    ]
  and generic_package_instantiation_fields = 
  {
         
    f_name: defining_name
    Lazy.t;
         
    f_generic_pkg_name: [
      | `CharLiteral
          of char_literal_fields
      | `DottedName
          of dotted_name_fields
      | `Identifier
          of identifier_fields
      | `StringLiteral
          of string_literal_fields
    ]
    Lazy.t;
         
    f_params: basic_assoc_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and generic_subp_instantiation =
    [
    | `GenericSubpInstantiation
        of generic_subp_instantiation_fields
    ]
  and generic_subp_instantiation_fields = 
  {
         
    f_overriding: overriding_node
    Lazy.t;
         
    f_kind: subp_kind
    Lazy.t;
         
    f_subp_name: defining_name
    Lazy.t;
         
    f_generic_subp_name: [
      | `CharLiteral
          of char_literal_fields
      | `DottedName
          of dotted_name_fields
      | `Identifier
          of identifier_fields
      | `StringLiteral
          of string_literal_fields
    ]
    Lazy.t;
         
    f_params: basic_assoc_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and generic_renaming_decl =
    [
    | `GenericPackageRenamingDecl
        of generic_package_renaming_decl_fields
    | `GenericSubpRenamingDecl
        of generic_subp_renaming_decl_fields
    ]

  
   
  and generic_package_renaming_decl =
    [
    | `GenericPackageRenamingDecl
        of generic_package_renaming_decl_fields
    ]
  and generic_package_renaming_decl_fields = 
  {
         
    f_name: defining_name
    Lazy.t;
         
    f_renames: [
      | `CharLiteral
          of char_literal_fields
      | `DottedName
          of dotted_name_fields
      | `Identifier
          of identifier_fields
      | `StringLiteral
          of string_literal_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and generic_subp_renaming_decl =
    [
    | `GenericSubpRenamingDecl
        of generic_subp_renaming_decl_fields
    ]
  and generic_subp_renaming_decl_fields = 
  {
         
    f_kind: subp_kind
    Lazy.t;
         
    f_name: defining_name
    Lazy.t;
         
    f_renames: [
      | `CharLiteral
          of char_literal_fields
      | `DottedName
          of dotted_name_fields
      | `Identifier
          of identifier_fields
      | `StringLiteral
          of string_literal_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and label_decl =
    [
    | `LabelDecl
        of label_decl_fields
    ]
  and label_decl_fields = 
  {
         
    f_name: defining_name
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and named_stmt_decl =
    [
    | `NamedStmtDecl
        of named_stmt_decl_fields
    ]
  and named_stmt_decl_fields = 
  {
         
    f_name: defining_name
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and number_decl =
    [
    | `NumberDecl
        of number_decl_fields
    ]
  and number_decl_fields = 
  {
         
    f_ids: defining_name_list
    Lazy.t;
         
    f_expr: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and object_decl =
    [
    | `ObjectDecl
        of object_decl_fields
    | `ExtendedReturnStmtObjectDecl
        of extended_return_stmt_object_decl_fields
    | `NoTypeObjectRenamingDecl
        of no_type_object_renaming_decl_fields
    ]
  and object_decl_fields = 
  {
         
    f_ids: defining_name_list
    Lazy.t;
         
    f_has_aliased: aliased_node
    option
    Lazy.t;
         
    f_has_constant: constant_node
    option
    Lazy.t;
         
    f_mode: mode
    option
    Lazy.t;
         
    f_type_expr: [
      | `AnonymousType
          of anonymous_type_fields
      | `ConstrainedSubtypeIndication
          of constrained_subtype_indication_fields
      | `DiscreteSubtypeIndication
          of discrete_subtype_indication_fields
      | `SubtypeIndication
          of subtype_indication_fields
    ]
    option
    Lazy.t;
         
    f_default_expr: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    option
    Lazy.t;
         
    f_renaming_clause: renaming_clause
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and extended_return_stmt_object_decl =
    [
    | `ExtendedReturnStmtObjectDecl
        of extended_return_stmt_object_decl_fields
    ]
  and extended_return_stmt_object_decl_fields = 
  {
         
    f_ids: defining_name_list
    Lazy.t;
         
    f_has_aliased: aliased_node
    option
    Lazy.t;
         
    f_has_constant: constant_node
    option
    Lazy.t;
         
    f_mode: mode
    option
    Lazy.t;
         
    f_type_expr: [
      | `AnonymousType
          of anonymous_type_fields
      | `ConstrainedSubtypeIndication
          of constrained_subtype_indication_fields
      | `DiscreteSubtypeIndication
          of discrete_subtype_indication_fields
      | `SubtypeIndication
          of subtype_indication_fields
    ]
    option
    Lazy.t;
         
    f_default_expr: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    option
    Lazy.t;
         
    f_renaming_clause: renaming_clause
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and no_type_object_renaming_decl =
    [
    | `NoTypeObjectRenamingDecl
        of no_type_object_renaming_decl_fields
    ]
  and no_type_object_renaming_decl_fields = 
  {
         
    f_ids: defining_name_list
    Lazy.t;
         
    f_has_aliased: aliased_node
    option
    Lazy.t;
         
    f_has_constant: constant_node
    option
    Lazy.t;
         
    f_mode: mode
    option
    Lazy.t;
         
    f_type_expr: [
      | `AnonymousType
          of anonymous_type_fields
      | `ConstrainedSubtypeIndication
          of constrained_subtype_indication_fields
      | `DiscreteSubtypeIndication
          of discrete_subtype_indication_fields
      | `SubtypeIndication
          of subtype_indication_fields
    ]
    option
    Lazy.t;
         
    f_default_expr: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    option
    Lazy.t;
         
    f_renaming_clause: renaming_clause
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and package_renaming_decl =
    [
    | `PackageRenamingDecl
        of package_renaming_decl_fields
    ]
  and package_renaming_decl_fields = 
  {
         
    f_name: defining_name
    Lazy.t;
         
    f_renames: renaming_clause
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and single_protected_decl =
    [
    | `SingleProtectedDecl
        of single_protected_decl_fields
    ]
  and single_protected_decl_fields = 
  {
         
    f_name: defining_name
    Lazy.t;
         
    f_interfaces: parent_list
    Lazy.t;
         
    f_definition: protected_def
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and single_task_decl =
    [
    | `SingleTaskDecl
        of single_task_decl_fields
    ]
  and single_task_decl_fields = 
  {
         
    f_task_type: single_task_type_decl
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and case_stmt_alternative =
    [
    | `CaseStmtAlternative
        of case_stmt_alternative_fields
    ]
  and case_stmt_alternative_fields = 
  {
         
    f_choices: alternatives_list
    Lazy.t;
         
    f_stmts: stmt_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and compilation_unit =
    [
    | `CompilationUnit
        of compilation_unit_fields
    ]
  and compilation_unit_fields = 
  {
         
    f_prelude: ada_node_list
    Lazy.t;
         
    f_body: [
      | `LibraryItem
          of library_item_fields
      | `Subunit
          of subunit_fields
    ]
    Lazy.t;
         
    f_pragmas: pragma_node_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and component_clause =
    [
    | `ComponentClause
        of component_clause_fields
    ]
  and component_clause_fields = 
  {
         
    f_id: identifier
    Lazy.t;
         
    f_position: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
         
    f_range: range_spec
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and component_def =
    [
    | `ComponentDef
        of component_def_fields
    ]
  and component_def_fields = 
  {
         
    f_has_aliased: aliased_node
    Lazy.t;
         
    f_has_constant: constant_node
    Lazy.t;
         
    f_type_expr: [
      | `AnonymousType
          of anonymous_type_fields
      | `ConstrainedSubtypeIndication
          of constrained_subtype_indication_fields
      | `DiscreteSubtypeIndication
          of discrete_subtype_indication_fields
      | `SubtypeIndication
          of subtype_indication_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and constant_node =
    [
    | `ConstantAbsent
        of constant_absent_fields
    | `ConstantPresent
        of constant_present_fields
    ]

  
   
  and constant_absent =
    [
    | `ConstantAbsent
        of constant_absent_fields
    ]
  and constant_absent_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and constant_present =
    [
    | `ConstantPresent
        of constant_present_fields
    ]
  and constant_present_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and constraint_node =
    [
    | `CompositeConstraint
        of composite_constraint_fields
    | `DeltaConstraint
        of delta_constraint_fields
    | `DigitsConstraint
        of digits_constraint_fields
    | `RangeConstraint
        of range_constraint_fields
    ]

  
   
  and composite_constraint =
    [
    | `CompositeConstraint
        of composite_constraint_fields
    ]
  and composite_constraint_fields = 
  {
         
    f_constraints: basic_assoc_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and delta_constraint =
    [
    | `DeltaConstraint
        of delta_constraint_fields
    ]
  and delta_constraint_fields = 
  {
         
    f_digits: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
         
    f_range: range_spec
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and digits_constraint =
    [
    | `DigitsConstraint
        of digits_constraint_fields
    ]
  and digits_constraint_fields = 
  {
         
    f_digits: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
         
    f_range: range_spec
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and range_constraint =
    [
    | `RangeConstraint
        of range_constraint_fields
    ]
  and range_constraint_fields = 
  {
         
    f_range: range_spec
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and declarative_part =
    [
    | `DeclarativePart
        of declarative_part_fields
    | `PrivatePart
        of private_part_fields
    | `PublicPart
        of public_part_fields
    ]
  and declarative_part_fields = 
  {
         
    f_decls: ada_node_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and private_part =
    [
    | `PrivatePart
        of private_part_fields
    ]
  and private_part_fields = 
  {
         
    f_decls: ada_node_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and public_part =
    [
    | `PublicPart
        of public_part_fields
    ]
  and public_part_fields = 
  {
         
    f_decls: ada_node_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and elsif_expr_part =
    [
    | `ElsifExprPart
        of elsif_expr_part_fields
    ]
  and elsif_expr_part_fields = 
  {
         
    f_cond_expr: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
         
    f_then_expr: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and elsif_stmt_part =
    [
    | `ElsifStmtPart
        of elsif_stmt_part_fields
    ]
  and elsif_stmt_part_fields = 
  {
         
    f_cond_expr: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
         
    f_stmts: stmt_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  (**
    * abstract_state_decl_expr
    * allocator
    * base_aggregate
    * bin_op
    * box_expr
    * case_expr_alternative
    * concat_op
    * concat_operand
    * cond_expr
    * contract_cases
    * decl_expr
    * membership_expr
    * name
    * paren_expr
    * quantified_expr
    * raise_expr
    * un_op
    *)
  and expr =
    [
    | `AbstractStateDeclExpr
        of abstract_state_decl_expr_fields
    | `Allocator
        of allocator_fields
    | `Aggregate
        of aggregate_fields
    | `BracketAggregate
        of bracket_aggregate_fields
    | `DeltaAggregate
        of delta_aggregate_fields
    | `BracketDeltaAggregate
        of bracket_delta_aggregate_fields
    | `NullRecordAggregate
        of null_record_aggregate_fields
    | `BinOp
        of bin_op_fields
    | `RelationOp
        of relation_op_fields
    | `BoxExpr
        of box_expr_fields
    | `CaseExprAlternative
        of case_expr_alternative_fields
    | `ConcatOp
        of concat_op_fields
    | `ConcatOperand
        of concat_operand_fields
    | `CaseExpr
        of case_expr_fields
    | `IfExpr
        of if_expr_fields
    | `ContractCases
        of contract_cases_fields
    | `DeclExpr
        of decl_expr_fields
    | `MembershipExpr
        of membership_expr_fields
    | `AttributeRef
        of attribute_ref_fields
    | `CallExpr
        of call_expr_fields
    | `DefiningName
        of defining_name_fields
    | `SyntheticDefiningName
        of synthetic_defining_name_fields
    | `DiscreteSubtypeName
        of discrete_subtype_name_fields
    | `DottedName
        of dotted_name_fields
    | `EndName
        of end_name_fields
    | `ExplicitDeref
        of explicit_deref_fields
    | `QualExpr
        of qual_expr_fields
    | `ReduceAttributeRef
        of reduce_attribute_ref_fields
    | `CharLiteral
        of char_literal_fields
    | `Identifier
        of identifier_fields
    | `OpAbs
        of op_abs_fields
    | `OpAnd
        of op_and_fields
    | `OpAndThen
        of op_and_then_fields
    | `OpConcat
        of op_concat_fields
    | `OpDiv
        of op_div_fields
    | `OpDoubleDot
        of op_double_dot_fields
    | `OpEq
        of op_eq_fields
    | `OpGt
        of op_gt_fields
    | `OpGte
        of op_gte_fields
    | `OpIn
        of op_in_fields
    | `OpLt
        of op_lt_fields
    | `OpLte
        of op_lte_fields
    | `OpMinus
        of op_minus_fields
    | `OpMod
        of op_mod_fields
    | `OpMult
        of op_mult_fields
    | `OpNeq
        of op_neq_fields
    | `OpNot
        of op_not_fields
    | `OpNotIn
        of op_not_in_fields
    | `OpOr
        of op_or_fields
    | `OpOrElse
        of op_or_else_fields
    | `OpPlus
        of op_plus_fields
    | `OpPow
        of op_pow_fields
    | `OpRem
        of op_rem_fields
    | `OpXor
        of op_xor_fields
    | `StringLiteral
        of string_literal_fields
    | `NullLiteral
        of null_literal_fields
    | `IntLiteral
        of int_literal_fields
    | `RealLiteral
        of real_literal_fields
    | `SyntheticIdentifier
        of synthetic_identifier_fields
    | `TargetName
        of target_name_fields
    | `UpdateAttributeRef
        of update_attribute_ref_fields
    | `ParenExpr
        of paren_expr_fields
    | `QuantifiedExpr
        of quantified_expr_fields
    | `RaiseExpr
        of raise_expr_fields
    | `UnOp
        of un_op_fields
    ]

  
   
  and abstract_state_decl_expr =
    [
    | `AbstractStateDeclExpr
        of abstract_state_decl_expr_fields
    ]
  and abstract_state_decl_expr_fields = 
  {
         
    f_state_decl: [
      | `AbstractStateDecl
          of abstract_state_decl_fields
      | `MultiAbstractStateDecl
          of multi_abstract_state_decl_fields
      | `ParenAbstractStateDecl
          of paren_abstract_state_decl_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and allocator =
    [
    | `Allocator
        of allocator_fields
    ]
  and allocator_fields = 
  {
         
    f_subpool: [
      | `AttributeRef
          of attribute_ref_fields
      | `CallExpr
          of call_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `QualExpr
          of qual_expr_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    option
    Lazy.t;
         
    f_type_or_expr: [
      | `ConstrainedSubtypeIndication
          of constrained_subtype_indication_fields
      | `DiscreteSubtypeIndication
          of discrete_subtype_indication_fields
      | `QualExpr
          of qual_expr_fields
      | `SubtypeIndication
          of subtype_indication_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  (**
    * aggregate
    * delta_aggregate
    * null_record_aggregate
    *)
  and base_aggregate =
    [
    | `Aggregate
        of aggregate_fields
    | `BracketAggregate
        of bracket_aggregate_fields
    | `DeltaAggregate
        of delta_aggregate_fields
    | `BracketDeltaAggregate
        of bracket_delta_aggregate_fields
    | `NullRecordAggregate
        of null_record_aggregate_fields
    ]

  
   
  and aggregate =
    [
    | `Aggregate
        of aggregate_fields
    | `BracketAggregate
        of bracket_aggregate_fields
    ]
  and aggregate_fields = 
  {
         
    f_ancestor_expr: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    option
    Lazy.t;
         
    f_assocs: basic_assoc_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and bracket_aggregate =
    [
    | `BracketAggregate
        of bracket_aggregate_fields
    ]
  and bracket_aggregate_fields = 
  {
         
    f_ancestor_expr: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    option
    Lazy.t;
         
    f_assocs: basic_assoc_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and delta_aggregate =
    [
    | `DeltaAggregate
        of delta_aggregate_fields
    | `BracketDeltaAggregate
        of bracket_delta_aggregate_fields
    ]
  and delta_aggregate_fields = 
  {
         
    f_ancestor_expr: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    option
    Lazy.t;
         
    f_assocs: basic_assoc_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and bracket_delta_aggregate =
    [
    | `BracketDeltaAggregate
        of bracket_delta_aggregate_fields
    ]
  and bracket_delta_aggregate_fields = 
  {
         
    f_ancestor_expr: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    option
    Lazy.t;
         
    f_assocs: basic_assoc_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and null_record_aggregate =
    [
    | `NullRecordAggregate
        of null_record_aggregate_fields
    ]
  and null_record_aggregate_fields = 
  {
         
    f_ancestor_expr: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    option
    Lazy.t;
         
    f_assocs: basic_assoc_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and bin_op =
    [
    | `BinOp
        of bin_op_fields
    | `RelationOp
        of relation_op_fields
    ]
  and bin_op_fields = 
  {
         
    f_left: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
         
    f_op: [
      | `OpAnd
          of op_and_fields
      | `OpAndThen
          of op_and_then_fields
      | `OpDiv
          of op_div_fields
      | `OpDoubleDot
          of op_double_dot_fields
      | `OpEq
          of op_eq_fields
      | `OpGt
          of op_gt_fields
      | `OpGte
          of op_gte_fields
      | `OpLt
          of op_lt_fields
      | `OpLte
          of op_lte_fields
      | `OpMinus
          of op_minus_fields
      | `OpMod
          of op_mod_fields
      | `OpMult
          of op_mult_fields
      | `OpNeq
          of op_neq_fields
      | `OpOr
          of op_or_fields
      | `OpOrElse
          of op_or_else_fields
      | `OpPlus
          of op_plus_fields
      | `OpPow
          of op_pow_fields
      | `OpRem
          of op_rem_fields
      | `OpXor
          of op_xor_fields
    ]
    Lazy.t;
         
    f_right: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and relation_op =
    [
    | `RelationOp
        of relation_op_fields
    ]
  and relation_op_fields = 
  {
         
    f_left: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
         
    f_op: [
      | `OpAnd
          of op_and_fields
      | `OpAndThen
          of op_and_then_fields
      | `OpDiv
          of op_div_fields
      | `OpDoubleDot
          of op_double_dot_fields
      | `OpEq
          of op_eq_fields
      | `OpGt
          of op_gt_fields
      | `OpGte
          of op_gte_fields
      | `OpLt
          of op_lt_fields
      | `OpLte
          of op_lte_fields
      | `OpMinus
          of op_minus_fields
      | `OpMod
          of op_mod_fields
      | `OpMult
          of op_mult_fields
      | `OpNeq
          of op_neq_fields
      | `OpOr
          of op_or_fields
      | `OpOrElse
          of op_or_else_fields
      | `OpPlus
          of op_plus_fields
      | `OpPow
          of op_pow_fields
      | `OpRem
          of op_rem_fields
      | `OpXor
          of op_xor_fields
    ]
    Lazy.t;
         
    f_right: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and box_expr =
    [
    | `BoxExpr
        of box_expr_fields
    ]
  and box_expr_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and case_expr_alternative =
    [
    | `CaseExprAlternative
        of case_expr_alternative_fields
    ]
  and case_expr_alternative_fields = 
  {
         
    f_choices: alternatives_list
    Lazy.t;
         
    f_expr: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and concat_op =
    [
    | `ConcatOp
        of concat_op_fields
    ]
  and concat_op_fields = 
  {
         
    f_first_operand: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
         
    f_other_operands: concat_operand_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and concat_operand =
    [
    | `ConcatOperand
        of concat_operand_fields
    ]
  and concat_operand_fields = 
  {
         
    f_operator: op_concat
    Lazy.t;
         
    f_operand: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and cond_expr =
    [
    | `CaseExpr
        of case_expr_fields
    | `IfExpr
        of if_expr_fields
    ]

  
   
  and case_expr =
    [
    | `CaseExpr
        of case_expr_fields
    ]
  and case_expr_fields = 
  {
         
    f_expr: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
         
    f_cases: case_expr_alternative_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and if_expr =
    [
    | `IfExpr
        of if_expr_fields
    ]
  and if_expr_fields = 
  {
         
    f_cond_expr: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
         
    f_then_expr: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
         
    f_alternatives: elsif_expr_part_list
    Lazy.t;
         
    f_else_expr: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and contract_cases =
    [
    | `ContractCases
        of contract_cases_fields
    ]
  and contract_cases_fields = 
  {
         
    f_contract_cases: contract_case_assoc_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and decl_expr =
    [
    | `DeclExpr
        of decl_expr_fields
    ]
  and decl_expr_fields = 
  {
         
    f_decls: basic_decl_list
    Lazy.t;
         
    f_expr: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and membership_expr =
    [
    | `MembershipExpr
        of membership_expr_fields
    ]
  and membership_expr_fields = 
  {
         
    f_expr: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
         
    f_op: [
      | `OpIn
          of op_in_fields
      | `OpNotIn
          of op_not_in_fields
    ]
    Lazy.t;
         
    f_membership_exprs: expr_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  (**
    * attribute_ref
    * call_expr
    * defining_name
    * discrete_subtype_name
    * dotted_name
    * end_name
    * explicit_deref
    * qual_expr
    * reduce_attribute_ref
    * single_tok_node
    * synthetic_identifier
    * target_name
    * update_attribute_ref
    *)
  and name =
    [
    | `AttributeRef
        of attribute_ref_fields
    | `CallExpr
        of call_expr_fields
    | `DefiningName
        of defining_name_fields
    | `SyntheticDefiningName
        of synthetic_defining_name_fields
    | `DiscreteSubtypeName
        of discrete_subtype_name_fields
    | `DottedName
        of dotted_name_fields
    | `EndName
        of end_name_fields
    | `ExplicitDeref
        of explicit_deref_fields
    | `QualExpr
        of qual_expr_fields
    | `ReduceAttributeRef
        of reduce_attribute_ref_fields
    | `CharLiteral
        of char_literal_fields
    | `Identifier
        of identifier_fields
    | `OpAbs
        of op_abs_fields
    | `OpAnd
        of op_and_fields
    | `OpAndThen
        of op_and_then_fields
    | `OpConcat
        of op_concat_fields
    | `OpDiv
        of op_div_fields
    | `OpDoubleDot
        of op_double_dot_fields
    | `OpEq
        of op_eq_fields
    | `OpGt
        of op_gt_fields
    | `OpGte
        of op_gte_fields
    | `OpIn
        of op_in_fields
    | `OpLt
        of op_lt_fields
    | `OpLte
        of op_lte_fields
    | `OpMinus
        of op_minus_fields
    | `OpMod
        of op_mod_fields
    | `OpMult
        of op_mult_fields
    | `OpNeq
        of op_neq_fields
    | `OpNot
        of op_not_fields
    | `OpNotIn
        of op_not_in_fields
    | `OpOr
        of op_or_fields
    | `OpOrElse
        of op_or_else_fields
    | `OpPlus
        of op_plus_fields
    | `OpPow
        of op_pow_fields
    | `OpRem
        of op_rem_fields
    | `OpXor
        of op_xor_fields
    | `StringLiteral
        of string_literal_fields
    | `NullLiteral
        of null_literal_fields
    | `IntLiteral
        of int_literal_fields
    | `RealLiteral
        of real_literal_fields
    | `SyntheticIdentifier
        of synthetic_identifier_fields
    | `TargetName
        of target_name_fields
    | `UpdateAttributeRef
        of update_attribute_ref_fields
    ]

  
   
  and attribute_ref =
    [
    | `AttributeRef
        of attribute_ref_fields
    ]
  and attribute_ref_fields = 
  {
         
    f_prefix: [
      | `AttributeRef
          of attribute_ref_fields
      | `CallExpr
          of call_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `QualExpr
          of qual_expr_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
         
    f_attribute: identifier
    Lazy.t;
         
    f_args: basic_assoc_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and call_expr =
    [
    | `CallExpr
        of call_expr_fields
    ]
  and call_expr_fields = 
  {
         
    f_name: [
      | `AttributeRef
          of attribute_ref_fields
      | `CallExpr
          of call_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `QualExpr
          of qual_expr_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
         
    f_suffix: [
      | `AssocList
          of assoc_list_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `CallExpr
          of call_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `DiscreteSubtypeIndication
          of discrete_subtype_indication_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `QualExpr
          of qual_expr_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and defining_name =
    [
    | `DefiningName
        of defining_name_fields
    | `SyntheticDefiningName
        of synthetic_defining_name_fields
    ]
  and defining_name_fields = 
  {
         
    f_name: [
      | `CharLiteral
          of char_literal_fields
      | `DottedName
          of dotted_name_fields
      | `Identifier
          of identifier_fields
      | `StringLiteral
          of string_literal_fields
      | `SyntheticIdentifier
          of synthetic_identifier_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and synthetic_defining_name =
    [
    | `SyntheticDefiningName
        of synthetic_defining_name_fields
    ]
  and synthetic_defining_name_fields = 
  {
         
    f_name: [
      | `CharLiteral
          of char_literal_fields
      | `DottedName
          of dotted_name_fields
      | `Identifier
          of identifier_fields
      | `StringLiteral
          of string_literal_fields
      | `SyntheticIdentifier
          of synthetic_identifier_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and discrete_subtype_name =
    [
    | `DiscreteSubtypeName
        of discrete_subtype_name_fields
    ]
  and discrete_subtype_name_fields = 
  {
         
    f_subtype: discrete_subtype_indication
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and dotted_name =
    [
    | `DottedName
        of dotted_name_fields
    ]
  and dotted_name_fields = 
  {
         
    f_prefix: [
      | `AttributeRef
          of attribute_ref_fields
      | `CallExpr
          of call_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `QualExpr
          of qual_expr_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
         
    f_suffix: [
      | `CharLiteral
          of char_literal_fields
      | `Identifier
          of identifier_fields
      | `StringLiteral
          of string_literal_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and end_name =
    [
    | `EndName
        of end_name_fields
    ]
  and end_name_fields = 
  {
         
    f_name: [
      | `CharLiteral
          of char_literal_fields
      | `DottedName
          of dotted_name_fields
      | `Identifier
          of identifier_fields
      | `StringLiteral
          of string_literal_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and explicit_deref =
    [
    | `ExplicitDeref
        of explicit_deref_fields
    ]
  and explicit_deref_fields = 
  {
         
    f_prefix: [
      | `AttributeRef
          of attribute_ref_fields
      | `CallExpr
          of call_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `QualExpr
          of qual_expr_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and qual_expr =
    [
    | `QualExpr
        of qual_expr_fields
    ]
  and qual_expr_fields = 
  {
         
    f_prefix: [
      | `AttributeRef
          of attribute_ref_fields
      | `CallExpr
          of call_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `QualExpr
          of qual_expr_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
         
    f_suffix: [
      | `Aggregate
          of aggregate_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and reduce_attribute_ref =
    [
    | `ReduceAttributeRef
        of reduce_attribute_ref_fields
    ]
  and reduce_attribute_ref_fields = 
  {
         
    f_prefix: [
      | `AttributeRef
          of attribute_ref_fields
      | `CallExpr
          of call_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `QualExpr
          of qual_expr_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
      | `ValueSequence
          of value_sequence_fields
    ]
    Lazy.t;
         
    f_attribute: identifier
    Lazy.t;
         
    f_args: basic_assoc_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  (**
    * base_id
    * null_literal
    * num_literal
    *)
  and single_tok_node =
    [
    | `CharLiteral
        of char_literal_fields
    | `Identifier
        of identifier_fields
    | `OpAbs
        of op_abs_fields
    | `OpAnd
        of op_and_fields
    | `OpAndThen
        of op_and_then_fields
    | `OpConcat
        of op_concat_fields
    | `OpDiv
        of op_div_fields
    | `OpDoubleDot
        of op_double_dot_fields
    | `OpEq
        of op_eq_fields
    | `OpGt
        of op_gt_fields
    | `OpGte
        of op_gte_fields
    | `OpIn
        of op_in_fields
    | `OpLt
        of op_lt_fields
    | `OpLte
        of op_lte_fields
    | `OpMinus
        of op_minus_fields
    | `OpMod
        of op_mod_fields
    | `OpMult
        of op_mult_fields
    | `OpNeq
        of op_neq_fields
    | `OpNot
        of op_not_fields
    | `OpNotIn
        of op_not_in_fields
    | `OpOr
        of op_or_fields
    | `OpOrElse
        of op_or_else_fields
    | `OpPlus
        of op_plus_fields
    | `OpPow
        of op_pow_fields
    | `OpRem
        of op_rem_fields
    | `OpXor
        of op_xor_fields
    | `StringLiteral
        of string_literal_fields
    | `NullLiteral
        of null_literal_fields
    | `IntLiteral
        of int_literal_fields
    | `RealLiteral
        of real_literal_fields
    ]

  
   
  (**
    * char_literal
    * identifier
    * op
    * string_literal
    *)
  and base_id =
    [
    | `CharLiteral
        of char_literal_fields
    | `Identifier
        of identifier_fields
    | `OpAbs
        of op_abs_fields
    | `OpAnd
        of op_and_fields
    | `OpAndThen
        of op_and_then_fields
    | `OpConcat
        of op_concat_fields
    | `OpDiv
        of op_div_fields
    | `OpDoubleDot
        of op_double_dot_fields
    | `OpEq
        of op_eq_fields
    | `OpGt
        of op_gt_fields
    | `OpGte
        of op_gte_fields
    | `OpIn
        of op_in_fields
    | `OpLt
        of op_lt_fields
    | `OpLte
        of op_lte_fields
    | `OpMinus
        of op_minus_fields
    | `OpMod
        of op_mod_fields
    | `OpMult
        of op_mult_fields
    | `OpNeq
        of op_neq_fields
    | `OpNot
        of op_not_fields
    | `OpNotIn
        of op_not_in_fields
    | `OpOr
        of op_or_fields
    | `OpOrElse
        of op_or_else_fields
    | `OpPlus
        of op_plus_fields
    | `OpPow
        of op_pow_fields
    | `OpRem
        of op_rem_fields
    | `OpXor
        of op_xor_fields
    | `StringLiteral
        of string_literal_fields
    ]

  
   
  and char_literal =
    [
    | `CharLiteral
        of char_literal_fields
    ]
  and char_literal_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and identifier =
    [
    | `Identifier
        of identifier_fields
    ]
  and identifier_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and op =
    [
    | `OpAbs
        of op_abs_fields
    | `OpAnd
        of op_and_fields
    | `OpAndThen
        of op_and_then_fields
    | `OpConcat
        of op_concat_fields
    | `OpDiv
        of op_div_fields
    | `OpDoubleDot
        of op_double_dot_fields
    | `OpEq
        of op_eq_fields
    | `OpGt
        of op_gt_fields
    | `OpGte
        of op_gte_fields
    | `OpIn
        of op_in_fields
    | `OpLt
        of op_lt_fields
    | `OpLte
        of op_lte_fields
    | `OpMinus
        of op_minus_fields
    | `OpMod
        of op_mod_fields
    | `OpMult
        of op_mult_fields
    | `OpNeq
        of op_neq_fields
    | `OpNot
        of op_not_fields
    | `OpNotIn
        of op_not_in_fields
    | `OpOr
        of op_or_fields
    | `OpOrElse
        of op_or_else_fields
    | `OpPlus
        of op_plus_fields
    | `OpPow
        of op_pow_fields
    | `OpRem
        of op_rem_fields
    | `OpXor
        of op_xor_fields
    ]

  
   
  and op_abs =
    [
    | `OpAbs
        of op_abs_fields
    ]
  and op_abs_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and op_and =
    [
    | `OpAnd
        of op_and_fields
    ]
  and op_and_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and op_and_then =
    [
    | `OpAndThen
        of op_and_then_fields
    ]
  and op_and_then_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and op_concat =
    [
    | `OpConcat
        of op_concat_fields
    ]
  and op_concat_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and op_div =
    [
    | `OpDiv
        of op_div_fields
    ]
  and op_div_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and op_double_dot =
    [
    | `OpDoubleDot
        of op_double_dot_fields
    ]
  and op_double_dot_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and op_eq =
    [
    | `OpEq
        of op_eq_fields
    ]
  and op_eq_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and op_gt =
    [
    | `OpGt
        of op_gt_fields
    ]
  and op_gt_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and op_gte =
    [
    | `OpGte
        of op_gte_fields
    ]
  and op_gte_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and op_in =
    [
    | `OpIn
        of op_in_fields
    ]
  and op_in_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and op_lt =
    [
    | `OpLt
        of op_lt_fields
    ]
  and op_lt_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and op_lte =
    [
    | `OpLte
        of op_lte_fields
    ]
  and op_lte_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and op_minus =
    [
    | `OpMinus
        of op_minus_fields
    ]
  and op_minus_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and op_mod =
    [
    | `OpMod
        of op_mod_fields
    ]
  and op_mod_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and op_mult =
    [
    | `OpMult
        of op_mult_fields
    ]
  and op_mult_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and op_neq =
    [
    | `OpNeq
        of op_neq_fields
    ]
  and op_neq_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and op_not =
    [
    | `OpNot
        of op_not_fields
    ]
  and op_not_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and op_not_in =
    [
    | `OpNotIn
        of op_not_in_fields
    ]
  and op_not_in_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and op_or =
    [
    | `OpOr
        of op_or_fields
    ]
  and op_or_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and op_or_else =
    [
    | `OpOrElse
        of op_or_else_fields
    ]
  and op_or_else_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and op_plus =
    [
    | `OpPlus
        of op_plus_fields
    ]
  and op_plus_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and op_pow =
    [
    | `OpPow
        of op_pow_fields
    ]
  and op_pow_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and op_rem =
    [
    | `OpRem
        of op_rem_fields
    ]
  and op_rem_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and op_xor =
    [
    | `OpXor
        of op_xor_fields
    ]
  and op_xor_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and string_literal =
    [
    | `StringLiteral
        of string_literal_fields
    ]
  and string_literal_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and null_literal =
    [
    | `NullLiteral
        of null_literal_fields
    ]
  and null_literal_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and num_literal =
    [
    | `IntLiteral
        of int_literal_fields
    | `RealLiteral
        of real_literal_fields
    ]

  
   
  and int_literal =
    [
    | `IntLiteral
        of int_literal_fields
    ]
  and int_literal_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and real_literal =
    [
    | `RealLiteral
        of real_literal_fields
    ]
  and real_literal_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and synthetic_identifier =
    [
    | `SyntheticIdentifier
        of synthetic_identifier_fields
    ]
  and synthetic_identifier_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and target_name =
    [
    | `TargetName
        of target_name_fields
    ]
  and target_name_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and update_attribute_ref =
    [
    | `UpdateAttributeRef
        of update_attribute_ref_fields
    ]
  and update_attribute_ref_fields = 
  {
         
    f_prefix: [
      | `AttributeRef
          of attribute_ref_fields
      | `CallExpr
          of call_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `QualExpr
          of qual_expr_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
         
    f_attribute: identifier
    Lazy.t;
         
    f_values: base_aggregate
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and paren_expr =
    [
    | `ParenExpr
        of paren_expr_fields
    ]
  and paren_expr_fields = 
  {
         
    f_expr: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and quantified_expr =
    [
    | `QuantifiedExpr
        of quantified_expr_fields
    ]
  and quantified_expr_fields = 
  {
         
    f_quantifier: quantifier
    Lazy.t;
         
    f_loop_spec: for_loop_spec
    Lazy.t;
         
    f_expr: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and raise_expr =
    [
    | `RaiseExpr
        of raise_expr_fields
    ]
  and raise_expr_fields = 
  {
         
    f_exception_name: [
      | `AttributeRef
          of attribute_ref_fields
      | `CallExpr
          of call_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `QualExpr
          of qual_expr_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    option
    Lazy.t;
         
    f_error_message: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and un_op =
    [
    | `UnOp
        of un_op_fields
    ]
  and un_op_fields = 
  {
         
    f_op: [
      | `OpAbs
          of op_abs_fields
      | `OpMinus
          of op_minus_fields
      | `OpNot
          of op_not_fields
      | `OpPlus
          of op_plus_fields
    ]
    Lazy.t;
         
    f_expr: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and handled_stmts =
    [
    | `HandledStmts
        of handled_stmts_fields
    ]
  and handled_stmts_fields = 
  {
         
    f_stmts: stmt_list
    Lazy.t;
         
    f_exceptions: ada_node_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and interface_kind =
    [
    | `InterfaceKindLimited
        of interface_kind_limited_fields
    | `InterfaceKindProtected
        of interface_kind_protected_fields
    | `InterfaceKindSynchronized
        of interface_kind_synchronized_fields
    | `InterfaceKindTask
        of interface_kind_task_fields
    ]

  
   
  and interface_kind_limited =
    [
    | `InterfaceKindLimited
        of interface_kind_limited_fields
    ]
  and interface_kind_limited_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and interface_kind_protected =
    [
    | `InterfaceKindProtected
        of interface_kind_protected_fields
    ]
  and interface_kind_protected_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and interface_kind_synchronized =
    [
    | `InterfaceKindSynchronized
        of interface_kind_synchronized_fields
    ]
  and interface_kind_synchronized_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and interface_kind_task =
    [
    | `InterfaceKindTask
        of interface_kind_task_fields
    ]
  and interface_kind_task_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and iter_type =
    [
    | `IterTypeIn
        of iter_type_in_fields
    | `IterTypeOf
        of iter_type_of_fields
    ]

  
   
  and iter_type_in =
    [
    | `IterTypeIn
        of iter_type_in_fields
    ]
  and iter_type_in_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and iter_type_of =
    [
    | `IterTypeOf
        of iter_type_of_fields
    ]
  and iter_type_of_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and library_item =
    [
    | `LibraryItem
        of library_item_fields
    ]
  and library_item_fields = 
  {
         
    f_has_private: private_node
    Lazy.t;
         
    f_item: [
      | `AbstractSubpDecl
          of abstract_subp_decl_fields
      | `ErrorDecl
          of error_decl_fields
      | `ExprFunction
          of expr_function_fields
      | `GenericPackageDecl
          of generic_package_decl_fields
      | `GenericPackageInstantiation
          of generic_package_instantiation_fields
      | `GenericPackageRenamingDecl
          of generic_package_renaming_decl_fields
      | `GenericSubpDecl
          of generic_subp_decl_fields
      | `GenericSubpInstantiation
          of generic_subp_instantiation_fields
      | `GenericSubpRenamingDecl
          of generic_subp_renaming_decl_fields
      | `NullSubpDecl
          of null_subp_decl_fields
      | `PackageBody
          of package_body_fields
      | `PackageDecl
          of package_decl_fields
      | `PackageRenamingDecl
          of package_renaming_decl_fields
      | `SubpBody
          of subp_body_fields
      | `SubpDecl
          of subp_decl_fields
      | `SubpRenamingDecl
          of subp_renaming_decl_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and limited_node =
    [
    | `LimitedAbsent
        of limited_absent_fields
    | `LimitedPresent
        of limited_present_fields
    ]

  
   
  and limited_absent =
    [
    | `LimitedAbsent
        of limited_absent_fields
    ]
  and limited_absent_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and limited_present =
    [
    | `LimitedPresent
        of limited_present_fields
    ]
  and limited_present_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and loop_spec =
    [
    | `ForLoopSpec
        of for_loop_spec_fields
    | `WhileLoopSpec
        of while_loop_spec_fields
    ]

  
   
  and for_loop_spec =
    [
    | `ForLoopSpec
        of for_loop_spec_fields
    ]
  and for_loop_spec_fields = 
  {
         
    f_var_decl: for_loop_var_decl
    Lazy.t;
         
    f_loop_type: iter_type
    Lazy.t;
         
    f_has_reverse: reverse_node
    Lazy.t;
         
    f_iter_expr: [
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `CallExpr
          of call_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `DiscreteSubtypeIndication
          of discrete_subtype_indication_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `QualExpr
          of qual_expr_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
         
    f_iter_filter: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and while_loop_spec =
    [
    | `WhileLoopSpec
        of while_loop_spec_fields
    ]
  and while_loop_spec_fields = 
  {
         
    f_expr: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and mode =
    [
    | `ModeDefault
        of mode_default_fields
    | `ModeIn
        of mode_in_fields
    | `ModeInOut
        of mode_in_out_fields
    | `ModeOut
        of mode_out_fields
    ]

  
   
  and mode_default =
    [
    | `ModeDefault
        of mode_default_fields
    ]
  and mode_default_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and mode_in =
    [
    | `ModeIn
        of mode_in_fields
    ]
  and mode_in_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and mode_in_out =
    [
    | `ModeInOut
        of mode_in_out_fields
    ]
  and mode_in_out_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and mode_out =
    [
    | `ModeOut
        of mode_out_fields
    ]
  and mode_out_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and multi_abstract_state_decl =
    [
    | `MultiAbstractStateDecl
        of multi_abstract_state_decl_fields
    ]
  and multi_abstract_state_decl_fields = 
  {
         
    f_decls: abstract_state_decl_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and not_null =
    [
    | `NotNullAbsent
        of not_null_absent_fields
    | `NotNullPresent
        of not_null_present_fields
    ]

  
   
  and not_null_absent =
    [
    | `NotNullAbsent
        of not_null_absent_fields
    ]
  and not_null_absent_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and not_null_present =
    [
    | `NotNullPresent
        of not_null_present_fields
    ]
  and not_null_present_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and null_component_decl =
    [
    | `NullComponentDecl
        of null_component_decl_fields
    ]
  and null_component_decl_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and others_designator =
    [
    | `OthersDesignator
        of others_designator_fields
    ]
  and others_designator_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and overriding_node =
    [
    | `OverridingNotOverriding
        of overriding_not_overriding_fields
    | `OverridingOverriding
        of overriding_overriding_fields
    | `OverridingUnspecified
        of overriding_unspecified_fields
    ]

  
   
  and overriding_not_overriding =
    [
    | `OverridingNotOverriding
        of overriding_not_overriding_fields
    ]
  and overriding_not_overriding_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and overriding_overriding =
    [
    | `OverridingOverriding
        of overriding_overriding_fields
    ]
  and overriding_overriding_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and overriding_unspecified =
    [
    | `OverridingUnspecified
        of overriding_unspecified_fields
    ]
  and overriding_unspecified_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and params =
    [
    | `Params
        of params_fields
    ]
  and params_fields = 
  {
         
    f_params: param_spec_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and paren_abstract_state_decl =
    [
    | `ParenAbstractStateDecl
        of paren_abstract_state_decl_fields
    ]
  and paren_abstract_state_decl_fields = 
  {
         
    f_decl: [
      | `AbstractStateDecl
          of abstract_state_decl_fields
      | `ParenAbstractStateDecl
          of paren_abstract_state_decl_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and pp_directive =
    [
    | `PpElseDirective
        of pp_else_directive_fields
    | `PpElsifDirective
        of pp_elsif_directive_fields
    | `PpEndIfDirective
        of pp_end_if_directive_fields
    | `PpIfDirective
        of pp_if_directive_fields
    ]

  
   
  and pp_else_directive =
    [
    | `PpElseDirective
        of pp_else_directive_fields
    ]
  and pp_else_directive_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and pp_elsif_directive =
    [
    | `PpElsifDirective
        of pp_elsif_directive_fields
    ]
  and pp_elsif_directive_fields = 
  {
         
    f_expr: [
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `Identifier
          of identifier_fields
      | `ParenExpr
          of paren_expr_fields
      | `RelationOp
          of relation_op_fields
      | `UnOp
          of un_op_fields
    ]
    Lazy.t;
         
    f_then_kw: pp_then_kw
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and pp_end_if_directive =
    [
    | `PpEndIfDirective
        of pp_end_if_directive_fields
    ]
  and pp_end_if_directive_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and pp_if_directive =
    [
    | `PpIfDirective
        of pp_if_directive_fields
    ]
  and pp_if_directive_fields = 
  {
         
    f_expr: [
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `Identifier
          of identifier_fields
      | `ParenExpr
          of paren_expr_fields
      | `RelationOp
          of relation_op_fields
      | `UnOp
          of un_op_fields
    ]
    Lazy.t;
         
    f_then_kw: pp_then_kw
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and pp_then_kw =
    [
    | `PpThenKw
        of pp_then_kw_fields
    ]
  and pp_then_kw_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and pragma_node =
    [
    | `PragmaNode
        of pragma_node_fields
    ]
  and pragma_node_fields = 
  {
         
    f_id: identifier
    Lazy.t;
         
    f_args: base_assoc_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and private_node =
    [
    | `PrivateAbsent
        of private_absent_fields
    | `PrivatePresent
        of private_present_fields
    ]

  
   
  and private_absent =
    [
    | `PrivateAbsent
        of private_absent_fields
    ]
  and private_absent_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and private_present =
    [
    | `PrivatePresent
        of private_present_fields
    ]
  and private_present_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and protected_def =
    [
    | `ProtectedDef
        of protected_def_fields
    ]
  and protected_def_fields = 
  {
         
    f_public_part: public_part
    Lazy.t;
         
    f_private_part: private_part
    option
    Lazy.t;
         
    f_end_name: end_name
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and protected_node =
    [
    | `ProtectedAbsent
        of protected_absent_fields
    | `ProtectedPresent
        of protected_present_fields
    ]

  
   
  and protected_absent =
    [
    | `ProtectedAbsent
        of protected_absent_fields
    ]
  and protected_absent_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and protected_present =
    [
    | `ProtectedPresent
        of protected_present_fields
    ]
  and protected_present_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and quantifier =
    [
    | `QuantifierAll
        of quantifier_all_fields
    | `QuantifierSome
        of quantifier_some_fields
    ]

  
   
  and quantifier_all =
    [
    | `QuantifierAll
        of quantifier_all_fields
    ]
  and quantifier_all_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and quantifier_some =
    [
    | `QuantifierSome
        of quantifier_some_fields
    ]
  and quantifier_some_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and range_spec =
    [
    | `RangeSpec
        of range_spec_fields
    ]
  and range_spec_fields = 
  {
         
    f_range: [
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BoxExpr
          of box_expr_fields
      | `CallExpr
          of call_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `QualExpr
          of qual_expr_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and renaming_clause =
    [
    | `RenamingClause
        of renaming_clause_fields
    | `SyntheticRenamingClause
        of synthetic_renaming_clause_fields
    ]
  and renaming_clause_fields = 
  {
         
    f_renamed_object: [
      | `AttributeRef
          of attribute_ref_fields
      | `CallExpr
          of call_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `QualExpr
          of qual_expr_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and synthetic_renaming_clause =
    [
    | `SyntheticRenamingClause
        of synthetic_renaming_clause_fields
    ]
  and synthetic_renaming_clause_fields = 
  {
         
    f_renamed_object: [
      | `AttributeRef
          of attribute_ref_fields
      | `CallExpr
          of call_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `QualExpr
          of qual_expr_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and reverse_node =
    [
    | `ReverseAbsent
        of reverse_absent_fields
    | `ReversePresent
        of reverse_present_fields
    ]

  
   
  and reverse_absent =
    [
    | `ReverseAbsent
        of reverse_absent_fields
    ]
  and reverse_absent_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and reverse_present =
    [
    | `ReversePresent
        of reverse_present_fields
    ]
  and reverse_present_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and select_when_part =
    [
    | `SelectWhenPart
        of select_when_part_fields
    ]
  and select_when_part_fields = 
  {
         
    f_cond_expr: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    option
    Lazy.t;
         
    f_stmts: stmt_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  (**
    * composite_stmt
    * error_stmt
    * simple_stmt
    *)
  and stmt =
    [
    | `AcceptStmt
        of accept_stmt_fields
    | `AcceptStmtWithStmts
        of accept_stmt_with_stmts_fields
    | `ForLoopStmt
        of for_loop_stmt_fields
    | `LoopStmt
        of loop_stmt_fields
    | `WhileLoopStmt
        of while_loop_stmt_fields
    | `BeginBlock
        of begin_block_fields
    | `DeclBlock
        of decl_block_fields
    | `CaseStmt
        of case_stmt_fields
    | `ExtendedReturnStmt
        of extended_return_stmt_fields
    | `IfStmt
        of if_stmt_fields
    | `NamedStmt
        of named_stmt_fields
    | `SelectStmt
        of select_stmt_fields
    | `ErrorStmt
        of error_stmt_fields
    | `AbortStmt
        of abort_stmt_fields
    | `AssignStmt
        of assign_stmt_fields
    | `CallStmt
        of call_stmt_fields
    | `DelayStmt
        of delay_stmt_fields
    | `ExitStmt
        of exit_stmt_fields
    | `GotoStmt
        of goto_stmt_fields
    | `Label
        of label_fields
    | `NullStmt
        of null_stmt_fields
    | `RaiseStmt
        of raise_stmt_fields
    | `RequeueStmt
        of requeue_stmt_fields
    | `ReturnStmt
        of return_stmt_fields
    | `TerminateAlternative
        of terminate_alternative_fields
    ]

  
   
  (**
    * accept_stmt
    * base_loop_stmt
    * block_stmt
    * case_stmt
    * extended_return_stmt
    * if_stmt
    * named_stmt
    * select_stmt
    *)
  and composite_stmt =
    [
    | `AcceptStmt
        of accept_stmt_fields
    | `AcceptStmtWithStmts
        of accept_stmt_with_stmts_fields
    | `ForLoopStmt
        of for_loop_stmt_fields
    | `LoopStmt
        of loop_stmt_fields
    | `WhileLoopStmt
        of while_loop_stmt_fields
    | `BeginBlock
        of begin_block_fields
    | `DeclBlock
        of decl_block_fields
    | `CaseStmt
        of case_stmt_fields
    | `ExtendedReturnStmt
        of extended_return_stmt_fields
    | `IfStmt
        of if_stmt_fields
    | `NamedStmt
        of named_stmt_fields
    | `SelectStmt
        of select_stmt_fields
    ]

  
   
  and accept_stmt =
    [
    | `AcceptStmt
        of accept_stmt_fields
    | `AcceptStmtWithStmts
        of accept_stmt_with_stmts_fields
    ]
  and accept_stmt_fields = 
  {
         
    f_name: identifier
    Lazy.t;
         
    f_entry_index_expr: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    option
    Lazy.t;
         
    f_params: entry_completion_formal_params
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and accept_stmt_with_stmts =
    [
    | `AcceptStmtWithStmts
        of accept_stmt_with_stmts_fields
    ]
  and accept_stmt_with_stmts_fields = 
  {
         
    f_name: identifier
    Lazy.t;
         
    f_entry_index_expr: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    option
    Lazy.t;
         
    f_params: entry_completion_formal_params
    Lazy.t;
         
    f_stmts: handled_stmts
    Lazy.t;
         
    f_end_name: end_name
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and base_loop_stmt =
    [
    | `ForLoopStmt
        of for_loop_stmt_fields
    | `LoopStmt
        of loop_stmt_fields
    | `WhileLoopStmt
        of while_loop_stmt_fields
    ]

  
   
  and for_loop_stmt =
    [
    | `ForLoopStmt
        of for_loop_stmt_fields
    ]
  and for_loop_stmt_fields = 
  {
         
    f_spec: loop_spec
    option
    Lazy.t;
         
    f_stmts: stmt_list
    Lazy.t;
         
    f_end_name: end_name
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and loop_stmt =
    [
    | `LoopStmt
        of loop_stmt_fields
    ]
  and loop_stmt_fields = 
  {
         
    f_spec: loop_spec
    option
    Lazy.t;
         
    f_stmts: stmt_list
    Lazy.t;
         
    f_end_name: end_name
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and while_loop_stmt =
    [
    | `WhileLoopStmt
        of while_loop_stmt_fields
    ]
  and while_loop_stmt_fields = 
  {
         
    f_spec: loop_spec
    option
    Lazy.t;
         
    f_stmts: stmt_list
    Lazy.t;
         
    f_end_name: end_name
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and block_stmt =
    [
    | `BeginBlock
        of begin_block_fields
    | `DeclBlock
        of decl_block_fields
    ]

  
   
  and begin_block =
    [
    | `BeginBlock
        of begin_block_fields
    ]
  and begin_block_fields = 
  {
         
    f_stmts: handled_stmts
    Lazy.t;
         
    f_end_name: end_name
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and decl_block =
    [
    | `DeclBlock
        of decl_block_fields
    ]
  and decl_block_fields = 
  {
         
    f_decls: declarative_part
    Lazy.t;
         
    f_stmts: handled_stmts
    Lazy.t;
         
    f_end_name: end_name
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and case_stmt =
    [
    | `CaseStmt
        of case_stmt_fields
    ]
  and case_stmt_fields = 
  {
         
    f_expr: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
         
    f_pragmas: pragma_node_list
    Lazy.t;
         
    f_alternatives: case_stmt_alternative_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and extended_return_stmt =
    [
    | `ExtendedReturnStmt
        of extended_return_stmt_fields
    ]
  and extended_return_stmt_fields = 
  {
         
    f_decl: extended_return_stmt_object_decl
    Lazy.t;
         
    f_stmts: handled_stmts
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and if_stmt =
    [
    | `IfStmt
        of if_stmt_fields
    ]
  and if_stmt_fields = 
  {
         
    f_cond_expr: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
         
    f_then_stmts: stmt_list
    Lazy.t;
         
    f_alternatives: elsif_stmt_part_list
    Lazy.t;
         
    f_else_stmts: stmt_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and named_stmt =
    [
    | `NamedStmt
        of named_stmt_fields
    ]
  and named_stmt_fields = 
  {
         
    f_decl: named_stmt_decl
    Lazy.t;
         
    f_stmt: [
      | `BeginBlock
          of begin_block_fields
      | `DeclBlock
          of decl_block_fields
      | `ForLoopStmt
          of for_loop_stmt_fields
      | `LoopStmt
          of loop_stmt_fields
      | `WhileLoopStmt
          of while_loop_stmt_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and select_stmt =
    [
    | `SelectStmt
        of select_stmt_fields
    ]
  and select_stmt_fields = 
  {
         
    f_guards: select_when_part_list
    Lazy.t;
         
    f_else_stmts: stmt_list
    Lazy.t;
         
    f_abort_stmts: stmt_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and error_stmt =
    [
    | `ErrorStmt
        of error_stmt_fields
    ]
  and error_stmt_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and simple_stmt =
    [
    | `AbortStmt
        of abort_stmt_fields
    | `AssignStmt
        of assign_stmt_fields
    | `CallStmt
        of call_stmt_fields
    | `DelayStmt
        of delay_stmt_fields
    | `ExitStmt
        of exit_stmt_fields
    | `GotoStmt
        of goto_stmt_fields
    | `Label
        of label_fields
    | `NullStmt
        of null_stmt_fields
    | `RaiseStmt
        of raise_stmt_fields
    | `RequeueStmt
        of requeue_stmt_fields
    | `ReturnStmt
        of return_stmt_fields
    | `TerminateAlternative
        of terminate_alternative_fields
    ]

  
   
  and abort_stmt =
    [
    | `AbortStmt
        of abort_stmt_fields
    ]
  and abort_stmt_fields = 
  {
         
    f_names: name_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and assign_stmt =
    [
    | `AssignStmt
        of assign_stmt_fields
    ]
  and assign_stmt_fields = 
  {
         
    f_dest: [
      | `AttributeRef
          of attribute_ref_fields
      | `CallExpr
          of call_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `QualExpr
          of qual_expr_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
         
    f_expr: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and call_stmt =
    [
    | `CallStmt
        of call_stmt_fields
    ]
  and call_stmt_fields = 
  {
         
    f_call: [
      | `AttributeRef
          of attribute_ref_fields
      | `CallExpr
          of call_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `QualExpr
          of qual_expr_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and delay_stmt =
    [
    | `DelayStmt
        of delay_stmt_fields
    ]
  and delay_stmt_fields = 
  {
         
    f_has_until: until_node
    Lazy.t;
         
    f_expr: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and exit_stmt =
    [
    | `ExitStmt
        of exit_stmt_fields
    ]
  and exit_stmt_fields = 
  {
         
    f_loop_name: [
      | `CharLiteral
          of char_literal_fields
      | `DottedName
          of dotted_name_fields
      | `Identifier
          of identifier_fields
      | `StringLiteral
          of string_literal_fields
    ]
    option
    Lazy.t;
         
    f_cond_expr: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and goto_stmt =
    [
    | `GotoStmt
        of goto_stmt_fields
    ]
  and goto_stmt_fields = 
  {
         
    f_label_name: [
      | `CharLiteral
          of char_literal_fields
      | `DottedName
          of dotted_name_fields
      | `Identifier
          of identifier_fields
      | `StringLiteral
          of string_literal_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and label =
    [
    | `Label
        of label_fields
    ]
  and label_fields = 
  {
         
    f_decl: label_decl
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and null_stmt =
    [
    | `NullStmt
        of null_stmt_fields
    ]
  and null_stmt_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and raise_stmt =
    [
    | `RaiseStmt
        of raise_stmt_fields
    ]
  and raise_stmt_fields = 
  {
         
    f_exception_name: [
      | `AttributeRef
          of attribute_ref_fields
      | `CallExpr
          of call_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `QualExpr
          of qual_expr_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    option
    Lazy.t;
         
    f_error_message: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and requeue_stmt =
    [
    | `RequeueStmt
        of requeue_stmt_fields
    ]
  and requeue_stmt_fields = 
  {
         
    f_call_name: [
      | `AttributeRef
          of attribute_ref_fields
      | `CallExpr
          of call_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `QualExpr
          of qual_expr_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
         
    f_has_abort: abort_node
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and return_stmt =
    [
    | `ReturnStmt
        of return_stmt_fields
    ]
  and return_stmt_fields = 
  {
         
    f_return_expr: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and terminate_alternative =
    [
    | `TerminateAlternative
        of terminate_alternative_fields
    ]
  and terminate_alternative_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and subp_kind =
    [
    | `SubpKindFunction
        of subp_kind_function_fields
    | `SubpKindProcedure
        of subp_kind_procedure_fields
    ]

  
   
  and subp_kind_function =
    [
    | `SubpKindFunction
        of subp_kind_function_fields
    ]
  and subp_kind_function_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and subp_kind_procedure =
    [
    | `SubpKindProcedure
        of subp_kind_procedure_fields
    ]
  and subp_kind_procedure_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and subunit =
    [
    | `Subunit
        of subunit_fields
    ]
  and subunit_fields = 
  {
         
    f_name: [
      | `CharLiteral
          of char_literal_fields
      | `DottedName
          of dotted_name_fields
      | `Identifier
          of identifier_fields
      | `StringLiteral
          of string_literal_fields
    ]
    Lazy.t;
         
    f_body: [
      | `PackageBody
          of package_body_fields
      | `ProtectedBody
          of protected_body_fields
      | `SubpBody
          of subp_body_fields
      | `TaskBody
          of task_body_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and synchronized_node =
    [
    | `SynchronizedAbsent
        of synchronized_absent_fields
    | `SynchronizedPresent
        of synchronized_present_fields
    ]

  
   
  and synchronized_absent =
    [
    | `SynchronizedAbsent
        of synchronized_absent_fields
    ]
  and synchronized_absent_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and synchronized_present =
    [
    | `SynchronizedPresent
        of synchronized_present_fields
    ]
  and synchronized_present_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and tagged_node =
    [
    | `TaggedAbsent
        of tagged_absent_fields
    | `TaggedPresent
        of tagged_present_fields
    ]

  
   
  and tagged_absent =
    [
    | `TaggedAbsent
        of tagged_absent_fields
    ]
  and tagged_absent_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and tagged_present =
    [
    | `TaggedPresent
        of tagged_present_fields
    ]
  and tagged_present_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and task_def =
    [
    | `TaskDef
        of task_def_fields
    ]
  and task_def_fields = 
  {
         
    f_interfaces: parent_list
    Lazy.t;
         
    f_public_part: public_part
    Lazy.t;
         
    f_private_part: private_part
    option
    Lazy.t;
         
    f_end_name: end_name
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and type_attributes_repository =
    [
    | `TypeAttributesRepository
        of type_attributes_repository_fields
    ]
  and type_attributes_repository_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  (**
    * access_def
    * array_type_def
    * derived_type_def
    * enum_type_def
    * formal_discrete_type_def
    * interface_type_def
    * mod_int_type_def
    * private_type_def
    * real_type_def
    * record_type_def
    * signed_int_type_def
    *)
  and type_def =
    [
    | `AccessToSubpDef
        of access_to_subp_def_fields
    | `AnonymousTypeAccessDef
        of anonymous_type_access_def_fields
    | `TypeAccessDef
        of type_access_def_fields
    | `ArrayTypeDef
        of array_type_def_fields
    | `DerivedTypeDef
        of derived_type_def_fields
    | `EnumTypeDef
        of enum_type_def_fields
    | `FormalDiscreteTypeDef
        of formal_discrete_type_def_fields
    | `InterfaceTypeDef
        of interface_type_def_fields
    | `ModIntTypeDef
        of mod_int_type_def_fields
    | `PrivateTypeDef
        of private_type_def_fields
    | `DecimalFixedPointDef
        of decimal_fixed_point_def_fields
    | `FloatingPointDef
        of floating_point_def_fields
    | `OrdinaryFixedPointDef
        of ordinary_fixed_point_def_fields
    | `RecordTypeDef
        of record_type_def_fields
    | `SignedIntTypeDef
        of signed_int_type_def_fields
    ]

  
   
  (**
    * access_to_subp_def
    * base_type_access_def
    *)
  and access_def =
    [
    | `AccessToSubpDef
        of access_to_subp_def_fields
    | `AnonymousTypeAccessDef
        of anonymous_type_access_def_fields
    | `TypeAccessDef
        of type_access_def_fields
    ]

  
   
  and access_to_subp_def =
    [
    | `AccessToSubpDef
        of access_to_subp_def_fields
    ]
  and access_to_subp_def_fields = 
  {
         
    f_has_not_null: not_null
    Lazy.t;
         
    f_has_protected: protected_node
    Lazy.t;
         
    f_subp_spec: subp_spec
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and base_type_access_def =
    [
    | `AnonymousTypeAccessDef
        of anonymous_type_access_def_fields
    | `TypeAccessDef
        of type_access_def_fields
    ]

  
   
  and anonymous_type_access_def =
    [
    | `AnonymousTypeAccessDef
        of anonymous_type_access_def_fields
    ]
  and anonymous_type_access_def_fields = 
  {
         
    f_has_not_null: not_null
    Lazy.t;
         
    f_type_decl: base_type_decl
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and type_access_def =
    [
    | `TypeAccessDef
        of type_access_def_fields
    ]
  and type_access_def_fields = 
  {
         
    f_has_not_null: not_null
    Lazy.t;
         
    f_has_all: all_node
    Lazy.t;
         
    f_has_constant: constant_node
    Lazy.t;
         
    f_subtype_indication: subtype_indication
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and array_type_def =
    [
    | `ArrayTypeDef
        of array_type_def_fields
    ]
  and array_type_def_fields = 
  {
         
    f_indices: array_indices
    Lazy.t;
         
    f_component_type: component_def
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and derived_type_def =
    [
    | `DerivedTypeDef
        of derived_type_def_fields
    ]
  and derived_type_def_fields = 
  {
         
    f_has_abstract: abstract_node
    Lazy.t;
         
    f_has_limited: limited_node
    Lazy.t;
         
    f_has_synchronized: synchronized_node
    Lazy.t;
         
    f_subtype_indication: subtype_indication
    Lazy.t;
         
    f_interfaces: parent_list
    Lazy.t;
         
    f_record_extension: base_record_def
    option
    Lazy.t;
         
    f_has_with_private: with_private
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and enum_type_def =
    [
    | `EnumTypeDef
        of enum_type_def_fields
    ]
  and enum_type_def_fields = 
  {
         
    f_enum_literals: enum_literal_decl_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and formal_discrete_type_def =
    [
    | `FormalDiscreteTypeDef
        of formal_discrete_type_def_fields
    ]
  and formal_discrete_type_def_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and interface_type_def =
    [
    | `InterfaceTypeDef
        of interface_type_def_fields
    ]
  and interface_type_def_fields = 
  {
         
    f_interface_kind: interface_kind
    option
    Lazy.t;
         
    f_interfaces: parent_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and mod_int_type_def =
    [
    | `ModIntTypeDef
        of mod_int_type_def_fields
    ]
  and mod_int_type_def_fields = 
  {
         
    f_expr: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BoxExpr
          of box_expr_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and private_type_def =
    [
    | `PrivateTypeDef
        of private_type_def_fields
    ]
  and private_type_def_fields = 
  {
         
    f_has_abstract: abstract_node
    Lazy.t;
         
    f_has_tagged: tagged_node
    Lazy.t;
         
    f_has_limited: limited_node
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and real_type_def =
    [
    | `DecimalFixedPointDef
        of decimal_fixed_point_def_fields
    | `FloatingPointDef
        of floating_point_def_fields
    | `OrdinaryFixedPointDef
        of ordinary_fixed_point_def_fields
    ]

  
   
  and decimal_fixed_point_def =
    [
    | `DecimalFixedPointDef
        of decimal_fixed_point_def_fields
    ]
  and decimal_fixed_point_def_fields = 
  {
         
    f_delta: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BoxExpr
          of box_expr_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
         
    f_digits: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BoxExpr
          of box_expr_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
         
    f_range: range_spec
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and floating_point_def =
    [
    | `FloatingPointDef
        of floating_point_def_fields
    ]
  and floating_point_def_fields = 
  {
         
    f_num_digits: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BoxExpr
          of box_expr_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
         
    f_range: range_spec
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and ordinary_fixed_point_def =
    [
    | `OrdinaryFixedPointDef
        of ordinary_fixed_point_def_fields
    ]
  and ordinary_fixed_point_def_fields = 
  {
         
    f_delta: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BoxExpr
          of box_expr_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
         
    f_range: range_spec
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and record_type_def =
    [
    | `RecordTypeDef
        of record_type_def_fields
    ]
  and record_type_def_fields = 
  {
         
    f_has_abstract: abstract_node
    Lazy.t;
         
    f_has_tagged: tagged_node
    Lazy.t;
         
    f_has_limited: limited_node
    Lazy.t;
         
    f_record_def: base_record_def
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and signed_int_type_def =
    [
    | `SignedIntTypeDef
        of signed_int_type_def_fields
    ]
  and signed_int_type_def_fields = 
  {
         
    f_range: range_spec
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  (**
    * anonymous_type
    * enum_lit_synth_type_expr
    * subtype_indication
    * synthetic_type_expr
    *)
  and type_expr =
    [
    | `AnonymousType
        of anonymous_type_fields
    | `EnumLitSynthTypeExpr
        of enum_lit_synth_type_expr_fields
    | `SubtypeIndication
        of subtype_indication_fields
    | `ConstrainedSubtypeIndication
        of constrained_subtype_indication_fields
    | `DiscreteSubtypeIndication
        of discrete_subtype_indication_fields
    | `SyntheticTypeExpr
        of synthetic_type_expr_fields
    ]

  
   
  and anonymous_type =
    [
    | `AnonymousType
        of anonymous_type_fields
    ]
  and anonymous_type_fields = 
  {
         
    f_type_decl: anonymous_type_decl
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and enum_lit_synth_type_expr =
    [
    | `EnumLitSynthTypeExpr
        of enum_lit_synth_type_expr_fields
    ]
  and enum_lit_synth_type_expr_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and subtype_indication =
    [
    | `SubtypeIndication
        of subtype_indication_fields
    | `ConstrainedSubtypeIndication
        of constrained_subtype_indication_fields
    | `DiscreteSubtypeIndication
        of discrete_subtype_indication_fields
    ]
  and subtype_indication_fields = 
  {
         
    f_has_not_null: not_null
    Lazy.t;
         
    f_name: [
      | `AttributeRef
          of attribute_ref_fields
      | `CharLiteral
          of char_literal_fields
      | `DottedName
          of dotted_name_fields
      | `Identifier
          of identifier_fields
      | `StringLiteral
          of string_literal_fields
    ]
    Lazy.t;
         
    f_constraint: constraint_node
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and constrained_subtype_indication =
    [
    | `ConstrainedSubtypeIndication
        of constrained_subtype_indication_fields
    ]
  and constrained_subtype_indication_fields = 
  {
         
    f_has_not_null: not_null
    Lazy.t;
         
    f_name: [
      | `AttributeRef
          of attribute_ref_fields
      | `CharLiteral
          of char_literal_fields
      | `DottedName
          of dotted_name_fields
      | `Identifier
          of identifier_fields
      | `StringLiteral
          of string_literal_fields
    ]
    Lazy.t;
         
    f_constraint: constraint_node
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and discrete_subtype_indication =
    [
    | `DiscreteSubtypeIndication
        of discrete_subtype_indication_fields
    ]
  and discrete_subtype_indication_fields = 
  {
         
    f_has_not_null: not_null
    Lazy.t;
         
    f_name: [
      | `AttributeRef
          of attribute_ref_fields
      | `CharLiteral
          of char_literal_fields
      | `DottedName
          of dotted_name_fields
      | `Identifier
          of identifier_fields
      | `StringLiteral
          of string_literal_fields
    ]
    Lazy.t;
         
    f_constraint: constraint_node
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and synthetic_type_expr =
    [
    | `SyntheticTypeExpr
        of synthetic_type_expr_fields
    ]
  and synthetic_type_expr_fields = 
  {
         
    f_target_type: base_type_decl
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and unconstrained_array_index =
    [
    | `UnconstrainedArrayIndex
        of unconstrained_array_index_fields
    ]
  and unconstrained_array_index_fields = 
  {
         
    f_subtype_indication: subtype_indication
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and until_node =
    [
    | `UntilAbsent
        of until_absent_fields
    | `UntilPresent
        of until_present_fields
    ]

  
   
  and until_absent =
    [
    | `UntilAbsent
        of until_absent_fields
    ]
  and until_absent_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and until_present =
    [
    | `UntilPresent
        of until_present_fields
    ]
  and until_present_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and use_clause =
    [
    | `UsePackageClause
        of use_package_clause_fields
    | `UseTypeClause
        of use_type_clause_fields
    ]

  
   
  and use_package_clause =
    [
    | `UsePackageClause
        of use_package_clause_fields
    ]
  and use_package_clause_fields = 
  {
         
    f_packages: name_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and use_type_clause =
    [
    | `UseTypeClause
        of use_type_clause_fields
    ]
  and use_type_clause_fields = 
  {
         
    f_has_all: all_node
    Lazy.t;
         
    f_types: name_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and value_sequence =
    [
    | `ValueSequence
        of value_sequence_fields
    ]
  and value_sequence_fields = 
  {
         
    f_iter_assoc: iterated_assoc
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and variant =
    [
    | `Variant
        of variant_fields
    ]
  and variant_fields = 
  {
         
    f_choices: alternatives_list
    Lazy.t;
         
    f_components: component_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and variant_part =
    [
    | `VariantPart
        of variant_part_fields
    ]
  and variant_part_fields = 
  {
         
    f_discr_name: identifier
    Lazy.t;
         
    f_variant: variant_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and with_clause =
    [
    | `WithClause
        of with_clause_fields
    ]
  and with_clause_fields = 
  {
         
    f_has_limited: limited_node
    Lazy.t;
         
    f_has_private: private_node
    Lazy.t;
         
    f_packages: name_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and with_private =
    [
    | `WithPrivateAbsent
        of with_private_absent_fields
    | `WithPrivatePresent
        of with_private_present_fields
    ]

  
   
  and with_private_absent =
    [
    | `WithPrivateAbsent
        of with_private_absent_fields
    ]
  and with_private_absent_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and with_private_present =
    [
    | `WithPrivatePresent
        of with_private_present_fields
    ]
  and with_private_present_fields = 
  {
    c_value : entity;
    context : analysis_context
  }



module Entity : sig
  type t = entity

  val info : t -> entity_info
end

module AnalysisUnit : sig
  (**
  * This type represents the analysis of a single file.
  *
  * This type has strong-reference semantics and is ref-counted. Furthermore, a
  * reference to a unit contains an implicit reference to the context that owns
  * it. This means that keeping a reference to a unit will keep the context and
  * all the unit it contains allocated.
  *)

  type t = analysis_unit

  val root : t -> ada_node option
  (**
  * Return the root node for this unit, or ``None`` if there is none.
  *)

  val diagnostics : t -> Diagnostic.t list
  (**
   * Diagnostics for this unit.
   *)

  val filename : t -> string
  (**
  * Return the filename this unit is associated to.
  *)

  val reparse : ?charset:string -> ?buffer:string -> t -> unit
  (**
  * Reparse an analysis unit from a buffer, if provided, or from the original
  * file otherwise. If ``Charset`` is empty or ``None``, use the last charset
  * successfuly used for this unit, otherwise use it to decode the content of
  * the source file.
  *
  * If any failure occurs, such as decoding, lexing or parsing failure,
  * diagnostic are emitted to explain what happened.
  *)

  val first_token : t -> Token.t option
  (**
  * Return a reference to the first token scanned in this unit.
  *)

  val last_token : t -> Token.t option
  (**
  * Return a reference to the last token scanned in this unit.
  *)

  val token_count : t -> int
  (**
  * Return the number of tokens in this unit.
  *)

  val trivia_count : t -> int
  (**
  * Return the number of trivias in this unit. This is 0 for units that were
  * parsed with trivia analysis disabled.
  *)

  
  val fold_tokens : ('a -> Token.t -> 'a) -> 'a -> t -> 'a
  (**
   * Fold all the token this node contains by calling f on each token.
   *)

  val iter_tokens : (Token.t -> unit) -> t-> unit
  (**
   * Iterate over all token this node contains by calling f on each token.
   *)

  val map_tokens : (Token.t -> 'a) -> t -> 'a list
  (**
   * Map each token calling the given function
   *)

  val tokens : t -> Token.t list
  (**
   * Return a list of tokens for the given node
   *)

end

module AnalysisContext : sig
  (**
  * This type represents a context for all source analysis. This is the first
  * type you need to create to use Libadalang. It will contain the results of
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
  *)

  type t = analysis_context

  val create :
    ?charset:string
    -> ?with_trivia:bool
    -> ?tab_stop:int
    -> ?unit_provider:UnitProvider.t
    -> unit
    -> t
  (**
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
  * corresponds to a unit reference during semantic analysis. If it is
  * ``None``, the default one is used instead.
  *
  * ``Tab_Stop`` is a positive number to describe the effect of tabulation
  * characters on the column number in source files.
  *)

  val get_from_file :
    ?charset:string
    -> ?reparse:bool
    -> ?grammar_rule:GrammarRule.t
    -> t
    -> string
    -> AnalysisUnit.t
  (**
  * Create a new analysis unit for ``Filename`` or return the existing one if
  * any. If ``Reparse`` is true and the analysis unit already exists, reparse
  * it from ``Filename``.
  *
  * ``Rule`` controls which grammar rule is used to parse the unit.
  *
  * Use ``Charset`` in order to decode the source. If ``Charset`` is empty then
  * use the context's default charset.
  *
  * If any failure occurs, such as file opening, decoding, lexing or parsing
  * failure, return an analysis unit anyway: errors are described as
  * diagnostics of the returned analysis unit.
  *)

  val get_from_buffer :
    ?charset:string
    -> ?grammar_rule:GrammarRule.t
    -> t
    -> string
    -> string
    -> AnalysisUnit.t
  (**
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
  * failure, return an analysis unit anyway: errors are described as
  * diagnostics of the returned analysis unit.
  *)
end


module Aspect : sig
  (**
  * Composite field representing the aspect of an entity (:rmlink:`13`).
  *)

  
  type t = {
      exists :
         bool;
      node :
         ada_node option;
      value :
         expr option;
  }

end


module CompletionItem : sig
  (**

  *)

  
  type t = {
      decl :
         basic_decl option;
      is_dot_call :
         bool;
      is_visible :
         bool;
      weight :
         int;
  }

end


module DiscreteRange : sig
  (**
  * Represent the range of a discrete type or subtype. The bounds are not
  * evaluated, you need to call ``eval_as_int`` on them, if they're static, to
  * get their value.
  *)

  
  type t = {
      low_bound :
         expr option;
      high_bound :
         expr option;
  }

end


module DiscriminantValues : sig
  (**
  * Represent a set of values (as a list of choices) on a discriminant.
  *)

  
  type t = {
      discriminant :
         identifier option;
      values :
         alternatives_list option;
  }

end


module DocAnnotation : sig
  (**
  * Documentation annotation.
  *)

  
  type t = {
      key :
         string;
      value :
         string;
  }

end


module ParamActual : sig
  (**
  * Data structure used by zip_with_params, Name.call_params,
  * GenericInstantiation.inst_params, BaseAggregate.aggregate_params,
  * SubtypeIndication.subtype_constraints, and EnumRepClause.params properties.
  * Associates an expression (the actual) to a formal param declaration (the
  * parameter).
  *)

  
  type t = {
      param :
         defining_name option;
      actual :
         expr option;
  }

end


module RefResult : sig
  (**
  * Result for a cross reference query returning a reference.
  *)

  
  type t = {
      ref_ :
         base_id option;
      kind :
         RefResultKind.t;
  }

end


module RefdDecl : sig
  (**
  * Result for a cross reference query returning a referenced decl.
  *)

  
  type t = {
      decl :
         basic_decl option;
      kind :
         RefResultKind.t;
  }

end


module RefdDef : sig
  (**
  * Result for a cross reference query returning a referenced defining name.
  *)

  
  type t = {
      def_name :
         defining_name option;
      kind :
         RefResultKind.t;
  }

end


module Shape : sig
  (**
  * Represent one of the shapes that a variant record can have, as a list of
  * the available components.
  *)

  
  type t = {
      components :
         base_formal_param_decl list;
      discriminants_values :
         DiscriminantValues.t list;
  }

end


module Substitution : sig
  (**
  * Represent a substitution of a BasicDecl by a given value. This can then be
  * used as part of an environment in the eval_as_*_in_env property. See the
  * declaration of those properties for more details.
  *)

  
  type t = {
      from_decl :
         basic_decl option;
      to_value :
         BigInteger.t;
      value_type :
         base_type_decl option;
  }

end


type _ node =
  | AdaNode :
      ada_node node
  | AbortNode :
      abort_node node
  | AbortAbsent :
      abort_absent node
  | AbortPresent :
      abort_present node
  | AbstractNode :
      abstract_node node
  | AbstractAbsent :
      abstract_absent node
  | AbstractPresent :
      abstract_present node
  | AdaList :
      ada_list node
  | AdaNodeList :
      ada_node_list node
  | AbstractStateDeclList :
      abstract_state_decl_list node
  | AlternativesList :
      alternatives_list node
  | ConstraintList :
      constraint_list node
  | DeclList :
      decl_list node
  | StmtList :
      stmt_list node
  | AspectAssocList :
      aspect_assoc_list node
  | BaseAssocList :
      base_assoc_list node
  | BasicAssocList :
      basic_assoc_list node
  | AssocList :
      assoc_list node
  | BasicDeclList :
      basic_decl_list node
  | CaseExprAlternativeList :
      case_expr_alternative_list node
  | CaseStmtAlternativeList :
      case_stmt_alternative_list node
  | CompilationUnitList :
      compilation_unit_list node
  | ConcatOperandList :
      concat_operand_list node
  | ContractCaseAssocList :
      contract_case_assoc_list node
  | DefiningNameList :
      defining_name_list node
  | DiscriminantSpecList :
      discriminant_spec_list node
  | ElsifExprPartList :
      elsif_expr_part_list node
  | ElsifStmtPartList :
      elsif_stmt_part_list node
  | EnumLiteralDeclList :
      enum_literal_decl_list node
  | ExprList :
      expr_list node
  | ExprAlternativesList :
      expr_alternatives_list node
  | IdentifierList :
      identifier_list node
  | DiscriminantChoiceList :
      discriminant_choice_list node
  | NameList :
      name_list node
  | ParentList :
      parent_list node
  | ParamSpecList :
      param_spec_list node
  | PragmaNodeList :
      pragma_node_list node
  | SelectWhenPartList :
      select_when_part_list node
  | UnconstrainedArrayIndexList :
      unconstrained_array_index_list node
  | VariantList :
      variant_list node
  | AliasedNode :
      aliased_node node
  | AliasedAbsent :
      aliased_absent node
  | AliasedPresent :
      aliased_present node
  | AllNode :
      all_node node
  | AllAbsent :
      all_absent node
  | AllPresent :
      all_present node
  | ArrayIndices :
      array_indices node
  | ConstrainedArrayIndices :
      constrained_array_indices node
  | UnconstrainedArrayIndices :
      unconstrained_array_indices node
  | AspectAssoc :
      aspect_assoc node
  | AspectClause :
      aspect_clause node
  | AtClause :
      at_clause node
  | AttributeDefClause :
      attribute_def_clause node
  | EnumRepClause :
      enum_rep_clause node
  | RecordRepClause :
      record_rep_clause node
  | AspectSpec :
      aspect_spec node
  | BaseAssoc :
      base_assoc node
  | ContractCaseAssoc :
      contract_case_assoc node
  | PragmaArgumentAssoc :
      pragma_argument_assoc node
  | BaseFormalParamHolder :
      base_formal_param_holder node
  | BaseSubpSpec :
      base_subp_spec node
  | EntrySpec :
      entry_spec node
  | EnumSubpSpec :
      enum_subp_spec node
  | SubpSpec :
      subp_spec node
  | SyntheticBinarySpec :
      synthetic_binary_spec node
  | SyntheticUnarySpec :
      synthetic_unary_spec node
  | ComponentList :
      component_list node
  | DiscriminantPart :
      discriminant_part node
  | KnownDiscriminantPart :
      known_discriminant_part node
  | UnknownDiscriminantPart :
      unknown_discriminant_part node
  | EntryCompletionFormalParams :
      entry_completion_formal_params node
  | GenericFormalPart :
      generic_formal_part node
  | BaseRecordDef :
      base_record_def node
  | NullRecordDef :
      null_record_def node
  | RecordDef :
      record_def node
  | BasicAssoc :
      basic_assoc node
  | AggregateAssoc :
      aggregate_assoc node
  | MultiDimArrayAssoc :
      multi_dim_array_assoc node
  | CompositeConstraintAssoc :
      composite_constraint_assoc node
  | IteratedAssoc :
      iterated_assoc node
  | ParamAssoc :
      param_assoc node
  | BasicDecl :
      basic_decl node
  | AbstractStateDecl :
      abstract_state_decl node
  | AnonymousExprDecl :
      anonymous_expr_decl node
  | BaseFormalParamDecl :
      base_formal_param_decl node
  | ComponentDecl :
      component_decl node
  | DiscriminantSpec :
      discriminant_spec node
  | GenericFormal :
      generic_formal node
  | GenericFormalObjDecl :
      generic_formal_obj_decl node
  | GenericFormalPackage :
      generic_formal_package node
  | GenericFormalSubpDecl :
      generic_formal_subp_decl node
  | GenericFormalTypeDecl :
      generic_formal_type_decl node
  | ParamSpec :
      param_spec node
  | SyntheticFormalParamDecl :
      synthetic_formal_param_decl node
  | BasePackageDecl :
      base_package_decl node
  | GenericPackageInternal :
      generic_package_internal node
  | PackageDecl :
      package_decl node
  | BaseTypeDecl :
      base_type_decl node
  | BaseSubtypeDecl :
      base_subtype_decl node
  | DiscreteBaseSubtypeDecl :
      discrete_base_subtype_decl node
  | SubtypeDecl :
      subtype_decl node
  | ClasswideTypeDecl :
      classwide_type_decl node
  | IncompleteTypeDecl :
      incomplete_type_decl node
  | IncompleteFormalTypeDecl :
      incomplete_formal_type_decl node
  | IncompleteTaggedTypeDecl :
      incomplete_tagged_type_decl node
  | ProtectedTypeDecl :
      protected_type_decl node
  | TaskTypeDecl :
      task_type_decl node
  | SingleTaskTypeDecl :
      single_task_type_decl node
  | TypeDecl :
      type_decl node
  | AnonymousTypeDecl :
      anonymous_type_decl node
  | SynthAnonymousTypeDecl :
      synth_anonymous_type_decl node
  | ConcreteTypeDecl :
      concrete_type_decl node
  | FormalTypeDecl :
      formal_type_decl node
  | BasicSubpDecl :
      basic_subp_decl node
  | ClassicSubpDecl :
      classic_subp_decl node
  | AbstractSubpDecl :
      abstract_subp_decl node
  | FormalSubpDecl :
      formal_subp_decl node
  | AbstractFormalSubpDecl :
      abstract_formal_subp_decl node
  | ConcreteFormalSubpDecl :
      concrete_formal_subp_decl node
  | SubpDecl :
      subp_decl node
  | EntryDecl :
      entry_decl node
  | EnumLiteralDecl :
      enum_literal_decl node
  | SyntheticCharEnumLit :
      synthetic_char_enum_lit node
  | GenericSubpInternal :
      generic_subp_internal node
  | SyntheticSubpDecl :
      synthetic_subp_decl node
  | BodyNode :
      body_node node
  | BaseSubpBody :
      base_subp_body node
  | ExprFunction :
      expr_function node
  | NullSubpDecl :
      null_subp_decl node
  | SubpBody :
      subp_body node
  | SubpRenamingDecl :
      subp_renaming_decl node
  | BodyStub :
      body_stub node
  | PackageBodyStub :
      package_body_stub node
  | ProtectedBodyStub :
      protected_body_stub node
  | SubpBodyStub :
      subp_body_stub node
  | TaskBodyStub :
      task_body_stub node
  | EntryBody :
      entry_body node
  | PackageBody :
      package_body node
  | ProtectedBody :
      protected_body node
  | TaskBody :
      task_body node
  | EntryIndexSpec :
      entry_index_spec node
  | ErrorDecl :
      error_decl node
  | ExceptionDecl :
      exception_decl node
  | ExceptionHandler :
      exception_handler node
  | ForLoopVarDecl :
      for_loop_var_decl node
  | GenericDecl :
      generic_decl node
  | GenericPackageDecl :
      generic_package_decl node
  | GenericSubpDecl :
      generic_subp_decl node
  | GenericInstantiation :
      generic_instantiation node
  | GenericPackageInstantiation :
      generic_package_instantiation node
  | GenericSubpInstantiation :
      generic_subp_instantiation node
  | GenericRenamingDecl :
      generic_renaming_decl node
  | GenericPackageRenamingDecl :
      generic_package_renaming_decl node
  | GenericSubpRenamingDecl :
      generic_subp_renaming_decl node
  | LabelDecl :
      label_decl node
  | NamedStmtDecl :
      named_stmt_decl node
  | NumberDecl :
      number_decl node
  | ObjectDecl :
      object_decl node
  | ExtendedReturnStmtObjectDecl :
      extended_return_stmt_object_decl node
  | NoTypeObjectRenamingDecl :
      no_type_object_renaming_decl node
  | PackageRenamingDecl :
      package_renaming_decl node
  | SingleProtectedDecl :
      single_protected_decl node
  | SingleTaskDecl :
      single_task_decl node
  | CaseStmtAlternative :
      case_stmt_alternative node
  | CompilationUnit :
      compilation_unit node
  | ComponentClause :
      component_clause node
  | ComponentDef :
      component_def node
  | ConstantNode :
      constant_node node
  | ConstantAbsent :
      constant_absent node
  | ConstantPresent :
      constant_present node
  | Constraint :
      constraint_node node
  | CompositeConstraint :
      composite_constraint node
  | DeltaConstraint :
      delta_constraint node
  | DigitsConstraint :
      digits_constraint node
  | RangeConstraint :
      range_constraint node
  | DeclarativePart :
      declarative_part node
  | PrivatePart :
      private_part node
  | PublicPart :
      public_part node
  | ElsifExprPart :
      elsif_expr_part node
  | ElsifStmtPart :
      elsif_stmt_part node
  | Expr :
      expr node
  | AbstractStateDeclExpr :
      abstract_state_decl_expr node
  | Allocator :
      allocator node
  | BaseAggregate :
      base_aggregate node
  | Aggregate :
      aggregate node
  | BracketAggregate :
      bracket_aggregate node
  | DeltaAggregate :
      delta_aggregate node
  | BracketDeltaAggregate :
      bracket_delta_aggregate node
  | NullRecordAggregate :
      null_record_aggregate node
  | BinOp :
      bin_op node
  | RelationOp :
      relation_op node
  | BoxExpr :
      box_expr node
  | CaseExprAlternative :
      case_expr_alternative node
  | ConcatOp :
      concat_op node
  | ConcatOperand :
      concat_operand node
  | CondExpr :
      cond_expr node
  | CaseExpr :
      case_expr node
  | IfExpr :
      if_expr node
  | ContractCases :
      contract_cases node
  | DeclExpr :
      decl_expr node
  | MembershipExpr :
      membership_expr node
  | Name :
      name node
  | AttributeRef :
      attribute_ref node
  | CallExpr :
      call_expr node
  | DefiningName :
      defining_name node
  | SyntheticDefiningName :
      synthetic_defining_name node
  | DiscreteSubtypeName :
      discrete_subtype_name node
  | DottedName :
      dotted_name node
  | EndName :
      end_name node
  | ExplicitDeref :
      explicit_deref node
  | QualExpr :
      qual_expr node
  | ReduceAttributeRef :
      reduce_attribute_ref node
  | SingleTokNode :
      single_tok_node node
  | BaseId :
      base_id node
  | CharLiteral :
      char_literal node
  | Identifier :
      identifier node
  | Op :
      op node
  | OpAbs :
      op_abs node
  | OpAnd :
      op_and node
  | OpAndThen :
      op_and_then node
  | OpConcat :
      op_concat node
  | OpDiv :
      op_div node
  | OpDoubleDot :
      op_double_dot node
  | OpEq :
      op_eq node
  | OpGt :
      op_gt node
  | OpGte :
      op_gte node
  | OpIn :
      op_in node
  | OpLt :
      op_lt node
  | OpLte :
      op_lte node
  | OpMinus :
      op_minus node
  | OpMod :
      op_mod node
  | OpMult :
      op_mult node
  | OpNeq :
      op_neq node
  | OpNot :
      op_not node
  | OpNotIn :
      op_not_in node
  | OpOr :
      op_or node
  | OpOrElse :
      op_or_else node
  | OpPlus :
      op_plus node
  | OpPow :
      op_pow node
  | OpRem :
      op_rem node
  | OpXor :
      op_xor node
  | StringLiteral :
      string_literal node
  | NullLiteral :
      null_literal node
  | NumLiteral :
      num_literal node
  | IntLiteral :
      int_literal node
  | RealLiteral :
      real_literal node
  | SyntheticIdentifier :
      synthetic_identifier node
  | TargetName :
      target_name node
  | UpdateAttributeRef :
      update_attribute_ref node
  | ParenExpr :
      paren_expr node
  | QuantifiedExpr :
      quantified_expr node
  | RaiseExpr :
      raise_expr node
  | UnOp :
      un_op node
  | HandledStmts :
      handled_stmts node
  | InterfaceKind :
      interface_kind node
  | InterfaceKindLimited :
      interface_kind_limited node
  | InterfaceKindProtected :
      interface_kind_protected node
  | InterfaceKindSynchronized :
      interface_kind_synchronized node
  | InterfaceKindTask :
      interface_kind_task node
  | IterType :
      iter_type node
  | IterTypeIn :
      iter_type_in node
  | IterTypeOf :
      iter_type_of node
  | LibraryItem :
      library_item node
  | LimitedNode :
      limited_node node
  | LimitedAbsent :
      limited_absent node
  | LimitedPresent :
      limited_present node
  | LoopSpec :
      loop_spec node
  | ForLoopSpec :
      for_loop_spec node
  | WhileLoopSpec :
      while_loop_spec node
  | Mode :
      mode node
  | ModeDefault :
      mode_default node
  | ModeIn :
      mode_in node
  | ModeInOut :
      mode_in_out node
  | ModeOut :
      mode_out node
  | MultiAbstractStateDecl :
      multi_abstract_state_decl node
  | NotNull :
      not_null node
  | NotNullAbsent :
      not_null_absent node
  | NotNullPresent :
      not_null_present node
  | NullComponentDecl :
      null_component_decl node
  | OthersDesignator :
      others_designator node
  | OverridingNode :
      overriding_node node
  | OverridingNotOverriding :
      overriding_not_overriding node
  | OverridingOverriding :
      overriding_overriding node
  | OverridingUnspecified :
      overriding_unspecified node
  | Params :
      params node
  | ParenAbstractStateDecl :
      paren_abstract_state_decl node
  | PpDirective :
      pp_directive node
  | PpElseDirective :
      pp_else_directive node
  | PpElsifDirective :
      pp_elsif_directive node
  | PpEndIfDirective :
      pp_end_if_directive node
  | PpIfDirective :
      pp_if_directive node
  | PpThenKw :
      pp_then_kw node
  | PragmaNode :
      pragma_node node
  | PrivateNode :
      private_node node
  | PrivateAbsent :
      private_absent node
  | PrivatePresent :
      private_present node
  | ProtectedDef :
      protected_def node
  | ProtectedNode :
      protected_node node
  | ProtectedAbsent :
      protected_absent node
  | ProtectedPresent :
      protected_present node
  | Quantifier :
      quantifier node
  | QuantifierAll :
      quantifier_all node
  | QuantifierSome :
      quantifier_some node
  | RangeSpec :
      range_spec node
  | RenamingClause :
      renaming_clause node
  | SyntheticRenamingClause :
      synthetic_renaming_clause node
  | ReverseNode :
      reverse_node node
  | ReverseAbsent :
      reverse_absent node
  | ReversePresent :
      reverse_present node
  | SelectWhenPart :
      select_when_part node
  | Stmt :
      stmt node
  | CompositeStmt :
      composite_stmt node
  | AcceptStmt :
      accept_stmt node
  | AcceptStmtWithStmts :
      accept_stmt_with_stmts node
  | BaseLoopStmt :
      base_loop_stmt node
  | ForLoopStmt :
      for_loop_stmt node
  | LoopStmt :
      loop_stmt node
  | WhileLoopStmt :
      while_loop_stmt node
  | BlockStmt :
      block_stmt node
  | BeginBlock :
      begin_block node
  | DeclBlock :
      decl_block node
  | CaseStmt :
      case_stmt node
  | ExtendedReturnStmt :
      extended_return_stmt node
  | IfStmt :
      if_stmt node
  | NamedStmt :
      named_stmt node
  | SelectStmt :
      select_stmt node
  | ErrorStmt :
      error_stmt node
  | SimpleStmt :
      simple_stmt node
  | AbortStmt :
      abort_stmt node
  | AssignStmt :
      assign_stmt node
  | CallStmt :
      call_stmt node
  | DelayStmt :
      delay_stmt node
  | ExitStmt :
      exit_stmt node
  | GotoStmt :
      goto_stmt node
  | Label :
      label node
  | NullStmt :
      null_stmt node
  | RaiseStmt :
      raise_stmt node
  | RequeueStmt :
      requeue_stmt node
  | ReturnStmt :
      return_stmt node
  | TerminateAlternative :
      terminate_alternative node
  | SubpKind :
      subp_kind node
  | SubpKindFunction :
      subp_kind_function node
  | SubpKindProcedure :
      subp_kind_procedure node
  | Subunit :
      subunit node
  | SynchronizedNode :
      synchronized_node node
  | SynchronizedAbsent :
      synchronized_absent node
  | SynchronizedPresent :
      synchronized_present node
  | TaggedNode :
      tagged_node node
  | TaggedAbsent :
      tagged_absent node
  | TaggedPresent :
      tagged_present node
  | TaskDef :
      task_def node
  | TypeAttributesRepository :
      type_attributes_repository node
  | TypeDef :
      type_def node
  | AccessDef :
      access_def node
  | AccessToSubpDef :
      access_to_subp_def node
  | BaseTypeAccessDef :
      base_type_access_def node
  | AnonymousTypeAccessDef :
      anonymous_type_access_def node
  | TypeAccessDef :
      type_access_def node
  | ArrayTypeDef :
      array_type_def node
  | DerivedTypeDef :
      derived_type_def node
  | EnumTypeDef :
      enum_type_def node
  | FormalDiscreteTypeDef :
      formal_discrete_type_def node
  | InterfaceTypeDef :
      interface_type_def node
  | ModIntTypeDef :
      mod_int_type_def node
  | PrivateTypeDef :
      private_type_def node
  | RealTypeDef :
      real_type_def node
  | DecimalFixedPointDef :
      decimal_fixed_point_def node
  | FloatingPointDef :
      floating_point_def node
  | OrdinaryFixedPointDef :
      ordinary_fixed_point_def node
  | RecordTypeDef :
      record_type_def node
  | SignedIntTypeDef :
      signed_int_type_def node
  | TypeExpr :
      type_expr node
  | AnonymousType :
      anonymous_type node
  | EnumLitSynthTypeExpr :
      enum_lit_synth_type_expr node
  | SubtypeIndication :
      subtype_indication node
  | ConstrainedSubtypeIndication :
      constrained_subtype_indication node
  | DiscreteSubtypeIndication :
      discrete_subtype_indication node
  | SyntheticTypeExpr :
      synthetic_type_expr node
  | UnconstrainedArrayIndex :
      unconstrained_array_index node
  | UntilNode :
      until_node node
  | UntilAbsent :
      until_absent node
  | UntilPresent :
      until_present node
  | UseClause :
      use_clause node
  | UsePackageClause :
      use_package_clause node
  | UseTypeClause :
      use_type_clause node
  | ValueSequence :
      value_sequence node
  | Variant :
      variant node
  | VariantPart :
      variant_part node
  | WithClause :
      with_clause node
  | WithPrivate :
      with_private node
  | WithPrivateAbsent :
      with_private_absent node
  | WithPrivatePresent :
      with_private_present node

module WithPrivatePresent : sig
  (**

  *)

  type t =
    [
      | `WithPrivatePresent of
          with_private_present_fields
    ]

  type fields = with_private_present_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module WithPrivateAbsent : sig
  (**

  *)

  type t =
    [
      | `WithPrivateAbsent of
          with_private_absent_fields
    ]

  type fields = with_private_absent_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module WithPrivate : sig
  (**
  * Qualifier for the ``private`` keyword in ``with private`` record clauses.
  *)

  type t =
    [
      | WithPrivateAbsent.t
      | WithPrivatePresent.t
    ]


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int


      
  val p_as_bool :
    [< with_private ]
    -> bool
  (**
  * Return whether this is an instance of WithPrivatePresent
  *)



end

module WithClause : sig
  (**
  * With clause (:rmlink:`10.1.2`).
  *)

  type t =
    [
      | `WithClause of
          with_clause_fields
    ]

  type fields = with_clause_fields =
    
  {
         
    f_has_limited: limited_node
    Lazy.t;
         
    f_has_private: private_node
    Lazy.t;
         
    f_packages: name_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_has_limited :
    [< with_clause]
    -> limited_node

      
  val f_has_private :
    [< with_clause]
    -> private_node

      
  val f_packages :
    [< with_clause]
    -> name_list


end

module VariantPart : sig
  (**
  * Variant part in a discriminated type record declaration (:rmlink:`3.8.1`).
  *
  * This corresponds to the whole ``case ... is ... end case;`` block.
  *)

  type t =
    [
      | `VariantPart of
          variant_part_fields
    ]

  type fields = variant_part_fields =
    
  {
         
    f_discr_name: identifier
    Lazy.t;
         
    f_variant: variant_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_discr_name :
    [< variant_part]
    -> identifier

      
  val f_variant :
    [< variant_part]
    -> variant_list


end

module Variant : sig
  (**
  * Single variant in a discriminated type record declaration.
  *
  * This corresponds to a ``when ... => ...`` section in a variant part.
  *)

  type t =
    [
      | `Variant of
          variant_fields
    ]

  type fields = variant_fields =
    
  {
         
    f_choices: alternatives_list
    Lazy.t;
         
    f_components: component_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_choices :
    [< variant]
    -> alternatives_list

      
  val f_components :
    [< variant]
    -> component_list


end

module ValueSequence : sig
  (**
  * The value sequence of a reduction expression (see ``ReduceAttributeRef``).
  * Ada 2022, RM 4.5.10.
  *)

  type t =
    [
      | `ValueSequence of
          value_sequence_fields
    ]

  type fields = value_sequence_fields =
    
  {
         
    f_iter_assoc: iterated_assoc
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_iter_assoc :
    [< value_sequence]
    -> iterated_assoc


end

module UseTypeClause : sig
  (**
  * Use clause for types (:rmlink:`8.4`).
  *)

  type t =
    [
      | `UseTypeClause of
          use_type_clause_fields
    ]

  type fields = use_type_clause_fields =
    
  {
         
    f_has_all: all_node
    Lazy.t;
         
    f_types: name_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_has_all :
    [< use_type_clause]
    -> all_node

      
  val f_types :
    [< use_type_clause]
    -> name_list


end

module UsePackageClause : sig
  (**
  * Use clause for packages (:rmlink:`8.4`).
  *)

  type t =
    [
      | `UsePackageClause of
          use_package_clause_fields
    ]

  type fields = use_package_clause_fields =
    
  {
         
    f_packages: name_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_packages :
    [< use_package_clause]
    -> name_list


end

module UseClause : sig
  (**
  * Base class for use clauses (:rmlink:`10.1.2`).
  *)

  type t =
    [
      | UsePackageClause.t
      | UseTypeClause.t
    ]


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module UntilPresent : sig
  (**

  *)

  type t =
    [
      | `UntilPresent of
          until_present_fields
    ]

  type fields = until_present_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module UntilAbsent : sig
  (**

  *)

  type t =
    [
      | `UntilAbsent of
          until_absent_fields
    ]

  type fields = until_absent_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module UntilNode : sig
  (**
  * Qualifier for the ``until`` keyword.
  *)

  type t =
    [
      | UntilAbsent.t
      | UntilPresent.t
    ]


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int


      
  val p_as_bool :
    [< until_node ]
    -> bool
  (**
  * Return whether this is an instance of UntilPresent
  *)



end

module UnconstrainedArrayIndex : sig
  (**
  * List of unconstrained array indexes.
  *)

  type t =
    [
      | `UnconstrainedArrayIndex of
          unconstrained_array_index_fields
    ]

  type fields = unconstrained_array_index_fields =
    
  {
         
    f_subtype_indication: subtype_indication
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_subtype_indication :
    [< unconstrained_array_index]
    -> subtype_indication


end

module SyntheticTypeExpr : sig
  (**
  * Synthetic type expression. The designated type is already known at
  * instantiation time and is to be given in the ``target_type`` field.
  *)

  type t =
    [
      | `SyntheticTypeExpr of
          synthetic_type_expr_fields
    ]

  type fields = synthetic_type_expr_fields =
    
  {
         
    f_target_type: base_type_decl
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_target_type :
    [< synthetic_type_expr]
    -> base_type_decl


end

module DiscreteSubtypeIndication : sig
  (**
  * Reference to a type with a general constraint.
  *)

  type t =
    [
      | `DiscreteSubtypeIndication of
          discrete_subtype_indication_fields
    ]

  type fields = discrete_subtype_indication_fields =
    
  {
         
    f_has_not_null: not_null
    Lazy.t;
         
    f_name: [
      | `AttributeRef
          of attribute_ref_fields
      | `CharLiteral
          of char_literal_fields
      | `DottedName
          of dotted_name_fields
      | `Identifier
          of identifier_fields
      | `StringLiteral
          of string_literal_fields
    ]
    Lazy.t;
         
    f_constraint: constraint_node
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_has_not_null :
    [< discrete_subtype_indication]
    -> not_null

      
  val f_name :
    [< discrete_subtype_indication]
    -> [attribute_ref | char_literal | dotted_name | identifier | string_literal]

      
  val f_constraint :
    [< discrete_subtype_indication]
    -> constraint_node option


end

module ConstrainedSubtypeIndication : sig
  (**
  * Reference to a type with a range constraint.
  *)

  type t =
    [
      | `ConstrainedSubtypeIndication of
          constrained_subtype_indication_fields
    ]

  type fields = constrained_subtype_indication_fields =
    
  {
         
    f_has_not_null: not_null
    Lazy.t;
         
    f_name: [
      | `AttributeRef
          of attribute_ref_fields
      | `CharLiteral
          of char_literal_fields
      | `DottedName
          of dotted_name_fields
      | `Identifier
          of identifier_fields
      | `StringLiteral
          of string_literal_fields
    ]
    Lazy.t;
         
    f_constraint: constraint_node
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_has_not_null :
    [< constrained_subtype_indication]
    -> not_null

      
  val f_name :
    [< constrained_subtype_indication]
    -> [attribute_ref | char_literal | dotted_name | identifier | string_literal]

      
  val f_constraint :
    [< constrained_subtype_indication]
    -> constraint_node option


end

module SubtypeIndication : sig
  (**
  * Reference to a type by name (:rmlink:`3.2.2`).
  *)

  type t =
    [
      | `SubtypeIndication of
          subtype_indication_fields
      | `ConstrainedSubtypeIndication of
          constrained_subtype_indication_fields
      | `DiscreteSubtypeIndication of
          discrete_subtype_indication_fields
    ]

  type fields = subtype_indication_fields =
    
  {
         
    f_has_not_null: not_null
    Lazy.t;
         
    f_name: [
      | `AttributeRef
          of attribute_ref_fields
      | `CharLiteral
          of char_literal_fields
      | `DottedName
          of dotted_name_fields
      | `Identifier
          of identifier_fields
      | `StringLiteral
          of string_literal_fields
    ]
    Lazy.t;
         
    f_constraint: constraint_node
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int


      
  val p_subtype_constraints :
    [< subtype_indication ]
    -> ParamActual.t list
  (**
  * Returns an array of pairs, associating formal parameters to actual or
  * default expressions.
  *)

      
  val p_is_static_subtype :
    ?imprecise_fallback:
    bool
    -> [< subtype_indication ]
    -> bool
  (**
  * Returns whether Self denotes a static subtype or not.
  *)


      
  val f_has_not_null :
    [< subtype_indication]
    -> not_null

      
  val f_name :
    [< subtype_indication]
    -> [attribute_ref | char_literal | dotted_name | identifier | string_literal]

      
  val f_constraint :
    [< subtype_indication]
    -> constraint_node option


end

module EnumLitSynthTypeExpr : sig
  (**
  * Synthetic node. Represents the type expression for an enum literal.
  *)

  type t =
    [
      | `EnumLitSynthTypeExpr of
          enum_lit_synth_type_expr_fields
    ]

  type fields = enum_lit_synth_type_expr_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module AnonymousType : sig
  (**
  * Container for inline anonymous array and access types declarations.
  *)

  type t =
    [
      | `AnonymousType of
          anonymous_type_fields
    ]

  type fields = anonymous_type_fields =
    
  {
         
    f_type_decl: anonymous_type_decl
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_type_decl :
    [< anonymous_type]
    -> anonymous_type_decl


end

module TypeExpr : sig
  (**
  * A type expression is an abstract node that embodies the concept of a
  * reference to a type.
  *
  * Since Ada has both subtype_indications and anonymous (inline) type
  * declarations, a type expression contains one or the other.
  *
  * This node has no ARM correspondence.
  *)

  type t =
    [
      | AnonymousType.t
      | EnumLitSynthTypeExpr.t
      | SubtypeIndication.t
      | SyntheticTypeExpr.t
    ]


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int


      
  val p_type_name :
    [< type_expr ]
    -> name option
  (**
  * Return the name node for this type expression, if applicable, else null
  *)

      
  val p_designated_type_decl :
    [< type_expr ]
    -> base_type_decl option
  (**
  * Returns the type declaration designated by this type expression.
  *)

      
  val p_designated_type_decl_from :
    [< type_expr ]
    -> ada_node
    -> base_type_decl option
  (**
  * Return the type declaration designated by this type expression as viewed
  * from the node given by origin_node.
  *)



end

module SignedIntTypeDef : sig
  (**
  * Type definition for a signed integer type (:rmlink:`3.5.4`).
  *)

  type t =
    [
      | `SignedIntTypeDef of
          signed_int_type_def_fields
    ]

  type fields = signed_int_type_def_fields =
    
  {
         
    f_range: range_spec
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_range :
    [< signed_int_type_def]
    -> range_spec


end

module RecordTypeDef : sig
  (**
  * Type definition for a record (:rmlink:`3.8`).
  *)

  type t =
    [
      | `RecordTypeDef of
          record_type_def_fields
    ]

  type fields = record_type_def_fields =
    
  {
         
    f_has_abstract: abstract_node
    Lazy.t;
         
    f_has_tagged: tagged_node
    Lazy.t;
         
    f_has_limited: limited_node
    Lazy.t;
         
    f_record_def: base_record_def
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_has_abstract :
    [< record_type_def]
    -> abstract_node

      
  val f_has_tagged :
    [< record_type_def]
    -> tagged_node

      
  val f_has_limited :
    [< record_type_def]
    -> limited_node

      
  val f_record_def :
    [< record_type_def]
    -> base_record_def


end

module OrdinaryFixedPointDef : sig
  (**
  * Type definition for ordinary fixed-point numbers (:rmlink:`3.5.9`).
  *)

  type t =
    [
      | `OrdinaryFixedPointDef of
          ordinary_fixed_point_def_fields
    ]

  type fields = ordinary_fixed_point_def_fields =
    
  {
         
    f_delta: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BoxExpr
          of box_expr_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
         
    f_range: range_spec
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_delta :
    [< ordinary_fixed_point_def]
    -> [allocator | attribute_ref | base_aggregate | bin_op | box_expr | call_expr | char_literal | concat_op | cond_expr | decl_expr | dotted_name | explicit_deref | identifier | null_literal | num_literal | paren_expr | qual_expr | quantified_expr | raise_expr | reduce_attribute_ref | string_literal | target_name | un_op | update_attribute_ref]

      
  val f_range :
    [< ordinary_fixed_point_def]
    -> range_spec option


end

module FloatingPointDef : sig
  (**
  * Type definition for floating-point numbers (:rmlink:`3.5.7`).
  *)

  type t =
    [
      | `FloatingPointDef of
          floating_point_def_fields
    ]

  type fields = floating_point_def_fields =
    
  {
         
    f_num_digits: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BoxExpr
          of box_expr_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
         
    f_range: range_spec
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_num_digits :
    [< floating_point_def]
    -> [allocator | attribute_ref | base_aggregate | bin_op | box_expr | call_expr | char_literal | concat_op | cond_expr | decl_expr | dotted_name | explicit_deref | identifier | null_literal | num_literal | paren_expr | qual_expr | quantified_expr | raise_expr | reduce_attribute_ref | string_literal | target_name | un_op | update_attribute_ref]

      
  val f_range :
    [< floating_point_def]
    -> range_spec option


end

module DecimalFixedPointDef : sig
  (**
  * Type definition for decimal fixed-point numbers (:rmlink:`3.5.9`).
  *)

  type t =
    [
      | `DecimalFixedPointDef of
          decimal_fixed_point_def_fields
    ]

  type fields = decimal_fixed_point_def_fields =
    
  {
         
    f_delta: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BoxExpr
          of box_expr_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
         
    f_digits: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BoxExpr
          of box_expr_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
         
    f_range: range_spec
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_delta :
    [< decimal_fixed_point_def]
    -> [allocator | attribute_ref | base_aggregate | bin_op | box_expr | call_expr | char_literal | concat_op | cond_expr | decl_expr | dotted_name | explicit_deref | identifier | null_literal | num_literal | paren_expr | qual_expr | quantified_expr | raise_expr | reduce_attribute_ref | string_literal | target_name | un_op | update_attribute_ref]

      
  val f_digits :
    [< decimal_fixed_point_def]
    -> [allocator | attribute_ref | base_aggregate | bin_op | box_expr | call_expr | char_literal | concat_op | cond_expr | decl_expr | dotted_name | explicit_deref | identifier | null_literal | num_literal | paren_expr | qual_expr | quantified_expr | raise_expr | reduce_attribute_ref | string_literal | target_name | un_op | update_attribute_ref]

      
  val f_range :
    [< decimal_fixed_point_def]
    -> range_spec option


end

module RealTypeDef : sig
  (**
  * Type definition for real numbers (:rmlink:`3.5.6`).
  *)

  type t =
    [
      | DecimalFixedPointDef.t
      | FloatingPointDef.t
      | OrdinaryFixedPointDef.t
    ]


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module PrivateTypeDef : sig
  (**
  * Type definition for a private type.
  *
  * Libadalang diverges from the ARM here, treating private types like regular
  * type declarations that have an embedded type definition. This type
  * definition hence corresponds to :rmlink:`7.3`.
  *)

  type t =
    [
      | `PrivateTypeDef of
          private_type_def_fields
    ]

  type fields = private_type_def_fields =
    
  {
         
    f_has_abstract: abstract_node
    Lazy.t;
         
    f_has_tagged: tagged_node
    Lazy.t;
         
    f_has_limited: limited_node
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_has_abstract :
    [< private_type_def]
    -> abstract_node

      
  val f_has_tagged :
    [< private_type_def]
    -> tagged_node

      
  val f_has_limited :
    [< private_type_def]
    -> limited_node


end

module ModIntTypeDef : sig
  (**
  * Type definition for a modular integer type (:rmlink:`3.5.4`).
  *)

  type t =
    [
      | `ModIntTypeDef of
          mod_int_type_def_fields
    ]

  type fields = mod_int_type_def_fields =
    
  {
         
    f_expr: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BoxExpr
          of box_expr_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_expr :
    [< mod_int_type_def]
    -> [allocator | attribute_ref | base_aggregate | bin_op | box_expr | call_expr | char_literal | concat_op | cond_expr | decl_expr | dotted_name | explicit_deref | identifier | null_literal | num_literal | paren_expr | qual_expr | quantified_expr | raise_expr | reduce_attribute_ref | string_literal | target_name | un_op | update_attribute_ref]


end

module InterfaceTypeDef : sig
  (**
  * Type definition for an interface (:rmlink:`3.9.4`).
  *)

  type t =
    [
      | `InterfaceTypeDef of
          interface_type_def_fields
    ]

  type fields = interface_type_def_fields =
    
  {
         
    f_interface_kind: interface_kind
    option
    Lazy.t;
         
    f_interfaces: parent_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_interface_kind :
    [< interface_type_def]
    -> interface_kind option

      
  val f_interfaces :
    [< interface_type_def]
    -> parent_list


end

module FormalDiscreteTypeDef : sig
  (**
  * Type definition for discrete types in generic formals (:rmlink:`12.5.2`).
  *)

  type t =
    [
      | `FormalDiscreteTypeDef of
          formal_discrete_type_def_fields
    ]

  type fields = formal_discrete_type_def_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module EnumTypeDef : sig
  (**
  * Type definition for enumerations (:rmlink:`3.5.1`).
  *)

  type t =
    [
      | `EnumTypeDef of
          enum_type_def_fields
    ]

  type fields = enum_type_def_fields =
    
  {
         
    f_enum_literals: enum_literal_decl_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_enum_literals :
    [< enum_type_def]
    -> enum_literal_decl_list


end

module DerivedTypeDef : sig
  (**
  * Type definition for a derived type (:rmlink:`3.4`).
  *)

  type t =
    [
      | `DerivedTypeDef of
          derived_type_def_fields
    ]

  type fields = derived_type_def_fields =
    
  {
         
    f_has_abstract: abstract_node
    Lazy.t;
         
    f_has_limited: limited_node
    Lazy.t;
         
    f_has_synchronized: synchronized_node
    Lazy.t;
         
    f_subtype_indication: subtype_indication
    Lazy.t;
         
    f_interfaces: parent_list
    Lazy.t;
         
    f_record_extension: base_record_def
    option
    Lazy.t;
         
    f_has_with_private: with_private
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_has_abstract :
    [< derived_type_def]
    -> abstract_node

      
  val f_has_limited :
    [< derived_type_def]
    -> limited_node

      
  val f_has_synchronized :
    [< derived_type_def]
    -> synchronized_node

      
  val f_subtype_indication :
    [< derived_type_def]
    -> subtype_indication

      
  val f_interfaces :
    [< derived_type_def]
    -> parent_list

      
  val f_record_extension :
    [< derived_type_def]
    -> base_record_def option

      
  val f_has_with_private :
    [< derived_type_def]
    -> with_private


end

module ArrayTypeDef : sig
  (**
  * Type definition for an array (:rmlink:`3.6`).
  *)

  type t =
    [
      | `ArrayTypeDef of
          array_type_def_fields
    ]

  type fields = array_type_def_fields =
    
  {
         
    f_indices: array_indices
    Lazy.t;
         
    f_component_type: component_def
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_indices :
    [< array_type_def]
    -> array_indices

      
  val f_component_type :
    [< array_type_def]
    -> component_def


end

module TypeAccessDef : sig
  (**
  * Syntactic type definition for accesses.
  *)

  type t =
    [
      | `TypeAccessDef of
          type_access_def_fields
    ]

  type fields = type_access_def_fields =
    
  {
         
    f_has_not_null: not_null
    Lazy.t;
         
    f_has_all: all_node
    Lazy.t;
         
    f_has_constant: constant_node
    Lazy.t;
         
    f_subtype_indication: subtype_indication
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_has_not_null :
    [< type_access_def]
    -> not_null

      
  val f_has_all :
    [< type_access_def]
    -> all_node

      
  val f_has_constant :
    [< type_access_def]
    -> constant_node

      
  val f_subtype_indication :
    [< type_access_def]
    -> subtype_indication


end

module AnonymousTypeAccessDef : sig
  (**
  * Synthetic type access, that will directly reference a type decl. It is used
  * to generate synthetic anonymous access types.
  *)

  type t =
    [
      | `AnonymousTypeAccessDef of
          anonymous_type_access_def_fields
    ]

  type fields = anonymous_type_access_def_fields =
    
  {
         
    f_has_not_null: not_null
    Lazy.t;
         
    f_type_decl: base_type_decl
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_has_not_null :
    [< anonymous_type_access_def]
    -> not_null

      
  val f_type_decl :
    [< anonymous_type_access_def]
    -> base_type_decl


end

module BaseTypeAccessDef : sig
  (**
  * Base class for access type definitions (:rmlink:`3.10`).
  *)

  type t =
    [
      | AnonymousTypeAccessDef.t
      | TypeAccessDef.t
    ]


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_has_not_null :
    [< base_type_access_def]
    -> not_null


end

module AccessToSubpDef : sig
  (**
  * Type definition for accesses to subprograms (:rmlink:`3.10`).
  *)

  type t =
    [
      | `AccessToSubpDef of
          access_to_subp_def_fields
    ]

  type fields = access_to_subp_def_fields =
    
  {
         
    f_has_not_null: not_null
    Lazy.t;
         
    f_has_protected: protected_node
    Lazy.t;
         
    f_subp_spec: subp_spec
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_has_not_null :
    [< access_to_subp_def]
    -> not_null

      
  val f_has_protected :
    [< access_to_subp_def]
    -> protected_node

      
  val f_subp_spec :
    [< access_to_subp_def]
    -> subp_spec


end

module AccessDef : sig
  (**
  * Base class for access type definitions (:rmlink:`3.10`).
  *)

  type t =
    [
      | AccessToSubpDef.t
      | BaseTypeAccessDef.t
    ]


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_has_not_null :
    [< access_def]
    -> not_null


end

module TypeDef : sig
  (**
  * Base class for type definitions (:rmlink:`3.2.1`).
  *)

  type t =
    [
      | AccessDef.t
      | ArrayTypeDef.t
      | DerivedTypeDef.t
      | EnumTypeDef.t
      | FormalDiscreteTypeDef.t
      | InterfaceTypeDef.t
      | ModIntTypeDef.t
      | PrivateTypeDef.t
      | RealTypeDef.t
      | RecordTypeDef.t
      | SignedIntTypeDef.t
    ]


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module TypeAttributesRepository : sig
  (**
  * Synthetic node that contains the lazy fields for the attribute subprograms
  * of a given type. The lazy fields are not directly on the BaseTypeDecl node
  * itself to minimize its size in memory: with this indirection, a type for
  * which no function attribute is ever synthesized will not waste any memory.
  *)

  type t =
    [
      | `TypeAttributesRepository of
          type_attributes_repository_fields
    ]

  type fields = type_attributes_repository_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module TaskDef : sig
  (**
  * Type definition for a task type (:rmlink:`9.1`).
  *)

  type t =
    [
      | `TaskDef of
          task_def_fields
    ]

  type fields = task_def_fields =
    
  {
         
    f_interfaces: parent_list
    Lazy.t;
         
    f_public_part: public_part
    Lazy.t;
         
    f_private_part: private_part
    option
    Lazy.t;
         
    f_end_name: end_name
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_interfaces :
    [< task_def]
    -> parent_list

      
  val f_public_part :
    [< task_def]
    -> public_part

      
  val f_private_part :
    [< task_def]
    -> private_part option

      
  val f_end_name :
    [< task_def]
    -> end_name option


end

module TaggedPresent : sig
  (**

  *)

  type t =
    [
      | `TaggedPresent of
          tagged_present_fields
    ]

  type fields = tagged_present_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module TaggedAbsent : sig
  (**

  *)

  type t =
    [
      | `TaggedAbsent of
          tagged_absent_fields
    ]

  type fields = tagged_absent_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module TaggedNode : sig
  (**
  * Qualifier for the ``tagged`` keyword.
  *)

  type t =
    [
      | TaggedAbsent.t
      | TaggedPresent.t
    ]


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int


      
  val p_as_bool :
    [< tagged_node ]
    -> bool
  (**
  * Return whether this is an instance of TaggedPresent
  *)



end

module SynchronizedPresent : sig
  (**

  *)

  type t =
    [
      | `SynchronizedPresent of
          synchronized_present_fields
    ]

  type fields = synchronized_present_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module SynchronizedAbsent : sig
  (**

  *)

  type t =
    [
      | `SynchronizedAbsent of
          synchronized_absent_fields
    ]

  type fields = synchronized_absent_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module SynchronizedNode : sig
  (**
  * Qualifier for the ``synchronized`` keyword.
  *)

  type t =
    [
      | SynchronizedAbsent.t
      | SynchronizedPresent.t
    ]


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int


      
  val p_as_bool :
    [< synchronized_node ]
    -> bool
  (**
  * Return whether this is an instance of SynchronizedPresent
  *)



end

module Subunit : sig
  (**
  * Subunit (``separate``) (:rmlink:`10.1.3`).
  *)

  type t =
    [
      | `Subunit of
          subunit_fields
    ]

  type fields = subunit_fields =
    
  {
         
    f_name: [
      | `CharLiteral
          of char_literal_fields
      | `DottedName
          of dotted_name_fields
      | `Identifier
          of identifier_fields
      | `StringLiteral
          of string_literal_fields
    ]
    Lazy.t;
         
    f_body: [
      | `PackageBody
          of package_body_fields
      | `ProtectedBody
          of protected_body_fields
      | `SubpBody
          of subp_body_fields
      | `TaskBody
          of task_body_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int


      
  val p_body_root :
    [< subunit ]
    -> basic_decl option
  (**
  * Return the body in which this subunit is rooted.
  *)


      
  val f_name :
    [< subunit]
    -> [char_literal | dotted_name | identifier | string_literal]

      
  val f_body :
    [< subunit]
    -> [package_body | protected_body | subp_body | task_body]


end

module SubpKindProcedure : sig
  (**

  *)

  type t =
    [
      | `SubpKindProcedure of
          subp_kind_procedure_fields
    ]

  type fields = subp_kind_procedure_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module SubpKindFunction : sig
  (**

  *)

  type t =
    [
      | `SubpKindFunction of
          subp_kind_function_fields
    ]

  type fields = subp_kind_function_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module SubpKind : sig
  (**
  * Qualifier for a subprogram kind.
  *)

  type t =
    [
      | SubpKindFunction.t
      | SubpKindProcedure.t
    ]


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module TerminateAlternative : sig
  (**
  * ``terminate`` alternative in a ``select`` statement (:rmlink:`9.7`).
  *)

  type t =
    [
      | `TerminateAlternative of
          terminate_alternative_fields
    ]

  type fields = terminate_alternative_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module ReturnStmt : sig
  (**
  * ``return`` statement (:rmlink:`6.5`).
  *)

  type t =
    [
      | `ReturnStmt of
          return_stmt_fields
    ]

  type fields = return_stmt_fields =
    
  {
         
    f_return_expr: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_return_expr :
    [< return_stmt]
    -> [allocator | attribute_ref | base_aggregate | bin_op | call_expr | char_literal | concat_op | cond_expr | decl_expr | dotted_name | explicit_deref | identifier | membership_expr | null_literal | num_literal | paren_expr | qual_expr | quantified_expr | raise_expr | reduce_attribute_ref | string_literal | target_name | un_op | update_attribute_ref] option


end

module RequeueStmt : sig
  (**
  * ``requeue`` statement (:rmlink:`9.5.4`).
  *)

  type t =
    [
      | `RequeueStmt of
          requeue_stmt_fields
    ]

  type fields = requeue_stmt_fields =
    
  {
         
    f_call_name: [
      | `AttributeRef
          of attribute_ref_fields
      | `CallExpr
          of call_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `QualExpr
          of qual_expr_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
         
    f_has_abort: abort_node
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_call_name :
    [< requeue_stmt]
    -> [attribute_ref | call_expr | char_literal | dotted_name | explicit_deref | identifier | qual_expr | reduce_attribute_ref | string_literal | target_name | update_attribute_ref]

      
  val f_has_abort :
    [< requeue_stmt]
    -> abort_node


end

module RaiseStmt : sig
  (**
  * ``raise`` statement (:rmlink:`11.3`).
  *)

  type t =
    [
      | `RaiseStmt of
          raise_stmt_fields
    ]

  type fields = raise_stmt_fields =
    
  {
         
    f_exception_name: [
      | `AttributeRef
          of attribute_ref_fields
      | `CallExpr
          of call_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `QualExpr
          of qual_expr_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    option
    Lazy.t;
         
    f_error_message: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_exception_name :
    [< raise_stmt]
    -> [attribute_ref | call_expr | char_literal | dotted_name | explicit_deref | identifier | qual_expr | reduce_attribute_ref | string_literal | target_name | update_attribute_ref] option

      
  val f_error_message :
    [< raise_stmt]
    -> [allocator | attribute_ref | base_aggregate | bin_op | call_expr | char_literal | concat_op | cond_expr | decl_expr | dotted_name | explicit_deref | identifier | membership_expr | null_literal | num_literal | paren_expr | qual_expr | quantified_expr | raise_expr | reduce_attribute_ref | string_literal | target_name | un_op | update_attribute_ref] option


end

module NullStmt : sig
  (**
  * ``null;`` statement (:rmlink:`5.1`).
  *)

  type t =
    [
      | `NullStmt of
          null_stmt_fields
    ]

  type fields = null_stmt_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module Label : sig
  (**
  * Statement to declare a code label (:rmlink:`5.1`).
  *)

  type t =
    [
      | `Label of
          label_fields
    ]

  type fields = label_fields =
    
  {
         
    f_decl: label_decl
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_decl :
    [< label]
    -> label_decl


end

module GotoStmt : sig
  (**
  * ``goto`` statement (:rmlink:`5.8`).
  *)

  type t =
    [
      | `GotoStmt of
          goto_stmt_fields
    ]

  type fields = goto_stmt_fields =
    
  {
         
    f_label_name: [
      | `CharLiteral
          of char_literal_fields
      | `DottedName
          of dotted_name_fields
      | `Identifier
          of identifier_fields
      | `StringLiteral
          of string_literal_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_label_name :
    [< goto_stmt]
    -> [char_literal | dotted_name | identifier | string_literal]


end

module ExitStmt : sig
  (**
  * ``exit`` statement (:rmlink:`5.7`).
  *)

  type t =
    [
      | `ExitStmt of
          exit_stmt_fields
    ]

  type fields = exit_stmt_fields =
    
  {
         
    f_loop_name: [
      | `CharLiteral
          of char_literal_fields
      | `DottedName
          of dotted_name_fields
      | `Identifier
          of identifier_fields
      | `StringLiteral
          of string_literal_fields
    ]
    option
    Lazy.t;
         
    f_cond_expr: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_loop_name :
    [< exit_stmt]
    -> [char_literal | dotted_name | identifier | string_literal] option

      
  val f_cond_expr :
    [< exit_stmt]
    -> [allocator | attribute_ref | base_aggregate | bin_op | call_expr | char_literal | concat_op | cond_expr | decl_expr | dotted_name | explicit_deref | identifier | membership_expr | null_literal | num_literal | paren_expr | qual_expr | quantified_expr | raise_expr | reduce_attribute_ref | string_literal | target_name | un_op | update_attribute_ref] option


end

module DelayStmt : sig
  (**
  * ``delay`` statement (:rmlink:`9.6`).
  *)

  type t =
    [
      | `DelayStmt of
          delay_stmt_fields
    ]

  type fields = delay_stmt_fields =
    
  {
         
    f_has_until: until_node
    Lazy.t;
         
    f_expr: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_has_until :
    [< delay_stmt]
    -> until_node

      
  val f_expr :
    [< delay_stmt]
    -> [allocator | attribute_ref | base_aggregate | bin_op | call_expr | char_literal | concat_op | cond_expr | decl_expr | dotted_name | explicit_deref | identifier | membership_expr | null_literal | num_literal | paren_expr | qual_expr | quantified_expr | raise_expr | reduce_attribute_ref | string_literal | target_name | un_op | update_attribute_ref]


end

module CallStmt : sig
  (**
  * Statement for entry or procedure calls (:rmlink:`6.4`).
  *)

  type t =
    [
      | `CallStmt of
          call_stmt_fields
    ]

  type fields = call_stmt_fields =
    
  {
         
    f_call: [
      | `AttributeRef
          of attribute_ref_fields
      | `CallExpr
          of call_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `QualExpr
          of qual_expr_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_call :
    [< call_stmt]
    -> [attribute_ref | call_expr | char_literal | dotted_name | explicit_deref | identifier | qual_expr | reduce_attribute_ref | string_literal | target_name | update_attribute_ref]


end

module AssignStmt : sig
  (**
  * Statement for assignments (:rmlink:`5.2`).
  *)

  type t =
    [
      | `AssignStmt of
          assign_stmt_fields
    ]

  type fields = assign_stmt_fields =
    
  {
         
    f_dest: [
      | `AttributeRef
          of attribute_ref_fields
      | `CallExpr
          of call_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `QualExpr
          of qual_expr_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
         
    f_expr: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_dest :
    [< assign_stmt]
    -> [attribute_ref | call_expr | char_literal | dotted_name | explicit_deref | identifier | qual_expr | reduce_attribute_ref | string_literal | target_name | update_attribute_ref]

      
  val f_expr :
    [< assign_stmt]
    -> [allocator | attribute_ref | base_aggregate | bin_op | call_expr | char_literal | concat_op | cond_expr | decl_expr | dotted_name | explicit_deref | identifier | membership_expr | null_literal | num_literal | paren_expr | qual_expr | quantified_expr | raise_expr | reduce_attribute_ref | string_literal | target_name | un_op | update_attribute_ref]


end

module AbortStmt : sig
  (**
  * ``abort`` statement (:rmlink:`9.8`).
  *)

  type t =
    [
      | `AbortStmt of
          abort_stmt_fields
    ]

  type fields = abort_stmt_fields =
    
  {
         
    f_names: name_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_names :
    [< abort_stmt]
    -> name_list


end

module SimpleStmt : sig
  (**
  * Base class for simple statements (:rmlink:`5.1`).
  *)

  type t =
    [
      | AbortStmt.t
      | AssignStmt.t
      | CallStmt.t
      | DelayStmt.t
      | ExitStmt.t
      | GotoStmt.t
      | Label.t
      | NullStmt.t
      | RaiseStmt.t
      | RequeueStmt.t
      | ReturnStmt.t
      | TerminateAlternative.t
    ]


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module ErrorStmt : sig
  (**
  * Placeholder node for syntax errors in lists of statements.
  *)

  type t =
    [
      | `ErrorStmt of
          error_stmt_fields
    ]

  type fields = error_stmt_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module SelectStmt : sig
  (**
  * ``select`` statements block (:rmlink:`9.7`).
  *)

  type t =
    [
      | `SelectStmt of
          select_stmt_fields
    ]

  type fields = select_stmt_fields =
    
  {
         
    f_guards: select_when_part_list
    Lazy.t;
         
    f_else_stmts: stmt_list
    Lazy.t;
         
    f_abort_stmts: stmt_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_guards :
    [< select_stmt]
    -> select_when_part_list

      
  val f_else_stmts :
    [< select_stmt]
    -> stmt_list

      
  val f_abort_stmts :
    [< select_stmt]
    -> stmt_list


end

module NamedStmt : sig
  (**
  * Wrapper class, used for composite statements that can be named (declare
  * blocks, loops). This allows to both have a BasicDecl for the named entity
  * declared, and a CompositeStmt for the statement hierarchy.
  *)

  type t =
    [
      | `NamedStmt of
          named_stmt_fields
    ]

  type fields = named_stmt_fields =
    
  {
         
    f_decl: named_stmt_decl
    Lazy.t;
         
    f_stmt: [
      | `BeginBlock
          of begin_block_fields
      | `DeclBlock
          of decl_block_fields
      | `ForLoopStmt
          of for_loop_stmt_fields
      | `LoopStmt
          of loop_stmt_fields
      | `WhileLoopStmt
          of while_loop_stmt_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_decl :
    [< named_stmt]
    -> named_stmt_decl

      
  val f_stmt :
    [< named_stmt]
    -> [base_loop_stmt | block_stmt]


end

module IfStmt : sig
  (**
  * ``if`` statement block (:rmlink:`5.3`).
  *)

  type t =
    [
      | `IfStmt of
          if_stmt_fields
    ]

  type fields = if_stmt_fields =
    
  {
         
    f_cond_expr: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
         
    f_then_stmts: stmt_list
    Lazy.t;
         
    f_alternatives: elsif_stmt_part_list
    Lazy.t;
         
    f_else_stmts: stmt_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_cond_expr :
    [< if_stmt]
    -> [allocator | attribute_ref | base_aggregate | bin_op | call_expr | char_literal | concat_op | cond_expr | decl_expr | dotted_name | explicit_deref | identifier | membership_expr | null_literal | num_literal | paren_expr | qual_expr | quantified_expr | raise_expr | reduce_attribute_ref | string_literal | target_name | un_op | update_attribute_ref]

      
  val f_then_stmts :
    [< if_stmt]
    -> stmt_list

      
  val f_alternatives :
    [< if_stmt]
    -> elsif_stmt_part_list

      
  val f_else_stmts :
    [< if_stmt]
    -> stmt_list


end

module ExtendedReturnStmt : sig
  (**
  * Extended ``return`` statement (:rmlink:`6.5`).
  *)

  type t =
    [
      | `ExtendedReturnStmt of
          extended_return_stmt_fields
    ]

  type fields = extended_return_stmt_fields =
    
  {
         
    f_decl: extended_return_stmt_object_decl
    Lazy.t;
         
    f_stmts: handled_stmts
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_decl :
    [< extended_return_stmt]
    -> extended_return_stmt_object_decl

      
  val f_stmts :
    [< extended_return_stmt]
    -> handled_stmts option


end

module CaseStmt : sig
  (**
  * ``case`` statement (:rmlink:`5.4`).
  *)

  type t =
    [
      | `CaseStmt of
          case_stmt_fields
    ]

  type fields = case_stmt_fields =
    
  {
         
    f_expr: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
         
    f_pragmas: pragma_node_list
    Lazy.t;
         
    f_alternatives: case_stmt_alternative_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_expr :
    [< case_stmt]
    -> [allocator | attribute_ref | base_aggregate | bin_op | call_expr | char_literal | concat_op | cond_expr | decl_expr | dotted_name | explicit_deref | identifier | membership_expr | null_literal | num_literal | paren_expr | qual_expr | quantified_expr | raise_expr | reduce_attribute_ref | string_literal | target_name | un_op | update_attribute_ref]

      
  val f_pragmas :
    [< case_stmt]
    -> pragma_node_list

      
  val f_alternatives :
    [< case_stmt]
    -> case_stmt_alternative_list


end

module DeclBlock : sig
  (**
  * Statement block with a declarative part (:rmlink:`5.6`).
  *)

  type t =
    [
      | `DeclBlock of
          decl_block_fields
    ]

  type fields = decl_block_fields =
    
  {
         
    f_decls: declarative_part
    Lazy.t;
         
    f_stmts: handled_stmts
    Lazy.t;
         
    f_end_name: end_name
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_decls :
    [< decl_block]
    -> declarative_part

      
  val f_stmts :
    [< decl_block]
    -> handled_stmts

      
  val f_end_name :
    [< decl_block]
    -> end_name option


end

module BeginBlock : sig
  (**
  * Statement block with no declarative part (:rmlink:`5.6`).
  *)

  type t =
    [
      | `BeginBlock of
          begin_block_fields
    ]

  type fields = begin_block_fields =
    
  {
         
    f_stmts: handled_stmts
    Lazy.t;
         
    f_end_name: end_name
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_stmts :
    [< begin_block]
    -> handled_stmts

      
  val f_end_name :
    [< begin_block]
    -> end_name option


end

module BlockStmt : sig
  (**
  * Base class for statement blocks (:rmlink:`5.6`).
  *)

  type t =
    [
      | BeginBlock.t
      | DeclBlock.t
    ]


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module WhileLoopStmt : sig
  (**
  * Statement for ``while`` loops (``while ... loop ... end loop;``)
  * (:rmlink:`5.5`).
  *)

  type t =
    [
      | `WhileLoopStmt of
          while_loop_stmt_fields
    ]

  type fields = while_loop_stmt_fields =
    
  {
         
    f_spec: loop_spec
    option
    Lazy.t;
         
    f_stmts: stmt_list
    Lazy.t;
         
    f_end_name: end_name
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_spec :
    [< while_loop_stmt]
    -> loop_spec option

      
  val f_stmts :
    [< while_loop_stmt]
    -> stmt_list

      
  val f_end_name :
    [< while_loop_stmt]
    -> end_name option


end

module LoopStmt : sig
  (**
  * Statement for simple loops (``loop ... end loop;``) (:rmlink:`5.5`).
  *)

  type t =
    [
      | `LoopStmt of
          loop_stmt_fields
    ]

  type fields = loop_stmt_fields =
    
  {
         
    f_spec: loop_spec
    option
    Lazy.t;
         
    f_stmts: stmt_list
    Lazy.t;
         
    f_end_name: end_name
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_spec :
    [< loop_stmt]
    -> loop_spec option

      
  val f_stmts :
    [< loop_stmt]
    -> stmt_list

      
  val f_end_name :
    [< loop_stmt]
    -> end_name option


end

module ForLoopStmt : sig
  (**
  * Statement for ``for`` loops (``for ... loop ... end loop;``)
  * (:rmlink:`5.5`).
  *)

  type t =
    [
      | `ForLoopStmt of
          for_loop_stmt_fields
    ]

  type fields = for_loop_stmt_fields =
    
  {
         
    f_spec: loop_spec
    option
    Lazy.t;
         
    f_stmts: stmt_list
    Lazy.t;
         
    f_end_name: end_name
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_spec :
    [< for_loop_stmt]
    -> loop_spec option

      
  val f_stmts :
    [< for_loop_stmt]
    -> stmt_list

      
  val f_end_name :
    [< for_loop_stmt]
    -> end_name option


end

module BaseLoopStmt : sig
  (**
  * Base class for loop statements (:rmlink:`5.5`).
  *)

  type t =
    [
      | ForLoopStmt.t
      | LoopStmt.t
      | WhileLoopStmt.t
    ]


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_spec :
    [< base_loop_stmt]
    -> loop_spec option

      
  val f_stmts :
    [< base_loop_stmt]
    -> stmt_list

      
  val f_end_name :
    [< base_loop_stmt]
    -> end_name option


end

module AcceptStmtWithStmts : sig
  (**
  * Extended ``accept`` statement (:rmlink:`9.5.2`).
  *)

  type t =
    [
      | `AcceptStmtWithStmts of
          accept_stmt_with_stmts_fields
    ]

  type fields = accept_stmt_with_stmts_fields =
    
  {
         
    f_name: identifier
    Lazy.t;
         
    f_entry_index_expr: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    option
    Lazy.t;
         
    f_params: entry_completion_formal_params
    Lazy.t;
         
    f_stmts: handled_stmts
    Lazy.t;
         
    f_end_name: end_name
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_name :
    [< accept_stmt_with_stmts]
    -> identifier

      
  val f_entry_index_expr :
    [< accept_stmt_with_stmts]
    -> [allocator | attribute_ref | base_aggregate | bin_op | call_expr | char_literal | concat_op | cond_expr | decl_expr | dotted_name | explicit_deref | identifier | membership_expr | null_literal | num_literal | paren_expr | qual_expr | quantified_expr | raise_expr | reduce_attribute_ref | string_literal | target_name | un_op | update_attribute_ref] option

      
  val f_params :
    [< accept_stmt_with_stmts]
    -> entry_completion_formal_params

      
  val f_stmts :
    [< accept_stmt_with_stmts]
    -> handled_stmts

      
  val f_end_name :
    [< accept_stmt_with_stmts]
    -> end_name option


end

module AcceptStmt : sig
  (**
  * ``accept`` statement (:rmlink:`9.5.2`).
  *)

  type t =
    [
      | `AcceptStmt of
          accept_stmt_fields
      | `AcceptStmtWithStmts of
          accept_stmt_with_stmts_fields
    ]

  type fields = accept_stmt_fields =
    
  {
         
    f_name: identifier
    Lazy.t;
         
    f_entry_index_expr: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    option
    Lazy.t;
         
    f_params: entry_completion_formal_params
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int


      
  val p_corresponding_entry :
    ?origin:
    ada_node
    -> [< accept_stmt ]
    -> entry_decl option
  (**
  * Return the entry which corresponds to this accept statement.
  *)


      
  val f_name :
    [< accept_stmt]
    -> identifier

      
  val f_entry_index_expr :
    [< accept_stmt]
    -> [allocator | attribute_ref | base_aggregate | bin_op | call_expr | char_literal | concat_op | cond_expr | decl_expr | dotted_name | explicit_deref | identifier | membership_expr | null_literal | num_literal | paren_expr | qual_expr | quantified_expr | raise_expr | reduce_attribute_ref | string_literal | target_name | un_op | update_attribute_ref] option

      
  val f_params :
    [< accept_stmt]
    -> entry_completion_formal_params


end

module CompositeStmt : sig
  (**
  * Base class for composite statements (:rmlink:`5.1`).
  *)

  type t =
    [
      | AcceptStmt.t
      | BaseLoopStmt.t
      | BlockStmt.t
      | CaseStmt.t
      | ExtendedReturnStmt.t
      | IfStmt.t
      | NamedStmt.t
      | SelectStmt.t
    ]


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module Stmt : sig
  (**
  * Bass class for statements (:rmlink:`5.1`).
  *)

  type t =
    [
      | CompositeStmt.t
      | ErrorStmt.t
      | SimpleStmt.t
    ]


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int


      
  val p_is_ghost_code :
    [< stmt ]
    -> bool
  (**
  * Return whether this statement is ghost code or not. See SPARK RM 6.9.
  *)



end

module SelectWhenPart : sig
  (**
  * Alternative part in a ``select`` statements block (:rmlink:`9.7`).
  *)

  type t =
    [
      | `SelectWhenPart of
          select_when_part_fields
    ]

  type fields = select_when_part_fields =
    
  {
         
    f_cond_expr: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    option
    Lazy.t;
         
    f_stmts: stmt_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_cond_expr :
    [< select_when_part]
    -> [allocator | attribute_ref | base_aggregate | bin_op | call_expr | char_literal | concat_op | cond_expr | decl_expr | dotted_name | explicit_deref | identifier | membership_expr | null_literal | num_literal | paren_expr | qual_expr | quantified_expr | raise_expr | reduce_attribute_ref | string_literal | target_name | un_op | update_attribute_ref] option

      
  val f_stmts :
    [< select_when_part]
    -> stmt_list


end

module ReversePresent : sig
  (**

  *)

  type t =
    [
      | `ReversePresent of
          reverse_present_fields
    ]

  type fields = reverse_present_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module ReverseAbsent : sig
  (**

  *)

  type t =
    [
      | `ReverseAbsent of
          reverse_absent_fields
    ]

  type fields = reverse_absent_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module ReverseNode : sig
  (**
  * Qualifier for the ``reverse`` keyword.
  *)

  type t =
    [
      | ReverseAbsent.t
      | ReversePresent.t
    ]


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int


      
  val p_as_bool :
    [< reverse_node ]
    -> bool
  (**
  * Return whether this is an instance of ReversePresent
  *)



end

module SyntheticRenamingClause : sig
  (**
  * Synthetic renaming clause. Used to synthesize object decls with renamings.
  * (See to_anonymous_object_decl).
  *)

  type t =
    [
      | `SyntheticRenamingClause of
          synthetic_renaming_clause_fields
    ]

  type fields = synthetic_renaming_clause_fields =
    
  {
         
    f_renamed_object: [
      | `AttributeRef
          of attribute_ref_fields
      | `CallExpr
          of call_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `QualExpr
          of qual_expr_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_renamed_object :
    [< synthetic_renaming_clause]
    -> [attribute_ref | call_expr | char_literal | dotted_name | explicit_deref | identifier | qual_expr | reduce_attribute_ref | string_literal | target_name | update_attribute_ref]


end

module RenamingClause : sig
  (**
  * Renaming clause, used everywhere renamings are valid.
  *)

  type t =
    [
      | `RenamingClause of
          renaming_clause_fields
      | `SyntheticRenamingClause of
          synthetic_renaming_clause_fields
    ]

  type fields = renaming_clause_fields =
    
  {
         
    f_renamed_object: [
      | `AttributeRef
          of attribute_ref_fields
      | `CallExpr
          of call_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `QualExpr
          of qual_expr_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_renamed_object :
    [< renaming_clause]
    -> [attribute_ref | call_expr | char_literal | dotted_name | explicit_deref | identifier | qual_expr | reduce_attribute_ref | string_literal | target_name | update_attribute_ref]


end

module RangeSpec : sig
  (**
  * Range specification (:rmlink:`3.5.7`).
  *)

  type t =
    [
      | `RangeSpec of
          range_spec_fields
    ]

  type fields = range_spec_fields =
    
  {
         
    f_range: [
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BoxExpr
          of box_expr_fields
      | `CallExpr
          of call_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `QualExpr
          of qual_expr_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_range :
    [< range_spec]
    -> [attribute_ref | bin_op | box_expr | call_expr | char_literal | dotted_name | explicit_deref | identifier | qual_expr | reduce_attribute_ref | string_literal | target_name | update_attribute_ref]


end

module QuantifierSome : sig
  (**

  *)

  type t =
    [
      | `QuantifierSome of
          quantifier_some_fields
    ]

  type fields = quantifier_some_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module QuantifierAll : sig
  (**

  *)

  type t =
    [
      | `QuantifierAll of
          quantifier_all_fields
    ]

  type fields = quantifier_all_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module Quantifier : sig
  (**
  * Type for quantified expressions.
  *)

  type t =
    [
      | QuantifierAll.t
      | QuantifierSome.t
    ]


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module ProtectedPresent : sig
  (**

  *)

  type t =
    [
      | `ProtectedPresent of
          protected_present_fields
    ]

  type fields = protected_present_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module ProtectedAbsent : sig
  (**

  *)

  type t =
    [
      | `ProtectedAbsent of
          protected_absent_fields
    ]

  type fields = protected_absent_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module ProtectedNode : sig
  (**
  * Qualifier for the ``protected`` keyword.
  *)

  type t =
    [
      | ProtectedAbsent.t
      | ProtectedPresent.t
    ]


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int


      
  val p_as_bool :
    [< protected_node ]
    -> bool
  (**
  * Return whether this is an instance of ProtectedPresent
  *)



end

module ProtectedDef : sig
  (**
  * Type definition for a protected object (:rmlink:`9.4`).
  *)

  type t =
    [
      | `ProtectedDef of
          protected_def_fields
    ]

  type fields = protected_def_fields =
    
  {
         
    f_public_part: public_part
    Lazy.t;
         
    f_private_part: private_part
    option
    Lazy.t;
         
    f_end_name: end_name
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_public_part :
    [< protected_def]
    -> public_part

      
  val f_private_part :
    [< protected_def]
    -> private_part option

      
  val f_end_name :
    [< protected_def]
    -> end_name option


end

module PrivatePresent : sig
  (**

  *)

  type t =
    [
      | `PrivatePresent of
          private_present_fields
    ]

  type fields = private_present_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module PrivateAbsent : sig
  (**

  *)

  type t =
    [
      | `PrivateAbsent of
          private_absent_fields
    ]

  type fields = private_absent_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module PrivateNode : sig
  (**
  * Qualifier for the ``private`` keyword.
  *)

  type t =
    [
      | PrivateAbsent.t
      | PrivatePresent.t
    ]


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int


      
  val p_as_bool :
    [< private_node ]
    -> bool
  (**
  * Return whether this is an instance of PrivatePresent
  *)



end

module PragmaNode : sig
  (**
  * Class for pragmas (:rmlink:`2.8`). Pragmas are compiler directives, that
  * can be language or compiler defined.
  *)

  type t =
    [
      | `PragmaNode of
          pragma_node_fields
    ]

  type fields = pragma_node_fields =
    
  {
         
    f_id: identifier
    Lazy.t;
         
    f_args: base_assoc_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int


      
  val p_is_ghost_code :
    [< pragma_node ]
    -> bool
  (**
  * Return whether this pragma is ghost code or not. See SPARK RM 6.9.
  *)

      
  val p_associated_entities :
    [< pragma_node ]
    -> defining_name list
  (**
  * Return an array of ``BasicDecl`` instances associated with this pragma, or
  * an empty array if non applicable.
  *)


      
  val f_id :
    [< pragma_node]
    -> identifier

      
  val f_args :
    [< pragma_node]
    -> base_assoc_list


end

module PpThenKw : sig
  (**
  * ``then`` keyword in preprocessor directives.
  *)

  type t =
    [
      | `PpThenKw of
          pp_then_kw_fields
    ]

  type fields = pp_then_kw_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module PpIfDirective : sig
  (**
  * ``if ... [then]`` preprocessor directive.
  *)

  type t =
    [
      | `PpIfDirective of
          pp_if_directive_fields
    ]

  type fields = pp_if_directive_fields =
    
  {
         
    f_expr: [
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `Identifier
          of identifier_fields
      | `ParenExpr
          of paren_expr_fields
      | `RelationOp
          of relation_op_fields
      | `UnOp
          of un_op_fields
    ]
    Lazy.t;
         
    f_then_kw: pp_then_kw
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_expr :
    [< pp_if_directive]
    -> [attribute_ref | bin_op | identifier | paren_expr | un_op]

      
  val f_then_kw :
    [< pp_if_directive]
    -> pp_then_kw option


end

module PpEndIfDirective : sig
  (**
  * ``end if;`` preprocessor directive.
  *)

  type t =
    [
      | `PpEndIfDirective of
          pp_end_if_directive_fields
    ]

  type fields = pp_end_if_directive_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module PpElsifDirective : sig
  (**
  * ``elsif ... [then]`` preprocessor directive.
  *)

  type t =
    [
      | `PpElsifDirective of
          pp_elsif_directive_fields
    ]

  type fields = pp_elsif_directive_fields =
    
  {
         
    f_expr: [
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `Identifier
          of identifier_fields
      | `ParenExpr
          of paren_expr_fields
      | `RelationOp
          of relation_op_fields
      | `UnOp
          of un_op_fields
    ]
    Lazy.t;
         
    f_then_kw: pp_then_kw
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_expr :
    [< pp_elsif_directive]
    -> [attribute_ref | bin_op | identifier | paren_expr | un_op]

      
  val f_then_kw :
    [< pp_elsif_directive]
    -> pp_then_kw option


end

module PpElseDirective : sig
  (**
  * ``else`` preprocessor directive.
  *)

  type t =
    [
      | `PpElseDirective of
          pp_else_directive_fields
    ]

  type fields = pp_else_directive_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module PpDirective : sig
  (**
  * Base node for all preprocessor directives.
  *)

  type t =
    [
      | PpElseDirective.t
      | PpElsifDirective.t
      | PpEndIfDirective.t
      | PpIfDirective.t
    ]


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module ParenAbstractStateDecl : sig
  (**
  * Holds an AbstractStateDecl between parentheses. Needed to support the
  * syntax:
  *
  * .. code:: ada
  *
  *    package Pkg
  *        with Abstract_State => (A, (B with Some_Aspect))
  *)

  type t =
    [
      | `ParenAbstractStateDecl of
          paren_abstract_state_decl_fields
    ]

  type fields = paren_abstract_state_decl_fields =
    
  {
         
    f_decl: [
      | `AbstractStateDecl
          of abstract_state_decl_fields
      | `ParenAbstractStateDecl
          of paren_abstract_state_decl_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_decl :
    [< paren_abstract_state_decl]
    -> [abstract_state_decl | paren_abstract_state_decl]


end

module Params : sig
  (**
  * List of parameter specifications.
  *)

  type t =
    [
      | `Params of
          params_fields
    ]

  type fields = params_fields =
    
  {
         
    f_params: param_spec_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_params :
    [< params]
    -> param_spec_list


end

module OverridingUnspecified : sig
  (**

  *)

  type t =
    [
      | `OverridingUnspecified of
          overriding_unspecified_fields
    ]

  type fields = overriding_unspecified_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module OverridingOverriding : sig
  (**

  *)

  type t =
    [
      | `OverridingOverriding of
          overriding_overriding_fields
    ]

  type fields = overriding_overriding_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module OverridingNotOverriding : sig
  (**

  *)

  type t =
    [
      | `OverridingNotOverriding of
          overriding_not_overriding_fields
    ]

  type fields = overriding_not_overriding_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module OverridingNode : sig
  (**
  * Syntactic indicators for subprogram overriding modes.
  *)

  type t =
    [
      | OverridingNotOverriding.t
      | OverridingOverriding.t
      | OverridingUnspecified.t
    ]


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module OthersDesignator : sig
  (**
  * ``other`` designator.
  *)

  type t =
    [
      | `OthersDesignator of
          others_designator_fields
    ]

  type fields = others_designator_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module NullComponentDecl : sig
  (**
  * Placeholder for the ``null`` in lists of components (:rmlink:`3.8`).
  *)

  type t =
    [
      | `NullComponentDecl of
          null_component_decl_fields
    ]

  type fields = null_component_decl_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module NotNullPresent : sig
  (**

  *)

  type t =
    [
      | `NotNullPresent of
          not_null_present_fields
    ]

  type fields = not_null_present_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module NotNullAbsent : sig
  (**

  *)

  type t =
    [
      | `NotNullAbsent of
          not_null_absent_fields
    ]

  type fields = not_null_absent_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module NotNull : sig
  (**
  * Qualifier for the ``not null`` keywords.
  *)

  type t =
    [
      | NotNullAbsent.t
      | NotNullPresent.t
    ]


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int


      
  val p_as_bool :
    [< not_null ]
    -> bool
  (**
  * Return whether this is an instance of NotNullPresent
  *)



end

module MultiAbstractStateDecl : sig
  (**
  * Node that holds several AbstractStateDecl nodes, which is necessary when
  * the Abstract_State aspect is associated with an aggregate in order to
  * declare a list of abstract states.
  *)

  type t =
    [
      | `MultiAbstractStateDecl of
          multi_abstract_state_decl_fields
    ]

  type fields = multi_abstract_state_decl_fields =
    
  {
         
    f_decls: abstract_state_decl_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_decls :
    [< multi_abstract_state_decl]
    -> abstract_state_decl_list


end

module ModeOut : sig
  (**

  *)

  type t =
    [
      | `ModeOut of
          mode_out_fields
    ]

  type fields = mode_out_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module ModeInOut : sig
  (**

  *)

  type t =
    [
      | `ModeInOut of
          mode_in_out_fields
    ]

  type fields = mode_in_out_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module ModeIn : sig
  (**

  *)

  type t =
    [
      | `ModeIn of
          mode_in_fields
    ]

  type fields = mode_in_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module ModeDefault : sig
  (**

  *)

  type t =
    [
      | `ModeDefault of
          mode_default_fields
    ]

  type fields = mode_default_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module Mode : sig
  (**
  * Syntactic indicators for passing modes in formals (:rmlink:`6.1`).
  *)

  type t =
    [
      | ModeDefault.t
      | ModeIn.t
      | ModeInOut.t
      | ModeOut.t
    ]


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module WhileLoopSpec : sig
  (**
  * Specification for a ``while`` loop (:rmlink:`5.5`).
  *)

  type t =
    [
      | `WhileLoopSpec of
          while_loop_spec_fields
    ]

  type fields = while_loop_spec_fields =
    
  {
         
    f_expr: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_expr :
    [< while_loop_spec]
    -> [allocator | attribute_ref | base_aggregate | bin_op | call_expr | char_literal | concat_op | cond_expr | decl_expr | dotted_name | explicit_deref | identifier | membership_expr | null_literal | num_literal | paren_expr | qual_expr | quantified_expr | raise_expr | reduce_attribute_ref | string_literal | target_name | un_op | update_attribute_ref]


end

module ForLoopSpec : sig
  (**
  * Specification for a ``for`` loop (:rmlink:`5.5`).
  *)

  type t =
    [
      | `ForLoopSpec of
          for_loop_spec_fields
    ]

  type fields = for_loop_spec_fields =
    
  {
         
    f_var_decl: for_loop_var_decl
    Lazy.t;
         
    f_loop_type: iter_type
    Lazy.t;
         
    f_has_reverse: reverse_node
    Lazy.t;
         
    f_iter_expr: [
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `CallExpr
          of call_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `DiscreteSubtypeIndication
          of discrete_subtype_indication_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `QualExpr
          of qual_expr_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
         
    f_iter_filter: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_var_decl :
    [< for_loop_spec]
    -> for_loop_var_decl

      
  val f_loop_type :
    [< for_loop_spec]
    -> iter_type

      
  val f_has_reverse :
    [< for_loop_spec]
    -> reverse_node

      
  val f_iter_expr :
    [< for_loop_spec]
    -> [attribute_ref | bin_op | call_expr | char_literal | discrete_subtype_indication | dotted_name | explicit_deref | identifier | qual_expr | reduce_attribute_ref | string_literal | target_name | update_attribute_ref]

      
  val f_iter_filter :
    [< for_loop_spec]
    -> [allocator | attribute_ref | base_aggregate | bin_op | call_expr | char_literal | concat_op | cond_expr | decl_expr | dotted_name | explicit_deref | identifier | membership_expr | null_literal | num_literal | paren_expr | qual_expr | quantified_expr | raise_expr | reduce_attribute_ref | string_literal | target_name | un_op | update_attribute_ref] option


end

module LoopSpec : sig
  (**
  * Base class for loop specifications (:rmlink:`5.5`).
  *)

  type t =
    [
      | ForLoopSpec.t
      | WhileLoopSpec.t
    ]


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module LimitedPresent : sig
  (**

  *)

  type t =
    [
      | `LimitedPresent of
          limited_present_fields
    ]

  type fields = limited_present_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module LimitedAbsent : sig
  (**

  *)

  type t =
    [
      | `LimitedAbsent of
          limited_absent_fields
    ]

  type fields = limited_absent_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module LimitedNode : sig
  (**
  * Qualifier for the ``limited`` keyword.
  *)

  type t =
    [
      | LimitedAbsent.t
      | LimitedPresent.t
    ]


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int


      
  val p_as_bool :
    [< limited_node ]
    -> bool
  (**
  * Return whether this is an instance of LimitedPresent
  *)



end

module LibraryItem : sig
  (**
  * Library item in a compilation unit (:rmlink:`10.1.1`).
  *)

  type t =
    [
      | `LibraryItem of
          library_item_fields
    ]

  type fields = library_item_fields =
    
  {
         
    f_has_private: private_node
    Lazy.t;
         
    f_item: [
      | `AbstractSubpDecl
          of abstract_subp_decl_fields
      | `ErrorDecl
          of error_decl_fields
      | `ExprFunction
          of expr_function_fields
      | `GenericPackageDecl
          of generic_package_decl_fields
      | `GenericPackageInstantiation
          of generic_package_instantiation_fields
      | `GenericPackageRenamingDecl
          of generic_package_renaming_decl_fields
      | `GenericSubpDecl
          of generic_subp_decl_fields
      | `GenericSubpInstantiation
          of generic_subp_instantiation_fields
      | `GenericSubpRenamingDecl
          of generic_subp_renaming_decl_fields
      | `NullSubpDecl
          of null_subp_decl_fields
      | `PackageBody
          of package_body_fields
      | `PackageDecl
          of package_decl_fields
      | `PackageRenamingDecl
          of package_renaming_decl_fields
      | `SubpBody
          of subp_body_fields
      | `SubpDecl
          of subp_decl_fields
      | `SubpRenamingDecl
          of subp_renaming_decl_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_has_private :
    [< library_item]
    -> private_node

      
  val f_item :
    [< library_item]
    -> [abstract_subp_decl | base_subp_body | error_decl | generic_decl | generic_instantiation | generic_renaming_decl | package_body | package_decl | package_renaming_decl | subp_decl]


end

module IterTypeOf : sig
  (**

  *)

  type t =
    [
      | `IterTypeOf of
          iter_type_of_fields
    ]

  type fields = iter_type_of_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module IterTypeIn : sig
  (**

  *)

  type t =
    [
      | `IterTypeIn of
          iter_type_in_fields
    ]

  type fields = iter_type_in_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module IterType : sig
  (**
  * Iteration type for ``for`` loops.
  *)

  type t =
    [
      | IterTypeIn.t
      | IterTypeOf.t
    ]


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module InterfaceKindTask : sig
  (**

  *)

  type t =
    [
      | `InterfaceKindTask of
          interface_kind_task_fields
    ]

  type fields = interface_kind_task_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module InterfaceKindSynchronized : sig
  (**

  *)

  type t =
    [
      | `InterfaceKindSynchronized of
          interface_kind_synchronized_fields
    ]

  type fields = interface_kind_synchronized_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module InterfaceKindProtected : sig
  (**

  *)

  type t =
    [
      | `InterfaceKindProtected of
          interface_kind_protected_fields
    ]

  type fields = interface_kind_protected_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module InterfaceKindLimited : sig
  (**

  *)

  type t =
    [
      | `InterfaceKindLimited of
          interface_kind_limited_fields
    ]

  type fields = interface_kind_limited_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module InterfaceKind : sig
  (**
  * Kind of interface type.
  *)

  type t =
    [
      | InterfaceKindLimited.t
      | InterfaceKindProtected.t
      | InterfaceKindSynchronized.t
      | InterfaceKindTask.t
    ]


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module HandledStmts : sig
  (**
  * List of statements, with optional exception handlers (:rmlink:`11.2`).
  *)

  type t =
    [
      | `HandledStmts of
          handled_stmts_fields
    ]

  type fields = handled_stmts_fields =
    
  {
         
    f_stmts: stmt_list
    Lazy.t;
         
    f_exceptions: ada_node_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_stmts :
    [< handled_stmts]
    -> stmt_list

      
  val f_exceptions :
    [< handled_stmts]
    -> ada_node_list


end

module UnOp : sig
  (**
  * Unary expression.
  *
  * This encompasses several ARM expressions, because it is used for every
  * unary operator in Ada. Those expressions are all documented in
  * :rmlink:`4.4`.
  *)

  type t =
    [
      | `UnOp of
          un_op_fields
    ]

  type fields = un_op_fields =
    
  {
         
    f_op: [
      | `OpAbs
          of op_abs_fields
      | `OpMinus
          of op_minus_fields
      | `OpNot
          of op_not_fields
      | `OpPlus
          of op_plus_fields
    ]
    Lazy.t;
         
    f_expr: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_op :
    [< un_op]
    -> [op_abs | op_minus | op_not | op_plus]

      
  val f_expr :
    [< un_op]
    -> [allocator | attribute_ref | base_aggregate | bin_op | call_expr | char_literal | cond_expr | decl_expr | dotted_name | explicit_deref | identifier | null_literal | num_literal | paren_expr | qual_expr | quantified_expr | raise_expr | reduce_attribute_ref | string_literal | target_name | un_op | update_attribute_ref]


end

module RaiseExpr : sig
  (**
  * Expression to raise an exception (:rmlink:`4.4`).
  *)

  type t =
    [
      | `RaiseExpr of
          raise_expr_fields
    ]

  type fields = raise_expr_fields =
    
  {
         
    f_exception_name: [
      | `AttributeRef
          of attribute_ref_fields
      | `CallExpr
          of call_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `QualExpr
          of qual_expr_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    option
    Lazy.t;
         
    f_error_message: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_exception_name :
    [< raise_expr]
    -> [attribute_ref | call_expr | char_literal | dotted_name | explicit_deref | identifier | qual_expr | reduce_attribute_ref | string_literal | target_name | update_attribute_ref] option

      
  val f_error_message :
    [< raise_expr]
    -> [allocator | attribute_ref | base_aggregate | bin_op | call_expr | char_literal | concat_op | cond_expr | decl_expr | dotted_name | explicit_deref | identifier | membership_expr | null_literal | num_literal | paren_expr | qual_expr | quantified_expr | raise_expr | reduce_attribute_ref | string_literal | target_name | un_op | update_attribute_ref] option


end

module QuantifiedExpr : sig
  (**
  * Quantified expression (:rmlink:`4.5.8`).
  *)

  type t =
    [
      | `QuantifiedExpr of
          quantified_expr_fields
    ]

  type fields = quantified_expr_fields =
    
  {
         
    f_quantifier: quantifier
    Lazy.t;
         
    f_loop_spec: for_loop_spec
    Lazy.t;
         
    f_expr: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_quantifier :
    [< quantified_expr]
    -> quantifier

      
  val f_loop_spec :
    [< quantified_expr]
    -> for_loop_spec

      
  val f_expr :
    [< quantified_expr]
    -> [allocator | attribute_ref | base_aggregate | bin_op | call_expr | char_literal | concat_op | cond_expr | decl_expr | dotted_name | explicit_deref | identifier | membership_expr | null_literal | num_literal | paren_expr | qual_expr | quantified_expr | raise_expr | reduce_attribute_ref | string_literal | target_name | un_op | update_attribute_ref]


end

module ParenExpr : sig
  (**
  * Parenthesized expression.
  *)

  type t =
    [
      | `ParenExpr of
          paren_expr_fields
    ]

  type fields = paren_expr_fields =
    
  {
         
    f_expr: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_expr :
    [< paren_expr]
    -> [allocator | attribute_ref | base_aggregate | bin_op | call_expr | char_literal | concat_op | cond_expr | decl_expr | dotted_name | explicit_deref | identifier | membership_expr | null_literal | num_literal | paren_expr | qual_expr | quantified_expr | raise_expr | reduce_attribute_ref | string_literal | target_name | un_op | update_attribute_ref]


end

module UpdateAttributeRef : sig
  (**
  * Reference to the ``Update`` attribute, which is a non standard GNAT
  * attribute.
  *)

  type t =
    [
      | `UpdateAttributeRef of
          update_attribute_ref_fields
    ]

  type fields = update_attribute_ref_fields =
    
  {
         
    f_prefix: [
      | `AttributeRef
          of attribute_ref_fields
      | `CallExpr
          of call_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `QualExpr
          of qual_expr_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
         
    f_attribute: identifier
    Lazy.t;
         
    f_values: base_aggregate
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_prefix :
    [< update_attribute_ref]
    -> [attribute_ref | call_expr | char_literal | dotted_name | explicit_deref | identifier | qual_expr | reduce_attribute_ref | string_literal | target_name | update_attribute_ref]

      
  val f_attribute :
    [< update_attribute_ref]
    -> identifier

      
  val f_values :
    [< update_attribute_ref]
    -> base_aggregate


end

module TargetName : sig
  (**
  * Name for Ada 2020 ``@`` (:rmlink:`5.2.1`).
  *)

  type t =
    [
      | `TargetName of
          target_name_fields
    ]

  type fields = target_name_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module SyntheticIdentifier : sig
  (**
  * Synthetic identifier.
  *)

  type t =
    [
      | `SyntheticIdentifier of
          synthetic_identifier_fields
    ]

  type fields = synthetic_identifier_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module RealLiteral : sig
  (**
  * Literal for a real number (:rmlink:`2.4`).
  *)

  type t =
    [
      | `RealLiteral of
          real_literal_fields
    ]

  type fields = real_literal_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module IntLiteral : sig
  (**
  * Literal for an integer (:rmlink:`2.4`).
  *)

  type t =
    [
      | `IntLiteral of
          int_literal_fields
    ]

  type fields = int_literal_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int


      
  val p_denoted_value :
    [< int_literal ]
    -> BigInteger.t
  (**
  * Return the value that this literal denotes.
  *)



end

module NumLiteral : sig
  (**
  * Base class for number literals (:rmlink:`2.4`).
  *)

  type t =
    [
      | IntLiteral.t
      | RealLiteral.t
    ]


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module NullLiteral : sig
  (**
  * The ``null`` literal (:rmlink:`4.4`).
  *)

  type t =
    [
      | `NullLiteral of
          null_literal_fields
    ]

  type fields = null_literal_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module StringLiteral : sig
  (**
  * String literal (:rmlink:`2.6`).
  *)

  type t =
    [
      | `StringLiteral of
          string_literal_fields
    ]

  type fields = string_literal_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int


      
  val p_denoted_value :
    [< string_literal ]
    -> string
  (**
  * Return the value that this literal denotes.
  *)



end

module OpXor : sig
  (**

  *)

  type t =
    [
      | `OpXor of
          op_xor_fields
    ]

  type fields = op_xor_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module OpRem : sig
  (**

  *)

  type t =
    [
      | `OpRem of
          op_rem_fields
    ]

  type fields = op_rem_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module OpPow : sig
  (**

  *)

  type t =
    [
      | `OpPow of
          op_pow_fields
    ]

  type fields = op_pow_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module OpPlus : sig
  (**

  *)

  type t =
    [
      | `OpPlus of
          op_plus_fields
    ]

  type fields = op_plus_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module OpOrElse : sig
  (**

  *)

  type t =
    [
      | `OpOrElse of
          op_or_else_fields
    ]

  type fields = op_or_else_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module OpOr : sig
  (**

  *)

  type t =
    [
      | `OpOr of
          op_or_fields
    ]

  type fields = op_or_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module OpNotIn : sig
  (**

  *)

  type t =
    [
      | `OpNotIn of
          op_not_in_fields
    ]

  type fields = op_not_in_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module OpNot : sig
  (**

  *)

  type t =
    [
      | `OpNot of
          op_not_fields
    ]

  type fields = op_not_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module OpNeq : sig
  (**

  *)

  type t =
    [
      | `OpNeq of
          op_neq_fields
    ]

  type fields = op_neq_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module OpMult : sig
  (**

  *)

  type t =
    [
      | `OpMult of
          op_mult_fields
    ]

  type fields = op_mult_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module OpMod : sig
  (**

  *)

  type t =
    [
      | `OpMod of
          op_mod_fields
    ]

  type fields = op_mod_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module OpMinus : sig
  (**

  *)

  type t =
    [
      | `OpMinus of
          op_minus_fields
    ]

  type fields = op_minus_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module OpLte : sig
  (**

  *)

  type t =
    [
      | `OpLte of
          op_lte_fields
    ]

  type fields = op_lte_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module OpLt : sig
  (**

  *)

  type t =
    [
      | `OpLt of
          op_lt_fields
    ]

  type fields = op_lt_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module OpIn : sig
  (**

  *)

  type t =
    [
      | `OpIn of
          op_in_fields
    ]

  type fields = op_in_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module OpGte : sig
  (**

  *)

  type t =
    [
      | `OpGte of
          op_gte_fields
    ]

  type fields = op_gte_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module OpGt : sig
  (**

  *)

  type t =
    [
      | `OpGt of
          op_gt_fields
    ]

  type fields = op_gt_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module OpEq : sig
  (**

  *)

  type t =
    [
      | `OpEq of
          op_eq_fields
    ]

  type fields = op_eq_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module OpDoubleDot : sig
  (**

  *)

  type t =
    [
      | `OpDoubleDot of
          op_double_dot_fields
    ]

  type fields = op_double_dot_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module OpDiv : sig
  (**

  *)

  type t =
    [
      | `OpDiv of
          op_div_fields
    ]

  type fields = op_div_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module OpConcat : sig
  (**

  *)

  type t =
    [
      | `OpConcat of
          op_concat_fields
    ]

  type fields = op_concat_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module OpAndThen : sig
  (**

  *)

  type t =
    [
      | `OpAndThen of
          op_and_then_fields
    ]

  type fields = op_and_then_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module OpAnd : sig
  (**

  *)

  type t =
    [
      | `OpAnd of
          op_and_fields
    ]

  type fields = op_and_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module OpAbs : sig
  (**

  *)

  type t =
    [
      | `OpAbs of
          op_abs_fields
    ]

  type fields = op_abs_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module Op : sig
  (**
  * Operation in a binary expression.
  *
  * Note that the ARM does not consider "double_dot" ("..") as a binary
  * operator, but we process it this way here anyway to keep things simple.
  *)

  type t =
    [
      | OpAbs.t
      | OpAnd.t
      | OpAndThen.t
      | OpConcat.t
      | OpDiv.t
      | OpDoubleDot.t
      | OpEq.t
      | OpGt.t
      | OpGte.t
      | OpIn.t
      | OpLt.t
      | OpLte.t
      | OpMinus.t
      | OpMod.t
      | OpMult.t
      | OpNeq.t
      | OpNot.t
      | OpNotIn.t
      | OpOr.t
      | OpOrElse.t
      | OpPlus.t
      | OpPow.t
      | OpRem.t
      | OpXor.t
    ]


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module Identifier : sig
  (**
  * Regular identifier (:rmlink:`2.3`).
  *)

  type t =
    [
      | `Identifier of
          identifier_fields
    ]

  type fields = identifier_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module CharLiteral : sig
  (**
  * Character literal (:rmlink:`4.1`).
  *)

  type t =
    [
      | `CharLiteral of
          char_literal_fields
    ]

  type fields = char_literal_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int


      
  val p_denoted_value :
    [< char_literal ]
    -> string
  (**
  * Return the value that this literal denotes.
  *)



end

module BaseId : sig
  (**
  * Base class for identifiers.
  *)

  type t =
    [
      | CharLiteral.t
      | Identifier.t
      | Op.t
      | StringLiteral.t
    ]


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module SingleTokNode : sig
  (**
  * Base class for nodes that are made up of a single token.
  *)

  type t =
    [
      | BaseId.t
      | NullLiteral.t
      | NumLiteral.t
    ]


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module ReduceAttributeRef : sig
  (**
  * Reduction expression (``Reduce`` attribute). Ada 2022, RM 4.5.10.
  *)

  type t =
    [
      | `ReduceAttributeRef of
          reduce_attribute_ref_fields
    ]

  type fields = reduce_attribute_ref_fields =
    
  {
         
    f_prefix: [
      | `AttributeRef
          of attribute_ref_fields
      | `CallExpr
          of call_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `QualExpr
          of qual_expr_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
      | `ValueSequence
          of value_sequence_fields
    ]
    Lazy.t;
         
    f_attribute: identifier
    Lazy.t;
         
    f_args: basic_assoc_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_prefix :
    [< reduce_attribute_ref]
    -> [attribute_ref | call_expr | char_literal | dotted_name | explicit_deref | identifier | qual_expr | reduce_attribute_ref | string_literal | target_name | update_attribute_ref | value_sequence]

      
  val f_attribute :
    [< reduce_attribute_ref]
    -> identifier

      
  val f_args :
    [< reduce_attribute_ref]
    -> basic_assoc_list


end

module QualExpr : sig
  (**
  * Qualified expression (``...'(...)``) .(:rmlink:`4.7`).
  *)

  type t =
    [
      | `QualExpr of
          qual_expr_fields
    ]

  type fields = qual_expr_fields =
    
  {
         
    f_prefix: [
      | `AttributeRef
          of attribute_ref_fields
      | `CallExpr
          of call_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `QualExpr
          of qual_expr_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
         
    f_suffix: [
      | `Aggregate
          of aggregate_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_prefix :
    [< qual_expr]
    -> [attribute_ref | call_expr | char_literal | dotted_name | explicit_deref | identifier | qual_expr | reduce_attribute_ref | string_literal | target_name | update_attribute_ref]

      
  val f_suffix :
    [< qual_expr]
    -> [base_aggregate | paren_expr]


end

module ExplicitDeref : sig
  (**
  * Explicit dereference expression (``.all``) (:rmlink:`4.1`).
  *)

  type t =
    [
      | `ExplicitDeref of
          explicit_deref_fields
    ]

  type fields = explicit_deref_fields =
    
  {
         
    f_prefix: [
      | `AttributeRef
          of attribute_ref_fields
      | `CallExpr
          of call_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `QualExpr
          of qual_expr_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_prefix :
    [< explicit_deref]
    -> [attribute_ref | call_expr | char_literal | dotted_name | explicit_deref | identifier | qual_expr | reduce_attribute_ref | string_literal | target_name | update_attribute_ref]


end

module EndName : sig
  (**
  * Entity name in ``end ...;`` syntactic constructs.
  *)

  type t =
    [
      | `EndName of
          end_name_fields
    ]

  type fields = end_name_fields =
    
  {
         
    f_name: [
      | `CharLiteral
          of char_literal_fields
      | `DottedName
          of dotted_name_fields
      | `Identifier
          of identifier_fields
      | `StringLiteral
          of string_literal_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int


      
  val p_basic_decl :
    [< end_name ]
    -> basic_decl option
  (**
  * Returns this EndName's basic declaration
  *)


      
  val f_name :
    [< end_name]
    -> [char_literal | dotted_name | identifier | string_literal]


end

module DottedName : sig
  (**
  * Name to select a suffix in a prefix (:rmlink:`4.1.3`).
  *)

  type t =
    [
      | `DottedName of
          dotted_name_fields
    ]

  type fields = dotted_name_fields =
    
  {
         
    f_prefix: [
      | `AttributeRef
          of attribute_ref_fields
      | `CallExpr
          of call_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `QualExpr
          of qual_expr_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
         
    f_suffix: [
      | `CharLiteral
          of char_literal_fields
      | `Identifier
          of identifier_fields
      | `StringLiteral
          of string_literal_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_prefix :
    [< dotted_name]
    -> [attribute_ref | call_expr | char_literal | dotted_name | explicit_deref | identifier | qual_expr | reduce_attribute_ref | string_literal | target_name | update_attribute_ref]

      
  val f_suffix :
    [< dotted_name]
    -> [char_literal | identifier | string_literal]


end

module DiscreteSubtypeName : sig
  (**
  * Subtype name for membership test expressions (:rmlink:`3.6`).
  *)

  type t =
    [
      | `DiscreteSubtypeName of
          discrete_subtype_name_fields
    ]

  type fields = discrete_subtype_name_fields =
    
  {
         
    f_subtype: discrete_subtype_indication
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_subtype :
    [< discrete_subtype_name]
    -> discrete_subtype_indication


end

module SyntheticDefiningName : sig
  (**
  * Synthetic DefiningName.
  *)

  type t =
    [
      | `SyntheticDefiningName of
          synthetic_defining_name_fields
    ]

  type fields = synthetic_defining_name_fields =
    
  {
         
    f_name: [
      | `CharLiteral
          of char_literal_fields
      | `DottedName
          of dotted_name_fields
      | `Identifier
          of identifier_fields
      | `StringLiteral
          of string_literal_fields
      | `SyntheticIdentifier
          of synthetic_identifier_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_name :
    [< synthetic_defining_name]
    -> [char_literal | dotted_name | identifier | string_literal | synthetic_identifier]


end

module DefiningName : sig
  (**
  * Name that defines an entity (:rmlink:`3.1`).
  *)

  type t =
    [
      | `DefiningName of
          defining_name_fields
      | `SyntheticDefiningName of
          synthetic_defining_name_fields
    ]

  type fields = defining_name_fields =
    
  {
         
    f_name: [
      | `CharLiteral
          of char_literal_fields
      | `DottedName
          of dotted_name_fields
      | `Identifier
          of identifier_fields
      | `StringLiteral
          of string_literal_fields
      | `SyntheticIdentifier
          of synthetic_identifier_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int


      
  val p_canonical_fully_qualified_name :
    [< defining_name ]
    -> string
  (**
  * Return a canonical representation of the fully qualified name corresponding
  * to this defining name.
  *)

      
  val p_unique_identifying_name :
    [< defining_name ]
    -> string
  (**
  * Return a unique identifying name for this defining name, provided this
  * declaration is a public declaration. In the case of subprograms, this will
  * include the profile.
  *
  * .. attention:: This will only return a unique name for public declarations.
  *    Notably, anything nested in an unnamed declare block won't be handled
  *    correctly.
  *)

      
  val p_fully_qualified_name_array :
    [< defining_name ]
    -> string list
  (**
  * Return the fully qualified name corresponding to this defining name, as an
  * array of symbols.
  *)

      
  val p_fully_qualified_name :
    [< defining_name ]
    -> string
  (**
  * Return the fully qualified name corresponding to this defining name.
  *)

      
  val p_basic_decl :
    [< defining_name ]
    -> basic_decl option
  (**
  * Returns this DefiningName's basic declaration
  *)

      
  val p_find_refs :
    ?imprecise_fallback:
    bool
    -> [< defining_name ]
    -> ada_node
    -> RefResult.t list
  (**
  * Find all references to this defining name in the given ``root`` and its
  * children.
  *)

      
  val p_find_all_references :
    ?follow_renamings:
    bool
    -> ?imprecise_fallback:
    bool
    -> [< defining_name ]
    -> analysis_unit list
    -> RefResult.t list
  (**
  * Searches all references to this defining name in the given list of units.
  *
  * If ``follow_renamings`` is True, also this also includes references that
  * ultimately refer to this defining name, by unwinding renaming clauses.
  *)

      
  val p_find_all_calls :
    ?follow_renamings:
    bool
    -> ?imprecise_fallback:
    bool
    -> [< defining_name ]
    -> analysis_unit list
    -> RefResult.t list
  (**
  * Return the list of all possible calls to the subprogram which Self is the
  * defining name of.
  *
  * This will return the name corresponding to the call, excluding the
  * parameters if there are any. For instance, it will return ``A`` for the ``A
  * (B)`` call.
  *
  * .. note:: This does not yet support calls done inside generics.
  *)

      
  val p_next_part :
    ?imprecise_fallback:
    bool
    -> [< defining_name ]
    -> defining_name option
  (**
  * Like ``BasicDecl.next_part_for_decl`` on a defining name
  *)

      
  val p_previous_part :
    ?imprecise_fallback:
    bool
    -> [< defining_name ]
    -> defining_name option
  (**
  * Like ``BasicDecl.previous_part_for_decl`` on a defining name
  *)

      
  val p_canonical_part :
    ?imprecise_fallback:
    bool
    -> [< defining_name ]
    -> defining_name option
  (**
  * Like ``BasicDecl.canonical_part`` on a defining name
  *)

      
  val p_most_visible_part :
    ?imprecise_fallback:
    bool
    -> [< defining_name ]
    -> ada_node
    -> defining_name option
  (**
  * Given an origin node and the entity represented by Self, this property
  * returns the most visible completion of Self that can be seen by origin,
  * according to Ada's visibility rules.
  *)

      
  val p_all_parts :
    ?imprecise_fallback:
    bool
    -> [< defining_name ]
    -> defining_name list
  (**
  * Return all parts that define this entity, sorted from first part to last
  * part.
  *)

      
  val p_get_aspect :
    ?imprecise_fallback:
    bool
    -> [< defining_name ]
    -> string
    -> Aspect.t
  (**
  * Return the aspect with name ``name`` associated to entity that this name
  * defines.
  *
  * Aspects are properties of entities that can be specified by the Ada
  * program, either via aspect specifications, pragmas, or attributes.
  *
  * This will return the syntactic node corresponding to attribute directly.
  *
  * Note: for some aspects (e.g. ``Inline``), Libadalang will check if they are
  * defined on any part of the entity.
  *)

      
  val p_has_aspect :
    ?imprecise_fallback:
    bool
    -> [< defining_name ]
    -> string
    -> bool
  (**
  * Returns whether the boolean aspect named ``name`` is set on the entity
  * represented by this node.
  *
  * "Aspect" is used as in RM terminology (see :rmlink:`13.1`).
  *)

      
  val p_get_pragma :
    [< defining_name ]
    -> string
    -> pragma_node option
  (**
  * Return the pragma with name ``name`` associated to this entity.
  *
  * Please use the ``p_get_aspects`` property instead if you are interested in
  * aspects, i.e. information that can be represented by either aspect
  * specification nodes, pragma nodes or attribute definition nodes.
  *)

      
  val p_get_representation_clause :
    ?imprecise_fallback:
    bool
    -> [< defining_name ]
    -> string
    -> attribute_def_clause option
  (**
  * Return the representation clause associated to this entity that defines the
  * given attribute name.
  *)

      
  val p_get_at_clause :
    ?imprecise_fallback:
    bool
    -> [< defining_name ]
    -> at_clause option
  (**
  * Return the at clause associated to this entity.
  *)

      
  val p_is_imported :
    [< defining_name ]
    -> bool
  (**
  * Whether this entity defined by this name is imported from another language.
  *)

      
  val p_is_ghost_code :
    [< defining_name ]
    -> bool
  (**
  * Return whether the entity defined by this name is ghost or not. See SPARK
  * RM 6.9.
  *)


      
  val f_name :
    [< defining_name]
    -> [char_literal | dotted_name | identifier | string_literal | synthetic_identifier]


end

module CallExpr : sig
  (**
  * Represent a syntactic call expression.
  *
  * At the semantic level, this can be either a subprogram call, an array
  * subcomponent access expression, an array slice or a type conversion, all
  * described in :rmlink:`4.1`, except for subprogram call statements,
  * described in :rmlink:`6.4`.
  *)

  type t =
    [
      | `CallExpr of
          call_expr_fields
    ]

  type fields = call_expr_fields =
    
  {
         
    f_name: [
      | `AttributeRef
          of attribute_ref_fields
      | `CallExpr
          of call_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `QualExpr
          of qual_expr_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
         
    f_suffix: [
      | `AssocList
          of assoc_list_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `CallExpr
          of call_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `DiscreteSubtypeIndication
          of discrete_subtype_indication_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `QualExpr
          of qual_expr_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int


      
  val p_kind :
    [< call_expr ]
    -> CallExprKind.t
  (**
  * Return whether this expression is a subprogram call, an array subcomponent
  * access expression, an array slice or a type conversion.
  *)

      
  val p_is_array_slice :
    [< call_expr ]
    -> bool
  (**
  * Return whether this CallExpr is actually an access to a slice of the array
  * denoted by the prefix of this CallExpr.
  *)


      
  val f_name :
    [< call_expr]
    -> [attribute_ref | call_expr | char_literal | dotted_name | explicit_deref | identifier | qual_expr | reduce_attribute_ref | string_literal | target_name | update_attribute_ref]

      
  val f_suffix :
    [< call_expr]
    -> [attribute_ref | basic_assoc_list | bin_op | call_expr | char_literal | discrete_subtype_indication | dotted_name | explicit_deref | identifier | qual_expr | reduce_attribute_ref | string_literal | target_name | update_attribute_ref]


end

module AttributeRef : sig
  (**
  * Expression to reference an attribute (:rmlink:`4.1.4`).
  *)

  type t =
    [
      | `AttributeRef of
          attribute_ref_fields
    ]

  type fields = attribute_ref_fields =
    
  {
         
    f_prefix: [
      | `AttributeRef
          of attribute_ref_fields
      | `CallExpr
          of call_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `QualExpr
          of qual_expr_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
         
    f_attribute: identifier
    Lazy.t;
         
    f_args: basic_assoc_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_prefix :
    [< attribute_ref]
    -> [attribute_ref | call_expr | char_literal | dotted_name | explicit_deref | identifier | qual_expr | reduce_attribute_ref | string_literal | target_name | update_attribute_ref]

      
  val f_attribute :
    [< attribute_ref]
    -> identifier

      
  val f_args :
    [< attribute_ref]
    -> basic_assoc_list


end

module Name : sig
  (**
  * Base class for names (:rmlink:`4.1`).
  *)

  type t =
    [
      | AttributeRef.t
      | CallExpr.t
      | DefiningName.t
      | DiscreteSubtypeName.t
      | DottedName.t
      | EndName.t
      | ExplicitDeref.t
      | QualExpr.t
      | ReduceAttributeRef.t
      | SingleTokNode.t
      | SyntheticIdentifier.t
      | TargetName.t
      | UpdateAttributeRef.t
    ]


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int


      
  val p_enclosing_defining_name :
    [< name ]
    -> defining_name option
  (**
  * If this name is part of a defining name, return the enclosing defining name
  * node.
  *)

      
  val p_is_defining :
    [< name ]
    -> bool
  (**
  * Return True if this name is part of a defining name.
  *)

      
  val p_name_is :
    [< name ]
    -> string
    -> bool
  (**
  * Helper. Check that this name matches ``sym``.
  *)

      
  val p_is_direct_call :
    [< name ]
    -> bool
  (**
  * Return True iff this name represents a call to a subprogram which is
  * referred by its defining name. (i.e. not through a subprogram access).
  *)

      
  val p_is_access_call :
    [< name ]
    -> bool
  (**
  * Return True iff this name represents a call to subprogram through an access
  * type.
  *)

      
  val p_is_call :
    [< name ]
    -> bool
  (**
  * Returns True if this Name corresponds to a call.
  *)

      
  val p_is_dot_call :
    ?imprecise_fallback:
    bool
    -> [< name ]
    -> bool
  (**
  * Returns True if this Name corresponds to a dot notation call.
  *)

      
  val p_failsafe_referenced_def_name :
    ?imprecise_fallback:
    bool
    -> [< name ]
    -> RefdDef.t
  (**
  * Failsafe version of ``referenced_defining_name``. Returns a ``RefdDef``,
  * which can be precise, imprecise, or error.
  *)

      
  val p_referenced_defining_name :
    ?imprecise_fallback:
    bool
    -> [< name ]
    -> defining_name option
  (**
  * Like ``referenced_decl``, but will return the defining identifier for the
  * decl, rather than the basic declaration node itself.
  *)

      
  val p_all_env_elements :
    ?seq:
    bool
    -> ?seq_from:
    ada_node
    -> [< name ]
    -> ada_node list
  (**
  * Return all elements in self's scope that are lexically named like Self.
  *)

      
  val p_called_subp_spec :
    [< name ]
    -> base_formal_param_holder option
  (**
  * Return the subprogram specification of the subprogram or subprogram access
  * that is being called by this exact Name, if relevant.
  *)

      
  val p_referenced_decl :
    ?imprecise_fallback:
    bool
    -> [< name ]
    -> basic_decl option
  (**
  * Return the declaration this node references after name resolution. If
  * imprecise_fallback is True, errors raised during resolution of the xref
  * equation are catched and a fallback mechanism is triggered, which tries to
  * find the referenced declaration in an ad-hoc way.
  *)

      
  val p_failsafe_referenced_decl :
    ?imprecise_fallback:
    bool
    -> [< name ]
    -> RefdDecl.t
  (**
  * Failsafe version of ``referenced_decl``. Returns a ``RefdDecl``, which can
  * be precise, imprecise, or error.
  *)

      
  val p_referenced_decl_internal :
    ?imprecise_fallback:
    bool
    -> [< name ]
    -> RefdDecl.t
  (**
  * Return the declaration this node references. Try not to run name res if
  * already resolved.
  *
  * .. warning:: INTERNAL USE ONLY.
  *)

      
  val p_name_designated_type :
    [< name ]
    -> base_type_decl option
  (**
  * Like SubtypeIndication.designated_type, but on names, since because of
  * Ada's ambiguous grammar, some subtype indications will be parsed as names.
  *)

      
  val p_is_static_subtype :
    ?imprecise_fallback:
    bool
    -> [< name ]
    -> bool
  (**
  * Returns whether Self denotes a static subtype or not.
  *)

      
  val p_name_matches :
    [< name ]
    -> name
    -> bool
  (**
  * Return whether two names match each other.
  *
  * This compares the symbol for Identifier and StringLiteral nodes. We
  * consider that there is no match for all other node kinds.
  *)

      
  val p_relative_name :
    [< name ]
    -> single_tok_node option
  (**
  * Returns the relative name of this instance. For example, for a prefix
  * ``A.B.C``, this will return ``C``.
  *)

      
  val p_is_operator_name :
    [< name ]
    -> bool
  (**
  * Return whether the name that Self designates is an operator.
  *)

      
  val p_is_write_reference :
    ?imprecise_fallback:
    bool
    -> [< name ]
    -> bool
  (**
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
  *)

      
  val p_is_static_call :
    ?imprecise_fallback:
    bool
    -> [< name ]
    -> bool
  (**
  * Returns True if this Name corresponds to a static non-dispatching call. In
  * other words, this will return True if and only if the target of the call is
  * known statically.
  *
  * .. note:: This is an experimental feature. There might be some discrepancy
  *    with the GNAT concept of "static call".
  *)

      
  val p_as_symbol_array :
    [< name ]
    -> string list
  (**
  * Turn this name into an array of symbols.
  *
  * For instance, a node with name ``A.B.C`` is turned into ``['A', 'B',
  * 'C']``.
  *
  * Only simple name kinds are allowed: Identifer, DottedName and DefiningName.
  * Any other kind will trigger a PropertyError.
  *)

      
  val p_canonical_text :
    [< name ]
    -> string
  (**
  * Return a canonicalized version of this name's text.
  *
  * Only simple name kinds are allowed: Identifer, DottedName and DefiningName.
  * Any other kind will trigger a PropertyError.
  *)

      
  val p_is_constant :
    [< name ]
    -> bool
  (**
  * Return whether this name denotes a constant value.
  *)

      
  val p_call_params :
    [< name ]
    -> ParamActual.t list
  (**
  * Returns an array of pairs, associating formal parameters to actual or
  * default expressions.
  *)



end

module MembershipExpr : sig
  (**
  * Represent a membership test (in/not in operators) (:rmlink:`4.4`).
  *
  * Note that we don't consider them as binary operators since multiple
  * expressions on the right hand side are allowed.
  *)

  type t =
    [
      | `MembershipExpr of
          membership_expr_fields
    ]

  type fields = membership_expr_fields =
    
  {
         
    f_expr: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
         
    f_op: [
      | `OpIn
          of op_in_fields
      | `OpNotIn
          of op_not_in_fields
    ]
    Lazy.t;
         
    f_membership_exprs: expr_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_expr :
    [< membership_expr]
    -> [allocator | attribute_ref | base_aggregate | bin_op | call_expr | char_literal | concat_op | cond_expr | decl_expr | dotted_name | explicit_deref | identifier | membership_expr | null_literal | num_literal | paren_expr | qual_expr | quantified_expr | raise_expr | reduce_attribute_ref | string_literal | target_name | un_op | update_attribute_ref]

      
  val f_op :
    [< membership_expr]
    -> [op_in | op_not_in]

      
  val f_membership_exprs :
    [< membership_expr]
    -> expr_list


end

module DeclExpr : sig
  (**
  * Declare expression (Ada 2020, :rmlink:`4.5.9`).
  *)

  type t =
    [
      | `DeclExpr of
          decl_expr_fields
    ]

  type fields = decl_expr_fields =
    
  {
         
    f_decls: basic_decl_list
    Lazy.t;
         
    f_expr: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_decls :
    [< decl_expr]
    -> basic_decl_list

      
  val f_expr :
    [< decl_expr]
    -> [allocator | attribute_ref | base_aggregate | bin_op | call_expr | char_literal | concat_op | cond_expr | decl_expr | dotted_name | explicit_deref | identifier | membership_expr | null_literal | num_literal | paren_expr | qual_expr | quantified_expr | raise_expr | reduce_attribute_ref | string_literal | target_name | un_op | update_attribute_ref]


end

module ContractCases : sig
  (**
  * List of associations for the ``Contract_Case`` aspect.
  *
  * Contract cases is a non standard Ada extension that's mainly useful in
  * SPARK. See the SPARK RM for more details.
  *)

  type t =
    [
      | `ContractCases of
          contract_cases_fields
    ]

  type fields = contract_cases_fields =
    
  {
         
    f_contract_cases: contract_case_assoc_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_contract_cases :
    [< contract_cases]
    -> contract_case_assoc_list


end

module IfExpr : sig
  (**
  * ``if`` expression (:rmlink`4.5.7`).
  *)

  type t =
    [
      | `IfExpr of
          if_expr_fields
    ]

  type fields = if_expr_fields =
    
  {
         
    f_cond_expr: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
         
    f_then_expr: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
         
    f_alternatives: elsif_expr_part_list
    Lazy.t;
         
    f_else_expr: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_cond_expr :
    [< if_expr]
    -> [allocator | attribute_ref | base_aggregate | bin_op | call_expr | char_literal | concat_op | cond_expr | decl_expr | dotted_name | explicit_deref | identifier | membership_expr | null_literal | num_literal | paren_expr | qual_expr | quantified_expr | raise_expr | reduce_attribute_ref | string_literal | target_name | un_op | update_attribute_ref]

      
  val f_then_expr :
    [< if_expr]
    -> [allocator | attribute_ref | base_aggregate | bin_op | call_expr | char_literal | concat_op | cond_expr | decl_expr | dotted_name | explicit_deref | identifier | membership_expr | null_literal | num_literal | paren_expr | qual_expr | quantified_expr | raise_expr | reduce_attribute_ref | string_literal | target_name | un_op | update_attribute_ref]

      
  val f_alternatives :
    [< if_expr]
    -> elsif_expr_part_list

      
  val f_else_expr :
    [< if_expr]
    -> [allocator | attribute_ref | base_aggregate | bin_op | call_expr | char_literal | concat_op | cond_expr | decl_expr | dotted_name | explicit_deref | identifier | membership_expr | null_literal | num_literal | paren_expr | qual_expr | quantified_expr | raise_expr | reduce_attribute_ref | string_literal | target_name | un_op | update_attribute_ref] option


end

module CaseExpr : sig
  (**
  * ``case`` expression (:rmlink:`4.5.7`).
  *)

  type t =
    [
      | `CaseExpr of
          case_expr_fields
    ]

  type fields = case_expr_fields =
    
  {
         
    f_expr: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
         
    f_cases: case_expr_alternative_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_expr :
    [< case_expr]
    -> [allocator | attribute_ref | base_aggregate | bin_op | call_expr | char_literal | concat_op | cond_expr | decl_expr | dotted_name | explicit_deref | identifier | membership_expr | null_literal | num_literal | paren_expr | qual_expr | quantified_expr | raise_expr | reduce_attribute_ref | string_literal | target_name | un_op | update_attribute_ref]

      
  val f_cases :
    [< case_expr]
    -> case_expr_alternative_list


end

module CondExpr : sig
  (**
  * Base class for a conditional expressions (:rmlink:`4.5.7`).
  *)

  type t =
    [
      | CaseExpr.t
      | IfExpr.t
    ]


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int


      
  val p_dependent_exprs :
    [< cond_expr ]
    -> expr list
  (**
  * Return the dependent expressions for this conditional expression.
  *)



end

module ConcatOperand : sig
  (**
  * A concatenation operator and its RHS operand.
  *
  * This node is used to represent the tuple ("&", operand) used by the
  * ``ConcatOp`` node to store its ``other_operands`` list.
  *)

  type t =
    [
      | `ConcatOperand of
          concat_operand_fields
    ]

  type fields = concat_operand_fields =
    
  {
         
    f_operator: op_concat
    Lazy.t;
         
    f_operand: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_operator :
    [< concat_operand]
    -> op_concat

      
  val f_operand :
    [< concat_operand]
    -> [allocator | attribute_ref | base_aggregate | bin_op | call_expr | char_literal | cond_expr | decl_expr | dotted_name | explicit_deref | identifier | null_literal | num_literal | paren_expr | qual_expr | quantified_expr | raise_expr | reduce_attribute_ref | string_literal | target_name | un_op | update_attribute_ref]


end

module ConcatOp : sig
  (**
  * Concatenation expression.
  *
  * Since concatenation expression can be huge in practice, this node handles
  * them as a list of operands rather than a deep tree of binary operators, in
  * order to avoid crashes while parsing of running name resolution on such
  * huge expression.
  *
  * The purpose of this node is to replace the arbitraty too deep tree of
  * binary operators (which can lead to a stack overflow), as for example with
  * ``"A & B & C & D & E"``:
  *
  * .. code::
  *
  *    BinOp(
  *      Binop(
  *        BinOp(
  *          BinOp(A, &, B), & , C), &, D), &, E)
  *
  * by a single operator, handling a list of operands that can be processed
  * without having to perform deep recursions:
  *
  * .. code::
  *
  *    ConcatOp(A,
  *      ConcatOperand(&, B),
  *      ConcatOperand(&, C),
  *      ConcatOperand(&, D),
  *      ConcatOperand(&, E))
  *)

  type t =
    [
      | `ConcatOp of
          concat_op_fields
    ]

  type fields = concat_op_fields =
    
  {
         
    f_first_operand: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
         
    f_other_operands: concat_operand_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int


      
  val p_operands :
    [< concat_op ]
    -> expr list
  (**
  * Return the operands of this concatenation expression
  *)


      
  val f_first_operand :
    [< concat_op]
    -> [allocator | attribute_ref | base_aggregate | bin_op | call_expr | char_literal | cond_expr | decl_expr | dotted_name | explicit_deref | identifier | null_literal | num_literal | paren_expr | qual_expr | quantified_expr | raise_expr | reduce_attribute_ref | string_literal | target_name | un_op | update_attribute_ref]

      
  val f_other_operands :
    [< concat_op]
    -> concat_operand_list


end

module CaseExprAlternative : sig
  (**
  * Alternative in a ``case`` expression (``when ... => ...``).
  *)

  type t =
    [
      | `CaseExprAlternative of
          case_expr_alternative_fields
    ]

  type fields = case_expr_alternative_fields =
    
  {
         
    f_choices: alternatives_list
    Lazy.t;
         
    f_expr: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_choices :
    [< case_expr_alternative]
    -> alternatives_list

      
  val f_expr :
    [< case_expr_alternative]
    -> [allocator | attribute_ref | base_aggregate | bin_op | call_expr | char_literal | concat_op | cond_expr | decl_expr | dotted_name | explicit_deref | identifier | membership_expr | null_literal | num_literal | paren_expr | qual_expr | quantified_expr | raise_expr | reduce_attribute_ref | string_literal | target_name | un_op | update_attribute_ref]


end

module BoxExpr : sig
  (**
  * Box expression (``<>``).
  *
  * This is not an expression per-se in Ada, but treating it as one helps us
  * keep coherent types in some cases, like aggregates expressions.
  *)

  type t =
    [
      | `BoxExpr of
          box_expr_fields
    ]

  type fields = box_expr_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module RelationOp : sig
  (**
  * Binary operation that compares two value, producing a boolean
  * (:rmlink:`4.4`).
  *)

  type t =
    [
      | `RelationOp of
          relation_op_fields
    ]

  type fields = relation_op_fields =
    
  {
         
    f_left: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
         
    f_op: [
      | `OpAnd
          of op_and_fields
      | `OpAndThen
          of op_and_then_fields
      | `OpDiv
          of op_div_fields
      | `OpDoubleDot
          of op_double_dot_fields
      | `OpEq
          of op_eq_fields
      | `OpGt
          of op_gt_fields
      | `OpGte
          of op_gte_fields
      | `OpLt
          of op_lt_fields
      | `OpLte
          of op_lte_fields
      | `OpMinus
          of op_minus_fields
      | `OpMod
          of op_mod_fields
      | `OpMult
          of op_mult_fields
      | `OpNeq
          of op_neq_fields
      | `OpOr
          of op_or_fields
      | `OpOrElse
          of op_or_else_fields
      | `OpPlus
          of op_plus_fields
      | `OpPow
          of op_pow_fields
      | `OpRem
          of op_rem_fields
      | `OpXor
          of op_xor_fields
    ]
    Lazy.t;
         
    f_right: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_left :
    [< relation_op]
    -> [allocator | attribute_ref | base_aggregate | bin_op | call_expr | char_literal | concat_op | cond_expr | decl_expr | dotted_name | explicit_deref | identifier | membership_expr | null_literal | num_literal | paren_expr | qual_expr | quantified_expr | raise_expr | reduce_attribute_ref | string_literal | target_name | un_op | update_attribute_ref]

      
  val f_op :
    [< relation_op]
    -> [op_and | op_and_then | op_div | op_double_dot | op_eq | op_gt | op_gte | op_lt | op_lte | op_minus | op_mod | op_mult | op_neq | op_or | op_or_else | op_plus | op_pow | op_rem | op_xor]

      
  val f_right :
    [< relation_op]
    -> [allocator | attribute_ref | base_aggregate | bin_op | call_expr | char_literal | concat_op | cond_expr | decl_expr | dotted_name | explicit_deref | identifier | membership_expr | null_literal | num_literal | paren_expr | qual_expr | quantified_expr | raise_expr | reduce_attribute_ref | string_literal | target_name | un_op | update_attribute_ref]


end

module BinOp : sig
  (**
  * Binary expression.
  *
  * This encompasses several ARM expressions, because it is used for every
  * binary expression in Ada, all documented in ::rmlink:`4.4`.
  *)

  type t =
    [
      | `BinOp of
          bin_op_fields
      | `RelationOp of
          relation_op_fields
    ]

  type fields = bin_op_fields =
    
  {
         
    f_left: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
         
    f_op: [
      | `OpAnd
          of op_and_fields
      | `OpAndThen
          of op_and_then_fields
      | `OpDiv
          of op_div_fields
      | `OpDoubleDot
          of op_double_dot_fields
      | `OpEq
          of op_eq_fields
      | `OpGt
          of op_gt_fields
      | `OpGte
          of op_gte_fields
      | `OpLt
          of op_lt_fields
      | `OpLte
          of op_lte_fields
      | `OpMinus
          of op_minus_fields
      | `OpMod
          of op_mod_fields
      | `OpMult
          of op_mult_fields
      | `OpNeq
          of op_neq_fields
      | `OpOr
          of op_or_fields
      | `OpOrElse
          of op_or_else_fields
      | `OpPlus
          of op_plus_fields
      | `OpPow
          of op_pow_fields
      | `OpRem
          of op_rem_fields
      | `OpXor
          of op_xor_fields
    ]
    Lazy.t;
         
    f_right: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_left :
    [< bin_op]
    -> [allocator | attribute_ref | base_aggregate | bin_op | call_expr | char_literal | concat_op | cond_expr | decl_expr | dotted_name | explicit_deref | identifier | membership_expr | null_literal | num_literal | paren_expr | qual_expr | quantified_expr | raise_expr | reduce_attribute_ref | string_literal | target_name | un_op | update_attribute_ref]

      
  val f_op :
    [< bin_op]
    -> [op_and | op_and_then | op_div | op_double_dot | op_eq | op_gt | op_gte | op_lt | op_lte | op_minus | op_mod | op_mult | op_neq | op_or | op_or_else | op_plus | op_pow | op_rem | op_xor]

      
  val f_right :
    [< bin_op]
    -> [allocator | attribute_ref | base_aggregate | bin_op | call_expr | char_literal | concat_op | cond_expr | decl_expr | dotted_name | explicit_deref | identifier | membership_expr | null_literal | num_literal | paren_expr | qual_expr | quantified_expr | raise_expr | reduce_attribute_ref | string_literal | target_name | un_op | update_attribute_ref]


end

module NullRecordAggregate : sig
  (**
  * Aggregate for ``null record`` (:rmlink:`4.3`).
  *)

  type t =
    [
      | `NullRecordAggregate of
          null_record_aggregate_fields
    ]

  type fields = null_record_aggregate_fields =
    
  {
         
    f_ancestor_expr: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    option
    Lazy.t;
         
    f_assocs: basic_assoc_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_ancestor_expr :
    [< null_record_aggregate]
    -> [allocator | attribute_ref | base_aggregate | bin_op | call_expr | char_literal | concat_op | cond_expr | decl_expr | dotted_name | explicit_deref | identifier | membership_expr | null_literal | num_literal | paren_expr | qual_expr | quantified_expr | raise_expr | reduce_attribute_ref | string_literal | target_name | un_op | update_attribute_ref] option

      
  val f_assocs :
    [< null_record_aggregate]
    -> basic_assoc_list


end

module BracketDeltaAggregate : sig
  (**
  * Bracket delta aggregate (Ada 2020, :rmlink:`4.3`).
  *)

  type t =
    [
      | `BracketDeltaAggregate of
          bracket_delta_aggregate_fields
    ]

  type fields = bracket_delta_aggregate_fields =
    
  {
         
    f_ancestor_expr: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    option
    Lazy.t;
         
    f_assocs: basic_assoc_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_ancestor_expr :
    [< bracket_delta_aggregate]
    -> [allocator | attribute_ref | base_aggregate | bin_op | call_expr | char_literal | concat_op | cond_expr | decl_expr | dotted_name | explicit_deref | identifier | membership_expr | null_literal | num_literal | paren_expr | qual_expr | quantified_expr | raise_expr | reduce_attribute_ref | string_literal | target_name | un_op | update_attribute_ref] option

      
  val f_assocs :
    [< bracket_delta_aggregate]
    -> basic_assoc_list


end

module DeltaAggregate : sig
  (**
  * Aggregate for delta aggregate (Ada 2022, :rmlink:`4.3`).
  *)

  type t =
    [
      | `DeltaAggregate of
          delta_aggregate_fields
      | `BracketDeltaAggregate of
          bracket_delta_aggregate_fields
    ]

  type fields = delta_aggregate_fields =
    
  {
         
    f_ancestor_expr: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    option
    Lazy.t;
         
    f_assocs: basic_assoc_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_ancestor_expr :
    [< delta_aggregate]
    -> [allocator | attribute_ref | base_aggregate | bin_op | call_expr | char_literal | concat_op | cond_expr | decl_expr | dotted_name | explicit_deref | identifier | membership_expr | null_literal | num_literal | paren_expr | qual_expr | quantified_expr | raise_expr | reduce_attribute_ref | string_literal | target_name | un_op | update_attribute_ref] option

      
  val f_assocs :
    [< delta_aggregate]
    -> basic_assoc_list


end

module BracketAggregate : sig
  (**
  * Bracket array or container aggregate (Ada 2020, :rmlink:`4.3`).
  *)

  type t =
    [
      | `BracketAggregate of
          bracket_aggregate_fields
    ]

  type fields = bracket_aggregate_fields =
    
  {
         
    f_ancestor_expr: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    option
    Lazy.t;
         
    f_assocs: basic_assoc_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_ancestor_expr :
    [< bracket_aggregate]
    -> [allocator | attribute_ref | base_aggregate | bin_op | call_expr | char_literal | concat_op | cond_expr | decl_expr | dotted_name | explicit_deref | identifier | membership_expr | null_literal | num_literal | paren_expr | qual_expr | quantified_expr | raise_expr | reduce_attribute_ref | string_literal | target_name | un_op | update_attribute_ref] option

      
  val f_assocs :
    [< bracket_aggregate]
    -> basic_assoc_list


end

module Aggregate : sig
  (**
  * Aggregate that is not a ``null record`` aggregate (:rmlink:`4.3`).
  *)

  type t =
    [
      | `Aggregate of
          aggregate_fields
      | `BracketAggregate of
          bracket_aggregate_fields
    ]

  type fields = aggregate_fields =
    
  {
         
    f_ancestor_expr: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    option
    Lazy.t;
         
    f_assocs: basic_assoc_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_ancestor_expr :
    [< aggregate]
    -> [allocator | attribute_ref | base_aggregate | bin_op | call_expr | char_literal | concat_op | cond_expr | decl_expr | dotted_name | explicit_deref | identifier | membership_expr | null_literal | num_literal | paren_expr | qual_expr | quantified_expr | raise_expr | reduce_attribute_ref | string_literal | target_name | un_op | update_attribute_ref] option

      
  val f_assocs :
    [< aggregate]
    -> basic_assoc_list


end

module BaseAggregate : sig
  (**
  * Base class for aggregates (:rmlink:`4.3`).
  *)

  type t =
    [
      | Aggregate.t
      | DeltaAggregate.t
      | NullRecordAggregate.t
    ]


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int


      
  val p_aggregate_params :
    [< base_aggregate ]
    -> ParamActual.t list
  (**
  * Returns an array of pairs, associating formal parameters to actual
  * expressions. See ``zip_with_params``.
  *)

      
  val p_is_subaggregate :
    [< base_aggregate ]
    -> bool
  (**
  * Return whether this aggregate is actually a subaggregate of a
  * multidimensional array aggregate, as described in :rmlink:`4.3.3`.
  *)


      
  val f_ancestor_expr :
    [< base_aggregate]
    -> [allocator | attribute_ref | base_aggregate | bin_op | call_expr | char_literal | concat_op | cond_expr | decl_expr | dotted_name | explicit_deref | identifier | membership_expr | null_literal | num_literal | paren_expr | qual_expr | quantified_expr | raise_expr | reduce_attribute_ref | string_literal | target_name | un_op | update_attribute_ref] option

      
  val f_assocs :
    [< base_aggregate]
    -> basic_assoc_list


end

module Allocator : sig
  (**
  * Allocator expression (``new ...``) (:rmlink:`4.8`).
  *)

  type t =
    [
      | `Allocator of
          allocator_fields
    ]

  type fields = allocator_fields =
    
  {
         
    f_subpool: [
      | `AttributeRef
          of attribute_ref_fields
      | `CallExpr
          of call_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `QualExpr
          of qual_expr_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    option
    Lazy.t;
         
    f_type_or_expr: [
      | `ConstrainedSubtypeIndication
          of constrained_subtype_indication_fields
      | `DiscreteSubtypeIndication
          of discrete_subtype_indication_fields
      | `QualExpr
          of qual_expr_fields
      | `SubtypeIndication
          of subtype_indication_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int


      
  val p_get_allocated_type :
    [< allocator ]
    -> base_type_decl option
  (**
  * Return the allocated type for this allocator.
  *)


      
  val f_subpool :
    [< allocator]
    -> [attribute_ref | call_expr | char_literal | dotted_name | explicit_deref | identifier | qual_expr | reduce_attribute_ref | string_literal | target_name | update_attribute_ref] option

      
  val f_type_or_expr :
    [< allocator]
    -> [qual_expr | subtype_indication]


end

module AbstractStateDeclExpr : sig
  (**
  * Directly corresponds to the right-hand side of the Abstract_State aspect.
  * Only exists because the RHS of an AspectAssoc must be an expression: the
  * actual logic is in AbstractStateDecl.
  *)

  type t =
    [
      | `AbstractStateDeclExpr of
          abstract_state_decl_expr_fields
    ]

  type fields = abstract_state_decl_expr_fields =
    
  {
         
    f_state_decl: [
      | `AbstractStateDecl
          of abstract_state_decl_fields
      | `MultiAbstractStateDecl
          of multi_abstract_state_decl_fields
      | `ParenAbstractStateDecl
          of paren_abstract_state_decl_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_state_decl :
    [< abstract_state_decl_expr]
    -> [abstract_state_decl | multi_abstract_state_decl | paren_abstract_state_decl]


end

module Expr : sig
  (**
  * Base class for expressions (:rmlink:`4.4`).
  *)

  type t =
    [
      | AbstractStateDeclExpr.t
      | Allocator.t
      | BaseAggregate.t
      | BinOp.t
      | BoxExpr.t
      | CaseExprAlternative.t
      | ConcatOp.t
      | ConcatOperand.t
      | CondExpr.t
      | ContractCases.t
      | DeclExpr.t
      | MembershipExpr.t
      | Name.t
      | ParenExpr.t
      | QuantifiedExpr.t
      | RaiseExpr.t
      | UnOp.t
    ]


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int


      
  val p_expression_type :
    [< expr ]
    -> base_type_decl option
  (**
  * Return the declaration corresponding to the type of this expression after
  * name resolution.
  *)

      
  val p_expected_expression_type :
    [< expr ]
    -> base_type_decl option
  (**
  * Return the declaration corresponding to the expected type of this
  * expression after name resolution.
  *)

      
  val p_is_dynamically_tagged :
    ?imprecise_fallback:
    bool
    -> [< expr ]
    -> bool
  (**
  * Returns whether this expression is dynamically tagged (See
  * :rmlink:`3.9.2`).
  *)

      
  val p_is_dispatching_call :
    ?imprecise_fallback:
    bool
    -> [< expr ]
    -> bool
  (**
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
  *)

      
  val p_is_static_expr :
    ?imprecise_fallback:
    bool
    -> [< expr ]
    -> bool
  (**
  * Return whether this expression is static according to the ARM definition of
  * static. See :rmlink:`4.9`.
  *)

      
  val p_first_corresponding_decl :
    [< expr ]
    -> basic_decl option
  (**
  * Return the first decl that is lexically named like self in self's scope.
  *)

      
  val p_eval_as_int :
    [< expr ]
    -> BigInteger.t
  (**
  * Statically evaluates self, and returns the value of the evaluation as an
  * integer.
  *
  * .. note:: In order for a call to this not to raise, the expression needs to
  *    be a static expression, as specified in :rmlink:`4.9`. You can verify
  *    whether an expression is static with the ``is_static_expr`` property.
  *
  * .. attention:: This is an experimental feature, so even if it is exposed to
  *    allow experiments, it is totally unsupported and the API and behavior
  *    are very likely to change in the future.
  *)

      
  val p_eval_as_int_in_env :
    [< expr ]
    -> Substitution.t list
    -> BigInteger.t
  (**
  * Statically evaluates self, and returns the value of the evaluation as an
  * integer. The given environment is used to substitute references to
  * declarations by actual values.
  *
  * .. note:: In order for a call to this not to raise, the expression needs to
  *    be a static expression, as specified in :rmlink:`4.9`. You can verify
  *    whether an expression is static with the ``is_static_expr`` property.
  *
  * .. attention:: This is an experimental feature, so even if it is exposed to
  *    allow experiments, it is totally unsupported and the API and behavior
  *    are very likely to change in the future.
  *)

      
  val p_eval_as_string :
    [< expr ]
    -> string
  (**
  * Statically evaluates self, and returns the value of the evaluation as a
  * string.
  *
  * .. note:: In order for a call to this not to raise, the expression needs to
  *    be a static expression, as specified in :rmlink:`4.9`. You can verify
  *    whether an expression is static with the ``is_static_expr`` property.
  *
  * .. attention:: This is an experimental feature, so even if it is exposed to
  *    allow experiments, it is totally unsupported and the API and behavior
  *    are very likely to change in the future.
  *)

      
  val p_eval_as_string_in_env :
    [< expr ]
    -> Substitution.t list
    -> string
  (**
  * Statically evaluates self, and returns the value of the evaluation as a
  * string. The given environment is used to substitute references to
  * declarations by actual values.
  *
  * .. note:: In order for a call to this not to raise, the expression needs to
  *    be a static expression, as specified in :rmlink:`4.9`. You can verify
  *    whether an expression is static with the ``is_static_expr`` property.
  *
  * .. attention:: This is an experimental feature, so even if it is exposed to
  *    allow experiments, it is totally unsupported and the API and behavior
  *    are very likely to change in the future.
  *)

      
  val p_matching_nodes :
    [< expr ]
    -> ada_node list
  (**
  * Return the list of AST nodes that can be a match for this expression before
  * overloading analysis.
  *)



end

module ElsifStmtPart : sig
  (**
  * ``elsif`` part in an ``if`` statement block.
  *)

  type t =
    [
      | `ElsifStmtPart of
          elsif_stmt_part_fields
    ]

  type fields = elsif_stmt_part_fields =
    
  {
         
    f_cond_expr: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
         
    f_stmts: stmt_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_cond_expr :
    [< elsif_stmt_part]
    -> [allocator | attribute_ref | base_aggregate | bin_op | call_expr | char_literal | concat_op | cond_expr | decl_expr | dotted_name | explicit_deref | identifier | membership_expr | null_literal | num_literal | paren_expr | qual_expr | quantified_expr | raise_expr | reduce_attribute_ref | string_literal | target_name | un_op | update_attribute_ref]

      
  val f_stmts :
    [< elsif_stmt_part]
    -> stmt_list


end

module ElsifExprPart : sig
  (**
  * ``elsif`` block, part of an ``if`` expression.
  *)

  type t =
    [
      | `ElsifExprPart of
          elsif_expr_part_fields
    ]

  type fields = elsif_expr_part_fields =
    
  {
         
    f_cond_expr: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
         
    f_then_expr: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_cond_expr :
    [< elsif_expr_part]
    -> [allocator | attribute_ref | base_aggregate | bin_op | call_expr | char_literal | concat_op | cond_expr | decl_expr | dotted_name | explicit_deref | identifier | membership_expr | null_literal | num_literal | paren_expr | qual_expr | quantified_expr | raise_expr | reduce_attribute_ref | string_literal | target_name | un_op | update_attribute_ref]

      
  val f_then_expr :
    [< elsif_expr_part]
    -> [allocator | attribute_ref | base_aggregate | bin_op | call_expr | char_literal | concat_op | cond_expr | decl_expr | dotted_name | explicit_deref | identifier | membership_expr | null_literal | num_literal | paren_expr | qual_expr | quantified_expr | raise_expr | reduce_attribute_ref | string_literal | target_name | un_op | update_attribute_ref]


end

module PublicPart : sig
  (**
  * List of declarations in a public part.
  *)

  type t =
    [
      | `PublicPart of
          public_part_fields
    ]

  type fields = public_part_fields =
    
  {
         
    f_decls: ada_node_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_decls :
    [< public_part]
    -> ada_node_list


end

module PrivatePart : sig
  (**
  * List of declarations in a private part.
  *)

  type t =
    [
      | `PrivatePart of
          private_part_fields
    ]

  type fields = private_part_fields =
    
  {
         
    f_decls: ada_node_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_decls :
    [< private_part]
    -> ada_node_list


end

module DeclarativePart : sig
  (**
  * List of declarations (:rmlink:`3.11`).
  *)

  type t =
    [
      | `DeclarativePart of
          declarative_part_fields
      | `PrivatePart of
          private_part_fields
      | `PublicPart of
          public_part_fields
    ]

  type fields = declarative_part_fields =
    
  {
         
    f_decls: ada_node_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_decls :
    [< declarative_part]
    -> ada_node_list


end

module RangeConstraint : sig
  (**
  * Range-based type constraint (:rmlink:`3.5`).
  *)

  type t =
    [
      | `RangeConstraint of
          range_constraint_fields
    ]

  type fields = range_constraint_fields =
    
  {
         
    f_range: range_spec
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_range :
    [< range_constraint]
    -> range_spec


end

module DigitsConstraint : sig
  (**
  * Digits and range type constraint (:rmlink:`3.5.9`).
  *)

  type t =
    [
      | `DigitsConstraint of
          digits_constraint_fields
    ]

  type fields = digits_constraint_fields =
    
  {
         
    f_digits: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
         
    f_range: range_spec
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_digits :
    [< digits_constraint]
    -> [allocator | attribute_ref | base_aggregate | bin_op | call_expr | char_literal | concat_op | cond_expr | decl_expr | dotted_name | explicit_deref | identifier | null_literal | num_literal | paren_expr | qual_expr | quantified_expr | raise_expr | reduce_attribute_ref | string_literal | target_name | un_op | update_attribute_ref]

      
  val f_range :
    [< digits_constraint]
    -> range_spec option


end

module DeltaConstraint : sig
  (**
  * Delta and range type constraint (:rmlink:`J.3`).
  *)

  type t =
    [
      | `DeltaConstraint of
          delta_constraint_fields
    ]

  type fields = delta_constraint_fields =
    
  {
         
    f_digits: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
         
    f_range: range_spec
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_digits :
    [< delta_constraint]
    -> [allocator | attribute_ref | base_aggregate | bin_op | call_expr | char_literal | concat_op | cond_expr | decl_expr | dotted_name | explicit_deref | identifier | null_literal | num_literal | paren_expr | qual_expr | quantified_expr | raise_expr | reduce_attribute_ref | string_literal | target_name | un_op | update_attribute_ref]

      
  val f_range :
    [< delta_constraint]
    -> range_spec option


end

module CompositeConstraint : sig
  (**
  * Constraint for a composite type (:rmlink:`3.6.1`). Due to ambiguities in
  * the Ada grammar, this could be either a list of index constraints, if the
  * owning type is an array type, or a list of discriminant constraints, if the
  * owning type is a discriminated record type.
  *)

  type t =
    [
      | `CompositeConstraint of
          composite_constraint_fields
    ]

  type fields = composite_constraint_fields =
    
  {
         
    f_constraints: basic_assoc_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int


      
  val p_is_index_constraint :
    [< composite_constraint ]
    -> bool
  (**
  * Whether this composite constraint is an index constraint.
  *)

      
  val p_is_discriminant_constraint :
    [< composite_constraint ]
    -> bool
  (**
  * Whether this composite constraint is a discriminant constraint.
  *)


      
  val f_constraints :
    [< composite_constraint]
    -> basic_assoc_list


end

module Constraint : sig
  (**
  * Base class for type constraints (:rmlink:`3.2.2`).
  *)

  type t =
    [
      | CompositeConstraint.t
      | DeltaConstraint.t
      | DigitsConstraint.t
      | RangeConstraint.t
    ]


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module ConstantPresent : sig
  (**

  *)

  type t =
    [
      | `ConstantPresent of
          constant_present_fields
    ]

  type fields = constant_present_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module ConstantAbsent : sig
  (**

  *)

  type t =
    [
      | `ConstantAbsent of
          constant_absent_fields
    ]

  type fields = constant_absent_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module ConstantNode : sig
  (**
  * Qualifier for the ``constant`` keyword.
  *)

  type t =
    [
      | ConstantAbsent.t
      | ConstantPresent.t
    ]


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int


      
  val p_as_bool :
    [< constant_node ]
    -> bool
  (**
  * Return whether this is an instance of ConstantPresent
  *)



end

module ComponentDef : sig
  (**
  * Definition for a component (:rmlink:`3.6`).
  *)

  type t =
    [
      | `ComponentDef of
          component_def_fields
    ]

  type fields = component_def_fields =
    
  {
         
    f_has_aliased: aliased_node
    Lazy.t;
         
    f_has_constant: constant_node
    Lazy.t;
         
    f_type_expr: [
      | `AnonymousType
          of anonymous_type_fields
      | `ConstrainedSubtypeIndication
          of constrained_subtype_indication_fields
      | `DiscreteSubtypeIndication
          of discrete_subtype_indication_fields
      | `SubtypeIndication
          of subtype_indication_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_has_aliased :
    [< component_def]
    -> aliased_node

      
  val f_has_constant :
    [< component_def]
    -> constant_node

      
  val f_type_expr :
    [< component_def]
    -> [anonymous_type | subtype_indication]


end

module ComponentClause : sig
  (**
  * Representation clause for a single component (:rmlink:`13.5.1`).
  *)

  type t =
    [
      | `ComponentClause of
          component_clause_fields
    ]

  type fields = component_clause_fields =
    
  {
         
    f_id: identifier
    Lazy.t;
         
    f_position: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
         
    f_range: range_spec
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_id :
    [< component_clause]
    -> identifier

      
  val f_position :
    [< component_clause]
    -> [allocator | attribute_ref | base_aggregate | bin_op | call_expr | char_literal | concat_op | cond_expr | decl_expr | dotted_name | explicit_deref | identifier | null_literal | num_literal | paren_expr | qual_expr | quantified_expr | raise_expr | reduce_attribute_ref | string_literal | target_name | un_op | update_attribute_ref]

      
  val f_range :
    [< component_clause]
    -> range_spec


end

module CompilationUnit : sig
  (**
  * Root node for all Ada analysis units (:rmlink:`10.1.1`).
  *)

  type t =
    [
      | `CompilationUnit of
          compilation_unit_fields
    ]

  type fields = compilation_unit_fields =
    
  {
         
    f_prelude: ada_node_list
    Lazy.t;
         
    f_body: [
      | `LibraryItem
          of library_item_fields
      | `Subunit
          of subunit_fields
    ]
    Lazy.t;
         
    f_pragmas: pragma_node_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int


      
  val p_syntactic_fully_qualified_name :
    [< compilation_unit ]
    -> string list
  (**
  * Return the syntactic fully qualified name of this compilation unit.
  *)

      
  val p_unit_kind :
    [< compilation_unit ]
    -> AnalysisUnitKind.t
  (**
  * Return the kind corresponding to this analysis unit.
  *)

      
  val p_withed_units :
    [< compilation_unit ]
    -> compilation_unit list
  (**
  * Look for all "with" clauses at the top of this compilation unit and return
  * all the compilation units designated by them. For the complete dependencies
  * list of compilation units, see the ``unit_dependencies`` property.
  *)

      
  val p_imported_units :
    [< compilation_unit ]
    -> compilation_unit list
  (**
  * Return all the compilation units that are directly imported by this one.
  * This includes "with"ed units as well as the direct parent unit.
  *)

      
  val p_unit_dependencies :
    [< compilation_unit ]
    -> compilation_unit list
  (**
  * Return the list of all the compilation units that are (direct and indirect)
  * dependencies of this one. See the ``withed_units``/``imported_units``
  * properties to only get the direct dependencies of this unit.
  *)

      
  val p_decl :
    [< compilation_unit ]
    -> basic_decl option
  (**
  * Get the root basic decl defined in this compilation unit.
  *)

      
  val p_is_preelaborable :
    ?imprecise_fallback:
    bool
    -> [< compilation_unit ]
    -> bool
  (**
  * Whether this compilation unit is preelaborable or not.
  *)

      
  val p_other_part :
    [< compilation_unit ]
    -> compilation_unit option
  (**
  * If this compilation unit is of kind UnitSpecification, return its
  * corresponding body unit, and conversely.
  *)

      
  val p_has_restriction :
    [< compilation_unit ]
    -> string
    -> bool
  (**
  * Whether this compilation unit is affected by the restriction with the given
  * name.
  *
  * .. warning:: This property only supports the ``No_Elaboration_Code``
  *    restriction for now.
  *)

      
  val p_all_config_pragmas :
    [< compilation_unit ]
    -> pragma_node list
  (**
  * Return the list of configuration pragmas that apply to the current unit.
  *
  * .. note:: Using this property before creating the configuration pragmas
  *    files mapping using subprograms from the ``Libadalang.Config_Pragmas``
  *    package will raise an error.
  *)

      
  val p_config_pragmas :
    [< compilation_unit ]
    -> string
    -> pragma_node list
  (**
  * Return the list of configuration pragmas wih the given name that apply to
  * the current unit.
  *
  * .. note:: Using this property before creating the configuration pragmas
  *    files mapping using subprograms from the ``Libadalang.Config_Pragmas``
  *    package will raise an error.
  *)


      
  val f_prelude :
    [< compilation_unit]
    -> ada_node_list

      
  val f_body :
    [< compilation_unit]
    -> [library_item | subunit]

      
  val f_pragmas :
    [< compilation_unit]
    -> pragma_node_list


end

module CaseStmtAlternative : sig
  (**
  * Alternative in a ``case`` statement (``when ... => ...``).
  *)

  type t =
    [
      | `CaseStmtAlternative of
          case_stmt_alternative_fields
    ]

  type fields = case_stmt_alternative_fields =
    
  {
         
    f_choices: alternatives_list
    Lazy.t;
         
    f_stmts: stmt_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_choices :
    [< case_stmt_alternative]
    -> alternatives_list

      
  val f_stmts :
    [< case_stmt_alternative]
    -> stmt_list


end

module SingleTaskDecl : sig
  (**
  * Declaration for a single task (:rmlink:`9.1`).
  *)

  type t =
    [
      | `SingleTaskDecl of
          single_task_decl_fields
    ]

  type fields = single_task_decl_fields =
    
  {
         
    f_task_type: single_task_type_decl
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_task_type :
    [< single_task_decl]
    -> single_task_type_decl


end

module SingleProtectedDecl : sig
  (**
  * Declaration for a single protected object (:rmlink:`9.4`).
  *)

  type t =
    [
      | `SingleProtectedDecl of
          single_protected_decl_fields
    ]

  type fields = single_protected_decl_fields =
    
  {
         
    f_name: defining_name
    Lazy.t;
         
    f_interfaces: parent_list
    Lazy.t;
         
    f_definition: protected_def
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_name :
    [< single_protected_decl]
    -> defining_name

      
  val f_interfaces :
    [< single_protected_decl]
    -> parent_list

      
  val f_definition :
    [< single_protected_decl]
    -> protected_def


end

module PackageRenamingDecl : sig
  (**
  * Declaration for a package renaming (:rmlink:`8.5.3`).
  *)

  type t =
    [
      | `PackageRenamingDecl of
          package_renaming_decl_fields
    ]

  type fields = package_renaming_decl_fields =
    
  {
         
    f_name: defining_name
    Lazy.t;
         
    f_renames: renaming_clause
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int


      
  val p_renamed_package :
    [< package_renaming_decl ]
    -> basic_decl option
  (**
  * Return the declaration of the package that is renamed by Self.
  *)

      
  val p_final_renamed_package :
    [< package_renaming_decl ]
    -> basic_decl option
  (**
  * Return the declaration of the package that is ultimately renamed by Self,
  * skipping through all intermediate package renamings.
  *)


      
  val f_name :
    [< package_renaming_decl]
    -> defining_name

      
  val f_renames :
    [< package_renaming_decl]
    -> renaming_clause


end

module NoTypeObjectRenamingDecl : sig
  (**
  * Object declaration without subtype indication. This node has been
  * introduced to cover a special case for ``ObjectDecl``, where ``type_expr``
  * is made optional (AI12-0275), and therefore cannot fit in an
  * ``ObjectDecl``.
  *)

  type t =
    [
      | `NoTypeObjectRenamingDecl of
          no_type_object_renaming_decl_fields
    ]

  type fields = no_type_object_renaming_decl_fields =
    
  {
         
    f_ids: defining_name_list
    Lazy.t;
         
    f_has_aliased: aliased_node
    option
    Lazy.t;
         
    f_has_constant: constant_node
    option
    Lazy.t;
         
    f_mode: mode
    option
    Lazy.t;
         
    f_type_expr: [
      | `AnonymousType
          of anonymous_type_fields
      | `ConstrainedSubtypeIndication
          of constrained_subtype_indication_fields
      | `DiscreteSubtypeIndication
          of discrete_subtype_indication_fields
      | `SubtypeIndication
          of subtype_indication_fields
    ]
    option
    Lazy.t;
         
    f_default_expr: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    option
    Lazy.t;
         
    f_renaming_clause: renaming_clause
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_ids :
    [< no_type_object_renaming_decl]
    -> defining_name_list

      
  val f_has_aliased :
    [< no_type_object_renaming_decl]
    -> aliased_node option

      
  val f_has_constant :
    [< no_type_object_renaming_decl]
    -> constant_node option

      
  val f_mode :
    [< no_type_object_renaming_decl]
    -> mode option

      
  val f_type_expr :
    [< no_type_object_renaming_decl]
    -> [anonymous_type | subtype_indication] option

      
  val f_default_expr :
    [< no_type_object_renaming_decl]
    -> [allocator | attribute_ref | base_aggregate | bin_op | call_expr | char_literal | concat_op | cond_expr | decl_expr | dotted_name | explicit_deref | identifier | membership_expr | null_literal | num_literal | paren_expr | qual_expr | quantified_expr | raise_expr | reduce_attribute_ref | string_literal | target_name | un_op | update_attribute_ref] option

      
  val f_renaming_clause :
    [< no_type_object_renaming_decl]
    -> renaming_clause option


end

module ExtendedReturnStmtObjectDecl : sig
  (**
  * Object declaration that is part of an extended return statement
  * (:rmlink:`6.5`).
  *)

  type t =
    [
      | `ExtendedReturnStmtObjectDecl of
          extended_return_stmt_object_decl_fields
    ]

  type fields = extended_return_stmt_object_decl_fields =
    
  {
         
    f_ids: defining_name_list
    Lazy.t;
         
    f_has_aliased: aliased_node
    option
    Lazy.t;
         
    f_has_constant: constant_node
    option
    Lazy.t;
         
    f_mode: mode
    option
    Lazy.t;
         
    f_type_expr: [
      | `AnonymousType
          of anonymous_type_fields
      | `ConstrainedSubtypeIndication
          of constrained_subtype_indication_fields
      | `DiscreteSubtypeIndication
          of discrete_subtype_indication_fields
      | `SubtypeIndication
          of subtype_indication_fields
    ]
    option
    Lazy.t;
         
    f_default_expr: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    option
    Lazy.t;
         
    f_renaming_clause: renaming_clause
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_ids :
    [< extended_return_stmt_object_decl]
    -> defining_name_list

      
  val f_has_aliased :
    [< extended_return_stmt_object_decl]
    -> aliased_node option

      
  val f_has_constant :
    [< extended_return_stmt_object_decl]
    -> constant_node option

      
  val f_mode :
    [< extended_return_stmt_object_decl]
    -> mode option

      
  val f_type_expr :
    [< extended_return_stmt_object_decl]
    -> [anonymous_type | subtype_indication] option

      
  val f_default_expr :
    [< extended_return_stmt_object_decl]
    -> [allocator | attribute_ref | base_aggregate | bin_op | call_expr | char_literal | concat_op | cond_expr | decl_expr | dotted_name | explicit_deref | identifier | membership_expr | null_literal | num_literal | paren_expr | qual_expr | quantified_expr | raise_expr | reduce_attribute_ref | string_literal | target_name | un_op | update_attribute_ref] option

      
  val f_renaming_clause :
    [< extended_return_stmt_object_decl]
    -> renaming_clause option


end

module ObjectDecl : sig
  (**
  * Base class for Ada object declarations (:rmlink:`3.3.1`). Ada object
  * declarations are variables/constants declarations that can be declared in
  * any declarative scope.
  *)

  type t =
    [
      | `ObjectDecl of
          object_decl_fields
      | `ExtendedReturnStmtObjectDecl of
          extended_return_stmt_object_decl_fields
      | `NoTypeObjectRenamingDecl of
          no_type_object_renaming_decl_fields
    ]

  type fields = object_decl_fields =
    
  {
         
    f_ids: defining_name_list
    Lazy.t;
         
    f_has_aliased: aliased_node
    option
    Lazy.t;
         
    f_has_constant: constant_node
    option
    Lazy.t;
         
    f_mode: mode
    option
    Lazy.t;
         
    f_type_expr: [
      | `AnonymousType
          of anonymous_type_fields
      | `ConstrainedSubtypeIndication
          of constrained_subtype_indication_fields
      | `DiscreteSubtypeIndication
          of discrete_subtype_indication_fields
      | `SubtypeIndication
          of subtype_indication_fields
    ]
    option
    Lazy.t;
         
    f_default_expr: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    option
    Lazy.t;
         
    f_renaming_clause: renaming_clause
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int


      
  val p_private_part_decl :
    [< object_decl ]
    -> basic_decl option
  (**
  * If this object decl is the constant completion of an object decl in the
  * public part, return the object decl from the public part.
  *)

      
  val p_public_part_decl :
    [< object_decl ]
    -> basic_decl option
  (**
  * If this object decl is the incomplete declaration of a constant in a public
  * part, return its completion in the private part.
  *)


      
  val f_ids :
    [< object_decl]
    -> defining_name_list

      
  val f_has_aliased :
    [< object_decl]
    -> aliased_node option

      
  val f_has_constant :
    [< object_decl]
    -> constant_node option

      
  val f_mode :
    [< object_decl]
    -> mode option

      
  val f_type_expr :
    [< object_decl]
    -> [anonymous_type | subtype_indication] option

      
  val f_default_expr :
    [< object_decl]
    -> [allocator | attribute_ref | base_aggregate | bin_op | call_expr | char_literal | concat_op | cond_expr | decl_expr | dotted_name | explicit_deref | identifier | membership_expr | null_literal | num_literal | paren_expr | qual_expr | quantified_expr | raise_expr | reduce_attribute_ref | string_literal | target_name | un_op | update_attribute_ref] option

      
  val f_renaming_clause :
    [< object_decl]
    -> renaming_clause option


end

module NumberDecl : sig
  (**
  * Declaration for a static constant number (:rmlink:`3.3.2`).
  *)

  type t =
    [
      | `NumberDecl of
          number_decl_fields
    ]

  type fields = number_decl_fields =
    
  {
         
    f_ids: defining_name_list
    Lazy.t;
         
    f_expr: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_ids :
    [< number_decl]
    -> defining_name_list

      
  val f_expr :
    [< number_decl]
    -> [allocator | attribute_ref | base_aggregate | bin_op | call_expr | char_literal | concat_op | cond_expr | decl_expr | dotted_name | explicit_deref | identifier | null_literal | num_literal | paren_expr | qual_expr | quantified_expr | raise_expr | reduce_attribute_ref | string_literal | target_name | un_op | update_attribute_ref]


end

module NamedStmtDecl : sig
  (**
  * BasicDecl that is always the declaration inside a named statement.
  *)

  type t =
    [
      | `NamedStmtDecl of
          named_stmt_decl_fields
    ]

  type fields = named_stmt_decl_fields =
    
  {
         
    f_name: defining_name
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_name :
    [< named_stmt_decl]
    -> defining_name


end

module LabelDecl : sig
  (**
  * Declaration for a code label (:rmlink:`5.1`).
  *)

  type t =
    [
      | `LabelDecl of
          label_decl_fields
    ]

  type fields = label_decl_fields =
    
  {
         
    f_name: defining_name
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_name :
    [< label_decl]
    -> defining_name


end

module GenericSubpRenamingDecl : sig
  (**
  * Declaration for a generic subprogram renaming.
  *)

  type t =
    [
      | `GenericSubpRenamingDecl of
          generic_subp_renaming_decl_fields
    ]

  type fields = generic_subp_renaming_decl_fields =
    
  {
         
    f_kind: subp_kind
    Lazy.t;
         
    f_name: defining_name
    Lazy.t;
         
    f_renames: [
      | `CharLiteral
          of char_literal_fields
      | `DottedName
          of dotted_name_fields
      | `Identifier
          of identifier_fields
      | `StringLiteral
          of string_literal_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_kind :
    [< generic_subp_renaming_decl]
    -> subp_kind

      
  val f_name :
    [< generic_subp_renaming_decl]
    -> defining_name

      
  val f_renames :
    [< generic_subp_renaming_decl]
    -> [char_literal | dotted_name | identifier | string_literal]


end

module GenericPackageRenamingDecl : sig
  (**
  * Declaration for a generic package renaming (:rmlink:`8.5.5`).
  *)

  type t =
    [
      | `GenericPackageRenamingDecl of
          generic_package_renaming_decl_fields
    ]

  type fields = generic_package_renaming_decl_fields =
    
  {
         
    f_name: defining_name
    Lazy.t;
         
    f_renames: [
      | `CharLiteral
          of char_literal_fields
      | `DottedName
          of dotted_name_fields
      | `Identifier
          of identifier_fields
      | `StringLiteral
          of string_literal_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_name :
    [< generic_package_renaming_decl]
    -> defining_name

      
  val f_renames :
    [< generic_package_renaming_decl]
    -> [char_literal | dotted_name | identifier | string_literal]


end

module GenericRenamingDecl : sig
  (**
  * Base node for all generic renaming declarations (:rmlink:`8.5.5`).
  *)

  type t =
    [
      | GenericPackageRenamingDecl.t
      | GenericSubpRenamingDecl.t
    ]


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module GenericSubpInstantiation : sig
  (**
  * Instantiations of a generic subprogram .
  *)

  type t =
    [
      | `GenericSubpInstantiation of
          generic_subp_instantiation_fields
    ]

  type fields = generic_subp_instantiation_fields =
    
  {
         
    f_overriding: overriding_node
    Lazy.t;
         
    f_kind: subp_kind
    Lazy.t;
         
    f_subp_name: defining_name
    Lazy.t;
         
    f_generic_subp_name: [
      | `CharLiteral
          of char_literal_fields
      | `DottedName
          of dotted_name_fields
      | `Identifier
          of identifier_fields
      | `StringLiteral
          of string_literal_fields
    ]
    Lazy.t;
         
    f_params: basic_assoc_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int


      
  val p_designated_subp :
    [< generic_subp_instantiation ]
    -> basic_subp_decl option
  (**
  * Return the subprogram decl designated by this instantiation.
  *)


      
  val f_overriding :
    [< generic_subp_instantiation]
    -> overriding_node

      
  val f_kind :
    [< generic_subp_instantiation]
    -> subp_kind

      
  val f_subp_name :
    [< generic_subp_instantiation]
    -> defining_name

      
  val f_generic_subp_name :
    [< generic_subp_instantiation]
    -> [char_literal | dotted_name | identifier | string_literal]

      
  val f_params :
    [< generic_subp_instantiation]
    -> basic_assoc_list


end

module GenericPackageInstantiation : sig
  (**
  * Instantiations of a generic package.
  *)

  type t =
    [
      | `GenericPackageInstantiation of
          generic_package_instantiation_fields
    ]

  type fields = generic_package_instantiation_fields =
    
  {
         
    f_name: defining_name
    Lazy.t;
         
    f_generic_pkg_name: [
      | `CharLiteral
          of char_literal_fields
      | `DottedName
          of dotted_name_fields
      | `Identifier
          of identifier_fields
      | `StringLiteral
          of string_literal_fields
    ]
    Lazy.t;
         
    f_params: basic_assoc_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_name :
    [< generic_package_instantiation]
    -> defining_name

      
  val f_generic_pkg_name :
    [< generic_package_instantiation]
    -> [char_literal | dotted_name | identifier | string_literal]

      
  val f_params :
    [< generic_package_instantiation]
    -> basic_assoc_list


end

module GenericInstantiation : sig
  (**
  * Instantiations of generics (:rmlink:`12.3`).
  *)

  type t =
    [
      | GenericPackageInstantiation.t
      | GenericSubpInstantiation.t
    ]


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int


      
  val p_designated_generic_decl :
    [< generic_instantiation ]
    -> basic_decl option
  (**
  * Return the generic decl entity designated by this instantiation, containing
  * the generic context. This is equivalent to the expanded generic unit in
  * GNAT.
  *)

      
  val p_inst_params :
    [< generic_instantiation ]
    -> ParamActual.t list
  (**
  * Returns an array of pairs, associating formal parameters to actual or
  * default expressions.
  *)



end

module GenericSubpDecl : sig
  (**
  * Generic subprogram declaration (:rmlink:`12.1`).
  *)

  type t =
    [
      | `GenericSubpDecl of
          generic_subp_decl_fields
    ]

  type fields = generic_subp_decl_fields =
    
  {
         
    f_formal_part: generic_formal_part
    Lazy.t;
         
    f_subp_decl: generic_subp_internal
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int


      
  val p_body_part :
    ?imprecise_fallback:
    bool
    -> [< generic_subp_decl ]
    -> base_subp_body option
  (**
  * Return the BaseSubpBody corresponding to this node.
  *)


      
  val f_formal_part :
    [< generic_subp_decl]
    -> generic_formal_part

      
  val f_subp_decl :
    [< generic_subp_decl]
    -> generic_subp_internal


end

module GenericPackageDecl : sig
  (**
  * Generic package declaration (:rmlink:`12.1`).
  *)

  type t =
    [
      | `GenericPackageDecl of
          generic_package_decl_fields
    ]

  type fields = generic_package_decl_fields =
    
  {
         
    f_formal_part: generic_formal_part
    Lazy.t;
         
    f_package_decl: generic_package_internal
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int


      
  val p_body_part :
    [< generic_package_decl ]
    -> package_body option
  (**
  * Return the PackageBody corresponding to this node, or null if there is
  * none.
  *)


      
  val f_formal_part :
    [< generic_package_decl]
    -> generic_formal_part

      
  val f_package_decl :
    [< generic_package_decl]
    -> generic_package_internal


end

module GenericDecl : sig
  (**
  * Base class for generic declarations (:rmlink:`12.1`).
  *)

  type t =
    [
      | GenericPackageDecl.t
      | GenericSubpDecl.t
    ]


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_formal_part :
    [< generic_decl]
    -> generic_formal_part


end

module ForLoopVarDecl : sig
  (**
  * Declaration for the controlling variable in a ``for`` loop (:rmlink:`5.5`).
  *)

  type t =
    [
      | `ForLoopVarDecl of
          for_loop_var_decl_fields
    ]

  type fields = for_loop_var_decl_fields =
    
  {
         
    f_id: defining_name
    Lazy.t;
         
    f_id_type: [
      | `AnonymousType
          of anonymous_type_fields
      | `ConstrainedSubtypeIndication
          of constrained_subtype_indication_fields
      | `DiscreteSubtypeIndication
          of discrete_subtype_indication_fields
      | `SubtypeIndication
          of subtype_indication_fields
    ]
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_id :
    [< for_loop_var_decl]
    -> defining_name

      
  val f_id_type :
    [< for_loop_var_decl]
    -> [anonymous_type | subtype_indication] option


end

module ExceptionHandler : sig
  (**
  * Exception handler (:rmlink:`11.2`).
  *)

  type t =
    [
      | `ExceptionHandler of
          exception_handler_fields
    ]

  type fields = exception_handler_fields =
    
  {
         
    f_exception_name: defining_name
    option
    Lazy.t;
         
    f_handled_exceptions: alternatives_list
    Lazy.t;
         
    f_stmts: stmt_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_exception_name :
    [< exception_handler]
    -> defining_name option

      
  val f_handled_exceptions :
    [< exception_handler]
    -> alternatives_list

      
  val f_stmts :
    [< exception_handler]
    -> stmt_list


end

module ExceptionDecl : sig
  (**
  * Exception declarations (:rmlink:`11.1`).
  *)

  type t =
    [
      | `ExceptionDecl of
          exception_decl_fields
    ]

  type fields = exception_decl_fields =
    
  {
         
    f_ids: defining_name_list
    Lazy.t;
         
    f_renames: renaming_clause
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_ids :
    [< exception_decl]
    -> defining_name_list

      
  val f_renames :
    [< exception_decl]
    -> renaming_clause option


end

module ErrorDecl : sig
  (**
  * Placeholder node for syntax errors in lists of declarations.
  *)

  type t =
    [
      | `ErrorDecl of
          error_decl_fields
    ]

  type fields = error_decl_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module EntryIndexSpec : sig
  (**
  * Index specification for an entry body (:rmlink:`9.5.2`).
  *)

  type t =
    [
      | `EntryIndexSpec of
          entry_index_spec_fields
    ]

  type fields = entry_index_spec_fields =
    
  {
         
    f_id: defining_name
    Lazy.t;
         
    f_subtype: [
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `CallExpr
          of call_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConstrainedSubtypeIndication
          of constrained_subtype_indication_fields
      | `DiscreteSubtypeIndication
          of discrete_subtype_indication_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `QualExpr
          of qual_expr_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `SubtypeIndication
          of subtype_indication_fields
      | `TargetName
          of target_name_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_id :
    [< entry_index_spec]
    -> defining_name

      
  val f_subtype :
    [< entry_index_spec]
    -> [attribute_ref | bin_op | call_expr | char_literal | dotted_name | explicit_deref | identifier | qual_expr | reduce_attribute_ref | string_literal | subtype_indication | target_name | update_attribute_ref]


end

module TaskBody : sig
  (**
  * Task body (:rmlink:`9.1`).
  *)

  type t =
    [
      | `TaskBody of
          task_body_fields
    ]

  type fields = task_body_fields =
    
  {
         
    f_name: defining_name
    Lazy.t;
         
    f_decls: declarative_part
    Lazy.t;
         
    f_stmts: handled_stmts
    Lazy.t;
         
    f_end_name: end_name
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_name :
    [< task_body]
    -> defining_name

      
  val f_decls :
    [< task_body]
    -> declarative_part

      
  val f_stmts :
    [< task_body]
    -> handled_stmts

      
  val f_end_name :
    [< task_body]
    -> end_name option


end

module ProtectedBody : sig
  (**
  * Protected object body (:rmlink:`9.4`).
  *)

  type t =
    [
      | `ProtectedBody of
          protected_body_fields
    ]

  type fields = protected_body_fields =
    
  {
         
    f_name: defining_name
    Lazy.t;
         
    f_decls: declarative_part
    Lazy.t;
         
    f_end_name: end_name
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_name :
    [< protected_body]
    -> defining_name

      
  val f_decls :
    [< protected_body]
    -> declarative_part

      
  val f_end_name :
    [< protected_body]
    -> end_name option


end

module PackageBody : sig
  (**
  * Package body (:rmlink:`7.2`).
  *)

  type t =
    [
      | `PackageBody of
          package_body_fields
    ]

  type fields = package_body_fields =
    
  {
         
    f_package_name: defining_name
    Lazy.t;
         
    f_decls: declarative_part
    Lazy.t;
         
    f_stmts: handled_stmts
    option
    Lazy.t;
         
    f_end_name: end_name
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_package_name :
    [< package_body]
    -> defining_name

      
  val f_decls :
    [< package_body]
    -> declarative_part

      
  val f_stmts :
    [< package_body]
    -> handled_stmts option

      
  val f_end_name :
    [< package_body]
    -> end_name option


end

module EntryBody : sig
  (**
  * Entry body (:rmlink:`9.5.2`).
  *)

  type t =
    [
      | `EntryBody of
          entry_body_fields
    ]

  type fields = entry_body_fields =
    
  {
         
    f_entry_name: defining_name
    Lazy.t;
         
    f_index_spec: entry_index_spec
    option
    Lazy.t;
         
    f_params: entry_completion_formal_params
    Lazy.t;
         
    f_barrier: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
         
    f_decls: declarative_part
    Lazy.t;
         
    f_stmts: handled_stmts
    Lazy.t;
         
    f_end_name: end_name
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_entry_name :
    [< entry_body]
    -> defining_name

      
  val f_index_spec :
    [< entry_body]
    -> entry_index_spec option

      
  val f_params :
    [< entry_body]
    -> entry_completion_formal_params

      
  val f_barrier :
    [< entry_body]
    -> [allocator | attribute_ref | base_aggregate | bin_op | call_expr | char_literal | concat_op | cond_expr | decl_expr | dotted_name | explicit_deref | identifier | membership_expr | null_literal | num_literal | paren_expr | qual_expr | quantified_expr | raise_expr | reduce_attribute_ref | string_literal | target_name | un_op | update_attribute_ref]

      
  val f_decls :
    [< entry_body]
    -> declarative_part

      
  val f_stmts :
    [< entry_body]
    -> handled_stmts

      
  val f_end_name :
    [< entry_body]
    -> end_name option


end

module TaskBodyStub : sig
  (**
  * Stub for a task body (``is separate``) (:rmlink:`10.1.3`).
  *)

  type t =
    [
      | `TaskBodyStub of
          task_body_stub_fields
    ]

  type fields = task_body_stub_fields =
    
  {
         
    f_name: defining_name
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_name :
    [< task_body_stub]
    -> defining_name


end

module SubpBodyStub : sig
  (**
  * Stub for a subprogram body (``is separate``) (:rmlink:`10.1.3`).
  *)

  type t =
    [
      | `SubpBodyStub of
          subp_body_stub_fields
    ]

  type fields = subp_body_stub_fields =
    
  {
         
    f_overriding: overriding_node
    Lazy.t;
         
    f_subp_spec: subp_spec
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_overriding :
    [< subp_body_stub]
    -> overriding_node

      
  val f_subp_spec :
    [< subp_body_stub]
    -> subp_spec


end

module ProtectedBodyStub : sig
  (**
  * Stub for a protected object body (``is separate``) (:rmlink:`10.1.3`).
  *)

  type t =
    [
      | `ProtectedBodyStub of
          protected_body_stub_fields
    ]

  type fields = protected_body_stub_fields =
    
  {
         
    f_name: defining_name
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_name :
    [< protected_body_stub]
    -> defining_name


end

module PackageBodyStub : sig
  (**
  * Stub for a package body (``is separate``) (:rmlink:`10.1.3`).
  *)

  type t =
    [
      | `PackageBodyStub of
          package_body_stub_fields
    ]

  type fields = package_body_stub_fields =
    
  {
         
    f_name: defining_name
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_name :
    [< package_body_stub]
    -> defining_name


end

module BodyStub : sig
  (**
  * Base class for a body stub (:rmlink:`10.1.3`). A body stub is meant to be
  * completed by .
  *)

  type t =
    [
      | PackageBodyStub.t
      | ProtectedBodyStub.t
      | SubpBodyStub.t
      | TaskBodyStub.t
    ]


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int


      
  val p_syntactic_fully_qualified_name :
    [< body_stub ]
    -> string list
  (**
  * Return the syntactic fully qualified name to refer to this body.
  *
  * Note that this can raise a Property_Error when the stub is in an illegal
  * place (too nested, in a declare block, etc.).
  *)



end

module SubpRenamingDecl : sig
  (**
  * Declaration for a subprogram renaming (:rmlink:`8.5.4`).
  *)

  type t =
    [
      | `SubpRenamingDecl of
          subp_renaming_decl_fields
    ]

  type fields = subp_renaming_decl_fields =
    
  {
         
    f_overriding: overriding_node
    Lazy.t;
         
    f_subp_spec: subp_spec
    Lazy.t;
         
    f_renames: renaming_clause
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_overriding :
    [< subp_renaming_decl]
    -> overriding_node

      
  val f_subp_spec :
    [< subp_renaming_decl]
    -> subp_spec

      
  val f_renames :
    [< subp_renaming_decl]
    -> renaming_clause


end

module SubpBody : sig
  (**
  * Subprogram body(:rmlink:`6.3`) .
  *)

  type t =
    [
      | `SubpBody of
          subp_body_fields
    ]

  type fields = subp_body_fields =
    
  {
         
    f_overriding: overriding_node
    Lazy.t;
         
    f_subp_spec: subp_spec
    Lazy.t;
         
    f_decls: declarative_part
    Lazy.t;
         
    f_stmts: handled_stmts
    Lazy.t;
         
    f_end_name: end_name
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_overriding :
    [< subp_body]
    -> overriding_node

      
  val f_subp_spec :
    [< subp_body]
    -> subp_spec

      
  val f_decls :
    [< subp_body]
    -> declarative_part

      
  val f_stmts :
    [< subp_body]
    -> handled_stmts

      
  val f_end_name :
    [< subp_body]
    -> end_name option


end

module NullSubpDecl : sig
  (**
  * Declaration for a null subprogram (:rmlink:`6.1`).
  *)

  type t =
    [
      | `NullSubpDecl of
          null_subp_decl_fields
    ]

  type fields = null_subp_decl_fields =
    
  {
         
    f_overriding: overriding_node
    Lazy.t;
         
    f_subp_spec: subp_spec
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_overriding :
    [< null_subp_decl]
    -> overriding_node

      
  val f_subp_spec :
    [< null_subp_decl]
    -> subp_spec


end

module ExprFunction : sig
  (**
  * Expression function (:rmlink:`6.8`).
  *)

  type t =
    [
      | `ExprFunction of
          expr_function_fields
    ]

  type fields = expr_function_fields =
    
  {
         
    f_overriding: overriding_node
    Lazy.t;
         
    f_subp_spec: subp_spec
    Lazy.t;
         
    f_expr: [
      | `Aggregate
          of aggregate_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_overriding :
    [< expr_function]
    -> overriding_node

      
  val f_subp_spec :
    [< expr_function]
    -> subp_spec

      
  val f_expr :
    [< expr_function]
    -> [base_aggregate | paren_expr]


end

module BaseSubpBody : sig
  (**
  * Base class for subprogram bodies (:rmlink:`6.3`).
  *)

  type t =
    [
      | ExprFunction.t
      | NullSubpDecl.t
      | SubpBody.t
      | SubpRenamingDecl.t
    ]


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_overriding :
    [< base_subp_body]
    -> overriding_node

      
  val f_subp_spec :
    [< base_subp_body]
    -> subp_spec


end

module BodyNode : sig
  (**
  * Base class for an Ada body (:rmlink:`3.11`). A body is the completion of a
  * declaration.
  *)

  type t =
    [
      | BaseSubpBody.t
      | BodyStub.t
      | EntryBody.t
      | PackageBody.t
      | ProtectedBody.t
      | TaskBody.t
    ]


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int


      
  val p_previous_part :
    ?imprecise_fallback:
    bool
    -> [< body_node ]
    -> basic_decl option
  (**
  * Return the previous part for this body. Might be a declaration or a body
  * stub.
  *)

      
  val p_decl_part :
    ?imprecise_fallback:
    bool
    -> [< body_node ]
    -> basic_decl option
  (**
  * Return the decl corresponding to this node if applicable.
  *)

      
  val p_subunit_root :
    [< body_node ]
    -> basic_decl option
  (**
  * If self is a subunit, return the body in which it is rooted.
  *)



end

module SyntheticSubpDecl : sig
  (**
  * Synthetic subprogram declaration.
  *
  * Is used to represent predefined operators. This should also be usable for
  * synthesizing function attributes.
  *)

  type t =
    [
      | `SyntheticSubpDecl of
          synthetic_subp_decl_fields
    ]

  type fields = synthetic_subp_decl_fields =
    
  {
         
    f_spec: [
      | `SyntheticBinarySpec
          of synthetic_binary_spec_fields
      | `SyntheticUnarySpec
          of synthetic_unary_spec_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_spec :
    [< synthetic_subp_decl]
    -> [synthetic_binary_spec | synthetic_unary_spec]


end

module GenericSubpInternal : sig
  (**
  * Internal node for generic subprograms.
  *)

  type t =
    [
      | `GenericSubpInternal of
          generic_subp_internal_fields
    ]

  type fields = generic_subp_internal_fields =
    
  {
         
    f_subp_spec: subp_spec
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_subp_spec :
    [< generic_subp_internal]
    -> subp_spec


end

module SyntheticCharEnumLit : sig
  (**
  * Synthetic character enum literal declaration.
  *)

  type t =
    [
      | `SyntheticCharEnumLit of
          synthetic_char_enum_lit_fields
    ]

  type fields = synthetic_char_enum_lit_fields =
    
  {
         
    f_name: defining_name
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int


      
  val p_expr :
    [< synthetic_char_enum_lit ]
    -> defining_name option
  (**
  * Return the CharLiteral expression corresponding to this enum literal.
  *)


      
  val f_name :
    [< synthetic_char_enum_lit]
    -> defining_name


end

module EnumLiteralDecl : sig
  (**
  * Declaration for an enumeration literal (:rmlink:`3.5.1`).
  *)

  type t =
    [
      | `EnumLiteralDecl of
          enum_literal_decl_fields
      | `SyntheticCharEnumLit of
          synthetic_char_enum_lit_fields
    ]

  type fields = enum_literal_decl_fields =
    
  {
         
    f_name: defining_name
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int


      
  val p_enum_type :
    [< enum_literal_decl ]
    -> type_decl option
  (**
  * Return the enum type corresponding to this enum literal.
  *)


      
  val f_name :
    [< enum_literal_decl]
    -> defining_name


end

module EntryDecl : sig
  (**
  * Entry declaration (:rmlink:`9.4`).
  *)

  type t =
    [
      | `EntryDecl of
          entry_decl_fields
    ]

  type fields = entry_decl_fields =
    
  {
         
    f_overriding: overriding_node
    Lazy.t;
         
    f_spec: entry_spec
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int


      
  val p_body_part :
    ?imprecise_fallback:
    bool
    -> [< entry_decl ]
    -> entry_body option
  (**
  * Return the entry body associated to this entry declaration.
  *)

      
  val p_accept_stmts :
    [< entry_decl ]
    -> accept_stmt list
  (**
  * Return an array of accept statements corresponding to this entry.
  *)


      
  val f_overriding :
    [< entry_decl]
    -> overriding_node

      
  val f_spec :
    [< entry_decl]
    -> entry_spec


end

module SubpDecl : sig
  (**
  * Regular subprogram declaration (:rmlink:`6.1`).
  *)

  type t =
    [
      | `SubpDecl of
          subp_decl_fields
    ]

  type fields = subp_decl_fields =
    
  {
         
    f_overriding: overriding_node
    Lazy.t;
         
    f_subp_spec: subp_spec
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_overriding :
    [< subp_decl]
    -> overriding_node

      
  val f_subp_spec :
    [< subp_decl]
    -> subp_spec


end

module ConcreteFormalSubpDecl : sig
  (**
  * Formal declaration for a concrete subprogram (:rmlink:`12.6`).
  *)

  type t =
    [
      | `ConcreteFormalSubpDecl of
          concrete_formal_subp_decl_fields
    ]

  type fields = concrete_formal_subp_decl_fields =
    
  {
         
    f_overriding: overriding_node
    Lazy.t;
         
    f_subp_spec: subp_spec
    Lazy.t;
         
    f_default_expr: [
      | `AttributeRef
          of attribute_ref_fields
      | `BoxExpr
          of box_expr_fields
      | `CallExpr
          of call_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `NullLiteral
          of null_literal_fields
      | `QualExpr
          of qual_expr_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_overriding :
    [< concrete_formal_subp_decl]
    -> overriding_node

      
  val f_subp_spec :
    [< concrete_formal_subp_decl]
    -> subp_spec

      
  val f_default_expr :
    [< concrete_formal_subp_decl]
    -> [attribute_ref | box_expr | call_expr | char_literal | dotted_name | explicit_deref | identifier | null_literal | qual_expr | reduce_attribute_ref | string_literal | target_name | update_attribute_ref] option


end

module AbstractFormalSubpDecl : sig
  (**
  * Formal declaration for an abstract subprogram (:rmlink:`12.6`).
  *)

  type t =
    [
      | `AbstractFormalSubpDecl of
          abstract_formal_subp_decl_fields
    ]

  type fields = abstract_formal_subp_decl_fields =
    
  {
         
    f_overriding: overriding_node
    Lazy.t;
         
    f_subp_spec: subp_spec
    Lazy.t;
         
    f_default_expr: [
      | `AttributeRef
          of attribute_ref_fields
      | `BoxExpr
          of box_expr_fields
      | `CallExpr
          of call_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `NullLiteral
          of null_literal_fields
      | `QualExpr
          of qual_expr_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_overriding :
    [< abstract_formal_subp_decl]
    -> overriding_node

      
  val f_subp_spec :
    [< abstract_formal_subp_decl]
    -> subp_spec

      
  val f_default_expr :
    [< abstract_formal_subp_decl]
    -> [attribute_ref | box_expr | call_expr | char_literal | dotted_name | explicit_deref | identifier | null_literal | qual_expr | reduce_attribute_ref | string_literal | target_name | update_attribute_ref] option


end

module FormalSubpDecl : sig
  (**
  * Formal subprogram declarations, in generic declarations formal parts
  * (:rmlink:`12.6`).
  *)

  type t =
    [
      | AbstractFormalSubpDecl.t
      | ConcreteFormalSubpDecl.t
    ]


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_overriding :
    [< formal_subp_decl]
    -> overriding_node

      
  val f_subp_spec :
    [< formal_subp_decl]
    -> subp_spec

      
  val f_default_expr :
    [< formal_subp_decl]
    -> [attribute_ref | box_expr | call_expr | char_literal | dotted_name | explicit_deref | identifier | null_literal | qual_expr | reduce_attribute_ref | string_literal | target_name | update_attribute_ref] option


end

module AbstractSubpDecl : sig
  (**
  * Declaration for an abstract subprogram (:rmlink:`3.9.3`).
  *)

  type t =
    [
      | `AbstractSubpDecl of
          abstract_subp_decl_fields
    ]

  type fields = abstract_subp_decl_fields =
    
  {
         
    f_overriding: overriding_node
    Lazy.t;
         
    f_subp_spec: subp_spec
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_overriding :
    [< abstract_subp_decl]
    -> overriding_node

      
  val f_subp_spec :
    [< abstract_subp_decl]
    -> subp_spec


end

module ClassicSubpDecl : sig
  (**
  * This is an intermediate abstract class for subprogram declarations with a
  * common structure: overriding indicator, ``SubpSpec``, aspects, <other
  * fields>.
  *)

  type t =
    [
      | AbstractSubpDecl.t
      | FormalSubpDecl.t
      | SubpDecl.t
    ]


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int


      
  val p_body_part :
    ?imprecise_fallback:
    bool
    -> [< classic_subp_decl ]
    -> base_subp_body option
  (**
  * Return the BaseSubpBody corresponding to this node.
  *)


      
  val f_overriding :
    [< classic_subp_decl]
    -> overriding_node

      
  val f_subp_spec :
    [< classic_subp_decl]
    -> subp_spec


end

module BasicSubpDecl : sig
  (**
  * Base class for subprogram declarations.
  *)

  type t =
    [
      | ClassicSubpDecl.t
      | EntryDecl.t
      | EnumLiteralDecl.t
      | GenericSubpInternal.t
      | SyntheticSubpDecl.t
    ]


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int


      
  val p_subp_decl_spec :
    [< basic_subp_decl ]
    -> base_subp_spec option
  (**
  * Return the specification for this subprogram
  *)



end

module FormalTypeDecl : sig
  (**
  * A formal type declaration.
  *)

  type t =
    [
      | `FormalTypeDecl of
          formal_type_decl_fields
    ]

  type fields = formal_type_decl_fields =
    
  {
         
    f_name: defining_name
    option
    Lazy.t;
         
    f_discriminants: discriminant_part
    option
    Lazy.t;
         
    f_type_def: type_def
    Lazy.t;
         
    f_default_type: [
      | `AttributeRef
          of attribute_ref_fields
      | `CallExpr
          of call_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `QualExpr
          of qual_expr_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_name :
    [< formal_type_decl]
    -> defining_name option

      
  val f_discriminants :
    [< formal_type_decl]
    -> discriminant_part option

      
  val f_type_def :
    [< formal_type_decl]
    -> type_def

      
  val f_default_type :
    [< formal_type_decl]
    -> [attribute_ref | call_expr | char_literal | dotted_name | explicit_deref | identifier | qual_expr | reduce_attribute_ref | string_literal | target_name | update_attribute_ref] option


end

module ConcreteTypeDecl : sig
  (**
  * A concrete type declaration.
  *)

  type t =
    [
      | `ConcreteTypeDecl of
          concrete_type_decl_fields
    ]

  type fields = concrete_type_decl_fields =
    
  {
         
    f_name: defining_name
    option
    Lazy.t;
         
    f_discriminants: discriminant_part
    option
    Lazy.t;
         
    f_type_def: type_def
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_name :
    [< concrete_type_decl]
    -> defining_name option

      
  val f_discriminants :
    [< concrete_type_decl]
    -> discriminant_part option

      
  val f_type_def :
    [< concrete_type_decl]
    -> type_def


end

module SynthAnonymousTypeDecl : sig
  (**
  * Synthetic anonymous type decl. Used to generate anonymous access types.
  *)

  type t =
    [
      | `SynthAnonymousTypeDecl of
          synth_anonymous_type_decl_fields
    ]

  type fields = synth_anonymous_type_decl_fields =
    
  {
         
    f_name: defining_name
    option
    Lazy.t;
         
    f_discriminants: discriminant_part
    option
    Lazy.t;
         
    f_type_def: type_def
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_name :
    [< synth_anonymous_type_decl]
    -> defining_name option

      
  val f_discriminants :
    [< synth_anonymous_type_decl]
    -> discriminant_part option

      
  val f_type_def :
    [< synth_anonymous_type_decl]
    -> type_def


end

module AnonymousTypeDecl : sig
  (**
  * Anonymous type declaration (for anonymous array or access types). This
  * class has no RM existence, and anonymous (sub)types are refered to
  * implicitly in the RM.
  *)

  type t =
    [
      | `AnonymousTypeDecl of
          anonymous_type_decl_fields
      | `SynthAnonymousTypeDecl of
          synth_anonymous_type_decl_fields
    ]

  type fields = anonymous_type_decl_fields =
    
  {
         
    f_name: defining_name
    option
    Lazy.t;
         
    f_discriminants: discriminant_part
    option
    Lazy.t;
         
    f_type_def: type_def
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_name :
    [< anonymous_type_decl]
    -> defining_name option

      
  val f_discriminants :
    [< anonymous_type_decl]
    -> discriminant_part option

      
  val f_type_def :
    [< anonymous_type_decl]
    -> type_def


end

module TypeDecl : sig
  (**
  * Type declarations that embed a type definition node. Corresponds to the
  * ARM's full type declarations (:rmlink:`3.2.1`).
  *)

  type t =
    [
      | AnonymousTypeDecl.t
      | ConcreteTypeDecl.t
      | FormalTypeDecl.t
    ]


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int


      
  val p_get_primitives :
    ?only_inherited:
    bool
    -> ?include_predefined_operators:
    bool
    -> [< type_decl ]
    -> basic_decl list
  (**
  * Return the list of all primitive operations that are available on this
  * type. If ``only_inherited`` is True, it will only return the primitives
  * that are implicitly inherited by this type, discarding those explicitly
  * defined on this type. Predefined operators are included in the result iff
  * ``include_predefined_operators`` is True. It defaults to False.
  *)


      
  val f_name :
    [< type_decl]
    -> defining_name option

      
  val f_discriminants :
    [< type_decl]
    -> discriminant_part option

      
  val f_type_def :
    [< type_decl]
    -> type_def


end

module SingleTaskTypeDecl : sig
  (**
  * Type declaration for a single task (:rmlink:`9.1`).
  *)

  type t =
    [
      | `SingleTaskTypeDecl of
          single_task_type_decl_fields
    ]

  type fields = single_task_type_decl_fields =
    
  {
         
    f_name: defining_name
    option
    Lazy.t;
         
    f_discriminants: discriminant_part
    option
    Lazy.t;
         
    f_definition: task_def
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_name :
    [< single_task_type_decl]
    -> defining_name option

      
  val f_discriminants :
    [< single_task_type_decl]
    -> discriminant_part option

      
  val f_definition :
    [< single_task_type_decl]
    -> task_def option


end

module TaskTypeDecl : sig
  (**
  * Declaration for a task type (:rmlink:`9.1`).
  *)

  type t =
    [
      | `TaskTypeDecl of
          task_type_decl_fields
      | `SingleTaskTypeDecl of
          single_task_type_decl_fields
    ]

  type fields = task_type_decl_fields =
    
  {
         
    f_name: defining_name
    option
    Lazy.t;
         
    f_discriminants: discriminant_part
    option
    Lazy.t;
         
    f_definition: task_def
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_name :
    [< task_type_decl]
    -> defining_name option

      
  val f_discriminants :
    [< task_type_decl]
    -> discriminant_part option

      
  val f_definition :
    [< task_type_decl]
    -> task_def option


end

module ProtectedTypeDecl : sig
  (**
  * Declaration for a protected type (:rmlink:`9.4`).
  *)

  type t =
    [
      | `ProtectedTypeDecl of
          protected_type_decl_fields
    ]

  type fields = protected_type_decl_fields =
    
  {
         
    f_name: defining_name
    option
    Lazy.t;
         
    f_discriminants: discriminant_part
    option
    Lazy.t;
         
    f_interfaces: parent_list
    Lazy.t;
         
    f_definition: protected_def
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_name :
    [< protected_type_decl]
    -> defining_name option

      
  val f_discriminants :
    [< protected_type_decl]
    -> discriminant_part option

      
  val f_interfaces :
    [< protected_type_decl]
    -> parent_list

      
  val f_definition :
    [< protected_type_decl]
    -> protected_def


end

module IncompleteTaggedTypeDecl : sig
  (**
  * Incomplete declaration for a tagged type.
  *)

  type t =
    [
      | `IncompleteTaggedTypeDecl of
          incomplete_tagged_type_decl_fields
    ]

  type fields = incomplete_tagged_type_decl_fields =
    
  {
         
    f_name: defining_name
    option
    Lazy.t;
         
    f_discriminants: discriminant_part
    option
    Lazy.t;
         
    f_has_abstract: abstract_node
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_name :
    [< incomplete_tagged_type_decl]
    -> defining_name option

      
  val f_discriminants :
    [< incomplete_tagged_type_decl]
    -> discriminant_part option

      
  val f_has_abstract :
    [< incomplete_tagged_type_decl]
    -> abstract_node


end

module IncompleteFormalTypeDecl : sig
  (**
  * A formal incomplete type declaration.
  *)

  type t =
    [
      | `IncompleteFormalTypeDecl of
          incomplete_formal_type_decl_fields
    ]

  type fields = incomplete_formal_type_decl_fields =
    
  {
         
    f_name: defining_name
    option
    Lazy.t;
         
    f_discriminants: discriminant_part
    option
    Lazy.t;
         
    f_is_tagged: tagged_node
    option
    Lazy.t;
         
    f_default_type: [
      | `AttributeRef
          of attribute_ref_fields
      | `CallExpr
          of call_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `QualExpr
          of qual_expr_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_name :
    [< incomplete_formal_type_decl]
    -> defining_name option

      
  val f_discriminants :
    [< incomplete_formal_type_decl]
    -> discriminant_part option

      
  val f_is_tagged :
    [< incomplete_formal_type_decl]
    -> tagged_node option

      
  val f_default_type :
    [< incomplete_formal_type_decl]
    -> [attribute_ref | call_expr | char_literal | dotted_name | explicit_deref | identifier | qual_expr | reduce_attribute_ref | string_literal | target_name | update_attribute_ref] option


end

module IncompleteTypeDecl : sig
  (**
  * Incomplete declaration for a type (:rmlink:`12.5`).
  *)

  type t =
    [
      | `IncompleteTypeDecl of
          incomplete_type_decl_fields
      | `IncompleteFormalTypeDecl of
          incomplete_formal_type_decl_fields
      | `IncompleteTaggedTypeDecl of
          incomplete_tagged_type_decl_fields
    ]

  type fields = incomplete_type_decl_fields =
    
  {
         
    f_name: defining_name
    option
    Lazy.t;
         
    f_discriminants: discriminant_part
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_name :
    [< incomplete_type_decl]
    -> defining_name option

      
  val f_discriminants :
    [< incomplete_type_decl]
    -> discriminant_part option


end

module ClasswideTypeDecl : sig
  (**
  * Synthetic node (not parsed, generated from a property call). Refers to the
  * classwide type for a given tagged type (:rmlink:`3.4.1`).
  *)

  type t =
    [
      | `ClasswideTypeDecl of
          classwide_type_decl_fields
    ]

  type fields = classwide_type_decl_fields =
    
  {
         
    f_name: defining_name
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_name :
    [< classwide_type_decl]
    -> defining_name option


end

module SubtypeDecl : sig
  (**
  * Subtype declaration (:rmlink:`3.2.2`).
  *)

  type t =
    [
      | `SubtypeDecl of
          subtype_decl_fields
    ]

  type fields = subtype_decl_fields =
    
  {
         
    f_name: defining_name
    option
    Lazy.t;
         
    f_subtype: subtype_indication
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_name :
    [< subtype_decl]
    -> defining_name option

      
  val f_subtype :
    [< subtype_decl]
    -> subtype_indication


end

module DiscreteBaseSubtypeDecl : sig
  (**
  * Specific ``BaseSubtypeDecl`` synthetic subclass for the base type of scalar
  * types.
  *)

  type t =
    [
      | `DiscreteBaseSubtypeDecl of
          discrete_base_subtype_decl_fields
    ]

  type fields = discrete_base_subtype_decl_fields =
    
  {
         
    f_name: defining_name
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_name :
    [< discrete_base_subtype_decl]
    -> defining_name option


end

module BaseSubtypeDecl : sig
  (**
  * Base class for subtype declarations (:rmlink:`3.2.2`).
  *)

  type t =
    [
      | DiscreteBaseSubtypeDecl.t
      | SubtypeDecl.t
    ]


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int


      
  val p_get_type :
    ?origin:
    ada_node
    -> [< base_subtype_decl ]
    -> base_type_decl option
  (**
  * Get the type for this subtype.
  *)


      
  val f_name :
    [< base_subtype_decl]
    -> defining_name option


end

module BaseTypeDecl : sig
  (**
  * Base class for type declarations. It unifies every kind of type that exists
  * in Ada, including types that have no source existence like classwide types.
  *)

  type t =
    [
      | BaseSubtypeDecl.t
      | ClasswideTypeDecl.t
      | IncompleteTypeDecl.t
      | ProtectedTypeDecl.t
      | TaskTypeDecl.t
      | TypeDecl.t
    ]


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int


      
  val p_base_subtype :
    ?origin:
    ada_node
    -> [< base_type_decl ]
    -> base_type_decl option
  (**
  * If this type decl is a subtype decl, return the base subtype. If not,
  * return ``Self``.
  *)

      
  val p_private_completion :
    [< base_type_decl ]
    -> base_type_decl option
  (**
  * Return the private completion for this type, if there is one.
  *)

      
  val p_is_inherited_primitive :
    [< base_type_decl ]
    -> basic_decl
    -> bool
  (**
  * Assuming that P is a primitive of Self, return whether the given primitive
  * P is inherited from one of Self's parents.
  *)

      
  val p_get_record_representation_clause :
    ?imprecise_fallback:
    bool
    -> [< base_type_decl ]
    -> record_rep_clause option
  (**
  * Return the record representation clause associated to this type decl, if
  * applicable (i.e. this type decl defines a record type).
  *)

      
  val p_get_enum_representation_clause :
    ?imprecise_fallback:
    bool
    -> [< base_type_decl ]
    -> enum_rep_clause option
  (**
  * Return the enum representation clause associated to this type decl, if
  * applicable (i.e. this type decl defines an enum type).
  *)

      
  val p_is_record_type :
    ?origin:
    ada_node
    -> [< base_type_decl ]
    -> bool
  (**
  * Return whether this type is a record type.
  *
  * .. attention:: Private tagged types extending public tagged records are not
  *    considered as record types.
  *)

      
  val p_is_array_type :
    ?origin:
    ada_node
    -> [< base_type_decl ]
    -> bool
  (**
  * Return whether this type is an array type.
  *)

      
  val p_find_derived_types :
    ?imprecise_fallback:
    bool
    -> [< base_type_decl ]
    -> ada_node
    -> ada_node
    -> type_decl list
  (**
  * Find types derived from self in the given ``root`` and its children.
  *)

      
  val p_is_real_type :
    ?origin:
    ada_node
    -> [< base_type_decl ]
    -> bool
  (**
  * Whether type is a real type or not.
  *)

      
  val p_is_float_type :
    ?origin:
    ada_node
    -> [< base_type_decl ]
    -> bool
  (**
  * Whether type is a float type or not.
  *)

      
  val p_is_fixed_point :
    ?origin:
    ada_node
    -> [< base_type_decl ]
    -> bool
  (**
  * Whether type is a fixed point type or not.
  *)

      
  val p_is_enum_type :
    ?origin:
    ada_node
    -> [< base_type_decl ]
    -> bool
  (**
  * Whether type is an enum type
  *)

      
  val p_is_access_type :
    ?origin:
    ada_node
    -> [< base_type_decl ]
    -> bool
  (**
  * Whether Self is an access type or not
  *)

      
  val p_is_char_type :
    ?origin:
    ada_node
    -> [< base_type_decl ]
    -> bool
  (**
  * Whether type is a character type or not
  *)

      
  val p_discrete_range :
    [< base_type_decl ]
    -> DiscreteRange.t
  (**
  * Return the discrete range for this type decl, if applicable.
  *)

      
  val p_is_discrete_type :
    ?origin:
    ada_node
    -> [< base_type_decl ]
    -> bool
  (**
  * Whether type is a discrete type or not.
  *)

      
  val p_is_int_type :
    ?origin:
    ada_node
    -> [< base_type_decl ]
    -> bool
  (**
  * Whether type is an integer type or not.
  *)

      
  val p_accessed_type :
    ?origin:
    ada_node
    -> [< base_type_decl ]
    -> base_type_decl option
  (**
  * If this type is an access type, or a type with an Implicit_Dereference
  * aspect, return the type of a dereference of an instance of this type.
  *)

      
  val p_is_tagged_type :
    ?origin:
    ada_node
    -> [< base_type_decl ]
    -> bool
  (**
  * Whether type is tagged or not
  *)

      
  val p_base_type :
    ?origin:
    ada_node
    -> [< base_type_decl ]
    -> base_type_decl option
  (**
  * Return the base type entity for this derived type declaration
  *)

      
  val p_base_types :
    ?origin:
    ada_node
    -> [< base_type_decl ]
    -> base_type_decl list
  (**
  * Return the list of base types for Self.
  *)

      
  val p_find_all_derived_types :
    ?imprecise_fallback:
    bool
    -> [< base_type_decl ]
    -> analysis_unit list
    -> type_decl list
  (**
  * Return the list of all types that inherit (directly or inderictly) from
  * Self among the given units.
  *)

      
  val p_comp_type :
    ?is_subscript:
    bool
    -> ?origin:
    ada_node
    -> [< base_type_decl ]
    -> base_type_decl option
  (**
  * Return the component type of ``Self``, if applicable. The component type is
  * the type you'll get if you call a value whose type is ``Self``. So it can
  * either be:
  *
  * 1. The component type for an array.
  *
  * 2. The return type for an access to function.
  *)

      
  val p_index_type :
    ?origin:
    ada_node
    -> [< base_type_decl ]
    -> int
    -> base_type_decl option
  (**
  * Return the index type for dimension ``dim`` for this type, if applicable.
  *
  * .. warning:: ``dim`` is 0-based, so the first ``index_type`` is at index 0.
  *)

      
  val p_is_derived_type :
    ?origin:
    ada_node
    -> [< base_type_decl ]
    -> base_type_decl
    -> bool
  (**
  * Whether Self is derived from other_type.
  *)

      
  val p_is_interface_type :
    ?origin:
    ada_node
    -> [< base_type_decl ]
    -> bool
  (**
  * Return True iff this type declaration is an interface definition.
  *)

      
  val p_matching_type :
    ?origin:
    ada_node
    -> [< base_type_decl ]
    -> base_type_decl
    -> bool
  (**
  * Return whether ``self`` matches ``expected_type``.
  *)

      
  val p_canonical_type :
    ?origin:
    ada_node
    -> [< base_type_decl ]
    -> base_type_decl option
  (**
  * Return the canonical type declaration for this type declaration. For
  * subtypes, it will return the base type declaration.
  *)

      
  val p_previous_part :
    ?go_to_incomplete:
    bool
    -> [< base_type_decl ]
    -> base_type_decl option
  (**
  * Returns the previous part for this type decl.
  *)

      
  val p_next_part :
    [< base_type_decl ]
    -> base_type_decl option
  (**
  * Returns the next part for this type decl.
  *
  * .. note:: Since this property returns a ``BaseTypeDecl``, it cannot be used
  *    to retrieve the next part of ``TaskTypeDecl`` and ``ProtectedTypeDecl``
  *    nodes as their next part is actually a ``Body``. Use
  *    ``BasicDecl.next_part_for_decl`` for those instead.
  *)

      
  val p_full_view :
    [< base_type_decl ]
    -> base_type_decl option
  (**
  * Return the full completion of this type.
  *)

      
  val p_is_definite_subtype :
    ?origin:
    ada_node
    -> [< base_type_decl ]
    -> bool
  (**
  * Returns whether this is a definite subtype.
  *
  * For convenience, this will return ``False`` for incomplete types, even
  * though the correct answer is more akin to "non applicable".
  *)

      
  val p_is_private :
    [< base_type_decl ]
    -> bool
  (**
  * Whether node is a private view of corresponding type.
  *)

      
  val p_discriminants_list :
    ?origin:
    ada_node
    -> [< base_type_decl ]
    -> base_formal_param_decl list
  (**
  * Return the list of all discriminants of this type. If this type has no
  * discriminant or only unknown discriminants, an empty list is returned.
  *)

      
  val p_root_type :
    ?origin:
    ada_node
    -> [< base_type_decl ]
    -> base_type_decl option
  (**
  * Return the type that is at the root of the derivation hierarchy (ignoring
  * secondary interfaces derivations for tagged types)
  *)

      
  val p_shapes :
    ?include_discriminants:
    bool
    -> ?origin:
    ada_node
    -> [< base_type_decl ]
    -> Shape.t list
  (**
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
  * .. attention:: This property is inaccurate when called on a record
  *    extension which defines components under a certain condition C, and this
  *    same condition is used to define some components in the parent record:
  *    in that case, any feasible shape will in practice contain either both
  *    the components defined under condition C in the child record and the
  *    parent record, or none of them.However, due to the simplified algorithm
  *    we use here to compute the feasible shapes, we will also return shapes
  *    that include the components of the child record but not the parent
  *    record, and conversely.
  *)


      
  val f_name :
    [< base_type_decl]
    -> defining_name option


end

module PackageDecl : sig
  (**
  * Non-generic package declarations (:rmlink:`7.1`).
  *)

  type t =
    [
      | `PackageDecl of
          package_decl_fields
    ]

  type fields = package_decl_fields =
    
  {
         
    f_package_name: defining_name
    Lazy.t;
         
    f_public_part: public_part
    Lazy.t;
         
    f_private_part: private_part
    option
    Lazy.t;
         
    f_end_name: end_name
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_package_name :
    [< package_decl]
    -> defining_name

      
  val f_public_part :
    [< package_decl]
    -> public_part

      
  val f_private_part :
    [< package_decl]
    -> private_part option

      
  val f_end_name :
    [< package_decl]
    -> end_name option


end

module GenericPackageInternal : sig
  (**
  * This class denotes the internal package contained by a GenericPackageDecl.
  *)

  type t =
    [
      | `GenericPackageInternal of
          generic_package_internal_fields
    ]

  type fields = generic_package_internal_fields =
    
  {
         
    f_package_name: defining_name
    Lazy.t;
         
    f_public_part: public_part
    Lazy.t;
         
    f_private_part: private_part
    option
    Lazy.t;
         
    f_end_name: end_name
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_package_name :
    [< generic_package_internal]
    -> defining_name

      
  val f_public_part :
    [< generic_package_internal]
    -> public_part

      
  val f_private_part :
    [< generic_package_internal]
    -> private_part option

      
  val f_end_name :
    [< generic_package_internal]
    -> end_name option


end

module BasePackageDecl : sig
  (**
  * Base class for package declarations. This will be used both for non-generic
  * package declarations (via ``package_decl``) and for generic ones (via
  * ``generic_package_internal``).
  *)

  type t =
    [
      | GenericPackageInternal.t
      | PackageDecl.t
    ]


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int


      
  val p_body_part :
    [< base_package_decl ]
    -> package_body option
  (**
  * Return the PackageBody corresponding to this node.
  *)


      
  val f_package_name :
    [< base_package_decl]
    -> defining_name

      
  val f_public_part :
    [< base_package_decl]
    -> public_part

      
  val f_private_part :
    [< base_package_decl]
    -> private_part option

      
  val f_end_name :
    [< base_package_decl]
    -> end_name option


end

module SyntheticFormalParamDecl : sig
  (**
  * Synthetic parameter declaration.
  *)

  type t =
    [
      | `SyntheticFormalParamDecl of
          synthetic_formal_param_decl_fields
    ]

  type fields = synthetic_formal_param_decl_fields =
    
  {
         
    f_param_type: type_expr
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_param_type :
    [< synthetic_formal_param_decl]
    -> type_expr


end

module ParamSpec : sig
  (**
  * Specification for a parameter (:rmlink:`6.1`).
  *)

  type t =
    [
      | `ParamSpec of
          param_spec_fields
    ]

  type fields = param_spec_fields =
    
  {
         
    f_ids: defining_name_list
    Lazy.t;
         
    f_has_aliased: aliased_node
    Lazy.t;
         
    f_mode: mode
    option
    Lazy.t;
         
    f_type_expr: [
      | `AnonymousType
          of anonymous_type_fields
      | `ConstrainedSubtypeIndication
          of constrained_subtype_indication_fields
      | `DiscreteSubtypeIndication
          of discrete_subtype_indication_fields
      | `SubtypeIndication
          of subtype_indication_fields
    ]
    Lazy.t;
         
    f_default_expr: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_ids :
    [< param_spec]
    -> defining_name_list

      
  val f_has_aliased :
    [< param_spec]
    -> aliased_node

      
  val f_mode :
    [< param_spec]
    -> mode option

      
  val f_type_expr :
    [< param_spec]
    -> [anonymous_type | subtype_indication]

      
  val f_default_expr :
    [< param_spec]
    -> [allocator | attribute_ref | base_aggregate | bin_op | call_expr | char_literal | concat_op | cond_expr | decl_expr | dotted_name | explicit_deref | identifier | membership_expr | null_literal | num_literal | paren_expr | qual_expr | quantified_expr | raise_expr | reduce_attribute_ref | string_literal | target_name | un_op | update_attribute_ref] option


end

module GenericFormalTypeDecl : sig
  (**
  * Formal declaration for a type (:rmlink:`12.1`).
  *)

  type t =
    [
      | `GenericFormalTypeDecl of
          generic_formal_type_decl_fields
    ]

  type fields = generic_formal_type_decl_fields =
    
  {
         
    f_decl: [
      | `AbstractFormalSubpDecl
          of abstract_formal_subp_decl_fields
      | `ConcreteFormalSubpDecl
          of concrete_formal_subp_decl_fields
      | `ExtendedReturnStmtObjectDecl
          of extended_return_stmt_object_decl_fields
      | `FormalTypeDecl
          of formal_type_decl_fields
      | `GenericPackageInstantiation
          of generic_package_instantiation_fields
      | `GenericSubpInstantiation
          of generic_subp_instantiation_fields
      | `IncompleteFormalTypeDecl
          of incomplete_formal_type_decl_fields
      | `NoTypeObjectRenamingDecl
          of no_type_object_renaming_decl_fields
      | `NumberDecl
          of number_decl_fields
      | `ObjectDecl
          of object_decl_fields
      | `SingleProtectedDecl
          of single_protected_decl_fields
      | `SingleTaskDecl
          of single_task_decl_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_decl :
    [< generic_formal_type_decl]
    -> [formal_subp_decl | formal_type_decl | generic_instantiation | incomplete_formal_type_decl | number_decl | object_decl | single_protected_decl | single_task_decl]


end

module GenericFormalSubpDecl : sig
  (**
  * Formal declaration for a subprogram (:rmlink:`12.1`).
  *)

  type t =
    [
      | `GenericFormalSubpDecl of
          generic_formal_subp_decl_fields
    ]

  type fields = generic_formal_subp_decl_fields =
    
  {
         
    f_decl: [
      | `AbstractFormalSubpDecl
          of abstract_formal_subp_decl_fields
      | `ConcreteFormalSubpDecl
          of concrete_formal_subp_decl_fields
      | `ExtendedReturnStmtObjectDecl
          of extended_return_stmt_object_decl_fields
      | `FormalTypeDecl
          of formal_type_decl_fields
      | `GenericPackageInstantiation
          of generic_package_instantiation_fields
      | `GenericSubpInstantiation
          of generic_subp_instantiation_fields
      | `IncompleteFormalTypeDecl
          of incomplete_formal_type_decl_fields
      | `NoTypeObjectRenamingDecl
          of no_type_object_renaming_decl_fields
      | `NumberDecl
          of number_decl_fields
      | `ObjectDecl
          of object_decl_fields
      | `SingleProtectedDecl
          of single_protected_decl_fields
      | `SingleTaskDecl
          of single_task_decl_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_decl :
    [< generic_formal_subp_decl]
    -> [formal_subp_decl | formal_type_decl | generic_instantiation | incomplete_formal_type_decl | number_decl | object_decl | single_protected_decl | single_task_decl]


end

module GenericFormalPackage : sig
  (**
  * Formal declaration for a package (:rmlink:`12.1`).
  *)

  type t =
    [
      | `GenericFormalPackage of
          generic_formal_package_fields
    ]

  type fields = generic_formal_package_fields =
    
  {
         
    f_decl: [
      | `AbstractFormalSubpDecl
          of abstract_formal_subp_decl_fields
      | `ConcreteFormalSubpDecl
          of concrete_formal_subp_decl_fields
      | `ExtendedReturnStmtObjectDecl
          of extended_return_stmt_object_decl_fields
      | `FormalTypeDecl
          of formal_type_decl_fields
      | `GenericPackageInstantiation
          of generic_package_instantiation_fields
      | `GenericSubpInstantiation
          of generic_subp_instantiation_fields
      | `IncompleteFormalTypeDecl
          of incomplete_formal_type_decl_fields
      | `NoTypeObjectRenamingDecl
          of no_type_object_renaming_decl_fields
      | `NumberDecl
          of number_decl_fields
      | `ObjectDecl
          of object_decl_fields
      | `SingleProtectedDecl
          of single_protected_decl_fields
      | `SingleTaskDecl
          of single_task_decl_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_decl :
    [< generic_formal_package]
    -> [formal_subp_decl | formal_type_decl | generic_instantiation | incomplete_formal_type_decl | number_decl | object_decl | single_protected_decl | single_task_decl]


end

module GenericFormalObjDecl : sig
  (**
  * Formal declaration for an object.
  *)

  type t =
    [
      | `GenericFormalObjDecl of
          generic_formal_obj_decl_fields
    ]

  type fields = generic_formal_obj_decl_fields =
    
  {
         
    f_decl: [
      | `AbstractFormalSubpDecl
          of abstract_formal_subp_decl_fields
      | `ConcreteFormalSubpDecl
          of concrete_formal_subp_decl_fields
      | `ExtendedReturnStmtObjectDecl
          of extended_return_stmt_object_decl_fields
      | `FormalTypeDecl
          of formal_type_decl_fields
      | `GenericPackageInstantiation
          of generic_package_instantiation_fields
      | `GenericSubpInstantiation
          of generic_subp_instantiation_fields
      | `IncompleteFormalTypeDecl
          of incomplete_formal_type_decl_fields
      | `NoTypeObjectRenamingDecl
          of no_type_object_renaming_decl_fields
      | `NumberDecl
          of number_decl_fields
      | `ObjectDecl
          of object_decl_fields
      | `SingleProtectedDecl
          of single_protected_decl_fields
      | `SingleTaskDecl
          of single_task_decl_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_decl :
    [< generic_formal_obj_decl]
    -> [formal_subp_decl | formal_type_decl | generic_instantiation | incomplete_formal_type_decl | number_decl | object_decl | single_protected_decl | single_task_decl]


end

module GenericFormal : sig
  (**
  * Enclosing declaration for a generic formal. The real declaration is
  * accessible via the ``decl`` field.
  *)

  type t =
    [
      | GenericFormalObjDecl.t
      | GenericFormalPackage.t
      | GenericFormalSubpDecl.t
      | GenericFormalTypeDecl.t
    ]


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_decl :
    [< generic_formal]
    -> [formal_subp_decl | formal_type_decl | generic_instantiation | incomplete_formal_type_decl | number_decl | object_decl | single_protected_decl | single_task_decl]


end

module DiscriminantSpec : sig
  (**
  * Known list of discriminants in type declarations (:rmlink:`3.7`).
  *)

  type t =
    [
      | `DiscriminantSpec of
          discriminant_spec_fields
    ]

  type fields = discriminant_spec_fields =
    
  {
         
    f_ids: defining_name_list
    Lazy.t;
         
    f_type_expr: [
      | `AnonymousType
          of anonymous_type_fields
      | `ConstrainedSubtypeIndication
          of constrained_subtype_indication_fields
      | `DiscreteSubtypeIndication
          of discrete_subtype_indication_fields
      | `SubtypeIndication
          of subtype_indication_fields
    ]
    Lazy.t;
         
    f_default_expr: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_ids :
    [< discriminant_spec]
    -> defining_name_list

      
  val f_type_expr :
    [< discriminant_spec]
    -> [anonymous_type | subtype_indication]

      
  val f_default_expr :
    [< discriminant_spec]
    -> [allocator | attribute_ref | base_aggregate | bin_op | call_expr | char_literal | concat_op | cond_expr | decl_expr | dotted_name | explicit_deref | identifier | membership_expr | null_literal | num_literal | paren_expr | qual_expr | quantified_expr | raise_expr | reduce_attribute_ref | string_literal | target_name | un_op | update_attribute_ref] option


end

module ComponentDecl : sig
  (**
  * Declaration for a component (:rmlink:`3.8`).
  *)

  type t =
    [
      | `ComponentDecl of
          component_decl_fields
    ]

  type fields = component_decl_fields =
    
  {
         
    f_ids: defining_name_list
    Lazy.t;
         
    f_component_def: component_def
    Lazy.t;
         
    f_default_expr: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_ids :
    [< component_decl]
    -> defining_name_list

      
  val f_component_def :
    [< component_decl]
    -> component_def

      
  val f_default_expr :
    [< component_decl]
    -> [allocator | attribute_ref | base_aggregate | bin_op | call_expr | char_literal | concat_op | cond_expr | decl_expr | dotted_name | explicit_deref | identifier | membership_expr | null_literal | num_literal | paren_expr | qual_expr | quantified_expr | raise_expr | reduce_attribute_ref | string_literal | target_name | un_op | update_attribute_ref] option


end

module BaseFormalParamDecl : sig
  (**
  * Base class for formal parameter declarations. This is used both for records
  * components and for subprogram parameters.
  *
  * This is a Libadalang abstraction, that has no ARM existence.
  *)

  type t =
    [
      | ComponentDecl.t
      | DiscriminantSpec.t
      | GenericFormal.t
      | ParamSpec.t
      | SyntheticFormalParamDecl.t
    ]


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int


      
  val p_formal_type :
    ?origin:
    ada_node
    -> [< base_formal_param_decl ]
    -> base_type_decl option
  (**
  * Return the type for this formal.
  *)



end

module AnonymousExprDecl : sig
  (**
  * Represents a anonymous declaration that holds an expression.
  *
  * This is used to store the results of queries such as ``referenced_decl``
  * called on references to object formals from inside a instantiated generic
  * in order to return the relevant actual.
  *
  * Indeed, ``referenced_decl`` must return a ``BasicDecl``, but actuals of
  * generic instantiations are ``Expr``. This wrapper node is therefore a way
  * to both satisfy the ``BasicDecl`` interface, and provide to the user the
  * expression of the actual through the ``expr`` field.
  *)

  type t =
    [
      | `AnonymousExprDecl of
          anonymous_expr_decl_fields
    ]

  type fields = anonymous_expr_decl_fields =
    
  {
         
    f_expr: expr
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int


      
  val p_get_formal :
    ?imprecise_fallback:
    bool
    -> [< anonymous_expr_decl ]
    -> defining_name option
  (**
  * Return the generic formal object declaration corresponding to this actual.
  *)


      
  val f_expr :
    [< anonymous_expr_decl]
    -> expr


end

module AbstractStateDecl : sig
  (**
  * Contained (directly or indirectly) in an AbstractStateDeclExpr, and is used
  * to represent the BasicDecl associated with the abstract state introduced by
  * the Abstract_State aspect. This node is necessary because all of our name
  * resolution routines expect BasicDecls as environments' values.
  *
  * The only purpose of this node is to populate the env with the abstract
  * state declared through this node, so it can be referred in SPARK aspects
  * such as Global, Depends, Refined_State, etc.
  *)

  type t =
    [
      | `AbstractStateDecl of
          abstract_state_decl_fields
    ]

  type fields = abstract_state_decl_fields =
    
  {
         
    f_name: defining_name
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_name :
    [< abstract_state_decl]
    -> defining_name


end

module BasicDecl : sig
  (**
  * Root class for an Ada declaration (:rmlink:`3.1`). A declaration associates
  * a name with a language entity, for example a type or a variable.
  *)

  type t =
    [
      | AbstractStateDecl.t
      | AnonymousExprDecl.t
      | BaseFormalParamDecl.t
      | BasePackageDecl.t
      | BaseTypeDecl.t
      | BasicSubpDecl.t
      | BodyNode.t
      | EntryIndexSpec.t
      | ErrorDecl.t
      | ExceptionDecl.t
      | ExceptionHandler.t
      | ForLoopVarDecl.t
      | GenericDecl.t
      | GenericInstantiation.t
      | GenericRenamingDecl.t
      | LabelDecl.t
      | NamedStmtDecl.t
      | NumberDecl.t
      | ObjectDecl.t
      | PackageRenamingDecl.t
      | SingleProtectedDecl.t
      | SingleTaskDecl.t
    ]


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int


      
  val p_is_formal :
    [< basic_decl ]
    -> bool
  (**
  * Whether this decl is the nested decl of a generic formal declaration.
  *)

      
  val p_doc_annotations :
    [< basic_decl ]
    -> DocAnnotation.t list
  (**
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
  *    allow experiments, it is totally unsupported and the API and behavior
  *    are very likely to change in the future.
  *)

      
  val p_doc :
    [< basic_decl ]
    -> string
  (**
  * Return the documentation associated with this decl. Raises a property error
  * if the doc is incorrectly formatted.
  *
  * .. attention:: This is an experimental feature, so even if it is exposed to
  *    allow experiments, it is totally unsupported and the API and behavior
  *    are very likely to change in the future.
  *)

      
  val p_previous_part_for_decl :
    ?imprecise_fallback:
    bool
    -> [< basic_decl ]
    -> basic_decl option
  (**
  * Return the previous part for this decl, if applicable.
  *
  * .. note:: It is not named previous_part, because BaseTypeDecl has a more
  *    precise version of previous_part that returns a BaseTypeDecl. Probably,
  *    we want to rename the specific versions, and have the root property be
  *    named previous_part. (TODO R925-008)
  *)

      
  val p_canonical_part :
    ?imprecise_fallback:
    bool
    -> [< basic_decl ]
    -> basic_decl option
  (**
  * Return the canonical part for this decl. In the case of decls composed of
  * several parts, the canonical part will be the first part.
  *)

      
  val p_all_parts :
    ?imprecise_fallback:
    bool
    -> [< basic_decl ]
    -> basic_decl list
  (**
  * Return all parts that define this entity, sorted from first part to last
  * part.
  *)

      
  val p_is_static_decl :
    ?imprecise_fallback:
    bool
    -> [< basic_decl ]
    -> bool
  (**
  * Return whether this declaration is static.
  *)

      
  val p_get_aspect_assoc :
    [< basic_decl ]
    -> string
    -> aspect_assoc option
  (**
  * Return the aspect with name ``name`` for this entity.
  *)

      
  val p_get_aspect_spec_expr :
    [< basic_decl ]
    -> string
    -> expr option
  (**
  * Return the expression associated to the aspect with name ``name`` for this
  * entity.
  *)

      
  val p_get_aspect :
    ?imprecise_fallback:
    bool
    -> [< basic_decl ]
    -> string
    -> Aspect.t
  (**
  * Return the aspect with name ``name`` associated to this entity.
  *
  * Aspects are properties of entities that can be specified by the Ada
  * program, either via aspect specifications, pragmas, or attributes.
  *
  * This will return the syntactic node corresponding to attribute directly.
  *
  * Note: for some aspects (e.g. Inline), Libadalang will check if they are
  * defined on any part of the entity.
  *)

      
  val p_has_aspect :
    ?imprecise_fallback:
    bool
    -> [< basic_decl ]
    -> string
    -> bool
  (**
  * Returns whether the boolean aspect named ``name`` is set on the entity
  * represented by this node.
  *
  * "Aspect" is used as in RM terminology (see :rmlink:`13`).
  *)

      
  val p_get_pragma :
    [< basic_decl ]
    -> string
    -> pragma_node option
  (**
  * Return the pragma with name ``name`` associated to this entity.
  *
  * Please use the ``p_get_aspects`` property instead if you are interested in
  * aspects, i.e. information that can be represented by either aspect
  * specification nodes, pragma nodes or attribute definition nodes.
  *)

      
  val p_get_representation_clause :
    ?imprecise_fallback:
    bool
    -> [< basic_decl ]
    -> string
    -> attribute_def_clause option
  (**
  * Return the representation clause associated to this type decl that defines
  * the given attribute name.
  *)

      
  val p_get_at_clause :
    ?imprecise_fallback:
    bool
    -> [< basic_decl ]
    -> at_clause option
  (**
  * Return the at clause associated to this declaration.
  *)

      
  val p_is_imported :
    [< basic_decl ]
    -> bool
  (**
  * Whether this declaration is imported from another language.
  *)

      
  val p_is_ghost_code :
    [< basic_decl ]
    -> bool
  (**
  * Return whether this declaration is ghost code or not. See SPARK RM 6.9.
  *)

      
  val p_is_compilation_unit_root :
    [< basic_decl ]
    -> bool
  (**
  * Whether a BasicDecl is the root decl for its unit.
  *)

      
  val p_is_visible :
    [< basic_decl ]
    -> ada_node
    -> bool
  (**
  * Return whether this declaration is visible from the point of view of the
  * given ``origin`` node.
  *
  * .. attention:: Only package-level (public or private) declarations are
  *    supported for now.
  *)

      
  val p_base_subp_declarations :
    ?imprecise_fallback:
    bool
    -> [< basic_decl ]
    -> basic_decl list
  (**
  * If Self declares a primitive subprogram of some tagged type T, return the
  * set of all subprogram declarations that it overrides (including itself).
  *
  * .. note:: for the moment this only works for tagged types. Remains to be
  *    seen if we need to extend it.
  *)

      
  val p_root_subp_declarations :
    ?origin:
    ada_node
    -> ?imprecise_fallback:
    bool
    -> [< basic_decl ]
    -> basic_decl list
  (**
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
  *)

      
  val p_find_all_overrides :
    ?imprecise_fallback:
    bool
    -> [< basic_decl ]
    -> analysis_unit list
    -> basic_decl list
  (**
  * If Self is the declaration of a primitive of some type T, return the list
  * of all subprogram that override this subprogram among the given units.
  *)

      
  val p_defining_names :
    [< basic_decl ]
    -> defining_name list
  (**
  * Get all the names of this basic declaration.
  *)

      
  val p_defining_name :
    [< basic_decl ]
    -> defining_name option
  (**
  * Get the name of this declaration. If this declaration has several names, it
  * will return the first one.
  *)

      
  val p_type_expression :
    [< basic_decl ]
    -> type_expr option
  (**
  * Return the type expression for this BasicDecl if applicable, a null
  * otherwise.
  *)

      
  val p_subp_spec_or_null :
    ?follow_generic:
    bool
    -> [< basic_decl ]
    -> base_subp_spec option
  (**
  * If Self is a Subp, returns the specification of this subprogram.
  *
  * If ``follow_generic`` is True, will also work for instances of
  * ``GenericSubpDecl``.
  *)

      
  val p_is_subprogram :
    [< basic_decl ]
    -> bool
  (**
  * Return True if self is a subprogram node in the general sense (which is, an
  * entity that can be called). This includes separates and entries.
  *
  * .. attention: This is a purely syntactic query and will return True for
  *    everything that is a syntactic entity that can be called like a
  *    subprogram in some contexts, even generic formal subprograms for
  *    example.
  *)

      
  val p_relative_name :
    [< basic_decl ]
    -> single_tok_node option
  (**
  * Return the relative name for Self. If Self's defining name is ``A.B.C``,
  * return ``C`` as a node.
  *)

      
  val p_relative_name_text :
    [< basic_decl ]
    -> string
  (**
  * Return the relative name for Self, as text.
  *)

      
  val p_next_part_for_decl :
    ?imprecise_fallback:
    bool
    -> [< basic_decl ]
    -> basic_decl option
  (**
  * Return the next part of this declaration, if applicable.
  *
  * .. note:: It is not named next_part, because BaseTypeDecl has a more
  *    precise version of next_part that returns a BaseTypeDecl. Probably, we
  *    want to rename the specific versions, and have the root property be
  *    named next_part. (TODO R925-008)
  *)

      
  val p_body_part_for_decl :
    ?imprecise_fallback:
    bool
    -> [< basic_decl ]
    -> body_node option
  (**
  * Return the body corresponding to this declaration, if applicable.
  *
  * .. note:: It is not named body_part, subclasses have more precise versions
  *    named body_part and returning a more precise result. Probably, we want
  *    to rename the specific versions, and have the root property be named
  *    body_part. (TODO R925-008)
  *)

      
  val p_most_visible_part :
    ?imprecise_fallback:
    bool
    -> [< basic_decl ]
    -> ada_node
    -> basic_decl option
  (**
  * Given an origin node and the entity represented by Self, this property
  * returns the most visible completion of Self that can be seen by origin,
  * according to Ada's visibility rules.
  *)

      
  val p_fully_qualified_name_array :
    ?include_profile:
    bool
    -> [< basic_decl ]
    -> string list
  (**
  * Return the fully qualified name corresponding to this declaration, as an
  * array of symbols.
  *)

      
  val p_fully_qualified_name :
    [< basic_decl ]
    -> string
  (**
  * Return the fully qualified name corresponding to this declaration.
  *)

      
  val p_canonical_fully_qualified_name :
    [< basic_decl ]
    -> string
  (**
  * Return a canonical representation of the fully qualified name corresponding
  * to this declaration.
  *)

      
  val p_unique_identifying_name :
    [< basic_decl ]
    -> string
  (**
  * Return a unique identifying name for this declaration, provided this
  * declaration is a public declaration. In the case of subprograms, this will
  * include the profile.
  *
  * .. attention:: This will only return a unique name for public declarations.
  *    Notably, anything nested in an unnamed declare block won't be handled
  *    correctly.
  *)

      
  val p_is_constant_object :
    [< basic_decl ]
    -> bool
  (**
  * Return whether this object is constant or not.
  *)



end

module ParamAssoc : sig
  (**
  * Assocation (X => Y) used for parameter associations (:rmlink:`6.4`).
  *)

  type t =
    [
      | `ParamAssoc of
          param_assoc_fields
    ]

  type fields = param_assoc_fields =
    
  {
         
    f_designator: [
      | `Identifier
          of identifier_fields
      | `OthersDesignator
          of others_designator_fields
      | `StringLiteral
          of string_literal_fields
    ]
    option
    Lazy.t;
         
    f_r_expr: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BoxExpr
          of box_expr_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_designator :
    [< param_assoc]
    -> [identifier | others_designator | string_literal] option

      
  val f_r_expr :
    [< param_assoc]
    -> [allocator | attribute_ref | base_aggregate | bin_op | box_expr | call_expr | char_literal | concat_op | cond_expr | decl_expr | dotted_name | explicit_deref | identifier | membership_expr | null_literal | num_literal | paren_expr | qual_expr | quantified_expr | raise_expr | reduce_attribute_ref | string_literal | target_name | un_op | update_attribute_ref]


end

module IteratedAssoc : sig
  (**
  * Iterated association (Ada 2020, :rmlink:`4.3.3`).
  *)

  type t =
    [
      | `IteratedAssoc of
          iterated_assoc_fields
    ]

  type fields = iterated_assoc_fields =
    
  {
         
    f_spec: for_loop_spec
    Lazy.t;
         
    f_r_expr: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_spec :
    [< iterated_assoc]
    -> for_loop_spec

      
  val f_r_expr :
    [< iterated_assoc]
    -> [allocator | attribute_ref | base_aggregate | bin_op | call_expr | char_literal | concat_op | cond_expr | decl_expr | dotted_name | explicit_deref | identifier | membership_expr | null_literal | num_literal | paren_expr | qual_expr | quantified_expr | raise_expr | reduce_attribute_ref | string_literal | target_name | un_op | update_attribute_ref]


end

module CompositeConstraintAssoc : sig
  (**
  * Association of discriminant names to an expression (:rmlink:`3.7.1`).
  *)

  type t =
    [
      | `CompositeConstraintAssoc of
          composite_constraint_assoc_fields
    ]

  type fields = composite_constraint_assoc_fields =
    
  {
         
    f_ids: identifier_list
    Lazy.t;
         
    f_constraint_expr: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DiscreteSubtypeIndication
          of discrete_subtype_indication_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_ids :
    [< composite_constraint_assoc]
    -> identifier_list

      
  val f_constraint_expr :
    [< composite_constraint_assoc]
    -> [allocator | attribute_ref | base_aggregate | bin_op | call_expr | char_literal | concat_op | cond_expr | decl_expr | discrete_subtype_indication | dotted_name | explicit_deref | identifier | membership_expr | null_literal | num_literal | paren_expr | qual_expr | quantified_expr | raise_expr | reduce_attribute_ref | string_literal | target_name | un_op | update_attribute_ref]


end

module MultiDimArrayAssoc : sig
  (**
  * Association used for multi-dimension array aggregates.
  *)

  type t =
    [
      | `MultiDimArrayAssoc of
          multi_dim_array_assoc_fields
    ]

  type fields = multi_dim_array_assoc_fields =
    
  {
         
    f_designators: alternatives_list
    Lazy.t;
         
    f_r_expr: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BoxExpr
          of box_expr_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_designators :
    [< multi_dim_array_assoc]
    -> alternatives_list

      
  val f_r_expr :
    [< multi_dim_array_assoc]
    -> [allocator | attribute_ref | base_aggregate | bin_op | box_expr | call_expr | char_literal | concat_op | cond_expr | decl_expr | dotted_name | explicit_deref | identifier | membership_expr | null_literal | num_literal | paren_expr | qual_expr | quantified_expr | raise_expr | reduce_attribute_ref | string_literal | target_name | un_op | update_attribute_ref]


end

module AggregateAssoc : sig
  (**
  * Assocation (X => Y) used for aggregates associations (:rmlink:`4.3`).
  *)

  type t =
    [
      | `AggregateAssoc of
          aggregate_assoc_fields
      | `MultiDimArrayAssoc of
          multi_dim_array_assoc_fields
    ]

  type fields = aggregate_assoc_fields =
    
  {
         
    f_designators: alternatives_list
    Lazy.t;
         
    f_r_expr: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BoxExpr
          of box_expr_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_designators :
    [< aggregate_assoc]
    -> alternatives_list

      
  val f_r_expr :
    [< aggregate_assoc]
    -> [allocator | attribute_ref | base_aggregate | bin_op | box_expr | call_expr | char_literal | concat_op | cond_expr | decl_expr | dotted_name | explicit_deref | identifier | membership_expr | null_literal | num_literal | paren_expr | qual_expr | quantified_expr | raise_expr | reduce_attribute_ref | string_literal | target_name | un_op | update_attribute_ref]


end

module BasicAssoc : sig
  (**
  * Association of one or several names to an expression.
  *)

  type t =
    [
      | AggregateAssoc.t
      | CompositeConstraintAssoc.t
      | IteratedAssoc.t
      | ParamAssoc.t
    ]


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int


      
  val p_get_params :
    ?imprecise_fallback:
    bool
    -> [< basic_assoc ]
    -> defining_name list
  (**
  * Return the list of parameters that this association refers to.
  *)



end

module RecordDef : sig
  (**
  * Record definition that contains components (``record ... end record``).
  *)

  type t =
    [
      | `RecordDef of
          record_def_fields
    ]

  type fields = record_def_fields =
    
  {
         
    f_components: component_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_components :
    [< record_def]
    -> component_list


end

module NullRecordDef : sig
  (**
  * Record definition for ``null record``.
  *)

  type t =
    [
      | `NullRecordDef of
          null_record_def_fields
    ]

  type fields = null_record_def_fields =
    
  {
         
    f_components: component_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_components :
    [< null_record_def]
    -> component_list


end

module BaseRecordDef : sig
  (**
  * Base class for record definitions (:rmlink:`3.8`).
  *)

  type t =
    [
      | NullRecordDef.t
      | RecordDef.t
    ]


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_components :
    [< base_record_def]
    -> component_list


end

module GenericFormalPart : sig
  (**
  * List of declaration for generic formals (:rmlink:`12.1`).
  *)

  type t =
    [
      | `GenericFormalPart of
          generic_formal_part_fields
    ]

  type fields = generic_formal_part_fields =
    
  {
         
    f_decls: ada_node_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_decls :
    [< generic_formal_part]
    -> ada_node_list


end

module EntryCompletionFormalParams : sig
  (**
  * Formal parameters for the completion of an ``EntryDecl`` (either an
  * ``EntryBody`` or an ``AcceptStmt``).
  *)

  type t =
    [
      | `EntryCompletionFormalParams of
          entry_completion_formal_params_fields
    ]

  type fields = entry_completion_formal_params_fields =
    
  {
         
    f_params: params
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_params :
    [< entry_completion_formal_params]
    -> params option


end

module UnknownDiscriminantPart : sig
  (**
  * Unknown list of discriminants in type declarations (:rmlink:`3.7`).
  *)

  type t =
    [
      | `UnknownDiscriminantPart of
          unknown_discriminant_part_fields
    ]

  type fields = unknown_discriminant_part_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module KnownDiscriminantPart : sig
  (**
  * Known list of discriminants in type declarations (:rmlink:`3.7`).
  *)

  type t =
    [
      | `KnownDiscriminantPart of
          known_discriminant_part_fields
    ]

  type fields = known_discriminant_part_fields =
    
  {
         
    f_discr_specs: discriminant_spec_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_discr_specs :
    [< known_discriminant_part]
    -> discriminant_spec_list


end

module DiscriminantPart : sig
  (**
  * Specification for discriminants in type declarations.
  *)

  type t =
    [
      | KnownDiscriminantPart.t
      | UnknownDiscriminantPart.t
    ]


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module ComponentList : sig
  (**
  * List of component declarations (:rmlink:`3.8`).
  *)

  type t =
    [
      | `ComponentList of
          component_list_fields
    ]

  type fields = component_list_fields =
    
  {
         
    f_components: ada_node_list
    Lazy.t;
         
    f_variant_part: variant_part
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_components :
    [< component_list]
    -> ada_node_list

      
  val f_variant_part :
    [< component_list]
    -> variant_part option


end

module SyntheticUnarySpec : sig
  (**
  * Synthetic subprogram specification for unary operators.
  *)

  type t =
    [
      | `SyntheticUnarySpec of
          synthetic_unary_spec_fields
    ]

  type fields = synthetic_unary_spec_fields =
    
  {
         
    f_right_param: synthetic_formal_param_decl
    Lazy.t;
         
    f_return_type_expr: synthetic_type_expr
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_right_param :
    [< synthetic_unary_spec]
    -> synthetic_formal_param_decl

      
  val f_return_type_expr :
    [< synthetic_unary_spec]
    -> synthetic_type_expr


end

module SyntheticBinarySpec : sig
  (**
  * Synthetic subprogram specification for binary operators.
  *)

  type t =
    [
      | `SyntheticBinarySpec of
          synthetic_binary_spec_fields
    ]

  type fields = synthetic_binary_spec_fields =
    
  {
         
    f_left_param: synthetic_formal_param_decl
    Lazy.t;
         
    f_right_param: synthetic_formal_param_decl
    Lazy.t;
         
    f_return_type_expr: type_expr
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_left_param :
    [< synthetic_binary_spec]
    -> synthetic_formal_param_decl

      
  val f_right_param :
    [< synthetic_binary_spec]
    -> synthetic_formal_param_decl

      
  val f_return_type_expr :
    [< synthetic_binary_spec]
    -> type_expr


end

module SubpSpec : sig
  (**
  * Subprogram specification (:rmlink:`6.1`).
  *)

  type t =
    [
      | `SubpSpec of
          subp_spec_fields
    ]

  type fields = subp_spec_fields =
    
  {
         
    f_subp_kind: subp_kind
    Lazy.t;
         
    f_subp_name: defining_name
    option
    Lazy.t;
         
    f_subp_params: params
    option
    Lazy.t;
         
    f_subp_returns: [
      | `AnonymousType
          of anonymous_type_fields
      | `ConstrainedSubtypeIndication
          of constrained_subtype_indication_fields
      | `DiscreteSubtypeIndication
          of discrete_subtype_indication_fields
      | `SubtypeIndication
          of subtype_indication_fields
    ]
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_subp_kind :
    [< subp_spec]
    -> subp_kind

      
  val f_subp_name :
    [< subp_spec]
    -> defining_name option

      
  val f_subp_params :
    [< subp_spec]
    -> params option

      
  val f_subp_returns :
    [< subp_spec]
    -> [anonymous_type | subtype_indication] option


end

module EnumSubpSpec : sig
  (**
  * Synthetic node for the abstract subprogram spec of an enum literal.
  *
  * NOTE: This has no existence in the ARM. While enum literals are functions
  * semantically, they're not such syntactically.
  *)

  type t =
    [
      | `EnumSubpSpec of
          enum_subp_spec_fields
    ]

  type fields = enum_subp_spec_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module EntrySpec : sig
  (**
  * Entry specification.
  *
  * This node does not have ARM existence, because in the RM subprogram
  * specifications don't encompass the ad-hoc specifications that happen in
  * entry declarations. Entry declarations are described in :rmlink:`9.5.2`.
  *)

  type t =
    [
      | `EntrySpec of
          entry_spec_fields
    ]

  type fields = entry_spec_fields =
    
  {
         
    f_entry_name: defining_name
    Lazy.t;
         
    f_family_type: [
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `CallExpr
          of call_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConstrainedSubtypeIndication
          of constrained_subtype_indication_fields
      | `DiscreteSubtypeIndication
          of discrete_subtype_indication_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `QualExpr
          of qual_expr_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `SubtypeIndication
          of subtype_indication_fields
      | `TargetName
          of target_name_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    option
    Lazy.t;
         
    f_entry_params: params
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_entry_name :
    [< entry_spec]
    -> defining_name

      
  val f_family_type :
    [< entry_spec]
    -> [attribute_ref | bin_op | call_expr | char_literal | dotted_name | explicit_deref | identifier | qual_expr | reduce_attribute_ref | string_literal | subtype_indication | target_name | update_attribute_ref] option

      
  val f_entry_params :
    [< entry_spec]
    -> params option


end

module BaseSubpSpec : sig
  (**
  * Base class for subprogram specifications (:rmlink:`6.1`).
  *)

  type t =
    [
      | EntrySpec.t
      | EnumSubpSpec.t
      | SubpSpec.t
      | SyntheticBinarySpec.t
      | SyntheticUnarySpec.t
    ]


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int


      
  val p_returns :
    [< base_subp_spec ]
    -> type_expr option
  (**
  * Syntax property. Return the type expression node corresponding to the
  * return of this subprogram spec.
  *)

      
  val p_params :
    [< base_subp_spec ]
    -> param_spec list
  (**
  * Returns the array of parameters specification for this subprogram spec.
  *)

      
  val p_primitive_subp_types :
    ?imprecise_fallback:
    bool
    -> [< base_subp_spec ]
    -> base_type_decl list
  (**
  * Return the types of which this subprogram is a primitive of.
  *)

      
  val p_primitive_subp_first_type :
    ?imprecise_fallback:
    bool
    -> [< base_subp_spec ]
    -> base_type_decl option
  (**
  * Return the first type of which this subprogram is a primitive of.
  *)

      
  val p_primitive_subp_tagged_type :
    ?imprecise_fallback:
    bool
    -> [< base_subp_spec ]
    -> base_type_decl option
  (**
  * If this subprogram is a primitive for a tagged type, then return this type.
  *)

      
  val p_return_type :
    ?origin:
    ada_node
    -> [< base_subp_spec ]
    -> base_type_decl option
  (**
  * Returns the return type of Self, if applicable (e.g. if Self is a
  * subprogram). Else, returns null.
  *)



end

module BaseFormalParamHolder : sig
  (**
  * Base class for lists of formal parameters. This is used in every case a
  * list of "formals" can be called or instantiated, so in all the following
  * cases:
  *
  * * Subprogram specifications (and subprogram calls).
  *
  * * Component lists (and aggregates).
  *
  * * Generic formals (and generic instantiations).
  *
  * This allows to share the parameter unpacking/matching logic.
  *
  * This is a Libadalang abstraction that has no existence in the Ada reference
  * manual.
  *)

  type t =
    [
      | BaseSubpSpec.t
      | ComponentList.t
      | DiscriminantPart.t
      | EntryCompletionFormalParams.t
      | GenericFormalPart.t
    ]


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int


      
  val p_abstract_formal_params :
    [< base_formal_param_holder ]
    -> base_formal_param_decl list
  (**
  * Return the list of abstract formal parameters for this holder.
  *)

      
  val p_formal_params :
    [< base_formal_param_holder ]
    -> defining_name list
  (**
  * Return all parameters as a ``DefiningName`` array. This property doesn't
  * return record discriminants nor variants when called on a record component
  * list.
  *)

      
  val p_nb_min_params :
    [< base_formal_param_holder ]
    -> int
  (**
  * Return the minimum number of parameters this subprogram can be called while
  * still being a legal call.
  *)

      
  val p_nb_max_params :
    [< base_formal_param_holder ]
    -> int
  (**
  * Return the maximum number of parameters this subprogram can be called while
  * still being a legal call.
  *)

      
  val p_param_types :
    ?origin:
    ada_node
    -> [< base_formal_param_holder ]
    -> base_type_decl list
  (**
  * Returns the type of each parameter of Self.
  *)



end

module PragmaArgumentAssoc : sig
  (**
  * Argument assocation in a pragma.
  *)

  type t =
    [
      | `PragmaArgumentAssoc of
          pragma_argument_assoc_fields
    ]

  type fields = pragma_argument_assoc_fields =
    
  {
         
    f_name: [
      | `AttributeRef
          of attribute_ref_fields
      | `Identifier
          of identifier_fields
    ]
    option
    Lazy.t;
         
    f_expr: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_name :
    [< pragma_argument_assoc]
    -> [attribute_ref | identifier] option

      
  val f_expr :
    [< pragma_argument_assoc]
    -> [allocator | attribute_ref | base_aggregate | bin_op | call_expr | char_literal | concat_op | cond_expr | decl_expr | dotted_name | explicit_deref | identifier | membership_expr | null_literal | num_literal | paren_expr | qual_expr | quantified_expr | raise_expr | reduce_attribute_ref | string_literal | target_name | un_op | update_attribute_ref]


end

module ContractCaseAssoc : sig
  (**
  * Single association for the ``Contract_Case`` aspect.
  *)

  type t =
    [
      | `ContractCaseAssoc of
          contract_case_assoc_fields
    ]

  type fields = contract_case_assoc_fields =
    
  {
         
    f_guard: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `OthersDesignator
          of others_designator_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
         
    f_consequence: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_guard :
    [< contract_case_assoc]
    -> [allocator | attribute_ref | base_aggregate | bin_op | call_expr | char_literal | concat_op | cond_expr | decl_expr | dotted_name | explicit_deref | identifier | membership_expr | null_literal | num_literal | others_designator | paren_expr | qual_expr | quantified_expr | raise_expr | reduce_attribute_ref | string_literal | target_name | un_op | update_attribute_ref]

      
  val f_consequence :
    [< contract_case_assoc]
    -> [allocator | attribute_ref | base_aggregate | bin_op | call_expr | char_literal | concat_op | cond_expr | decl_expr | dotted_name | explicit_deref | identifier | membership_expr | null_literal | num_literal | paren_expr | qual_expr | quantified_expr | raise_expr | reduce_attribute_ref | string_literal | target_name | un_op | update_attribute_ref]


end

module BaseAssoc : sig
  (**
  * Abstract class for a key/value association, where the value is an
  * expression.
  *)

  type t =
    [
      | ContractCaseAssoc.t
      | PragmaArgumentAssoc.t
    ]


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int


      
  val p_assoc_expr :
    [< base_assoc ]
    -> expr option
  (**
  * Returns the expression side of this assoc node.
  *)



end

module AspectSpec : sig
  (**
  * List of aspects in a declaration (:rmlink:`13.1.1`).
  *)

  type t =
    [
      | `AspectSpec of
          aspect_spec_fields
    ]

  type fields = aspect_spec_fields =
    
  {
         
    f_aspect_assocs: aspect_assoc_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_aspect_assocs :
    [< aspect_spec]
    -> aspect_assoc_list


end

module RecordRepClause : sig
  (**
  * Representation clause for a record type (:rmlink:`13.5.1`).
  *)

  type t =
    [
      | `RecordRepClause of
          record_rep_clause_fields
    ]

  type fields = record_rep_clause_fields =
    
  {
         
    f_name: [
      | `CharLiteral
          of char_literal_fields
      | `DottedName
          of dotted_name_fields
      | `Identifier
          of identifier_fields
      | `StringLiteral
          of string_literal_fields
    ]
    Lazy.t;
         
    f_at_expr: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    option
    Lazy.t;
         
    f_components: ada_node_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_name :
    [< record_rep_clause]
    -> [char_literal | dotted_name | identifier | string_literal]

      
  val f_at_expr :
    [< record_rep_clause]
    -> [allocator | attribute_ref | base_aggregate | bin_op | call_expr | char_literal | concat_op | cond_expr | decl_expr | dotted_name | explicit_deref | identifier | null_literal | num_literal | paren_expr | qual_expr | quantified_expr | raise_expr | reduce_attribute_ref | string_literal | target_name | un_op | update_attribute_ref] option

      
  val f_components :
    [< record_rep_clause]
    -> ada_node_list


end

module EnumRepClause : sig
  (**
  * Representation clause for enumeration types (:rmlink:`13.4`).
  *)

  type t =
    [
      | `EnumRepClause of
          enum_rep_clause_fields
    ]

  type fields = enum_rep_clause_fields =
    
  {
         
    f_type_name: [
      | `CharLiteral
          of char_literal_fields
      | `DottedName
          of dotted_name_fields
      | `Identifier
          of identifier_fields
      | `StringLiteral
          of string_literal_fields
    ]
    Lazy.t;
         
    f_aggregate: base_aggregate
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int


      
  val p_params :
    [< enum_rep_clause ]
    -> ParamActual.t list
  (**
  * Returns an array of pairs, associating enum literals to representation
  * clause actuals.
  *)


      
  val f_type_name :
    [< enum_rep_clause]
    -> [char_literal | dotted_name | identifier | string_literal]

      
  val f_aggregate :
    [< enum_rep_clause]
    -> base_aggregate


end

module AttributeDefClause : sig
  (**
  * Clause for an attribute definition (``for ...'Attribute use ...;``)
  * (:rmlink:`13.3`).
  *)

  type t =
    [
      | `AttributeDefClause of
          attribute_def_clause_fields
    ]

  type fields = attribute_def_clause_fields =
    
  {
         
    f_attribute_expr: [
      | `AttributeRef
          of attribute_ref_fields
      | `CallExpr
          of call_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `QualExpr
          of qual_expr_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
         
    f_expr: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_attribute_expr :
    [< attribute_def_clause]
    -> [attribute_ref | call_expr | char_literal | dotted_name | explicit_deref | identifier | qual_expr | reduce_attribute_ref | string_literal | target_name | update_attribute_ref]

      
  val f_expr :
    [< attribute_def_clause]
    -> [allocator | attribute_ref | base_aggregate | bin_op | call_expr | char_literal | concat_op | cond_expr | decl_expr | dotted_name | explicit_deref | identifier | membership_expr | null_literal | num_literal | paren_expr | qual_expr | quantified_expr | raise_expr | reduce_attribute_ref | string_literal | target_name | un_op | update_attribute_ref]


end

module AtClause : sig
  (**
  * Representation clause (``for .. use at ...;``) (:rmlink:`13.5.1`).
  *)

  type t =
    [
      | `AtClause of
          at_clause_fields
    ]

  type fields = at_clause_fields =
    
  {
         
    f_name: [
      | `CharLiteral
          of char_literal_fields
      | `Identifier
          of identifier_fields
      | `StringLiteral
          of string_literal_fields
    ]
    Lazy.t;
         
    f_expr: [
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_name :
    [< at_clause]
    -> [char_literal | identifier | string_literal]

      
  val f_expr :
    [< at_clause]
    -> [allocator | attribute_ref | base_aggregate | bin_op | call_expr | char_literal | concat_op | cond_expr | decl_expr | dotted_name | explicit_deref | identifier | membership_expr | null_literal | num_literal | paren_expr | qual_expr | quantified_expr | raise_expr | reduce_attribute_ref | string_literal | target_name | un_op | update_attribute_ref]


end

module AspectClause : sig
  (**
  * Base class for aspect clauses.
  *)

  type t =
    [
      | AtClause.t
      | AttributeDefClause.t
      | EnumRepClause.t
      | RecordRepClause.t
    ]


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module AspectAssoc : sig
  (**
  * Name/expression association in an aspect.
  *)

  type t =
    [
      | `AspectAssoc of
          aspect_assoc_fields
    ]

  type fields = aspect_assoc_fields =
    
  {
         
    f_id: [
      | `AttributeRef
          of attribute_ref_fields
      | `CallExpr
          of call_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `QualExpr
          of qual_expr_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    Lazy.t;
         
    f_expr: [
      | `AbstractStateDeclExpr
          of abstract_state_decl_expr_fields
      | `Aggregate
          of aggregate_fields
      | `Allocator
          of allocator_fields
      | `AttributeRef
          of attribute_ref_fields
      | `BinOp
          of bin_op_fields
      | `BracketAggregate
          of bracket_aggregate_fields
      | `BracketDeltaAggregate
          of bracket_delta_aggregate_fields
      | `CallExpr
          of call_expr_fields
      | `CaseExpr
          of case_expr_fields
      | `CharLiteral
          of char_literal_fields
      | `ConcatOp
          of concat_op_fields
      | `ContractCases
          of contract_cases_fields
      | `DeclExpr
          of decl_expr_fields
      | `DeltaAggregate
          of delta_aggregate_fields
      | `DottedName
          of dotted_name_fields
      | `ExplicitDeref
          of explicit_deref_fields
      | `Identifier
          of identifier_fields
      | `IfExpr
          of if_expr_fields
      | `IntLiteral
          of int_literal_fields
      | `MembershipExpr
          of membership_expr_fields
      | `NullLiteral
          of null_literal_fields
      | `NullRecordAggregate
          of null_record_aggregate_fields
      | `ParenExpr
          of paren_expr_fields
      | `QualExpr
          of qual_expr_fields
      | `QuantifiedExpr
          of quantified_expr_fields
      | `RaiseExpr
          of raise_expr_fields
      | `RealLiteral
          of real_literal_fields
      | `ReduceAttributeRef
          of reduce_attribute_ref_fields
      | `RelationOp
          of relation_op_fields
      | `StringLiteral
          of string_literal_fields
      | `TargetName
          of target_name_fields
      | `UnOp
          of un_op_fields
      | `UpdateAttributeRef
          of update_attribute_ref_fields
    ]
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int


      
  val p_is_ghost_code :
    [< aspect_assoc ]
    -> bool
  (**
  * Return whether this aspect is ghost code or not. See SPARK RM 6.9.
  *)


      
  val f_id :
    [< aspect_assoc]
    -> [attribute_ref | call_expr | char_literal | dotted_name | explicit_deref | identifier | qual_expr | reduce_attribute_ref | string_literal | target_name | update_attribute_ref]

      
  val f_expr :
    [< aspect_assoc]
    -> [abstract_state_decl_expr | allocator | attribute_ref | base_aggregate | bin_op | call_expr | char_literal | concat_op | cond_expr | contract_cases | decl_expr | dotted_name | explicit_deref | identifier | membership_expr | null_literal | num_literal | paren_expr | qual_expr | quantified_expr | raise_expr | reduce_attribute_ref | string_literal | target_name | un_op | update_attribute_ref] option


end

module UnconstrainedArrayIndices : sig
  (**
  * Unconstrained specification for array indexes (:rmlink:`3.6`).
  *)

  type t =
    [
      | `UnconstrainedArrayIndices of
          unconstrained_array_indices_fields
    ]

  type fields = unconstrained_array_indices_fields =
    
  {
         
    f_types: unconstrained_array_index_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_types :
    [< unconstrained_array_indices]
    -> unconstrained_array_index_list


end

module ConstrainedArrayIndices : sig
  (**
  * Constrained specification for array indexes (:rmlink:`3.6`).
  *)

  type t =
    [
      | `ConstrainedArrayIndices of
          constrained_array_indices_fields
    ]

  type fields = constrained_array_indices_fields =
    
  {
         
    f_list: constraint_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_list :
    [< constrained_array_indices]
    -> constraint_list


end

module ArrayIndices : sig
  (**
  * Specification for array indexes (:rmlink:`3.6`).
  *)

  type t =
    [
      | ConstrainedArrayIndices.t
      | UnconstrainedArrayIndices.t
    ]


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module AllPresent : sig
  (**

  *)

  type t =
    [
      | `AllPresent of
          all_present_fields
    ]

  type fields = all_present_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module AllAbsent : sig
  (**

  *)

  type t =
    [
      | `AllAbsent of
          all_absent_fields
    ]

  type fields = all_absent_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module AllNode : sig
  (**
  * Qualifier for the ``all`` keyword.
  *)

  type t =
    [
      | AllAbsent.t
      | AllPresent.t
    ]


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int


      
  val p_as_bool :
    [< all_node ]
    -> bool
  (**
  * Return whether this is an instance of AllPresent
  *)



end

module AliasedPresent : sig
  (**

  *)

  type t =
    [
      | `AliasedPresent of
          aliased_present_fields
    ]

  type fields = aliased_present_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module AliasedAbsent : sig
  (**

  *)

  type t =
    [
      | `AliasedAbsent of
          aliased_absent_fields
    ]

  type fields = aliased_absent_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module AliasedNode : sig
  (**
  * Qualifier for the ``aliased`` keyword.
  *)

  type t =
    [
      | AliasedAbsent.t
      | AliasedPresent.t
    ]


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int


      
  val p_as_bool :
    [< aliased_node ]
    -> bool
  (**
  * Return whether this is an instance of AliasedPresent
  *)



end

module VariantList : sig
  (**
  * List of Variant.
  *)

  type t =
    [
      | `VariantList of
          variant_list_fields
    ]

  type fields = variant_list_fields =
    
  {
    list : variant list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




  val f_list :
    [< variant_list]
    -> variant list

end

module UnconstrainedArrayIndexList : sig
  (**
  * List of UnconstrainedArrayIndex.
  *)

  type t =
    [
      | `UnconstrainedArrayIndexList of
          unconstrained_array_index_list_fields
    ]

  type fields = unconstrained_array_index_list_fields =
    
  {
    list : unconstrained_array_index list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




  val f_list :
    [< unconstrained_array_index_list]
    -> unconstrained_array_index list

end

module SelectWhenPartList : sig
  (**
  * List of SelectWhenPart.
  *)

  type t =
    [
      | `SelectWhenPartList of
          select_when_part_list_fields
    ]

  type fields = select_when_part_list_fields =
    
  {
    list : select_when_part list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




  val f_list :
    [< select_when_part_list]
    -> select_when_part list

end

module PragmaNodeList : sig
  (**
  * List of Pragma.
  *)

  type t =
    [
      | `PragmaNodeList of
          pragma_node_list_fields
    ]

  type fields = pragma_node_list_fields =
    
  {
    list : pragma_node list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




  val f_list :
    [< pragma_node_list]
    -> pragma_node list

end

module ParamSpecList : sig
  (**
  * List of ParamSpec.
  *)

  type t =
    [
      | `ParamSpecList of
          param_spec_list_fields
    ]

  type fields = param_spec_list_fields =
    
  {
    list : param_spec list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




  val f_list :
    [< param_spec_list]
    -> param_spec list

end

module ParentList : sig
  (**
  * List of parents in a type declaration.
  *
  * This list node can contain one of the following nodes: ``char_literal``,
  * ``dotted_name``, ``identifier``, ``string_literal``
  *)

  type t =
    [
      | `ParentList of
          parent_list_fields
    ]

  type fields = parent_list_fields =
    
  {
    list : name list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




  val f_list :
    [< parent_list]
    -> name list

end

module NameList : sig
  (**
  * List of Name.
  *
  * This list node can contain one of the following nodes: ``attribute_ref``,
  * ``call_expr``, ``char_literal``, ``dotted_name``, ``explicit_deref``,
  * ``identifier``, ``qual_expr``, ``reduce_attribute_ref``,
  * ``string_literal``, ``target_name``, ``update_attribute_ref``
  *)

  type t =
    [
      | `NameList of
          name_list_fields
      | `ParentList of
          parent_list_fields
    ]

  type fields = name_list_fields =
    
  {
    list : name list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




  val f_list :
    [< name_list]
    -> name list

end

module DiscriminantChoiceList : sig
  (**
  * List of discriminant associations.
  *)

  type t =
    [
      | `DiscriminantChoiceList of
          discriminant_choice_list_fields
    ]

  type fields = discriminant_choice_list_fields =
    
  {
    list : identifier list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




  val f_list :
    [< discriminant_choice_list]
    -> identifier list

end

module IdentifierList : sig
  (**
  * List of Identifier.
  *)

  type t =
    [
      | DiscriminantChoiceList.t
    ]


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




  val f_list :
    [< identifier_list]
    -> identifier list

end

module ExprAlternativesList : sig
  (**
  * List of alternatives in a membership test expression.
  *
  * This list node can contain one of the following nodes: ``allocator``,
  * ``attribute_ref``, ``base_aggregate``, ``bin_op``, ``call_expr``,
  * ``char_literal``, ``concat_op``, ``cond_expr``, ``decl_expr``,
  * ``discrete_subtype_name``, ``dotted_name``, ``explicit_deref``,
  * ``identifier``, ``null_literal``, ``num_literal``, ``paren_expr``,
  * ``qual_expr``, ``quantified_expr``, ``raise_expr``,
  * ``reduce_attribute_ref``, ``string_literal``, ``target_name``, ``un_op``,
  * ``update_attribute_ref``
  *)

  type t =
    [
      | `ExprAlternativesList of
          expr_alternatives_list_fields
    ]

  type fields = expr_alternatives_list_fields =
    
  {
    list : expr list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




  val f_list :
    [< expr_alternatives_list]
    -> expr list

end

module ExprList : sig
  (**
  * List of Expr.
  *
  * This list node can contain one of the following nodes: ``allocator``,
  * ``attribute_ref``, ``base_aggregate``, ``bin_op``, ``call_expr``,
  * ``char_literal``, ``concat_op``, ``cond_expr``, ``decl_expr``,
  * ``discrete_subtype_name``, ``dotted_name``, ``explicit_deref``,
  * ``identifier``, ``null_literal``, ``num_literal``, ``paren_expr``,
  * ``qual_expr``, ``quantified_expr``, ``raise_expr``,
  * ``reduce_attribute_ref``, ``string_literal``, ``target_name``, ``un_op``,
  * ``update_attribute_ref``
  *)

  type t =
    [
      | ExprAlternativesList.t
    ]


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




  val f_list :
    [< expr_list]
    -> expr list

end

module EnumLiteralDeclList : sig
  (**
  * List of EnumLiteralDecl.
  *)

  type t =
    [
      | `EnumLiteralDeclList of
          enum_literal_decl_list_fields
    ]

  type fields = enum_literal_decl_list_fields =
    
  {
    list : enum_literal_decl list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




  val f_list :
    [< enum_literal_decl_list]
    -> enum_literal_decl list

end

module ElsifStmtPartList : sig
  (**
  * List of ElsifStmtPart.
  *)

  type t =
    [
      | `ElsifStmtPartList of
          elsif_stmt_part_list_fields
    ]

  type fields = elsif_stmt_part_list_fields =
    
  {
    list : elsif_stmt_part list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




  val f_list :
    [< elsif_stmt_part_list]
    -> elsif_stmt_part list

end

module ElsifExprPartList : sig
  (**
  * List of ElsifExprPart.
  *)

  type t =
    [
      | `ElsifExprPartList of
          elsif_expr_part_list_fields
    ]

  type fields = elsif_expr_part_list_fields =
    
  {
    list : elsif_expr_part list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




  val f_list :
    [< elsif_expr_part_list]
    -> elsif_expr_part list

end

module DiscriminantSpecList : sig
  (**
  * List of DiscriminantSpec.
  *)

  type t =
    [
      | `DiscriminantSpecList of
          discriminant_spec_list_fields
    ]

  type fields = discriminant_spec_list_fields =
    
  {
    list : discriminant_spec list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




  val f_list :
    [< discriminant_spec_list]
    -> discriminant_spec list

end

module DefiningNameList : sig
  (**
  * List of DefiningName.
  *)

  type t =
    [
      | `DefiningNameList of
          defining_name_list_fields
    ]

  type fields = defining_name_list_fields =
    
  {
    list : defining_name list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




  val f_list :
    [< defining_name_list]
    -> defining_name list

end

module ContractCaseAssocList : sig
  (**
  * List of ContractCaseAssoc.
  *)

  type t =
    [
      | `ContractCaseAssocList of
          contract_case_assoc_list_fields
    ]

  type fields = contract_case_assoc_list_fields =
    
  {
    list : contract_case_assoc list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




  val f_list :
    [< contract_case_assoc_list]
    -> contract_case_assoc list

end

module ConcatOperandList : sig
  (**
  * List of ConcatOperand.
  *)

  type t =
    [
      | `ConcatOperandList of
          concat_operand_list_fields
    ]

  type fields = concat_operand_list_fields =
    
  {
    list : concat_operand list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




  val f_list :
    [< concat_operand_list]
    -> concat_operand list

end

module CompilationUnitList : sig
  (**
  * List of CompilationUnit.
  *)

  type t =
    [
      | `CompilationUnitList of
          compilation_unit_list_fields
    ]

  type fields = compilation_unit_list_fields =
    
  {
    list : compilation_unit list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




  val f_list :
    [< compilation_unit_list]
    -> compilation_unit list

end

module CaseStmtAlternativeList : sig
  (**
  * List of CaseStmtAlternative.
  *)

  type t =
    [
      | `CaseStmtAlternativeList of
          case_stmt_alternative_list_fields
    ]

  type fields = case_stmt_alternative_list_fields =
    
  {
    list : case_stmt_alternative list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




  val f_list :
    [< case_stmt_alternative_list]
    -> case_stmt_alternative list

end

module CaseExprAlternativeList : sig
  (**
  * List of CaseExprAlternative.
  *)

  type t =
    [
      | `CaseExprAlternativeList of
          case_expr_alternative_list_fields
    ]

  type fields = case_expr_alternative_list_fields =
    
  {
    list : case_expr_alternative list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




  val f_list :
    [< case_expr_alternative_list]
    -> case_expr_alternative list

end

module BasicDeclList : sig
  (**
  * List of BasicDecl.
  *
  * This list node can contain one of the following nodes: ``number_decl``,
  * ``object_decl``, ``single_protected_decl``, ``single_task_decl``
  *)

  type t =
    [
      | `BasicDeclList of
          basic_decl_list_fields
    ]

  type fields = basic_decl_list_fields =
    
  {
    list : basic_decl list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




  val f_list :
    [< basic_decl_list]
    -> basic_decl list

end

module AssocList : sig
  (**
  * List of associations.
  *)

  type t =
    [
      | `AssocList of
          assoc_list_fields
    ]

  type fields = assoc_list_fields =
    
  {
    list : basic_assoc list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int


      
  val p_zip_with_params :
    ?imprecise_fallback:
    bool
    -> [< assoc_list ]
    -> ParamActual.t list
  (**
  * Returns an array of pairs, associating formal parameters to actual
  * expressions. The formals to match are retrieved by resolving the call which
  * this AssocList represents the actuals of.
  *)



  val f_list :
    [< assoc_list]
    -> basic_assoc list

end

module BasicAssocList : sig
  (**
  * List of BasicAssoc.
  *)

  type t =
    [
      | AssocList.t
    ]


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




  val f_list :
    [< basic_assoc_list]
    -> basic_assoc list

end

module BaseAssocList : sig
  (**
  * List of BaseAssoc.
  *)

  type t =
    [
      | `BaseAssocList of
          base_assoc_list_fields
    ]

  type fields = base_assoc_list_fields =
    
  {
    list : base_assoc list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




  val f_list :
    [< base_assoc_list]
    -> base_assoc list

end

module AspectAssocList : sig
  (**
  * List of AspectAssoc.
  *)

  type t =
    [
      | `AspectAssocList of
          aspect_assoc_list_fields
    ]

  type fields = aspect_assoc_list_fields =
    
  {
    list : aspect_assoc list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




  val f_list :
    [< aspect_assoc_list]
    -> aspect_assoc list

end

module StmtList : sig
  (**
  * List of statements.
  *
  * This list node can contain one of the following nodes: ``pragma_node``,
  * ``stmt``
  *)

  type t =
    [
      | `StmtList of
          stmt_list_fields
    ]

  type fields = stmt_list_fields =
    
  {
    list : ada_node list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




  val f_list :
    [< stmt_list]
    -> ada_node list

end

module DeclList : sig
  (**
  * List of declarations.
  *
  * This list node can contain one of the following nodes:
  * ``abstract_subp_decl``, ``aspect_clause``, ``component_decl``,
  * ``entry_decl``, ``expr_function``, ``null_subp_decl``, ``pragma_node``,
  * ``subp_decl``, ``subp_renaming_decl``
  *)

  type t =
    [
      | `DeclList of
          decl_list_fields
    ]

  type fields = decl_list_fields =
    
  {
    list : ada_node list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




  val f_list :
    [< decl_list]
    -> ada_node list

end

module ConstraintList : sig
  (**
  * List of constraints.
  *
  * This list node can contain one of the following nodes: ``attribute_ref``,
  * ``bin_op``, ``call_expr``, ``char_literal``, ``dotted_name``,
  * ``explicit_deref``, ``identifier``, ``qual_expr``,
  * ``reduce_attribute_ref``, ``string_literal``, ``subtype_indication``,
  * ``target_name``, ``update_attribute_ref``
  *)

  type t =
    [
      | `ConstraintList of
          constraint_list_fields
    ]

  type fields = constraint_list_fields =
    
  {
    list : ada_node list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




  val f_list :
    [< constraint_list]
    -> ada_node list

end

module AlternativesList : sig
  (**
  * List of alternatives in a ``when ...`` clause.
  *
  * This list node can contain one of the following nodes: ``allocator``,
  * ``attribute_ref``, ``base_aggregate``, ``bin_op``, ``call_expr``,
  * ``char_literal``, ``concat_op``, ``cond_expr``, ``decl_expr``,
  * ``discrete_subtype_indication``, ``dotted_name``, ``explicit_deref``,
  * ``identifier``, ``membership_expr``, ``null_literal``, ``num_literal``,
  * ``others_designator``, ``paren_expr``, ``qual_expr``, ``quantified_expr``,
  * ``raise_expr``, ``reduce_attribute_ref``, ``string_literal``,
  * ``target_name``, ``un_op``, ``update_attribute_ref``
  *)

  type t =
    [
      | `AlternativesList of
          alternatives_list_fields
    ]

  type fields = alternatives_list_fields =
    
  {
    list : ada_node list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




  val f_list :
    [< alternatives_list]
    -> ada_node list

end

module AbstractStateDeclList : sig
  (**
  * List of AbstractStateDecls.
  *
  * This list node can contain one of the following nodes:
  * ``abstract_state_decl``, ``paren_abstract_state_decl``
  *)

  type t =
    [
      | `AbstractStateDeclList of
          abstract_state_decl_list_fields
    ]

  type fields = abstract_state_decl_list_fields =
    
  {
    list : ada_node list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




  val f_list :
    [< abstract_state_decl_list]
    -> ada_node list

end

module AdaNodeList : sig
  (**
  * List of AdaNode.
  *
  * This list node can contain one of the following nodes:
  * ``abstract_state_decl``, ``abstract_subp_decl``, ``allocator``,
  * ``aspect_clause``, ``attribute_ref``, ``base_aggregate``, ``bin_op``,
  * ``body_node``, ``call_expr``, ``char_literal``, ``component_clause``,
  * ``component_decl``, ``concat_op``, ``concrete_type_decl``, ``cond_expr``,
  * ``decl_expr``, ``dotted_name``, ``entry_decl``, ``error_decl``,
  * ``exception_decl``, ``exception_handler``, ``explicit_deref``,
  * ``generic_decl``, ``generic_formal``, ``generic_instantiation``,
  * ``generic_renaming_decl``, ``identifier``, ``incomplete_type_decl``,
  * ``membership_expr``, ``null_component_decl``, ``null_literal``,
  * ``num_literal``, ``number_decl``, ``object_decl``, ``others_designator``,
  * ``package_decl``, ``package_renaming_decl``, ``paren_abstract_state_decl``,
  * ``paren_expr``, ``pragma_node``, ``protected_type_decl``, ``qual_expr``,
  * ``quantified_expr``, ``raise_expr``, ``reduce_attribute_ref``,
  * ``single_protected_decl``, ``single_task_decl``, ``stmt``,
  * ``string_literal``, ``subp_decl``, ``subtype_decl``,
  * ``subtype_indication``, ``target_name``, ``task_type_decl``, ``un_op``,
  * ``update_attribute_ref``, ``use_clause``, ``with_clause``
  *)

  type t =
    [
      | `AdaNodeList of
          ada_node_list_fields
      | `AbstractStateDeclList of
          abstract_state_decl_list_fields
      | `AlternativesList of
          alternatives_list_fields
      | `ConstraintList of
          constraint_list_fields
      | `DeclList of
          decl_list_fields
      | `StmtList of
          stmt_list_fields
    ]

  type fields = ada_node_list_fields =
    
  {
    list : ada_node list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




  val f_list :
    [< ada_node_list]
    -> ada_node list

end

module AdaList : sig
  (**

  *)

  type t =
    [
      | AdaNodeList.t
      | AspectAssocList.t
      | BaseAssocList.t
      | BasicAssocList.t
      | BasicDeclList.t
      | CaseExprAlternativeList.t
      | CaseStmtAlternativeList.t
      | CompilationUnitList.t
      | ConcatOperandList.t
      | ContractCaseAssocList.t
      | DefiningNameList.t
      | DiscriminantSpecList.t
      | ElsifExprPartList.t
      | ElsifStmtPartList.t
      | EnumLiteralDeclList.t
      | ExprList.t
      | IdentifierList.t
      | NameList.t
      | ParamSpecList.t
      | PragmaNodeList.t
      | SelectWhenPartList.t
      | UnconstrainedArrayIndexList.t
      | VariantList.t
    ]


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module AbstractPresent : sig
  (**

  *)

  type t =
    [
      | `AbstractPresent of
          abstract_present_fields
    ]

  type fields = abstract_present_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module AbstractAbsent : sig
  (**

  *)

  type t =
    [
      | `AbstractAbsent of
          abstract_absent_fields
    ]

  type fields = abstract_absent_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module AbstractNode : sig
  (**
  * Qualifier for the ``abstract`` keyword.
  *)

  type t =
    [
      | AbstractAbsent.t
      | AbstractPresent.t
    ]


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int


      
  val p_as_bool :
    [< abstract_node ]
    -> bool
  (**
  * Return whether this is an instance of AbstractPresent
  *)



end

module AbortPresent : sig
  (**

  *)

  type t =
    [
      | `AbortPresent of
          abort_present_fields
    ]

  type fields = abort_present_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module AbortAbsent : sig
  (**

  *)

  type t =
    [
      | `AbortAbsent of
          abort_absent_fields
    ]

  type fields = abort_absent_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module AbortNode : sig
  (**
  * Qualifier for the ``abort`` keyword.
  *)

  type t =
    [
      | AbortAbsent.t
      | AbortPresent.t
    ]


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int


      
  val p_as_bool :
    [< abort_node ]
    -> bool
  (**
  * Return whether this is an instance of AbortPresent
  *)



end

module AdaNode : sig
  (**
  * Root node class for the Ada syntax tree.
  *)

  type t =
    [
      | AbortNode.t
      | AbstractNode.t
      | AdaList.t
      | AliasedNode.t
      | AllNode.t
      | ArrayIndices.t
      | AspectAssoc.t
      | AspectClause.t
      | AspectSpec.t
      | BaseAssoc.t
      | BaseFormalParamHolder.t
      | BaseRecordDef.t
      | BasicAssoc.t
      | BasicDecl.t
      | CaseStmtAlternative.t
      | CompilationUnit.t
      | ComponentClause.t
      | ComponentDef.t
      | ConstantNode.t
      | Constraint.t
      | DeclarativePart.t
      | ElsifExprPart.t
      | ElsifStmtPart.t
      | Expr.t
      | HandledStmts.t
      | InterfaceKind.t
      | IterType.t
      | LibraryItem.t
      | LimitedNode.t
      | LoopSpec.t
      | Mode.t
      | MultiAbstractStateDecl.t
      | NotNull.t
      | NullComponentDecl.t
      | OthersDesignator.t
      | OverridingNode.t
      | Params.t
      | ParenAbstractStateDecl.t
      | PpDirective.t
      | PpThenKw.t
      | PragmaNode.t
      | PrivateNode.t
      | ProtectedDef.t
      | ProtectedNode.t
      | Quantifier.t
      | RangeSpec.t
      | RenamingClause.t
      | ReverseNode.t
      | SelectWhenPart.t
      | Stmt.t
      | SubpKind.t
      | Subunit.t
      | SynchronizedNode.t
      | TaggedNode.t
      | TaskDef.t
      | TypeAttributesRepository.t
      | TypeDef.t
      | TypeExpr.t
      | UnconstrainedArrayIndex.t
      | UntilNode.t
      | UseClause.t
      | ValueSequence.t
      | Variant.t
      | VariantPart.t
      | WithClause.t
      | WithPrivate.t
    ]


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int

  val kind_name : [< ada_node] -> string
  (**
  * Return the kind of this node.
  *)

  val text : [< ada_node ] -> string
  (**
   * Return the source buffer slice corresponding to the text that spans
   * between the first and the last tokens of the given node.
   *)

  val image : [< ada_node ] -> string
  (**
  * Return a representation of this node as a string.
  *)

  val sloc_range : [< ada_node ] -> SlocRange.t
  (**
  * Return the spanning source location range for this node.
  *
  * Note that this returns the sloc of the parent for synthetic nodes.
  *)

  val lookup : [< ada_node ] -> Sloc.t -> ada_node option
  (**
  * Return the bottom-most node from in ``Node`` and its children which
  * contains ``Sloc``, or ``None`` if there is none.
  *)

  
  val fold_tokens : ('a -> Token.t -> 'a) -> 'a -> [< ada_node] -> 'a
  (**
   * Fold all the token this node contains by calling f on each token.
   *)

  val iter_tokens : (Token.t -> unit) -> [< ada_node]-> unit
  (**
   * Iterate over all token this node contains by calling f on each token.
   *)

  val map_tokens : (Token.t -> 'a) -> [< ada_node] -> 'a list
  (**
   * Map each token calling the given function
   *)

  val tokens : [< ada_node] -> Token.t list
  (**
   * Return a list of tokens for the given node
   *)


  val entity_image : [< ada_node ] -> string

  val children_opt : [< ada_node ] -> ada_node option list
  (**
   * Return an optional list of nodes which are the children of the given node.
   * Each child is optional because it can either be because of a syntax error,
   * or an optional field evaluated to null.
   *)

  val fold_fields :
    ('a -> ada_node -> 'a) -> 'a -> [< ada_node ] -> 'a
  (**
   * Fold all fields of the given node. This skips any child that is None
   * because of a syntax error or because the field is optional
   *)

  val iter_fields :
    (ada_node -> unit) -> [< ada_node ] -> unit
  (**
   * Iter all fields of the given node. This skips any child that is None
   * because of a syntax error or because the field is optional
   *)

  val exists_fields :
    (ada_node -> bool) -> [< ada_node ] -> bool
  (**
   * Fold all fields of the given node. Return true if the given predicate is
   * evaluated to true for at least one node.
   *)

  val for_all_fields :
    (ada_node -> bool) -> [< ada_node ] -> bool
  (**
   * Fold all fields of the given node. Return true if the given predicate is
   * evaluated to true for all nodes.
   *)

  val fold :
    ('a -> ada_node -> 'a) -> 'a -> [< ada_node ] -> 'a
  (**
   * Fold the entire AST, below the given node, and call the given function on
   * each node in prefix order.
   *)

  val iter :
    (ada_node -> unit) -> [< ada_node ] -> unit
  (**
   * Iterate over the entire AST, below the given node, and call the given
   * function on each node in prefix order.
   *)

  val filter :
    (ada_node -> bool)
    -> [< ada_node ]
    -> ada_node list
  (**
   * Fold the entire AST, below the given node, and return the list of node
   * evaluated to true by the given function
   *)

  val exists :
    (ada_node -> bool) -> [< ada_node ] -> bool
  (**
   * Fold the entire AST, below the given node, and return true if the given
   * predicate is evaluated to true by at least one node.
   *)

  val for_all :
    (ada_node -> bool) -> [< ada_node ] -> bool
  (**
   * Fold the entire AST, below the given node, and return true if the given
   * predicate is evaluated to true for all nodes.
   *)

  val lookup_with_kind :
    'a node -> [< ada_node] -> Sloc.t -> 'a option
  (**
   * Given the kind of a node, a source location and a node, return the deepest
   * node containing the source location and of the right kind. Returns None if
   * there is no match.
   *)

  val as_a : 'a node -> [< ada_node ] -> 'a option
  (**
   * Given the kind of a node, try to cast the given node to this kind. Return
   * None if the node is not of this type and thus, cannot be cast.
   *)

  val find : 'a node -> [< ada_node ] -> 'a
  (**
   * Given the kind of node, return the first node found by walking the given
   * node. The type of the resulting node depends on the desired kind
   *)


  val findall : 'a node -> [< ada_node ] -> 'a list
  (**
   * Given the kind of node, return the all nodes of this kind found by walking
   * the given node. The type of the resulting nodes depends on the desired
   * kind
   *)

  val fields_with_names :
    [< ada_node ] -> (string * ada_node option) list
  (**
   * Given a node, return the list of it's fields, together with the name of
   * the field. This function does not raise SyntaxError, but instead the
   * returned node is None.
   *)

  val pp_tree : Format.formatter -> [< ada_node] -> unit
  (**
   * Pretty print the whole tree by completely walking it.
   *)


      
  val p_declarative_scope :
    [< ada_node ]
    -> declarative_part option
  (**
  * Return the scope of definition of this basic declaration.
  *)

      
  val p_enclosing_compilation_unit :
    [< ada_node ]
    -> compilation_unit option
  (**
  * Return the compilation unit containing this node.
  *
  * .. note:: This returns the ``compilation_unit`` node, which is different
  *    from the ``AnalysisUnit``. In particular, an analysis unit can contain
  *    multiple compilation units.
  *)

      
  val p_get_uninstantiated_node :
    [< ada_node ]
    -> ada_node option
  (**
  * Assuming this node comes from an instantiated generic declaration, return
  * its non-instantiated counterpart lying in the generic declaration.
  *)

      
  val p_complete :
    [< ada_node ]
    -> unit
  (**
  * Return possible completions at this point in the file.
  *)

      
  val p_valid_keywords :
    [< ada_node ]
    -> string list
  (**
  * Return the list of keywords that are valid at this point in the file.
  *
  * .. note:: This is work in progress. It will return all keywords for now,
  *    without looking at the context.
  *)

      
  val p_generic_instantiations :
    [< ada_node ]
    -> generic_instantiation list
  (**
  * Return the potentially empty list of generic package/subprogram
  * instantiations that led to the creation of this entity. Outer-most
  * instantiations appear last.
  *)

      
  val p_semantic_parent :
    [< ada_node ]
    -> ada_node option
  (**
  * Return the semantic parent for this node, if applicable, null otherwise.
  *
  * .. note:: A node lying outside of a library item's declaration or subunit's
  *    body does not have a parent environment, meaning that this property will
  *    return null.
  *)

      
  val p_parent_basic_decl :
    [< ada_node ]
    -> basic_decl option
  (**
  * Return the parent basic decl for this node, if applicable, null otherwise.
  *
  * .. note:: If the parent BasicDecl of the given node is a generic
  *    declaration, this call will return the instantiation from which the node
  *    was retrieved instead, if any.
  *
  * .. note:: When called on a subunit's body, this property will return the
  *    its corresponding body stub.
  *
  * .. note:: When called on a node lying outside of a library item's
  *    declaration or subunit's body this property will return null.
  *)

      
  val p_filter_is_imported_by :
    [< ada_node ]
    -> analysis_unit list
    -> bool
    -> analysis_unit list
  (**
  * Filters out among the list of given units those that cannot refer to the
  * unit in which this node lies. If transitive is True, the whole transitive
  * closure of imports will be used to find a reference to the unit of this
  * node.
  *)

      
  val p_xref_entry_point :
    [< ada_node ]
    -> bool
  (**
  * Designates entities that are entry point for the xref solving
  * infrastructure. If this returns true, then resolve_names can be called on
  * it.
  *
  * .. note:: For convenience, and unlike what is defined in the ARM wrt.
  *    complete contexts for name resolution, ``xref_entry_points`` can be
  *    nested.
  *)

      
  val p_resolve_names :
    [< ada_node ]
    -> bool
  (**
  * This will resolve names for this node. If the operation is successful, then
  * type_var and ref_var will be bound on appropriate subnodes of the
  * statement.
  *)

      
  val p_standard_unit :
    [< ada_node ]
    -> analysis_unit
  (**
  * Static method. Return the analysis unit corresponding to the Standard
  * package.
  *)

      
  val p_std_entity :
    [< ada_node ]
    -> string
    -> ada_node option
  (**
  * Static property. Return an entity from the standard package with name
  * ``sym``.
  *)

      
  val p_bool_type :
    [< ada_node ]
    -> base_type_decl option
  (**
  * Static method. Return the standard Boolean type.
  *)

      
  val p_int_type :
    [< ada_node ]
    -> base_type_decl option
  (**
  * Static method. Return the standard Integer type.
  *)

      
  val p_universal_int_type :
    [< ada_node ]
    -> ada_node option
  (**
  * Static method. Return the standard Universal Integer type.
  *)

      
  val p_universal_real_type :
    [< ada_node ]
    -> ada_node option
  (**
  * Static method. Return the standard Universal Real type.
  *)

      
  val p_std_char_type :
    [< ada_node ]
    -> base_type_decl option
  (**
  * Static method. Return the standard Character type.
  *)

      
  val p_std_wide_char_type :
    [< ada_node ]
    -> base_type_decl option
  (**
  * Static method. Return the standard Wide_Character type.
  *)

      
  val p_std_wide_wide_char_type :
    [< ada_node ]
    -> base_type_decl option
  (**
  * Static method. Return the standard Wide_Wide_Character type.
  *)

      
  val p_top_level_decl :
    [< ada_node ]
    -> analysis_unit
    -> basic_decl option
  (**
  * Static method. Get the top-level decl in ``unit``.  This is the body of a
  * Subunit, or the item of a ``LibraryItem``.
  *)

      
  val p_choice_match :
    [< ada_node ]
    -> BigInteger.t
    -> bool
  (**
  * Assuming that self is a choice expression (such as what can appear in an
  * alternative of a case statement or in the RHS of a membership expression,
  * this property returns whether the given value satisfies it.
  *
  * .. attention:: This is an experimental feature, so even if it is exposed to
  *    allow experiments, it is totally unsupported and the API and behavior
  *    are very likely to change in the future.
  *)

      
  val p_gnat_xref :
    ?imprecise_fallback:
    bool
    -> [< ada_node ]
    -> defining_name option
  (**
  * Return a cross reference from this name to a defining identifier, trying to
  * mimic GNAT's xrefs as much as possible.
  *)

      
  val parent :
    [< ada_node ]
    -> ada_node option
  (**
  * Return the syntactic parent for this node. Return null for the root node.
  *)

      
  val parents :
    ?with_self:
    bool
    -> [< ada_node ]
    -> ada_node list
  (**
  * Return an array that contains the lexical parents, this node included iff
  * ``with_self`` is True. Nearer parents are first in the list.
  *)

      
  val children :
    [< ada_node ]
    -> ada_node list
  (**
  * Return an array that contains the direct lexical children.
  *
  * .. warning:: This constructs a whole array every-time you call it, and as
  *    such is less efficient than calling the ``Child`` built-in.
  *)

      
  val token_start :
    [< ada_node ]
    -> Token.t option
  (**
  * Return the first token used to parse this node.
  *)

      
  val token_end :
    [< ada_node ]
    -> Token.t option
  (**
  * Return the last token used to parse this node.
  *)

      
  val child_index :
    [< ada_node ]
    -> int
  (**
  * Return the 0-based index for Node in its parent's children.
  *)

      
  val previous_sibling :
    [< ada_node ]
    -> ada_node option
  (**
  * Return the node's previous sibling, or null if there is no such sibling.
  *)

      
  val next_sibling :
    [< ada_node ]
    -> ada_node option
  (**
  * Return the node's next sibling, or null if there is no such sibling.
  *)

      
  val unit :
    [< ada_node ]
    -> analysis_unit
  (**
  * Return the analysis unit owning this node.
  *)

      
  val is_ghost :
    [< ada_node ]
    -> bool
  (**
  * Return whether the node is a ghost.
  *
  * Unlike regular nodes, ghost nodes cover no token in the input source: they
  * are logically located instead between two tokens. Both the ``token_start``
  * and the ``token_end`` of all ghost nodes is the token right after this
  * logical position.
  *)

      
  val full_sloc_image :
    [< ada_node ]
    -> string
  (**
  * Return a string containing the filename + the sloc in GNU conformant
  * format. Useful to create diagnostics from a node.
  *)



end


