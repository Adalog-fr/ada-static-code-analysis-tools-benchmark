





(**
 * OCaml binding of the GprParser API.
 *)

(**
 * Subprograms may raise this when they cannot open a source file. Note that
 * this does *not* concern analysis unit getters, which create diagnostic
 * vectors for such errors.
 *)
exception FileReadError of string

(**
 * Raised when introspection functions (``GprParser.Introspection``) are
 * provided mismatching types/values.
 *)
exception BadTypeError of string

(**
 * Raised when introspection functions (``GprParser.Introspection``) are passed
 * an out of bounds index.
 *)
exception OutOfBoundsError of string

(**
 * Raised by lexing functions (``GprParser.Lexer``) when the input contains an
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
 * Raised by lexing functions (``GprParser.Lexer``) when the input charset is
 * not supported.
 *)
exception UnknownCharset of string





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

module GrammarRule : sig
  (**
  * Gramar rule to use for parsing.
  *)

  type t =
  | ProjectQualifierRule
  | ProjectExtensionRule
  | ProjectDeclarationRule
  | ProjectRule
  | DeclarativeItemsRule
  | DeclarativeItemRule
  | SimpleDeclarativeItemsRule
  | SimpleDeclarativeItemRule
  | VariableDeclRule
  | AttributeDeclRule
  | AssociativeArrayIndexRule
  | PackageDeclRule
  | PackageRenamingRule
  | PackageExtensionRule
  | PackageSpecRule
  | EmptyDeclarationRule
  | CaseConstructionRule
  | CaseItemRule
  | OthersDesignatorRule
  | ChoiceRule
  | DiscreteChoiceListRule
  | WithDeclRule
  | ContextClausesRule
  | AdaWithClauseRule
  | AdaContextRule
  | AdaContextItemRule
  | AdaContextSkipRule
  | AdaUseClauseRule
  | AdaPragmaRule
  | AdaSubpKindRule
  | AdaPkgKindRule
  | AdaLibraryItemRule
  | AdaPreludeRule
  | TypedStringDeclRule
  | IdentifierRule
  | StringLiteralRule
  | NumLiteralRule
  | StaticNameRule
  | AttributeReferenceRule
  | VariableReferenceRule
  | TypeReferenceRule
  | BuiltinFunctionCallRule
  | ExpressionRule
  | ExpressionListRule
  | StringLiteralAtRule
  | ProjectReferenceRule
  | TermRule
  | CompilationUnitRule

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

  

end

type analysis_context

and analysis_unit

and entity


  and entity_info = {
      rebindings :
         Rebindings.t;
      from_rebound :
         bool;
  }





  
   
  (**
    * ada_prelude_node
    * all_qualifier
    * attribute_decl
    * attribute_reference
    * base_list
    * builtin_function_call
    * case_construction
    * case_item
    * compilation_unit
    * empty_decl
    * expr
    * limited_node
    * others_designator
    * package_decl
    * package_extension
    * package_renaming
    * package_spec
    * private_node
    * project
    * project_declaration
    * project_extension
    * project_qualifier
    * project_reference
    * string_literal_at
    * terms
    * type_reference
    * typed_string_decl
    * variable_decl
    * variable_reference
    * with_decl
    *)
  and gpr_node =
    [
    | `AdaAccessSubp
        of ada_access_subp_fields
    | `AdaPragma
        of ada_pragma_fields
    | `AdaUse
        of ada_use_fields
    | `AdaWith
        of ada_with_fields
    | `AdaEntityKindFunction
        of ada_entity_kind_function_fields
    | `AdaEntityKindPackage
        of ada_entity_kind_package_fields
    | `AdaEntityKindProcedure
        of ada_entity_kind_procedure_fields
    | `AdaGeneric
        of ada_generic_fields
    | `AdaLibraryItem
        of ada_library_item_fields
    | `AdaPkg
        of ada_pkg_fields
    | `AdaPkgBody
        of ada_pkg_body_fields
    | `AdaSubp
        of ada_subp_fields
    | `AdaPrelude
        of ada_prelude_fields
    | `AdaSeparate
        of ada_separate_fields
    | `AdaSkip
        of ada_skip_fields
    | `AdaWithFormal
        of ada_with_formal_fields
    | `AllQualifierAbsent
        of all_qualifier_absent_fields
    | `AllQualifierPresent
        of all_qualifier_present_fields
    | `AttributeDecl
        of attribute_decl_fields
    | `AttributeReference
        of attribute_reference_fields
    | `AdaContextClauseList
        of ada_context_clause_list_fields
    | `AdaPreludeNodeList
        of ada_prelude_node_list_fields
    | `AdaSkipList
        of ada_skip_list_fields
    | `CaseItemList
        of case_item_list_fields
    | `ExprList
        of expr_list_fields
    | `GprNodeList
        of gpr_node_list_fields
    | `Choices
        of choices_fields
    | `TermList
        of term_list_fields
    | `IdentifierList
        of identifier_list_fields
    | `StringLiteralList
        of string_literal_list_fields
    | `TermListList
        of term_list_list_fields
    | `WithDeclList
        of with_decl_list_fields
    | `BuiltinFunctionCall
        of builtin_function_call_fields
    | `CaseConstruction
        of case_construction_fields
    | `CaseItem
        of case_item_fields
    | `CompilationUnit
        of compilation_unit_fields
    | `EmptyDecl
        of empty_decl_fields
    | `Prefix
        of prefix_fields
    | `Identifier
        of identifier_fields
    | `NumLiteral
        of num_literal_fields
    | `StringLiteral
        of string_literal_fields
    | `LimitedAbsent
        of limited_absent_fields
    | `LimitedPresent
        of limited_present_fields
    | `OthersDesignator
        of others_designator_fields
    | `PackageDecl
        of package_decl_fields
    | `PackageExtension
        of package_extension_fields
    | `PackageRenaming
        of package_renaming_fields
    | `PackageSpec
        of package_spec_fields
    | `PrivateAbsent
        of private_absent_fields
    | `PrivatePresent
        of private_present_fields
    | `Project
        of project_fields
    | `ProjectDeclaration
        of project_declaration_fields
    | `ProjectExtension
        of project_extension_fields
    | `ProjectQualifierAbstract
        of project_qualifier_abstract_fields
    | `ProjectQualifierAggregate
        of project_qualifier_aggregate_fields
    | `ProjectQualifierAggregateLibrary
        of project_qualifier_aggregate_library_fields
    | `ProjectQualifierConfiguration
        of project_qualifier_configuration_fields
    | `ProjectQualifierLibrary
        of project_qualifier_library_fields
    | `ProjectQualifierStandard
        of project_qualifier_standard_fields
    | `ProjectReference
        of project_reference_fields
    | `StringLiteralAt
        of string_literal_at_fields
    | `Terms
        of terms_fields
    | `TypeReference
        of type_reference_fields
    | `TypedStringDecl
        of typed_string_decl_fields
    | `VariableDecl
        of variable_decl_fields
    | `VariableReference
        of variable_reference_fields
    | `WithDecl
        of with_decl_fields
    ]

  
   
  (**
    * ada_access_subp
    * ada_context_clause
    * ada_entity_kind
    * ada_generic
    * ada_library_item
    * ada_main
    * ada_prelude
    * ada_separate
    * ada_skip
    * ada_with_formal
    *)
  and ada_prelude_node =
    [
    | `AdaAccessSubp
        of ada_access_subp_fields
    | `AdaPragma
        of ada_pragma_fields
    | `AdaUse
        of ada_use_fields
    | `AdaWith
        of ada_with_fields
    | `AdaEntityKindFunction
        of ada_entity_kind_function_fields
    | `AdaEntityKindPackage
        of ada_entity_kind_package_fields
    | `AdaEntityKindProcedure
        of ada_entity_kind_procedure_fields
    | `AdaGeneric
        of ada_generic_fields
    | `AdaLibraryItem
        of ada_library_item_fields
    | `AdaPkg
        of ada_pkg_fields
    | `AdaPkgBody
        of ada_pkg_body_fields
    | `AdaSubp
        of ada_subp_fields
    | `AdaPrelude
        of ada_prelude_fields
    | `AdaSeparate
        of ada_separate_fields
    | `AdaSkip
        of ada_skip_fields
    | `AdaWithFormal
        of ada_with_formal_fields
    ]

  
   
  and ada_access_subp =
    [
    | `AdaAccessSubp
        of ada_access_subp_fields
    ]
  and ada_access_subp_fields = 
  {
         
    f_subp_kind: [
      | `AdaEntityKindFunction
          of ada_entity_kind_function_fields
      | `AdaEntityKindProcedure
          of ada_entity_kind_procedure_fields
    ]
    Lazy.t;
         
    f_skips: ada_skip_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and ada_context_clause =
    [
    | `AdaPragma
        of ada_pragma_fields
    | `AdaUse
        of ada_use_fields
    | `AdaWith
        of ada_with_fields
    ]

  
   
  and ada_pragma =
    [
    | `AdaPragma
        of ada_pragma_fields
    ]
  and ada_pragma_fields = 
  {
         
    f_skips: ada_skip_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and ada_use =
    [
    | `AdaUse
        of ada_use_fields
    ]
  and ada_use_fields = 
  {
         
    f_skips: ada_skip_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and ada_with =
    [
    | `AdaWith
        of ada_with_fields
    ]
  and ada_with_fields = 
  {
         
    f_has_limited: limited_node
    Lazy.t;
         
    f_has_private: private_node
    Lazy.t;
         
    f_packages: expr_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and ada_entity_kind =
    [
    | `AdaEntityKindFunction
        of ada_entity_kind_function_fields
    | `AdaEntityKindPackage
        of ada_entity_kind_package_fields
    | `AdaEntityKindProcedure
        of ada_entity_kind_procedure_fields
    ]

  
   
  and ada_entity_kind_function =
    [
    | `AdaEntityKindFunction
        of ada_entity_kind_function_fields
    ]
  and ada_entity_kind_function_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and ada_entity_kind_package =
    [
    | `AdaEntityKindPackage
        of ada_entity_kind_package_fields
    ]
  and ada_entity_kind_package_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and ada_entity_kind_procedure =
    [
    | `AdaEntityKindProcedure
        of ada_entity_kind_procedure_fields
    ]
  and ada_entity_kind_procedure_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and ada_generic =
    [
    | `AdaGeneric
        of ada_generic_fields
    ]
  and ada_generic_fields = 
  {
         
    f_skips: ada_prelude_node_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and ada_library_item =
    [
    | `AdaLibraryItem
        of ada_library_item_fields
    ]
  and ada_library_item_fields = 
  {
         
    f_generic_stub: ada_generic
    option
    Lazy.t;
         
    f_separate: ada_separate
    option
    Lazy.t;
         
    f_main: ada_main
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and ada_main =
    [
    | `AdaPkg
        of ada_pkg_fields
    | `AdaPkgBody
        of ada_pkg_body_fields
    | `AdaSubp
        of ada_subp_fields
    ]

  
   
  and ada_pkg =
    [
    | `AdaPkg
        of ada_pkg_fields
    ]
  and ada_pkg_fields = 
  {
         
    f_has_private: private_node
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and ada_pkg_body =
    [
    | `AdaPkgBody
        of ada_pkg_body_fields
    ]
  and ada_pkg_body_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and ada_subp =
    [
    | `AdaSubp
        of ada_subp_fields
    ]
  and ada_subp_fields = 
  {
         
    f_subp_kind: [
      | `AdaEntityKindFunction
          of ada_entity_kind_function_fields
      | `AdaEntityKindProcedure
          of ada_entity_kind_procedure_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and ada_prelude =
    [
    | `AdaPrelude
        of ada_prelude_fields
    ]
  and ada_prelude_fields = 
  {
         
    f_context_clauses: ada_context_clause_list
    Lazy.t;
         
    f_library_item: ada_library_item
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and ada_separate =
    [
    | `AdaSeparate
        of ada_separate_fields
    ]
  and ada_separate_fields = 
  {
         
    f_parent_name: [
      | `Identifier
          of identifier_fields
      | `Prefix
          of prefix_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and ada_skip =
    [
    | `AdaSkip
        of ada_skip_fields
    ]
  and ada_skip_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and ada_with_formal =
    [
    | `AdaWithFormal
        of ada_with_formal_fields
    ]
  and ada_with_formal_fields = 
  {
         
    f_kind: ada_entity_kind
    Lazy.t;
         
    f_skips: ada_skip_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and all_qualifier =
    [
    | `AllQualifierAbsent
        of all_qualifier_absent_fields
    | `AllQualifierPresent
        of all_qualifier_present_fields
    ]

  
   
  and all_qualifier_absent =
    [
    | `AllQualifierAbsent
        of all_qualifier_absent_fields
    ]
  and all_qualifier_absent_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and all_qualifier_present =
    [
    | `AllQualifierPresent
        of all_qualifier_present_fields
    ]
  and all_qualifier_present_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and attribute_decl =
    [
    | `AttributeDecl
        of attribute_decl_fields
    ]
  and attribute_decl_fields = 
  {
         
    f_attr_name: identifier
    Lazy.t;
         
    f_attr_index: [
      | `OthersDesignator
          of others_designator_fields
      | `StringLiteralAt
          of string_literal_at_fields
    ]
    option
    Lazy.t;
         
    f_expr: term_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and attribute_reference =
    [
    | `AttributeReference
        of attribute_reference_fields
    ]
  and attribute_reference_fields = 
  {
         
    f_attribute_name: identifier
    Lazy.t;
         
    f_attribute_index: [
      | `OthersDesignator
          of others_designator_fields
      | `StringLiteral
          of string_literal_fields
    ]
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  (**
    * ada_context_clause_list
    * ada_prelude_node_list
    * ada_skip_list
    * case_item_list
    * expr_list
    * gpr_node_list
    * identifier_list
    * string_literal_list
    * term_list_list
    * with_decl_list
    *)
  and base_list =
    [
    | `AdaContextClauseList
        of ada_context_clause_list_fields
    | `AdaPreludeNodeList
        of ada_prelude_node_list_fields
    | `AdaSkipList
        of ada_skip_list_fields
    | `CaseItemList
        of case_item_list_fields
    | `ExprList
        of expr_list_fields
    | `GprNodeList
        of gpr_node_list_fields
    | `Choices
        of choices_fields
    | `TermList
        of term_list_fields
    | `IdentifierList
        of identifier_list_fields
    | `StringLiteralList
        of string_literal_list_fields
    | `TermListList
        of term_list_list_fields
    | `WithDeclList
        of with_decl_list_fields
    ]

  
   
  and ada_context_clause_list =
    [
    | `AdaContextClauseList
        of ada_context_clause_list_fields
    ]
  and ada_context_clause_list_fields = 
  {
    list : ada_context_clause list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and ada_prelude_node_list =
    [
    | `AdaPreludeNodeList
        of ada_prelude_node_list_fields
    ]
  and ada_prelude_node_list_fields = 
  {
    list : ada_prelude_node list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and ada_skip_list =
    [
    | `AdaSkipList
        of ada_skip_list_fields
    ]
  and ada_skip_list_fields = 
  {
    list : ada_skip list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and case_item_list =
    [
    | `CaseItemList
        of case_item_list_fields
    ]
  and case_item_list_fields = 
  {
    list : case_item list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and expr_list =
    [
    | `ExprList
        of expr_list_fields
    ]
  and expr_list_fields = 
  {
    list : expr list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and gpr_node_list =
    [
    | `GprNodeList
        of gpr_node_list_fields
    | `Choices
        of choices_fields
    | `TermList
        of term_list_fields
    ]
  and gpr_node_list_fields = 
  {
    list : gpr_node list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and choices =
    [
    | `Choices
        of choices_fields
    ]
  and choices_fields = 
  {
    list : gpr_node list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and term_list =
    [
    | `TermList
        of term_list_fields
    ]
  and term_list_fields = 
  {
    list : gpr_node list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and identifier_list =
    [
    | `IdentifierList
        of identifier_list_fields
    ]
  and identifier_list_fields = 
  {
    list : identifier list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and string_literal_list =
    [
    | `StringLiteralList
        of string_literal_list_fields
    ]
  and string_literal_list_fields = 
  {
    list : string_literal list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and term_list_list =
    [
    | `TermListList
        of term_list_list_fields
    ]
  and term_list_list_fields = 
  {
    list : term_list list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and with_decl_list =
    [
    | `WithDeclList
        of with_decl_list_fields
    ]
  and with_decl_list_fields = 
  {
    list : with_decl list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and builtin_function_call =
    [
    | `BuiltinFunctionCall
        of builtin_function_call_fields
    ]
  and builtin_function_call_fields = 
  {
         
    f_function_name: identifier
    Lazy.t;
         
    f_parameters: terms
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and case_construction =
    [
    | `CaseConstruction
        of case_construction_fields
    ]
  and case_construction_fields = 
  {
         
    f_var_ref: variable_reference
    Lazy.t;
         
    f_items: case_item_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and case_item =
    [
    | `CaseItem
        of case_item_fields
    ]
  and case_item_fields = 
  {
         
    f_choice: choices
    Lazy.t;
         
    f_decls: gpr_node_list
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
         
    f_project: project
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and empty_decl =
    [
    | `EmptyDecl
        of empty_decl_fields
    ]
  and empty_decl_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  (**
    * prefix
    * single_tok_node
    *)
  and expr =
    [
    | `Prefix
        of prefix_fields
    | `Identifier
        of identifier_fields
    | `NumLiteral
        of num_literal_fields
    | `StringLiteral
        of string_literal_fields
    ]

  
   
  and prefix =
    [
    | `Prefix
        of prefix_fields
    ]
  and prefix_fields = 
  {
         
    f_prefix: [
      | `Identifier
          of identifier_fields
      | `Prefix
          of prefix_fields
    ]
    Lazy.t;
         
    f_suffix: identifier
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and single_tok_node =
    [
    | `Identifier
        of identifier_fields
    | `NumLiteral
        of num_literal_fields
    | `StringLiteral
        of string_literal_fields
    ]

  
   
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


  
   
  and num_literal =
    [
    | `NumLiteral
        of num_literal_fields
    ]
  and num_literal_fields = 
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


  
   
  and package_decl =
    [
    | `PackageDecl
        of package_decl_fields
    ]
  and package_decl_fields = 
  {
         
    f_pkg_name: identifier
    Lazy.t;
         
    f_pkg_spec: [
      | `PackageRenaming
          of package_renaming_fields
      | `PackageSpec
          of package_spec_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and package_extension =
    [
    | `PackageExtension
        of package_extension_fields
    ]
  and package_extension_fields = 
  {
         
    f_extended_name: identifier_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and package_renaming =
    [
    | `PackageRenaming
        of package_renaming_fields
    ]
  and package_renaming_fields = 
  {
         
    f_renamed_name: identifier_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and package_spec =
    [
    | `PackageSpec
        of package_spec_fields
    ]
  and package_spec_fields = 
  {
         
    f_extension: package_extension
    option
    Lazy.t;
         
    f_decls: gpr_node_list
    Lazy.t;
         
    f_end_name: identifier
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


  
   
  and project =
    [
    | `Project
        of project_fields
    ]
  and project_fields = 
  {
         
    f_context_clauses: with_decl_list
    Lazy.t;
         
    f_project_decl: project_declaration
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and project_declaration =
    [
    | `ProjectDeclaration
        of project_declaration_fields
    ]
  and project_declaration_fields = 
  {
         
    f_qualifier: project_qualifier
    option
    Lazy.t;
         
    f_project_name: [
      | `Identifier
          of identifier_fields
      | `Prefix
          of prefix_fields
    ]
    Lazy.t;
         
    f_extension: project_extension
    option
    Lazy.t;
         
    f_decls: gpr_node_list
    Lazy.t;
         
    f_end_name: [
      | `Identifier
          of identifier_fields
      | `Prefix
          of prefix_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and project_extension =
    [
    | `ProjectExtension
        of project_extension_fields
    ]
  and project_extension_fields = 
  {
         
    f_is_all: all_qualifier
    Lazy.t;
         
    f_path_name: string_literal
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and project_qualifier =
    [
    | `ProjectQualifierAbstract
        of project_qualifier_abstract_fields
    | `ProjectQualifierAggregate
        of project_qualifier_aggregate_fields
    | `ProjectQualifierAggregateLibrary
        of project_qualifier_aggregate_library_fields
    | `ProjectQualifierConfiguration
        of project_qualifier_configuration_fields
    | `ProjectQualifierLibrary
        of project_qualifier_library_fields
    | `ProjectQualifierStandard
        of project_qualifier_standard_fields
    ]

  
   
  and project_qualifier_abstract =
    [
    | `ProjectQualifierAbstract
        of project_qualifier_abstract_fields
    ]
  and project_qualifier_abstract_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and project_qualifier_aggregate =
    [
    | `ProjectQualifierAggregate
        of project_qualifier_aggregate_fields
    ]
  and project_qualifier_aggregate_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and project_qualifier_aggregate_library =
    [
    | `ProjectQualifierAggregateLibrary
        of project_qualifier_aggregate_library_fields
    ]
  and project_qualifier_aggregate_library_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and project_qualifier_configuration =
    [
    | `ProjectQualifierConfiguration
        of project_qualifier_configuration_fields
    ]
  and project_qualifier_configuration_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and project_qualifier_library =
    [
    | `ProjectQualifierLibrary
        of project_qualifier_library_fields
    ]
  and project_qualifier_library_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and project_qualifier_standard =
    [
    | `ProjectQualifierStandard
        of project_qualifier_standard_fields
    ]
  and project_qualifier_standard_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and project_reference =
    [
    | `ProjectReference
        of project_reference_fields
    ]
  and project_reference_fields = 
  {
         
    f_attr_ref: attribute_reference
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and string_literal_at =
    [
    | `StringLiteralAt
        of string_literal_at_fields
    ]
  and string_literal_at_fields = 
  {
         
    f_str_lit: string_literal
    Lazy.t;
         
    f_at_lit: num_literal
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and terms =
    [
    | `Terms
        of terms_fields
    ]
  and terms_fields = 
  {
         
    f_terms: term_list_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and type_reference =
    [
    | `TypeReference
        of type_reference_fields
    ]
  and type_reference_fields = 
  {
         
    f_var_type_name: identifier_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and typed_string_decl =
    [
    | `TypedStringDecl
        of typed_string_decl_fields
    ]
  and typed_string_decl_fields = 
  {
         
    f_type_id: identifier
    Lazy.t;
         
    f_string_literals: string_literal_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and variable_decl =
    [
    | `VariableDecl
        of variable_decl_fields
    ]
  and variable_decl_fields = 
  {
         
    f_var_name: identifier
    Lazy.t;
         
    f_var_type: type_reference
    option
    Lazy.t;
         
    f_expr: term_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and variable_reference =
    [
    | `VariableReference
        of variable_reference_fields
    ]
  and variable_reference_fields = 
  {
         
    f_variable_name: identifier_list
    Lazy.t;
         
    f_attribute_ref: attribute_reference
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and with_decl =
    [
    | `WithDecl
        of with_decl_fields
    ]
  and with_decl_fields = 
  {
         
    f_is_limited: limited_node
    Lazy.t;
         
    f_path_names: string_literal_list
    Lazy.t;
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

  val root : t -> gpr_node option
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
  * type you need to create to use GprParser. It will contain the results of
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


type _ node =
  | GprNode :
      gpr_node node
  | AdaPreludeNode :
      ada_prelude_node node
  | AdaAccessSubp :
      ada_access_subp node
  | AdaContextClause :
      ada_context_clause node
  | AdaPragma :
      ada_pragma node
  | AdaUse :
      ada_use node
  | AdaWith :
      ada_with node
  | AdaEntityKind :
      ada_entity_kind node
  | AdaEntityKindFunction :
      ada_entity_kind_function node
  | AdaEntityKindPackage :
      ada_entity_kind_package node
  | AdaEntityKindProcedure :
      ada_entity_kind_procedure node
  | AdaGeneric :
      ada_generic node
  | AdaLibraryItem :
      ada_library_item node
  | AdaMain :
      ada_main node
  | AdaPkg :
      ada_pkg node
  | AdaPkgBody :
      ada_pkg_body node
  | AdaSubp :
      ada_subp node
  | AdaPrelude :
      ada_prelude node
  | AdaSeparate :
      ada_separate node
  | AdaSkip :
      ada_skip node
  | AdaWithFormal :
      ada_with_formal node
  | AllQualifier :
      all_qualifier node
  | AllQualifierAbsent :
      all_qualifier_absent node
  | AllQualifierPresent :
      all_qualifier_present node
  | AttributeDecl :
      attribute_decl node
  | AttributeReference :
      attribute_reference node
  | BaseList :
      base_list node
  | AdaContextClauseList :
      ada_context_clause_list node
  | AdaPreludeNodeList :
      ada_prelude_node_list node
  | AdaSkipList :
      ada_skip_list node
  | CaseItemList :
      case_item_list node
  | ExprList :
      expr_list node
  | GprNodeList :
      gpr_node_list node
  | Choices :
      choices node
  | TermList :
      term_list node
  | IdentifierList :
      identifier_list node
  | StringLiteralList :
      string_literal_list node
  | TermListList :
      term_list_list node
  | WithDeclList :
      with_decl_list node
  | BuiltinFunctionCall :
      builtin_function_call node
  | CaseConstruction :
      case_construction node
  | CaseItem :
      case_item node
  | CompilationUnit :
      compilation_unit node
  | EmptyDecl :
      empty_decl node
  | Expr :
      expr node
  | Prefix :
      prefix node
  | SingleTokNode :
      single_tok_node node
  | Identifier :
      identifier node
  | NumLiteral :
      num_literal node
  | StringLiteral :
      string_literal node
  | LimitedNode :
      limited_node node
  | LimitedAbsent :
      limited_absent node
  | LimitedPresent :
      limited_present node
  | OthersDesignator :
      others_designator node
  | PackageDecl :
      package_decl node
  | PackageExtension :
      package_extension node
  | PackageRenaming :
      package_renaming node
  | PackageSpec :
      package_spec node
  | PrivateNode :
      private_node node
  | PrivateAbsent :
      private_absent node
  | PrivatePresent :
      private_present node
  | Project :
      project node
  | ProjectDeclaration :
      project_declaration node
  | ProjectExtension :
      project_extension node
  | ProjectQualifier :
      project_qualifier node
  | ProjectQualifierAbstract :
      project_qualifier_abstract node
  | ProjectQualifierAggregate :
      project_qualifier_aggregate node
  | ProjectQualifierAggregateLibrary :
      project_qualifier_aggregate_library node
  | ProjectQualifierConfiguration :
      project_qualifier_configuration node
  | ProjectQualifierLibrary :
      project_qualifier_library node
  | ProjectQualifierStandard :
      project_qualifier_standard node
  | ProjectReference :
      project_reference node
  | StringLiteralAt :
      string_literal_at node
  | Terms :
      terms node
  | TypeReference :
      type_reference node
  | TypedStringDecl :
      typed_string_decl node
  | VariableDecl :
      variable_decl node
  | VariableReference :
      variable_reference node
  | WithDecl :
      with_decl node

module WithDecl : sig
  (**

  *)

  type t =
    [
      | `WithDecl of
          with_decl_fields
    ]

  type fields = with_decl_fields =
    
  {
         
    f_is_limited: limited_node
    Lazy.t;
         
    f_path_names: string_literal_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_is_limited :
    [< with_decl]
    -> limited_node

      
  val f_path_names :
    [< with_decl]
    -> string_literal_list


end

module VariableReference : sig
  (**

  *)

  type t =
    [
      | `VariableReference of
          variable_reference_fields
    ]

  type fields = variable_reference_fields =
    
  {
         
    f_variable_name: identifier_list
    Lazy.t;
         
    f_attribute_ref: attribute_reference
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_variable_name :
    [< variable_reference]
    -> identifier_list

      
  val f_attribute_ref :
    [< variable_reference]
    -> attribute_reference option


end

module VariableDecl : sig
  (**

  *)

  type t =
    [
      | `VariableDecl of
          variable_decl_fields
    ]

  type fields = variable_decl_fields =
    
  {
         
    f_var_name: identifier
    Lazy.t;
         
    f_var_type: type_reference
    option
    Lazy.t;
         
    f_expr: term_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_var_name :
    [< variable_decl]
    -> identifier

      
  val f_var_type :
    [< variable_decl]
    -> type_reference option

      
  val f_expr :
    [< variable_decl]
    -> term_list


end

module TypedStringDecl : sig
  (**

  *)

  type t =
    [
      | `TypedStringDecl of
          typed_string_decl_fields
    ]

  type fields = typed_string_decl_fields =
    
  {
         
    f_type_id: identifier
    Lazy.t;
         
    f_string_literals: string_literal_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_type_id :
    [< typed_string_decl]
    -> identifier

      
  val f_string_literals :
    [< typed_string_decl]
    -> string_literal_list


end

module TypeReference : sig
  (**

  *)

  type t =
    [
      | `TypeReference of
          type_reference_fields
    ]

  type fields = type_reference_fields =
    
  {
         
    f_var_type_name: identifier_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_var_type_name :
    [< type_reference]
    -> identifier_list


end

module Terms : sig
  (**

  *)

  type t =
    [
      | `Terms of
          terms_fields
    ]

  type fields = terms_fields =
    
  {
         
    f_terms: term_list_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_terms :
    [< terms]
    -> term_list_list


end

module StringLiteralAt : sig
  (**

  *)

  type t =
    [
      | `StringLiteralAt of
          string_literal_at_fields
    ]

  type fields = string_literal_at_fields =
    
  {
         
    f_str_lit: string_literal
    Lazy.t;
         
    f_at_lit: num_literal
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_str_lit :
    [< string_literal_at]
    -> string_literal

      
  val f_at_lit :
    [< string_literal_at]
    -> num_literal option


end

module ProjectReference : sig
  (**

  *)

  type t =
    [
      | `ProjectReference of
          project_reference_fields
    ]

  type fields = project_reference_fields =
    
  {
         
    f_attr_ref: attribute_reference
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_attr_ref :
    [< project_reference]
    -> attribute_reference


end

module ProjectQualifierStandard : sig
  (**

  *)

  type t =
    [
      | `ProjectQualifierStandard of
          project_qualifier_standard_fields
    ]

  type fields = project_qualifier_standard_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module ProjectQualifierLibrary : sig
  (**

  *)

  type t =
    [
      | `ProjectQualifierLibrary of
          project_qualifier_library_fields
    ]

  type fields = project_qualifier_library_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module ProjectQualifierConfiguration : sig
  (**

  *)

  type t =
    [
      | `ProjectQualifierConfiguration of
          project_qualifier_configuration_fields
    ]

  type fields = project_qualifier_configuration_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module ProjectQualifierAggregateLibrary : sig
  (**

  *)

  type t =
    [
      | `ProjectQualifierAggregateLibrary of
          project_qualifier_aggregate_library_fields
    ]

  type fields = project_qualifier_aggregate_library_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module ProjectQualifierAggregate : sig
  (**

  *)

  type t =
    [
      | `ProjectQualifierAggregate of
          project_qualifier_aggregate_fields
    ]

  type fields = project_qualifier_aggregate_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module ProjectQualifierAbstract : sig
  (**

  *)

  type t =
    [
      | `ProjectQualifierAbstract of
          project_qualifier_abstract_fields
    ]

  type fields = project_qualifier_abstract_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module ProjectQualifier : sig
  (**

  *)

  type t =
    [
      | ProjectQualifierAbstract.t
      | ProjectQualifierAggregate.t
      | ProjectQualifierAggregateLibrary.t
      | ProjectQualifierConfiguration.t
      | ProjectQualifierLibrary.t
      | ProjectQualifierStandard.t
    ]


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module ProjectExtension : sig
  (**

  *)

  type t =
    [
      | `ProjectExtension of
          project_extension_fields
    ]

  type fields = project_extension_fields =
    
  {
         
    f_is_all: all_qualifier
    Lazy.t;
         
    f_path_name: string_literal
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_is_all :
    [< project_extension]
    -> all_qualifier

      
  val f_path_name :
    [< project_extension]
    -> string_literal


end

module ProjectDeclaration : sig
  (**

  *)

  type t =
    [
      | `ProjectDeclaration of
          project_declaration_fields
    ]

  type fields = project_declaration_fields =
    
  {
         
    f_qualifier: project_qualifier
    option
    Lazy.t;
         
    f_project_name: [
      | `Identifier
          of identifier_fields
      | `Prefix
          of prefix_fields
    ]
    Lazy.t;
         
    f_extension: project_extension
    option
    Lazy.t;
         
    f_decls: gpr_node_list
    Lazy.t;
         
    f_end_name: [
      | `Identifier
          of identifier_fields
      | `Prefix
          of prefix_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_qualifier :
    [< project_declaration]
    -> project_qualifier option

      
  val f_project_name :
    [< project_declaration]
    -> [identifier | prefix]

      
  val f_extension :
    [< project_declaration]
    -> project_extension option

      
  val f_decls :
    [< project_declaration]
    -> gpr_node_list

      
  val f_end_name :
    [< project_declaration]
    -> [identifier | prefix]


end

module Project : sig
  (**

  *)

  type t =
    [
      | `Project of
          project_fields
    ]

  type fields = project_fields =
    
  {
         
    f_context_clauses: with_decl_list
    Lazy.t;
         
    f_project_decl: project_declaration
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_context_clauses :
    [< project]
    -> with_decl_list

      
  val f_project_decl :
    [< project]
    -> project_declaration


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

module PackageSpec : sig
  (**

  *)

  type t =
    [
      | `PackageSpec of
          package_spec_fields
    ]

  type fields = package_spec_fields =
    
  {
         
    f_extension: package_extension
    option
    Lazy.t;
         
    f_decls: gpr_node_list
    Lazy.t;
         
    f_end_name: identifier
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_extension :
    [< package_spec]
    -> package_extension option

      
  val f_decls :
    [< package_spec]
    -> gpr_node_list

      
  val f_end_name :
    [< package_spec]
    -> identifier


end

module PackageRenaming : sig
  (**

  *)

  type t =
    [
      | `PackageRenaming of
          package_renaming_fields
    ]

  type fields = package_renaming_fields =
    
  {
         
    f_renamed_name: identifier_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_renamed_name :
    [< package_renaming]
    -> identifier_list


end

module PackageExtension : sig
  (**

  *)

  type t =
    [
      | `PackageExtension of
          package_extension_fields
    ]

  type fields = package_extension_fields =
    
  {
         
    f_extended_name: identifier_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_extended_name :
    [< package_extension]
    -> identifier_list


end

module PackageDecl : sig
  (**

  *)

  type t =
    [
      | `PackageDecl of
          package_decl_fields
    ]

  type fields = package_decl_fields =
    
  {
         
    f_pkg_name: identifier
    Lazy.t;
         
    f_pkg_spec: [
      | `PackageRenaming
          of package_renaming_fields
      | `PackageSpec
          of package_spec_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_pkg_name :
    [< package_decl]
    -> identifier

      
  val f_pkg_spec :
    [< package_decl]
    -> [package_renaming | package_spec]


end

module OthersDesignator : sig
  (**

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

module StringLiteral : sig
  (**

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




end

module NumLiteral : sig
  (**

  *)

  type t =
    [
      | `NumLiteral of
          num_literal_fields
    ]

  type fields = num_literal_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module Identifier : sig
  (**

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

module SingleTokNode : sig
  (**

  *)

  type t =
    [
      | Identifier.t
      | NumLiteral.t
      | StringLiteral.t
    ]


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module Prefix : sig
  (**

  *)

  type t =
    [
      | `Prefix of
          prefix_fields
    ]

  type fields = prefix_fields =
    
  {
         
    f_prefix: [
      | `Identifier
          of identifier_fields
      | `Prefix
          of prefix_fields
    ]
    Lazy.t;
         
    f_suffix: identifier
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_prefix :
    [< prefix]
    -> [identifier | prefix]

      
  val f_suffix :
    [< prefix]
    -> identifier


end

module Expr : sig
  (**

  *)

  type t =
    [
      | Prefix.t
      | SingleTokNode.t
    ]


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module EmptyDecl : sig
  (**

  *)

  type t =
    [
      | `EmptyDecl of
          empty_decl_fields
    ]

  type fields = empty_decl_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module CompilationUnit : sig
  (**

  *)

  type t =
    [
      | `CompilationUnit of
          compilation_unit_fields
    ]

  type fields = compilation_unit_fields =
    
  {
         
    f_project: project
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_project :
    [< compilation_unit]
    -> project


end

module CaseItem : sig
  (**

  *)

  type t =
    [
      | `CaseItem of
          case_item_fields
    ]

  type fields = case_item_fields =
    
  {
         
    f_choice: choices
    Lazy.t;
         
    f_decls: gpr_node_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_choice :
    [< case_item]
    -> choices

      
  val f_decls :
    [< case_item]
    -> gpr_node_list


end

module CaseConstruction : sig
  (**

  *)

  type t =
    [
      | `CaseConstruction of
          case_construction_fields
    ]

  type fields = case_construction_fields =
    
  {
         
    f_var_ref: variable_reference
    Lazy.t;
         
    f_items: case_item_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_var_ref :
    [< case_construction]
    -> variable_reference

      
  val f_items :
    [< case_construction]
    -> case_item_list


end

module BuiltinFunctionCall : sig
  (**

  *)

  type t =
    [
      | `BuiltinFunctionCall of
          builtin_function_call_fields
    ]

  type fields = builtin_function_call_fields =
    
  {
         
    f_function_name: identifier
    Lazy.t;
         
    f_parameters: terms
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_function_name :
    [< builtin_function_call]
    -> identifier

      
  val f_parameters :
    [< builtin_function_call]
    -> terms


end

module WithDeclList : sig
  (**
  * List of WithDecl.
  *)

  type t =
    [
      | `WithDeclList of
          with_decl_list_fields
    ]

  type fields = with_decl_list_fields =
    
  {
    list : with_decl list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




  val f_list :
    [< with_decl_list]
    -> with_decl list

end

module TermListList : sig
  (**
  * List of TermList.
  *)

  type t =
    [
      | `TermListList of
          term_list_list_fields
    ]

  type fields = term_list_list_fields =
    
  {
    list : term_list list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




  val f_list :
    [< term_list_list]
    -> term_list list

end

module StringLiteralList : sig
  (**
  * List of StringLiteral.
  *)

  type t =
    [
      | `StringLiteralList of
          string_literal_list_fields
    ]

  type fields = string_literal_list_fields =
    
  {
    list : string_literal list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




  val f_list :
    [< string_literal_list]
    -> string_literal list

end

module IdentifierList : sig
  (**
  * List of Identifier.
  *)

  type t =
    [
      | `IdentifierList of
          identifier_list_fields
    ]

  type fields = identifier_list_fields =
    
  {
    list : identifier list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




  val f_list :
    [< identifier_list]
    -> identifier list

end

module TermList : sig
  (**
  * This list node can contain one of the following nodes:
  * ``builtin_function_call``, ``project_reference``, ``string_literal_at``,
  * ``terms``, ``variable_reference``
  *)

  type t =
    [
      | `TermList of
          term_list_fields
    ]

  type fields = term_list_fields =
    
  {
    list : gpr_node list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




  val f_list :
    [< term_list]
    -> gpr_node list

end

module Choices : sig
  (**
  * This list node can contain one of the following nodes:
  * ``others_designator``, ``string_literal``
  *)

  type t =
    [
      | `Choices of
          choices_fields
    ]

  type fields = choices_fields =
    
  {
    list : gpr_node list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




  val f_list :
    [< choices]
    -> gpr_node list

end

module GprNodeList : sig
  (**
  * List of GprNode.
  *
  * This list node can contain one of the following nodes: ``attribute_decl``,
  * ``builtin_function_call``, ``case_construction``, ``empty_decl``,
  * ``others_designator``, ``package_decl``, ``project_reference``,
  * ``string_literal_at``, ``string_literal``, ``terms``,
  * ``typed_string_decl``, ``variable_decl``, ``variable_reference``
  *)

  type t =
    [
      | `GprNodeList of
          gpr_node_list_fields
      | `Choices of
          choices_fields
      | `TermList of
          term_list_fields
    ]

  type fields = gpr_node_list_fields =
    
  {
    list : gpr_node list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




  val f_list :
    [< gpr_node_list]
    -> gpr_node list

end

module ExprList : sig
  (**
  * List of Expr.
  *
  * This list node can contain one of the following nodes: ``identifier``,
  * ``prefix``
  *)

  type t =
    [
      | `ExprList of
          expr_list_fields
    ]

  type fields = expr_list_fields =
    
  {
    list : expr list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




  val f_list :
    [< expr_list]
    -> expr list

end

module CaseItemList : sig
  (**
  * List of CaseItem.
  *)

  type t =
    [
      | `CaseItemList of
          case_item_list_fields
    ]

  type fields = case_item_list_fields =
    
  {
    list : case_item list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




  val f_list :
    [< case_item_list]
    -> case_item list

end

module AdaSkipList : sig
  (**
  * List of AdaSkip.
  *)

  type t =
    [
      | `AdaSkipList of
          ada_skip_list_fields
    ]

  type fields = ada_skip_list_fields =
    
  {
    list : ada_skip list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




  val f_list :
    [< ada_skip_list]
    -> ada_skip list

end

module AdaPreludeNodeList : sig
  (**
  * List of AdaPreludeNode.
  *
  * This list node can contain one of the following nodes: ``ada_access_subp``,
  * ``ada_skip``, ``ada_with_formal``
  *)

  type t =
    [
      | `AdaPreludeNodeList of
          ada_prelude_node_list_fields
    ]

  type fields = ada_prelude_node_list_fields =
    
  {
    list : ada_prelude_node list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




  val f_list :
    [< ada_prelude_node_list]
    -> ada_prelude_node list

end

module AdaContextClauseList : sig
  (**
  * List of AdaContextClause.
  *)

  type t =
    [
      | `AdaContextClauseList of
          ada_context_clause_list_fields
    ]

  type fields = ada_context_clause_list_fields =
    
  {
    list : ada_context_clause list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




  val f_list :
    [< ada_context_clause_list]
    -> ada_context_clause list

end

module BaseList : sig
  (**

  *)

  type t =
    [
      | AdaContextClauseList.t
      | AdaPreludeNodeList.t
      | AdaSkipList.t
      | CaseItemList.t
      | ExprList.t
      | GprNodeList.t
      | IdentifierList.t
      | StringLiteralList.t
      | TermListList.t
      | WithDeclList.t
    ]


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module AttributeReference : sig
  (**

  *)

  type t =
    [
      | `AttributeReference of
          attribute_reference_fields
    ]

  type fields = attribute_reference_fields =
    
  {
         
    f_attribute_name: identifier
    Lazy.t;
         
    f_attribute_index: [
      | `OthersDesignator
          of others_designator_fields
      | `StringLiteral
          of string_literal_fields
    ]
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_attribute_name :
    [< attribute_reference]
    -> identifier

      
  val f_attribute_index :
    [< attribute_reference]
    -> [others_designator | string_literal] option


end

module AttributeDecl : sig
  (**

  *)

  type t =
    [
      | `AttributeDecl of
          attribute_decl_fields
    ]

  type fields = attribute_decl_fields =
    
  {
         
    f_attr_name: identifier
    Lazy.t;
         
    f_attr_index: [
      | `OthersDesignator
          of others_designator_fields
      | `StringLiteralAt
          of string_literal_at_fields
    ]
    option
    Lazy.t;
         
    f_expr: term_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_attr_name :
    [< attribute_decl]
    -> identifier

      
  val f_attr_index :
    [< attribute_decl]
    -> [others_designator | string_literal_at] option

      
  val f_expr :
    [< attribute_decl]
    -> term_list


end

module AllQualifierPresent : sig
  (**

  *)

  type t =
    [
      | `AllQualifierPresent of
          all_qualifier_present_fields
    ]

  type fields = all_qualifier_present_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module AllQualifierAbsent : sig
  (**

  *)

  type t =
    [
      | `AllQualifierAbsent of
          all_qualifier_absent_fields
    ]

  type fields = all_qualifier_absent_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module AllQualifier : sig
  (**

  *)

  type t =
    [
      | AllQualifierAbsent.t
      | AllQualifierPresent.t
    ]


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int


      
  val p_as_bool :
    [< all_qualifier ]
    -> bool
  (**
  * Return whether this is an instance of AllQualifierPresent
  *)



end

module AdaWithFormal : sig
  (**

  *)

  type t =
    [
      | `AdaWithFormal of
          ada_with_formal_fields
    ]

  type fields = ada_with_formal_fields =
    
  {
         
    f_kind: ada_entity_kind
    Lazy.t;
         
    f_skips: ada_skip_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_kind :
    [< ada_with_formal]
    -> ada_entity_kind

      
  val f_skips :
    [< ada_with_formal]
    -> ada_skip_list


end

module AdaSkip : sig
  (**

  *)

  type t =
    [
      | `AdaSkip of
          ada_skip_fields
    ]

  type fields = ada_skip_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module AdaSeparate : sig
  (**

  *)

  type t =
    [
      | `AdaSeparate of
          ada_separate_fields
    ]

  type fields = ada_separate_fields =
    
  {
         
    f_parent_name: [
      | `Identifier
          of identifier_fields
      | `Prefix
          of prefix_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_parent_name :
    [< ada_separate]
    -> [identifier | prefix]


end

module AdaPrelude : sig
  (**

  *)

  type t =
    [
      | `AdaPrelude of
          ada_prelude_fields
    ]

  type fields = ada_prelude_fields =
    
  {
         
    f_context_clauses: ada_context_clause_list
    Lazy.t;
         
    f_library_item: ada_library_item
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_context_clauses :
    [< ada_prelude]
    -> ada_context_clause_list

      
  val f_library_item :
    [< ada_prelude]
    -> ada_library_item


end

module AdaSubp : sig
  (**

  *)

  type t =
    [
      | `AdaSubp of
          ada_subp_fields
    ]

  type fields = ada_subp_fields =
    
  {
         
    f_subp_kind: [
      | `AdaEntityKindFunction
          of ada_entity_kind_function_fields
      | `AdaEntityKindProcedure
          of ada_entity_kind_procedure_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_subp_kind :
    [< ada_subp]
    -> [ada_entity_kind_function | ada_entity_kind_procedure]


end

module AdaPkgBody : sig
  (**

  *)

  type t =
    [
      | `AdaPkgBody of
          ada_pkg_body_fields
    ]

  type fields = ada_pkg_body_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module AdaPkg : sig
  (**

  *)

  type t =
    [
      | `AdaPkg of
          ada_pkg_fields
    ]

  type fields = ada_pkg_fields =
    
  {
         
    f_has_private: private_node
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_has_private :
    [< ada_pkg]
    -> private_node


end

module AdaMain : sig
  (**

  *)

  type t =
    [
      | AdaPkg.t
      | AdaPkgBody.t
      | AdaSubp.t
    ]


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module AdaLibraryItem : sig
  (**

  *)

  type t =
    [
      | `AdaLibraryItem of
          ada_library_item_fields
    ]

  type fields = ada_library_item_fields =
    
  {
         
    f_generic_stub: ada_generic
    option
    Lazy.t;
         
    f_separate: ada_separate
    option
    Lazy.t;
         
    f_main: ada_main
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_generic_stub :
    [< ada_library_item]
    -> ada_generic option

      
  val f_separate :
    [< ada_library_item]
    -> ada_separate option

      
  val f_main :
    [< ada_library_item]
    -> ada_main


end

module AdaGeneric : sig
  (**

  *)

  type t =
    [
      | `AdaGeneric of
          ada_generic_fields
    ]

  type fields = ada_generic_fields =
    
  {
         
    f_skips: ada_prelude_node_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_skips :
    [< ada_generic]
    -> ada_prelude_node_list


end

module AdaEntityKindProcedure : sig
  (**

  *)

  type t =
    [
      | `AdaEntityKindProcedure of
          ada_entity_kind_procedure_fields
    ]

  type fields = ada_entity_kind_procedure_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module AdaEntityKindPackage : sig
  (**

  *)

  type t =
    [
      | `AdaEntityKindPackage of
          ada_entity_kind_package_fields
    ]

  type fields = ada_entity_kind_package_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module AdaEntityKindFunction : sig
  (**

  *)

  type t =
    [
      | `AdaEntityKindFunction of
          ada_entity_kind_function_fields
    ]

  type fields = ada_entity_kind_function_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module AdaEntityKind : sig
  (**

  *)

  type t =
    [
      | AdaEntityKindFunction.t
      | AdaEntityKindPackage.t
      | AdaEntityKindProcedure.t
    ]


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module AdaWith : sig
  (**

  *)

  type t =
    [
      | `AdaWith of
          ada_with_fields
    ]

  type fields = ada_with_fields =
    
  {
         
    f_has_limited: limited_node
    Lazy.t;
         
    f_has_private: private_node
    Lazy.t;
         
    f_packages: expr_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_has_limited :
    [< ada_with]
    -> limited_node

      
  val f_has_private :
    [< ada_with]
    -> private_node

      
  val f_packages :
    [< ada_with]
    -> expr_list


end

module AdaUse : sig
  (**

  *)

  type t =
    [
      | `AdaUse of
          ada_use_fields
    ]

  type fields = ada_use_fields =
    
  {
         
    f_skips: ada_skip_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_skips :
    [< ada_use]
    -> ada_skip_list


end

module AdaPragma : sig
  (**

  *)

  type t =
    [
      | `AdaPragma of
          ada_pragma_fields
    ]

  type fields = ada_pragma_fields =
    
  {
         
    f_skips: ada_skip_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_skips :
    [< ada_pragma]
    -> ada_skip_list


end

module AdaContextClause : sig
  (**

  *)

  type t =
    [
      | AdaPragma.t
      | AdaUse.t
      | AdaWith.t
    ]


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module AdaAccessSubp : sig
  (**

  *)

  type t =
    [
      | `AdaAccessSubp of
          ada_access_subp_fields
    ]

  type fields = ada_access_subp_fields =
    
  {
         
    f_subp_kind: [
      | `AdaEntityKindFunction
          of ada_entity_kind_function_fields
      | `AdaEntityKindProcedure
          of ada_entity_kind_procedure_fields
    ]
    Lazy.t;
         
    f_skips: ada_skip_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_subp_kind :
    [< ada_access_subp]
    -> [ada_entity_kind_function | ada_entity_kind_procedure]

      
  val f_skips :
    [< ada_access_subp]
    -> ada_skip_list


end

module AdaPreludeNode : sig
  (**

  *)

  type t =
    [
      | AdaAccessSubp.t
      | AdaContextClause.t
      | AdaEntityKind.t
      | AdaGeneric.t
      | AdaLibraryItem.t
      | AdaMain.t
      | AdaPrelude.t
      | AdaSeparate.t
      | AdaSkip.t
      | AdaWithFormal.t
    ]


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module GprNode : sig
  (**

  *)

  type t =
    [
      | AdaPreludeNode.t
      | AllQualifier.t
      | AttributeDecl.t
      | AttributeReference.t
      | BaseList.t
      | BuiltinFunctionCall.t
      | CaseConstruction.t
      | CaseItem.t
      | CompilationUnit.t
      | EmptyDecl.t
      | Expr.t
      | LimitedNode.t
      | OthersDesignator.t
      | PackageDecl.t
      | PackageExtension.t
      | PackageRenaming.t
      | PackageSpec.t
      | PrivateNode.t
      | Project.t
      | ProjectDeclaration.t
      | ProjectExtension.t
      | ProjectQualifier.t
      | ProjectReference.t
      | StringLiteralAt.t
      | Terms.t
      | TypeReference.t
      | TypedStringDecl.t
      | VariableDecl.t
      | VariableReference.t
      | WithDecl.t
    ]


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int

  val kind_name : [< gpr_node] -> string
  (**
  * Return the kind of this node.
  *)

  val text : [< gpr_node ] -> string
  (**
   * Return the source buffer slice corresponding to the text that spans
   * between the first and the last tokens of the given node.
   *)

  val image : [< gpr_node ] -> string
  (**
  * Return a representation of this node as a string.
  *)

  val sloc_range : [< gpr_node ] -> SlocRange.t
  (**
  * Return the spanning source location range for this node.
  *
  * Note that this returns the sloc of the parent for synthetic nodes.
  *)

  val lookup : [< gpr_node ] -> Sloc.t -> gpr_node option
  (**
  * Return the bottom-most node from in ``Node`` and its children which
  * contains ``Sloc``, or ``None`` if there is none.
  *)

  
  val fold_tokens : ('a -> Token.t -> 'a) -> 'a -> [< gpr_node] -> 'a
  (**
   * Fold all the token this node contains by calling f on each token.
   *)

  val iter_tokens : (Token.t -> unit) -> [< gpr_node]-> unit
  (**
   * Iterate over all token this node contains by calling f on each token.
   *)

  val map_tokens : (Token.t -> 'a) -> [< gpr_node] -> 'a list
  (**
   * Map each token calling the given function
   *)

  val tokens : [< gpr_node] -> Token.t list
  (**
   * Return a list of tokens for the given node
   *)


  val entity_image : [< gpr_node ] -> string

  val children_opt : [< gpr_node ] -> gpr_node option list
  (**
   * Return an optional list of nodes which are the children of the given node.
   * Each child is optional because it can either be because of a syntax error,
   * or an optional field evaluated to null.
   *)

  val fold_fields :
    ('a -> gpr_node -> 'a) -> 'a -> [< gpr_node ] -> 'a
  (**
   * Fold all fields of the given node. This skips any child that is None
   * because of a syntax error or because the field is optional
   *)

  val iter_fields :
    (gpr_node -> unit) -> [< gpr_node ] -> unit
  (**
   * Iter all fields of the given node. This skips any child that is None
   * because of a syntax error or because the field is optional
   *)

  val exists_fields :
    (gpr_node -> bool) -> [< gpr_node ] -> bool
  (**
   * Fold all fields of the given node. Return true if the given predicate is
   * evaluated to true for at least one node.
   *)

  val for_all_fields :
    (gpr_node -> bool) -> [< gpr_node ] -> bool
  (**
   * Fold all fields of the given node. Return true if the given predicate is
   * evaluated to true for all nodes.
   *)

  val fold :
    ('a -> gpr_node -> 'a) -> 'a -> [< gpr_node ] -> 'a
  (**
   * Fold the entire AST, below the given node, and call the given function on
   * each node in prefix order.
   *)

  val iter :
    (gpr_node -> unit) -> [< gpr_node ] -> unit
  (**
   * Iterate over the entire AST, below the given node, and call the given
   * function on each node in prefix order.
   *)

  val filter :
    (gpr_node -> bool)
    -> [< gpr_node ]
    -> gpr_node list
  (**
   * Fold the entire AST, below the given node, and return the list of node
   * evaluated to true by the given function
   *)

  val exists :
    (gpr_node -> bool) -> [< gpr_node ] -> bool
  (**
   * Fold the entire AST, below the given node, and return true if the given
   * predicate is evaluated to true by at least one node.
   *)

  val for_all :
    (gpr_node -> bool) -> [< gpr_node ] -> bool
  (**
   * Fold the entire AST, below the given node, and return true if the given
   * predicate is evaluated to true for all nodes.
   *)

  val lookup_with_kind :
    'a node -> [< gpr_node] -> Sloc.t -> 'a option
  (**
   * Given the kind of a node, a source location and a node, return the deepest
   * node containing the source location and of the right kind. Returns None if
   * there is no match.
   *)

  val as_a : 'a node -> [< gpr_node ] -> 'a option
  (**
   * Given the kind of a node, try to cast the given node to this kind. Return
   * None if the node is not of this type and thus, cannot be cast.
   *)

  val find : 'a node -> [< gpr_node ] -> 'a
  (**
   * Given the kind of node, return the first node found by walking the given
   * node. The type of the resulting node depends on the desired kind
   *)


  val findall : 'a node -> [< gpr_node ] -> 'a list
  (**
   * Given the kind of node, return the all nodes of this kind found by walking
   * the given node. The type of the resulting nodes depends on the desired
   * kind
   *)

  val fields_with_names :
    [< gpr_node ] -> (string * gpr_node option) list
  (**
   * Given a node, return the list of it's fields, together with the name of
   * the field. This function does not raise SyntaxError, but instead the
   * returned node is None.
   *)

  val pp_tree : Format.formatter -> [< gpr_node] -> unit
  (**
   * Pretty print the whole tree by completely walking it.
   *)


      
  val parent :
    [< gpr_node ]
    -> gpr_node option
  (**
  * Return the syntactic parent for this node. Return null for the root node.
  *)

      
  val parents :
    ?with_self:
    bool
    -> [< gpr_node ]
    -> gpr_node list
  (**
  * Return an array that contains the lexical parents, this node included iff
  * ``with_self`` is True. Nearer parents are first in the list.
  *)

      
  val children :
    [< gpr_node ]
    -> gpr_node list
  (**
  * Return an array that contains the direct lexical children.
  *
  * .. warning:: This constructs a whole array every-time you call it, and as
  *    such is less efficient than calling the ``Child`` built-in.
  *)

      
  val token_start :
    [< gpr_node ]
    -> Token.t option
  (**
  * Return the first token used to parse this node.
  *)

      
  val token_end :
    [< gpr_node ]
    -> Token.t option
  (**
  * Return the last token used to parse this node.
  *)

      
  val child_index :
    [< gpr_node ]
    -> int
  (**
  * Return the 0-based index for Node in its parent's children.
  *)

      
  val previous_sibling :
    [< gpr_node ]
    -> gpr_node option
  (**
  * Return the node's previous sibling, or null if there is no such sibling.
  *)

      
  val next_sibling :
    [< gpr_node ]
    -> gpr_node option
  (**
  * Return the node's next sibling, or null if there is no such sibling.
  *)

      
  val unit :
    [< gpr_node ]
    -> analysis_unit
  (**
  * Return the analysis unit owning this node.
  *)

      
  val is_ghost :
    [< gpr_node ]
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
    [< gpr_node ]
    -> string
  (**
  * Return a string containing the filename + the sloc in GNU conformant
  * format. Useful to create diagnostics from a node.
  *)



end


