








#ifndef GPR_PARSER
#define GPR_PARSER

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
 * This type represents a context for all source analysis. This is the first
 * type you need to create to use gpr_parser. It will contain the results of
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
} *gpr_analysis_context;

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
} *gpr_analysis_unit;

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
typedef void* gpr_base_node;

/*
 * Kind of AST nodes in parse trees.
 */
typedef enum {
    

        /* gpr_node (abstract)  */
        /*

         */
    

        /* ada_prelude_node (abstract)  */
        /*

         */
    

        /*

         */
        gpr_ada_access_subp = 1,
    

        /* ada_context_clause (abstract)  */
        /*

         */
    

        /*

         */
        gpr_ada_pragma = 2,
    

        /*

         */
        gpr_ada_use = 3,
    

        /*

         */
        gpr_ada_with = 4,
    

        /* ada_entity_kind (abstract)  */
        /*

         */
    

        /*

         */
        gpr_ada_entity_kind_function = 5,
    

        /*

         */
        gpr_ada_entity_kind_package = 6,
    

        /*

         */
        gpr_ada_entity_kind_procedure = 7,
    

        /*

         */
        gpr_ada_generic = 8,
    

        /*

         */
        gpr_ada_library_item = 9,
    

        /* ada_main (abstract)  */
        /*

         */
    

        /*

         */
        gpr_ada_pkg = 10,
    

        /*

         */
        gpr_ada_pkg_body = 11,
    

        /*

         */
        gpr_ada_subp = 12,
    

        /*

         */
        gpr_ada_prelude = 13,
    

        /*

         */
        gpr_ada_separate = 14,
    

        /*

         */
        gpr_ada_skip = 15,
    

        /*

         */
        gpr_ada_with_formal = 16,
    

        /* all_qualifier (abstract)  */
        /*

         */
    

        /*

         */
        gpr_all_qualifier_absent = 17,
    

        /*

         */
        gpr_all_qualifier_present = 18,
    

        /*

         */
        gpr_attribute_decl = 19,
    

        /*

         */
        gpr_attribute_reference = 20,
    

        /* base_list (abstract)  */
        /*

         */
    

        /*
         * List of AdaContextClause.
         */
        gpr_ada_context_clause_list = 21,
    

        /*
         * List of AdaPreludeNode.
         *
         * This list node can contain one of the following nodes:
         * ``gpr_ada_access_subp``, ``gpr_ada_skip``, ``gpr_ada_with_formal``
         */
        gpr_ada_prelude_node_list = 22,
    

        /*
         * List of AdaSkip.
         */
        gpr_ada_skip_list = 23,
    

        /*
         * List of CaseItem.
         */
        gpr_case_item_list = 24,
    

        /*
         * List of Expr.
         *
         * This list node can contain one of the following nodes:
         * ``gpr_identifier``, ``gpr_prefix``
         */
        gpr_expr_list = 25,
    

        /*
         * List of GprNode.
         *
         * This list node can contain one of the following nodes:
         * ``gpr_attribute_decl``, ``gpr_builtin_function_call``,
         * ``gpr_case_construction``, ``gpr_empty_decl``,
         * ``gpr_others_designator``, ``gpr_package_decl``,
         * ``gpr_project_reference``, ``gpr_string_literal_at``,
         * ``gpr_string_literal``, ``gpr_terms``, ``gpr_typed_string_decl``,
         * ``gpr_variable_decl``, ``gpr_variable_reference``
         */
        gpr_gpr_node_list = 26,
    

        /*
         * This list node can contain one of the following nodes:
         * ``gpr_others_designator``, ``gpr_string_literal``
         */
        gpr_choices = 27,
    

        /*
         * This list node can contain one of the following nodes:
         * ``gpr_builtin_function_call``, ``gpr_project_reference``,
         * ``gpr_string_literal_at``, ``gpr_terms``, ``gpr_variable_reference``
         */
        gpr_term_list = 28,
    

        /*
         * List of Identifier.
         */
        gpr_identifier_list = 29,
    

        /*
         * List of StringLiteral.
         */
        gpr_string_literal_list = 30,
    

        /*
         * List of TermList.
         */
        gpr_term_list_list = 31,
    

        /*
         * List of WithDecl.
         */
        gpr_with_decl_list = 32,
    

        /*

         */
        gpr_builtin_function_call = 33,
    

        /*

         */
        gpr_case_construction = 34,
    

        /*

         */
        gpr_case_item = 35,
    

        /*

         */
        gpr_compilation_unit = 36,
    

        /*

         */
        gpr_empty_decl = 37,
    

        /* expr (abstract)  */
        /*

         */
    

        /*

         */
        gpr_prefix = 38,
    

        /* single_tok_node (abstract)  */
        /*

         */
    

        /*

         */
        gpr_identifier = 39,
    

        /*

         */
        gpr_num_literal = 40,
    

        /*

         */
        gpr_string_literal = 41,
    

        /* limited_node (abstract)  */
        /*

         */
    

        /*

         */
        gpr_limited_absent = 42,
    

        /*

         */
        gpr_limited_present = 43,
    

        /*

         */
        gpr_others_designator = 44,
    

        /*

         */
        gpr_package_decl = 45,
    

        /*

         */
        gpr_package_extension = 46,
    

        /*

         */
        gpr_package_renaming = 47,
    

        /*

         */
        gpr_package_spec = 48,
    

        /* private_node (abstract)  */
        /*

         */
    

        /*

         */
        gpr_private_absent = 49,
    

        /*

         */
        gpr_private_present = 50,
    

        /*

         */
        gpr_project = 51,
    

        /*

         */
        gpr_project_declaration = 52,
    

        /*

         */
        gpr_project_extension = 53,
    

        /* project_qualifier (abstract)  */
        /*

         */
    

        /*

         */
        gpr_project_qualifier_abstract = 54,
    

        /*

         */
        gpr_project_qualifier_aggregate = 55,
    

        /*

         */
        gpr_project_qualifier_aggregate_library = 56,
    

        /*

         */
        gpr_project_qualifier_configuration = 57,
    

        /*

         */
        gpr_project_qualifier_library = 58,
    

        /*

         */
        gpr_project_qualifier_standard = 59,
    

        /*

         */
        gpr_project_reference = 60,
    

        /*

         */
        gpr_string_literal_at = 61,
    

        /*

         */
        gpr_terms = 62,
    

        /*

         */
        gpr_type_reference = 63,
    

        /*

         */
        gpr_typed_string_decl = 64,
    

        /*

         */
        gpr_variable_decl = 65,
    

        /*

         */
        gpr_variable_reference = 66,
    

        /*

         */
        gpr_with_decl = 67,
} gpr_node_kind_enum;

/*
 * Reference to a symbol. Symbols are owned by analysis contexts, so they must
 * not outlive them. This type exists only in the C API, and roughly wraps the
 * corresponding Ada type (an array fat pointer).
 */
typedef struct {
   void *data;
   void *bounds;
} gpr_symbol_type;

/*
 * Type to contain Unicode text data.
 */
typedef struct {
   int length;
   int ref_count;
   uint32_t content[1];
} *gpr_string_type;

/*
 * Data type for env rebindings. For internal use only.
 */
typedef void *gpr_env_rebindings_type;

typedef uint8_t gpr_bool;

/* Helper data structures for source location handling.  */

/*
 * Location in a source file. Line and column numbers are one-based.
 */
typedef struct {
    uint32_t line;
    uint16_t column;
} gpr_source_location;

/*
 * Location of a span of text in a source file.
 */
typedef struct {
    gpr_source_location start;
    gpr_source_location end;
} gpr_source_location_range;


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
} gpr_text;

/*
 * Arbitrarily large integer.
 */
typedef void *gpr_big_integer;

/*
 * Kind for this token.
 */
typedef enum {
   
      
      GPR_TERMINATION = 0
      ,
      GPR_LEXING_FAILURE = 1
      ,
      GPR_IDENTIFIER = 2
      ,
      GPR_ALL = 3
      ,
      GPR_ABSTRACT = 4
      ,
      GPR_AT = 5
      ,
      GPR_CASE = 6
      ,
      GPR_END = 7
      ,
      GPR_FOR = 8
      ,
      GPR_IS = 9
      ,
      GPR_LIMITED = 10
      ,
      GPR_PRIVATE = 11
      ,
      GPR_NULL = 12
      ,
      GPR_OTHERS = 13
      ,
      GPR_PACKAGE = 14
      ,
      GPR_RENAMES = 15
      ,
      GPR_TYPE = 16
      ,
      GPR_USE = 17
      ,
      GPR_PRAGMA = 18
      ,
      GPR_WHEN = 19
      ,
      GPR_WITH = 20
      ,
      GPR_EXTENDS = 21
      ,
      GPR_PAR_OPEN = 22
      ,
      GPR_PAR_CLOSE = 23
      ,
      GPR_SEMICOLON = 24
      ,
      GPR_COLON = 25
      ,
      GPR_COMMA = 26
      ,
      GPR_DOT = 27
      ,
      GPR_AMP = 28
      ,
      GPR_TICK = 29
      ,
      GPR_PIPE = 30
      ,
      GPR_ASSIGN = 31
      ,
      GPR_ARROW = 32
      ,
      GPR_STRING = 33
      ,
      GPR_NUMBER = 34
      ,
      GPR_LABEL = 35
      ,
      GPR_CHAR = 36
      ,
      GPR_COMMENT = 37
      ,
      GPR_WHITESPACE = 38
} gpr_token_kind;

typedef struct
{
   uint64_t version;
} *gpr_token_data_handler;

/*
 * Reference to a token in an analysis unit.
 */
typedef struct {
    /* Private data associated to this token, including stale reference
       checking data, or NULL if this designates no token.  */
    gpr_analysis_context context;
    gpr_token_data_handler token_data;

    /* Internal identifiers for this token.  */
    int token_index, trivia_index;

    gpr_token_kind kind;
    gpr_text text;
    gpr_source_location_range sloc_range;
} gpr_token;


/*
 * Diagnostic for an analysis unit: cannot open the source file, parsing error,
 * ...
 */
typedef struct {
    gpr_source_location_range sloc_range;
    gpr_text message;
} gpr_diagnostic;

   typedef enum {
      GPR_ANALYSIS_UNIT_KIND_UNIT_SPECIFICATION, GPR_ANALYSIS_UNIT_KIND_UNIT_BODY
   } gpr_analysis_unit_kind;
   /*
    * Specify a kind of analysis unit. Specification units provide an interface
    * to the outer world while body units provide an implementation for the
    * corresponding interface.
    */
   typedef enum {
      GPR_LOOKUP_KIND_RECURSIVE, GPR_LOOKUP_KIND_FLAT, GPR_LOOKUP_KIND_MINIMAL
   } gpr_lookup_kind;
   /*

    */
   typedef enum {
      GPR_DESIGNATED_ENV_KIND_NONE, GPR_DESIGNATED_ENV_KIND_CURRENT_ENV, GPR_DESIGNATED_ENV_KIND_NAMED_ENV, GPR_DESIGNATED_ENV_KIND_DIRECT_ENV
   } gpr_designated_env_kind;
   /*
    * Discriminant for DesignatedEnv structures.
    */
   typedef enum {
      GPR_GRAMMAR_RULE_PROJECT_QUALIFIER_RULE, GPR_GRAMMAR_RULE_PROJECT_EXTENSION_RULE, GPR_GRAMMAR_RULE_PROJECT_DECLARATION_RULE, GPR_GRAMMAR_RULE_PROJECT_RULE, GPR_GRAMMAR_RULE_DECLARATIVE_ITEMS_RULE, GPR_GRAMMAR_RULE_DECLARATIVE_ITEM_RULE, GPR_GRAMMAR_RULE_SIMPLE_DECLARATIVE_ITEMS_RULE, GPR_GRAMMAR_RULE_SIMPLE_DECLARATIVE_ITEM_RULE, GPR_GRAMMAR_RULE_VARIABLE_DECL_RULE, GPR_GRAMMAR_RULE_ATTRIBUTE_DECL_RULE, GPR_GRAMMAR_RULE_ASSOCIATIVE_ARRAY_INDEX_RULE, GPR_GRAMMAR_RULE_PACKAGE_DECL_RULE, GPR_GRAMMAR_RULE_PACKAGE_RENAMING_RULE, GPR_GRAMMAR_RULE_PACKAGE_EXTENSION_RULE, GPR_GRAMMAR_RULE_PACKAGE_SPEC_RULE, GPR_GRAMMAR_RULE_EMPTY_DECLARATION_RULE, GPR_GRAMMAR_RULE_CASE_CONSTRUCTION_RULE, GPR_GRAMMAR_RULE_CASE_ITEM_RULE, GPR_GRAMMAR_RULE_OTHERS_DESIGNATOR_RULE, GPR_GRAMMAR_RULE_CHOICE_RULE, GPR_GRAMMAR_RULE_DISCRETE_CHOICE_LIST_RULE, GPR_GRAMMAR_RULE_WITH_DECL_RULE, GPR_GRAMMAR_RULE_CONTEXT_CLAUSES_RULE, GPR_GRAMMAR_RULE_ADA_WITH_CLAUSE_RULE, GPR_GRAMMAR_RULE_ADA_CONTEXT_RULE, GPR_GRAMMAR_RULE_ADA_CONTEXT_ITEM_RULE, GPR_GRAMMAR_RULE_ADA_CONTEXT_SKIP_RULE, GPR_GRAMMAR_RULE_ADA_USE_CLAUSE_RULE, GPR_GRAMMAR_RULE_ADA_PRAGMA_RULE, GPR_GRAMMAR_RULE_ADA_SUBP_KIND_RULE, GPR_GRAMMAR_RULE_ADA_PKG_KIND_RULE, GPR_GRAMMAR_RULE_ADA_LIBRARY_ITEM_RULE, GPR_GRAMMAR_RULE_ADA_PRELUDE_RULE, GPR_GRAMMAR_RULE_TYPED_STRING_DECL_RULE, GPR_GRAMMAR_RULE_IDENTIFIER_RULE, GPR_GRAMMAR_RULE_STRING_LITERAL_RULE, GPR_GRAMMAR_RULE_NUM_LITERAL_RULE, GPR_GRAMMAR_RULE_STATIC_NAME_RULE, GPR_GRAMMAR_RULE_ATTRIBUTE_REFERENCE_RULE, GPR_GRAMMAR_RULE_VARIABLE_REFERENCE_RULE, GPR_GRAMMAR_RULE_TYPE_REFERENCE_RULE, GPR_GRAMMAR_RULE_BUILTIN_FUNCTION_CALL_RULE, GPR_GRAMMAR_RULE_EXPRESSION_RULE, GPR_GRAMMAR_RULE_EXPRESSION_LIST_RULE, GPR_GRAMMAR_RULE_STRING_LITERAL_AT_RULE, GPR_GRAMMAR_RULE_PROJECT_REFERENCE_RULE, GPR_GRAMMAR_RULE_TERM_RULE, GPR_GRAMMAR_RULE_COMPILATION_UNIT_RULE
   } gpr_grammar_rule;
   /*
    * Gramar rule to use for parsing.
    */

const gpr_grammar_rule gpr_default_grammar_rule = GPR_GRAMMAR_RULE_COMPILATION_UNIT_RULE;

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
} gpr_exception_kind;

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
   gpr_exception_kind kind;

   /*
 * Message and context information associated with this exception.
 */
   const char *information;
} gpr_exception;

/*
 * Array types incomplete declarations
 */

        

typedef struct gpr_gpr_node_array_record *gpr_gpr_node_array;


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



typedef void* gpr_gpr_node_iterator;



/*
 * Struct types declarations
 */

        



typedef struct {
} gpr_internal_metadata;



        



typedef struct {
        gpr_internal_metadata md;
        gpr_env_rebindings_type rebindings;
        gpr_bool from_rebound;
} gpr_internal_entity_info;



        



typedef struct {
        gpr_base_node node;
        gpr_internal_entity_info info;
} gpr_base_entity;




/*
 * Types for event handler
 */

/*
 * Interface to handle events sent by the analysis context.
 */
typedef void *gpr_event_handler;

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
typedef void (*gpr_event_handler_unit_requested_callback)(
   void *data,
   gpr_analysis_context context,
   gpr_text *name,
   gpr_analysis_unit from,
   gpr_bool found,
   gpr_bool is_not_found_error
);

/*
 * Callback type for functions that are called when destroying an event
 * handler.
 */
typedef void (*gpr_event_handler_destroy_callback)(void *data);

/*
 * Callback type for functions that are called when a unit is parsed.
 *
 * ``unit`` is the resulting unit.
 *
 * ``reparsed`` indicates whether the unit was reparsed, or whether it was the
 * first parse.
 */
typedef void (*gpr_event_handler_unit_parsed_callback)(
   void *data,
   gpr_analysis_context context,
   gpr_analysis_unit unit,
   gpr_bool reparsed
);

/*
 * Types for file readers
 */

/*
 * Interface to override how source files are fetched and decoded.
 */
typedef void *gpr_file_reader;

/*
 * Callback type for functions that are called when destroying a file reader.
 */
typedef void (*gpr_file_reader_destroy_callback)(void *data);

/*
 * Callback type for functions that are called to fetch the decoded source
 * buffer for a requested filename.
 */
typedef void (*gpr_file_reader_read_callback)(
   void *data,
   const char *filename,
   const char *charset,
   int read_bom,
   gpr_text *buffer,
   gpr_diagnostic *diagnostic
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
typedef void *gpr_unit_provider;

/*
 * Callback type for functions that are called when destroying a unit file
 * provider type.
 */
typedef void (*gpr_unit_provider_destroy_callback)(void *data);

/*
 * Callback type for functions that are called to turn a unit reference encoded
 * as a unit name into an analysis unit.
 */
typedef char *(*gpr_unit_provider_get_unit_filename_callback)(
   void *data,
   gpr_text *name,
   gpr_analysis_unit_kind kind
);

/*
 * Callback type for functions that are called to turn a unit reference encoded
 * as a unit name into an analysis unit.
 */
typedef gpr_analysis_unit (*gpr_unit_provider_get_unit_from_name_callback)(
   void *data,
   gpr_analysis_context context,
   gpr_text *name,
   gpr_analysis_unit_kind kind,
   const char *charset,
   int reparse
);

/* All the functions below can potentially raise an exception, so
   gpr_get_last_exception must be checked after them even
   before trying to use the returned value.  */


/*
 * Array types declarations
 */

        



/*

 */
struct gpr_gpr_node_array_record {
   int n;
   int ref_count;
   gpr_base_entity items[1];
};

/* Create a length-sized array.  */
extern gpr_gpr_node_array
gpr_gpr_node_array_create(int length);

/* Increment the ref-count for "a".  */
extern void
gpr_gpr_node_array_inc_ref(gpr_gpr_node_array a);

/* Decrement the ref-count for "a". This deallocates it if the ref-count drops
   to 0.  */
extern void
gpr_gpr_node_array_dec_ref(gpr_gpr_node_array a);



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
extern gpr_analysis_context
gpr_create_analysis_context(
   const char *charset,
   gpr_file_reader file_reader,
   gpr_unit_provider unit_provider,
   gpr_event_handler event_handler,
   int with_trivia,
   int tab_stop
);

/*
 * Increase the reference count to an analysis context. Return the reference
 * for convenience.
 */
extern gpr_analysis_context
gpr_context_incref(gpr_analysis_context context);

/*
 * Decrease the reference count to an analysis context. Destruction happens
 * when the ref-count reaches 0.
 */
extern void
gpr_context_decref(gpr_analysis_context context);

/*
 * If the given string is a valid symbol, yield it as a symbol and return true.
 * Otherwise, return false.
 */
extern int
gpr_context_symbol(gpr_analysis_context context,
                                   gpr_text *text,
                                   gpr_symbol_type *symbol);

/*
 * Debug helper. Set whether ``Property_Error`` exceptions raised in
 * ``Populate_Lexical_Env`` should be discarded. They are by default.
 */
extern void
gpr_context_discard_errors_in_populate_lexical_env(
        gpr_analysis_context context,
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
extern gpr_analysis_unit
gpr_get_analysis_unit_from_file(
        gpr_analysis_context context,
        const char *filename,
        const char *charset,
        int reparse,
        gpr_grammar_rule rule);

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
extern gpr_analysis_unit
gpr_get_analysis_unit_from_buffer(
        gpr_analysis_context context,
        const char *filename,
        const char *charset,
        const char *buffer,
        size_t buffer_size,
        gpr_grammar_rule rule);


/*
 * Return the root node for this unit, or ``NULL`` if there is none.
 */
extern void
gpr_unit_root(gpr_analysis_unit unit,
                              gpr_base_entity *result_p);

/*
 * Return a reference to the first token scanned in this unit.
 */
extern void
gpr_unit_first_token(gpr_analysis_unit unit,
                                     gpr_token *token);

/*
 * Return a reference to the last token scanned in this unit.
 */
extern void
gpr_unit_last_token(gpr_analysis_unit unit,
                                    gpr_token *token);

/*
 * Return the number of tokens in this unit.
 */
extern int
gpr_unit_token_count(gpr_analysis_unit unit);

/*
 * Return the number of trivias in this unit. This is 0 for units that were
 * parsed with trivia analysis disabled.
 */
extern int
gpr_unit_trivia_count(gpr_analysis_unit unit);

/*
 * Debug helper: output the lexical envs for the given analysis unit.
 */
extern void
gpr_unit_dump_lexical_env(gpr_analysis_unit unit);

/*
 * Return the filename this unit is associated to.
 *
 * The returned string is dynamically allocated and the caller must free it
 * when done with it.
 */
extern char *
gpr_unit_filename(gpr_analysis_unit unit);

/*
 * Return the number of diagnostics associated to this unit.
 */
extern unsigned
gpr_unit_diagnostic_count(gpr_analysis_unit unit);

/*
 * Get the Nth diagnostic in this unit and store it into ``*diagnostic_p``.
 * Return zero on failure (when N is too big).
 */
extern int
gpr_unit_diagnostic(gpr_analysis_unit unit,
                                    unsigned n,
                                    gpr_diagnostic *diagnostic_p);

/*
 * Return the context that owns this unit.
 */
extern gpr_analysis_context
gpr_unit_context(gpr_analysis_unit context);

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
gpr_unit_reparse_from_file(gpr_analysis_unit unit,
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
gpr_unit_reparse_from_buffer (gpr_analysis_unit unit,
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
gpr_unit_populate_lexical_env(gpr_analysis_unit unit);

/*
 * General AST node primitives
 */

/*
 * Return whether this node is a null node reference.
 */
static inline int
gpr_node_is_null(gpr_base_entity *node) {
    return node->node == NULL;
}

/*
 * Return the kind of this node.
 */
extern gpr_node_kind_enum
gpr_node_kind(gpr_base_entity *node);

/*
 * Helper for textual dump: return the kind name for this node. The returned
 * string is a copy and thus must be free'd by the caller.
 */
extern void
gpr_kind_name(gpr_node_kind_enum kind, gpr_text *result);

/*
 * Return the analysis unit that owns this node.
 */
extern int
gpr_node_unit(gpr_base_entity *node,
                              gpr_analysis_unit *unit_p);

/*
 * Return whether this node is a node that contains only a single token.
 */
extern int
gpr_node_is_token_node(gpr_base_entity *node);

/*
 * Return whether this node is synthetic.
 */
extern int
gpr_node_is_synthetic(gpr_base_entity *node);

/*
 * Return a representation of this node as a string.
 */
extern void
gpr_node_image(gpr_base_entity *node,
                               gpr_text *result);

/*
 * Return the source buffer slice corresponding to the text that spans between
 * the first and the last tokens of this node.
 *
 * Note that this returns the empty string for synthetic nodes.
 */
extern void
gpr_node_text(gpr_base_entity *node,
                              gpr_text *text);

/*
 * Return the spanning source location range for this node.
 *
 * Note that this returns the sloc of the parent for synthetic nodes.
 */
extern void
gpr_node_sloc_range(gpr_base_entity *node,
                                    gpr_source_location_range *sloc_range);

/*
 * Return the bottom-most node from in ``Node`` and its children which contains
 * ``Sloc``, or ``NULL`` if there is none.
 */
extern void
gpr_lookup_in_node(gpr_base_entity *node,
                                   const gpr_source_location *sloc,
                                   gpr_base_entity *result_p);

/*
 * Return the number of children in this node.
 */
extern unsigned
gpr_node_children_count(gpr_base_entity *node);

/*
 * Return the Nth child for in this node's fields and store it into
 * ``*child_p``.  Return zero on failure (when ``N`` is too big).
 */
extern int
gpr_node_child(gpr_base_entity *node,
                               unsigned n,
                               gpr_base_entity* child_p);

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
gpr_text_to_locale_string(gpr_text *text);

/*
 * Free dynamically allocated memory.
 *
 * This is a helper to free objects from dynamic languages.
 */
extern void
gpr_free(void *address);

/*
 * If this text object owns the buffer it references, free this buffer.
 *
 * Note that even though this accepts a pointer to a text object, it does not
 * deallocates the text object itself but rather the buffer it references.
 */
extern void
gpr_destroy_text(gpr_text *text);

/*
 * Return the text associated to this symbol.
 */
extern void
gpr_symbol_text(gpr_symbol_type *symbol,
                                gpr_text *text);

/*
 * Create a big integer from its string representation (in base 10).
 */
extern gpr_big_integer
gpr_create_big_integer(gpr_text *text);

/*
 * Return the string representation (in base 10) of this big integer.
 */
extern void
gpr_big_integer_text(gpr_big_integer bigint,
                                     gpr_text *text);

/*
 * Decrease the reference count for this big integer.
 */
extern void
gpr_big_integer_decref(gpr_big_integer bigint);

/*
 * Allocate strings to represent the library version number and build date and
 * put them in Version/Build_Date. Callers are expected to call free() on the
 * returned string once done.
 */
extern void
gpr_get_versions(char **version, char **build_date);

/*
 * Create a string value from its content (UTF32 with native endianity).
 *
 * Note that the CONTENT buffer argument is copied: the returned value does not
 * contain a reference to it.
 */
extern gpr_string_type
gpr_create_string(uint32_t *content, int length);

/*
 * Decrease the reference count for this string.
 */
extern void
gpr_string_dec_ref(gpr_string_type self);

/*
 * Kind-specific AST node primitives
 */

/* All these primitives return their result through an OUT parameter.  They
   return a boolean telling whether the operation was successful (it can fail
   if the node does not have the proper type, for instance).  When an AST node
   is returned, its ref-count is left as-is.  */

        



/*
 * Return the syntactic parent for this node. Return null for the root node.
 */
extern int gpr_gpr_node_parent(
    gpr_base_entity *node,


    gpr_base_entity *value_p
);


        



/*
 * Return an array that contains the lexical parents, this node included iff
 * ``with_self`` is True. Nearer parents are first in the list.
 */
extern int gpr_gpr_node_parents(
    gpr_base_entity *node,

        
        gpr_bool
        with_self,

    gpr_gpr_node_array *value_p
);


        



/*
 * Return an array that contains the direct lexical children.
 *
 * .. warning:: This constructs a whole array every-time you call it, and as
 *    such is less efficient than calling the ``Child`` built-in.
 */
extern int gpr_gpr_node_children(
    gpr_base_entity *node,


    gpr_gpr_node_array *value_p
);


        



/*
 * Return the first token used to parse this node.
 */
extern int gpr_gpr_node_token_start(
    gpr_base_entity *node,


    gpr_token *value_p
);


        



/*
 * Return the last token used to parse this node.
 */
extern int gpr_gpr_node_token_end(
    gpr_base_entity *node,


    gpr_token *value_p
);


        



/*
 * Return the 0-based index for Node in its parent's children.
 */
extern int gpr_gpr_node_child_index(
    gpr_base_entity *node,


    int *value_p
);


        



/*
 * Return the node's previous sibling, or null if there is no such sibling.
 */
extern int gpr_gpr_node_previous_sibling(
    gpr_base_entity *node,


    gpr_base_entity *value_p
);


        



/*
 * Return the node's next sibling, or null if there is no such sibling.
 */
extern int gpr_gpr_node_next_sibling(
    gpr_base_entity *node,


    gpr_base_entity *value_p
);


        



/*
 * Return the analysis unit owning this node.
 */
extern int gpr_gpr_node_unit(
    gpr_base_entity *node,


    gpr_analysis_unit *value_p
);


        



/*
 * Return whether the node is a ghost.
 *
 * Unlike regular nodes, ghost nodes cover no token in the input source: they
 * are logically located instead between two tokens. Both the ``token_start``
 * and the ``token_end`` of all ghost nodes is the token right after this
 * logical position.
 */
extern int gpr_gpr_node_is_ghost(
    gpr_base_entity *node,


    gpr_bool *value_p
);


        



/*
 * Return a string containing the filename + the sloc in GNU conformant format.
 * Useful to create diagnostics from a node.
 */
extern int gpr_gpr_node_full_sloc_image(
    gpr_base_entity *node,


    gpr_string_type *value_p
);


        



/*
 * This field can contain one of the following nodes:
 * ``gpr_ada_entity_kind_function``, ``gpr_ada_entity_kind_procedure``
 */
extern int gpr_ada_access_subp_f_subp_kind(
    gpr_base_entity *node,


    gpr_base_entity *value_p
);


        



/*

 */
extern int gpr_ada_access_subp_f_skips(
    gpr_base_entity *node,


    gpr_base_entity *value_p
);


        



/*

 */
extern int gpr_ada_pragma_f_skips(
    gpr_base_entity *node,


    gpr_base_entity *value_p
);


        



/*

 */
extern int gpr_ada_use_f_skips(
    gpr_base_entity *node,


    gpr_base_entity *value_p
);


        



/*

 */
extern int gpr_ada_with_f_has_limited(
    gpr_base_entity *node,


    gpr_base_entity *value_p
);


        



/*

 */
extern int gpr_ada_with_f_has_private(
    gpr_base_entity *node,


    gpr_base_entity *value_p
);


        



/*
 * This field contains a list that itself contains one of the following nodes:
 * ``gpr_identifier``, ``gpr_prefix``
 */
extern int gpr_ada_with_f_packages(
    gpr_base_entity *node,


    gpr_base_entity *value_p
);


        



/*

 */
extern int gpr_ada_generic_f_skips(
    gpr_base_entity *node,


    gpr_base_entity *value_p
);


        



/*

 */
extern int gpr_ada_library_item_f_generic_stub(
    gpr_base_entity *node,


    gpr_base_entity *value_p
);


        



/*

 */
extern int gpr_ada_library_item_f_separate(
    gpr_base_entity *node,


    gpr_base_entity *value_p
);


        



/*

 */
extern int gpr_ada_library_item_f_main(
    gpr_base_entity *node,


    gpr_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``gpr_identifier``,
 * ``gpr_prefix``
 */
extern int gpr_ada_main_f_name(
    gpr_base_entity *node,


    gpr_base_entity *value_p
);


        



/*

 */
extern int gpr_ada_pkg_f_has_private(
    gpr_base_entity *node,


    gpr_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes:
 * ``gpr_ada_entity_kind_function``, ``gpr_ada_entity_kind_procedure``
 */
extern int gpr_ada_subp_f_subp_kind(
    gpr_base_entity *node,


    gpr_base_entity *value_p
);


        



/*

 */
extern int gpr_ada_prelude_f_context_clauses(
    gpr_base_entity *node,


    gpr_base_entity *value_p
);


        



/*

 */
extern int gpr_ada_prelude_f_library_item(
    gpr_base_entity *node,


    gpr_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``gpr_identifier``,
 * ``gpr_prefix``
 */
extern int gpr_ada_separate_f_parent_name(
    gpr_base_entity *node,


    gpr_base_entity *value_p
);


        



/*

 */
extern int gpr_ada_with_formal_f_kind(
    gpr_base_entity *node,


    gpr_base_entity *value_p
);


        



/*

 */
extern int gpr_ada_with_formal_f_skips(
    gpr_base_entity *node,


    gpr_base_entity *value_p
);


        



/*
 * Return whether this is an instance of AllQualifierPresent
 */
extern int gpr_all_qualifier_p_as_bool(
    gpr_base_entity *node,


    gpr_bool *value_p
);


        



/*

 */
extern int gpr_attribute_decl_f_attr_name(
    gpr_base_entity *node,


    gpr_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes:
 * ``gpr_others_designator``, ``gpr_string_literal_at``
 */
extern int gpr_attribute_decl_f_attr_index(
    gpr_base_entity *node,


    gpr_base_entity *value_p
);


        



/*
 * This field contains a list that itself contains one of the following nodes:
 * ``gpr_builtin_function_call``, ``gpr_project_reference``,
 * ``gpr_string_literal_at``, ``gpr_terms``, ``gpr_variable_reference``
 */
extern int gpr_attribute_decl_f_expr(
    gpr_base_entity *node,


    gpr_base_entity *value_p
);


        



/*

 */
extern int gpr_attribute_reference_f_attribute_name(
    gpr_base_entity *node,


    gpr_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes:
 * ``gpr_others_designator``, ``gpr_string_literal``
 */
extern int gpr_attribute_reference_f_attribute_index(
    gpr_base_entity *node,


    gpr_base_entity *value_p
);


        



/*

 */
extern int gpr_builtin_function_call_f_function_name(
    gpr_base_entity *node,


    gpr_base_entity *value_p
);


        



/*

 */
extern int gpr_builtin_function_call_f_parameters(
    gpr_base_entity *node,


    gpr_base_entity *value_p
);


        



/*

 */
extern int gpr_case_construction_f_var_ref(
    gpr_base_entity *node,


    gpr_base_entity *value_p
);


        



/*

 */
extern int gpr_case_construction_f_items(
    gpr_base_entity *node,


    gpr_base_entity *value_p
);


        



/*
 * This field contains a list that itself contains one of the following nodes:
 * ``gpr_others_designator``, ``gpr_string_literal``
 */
extern int gpr_case_item_f_choice(
    gpr_base_entity *node,


    gpr_base_entity *value_p
);


        



/*
 * This field contains a list that itself contains one of the following nodes:
 * ``gpr_attribute_decl``, ``gpr_case_construction``, ``gpr_empty_decl``,
 * ``gpr_variable_decl``
 */
extern int gpr_case_item_f_decls(
    gpr_base_entity *node,


    gpr_base_entity *value_p
);


        



/*

 */
extern int gpr_compilation_unit_f_project(
    gpr_base_entity *node,


    gpr_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``gpr_identifier``,
 * ``gpr_prefix``
 */
extern int gpr_prefix_f_prefix(
    gpr_base_entity *node,


    gpr_base_entity *value_p
);


        



/*

 */
extern int gpr_prefix_f_suffix(
    gpr_base_entity *node,


    gpr_base_entity *value_p
);


        



/*
 * Return whether this is an instance of LimitedPresent
 */
extern int gpr_limited_node_p_as_bool(
    gpr_base_entity *node,


    gpr_bool *value_p
);


        



/*

 */
extern int gpr_package_decl_f_pkg_name(
    gpr_base_entity *node,


    gpr_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``gpr_package_renaming``,
 * ``gpr_package_spec``
 */
extern int gpr_package_decl_f_pkg_spec(
    gpr_base_entity *node,


    gpr_base_entity *value_p
);


        



/*

 */
extern int gpr_package_extension_f_extended_name(
    gpr_base_entity *node,


    gpr_base_entity *value_p
);


        



/*

 */
extern int gpr_package_renaming_f_renamed_name(
    gpr_base_entity *node,


    gpr_base_entity *value_p
);


        



/*

 */
extern int gpr_package_spec_f_extension(
    gpr_base_entity *node,


    gpr_base_entity *value_p
);


        



/*
 * This field contains a list that itself contains one of the following nodes:
 * ``gpr_attribute_decl``, ``gpr_case_construction``, ``gpr_empty_decl``,
 * ``gpr_variable_decl``
 */
extern int gpr_package_spec_f_decls(
    gpr_base_entity *node,


    gpr_base_entity *value_p
);


        



/*

 */
extern int gpr_package_spec_f_end_name(
    gpr_base_entity *node,


    gpr_base_entity *value_p
);


        



/*
 * Return whether this is an instance of PrivatePresent
 */
extern int gpr_private_node_p_as_bool(
    gpr_base_entity *node,


    gpr_bool *value_p
);


        



/*

 */
extern int gpr_project_f_context_clauses(
    gpr_base_entity *node,


    gpr_base_entity *value_p
);


        



/*

 */
extern int gpr_project_f_project_decl(
    gpr_base_entity *node,


    gpr_base_entity *value_p
);


        



/*

 */
extern int gpr_project_declaration_f_qualifier(
    gpr_base_entity *node,


    gpr_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``gpr_identifier``,
 * ``gpr_prefix``
 */
extern int gpr_project_declaration_f_project_name(
    gpr_base_entity *node,


    gpr_base_entity *value_p
);


        



/*

 */
extern int gpr_project_declaration_f_extension(
    gpr_base_entity *node,


    gpr_base_entity *value_p
);


        



/*
 * This field contains a list that itself contains one of the following nodes:
 * ``gpr_attribute_decl``, ``gpr_case_construction``, ``gpr_empty_decl``,
 * ``gpr_package_decl``, ``gpr_typed_string_decl``, ``gpr_variable_decl``
 */
extern int gpr_project_declaration_f_decls(
    gpr_base_entity *node,


    gpr_base_entity *value_p
);


        



/*
 * This field can contain one of the following nodes: ``gpr_identifier``,
 * ``gpr_prefix``
 */
extern int gpr_project_declaration_f_end_name(
    gpr_base_entity *node,


    gpr_base_entity *value_p
);


        



/*

 */
extern int gpr_project_extension_f_is_all(
    gpr_base_entity *node,


    gpr_base_entity *value_p
);


        



/*

 */
extern int gpr_project_extension_f_path_name(
    gpr_base_entity *node,


    gpr_base_entity *value_p
);


        



/*

 */
extern int gpr_project_reference_f_attr_ref(
    gpr_base_entity *node,


    gpr_base_entity *value_p
);


        



/*

 */
extern int gpr_string_literal_at_f_str_lit(
    gpr_base_entity *node,


    gpr_base_entity *value_p
);


        



/*

 */
extern int gpr_string_literal_at_f_at_lit(
    gpr_base_entity *node,


    gpr_base_entity *value_p
);


        



/*

 */
extern int gpr_terms_f_terms(
    gpr_base_entity *node,


    gpr_base_entity *value_p
);


        



/*

 */
extern int gpr_type_reference_f_var_type_name(
    gpr_base_entity *node,


    gpr_base_entity *value_p
);


        



/*

 */
extern int gpr_typed_string_decl_f_type_id(
    gpr_base_entity *node,


    gpr_base_entity *value_p
);


        



/*

 */
extern int gpr_typed_string_decl_f_string_literals(
    gpr_base_entity *node,


    gpr_base_entity *value_p
);


        



/*

 */
extern int gpr_variable_decl_f_var_name(
    gpr_base_entity *node,


    gpr_base_entity *value_p
);


        



/*

 */
extern int gpr_variable_decl_f_var_type(
    gpr_base_entity *node,


    gpr_base_entity *value_p
);


        



/*
 * This field contains a list that itself contains one of the following nodes:
 * ``gpr_builtin_function_call``, ``gpr_project_reference``,
 * ``gpr_string_literal_at``, ``gpr_terms``, ``gpr_variable_reference``
 */
extern int gpr_variable_decl_f_expr(
    gpr_base_entity *node,


    gpr_base_entity *value_p
);


        



/*

 */
extern int gpr_variable_reference_f_variable_name(
    gpr_base_entity *node,


    gpr_base_entity *value_p
);


        



/*

 */
extern int gpr_variable_reference_f_attribute_ref(
    gpr_base_entity *node,


    gpr_base_entity *value_p
);


        



/*

 */
extern int gpr_with_decl_f_is_limited(
    gpr_base_entity *node,


    gpr_base_entity *value_p
);


        



/*

 */
extern int gpr_with_decl_f_path_names(
    gpr_base_entity *node,


    gpr_base_entity *value_p
);



/*
 * Event handlers
 */

/*
 * Create an event handler. When done with it, the result must be passed to
 * ``gpr_dec_ref_event_handler``.
 *
 * Pass as ``data`` a pointer to hold your private data: it will be passed to
 * all callbacks below.
 *
 * ``destroy`` is a callback that is called by ``gpr_dec_ref_event_handler`` to
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
extern gpr_event_handler
gpr_create_event_handler(
   void *data,
   gpr_event_handler_destroy_callback destroy_func,
   gpr_event_handler_unit_requested_callback unit_requested_func,
   gpr_event_handler_unit_parsed_callback unit_parsed_func
);

/*
 * Release an ownership share for this event handler. This destroys the event
 * handler if there are no shares left.
 */
extern void
gpr_dec_ref_event_handler(gpr_event_handler self);

/*
 * File readers
 */

/*
 * Create a file reader. When done with it, the result must be passed to
 * ``gpr_dec_ref_file_reader``.
 *
 * Pass as ``data`` a pointer to hold your private data: it will be passed to
 * all callbacks below.
 *
 * ``destroy`` is a callback that is called by ``gpr_dec_ref_file_reader`` to
 * leave a chance to free resources that ``data`` may hold.
 *
 * ``read`` is a callback. For a given filename/charset and whether to read the
 * BOM (Byte Order Mark), it tries to fetch the contents of the source file,
 * returned in ``Contents``. If there is an error, it must return it in
 * ``Diagnostic`` instead.
 */
extern gpr_file_reader
gpr_create_file_reader(
   void *data,
   gpr_file_reader_destroy_callback destroy_func,
   gpr_file_reader_read_callback read_func
);

/*
 * Release an ownership share for this file reader. This destroys the file
 * reader if there are no shares left.
 */
extern void
gpr_dec_ref_file_reader(gpr_file_reader self);




/*
 * Unit providers
 */

/*
 * Create a unit provider. When done with it, the result must be passed to
 * ``gpr_destroy_unit_provider``.
 *
 * Pass as ``data`` a pointer to hold your private data: it will be passed to
 * all callbacks below.
 *
 * ``destroy`` is a callback that is called by ``gpr_destroy_unit_provider`` to
 * leave a chance to free resources that ``data`` may hold.
 *
 * ``get_unit_from_node`` is a callback. It turns an analysis unit reference
 * represented as a node into an analysis unit. It should return ``NULL`` if
 * the node is not a valid unit name representation.
 *
 * ``get_unit_from_name`` is a callback similar to ``get_unit_from_node``
 * except it takes an analysis unit reference represented as a string.
 */
extern gpr_unit_provider
gpr_create_unit_provider(
   void *data,
   gpr_unit_provider_destroy_callback destroy_func,
   gpr_unit_provider_get_unit_filename_callback get_unit_filename_func,
   gpr_unit_provider_get_unit_from_name_callback get_unit_from_name_func
);

/*
 * Release an ownership share for this unit provider. This destroys the unit
 * provider if there are no shares left.
 */
extern void
gpr_dec_ref_unit_provider(void *data);




/*
 * Misc
 */

/*
 * Return exception information for the last error that happened in the current
 * thread. Will be automatically allocated on error and free'd on the next
 * error.
 */
extern const gpr_exception *
gpr_get_last_exception(void);

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
gpr_token_kind_name(gpr_token_kind kind);

/*
 * Return a reference to the next token in the corresponding analysis unit.
 */
extern void
gpr_token_next(gpr_token *token,
                               gpr_token *next_token);

/*
 * Return a reference to the previous token in the corresponding analysis unit.
 */
extern void
gpr_token_previous(gpr_token *token,
                                   gpr_token *previous_token);

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
gpr_token_range_text(gpr_token *first,
                                     gpr_token *last,
                                     gpr_text *result);

/*
 * Return whether ``L`` and ``R`` are structurally equivalent tokens. This
 * means that their position in the stream won't be taken into account, only
 * the kind and text of the token.
 */
extern void
gpr_token_is_equivalent(gpr_token *left,
                                        gpr_token *right);

/*
 * Return a representation of this entity as a string.
 */
extern void
gpr_entity_image(gpr_base_entity ent, gpr_text *result);

#ifdef __cplusplus
}
#endif

#endif
