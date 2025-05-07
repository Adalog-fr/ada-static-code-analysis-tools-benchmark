--
--  Copyright (C) 2014-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Exceptions;                  use Ada.Exceptions;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;
with Ada.Unchecked_Conversion;

with System;

with Interfaces;           use Interfaces;
with Interfaces.C;         use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

with Langkit_Support.Slocs; use Langkit_Support.Slocs;
with Langkit_Support.Text;  use Langkit_Support.Text;

with Libadalang.Common;   use Libadalang.Common;




--  Internal package: defines data types and subprograms to provide the
--  implementation of the exported C API (see the corresponding C header file).

private package Libadalang.Implementation.C is

   subtype ada_analysis_context is Internal_Context;
   --  This type represents a context for all source analysis. This is the
   --  first type you need to create to use Libadalang. It will contain the
   --  results of all analysis, and is the main holder for all the data.
   --
   --  You can create several analysis contexts if you need to, which enables
   --  you, for example to:
   --
   --  * analyze several different projects at the same time;
   --
   --  * analyze different parts of the same projects in parallel.
   --
   --  In the current design, contexts always keep all of their analysis units
   --  allocated. If you need to get this memory released, the only option at
   --  your disposal is to destroy your analysis context instance.
   --
   --  This structure is partially opaque: some fields are exposed to allow
   --  direct access, for performance concerns.

   subtype ada_analysis_unit is Internal_Unit;
   --  This type represents the analysis of a single file.
   --
   --  This type has strong-reference semantics and is ref-counted.
   --  Furthermore, a reference to a unit contains an implicit reference to the
   --  context that owns it. This means that keeping a reference to a unit will
   --  keep the context and all the unit it contains allocated.
   --
   --  This structure is partially opaque: some fields are exposed to allow
   --  direct access, for performance concerns.

   type ada_base_node is new System.Address;
   --  Data type for all nodes. Nodes are assembled to make up a tree.  See the
   --  node primitives below to inspect such trees.
   --
   --  Unlike for contexts and units, this type has weak-reference semantics:
   --  keeping a reference to a node has no effect on the decision to keep the
   --  unit that it owns allocated. This means that once all references to the
   --  context and units related to a node are dropped, the context and its
   --  units are deallocated and the node becomes a stale reference: most
   --  operations on it will raise a ``Stale_Reference_Error``.
   --
   --  Note that since reparsing an analysis unit deallocates all the nodes it
   --  contains, this operation makes all reference to these nodes stale as
   --  well.

   type ada_node_kind_enum is new int;
   --  Kind of AST nodes in parse trees.

   



subtype ada_base_entity is Internal_Entity;
type ada_base_entity_Ptr is access Internal_Entity;




   type ada_symbol_type is record
      Data, Bounds : System.Address;
   end record
      with Convention => C;
   --  Reference to a symbol. Symbols are owned by analysis contexts, so they
   --  must not outlive them. This type exists only in the C API, and roughly
   --  wraps the corresponding Ada type (an array fat pointer).

   subtype ada_string_type is String_Type;

   --  Helper data structures for source location handling

   type ada_source_location is record
      Line   : Unsigned_32;
      Column : Unsigned_16;
   end record
     with Convention => C;

   type ada_source_location_range is record
      Start_S, End_S : ada_source_location;
   end record
     with Convention => C;

   type ada_text is record
      Chars  : System.Address;
      --  Address for the content of the string.

      Length : size_t;
      --  Size of the string (in characters).

      Is_Allocated : int;
   end record
     with Convention => C;
   --  String encoded in UTF-32 (native endianness).

   type ada_big_integer is new System.Address;
   --  Arbitrarily large integer.

   type ada_token is record
      Context                   : ada_analysis_context;
      Token_Data                : Token_Data_Handler_Access;
      Token_Index, Trivia_Index : int;

      Kind       : int;
      Text       : ada_text;
      Sloc_Range : ada_source_location_range;
   end record
     with Convention => C;
   --  Reference to a token in an analysis unit.

   type ada_diagnostic is record
      Sloc_Range : ada_source_location_range;
      Message    : ada_text;
      --  When the API returns a diagnostic, it is up to the caller to free the
      --  message string.
   end record
     with Convention => C;
   --  Diagnostic for an analysis unit: cannot open the source file, parsing
   --  error, ...

   type ada_exception_kind is (
      Exception_File_Read_Error, Exception_Bad_Type_Error, Exception_Out_Of_Bounds_Error, Exception_Invalid_Input, Exception_Invalid_Symbol_Error, Exception_Invalid_Unit_Name_Error, Exception_Native_Exception, Exception_Precondition_Failure, Exception_Property_Error, Exception_Template_Args_Error, Exception_Template_Format_Error, Exception_Template_Instantiation_Error, Exception_Stale_Reference_Error, Exception_Syntax_Error, Exception_Unknown_Charset, Exception_Invalid_Project, Exception_Unsupported_View_Error
   ) with Convention => C;
   --  Enumerated type describing all possible exceptions that need to be
   --  handled in the C bindings.

   type ada_exception is record
      Kind : ada_exception_kind;
      --  The kind of this exception.

      Information : chars_ptr;
      --  Message and context information associated with this exception.
   end record;
   --  Holder for native exceptions-related information.  Memory management for
   --  this and all the fields is handled by the library: one just has to make
   --  sure not to keep references to it.
   --
   --  .. TODO: For the moment, this structure contains already formatted
   --     information, but depending on possible future Ada runtime
   --     improvements, this might change.

   type ada_exception_Ptr is access ada_exception;

   type ada_bool is new Unsigned_8;
   subtype uint32_t is Unsigned_32;

      subtype ada_analysis_unit_kind is Analysis_Unit_Kind;
      subtype ada_lookup_kind is Lookup_Kind;
      subtype ada_designated_env_kind is Designated_Env_Kind;
      subtype ada_ref_result_kind is Ref_Result_Kind;
      subtype ada_call_expr_kind is Call_Expr_Kind;
      subtype ada_grammar_rule is Grammar_Rule;

   procedure Free (Address : System.Address)
     with Export        => True,
          Convention    => C,
          External_Name => "ada_free";
   --  Free dynamically allocated memory.
   --
   --  This is a helper to free objects from dynamic languages.
   --  Helper to free objects in dynamic languages

   procedure ada_destroy_text (T : access ada_text)
     with Export        => True,
          Convention    => C,
          External_Name => "ada_destroy_text";
   --  If this text object owns the buffer it references, free this buffer.
   --
   --  Note that even though this accepts a pointer to a text object, it does
   --  not deallocates the text object itself but rather the buffer it
   --  references.

   procedure ada_symbol_text
     (Symbol : access ada_symbol_type; Text : access ada_text)
      with Export, Convention => C,
           External_Name => "ada_symbol_text";
   --  Return the text associated to this symbol.

   function ada_create_big_integer
     (Text : access ada_text) return ada_big_integer
      with Export, Convention => C,
           External_Name => "ada_create_big_integer";
   --  Create a big integer from its string representation (in base 10).

   procedure ada_big_integer_text
     (Bigint : ada_big_integer; Text : access ada_text)
      with Export, Convention => C,
           External_Name => "ada_big_integer_text";
   --  Return the string representation (in base 10) of this big integer.

   procedure ada_big_integer_decref
     (Bigint : ada_big_integer)
      with Export, Convention => C,
           External_Name => "ada_big_integer_decref";
   --  Decrease the reference count for this big integer.

   procedure ada_get_versions
     (Version, Build_Date : access chars_ptr)
      with Export, Convention => C,
           External_Name => "ada_get_versions";
   --  Allocate strings to represent the library version number and build date
   --  and put them in Version/Build_Date. Callers are expected to call free()
   --  on the returned string once done.

   function ada_create_string
     (Content : System.Address; Length : int) return ada_string_type
      with Export, Convention => C,
           External_Name => "ada_create_string";
   --  Create a string value from its content (UTF32 with native endianity).
   --
   --  Note that the CONTENT buffer argument is copied: the returned value does
   --  not contain a reference to it.

   procedure ada_string_dec_ref (Self : ada_string_type)
      with Export, Convention => C,
           External_Name => "ada_string_dec_ref";
   --  Decrease the reference count for this string.

   ------------------
   -- File readers --
   ------------------

   type ada_file_reader is new System.Address;
   --  Interface to override how source files are fetched and decoded.

   type ada_file_reader_destroy_callback is access procedure
     (Data : System.Address)
      with Convention => C;
   --  Callback type for functions that are called when destroying a file
   --  reader.

   type ada_file_reader_read_callback is access procedure
     (Data       : System.Address;
      Filename   : chars_ptr;
      Charset    : chars_ptr;
      Read_BOM   : int;
      Buffer     : access ada_text;
      Diagnostic : access ada_diagnostic)
      with Convention => C;
   --  Callback type for functions that are called to fetch the decoded source
   --  buffer for a requested filename.

   --------------------
   -- Event handlers --
   --------------------

   type ada_event_handler is new System.Address;
   --  Interface to handle events sent by the analysis context.

   type ada_event_handler_unit_requested_callback is access procedure
     (Data               : System.Address;
      Context            : ada_analysis_context;
      Name               : ada_text;
      From               : ada_analysis_unit;
      Found              : ada_bool;
      Is_Not_Found_Error : ada_bool)
      with Convention => C;
   --  Callback type for functions that are called when a unit is requested.
   --
   --  ``name`` is the name of the requested unit.
   --
   --  ``from`` is the unit from which the unit was requested.
   --
   --  ``found`` indicates whether the requested unit was found or not.
   --
   --  ``is_not_found_error`` indicates whether the fact that the unit was not
   --  found is an error or not.
   --
   --  .. warning:: The interface of this callback is probably subject to
   --     change, so should be treated as experimental.

   type ada_event_handler_unit_parsed_callback is access procedure
     (Data     : System.Address;
      Context  : ada_analysis_context;
      Unit     : ada_analysis_unit;
      Reparsed : ada_bool)
      with Convention => C;
   --  Callback type for functions that are called when a unit is parsed.
   --
   --  ``unit`` is the resulting unit.
   --
   --  ``reparsed`` indicates whether the unit was reparsed, or whether it was
   --  the first parse.

   type ada_event_handler_destroy_callback is access procedure
     (Data : System.Address)
      with Convention => C;
   --  Callback type for functions that are called when destroying an event
   --  handler.

   --------------------
   -- Unit providers --
   --------------------

   type ada_unit_provider is new System.Address;
   --  Interface to fetch analysis units from a name and a unit kind.
   --
   --  The unit provider mechanism provides an abstraction which assumes that
   --  to any couple (unit name, unit kind) we can associate at most one source
   --  file. This means that several couples can be associated to the same
   --  source file, but on the other hand, only one one source file can be
   --  associated to a couple.
   --
   --  This is used to make the semantic analysis able to switch from one
   --  analysis units to another.
   --
   --  See the documentation of each unit provider for the exact semantics of
   --  the unit name/kind information.

   type ada_unit_provider_destroy_callback is access procedure
     (Data : System.Address)
      with Convention => C;
   --  Callback type for functions that are called when destroying a unit file
   --  provider type.

   type ada_unit_provider_get_unit_filename_callback is access function
     (Data        : System.Address;
      Name        : ada_text;
      Kind        : ada_analysis_unit_kind) return chars_ptr
      with Convention => C;
   --  Callback type for functions that are called to turn a unit reference
   --  encoded as a unit name into an analysis unit.

   type ada_unit_provider_get_unit_from_name_callback is access function
     (Data        : System.Address;
      Context     : ada_analysis_context;
      Name        : ada_text;
      Kind        : ada_analysis_unit_kind;
      Charset     : chars_ptr;
      Reparse     : int) return ada_analysis_unit
      with Convention => C;
   --  Callback type for functions that are called to turn a unit reference
   --  encoded as a unit name into an analysis unit.

   -------------------------
   -- Analysis primitives --
   -------------------------

   function ada_create_analysis_context
     (Charset       : chars_ptr;
      File_Reader   : ada_file_reader;
      Unit_Provider : ada_unit_provider;
      Event_Handler : ada_event_handler;
      With_Trivia   : int;
      Tab_Stop      : int) return ada_analysis_context
      with Export        => True,
           Convention    => C,
           External_name => "ada_create_analysis_context";
   --  Create a new analysis context.
   --
   --  ``Charset`` will be used as a default charset to decode input sources in
   --  analysis units. Please see ``GNATCOLL.Iconv`` for several supported
   --  charsets. Be careful: passing an unsupported charset is not guaranteed
   --  to raise an error here. If no charset is provided, ``"iso-8859-1"`` is
   --  the default.
   --
   --  .. TODO: Passing an unsupported charset here is not guaranteed to raise
   --     an error right here, but this would be really helpful for users.
   --
   --  When ``With_Trivia`` is true, the parsed analysis units will contain
   --  trivias.
   --
   --  If provided, ``File_Reader`` will be used to fetch the contents of
   --  source files instead of the default, which is to just read it from the
   --  filesystem and decode it using the regular charset rules. Note that if
   --  provided, all parsing APIs that provide a buffer are forbidden, and any
   --  use of the rewriting API with the returned context is rejected.
   --
   --  If provided, ``Unit_Provider`` will be used to query the file name that
   --  corresponds to a unit reference during semantic analysis. If it is
   --  ``NULL``, the default one is used instead.
   --
   --  ``Tab_Stop`` is a positive number to describe the effect of tabulation
   --  characters on the column number in source files.

   function ada_context_incref
     (Context : ada_analysis_context)
      return ada_analysis_context
      with Export        => True,
           Convention    => C,
           External_name => "ada_context_incref";
   --  Increase the reference count to an analysis context. Return the
   --  reference for convenience.

   procedure ada_context_decref
     (Context : ada_analysis_context)
      with Export        => True,
           Convention    => C,
           External_name => "ada_context_decref";
   --  Decrease the reference count to an analysis context. Destruction happens
   --  when the ref-count reaches 0.

   function ada_context_symbol
     (Context : ada_analysis_context;
      Text    : access ada_text;
      Symbol  : access ada_symbol_type) return int
      with Export, Convention => C,
           External_name => "ada_context_symbol";
   --  If the given string is a valid symbol, yield it as a symbol and return
   --  true. Otherwise, return false.

   procedure ada_context_discard_errors_in_populate_lexical_env
     (Context : ada_analysis_context;
      Discard : int)
      with Export        => True,
           Convention    => C,
           External_name => "ada_context_discard_errors_in_populate_lexical_env";
   --  Debug helper. Set whether ``Property_Error`` exceptions raised in
   --  ``Populate_Lexical_Env`` should be discarded. They are by default.

   function ada_get_analysis_unit_from_file
     (Context           : ada_analysis_context;
      Filename, Charset : chars_ptr;
      Reparse           : int;
      Rule              : ada_grammar_rule)
      return ada_analysis_unit
      with Export        => True,
           Convention    => C,
           External_name =>
              "ada_get_analysis_unit_from_file";
   --  Create a new analysis unit for ``Filename`` or return the existing one
   --  if any. If ``Reparse`` is true and the analysis unit already exists,
   --  reparse it from ``Filename``.
   --
   --  ``Rule`` controls which grammar rule is used to parse the unit.
   --
   --  Use ``Charset`` in order to decode the source. If ``Charset`` is empty
   --  then use the context's default charset.
   --
   --  If any failure occurs, such as file opening, decoding, lexing or parsing
   --  failure, return an analysis unit anyway: errors are described as
   --  diagnostics of the returned analysis unit.

   function ada_get_analysis_unit_from_buffer
     (Context           : ada_analysis_context;
      Filename, Charset : chars_ptr;
      Buffer            : chars_ptr;
      Buffer_Size       : size_t;
      Rule              : ada_grammar_rule)
      return ada_analysis_unit
      with Export        => True,
           Convention    => C,
           External_name =>
              "ada_get_analysis_unit_from_buffer";
   --  Create a new analysis unit for ``Filename`` or return the existing one
   --  if any. Whether the analysis unit already exists or not, (re)parse it
   --  from the source code in ``Buffer``.
   --
   --  ``Rule`` controls which grammar rule is used to parse the unit.
   --
   --  Use ``Charset`` in order to decode the source. If ``Charset`` is empty
   --  then use the context's default charset.
   --
   --  If any failure occurs, such as file opening, decoding, lexing or parsing
   --  failure, return an analysis unit anyway: errors are described as
   --  diagnostics of the returned analysis unit.

   function ada_get_analysis_unit_from_provider
     (Context : ada_analysis_context;
      Name    : ada_text;
      Kind    : ada_analysis_unit_kind;
      Charset : chars_ptr;
      Reparse : int) return ada_analysis_unit
      with Export        => True,
           Convention    => C,
           External_name =>
              "ada_get_analysis_unit_from_provider";
   --  Create a new analysis unit for ``Name``/``Kind`` or return the
      --  existing one if any. If ``Reparse`` is true and the analysis unit
      --  already exists, reparse it from the on-disk source file.
      --
      --  The ``Name`` and ``Kind`` arguments are forwarded directly to query
      --  the context's unit provider and get the filename for the returned
      --  unit. See the documentation of the relevant unit provider for their
      --  exact semantics.
      --
      --  Use ``Charset`` in order to decode the source. If ``Charset`` is
      --  empty then use the context's default charset.
      --
      --  If the unit name cannot be tuned into a file name, return ``NULL``.
      --  If any other failure occurs, such as file opening, decoding, lexing
      --  or parsing failure, return an analysis unit anyway: errors are
      --  described as diagnostics of the returned analysis unit.

   procedure ada_unit_root
     (Unit     : ada_analysis_unit;
      Result_P : ada_base_entity_Ptr)
      with Export        => True,
           Convention    => C,
           External_name => "ada_unit_root";
   --  Return the root node for this unit, or ``NULL`` if there is none.

   procedure ada_unit_first_token
     (Unit  : ada_analysis_unit;
      Token : access ada_token)
      with Export        => True,
           Convention    => C,
           External_name => "ada_unit_first_token";
   --  Return a reference to the first token scanned in this unit.

   procedure ada_unit_last_token
     (Unit  : ada_analysis_unit;
      Token : access ada_token)
      with Export        => True,
           Convention    => C,
           External_name => "ada_unit_last_token";
   --  Return a reference to the last token scanned in this unit.

   function ada_unit_token_count
     (Unit : ada_analysis_unit) return int
      with Export        => True,
           Convention    => C,
           External_Name => "ada_unit_token_count";
   --  Return the number of tokens in this unit.

   function ada_unit_trivia_count
     (Unit : ada_analysis_unit) return int
      with Export        => True,
           Convention    => C,
           External_Name => "ada_unit_trivia_count";
   --  Return the number of trivias in this unit. This is 0 for units that were
   --  parsed with trivia analysis disabled.

   procedure ada_unit_lookup_token
     (Unit   : ada_analysis_unit;
      Sloc   : access ada_source_location;
      Result : access ada_token)
      with Export        => True,
           Convention    => C,
           External_Name => "ada_unit_lookup_token";
   --  Look for a token in this unit that contains the given source location.
   --  If this falls before the first token, return the first token. If this
   --  falls between two tokens, return the token that appears before. If this
   --  falls after the last token, return the last token. If there is no token
   --  in this unit, return no token.

   procedure ada_unit_dump_lexical_env
     (Unit : ada_analysis_unit)
      with Export        => True,
           Convention    => C,
           External_Name => "ada_unit_dump_lexical_env";

   function ada_unit_filename
     (Unit : ada_analysis_unit)
      return chars_ptr
      with Export        => True,
           Convention    => C,
           External_name => "ada_unit_filename";
   --  Return the filename this unit is associated to.
   --
   --  The returned string is dynamically allocated and the caller must free it
   --  when done with it.

   function ada_unit_diagnostic_count
     (Unit : ada_analysis_unit) return unsigned
      with Export        => True,
           Convention    => C,
           External_name => "ada_unit_diagnostic_count";
   --  Return the number of diagnostics associated to this unit.

   function ada_unit_diagnostic
     (Unit         : ada_analysis_unit;
      N            : unsigned;
      Diagnostic_P : access ada_diagnostic) return int
      with Export        => True,
           Convention    => C,
           External_name => "ada_unit_diagnostic";
   --  Get the Nth diagnostic in this unit and store it into ``*diagnostic_p``.
   --  Return zero on failure (when N is too big).

   function ada_unit_context
     (Unit : ada_analysis_unit)
      return ada_analysis_context
      with Export        => True,
           Convention    => C,
           External_name => "ada_unit_context";
   --  Return the context that owns this unit.

   procedure ada_unit_reparse_from_file
     (Unit : ada_analysis_unit; Charset : chars_ptr)
      with Export        => True,
           Convention    => C,
           External_name => "ada_unit_reparse_from_file";
   --  Reparse an analysis unit from the associated file.
   --
   --  Use ``Charset`` in order to decode the source. If ``Charset`` is empty
   --  then use the context's default charset.
   --
   --  If any failure occurs, such as decoding, lexing or parsing failure,
   --  diagnostic are emitted to explain what happened.

   procedure ada_unit_reparse_from_buffer
     (Unit        : ada_analysis_unit;
      Charset     : chars_ptr;
      Buffer      : chars_ptr;
      Buffer_Size : size_t)
      with Export        => True,
           Convention    => C,
           External_name => "ada_unit_reparse_from_buffer";
   --  Reparse an analysis unit from a buffer.
   --
   --  Use ``Charset`` in order to decode the source. If ``Charset`` is empty
   --  then use the context's default charset.
   --
   --  If any failure occurs, such as decoding, lexing or parsing failure,
   --  diagnostic are emitted to explain what happened.

   function ada_unit_populate_lexical_env
     (Unit : ada_analysis_unit)
      return int
      with Export        => True,
           Convention    => C,
           External_name => "ada_unit_populate_lexical_env";
   --  Create lexical environments for this analysis unit, according to the
   --  specifications given in the language spec.
   --
   --  If not done before, it will be automatically called during semantic
   --  analysis. Calling it before enables one to control where the latency
   --  occurs.
   --
   --  Depending on whether errors are discarded (see
   --  ``Discard_Errors_In_Populate_Lexical_Env``), return ``0`` on failure and
   --  ``1`` on success.

   ---------------------------------
   -- General AST node primitives --
   ---------------------------------

   function ada_node_kind
     (Node : ada_base_entity_Ptr) return ada_node_kind_enum
      with Export        => True,
           Convention    => C,
           External_name => "ada_node_kind";
   --  Return the kind of this node.

   procedure ada_kind_name
     (Kind : ada_node_kind_enum; Result : access ada_text)
      with Export        => True,
           Convention    => C,
           External_name => "ada_kind_name";
   --  Helper for textual dump: return the kind name for this node. The
   --  returned string is a copy and thus must be free'd by the caller.

   function ada_node_unit
     (Node : ada_base_entity_Ptr) return ada_analysis_unit
      with Export => True,
           Convention => C,
           External_Name => "ada_node_unit";
   --  Return the analysis unit that owns this node.

   function ada_is_token_node
     (Node : ada_base_entity_Ptr) return int
      with Export        => True,
           Convention    => C,
           External_name => "ada_node_is_token_node";
   --  Return whether this node is a node that contains only a single token.

   function ada_is_synthetic
     (Node : ada_base_entity_Ptr) return int
      with Export        => True,
           Convention    => C,
           External_name => "ada_node_is_synthetic";
   --  Return whether this node is synthetic.

   procedure ada_node_image
     (Node : ada_base_entity_Ptr; Result : access ada_text)
      with Export        => True,
           Convention    => C,
           External_name => "ada_node_image";
   --  Return a representation of this node as a string.

   procedure ada_node_text
     (Node : ada_base_entity_Ptr;
      Text : access ada_text)
      with Export, Convention => C,
           External_Name      => "ada_node_text";
   --  Return the source buffer slice corresponding to the text that spans
   --  between the first and the last tokens of this node.
   --
   --  Note that this returns the empty string for synthetic nodes.

   procedure ada_node_sloc_range
     (Node         : ada_base_entity_Ptr;
      Sloc_Range_P : access ada_source_location_range)
      with Export        => True,
           Convention    => C,
           External_name => "ada_node_sloc_range";
   --  Return the spanning source location range for this node.
   --
   --  Note that this returns the sloc of the parent for synthetic nodes.

   procedure ada_lookup_in_node
     (Node   : ada_base_entity_Ptr;
      Sloc   : ada_source_location;
      Result : ada_base_entity_Ptr)
      with Export        => True,
           Convention    => C,
           External_name => "ada_lookup_in_node";
   --  Return the bottom-most node from in ``Node`` and its children which
   --  contains ``Sloc``, or ``NULL`` if there is none.

   function ada_node_children_count
     (Node : ada_base_entity_Ptr) return unsigned
      with Export        => True,
           Convention    => C,
           External_name => "ada_node_children_count";
   --  Return the number of children in this node.

   function ada_node_child
     (Node    : ada_base_entity_Ptr;
      N       : unsigned;
      Child_P : ada_base_entity_Ptr) return int
      with Export        => True,
           Convention    => C,
           External_name => "ada_node_child";
   --  Return the Nth child for in this node's fields and store it into
   --  ``*child_p``.  Return zero on failure (when ``N`` is too big).

   function ada_text_to_locale_string
     (Text : ada_text) return System.Address
      with Export        => True,
           Convention    => C,
           External_name => "ada_text_to_locale_string";
   --  Encode some text using the current locale. The result is dynamically
   --  allocated: it is up to the caller to free it when done with it.
   --
   --  This is a development helper to make it quick and easy to print token
   --  and diagnostic text: it ignores errors (when the locale does not support
   --  some characters). Production code should use real conversion routines
   --  such as libiconv's in order to deal with UTF-32 texts.

   ------------------
   -- File readers --
   ------------------

   function ada_create_file_reader
     (Data         : System.Address;
      Destroy_Func : ada_file_reader_destroy_callback;
      Read_Func    : ada_file_reader_read_callback) return ada_file_reader
      with Export        => True,
           Convention    => C,
           External_name => "ada_create_file_reader";
   --  Create a file reader. When done with it, the result must be passed to
   --  ``ada_dec_ref_file_reader``.
   --
   --  Pass as ``data`` a pointer to hold your private data: it will be passed
   --  to all callbacks below.
   --
   --  ``destroy`` is a callback that is called by ``ada_dec_ref_file_reader``
   --  to leave a chance to free resources that ``data`` may hold.
   --
   --  ``read`` is a callback. For a given filename/charset and whether to read
   --  the BOM (Byte Order Mark), it tries to fetch the contents of the source
   --  file, returned in ``Contents``. If there is an error, it must return it
   --  in ``Diagnostic`` instead.

   procedure ada_dec_ref_file_reader
     (File_Reader : ada_file_reader)
      with Export        => True,
           Convention    => C,
           External_name =>
              "ada_dec_ref_file_reader";
   --  Release an ownership share for this file reader. This destroys the file
   --  reader if there are no shares left.

   


   --------------------
   -- Event handlers --
   --------------------

   function ada_create_event_handler
     (Data                : System.Address;
      Destroy_Func        : ada_event_handler_destroy_callback;
      Unit_Requested_Func : ada_event_handler_unit_requested_callback;
      Unit_Parsed_Func    : ada_event_handler_unit_parsed_callback)
      return ada_event_handler
      with Export        => True,
           Convention    => C,
           External_name => "ada_create_event_handler";
   --  Create an event handler. When done with it, the result must be passed to
   --  ``ada_dec_ref_event_handler``.
   --
   --  Pass as ``data`` a pointer to hold your private data: it will be passed
   --  to all callbacks below.
   --
   --  ``destroy`` is a callback that is called by
   --  ``ada_dec_ref_event_handler`` to leave a chance to free resources that
   --  ``data`` may hold.
   --
   --  ``unit_requested`` is a callback that will be called when a unit is
   --  requested.
   --
   --  .. warning:: Please note that the unit requested callback can be called
   --     *many* times for the same unit, so in all likeliness, those events
   --     should be filtered if they're used to forward diagnostics to the
   --     user.
   --
   --  ``unit_parsed`` is a callback that will be called when a unit is parsed.

   procedure ada_dec_ref_event_handler
     (Handler : ada_event_handler)
      with Export        => True,
           Convention    => C,
           External_name =>
              "ada_dec_ref_event_handler";
   --  Release an ownership share for this event handler. This destroys the
   --  event handler if there are no shares left.

   


   --------------------
   -- Unit providers --
   --------------------

   function ada_create_unit_provider
     (Data                    : System.Address;
      Destroy_Func            : ada_unit_provider_destroy_callback;
      Get_Unit_Filename_Func  : ada_unit_provider_get_unit_filename_callback;
      Get_Unit_From_Name_Func : ada_unit_provider_get_unit_from_name_callback)
      return ada_unit_provider
      with Export        => True,
           Convention    => C,
           External_name => "ada_create_unit_provider";
   --  Create a unit provider. When done with it, the result must be passed to
   --  ``ada_destroy_unit_provider``.
   --
   --  Pass as ``data`` a pointer to hold your private data: it will be passed
   --  to all callbacks below.
   --
   --  ``destroy`` is a callback that is called by
   --  ``ada_destroy_unit_provider`` to leave a chance to free resources that
   --  ``data`` may hold.
   --
   --  ``get_unit_from_node`` is a callback. It turns an analysis unit
   --  reference represented as a node into an analysis unit. It should return
   --  ``NULL`` if the node is not a valid unit name representation.
   --
   --  ``get_unit_from_name`` is a callback similar to ``get_unit_from_node``
   --  except it takes an analysis unit reference represented as a string.

   procedure ada_dec_ref_unit_provider
     (Provider : ada_unit_provider)
      with Export        => True,
           Convention    => C,
           External_name =>
              "ada_dec_ref_unit_provider";
   --  Release an ownership share for this unit provider. This destroys the
   --  unit provider if there are no shares left.

   


   ------------------
   -- Struct types --
   ------------------

         



subtype ada_internal_aspect is Internal_Aspect;
type ada_internal_aspect_Ptr is access Internal_Aspect;



         



subtype ada_internal_completion_item is Internal_Completion_Item;
type ada_internal_completion_item_Ptr is access Internal_Completion_Item;



         



subtype ada_internal_discrete_range is Internal_Discrete_Range;
type ada_internal_discrete_range_Ptr is access Internal_Discrete_Range;



         



subtype ada_internal_discriminant_values is Internal_Discriminant_Values;
type ada_internal_discriminant_values_Ptr is access Internal_Discriminant_Values;



         



subtype ada_internal_doc_annotation is Internal_Doc_Annotation;
type ada_internal_doc_annotation_Ptr is access Internal_Doc_Annotation;

procedure ada_internal_doc_annotation_inc_ref (R : ada_internal_doc_annotation_Ptr)
   with Export        => True,
        Convention    => C,
        External_name => "ada_internal_doc_annotation_inc_ref";
procedure ada_internal_doc_annotation_dec_ref (R : ada_internal_doc_annotation_Ptr)
   with Export        => True,
        Convention    => C,
        External_name => "ada_internal_doc_annotation_dec_ref";


         



subtype ada_internal_param_actual is Internal_Param_Actual;
type ada_internal_param_actual_Ptr is access Internal_Param_Actual;



         



subtype ada_internal_ref_result is Internal_Ref_Result;
type ada_internal_ref_result_Ptr is access Internal_Ref_Result;



         



subtype ada_internal_refd_decl is Internal_Refd_Decl;
type ada_internal_refd_decl_Ptr is access Internal_Refd_Decl;



         



subtype ada_internal_refd_def is Internal_Refd_Def;
type ada_internal_refd_def_Ptr is access Internal_Refd_Def;



         



subtype ada_internal_shape is Internal_Shape;
type ada_internal_shape_Ptr is access Internal_Shape;

procedure ada_internal_shape_inc_ref (R : ada_internal_shape_Ptr)
   with Export        => True,
        Convention    => C,
        External_name => "ada_internal_shape_inc_ref";
procedure ada_internal_shape_dec_ref (R : ada_internal_shape_Ptr)
   with Export        => True,
        Convention    => C,
        External_name => "ada_internal_shape_dec_ref";


         



subtype ada_internal_substitution is Internal_Substitution;
type ada_internal_substitution_Ptr is access Internal_Substitution;

procedure ada_internal_substitution_inc_ref (R : ada_internal_substitution_Ptr)
   with Export        => True,
        Convention    => C,
        External_name => "ada_internal_substitution_inc_ref";
procedure ada_internal_substitution_dec_ref (R : ada_internal_substitution_Ptr)
   with Export        => True,
        Convention    => C,
        External_name => "ada_internal_substitution_dec_ref";



   -----------------
   -- Array types --
   -----------------

         



subtype ada_discriminant_values_array is Internal_Discriminant_Values_Array_Access;
type ada_discriminant_values_array_Ptr is access Internal_Discriminant_Values_Array_Access;

function ada_discriminant_values_array_create (Length : int) return Internal_Discriminant_Values_Array_Access
   with Export        => True,
        Convention    => C,
        External_name => "ada_discriminant_values_array_create";

procedure ada_discriminant_values_array_inc_ref (A : Internal_Discriminant_Values_Array_Access)
   with Export        => True,
        Convention    => C,
        External_name => "ada_discriminant_values_array_inc_ref";

procedure ada_discriminant_values_array_dec_ref (A : Internal_Discriminant_Values_Array_Access)
   with Export        => True,
        Convention    => C,
        External_name => "ada_discriminant_values_array_dec_ref";


         



subtype ada_doc_annotation_array is Internal_Doc_Annotation_Array_Access;
type ada_doc_annotation_array_Ptr is access Internal_Doc_Annotation_Array_Access;

function ada_doc_annotation_array_create (Length : int) return Internal_Doc_Annotation_Array_Access
   with Export        => True,
        Convention    => C,
        External_name => "ada_doc_annotation_array_create";

procedure ada_doc_annotation_array_inc_ref (A : Internal_Doc_Annotation_Array_Access)
   with Export        => True,
        Convention    => C,
        External_name => "ada_doc_annotation_array_inc_ref";

procedure ada_doc_annotation_array_dec_ref (A : Internal_Doc_Annotation_Array_Access)
   with Export        => True,
        Convention    => C,
        External_name => "ada_doc_annotation_array_dec_ref";


         



subtype ada_ada_node_array is Internal_Entity_Array_Access;
type ada_ada_node_array_Ptr is access Internal_Entity_Array_Access;

function ada_ada_node_array_create (Length : int) return Internal_Entity_Array_Access
   with Export        => True,
        Convention    => C,
        External_name => "ada_ada_node_array_create";

procedure ada_ada_node_array_inc_ref (A : Internal_Entity_Array_Access)
   with Export        => True,
        Convention    => C,
        External_name => "ada_ada_node_array_inc_ref";

procedure ada_ada_node_array_dec_ref (A : Internal_Entity_Array_Access)
   with Export        => True,
        Convention    => C,
        External_name => "ada_ada_node_array_dec_ref";


         



subtype ada_param_actual_array is Internal_Param_Actual_Array_Access;
type ada_param_actual_array_Ptr is access Internal_Param_Actual_Array_Access;

function ada_param_actual_array_create (Length : int) return Internal_Param_Actual_Array_Access
   with Export        => True,
        Convention    => C,
        External_name => "ada_param_actual_array_create";

procedure ada_param_actual_array_inc_ref (A : Internal_Param_Actual_Array_Access)
   with Export        => True,
        Convention    => C,
        External_name => "ada_param_actual_array_inc_ref";

procedure ada_param_actual_array_dec_ref (A : Internal_Param_Actual_Array_Access)
   with Export        => True,
        Convention    => C,
        External_name => "ada_param_actual_array_dec_ref";


         



subtype ada_ref_result_array is Internal_Ref_Result_Array_Access;
type ada_ref_result_array_Ptr is access Internal_Ref_Result_Array_Access;

function ada_ref_result_array_create (Length : int) return Internal_Ref_Result_Array_Access
   with Export        => True,
        Convention    => C,
        External_name => "ada_ref_result_array_create";

procedure ada_ref_result_array_inc_ref (A : Internal_Ref_Result_Array_Access)
   with Export        => True,
        Convention    => C,
        External_name => "ada_ref_result_array_inc_ref";

procedure ada_ref_result_array_dec_ref (A : Internal_Ref_Result_Array_Access)
   with Export        => True,
        Convention    => C,
        External_name => "ada_ref_result_array_dec_ref";


         



subtype ada_shape_array is Internal_Shape_Array_Access;
type ada_shape_array_Ptr is access Internal_Shape_Array_Access;

function ada_shape_array_create (Length : int) return Internal_Shape_Array_Access
   with Export        => True,
        Convention    => C,
        External_name => "ada_shape_array_create";

procedure ada_shape_array_inc_ref (A : Internal_Shape_Array_Access)
   with Export        => True,
        Convention    => C,
        External_name => "ada_shape_array_inc_ref";

procedure ada_shape_array_dec_ref (A : Internal_Shape_Array_Access)
   with Export        => True,
        Convention    => C,
        External_name => "ada_shape_array_dec_ref";


         



subtype ada_substitution_array is Internal_Substitution_Array_Access;
type ada_substitution_array_Ptr is access Internal_Substitution_Array_Access;

function ada_substitution_array_create (Length : int) return Internal_Substitution_Array_Access
   with Export        => True,
        Convention    => C,
        External_name => "ada_substitution_array_create";

procedure ada_substitution_array_inc_ref (A : Internal_Substitution_Array_Access)
   with Export        => True,
        Convention    => C,
        External_name => "ada_substitution_array_inc_ref";

procedure ada_substitution_array_dec_ref (A : Internal_Substitution_Array_Access)
   with Export        => True,
        Convention    => C,
        External_name => "ada_substitution_array_dec_ref";


         



subtype ada_analysis_unit_array is Internal_Unit_Array_Access;
type ada_analysis_unit_array_Ptr is access Internal_Unit_Array_Access;

function ada_analysis_unit_array_create (Length : int) return Internal_Unit_Array_Access
   with Export        => True,
        Convention    => C,
        External_name => "ada_analysis_unit_array_create";

procedure ada_analysis_unit_array_inc_ref (A : Internal_Unit_Array_Access)
   with Export        => True,
        Convention    => C,
        External_name => "ada_analysis_unit_array_inc_ref";

procedure ada_analysis_unit_array_dec_ref (A : Internal_Unit_Array_Access)
   with Export        => True,
        Convention    => C,
        External_name => "ada_analysis_unit_array_dec_ref";


         



subtype ada_unbounded_text_type_array is Symbol_Type_Array_Access;
type ada_unbounded_text_type_array_Ptr is access Symbol_Type_Array_Access;

function ada_unbounded_text_type_array_create (Length : int) return Symbol_Type_Array_Access
   with Export        => True,
        Convention    => C,
        External_name => "ada_unbounded_text_type_array_create";

procedure ada_unbounded_text_type_array_inc_ref (A : Symbol_Type_Array_Access)
   with Export        => True,
        Convention    => C,
        External_name => "ada_unbounded_text_type_array_inc_ref";

procedure ada_unbounded_text_type_array_dec_ref (A : Symbol_Type_Array_Access)
   with Export        => True,
        Convention    => C,
        External_name => "ada_unbounded_text_type_array_dec_ref";



   --------------------
   -- Iterator types --
   --------------------

           



subtype ada_completion_item_iterator is Internal_Completion_Item_Iterator_Access;

function ada_completion_item_iterator_next
  (I : Internal_Completion_Item_Iterator_Access;
   E : access Internal_Completion_Item) return int
   with Export        => True,
        Convention    => C,
        External_name => "ada_completion_item_iterator_next";

procedure ada_completion_item_iterator_inc_ref (A : Internal_Completion_Item_Iterator_Access)
   with Export        => True,
        Convention    => C,
        External_name => "ada_completion_item_iterator_inc_ref";

procedure ada_completion_item_iterator_dec_ref (A : Internal_Completion_Item_Iterator_Access)
   with Export        => True,
        Convention    => C,
        External_name => "ada_completion_item_iterator_dec_ref";



   ----------
   -- Misc --
   ----------

   function ada_get_last_exception return ada_exception_Ptr
     with Export        => True,
          Convention    => C,
          External_Name => "ada_get_last_exception";
   --  Return exception information for the last error that happened in the
   --  current thread. Will be automatically allocated on error and free'd on
   --  the next error.

   procedure Clear_Last_Exception;
   --  Free the information contained in Last_Exception

   procedure Set_Last_Exception (Exc : Exception_Occurrence);
   --  Free the information contained in Last_Exception and replace it with
   --  newly allocated information from Exc.

   function ada_token_kind_name (Kind : int) return chars_ptr
      with Export => True,
           Convention => C,
           External_Name => "ada_token_kind_name";
   --  Return a human-readable name for a token kind.
   --
   --  The returned string is dynamically allocated and the caller must free it
   --  when done with it.
   --
   --  If the given kind is invalid, return ``NULL`` and set the last exception
   --  accordingly.

   procedure ada_token_next
     (Token      : ada_token;
      Next_Token : access ada_token)
      with Export        => True,
           Convention    => C,
           External_name => "ada_token_next";
   --  Return a reference to the next token in the corresponding analysis unit.

   procedure ada_token_previous
     (Token          : ada_token;
      Previous_Token : access ada_token)
      with Export        => True,
           Convention    => C,
           External_name => "ada_token_previous";
   --  Return a reference to the previous token in the corresponding analysis
   --  unit.

   function ada_token_range_text
     (First, Last : ada_token;
      Text        : access ada_text) return int
      with Export => True,
           Convention => C,
           External_Name => "ada_token_range_text";
   --  Compute the source buffer slice corresponding to the text that spans
   --  between the ``First`` and ``Last`` tokens (both included). This yields
   --  an empty slice if ``Last`` actually appears before ``First``. Put the
   --  result in ``RESULT``.
   --
   --  This returns ``0`` if ``First`` and ``Last`` don't belong to the same
   --  analysis unit. Return ``1`` if successful.

   function ada_token_is_equivalent
     (Left  : ada_token;
      Right : ada_token) return ada_bool
      with Export        => True,
           Convention    => C,
           External_name => "ada_token_is_equivalent";
   --  Return whether ``L`` and ``R`` are structurally equivalent tokens. This
   --  means that their position in the stream won't be taken into account,
   --  only the kind and text of the token.

   procedure ada_entity_image
     (Ent : ada_base_entity_Ptr; Result : access ada_text)
      with Export        => True,
           Convention    => C,
           External_name => "ada_entity_image";
   --  Return a representation of this entity as a string.

   ---------------------------------------
   -- Kind-specific AST node primitives --
   ---------------------------------------

   --  All these primitives return their result through an OUT parameter. They
   --  return a boolean telling whether the operation was successful (it can
   --  fail if the node does not have the proper type, for instance). When an
   --  AST node is returned, its ref-count is left as-is.

           
   

   
   

   function ada_ada_node_p_declarative_scope
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_ada_node_p_declarative_scope";
   --  Return the scope of definition of this basic declaration.

           
   

   
   

   function ada_ada_node_p_enclosing_compilation_unit
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_ada_node_p_enclosing_compilation_unit";
   --  Return the compilation unit containing this node.
   --
   --  .. note:: This returns the :ada:ref:`Compilation_Unit` node, which is
   --     different from the ``AnalysisUnit``. In particular, an analysis unit
   --     can contain multiple compilation units.

           
   

   
   

   function ada_ada_node_p_get_uninstantiated_node
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_ada_node_p_get_uninstantiated_node";
   --  Assuming this node comes from an instantiated generic declaration,
   --  return its non-instantiated counterpart lying in the generic
   --  declaration.

           
   

   
   

   function ada_ada_node_p_complete
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_completion_item_iterator) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_ada_node_p_complete";
   --  Return possible completions at this point in the file.

           
   

   
   

   function ada_ada_node_p_valid_keywords
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_unbounded_text_type_array) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_ada_node_p_valid_keywords";
   --  Return the list of keywords that are valid at this point in the file.
   --
   --  .. note:: This is work in progress. It will return all keywords for now,
   --     without looking at the context.

           
   

   
   

   function ada_ada_node_p_generic_instantiations
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_ada_node_array) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_ada_node_p_generic_instantiations";
   --  Return the potentially empty list of generic package/subprogram
   --  instantiations that led to the creation of this entity. Outer-most
   --  instantiations appear last.

           
   

   
   

   function ada_ada_node_p_semantic_parent
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_ada_node_p_semantic_parent";
   --  Return the semantic parent for this node, if applicable, null otherwise.
   --
   --  .. note:: A node lying outside of a library item's declaration or
   --     subunit's body does not have a parent environment, meaning that this
   --     property will return null.

           
   

   
   

   function ada_ada_node_p_parent_basic_decl
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_ada_node_p_parent_basic_decl";
   --  Return the parent basic decl for this node, if applicable, null
   --  otherwise.
   --
   --  .. note:: If the parent BasicDecl of the given node is a generic
   --     declaration, this call will return the instantiation from which the
   --     node was retrieved instead, if any.
   --
   --  .. note:: When called on a subunit's body, this property will return the
   --     its corresponding body stub.
   --
   --  .. note:: When called on a node lying outside of a library item's
   --     declaration or subunit's body this property will return null.

           
   

   
   

   function ada_ada_node_p_filter_is_imported_by
     (Node : ada_base_entity_Ptr;

         Units :
            
            ada_analysis_unit_array;
         Transitive :
            
            ada_bool;

      Value_P : access ada_analysis_unit_array) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_ada_node_p_filter_is_imported_by";
   --  Filters out among the list of given units those that cannot refer to the
   --  unit in which this node lies. If transitive is True, the whole
   --  transitive closure of imports will be used to find a reference to the
   --  unit of this node.

           
   

   
   

   function ada_ada_node_p_xref_entry_point
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_bool) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_ada_node_p_xref_entry_point";
   --  Designates entities that are entry point for the xref solving
   --  infrastructure. If this returns true, then resolve_names can be called
   --  on it.
   --
   --  .. note:: For convenience, and unlike what is defined in the ARM wrt.
   --     complete contexts for name resolution, ``xref_entry_points`` can be
   --     nested.

           
   

   
   

   function ada_ada_node_p_resolve_names
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_bool) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_ada_node_p_resolve_names";
   --  This will resolve names for this node. If the operation is successful,
   --  then type_var and ref_var will be bound on appropriate subnodes of the
   --  statement.

           
   

   
   

   function ada_ada_node_p_standard_unit
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_analysis_unit) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_ada_node_p_standard_unit";
   --  Static method. Return the analysis unit corresponding to the Standard
   --  package.

           
   

   
   

   function ada_ada_node_p_std_entity
     (Node : ada_base_entity_Ptr;

         Sym :
            access constant
            ada_symbol_type;

      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_ada_node_p_std_entity";
   --  Static property. Return an entity from the standard package with name
   --  ``sym``.

           
   

   
   

   function ada_ada_node_p_bool_type
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_ada_node_p_bool_type";
   --  Static method. Return the standard Boolean type.

           
   

   
   

   function ada_ada_node_p_int_type
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_ada_node_p_int_type";
   --  Static method. Return the standard Integer type.

           
   

   
   

   function ada_ada_node_p_universal_int_type
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_ada_node_p_universal_int_type";
   --  Static method. Return the standard Universal Integer type.

           
   

   
   

   function ada_ada_node_p_universal_real_type
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_ada_node_p_universal_real_type";
   --  Static method. Return the standard Universal Real type.

           
   

   
   

   function ada_ada_node_p_std_char_type
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_ada_node_p_std_char_type";
   --  Static method. Return the standard Character type.

           
   

   
   

   function ada_ada_node_p_std_wide_char_type
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_ada_node_p_std_wide_char_type";
   --  Static method. Return the standard Wide_Character type.

           
   

   
   

   function ada_ada_node_p_std_wide_wide_char_type
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_ada_node_p_std_wide_wide_char_type";
   --  Static method. Return the standard Wide_Wide_Character type.

           
   

   
   

   function ada_ada_node_p_top_level_decl
     (Node : ada_base_entity_Ptr;

         Unit :
            
            ada_analysis_unit;

      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_ada_node_p_top_level_decl";
   --  Static method. Get the top-level decl in ``unit``.  This is the body of
   --  a Subunit, or the item of a ``LibraryItem``.

           
   

   
   

   function ada_ada_node_p_choice_match
     (Node : ada_base_entity_Ptr;

         Value :
            access constant
            ada_big_integer;

      Value_P : access ada_bool) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_ada_node_p_choice_match";
   --  Assuming that self is a choice expression (such as what can appear in an
   --  alternative of a case statement or in the RHS of a membership
   --  expression, this property returns whether the given value satisfies it.
   --
   --  .. attention:: This is an experimental feature, so even if it is exposed
   --     to allow experiments, it is totally unsupported and the API and
   --     behavior are very likely to change in the future.

           
   

   
   

   function ada_ada_node_p_gnat_xref
     (Node : ada_base_entity_Ptr;

         Imprecise_Fallback :
            
            ada_bool;

      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_ada_node_p_gnat_xref";
   --  Return a cross reference from this name to a defining identifier, trying
   --  to mimic GNAT's xrefs as much as possible.

           
   

   
   

   function ada_ada_node_parent
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_ada_node_parent";
   --  Return the syntactic parent for this node. Return null for the root
   --  node.

           
   

   
   

   function ada_ada_node_parents
     (Node : ada_base_entity_Ptr;

         With_Self :
            
            ada_bool;

      Value_P : access ada_ada_node_array) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_ada_node_parents";
   --  Return an array that contains the lexical parents, this node included
   --  iff ``with_self`` is True. Nearer parents are first in the list.

           
   

   
   

   function ada_ada_node_children
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_ada_node_array) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_ada_node_children";
   --  Return an array that contains the direct lexical children.
   --
   --  .. warning:: This constructs a whole array every-time you call it, and
   --     as such is less efficient than calling the ``Child`` built-in.

           
   

   
   

   function ada_ada_node_token_start
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_token) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_ada_node_token_start";
   --  Return the first token used to parse this node.

           
   

   
   

   function ada_ada_node_token_end
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_token) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_ada_node_token_end";
   --  Return the last token used to parse this node.

           
   

   
   

   function ada_ada_node_child_index
     (Node : ada_base_entity_Ptr;


      Value_P : access int) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_ada_node_child_index";
   --  Return the 0-based index for Node in its parent's children.

           
   

   
   

   function ada_ada_node_previous_sibling
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_ada_node_previous_sibling";
   --  Return the node's previous sibling, or null if there is no such sibling.

           
   

   
   

   function ada_ada_node_next_sibling
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_ada_node_next_sibling";
   --  Return the node's next sibling, or null if there is no such sibling.

           
   

   
   

   function ada_ada_node_unit
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_analysis_unit) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_ada_node_unit";
   --  Return the analysis unit owning this node.

           
   

   
   

   function ada_ada_node_is_ghost
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_bool) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_ada_node_is_ghost";
   --  Return whether the node is a ghost.
   --
   --  Unlike regular nodes, ghost nodes cover no token in the input source:
   --  they are logically located instead between two tokens. Both the
   --  ``token_start`` and the ``token_end`` of all ghost nodes is the token
   --  right after this logical position.

           
   

   
   

   function ada_ada_node_full_sloc_image
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_string_type) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_ada_node_full_sloc_image";
   --  Return a string containing the filename + the sloc in GNU conformant
   --  format. Useful to create diagnostics from a node.

           
   

   
   

   function ada_abort_node_p_as_bool
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_bool) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_abort_node_p_as_bool";
   --  Return whether this is an instance of AbortPresent

           
   

   
   

   function ada_abstract_node_p_as_bool
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_bool) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_abstract_node_p_as_bool";
   --  Return whether this is an instance of AbstractPresent

           
   

   
   

   function ada_assoc_list_p_zip_with_params
     (Node : ada_base_entity_Ptr;

         Imprecise_Fallback :
            
            ada_bool;

      Value_P : access ada_param_actual_array) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_assoc_list_p_zip_with_params";
   --  Returns an array of pairs, associating formal parameters to actual
   --  expressions. The formals to match are retrieved by resolving the call
   --  which this AssocList represents the actuals of.

           
   

   
   

   function ada_aliased_node_p_as_bool
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_bool) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_aliased_node_p_as_bool";
   --  Return whether this is an instance of AliasedPresent

           
   

   
   

   function ada_all_node_p_as_bool
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_bool) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_all_node_p_as_bool";
   --  Return whether this is an instance of AllPresent

           
   

   
   

   function ada_constrained_array_indices_f_list
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_constrained_array_indices_f_list";
   --  This field contains a list that itself contains one of the following
   --  nodes: :ada:ref:`Attribute_Ref`, :ada:ref:`Bin_Op`,
   --  :ada:ref:`Call_Expr`, :ada:ref:`Char_Literal`, :ada:ref:`Dotted_Name`,
   --  :ada:ref:`Explicit_Deref`, :ada:ref:`Identifier`, :ada:ref:`Qual_Expr`,
   --  :ada:ref:`Reduce_Attribute_Ref`, :ada:ref:`String_Literal`,
   --  :ada:ref:`Subtype_Indication`, :ada:ref:`Target_Name`,
   --  :ada:ref:`Update_Attribute_Ref`

           
   

   
   

   function ada_unconstrained_array_indices_f_types
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_unconstrained_array_indices_f_types";
   

           
   

   
   

   function ada_aspect_assoc_f_id
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_aspect_assoc_f_id";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Attribute_Ref`, :ada:ref:`Call_Expr`, :ada:ref:`Char_Literal`,
   --  :ada:ref:`Dotted_Name`, :ada:ref:`Explicit_Deref`,
   --  :ada:ref:`Identifier`, :ada:ref:`Qual_Expr`,
   --  :ada:ref:`Reduce_Attribute_Ref`, :ada:ref:`String_Literal`,
   --  :ada:ref:`Target_Name`, :ada:ref:`Update_Attribute_Ref`

           
   

   
   

   function ada_aspect_assoc_f_expr
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_aspect_assoc_f_expr";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Abstract_State_Decl_Expr`, :ada:ref:`Allocator`,
   --  :ada:ref:`Attribute_Ref`, :ada:ref:`Base_Aggregate`, :ada:ref:`Bin_Op`,
   --  :ada:ref:`Call_Expr`, :ada:ref:`Char_Literal`, :ada:ref:`Concat_Op`,
   --  :ada:ref:`Cond_Expr`, :ada:ref:`Contract_Cases`, :ada:ref:`Decl_Expr`,
   --  :ada:ref:`Dotted_Name`, :ada:ref:`Explicit_Deref`,
   --  :ada:ref:`Identifier`, :ada:ref:`Membership_Expr`,
   --  :ada:ref:`Null_Literal`, :ada:ref:`Num_Literal`, :ada:ref:`Paren_Expr`,
   --  :ada:ref:`Qual_Expr`, :ada:ref:`Quantified_Expr`, :ada:ref:`Raise_Expr`,
   --  :ada:ref:`Reduce_Attribute_Ref`, :ada:ref:`String_Literal`,
   --  :ada:ref:`Target_Name`, :ada:ref:`Un_Op`,
   --  :ada:ref:`Update_Attribute_Ref`

           
   

   
   

   function ada_aspect_assoc_p_is_ghost_code
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_bool) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_aspect_assoc_p_is_ghost_code";
   --  Return whether this aspect is ghost code or not. See SPARK RM 6.9.

           
   

   
   

   function ada_at_clause_f_name
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_at_clause_f_name";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Char_Literal`, :ada:ref:`Identifier`,
   --  :ada:ref:`String_Literal`

           
   

   
   

   function ada_at_clause_f_expr
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_at_clause_f_expr";
   --  This field can contain one of the following nodes: :ada:ref:`Allocator`,
   --  :ada:ref:`Attribute_Ref`, :ada:ref:`Base_Aggregate`, :ada:ref:`Bin_Op`,
   --  :ada:ref:`Call_Expr`, :ada:ref:`Char_Literal`, :ada:ref:`Concat_Op`,
   --  :ada:ref:`Cond_Expr`, :ada:ref:`Decl_Expr`, :ada:ref:`Dotted_Name`,
   --  :ada:ref:`Explicit_Deref`, :ada:ref:`Identifier`,
   --  :ada:ref:`Membership_Expr`, :ada:ref:`Null_Literal`,
   --  :ada:ref:`Num_Literal`, :ada:ref:`Paren_Expr`, :ada:ref:`Qual_Expr`,
   --  :ada:ref:`Quantified_Expr`, :ada:ref:`Raise_Expr`,
   --  :ada:ref:`Reduce_Attribute_Ref`, :ada:ref:`String_Literal`,
   --  :ada:ref:`Target_Name`, :ada:ref:`Un_Op`,
   --  :ada:ref:`Update_Attribute_Ref`

           
   

   
   

   function ada_attribute_def_clause_f_attribute_expr
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_attribute_def_clause_f_attribute_expr";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Attribute_Ref`, :ada:ref:`Call_Expr`, :ada:ref:`Char_Literal`,
   --  :ada:ref:`Dotted_Name`, :ada:ref:`Explicit_Deref`,
   --  :ada:ref:`Identifier`, :ada:ref:`Qual_Expr`,
   --  :ada:ref:`Reduce_Attribute_Ref`, :ada:ref:`String_Literal`,
   --  :ada:ref:`Target_Name`, :ada:ref:`Update_Attribute_Ref`

           
   

   
   

   function ada_attribute_def_clause_f_expr
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_attribute_def_clause_f_expr";
   --  This field can contain one of the following nodes: :ada:ref:`Allocator`,
   --  :ada:ref:`Attribute_Ref`, :ada:ref:`Base_Aggregate`, :ada:ref:`Bin_Op`,
   --  :ada:ref:`Call_Expr`, :ada:ref:`Char_Literal`, :ada:ref:`Concat_Op`,
   --  :ada:ref:`Cond_Expr`, :ada:ref:`Decl_Expr`, :ada:ref:`Dotted_Name`,
   --  :ada:ref:`Explicit_Deref`, :ada:ref:`Identifier`,
   --  :ada:ref:`Membership_Expr`, :ada:ref:`Null_Literal`,
   --  :ada:ref:`Num_Literal`, :ada:ref:`Paren_Expr`, :ada:ref:`Qual_Expr`,
   --  :ada:ref:`Quantified_Expr`, :ada:ref:`Raise_Expr`,
   --  :ada:ref:`Reduce_Attribute_Ref`, :ada:ref:`String_Literal`,
   --  :ada:ref:`Target_Name`, :ada:ref:`Un_Op`,
   --  :ada:ref:`Update_Attribute_Ref`

           
   

   
   

   function ada_enum_rep_clause_f_type_name
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_enum_rep_clause_f_type_name";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Char_Literal`, :ada:ref:`Dotted_Name`, :ada:ref:`Identifier`,
   --  :ada:ref:`String_Literal`

           
   

   
   

   function ada_enum_rep_clause_f_aggregate
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_enum_rep_clause_f_aggregate";
   

           
   

   
   

   function ada_enum_rep_clause_p_params
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_param_actual_array) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_enum_rep_clause_p_params";
   --  Returns an array of pairs, associating enum literals to representation
   --  clause actuals.

           
   

   
   

   function ada_record_rep_clause_f_name
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_record_rep_clause_f_name";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Char_Literal`, :ada:ref:`Dotted_Name`, :ada:ref:`Identifier`,
   --  :ada:ref:`String_Literal`

           
   

   
   

   function ada_record_rep_clause_f_at_expr
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_record_rep_clause_f_at_expr";
   --  This field can contain one of the following nodes: :ada:ref:`Allocator`,
   --  :ada:ref:`Attribute_Ref`, :ada:ref:`Base_Aggregate`, :ada:ref:`Bin_Op`,
   --  :ada:ref:`Call_Expr`, :ada:ref:`Char_Literal`, :ada:ref:`Concat_Op`,
   --  :ada:ref:`Cond_Expr`, :ada:ref:`Decl_Expr`, :ada:ref:`Dotted_Name`,
   --  :ada:ref:`Explicit_Deref`, :ada:ref:`Identifier`,
   --  :ada:ref:`Null_Literal`, :ada:ref:`Num_Literal`, :ada:ref:`Paren_Expr`,
   --  :ada:ref:`Qual_Expr`, :ada:ref:`Quantified_Expr`, :ada:ref:`Raise_Expr`,
   --  :ada:ref:`Reduce_Attribute_Ref`, :ada:ref:`String_Literal`,
   --  :ada:ref:`Target_Name`, :ada:ref:`Un_Op`,
   --  :ada:ref:`Update_Attribute_Ref`

           
   

   
   

   function ada_record_rep_clause_f_components
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_record_rep_clause_f_components";
   --  This field contains a list that itself contains one of the following
   --  nodes: :ada:ref:`Component_Clause`, :ada:ref:`Pragma_Node`

           
   

   
   

   function ada_aspect_spec_f_aspect_assocs
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_aspect_spec_f_aspect_assocs";
   

           
   

   
   

   function ada_base_assoc_p_assoc_expr
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_base_assoc_p_assoc_expr";
   --  Returns the expression side of this assoc node.

           
   

   
   

   function ada_contract_case_assoc_f_guard
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_contract_case_assoc_f_guard";
   --  This field can contain one of the following nodes: :ada:ref:`Allocator`,
   --  :ada:ref:`Attribute_Ref`, :ada:ref:`Base_Aggregate`, :ada:ref:`Bin_Op`,
   --  :ada:ref:`Call_Expr`, :ada:ref:`Char_Literal`, :ada:ref:`Concat_Op`,
   --  :ada:ref:`Cond_Expr`, :ada:ref:`Decl_Expr`, :ada:ref:`Dotted_Name`,
   --  :ada:ref:`Explicit_Deref`, :ada:ref:`Identifier`,
   --  :ada:ref:`Membership_Expr`, :ada:ref:`Null_Literal`,
   --  :ada:ref:`Num_Literal`, :ada:ref:`Others_Designator`,
   --  :ada:ref:`Paren_Expr`, :ada:ref:`Qual_Expr`, :ada:ref:`Quantified_Expr`,
   --  :ada:ref:`Raise_Expr`, :ada:ref:`Reduce_Attribute_Ref`,
   --  :ada:ref:`String_Literal`, :ada:ref:`Target_Name`, :ada:ref:`Un_Op`,
   --  :ada:ref:`Update_Attribute_Ref`

           
   

   
   

   function ada_contract_case_assoc_f_consequence
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_contract_case_assoc_f_consequence";
   --  This field can contain one of the following nodes: :ada:ref:`Allocator`,
   --  :ada:ref:`Attribute_Ref`, :ada:ref:`Base_Aggregate`, :ada:ref:`Bin_Op`,
   --  :ada:ref:`Call_Expr`, :ada:ref:`Char_Literal`, :ada:ref:`Concat_Op`,
   --  :ada:ref:`Cond_Expr`, :ada:ref:`Decl_Expr`, :ada:ref:`Dotted_Name`,
   --  :ada:ref:`Explicit_Deref`, :ada:ref:`Identifier`,
   --  :ada:ref:`Membership_Expr`, :ada:ref:`Null_Literal`,
   --  :ada:ref:`Num_Literal`, :ada:ref:`Paren_Expr`, :ada:ref:`Qual_Expr`,
   --  :ada:ref:`Quantified_Expr`, :ada:ref:`Raise_Expr`,
   --  :ada:ref:`Reduce_Attribute_Ref`, :ada:ref:`String_Literal`,
   --  :ada:ref:`Target_Name`, :ada:ref:`Un_Op`,
   --  :ada:ref:`Update_Attribute_Ref`

           
   

   
   

   function ada_pragma_argument_assoc_f_name
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_pragma_argument_assoc_f_name";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Attribute_Ref`, :ada:ref:`Identifier`

           
   

   
   

   function ada_pragma_argument_assoc_f_expr
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_pragma_argument_assoc_f_expr";
   --  This field can contain one of the following nodes: :ada:ref:`Allocator`,
   --  :ada:ref:`Attribute_Ref`, :ada:ref:`Base_Aggregate`, :ada:ref:`Bin_Op`,
   --  :ada:ref:`Call_Expr`, :ada:ref:`Char_Literal`, :ada:ref:`Concat_Op`,
   --  :ada:ref:`Cond_Expr`, :ada:ref:`Decl_Expr`, :ada:ref:`Dotted_Name`,
   --  :ada:ref:`Explicit_Deref`, :ada:ref:`Identifier`,
   --  :ada:ref:`Membership_Expr`, :ada:ref:`Null_Literal`,
   --  :ada:ref:`Num_Literal`, :ada:ref:`Paren_Expr`, :ada:ref:`Qual_Expr`,
   --  :ada:ref:`Quantified_Expr`, :ada:ref:`Raise_Expr`,
   --  :ada:ref:`Reduce_Attribute_Ref`, :ada:ref:`String_Literal`,
   --  :ada:ref:`Target_Name`, :ada:ref:`Un_Op`,
   --  :ada:ref:`Update_Attribute_Ref`

           
   

   
   

   function ada_base_formal_param_holder_p_abstract_formal_params
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_ada_node_array) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_base_formal_param_holder_p_abstract_formal_params";
   --  Return the list of abstract formal parameters for this holder.

           
   

   
   

   function ada_base_formal_param_holder_p_formal_params
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_ada_node_array) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_base_formal_param_holder_p_formal_params";
   --  Return all parameters as a ``DefiningName`` array. This property doesn't
   --  return record discriminants nor variants when called on a record
   --  component list.

           
   

   
   

   function ada_base_formal_param_holder_p_nb_min_params
     (Node : ada_base_entity_Ptr;


      Value_P : access int) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_base_formal_param_holder_p_nb_min_params";
   --  Return the minimum number of parameters this subprogram can be called
   --  while still being a legal call.

           
   

   
   

   function ada_base_formal_param_holder_p_nb_max_params
     (Node : ada_base_entity_Ptr;


      Value_P : access int) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_base_formal_param_holder_p_nb_max_params";
   --  Return the maximum number of parameters this subprogram can be called
   --  while still being a legal call.

           
   

   
   

   function ada_base_formal_param_holder_p_param_types
     (Node : ada_base_entity_Ptr;

         Origin :
            access constant
            ada_base_entity;

      Value_P : access ada_ada_node_array) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_base_formal_param_holder_p_param_types";
   --  Returns the type of each parameter of Self.

           
   

   
   

   function ada_base_subp_spec_p_returns
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_base_subp_spec_p_returns";
   --  Syntax property. Return the type expression node corresponding to the
   --  return of this subprogram spec.

           
   

   
   

   function ada_base_subp_spec_p_params
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_ada_node_array) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_base_subp_spec_p_params";
   --  Returns the array of parameters specification for this subprogram spec.

           
   

   
   

   function ada_base_subp_spec_p_primitive_subp_types
     (Node : ada_base_entity_Ptr;

         Imprecise_Fallback :
            
            ada_bool;

      Value_P : access ada_ada_node_array) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_base_subp_spec_p_primitive_subp_types";
   --  Return the types of which this subprogram is a primitive of.

           
   

   
   

   function ada_base_subp_spec_p_primitive_subp_first_type
     (Node : ada_base_entity_Ptr;

         Imprecise_Fallback :
            
            ada_bool;

      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_base_subp_spec_p_primitive_subp_first_type";
   --  Return the first type of which this subprogram is a primitive of.

           
   

   
   

   function ada_base_subp_spec_p_primitive_subp_tagged_type
     (Node : ada_base_entity_Ptr;

         Imprecise_Fallback :
            
            ada_bool;

      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_base_subp_spec_p_primitive_subp_tagged_type";
   --  If this subprogram is a primitive for a tagged type, then return this
   --  type.

           
   

   
   

   function ada_base_subp_spec_p_return_type
     (Node : ada_base_entity_Ptr;

         Origin :
            access constant
            ada_base_entity;

      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_base_subp_spec_p_return_type";
   --  Returns the return type of Self, if applicable (e.g. if Self is a
   --  subprogram). Else, returns null.

           
   

   
   

   function ada_entry_spec_f_entry_name
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_entry_spec_f_entry_name";
   

           
   

   
   

   function ada_entry_spec_f_family_type
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_entry_spec_f_family_type";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Attribute_Ref`, :ada:ref:`Bin_Op`, :ada:ref:`Call_Expr`,
   --  :ada:ref:`Char_Literal`, :ada:ref:`Dotted_Name`,
   --  :ada:ref:`Explicit_Deref`, :ada:ref:`Identifier`, :ada:ref:`Qual_Expr`,
   --  :ada:ref:`Reduce_Attribute_Ref`, :ada:ref:`String_Literal`,
   --  :ada:ref:`Subtype_Indication`, :ada:ref:`Target_Name`,
   --  :ada:ref:`Update_Attribute_Ref`

           
   

   
   

   function ada_entry_spec_f_entry_params
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_entry_spec_f_entry_params";
   

           
   

   
   

   function ada_subp_spec_f_subp_kind
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_subp_spec_f_subp_kind";
   

           
   

   
   

   function ada_subp_spec_f_subp_name
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_subp_spec_f_subp_name";
   

           
   

   
   

   function ada_subp_spec_f_subp_params
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_subp_spec_f_subp_params";
   

           
   

   
   

   function ada_subp_spec_f_subp_returns
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_subp_spec_f_subp_returns";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Anonymous_Type`, :ada:ref:`Subtype_Indication`

           
   

   
   

   function ada_synthetic_binary_spec_f_left_param
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_synthetic_binary_spec_f_left_param";
   

           
   

   
   

   function ada_synthetic_binary_spec_f_right_param
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_synthetic_binary_spec_f_right_param";
   

           
   

   
   

   function ada_synthetic_binary_spec_f_return_type_expr
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_synthetic_binary_spec_f_return_type_expr";
   

           
   

   
   

   function ada_synthetic_unary_spec_f_right_param
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_synthetic_unary_spec_f_right_param";
   

           
   

   
   

   function ada_synthetic_unary_spec_f_return_type_expr
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_synthetic_unary_spec_f_return_type_expr";
   

           
   

   
   

   function ada_component_list_f_components
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_component_list_f_components";
   --  This field contains a list that itself contains one of the following
   --  nodes: :ada:ref:`Aspect_Clause`, :ada:ref:`Component_Decl`,
   --  :ada:ref:`Null_Component_Decl`, :ada:ref:`Pragma_Node`

           
   

   
   

   function ada_component_list_f_variant_part
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_component_list_f_variant_part";
   

           
   

   
   

   function ada_known_discriminant_part_f_discr_specs
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_known_discriminant_part_f_discr_specs";
   

           
   

   
   

   function ada_entry_completion_formal_params_f_params
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_entry_completion_formal_params_f_params";
   

           
   

   
   

   function ada_generic_formal_part_f_decls
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_generic_formal_part_f_decls";
   --  This field contains a list that itself contains one of the following
   --  nodes: :ada:ref:`Generic_Formal`, :ada:ref:`Pragma_Node`,
   --  :ada:ref:`Use_Clause`

           
   

   
   

   function ada_base_record_def_f_components
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_base_record_def_f_components";
   

           
   

   
   

   function ada_basic_assoc_p_get_params
     (Node : ada_base_entity_Ptr;

         Imprecise_Fallback :
            
            ada_bool;

      Value_P : access ada_ada_node_array) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_basic_assoc_p_get_params";
   --  Return the list of parameters that this association refers to.

           
   

   
   

   function ada_aggregate_assoc_f_designators
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_aggregate_assoc_f_designators";
   --  This field contains a list that itself contains one of the following
   --  nodes: :ada:ref:`Allocator`, :ada:ref:`Attribute_Ref`,
   --  :ada:ref:`Base_Aggregate`, :ada:ref:`Bin_Op`, :ada:ref:`Call_Expr`,
   --  :ada:ref:`Char_Literal`, :ada:ref:`Concat_Op`, :ada:ref:`Cond_Expr`,
   --  :ada:ref:`Decl_Expr`, :ada:ref:`Discrete_Subtype_Indication`,
   --  :ada:ref:`Dotted_Name`, :ada:ref:`Explicit_Deref`,
   --  :ada:ref:`Identifier`, :ada:ref:`Membership_Expr`,
   --  :ada:ref:`Null_Literal`, :ada:ref:`Num_Literal`,
   --  :ada:ref:`Others_Designator`, :ada:ref:`Paren_Expr`,
   --  :ada:ref:`Qual_Expr`, :ada:ref:`Quantified_Expr`, :ada:ref:`Raise_Expr`,
   --  :ada:ref:`Reduce_Attribute_Ref`, :ada:ref:`String_Literal`,
   --  :ada:ref:`Target_Name`, :ada:ref:`Un_Op`,
   --  :ada:ref:`Update_Attribute_Ref`

           
   

   
   

   function ada_aggregate_assoc_f_r_expr
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_aggregate_assoc_f_r_expr";
   --  This field can contain one of the following nodes: :ada:ref:`Allocator`,
   --  :ada:ref:`Attribute_Ref`, :ada:ref:`Base_Aggregate`, :ada:ref:`Bin_Op`,
   --  :ada:ref:`Box_Expr`, :ada:ref:`Call_Expr`, :ada:ref:`Char_Literal`,
   --  :ada:ref:`Concat_Op`, :ada:ref:`Cond_Expr`, :ada:ref:`Decl_Expr`,
   --  :ada:ref:`Dotted_Name`, :ada:ref:`Explicit_Deref`,
   --  :ada:ref:`Identifier`, :ada:ref:`Membership_Expr`,
   --  :ada:ref:`Null_Literal`, :ada:ref:`Num_Literal`, :ada:ref:`Paren_Expr`,
   --  :ada:ref:`Qual_Expr`, :ada:ref:`Quantified_Expr`, :ada:ref:`Raise_Expr`,
   --  :ada:ref:`Reduce_Attribute_Ref`, :ada:ref:`String_Literal`,
   --  :ada:ref:`Target_Name`, :ada:ref:`Un_Op`,
   --  :ada:ref:`Update_Attribute_Ref`

           
   

   
   

   function ada_composite_constraint_assoc_f_ids
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_composite_constraint_assoc_f_ids";
   

           
   

   
   

   function ada_composite_constraint_assoc_f_constraint_expr
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_composite_constraint_assoc_f_constraint_expr";
   --  This field can contain one of the following nodes: :ada:ref:`Allocator`,
   --  :ada:ref:`Attribute_Ref`, :ada:ref:`Base_Aggregate`, :ada:ref:`Bin_Op`,
   --  :ada:ref:`Call_Expr`, :ada:ref:`Char_Literal`, :ada:ref:`Concat_Op`,
   --  :ada:ref:`Cond_Expr`, :ada:ref:`Decl_Expr`,
   --  :ada:ref:`Discrete_Subtype_Indication`, :ada:ref:`Dotted_Name`,
   --  :ada:ref:`Explicit_Deref`, :ada:ref:`Identifier`,
   --  :ada:ref:`Membership_Expr`, :ada:ref:`Null_Literal`,
   --  :ada:ref:`Num_Literal`, :ada:ref:`Paren_Expr`, :ada:ref:`Qual_Expr`,
   --  :ada:ref:`Quantified_Expr`, :ada:ref:`Raise_Expr`,
   --  :ada:ref:`Reduce_Attribute_Ref`, :ada:ref:`String_Literal`,
   --  :ada:ref:`Target_Name`, :ada:ref:`Un_Op`,
   --  :ada:ref:`Update_Attribute_Ref`

           
   

   
   

   function ada_iterated_assoc_f_spec
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_iterated_assoc_f_spec";
   

           
   

   
   

   function ada_iterated_assoc_f_r_expr
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_iterated_assoc_f_r_expr";
   --  This field can contain one of the following nodes: :ada:ref:`Allocator`,
   --  :ada:ref:`Attribute_Ref`, :ada:ref:`Base_Aggregate`, :ada:ref:`Bin_Op`,
   --  :ada:ref:`Call_Expr`, :ada:ref:`Char_Literal`, :ada:ref:`Concat_Op`,
   --  :ada:ref:`Cond_Expr`, :ada:ref:`Decl_Expr`, :ada:ref:`Dotted_Name`,
   --  :ada:ref:`Explicit_Deref`, :ada:ref:`Identifier`,
   --  :ada:ref:`Membership_Expr`, :ada:ref:`Null_Literal`,
   --  :ada:ref:`Num_Literal`, :ada:ref:`Paren_Expr`, :ada:ref:`Qual_Expr`,
   --  :ada:ref:`Quantified_Expr`, :ada:ref:`Raise_Expr`,
   --  :ada:ref:`Reduce_Attribute_Ref`, :ada:ref:`String_Literal`,
   --  :ada:ref:`Target_Name`, :ada:ref:`Un_Op`,
   --  :ada:ref:`Update_Attribute_Ref`

           
   

   
   

   function ada_param_assoc_f_designator
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_param_assoc_f_designator";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Identifier`, :ada:ref:`Others_Designator`,
   --  :ada:ref:`String_Literal`

           
   

   
   

   function ada_param_assoc_f_r_expr
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_param_assoc_f_r_expr";
   --  This field can contain one of the following nodes: :ada:ref:`Allocator`,
   --  :ada:ref:`Attribute_Ref`, :ada:ref:`Base_Aggregate`, :ada:ref:`Bin_Op`,
   --  :ada:ref:`Box_Expr`, :ada:ref:`Call_Expr`, :ada:ref:`Char_Literal`,
   --  :ada:ref:`Concat_Op`, :ada:ref:`Cond_Expr`, :ada:ref:`Decl_Expr`,
   --  :ada:ref:`Dotted_Name`, :ada:ref:`Explicit_Deref`,
   --  :ada:ref:`Identifier`, :ada:ref:`Membership_Expr`,
   --  :ada:ref:`Null_Literal`, :ada:ref:`Num_Literal`, :ada:ref:`Paren_Expr`,
   --  :ada:ref:`Qual_Expr`, :ada:ref:`Quantified_Expr`, :ada:ref:`Raise_Expr`,
   --  :ada:ref:`Reduce_Attribute_Ref`, :ada:ref:`String_Literal`,
   --  :ada:ref:`Target_Name`, :ada:ref:`Un_Op`,
   --  :ada:ref:`Update_Attribute_Ref`

           
   

   
   

   function ada_basic_decl_p_is_formal
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_bool) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_basic_decl_p_is_formal";
   --  Whether this decl is the nested decl of a generic formal declaration.

           
   

   
   

   function ada_basic_decl_p_doc_annotations
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_doc_annotation_array) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_basic_decl_p_doc_annotations";
   --  Return the documentation annotations associated with this decl.
   --  Annotations are any comment line of the form:
   --
   --  .. code::
   --
   --     --% [annotation_name]: [annotation]
   --
   --  Raises a property error if the doc is incorrectly formatted.
   --
   --  .. attention:: This is an experimental feature, so even if it is exposed
   --     to allow experiments, it is totally unsupported and the API and
   --     behavior are very likely to change in the future.

           
   

   
   

   function ada_basic_decl_p_doc
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_string_type) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_basic_decl_p_doc";
   --  Return the documentation associated with this decl. Raises a property
   --  error if the doc is incorrectly formatted.
   --
   --  .. attention:: This is an experimental feature, so even if it is exposed
   --     to allow experiments, it is totally unsupported and the API and
   --     behavior are very likely to change in the future.

           
   

   
   

   function ada_basic_decl_p_previous_part_for_decl
     (Node : ada_base_entity_Ptr;

         Imprecise_Fallback :
            
            ada_bool;

      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_basic_decl_p_previous_part_for_decl";
   --  Return the previous part for this decl, if applicable.
   --
   --  .. note:: It is not named previous_part, because BaseTypeDecl has a more
   --     precise version of previous_part that returns a BaseTypeDecl.
   --     Probably, we want to rename the specific versions, and have the root
   --     property be named previous_part. (TODO R925-008)

           
   

   
   

   function ada_basic_decl_p_canonical_part
     (Node : ada_base_entity_Ptr;

         Imprecise_Fallback :
            
            ada_bool;

      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_basic_decl_p_canonical_part";
   --  Return the canonical part for this decl. In the case of decls composed
   --  of several parts, the canonical part will be the first part.

           
   

   
   

   function ada_basic_decl_p_all_parts
     (Node : ada_base_entity_Ptr;

         Imprecise_Fallback :
            
            ada_bool;

      Value_P : access ada_ada_node_array) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_basic_decl_p_all_parts";
   --  Return all parts that define this entity, sorted from first part to last
   --  part.

           
   

   
   

   function ada_basic_decl_p_is_static_decl
     (Node : ada_base_entity_Ptr;

         Imprecise_Fallback :
            
            ada_bool;

      Value_P : access ada_bool) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_basic_decl_p_is_static_decl";
   --  Return whether this declaration is static.

           
   

   
   

   function ada_basic_decl_f_aspects
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_basic_decl_f_aspects";
   --  Return the list of aspects that are attached to this node.

           
   

   
   

   function ada_basic_decl_p_get_aspect_assoc
     (Node : ada_base_entity_Ptr;

         Name :
            access constant
            ada_symbol_type;

      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_basic_decl_p_get_aspect_assoc";
   --  Return the aspect with name ``name`` for this entity.

           
   

   
   

   function ada_basic_decl_p_get_aspect_spec_expr
     (Node : ada_base_entity_Ptr;

         Name :
            access constant
            ada_symbol_type;

      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_basic_decl_p_get_aspect_spec_expr";
   --  Return the expression associated to the aspect with name ``name`` for
   --  this entity.

           
   

   
   

   function ada_basic_decl_p_get_aspect
     (Node : ada_base_entity_Ptr;

         Name :
            access constant
            ada_symbol_type;
         Imprecise_Fallback :
            
            ada_bool;

      Value_P : access ada_internal_aspect) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_basic_decl_p_get_aspect";
   --  Return the aspect with name ``name`` associated to this entity.
   --
   --  Aspects are properties of entities that can be specified by the Ada
   --  program, either via aspect specifications, pragmas, or attributes.
   --
   --  This will return the syntactic node corresponding to attribute directly.
   --
   --  Note: for some aspects (e.g. Inline), Libadalang will check if they are
   --  defined on any part of the entity.

           
   

   
   

   function ada_basic_decl_p_has_aspect
     (Node : ada_base_entity_Ptr;

         Name :
            access constant
            ada_symbol_type;
         Imprecise_Fallback :
            
            ada_bool;

      Value_P : access ada_bool) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_basic_decl_p_has_aspect";
   --  Returns whether the boolean aspect named ``name`` is set on the entity
   --  represented by this node.
   --
   --  "Aspect" is used as in RM terminology (see :rmlink:`13`).

           
   

   
   

   function ada_basic_decl_p_get_pragma
     (Node : ada_base_entity_Ptr;

         Name :
            access constant
            ada_symbol_type;

      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_basic_decl_p_get_pragma";
   --  Return the pragma with name ``name`` associated to this entity.
   --
   --  Please use the ``p_get_aspects`` property instead if you are interested
   --  in aspects, i.e. information that can be represented by either aspect
   --  specification nodes, pragma nodes or attribute definition nodes.

           
   

   
   

   function ada_basic_decl_p_get_representation_clause
     (Node : ada_base_entity_Ptr;

         Name :
            access constant
            ada_symbol_type;
         Imprecise_Fallback :
            
            ada_bool;

      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_basic_decl_p_get_representation_clause";
   --  Return the representation clause associated to this type decl that
   --  defines the given attribute name.

           
   

   
   

   function ada_basic_decl_p_get_at_clause
     (Node : ada_base_entity_Ptr;

         Imprecise_Fallback :
            
            ada_bool;

      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_basic_decl_p_get_at_clause";
   --  Return the at clause associated to this declaration.

           
   

   
   

   function ada_basic_decl_p_is_imported
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_bool) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_basic_decl_p_is_imported";
   --  Whether this declaration is imported from another language.

           
   

   
   

   function ada_basic_decl_p_is_ghost_code
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_bool) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_basic_decl_p_is_ghost_code";
   --  Return whether this declaration is ghost code or not. See SPARK RM 6.9.

           
   

   
   

   function ada_basic_decl_p_is_compilation_unit_root
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_bool) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_basic_decl_p_is_compilation_unit_root";
   --  Whether a BasicDecl is the root decl for its unit.

           
   

   
   

   function ada_basic_decl_p_is_visible
     (Node : ada_base_entity_Ptr;

         From_Node :
            access constant
            ada_base_entity;

      Value_P : access ada_bool) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_basic_decl_p_is_visible";
   --  Return whether this declaration is visible from the point of view of the
   --  given ``origin`` node.
   --
   --  .. attention:: Only package-level (public or private) declarations are
   --     supported for now.

           
   

   
   

   function ada_basic_decl_p_base_subp_declarations
     (Node : ada_base_entity_Ptr;

         Imprecise_Fallback :
            
            ada_bool;

      Value_P : access ada_ada_node_array) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_basic_decl_p_base_subp_declarations";
   --  If Self declares a primitive subprogram of some tagged type T, return
   --  the set of all subprogram declarations that it overrides (including
   --  itself).
   --
   --  .. note:: for the moment this only works for tagged types. Remains to be
   --     seen if we need to extend it.

           
   

   
   

   function ada_basic_decl_p_root_subp_declarations
     (Node : ada_base_entity_Ptr;

         Origin :
            access constant
            ada_base_entity;
         Imprecise_Fallback :
            
            ada_bool;

      Value_P : access ada_ada_node_array) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_basic_decl_p_root_subp_declarations";
   --  If Self declares a primitive subprogram of some tagged type T, return
   --  the root subprogram declarations that it overrides. There can be
   --  several, as in the following scenario:
   --
   --  * package Root defines the root tagged type T and subprogram Foo.
   --
   --  * package Itf defines interface I and abstract subprogram Foo.
   --
   --  * package D defines "type U is new Root.T and Itf.I" and an overriding
   --    subprogram Foo.
   --
   --  Here, root_subp_declarations of Foo defined in package D will return
   --  both Foo from package Root and Foo from package Itf.

           
   

   
   

   function ada_basic_decl_p_find_all_overrides
     (Node : ada_base_entity_Ptr;

         Units :
            
            ada_analysis_unit_array;
         Imprecise_Fallback :
            
            ada_bool;

      Value_P : access ada_ada_node_array) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_basic_decl_p_find_all_overrides";
   --  If Self is the declaration of a primitive of some type T, return the
   --  list of all subprogram that override this subprogram among the given
   --  units.

           
   

   
   

   function ada_basic_decl_p_defining_names
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_ada_node_array) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_basic_decl_p_defining_names";
   --  Get all the names of this basic declaration.

           
   

   
   

   function ada_basic_decl_p_defining_name
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_basic_decl_p_defining_name";
   --  Get the name of this declaration. If this declaration has several names,
   --  it will return the first one.

           
   

   
   

   function ada_basic_decl_p_type_expression
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_basic_decl_p_type_expression";
   --  Return the type expression for this BasicDecl if applicable, a null
   --  otherwise.

           
   

   
   

   function ada_basic_decl_p_subp_spec_or_null
     (Node : ada_base_entity_Ptr;

         Follow_Generic :
            
            ada_bool;

      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_basic_decl_p_subp_spec_or_null";
   --  If Self is a Subp, returns the specification of this subprogram.
   --
   --  If ``follow_generic`` is True, will also work for instances of
   --  ``GenericSubpDecl``.

           
   

   
   

   function ada_basic_decl_p_is_subprogram
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_bool) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_basic_decl_p_is_subprogram";
   --  Return True if self is a subprogram node in the general sense (which is,
   --  an entity that can be called). This includes separates and entries.
   --
   --  .. attention: This is a purely syntactic query and will return True for
   --     everything that is a syntactic entity that can be called like a
   --     subprogram in some contexts, even generic formal subprograms for
   --     example.

           
   

   
   

   function ada_basic_decl_p_relative_name
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_basic_decl_p_relative_name";
   --  Return the relative name for Self. If Self's defining name is ``A.B.C``,
   --  return ``C`` as a node.

           
   

   
   

   function ada_basic_decl_p_relative_name_text
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_symbol_type) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_basic_decl_p_relative_name_text";
   --  Return the relative name for Self, as text.

           
   

   
   

   function ada_basic_decl_p_next_part_for_decl
     (Node : ada_base_entity_Ptr;

         Imprecise_Fallback :
            
            ada_bool;

      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_basic_decl_p_next_part_for_decl";
   --  Return the next part of this declaration, if applicable.
   --
   --  .. note:: It is not named next_part, because BaseTypeDecl has a more
   --     precise version of next_part that returns a BaseTypeDecl. Probably,
   --     we want to rename the specific versions, and have the root property
   --     be named next_part. (TODO R925-008)

           
   

   
   

   function ada_basic_decl_p_body_part_for_decl
     (Node : ada_base_entity_Ptr;

         Imprecise_Fallback :
            
            ada_bool;

      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_basic_decl_p_body_part_for_decl";
   --  Return the body corresponding to this declaration, if applicable.
   --
   --  .. note:: It is not named body_part, subclasses have more precise
   --     versions named body_part and returning a more precise result.
   --     Probably, we want to rename the specific versions, and have the root
   --     property be named body_part. (TODO R925-008)

           
   

   
   

   function ada_basic_decl_p_most_visible_part
     (Node : ada_base_entity_Ptr;

         Origin :
            access constant
            ada_base_entity;
         Imprecise_Fallback :
            
            ada_bool;

      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_basic_decl_p_most_visible_part";
   --  Given an origin node and the entity represented by Self, this property
   --  returns the most visible completion of Self that can be seen by origin,
   --  according to Ada's visibility rules.

           
   

   
   

   function ada_basic_decl_p_fully_qualified_name_array
     (Node : ada_base_entity_Ptr;

         Include_Profile :
            
            ada_bool;

      Value_P : access ada_unbounded_text_type_array) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_basic_decl_p_fully_qualified_name_array";
   --  Return the fully qualified name corresponding to this declaration, as an
   --  array of symbols.

           
   

   
   

   function ada_basic_decl_p_fully_qualified_name
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_string_type) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_basic_decl_p_fully_qualified_name";
   --  Return the fully qualified name corresponding to this declaration.

           
   

   
   

   function ada_basic_decl_p_canonical_fully_qualified_name
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_string_type) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_basic_decl_p_canonical_fully_qualified_name";
   --  Return a canonical representation of the fully qualified name
   --  corresponding to this declaration.

           
   

   
   

   function ada_basic_decl_p_unique_identifying_name
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_string_type) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_basic_decl_p_unique_identifying_name";
   --  Return a unique identifying name for this declaration, provided this
   --  declaration is a public declaration. In the case of subprograms, this
   --  will include the profile.
   --
   --  .. attention:: This will only return a unique name for public
   --     declarations. Notably, anything nested in an unnamed declare block
   --     won't be handled correctly.

           
   

   
   

   function ada_basic_decl_p_is_constant_object
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_bool) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_basic_decl_p_is_constant_object";
   --  Return whether this object is constant or not.

           
   

   
   

   function ada_abstract_state_decl_f_name
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_abstract_state_decl_f_name";
   

           
   

   
   

   function ada_anonymous_expr_decl_f_expr
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_anonymous_expr_decl_f_expr";
   --  Return the expression wrapped by this declaration.

           
   

   
   

   function ada_anonymous_expr_decl_p_get_formal
     (Node : ada_base_entity_Ptr;

         Imprecise_Fallback :
            
            ada_bool;

      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_anonymous_expr_decl_p_get_formal";
   --  Return the generic formal object declaration corresponding to this
   --  actual.

           
   

   
   

   function ada_base_formal_param_decl_p_formal_type
     (Node : ada_base_entity_Ptr;

         Origin :
            access constant
            ada_base_entity;

      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_base_formal_param_decl_p_formal_type";
   --  Return the type for this formal.

           
   

   
   

   function ada_component_decl_f_ids
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_component_decl_f_ids";
   

           
   

   
   

   function ada_component_decl_f_component_def
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_component_decl_f_component_def";
   

           
   

   
   

   function ada_component_decl_f_default_expr
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_component_decl_f_default_expr";
   --  This field can contain one of the following nodes: :ada:ref:`Allocator`,
   --  :ada:ref:`Attribute_Ref`, :ada:ref:`Base_Aggregate`, :ada:ref:`Bin_Op`,
   --  :ada:ref:`Call_Expr`, :ada:ref:`Char_Literal`, :ada:ref:`Concat_Op`,
   --  :ada:ref:`Cond_Expr`, :ada:ref:`Decl_Expr`, :ada:ref:`Dotted_Name`,
   --  :ada:ref:`Explicit_Deref`, :ada:ref:`Identifier`,
   --  :ada:ref:`Membership_Expr`, :ada:ref:`Null_Literal`,
   --  :ada:ref:`Num_Literal`, :ada:ref:`Paren_Expr`, :ada:ref:`Qual_Expr`,
   --  :ada:ref:`Quantified_Expr`, :ada:ref:`Raise_Expr`,
   --  :ada:ref:`Reduce_Attribute_Ref`, :ada:ref:`String_Literal`,
   --  :ada:ref:`Target_Name`, :ada:ref:`Un_Op`,
   --  :ada:ref:`Update_Attribute_Ref`

           
   

   
   

   function ada_discriminant_spec_f_ids
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_discriminant_spec_f_ids";
   

           
   

   
   

   function ada_discriminant_spec_f_type_expr
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_discriminant_spec_f_type_expr";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Anonymous_Type`, :ada:ref:`Subtype_Indication`

           
   

   
   

   function ada_discriminant_spec_f_default_expr
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_discriminant_spec_f_default_expr";
   --  This field can contain one of the following nodes: :ada:ref:`Allocator`,
   --  :ada:ref:`Attribute_Ref`, :ada:ref:`Base_Aggregate`, :ada:ref:`Bin_Op`,
   --  :ada:ref:`Call_Expr`, :ada:ref:`Char_Literal`, :ada:ref:`Concat_Op`,
   --  :ada:ref:`Cond_Expr`, :ada:ref:`Decl_Expr`, :ada:ref:`Dotted_Name`,
   --  :ada:ref:`Explicit_Deref`, :ada:ref:`Identifier`,
   --  :ada:ref:`Membership_Expr`, :ada:ref:`Null_Literal`,
   --  :ada:ref:`Num_Literal`, :ada:ref:`Paren_Expr`, :ada:ref:`Qual_Expr`,
   --  :ada:ref:`Quantified_Expr`, :ada:ref:`Raise_Expr`,
   --  :ada:ref:`Reduce_Attribute_Ref`, :ada:ref:`String_Literal`,
   --  :ada:ref:`Target_Name`, :ada:ref:`Un_Op`,
   --  :ada:ref:`Update_Attribute_Ref`

           
   

   
   

   function ada_generic_formal_f_decl
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_generic_formal_f_decl";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Formal_Subp_Decl`, :ada:ref:`Formal_Type_Decl`,
   --  :ada:ref:`Generic_Instantiation`,
   --  :ada:ref:`Incomplete_Formal_Type_Decl`, :ada:ref:`Number_Decl`,
   --  :ada:ref:`Object_Decl`, :ada:ref:`Single_Protected_Decl`,
   --  :ada:ref:`Single_Task_Decl`

           
   

   
   

   function ada_param_spec_f_ids
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_param_spec_f_ids";
   

           
   

   
   

   function ada_param_spec_f_has_aliased
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_param_spec_f_has_aliased";
   

           
   

   
   

   function ada_param_spec_f_mode
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_param_spec_f_mode";
   

           
   

   
   

   function ada_param_spec_f_type_expr
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_param_spec_f_type_expr";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Anonymous_Type`, :ada:ref:`Subtype_Indication`

           
   

   
   

   function ada_param_spec_f_default_expr
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_param_spec_f_default_expr";
   --  This field can contain one of the following nodes: :ada:ref:`Allocator`,
   --  :ada:ref:`Attribute_Ref`, :ada:ref:`Base_Aggregate`, :ada:ref:`Bin_Op`,
   --  :ada:ref:`Call_Expr`, :ada:ref:`Char_Literal`, :ada:ref:`Concat_Op`,
   --  :ada:ref:`Cond_Expr`, :ada:ref:`Decl_Expr`, :ada:ref:`Dotted_Name`,
   --  :ada:ref:`Explicit_Deref`, :ada:ref:`Identifier`,
   --  :ada:ref:`Membership_Expr`, :ada:ref:`Null_Literal`,
   --  :ada:ref:`Num_Literal`, :ada:ref:`Paren_Expr`, :ada:ref:`Qual_Expr`,
   --  :ada:ref:`Quantified_Expr`, :ada:ref:`Raise_Expr`,
   --  :ada:ref:`Reduce_Attribute_Ref`, :ada:ref:`String_Literal`,
   --  :ada:ref:`Target_Name`, :ada:ref:`Un_Op`,
   --  :ada:ref:`Update_Attribute_Ref`

           
   

   
   

   function ada_synthetic_formal_param_decl_f_param_type
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_synthetic_formal_param_decl_f_param_type";
   

           
   

   
   

   function ada_base_package_decl_f_package_name
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_base_package_decl_f_package_name";
   

           
   

   
   

   function ada_base_package_decl_f_public_part
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_base_package_decl_f_public_part";
   

           
   

   
   

   function ada_base_package_decl_f_private_part
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_base_package_decl_f_private_part";
   

           
   

   
   

   function ada_base_package_decl_f_end_name
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_base_package_decl_f_end_name";
   

           
   

   
   

   function ada_base_package_decl_p_body_part
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_base_package_decl_p_body_part";
   --  Return the PackageBody corresponding to this node.

           
   

   
   

   function ada_base_type_decl_f_name
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_base_type_decl_f_name";
   

           
   

   
   

   function ada_base_type_decl_p_base_subtype
     (Node : ada_base_entity_Ptr;

         Origin :
            access constant
            ada_base_entity;

      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_base_type_decl_p_base_subtype";
   --  If this type decl is a subtype decl, return the base subtype. If not,
   --  return ``Self``.

           
   

   
   

   function ada_base_type_decl_p_private_completion
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_base_type_decl_p_private_completion";
   --  Return the private completion for this type, if there is one.

           
   

   
   

   function ada_base_type_decl_p_is_inherited_primitive
     (Node : ada_base_entity_Ptr;

         P :
            access constant
            ada_base_entity;

      Value_P : access ada_bool) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_base_type_decl_p_is_inherited_primitive";
   --  Assuming that P is a primitive of Self, return whether the given
   --  primitive P is inherited from one of Self's parents.

           
   

   
   

   function ada_base_type_decl_p_get_record_representation_clause
     (Node : ada_base_entity_Ptr;

         Imprecise_Fallback :
            
            ada_bool;

      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_base_type_decl_p_get_record_representation_clause";
   --  Return the record representation clause associated to this type decl, if
   --  applicable (i.e. this type decl defines a record type).

           
   

   
   

   function ada_base_type_decl_p_get_enum_representation_clause
     (Node : ada_base_entity_Ptr;

         Imprecise_Fallback :
            
            ada_bool;

      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_base_type_decl_p_get_enum_representation_clause";
   --  Return the enum representation clause associated to this type decl, if
   --  applicable (i.e. this type decl defines an enum type).

           
   

   
   

   function ada_base_type_decl_p_is_record_type
     (Node : ada_base_entity_Ptr;

         Origin :
            access constant
            ada_base_entity;

      Value_P : access ada_bool) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_base_type_decl_p_is_record_type";
   --  Return whether this type is a record type.
   --
   --  .. attention:: Private tagged types extending public tagged records are
   --     not considered as record types.

           
   

   
   

   function ada_base_type_decl_p_is_array_type
     (Node : ada_base_entity_Ptr;

         Origin :
            access constant
            ada_base_entity;

      Value_P : access ada_bool) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_base_type_decl_p_is_array_type";
   --  Return whether this type is an array type.

           
   

   
   

   function ada_base_type_decl_p_find_derived_types
     (Node : ada_base_entity_Ptr;

         Root :
            access constant
            ada_base_entity;
         Origin :
            access constant
            ada_base_entity;
         Imprecise_Fallback :
            
            ada_bool;

      Value_P : access ada_ada_node_array) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_base_type_decl_p_find_derived_types";
   --  Find types derived from self in the given ``root`` and its children.

           
   

   
   

   function ada_base_type_decl_p_is_real_type
     (Node : ada_base_entity_Ptr;

         Origin :
            access constant
            ada_base_entity;

      Value_P : access ada_bool) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_base_type_decl_p_is_real_type";
   --  Whether type is a real type or not.

           
   

   
   

   function ada_base_type_decl_p_is_float_type
     (Node : ada_base_entity_Ptr;

         Origin :
            access constant
            ada_base_entity;

      Value_P : access ada_bool) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_base_type_decl_p_is_float_type";
   --  Whether type is a float type or not.

           
   

   
   

   function ada_base_type_decl_p_is_fixed_point
     (Node : ada_base_entity_Ptr;

         Origin :
            access constant
            ada_base_entity;

      Value_P : access ada_bool) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_base_type_decl_p_is_fixed_point";
   --  Whether type is a fixed point type or not.

           
   

   
   

   function ada_base_type_decl_p_is_enum_type
     (Node : ada_base_entity_Ptr;

         Origin :
            access constant
            ada_base_entity;

      Value_P : access ada_bool) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_base_type_decl_p_is_enum_type";
   --  Whether type is an enum type

           
   

   
   

   function ada_base_type_decl_p_is_access_type
     (Node : ada_base_entity_Ptr;

         Origin :
            access constant
            ada_base_entity;

      Value_P : access ada_bool) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_base_type_decl_p_is_access_type";
   --  Whether Self is an access type or not

           
   

   
   

   function ada_base_type_decl_p_is_char_type
     (Node : ada_base_entity_Ptr;

         Origin :
            access constant
            ada_base_entity;

      Value_P : access ada_bool) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_base_type_decl_p_is_char_type";
   --  Whether type is a character type or not

           
   

   
   

   function ada_base_type_decl_p_discrete_range
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_internal_discrete_range) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_base_type_decl_p_discrete_range";
   --  Return the discrete range for this type decl, if applicable.

           
   

   
   

   function ada_base_type_decl_p_is_discrete_type
     (Node : ada_base_entity_Ptr;

         Origin :
            access constant
            ada_base_entity;

      Value_P : access ada_bool) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_base_type_decl_p_is_discrete_type";
   --  Whether type is a discrete type or not.

           
   

   
   

   function ada_base_type_decl_p_is_int_type
     (Node : ada_base_entity_Ptr;

         Origin :
            access constant
            ada_base_entity;

      Value_P : access ada_bool) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_base_type_decl_p_is_int_type";
   --  Whether type is an integer type or not.

           
   

   
   

   function ada_base_type_decl_p_accessed_type
     (Node : ada_base_entity_Ptr;

         Origin :
            access constant
            ada_base_entity;

      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_base_type_decl_p_accessed_type";
   --  If this type is an access type, or a type with an Implicit_Dereference
   --  aspect, return the type of a dereference of an instance of this type.

           
   

   
   

   function ada_base_type_decl_p_is_tagged_type
     (Node : ada_base_entity_Ptr;

         Origin :
            access constant
            ada_base_entity;

      Value_P : access ada_bool) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_base_type_decl_p_is_tagged_type";
   --  Whether type is tagged or not

           
   

   
   

   function ada_base_type_decl_p_base_type
     (Node : ada_base_entity_Ptr;

         Origin :
            access constant
            ada_base_entity;

      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_base_type_decl_p_base_type";
   --  Return the base type entity for this derived type declaration

           
   

   
   

   function ada_base_type_decl_p_base_types
     (Node : ada_base_entity_Ptr;

         Origin :
            access constant
            ada_base_entity;

      Value_P : access ada_ada_node_array) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_base_type_decl_p_base_types";
   --  Return the list of base types for Self.

           
   

   
   

   function ada_base_type_decl_p_find_all_derived_types
     (Node : ada_base_entity_Ptr;

         Units :
            
            ada_analysis_unit_array;
         Imprecise_Fallback :
            
            ada_bool;

      Value_P : access ada_ada_node_array) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_base_type_decl_p_find_all_derived_types";
   --  Return the list of all types that inherit (directly or inderictly) from
   --  Self among the given units.

           
   

   
   

   function ada_base_type_decl_p_comp_type
     (Node : ada_base_entity_Ptr;

         Is_Subscript :
            
            ada_bool;
         Origin :
            access constant
            ada_base_entity;

      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_base_type_decl_p_comp_type";
   --  Return the component type of ``Self``, if applicable. The component type
   --  is the type you'll get if you call a value whose type is ``Self``. So it
   --  can either be:
   --
   --  1. The component type for an array.
   --
   --  2. The return type for an access to function.

           
   

   
   

   function ada_base_type_decl_p_index_type
     (Node : ada_base_entity_Ptr;

         Dim :
            
            int;
         Origin :
            access constant
            ada_base_entity;

      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_base_type_decl_p_index_type";
   --  Return the index type for dimension ``dim`` for this type, if
   --  applicable.
   --
   --  .. warning:: ``dim`` is 0-based, so the first ``index_type`` is at index
   --     0.

           
   

   
   

   function ada_base_type_decl_p_is_derived_type
     (Node : ada_base_entity_Ptr;

         Other_Type :
            access constant
            ada_base_entity;
         Origin :
            access constant
            ada_base_entity;

      Value_P : access ada_bool) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_base_type_decl_p_is_derived_type";
   --  Whether Self is derived from other_type.

           
   

   
   

   function ada_base_type_decl_p_is_interface_type
     (Node : ada_base_entity_Ptr;

         Origin :
            access constant
            ada_base_entity;

      Value_P : access ada_bool) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_base_type_decl_p_is_interface_type";
   --  Return True iff this type declaration is an interface definition.

           
   

   
   

   function ada_base_type_decl_p_matching_type
     (Node : ada_base_entity_Ptr;

         Expected_Type :
            access constant
            ada_base_entity;
         Origin :
            access constant
            ada_base_entity;

      Value_P : access ada_bool) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_base_type_decl_p_matching_type";
   --  Return whether ``self`` matches ``expected_type``.

           
   

   
   

   function ada_base_type_decl_p_canonical_type
     (Node : ada_base_entity_Ptr;

         Origin :
            access constant
            ada_base_entity;

      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_base_type_decl_p_canonical_type";
   --  Return the canonical type declaration for this type declaration. For
   --  subtypes, it will return the base type declaration.

           
   

   
   

   function ada_base_type_decl_p_previous_part
     (Node : ada_base_entity_Ptr;

         Go_To_Incomplete :
            
            ada_bool;

      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_base_type_decl_p_previous_part";
   --  Returns the previous part for this type decl.

           
   

   
   

   function ada_base_type_decl_p_next_part
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_base_type_decl_p_next_part";
   --  Returns the next part for this type decl.
   --
   --  .. note:: Since this property returns a ``BaseTypeDecl``, it cannot be
   --     used to retrieve the next part of ``TaskTypeDecl`` and
   --     ``ProtectedTypeDecl`` nodes as their next part is actually a
   --     ``Body``. Use ``BasicDecl.next_part_for_decl`` for those instead.

           
   

   
   

   function ada_base_type_decl_p_full_view
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_base_type_decl_p_full_view";
   --  Return the full completion of this type.

           
   

   
   

   function ada_base_type_decl_p_is_definite_subtype
     (Node : ada_base_entity_Ptr;

         Origin :
            access constant
            ada_base_entity;

      Value_P : access ada_bool) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_base_type_decl_p_is_definite_subtype";
   --  Returns whether this is a definite subtype.
   --
   --  For convenience, this will return ``False`` for incomplete types, even
   --  though the correct answer is more akin to "non applicable".

           
   

   
   

   function ada_base_type_decl_p_is_private
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_bool) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_base_type_decl_p_is_private";
   --  Whether node is a private view of corresponding type.

           
   

   
   

   function ada_base_type_decl_p_discriminants_list
     (Node : ada_base_entity_Ptr;

         Origin :
            access constant
            ada_base_entity;

      Value_P : access ada_ada_node_array) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_base_type_decl_p_discriminants_list";
   --  Return the list of all discriminants of this type. If this type has no
   --  discriminant or only unknown discriminants, an empty list is returned.

           
   

   
   

   function ada_base_type_decl_p_root_type
     (Node : ada_base_entity_Ptr;

         Origin :
            access constant
            ada_base_entity;

      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_base_type_decl_p_root_type";
   --  Return the type that is at the root of the derivation hierarchy
   --  (ignoring secondary interfaces derivations for tagged types)

           
   

   
   

   function ada_base_type_decl_p_shapes
     (Node : ada_base_entity_Ptr;

         Include_Discriminants :
            
            ada_bool;
         Origin :
            access constant
            ada_base_entity;

      Value_P : access ada_shape_array) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_base_type_decl_p_shapes";
   --  Must be called on a record (sub-)type declaration. Return all the
   --  possible shapes that a value of this record type can take. For example,
   --  consider the following record definition:
   --
   --  .. code::
   --
   --     type R (A : Integer; B : Integer) is record
   --         X : Integer;
   --         case A is
   --             when 1 .. 10 =>
   --                 Y_1 : Integer;
   --                 case B is
   --                     when 1 .. 10 =>
   --                         Z_1 : Integer;
   --                     when others => null;
   --                 end case;
   --             when 11 .. 20 =>
   --                 Y_2 : Integer;
   --                 case B is
   --                     when 1 .. 10 =>
   --                         Z_2 : Integer;
   --                     when others => null;
   --                 end case;
   --             when others => null;
   --         end case;
   --     end record;
   --
   --  For this instance, this property will return the following results:
   --
   --  .. code::
   --
   --     [
   --         [X, Y_1, Z_1],
   --         [X, Y_1],
   --         [X, Y_2, Z_2],
   --         [X, Y_2],
   --         [X]
   --     ]
   --
   --  .. attention:: This property is inaccurate when called on a record
   --     extension which defines components under a certain condition C, and
   --     this same condition is used to define some components in the parent
   --     record: in that case, any feasible shape will in practice contain
   --     either both the components defined under condition C in the child
   --     record and the parent record, or none of them.However, due to the
   --     simplified algorithm we use here to compute the feasible shapes, we
   --     will also return shapes that include the components of the child
   --     record but not the parent record, and conversely.

           
   

   
   

   function ada_base_subtype_decl_p_get_type
     (Node : ada_base_entity_Ptr;

         Origin :
            access constant
            ada_base_entity;

      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_base_subtype_decl_p_get_type";
   --  Get the type for this subtype.

           
   

   
   

   function ada_subtype_decl_f_subtype
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_subtype_decl_f_subtype";
   

           
   

   
   

   function ada_incomplete_type_decl_f_discriminants
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_incomplete_type_decl_f_discriminants";
   

           
   

   
   

   function ada_incomplete_formal_type_decl_f_is_tagged
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_incomplete_formal_type_decl_f_is_tagged";
   

           
   

   
   

   function ada_incomplete_formal_type_decl_f_default_type
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_incomplete_formal_type_decl_f_default_type";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Attribute_Ref`, :ada:ref:`Call_Expr`, :ada:ref:`Char_Literal`,
   --  :ada:ref:`Dotted_Name`, :ada:ref:`Explicit_Deref`,
   --  :ada:ref:`Identifier`, :ada:ref:`Qual_Expr`,
   --  :ada:ref:`Reduce_Attribute_Ref`, :ada:ref:`String_Literal`,
   --  :ada:ref:`Target_Name`, :ada:ref:`Update_Attribute_Ref`

           
   

   
   

   function ada_incomplete_tagged_type_decl_f_has_abstract
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_incomplete_tagged_type_decl_f_has_abstract";
   

           
   

   
   

   function ada_protected_type_decl_f_discriminants
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_protected_type_decl_f_discriminants";
   

           
   

   
   

   function ada_protected_type_decl_f_interfaces
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_protected_type_decl_f_interfaces";
   --  This field contains a list that itself contains one of the following
   --  nodes: :ada:ref:`Char_Literal`, :ada:ref:`Dotted_Name`,
   --  :ada:ref:`Identifier`, :ada:ref:`String_Literal`

           
   

   
   

   function ada_protected_type_decl_f_definition
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_protected_type_decl_f_definition";
   

           
   

   
   

   function ada_task_type_decl_f_discriminants
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_task_type_decl_f_discriminants";
   

           
   

   
   

   function ada_task_type_decl_f_definition
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_task_type_decl_f_definition";
   

           
   

   
   

   function ada_type_decl_f_discriminants
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_type_decl_f_discriminants";
   

           
   

   
   

   function ada_type_decl_f_type_def
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_type_decl_f_type_def";
   

           
   

   
   

   function ada_type_decl_p_get_primitives
     (Node : ada_base_entity_Ptr;

         Only_Inherited :
            
            ada_bool;
         Include_Predefined_Operators :
            
            ada_bool;

      Value_P : access ada_ada_node_array) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_type_decl_p_get_primitives";
   --  Return the list of all primitive operations that are available on this
   --  type. If ``only_inherited`` is True, it will only return the primitives
   --  that are implicitly inherited by this type, discarding those explicitly
   --  defined on this type. Predefined operators are included in the result
   --  iff ``include_predefined_operators`` is True. It defaults to False.

           
   

   
   

   function ada_formal_type_decl_f_default_type
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_formal_type_decl_f_default_type";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Attribute_Ref`, :ada:ref:`Call_Expr`, :ada:ref:`Char_Literal`,
   --  :ada:ref:`Dotted_Name`, :ada:ref:`Explicit_Deref`,
   --  :ada:ref:`Identifier`, :ada:ref:`Qual_Expr`,
   --  :ada:ref:`Reduce_Attribute_Ref`, :ada:ref:`String_Literal`,
   --  :ada:ref:`Target_Name`, :ada:ref:`Update_Attribute_Ref`

           
   

   
   

   function ada_basic_subp_decl_p_subp_decl_spec
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_basic_subp_decl_p_subp_decl_spec";
   --  Return the specification for this subprogram

           
   

   
   

   function ada_classic_subp_decl_f_overriding
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_classic_subp_decl_f_overriding";
   

           
   

   
   

   function ada_classic_subp_decl_f_subp_spec
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_classic_subp_decl_f_subp_spec";
   

           
   

   
   

   function ada_classic_subp_decl_p_body_part
     (Node : ada_base_entity_Ptr;

         Imprecise_Fallback :
            
            ada_bool;

      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_classic_subp_decl_p_body_part";
   --  Return the BaseSubpBody corresponding to this node.

           
   

   
   

   function ada_formal_subp_decl_f_default_expr
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_formal_subp_decl_f_default_expr";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Attribute_Ref`, :ada:ref:`Box_Expr`, :ada:ref:`Call_Expr`,
   --  :ada:ref:`Char_Literal`, :ada:ref:`Dotted_Name`,
   --  :ada:ref:`Explicit_Deref`, :ada:ref:`Identifier`,
   --  :ada:ref:`Null_Literal`, :ada:ref:`Qual_Expr`,
   --  :ada:ref:`Reduce_Attribute_Ref`, :ada:ref:`String_Literal`,
   --  :ada:ref:`Target_Name`, :ada:ref:`Update_Attribute_Ref`

           
   

   
   

   function ada_entry_decl_f_overriding
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_entry_decl_f_overriding";
   

           
   

   
   

   function ada_entry_decl_f_spec
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_entry_decl_f_spec";
   

           
   

   
   

   function ada_entry_decl_p_body_part
     (Node : ada_base_entity_Ptr;

         Imprecise_Fallback :
            
            ada_bool;

      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_entry_decl_p_body_part";
   --  Return the entry body associated to this entry declaration.

           
   

   
   

   function ada_entry_decl_p_accept_stmts
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_ada_node_array) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_entry_decl_p_accept_stmts";
   --  Return an array of accept statements corresponding to this entry.

           
   

   
   

   function ada_enum_literal_decl_f_name
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_enum_literal_decl_f_name";
   

           
   

   
   

   function ada_enum_literal_decl_p_enum_type
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_enum_literal_decl_p_enum_type";
   --  Return the enum type corresponding to this enum literal.

           
   

   
   

   function ada_synthetic_char_enum_lit_p_expr
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_synthetic_char_enum_lit_p_expr";
   --  Return the CharLiteral expression corresponding to this enum literal.

           
   

   
   

   function ada_generic_subp_internal_f_subp_spec
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_generic_subp_internal_f_subp_spec";
   

           
   

   
   

   function ada_synthetic_subp_decl_f_spec
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_synthetic_subp_decl_f_spec";
   

           
   

   
   

   function ada_body_node_p_previous_part
     (Node : ada_base_entity_Ptr;

         Imprecise_Fallback :
            
            ada_bool;

      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_body_node_p_previous_part";
   --  Return the previous part for this body. Might be a declaration or a body
   --  stub.

           
   

   
   

   function ada_body_node_p_decl_part
     (Node : ada_base_entity_Ptr;

         Imprecise_Fallback :
            
            ada_bool;

      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_body_node_p_decl_part";
   --  Return the decl corresponding to this node if applicable.

           
   

   
   

   function ada_body_node_p_subunit_root
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_body_node_p_subunit_root";
   --  If self is a subunit, return the body in which it is rooted.

           
   

   
   

   function ada_base_subp_body_f_overriding
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_base_subp_body_f_overriding";
   

           
   

   
   

   function ada_base_subp_body_f_subp_spec
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_base_subp_body_f_subp_spec";
   

           
   

   
   

   function ada_expr_function_f_expr
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_expr_function_f_expr";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Base_Aggregate`, :ada:ref:`Paren_Expr`

           
   

   
   

   function ada_subp_body_f_decls
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_subp_body_f_decls";
   

           
   

   
   

   function ada_subp_body_f_stmts
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_subp_body_f_stmts";
   

           
   

   
   

   function ada_subp_body_f_end_name
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_subp_body_f_end_name";
   

           
   

   
   

   function ada_subp_renaming_decl_f_renames
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_subp_renaming_decl_f_renames";
   

           
   

   
   

   function ada_body_stub_p_syntactic_fully_qualified_name
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_unbounded_text_type_array) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_body_stub_p_syntactic_fully_qualified_name";
   --  Return the syntactic fully qualified name to refer to this body.
   --
   --  Note that this can raise a Property_Error when the stub is in an illegal
   --  place (too nested, in a declare block, etc.).

           
   

   
   

   function ada_package_body_stub_f_name
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_package_body_stub_f_name";
   

           
   

   
   

   function ada_protected_body_stub_f_name
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_protected_body_stub_f_name";
   

           
   

   
   

   function ada_subp_body_stub_f_overriding
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_subp_body_stub_f_overriding";
   

           
   

   
   

   function ada_subp_body_stub_f_subp_spec
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_subp_body_stub_f_subp_spec";
   

           
   

   
   

   function ada_task_body_stub_f_name
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_task_body_stub_f_name";
   

           
   

   
   

   function ada_entry_body_f_entry_name
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_entry_body_f_entry_name";
   

           
   

   
   

   function ada_entry_body_f_index_spec
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_entry_body_f_index_spec";
   

           
   

   
   

   function ada_entry_body_f_params
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_entry_body_f_params";
   

           
   

   
   

   function ada_entry_body_f_barrier
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_entry_body_f_barrier";
   --  This field can contain one of the following nodes: :ada:ref:`Allocator`,
   --  :ada:ref:`Attribute_Ref`, :ada:ref:`Base_Aggregate`, :ada:ref:`Bin_Op`,
   --  :ada:ref:`Call_Expr`, :ada:ref:`Char_Literal`, :ada:ref:`Concat_Op`,
   --  :ada:ref:`Cond_Expr`, :ada:ref:`Decl_Expr`, :ada:ref:`Dotted_Name`,
   --  :ada:ref:`Explicit_Deref`, :ada:ref:`Identifier`,
   --  :ada:ref:`Membership_Expr`, :ada:ref:`Null_Literal`,
   --  :ada:ref:`Num_Literal`, :ada:ref:`Paren_Expr`, :ada:ref:`Qual_Expr`,
   --  :ada:ref:`Quantified_Expr`, :ada:ref:`Raise_Expr`,
   --  :ada:ref:`Reduce_Attribute_Ref`, :ada:ref:`String_Literal`,
   --  :ada:ref:`Target_Name`, :ada:ref:`Un_Op`,
   --  :ada:ref:`Update_Attribute_Ref`

           
   

   
   

   function ada_entry_body_f_decls
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_entry_body_f_decls";
   

           
   

   
   

   function ada_entry_body_f_stmts
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_entry_body_f_stmts";
   

           
   

   
   

   function ada_entry_body_f_end_name
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_entry_body_f_end_name";
   

           
   

   
   

   function ada_package_body_f_package_name
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_package_body_f_package_name";
   

           
   

   
   

   function ada_package_body_f_decls
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_package_body_f_decls";
   

           
   

   
   

   function ada_package_body_f_stmts
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_package_body_f_stmts";
   

           
   

   
   

   function ada_package_body_f_end_name
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_package_body_f_end_name";
   

           
   

   
   

   function ada_protected_body_f_name
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_protected_body_f_name";
   

           
   

   
   

   function ada_protected_body_f_decls
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_protected_body_f_decls";
   

           
   

   
   

   function ada_protected_body_f_end_name
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_protected_body_f_end_name";
   

           
   

   
   

   function ada_task_body_f_name
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_task_body_f_name";
   

           
   

   
   

   function ada_task_body_f_decls
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_task_body_f_decls";
   

           
   

   
   

   function ada_task_body_f_stmts
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_task_body_f_stmts";
   

           
   

   
   

   function ada_task_body_f_end_name
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_task_body_f_end_name";
   

           
   

   
   

   function ada_entry_index_spec_f_id
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_entry_index_spec_f_id";
   

           
   

   
   

   function ada_entry_index_spec_f_subtype
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_entry_index_spec_f_subtype";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Attribute_Ref`, :ada:ref:`Bin_Op`, :ada:ref:`Call_Expr`,
   --  :ada:ref:`Char_Literal`, :ada:ref:`Dotted_Name`,
   --  :ada:ref:`Explicit_Deref`, :ada:ref:`Identifier`, :ada:ref:`Qual_Expr`,
   --  :ada:ref:`Reduce_Attribute_Ref`, :ada:ref:`String_Literal`,
   --  :ada:ref:`Subtype_Indication`, :ada:ref:`Target_Name`,
   --  :ada:ref:`Update_Attribute_Ref`

           
   

   
   

   function ada_exception_decl_f_ids
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_exception_decl_f_ids";
   

           
   

   
   

   function ada_exception_decl_f_renames
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_exception_decl_f_renames";
   

           
   

   
   

   function ada_exception_handler_f_exception_name
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_exception_handler_f_exception_name";
   

           
   

   
   

   function ada_exception_handler_f_handled_exceptions
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_exception_handler_f_handled_exceptions";
   --  This field contains a list that itself contains one of the following
   --  nodes: :ada:ref:`Attribute_Ref`, :ada:ref:`Call_Expr`,
   --  :ada:ref:`Char_Literal`, :ada:ref:`Dotted_Name`,
   --  :ada:ref:`Explicit_Deref`, :ada:ref:`Identifier`,
   --  :ada:ref:`Others_Designator`, :ada:ref:`Qual_Expr`,
   --  :ada:ref:`Reduce_Attribute_Ref`, :ada:ref:`String_Literal`,
   --  :ada:ref:`Target_Name`, :ada:ref:`Update_Attribute_Ref`

           
   

   
   

   function ada_exception_handler_f_stmts
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_exception_handler_f_stmts";
   --  This field contains a list that itself contains one of the following
   --  nodes: :ada:ref:`Pragma_Node`, :ada:ref:`Stmt`

           
   

   
   

   function ada_for_loop_var_decl_f_id
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_for_loop_var_decl_f_id";
   

           
   

   
   

   function ada_for_loop_var_decl_f_id_type
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_for_loop_var_decl_f_id_type";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Anonymous_Type`, :ada:ref:`Subtype_Indication`

           
   

   
   

   function ada_generic_decl_f_formal_part
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_generic_decl_f_formal_part";
   

           
   

   
   

   function ada_generic_package_decl_f_package_decl
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_generic_package_decl_f_package_decl";
   

           
   

   
   

   function ada_generic_package_decl_p_body_part
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_generic_package_decl_p_body_part";
   --  Return the PackageBody corresponding to this node, or null if there is
   --  none.

           
   

   
   

   function ada_generic_subp_decl_f_subp_decl
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_generic_subp_decl_f_subp_decl";
   

           
   

   
   

   function ada_generic_subp_decl_p_body_part
     (Node : ada_base_entity_Ptr;

         Imprecise_Fallback :
            
            ada_bool;

      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_generic_subp_decl_p_body_part";
   --  Return the BaseSubpBody corresponding to this node.

           
   

   
   

   function ada_generic_instantiation_p_designated_generic_decl
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_generic_instantiation_p_designated_generic_decl";
   --  Return the generic decl entity designated by this instantiation,
   --  containing the generic context. This is equivalent to the expanded
   --  generic unit in GNAT.

           
   

   
   

   function ada_generic_instantiation_p_inst_params
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_param_actual_array) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_generic_instantiation_p_inst_params";
   --  Returns an array of pairs, associating formal parameters to actual or
   --  default expressions.

           
   

   
   

   function ada_generic_package_instantiation_f_name
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_generic_package_instantiation_f_name";
   

           
   

   
   

   function ada_generic_package_instantiation_f_generic_pkg_name
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_generic_package_instantiation_f_generic_pkg_name";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Char_Literal`, :ada:ref:`Dotted_Name`, :ada:ref:`Identifier`,
   --  :ada:ref:`String_Literal`

           
   

   
   

   function ada_generic_package_instantiation_f_params
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_generic_package_instantiation_f_params";
   --  This field contains a list that itself contains one of the following
   --  nodes: :ada:ref:`Param_Assoc`

           
   

   
   

   function ada_generic_subp_instantiation_f_overriding
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_generic_subp_instantiation_f_overriding";
   

           
   

   
   

   function ada_generic_subp_instantiation_f_kind
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_generic_subp_instantiation_f_kind";
   

           
   

   
   

   function ada_generic_subp_instantiation_f_subp_name
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_generic_subp_instantiation_f_subp_name";
   

           
   

   
   

   function ada_generic_subp_instantiation_f_generic_subp_name
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_generic_subp_instantiation_f_generic_subp_name";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Char_Literal`, :ada:ref:`Dotted_Name`, :ada:ref:`Identifier`,
   --  :ada:ref:`String_Literal`

           
   

   
   

   function ada_generic_subp_instantiation_f_params
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_generic_subp_instantiation_f_params";
   --  This field contains a list that itself contains one of the following
   --  nodes: :ada:ref:`Param_Assoc`

           
   

   
   

   function ada_generic_subp_instantiation_p_designated_subp
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_generic_subp_instantiation_p_designated_subp";
   --  Return the subprogram decl designated by this instantiation.

           
   

   
   

   function ada_generic_package_renaming_decl_f_name
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_generic_package_renaming_decl_f_name";
   

           
   

   
   

   function ada_generic_package_renaming_decl_f_renames
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_generic_package_renaming_decl_f_renames";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Char_Literal`, :ada:ref:`Dotted_Name`, :ada:ref:`Identifier`,
   --  :ada:ref:`String_Literal`

           
   

   
   

   function ada_generic_subp_renaming_decl_f_kind
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_generic_subp_renaming_decl_f_kind";
   

           
   

   
   

   function ada_generic_subp_renaming_decl_f_name
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_generic_subp_renaming_decl_f_name";
   

           
   

   
   

   function ada_generic_subp_renaming_decl_f_renames
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_generic_subp_renaming_decl_f_renames";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Char_Literal`, :ada:ref:`Dotted_Name`, :ada:ref:`Identifier`,
   --  :ada:ref:`String_Literal`

           
   

   
   

   function ada_label_decl_f_name
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_label_decl_f_name";
   

           
   

   
   

   function ada_named_stmt_decl_f_name
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_named_stmt_decl_f_name";
   

           
   

   
   

   function ada_number_decl_f_ids
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_number_decl_f_ids";
   

           
   

   
   

   function ada_number_decl_f_expr
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_number_decl_f_expr";
   --  This field can contain one of the following nodes: :ada:ref:`Allocator`,
   --  :ada:ref:`Attribute_Ref`, :ada:ref:`Base_Aggregate`, :ada:ref:`Bin_Op`,
   --  :ada:ref:`Call_Expr`, :ada:ref:`Char_Literal`, :ada:ref:`Concat_Op`,
   --  :ada:ref:`Cond_Expr`, :ada:ref:`Decl_Expr`, :ada:ref:`Dotted_Name`,
   --  :ada:ref:`Explicit_Deref`, :ada:ref:`Identifier`,
   --  :ada:ref:`Null_Literal`, :ada:ref:`Num_Literal`, :ada:ref:`Paren_Expr`,
   --  :ada:ref:`Qual_Expr`, :ada:ref:`Quantified_Expr`, :ada:ref:`Raise_Expr`,
   --  :ada:ref:`Reduce_Attribute_Ref`, :ada:ref:`String_Literal`,
   --  :ada:ref:`Target_Name`, :ada:ref:`Un_Op`,
   --  :ada:ref:`Update_Attribute_Ref`

           
   

   
   

   function ada_object_decl_f_ids
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_object_decl_f_ids";
   

           
   

   
   

   function ada_object_decl_f_has_aliased
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_object_decl_f_has_aliased";
   

           
   

   
   

   function ada_object_decl_f_has_constant
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_object_decl_f_has_constant";
   

           
   

   
   

   function ada_object_decl_f_mode
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_object_decl_f_mode";
   

           
   

   
   

   function ada_object_decl_f_type_expr
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_object_decl_f_type_expr";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Anonymous_Type`, :ada:ref:`Subtype_Indication`

           
   

   
   

   function ada_object_decl_f_default_expr
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_object_decl_f_default_expr";
   --  This field can contain one of the following nodes: :ada:ref:`Allocator`,
   --  :ada:ref:`Attribute_Ref`, :ada:ref:`Base_Aggregate`, :ada:ref:`Bin_Op`,
   --  :ada:ref:`Call_Expr`, :ada:ref:`Char_Literal`, :ada:ref:`Concat_Op`,
   --  :ada:ref:`Cond_Expr`, :ada:ref:`Decl_Expr`, :ada:ref:`Dotted_Name`,
   --  :ada:ref:`Explicit_Deref`, :ada:ref:`Identifier`,
   --  :ada:ref:`Membership_Expr`, :ada:ref:`Null_Literal`,
   --  :ada:ref:`Num_Literal`, :ada:ref:`Paren_Expr`, :ada:ref:`Qual_Expr`,
   --  :ada:ref:`Quantified_Expr`, :ada:ref:`Raise_Expr`,
   --  :ada:ref:`Reduce_Attribute_Ref`, :ada:ref:`String_Literal`,
   --  :ada:ref:`Target_Name`, :ada:ref:`Un_Op`,
   --  :ada:ref:`Update_Attribute_Ref`

           
   

   
   

   function ada_object_decl_f_renaming_clause
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_object_decl_f_renaming_clause";
   

           
   

   
   

   function ada_object_decl_p_private_part_decl
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_object_decl_p_private_part_decl";
   --  If this object decl is the constant completion of an object decl in the
   --  public part, return the object decl from the public part.

           
   

   
   

   function ada_object_decl_p_public_part_decl
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_object_decl_p_public_part_decl";
   --  If this object decl is the incomplete declaration of a constant in a
   --  public part, return its completion in the private part.

           
   

   
   

   function ada_package_renaming_decl_f_name
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_package_renaming_decl_f_name";
   

           
   

   
   

   function ada_package_renaming_decl_f_renames
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_package_renaming_decl_f_renames";
   

           
   

   
   

   function ada_package_renaming_decl_p_renamed_package
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_package_renaming_decl_p_renamed_package";
   --  Return the declaration of the package that is renamed by Self.

           
   

   
   

   function ada_package_renaming_decl_p_final_renamed_package
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_package_renaming_decl_p_final_renamed_package";
   --  Return the declaration of the package that is ultimately renamed by
   --  Self, skipping through all intermediate package renamings.

           
   

   
   

   function ada_single_protected_decl_f_name
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_single_protected_decl_f_name";
   

           
   

   
   

   function ada_single_protected_decl_f_interfaces
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_single_protected_decl_f_interfaces";
   --  This field contains a list that itself contains one of the following
   --  nodes: :ada:ref:`Char_Literal`, :ada:ref:`Dotted_Name`,
   --  :ada:ref:`Identifier`, :ada:ref:`String_Literal`

           
   

   
   

   function ada_single_protected_decl_f_definition
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_single_protected_decl_f_definition";
   

           
   

   
   

   function ada_single_task_decl_f_task_type
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_single_task_decl_f_task_type";
   

           
   

   
   

   function ada_case_stmt_alternative_f_choices
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_case_stmt_alternative_f_choices";
   --  This field contains a list that itself contains one of the following
   --  nodes: :ada:ref:`Allocator`, :ada:ref:`Attribute_Ref`,
   --  :ada:ref:`Base_Aggregate`, :ada:ref:`Bin_Op`, :ada:ref:`Call_Expr`,
   --  :ada:ref:`Char_Literal`, :ada:ref:`Concat_Op`, :ada:ref:`Cond_Expr`,
   --  :ada:ref:`Decl_Expr`, :ada:ref:`Discrete_Subtype_Indication`,
   --  :ada:ref:`Dotted_Name`, :ada:ref:`Explicit_Deref`,
   --  :ada:ref:`Identifier`, :ada:ref:`Membership_Expr`,
   --  :ada:ref:`Null_Literal`, :ada:ref:`Num_Literal`,
   --  :ada:ref:`Others_Designator`, :ada:ref:`Paren_Expr`,
   --  :ada:ref:`Qual_Expr`, :ada:ref:`Quantified_Expr`, :ada:ref:`Raise_Expr`,
   --  :ada:ref:`Reduce_Attribute_Ref`, :ada:ref:`String_Literal`,
   --  :ada:ref:`Target_Name`, :ada:ref:`Un_Op`,
   --  :ada:ref:`Update_Attribute_Ref`

           
   

   
   

   function ada_case_stmt_alternative_f_stmts
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_case_stmt_alternative_f_stmts";
   --  This field contains a list that itself contains one of the following
   --  nodes: :ada:ref:`Pragma_Node`, :ada:ref:`Stmt`

           
   

   
   

   function ada_compilation_unit_f_prelude
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_compilation_unit_f_prelude";
   --  ``with``, ``use`` or ``pragma`` statements.
   --
   --  This field contains a list that itself contains one of the following
   --  nodes: :ada:ref:`Pragma_Node`, :ada:ref:`Use_Clause`,
   --  :ada:ref:`With_Clause`

           
   

   
   

   function ada_compilation_unit_f_body
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_compilation_unit_f_body";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Library_Item`, :ada:ref:`Subunit`

           
   

   
   

   function ada_compilation_unit_f_pragmas
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_compilation_unit_f_pragmas";
   

           
   

   
   

   function ada_compilation_unit_p_syntactic_fully_qualified_name
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_unbounded_text_type_array) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_compilation_unit_p_syntactic_fully_qualified_name";
   --  Return the syntactic fully qualified name of this compilation unit.

           
   

   
   

   function ada_compilation_unit_p_unit_kind
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_analysis_unit_kind) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_compilation_unit_p_unit_kind";
   --  Return the kind corresponding to this analysis unit.

           
   

   
   

   function ada_compilation_unit_p_withed_units
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_ada_node_array) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_compilation_unit_p_withed_units";
   --  Look for all "with" clauses at the top of this compilation unit and
   --  return all the compilation units designated by them. For the complete
   --  dependencies list of compilation units, see the ``unit_dependencies``
   --  property.

           
   

   
   

   function ada_compilation_unit_p_imported_units
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_ada_node_array) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_compilation_unit_p_imported_units";
   --  Return all the compilation units that are directly imported by this one.
   --  This includes "with"ed units as well as the direct parent unit.

           
   

   
   

   function ada_compilation_unit_p_unit_dependencies
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_ada_node_array) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_compilation_unit_p_unit_dependencies";
   --  Return the list of all the compilation units that are (direct and
   --  indirect) dependencies of this one. See the
   --  ``withed_units``/``imported_units`` properties to only get the direct
   --  dependencies of this unit.

           
   

   
   

   function ada_compilation_unit_p_decl
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_compilation_unit_p_decl";
   --  Get the root basic decl defined in this compilation unit.

           
   

   
   

   function ada_compilation_unit_p_is_preelaborable
     (Node : ada_base_entity_Ptr;

         Imprecise_Fallback :
            
            ada_bool;

      Value_P : access ada_bool) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_compilation_unit_p_is_preelaborable";
   --  Whether this compilation unit is preelaborable or not.

           
   

   
   

   function ada_compilation_unit_p_other_part
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_compilation_unit_p_other_part";
   --  If this compilation unit is of kind UnitSpecification, return its
   --  corresponding body unit, and conversely.

           
   

   
   

   function ada_compilation_unit_p_has_restriction
     (Node : ada_base_entity_Ptr;

         Name :
            access constant
            ada_symbol_type;

      Value_P : access ada_bool) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_compilation_unit_p_has_restriction";
   --  Whether this compilation unit is affected by the restriction with the
   --  given name.
   --
   --  .. warning:: This property only supports the ``No_Elaboration_Code``
   --     restriction for now.

           
   

   
   

   function ada_compilation_unit_p_all_config_pragmas
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_ada_node_array) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_compilation_unit_p_all_config_pragmas";
   --  Return the list of configuration pragmas that apply to the current unit.
   --
   --  .. note:: Using this property before creating the configuration pragmas
   --     files mapping using subprograms from the
   --     ``Libadalang.Config_Pragmas`` package will raise an error.

           
   

   
   

   function ada_compilation_unit_p_config_pragmas
     (Node : ada_base_entity_Ptr;

         Name :
            access constant
            ada_symbol_type;

      Value_P : access ada_ada_node_array) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_compilation_unit_p_config_pragmas";
   --  Return the list of configuration pragmas wih the given name that apply
   --  to the current unit.
   --
   --  .. note:: Using this property before creating the configuration pragmas
   --     files mapping using subprograms from the
   --     ``Libadalang.Config_Pragmas`` package will raise an error.

           
   

   
   

   function ada_component_clause_f_id
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_component_clause_f_id";
   

           
   

   
   

   function ada_component_clause_f_position
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_component_clause_f_position";
   --  This field can contain one of the following nodes: :ada:ref:`Allocator`,
   --  :ada:ref:`Attribute_Ref`, :ada:ref:`Base_Aggregate`, :ada:ref:`Bin_Op`,
   --  :ada:ref:`Call_Expr`, :ada:ref:`Char_Literal`, :ada:ref:`Concat_Op`,
   --  :ada:ref:`Cond_Expr`, :ada:ref:`Decl_Expr`, :ada:ref:`Dotted_Name`,
   --  :ada:ref:`Explicit_Deref`, :ada:ref:`Identifier`,
   --  :ada:ref:`Null_Literal`, :ada:ref:`Num_Literal`, :ada:ref:`Paren_Expr`,
   --  :ada:ref:`Qual_Expr`, :ada:ref:`Quantified_Expr`, :ada:ref:`Raise_Expr`,
   --  :ada:ref:`Reduce_Attribute_Ref`, :ada:ref:`String_Literal`,
   --  :ada:ref:`Target_Name`, :ada:ref:`Un_Op`,
   --  :ada:ref:`Update_Attribute_Ref`

           
   

   
   

   function ada_component_clause_f_range
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_component_clause_f_range";
   

           
   

   
   

   function ada_component_def_f_has_aliased
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_component_def_f_has_aliased";
   

           
   

   
   

   function ada_component_def_f_has_constant
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_component_def_f_has_constant";
   

           
   

   
   

   function ada_component_def_f_type_expr
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_component_def_f_type_expr";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Anonymous_Type`, :ada:ref:`Subtype_Indication`

           
   

   
   

   function ada_constant_node_p_as_bool
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_bool) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_constant_node_p_as_bool";
   --  Return whether this is an instance of ConstantPresent

           
   

   
   

   function ada_composite_constraint_f_constraints
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_composite_constraint_f_constraints";
   --  This field contains a list that itself contains one of the following
   --  nodes: :ada:ref:`Composite_Constraint_Assoc`

           
   

   
   

   function ada_composite_constraint_p_is_index_constraint
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_bool) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_composite_constraint_p_is_index_constraint";
   --  Whether this composite constraint is an index constraint.

           
   

   
   

   function ada_composite_constraint_p_is_discriminant_constraint
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_bool) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_composite_constraint_p_is_discriminant_constraint";
   --  Whether this composite constraint is a discriminant constraint.

           
   

   
   

   function ada_delta_constraint_f_digits
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_delta_constraint_f_digits";
   --  This field can contain one of the following nodes: :ada:ref:`Allocator`,
   --  :ada:ref:`Attribute_Ref`, :ada:ref:`Base_Aggregate`, :ada:ref:`Bin_Op`,
   --  :ada:ref:`Call_Expr`, :ada:ref:`Char_Literal`, :ada:ref:`Concat_Op`,
   --  :ada:ref:`Cond_Expr`, :ada:ref:`Decl_Expr`, :ada:ref:`Dotted_Name`,
   --  :ada:ref:`Explicit_Deref`, :ada:ref:`Identifier`,
   --  :ada:ref:`Null_Literal`, :ada:ref:`Num_Literal`, :ada:ref:`Paren_Expr`,
   --  :ada:ref:`Qual_Expr`, :ada:ref:`Quantified_Expr`, :ada:ref:`Raise_Expr`,
   --  :ada:ref:`Reduce_Attribute_Ref`, :ada:ref:`String_Literal`,
   --  :ada:ref:`Target_Name`, :ada:ref:`Un_Op`,
   --  :ada:ref:`Update_Attribute_Ref`

           
   

   
   

   function ada_delta_constraint_f_range
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_delta_constraint_f_range";
   

           
   

   
   

   function ada_digits_constraint_f_digits
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_digits_constraint_f_digits";
   --  This field can contain one of the following nodes: :ada:ref:`Allocator`,
   --  :ada:ref:`Attribute_Ref`, :ada:ref:`Base_Aggregate`, :ada:ref:`Bin_Op`,
   --  :ada:ref:`Call_Expr`, :ada:ref:`Char_Literal`, :ada:ref:`Concat_Op`,
   --  :ada:ref:`Cond_Expr`, :ada:ref:`Decl_Expr`, :ada:ref:`Dotted_Name`,
   --  :ada:ref:`Explicit_Deref`, :ada:ref:`Identifier`,
   --  :ada:ref:`Null_Literal`, :ada:ref:`Num_Literal`, :ada:ref:`Paren_Expr`,
   --  :ada:ref:`Qual_Expr`, :ada:ref:`Quantified_Expr`, :ada:ref:`Raise_Expr`,
   --  :ada:ref:`Reduce_Attribute_Ref`, :ada:ref:`String_Literal`,
   --  :ada:ref:`Target_Name`, :ada:ref:`Un_Op`,
   --  :ada:ref:`Update_Attribute_Ref`

           
   

   
   

   function ada_digits_constraint_f_range
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_digits_constraint_f_range";
   

           
   

   
   

   function ada_range_constraint_f_range
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_range_constraint_f_range";
   

           
   

   
   

   function ada_declarative_part_f_decls
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_declarative_part_f_decls";
   --  This field contains a list that itself contains one of the following
   --  nodes: :ada:ref:`Abstract_Subp_Decl`, :ada:ref:`Aspect_Clause`,
   --  :ada:ref:`Body_Node`, :ada:ref:`Component_Decl`,
   --  :ada:ref:`Concrete_Type_Decl`, :ada:ref:`Entry_Decl`,
   --  :ada:ref:`Error_Decl`, :ada:ref:`Exception_Decl`,
   --  :ada:ref:`Generic_Decl`, :ada:ref:`Generic_Instantiation`,
   --  :ada:ref:`Generic_Renaming_Decl`, :ada:ref:`Incomplete_Type_Decl`,
   --  :ada:ref:`Number_Decl`, :ada:ref:`Object_Decl`, :ada:ref:`Package_Decl`,
   --  :ada:ref:`Package_Renaming_Decl`, :ada:ref:`Pragma_Node`,
   --  :ada:ref:`Protected_Type_Decl`, :ada:ref:`Single_Protected_Decl`,
   --  :ada:ref:`Single_Task_Decl`, :ada:ref:`Subp_Decl`,
   --  :ada:ref:`Subtype_Decl`, :ada:ref:`Task_Type_Decl`,
   --  :ada:ref:`Use_Clause`

           
   

   
   

   function ada_elsif_expr_part_f_cond_expr
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_elsif_expr_part_f_cond_expr";
   --  This field can contain one of the following nodes: :ada:ref:`Allocator`,
   --  :ada:ref:`Attribute_Ref`, :ada:ref:`Base_Aggregate`, :ada:ref:`Bin_Op`,
   --  :ada:ref:`Call_Expr`, :ada:ref:`Char_Literal`, :ada:ref:`Concat_Op`,
   --  :ada:ref:`Cond_Expr`, :ada:ref:`Decl_Expr`, :ada:ref:`Dotted_Name`,
   --  :ada:ref:`Explicit_Deref`, :ada:ref:`Identifier`,
   --  :ada:ref:`Membership_Expr`, :ada:ref:`Null_Literal`,
   --  :ada:ref:`Num_Literal`, :ada:ref:`Paren_Expr`, :ada:ref:`Qual_Expr`,
   --  :ada:ref:`Quantified_Expr`, :ada:ref:`Raise_Expr`,
   --  :ada:ref:`Reduce_Attribute_Ref`, :ada:ref:`String_Literal`,
   --  :ada:ref:`Target_Name`, :ada:ref:`Un_Op`,
   --  :ada:ref:`Update_Attribute_Ref`

           
   

   
   

   function ada_elsif_expr_part_f_then_expr
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_elsif_expr_part_f_then_expr";
   --  This field can contain one of the following nodes: :ada:ref:`Allocator`,
   --  :ada:ref:`Attribute_Ref`, :ada:ref:`Base_Aggregate`, :ada:ref:`Bin_Op`,
   --  :ada:ref:`Call_Expr`, :ada:ref:`Char_Literal`, :ada:ref:`Concat_Op`,
   --  :ada:ref:`Cond_Expr`, :ada:ref:`Decl_Expr`, :ada:ref:`Dotted_Name`,
   --  :ada:ref:`Explicit_Deref`, :ada:ref:`Identifier`,
   --  :ada:ref:`Membership_Expr`, :ada:ref:`Null_Literal`,
   --  :ada:ref:`Num_Literal`, :ada:ref:`Paren_Expr`, :ada:ref:`Qual_Expr`,
   --  :ada:ref:`Quantified_Expr`, :ada:ref:`Raise_Expr`,
   --  :ada:ref:`Reduce_Attribute_Ref`, :ada:ref:`String_Literal`,
   --  :ada:ref:`Target_Name`, :ada:ref:`Un_Op`,
   --  :ada:ref:`Update_Attribute_Ref`

           
   

   
   

   function ada_elsif_stmt_part_f_cond_expr
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_elsif_stmt_part_f_cond_expr";
   --  This field can contain one of the following nodes: :ada:ref:`Allocator`,
   --  :ada:ref:`Attribute_Ref`, :ada:ref:`Base_Aggregate`, :ada:ref:`Bin_Op`,
   --  :ada:ref:`Call_Expr`, :ada:ref:`Char_Literal`, :ada:ref:`Concat_Op`,
   --  :ada:ref:`Cond_Expr`, :ada:ref:`Decl_Expr`, :ada:ref:`Dotted_Name`,
   --  :ada:ref:`Explicit_Deref`, :ada:ref:`Identifier`,
   --  :ada:ref:`Membership_Expr`, :ada:ref:`Null_Literal`,
   --  :ada:ref:`Num_Literal`, :ada:ref:`Paren_Expr`, :ada:ref:`Qual_Expr`,
   --  :ada:ref:`Quantified_Expr`, :ada:ref:`Raise_Expr`,
   --  :ada:ref:`Reduce_Attribute_Ref`, :ada:ref:`String_Literal`,
   --  :ada:ref:`Target_Name`, :ada:ref:`Un_Op`,
   --  :ada:ref:`Update_Attribute_Ref`

           
   

   
   

   function ada_elsif_stmt_part_f_stmts
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_elsif_stmt_part_f_stmts";
   --  This field contains a list that itself contains one of the following
   --  nodes: :ada:ref:`Pragma_Node`, :ada:ref:`Stmt`

           
   

   
   

   function ada_expr_p_expression_type
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_expr_p_expression_type";
   --  Return the declaration corresponding to the type of this expression
   --  after name resolution.

           
   

   
   

   function ada_expr_p_expected_expression_type
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_expr_p_expected_expression_type";
   --  Return the declaration corresponding to the expected type of this
   --  expression after name resolution.

           
   

   
   

   function ada_expr_p_is_dynamically_tagged
     (Node : ada_base_entity_Ptr;

         Imprecise_Fallback :
            
            ada_bool;

      Value_P : access ada_bool) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_expr_p_is_dynamically_tagged";
   --  Returns whether this expression is dynamically tagged (See
   --  :rmlink:`3.9.2`).

           
   

   
   

   function ada_expr_p_is_dispatching_call
     (Node : ada_base_entity_Ptr;

         Imprecise_Fallback :
            
            ada_bool;

      Value_P : access ada_bool) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_expr_p_is_dispatching_call";
   --  Returns True if this ``Name`` corresponds to a dispatching call,
   --  including:
   --
   --  * Calls done through subprogram access types.
   --
   --  * Calls to dispatching subprograms, in the object-oriented sense.
   --
   --  .. note:: This is an experimental feature. There might be some
   --     discrepancy with the GNAT concept of "dispatching call".
   --
   --  .. note:: This should only be called on a ``Name`` and ``UnOp`` or a
   --     ``BinOp``.

           
   

   
   

   function ada_expr_p_is_static_expr
     (Node : ada_base_entity_Ptr;

         Imprecise_Fallback :
            
            ada_bool;

      Value_P : access ada_bool) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_expr_p_is_static_expr";
   --  Return whether this expression is static according to the ARM definition
   --  of static. See :rmlink:`4.9`.

           
   

   
   

   function ada_expr_p_first_corresponding_decl
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_expr_p_first_corresponding_decl";
   --  Return the first decl that is lexically named like self in self's scope.

           
   

   
   

   function ada_expr_p_eval_as_int
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_big_integer) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_expr_p_eval_as_int";
   --  Statically evaluates self, and returns the value of the evaluation as an
   --  integer.
   --
   --  .. note:: In order for a call to this not to raise, the expression needs
   --     to be a static expression, as specified in :rmlink:`4.9`. You can
   --     verify whether an expression is static with the ``is_static_expr``
   --     property.
   --
   --  .. attention:: This is an experimental feature, so even if it is exposed
   --     to allow experiments, it is totally unsupported and the API and
   --     behavior are very likely to change in the future.

           
   

   
   

   function ada_expr_p_eval_as_int_in_env
     (Node : ada_base_entity_Ptr;

         Env :
            
            ada_substitution_array;

      Value_P : access ada_big_integer) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_expr_p_eval_as_int_in_env";
   --  Statically evaluates self, and returns the value of the evaluation as an
   --  integer. The given environment is used to substitute references to
   --  declarations by actual values.
   --
   --  .. note:: In order for a call to this not to raise, the expression needs
   --     to be a static expression, as specified in :rmlink:`4.9`. You can
   --     verify whether an expression is static with the ``is_static_expr``
   --     property.
   --
   --  .. attention:: This is an experimental feature, so even if it is exposed
   --     to allow experiments, it is totally unsupported and the API and
   --     behavior are very likely to change in the future.

           
   

   
   

   function ada_expr_p_eval_as_string
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_string_type) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_expr_p_eval_as_string";
   --  Statically evaluates self, and returns the value of the evaluation as a
   --  string.
   --
   --  .. note:: In order for a call to this not to raise, the expression needs
   --     to be a static expression, as specified in :rmlink:`4.9`. You can
   --     verify whether an expression is static with the ``is_static_expr``
   --     property.
   --
   --  .. attention:: This is an experimental feature, so even if it is exposed
   --     to allow experiments, it is totally unsupported and the API and
   --     behavior are very likely to change in the future.

           
   

   
   

   function ada_expr_p_eval_as_string_in_env
     (Node : ada_base_entity_Ptr;

         Env :
            
            ada_substitution_array;

      Value_P : access ada_string_type) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_expr_p_eval_as_string_in_env";
   --  Statically evaluates self, and returns the value of the evaluation as a
   --  string. The given environment is used to substitute references to
   --  declarations by actual values.
   --
   --  .. note:: In order for a call to this not to raise, the expression needs
   --     to be a static expression, as specified in :rmlink:`4.9`. You can
   --     verify whether an expression is static with the ``is_static_expr``
   --     property.
   --
   --  .. attention:: This is an experimental feature, so even if it is exposed
   --     to allow experiments, it is totally unsupported and the API and
   --     behavior are very likely to change in the future.

           
   

   
   

   function ada_expr_p_matching_nodes
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_ada_node_array) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_expr_p_matching_nodes";
   --  Return the list of AST nodes that can be a match for this expression
   --  before overloading analysis.

           
   

   
   

   function ada_abstract_state_decl_expr_f_state_decl
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_abstract_state_decl_expr_f_state_decl";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Abstract_State_Decl`, :ada:ref:`Multi_Abstract_State_Decl`,
   --  :ada:ref:`Paren_Abstract_State_Decl`

           
   

   
   

   function ada_allocator_f_subpool
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_allocator_f_subpool";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Attribute_Ref`, :ada:ref:`Call_Expr`, :ada:ref:`Char_Literal`,
   --  :ada:ref:`Dotted_Name`, :ada:ref:`Explicit_Deref`,
   --  :ada:ref:`Identifier`, :ada:ref:`Qual_Expr`,
   --  :ada:ref:`Reduce_Attribute_Ref`, :ada:ref:`String_Literal`,
   --  :ada:ref:`Target_Name`, :ada:ref:`Update_Attribute_Ref`

           
   

   
   

   function ada_allocator_f_type_or_expr
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_allocator_f_type_or_expr";
   --  This field can contain one of the following nodes: :ada:ref:`Qual_Expr`,
   --  :ada:ref:`Subtype_Indication`

           
   

   
   

   function ada_allocator_p_get_allocated_type
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_allocator_p_get_allocated_type";
   --  Return the allocated type for this allocator.

           
   

   
   

   function ada_base_aggregate_f_ancestor_expr
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_base_aggregate_f_ancestor_expr";
   --  This field can contain one of the following nodes: :ada:ref:`Allocator`,
   --  :ada:ref:`Attribute_Ref`, :ada:ref:`Base_Aggregate`, :ada:ref:`Bin_Op`,
   --  :ada:ref:`Call_Expr`, :ada:ref:`Char_Literal`, :ada:ref:`Concat_Op`,
   --  :ada:ref:`Cond_Expr`, :ada:ref:`Decl_Expr`, :ada:ref:`Dotted_Name`,
   --  :ada:ref:`Explicit_Deref`, :ada:ref:`Identifier`,
   --  :ada:ref:`Membership_Expr`, :ada:ref:`Null_Literal`,
   --  :ada:ref:`Num_Literal`, :ada:ref:`Paren_Expr`, :ada:ref:`Qual_Expr`,
   --  :ada:ref:`Quantified_Expr`, :ada:ref:`Raise_Expr`,
   --  :ada:ref:`Reduce_Attribute_Ref`, :ada:ref:`String_Literal`,
   --  :ada:ref:`Target_Name`, :ada:ref:`Un_Op`,
   --  :ada:ref:`Update_Attribute_Ref`

           
   

   
   

   function ada_base_aggregate_f_assocs
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_base_aggregate_f_assocs";
   --  This field contains a list that itself contains one of the following
   --  nodes: :ada:ref:`Aggregate_Assoc`, :ada:ref:`Iterated_Assoc`

           
   

   
   

   function ada_base_aggregate_p_aggregate_params
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_param_actual_array) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_base_aggregate_p_aggregate_params";
   --  Returns an array of pairs, associating formal parameters to actual
   --  expressions. See ``zip_with_params``.

           
   

   
   

   function ada_base_aggregate_p_is_subaggregate
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_bool) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_base_aggregate_p_is_subaggregate";
   --  Return whether this aggregate is actually a subaggregate of a
   --  multidimensional array aggregate, as described in :rmlink:`4.3.3`.

           
   

   
   

   function ada_bin_op_f_left
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_bin_op_f_left";
   --  This field can contain one of the following nodes: :ada:ref:`Allocator`,
   --  :ada:ref:`Attribute_Ref`, :ada:ref:`Base_Aggregate`, :ada:ref:`Bin_Op`,
   --  :ada:ref:`Call_Expr`, :ada:ref:`Char_Literal`, :ada:ref:`Concat_Op`,
   --  :ada:ref:`Cond_Expr`, :ada:ref:`Decl_Expr`, :ada:ref:`Dotted_Name`,
   --  :ada:ref:`Explicit_Deref`, :ada:ref:`Identifier`,
   --  :ada:ref:`Membership_Expr`, :ada:ref:`Null_Literal`,
   --  :ada:ref:`Num_Literal`, :ada:ref:`Paren_Expr`, :ada:ref:`Qual_Expr`,
   --  :ada:ref:`Quantified_Expr`, :ada:ref:`Raise_Expr`,
   --  :ada:ref:`Reduce_Attribute_Ref`, :ada:ref:`String_Literal`,
   --  :ada:ref:`Target_Name`, :ada:ref:`Un_Op`,
   --  :ada:ref:`Update_Attribute_Ref`

           
   

   
   

   function ada_bin_op_f_op
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_bin_op_f_op";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Op_And_Then`, :ada:ref:`Op_And`, :ada:ref:`Op_Div`,
   --  :ada:ref:`Op_Double_Dot`, :ada:ref:`Op_Eq`, :ada:ref:`Op_Gt`,
   --  :ada:ref:`Op_Gte`, :ada:ref:`Op_Lt`, :ada:ref:`Op_Lte`,
   --  :ada:ref:`Op_Minus`, :ada:ref:`Op_Mod`, :ada:ref:`Op_Mult`,
   --  :ada:ref:`Op_Neq`, :ada:ref:`Op_Or_Else`, :ada:ref:`Op_Or`,
   --  :ada:ref:`Op_Plus`, :ada:ref:`Op_Pow`, :ada:ref:`Op_Rem`,
   --  :ada:ref:`Op_Xor`

           
   

   
   

   function ada_bin_op_f_right
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_bin_op_f_right";
   --  This field can contain one of the following nodes: :ada:ref:`Allocator`,
   --  :ada:ref:`Attribute_Ref`, :ada:ref:`Base_Aggregate`, :ada:ref:`Bin_Op`,
   --  :ada:ref:`Call_Expr`, :ada:ref:`Char_Literal`, :ada:ref:`Concat_Op`,
   --  :ada:ref:`Cond_Expr`, :ada:ref:`Decl_Expr`, :ada:ref:`Dotted_Name`,
   --  :ada:ref:`Explicit_Deref`, :ada:ref:`Identifier`,
   --  :ada:ref:`Membership_Expr`, :ada:ref:`Null_Literal`,
   --  :ada:ref:`Num_Literal`, :ada:ref:`Paren_Expr`, :ada:ref:`Qual_Expr`,
   --  :ada:ref:`Quantified_Expr`, :ada:ref:`Raise_Expr`,
   --  :ada:ref:`Reduce_Attribute_Ref`, :ada:ref:`String_Literal`,
   --  :ada:ref:`Target_Name`, :ada:ref:`Un_Op`,
   --  :ada:ref:`Update_Attribute_Ref`

           
   

   
   

   function ada_case_expr_alternative_f_choices
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_case_expr_alternative_f_choices";
   --  This field contains a list that itself contains one of the following
   --  nodes: :ada:ref:`Allocator`, :ada:ref:`Attribute_Ref`,
   --  :ada:ref:`Base_Aggregate`, :ada:ref:`Bin_Op`, :ada:ref:`Call_Expr`,
   --  :ada:ref:`Char_Literal`, :ada:ref:`Concat_Op`, :ada:ref:`Cond_Expr`,
   --  :ada:ref:`Decl_Expr`, :ada:ref:`Discrete_Subtype_Indication`,
   --  :ada:ref:`Dotted_Name`, :ada:ref:`Explicit_Deref`,
   --  :ada:ref:`Identifier`, :ada:ref:`Membership_Expr`,
   --  :ada:ref:`Null_Literal`, :ada:ref:`Num_Literal`,
   --  :ada:ref:`Others_Designator`, :ada:ref:`Paren_Expr`,
   --  :ada:ref:`Qual_Expr`, :ada:ref:`Quantified_Expr`, :ada:ref:`Raise_Expr`,
   --  :ada:ref:`Reduce_Attribute_Ref`, :ada:ref:`String_Literal`,
   --  :ada:ref:`Target_Name`, :ada:ref:`Un_Op`,
   --  :ada:ref:`Update_Attribute_Ref`

           
   

   
   

   function ada_case_expr_alternative_f_expr
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_case_expr_alternative_f_expr";
   --  This field can contain one of the following nodes: :ada:ref:`Allocator`,
   --  :ada:ref:`Attribute_Ref`, :ada:ref:`Base_Aggregate`, :ada:ref:`Bin_Op`,
   --  :ada:ref:`Call_Expr`, :ada:ref:`Char_Literal`, :ada:ref:`Concat_Op`,
   --  :ada:ref:`Cond_Expr`, :ada:ref:`Decl_Expr`, :ada:ref:`Dotted_Name`,
   --  :ada:ref:`Explicit_Deref`, :ada:ref:`Identifier`,
   --  :ada:ref:`Membership_Expr`, :ada:ref:`Null_Literal`,
   --  :ada:ref:`Num_Literal`, :ada:ref:`Paren_Expr`, :ada:ref:`Qual_Expr`,
   --  :ada:ref:`Quantified_Expr`, :ada:ref:`Raise_Expr`,
   --  :ada:ref:`Reduce_Attribute_Ref`, :ada:ref:`String_Literal`,
   --  :ada:ref:`Target_Name`, :ada:ref:`Un_Op`,
   --  :ada:ref:`Update_Attribute_Ref`

           
   

   
   

   function ada_concat_op_f_first_operand
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_concat_op_f_first_operand";
   --  This field can contain one of the following nodes: :ada:ref:`Allocator`,
   --  :ada:ref:`Attribute_Ref`, :ada:ref:`Base_Aggregate`, :ada:ref:`Bin_Op`,
   --  :ada:ref:`Call_Expr`, :ada:ref:`Char_Literal`, :ada:ref:`Cond_Expr`,
   --  :ada:ref:`Decl_Expr`, :ada:ref:`Dotted_Name`, :ada:ref:`Explicit_Deref`,
   --  :ada:ref:`Identifier`, :ada:ref:`Null_Literal`, :ada:ref:`Num_Literal`,
   --  :ada:ref:`Paren_Expr`, :ada:ref:`Qual_Expr`, :ada:ref:`Quantified_Expr`,
   --  :ada:ref:`Raise_Expr`, :ada:ref:`Reduce_Attribute_Ref`,
   --  :ada:ref:`String_Literal`, :ada:ref:`Target_Name`, :ada:ref:`Un_Op`,
   --  :ada:ref:`Update_Attribute_Ref`

           
   

   
   

   function ada_concat_op_f_other_operands
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_concat_op_f_other_operands";
   

           
   

   
   

   function ada_concat_op_p_operands
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_ada_node_array) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_concat_op_p_operands";
   --  Return the operands of this concatenation expression

           
   

   
   

   function ada_concat_operand_f_operator
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_concat_operand_f_operator";
   

           
   

   
   

   function ada_concat_operand_f_operand
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_concat_operand_f_operand";
   --  This field can contain one of the following nodes: :ada:ref:`Allocator`,
   --  :ada:ref:`Attribute_Ref`, :ada:ref:`Base_Aggregate`, :ada:ref:`Bin_Op`,
   --  :ada:ref:`Call_Expr`, :ada:ref:`Char_Literal`, :ada:ref:`Cond_Expr`,
   --  :ada:ref:`Decl_Expr`, :ada:ref:`Dotted_Name`, :ada:ref:`Explicit_Deref`,
   --  :ada:ref:`Identifier`, :ada:ref:`Null_Literal`, :ada:ref:`Num_Literal`,
   --  :ada:ref:`Paren_Expr`, :ada:ref:`Qual_Expr`, :ada:ref:`Quantified_Expr`,
   --  :ada:ref:`Raise_Expr`, :ada:ref:`Reduce_Attribute_Ref`,
   --  :ada:ref:`String_Literal`, :ada:ref:`Target_Name`, :ada:ref:`Un_Op`,
   --  :ada:ref:`Update_Attribute_Ref`

           
   

   
   

   function ada_cond_expr_p_dependent_exprs
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_ada_node_array) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_cond_expr_p_dependent_exprs";
   --  Return the dependent expressions for this conditional expression.

           
   

   
   

   function ada_case_expr_f_expr
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_case_expr_f_expr";
   --  This field can contain one of the following nodes: :ada:ref:`Allocator`,
   --  :ada:ref:`Attribute_Ref`, :ada:ref:`Base_Aggregate`, :ada:ref:`Bin_Op`,
   --  :ada:ref:`Call_Expr`, :ada:ref:`Char_Literal`, :ada:ref:`Concat_Op`,
   --  :ada:ref:`Cond_Expr`, :ada:ref:`Decl_Expr`, :ada:ref:`Dotted_Name`,
   --  :ada:ref:`Explicit_Deref`, :ada:ref:`Identifier`,
   --  :ada:ref:`Membership_Expr`, :ada:ref:`Null_Literal`,
   --  :ada:ref:`Num_Literal`, :ada:ref:`Paren_Expr`, :ada:ref:`Qual_Expr`,
   --  :ada:ref:`Quantified_Expr`, :ada:ref:`Raise_Expr`,
   --  :ada:ref:`Reduce_Attribute_Ref`, :ada:ref:`String_Literal`,
   --  :ada:ref:`Target_Name`, :ada:ref:`Un_Op`,
   --  :ada:ref:`Update_Attribute_Ref`

           
   

   
   

   function ada_case_expr_f_cases
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_case_expr_f_cases";
   

           
   

   
   

   function ada_if_expr_f_cond_expr
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_if_expr_f_cond_expr";
   --  This field can contain one of the following nodes: :ada:ref:`Allocator`,
   --  :ada:ref:`Attribute_Ref`, :ada:ref:`Base_Aggregate`, :ada:ref:`Bin_Op`,
   --  :ada:ref:`Call_Expr`, :ada:ref:`Char_Literal`, :ada:ref:`Concat_Op`,
   --  :ada:ref:`Cond_Expr`, :ada:ref:`Decl_Expr`, :ada:ref:`Dotted_Name`,
   --  :ada:ref:`Explicit_Deref`, :ada:ref:`Identifier`,
   --  :ada:ref:`Membership_Expr`, :ada:ref:`Null_Literal`,
   --  :ada:ref:`Num_Literal`, :ada:ref:`Paren_Expr`, :ada:ref:`Qual_Expr`,
   --  :ada:ref:`Quantified_Expr`, :ada:ref:`Raise_Expr`,
   --  :ada:ref:`Reduce_Attribute_Ref`, :ada:ref:`String_Literal`,
   --  :ada:ref:`Target_Name`, :ada:ref:`Un_Op`,
   --  :ada:ref:`Update_Attribute_Ref`

           
   

   
   

   function ada_if_expr_f_then_expr
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_if_expr_f_then_expr";
   --  This field can contain one of the following nodes: :ada:ref:`Allocator`,
   --  :ada:ref:`Attribute_Ref`, :ada:ref:`Base_Aggregate`, :ada:ref:`Bin_Op`,
   --  :ada:ref:`Call_Expr`, :ada:ref:`Char_Literal`, :ada:ref:`Concat_Op`,
   --  :ada:ref:`Cond_Expr`, :ada:ref:`Decl_Expr`, :ada:ref:`Dotted_Name`,
   --  :ada:ref:`Explicit_Deref`, :ada:ref:`Identifier`,
   --  :ada:ref:`Membership_Expr`, :ada:ref:`Null_Literal`,
   --  :ada:ref:`Num_Literal`, :ada:ref:`Paren_Expr`, :ada:ref:`Qual_Expr`,
   --  :ada:ref:`Quantified_Expr`, :ada:ref:`Raise_Expr`,
   --  :ada:ref:`Reduce_Attribute_Ref`, :ada:ref:`String_Literal`,
   --  :ada:ref:`Target_Name`, :ada:ref:`Un_Op`,
   --  :ada:ref:`Update_Attribute_Ref`

           
   

   
   

   function ada_if_expr_f_alternatives
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_if_expr_f_alternatives";
   

           
   

   
   

   function ada_if_expr_f_else_expr
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_if_expr_f_else_expr";
   --  This field can contain one of the following nodes: :ada:ref:`Allocator`,
   --  :ada:ref:`Attribute_Ref`, :ada:ref:`Base_Aggregate`, :ada:ref:`Bin_Op`,
   --  :ada:ref:`Call_Expr`, :ada:ref:`Char_Literal`, :ada:ref:`Concat_Op`,
   --  :ada:ref:`Cond_Expr`, :ada:ref:`Decl_Expr`, :ada:ref:`Dotted_Name`,
   --  :ada:ref:`Explicit_Deref`, :ada:ref:`Identifier`,
   --  :ada:ref:`Membership_Expr`, :ada:ref:`Null_Literal`,
   --  :ada:ref:`Num_Literal`, :ada:ref:`Paren_Expr`, :ada:ref:`Qual_Expr`,
   --  :ada:ref:`Quantified_Expr`, :ada:ref:`Raise_Expr`,
   --  :ada:ref:`Reduce_Attribute_Ref`, :ada:ref:`String_Literal`,
   --  :ada:ref:`Target_Name`, :ada:ref:`Un_Op`,
   --  :ada:ref:`Update_Attribute_Ref`

           
   

   
   

   function ada_contract_cases_f_contract_cases
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_contract_cases_f_contract_cases";
   

           
   

   
   

   function ada_decl_expr_f_decls
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_decl_expr_f_decls";
   --  This field contains a list that itself contains one of the following
   --  nodes: :ada:ref:`Number_Decl`, :ada:ref:`Object_Decl`,
   --  :ada:ref:`Single_Protected_Decl`, :ada:ref:`Single_Task_Decl`

           
   

   
   

   function ada_decl_expr_f_expr
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_decl_expr_f_expr";
   --  This field can contain one of the following nodes: :ada:ref:`Allocator`,
   --  :ada:ref:`Attribute_Ref`, :ada:ref:`Base_Aggregate`, :ada:ref:`Bin_Op`,
   --  :ada:ref:`Call_Expr`, :ada:ref:`Char_Literal`, :ada:ref:`Concat_Op`,
   --  :ada:ref:`Cond_Expr`, :ada:ref:`Decl_Expr`, :ada:ref:`Dotted_Name`,
   --  :ada:ref:`Explicit_Deref`, :ada:ref:`Identifier`,
   --  :ada:ref:`Membership_Expr`, :ada:ref:`Null_Literal`,
   --  :ada:ref:`Num_Literal`, :ada:ref:`Paren_Expr`, :ada:ref:`Qual_Expr`,
   --  :ada:ref:`Quantified_Expr`, :ada:ref:`Raise_Expr`,
   --  :ada:ref:`Reduce_Attribute_Ref`, :ada:ref:`String_Literal`,
   --  :ada:ref:`Target_Name`, :ada:ref:`Un_Op`,
   --  :ada:ref:`Update_Attribute_Ref`

           
   

   
   

   function ada_membership_expr_f_expr
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_membership_expr_f_expr";
   --  This field can contain one of the following nodes: :ada:ref:`Allocator`,
   --  :ada:ref:`Attribute_Ref`, :ada:ref:`Base_Aggregate`, :ada:ref:`Bin_Op`,
   --  :ada:ref:`Call_Expr`, :ada:ref:`Char_Literal`, :ada:ref:`Concat_Op`,
   --  :ada:ref:`Cond_Expr`, :ada:ref:`Decl_Expr`, :ada:ref:`Dotted_Name`,
   --  :ada:ref:`Explicit_Deref`, :ada:ref:`Identifier`,
   --  :ada:ref:`Membership_Expr`, :ada:ref:`Null_Literal`,
   --  :ada:ref:`Num_Literal`, :ada:ref:`Paren_Expr`, :ada:ref:`Qual_Expr`,
   --  :ada:ref:`Quantified_Expr`, :ada:ref:`Raise_Expr`,
   --  :ada:ref:`Reduce_Attribute_Ref`, :ada:ref:`String_Literal`,
   --  :ada:ref:`Target_Name`, :ada:ref:`Un_Op`,
   --  :ada:ref:`Update_Attribute_Ref`

           
   

   
   

   function ada_membership_expr_f_op
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_membership_expr_f_op";
   --  This field can contain one of the following nodes: :ada:ref:`Op_In`,
   --  :ada:ref:`Op_Not_In`

           
   

   
   

   function ada_membership_expr_f_membership_exprs
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_membership_expr_f_membership_exprs";
   --  This field contains a list that itself contains one of the following
   --  nodes: :ada:ref:`Allocator`, :ada:ref:`Attribute_Ref`,
   --  :ada:ref:`Base_Aggregate`, :ada:ref:`Bin_Op`, :ada:ref:`Call_Expr`,
   --  :ada:ref:`Char_Literal`, :ada:ref:`Concat_Op`, :ada:ref:`Cond_Expr`,
   --  :ada:ref:`Decl_Expr`, :ada:ref:`Discrete_Subtype_Name`,
   --  :ada:ref:`Dotted_Name`, :ada:ref:`Explicit_Deref`,
   --  :ada:ref:`Identifier`, :ada:ref:`Null_Literal`, :ada:ref:`Num_Literal`,
   --  :ada:ref:`Paren_Expr`, :ada:ref:`Qual_Expr`, :ada:ref:`Quantified_Expr`,
   --  :ada:ref:`Raise_Expr`, :ada:ref:`Reduce_Attribute_Ref`,
   --  :ada:ref:`String_Literal`, :ada:ref:`Target_Name`, :ada:ref:`Un_Op`,
   --  :ada:ref:`Update_Attribute_Ref`

           
   

   
   

   function ada_name_p_enclosing_defining_name
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_name_p_enclosing_defining_name";
   --  If this name is part of a defining name, return the enclosing defining
   --  name node.

           
   

   
   

   function ada_name_p_is_defining
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_bool) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_name_p_is_defining";
   --  Return True if this name is part of a defining name.

           
   

   
   

   function ada_name_p_name_is
     (Node : ada_base_entity_Ptr;

         Sym :
            access constant
            ada_symbol_type;

      Value_P : access ada_bool) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_name_p_name_is";
   --  Helper. Check that this name matches ``sym``.

           
   

   
   

   function ada_name_p_is_direct_call
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_bool) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_name_p_is_direct_call";
   --  Return True iff this name represents a call to a subprogram which is
   --  referred by its defining name. (i.e. not through a subprogram access).

           
   

   
   

   function ada_name_p_is_access_call
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_bool) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_name_p_is_access_call";
   --  Return True iff this name represents a call to subprogram through an
   --  access type.

           
   

   
   

   function ada_name_p_is_call
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_bool) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_name_p_is_call";
   --  Returns True if this Name corresponds to a call.

           
   

   
   

   function ada_name_p_is_dot_call
     (Node : ada_base_entity_Ptr;

         Imprecise_Fallback :
            
            ada_bool;

      Value_P : access ada_bool) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_name_p_is_dot_call";
   --  Returns True if this Name corresponds to a dot notation call.

           
   

   
   

   function ada_name_p_failsafe_referenced_def_name
     (Node : ada_base_entity_Ptr;

         Imprecise_Fallback :
            
            ada_bool;

      Value_P : access ada_internal_refd_def) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_name_p_failsafe_referenced_def_name";
   --  Failsafe version of ``referenced_defining_name``. Returns a ``RefdDef``,
   --  which can be precise, imprecise, or error.

           
   

   
   

   function ada_name_p_referenced_defining_name
     (Node : ada_base_entity_Ptr;

         Imprecise_Fallback :
            
            ada_bool;

      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_name_p_referenced_defining_name";
   --  Like ``referenced_decl``, but will return the defining identifier for
   --  the decl, rather than the basic declaration node itself.

           
   

   
   

   function ada_name_p_all_env_elements
     (Node : ada_base_entity_Ptr;

         Seq :
            
            ada_bool;
         Seq_From :
            access constant
            ada_base_entity;

      Value_P : access ada_ada_node_array) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_name_p_all_env_elements";
   --  Return all elements in self's scope that are lexically named like Self.

           
   

   
   

   function ada_name_p_called_subp_spec
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_name_p_called_subp_spec";
   --  Return the subprogram specification of the subprogram or subprogram
   --  access that is being called by this exact Name, if relevant.

           
   

   
   

   function ada_name_p_referenced_decl
     (Node : ada_base_entity_Ptr;

         Imprecise_Fallback :
            
            ada_bool;

      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_name_p_referenced_decl";
   --  Return the declaration this node references after name resolution. If
   --  imprecise_fallback is True, errors raised during resolution of the xref
   --  equation are catched and a fallback mechanism is triggered, which tries
   --  to find the referenced declaration in an ad-hoc way.

           
   

   
   

   function ada_name_p_failsafe_referenced_decl
     (Node : ada_base_entity_Ptr;

         Imprecise_Fallback :
            
            ada_bool;

      Value_P : access ada_internal_refd_decl) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_name_p_failsafe_referenced_decl";
   --  Failsafe version of ``referenced_decl``. Returns a ``RefdDecl``, which
   --  can be precise, imprecise, or error.

           
   

   
   

   function ada_name_p_referenced_decl_internal
     (Node : ada_base_entity_Ptr;

         Imprecise_Fallback :
            
            ada_bool;

      Value_P : access ada_internal_refd_decl) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_name_p_referenced_decl_internal";
   --  Return the declaration this node references. Try not to run name res if
   --  already resolved.
   --
   --  .. warning:: INTERNAL USE ONLY.

           
   

   
   

   function ada_name_p_name_designated_type
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_name_p_name_designated_type";
   --  Like SubtypeIndication.designated_type, but on names, since because of
   --  Ada's ambiguous grammar, some subtype indications will be parsed as
   --  names.

           
   

   
   

   function ada_name_p_is_static_subtype
     (Node : ada_base_entity_Ptr;

         Imprecise_Fallback :
            
            ada_bool;

      Value_P : access ada_bool) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_name_p_is_static_subtype";
   --  Returns whether Self denotes a static subtype or not.

           
   

   
   

   function ada_name_p_name_matches
     (Node : ada_base_entity_Ptr;

         N :
            access constant
            ada_base_entity;

      Value_P : access ada_bool) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_name_p_name_matches";
   --  Return whether two names match each other.
   --
   --  This compares the symbol for Identifier and StringLiteral nodes. We
   --  consider that there is no match for all other node kinds.

           
   

   
   

   function ada_name_p_relative_name
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_name_p_relative_name";
   --  Returns the relative name of this instance. For example, for a prefix
   --  ``A.B.C``, this will return ``C``.

           
   

   
   

   function ada_name_p_is_operator_name
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_bool) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_name_p_is_operator_name";
   --  Return whether the name that Self designates is an operator.

           
   

   
   

   function ada_name_p_is_write_reference
     (Node : ada_base_entity_Ptr;

         Imprecise_Fallback :
            
            ada_bool;

      Value_P : access ada_bool) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_name_p_is_write_reference";
   --  Whether this name is a write reference.
   --
   --  For example, ``X`` is a write reference in the following cases:
   --
   --  1. ``X := 2;``
   --
   --  2. ``X (2) := 2;``
   --
   --  3. ``P(F => X)`` where F is declared ``out`` or ``in out``.
   --
   --  4. ``P(F => T (X))`` where F is declared ``out`` or ``in out``
   --
   --  5. ``X'Access``.
   --
   --  6. ``X.C := 2``, ``R.X := 2``
   --
   --  7. ``X.P`` where the formal for X is declared ``out`` or ``in out``.
   --
   --  .. note:: This is an experimental feature. There might be some
   --     discrepancy with the GNAT concept of "write reference".

           
   

   
   

   function ada_name_p_is_static_call
     (Node : ada_base_entity_Ptr;

         Imprecise_Fallback :
            
            ada_bool;

      Value_P : access ada_bool) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_name_p_is_static_call";
   --  Returns True if this Name corresponds to a static non-dispatching call.
   --  In other words, this will return True if and only if the target of the
   --  call is known statically.
   --
   --  .. note:: This is an experimental feature. There might be some
   --     discrepancy with the GNAT concept of "static call".

           
   

   
   

   function ada_name_p_as_symbol_array
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_unbounded_text_type_array) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_name_p_as_symbol_array";
   --  Turn this name into an array of symbols.
   --
   --  For instance, a node with name ``A.B.C`` is turned into ``['A', 'B',
   --  'C']``.
   --
   --  Only simple name kinds are allowed: Identifer, DottedName and
   --  DefiningName. Any other kind will trigger a PropertyError.

           
   

   
   

   function ada_name_p_canonical_text
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_symbol_type) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_name_p_canonical_text";
   --  Return a canonicalized version of this name's text.
   --
   --  Only simple name kinds are allowed: Identifer, DottedName and
   --  DefiningName. Any other kind will trigger a PropertyError.

           
   

   
   

   function ada_name_p_is_constant
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_bool) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_name_p_is_constant";
   --  Return whether this name denotes a constant value.

           
   

   
   

   function ada_name_p_call_params
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_param_actual_array) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_name_p_call_params";
   --  Returns an array of pairs, associating formal parameters to actual or
   --  default expressions.

           
   

   
   

   function ada_attribute_ref_f_prefix
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_attribute_ref_f_prefix";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Attribute_Ref`, :ada:ref:`Call_Expr`, :ada:ref:`Char_Literal`,
   --  :ada:ref:`Dotted_Name`, :ada:ref:`Explicit_Deref`,
   --  :ada:ref:`Identifier`, :ada:ref:`Qual_Expr`,
   --  :ada:ref:`Reduce_Attribute_Ref`, :ada:ref:`String_Literal`,
   --  :ada:ref:`Target_Name`, :ada:ref:`Update_Attribute_Ref`

           
   

   
   

   function ada_attribute_ref_f_attribute
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_attribute_ref_f_attribute";
   

           
   

   
   

   function ada_attribute_ref_f_args
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_attribute_ref_f_args";
   --  This field contains a list that itself contains one of the following
   --  nodes: :ada:ref:`Param_Assoc`

           
   

   
   

   function ada_call_expr_f_name
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_call_expr_f_name";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Attribute_Ref`, :ada:ref:`Call_Expr`, :ada:ref:`Char_Literal`,
   --  :ada:ref:`Dotted_Name`, :ada:ref:`Explicit_Deref`,
   --  :ada:ref:`Identifier`, :ada:ref:`Qual_Expr`,
   --  :ada:ref:`Reduce_Attribute_Ref`, :ada:ref:`String_Literal`,
   --  :ada:ref:`Target_Name`, :ada:ref:`Update_Attribute_Ref`

           
   

   
   

   function ada_call_expr_f_suffix
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_call_expr_f_suffix";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Attribute_Ref`, :ada:ref:`Basic_Assoc_List`,
   --  :ada:ref:`Bin_Op`, :ada:ref:`Call_Expr`, :ada:ref:`Char_Literal`,
   --  :ada:ref:`Discrete_Subtype_Indication`, :ada:ref:`Dotted_Name`,
   --  :ada:ref:`Explicit_Deref`, :ada:ref:`Identifier`, :ada:ref:`Qual_Expr`,
   --  :ada:ref:`Reduce_Attribute_Ref`, :ada:ref:`String_Literal`,
   --  :ada:ref:`Target_Name`, :ada:ref:`Update_Attribute_Ref`

           
   

   
   

   function ada_call_expr_p_kind
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_call_expr_kind) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_call_expr_p_kind";
   --  Return whether this expression is a subprogram call, an array
   --  subcomponent access expression, an array slice or a type conversion.

           
   

   
   

   function ada_call_expr_p_is_array_slice
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_bool) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_call_expr_p_is_array_slice";
   --  Return whether this CallExpr is actually an access to a slice of the
   --  array denoted by the prefix of this CallExpr.

           
   

   
   

   function ada_defining_name_f_name
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_defining_name_f_name";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Char_Literal`, :ada:ref:`Dotted_Name`, :ada:ref:`Identifier`,
   --  :ada:ref:`String_Literal`, :ada:ref:`Synthetic_Identifier`

           
   

   
   

   function ada_defining_name_p_canonical_fully_qualified_name
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_string_type) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_defining_name_p_canonical_fully_qualified_name";
   --  Return a canonical representation of the fully qualified name
   --  corresponding to this defining name.

           
   

   
   

   function ada_defining_name_p_unique_identifying_name
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_string_type) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_defining_name_p_unique_identifying_name";
   --  Return a unique identifying name for this defining name, provided this
   --  declaration is a public declaration. In the case of subprograms, this
   --  will include the profile.
   --
   --  .. attention:: This will only return a unique name for public
   --     declarations. Notably, anything nested in an unnamed declare block
   --     won't be handled correctly.

           
   

   
   

   function ada_defining_name_p_fully_qualified_name_array
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_unbounded_text_type_array) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_defining_name_p_fully_qualified_name_array";
   --  Return the fully qualified name corresponding to this defining name, as
   --  an array of symbols.

           
   

   
   

   function ada_defining_name_p_fully_qualified_name
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_string_type) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_defining_name_p_fully_qualified_name";
   --  Return the fully qualified name corresponding to this defining name.

           
   

   
   

   function ada_defining_name_p_basic_decl
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_defining_name_p_basic_decl";
   --  Returns this DefiningName's basic declaration

           
   

   
   

   function ada_defining_name_p_find_refs
     (Node : ada_base_entity_Ptr;

         Root :
            access constant
            ada_base_entity;
         Imprecise_Fallback :
            
            ada_bool;

      Value_P : access ada_ref_result_array) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_defining_name_p_find_refs";
   --  Find all references to this defining name in the given ``root`` and its
   --  children.

           
   

   
   

   function ada_defining_name_p_find_all_references
     (Node : ada_base_entity_Ptr;

         Units :
            
            ada_analysis_unit_array;
         Follow_Renamings :
            
            ada_bool;
         Imprecise_Fallback :
            
            ada_bool;

      Value_P : access ada_ref_result_array) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_defining_name_p_find_all_references";
   --  Searches all references to this defining name in the given list of
   --  units.
   --
   --  If ``follow_renamings`` is True, also this also includes references that
   --  ultimately refer to this defining name, by unwinding renaming clauses.

           
   

   
   

   function ada_defining_name_p_find_all_calls
     (Node : ada_base_entity_Ptr;

         Units :
            
            ada_analysis_unit_array;
         Follow_Renamings :
            
            ada_bool;
         Imprecise_Fallback :
            
            ada_bool;

      Value_P : access ada_ref_result_array) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_defining_name_p_find_all_calls";
   --  Return the list of all possible calls to the subprogram which Self is
   --  the defining name of.
   --
   --  This will return the name corresponding to the call, excluding the
   --  parameters if there are any. For instance, it will return ``A`` for the
   --  ``A (B)`` call.
   --
   --  .. note:: This does not yet support calls done inside generics.

           
   

   
   

   function ada_defining_name_p_next_part
     (Node : ada_base_entity_Ptr;

         Imprecise_Fallback :
            
            ada_bool;

      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_defining_name_p_next_part";
   --  Like ``BasicDecl.next_part_for_decl`` on a defining name

           
   

   
   

   function ada_defining_name_p_previous_part
     (Node : ada_base_entity_Ptr;

         Imprecise_Fallback :
            
            ada_bool;

      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_defining_name_p_previous_part";
   --  Like ``BasicDecl.previous_part_for_decl`` on a defining name

           
   

   
   

   function ada_defining_name_p_canonical_part
     (Node : ada_base_entity_Ptr;

         Imprecise_Fallback :
            
            ada_bool;

      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_defining_name_p_canonical_part";
   --  Like ``BasicDecl.canonical_part`` on a defining name

           
   

   
   

   function ada_defining_name_p_most_visible_part
     (Node : ada_base_entity_Ptr;

         Origin :
            access constant
            ada_base_entity;
         Imprecise_Fallback :
            
            ada_bool;

      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_defining_name_p_most_visible_part";
   --  Given an origin node and the entity represented by Self, this property
   --  returns the most visible completion of Self that can be seen by origin,
   --  according to Ada's visibility rules.

           
   

   
   

   function ada_defining_name_p_all_parts
     (Node : ada_base_entity_Ptr;

         Imprecise_Fallback :
            
            ada_bool;

      Value_P : access ada_ada_node_array) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_defining_name_p_all_parts";
   --  Return all parts that define this entity, sorted from first part to last
   --  part.

           
   

   
   

   function ada_defining_name_p_get_aspect
     (Node : ada_base_entity_Ptr;

         Name :
            access constant
            ada_symbol_type;
         Imprecise_Fallback :
            
            ada_bool;

      Value_P : access ada_internal_aspect) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_defining_name_p_get_aspect";
   --  Return the aspect with name ``name`` associated to entity that this name
   --  defines.
   --
   --  Aspects are properties of entities that can be specified by the Ada
   --  program, either via aspect specifications, pragmas, or attributes.
   --
   --  This will return the syntactic node corresponding to attribute directly.
   --
   --  Note: for some aspects (e.g. ``Inline``), Libadalang will check if they
   --  are defined on any part of the entity.

           
   

   
   

   function ada_defining_name_p_has_aspect
     (Node : ada_base_entity_Ptr;

         Name :
            access constant
            ada_symbol_type;
         Imprecise_Fallback :
            
            ada_bool;

      Value_P : access ada_bool) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_defining_name_p_has_aspect";
   --  Returns whether the boolean aspect named ``name`` is set on the entity
   --  represented by this node.
   --
   --  "Aspect" is used as in RM terminology (see :rmlink:`13.1`).

           
   

   
   

   function ada_defining_name_p_get_pragma
     (Node : ada_base_entity_Ptr;

         Name :
            access constant
            ada_symbol_type;

      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_defining_name_p_get_pragma";
   --  Return the pragma with name ``name`` associated to this entity.
   --
   --  Please use the ``p_get_aspects`` property instead if you are interested
   --  in aspects, i.e. information that can be represented by either aspect
   --  specification nodes, pragma nodes or attribute definition nodes.

           
   

   
   

   function ada_defining_name_p_get_representation_clause
     (Node : ada_base_entity_Ptr;

         Name :
            access constant
            ada_symbol_type;
         Imprecise_Fallback :
            
            ada_bool;

      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_defining_name_p_get_representation_clause";
   --  Return the representation clause associated to this entity that defines
   --  the given attribute name.

           
   

   
   

   function ada_defining_name_p_get_at_clause
     (Node : ada_base_entity_Ptr;

         Imprecise_Fallback :
            
            ada_bool;

      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_defining_name_p_get_at_clause";
   --  Return the at clause associated to this entity.

           
   

   
   

   function ada_defining_name_p_is_imported
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_bool) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_defining_name_p_is_imported";
   --  Whether this entity defined by this name is imported from another
   --  language.

           
   

   
   

   function ada_defining_name_p_is_ghost_code
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_bool) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_defining_name_p_is_ghost_code";
   --  Return whether the entity defined by this name is ghost or not. See
   --  SPARK RM 6.9.

           
   

   
   

   function ada_discrete_subtype_name_f_subtype
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_discrete_subtype_name_f_subtype";
   

           
   

   
   

   function ada_dotted_name_f_prefix
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_dotted_name_f_prefix";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Attribute_Ref`, :ada:ref:`Call_Expr`, :ada:ref:`Char_Literal`,
   --  :ada:ref:`Dotted_Name`, :ada:ref:`Explicit_Deref`,
   --  :ada:ref:`Identifier`, :ada:ref:`Qual_Expr`,
   --  :ada:ref:`Reduce_Attribute_Ref`, :ada:ref:`String_Literal`,
   --  :ada:ref:`Target_Name`, :ada:ref:`Update_Attribute_Ref`

           
   

   
   

   function ada_dotted_name_f_suffix
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_dotted_name_f_suffix";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Char_Literal`, :ada:ref:`Identifier`,
   --  :ada:ref:`String_Literal`

           
   

   
   

   function ada_end_name_f_name
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_end_name_f_name";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Char_Literal`, :ada:ref:`Dotted_Name`, :ada:ref:`Identifier`,
   --  :ada:ref:`String_Literal`

           
   

   
   

   function ada_end_name_p_basic_decl
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_end_name_p_basic_decl";
   --  Returns this EndName's basic declaration

           
   

   
   

   function ada_explicit_deref_f_prefix
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_explicit_deref_f_prefix";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Attribute_Ref`, :ada:ref:`Call_Expr`, :ada:ref:`Char_Literal`,
   --  :ada:ref:`Dotted_Name`, :ada:ref:`Explicit_Deref`,
   --  :ada:ref:`Identifier`, :ada:ref:`Qual_Expr`,
   --  :ada:ref:`Reduce_Attribute_Ref`, :ada:ref:`String_Literal`,
   --  :ada:ref:`Target_Name`, :ada:ref:`Update_Attribute_Ref`

           
   

   
   

   function ada_qual_expr_f_prefix
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_qual_expr_f_prefix";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Attribute_Ref`, :ada:ref:`Call_Expr`, :ada:ref:`Char_Literal`,
   --  :ada:ref:`Dotted_Name`, :ada:ref:`Explicit_Deref`,
   --  :ada:ref:`Identifier`, :ada:ref:`Qual_Expr`,
   --  :ada:ref:`Reduce_Attribute_Ref`, :ada:ref:`String_Literal`,
   --  :ada:ref:`Target_Name`, :ada:ref:`Update_Attribute_Ref`

           
   

   
   

   function ada_qual_expr_f_suffix
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_qual_expr_f_suffix";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Base_Aggregate`, :ada:ref:`Paren_Expr`

           
   

   
   

   function ada_reduce_attribute_ref_f_prefix
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_reduce_attribute_ref_f_prefix";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Attribute_Ref`, :ada:ref:`Call_Expr`, :ada:ref:`Char_Literal`,
   --  :ada:ref:`Dotted_Name`, :ada:ref:`Explicit_Deref`,
   --  :ada:ref:`Identifier`, :ada:ref:`Qual_Expr`,
   --  :ada:ref:`Reduce_Attribute_Ref`, :ada:ref:`String_Literal`,
   --  :ada:ref:`Target_Name`, :ada:ref:`Update_Attribute_Ref`,
   --  :ada:ref:`Value_Sequence`

           
   

   
   

   function ada_reduce_attribute_ref_f_attribute
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_reduce_attribute_ref_f_attribute";
   

           
   

   
   

   function ada_reduce_attribute_ref_f_args
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_reduce_attribute_ref_f_args";
   --  This field contains a list that itself contains one of the following
   --  nodes: :ada:ref:`Param_Assoc`

           
   

   
   

   function ada_char_literal_p_denoted_value
     (Node : ada_base_entity_Ptr;


      Value_P : access uint32_t) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_char_literal_p_denoted_value";
   --  Return the value that this literal denotes.

           
   

   
   

   function ada_string_literal_p_denoted_value
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_string_type) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_string_literal_p_denoted_value";
   --  Return the value that this literal denotes.

           
   

   
   

   function ada_int_literal_p_denoted_value
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_big_integer) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_int_literal_p_denoted_value";
   --  Return the value that this literal denotes.

           
   

   
   

   function ada_update_attribute_ref_f_prefix
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_update_attribute_ref_f_prefix";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Attribute_Ref`, :ada:ref:`Call_Expr`, :ada:ref:`Char_Literal`,
   --  :ada:ref:`Dotted_Name`, :ada:ref:`Explicit_Deref`,
   --  :ada:ref:`Identifier`, :ada:ref:`Qual_Expr`,
   --  :ada:ref:`Reduce_Attribute_Ref`, :ada:ref:`String_Literal`,
   --  :ada:ref:`Target_Name`, :ada:ref:`Update_Attribute_Ref`

           
   

   
   

   function ada_update_attribute_ref_f_attribute
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_update_attribute_ref_f_attribute";
   

           
   

   
   

   function ada_update_attribute_ref_f_values
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_update_attribute_ref_f_values";
   

           
   

   
   

   function ada_paren_expr_f_expr
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_paren_expr_f_expr";
   --  This field can contain one of the following nodes: :ada:ref:`Allocator`,
   --  :ada:ref:`Attribute_Ref`, :ada:ref:`Base_Aggregate`, :ada:ref:`Bin_Op`,
   --  :ada:ref:`Call_Expr`, :ada:ref:`Char_Literal`, :ada:ref:`Concat_Op`,
   --  :ada:ref:`Cond_Expr`, :ada:ref:`Decl_Expr`, :ada:ref:`Dotted_Name`,
   --  :ada:ref:`Explicit_Deref`, :ada:ref:`Identifier`,
   --  :ada:ref:`Membership_Expr`, :ada:ref:`Null_Literal`,
   --  :ada:ref:`Num_Literal`, :ada:ref:`Paren_Expr`, :ada:ref:`Qual_Expr`,
   --  :ada:ref:`Quantified_Expr`, :ada:ref:`Raise_Expr`,
   --  :ada:ref:`Reduce_Attribute_Ref`, :ada:ref:`String_Literal`,
   --  :ada:ref:`Target_Name`, :ada:ref:`Un_Op`,
   --  :ada:ref:`Update_Attribute_Ref`

           
   

   
   

   function ada_quantified_expr_f_quantifier
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_quantified_expr_f_quantifier";
   

           
   

   
   

   function ada_quantified_expr_f_loop_spec
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_quantified_expr_f_loop_spec";
   

           
   

   
   

   function ada_quantified_expr_f_expr
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_quantified_expr_f_expr";
   --  This field can contain one of the following nodes: :ada:ref:`Allocator`,
   --  :ada:ref:`Attribute_Ref`, :ada:ref:`Base_Aggregate`, :ada:ref:`Bin_Op`,
   --  :ada:ref:`Call_Expr`, :ada:ref:`Char_Literal`, :ada:ref:`Concat_Op`,
   --  :ada:ref:`Cond_Expr`, :ada:ref:`Decl_Expr`, :ada:ref:`Dotted_Name`,
   --  :ada:ref:`Explicit_Deref`, :ada:ref:`Identifier`,
   --  :ada:ref:`Membership_Expr`, :ada:ref:`Null_Literal`,
   --  :ada:ref:`Num_Literal`, :ada:ref:`Paren_Expr`, :ada:ref:`Qual_Expr`,
   --  :ada:ref:`Quantified_Expr`, :ada:ref:`Raise_Expr`,
   --  :ada:ref:`Reduce_Attribute_Ref`, :ada:ref:`String_Literal`,
   --  :ada:ref:`Target_Name`, :ada:ref:`Un_Op`,
   --  :ada:ref:`Update_Attribute_Ref`

           
   

   
   

   function ada_raise_expr_f_exception_name
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_raise_expr_f_exception_name";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Attribute_Ref`, :ada:ref:`Call_Expr`, :ada:ref:`Char_Literal`,
   --  :ada:ref:`Dotted_Name`, :ada:ref:`Explicit_Deref`,
   --  :ada:ref:`Identifier`, :ada:ref:`Qual_Expr`,
   --  :ada:ref:`Reduce_Attribute_Ref`, :ada:ref:`String_Literal`,
   --  :ada:ref:`Target_Name`, :ada:ref:`Update_Attribute_Ref`

           
   

   
   

   function ada_raise_expr_f_error_message
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_raise_expr_f_error_message";
   --  This field can contain one of the following nodes: :ada:ref:`Allocator`,
   --  :ada:ref:`Attribute_Ref`, :ada:ref:`Base_Aggregate`, :ada:ref:`Bin_Op`,
   --  :ada:ref:`Call_Expr`, :ada:ref:`Char_Literal`, :ada:ref:`Concat_Op`,
   --  :ada:ref:`Cond_Expr`, :ada:ref:`Decl_Expr`, :ada:ref:`Dotted_Name`,
   --  :ada:ref:`Explicit_Deref`, :ada:ref:`Identifier`,
   --  :ada:ref:`Membership_Expr`, :ada:ref:`Null_Literal`,
   --  :ada:ref:`Num_Literal`, :ada:ref:`Paren_Expr`, :ada:ref:`Qual_Expr`,
   --  :ada:ref:`Quantified_Expr`, :ada:ref:`Raise_Expr`,
   --  :ada:ref:`Reduce_Attribute_Ref`, :ada:ref:`String_Literal`,
   --  :ada:ref:`Target_Name`, :ada:ref:`Un_Op`,
   --  :ada:ref:`Update_Attribute_Ref`

           
   

   
   

   function ada_un_op_f_op
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_un_op_f_op";
   --  This field can contain one of the following nodes: :ada:ref:`Op_Abs`,
   --  :ada:ref:`Op_Minus`, :ada:ref:`Op_Not`, :ada:ref:`Op_Plus`

           
   

   
   

   function ada_un_op_f_expr
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_un_op_f_expr";
   --  This field can contain one of the following nodes: :ada:ref:`Allocator`,
   --  :ada:ref:`Attribute_Ref`, :ada:ref:`Base_Aggregate`, :ada:ref:`Bin_Op`,
   --  :ada:ref:`Call_Expr`, :ada:ref:`Char_Literal`, :ada:ref:`Cond_Expr`,
   --  :ada:ref:`Decl_Expr`, :ada:ref:`Dotted_Name`, :ada:ref:`Explicit_Deref`,
   --  :ada:ref:`Identifier`, :ada:ref:`Null_Literal`, :ada:ref:`Num_Literal`,
   --  :ada:ref:`Paren_Expr`, :ada:ref:`Qual_Expr`, :ada:ref:`Quantified_Expr`,
   --  :ada:ref:`Raise_Expr`, :ada:ref:`Reduce_Attribute_Ref`,
   --  :ada:ref:`String_Literal`, :ada:ref:`Target_Name`, :ada:ref:`Un_Op`,
   --  :ada:ref:`Update_Attribute_Ref`

           
   

   
   

   function ada_handled_stmts_f_stmts
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_handled_stmts_f_stmts";
   --  This field contains a list that itself contains one of the following
   --  nodes: :ada:ref:`Pragma_Node`, :ada:ref:`Stmt`

           
   

   
   

   function ada_handled_stmts_f_exceptions
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_handled_stmts_f_exceptions";
   --  This field contains a list that itself contains one of the following
   --  nodes: :ada:ref:`Exception_Handler`, :ada:ref:`Pragma_Node`

           
   

   
   

   function ada_library_item_f_has_private
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_library_item_f_has_private";
   

           
   

   
   

   function ada_library_item_f_item
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_library_item_f_item";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Abstract_Subp_Decl`, :ada:ref:`Base_Subp_Body`,
   --  :ada:ref:`Error_Decl`, :ada:ref:`Generic_Decl`,
   --  :ada:ref:`Generic_Instantiation`, :ada:ref:`Generic_Renaming_Decl`,
   --  :ada:ref:`Package_Body`, :ada:ref:`Package_Decl`,
   --  :ada:ref:`Package_Renaming_Decl`, :ada:ref:`Subp_Decl`

           
   

   
   

   function ada_limited_node_p_as_bool
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_bool) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_limited_node_p_as_bool";
   --  Return whether this is an instance of LimitedPresent

           
   

   
   

   function ada_for_loop_spec_f_var_decl
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_for_loop_spec_f_var_decl";
   

           
   

   
   

   function ada_for_loop_spec_f_loop_type
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_for_loop_spec_f_loop_type";
   

           
   

   
   

   function ada_for_loop_spec_f_has_reverse
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_for_loop_spec_f_has_reverse";
   

           
   

   
   

   function ada_for_loop_spec_f_iter_expr
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_for_loop_spec_f_iter_expr";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Attribute_Ref`, :ada:ref:`Bin_Op`, :ada:ref:`Call_Expr`,
   --  :ada:ref:`Char_Literal`, :ada:ref:`Discrete_Subtype_Indication`,
   --  :ada:ref:`Dotted_Name`, :ada:ref:`Explicit_Deref`,
   --  :ada:ref:`Identifier`, :ada:ref:`Qual_Expr`,
   --  :ada:ref:`Reduce_Attribute_Ref`, :ada:ref:`String_Literal`,
   --  :ada:ref:`Target_Name`, :ada:ref:`Update_Attribute_Ref`

           
   

   
   

   function ada_for_loop_spec_f_iter_filter
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_for_loop_spec_f_iter_filter";
   --  This field can contain one of the following nodes: :ada:ref:`Allocator`,
   --  :ada:ref:`Attribute_Ref`, :ada:ref:`Base_Aggregate`, :ada:ref:`Bin_Op`,
   --  :ada:ref:`Call_Expr`, :ada:ref:`Char_Literal`, :ada:ref:`Concat_Op`,
   --  :ada:ref:`Cond_Expr`, :ada:ref:`Decl_Expr`, :ada:ref:`Dotted_Name`,
   --  :ada:ref:`Explicit_Deref`, :ada:ref:`Identifier`,
   --  :ada:ref:`Membership_Expr`, :ada:ref:`Null_Literal`,
   --  :ada:ref:`Num_Literal`, :ada:ref:`Paren_Expr`, :ada:ref:`Qual_Expr`,
   --  :ada:ref:`Quantified_Expr`, :ada:ref:`Raise_Expr`,
   --  :ada:ref:`Reduce_Attribute_Ref`, :ada:ref:`String_Literal`,
   --  :ada:ref:`Target_Name`, :ada:ref:`Un_Op`,
   --  :ada:ref:`Update_Attribute_Ref`

           
   

   
   

   function ada_while_loop_spec_f_expr
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_while_loop_spec_f_expr";
   --  This field can contain one of the following nodes: :ada:ref:`Allocator`,
   --  :ada:ref:`Attribute_Ref`, :ada:ref:`Base_Aggregate`, :ada:ref:`Bin_Op`,
   --  :ada:ref:`Call_Expr`, :ada:ref:`Char_Literal`, :ada:ref:`Concat_Op`,
   --  :ada:ref:`Cond_Expr`, :ada:ref:`Decl_Expr`, :ada:ref:`Dotted_Name`,
   --  :ada:ref:`Explicit_Deref`, :ada:ref:`Identifier`,
   --  :ada:ref:`Membership_Expr`, :ada:ref:`Null_Literal`,
   --  :ada:ref:`Num_Literal`, :ada:ref:`Paren_Expr`, :ada:ref:`Qual_Expr`,
   --  :ada:ref:`Quantified_Expr`, :ada:ref:`Raise_Expr`,
   --  :ada:ref:`Reduce_Attribute_Ref`, :ada:ref:`String_Literal`,
   --  :ada:ref:`Target_Name`, :ada:ref:`Un_Op`,
   --  :ada:ref:`Update_Attribute_Ref`

           
   

   
   

   function ada_multi_abstract_state_decl_f_decls
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_multi_abstract_state_decl_f_decls";
   --  This field contains a list that itself contains one of the following
   --  nodes: :ada:ref:`Abstract_State_Decl`,
   --  :ada:ref:`Paren_Abstract_State_Decl`

           
   

   
   

   function ada_not_null_p_as_bool
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_bool) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_not_null_p_as_bool";
   --  Return whether this is an instance of NotNullPresent

           
   

   
   

   function ada_params_f_params
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_params_f_params";
   

           
   

   
   

   function ada_paren_abstract_state_decl_f_decl
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_paren_abstract_state_decl_f_decl";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Abstract_State_Decl`, :ada:ref:`Paren_Abstract_State_Decl`

           
   

   
   

   function ada_pp_elsif_directive_f_expr
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_pp_elsif_directive_f_expr";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Attribute_Ref`, :ada:ref:`Bin_Op`, :ada:ref:`Identifier`,
   --  :ada:ref:`Paren_Expr`, :ada:ref:`Un_Op`

           
   

   
   

   function ada_pp_elsif_directive_f_then_kw
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_pp_elsif_directive_f_then_kw";
   

           
   

   
   

   function ada_pp_if_directive_f_expr
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_pp_if_directive_f_expr";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Attribute_Ref`, :ada:ref:`Bin_Op`, :ada:ref:`Identifier`,
   --  :ada:ref:`Paren_Expr`, :ada:ref:`Un_Op`

           
   

   
   

   function ada_pp_if_directive_f_then_kw
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_pp_if_directive_f_then_kw";
   

           
   

   
   

   function ada_pragma_node_f_id
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_pragma_node_f_id";
   

           
   

   
   

   function ada_pragma_node_f_args
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_pragma_node_f_args";
   

           
   

   
   

   function ada_pragma_node_p_is_ghost_code
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_bool) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_pragma_node_p_is_ghost_code";
   --  Return whether this pragma is ghost code or not. See SPARK RM 6.9.

           
   

   
   

   function ada_pragma_node_p_associated_entities
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_ada_node_array) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_pragma_node_p_associated_entities";
   --  Return an array of ``BasicDecl`` instances associated with this pragma,
   --  or an empty array if non applicable.

           
   

   
   

   function ada_private_node_p_as_bool
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_bool) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_private_node_p_as_bool";
   --  Return whether this is an instance of PrivatePresent

           
   

   
   

   function ada_protected_def_f_public_part
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_protected_def_f_public_part";
   

           
   

   
   

   function ada_protected_def_f_private_part
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_protected_def_f_private_part";
   

           
   

   
   

   function ada_protected_def_f_end_name
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_protected_def_f_end_name";
   

           
   

   
   

   function ada_protected_node_p_as_bool
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_bool) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_protected_node_p_as_bool";
   --  Return whether this is an instance of ProtectedPresent

           
   

   
   

   function ada_range_spec_f_range
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_range_spec_f_range";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Attribute_Ref`, :ada:ref:`Bin_Op`, :ada:ref:`Box_Expr`,
   --  :ada:ref:`Call_Expr`, :ada:ref:`Char_Literal`, :ada:ref:`Dotted_Name`,
   --  :ada:ref:`Explicit_Deref`, :ada:ref:`Identifier`, :ada:ref:`Qual_Expr`,
   --  :ada:ref:`Reduce_Attribute_Ref`, :ada:ref:`String_Literal`,
   --  :ada:ref:`Target_Name`, :ada:ref:`Update_Attribute_Ref`

           
   

   
   

   function ada_renaming_clause_f_renamed_object
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_renaming_clause_f_renamed_object";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Attribute_Ref`, :ada:ref:`Call_Expr`, :ada:ref:`Char_Literal`,
   --  :ada:ref:`Dotted_Name`, :ada:ref:`Explicit_Deref`,
   --  :ada:ref:`Identifier`, :ada:ref:`Qual_Expr`,
   --  :ada:ref:`Reduce_Attribute_Ref`, :ada:ref:`String_Literal`,
   --  :ada:ref:`Target_Name`, :ada:ref:`Update_Attribute_Ref`

           
   

   
   

   function ada_reverse_node_p_as_bool
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_bool) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_reverse_node_p_as_bool";
   --  Return whether this is an instance of ReversePresent

           
   

   
   

   function ada_select_when_part_f_cond_expr
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_select_when_part_f_cond_expr";
   --  This field can contain one of the following nodes: :ada:ref:`Allocator`,
   --  :ada:ref:`Attribute_Ref`, :ada:ref:`Base_Aggregate`, :ada:ref:`Bin_Op`,
   --  :ada:ref:`Call_Expr`, :ada:ref:`Char_Literal`, :ada:ref:`Concat_Op`,
   --  :ada:ref:`Cond_Expr`, :ada:ref:`Decl_Expr`, :ada:ref:`Dotted_Name`,
   --  :ada:ref:`Explicit_Deref`, :ada:ref:`Identifier`,
   --  :ada:ref:`Membership_Expr`, :ada:ref:`Null_Literal`,
   --  :ada:ref:`Num_Literal`, :ada:ref:`Paren_Expr`, :ada:ref:`Qual_Expr`,
   --  :ada:ref:`Quantified_Expr`, :ada:ref:`Raise_Expr`,
   --  :ada:ref:`Reduce_Attribute_Ref`, :ada:ref:`String_Literal`,
   --  :ada:ref:`Target_Name`, :ada:ref:`Un_Op`,
   --  :ada:ref:`Update_Attribute_Ref`

           
   

   
   

   function ada_select_when_part_f_stmts
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_select_when_part_f_stmts";
   --  This field contains a list that itself contains one of the following
   --  nodes: :ada:ref:`Pragma_Node`, :ada:ref:`Stmt`

           
   

   
   

   function ada_stmt_p_is_ghost_code
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_bool) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_stmt_p_is_ghost_code";
   --  Return whether this statement is ghost code or not. See SPARK RM 6.9.

           
   

   
   

   function ada_accept_stmt_f_name
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_accept_stmt_f_name";
   

           
   

   
   

   function ada_accept_stmt_f_entry_index_expr
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_accept_stmt_f_entry_index_expr";
   --  This field can contain one of the following nodes: :ada:ref:`Allocator`,
   --  :ada:ref:`Attribute_Ref`, :ada:ref:`Base_Aggregate`, :ada:ref:`Bin_Op`,
   --  :ada:ref:`Call_Expr`, :ada:ref:`Char_Literal`, :ada:ref:`Concat_Op`,
   --  :ada:ref:`Cond_Expr`, :ada:ref:`Decl_Expr`, :ada:ref:`Dotted_Name`,
   --  :ada:ref:`Explicit_Deref`, :ada:ref:`Identifier`,
   --  :ada:ref:`Membership_Expr`, :ada:ref:`Null_Literal`,
   --  :ada:ref:`Num_Literal`, :ada:ref:`Paren_Expr`, :ada:ref:`Qual_Expr`,
   --  :ada:ref:`Quantified_Expr`, :ada:ref:`Raise_Expr`,
   --  :ada:ref:`Reduce_Attribute_Ref`, :ada:ref:`String_Literal`,
   --  :ada:ref:`Target_Name`, :ada:ref:`Un_Op`,
   --  :ada:ref:`Update_Attribute_Ref`

           
   

   
   

   function ada_accept_stmt_f_params
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_accept_stmt_f_params";
   

           
   

   
   

   function ada_accept_stmt_p_corresponding_entry
     (Node : ada_base_entity_Ptr;

         Origin :
            access constant
            ada_base_entity;

      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_accept_stmt_p_corresponding_entry";
   --  Return the entry which corresponds to this accept statement.

           
   

   
   

   function ada_accept_stmt_with_stmts_f_stmts
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_accept_stmt_with_stmts_f_stmts";
   

           
   

   
   

   function ada_accept_stmt_with_stmts_f_end_name
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_accept_stmt_with_stmts_f_end_name";
   

           
   

   
   

   function ada_base_loop_stmt_f_spec
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_base_loop_stmt_f_spec";
   

           
   

   
   

   function ada_base_loop_stmt_f_stmts
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_base_loop_stmt_f_stmts";
   --  This field contains a list that itself contains one of the following
   --  nodes: :ada:ref:`Pragma_Node`, :ada:ref:`Stmt`

           
   

   
   

   function ada_base_loop_stmt_f_end_name
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_base_loop_stmt_f_end_name";
   

           
   

   
   

   function ada_begin_block_f_stmts
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_begin_block_f_stmts";
   

           
   

   
   

   function ada_begin_block_f_end_name
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_begin_block_f_end_name";
   

           
   

   
   

   function ada_decl_block_f_decls
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_decl_block_f_decls";
   

           
   

   
   

   function ada_decl_block_f_stmts
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_decl_block_f_stmts";
   

           
   

   
   

   function ada_decl_block_f_end_name
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_decl_block_f_end_name";
   

           
   

   
   

   function ada_case_stmt_f_expr
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_case_stmt_f_expr";
   --  This field can contain one of the following nodes: :ada:ref:`Allocator`,
   --  :ada:ref:`Attribute_Ref`, :ada:ref:`Base_Aggregate`, :ada:ref:`Bin_Op`,
   --  :ada:ref:`Call_Expr`, :ada:ref:`Char_Literal`, :ada:ref:`Concat_Op`,
   --  :ada:ref:`Cond_Expr`, :ada:ref:`Decl_Expr`, :ada:ref:`Dotted_Name`,
   --  :ada:ref:`Explicit_Deref`, :ada:ref:`Identifier`,
   --  :ada:ref:`Membership_Expr`, :ada:ref:`Null_Literal`,
   --  :ada:ref:`Num_Literal`, :ada:ref:`Paren_Expr`, :ada:ref:`Qual_Expr`,
   --  :ada:ref:`Quantified_Expr`, :ada:ref:`Raise_Expr`,
   --  :ada:ref:`Reduce_Attribute_Ref`, :ada:ref:`String_Literal`,
   --  :ada:ref:`Target_Name`, :ada:ref:`Un_Op`,
   --  :ada:ref:`Update_Attribute_Ref`

           
   

   
   

   function ada_case_stmt_f_pragmas
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_case_stmt_f_pragmas";
   

           
   

   
   

   function ada_case_stmt_f_alternatives
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_case_stmt_f_alternatives";
   

           
   

   
   

   function ada_extended_return_stmt_f_decl
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_extended_return_stmt_f_decl";
   

           
   

   
   

   function ada_extended_return_stmt_f_stmts
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_extended_return_stmt_f_stmts";
   

           
   

   
   

   function ada_if_stmt_f_cond_expr
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_if_stmt_f_cond_expr";
   --  This field can contain one of the following nodes: :ada:ref:`Allocator`,
   --  :ada:ref:`Attribute_Ref`, :ada:ref:`Base_Aggregate`, :ada:ref:`Bin_Op`,
   --  :ada:ref:`Call_Expr`, :ada:ref:`Char_Literal`, :ada:ref:`Concat_Op`,
   --  :ada:ref:`Cond_Expr`, :ada:ref:`Decl_Expr`, :ada:ref:`Dotted_Name`,
   --  :ada:ref:`Explicit_Deref`, :ada:ref:`Identifier`,
   --  :ada:ref:`Membership_Expr`, :ada:ref:`Null_Literal`,
   --  :ada:ref:`Num_Literal`, :ada:ref:`Paren_Expr`, :ada:ref:`Qual_Expr`,
   --  :ada:ref:`Quantified_Expr`, :ada:ref:`Raise_Expr`,
   --  :ada:ref:`Reduce_Attribute_Ref`, :ada:ref:`String_Literal`,
   --  :ada:ref:`Target_Name`, :ada:ref:`Un_Op`,
   --  :ada:ref:`Update_Attribute_Ref`

           
   

   
   

   function ada_if_stmt_f_then_stmts
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_if_stmt_f_then_stmts";
   --  This field contains a list that itself contains one of the following
   --  nodes: :ada:ref:`Pragma_Node`, :ada:ref:`Stmt`

           
   

   
   

   function ada_if_stmt_f_alternatives
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_if_stmt_f_alternatives";
   

           
   

   
   

   function ada_if_stmt_f_else_stmts
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_if_stmt_f_else_stmts";
   --  This field contains a list that itself contains one of the following
   --  nodes: :ada:ref:`Pragma_Node`, :ada:ref:`Stmt`

           
   

   
   

   function ada_named_stmt_f_decl
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_named_stmt_f_decl";
   

           
   

   
   

   function ada_named_stmt_f_stmt
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_named_stmt_f_stmt";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Base_Loop_Stmt`, :ada:ref:`Block_Stmt`

           
   

   
   

   function ada_select_stmt_f_guards
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_select_stmt_f_guards";
   

           
   

   
   

   function ada_select_stmt_f_else_stmts
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_select_stmt_f_else_stmts";
   --  This field contains a list that itself contains one of the following
   --  nodes: :ada:ref:`Pragma_Node`, :ada:ref:`Stmt`

           
   

   
   

   function ada_select_stmt_f_abort_stmts
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_select_stmt_f_abort_stmts";
   --  This field contains a list that itself contains one of the following
   --  nodes: :ada:ref:`Pragma_Node`, :ada:ref:`Stmt`

           
   

   
   

   function ada_abort_stmt_f_names
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_abort_stmt_f_names";
   --  This field contains a list that itself contains one of the following
   --  nodes: :ada:ref:`Attribute_Ref`, :ada:ref:`Call_Expr`,
   --  :ada:ref:`Char_Literal`, :ada:ref:`Dotted_Name`,
   --  :ada:ref:`Explicit_Deref`, :ada:ref:`Identifier`, :ada:ref:`Qual_Expr`,
   --  :ada:ref:`Reduce_Attribute_Ref`, :ada:ref:`String_Literal`,
   --  :ada:ref:`Target_Name`, :ada:ref:`Update_Attribute_Ref`

           
   

   
   

   function ada_assign_stmt_f_dest
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_assign_stmt_f_dest";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Attribute_Ref`, :ada:ref:`Call_Expr`, :ada:ref:`Char_Literal`,
   --  :ada:ref:`Dotted_Name`, :ada:ref:`Explicit_Deref`,
   --  :ada:ref:`Identifier`, :ada:ref:`Qual_Expr`,
   --  :ada:ref:`Reduce_Attribute_Ref`, :ada:ref:`String_Literal`,
   --  :ada:ref:`Target_Name`, :ada:ref:`Update_Attribute_Ref`

           
   

   
   

   function ada_assign_stmt_f_expr
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_assign_stmt_f_expr";
   --  This field can contain one of the following nodes: :ada:ref:`Allocator`,
   --  :ada:ref:`Attribute_Ref`, :ada:ref:`Base_Aggregate`, :ada:ref:`Bin_Op`,
   --  :ada:ref:`Call_Expr`, :ada:ref:`Char_Literal`, :ada:ref:`Concat_Op`,
   --  :ada:ref:`Cond_Expr`, :ada:ref:`Decl_Expr`, :ada:ref:`Dotted_Name`,
   --  :ada:ref:`Explicit_Deref`, :ada:ref:`Identifier`,
   --  :ada:ref:`Membership_Expr`, :ada:ref:`Null_Literal`,
   --  :ada:ref:`Num_Literal`, :ada:ref:`Paren_Expr`, :ada:ref:`Qual_Expr`,
   --  :ada:ref:`Quantified_Expr`, :ada:ref:`Raise_Expr`,
   --  :ada:ref:`Reduce_Attribute_Ref`, :ada:ref:`String_Literal`,
   --  :ada:ref:`Target_Name`, :ada:ref:`Un_Op`,
   --  :ada:ref:`Update_Attribute_Ref`

           
   

   
   

   function ada_call_stmt_f_call
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_call_stmt_f_call";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Attribute_Ref`, :ada:ref:`Call_Expr`, :ada:ref:`Char_Literal`,
   --  :ada:ref:`Dotted_Name`, :ada:ref:`Explicit_Deref`,
   --  :ada:ref:`Identifier`, :ada:ref:`Qual_Expr`,
   --  :ada:ref:`Reduce_Attribute_Ref`, :ada:ref:`String_Literal`,
   --  :ada:ref:`Target_Name`, :ada:ref:`Update_Attribute_Ref`

           
   

   
   

   function ada_delay_stmt_f_has_until
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_delay_stmt_f_has_until";
   

           
   

   
   

   function ada_delay_stmt_f_expr
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_delay_stmt_f_expr";
   --  This field can contain one of the following nodes: :ada:ref:`Allocator`,
   --  :ada:ref:`Attribute_Ref`, :ada:ref:`Base_Aggregate`, :ada:ref:`Bin_Op`,
   --  :ada:ref:`Call_Expr`, :ada:ref:`Char_Literal`, :ada:ref:`Concat_Op`,
   --  :ada:ref:`Cond_Expr`, :ada:ref:`Decl_Expr`, :ada:ref:`Dotted_Name`,
   --  :ada:ref:`Explicit_Deref`, :ada:ref:`Identifier`,
   --  :ada:ref:`Membership_Expr`, :ada:ref:`Null_Literal`,
   --  :ada:ref:`Num_Literal`, :ada:ref:`Paren_Expr`, :ada:ref:`Qual_Expr`,
   --  :ada:ref:`Quantified_Expr`, :ada:ref:`Raise_Expr`,
   --  :ada:ref:`Reduce_Attribute_Ref`, :ada:ref:`String_Literal`,
   --  :ada:ref:`Target_Name`, :ada:ref:`Un_Op`,
   --  :ada:ref:`Update_Attribute_Ref`

           
   

   
   

   function ada_exit_stmt_f_loop_name
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_exit_stmt_f_loop_name";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Char_Literal`, :ada:ref:`Dotted_Name`, :ada:ref:`Identifier`,
   --  :ada:ref:`String_Literal`

           
   

   
   

   function ada_exit_stmt_f_cond_expr
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_exit_stmt_f_cond_expr";
   --  This field can contain one of the following nodes: :ada:ref:`Allocator`,
   --  :ada:ref:`Attribute_Ref`, :ada:ref:`Base_Aggregate`, :ada:ref:`Bin_Op`,
   --  :ada:ref:`Call_Expr`, :ada:ref:`Char_Literal`, :ada:ref:`Concat_Op`,
   --  :ada:ref:`Cond_Expr`, :ada:ref:`Decl_Expr`, :ada:ref:`Dotted_Name`,
   --  :ada:ref:`Explicit_Deref`, :ada:ref:`Identifier`,
   --  :ada:ref:`Membership_Expr`, :ada:ref:`Null_Literal`,
   --  :ada:ref:`Num_Literal`, :ada:ref:`Paren_Expr`, :ada:ref:`Qual_Expr`,
   --  :ada:ref:`Quantified_Expr`, :ada:ref:`Raise_Expr`,
   --  :ada:ref:`Reduce_Attribute_Ref`, :ada:ref:`String_Literal`,
   --  :ada:ref:`Target_Name`, :ada:ref:`Un_Op`,
   --  :ada:ref:`Update_Attribute_Ref`

           
   

   
   

   function ada_goto_stmt_f_label_name
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_goto_stmt_f_label_name";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Char_Literal`, :ada:ref:`Dotted_Name`, :ada:ref:`Identifier`,
   --  :ada:ref:`String_Literal`

           
   

   
   

   function ada_label_f_decl
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_label_f_decl";
   

           
   

   
   

   function ada_raise_stmt_f_exception_name
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_raise_stmt_f_exception_name";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Attribute_Ref`, :ada:ref:`Call_Expr`, :ada:ref:`Char_Literal`,
   --  :ada:ref:`Dotted_Name`, :ada:ref:`Explicit_Deref`,
   --  :ada:ref:`Identifier`, :ada:ref:`Qual_Expr`,
   --  :ada:ref:`Reduce_Attribute_Ref`, :ada:ref:`String_Literal`,
   --  :ada:ref:`Target_Name`, :ada:ref:`Update_Attribute_Ref`

           
   

   
   

   function ada_raise_stmt_f_error_message
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_raise_stmt_f_error_message";
   --  This field can contain one of the following nodes: :ada:ref:`Allocator`,
   --  :ada:ref:`Attribute_Ref`, :ada:ref:`Base_Aggregate`, :ada:ref:`Bin_Op`,
   --  :ada:ref:`Call_Expr`, :ada:ref:`Char_Literal`, :ada:ref:`Concat_Op`,
   --  :ada:ref:`Cond_Expr`, :ada:ref:`Decl_Expr`, :ada:ref:`Dotted_Name`,
   --  :ada:ref:`Explicit_Deref`, :ada:ref:`Identifier`,
   --  :ada:ref:`Membership_Expr`, :ada:ref:`Null_Literal`,
   --  :ada:ref:`Num_Literal`, :ada:ref:`Paren_Expr`, :ada:ref:`Qual_Expr`,
   --  :ada:ref:`Quantified_Expr`, :ada:ref:`Raise_Expr`,
   --  :ada:ref:`Reduce_Attribute_Ref`, :ada:ref:`String_Literal`,
   --  :ada:ref:`Target_Name`, :ada:ref:`Un_Op`,
   --  :ada:ref:`Update_Attribute_Ref`

           
   

   
   

   function ada_requeue_stmt_f_call_name
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_requeue_stmt_f_call_name";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Attribute_Ref`, :ada:ref:`Call_Expr`, :ada:ref:`Char_Literal`,
   --  :ada:ref:`Dotted_Name`, :ada:ref:`Explicit_Deref`,
   --  :ada:ref:`Identifier`, :ada:ref:`Qual_Expr`,
   --  :ada:ref:`Reduce_Attribute_Ref`, :ada:ref:`String_Literal`,
   --  :ada:ref:`Target_Name`, :ada:ref:`Update_Attribute_Ref`

           
   

   
   

   function ada_requeue_stmt_f_has_abort
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_requeue_stmt_f_has_abort";
   

           
   

   
   

   function ada_return_stmt_f_return_expr
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_return_stmt_f_return_expr";
   --  This field can contain one of the following nodes: :ada:ref:`Allocator`,
   --  :ada:ref:`Attribute_Ref`, :ada:ref:`Base_Aggregate`, :ada:ref:`Bin_Op`,
   --  :ada:ref:`Call_Expr`, :ada:ref:`Char_Literal`, :ada:ref:`Concat_Op`,
   --  :ada:ref:`Cond_Expr`, :ada:ref:`Decl_Expr`, :ada:ref:`Dotted_Name`,
   --  :ada:ref:`Explicit_Deref`, :ada:ref:`Identifier`,
   --  :ada:ref:`Membership_Expr`, :ada:ref:`Null_Literal`,
   --  :ada:ref:`Num_Literal`, :ada:ref:`Paren_Expr`, :ada:ref:`Qual_Expr`,
   --  :ada:ref:`Quantified_Expr`, :ada:ref:`Raise_Expr`,
   --  :ada:ref:`Reduce_Attribute_Ref`, :ada:ref:`String_Literal`,
   --  :ada:ref:`Target_Name`, :ada:ref:`Un_Op`,
   --  :ada:ref:`Update_Attribute_Ref`

           
   

   
   

   function ada_subunit_f_name
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_subunit_f_name";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Char_Literal`, :ada:ref:`Dotted_Name`, :ada:ref:`Identifier`,
   --  :ada:ref:`String_Literal`

           
   

   
   

   function ada_subunit_f_body
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_subunit_f_body";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Package_Body`, :ada:ref:`Protected_Body`,
   --  :ada:ref:`Subp_Body`, :ada:ref:`Task_Body`

           
   

   
   

   function ada_subunit_p_body_root
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_subunit_p_body_root";
   --  Return the body in which this subunit is rooted.

           
   

   
   

   function ada_synchronized_node_p_as_bool
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_bool) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_synchronized_node_p_as_bool";
   --  Return whether this is an instance of SynchronizedPresent

           
   

   
   

   function ada_tagged_node_p_as_bool
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_bool) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_tagged_node_p_as_bool";
   --  Return whether this is an instance of TaggedPresent

           
   

   
   

   function ada_task_def_f_interfaces
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_task_def_f_interfaces";
   --  This field contains a list that itself contains one of the following
   --  nodes: :ada:ref:`Char_Literal`, :ada:ref:`Dotted_Name`,
   --  :ada:ref:`Identifier`, :ada:ref:`String_Literal`

           
   

   
   

   function ada_task_def_f_public_part
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_task_def_f_public_part";
   

           
   

   
   

   function ada_task_def_f_private_part
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_task_def_f_private_part";
   

           
   

   
   

   function ada_task_def_f_end_name
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_task_def_f_end_name";
   

           
   

   
   

   function ada_access_def_f_has_not_null
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_access_def_f_has_not_null";
   

           
   

   
   

   function ada_access_to_subp_def_f_has_protected
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_access_to_subp_def_f_has_protected";
   

           
   

   
   

   function ada_access_to_subp_def_f_subp_spec
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_access_to_subp_def_f_subp_spec";
   

           
   

   
   

   function ada_anonymous_type_access_def_f_type_decl
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_anonymous_type_access_def_f_type_decl";
   

           
   

   
   

   function ada_type_access_def_f_has_all
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_type_access_def_f_has_all";
   

           
   

   
   

   function ada_type_access_def_f_has_constant
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_type_access_def_f_has_constant";
   

           
   

   
   

   function ada_type_access_def_f_subtype_indication
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_type_access_def_f_subtype_indication";
   

           
   

   
   

   function ada_array_type_def_f_indices
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_array_type_def_f_indices";
   

           
   

   
   

   function ada_array_type_def_f_component_type
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_array_type_def_f_component_type";
   

           
   

   
   

   function ada_derived_type_def_f_has_abstract
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_derived_type_def_f_has_abstract";
   

           
   

   
   

   function ada_derived_type_def_f_has_limited
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_derived_type_def_f_has_limited";
   

           
   

   
   

   function ada_derived_type_def_f_has_synchronized
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_derived_type_def_f_has_synchronized";
   

           
   

   
   

   function ada_derived_type_def_f_subtype_indication
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_derived_type_def_f_subtype_indication";
   

           
   

   
   

   function ada_derived_type_def_f_interfaces
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_derived_type_def_f_interfaces";
   --  This field contains a list that itself contains one of the following
   --  nodes: :ada:ref:`Char_Literal`, :ada:ref:`Dotted_Name`,
   --  :ada:ref:`Identifier`, :ada:ref:`String_Literal`

           
   

   
   

   function ada_derived_type_def_f_record_extension
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_derived_type_def_f_record_extension";
   

           
   

   
   

   function ada_derived_type_def_f_has_with_private
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_derived_type_def_f_has_with_private";
   

           
   

   
   

   function ada_enum_type_def_f_enum_literals
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_enum_type_def_f_enum_literals";
   

           
   

   
   

   function ada_interface_type_def_f_interface_kind
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_interface_type_def_f_interface_kind";
   

           
   

   
   

   function ada_interface_type_def_f_interfaces
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_interface_type_def_f_interfaces";
   --  This field contains a list that itself contains one of the following
   --  nodes: :ada:ref:`Char_Literal`, :ada:ref:`Dotted_Name`,
   --  :ada:ref:`Identifier`, :ada:ref:`String_Literal`

           
   

   
   

   function ada_mod_int_type_def_f_expr
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_mod_int_type_def_f_expr";
   --  This field can contain one of the following nodes: :ada:ref:`Allocator`,
   --  :ada:ref:`Attribute_Ref`, :ada:ref:`Base_Aggregate`, :ada:ref:`Bin_Op`,
   --  :ada:ref:`Box_Expr`, :ada:ref:`Call_Expr`, :ada:ref:`Char_Literal`,
   --  :ada:ref:`Concat_Op`, :ada:ref:`Cond_Expr`, :ada:ref:`Decl_Expr`,
   --  :ada:ref:`Dotted_Name`, :ada:ref:`Explicit_Deref`,
   --  :ada:ref:`Identifier`, :ada:ref:`Null_Literal`, :ada:ref:`Num_Literal`,
   --  :ada:ref:`Paren_Expr`, :ada:ref:`Qual_Expr`, :ada:ref:`Quantified_Expr`,
   --  :ada:ref:`Raise_Expr`, :ada:ref:`Reduce_Attribute_Ref`,
   --  :ada:ref:`String_Literal`, :ada:ref:`Target_Name`, :ada:ref:`Un_Op`,
   --  :ada:ref:`Update_Attribute_Ref`

           
   

   
   

   function ada_private_type_def_f_has_abstract
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_private_type_def_f_has_abstract";
   

           
   

   
   

   function ada_private_type_def_f_has_tagged
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_private_type_def_f_has_tagged";
   

           
   

   
   

   function ada_private_type_def_f_has_limited
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_private_type_def_f_has_limited";
   

           
   

   
   

   function ada_decimal_fixed_point_def_f_delta
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_decimal_fixed_point_def_f_delta";
   --  This field can contain one of the following nodes: :ada:ref:`Allocator`,
   --  :ada:ref:`Attribute_Ref`, :ada:ref:`Base_Aggregate`, :ada:ref:`Bin_Op`,
   --  :ada:ref:`Box_Expr`, :ada:ref:`Call_Expr`, :ada:ref:`Char_Literal`,
   --  :ada:ref:`Concat_Op`, :ada:ref:`Cond_Expr`, :ada:ref:`Decl_Expr`,
   --  :ada:ref:`Dotted_Name`, :ada:ref:`Explicit_Deref`,
   --  :ada:ref:`Identifier`, :ada:ref:`Null_Literal`, :ada:ref:`Num_Literal`,
   --  :ada:ref:`Paren_Expr`, :ada:ref:`Qual_Expr`, :ada:ref:`Quantified_Expr`,
   --  :ada:ref:`Raise_Expr`, :ada:ref:`Reduce_Attribute_Ref`,
   --  :ada:ref:`String_Literal`, :ada:ref:`Target_Name`, :ada:ref:`Un_Op`,
   --  :ada:ref:`Update_Attribute_Ref`

           
   

   
   

   function ada_decimal_fixed_point_def_f_digits
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_decimal_fixed_point_def_f_digits";
   --  This field can contain one of the following nodes: :ada:ref:`Allocator`,
   --  :ada:ref:`Attribute_Ref`, :ada:ref:`Base_Aggregate`, :ada:ref:`Bin_Op`,
   --  :ada:ref:`Box_Expr`, :ada:ref:`Call_Expr`, :ada:ref:`Char_Literal`,
   --  :ada:ref:`Concat_Op`, :ada:ref:`Cond_Expr`, :ada:ref:`Decl_Expr`,
   --  :ada:ref:`Dotted_Name`, :ada:ref:`Explicit_Deref`,
   --  :ada:ref:`Identifier`, :ada:ref:`Null_Literal`, :ada:ref:`Num_Literal`,
   --  :ada:ref:`Paren_Expr`, :ada:ref:`Qual_Expr`, :ada:ref:`Quantified_Expr`,
   --  :ada:ref:`Raise_Expr`, :ada:ref:`Reduce_Attribute_Ref`,
   --  :ada:ref:`String_Literal`, :ada:ref:`Target_Name`, :ada:ref:`Un_Op`,
   --  :ada:ref:`Update_Attribute_Ref`

           
   

   
   

   function ada_decimal_fixed_point_def_f_range
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_decimal_fixed_point_def_f_range";
   

           
   

   
   

   function ada_floating_point_def_f_num_digits
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_floating_point_def_f_num_digits";
   --  This field can contain one of the following nodes: :ada:ref:`Allocator`,
   --  :ada:ref:`Attribute_Ref`, :ada:ref:`Base_Aggregate`, :ada:ref:`Bin_Op`,
   --  :ada:ref:`Box_Expr`, :ada:ref:`Call_Expr`, :ada:ref:`Char_Literal`,
   --  :ada:ref:`Concat_Op`, :ada:ref:`Cond_Expr`, :ada:ref:`Decl_Expr`,
   --  :ada:ref:`Dotted_Name`, :ada:ref:`Explicit_Deref`,
   --  :ada:ref:`Identifier`, :ada:ref:`Null_Literal`, :ada:ref:`Num_Literal`,
   --  :ada:ref:`Paren_Expr`, :ada:ref:`Qual_Expr`, :ada:ref:`Quantified_Expr`,
   --  :ada:ref:`Raise_Expr`, :ada:ref:`Reduce_Attribute_Ref`,
   --  :ada:ref:`String_Literal`, :ada:ref:`Target_Name`, :ada:ref:`Un_Op`,
   --  :ada:ref:`Update_Attribute_Ref`

           
   

   
   

   function ada_floating_point_def_f_range
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_floating_point_def_f_range";
   

           
   

   
   

   function ada_ordinary_fixed_point_def_f_delta
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_ordinary_fixed_point_def_f_delta";
   --  This field can contain one of the following nodes: :ada:ref:`Allocator`,
   --  :ada:ref:`Attribute_Ref`, :ada:ref:`Base_Aggregate`, :ada:ref:`Bin_Op`,
   --  :ada:ref:`Box_Expr`, :ada:ref:`Call_Expr`, :ada:ref:`Char_Literal`,
   --  :ada:ref:`Concat_Op`, :ada:ref:`Cond_Expr`, :ada:ref:`Decl_Expr`,
   --  :ada:ref:`Dotted_Name`, :ada:ref:`Explicit_Deref`,
   --  :ada:ref:`Identifier`, :ada:ref:`Null_Literal`, :ada:ref:`Num_Literal`,
   --  :ada:ref:`Paren_Expr`, :ada:ref:`Qual_Expr`, :ada:ref:`Quantified_Expr`,
   --  :ada:ref:`Raise_Expr`, :ada:ref:`Reduce_Attribute_Ref`,
   --  :ada:ref:`String_Literal`, :ada:ref:`Target_Name`, :ada:ref:`Un_Op`,
   --  :ada:ref:`Update_Attribute_Ref`

           
   

   
   

   function ada_ordinary_fixed_point_def_f_range
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_ordinary_fixed_point_def_f_range";
   

           
   

   
   

   function ada_record_type_def_f_has_abstract
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_record_type_def_f_has_abstract";
   

           
   

   
   

   function ada_record_type_def_f_has_tagged
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_record_type_def_f_has_tagged";
   

           
   

   
   

   function ada_record_type_def_f_has_limited
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_record_type_def_f_has_limited";
   

           
   

   
   

   function ada_record_type_def_f_record_def
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_record_type_def_f_record_def";
   

           
   

   
   

   function ada_signed_int_type_def_f_range
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_signed_int_type_def_f_range";
   

           
   

   
   

   function ada_type_expr_p_type_name
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_type_expr_p_type_name";
   --  Return the name node for this type expression, if applicable, else null

           
   

   
   

   function ada_type_expr_p_designated_type_decl
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_type_expr_p_designated_type_decl";
   --  Returns the type declaration designated by this type expression.

           
   

   
   

   function ada_type_expr_p_designated_type_decl_from
     (Node : ada_base_entity_Ptr;

         Origin_Node :
            access constant
            ada_base_entity;

      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_type_expr_p_designated_type_decl_from";
   --  Return the type declaration designated by this type expression as viewed
   --  from the node given by origin_node.

           
   

   
   

   function ada_anonymous_type_f_type_decl
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_anonymous_type_f_type_decl";
   

           
   

   
   

   function ada_subtype_indication_f_has_not_null
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_subtype_indication_f_has_not_null";
   

           
   

   
   

   function ada_subtype_indication_f_name
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_subtype_indication_f_name";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Attribute_Ref`, :ada:ref:`Char_Literal`,
   --  :ada:ref:`Dotted_Name`, :ada:ref:`Identifier`, :ada:ref:`String_Literal`

           
   

   
   

   function ada_subtype_indication_f_constraint
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_subtype_indication_f_constraint";
   

           
   

   
   

   function ada_subtype_indication_p_subtype_constraints
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_param_actual_array) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_subtype_indication_p_subtype_constraints";
   --  Returns an array of pairs, associating formal parameters to actual or
   --  default expressions.

           
   

   
   

   function ada_subtype_indication_p_is_static_subtype
     (Node : ada_base_entity_Ptr;

         Imprecise_Fallback :
            
            ada_bool;

      Value_P : access ada_bool) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_subtype_indication_p_is_static_subtype";
   --  Returns whether Self denotes a static subtype or not.

           
   

   
   

   function ada_synthetic_type_expr_f_target_type
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_synthetic_type_expr_f_target_type";
   

           
   

   
   

   function ada_unconstrained_array_index_f_subtype_indication
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_unconstrained_array_index_f_subtype_indication";
   

           
   

   
   

   function ada_until_node_p_as_bool
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_bool) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_until_node_p_as_bool";
   --  Return whether this is an instance of UntilPresent

           
   

   
   

   function ada_use_package_clause_f_packages
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_use_package_clause_f_packages";
   --  This field contains a list that itself contains one of the following
   --  nodes: :ada:ref:`Char_Literal`, :ada:ref:`Dotted_Name`,
   --  :ada:ref:`Identifier`, :ada:ref:`String_Literal`

           
   

   
   

   function ada_use_type_clause_f_has_all
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_use_type_clause_f_has_all";
   

           
   

   
   

   function ada_use_type_clause_f_types
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_use_type_clause_f_types";
   --  This field contains a list that itself contains one of the following
   --  nodes: :ada:ref:`Attribute_Ref`, :ada:ref:`Call_Expr`,
   --  :ada:ref:`Char_Literal`, :ada:ref:`Dotted_Name`,
   --  :ada:ref:`Explicit_Deref`, :ada:ref:`Identifier`, :ada:ref:`Qual_Expr`,
   --  :ada:ref:`Reduce_Attribute_Ref`, :ada:ref:`String_Literal`,
   --  :ada:ref:`Target_Name`, :ada:ref:`Update_Attribute_Ref`

           
   

   
   

   function ada_value_sequence_f_iter_assoc
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_value_sequence_f_iter_assoc";
   

           
   

   
   

   function ada_variant_f_choices
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_variant_f_choices";
   --  This field contains a list that itself contains one of the following
   --  nodes: :ada:ref:`Allocator`, :ada:ref:`Attribute_Ref`,
   --  :ada:ref:`Base_Aggregate`, :ada:ref:`Bin_Op`, :ada:ref:`Call_Expr`,
   --  :ada:ref:`Char_Literal`, :ada:ref:`Concat_Op`, :ada:ref:`Cond_Expr`,
   --  :ada:ref:`Decl_Expr`, :ada:ref:`Discrete_Subtype_Indication`,
   --  :ada:ref:`Dotted_Name`, :ada:ref:`Explicit_Deref`,
   --  :ada:ref:`Identifier`, :ada:ref:`Membership_Expr`,
   --  :ada:ref:`Null_Literal`, :ada:ref:`Num_Literal`,
   --  :ada:ref:`Others_Designator`, :ada:ref:`Paren_Expr`,
   --  :ada:ref:`Qual_Expr`, :ada:ref:`Quantified_Expr`, :ada:ref:`Raise_Expr`,
   --  :ada:ref:`Reduce_Attribute_Ref`, :ada:ref:`String_Literal`,
   --  :ada:ref:`Target_Name`, :ada:ref:`Un_Op`,
   --  :ada:ref:`Update_Attribute_Ref`

           
   

   
   

   function ada_variant_f_components
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_variant_f_components";
   

           
   

   
   

   function ada_variant_part_f_discr_name
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_variant_part_f_discr_name";
   

           
   

   
   

   function ada_variant_part_f_variant
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_variant_part_f_variant";
   

           
   

   
   

   function ada_with_clause_f_has_limited
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_with_clause_f_has_limited";
   

           
   

   
   

   function ada_with_clause_f_has_private
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_with_clause_f_has_private";
   

           
   

   
   

   function ada_with_clause_f_packages
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_with_clause_f_packages";
   --  This field contains a list that itself contains one of the following
   --  nodes: :ada:ref:`Char_Literal`, :ada:ref:`Dotted_Name`,
   --  :ada:ref:`Identifier`, :ada:ref:`String_Literal`

           
   

   
   

   function ada_with_private_p_as_bool
     (Node : ada_base_entity_Ptr;


      Value_P : access ada_bool) return int

      with Export        => True,
           Convention    => C,
           External_name => "ada_with_private_p_as_bool";
   --  Return whether this is an instance of WithPrivatePresent


   ------------------------
   -- Conversion helpers --
   ------------------------

   --  The following conversion helpers are use by the various C bindings

   function Wrap (S : Source_Location) return ada_source_location is
     ((Unsigned_32 (S.Line), Unsigned_16 (S.Column)));
   function Unwrap (S : ada_source_location) return Source_Location is
     ((Line_Number (S.Line), Column_Number (S.Column)));

   function Wrap (S : Source_Location_Range) return ada_source_location_range is
     ((Start_S => (Unsigned_32 (S.Start_Line), Unsigned_16 (S.Start_Column)),
       End_S   => (Unsigned_32 (S.End_Line),   Unsigned_16 (S.End_Column))));
   function Unwrap (S : ada_source_location_range) return Source_Location_Range is
     ((Line_Number (S.Start_S.Line),
       Line_Number (S.End_S.Line),
       Column_Number (S.Start_S.Column),
       Column_Number (S.End_S.Column)));

   function Wrap (S : Unbounded_Wide_Wide_String) return ada_text;

   function Wrap_Alloc (S : Text_Type) return ada_text;
   function Wrap
     (S     : Text_Cst_Access;
      First : Positive;
      Last  : Natural) return ada_text;

   function Wrap (T : Text_Cst_Access) return ada_text is
     (if T = null
      then (Chars => System.Null_Address, Length => 0, Is_Allocated => 0)
      else (Chars => T.all'Address, Length => T.all'Length, Is_Allocated => 0));
   function Wrap (T : Text_Access) return ada_text is
     (Wrap (Text_Cst_Access (T)));

   --  The following conversions are used only at the interface between Ada and
   --  C (i.e. as parameters and return types for C entry points) for access
   --  types.  All read/writes for the pointed values are made through the
   --  access values and never through the System.Address values.  Thus, strict
   --  aliasing issues should not arise for these.
   --
   --  See <https://gcc.gnu.org/onlinedocs/gnat_ugn/
   --       Optimization-and-Strict-Aliasing.html>.

   pragma Warnings (Off, "possible aliasing problem for type");

   function Wrap_Big_Integer is new Ada.Unchecked_Conversion
     (Big_Integer_Type, ada_big_integer);
   function Unwrap_Big_Integer is new Ada.Unchecked_Conversion
     (ada_big_integer, Big_Integer_Type);

   function Wrap_Symbol is new Ada.Unchecked_Conversion
     (Symbol_Type, ada_symbol_type);
   function Unwrap_Symbol is new Ada.Unchecked_Conversion
     (ada_symbol_type, Symbol_Type);

   function Wrap is new Ada.Unchecked_Conversion
     (Bare_Ada_Node, ada_base_node);
   function Unwrap is new Ada.Unchecked_Conversion
     (ada_base_node, Bare_Ada_Node);

   function Wrap (Token : Token_Reference) return ada_token;
   function Unwrap (Token : ada_token) return Token_Reference;

   function Wrap_Private_File_Reader is new Ada.Unchecked_Conversion
     (Internal_File_Reader_Access, ada_file_reader);
   function Unwrap_Private_File_Reader is new Ada.Unchecked_Conversion
     (ada_file_reader, Internal_File_Reader_Access);

   function Wrap_Private_Event_Handler is new Ada.Unchecked_Conversion
     (Internal_Event_Handler_Access, ada_event_handler);
   function Unwrap_Private_Event_Handler is new Ada.Unchecked_Conversion
     (ada_event_handler, Internal_Event_Handler_Access);

   function Wrap_Private_Provider is new Ada.Unchecked_Conversion
     (Internal_Unit_Provider_Access, ada_unit_provider);
   function Unwrap_Private_Provider is new Ada.Unchecked_Conversion
     (ada_unit_provider, Internal_Unit_Provider_Access);

   function Convert is new Ada.Unchecked_Conversion
     (chars_ptr, System.Address);

         function Convert is new Ada.Unchecked_Conversion
           (Internal_Entity_Accept_Stmt_Array_Access, Internal_Entity_Array_Access);
         function Convert is new Ada.Unchecked_Conversion
           (Internal_Entity_Array_Access, Internal_Entity_Accept_Stmt_Array_Access);
         function Convert is new Ada.Unchecked_Conversion
           (Internal_Entity_Base_Assoc_Array_Access, Internal_Entity_Array_Access);
         function Convert is new Ada.Unchecked_Conversion
           (Internal_Entity_Array_Access, Internal_Entity_Base_Assoc_Array_Access);
         function Convert is new Ada.Unchecked_Conversion
           (Internal_Entity_Base_Formal_Param_Decl_Array_Access, Internal_Entity_Array_Access);
         function Convert is new Ada.Unchecked_Conversion
           (Internal_Entity_Array_Access, Internal_Entity_Base_Formal_Param_Decl_Array_Access);
         function Convert is new Ada.Unchecked_Conversion
           (Internal_Entity_Base_Type_Decl_Array_Access, Internal_Entity_Array_Access);
         function Convert is new Ada.Unchecked_Conversion
           (Internal_Entity_Array_Access, Internal_Entity_Base_Type_Decl_Array_Access);
         function Convert is new Ada.Unchecked_Conversion
           (Internal_Entity_Basic_Assoc_Array_Access, Internal_Entity_Array_Access);
         function Convert is new Ada.Unchecked_Conversion
           (Internal_Entity_Array_Access, Internal_Entity_Basic_Assoc_Array_Access);
         function Convert is new Ada.Unchecked_Conversion
           (Internal_Entity_Basic_Decl_Array_Access, Internal_Entity_Array_Access);
         function Convert is new Ada.Unchecked_Conversion
           (Internal_Entity_Array_Access, Internal_Entity_Basic_Decl_Array_Access);
         function Convert is new Ada.Unchecked_Conversion
           (Internal_Entity_Compilation_Unit_Array_Access, Internal_Entity_Array_Access);
         function Convert is new Ada.Unchecked_Conversion
           (Internal_Entity_Array_Access, Internal_Entity_Compilation_Unit_Array_Access);
         function Convert is new Ada.Unchecked_Conversion
           (Internal_Entity_Declarative_Part_Array_Access, Internal_Entity_Array_Access);
         function Convert is new Ada.Unchecked_Conversion
           (Internal_Entity_Array_Access, Internal_Entity_Declarative_Part_Array_Access);
         function Convert is new Ada.Unchecked_Conversion
           (Internal_Entity_Defining_Name_Array_Access, Internal_Entity_Array_Access);
         function Convert is new Ada.Unchecked_Conversion
           (Internal_Entity_Array_Access, Internal_Entity_Defining_Name_Array_Access);
         function Convert is new Ada.Unchecked_Conversion
           (Internal_Entity_Expr_Array_Access, Internal_Entity_Array_Access);
         function Convert is new Ada.Unchecked_Conversion
           (Internal_Entity_Array_Access, Internal_Entity_Expr_Array_Access);
         function Convert is new Ada.Unchecked_Conversion
           (Internal_Entity_Generic_Instantiation_Array_Access, Internal_Entity_Array_Access);
         function Convert is new Ada.Unchecked_Conversion
           (Internal_Entity_Array_Access, Internal_Entity_Generic_Instantiation_Array_Access);
         function Convert is new Ada.Unchecked_Conversion
           (Internal_Entity_Identifier_Array_Access, Internal_Entity_Array_Access);
         function Convert is new Ada.Unchecked_Conversion
           (Internal_Entity_Array_Access, Internal_Entity_Identifier_Array_Access);
         function Convert is new Ada.Unchecked_Conversion
           (Internal_Entity_Mode_Array_Access, Internal_Entity_Array_Access);
         function Convert is new Ada.Unchecked_Conversion
           (Internal_Entity_Array_Access, Internal_Entity_Mode_Array_Access);
         function Convert is new Ada.Unchecked_Conversion
           (Internal_Entity_Name_Array_Access, Internal_Entity_Array_Access);
         function Convert is new Ada.Unchecked_Conversion
           (Internal_Entity_Array_Access, Internal_Entity_Name_Array_Access);
         function Convert is new Ada.Unchecked_Conversion
           (Internal_Entity_Param_Spec_Array_Access, Internal_Entity_Array_Access);
         function Convert is new Ada.Unchecked_Conversion
           (Internal_Entity_Array_Access, Internal_Entity_Param_Spec_Array_Access);
         function Convert is new Ada.Unchecked_Conversion
           (Internal_Entity_Pragma_Node_Array_Access, Internal_Entity_Array_Access);
         function Convert is new Ada.Unchecked_Conversion
           (Internal_Entity_Array_Access, Internal_Entity_Pragma_Node_Array_Access);
         function Convert is new Ada.Unchecked_Conversion
           (Internal_Entity_Type_Decl_Array_Access, Internal_Entity_Array_Access);
         function Convert is new Ada.Unchecked_Conversion
           (Internal_Entity_Array_Access, Internal_Entity_Type_Decl_Array_Access);
         function Convert is new Ada.Unchecked_Conversion
           (Internal_Entity_Type_Expr_Array_Access, Internal_Entity_Array_Access);
         function Convert is new Ada.Unchecked_Conversion
           (Internal_Entity_Array_Access, Internal_Entity_Type_Expr_Array_Access);


   pragma Warnings (On, "possible aliasing problem for type");

end Libadalang.Implementation.C;
