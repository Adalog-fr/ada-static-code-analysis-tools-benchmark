







open Ctypes
open Foreign

(* Under Linux, disable GNAT's handling of SIGSEGV, which is incompatible with
   what the OCaml runtime is already doing. *)
let () =
  if Sys.unix then
    ignore
      (Dl.dlopen
        ~filename:"liblangkit_sigsegv_handler.so"
        ~flags:[Dl.RTLD_NOW]
        : Dl.library)

let so_ext = if Sys.unix then "so" else "dll"
let c_lib_name = Format.sprintf "libgpr_parser.%s" so_ext
let c_lib = Dl.dlopen ~filename:c_lib_name ~flags:[Dl.RTLD_NOW]

exception FileReadError of string

exception BadTypeError of string

exception OutOfBoundsError of string

exception InvalidInput of string

exception InvalidSymbolError of string

exception InvalidUnitNameError of string

exception NativeException of string

exception PreconditionFailure of string

exception PropertyError of string

exception TemplateArgsError of string

exception TemplateFormatError of string

exception TemplateInstantiationError of string

exception StaleReferenceError of string

exception SyntaxError of string

exception UnknownCharset of string





module Exception = struct

  type t = {
    kind : int;
    information : string;
  }

  let c_struct : t structure typ = structure "exception"
  let kind = field c_struct "kind" int
  let information = field c_struct "information" string
  let () = seal c_struct

  let wrap c_value_ptr =
    if is_null c_value_ptr then
      None
    else
      let c_value = !@ c_value_ptr in
      Some {
        kind = getf c_value kind;
        information = getf c_value information;
      }

  let unwrap value =
    match value with
    | None ->
        from_voidp c_struct null
    | Some value ->
        let c_value = make c_struct in
        setf c_value kind value.kind;
        setf c_value information value.information;
        allocate c_struct c_value

  let c_type = view (ptr c_struct) ~read:wrap ~write:unwrap

end

let get_last_exception = foreign ~from:c_lib
  "gpr_get_last_exception"
  (void @-> returning Exception.c_type)

(* When declaring an imported function with foreign, use raisable instead of
 returning, to check get_last_exception before returning *)
let raisable typ =
  let read value =
    match get_last_exception () with
    | None -> value
    | Some exc ->
        (match exc.kind with
         | 0 ->
             raise (FileReadError exc.information)
         | 1 ->
             raise (BadTypeError exc.information)
         | 2 ->
             raise (OutOfBoundsError exc.information)
         | 3 ->
             raise (InvalidInput exc.information)
         | 4 ->
             raise (InvalidSymbolError exc.information)
         | 5 ->
             raise (InvalidUnitNameError exc.information)
         | 6 ->
             raise (NativeException exc.information)
         | 7 ->
             raise (PreconditionFailure exc.information)
         | 8 ->
             raise (PropertyError exc.information)
         | 9 ->
             raise (TemplateArgsError exc.information)
         | 10 ->
             raise (TemplateFormatError exc.information)
         | 11 ->
             raise (TemplateInstantiationError exc.information)
         | 12 ->
             raise (StaleReferenceError exc.information)
         | 13 ->
             raise (SyntaxError exc.information)
         | 14 ->
             raise (UnknownCharset exc.information)
         | _ -> assert false)
  in
  let write value = value in
  let new_typ = view typ ~read ~write in
  returning new_typ

(* Module used to encode/decode UTF32 strings *)

(* Camomile needs to know the location of its standard library to work,
   so we use the following heuristic:
   - if the directory chosen at build time exists, we assume the installation
     is ok
   - otherwise we look for a directory 'share/camomile' next to the binary
   - otherwise we fail
*)

module type CamomileConfig = module type of CamomileLibrary.DefaultConfig

module CamomileDefaultConfig : CamomileConfig = CamomileLibrary.DefaultConfig

let ( ^/ ) = Filename.concat

let build_camomile_config root_path = (module struct
  let share_dir = root_path ^/ "share" ^/ "camomile"

  let datadir = share_dir ^/ "database"

  let localedir = share_dir ^/ "locales"

  let charmapdir = share_dir ^/ "charmaps"

  let unimapdir = share_dir ^/ "mappings"

  end : CamomileConfig)

module CamomileShareConfig =
  (val build_camomile_config
    (Filename.dirname Sys.executable_name ^/ Filename.parent_dir_name)
    : CamomileConfig)

(* In case we are building through an opam-installed env, find
   Camomile's stdlib through the appropriate opam env variable *)
module CamomileOpamConfig =
  (val
    let opam_dir = try Sys.getenv "OPAM_SWITCH_PREFIX" with _ -> "DUMMY" in
    build_camomile_config opam_dir : CamomileConfig)

let camomile_config =
  if Sys.file_exists CamomileDefaultConfig.datadir then
    (module CamomileDefaultConfig : CamomileConfig )
  else if Sys.file_exists CamomileShareConfig.datadir then
    (module CamomileShareConfig : CamomileConfig )
  else if Sys.file_exists CamomileOpamConfig.datadir then
    (module CamomileOpamConfig : CamomileConfig)
  else failwith "no camomile library found"

module CamomileConfig = (val camomile_config)

module Camomile = CamomileLibrary.Make (CamomileConfig)

module Text = struct
  type t = string

  let c_struct : t structure typ = structure "text"

  let chars = field c_struct "chars" (ptr uint32_t)

  let length = field c_struct "length" size_t

  let is_allocated = field c_struct "is_allocated" bool

  let () = seal c_struct

  let destroy_text = foreign ~from:c_lib "gpr_destroy_text"
    (ptr c_struct @-> raisable void)

  module UCS4Encoding = Camomile.CharEncoding.Make (Camomile.UCS4)

  let wrap (c_value : t structure) : t =
    let open Unsigned.Size_t in
    let open Camomile in
    let length = to_int (getf c_value length) in
    let chars = getf c_value chars in
    let f i =
      UChar.chr_of_uint (Unsigned.UInt32.to_int !@ (chars +@ i))
    in
    let result = UCS4.init length f in
    (* Now that the value is fully transformed to an ocaml value, we can
      free it by calling destroy_text *)
    destroy_text (addr c_value) ;
    UCS4Encoding.encode CharEncoding.utf8 result

  let unwrap (value : t) : t structure =
    let open Unsigned in
    let open Camomile in
    let text = UCS4Encoding.decode CharEncoding.utf8 value in
    let struct_length = Size_t.of_int (UCS4.length text) in
    let struct_chars = allocate_n uint32_t ~count:(UCS4.length text) in
    let i = ref 0 in
    let f c =
      struct_chars +@ !i <-@ (UInt32.of_int (UChar.code c));
      i := !i + 1
    in
    UCS4.iter f text ;
    let c_value = make c_struct in
    setf c_value length struct_length ;
    setf c_value chars struct_chars ;
    setf c_value is_allocated false ;
    (* We don't need to care about calling destroy_text here since we
     manually allocated the pointer, ctypes will take care of freeing the
     memory *)
    c_value

  let c_type = view c_struct ~read:wrap ~write:unwrap
end

module Character = struct
  (* Characters are encoded as strings because ocaml char are not unicode
   characters *)
  type t = string

  module UCharEncoding = Camomile.CharEncoding.Make (Camomile.UText)

  let of_int i =
    let open Camomile in
    let uchar = UChar.chr i in
    UCharEncoding.encode CharEncoding.utf8 (UText.init 1 (fun _ -> uchar))

  let of_int32 i =
    of_int (Unsigned.UInt32.to_int i)

  let wrap (c_value : Unsigned.UInt32.t) : t =
    of_int32 c_value

  let unwrap (value : string) : Unsigned.UInt32.t =
    let open Camomile in
    let text = UCharEncoding.decode CharEncoding.utf8 value in
    let uchar = UText.get text 0 in
    Unsigned.UInt32.of_int (UChar.code uchar)

  let c_type = view uint32_t ~read:wrap ~write:unwrap
end

module StringType = struct
  type t = string

  let c_struct : t structure typ = structure "string"
  let length_field = field c_struct "length" int
  let _ = field c_struct "ref_count" int
  (* Langkit strings are encoded in UTF-32 (native endianity). *)
  let content_field = field c_struct "content" uint32_t
  let () = seal c_struct

  let buffer_ptr_type = ptr uint32_t
  let c_type = ptr c_struct

  let create = foreign ~from:c_lib "gpr_create_string"
    (buffer_ptr_type @-> int @-> raisable c_type)
  let dec_ref = foreign ~from:c_lib "gpr_string_dec_ref"
    (c_type @-> raisable void)

  module UCharEncoding = Camomile.CharEncoding.Make (Camomile.UText)

  let wrap c_value_ptr =
    let open Text in
    let open Camomile in
    let c_value = !@ c_value_ptr in
    let length = getf c_value length_field in
    let content = c_value @. content_field in
    (* We use Camomile to encode utf32 strings to an ocaml string *)
    let f i = UChar.chr_of_uint (Unsigned.UInt32.to_int !@(content +@ i)) in
    let result =
      UCS4Encoding.encode CharEncoding.utf8 (UCS4.init length f)
    in
    dec_ref c_value_ptr;
    result

  let unwrap value =
    let open Text in
    let open Camomile in

    (* Create a buffer to contain the UTF-32 encoded string. *)
    let text = UCS4Encoding.decode CharEncoding.utf8 value in
    let length = UCS4.length text in
    let buffer = allocate_n uint32_t ~count:length in
    let i = ref 0 in
    let f c =
      buffer +@ !i <-@ (Unsigned.UInt32.of_int (UChar.code c));
      i := !i + 1
    in
    UCS4.iter f text ;

    (* ctypes is supposed to take care of freeing "buffer" before returning. *)
    create buffer length
end

module BigInteger = struct

  type t = Z.t

  let c_type = ptr void

  let create = foreign ~from:c_lib "gpr_create_big_integer"
    (ptr Text.c_type @-> raisable c_type)

  let text = foreign ~from:c_lib "gpr_big_integer_text"
    (c_type @-> ptr Text.c_type @-> raisable void)

  let decref = foreign ~from:c_lib "gpr_big_integer_decref"
    (c_type @-> raisable void)

  let wrap (c_value : unit ptr) : t =
    let c_text_ptr = allocate_n Text.c_type ~count:1 in
    text c_value c_text_ptr;
    decref c_value;
    Z.of_string (!@ c_text_ptr)

  let unwrap (value : t) : unit ptr =
    create (allocate Text.c_type (Z.to_string value))
end

module AnalysisUnitKind = struct
  type t =
  | UnitSpecification
  | UnitBody

  let name () = "AnalysisUnitKind"

  let wrap (c_value : int) : t =
    match c_value with
    | 0 -> UnitSpecification
    | 1 -> UnitBody
    | _ -> assert false

  let unwrap (value : t) : int =
    match value with
    | UnitSpecification -> 0
    | UnitBody -> 1

   let c_type = view int ~read:wrap ~write:unwrap
end

module LookupKind = struct
  type t =
  | Recursive
  | Flat
  | Minimal

  let name () = "LookupKind"

  let wrap (c_value : int) : t =
    match c_value with
    | 0 -> Recursive
    | 1 -> Flat
    | 2 -> Minimal
    | _ -> assert false

  let unwrap (value : t) : int =
    match value with
    | Recursive -> 0
    | Flat -> 1
    | Minimal -> 2

   let c_type = view int ~read:wrap ~write:unwrap
end

module DesignatedEnvKind = struct
  type t =
  | None
  | CurrentEnv
  | NamedEnv
  | DirectEnv

  let name () = "DesignatedEnvKind"

  let wrap (c_value : int) : t =
    match c_value with
    | 0 -> None
    | 1 -> CurrentEnv
    | 2 -> NamedEnv
    | 3 -> DirectEnv
    | _ -> assert false

  let unwrap (value : t) : int =
    match value with
    | None -> 0
    | CurrentEnv -> 1
    | NamedEnv -> 2
    | DirectEnv -> 3

   let c_type = view int ~read:wrap ~write:unwrap
end

module GrammarRule = struct
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

  let name () = "GrammarRule"

  let wrap (c_value : int) : t =
    match c_value with
    | 0 -> ProjectQualifierRule
    | 1 -> ProjectExtensionRule
    | 2 -> ProjectDeclarationRule
    | 3 -> ProjectRule
    | 4 -> DeclarativeItemsRule
    | 5 -> DeclarativeItemRule
    | 6 -> SimpleDeclarativeItemsRule
    | 7 -> SimpleDeclarativeItemRule
    | 8 -> VariableDeclRule
    | 9 -> AttributeDeclRule
    | 10 -> AssociativeArrayIndexRule
    | 11 -> PackageDeclRule
    | 12 -> PackageRenamingRule
    | 13 -> PackageExtensionRule
    | 14 -> PackageSpecRule
    | 15 -> EmptyDeclarationRule
    | 16 -> CaseConstructionRule
    | 17 -> CaseItemRule
    | 18 -> OthersDesignatorRule
    | 19 -> ChoiceRule
    | 20 -> DiscreteChoiceListRule
    | 21 -> WithDeclRule
    | 22 -> ContextClausesRule
    | 23 -> AdaWithClauseRule
    | 24 -> AdaContextRule
    | 25 -> AdaContextItemRule
    | 26 -> AdaContextSkipRule
    | 27 -> AdaUseClauseRule
    | 28 -> AdaPragmaRule
    | 29 -> AdaSubpKindRule
    | 30 -> AdaPkgKindRule
    | 31 -> AdaLibraryItemRule
    | 32 -> AdaPreludeRule
    | 33 -> TypedStringDeclRule
    | 34 -> IdentifierRule
    | 35 -> StringLiteralRule
    | 36 -> NumLiteralRule
    | 37 -> StaticNameRule
    | 38 -> AttributeReferenceRule
    | 39 -> VariableReferenceRule
    | 40 -> TypeReferenceRule
    | 41 -> BuiltinFunctionCallRule
    | 42 -> ExpressionRule
    | 43 -> ExpressionListRule
    | 44 -> StringLiteralAtRule
    | 45 -> ProjectReferenceRule
    | 46 -> TermRule
    | 47 -> CompilationUnitRule
    | _ -> assert false

  let unwrap (value : t) : int =
    match value with
    | ProjectQualifierRule -> 0
    | ProjectExtensionRule -> 1
    | ProjectDeclarationRule -> 2
    | ProjectRule -> 3
    | DeclarativeItemsRule -> 4
    | DeclarativeItemRule -> 5
    | SimpleDeclarativeItemsRule -> 6
    | SimpleDeclarativeItemRule -> 7
    | VariableDeclRule -> 8
    | AttributeDeclRule -> 9
    | AssociativeArrayIndexRule -> 10
    | PackageDeclRule -> 11
    | PackageRenamingRule -> 12
    | PackageExtensionRule -> 13
    | PackageSpecRule -> 14
    | EmptyDeclarationRule -> 15
    | CaseConstructionRule -> 16
    | CaseItemRule -> 17
    | OthersDesignatorRule -> 18
    | ChoiceRule -> 19
    | DiscreteChoiceListRule -> 20
    | WithDeclRule -> 21
    | ContextClausesRule -> 22
    | AdaWithClauseRule -> 23
    | AdaContextRule -> 24
    | AdaContextItemRule -> 25
    | AdaContextSkipRule -> 26
    | AdaUseClauseRule -> 27
    | AdaPragmaRule -> 28
    | AdaSubpKindRule -> 29
    | AdaPkgKindRule -> 30
    | AdaLibraryItemRule -> 31
    | AdaPreludeRule -> 32
    | TypedStringDeclRule -> 33
    | IdentifierRule -> 34
    | StringLiteralRule -> 35
    | NumLiteralRule -> 36
    | StaticNameRule -> 37
    | AttributeReferenceRule -> 38
    | VariableReferenceRule -> 39
    | TypeReferenceRule -> 40
    | BuiltinFunctionCallRule -> 41
    | ExpressionRule -> 42
    | ExpressionListRule -> 43
    | StringLiteralAtRule -> 44
    | ProjectReferenceRule -> 45
    | TermRule -> 46
    | CompilationUnitRule -> 47

   let c_type = view int ~read:wrap ~write:unwrap
end


let free = foreign ~from:c_lib
  "gpr_free"
  (ptr void @-> returning void)

(** Assuming char_ptr is a valid char*, convert it to a native Ocaml
  * string and free the C pointer.
  *)
let unwrap_str char_ptr =
  let str = Ctypes.coerce (ptr char) string char_ptr in
  free (Ctypes.coerce (ptr char) (ptr void) char_ptr);
  str


let default_grammar_rule = GrammarRule.CompilationUnitRule

module Sloc = struct
  type t = {
    line : int;
    column : int;
  }

  let c_struct : t structure typ = structure "sloc"
  let line = field c_struct "line" uint32_t
  let column = field c_struct "column" uint16_t
  let () = seal c_struct

  let wrap (c_value : t structure) : t = {
    line = Unsigned.UInt32.to_int (getf c_value line);
    column = Unsigned.UInt16.to_int (getf c_value column);
  }

  let unwrap (value : t) : t structure =
    let c_value = make c_struct in
    setf c_value line (Unsigned.UInt32.of_int (value.line));
    setf c_value column (Unsigned.UInt16.of_int (value.column));
    c_value

  let c_type = view c_struct ~read:wrap ~write:unwrap
end

module SlocRange = struct
  type t = {
    loc_start : Sloc.t;
    loc_end : Sloc.t;
  }

  let c_struct : t structure typ = structure "sloc_range"
  let loc_start = field c_struct "loc_start" Sloc.c_type
  let loc_end = field c_struct "loc_end" Sloc.c_type
  let () = seal c_struct

  let wrap (c_value : t structure) : t = {
    loc_start = getf c_value loc_start;
    loc_end = getf c_value loc_end;
  }

  let unwrap (value : t) : t structure =
    let c_value = make c_struct in
    setf c_value loc_start value.loc_start;
    setf c_value loc_end value.loc_end;
    c_value

  let c_type = view c_struct ~read:wrap ~write:unwrap

  let pp fmt sloc_range =
    Format.fprintf fmt "<SlocRange %d:%d-%d:%d>"
      sloc_range.loc_start.line
      sloc_range.loc_start.column
      sloc_range.loc_end.line
      sloc_range.loc_end.column
end

module Diagnostic = struct
  type t = {
    sloc_range : SlocRange.t;
    message : string
  }

  let c_struct : t structure typ = structure "diagnostic"
  let sloc_range = field c_struct "sloc_range" SlocRange.c_type
  let message = field c_struct "message" Text.c_type
  let () = seal c_struct

  let wrap (c_value : t structure) : t = {
    sloc_range = getf c_value sloc_range;
    message = getf c_value message;
  }

  let unwrap (value : t) : t structure =
    let c_value = make c_struct in
    setf c_value sloc_range value.sloc_range;
    setf c_value message value.message;
    c_value

  let c_type = view c_struct ~read:wrap ~write:unwrap
end

module TokenData = struct
  type t = unit ptr
end

module Token = struct
  (* We don't have access to AnalysisContextStruct at this point. We don't need
     to do anything with the context value except pass it around, so map it as
     an opaque pointer instead. *)
  type dummy_context = unit ptr

  type t = {
    context : dummy_context;
    token_data : TokenData.t;
    token_index : int;
    trivia_index : int;
    kind : int;
    text : string;
    sloc_range : SlocRange.t;
  }

  let c_type : t structure typ = structure "token"
  let context = field c_type "context" (ptr void)
  let token_data = field c_type "token_data" (ptr void)
  let token_index = field c_type "token_index" int
  let trivia_index = field c_type "trivia_index" int
  let kind = field c_type "kind" int
  let text = field c_type "text" Text.c_type
  let sloc_range = field c_type "sloc_range" SlocRange.c_type
  let () = seal c_type

  let wrap (c_value : t structure) : t option =
  let token_data = getf c_value token_data in
  if is_null token_data then
    None
  else
    Some {
      context = getf c_value context;
      token_data;
      token_index = getf c_value token_index;
      trivia_index = getf c_value trivia_index;
      kind = getf c_value kind;
      text = getf c_value text;
      sloc_range = getf c_value sloc_range;
    }

  let unwrap (value : t) : t structure =
    let c_value = make c_type in
    setf c_value context value.context;
    setf c_value token_data value.token_data;
    setf c_value token_index value.token_index;
    setf c_value trivia_index value.trivia_index;
    setf c_value kind value.kind;
    setf c_value text value.text;
    setf c_value sloc_range value.sloc_range;
    c_value

  let _token_kind_name = foreign ~from:c_lib
    "gpr_token_kind_name"
    (int @-> raisable (ptr char))

  let token_kind_name kind =
    unwrap_str (_token_kind_name kind)

  let kind_name token = token_kind_name token.kind

  let token_range_text = foreign ~from:c_lib
    "gpr_token_range_text"
    (ptr c_type @-> ptr c_type @-> ptr Text.c_type @-> raisable int)

  let token_next = foreign ~from:c_lib
    "gpr_token_next"
    (ptr c_type @-> ptr c_type @-> raisable void)

  let token_previous = foreign ~from:c_lib
    "gpr_token_previous"
    (ptr c_type @-> ptr c_type @-> raisable void)

  let is_equivalent = foreign ~from:c_lib
    "gpr_token_is_equivalent"
    (ptr c_type @-> ptr c_type @-> raisable bool)

  let pp fmt token =
    let pp_text fmt = function
      | "" -> Format.pp_print_string fmt ""
      | _ as text -> Format.fprintf fmt " %S" text
    in
    Format.fprintf fmt "<Token %s%a at %a>"
      (kind_name token)
      pp_text token.text
      SlocRange.pp token.sloc_range

  let text_range token_first token_last =
    let c_result_ptr = allocate_n Text.c_type ~count:1 in
    let res =
      token_range_text
        (addr (unwrap token_first))
        (addr (unwrap token_last))
        c_result_ptr
    in
    if res = 0 then
      raise (Invalid_argument
        (Format.asprintf "%a and %a come from different units"
          pp token_first
          pp token_last));
    !@ c_result_ptr

  let next token =
    let c_next_token_ptr = allocate_n c_type ~count:1 in
    token_next (addr (unwrap token)) c_next_token_ptr ;
    wrap (!@ c_next_token_ptr)

  let previous token =
    let c_next_token_ptr = allocate_n c_type ~count:1 in
    token_previous (addr (unwrap token)) c_next_token_ptr ;
    wrap (!@ c_next_token_ptr)

  let is_trivia token =
    token.trivia_index != 0

  let index token =
    match token.trivia_index with
    | 0 ->
        token.token_index - 1
    | _ ->
        token.trivia_index - 1

  let compare one other =
    let open Stdlib in
    let compare_token_data = compare one.token_data other.token_data in
    if compare_token_data = 0 then
      let compare_token_index = compare one.token_index other.token_index in
      if compare_token_index = 0 then
        compare one.trivia_index other.trivia_index
      else
        compare_token_index
    else
      compare_token_data

  let equal one other =
    compare one other = 0

  let hash token =
    Hashtbl.hash
      (token.token_data
       , token.token_index
       , token.trivia_index)

  let is_equivalent one other =
    is_equivalent (addr (unwrap one)) (addr (unwrap other))

end

module UnitProvider = struct
  (* The real C type of a context is a void*. But we use a pointer to this
     type, to be able to allocate a value of t and attach a finalizer to it. *)
  type t = unit ptr ptr

  let c_type = ptr void

  let null = allocate c_type null

  

end

module BareNode = struct
  type t = unit ptr
end

module Rebindings = struct
  type t = unit ptr
end



      
module EntityInfoStruct = struct
  type t

   
  let c_type : t structure typ = structure "entity_info"
  let rebindings =
    field c_type "rebindings" (ptr void)
  let from_rebound =
    field c_type "from_rebound" bool
  let () = seal c_type

end

         
module EntityStruct = struct
  type t

   
  let c_type : t structure typ = structure "gpr_node"
  let node =
    field c_type "node" (ptr void)
  let info =
    field c_type "info" EntityInfoStruct.c_type
  let () = seal c_type

end

      
module AnalysisUnitStruct : sig
  type t = unit ptr

  val c_type : t typ

  val unit_root : t -> EntityStruct.t structure ptr -> unit

  val unit_diagnostic_count : t -> int

  val unit_diagnostic : t -> int -> Diagnostic.t ptr -> int

  val unit_filename : t -> char ptr

  val unit_reparse_from_file : t -> string -> int

  val unit_reparse_from_buffer :
    t -> string -> string -> Unsigned.size_t -> int

  val unit_first_token : t -> Token.t structure ptr -> unit

  val unit_last_token : t -> Token.t structure ptr -> unit

  val unit_token_count : t -> int

  val unit_trivia_count : t -> int
end = struct
  (* Module defining the c structure of an analysis unit *)

  type t = unit ptr
  let c_type = ptr void

  let unit_root = foreign ~from:c_lib "gpr_unit_root"
    (c_type @-> ptr EntityStruct.c_type @-> raisable void)

  let unit_diagnostic_count = foreign ~from:c_lib
    "gpr_unit_diagnostic_count"
    (c_type @-> raisable int)

  let unit_diagnostic = foreign ~from:c_lib
    "gpr_unit_diagnostic"
    (c_type @-> int @-> ptr Diagnostic.c_type @-> raisable int)

  let unit_filename = foreign ~from:c_lib
    "gpr_unit_filename"
    (c_type @-> raisable (ptr char))

  let unit_reparse_from_file = foreign ~from:c_lib
    "gpr_unit_reparse_from_file"
    (c_type
     @-> string
     @-> raisable int)

  let unit_reparse_from_buffer = foreign ~from:c_lib
    "gpr_unit_reparse_from_buffer"
    (c_type
     @-> string
     @-> string
     @-> size_t
     @-> raisable int)

  let unit_first_token = foreign ~from:c_lib
    "gpr_unit_first_token"
    (c_type
     @-> ptr Token.c_type
     @-> raisable void)

  let unit_last_token = foreign ~from:c_lib
    "gpr_unit_last_token"
    (c_type
     @-> ptr Token.c_type
     @-> raisable void)

  let unit_token_count = foreign ~from:c_lib
    "gpr_unit_token_count"
    (c_type @-> raisable int)

  let unit_trivia_count = foreign ~from:c_lib
    "gpr_unit_trivia_count"
    (c_type @-> raisable int)
end


      
module AnalysisContextStruct : sig
  type t

  val c_type : t typ

  val create_analysis_context :
    string -> unit ptr -> unit ptr -> unit ptr -> bool -> int -> t

  val get_analysis_unit_from_file :
    t -> string -> string -> bool -> GrammarRule.t -> AnalysisUnitStruct.t

  val get_analysis_unit_from_buffer :
    t
    -> string (* Filename *)
    -> string (* Charset *)
    -> string (* Buffer *)
    -> Unsigned.size_t (* Buffer size *)
    -> GrammarRule.t
    -> AnalysisUnitStruct.t
end = struct
  (* Module defining the c structure of an analysis context *)

  (* The real C type of a context is a void*. But we use a pointer to this
     type, to be able to allocate a value of t and attach a finalizer to it.
     See wrap function *)
  type t = unit ptr ptr

  let unwrap (value : t) : unit ptr = !@value

  let context_decref =
    let f =
      foreign ~from:c_lib "gpr_context_decref"
        (ptr void @-> raisable void)
    in
    fun ctx -> f (unwrap ctx)

  let wrap (c_value : unit ptr) : t =
    (* To deallocate cleanly the context, we need to call context_decref.
       Allocate a value and attach a finalizer to it *)
    allocate ~finalise:context_decref (ptr void) c_value

  let c_type = view (ptr void) ~read:wrap ~write:unwrap

  let create_analysis_context =
    foreign ~from:c_lib "gpr_create_analysis_context"
      (string @-> ptr void @-> UnitProvider.c_type @-> ptr void @-> bool @-> int
      @-> raisable c_type)

  let get_analysis_unit_from_file =
    foreign ~from:c_lib "gpr_get_analysis_unit_from_file"
      ( c_type @-> string @-> string @-> bool @-> GrammarRule.c_type
      @-> raisable AnalysisUnitStruct.c_type )

  let get_analysis_unit_from_buffer =
    foreign ~from:c_lib "gpr_get_analysis_unit_from_buffer"
      ( c_type @-> string (* Filename *) @-> string (* Charset *)
      @-> string (* Buffer *) @-> size_t (* Buffer size *)
      @-> GrammarRule.c_type
      @-> raisable AnalysisUnitStruct.c_type )

end

      
module Symbol : sig
  type t = string

  val c_type : t structure typ

  val wrap : (t structure) -> t

  val unwrap : AnalysisContextStruct.t -> t -> (t structure)

  val symbol_text : t structure ptr -> string ptr -> unit

  val context_symbol :
    AnalysisContextStruct.t -> string ptr -> t structure ptr -> int
end = struct
  type t = string

  let c_type : t structure typ = structure "symbol"
  let data = field c_type "data" (ptr void)
  let bounds = field c_type "bounds" (ptr void)
  let () = seal c_type

  let symbol_text = foreign ~from:c_lib "gpr_symbol_text"
    (ptr c_type @-> ptr Text.c_type @-> raisable void)

  let wrap (c_value : t structure) : t =
    let c_result_ptr = allocate_n Text.c_type ~count:1 in
    symbol_text (addr c_value) c_result_ptr;
    !@ c_result_ptr

  let context_symbol = foreign ~from:c_lib "gpr_context_symbol"
    (AnalysisContextStruct.c_type
     @-> ptr Text.c_type
     @-> ptr c_type
     @-> raisable int)

  let unwrap (ctx : AnalysisContextStruct.t) (value : t) : t structure =
    let result = make c_type in
    let code =
      context_symbol ctx (allocate Text.c_type value) (addr result)
    in
    if code = 0 then
      raise (InvalidSymbolError value) ;
    result
end

         
module GprNodeArrayStruct = struct
  

  type t

  let c_struct : t structure typ = structure "gpr_node_array"
  let n = field c_struct "n" int
  let _ = field c_struct "ref_count" int
  let items = field c_struct "items"
    EntityStruct.c_type
  let () = seal c_struct

  let c_type = ptr c_struct

  let create = foreign ~from:c_lib "gpr_gpr_node_array_create"
    (int @-> raisable c_type)
  let dec_ref = foreign ~from:c_lib "gpr_gpr_node_array_dec_ref"
    (c_type @-> raisable void)

end


module CFunctions = struct
  let node_kind = foreign ~from:c_lib "gpr_node_kind"
    (ptr EntityStruct.c_type @-> raisable int)

  let image = foreign ~from:c_lib "gpr_node_image"
    (ptr EntityStruct.c_type
     @-> ptr Text.c_type
     @-> raisable void)

  let node_sloc_range = foreign ~from:c_lib
    "gpr_node_sloc_range"
    (ptr EntityStruct.c_type
     @-> ptr SlocRange.c_type
     @-> raisable void)

  let lookup_in_node = foreign ~from:c_lib
    "gpr_lookup_in_node"
    (ptr EntityStruct.c_type
     @-> ptr Sloc.c_type
     @-> ptr EntityStruct.c_type
     @-> raisable void)

  let entity_image = foreign ~from:c_lib
    "gpr_entity_image"
    (ptr EntityStruct.c_type
     @-> ptr Text.c_type
     @-> raisable void)

  let node_is_token_node = foreign ~from:c_lib
    "gpr_node_is_token_node"
    (ptr EntityStruct.c_type
     @-> raisable bool)

  let gpr_node_parent = foreign ~from:c_lib
    "gpr_gpr_node_parent"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let gpr_node_parents = foreign ~from:c_lib
    "gpr_gpr_node_parents"
    (ptr EntityStruct.c_type
        
    @-> bool
    @-> ptr GprNodeArrayStruct.c_type
    @-> raisable int)

  let gpr_node_children = foreign ~from:c_lib
    "gpr_gpr_node_children"
    (ptr EntityStruct.c_type
    @-> ptr GprNodeArrayStruct.c_type
    @-> raisable int)

  let gpr_node_token_start = foreign ~from:c_lib
    "gpr_gpr_node_token_start"
    (ptr EntityStruct.c_type
    @-> ptr Token.c_type
    @-> raisable int)

  let gpr_node_token_end = foreign ~from:c_lib
    "gpr_gpr_node_token_end"
    (ptr EntityStruct.c_type
    @-> ptr Token.c_type
    @-> raisable int)

  let gpr_node_child_index = foreign ~from:c_lib
    "gpr_gpr_node_child_index"
    (ptr EntityStruct.c_type
    @-> ptr int
    @-> raisable int)

  let gpr_node_previous_sibling = foreign ~from:c_lib
    "gpr_gpr_node_previous_sibling"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let gpr_node_next_sibling = foreign ~from:c_lib
    "gpr_gpr_node_next_sibling"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let gpr_node_unit = foreign ~from:c_lib
    "gpr_gpr_node_unit"
    (ptr EntityStruct.c_type
    @-> ptr AnalysisUnitStruct.c_type
    @-> raisable int)

  let gpr_node_is_ghost = foreign ~from:c_lib
    "gpr_gpr_node_is_ghost"
    (ptr EntityStruct.c_type
    @-> ptr bool
    @-> raisable int)

  let gpr_node_full_sloc_image = foreign ~from:c_lib
    "gpr_gpr_node_full_sloc_image"
    (ptr EntityStruct.c_type
    @-> ptr StringType.c_type
    @-> raisable int)

  let ada_access_subp_f_subp_kind = foreign ~from:c_lib
    "gpr_ada_access_subp_f_subp_kind"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let ada_access_subp_f_skips = foreign ~from:c_lib
    "gpr_ada_access_subp_f_skips"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let ada_pragma_f_skips = foreign ~from:c_lib
    "gpr_ada_pragma_f_skips"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let ada_use_f_skips = foreign ~from:c_lib
    "gpr_ada_use_f_skips"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let ada_with_f_has_limited = foreign ~from:c_lib
    "gpr_ada_with_f_has_limited"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let ada_with_f_has_private = foreign ~from:c_lib
    "gpr_ada_with_f_has_private"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let ada_with_f_packages = foreign ~from:c_lib
    "gpr_ada_with_f_packages"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let ada_generic_f_skips = foreign ~from:c_lib
    "gpr_ada_generic_f_skips"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let ada_library_item_f_generic_stub = foreign ~from:c_lib
    "gpr_ada_library_item_f_generic_stub"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let ada_library_item_f_separate = foreign ~from:c_lib
    "gpr_ada_library_item_f_separate"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let ada_library_item_f_main = foreign ~from:c_lib
    "gpr_ada_library_item_f_main"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let ada_main_f_name = foreign ~from:c_lib
    "gpr_ada_main_f_name"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let ada_pkg_f_has_private = foreign ~from:c_lib
    "gpr_ada_pkg_f_has_private"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let ada_subp_f_subp_kind = foreign ~from:c_lib
    "gpr_ada_subp_f_subp_kind"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let ada_prelude_f_context_clauses = foreign ~from:c_lib
    "gpr_ada_prelude_f_context_clauses"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let ada_prelude_f_library_item = foreign ~from:c_lib
    "gpr_ada_prelude_f_library_item"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let ada_separate_f_parent_name = foreign ~from:c_lib
    "gpr_ada_separate_f_parent_name"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let ada_with_formal_f_kind = foreign ~from:c_lib
    "gpr_ada_with_formal_f_kind"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let ada_with_formal_f_skips = foreign ~from:c_lib
    "gpr_ada_with_formal_f_skips"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let all_qualifier_p_as_bool = foreign ~from:c_lib
    "gpr_all_qualifier_p_as_bool"
    (ptr EntityStruct.c_type
    @-> ptr bool
    @-> raisable int)

  let attribute_decl_f_attr_name = foreign ~from:c_lib
    "gpr_attribute_decl_f_attr_name"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let attribute_decl_f_attr_index = foreign ~from:c_lib
    "gpr_attribute_decl_f_attr_index"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let attribute_decl_f_expr = foreign ~from:c_lib
    "gpr_attribute_decl_f_expr"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let attribute_reference_f_attribute_name = foreign ~from:c_lib
    "gpr_attribute_reference_f_attribute_name"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let attribute_reference_f_attribute_index = foreign ~from:c_lib
    "gpr_attribute_reference_f_attribute_index"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let builtin_function_call_f_function_name = foreign ~from:c_lib
    "gpr_builtin_function_call_f_function_name"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let builtin_function_call_f_parameters = foreign ~from:c_lib
    "gpr_builtin_function_call_f_parameters"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let case_construction_f_var_ref = foreign ~from:c_lib
    "gpr_case_construction_f_var_ref"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let case_construction_f_items = foreign ~from:c_lib
    "gpr_case_construction_f_items"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let case_item_f_choice = foreign ~from:c_lib
    "gpr_case_item_f_choice"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let case_item_f_decls = foreign ~from:c_lib
    "gpr_case_item_f_decls"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let compilation_unit_f_project = foreign ~from:c_lib
    "gpr_compilation_unit_f_project"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let prefix_f_prefix = foreign ~from:c_lib
    "gpr_prefix_f_prefix"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let prefix_f_suffix = foreign ~from:c_lib
    "gpr_prefix_f_suffix"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let limited_node_p_as_bool = foreign ~from:c_lib
    "gpr_limited_node_p_as_bool"
    (ptr EntityStruct.c_type
    @-> ptr bool
    @-> raisable int)

  let package_decl_f_pkg_name = foreign ~from:c_lib
    "gpr_package_decl_f_pkg_name"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let package_decl_f_pkg_spec = foreign ~from:c_lib
    "gpr_package_decl_f_pkg_spec"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let package_extension_f_extended_name = foreign ~from:c_lib
    "gpr_package_extension_f_extended_name"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let package_renaming_f_renamed_name = foreign ~from:c_lib
    "gpr_package_renaming_f_renamed_name"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let package_spec_f_extension = foreign ~from:c_lib
    "gpr_package_spec_f_extension"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let package_spec_f_decls = foreign ~from:c_lib
    "gpr_package_spec_f_decls"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let package_spec_f_end_name = foreign ~from:c_lib
    "gpr_package_spec_f_end_name"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let private_node_p_as_bool = foreign ~from:c_lib
    "gpr_private_node_p_as_bool"
    (ptr EntityStruct.c_type
    @-> ptr bool
    @-> raisable int)

  let project_f_context_clauses = foreign ~from:c_lib
    "gpr_project_f_context_clauses"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let project_f_project_decl = foreign ~from:c_lib
    "gpr_project_f_project_decl"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let project_declaration_f_qualifier = foreign ~from:c_lib
    "gpr_project_declaration_f_qualifier"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let project_declaration_f_project_name = foreign ~from:c_lib
    "gpr_project_declaration_f_project_name"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let project_declaration_f_extension = foreign ~from:c_lib
    "gpr_project_declaration_f_extension"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let project_declaration_f_decls = foreign ~from:c_lib
    "gpr_project_declaration_f_decls"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let project_declaration_f_end_name = foreign ~from:c_lib
    "gpr_project_declaration_f_end_name"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let project_extension_f_is_all = foreign ~from:c_lib
    "gpr_project_extension_f_is_all"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let project_extension_f_path_name = foreign ~from:c_lib
    "gpr_project_extension_f_path_name"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let project_reference_f_attr_ref = foreign ~from:c_lib
    "gpr_project_reference_f_attr_ref"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let string_literal_at_f_str_lit = foreign ~from:c_lib
    "gpr_string_literal_at_f_str_lit"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let string_literal_at_f_at_lit = foreign ~from:c_lib
    "gpr_string_literal_at_f_at_lit"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let terms_f_terms = foreign ~from:c_lib
    "gpr_terms_f_terms"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let type_reference_f_var_type_name = foreign ~from:c_lib
    "gpr_type_reference_f_var_type_name"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let typed_string_decl_f_type_id = foreign ~from:c_lib
    "gpr_typed_string_decl_f_type_id"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let typed_string_decl_f_string_literals = foreign ~from:c_lib
    "gpr_typed_string_decl_f_string_literals"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let variable_decl_f_var_name = foreign ~from:c_lib
    "gpr_variable_decl_f_var_name"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let variable_decl_f_var_type = foreign ~from:c_lib
    "gpr_variable_decl_f_var_type"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let variable_decl_f_expr = foreign ~from:c_lib
    "gpr_variable_decl_f_expr"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let variable_reference_f_variable_name = foreign ~from:c_lib
    "gpr_variable_reference_f_variable_name"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let variable_reference_f_attribute_ref = foreign ~from:c_lib
    "gpr_variable_reference_f_attribute_ref"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let with_decl_f_is_limited = foreign ~from:c_lib
    "gpr_with_decl_f_is_limited"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let with_decl_f_path_names = foreign ~from:c_lib
    "gpr_with_decl_f_path_names"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)


end

type analysis_context = {
  c_value : AnalysisContextStruct.t;
  unit_provider : UnitProvider.t
}

and analysis_unit = {
  c_value : AnalysisUnitStruct.t;
  context : analysis_context
}

and entity = EntityStruct.t structure


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




let rec unwrap_gpr_node value =
  (* This is the unique unwrap function that can be called for any node. *)
  match (value :> gpr_node) with
  | `AdaAccessSubp fields -> fields.c_value
  | `AdaPragma fields -> fields.c_value
  | `AdaUse fields -> fields.c_value
  | `AdaWith fields -> fields.c_value
  | `AdaEntityKindFunction fields -> fields.c_value
  | `AdaEntityKindPackage fields -> fields.c_value
  | `AdaEntityKindProcedure fields -> fields.c_value
  | `AdaGeneric fields -> fields.c_value
  | `AdaLibraryItem fields -> fields.c_value
  | `AdaPkg fields -> fields.c_value
  | `AdaPkgBody fields -> fields.c_value
  | `AdaSubp fields -> fields.c_value
  | `AdaPrelude fields -> fields.c_value
  | `AdaSeparate fields -> fields.c_value
  | `AdaSkip fields -> fields.c_value
  | `AdaWithFormal fields -> fields.c_value
  | `AllQualifierAbsent fields -> fields.c_value
  | `AllQualifierPresent fields -> fields.c_value
  | `AttributeDecl fields -> fields.c_value
  | `AttributeReference fields -> fields.c_value
  | `AdaContextClauseList fields -> fields.c_value
  | `AdaPreludeNodeList fields -> fields.c_value
  | `AdaSkipList fields -> fields.c_value
  | `CaseItemList fields -> fields.c_value
  | `ExprList fields -> fields.c_value
  | `GprNodeList fields -> fields.c_value
  | `Choices fields -> fields.c_value
  | `TermList fields -> fields.c_value
  | `IdentifierList fields -> fields.c_value
  | `StringLiteralList fields -> fields.c_value
  | `TermListList fields -> fields.c_value
  | `WithDeclList fields -> fields.c_value
  | `BuiltinFunctionCall fields -> fields.c_value
  | `CaseConstruction fields -> fields.c_value
  | `CaseItem fields -> fields.c_value
  | `CompilationUnit fields -> fields.c_value
  | `EmptyDecl fields -> fields.c_value
  | `Prefix fields -> fields.c_value
  | `Identifier fields -> fields.c_value
  | `NumLiteral fields -> fields.c_value
  | `StringLiteral fields -> fields.c_value
  | `LimitedAbsent fields -> fields.c_value
  | `LimitedPresent fields -> fields.c_value
  | `OthersDesignator fields -> fields.c_value
  | `PackageDecl fields -> fields.c_value
  | `PackageExtension fields -> fields.c_value
  | `PackageRenaming fields -> fields.c_value
  | `PackageSpec fields -> fields.c_value
  | `PrivateAbsent fields -> fields.c_value
  | `PrivatePresent fields -> fields.c_value
  | `Project fields -> fields.c_value
  | `ProjectDeclaration fields -> fields.c_value
  | `ProjectExtension fields -> fields.c_value
  | `ProjectQualifierAbstract fields -> fields.c_value
  | `ProjectQualifierAggregate fields -> fields.c_value
  | `ProjectQualifierAggregateLibrary fields -> fields.c_value
  | `ProjectQualifierConfiguration fields -> fields.c_value
  | `ProjectQualifierLibrary fields -> fields.c_value
  | `ProjectQualifierStandard fields -> fields.c_value
  | `ProjectReference fields -> fields.c_value
  | `StringLiteralAt fields -> fields.c_value
  | `Terms fields -> fields.c_value
  | `TypeReference fields -> fields.c_value
  | `TypedStringDecl fields -> fields.c_value
  | `VariableDecl fields -> fields.c_value
  | `VariableReference fields -> fields.c_value
  | `WithDecl fields -> fields.c_value


   

  and unwrap_entity_info value =
    let c_value = make EntityInfoStruct.c_type in
    setf c_value
      EntityInfoStruct.rebindings
      (value.rebindings);
    setf c_value
      EntityInfoStruct.from_rebound
      (value.from_rebound);
    c_value




   




and unwrap_analysis_unit
  (unit : analysis_unit) = unit.c_value

let rec wrap_gpr_node context c_value =
  (* Top level wrap function that dispatch to wrap function of concrete types
     depending on the node kind *)
  if is_null (getf c_value EntityStruct.node) then
    raise (SyntaxError "null node")
  else
    let kind = CFunctions.node_kind (addr c_value) in
    match kind with
    | 1 ->
        (wrap_ada_access_subp context (c_value)
         :> gpr_node)
    | 2 ->
        (wrap_ada_pragma context (c_value)
         :> gpr_node)
    | 3 ->
        (wrap_ada_use context (c_value)
         :> gpr_node)
    | 4 ->
        (wrap_ada_with context (c_value)
         :> gpr_node)
    | 5 ->
        (wrap_ada_entity_kind_function context (c_value)
         :> gpr_node)
    | 6 ->
        (wrap_ada_entity_kind_package context (c_value)
         :> gpr_node)
    | 7 ->
        (wrap_ada_entity_kind_procedure context (c_value)
         :> gpr_node)
    | 8 ->
        (wrap_ada_generic context (c_value)
         :> gpr_node)
    | 9 ->
        (wrap_ada_library_item context (c_value)
         :> gpr_node)
    | 10 ->
        (wrap_ada_pkg context (c_value)
         :> gpr_node)
    | 11 ->
        (wrap_ada_pkg_body context (c_value)
         :> gpr_node)
    | 12 ->
        (wrap_ada_subp context (c_value)
         :> gpr_node)
    | 13 ->
        (wrap_ada_prelude context (c_value)
         :> gpr_node)
    | 14 ->
        (wrap_ada_separate context (c_value)
         :> gpr_node)
    | 15 ->
        (wrap_ada_skip context (c_value)
         :> gpr_node)
    | 16 ->
        (wrap_ada_with_formal context (c_value)
         :> gpr_node)
    | 17 ->
        (wrap_all_qualifier_absent context (c_value)
         :> gpr_node)
    | 18 ->
        (wrap_all_qualifier_present context (c_value)
         :> gpr_node)
    | 19 ->
        (wrap_attribute_decl context (c_value)
         :> gpr_node)
    | 20 ->
        (wrap_attribute_reference context (c_value)
         :> gpr_node)
    | 21 ->
        (wrap_ada_context_clause_list context (c_value)
         :> gpr_node)
    | 22 ->
        (wrap_ada_prelude_node_list context (c_value)
         :> gpr_node)
    | 23 ->
        (wrap_ada_skip_list context (c_value)
         :> gpr_node)
    | 24 ->
        (wrap_case_item_list context (c_value)
         :> gpr_node)
    | 25 ->
        (wrap_expr_list context (c_value)
         :> gpr_node)
    | 26 ->
        (wrap_gpr_node_list context (c_value)
         :> gpr_node)
    | 27 ->
        (wrap_choices context (c_value)
         :> gpr_node)
    | 28 ->
        (wrap_term_list context (c_value)
         :> gpr_node)
    | 29 ->
        (wrap_identifier_list context (c_value)
         :> gpr_node)
    | 30 ->
        (wrap_string_literal_list context (c_value)
         :> gpr_node)
    | 31 ->
        (wrap_term_list_list context (c_value)
         :> gpr_node)
    | 32 ->
        (wrap_with_decl_list context (c_value)
         :> gpr_node)
    | 33 ->
        (wrap_builtin_function_call context (c_value)
         :> gpr_node)
    | 34 ->
        (wrap_case_construction context (c_value)
         :> gpr_node)
    | 35 ->
        (wrap_case_item context (c_value)
         :> gpr_node)
    | 36 ->
        (wrap_compilation_unit context (c_value)
         :> gpr_node)
    | 37 ->
        (wrap_empty_decl context (c_value)
         :> gpr_node)
    | 38 ->
        (wrap_prefix context (c_value)
         :> gpr_node)
    | 39 ->
        (wrap_identifier context (c_value)
         :> gpr_node)
    | 40 ->
        (wrap_num_literal context (c_value)
         :> gpr_node)
    | 41 ->
        (wrap_string_literal context (c_value)
         :> gpr_node)
    | 42 ->
        (wrap_limited_absent context (c_value)
         :> gpr_node)
    | 43 ->
        (wrap_limited_present context (c_value)
         :> gpr_node)
    | 44 ->
        (wrap_others_designator context (c_value)
         :> gpr_node)
    | 45 ->
        (wrap_package_decl context (c_value)
         :> gpr_node)
    | 46 ->
        (wrap_package_extension context (c_value)
         :> gpr_node)
    | 47 ->
        (wrap_package_renaming context (c_value)
         :> gpr_node)
    | 48 ->
        (wrap_package_spec context (c_value)
         :> gpr_node)
    | 49 ->
        (wrap_private_absent context (c_value)
         :> gpr_node)
    | 50 ->
        (wrap_private_present context (c_value)
         :> gpr_node)
    | 51 ->
        (wrap_project context (c_value)
         :> gpr_node)
    | 52 ->
        (wrap_project_declaration context (c_value)
         :> gpr_node)
    | 53 ->
        (wrap_project_extension context (c_value)
         :> gpr_node)
    | 54 ->
        (wrap_project_qualifier_abstract context (c_value)
         :> gpr_node)
    | 55 ->
        (wrap_project_qualifier_aggregate context (c_value)
         :> gpr_node)
    | 56 ->
        (wrap_project_qualifier_aggregate_library context (c_value)
         :> gpr_node)
    | 57 ->
        (wrap_project_qualifier_configuration context (c_value)
         :> gpr_node)
    | 58 ->
        (wrap_project_qualifier_library context (c_value)
         :> gpr_node)
    | 59 ->
        (wrap_project_qualifier_standard context (c_value)
         :> gpr_node)
    | 60 ->
        (wrap_project_reference context (c_value)
         :> gpr_node)
    | 61 ->
        (wrap_string_literal_at context (c_value)
         :> gpr_node)
    | 62 ->
        (wrap_terms context (c_value)
         :> gpr_node)
    | 63 ->
        (wrap_type_reference context (c_value)
         :> gpr_node)
    | 64 ->
        (wrap_typed_string_decl context (c_value)
         :> gpr_node)
    | 65 ->
        (wrap_variable_decl context (c_value)
         :> gpr_node)
    | 66 ->
        (wrap_variable_reference context (c_value)
         :> gpr_node)
    | 67 ->
        (wrap_with_decl context (c_value)
         :> gpr_node)
    | _ -> assert false

      

  and wrap_ada_prelude_node context c_value =
    (* This is an abstract node, call the root wrap function and filter to get
     the desired type *)
    match wrap_gpr_node context (c_value) with
      | `AdaAccessSubp _
      | `AdaPragma _
      | `AdaUse _
      | `AdaWith _
      | `AdaEntityKindFunction _
      | `AdaEntityKindPackage _
      | `AdaEntityKindProcedure _
      | `AdaGeneric _
      | `AdaLibraryItem _
      | `AdaPkg _
      | `AdaPkgBody _
      | `AdaSubp _
      | `AdaPrelude _
      | `AdaSeparate _
      | `AdaSkip _
      | `AdaWithFormal _
      as e -> e
      | _ ->
          (* This should not happen if the types are correct *)
          assert false

      


  and wrap_ada_access_subp context c_value
   : ada_access_subp =
    let f_subp_kind () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.ada_access_subp_f_subp_kind
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_ada_entity_kind context (field_c_value)
      in
         

      match node with
            
      | `AdaEntityKindFunction _
      | `AdaEntityKindProcedure _
      as e -> e
      | _ -> assert false
    in
    let f_skips () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.ada_access_subp_f_skips
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_ada_skip_list context (field_c_value)
      in
         

      node
    in
    if is_null (getf c_value EntityStruct.node) then
      raise (SyntaxError "null node")
    else
      `AdaAccessSubp {
        f_subp_kind
          = Lazy.from_fun f_subp_kind ;
        f_skips
          = Lazy.from_fun f_skips ;
        c_value = c_value;
        context = context
      }

      

  and wrap_ada_context_clause context c_value =
    (* This is an abstract node, call the root wrap function and filter to get
     the desired type *)
    match wrap_gpr_node context (c_value) with
      | `AdaPragma _
      | `AdaUse _
      | `AdaWith _
      as e -> e
      | _ ->
          (* This should not happen if the types are correct *)
          assert false

      


  and wrap_ada_pragma context c_value
   : ada_pragma =
    let f_skips () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.ada_pragma_f_skips
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_ada_skip_list context (field_c_value)
      in
         

      node
    in
    if is_null (getf c_value EntityStruct.node) then
      raise (SyntaxError "null node")
    else
      `AdaPragma {
        f_skips
          = Lazy.from_fun f_skips ;
        c_value = c_value;
        context = context
      }

      


  and wrap_ada_use context c_value
   : ada_use =
    let f_skips () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.ada_use_f_skips
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_ada_skip_list context (field_c_value)
      in
         

      node
    in
    if is_null (getf c_value EntityStruct.node) then
      raise (SyntaxError "null node")
    else
      `AdaUse {
        f_skips
          = Lazy.from_fun f_skips ;
        c_value = c_value;
        context = context
      }

      


  and wrap_ada_with context c_value
   : ada_with =
    let f_has_limited () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.ada_with_f_has_limited
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_limited_node context (field_c_value)
      in
         

      node
    in
    let f_has_private () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.ada_with_f_has_private
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_private_node context (field_c_value)
      in
         

      node
    in
    let f_packages () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.ada_with_f_packages
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_expr_list context (field_c_value)
      in
         

      node
    in
    if is_null (getf c_value EntityStruct.node) then
      raise (SyntaxError "null node")
    else
      `AdaWith {
        f_has_limited
          = Lazy.from_fun f_has_limited ;
        f_has_private
          = Lazy.from_fun f_has_private ;
        f_packages
          = Lazy.from_fun f_packages ;
        c_value = c_value;
        context = context
      }

      

  and wrap_ada_entity_kind context c_value =
    (* This is an abstract node, call the root wrap function and filter to get
     the desired type *)
    match wrap_gpr_node context (c_value) with
      | `AdaEntityKindFunction _
      | `AdaEntityKindPackage _
      | `AdaEntityKindProcedure _
      as e -> e
      | _ ->
          (* This should not happen if the types are correct *)
          assert false

      


  and wrap_ada_entity_kind_function context c_value
   : ada_entity_kind_function =
    if is_null (getf c_value EntityStruct.node) then
      raise (SyntaxError "null node")
    else
      `AdaEntityKindFunction {
        c_value = c_value;
        context = context
      }

      


  and wrap_ada_entity_kind_package context c_value
   : ada_entity_kind_package =
    if is_null (getf c_value EntityStruct.node) then
      raise (SyntaxError "null node")
    else
      `AdaEntityKindPackage {
        c_value = c_value;
        context = context
      }

      


  and wrap_ada_entity_kind_procedure context c_value
   : ada_entity_kind_procedure =
    if is_null (getf c_value EntityStruct.node) then
      raise (SyntaxError "null node")
    else
      `AdaEntityKindProcedure {
        c_value = c_value;
        context = context
      }

      


  and wrap_ada_generic context c_value
   : ada_generic =
    let f_skips () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.ada_generic_f_skips
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_gpr_node context (field_c_value)
      in
         

      node
    in
    if is_null (getf c_value EntityStruct.node) then
      raise (SyntaxError "null node")
    else
      `AdaGeneric {
        f_skips
          = Lazy.from_fun f_skips ;
        c_value = c_value;
        context = context
      }

      


  and wrap_ada_library_item context c_value
   : ada_library_item =
    let f_generic_stub () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.ada_library_item_f_generic_stub
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         if is_null (getf field_c_value EntityStruct.node) then None else Some (wrap_ada_generic context (field_c_value))
      in
         

      node
    in
    let f_separate () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.ada_library_item_f_separate
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         if is_null (getf field_c_value EntityStruct.node) then None else Some (wrap_ada_separate context (field_c_value))
      in
         

      node
    in
    let f_main () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.ada_library_item_f_main
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_ada_main context (field_c_value)
      in
         

      node
    in
    if is_null (getf c_value EntityStruct.node) then
      raise (SyntaxError "null node")
    else
      `AdaLibraryItem {
        f_generic_stub
          = Lazy.from_fun f_generic_stub ;
        f_separate
          = Lazy.from_fun f_separate ;
        f_main
          = Lazy.from_fun f_main ;
        c_value = c_value;
        context = context
      }

      

  and wrap_ada_main context c_value =
    (* This is an abstract node, call the root wrap function and filter to get
     the desired type *)
    match wrap_gpr_node context (c_value) with
      | `AdaPkg _
      | `AdaPkgBody _
      | `AdaSubp _
      as e -> e
      | _ ->
          (* This should not happen if the types are correct *)
          assert false

      


  and wrap_ada_pkg context c_value
   : ada_pkg =
    let f_has_private () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.ada_pkg_f_has_private
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_private_node context (field_c_value)
      in
         

      node
    in
    if is_null (getf c_value EntityStruct.node) then
      raise (SyntaxError "null node")
    else
      `AdaPkg {
        f_has_private
          = Lazy.from_fun f_has_private ;
        c_value = c_value;
        context = context
      }

      


  and wrap_ada_pkg_body context c_value
   : ada_pkg_body =
    if is_null (getf c_value EntityStruct.node) then
      raise (SyntaxError "null node")
    else
      `AdaPkgBody {
        c_value = c_value;
        context = context
      }

      


  and wrap_ada_subp context c_value
   : ada_subp =
    let f_subp_kind () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.ada_subp_f_subp_kind
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_ada_entity_kind context (field_c_value)
      in
         

      match node with
            
      | `AdaEntityKindFunction _
      | `AdaEntityKindProcedure _
      as e -> e
      | _ -> assert false
    in
    if is_null (getf c_value EntityStruct.node) then
      raise (SyntaxError "null node")
    else
      `AdaSubp {
        f_subp_kind
          = Lazy.from_fun f_subp_kind ;
        c_value = c_value;
        context = context
      }

      


  and wrap_ada_prelude context c_value
   : ada_prelude =
    let f_context_clauses () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.ada_prelude_f_context_clauses
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_ada_context_clause_list context (field_c_value)
      in
         

      node
    in
    let f_library_item () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.ada_prelude_f_library_item
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_ada_library_item context (field_c_value)
      in
         

      node
    in
    if is_null (getf c_value EntityStruct.node) then
      raise (SyntaxError "null node")
    else
      `AdaPrelude {
        f_context_clauses
          = Lazy.from_fun f_context_clauses ;
        f_library_item
          = Lazy.from_fun f_library_item ;
        c_value = c_value;
        context = context
      }

      


  and wrap_ada_separate context c_value
   : ada_separate =
    let f_parent_name () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.ada_separate_f_parent_name
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_expr context (field_c_value)
      in
         

      match node with
            
      | `Identifier _
      | `Prefix _
      as e -> e
      | _ -> assert false
    in
    if is_null (getf c_value EntityStruct.node) then
      raise (SyntaxError "null node")
    else
      `AdaSeparate {
        f_parent_name
          = Lazy.from_fun f_parent_name ;
        c_value = c_value;
        context = context
      }

      


  and wrap_ada_skip context c_value
   : ada_skip =
    if is_null (getf c_value EntityStruct.node) then
      raise (SyntaxError "null node")
    else
      `AdaSkip {
        c_value = c_value;
        context = context
      }

      


  and wrap_ada_with_formal context c_value
   : ada_with_formal =
    let f_kind () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.ada_with_formal_f_kind
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_ada_entity_kind context (field_c_value)
      in
         

      node
    in
    let f_skips () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.ada_with_formal_f_skips
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_ada_skip_list context (field_c_value)
      in
         

      node
    in
    if is_null (getf c_value EntityStruct.node) then
      raise (SyntaxError "null node")
    else
      `AdaWithFormal {
        f_kind
          = Lazy.from_fun f_kind ;
        f_skips
          = Lazy.from_fun f_skips ;
        c_value = c_value;
        context = context
      }

      

  and wrap_all_qualifier context c_value =
    (* This is an abstract node, call the root wrap function and filter to get
     the desired type *)
    match wrap_gpr_node context (c_value) with
      | `AllQualifierAbsent _
      | `AllQualifierPresent _
      as e -> e
      | _ ->
          (* This should not happen if the types are correct *)
          assert false

      


  and wrap_all_qualifier_absent context c_value
   : all_qualifier_absent =
    if is_null (getf c_value EntityStruct.node) then
      raise (SyntaxError "null node")
    else
      `AllQualifierAbsent {
        c_value = c_value;
        context = context
      }

      


  and wrap_all_qualifier_present context c_value
   : all_qualifier_present =
    if is_null (getf c_value EntityStruct.node) then
      raise (SyntaxError "null node")
    else
      `AllQualifierPresent {
        c_value = c_value;
        context = context
      }

      


  and wrap_attribute_decl context c_value
   : attribute_decl =
    let f_attr_name () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.attribute_decl_f_attr_name
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_identifier context (field_c_value)
      in
         

      node
    in
    let f_attr_index () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.attribute_decl_f_attr_index
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         if is_null (getf field_c_value EntityStruct.node) then None else Some (wrap_gpr_node context (field_c_value))
      in
         

      match node with
            
      | Some `OthersDesignator _
      | Some `StringLiteralAt _
      | None as e -> e
      | _ -> assert false
    in
    let f_expr () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.attribute_decl_f_expr
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_term_list context (field_c_value)
      in
         

      node
    in
    if is_null (getf c_value EntityStruct.node) then
      raise (SyntaxError "null node")
    else
      `AttributeDecl {
        f_attr_name
          = Lazy.from_fun f_attr_name ;
        f_attr_index
          = Lazy.from_fun f_attr_index ;
        f_expr
          = Lazy.from_fun f_expr ;
        c_value = c_value;
        context = context
      }

      


  and wrap_attribute_reference context c_value
   : attribute_reference =
    let f_attribute_name () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.attribute_reference_f_attribute_name
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_identifier context (field_c_value)
      in
         

      node
    in
    let f_attribute_index () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.attribute_reference_f_attribute_index
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         if is_null (getf field_c_value EntityStruct.node) then None else Some (wrap_gpr_node context (field_c_value))
      in
         

      match node with
            
      | Some `OthersDesignator _
      | Some `StringLiteral _
      | None as e -> e
      | _ -> assert false
    in
    if is_null (getf c_value EntityStruct.node) then
      raise (SyntaxError "null node")
    else
      `AttributeReference {
        f_attribute_name
          = Lazy.from_fun f_attribute_name ;
        f_attribute_index
          = Lazy.from_fun f_attribute_index ;
        c_value = c_value;
        context = context
      }

      

  and wrap_base_list context c_value =
    (* This is an abstract node, call the root wrap function and filter to get
     the desired type *)
    match wrap_gpr_node context (c_value) with
      | `AdaContextClauseList _
      | `AdaPreludeNodeList _
      | `AdaSkipList _
      | `CaseItemList _
      | `ExprList _
      | `GprNodeList _
      | `Choices _
      | `TermList _
      | `IdentifierList _
      | `StringLiteralList _
      | `TermListList _
      | `WithDeclList _
      as e -> e
      | _ ->
          (* This should not happen if the types are correct *)
          assert false

      


  and wrap_ada_context_clause_list context c_value
   : ada_context_clause_list =
    let list () =
      let c_value_ptr =
        allocate_n GprNodeArrayStruct.c_type ~count:1
      in
      let _ : int =
        CFunctions.gpr_node_children
          (addr c_value)
          (c_value_ptr)
      in
      let c_value = !@(!@(c_value_ptr)) in
      let length = getf c_value GprNodeArrayStruct.n in
      let items = c_value @. GprNodeArrayStruct.items in
      let f i =
        let fresh = allocate EntityStruct.c_type !@(items +@ i) in
        (* This can raise a SyntaxError, which is expected *)
        wrap_ada_context_clause context ((!@ fresh))
      in
      let result = List.init length f in
      GprNodeArrayStruct.dec_ref (!@ c_value_ptr);
      result
    in
    if is_null (getf c_value EntityStruct.node) then
      raise (SyntaxError "null node")
    else
      `AdaContextClauseList {
        list = Lazy.from_fun list;
        c_value = c_value;
        context = context
      }

      


  and wrap_ada_prelude_node_list context c_value
   : ada_prelude_node_list =
    let list () =
      let c_value_ptr =
        allocate_n GprNodeArrayStruct.c_type ~count:1
      in
      let _ : int =
        CFunctions.gpr_node_children
          (addr c_value)
          (c_value_ptr)
      in
      let c_value = !@(!@(c_value_ptr)) in
      let length = getf c_value GprNodeArrayStruct.n in
      let items = c_value @. GprNodeArrayStruct.items in
      let f i =
        let fresh = allocate EntityStruct.c_type !@(items +@ i) in
        (* This can raise a SyntaxError, which is expected *)
        wrap_ada_prelude_node context ((!@ fresh))
      in
      let result = List.init length f in
      GprNodeArrayStruct.dec_ref (!@ c_value_ptr);
      result
    in
    if is_null (getf c_value EntityStruct.node) then
      raise (SyntaxError "null node")
    else
      `AdaPreludeNodeList {
        list = Lazy.from_fun list;
        c_value = c_value;
        context = context
      }

      


  and wrap_ada_skip_list context c_value
   : ada_skip_list =
    let list () =
      let c_value_ptr =
        allocate_n GprNodeArrayStruct.c_type ~count:1
      in
      let _ : int =
        CFunctions.gpr_node_children
          (addr c_value)
          (c_value_ptr)
      in
      let c_value = !@(!@(c_value_ptr)) in
      let length = getf c_value GprNodeArrayStruct.n in
      let items = c_value @. GprNodeArrayStruct.items in
      let f i =
        let fresh = allocate EntityStruct.c_type !@(items +@ i) in
        (* This can raise a SyntaxError, which is expected *)
        wrap_ada_skip context ((!@ fresh))
      in
      let result = List.init length f in
      GprNodeArrayStruct.dec_ref (!@ c_value_ptr);
      result
    in
    if is_null (getf c_value EntityStruct.node) then
      raise (SyntaxError "null node")
    else
      `AdaSkipList {
        list = Lazy.from_fun list;
        c_value = c_value;
        context = context
      }

      


  and wrap_case_item_list context c_value
   : case_item_list =
    let list () =
      let c_value_ptr =
        allocate_n GprNodeArrayStruct.c_type ~count:1
      in
      let _ : int =
        CFunctions.gpr_node_children
          (addr c_value)
          (c_value_ptr)
      in
      let c_value = !@(!@(c_value_ptr)) in
      let length = getf c_value GprNodeArrayStruct.n in
      let items = c_value @. GprNodeArrayStruct.items in
      let f i =
        let fresh = allocate EntityStruct.c_type !@(items +@ i) in
        (* This can raise a SyntaxError, which is expected *)
        wrap_case_item context ((!@ fresh))
      in
      let result = List.init length f in
      GprNodeArrayStruct.dec_ref (!@ c_value_ptr);
      result
    in
    if is_null (getf c_value EntityStruct.node) then
      raise (SyntaxError "null node")
    else
      `CaseItemList {
        list = Lazy.from_fun list;
        c_value = c_value;
        context = context
      }

      


  and wrap_expr_list context c_value
   : expr_list =
    let list () =
      let c_value_ptr =
        allocate_n GprNodeArrayStruct.c_type ~count:1
      in
      let _ : int =
        CFunctions.gpr_node_children
          (addr c_value)
          (c_value_ptr)
      in
      let c_value = !@(!@(c_value_ptr)) in
      let length = getf c_value GprNodeArrayStruct.n in
      let items = c_value @. GprNodeArrayStruct.items in
      let f i =
        let fresh = allocate EntityStruct.c_type !@(items +@ i) in
        (* This can raise a SyntaxError, which is expected *)
        wrap_expr context ((!@ fresh))
      in
      let result = List.init length f in
      GprNodeArrayStruct.dec_ref (!@ c_value_ptr);
      result
    in
    if is_null (getf c_value EntityStruct.node) then
      raise (SyntaxError "null node")
    else
      `ExprList {
        list = Lazy.from_fun list;
        c_value = c_value;
        context = context
      }

      


  and wrap_gpr_node_list context c_value
   : gpr_node_list =
    let list () =
      let c_value_ptr =
        allocate_n GprNodeArrayStruct.c_type ~count:1
      in
      let _ : int =
        CFunctions.gpr_node_children
          (addr c_value)
          (c_value_ptr)
      in
      let c_value = !@(!@(c_value_ptr)) in
      let length = getf c_value GprNodeArrayStruct.n in
      let items = c_value @. GprNodeArrayStruct.items in
      let f i =
        let fresh = allocate EntityStruct.c_type !@(items +@ i) in
        (* This can raise a SyntaxError, which is expected *)
        wrap_gpr_node context ((!@ fresh))
      in
      let result = List.init length f in
      GprNodeArrayStruct.dec_ref (!@ c_value_ptr);
      result
    in
    if is_null (getf c_value EntityStruct.node) then
      raise (SyntaxError "null node")
    else
      `GprNodeList {
        list = Lazy.from_fun list;
        c_value = c_value;
        context = context
      }

      


  and wrap_choices context c_value
   : choices =
    let list () =
      let c_value_ptr =
        allocate_n GprNodeArrayStruct.c_type ~count:1
      in
      let _ : int =
        CFunctions.gpr_node_children
          (addr c_value)
          (c_value_ptr)
      in
      let c_value = !@(!@(c_value_ptr)) in
      let length = getf c_value GprNodeArrayStruct.n in
      let items = c_value @. GprNodeArrayStruct.items in
      let f i =
        let fresh = allocate EntityStruct.c_type !@(items +@ i) in
        (* This can raise a SyntaxError, which is expected *)
        wrap_gpr_node context ((!@ fresh))
      in
      let result = List.init length f in
      GprNodeArrayStruct.dec_ref (!@ c_value_ptr);
      result
    in
    if is_null (getf c_value EntityStruct.node) then
      raise (SyntaxError "null node")
    else
      `Choices {
        list = Lazy.from_fun list;
        c_value = c_value;
        context = context
      }

      


  and wrap_term_list context c_value
   : term_list =
    let list () =
      let c_value_ptr =
        allocate_n GprNodeArrayStruct.c_type ~count:1
      in
      let _ : int =
        CFunctions.gpr_node_children
          (addr c_value)
          (c_value_ptr)
      in
      let c_value = !@(!@(c_value_ptr)) in
      let length = getf c_value GprNodeArrayStruct.n in
      let items = c_value @. GprNodeArrayStruct.items in
      let f i =
        let fresh = allocate EntityStruct.c_type !@(items +@ i) in
        (* This can raise a SyntaxError, which is expected *)
        wrap_gpr_node context ((!@ fresh))
      in
      let result = List.init length f in
      GprNodeArrayStruct.dec_ref (!@ c_value_ptr);
      result
    in
    if is_null (getf c_value EntityStruct.node) then
      raise (SyntaxError "null node")
    else
      `TermList {
        list = Lazy.from_fun list;
        c_value = c_value;
        context = context
      }

      


  and wrap_identifier_list context c_value
   : identifier_list =
    let list () =
      let c_value_ptr =
        allocate_n GprNodeArrayStruct.c_type ~count:1
      in
      let _ : int =
        CFunctions.gpr_node_children
          (addr c_value)
          (c_value_ptr)
      in
      let c_value = !@(!@(c_value_ptr)) in
      let length = getf c_value GprNodeArrayStruct.n in
      let items = c_value @. GprNodeArrayStruct.items in
      let f i =
        let fresh = allocate EntityStruct.c_type !@(items +@ i) in
        (* This can raise a SyntaxError, which is expected *)
        wrap_identifier context ((!@ fresh))
      in
      let result = List.init length f in
      GprNodeArrayStruct.dec_ref (!@ c_value_ptr);
      result
    in
    if is_null (getf c_value EntityStruct.node) then
      raise (SyntaxError "null node")
    else
      `IdentifierList {
        list = Lazy.from_fun list;
        c_value = c_value;
        context = context
      }

      


  and wrap_string_literal_list context c_value
   : string_literal_list =
    let list () =
      let c_value_ptr =
        allocate_n GprNodeArrayStruct.c_type ~count:1
      in
      let _ : int =
        CFunctions.gpr_node_children
          (addr c_value)
          (c_value_ptr)
      in
      let c_value = !@(!@(c_value_ptr)) in
      let length = getf c_value GprNodeArrayStruct.n in
      let items = c_value @. GprNodeArrayStruct.items in
      let f i =
        let fresh = allocate EntityStruct.c_type !@(items +@ i) in
        (* This can raise a SyntaxError, which is expected *)
        wrap_string_literal context ((!@ fresh))
      in
      let result = List.init length f in
      GprNodeArrayStruct.dec_ref (!@ c_value_ptr);
      result
    in
    if is_null (getf c_value EntityStruct.node) then
      raise (SyntaxError "null node")
    else
      `StringLiteralList {
        list = Lazy.from_fun list;
        c_value = c_value;
        context = context
      }

      


  and wrap_term_list_list context c_value
   : term_list_list =
    let list () =
      let c_value_ptr =
        allocate_n GprNodeArrayStruct.c_type ~count:1
      in
      let _ : int =
        CFunctions.gpr_node_children
          (addr c_value)
          (c_value_ptr)
      in
      let c_value = !@(!@(c_value_ptr)) in
      let length = getf c_value GprNodeArrayStruct.n in
      let items = c_value @. GprNodeArrayStruct.items in
      let f i =
        let fresh = allocate EntityStruct.c_type !@(items +@ i) in
        (* This can raise a SyntaxError, which is expected *)
        wrap_term_list context ((!@ fresh))
      in
      let result = List.init length f in
      GprNodeArrayStruct.dec_ref (!@ c_value_ptr);
      result
    in
    if is_null (getf c_value EntityStruct.node) then
      raise (SyntaxError "null node")
    else
      `TermListList {
        list = Lazy.from_fun list;
        c_value = c_value;
        context = context
      }

      


  and wrap_with_decl_list context c_value
   : with_decl_list =
    let list () =
      let c_value_ptr =
        allocate_n GprNodeArrayStruct.c_type ~count:1
      in
      let _ : int =
        CFunctions.gpr_node_children
          (addr c_value)
          (c_value_ptr)
      in
      let c_value = !@(!@(c_value_ptr)) in
      let length = getf c_value GprNodeArrayStruct.n in
      let items = c_value @. GprNodeArrayStruct.items in
      let f i =
        let fresh = allocate EntityStruct.c_type !@(items +@ i) in
        (* This can raise a SyntaxError, which is expected *)
        wrap_with_decl context ((!@ fresh))
      in
      let result = List.init length f in
      GprNodeArrayStruct.dec_ref (!@ c_value_ptr);
      result
    in
    if is_null (getf c_value EntityStruct.node) then
      raise (SyntaxError "null node")
    else
      `WithDeclList {
        list = Lazy.from_fun list;
        c_value = c_value;
        context = context
      }

      


  and wrap_builtin_function_call context c_value
   : builtin_function_call =
    let f_function_name () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.builtin_function_call_f_function_name
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_identifier context (field_c_value)
      in
         

      node
    in
    let f_parameters () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.builtin_function_call_f_parameters
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_terms context (field_c_value)
      in
         

      node
    in
    if is_null (getf c_value EntityStruct.node) then
      raise (SyntaxError "null node")
    else
      `BuiltinFunctionCall {
        f_function_name
          = Lazy.from_fun f_function_name ;
        f_parameters
          = Lazy.from_fun f_parameters ;
        c_value = c_value;
        context = context
      }

      


  and wrap_case_construction context c_value
   : case_construction =
    let f_var_ref () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.case_construction_f_var_ref
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_variable_reference context (field_c_value)
      in
         

      node
    in
    let f_items () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.case_construction_f_items
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_case_item_list context (field_c_value)
      in
         

      node
    in
    if is_null (getf c_value EntityStruct.node) then
      raise (SyntaxError "null node")
    else
      `CaseConstruction {
        f_var_ref
          = Lazy.from_fun f_var_ref ;
        f_items
          = Lazy.from_fun f_items ;
        c_value = c_value;
        context = context
      }

      


  and wrap_case_item context c_value
   : case_item =
    let f_choice () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.case_item_f_choice
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_choices context (field_c_value)
      in
         

      node
    in
    let f_decls () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.case_item_f_decls
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_gpr_node_list context (field_c_value)
      in
         

      node
    in
    if is_null (getf c_value EntityStruct.node) then
      raise (SyntaxError "null node")
    else
      `CaseItem {
        f_choice
          = Lazy.from_fun f_choice ;
        f_decls
          = Lazy.from_fun f_decls ;
        c_value = c_value;
        context = context
      }

      


  and wrap_compilation_unit context c_value
   : compilation_unit =
    let f_project () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.compilation_unit_f_project
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_project context (field_c_value)
      in
         

      node
    in
    if is_null (getf c_value EntityStruct.node) then
      raise (SyntaxError "null node")
    else
      `CompilationUnit {
        f_project
          = Lazy.from_fun f_project ;
        c_value = c_value;
        context = context
      }

      


  and wrap_empty_decl context c_value
   : empty_decl =
    if is_null (getf c_value EntityStruct.node) then
      raise (SyntaxError "null node")
    else
      `EmptyDecl {
        c_value = c_value;
        context = context
      }

      

  and wrap_expr context c_value =
    (* This is an abstract node, call the root wrap function and filter to get
     the desired type *)
    match wrap_gpr_node context (c_value) with
      | `Prefix _
      | `Identifier _
      | `NumLiteral _
      | `StringLiteral _
      as e -> e
      | _ ->
          (* This should not happen if the types are correct *)
          assert false

      


  and wrap_prefix context c_value
   : prefix =
    let f_prefix () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.prefix_f_prefix
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_expr context (field_c_value)
      in
         

      match node with
            
      | `Identifier _
      | `Prefix _
      as e -> e
      | _ -> assert false
    in
    let f_suffix () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.prefix_f_suffix
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_identifier context (field_c_value)
      in
         

      node
    in
    if is_null (getf c_value EntityStruct.node) then
      raise (SyntaxError "null node")
    else
      `Prefix {
        f_prefix
          = Lazy.from_fun f_prefix ;
        f_suffix
          = Lazy.from_fun f_suffix ;
        c_value = c_value;
        context = context
      }

      

  and wrap_single_tok_node context c_value =
    (* This is an abstract node, call the root wrap function and filter to get
     the desired type *)
    match wrap_gpr_node context (c_value) with
      | `Identifier _
      | `NumLiteral _
      | `StringLiteral _
      as e -> e
      | _ ->
          (* This should not happen if the types are correct *)
          assert false

      


  and wrap_identifier context c_value
   : identifier =
    if is_null (getf c_value EntityStruct.node) then
      raise (SyntaxError "null node")
    else
      `Identifier {
        c_value = c_value;
        context = context
      }

      


  and wrap_num_literal context c_value
   : num_literal =
    if is_null (getf c_value EntityStruct.node) then
      raise (SyntaxError "null node")
    else
      `NumLiteral {
        c_value = c_value;
        context = context
      }

      


  and wrap_string_literal context c_value
   : string_literal =
    if is_null (getf c_value EntityStruct.node) then
      raise (SyntaxError "null node")
    else
      `StringLiteral {
        c_value = c_value;
        context = context
      }

      

  and wrap_limited_node context c_value =
    (* This is an abstract node, call the root wrap function and filter to get
     the desired type *)
    match wrap_gpr_node context (c_value) with
      | `LimitedAbsent _
      | `LimitedPresent _
      as e -> e
      | _ ->
          (* This should not happen if the types are correct *)
          assert false

      


  and wrap_limited_absent context c_value
   : limited_absent =
    if is_null (getf c_value EntityStruct.node) then
      raise (SyntaxError "null node")
    else
      `LimitedAbsent {
        c_value = c_value;
        context = context
      }

      


  and wrap_limited_present context c_value
   : limited_present =
    if is_null (getf c_value EntityStruct.node) then
      raise (SyntaxError "null node")
    else
      `LimitedPresent {
        c_value = c_value;
        context = context
      }

      


  and wrap_others_designator context c_value
   : others_designator =
    if is_null (getf c_value EntityStruct.node) then
      raise (SyntaxError "null node")
    else
      `OthersDesignator {
        c_value = c_value;
        context = context
      }

      


  and wrap_package_decl context c_value
   : package_decl =
    let f_pkg_name () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.package_decl_f_pkg_name
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_identifier context (field_c_value)
      in
         

      node
    in
    let f_pkg_spec () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.package_decl_f_pkg_spec
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_gpr_node context (field_c_value)
      in
         

      match node with
            
      | `PackageRenaming _
      | `PackageSpec _
      as e -> e
      | _ -> assert false
    in
    if is_null (getf c_value EntityStruct.node) then
      raise (SyntaxError "null node")
    else
      `PackageDecl {
        f_pkg_name
          = Lazy.from_fun f_pkg_name ;
        f_pkg_spec
          = Lazy.from_fun f_pkg_spec ;
        c_value = c_value;
        context = context
      }

      


  and wrap_package_extension context c_value
   : package_extension =
    let f_extended_name () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.package_extension_f_extended_name
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_identifier_list context (field_c_value)
      in
         

      node
    in
    if is_null (getf c_value EntityStruct.node) then
      raise (SyntaxError "null node")
    else
      `PackageExtension {
        f_extended_name
          = Lazy.from_fun f_extended_name ;
        c_value = c_value;
        context = context
      }

      


  and wrap_package_renaming context c_value
   : package_renaming =
    let f_renamed_name () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.package_renaming_f_renamed_name
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_identifier_list context (field_c_value)
      in
         

      node
    in
    if is_null (getf c_value EntityStruct.node) then
      raise (SyntaxError "null node")
    else
      `PackageRenaming {
        f_renamed_name
          = Lazy.from_fun f_renamed_name ;
        c_value = c_value;
        context = context
      }

      


  and wrap_package_spec context c_value
   : package_spec =
    let f_extension () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.package_spec_f_extension
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         if is_null (getf field_c_value EntityStruct.node) then None else Some (wrap_package_extension context (field_c_value))
      in
         

      node
    in
    let f_decls () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.package_spec_f_decls
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_gpr_node_list context (field_c_value)
      in
         

      node
    in
    let f_end_name () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.package_spec_f_end_name
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_identifier context (field_c_value)
      in
         

      node
    in
    if is_null (getf c_value EntityStruct.node) then
      raise (SyntaxError "null node")
    else
      `PackageSpec {
        f_extension
          = Lazy.from_fun f_extension ;
        f_decls
          = Lazy.from_fun f_decls ;
        f_end_name
          = Lazy.from_fun f_end_name ;
        c_value = c_value;
        context = context
      }

      

  and wrap_private_node context c_value =
    (* This is an abstract node, call the root wrap function and filter to get
     the desired type *)
    match wrap_gpr_node context (c_value) with
      | `PrivateAbsent _
      | `PrivatePresent _
      as e -> e
      | _ ->
          (* This should not happen if the types are correct *)
          assert false

      


  and wrap_private_absent context c_value
   : private_absent =
    if is_null (getf c_value EntityStruct.node) then
      raise (SyntaxError "null node")
    else
      `PrivateAbsent {
        c_value = c_value;
        context = context
      }

      


  and wrap_private_present context c_value
   : private_present =
    if is_null (getf c_value EntityStruct.node) then
      raise (SyntaxError "null node")
    else
      `PrivatePresent {
        c_value = c_value;
        context = context
      }

      


  and wrap_project context c_value
   : project =
    let f_context_clauses () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.project_f_context_clauses
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_with_decl_list context (field_c_value)
      in
         

      node
    in
    let f_project_decl () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.project_f_project_decl
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_project_declaration context (field_c_value)
      in
         

      node
    in
    if is_null (getf c_value EntityStruct.node) then
      raise (SyntaxError "null node")
    else
      `Project {
        f_context_clauses
          = Lazy.from_fun f_context_clauses ;
        f_project_decl
          = Lazy.from_fun f_project_decl ;
        c_value = c_value;
        context = context
      }

      


  and wrap_project_declaration context c_value
   : project_declaration =
    let f_qualifier () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.project_declaration_f_qualifier
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         if is_null (getf field_c_value EntityStruct.node) then None else Some (wrap_project_qualifier context (field_c_value))
      in
         

      node
    in
    let f_project_name () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.project_declaration_f_project_name
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_expr context (field_c_value)
      in
         

      match node with
            
      | `Identifier _
      | `Prefix _
      as e -> e
      | _ -> assert false
    in
    let f_extension () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.project_declaration_f_extension
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         if is_null (getf field_c_value EntityStruct.node) then None else Some (wrap_project_extension context (field_c_value))
      in
         

      node
    in
    let f_decls () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.project_declaration_f_decls
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_gpr_node_list context (field_c_value)
      in
         

      node
    in
    let f_end_name () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.project_declaration_f_end_name
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_expr context (field_c_value)
      in
         

      match node with
            
      | `Identifier _
      | `Prefix _
      as e -> e
      | _ -> assert false
    in
    if is_null (getf c_value EntityStruct.node) then
      raise (SyntaxError "null node")
    else
      `ProjectDeclaration {
        f_qualifier
          = Lazy.from_fun f_qualifier ;
        f_project_name
          = Lazy.from_fun f_project_name ;
        f_extension
          = Lazy.from_fun f_extension ;
        f_decls
          = Lazy.from_fun f_decls ;
        f_end_name
          = Lazy.from_fun f_end_name ;
        c_value = c_value;
        context = context
      }

      


  and wrap_project_extension context c_value
   : project_extension =
    let f_is_all () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.project_extension_f_is_all
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_all_qualifier context (field_c_value)
      in
         

      node
    in
    let f_path_name () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.project_extension_f_path_name
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_string_literal context (field_c_value)
      in
         

      node
    in
    if is_null (getf c_value EntityStruct.node) then
      raise (SyntaxError "null node")
    else
      `ProjectExtension {
        f_is_all
          = Lazy.from_fun f_is_all ;
        f_path_name
          = Lazy.from_fun f_path_name ;
        c_value = c_value;
        context = context
      }

      

  and wrap_project_qualifier context c_value =
    (* This is an abstract node, call the root wrap function and filter to get
     the desired type *)
    match wrap_gpr_node context (c_value) with
      | `ProjectQualifierAbstract _
      | `ProjectQualifierAggregate _
      | `ProjectQualifierAggregateLibrary _
      | `ProjectQualifierConfiguration _
      | `ProjectQualifierLibrary _
      | `ProjectQualifierStandard _
      as e -> e
      | _ ->
          (* This should not happen if the types are correct *)
          assert false

      


  and wrap_project_qualifier_abstract context c_value
   : project_qualifier_abstract =
    if is_null (getf c_value EntityStruct.node) then
      raise (SyntaxError "null node")
    else
      `ProjectQualifierAbstract {
        c_value = c_value;
        context = context
      }

      


  and wrap_project_qualifier_aggregate context c_value
   : project_qualifier_aggregate =
    if is_null (getf c_value EntityStruct.node) then
      raise (SyntaxError "null node")
    else
      `ProjectQualifierAggregate {
        c_value = c_value;
        context = context
      }

      


  and wrap_project_qualifier_aggregate_library context c_value
   : project_qualifier_aggregate_library =
    if is_null (getf c_value EntityStruct.node) then
      raise (SyntaxError "null node")
    else
      `ProjectQualifierAggregateLibrary {
        c_value = c_value;
        context = context
      }

      


  and wrap_project_qualifier_configuration context c_value
   : project_qualifier_configuration =
    if is_null (getf c_value EntityStruct.node) then
      raise (SyntaxError "null node")
    else
      `ProjectQualifierConfiguration {
        c_value = c_value;
        context = context
      }

      


  and wrap_project_qualifier_library context c_value
   : project_qualifier_library =
    if is_null (getf c_value EntityStruct.node) then
      raise (SyntaxError "null node")
    else
      `ProjectQualifierLibrary {
        c_value = c_value;
        context = context
      }

      


  and wrap_project_qualifier_standard context c_value
   : project_qualifier_standard =
    if is_null (getf c_value EntityStruct.node) then
      raise (SyntaxError "null node")
    else
      `ProjectQualifierStandard {
        c_value = c_value;
        context = context
      }

      


  and wrap_project_reference context c_value
   : project_reference =
    let f_attr_ref () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.project_reference_f_attr_ref
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_attribute_reference context (field_c_value)
      in
         

      node
    in
    if is_null (getf c_value EntityStruct.node) then
      raise (SyntaxError "null node")
    else
      `ProjectReference {
        f_attr_ref
          = Lazy.from_fun f_attr_ref ;
        c_value = c_value;
        context = context
      }

      


  and wrap_string_literal_at context c_value
   : string_literal_at =
    let f_str_lit () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.string_literal_at_f_str_lit
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_string_literal context (field_c_value)
      in
         

      node
    in
    let f_at_lit () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.string_literal_at_f_at_lit
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         if is_null (getf field_c_value EntityStruct.node) then None else Some (wrap_num_literal context (field_c_value))
      in
         

      node
    in
    if is_null (getf c_value EntityStruct.node) then
      raise (SyntaxError "null node")
    else
      `StringLiteralAt {
        f_str_lit
          = Lazy.from_fun f_str_lit ;
        f_at_lit
          = Lazy.from_fun f_at_lit ;
        c_value = c_value;
        context = context
      }

      


  and wrap_terms context c_value
   : terms =
    let f_terms () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.terms_f_terms
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_term_list_list context (field_c_value)
      in
         

      node
    in
    if is_null (getf c_value EntityStruct.node) then
      raise (SyntaxError "null node")
    else
      `Terms {
        f_terms
          = Lazy.from_fun f_terms ;
        c_value = c_value;
        context = context
      }

      


  and wrap_type_reference context c_value
   : type_reference =
    let f_var_type_name () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.type_reference_f_var_type_name
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_identifier_list context (field_c_value)
      in
         

      node
    in
    if is_null (getf c_value EntityStruct.node) then
      raise (SyntaxError "null node")
    else
      `TypeReference {
        f_var_type_name
          = Lazy.from_fun f_var_type_name ;
        c_value = c_value;
        context = context
      }

      


  and wrap_typed_string_decl context c_value
   : typed_string_decl =
    let f_type_id () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.typed_string_decl_f_type_id
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_identifier context (field_c_value)
      in
         

      node
    in
    let f_string_literals () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.typed_string_decl_f_string_literals
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_string_literal_list context (field_c_value)
      in
         

      node
    in
    if is_null (getf c_value EntityStruct.node) then
      raise (SyntaxError "null node")
    else
      `TypedStringDecl {
        f_type_id
          = Lazy.from_fun f_type_id ;
        f_string_literals
          = Lazy.from_fun f_string_literals ;
        c_value = c_value;
        context = context
      }

      


  and wrap_variable_decl context c_value
   : variable_decl =
    let f_var_name () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.variable_decl_f_var_name
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_identifier context (field_c_value)
      in
         

      node
    in
    let f_var_type () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.variable_decl_f_var_type
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         if is_null (getf field_c_value EntityStruct.node) then None else Some (wrap_type_reference context (field_c_value))
      in
         

      node
    in
    let f_expr () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.variable_decl_f_expr
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_term_list context (field_c_value)
      in
         

      node
    in
    if is_null (getf c_value EntityStruct.node) then
      raise (SyntaxError "null node")
    else
      `VariableDecl {
        f_var_name
          = Lazy.from_fun f_var_name ;
        f_var_type
          = Lazy.from_fun f_var_type ;
        f_expr
          = Lazy.from_fun f_expr ;
        c_value = c_value;
        context = context
      }

      


  and wrap_variable_reference context c_value
   : variable_reference =
    let f_variable_name () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.variable_reference_f_variable_name
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_identifier_list context (field_c_value)
      in
         

      node
    in
    let f_attribute_ref () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.variable_reference_f_attribute_ref
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         if is_null (getf field_c_value EntityStruct.node) then None else Some (wrap_attribute_reference context (field_c_value))
      in
         

      node
    in
    if is_null (getf c_value EntityStruct.node) then
      raise (SyntaxError "null node")
    else
      `VariableReference {
        f_variable_name
          = Lazy.from_fun f_variable_name ;
        f_attribute_ref
          = Lazy.from_fun f_attribute_ref ;
        c_value = c_value;
        context = context
      }

      


  and wrap_with_decl context c_value
   : with_decl =
    let f_is_limited () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.with_decl_f_is_limited
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_limited_node context (field_c_value)
      in
         

      node
    in
    let f_path_names () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.with_decl_f_path_names
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_string_literal_list context (field_c_value)
      in
         

      node
    in
    if is_null (getf c_value EntityStruct.node) then
      raise (SyntaxError "null node")
    else
      `WithDecl {
        f_is_limited
          = Lazy.from_fun f_is_limited ;
        f_path_names
          = Lazy.from_fun f_path_names ;
        c_value = c_value;
        context = context
      }



   
  and wrap_entity_info c_value = {
    rebindings = (getf c_value EntityInfoStruct.rebindings);
    from_rebound = (getf c_value EntityInfoStruct.from_rebound);
  }



   


and wrap_analysis_unit context c_value
   : analysis_unit = {
 c_value=c_value;
 context=context;
}

module Entity = struct
  type t = entity

  let info value =
    wrap_entity_info (getf value EntityStruct.info)

  let compare e1 e2 =
    let open Stdlib in
    let compare_node =
      compare (getf e1 EntityStruct.node) (getf e2 EntityStruct.node)
    in
    if compare_node = 0 then
      compare
        (getf (getf e1 EntityStruct.info) EntityInfoStruct.rebindings)
        (getf (getf e2 EntityStruct.info) EntityInfoStruct.rebindings)
    else
      compare_node

  let equal e1 e2 =
    compare e1 e2 = 0

  let hash e =
    Hashtbl.hash
      ( getf e EntityStruct.node
      , getf (getf e EntityStruct.info) EntityInfoStruct.rebindings )
end

module AnalysisUnit = struct
  type t = analysis_unit

  let root (unit : t) =
    let c_value = make EntityStruct.c_type in
    AnalysisUnitStruct.unit_root
      (unwrap_analysis_unit (unit))
      (addr c_value);
    if is_null (getf c_value EntityStruct.node) then None else Some (wrap_gpr_node unit.context (c_value))

  let diagnostics (unit : t) =
    let c_unit = unwrap_analysis_unit (unit) in
    let length = AnalysisUnitStruct.unit_diagnostic_count c_unit in
    let f i =
      let diag = allocate_n Diagnostic.c_type ~count:1 in
      let _ : int = AnalysisUnitStruct.unit_diagnostic c_unit i diag in
      !@ diag
    in
    List.init length f

  let filename (unit : t) =
    unwrap_str( AnalysisUnitStruct.unit_filename
      (unwrap_analysis_unit (unit)))

  let reparse ?charset:(charset="") ?buffer (unit : t) =
    match buffer with
    | None ->
        ignore
          (AnalysisUnitStruct.unit_reparse_from_file unit.c_value charset)
    | Some buffer ->
        ignore (AnalysisUnitStruct.unit_reparse_from_buffer unit.c_value
          charset buffer (Unsigned.Size_t.of_int (String.length buffer)))

  let first_token (unit : t) =
    let c_unit = unwrap_analysis_unit (unit) in
    let result_ptr = allocate_n Token.c_type ~count:1 in
    AnalysisUnitStruct.unit_first_token c_unit result_ptr ;
    Token.wrap (!@ result_ptr)

  let last_token (unit : t) =
    let c_unit = unwrap_analysis_unit (unit) in
    let result_ptr = allocate_n Token.c_type ~count:1 in
    AnalysisUnitStruct.unit_last_token c_unit result_ptr ;
    Token.wrap (!@ result_ptr)

  let token_count (unit : t) =
    AnalysisUnitStruct.unit_token_count
      (unwrap_analysis_unit (unit))

  let trivia_count (unit : t) =
    AnalysisUnitStruct.unit_trivia_count
      (unwrap_analysis_unit (unit))

  
  let fold_tokens f init node =
    match first_token node, last_token node with
    | Some tok_start, Some tok_end ->
        let rec aux acc tok_curr =
          let new_acc = f acc tok_curr in
          if Token.equal tok_curr tok_end then
            new_acc
          else (
            match Token.next tok_curr with
            | Some tok_next ->
                aux new_acc tok_next
            | None ->
                new_acc )
        in
        aux init tok_start
    | _ ->
        init

  let iter_tokens f node =
    match first_token node, last_token node with
    | Some tok_start, Some tok_end ->
        let rec aux tok_curr =
          f tok_curr;
          if not (Token.equal tok_curr tok_end) then (
            match Token.next tok_curr with
            | Some tok_next ->
                aux tok_next
            | None ->
                () )
        in
        aux tok_start
    | _ ->
        ()

  let map_tokens f node =
    match first_token node, last_token node with
    | Some tok_start, Some tok_end ->
        let rec aux tok_curr =
          let value = f tok_curr in
          if Token.equal tok_curr tok_end then
            [value]
          else (
            match Token.next tok_curr with
            | Some tok_next ->
                value :: aux tok_next
            | None ->
                [value] )
        in
        aux tok_start
    | _ ->
        []

  let tokens node =
    map_tokens (fun x -> x) node

end

module AnalysisContext = struct
  type t = analysis_context

  let create
    ?charset:(charset="")
    ?with_trivia:(with_trivia=true)
    ?tab_stop:(tab_stop=8)
    ?unit_provider:(unit_provider=UnitProvider.null) () : t =
    if tab_stop < 1 then
      raise (Invalid_argument "Invalid tab_stop (positive integer expected)") ;
    let c_context =
       AnalysisContextStruct.create_analysis_context
         charset
         Ctypes.null (* TODO: bind the file readers API to OCaml *)
         (!@unit_provider)
         Ctypes.null (* TODO: bind the event handlers API to OCaml *)
         with_trivia
         tab_stop
    in
    { c_value= c_context
      ; unit_provider= unit_provider }

  let get_from_file
    ?charset:(charset="")
    ?reparse:(reparse=false)
    ?grammar_rule:(grammar_rule=default_grammar_rule)
    (ctx : t)
    filename : AnalysisUnit.t =

    wrap_analysis_unit ctx (AnalysisContextStruct.get_analysis_unit_from_file ctx.c_value filename charset reparse grammar_rule)

  let get_from_buffer
    ?charset:(charset="")
    ?grammar_rule:(grammar_rule=default_grammar_rule)
    (ctx : t)
    filename
    buffer : AnalysisUnit.t =

    wrap_analysis_unit ctx (AnalysisContextStruct.get_analysis_unit_from_buffer ctx.c_value filename charset buffer (Unsigned.Size_t.of_int (String.length buffer)) grammar_rule)
end

   
module GprNodeArray : sig
   
  type t = gpr_node list

  val wrap : analysis_context -> GprNodeArrayStruct.t structure ptr -> t

  val unwrap : t -> GprNodeArrayStruct.t structure ptr

end = struct
   
  type t = gpr_node list

  let wrap (context : analysis_context) c_value_ptr =
    let c_value = !@ c_value_ptr in
    let length = getf c_value GprNodeArrayStruct.n in
    let items = c_value @. GprNodeArrayStruct.items in
    let f i =
      (* we want to allocate a fresh value for a record, otherwize, the c value
       * will still point to the memory at array location *)
      let fresh =
        allocate EntityStruct.c_type (!@ (items +@ i))
      in
      wrap_gpr_node context (!@ fresh)
    in
    let result = List.init length f in
    GprNodeArrayStruct.dec_ref c_value_ptr;
    result

  let unwrap value =
    let result = GprNodeArrayStruct.create (List.length value) in
    let items = result |-> GprNodeArrayStruct.items in
    let f i v =
      items +@ i <-@
        unwrap_gpr_node (v)
    in
    List.iteri f value;
    result


end


let context node =
  (* Given any node, extract the context field *)
  match (node :> gpr_node) with
  | `AdaAccessSubp fields -> fields.context
  | `AdaPragma fields -> fields.context
  | `AdaUse fields -> fields.context
  | `AdaWith fields -> fields.context
  | `AdaEntityKindFunction fields -> fields.context
  | `AdaEntityKindPackage fields -> fields.context
  | `AdaEntityKindProcedure fields -> fields.context
  | `AdaGeneric fields -> fields.context
  | `AdaLibraryItem fields -> fields.context
  | `AdaPkg fields -> fields.context
  | `AdaPkgBody fields -> fields.context
  | `AdaSubp fields -> fields.context
  | `AdaPrelude fields -> fields.context
  | `AdaSeparate fields -> fields.context
  | `AdaSkip fields -> fields.context
  | `AdaWithFormal fields -> fields.context
  | `AllQualifierAbsent fields -> fields.context
  | `AllQualifierPresent fields -> fields.context
  | `AttributeDecl fields -> fields.context
  | `AttributeReference fields -> fields.context
  | `AdaContextClauseList fields -> fields.context
  | `AdaPreludeNodeList fields -> fields.context
  | `AdaSkipList fields -> fields.context
  | `CaseItemList fields -> fields.context
  | `ExprList fields -> fields.context
  | `GprNodeList fields -> fields.context
  | `Choices fields -> fields.context
  | `TermList fields -> fields.context
  | `IdentifierList fields -> fields.context
  | `StringLiteralList fields -> fields.context
  | `TermListList fields -> fields.context
  | `WithDeclList fields -> fields.context
  | `BuiltinFunctionCall fields -> fields.context
  | `CaseConstruction fields -> fields.context
  | `CaseItem fields -> fields.context
  | `CompilationUnit fields -> fields.context
  | `EmptyDecl fields -> fields.context
  | `Prefix fields -> fields.context
  | `Identifier fields -> fields.context
  | `NumLiteral fields -> fields.context
  | `StringLiteral fields -> fields.context
  | `LimitedAbsent fields -> fields.context
  | `LimitedPresent fields -> fields.context
  | `OthersDesignator fields -> fields.context
  | `PackageDecl fields -> fields.context
  | `PackageExtension fields -> fields.context
  | `PackageRenaming fields -> fields.context
  | `PackageSpec fields -> fields.context
  | `PrivateAbsent fields -> fields.context
  | `PrivatePresent fields -> fields.context
  | `Project fields -> fields.context
  | `ProjectDeclaration fields -> fields.context
  | `ProjectExtension fields -> fields.context
  | `ProjectQualifierAbstract fields -> fields.context
  | `ProjectQualifierAggregate fields -> fields.context
  | `ProjectQualifierAggregateLibrary fields -> fields.context
  | `ProjectQualifierConfiguration fields -> fields.context
  | `ProjectQualifierLibrary fields -> fields.context
  | `ProjectQualifierStandard fields -> fields.context
  | `ProjectReference fields -> fields.context
  | `StringLiteralAt fields -> fields.context
  | `Terms fields -> fields.context
  | `TypeReference fields -> fields.context
  | `TypedStringDecl fields -> fields.context
  | `VariableDecl fields -> fields.context
  | `VariableReference fields -> fields.context
  | `WithDecl fields -> fields.context

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

module WithDecl = struct
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


  let equal node1 node2 =
    Entity.equal
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let hash node =
    Entity.hash
      (unwrap_gpr_node ((node :> gpr_node)))


  let f_is_limited node =
    match (node :> with_decl) with
    | `WithDecl fields ->
        Lazy.force fields.f_is_limited
  let f_path_names node =
    match (node :> with_decl) with
    | `WithDecl fields ->
        Lazy.force fields.f_path_names



end

module VariableReference = struct
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


  let equal node1 node2 =
    Entity.equal
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let hash node =
    Entity.hash
      (unwrap_gpr_node ((node :> gpr_node)))


  let f_variable_name node =
    match (node :> variable_reference) with
    | `VariableReference fields ->
        Lazy.force fields.f_variable_name
  let f_attribute_ref node =
    match (node :> variable_reference) with
    | `VariableReference fields ->
        Lazy.force fields.f_attribute_ref



end

module VariableDecl = struct
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


  let equal node1 node2 =
    Entity.equal
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let hash node =
    Entity.hash
      (unwrap_gpr_node ((node :> gpr_node)))


  let f_var_name node =
    match (node :> variable_decl) with
    | `VariableDecl fields ->
        Lazy.force fields.f_var_name
  let f_var_type node =
    match (node :> variable_decl) with
    | `VariableDecl fields ->
        Lazy.force fields.f_var_type
  let f_expr node =
    match (node :> variable_decl) with
    | `VariableDecl fields ->
        Lazy.force fields.f_expr



end

module TypedStringDecl = struct
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


  let equal node1 node2 =
    Entity.equal
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let hash node =
    Entity.hash
      (unwrap_gpr_node ((node :> gpr_node)))


  let f_type_id node =
    match (node :> typed_string_decl) with
    | `TypedStringDecl fields ->
        Lazy.force fields.f_type_id
  let f_string_literals node =
    match (node :> typed_string_decl) with
    | `TypedStringDecl fields ->
        Lazy.force fields.f_string_literals



end

module TypeReference = struct
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


  let equal node1 node2 =
    Entity.equal
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let hash node =
    Entity.hash
      (unwrap_gpr_node ((node :> gpr_node)))


  let f_var_type_name node =
    match (node :> type_reference) with
    | `TypeReference fields ->
        Lazy.force fields.f_var_type_name



end

module Terms = struct
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


  let equal node1 node2 =
    Entity.equal
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let hash node =
    Entity.hash
      (unwrap_gpr_node ((node :> gpr_node)))


  let f_terms node =
    match (node :> terms) with
    | `Terms fields ->
        Lazy.force fields.f_terms



end

module StringLiteralAt = struct
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


  let equal node1 node2 =
    Entity.equal
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let hash node =
    Entity.hash
      (unwrap_gpr_node ((node :> gpr_node)))


  let f_str_lit node =
    match (node :> string_literal_at) with
    | `StringLiteralAt fields ->
        Lazy.force fields.f_str_lit
  let f_at_lit node =
    match (node :> string_literal_at) with
    | `StringLiteralAt fields ->
        Lazy.force fields.f_at_lit



end

module ProjectReference = struct
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


  let equal node1 node2 =
    Entity.equal
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let hash node =
    Entity.hash
      (unwrap_gpr_node ((node :> gpr_node)))


  let f_attr_ref node =
    match (node :> project_reference) with
    | `ProjectReference fields ->
        Lazy.force fields.f_attr_ref



end

module ProjectQualifierStandard = struct
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


  let equal node1 node2 =
    Entity.equal
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let hash node =
    Entity.hash
      (unwrap_gpr_node ((node :> gpr_node)))





end

module ProjectQualifierLibrary = struct
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


  let equal node1 node2 =
    Entity.equal
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let hash node =
    Entity.hash
      (unwrap_gpr_node ((node :> gpr_node)))





end

module ProjectQualifierConfiguration = struct
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


  let equal node1 node2 =
    Entity.equal
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let hash node =
    Entity.hash
      (unwrap_gpr_node ((node :> gpr_node)))





end

module ProjectQualifierAggregateLibrary = struct
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


  let equal node1 node2 =
    Entity.equal
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let hash node =
    Entity.hash
      (unwrap_gpr_node ((node :> gpr_node)))





end

module ProjectQualifierAggregate = struct
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


  let equal node1 node2 =
    Entity.equal
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let hash node =
    Entity.hash
      (unwrap_gpr_node ((node :> gpr_node)))





end

module ProjectQualifierAbstract = struct
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


  let equal node1 node2 =
    Entity.equal
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let hash node =
    Entity.hash
      (unwrap_gpr_node ((node :> gpr_node)))





end

module ProjectQualifier = struct
  type t =
    [
      | ProjectQualifierAbstract.t
      | ProjectQualifierAggregate.t
      | ProjectQualifierAggregateLibrary.t
      | ProjectQualifierConfiguration.t
      | ProjectQualifierLibrary.t
      | ProjectQualifierStandard.t
    ]


  let equal node1 node2 =
    Entity.equal
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let hash node =
    Entity.hash
      (unwrap_gpr_node ((node :> gpr_node)))





end

module ProjectExtension = struct
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


  let equal node1 node2 =
    Entity.equal
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let hash node =
    Entity.hash
      (unwrap_gpr_node ((node :> gpr_node)))


  let f_is_all node =
    match (node :> project_extension) with
    | `ProjectExtension fields ->
        Lazy.force fields.f_is_all
  let f_path_name node =
    match (node :> project_extension) with
    | `ProjectExtension fields ->
        Lazy.force fields.f_path_name



end

module ProjectDeclaration = struct
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


  let equal node1 node2 =
    Entity.equal
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let hash node =
    Entity.hash
      (unwrap_gpr_node ((node :> gpr_node)))


  let f_qualifier node =
    match (node :> project_declaration) with
    | `ProjectDeclaration fields ->
        Lazy.force fields.f_qualifier
  let f_project_name node =
    match (node :> project_declaration) with
    | `ProjectDeclaration fields ->
        Lazy.force fields.f_project_name
  let f_extension node =
    match (node :> project_declaration) with
    | `ProjectDeclaration fields ->
        Lazy.force fields.f_extension
  let f_decls node =
    match (node :> project_declaration) with
    | `ProjectDeclaration fields ->
        Lazy.force fields.f_decls
  let f_end_name node =
    match (node :> project_declaration) with
    | `ProjectDeclaration fields ->
        Lazy.force fields.f_end_name



end

module Project = struct
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


  let equal node1 node2 =
    Entity.equal
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let hash node =
    Entity.hash
      (unwrap_gpr_node ((node :> gpr_node)))


  let f_context_clauses node =
    match (node :> project) with
    | `Project fields ->
        Lazy.force fields.f_context_clauses
  let f_project_decl node =
    match (node :> project) with
    | `Project fields ->
        Lazy.force fields.f_project_decl



end

module PrivatePresent = struct
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


  let equal node1 node2 =
    Entity.equal
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let hash node =
    Entity.hash
      (unwrap_gpr_node ((node :> gpr_node)))





end

module PrivateAbsent = struct
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


  let equal node1 node2 =
    Entity.equal
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let hash node =
    Entity.hash
      (unwrap_gpr_node ((node :> gpr_node)))





end

module PrivateNode = struct
  type t =
    [
      | PrivateAbsent.t
      | PrivatePresent.t
    ]


  let equal node1 node2 =
    Entity.equal
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let hash node =
    Entity.hash
      (unwrap_gpr_node ((node :> gpr_node)))

let p_as_bool
    (node)
    =
      let result_ptr =
        allocate_n bool ~count:1
      in
      (* The result of this call should already be checked by the raisable
         mechanism *)
      let _ : int =
        CFunctions.private_node_p_as_bool
          (addr (unwrap_gpr_node (node)))
          (result_ptr)
      in
      !@ result_ptr





end

module PackageSpec = struct
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


  let equal node1 node2 =
    Entity.equal
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let hash node =
    Entity.hash
      (unwrap_gpr_node ((node :> gpr_node)))


  let f_extension node =
    match (node :> package_spec) with
    | `PackageSpec fields ->
        Lazy.force fields.f_extension
  let f_decls node =
    match (node :> package_spec) with
    | `PackageSpec fields ->
        Lazy.force fields.f_decls
  let f_end_name node =
    match (node :> package_spec) with
    | `PackageSpec fields ->
        Lazy.force fields.f_end_name



end

module PackageRenaming = struct
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


  let equal node1 node2 =
    Entity.equal
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let hash node =
    Entity.hash
      (unwrap_gpr_node ((node :> gpr_node)))


  let f_renamed_name node =
    match (node :> package_renaming) with
    | `PackageRenaming fields ->
        Lazy.force fields.f_renamed_name



end

module PackageExtension = struct
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


  let equal node1 node2 =
    Entity.equal
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let hash node =
    Entity.hash
      (unwrap_gpr_node ((node :> gpr_node)))


  let f_extended_name node =
    match (node :> package_extension) with
    | `PackageExtension fields ->
        Lazy.force fields.f_extended_name



end

module PackageDecl = struct
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


  let equal node1 node2 =
    Entity.equal
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let hash node =
    Entity.hash
      (unwrap_gpr_node ((node :> gpr_node)))


  let f_pkg_name node =
    match (node :> package_decl) with
    | `PackageDecl fields ->
        Lazy.force fields.f_pkg_name
  let f_pkg_spec node =
    match (node :> package_decl) with
    | `PackageDecl fields ->
        Lazy.force fields.f_pkg_spec



end

module OthersDesignator = struct
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


  let equal node1 node2 =
    Entity.equal
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let hash node =
    Entity.hash
      (unwrap_gpr_node ((node :> gpr_node)))





end

module LimitedPresent = struct
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


  let equal node1 node2 =
    Entity.equal
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let hash node =
    Entity.hash
      (unwrap_gpr_node ((node :> gpr_node)))





end

module LimitedAbsent = struct
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


  let equal node1 node2 =
    Entity.equal
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let hash node =
    Entity.hash
      (unwrap_gpr_node ((node :> gpr_node)))





end

module LimitedNode = struct
  type t =
    [
      | LimitedAbsent.t
      | LimitedPresent.t
    ]


  let equal node1 node2 =
    Entity.equal
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let hash node =
    Entity.hash
      (unwrap_gpr_node ((node :> gpr_node)))

let p_as_bool
    (node)
    =
      let result_ptr =
        allocate_n bool ~count:1
      in
      (* The result of this call should already be checked by the raisable
         mechanism *)
      let _ : int =
        CFunctions.limited_node_p_as_bool
          (addr (unwrap_gpr_node (node)))
          (result_ptr)
      in
      !@ result_ptr





end

module StringLiteral = struct
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


  let equal node1 node2 =
    Entity.equal
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let hash node =
    Entity.hash
      (unwrap_gpr_node ((node :> gpr_node)))





end

module NumLiteral = struct
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


  let equal node1 node2 =
    Entity.equal
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let hash node =
    Entity.hash
      (unwrap_gpr_node ((node :> gpr_node)))





end

module Identifier = struct
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


  let equal node1 node2 =
    Entity.equal
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let hash node =
    Entity.hash
      (unwrap_gpr_node ((node :> gpr_node)))





end

module SingleTokNode = struct
  type t =
    [
      | Identifier.t
      | NumLiteral.t
      | StringLiteral.t
    ]


  let equal node1 node2 =
    Entity.equal
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let hash node =
    Entity.hash
      (unwrap_gpr_node ((node :> gpr_node)))





end

module Prefix = struct
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


  let equal node1 node2 =
    Entity.equal
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let hash node =
    Entity.hash
      (unwrap_gpr_node ((node :> gpr_node)))


  let f_prefix node =
    match (node :> prefix) with
    | `Prefix fields ->
        Lazy.force fields.f_prefix
  let f_suffix node =
    match (node :> prefix) with
    | `Prefix fields ->
        Lazy.force fields.f_suffix



end

module Expr = struct
  type t =
    [
      | Prefix.t
      | SingleTokNode.t
    ]


  let equal node1 node2 =
    Entity.equal
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let hash node =
    Entity.hash
      (unwrap_gpr_node ((node :> gpr_node)))





end

module EmptyDecl = struct
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


  let equal node1 node2 =
    Entity.equal
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let hash node =
    Entity.hash
      (unwrap_gpr_node ((node :> gpr_node)))





end

module CompilationUnit = struct
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


  let equal node1 node2 =
    Entity.equal
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let hash node =
    Entity.hash
      (unwrap_gpr_node ((node :> gpr_node)))


  let f_project node =
    match (node :> compilation_unit) with
    | `CompilationUnit fields ->
        Lazy.force fields.f_project



end

module CaseItem = struct
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


  let equal node1 node2 =
    Entity.equal
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let hash node =
    Entity.hash
      (unwrap_gpr_node ((node :> gpr_node)))


  let f_choice node =
    match (node :> case_item) with
    | `CaseItem fields ->
        Lazy.force fields.f_choice
  let f_decls node =
    match (node :> case_item) with
    | `CaseItem fields ->
        Lazy.force fields.f_decls



end

module CaseConstruction = struct
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


  let equal node1 node2 =
    Entity.equal
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let hash node =
    Entity.hash
      (unwrap_gpr_node ((node :> gpr_node)))


  let f_var_ref node =
    match (node :> case_construction) with
    | `CaseConstruction fields ->
        Lazy.force fields.f_var_ref
  let f_items node =
    match (node :> case_construction) with
    | `CaseConstruction fields ->
        Lazy.force fields.f_items



end

module BuiltinFunctionCall = struct
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


  let equal node1 node2 =
    Entity.equal
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let hash node =
    Entity.hash
      (unwrap_gpr_node ((node :> gpr_node)))


  let f_function_name node =
    match (node :> builtin_function_call) with
    | `BuiltinFunctionCall fields ->
        Lazy.force fields.f_function_name
  let f_parameters node =
    match (node :> builtin_function_call) with
    | `BuiltinFunctionCall fields ->
        Lazy.force fields.f_parameters



end

module WithDeclList = struct
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


  let equal node1 node2 =
    Entity.equal
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let hash node =
    Entity.hash
      (unwrap_gpr_node ((node :> gpr_node)))



  let f_list node =
    match (node :> with_decl_list) with
    | `WithDeclList fields ->
        Lazy.force fields.list



end

module TermListList = struct
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


  let equal node1 node2 =
    Entity.equal
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let hash node =
    Entity.hash
      (unwrap_gpr_node ((node :> gpr_node)))



  let f_list node =
    match (node :> term_list_list) with
    | `TermListList fields ->
        Lazy.force fields.list



end

module StringLiteralList = struct
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


  let equal node1 node2 =
    Entity.equal
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let hash node =
    Entity.hash
      (unwrap_gpr_node ((node :> gpr_node)))



  let f_list node =
    match (node :> string_literal_list) with
    | `StringLiteralList fields ->
        Lazy.force fields.list



end

module IdentifierList = struct
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


  let equal node1 node2 =
    Entity.equal
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let hash node =
    Entity.hash
      (unwrap_gpr_node ((node :> gpr_node)))



  let f_list node =
    match (node :> identifier_list) with
    | `IdentifierList fields ->
        Lazy.force fields.list



end

module TermList = struct
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


  let equal node1 node2 =
    Entity.equal
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let hash node =
    Entity.hash
      (unwrap_gpr_node ((node :> gpr_node)))



  let f_list node =
    match (node :> term_list) with
    | `TermList fields ->
        Lazy.force fields.list



end

module Choices = struct
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


  let equal node1 node2 =
    Entity.equal
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let hash node =
    Entity.hash
      (unwrap_gpr_node ((node :> gpr_node)))



  let f_list node =
    match (node :> choices) with
    | `Choices fields ->
        Lazy.force fields.list



end

module GprNodeList = struct
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


  let equal node1 node2 =
    Entity.equal
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let hash node =
    Entity.hash
      (unwrap_gpr_node ((node :> gpr_node)))



  let f_list node =
    match (node :> gpr_node_list) with
    | `GprNodeList fields ->
        Lazy.force fields.list
    | `Choices fields ->
        Lazy.force fields.list
    | `TermList fields ->
        Lazy.force fields.list



end

module ExprList = struct
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


  let equal node1 node2 =
    Entity.equal
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let hash node =
    Entity.hash
      (unwrap_gpr_node ((node :> gpr_node)))



  let f_list node =
    match (node :> expr_list) with
    | `ExprList fields ->
        Lazy.force fields.list



end

module CaseItemList = struct
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


  let equal node1 node2 =
    Entity.equal
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let hash node =
    Entity.hash
      (unwrap_gpr_node ((node :> gpr_node)))



  let f_list node =
    match (node :> case_item_list) with
    | `CaseItemList fields ->
        Lazy.force fields.list



end

module AdaSkipList = struct
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


  let equal node1 node2 =
    Entity.equal
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let hash node =
    Entity.hash
      (unwrap_gpr_node ((node :> gpr_node)))



  let f_list node =
    match (node :> ada_skip_list) with
    | `AdaSkipList fields ->
        Lazy.force fields.list



end

module AdaPreludeNodeList = struct
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


  let equal node1 node2 =
    Entity.equal
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let hash node =
    Entity.hash
      (unwrap_gpr_node ((node :> gpr_node)))



  let f_list node =
    match (node :> ada_prelude_node_list) with
    | `AdaPreludeNodeList fields ->
        Lazy.force fields.list



end

module AdaContextClauseList = struct
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


  let equal node1 node2 =
    Entity.equal
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let hash node =
    Entity.hash
      (unwrap_gpr_node ((node :> gpr_node)))



  let f_list node =
    match (node :> ada_context_clause_list) with
    | `AdaContextClauseList fields ->
        Lazy.force fields.list



end

module BaseList = struct
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


  let equal node1 node2 =
    Entity.equal
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let hash node =
    Entity.hash
      (unwrap_gpr_node ((node :> gpr_node)))





end

module AttributeReference = struct
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


  let equal node1 node2 =
    Entity.equal
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let hash node =
    Entity.hash
      (unwrap_gpr_node ((node :> gpr_node)))


  let f_attribute_name node =
    match (node :> attribute_reference) with
    | `AttributeReference fields ->
        Lazy.force fields.f_attribute_name
  let f_attribute_index node =
    match (node :> attribute_reference) with
    | `AttributeReference fields ->
        Lazy.force fields.f_attribute_index



end

module AttributeDecl = struct
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


  let equal node1 node2 =
    Entity.equal
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let hash node =
    Entity.hash
      (unwrap_gpr_node ((node :> gpr_node)))


  let f_attr_name node =
    match (node :> attribute_decl) with
    | `AttributeDecl fields ->
        Lazy.force fields.f_attr_name
  let f_attr_index node =
    match (node :> attribute_decl) with
    | `AttributeDecl fields ->
        Lazy.force fields.f_attr_index
  let f_expr node =
    match (node :> attribute_decl) with
    | `AttributeDecl fields ->
        Lazy.force fields.f_expr



end

module AllQualifierPresent = struct
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


  let equal node1 node2 =
    Entity.equal
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let hash node =
    Entity.hash
      (unwrap_gpr_node ((node :> gpr_node)))





end

module AllQualifierAbsent = struct
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


  let equal node1 node2 =
    Entity.equal
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let hash node =
    Entity.hash
      (unwrap_gpr_node ((node :> gpr_node)))





end

module AllQualifier = struct
  type t =
    [
      | AllQualifierAbsent.t
      | AllQualifierPresent.t
    ]


  let equal node1 node2 =
    Entity.equal
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let hash node =
    Entity.hash
      (unwrap_gpr_node ((node :> gpr_node)))

let p_as_bool
    (node)
    =
      let result_ptr =
        allocate_n bool ~count:1
      in
      (* The result of this call should already be checked by the raisable
         mechanism *)
      let _ : int =
        CFunctions.all_qualifier_p_as_bool
          (addr (unwrap_gpr_node (node)))
          (result_ptr)
      in
      !@ result_ptr





end

module AdaWithFormal = struct
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


  let equal node1 node2 =
    Entity.equal
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let hash node =
    Entity.hash
      (unwrap_gpr_node ((node :> gpr_node)))


  let f_kind node =
    match (node :> ada_with_formal) with
    | `AdaWithFormal fields ->
        Lazy.force fields.f_kind
  let f_skips node =
    match (node :> ada_with_formal) with
    | `AdaWithFormal fields ->
        Lazy.force fields.f_skips



end

module AdaSkip = struct
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


  let equal node1 node2 =
    Entity.equal
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let hash node =
    Entity.hash
      (unwrap_gpr_node ((node :> gpr_node)))





end

module AdaSeparate = struct
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


  let equal node1 node2 =
    Entity.equal
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let hash node =
    Entity.hash
      (unwrap_gpr_node ((node :> gpr_node)))


  let f_parent_name node =
    match (node :> ada_separate) with
    | `AdaSeparate fields ->
        Lazy.force fields.f_parent_name



end

module AdaPrelude = struct
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


  let equal node1 node2 =
    Entity.equal
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let hash node =
    Entity.hash
      (unwrap_gpr_node ((node :> gpr_node)))


  let f_context_clauses node =
    match (node :> ada_prelude) with
    | `AdaPrelude fields ->
        Lazy.force fields.f_context_clauses
  let f_library_item node =
    match (node :> ada_prelude) with
    | `AdaPrelude fields ->
        Lazy.force fields.f_library_item



end

module AdaSubp = struct
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


  let equal node1 node2 =
    Entity.equal
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let hash node =
    Entity.hash
      (unwrap_gpr_node ((node :> gpr_node)))


  let f_subp_kind node =
    match (node :> ada_subp) with
    | `AdaSubp fields ->
        Lazy.force fields.f_subp_kind



end

module AdaPkgBody = struct
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


  let equal node1 node2 =
    Entity.equal
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let hash node =
    Entity.hash
      (unwrap_gpr_node ((node :> gpr_node)))





end

module AdaPkg = struct
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


  let equal node1 node2 =
    Entity.equal
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let hash node =
    Entity.hash
      (unwrap_gpr_node ((node :> gpr_node)))


  let f_has_private node =
    match (node :> ada_pkg) with
    | `AdaPkg fields ->
        Lazy.force fields.f_has_private



end

module AdaMain = struct
  type t =
    [
      | AdaPkg.t
      | AdaPkgBody.t
      | AdaSubp.t
    ]


  let equal node1 node2 =
    Entity.equal
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let hash node =
    Entity.hash
      (unwrap_gpr_node ((node :> gpr_node)))





end

module AdaLibraryItem = struct
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


  let equal node1 node2 =
    Entity.equal
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let hash node =
    Entity.hash
      (unwrap_gpr_node ((node :> gpr_node)))


  let f_generic_stub node =
    match (node :> ada_library_item) with
    | `AdaLibraryItem fields ->
        Lazy.force fields.f_generic_stub
  let f_separate node =
    match (node :> ada_library_item) with
    | `AdaLibraryItem fields ->
        Lazy.force fields.f_separate
  let f_main node =
    match (node :> ada_library_item) with
    | `AdaLibraryItem fields ->
        Lazy.force fields.f_main



end

module AdaGeneric = struct
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


  let equal node1 node2 =
    Entity.equal
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let hash node =
    Entity.hash
      (unwrap_gpr_node ((node :> gpr_node)))


  let f_skips node =
    match (node :> ada_generic) with
    | `AdaGeneric fields ->
        Lazy.force fields.f_skips



end

module AdaEntityKindProcedure = struct
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


  let equal node1 node2 =
    Entity.equal
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let hash node =
    Entity.hash
      (unwrap_gpr_node ((node :> gpr_node)))





end

module AdaEntityKindPackage = struct
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


  let equal node1 node2 =
    Entity.equal
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let hash node =
    Entity.hash
      (unwrap_gpr_node ((node :> gpr_node)))





end

module AdaEntityKindFunction = struct
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


  let equal node1 node2 =
    Entity.equal
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let hash node =
    Entity.hash
      (unwrap_gpr_node ((node :> gpr_node)))





end

module AdaEntityKind = struct
  type t =
    [
      | AdaEntityKindFunction.t
      | AdaEntityKindPackage.t
      | AdaEntityKindProcedure.t
    ]


  let equal node1 node2 =
    Entity.equal
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let hash node =
    Entity.hash
      (unwrap_gpr_node ((node :> gpr_node)))





end

module AdaWith = struct
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


  let equal node1 node2 =
    Entity.equal
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let hash node =
    Entity.hash
      (unwrap_gpr_node ((node :> gpr_node)))


  let f_has_limited node =
    match (node :> ada_with) with
    | `AdaWith fields ->
        Lazy.force fields.f_has_limited
  let f_has_private node =
    match (node :> ada_with) with
    | `AdaWith fields ->
        Lazy.force fields.f_has_private
  let f_packages node =
    match (node :> ada_with) with
    | `AdaWith fields ->
        Lazy.force fields.f_packages



end

module AdaUse = struct
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


  let equal node1 node2 =
    Entity.equal
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let hash node =
    Entity.hash
      (unwrap_gpr_node ((node :> gpr_node)))


  let f_skips node =
    match (node :> ada_use) with
    | `AdaUse fields ->
        Lazy.force fields.f_skips



end

module AdaPragma = struct
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


  let equal node1 node2 =
    Entity.equal
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let hash node =
    Entity.hash
      (unwrap_gpr_node ((node :> gpr_node)))


  let f_skips node =
    match (node :> ada_pragma) with
    | `AdaPragma fields ->
        Lazy.force fields.f_skips



end

module AdaContextClause = struct
  type t =
    [
      | AdaPragma.t
      | AdaUse.t
      | AdaWith.t
    ]


  let equal node1 node2 =
    Entity.equal
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let hash node =
    Entity.hash
      (unwrap_gpr_node ((node :> gpr_node)))





end

module AdaAccessSubp = struct
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


  let equal node1 node2 =
    Entity.equal
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let hash node =
    Entity.hash
      (unwrap_gpr_node ((node :> gpr_node)))


  let f_subp_kind node =
    match (node :> ada_access_subp) with
    | `AdaAccessSubp fields ->
        Lazy.force fields.f_subp_kind
  let f_skips node =
    match (node :> ada_access_subp) with
    | `AdaAccessSubp fields ->
        Lazy.force fields.f_skips



end

module AdaPreludeNode = struct
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


  let equal node1 node2 =
    Entity.equal
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let hash node =
    Entity.hash
      (unwrap_gpr_node ((node :> gpr_node)))





end

module GprNode = struct
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


  let equal node1 node2 =
    Entity.equal
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_gpr_node ((node1 :> gpr_node)))
      (unwrap_gpr_node ((node2 :> gpr_node)))

  let hash node =
    Entity.hash
      (unwrap_gpr_node ((node :> gpr_node)))

let parent
    (node)
    =
      let result_ptr =
        allocate_n EntityStruct.c_type ~count:1
      in
      (* The result of this call should already be checked by the raisable
         mechanism *)
      let _ : int =
        CFunctions.gpr_node_parent
          (addr (unwrap_gpr_node (node)))
          (result_ptr)
      in
      if is_null (getf !@ result_ptr EntityStruct.node) then None else Some (wrap_gpr_node (context node) (!@ result_ptr))

let parents
    ?(with_self=true)
    (node)
    =
      let result_ptr =
        allocate_n GprNodeArrayStruct.c_type ~count:1
      in
      (* The result of this call should already be checked by the raisable
         mechanism *)
      let c_with_self =
            
        with_self
      in
      let _ : int =
        CFunctions.gpr_node_parents
          (addr (unwrap_gpr_node (node)))
          c_with_self
          (result_ptr)
      in
         
      GprNodeArray.wrap (context node) (!@ result_ptr)

let children
    (node)
    =
      let result_ptr =
        allocate_n GprNodeArrayStruct.c_type ~count:1
      in
      (* The result of this call should already be checked by the raisable
         mechanism *)
      let _ : int =
        CFunctions.gpr_node_children
          (addr (unwrap_gpr_node (node)))
          (result_ptr)
      in
      GprNodeArray.wrap (context node) (!@ result_ptr)

let token_start
    (node)
    =
      let result_ptr =
        allocate_n Token.c_type ~count:1
      in
      (* The result of this call should already be checked by the raisable
         mechanism *)
      let _ : int =
        CFunctions.gpr_node_token_start
          (addr (unwrap_gpr_node (node)))
          (result_ptr)
      in
      Token.wrap (!@ result_ptr)

let token_end
    (node)
    =
      let result_ptr =
        allocate_n Token.c_type ~count:1
      in
      (* The result of this call should already be checked by the raisable
         mechanism *)
      let _ : int =
        CFunctions.gpr_node_token_end
          (addr (unwrap_gpr_node (node)))
          (result_ptr)
      in
      Token.wrap (!@ result_ptr)

let child_index
    (node)
    =
      let result_ptr =
        allocate_n int ~count:1
      in
      (* The result of this call should already be checked by the raisable
         mechanism *)
      let _ : int =
        CFunctions.gpr_node_child_index
          (addr (unwrap_gpr_node (node)))
          (result_ptr)
      in
      !@ result_ptr

let previous_sibling
    (node)
    =
      let result_ptr =
        allocate_n EntityStruct.c_type ~count:1
      in
      (* The result of this call should already be checked by the raisable
         mechanism *)
      let _ : int =
        CFunctions.gpr_node_previous_sibling
          (addr (unwrap_gpr_node (node)))
          (result_ptr)
      in
      if is_null (getf !@ result_ptr EntityStruct.node) then None else Some (wrap_gpr_node (context node) (!@ result_ptr))

let next_sibling
    (node)
    =
      let result_ptr =
        allocate_n EntityStruct.c_type ~count:1
      in
      (* The result of this call should already be checked by the raisable
         mechanism *)
      let _ : int =
        CFunctions.gpr_node_next_sibling
          (addr (unwrap_gpr_node (node)))
          (result_ptr)
      in
      if is_null (getf !@ result_ptr EntityStruct.node) then None else Some (wrap_gpr_node (context node) (!@ result_ptr))

let unit
    (node)
    =
      let result_ptr =
        allocate_n AnalysisUnitStruct.c_type ~count:1
      in
      (* The result of this call should already be checked by the raisable
         mechanism *)
      let _ : int =
        CFunctions.gpr_node_unit
          (addr (unwrap_gpr_node (node)))
          (result_ptr)
      in
      wrap_analysis_unit (context node) (!@ result_ptr)

let is_ghost
    (node)
    =
      let result_ptr =
        allocate_n bool ~count:1
      in
      (* The result of this call should already be checked by the raisable
         mechanism *)
      let _ : int =
        CFunctions.gpr_node_is_ghost
          (addr (unwrap_gpr_node (node)))
          (result_ptr)
      in
      !@ result_ptr

let full_sloc_image
    (node)
    =
      let result_ptr =
        allocate_n StringType.c_type ~count:1
      in
      (* The result of this call should already be checked by the raisable
         mechanism *)
      let _ : int =
        CFunctions.gpr_node_full_sloc_image
          (addr (unwrap_gpr_node (node)))
          (result_ptr)
      in
      StringType.wrap (!@ result_ptr)





  let kind_name = function
    | #with_decl ->
        "WithDecl"
    | #variable_reference ->
        "VariableReference"
    | #variable_decl ->
        "VariableDecl"
    | #typed_string_decl ->
        "TypedStringDecl"
    | #type_reference ->
        "TypeReference"
    | #terms ->
        "Terms"
    | #string_literal_at ->
        "StringLiteralAt"
    | #project_reference ->
        "ProjectReference"
    | #project_qualifier_standard ->
        "ProjectQualifierStandard"
    | #project_qualifier_library ->
        "ProjectQualifierLibrary"
    | #project_qualifier_configuration ->
        "ProjectQualifierConfiguration"
    | #project_qualifier_aggregate_library ->
        "ProjectQualifierAggregateLibrary"
    | #project_qualifier_aggregate ->
        "ProjectQualifierAggregate"
    | #project_qualifier_abstract ->
        "ProjectQualifierAbstract"
    | #project_extension ->
        "ProjectExtension"
    | #project_declaration ->
        "ProjectDeclaration"
    | #project ->
        "Project"
    | #private_present ->
        "PrivatePresent"
    | #private_absent ->
        "PrivateAbsent"
    | #package_spec ->
        "PackageSpec"
    | #package_renaming ->
        "PackageRenaming"
    | #package_extension ->
        "PackageExtension"
    | #package_decl ->
        "PackageDecl"
    | #others_designator ->
        "OthersDesignator"
    | #limited_present ->
        "LimitedPresent"
    | #limited_absent ->
        "LimitedAbsent"
    | #string_literal ->
        "StringLiteral"
    | #num_literal ->
        "NumLiteral"
    | #identifier ->
        "Identifier"
    | #prefix ->
        "Prefix"
    | #empty_decl ->
        "EmptyDecl"
    | #compilation_unit ->
        "CompilationUnit"
    | #case_item ->
        "CaseItem"
    | #case_construction ->
        "CaseConstruction"
    | #builtin_function_call ->
        "BuiltinFunctionCall"
    | #with_decl_list ->
        "WithDeclList"
    | #term_list_list ->
        "TermListList"
    | #string_literal_list ->
        "StringLiteralList"
    | #identifier_list ->
        "IdentifierList"
    | #term_list ->
        "TermList"
    | #choices ->
        "Choices"
    | #gpr_node_list ->
        "GprNodeList"
    | #expr_list ->
        "ExprList"
    | #case_item_list ->
        "CaseItemList"
    | #ada_skip_list ->
        "AdaSkipList"
    | #ada_prelude_node_list ->
        "AdaPreludeNodeList"
    | #ada_context_clause_list ->
        "AdaContextClauseList"
    | #attribute_reference ->
        "AttributeReference"
    | #attribute_decl ->
        "AttributeDecl"
    | #all_qualifier_present ->
        "AllQualifierPresent"
    | #all_qualifier_absent ->
        "AllQualifierAbsent"
    | #ada_with_formal ->
        "AdaWithFormal"
    | #ada_skip ->
        "AdaSkip"
    | #ada_separate ->
        "AdaSeparate"
    | #ada_prelude ->
        "AdaPrelude"
    | #ada_subp ->
        "AdaSubp"
    | #ada_pkg_body ->
        "AdaPkgBody"
    | #ada_pkg ->
        "AdaPkg"
    | #ada_library_item ->
        "AdaLibraryItem"
    | #ada_generic ->
        "AdaGeneric"
    | #ada_entity_kind_procedure ->
        "AdaEntityKindProcedure"
    | #ada_entity_kind_package ->
        "AdaEntityKindPackage"
    | #ada_entity_kind_function ->
        "AdaEntityKindFunction"
    | #ada_with ->
        "AdaWith"
    | #ada_use ->
        "AdaUse"
    | #ada_pragma ->
        "AdaPragma"
    | #ada_access_subp ->
        "AdaAccessSubp"

  let text node =
    match token_start node, token_end node with
    | Some tok_start, Some tok_end ->
        Token.text_range tok_start tok_end
    | _ ->
        ""

  let image node =
    let c_result_ptr = allocate_n Text.c_type ~count:1 in
    CFunctions.image
      (addr (unwrap_gpr_node (node)))
      c_result_ptr;
    !@ c_result_ptr

  let entity_image node =
    let c_result_ptr = allocate_n Text.c_type ~count:1 in
    let node_c_value = unwrap_gpr_node (node) in
    CFunctions.entity_image (addr node_c_value) c_result_ptr;
    !@ c_result_ptr

  let is_token_node node =
    let node_c_value = unwrap_gpr_node (node) in
    CFunctions.node_is_token_node (addr node_c_value)

  let sloc_range node =
    let c_result_ptr = allocate_n SlocRange.c_type ~count:1 in
    CFunctions.node_sloc_range
      (addr (unwrap_gpr_node (node)))
      c_result_ptr;
    !@ c_result_ptr

  
  let fold_tokens f init node =
    match token_start node, token_end node with
    | Some tok_start, Some tok_end ->
        let rec aux acc tok_curr =
          let new_acc = f acc tok_curr in
          if Token.equal tok_curr tok_end then
            new_acc
          else (
            match Token.next tok_curr with
            | Some tok_next ->
                aux new_acc tok_next
            | None ->
                new_acc )
        in
        aux init tok_start
    | _ ->
        init

  let iter_tokens f node =
    match token_start node, token_end node with
    | Some tok_start, Some tok_end ->
        let rec aux tok_curr =
          f tok_curr;
          if not (Token.equal tok_curr tok_end) then (
            match Token.next tok_curr with
            | Some tok_next ->
                aux tok_next
            | None ->
                () )
        in
        aux tok_start
    | _ ->
        ()

  let map_tokens f node =
    match token_start node, token_end node with
    | Some tok_start, Some tok_end ->
        let rec aux tok_curr =
          let value = f tok_curr in
          if Token.equal tok_curr tok_end then
            [value]
          else (
            match Token.next tok_curr with
            | Some tok_next ->
                value :: aux tok_next
            | None ->
                [value] )
        in
        aux tok_start
    | _ ->
        []

  let tokens node =
    map_tokens (fun x -> x) node


  let lookup node sloc =
    let node_c_value = unwrap_gpr_node (node) in
    let sloc_ptr = allocate Sloc.c_type sloc in
    let result_ptr = allocate_n EntityStruct.c_type ~count:1 in
    CFunctions.lookup_in_node
      (addr node_c_value) sloc_ptr result_ptr;
    if is_null (getf !@ result_ptr EntityStruct.node) then None else Some (wrap_gpr_node (context node) (!@ result_ptr))

  let children_opt node =
    let node_c_value = unwrap_gpr_node (node) in
    let context = context node in
    let c_value_ptr = allocate_n GprNodeArrayStruct.c_type ~count:1 in
    let _ : int =
      CFunctions.gpr_node_children
        (addr node_c_value)
        (c_value_ptr)
    in
    let c_value = !@(!@(c_value_ptr)) in
    let length = getf c_value GprNodeArrayStruct.n in
    let items = c_value @. GprNodeArrayStruct.items in
    let f i =
      let fresh = allocate EntityStruct.c_type !@(items +@ i) in
      if is_null (getf !@ fresh EntityStruct.node) then None else Some (wrap_gpr_node context (!@ fresh))
    in
    let result = List.init length f in
    GprNodeArrayStruct.dec_ref (!@ c_value_ptr);
    result

  let iter_fields f node =
    children_opt (node :> gpr_node)
    |> List.iter (function None -> () | Some node -> f node)

  let fold_fields f acc node =
    children_opt (node :> gpr_node)
    |> List.fold_left (fun x -> function None -> x | Some node -> f x node) acc

  let exists_fields p node =
    children_opt (node :> gpr_node)
    |> List.exists (function | None -> false | Some node -> p node)

  let for_all_fields p node =
    children_opt (node :> gpr_node)
    |> List.for_all (function | None -> true | Some node -> p node)

  let fold f acc node =
    (* Use an auxiliary function here to have a better type for the function *)
    let rec aux acc node = fold_fields aux (f acc node) node in
    aux acc (node :> gpr_node)

  let iter f node =
    (* Use an auxiliary function here to have a better type for the function *)
    let rec aux node = f node; iter_fields aux node in
    aux (node :> gpr_node)

  let filter p node =
    fold (fun acc node -> if p node then node :: acc else acc) [] node
    |> List.rev

  let exists p node =
    (* Use an auxiliary function here to have a better type for the function *)
    let rec aux node =
      p node || exists_fields aux node in aux (node :> gpr_node)

  let for_all p node =
    (* Use an auxiliary function here to have a better type for the function *)
    let rec aux node = p node && for_all_fields aux node in
    aux (node :> gpr_node)

  let lookup_with_kind :
    type a. a node -> [< gpr_node] -> Sloc.t -> a option =
    fun node_type node sloc ->
      let lookup_res = lookup node sloc in
      let rec aux : a node -> [< gpr_node] -> a option =
        fun node_type node ->
        match node_type, node with
        | GprNode
          , (#gpr_node as node) ->
          Some node
        | AdaPreludeNode
          , (#ada_prelude_node as node) ->
          Some node
        | AdaAccessSubp
          , (#ada_access_subp as node) ->
          Some node
        | AdaContextClause
          , (#ada_context_clause as node) ->
          Some node
        | AdaPragma
          , (#ada_pragma as node) ->
          Some node
        | AdaUse
          , (#ada_use as node) ->
          Some node
        | AdaWith
          , (#ada_with as node) ->
          Some node
        | AdaEntityKind
          , (#ada_entity_kind as node) ->
          Some node
        | AdaEntityKindFunction
          , (#ada_entity_kind_function as node) ->
          Some node
        | AdaEntityKindPackage
          , (#ada_entity_kind_package as node) ->
          Some node
        | AdaEntityKindProcedure
          , (#ada_entity_kind_procedure as node) ->
          Some node
        | AdaGeneric
          , (#ada_generic as node) ->
          Some node
        | AdaLibraryItem
          , (#ada_library_item as node) ->
          Some node
        | AdaMain
          , (#ada_main as node) ->
          Some node
        | AdaPkg
          , (#ada_pkg as node) ->
          Some node
        | AdaPkgBody
          , (#ada_pkg_body as node) ->
          Some node
        | AdaSubp
          , (#ada_subp as node) ->
          Some node
        | AdaPrelude
          , (#ada_prelude as node) ->
          Some node
        | AdaSeparate
          , (#ada_separate as node) ->
          Some node
        | AdaSkip
          , (#ada_skip as node) ->
          Some node
        | AdaWithFormal
          , (#ada_with_formal as node) ->
          Some node
        | AllQualifier
          , (#all_qualifier as node) ->
          Some node
        | AllQualifierAbsent
          , (#all_qualifier_absent as node) ->
          Some node
        | AllQualifierPresent
          , (#all_qualifier_present as node) ->
          Some node
        | AttributeDecl
          , (#attribute_decl as node) ->
          Some node
        | AttributeReference
          , (#attribute_reference as node) ->
          Some node
        | BaseList
          , (#base_list as node) ->
          Some node
        | AdaContextClauseList
          , (#ada_context_clause_list as node) ->
          Some node
        | AdaPreludeNodeList
          , (#ada_prelude_node_list as node) ->
          Some node
        | AdaSkipList
          , (#ada_skip_list as node) ->
          Some node
        | CaseItemList
          , (#case_item_list as node) ->
          Some node
        | ExprList
          , (#expr_list as node) ->
          Some node
        | GprNodeList
          , (#gpr_node_list as node) ->
          Some node
        | Choices
          , (#choices as node) ->
          Some node
        | TermList
          , (#term_list as node) ->
          Some node
        | IdentifierList
          , (#identifier_list as node) ->
          Some node
        | StringLiteralList
          , (#string_literal_list as node) ->
          Some node
        | TermListList
          , (#term_list_list as node) ->
          Some node
        | WithDeclList
          , (#with_decl_list as node) ->
          Some node
        | BuiltinFunctionCall
          , (#builtin_function_call as node) ->
          Some node
        | CaseConstruction
          , (#case_construction as node) ->
          Some node
        | CaseItem
          , (#case_item as node) ->
          Some node
        | CompilationUnit
          , (#compilation_unit as node) ->
          Some node
        | EmptyDecl
          , (#empty_decl as node) ->
          Some node
        | Expr
          , (#expr as node) ->
          Some node
        | Prefix
          , (#prefix as node) ->
          Some node
        | SingleTokNode
          , (#single_tok_node as node) ->
          Some node
        | Identifier
          , (#identifier as node) ->
          Some node
        | NumLiteral
          , (#num_literal as node) ->
          Some node
        | StringLiteral
          , (#string_literal as node) ->
          Some node
        | LimitedNode
          , (#limited_node as node) ->
          Some node
        | LimitedAbsent
          , (#limited_absent as node) ->
          Some node
        | LimitedPresent
          , (#limited_present as node) ->
          Some node
        | OthersDesignator
          , (#others_designator as node) ->
          Some node
        | PackageDecl
          , (#package_decl as node) ->
          Some node
        | PackageExtension
          , (#package_extension as node) ->
          Some node
        | PackageRenaming
          , (#package_renaming as node) ->
          Some node
        | PackageSpec
          , (#package_spec as node) ->
          Some node
        | PrivateNode
          , (#private_node as node) ->
          Some node
        | PrivateAbsent
          , (#private_absent as node) ->
          Some node
        | PrivatePresent
          , (#private_present as node) ->
          Some node
        | Project
          , (#project as node) ->
          Some node
        | ProjectDeclaration
          , (#project_declaration as node) ->
          Some node
        | ProjectExtension
          , (#project_extension as node) ->
          Some node
        | ProjectQualifier
          , (#project_qualifier as node) ->
          Some node
        | ProjectQualifierAbstract
          , (#project_qualifier_abstract as node) ->
          Some node
        | ProjectQualifierAggregate
          , (#project_qualifier_aggregate as node) ->
          Some node
        | ProjectQualifierAggregateLibrary
          , (#project_qualifier_aggregate_library as node) ->
          Some node
        | ProjectQualifierConfiguration
          , (#project_qualifier_configuration as node) ->
          Some node
        | ProjectQualifierLibrary
          , (#project_qualifier_library as node) ->
          Some node
        | ProjectQualifierStandard
          , (#project_qualifier_standard as node) ->
          Some node
        | ProjectReference
          , (#project_reference as node) ->
          Some node
        | StringLiteralAt
          , (#string_literal_at as node) ->
          Some node
        | Terms
          , (#terms as node) ->
          Some node
        | TypeReference
          , (#type_reference as node) ->
          Some node
        | TypedStringDecl
          , (#typed_string_decl as node) ->
          Some node
        | VariableDecl
          , (#variable_decl as node) ->
          Some node
        | VariableReference
          , (#variable_reference as node) ->
          Some node
        | WithDecl
          , (#with_decl as node) ->
          Some node
        | _ -> (match parent node with
                | Some parent_node -> aux node_type parent_node
                | _ -> None) in
    match lookup_res with
      | Some node -> aux node_type node
      | _ -> None

  let as_a : type a. a node -> [< gpr_node ] -> a option =
   fun node_type node ->
    match node_type, (node :> gpr_node) with
    | GprNode
      , (#gpr_node as node) ->
        Some node
    | AdaPreludeNode
      , (#ada_prelude_node as node) ->
        Some node
    | AdaAccessSubp
      , (#ada_access_subp as node) ->
        Some node
    | AdaContextClause
      , (#ada_context_clause as node) ->
        Some node
    | AdaPragma
      , (#ada_pragma as node) ->
        Some node
    | AdaUse
      , (#ada_use as node) ->
        Some node
    | AdaWith
      , (#ada_with as node) ->
        Some node
    | AdaEntityKind
      , (#ada_entity_kind as node) ->
        Some node
    | AdaEntityKindFunction
      , (#ada_entity_kind_function as node) ->
        Some node
    | AdaEntityKindPackage
      , (#ada_entity_kind_package as node) ->
        Some node
    | AdaEntityKindProcedure
      , (#ada_entity_kind_procedure as node) ->
        Some node
    | AdaGeneric
      , (#ada_generic as node) ->
        Some node
    | AdaLibraryItem
      , (#ada_library_item as node) ->
        Some node
    | AdaMain
      , (#ada_main as node) ->
        Some node
    | AdaPkg
      , (#ada_pkg as node) ->
        Some node
    | AdaPkgBody
      , (#ada_pkg_body as node) ->
        Some node
    | AdaSubp
      , (#ada_subp as node) ->
        Some node
    | AdaPrelude
      , (#ada_prelude as node) ->
        Some node
    | AdaSeparate
      , (#ada_separate as node) ->
        Some node
    | AdaSkip
      , (#ada_skip as node) ->
        Some node
    | AdaWithFormal
      , (#ada_with_formal as node) ->
        Some node
    | AllQualifier
      , (#all_qualifier as node) ->
        Some node
    | AllQualifierAbsent
      , (#all_qualifier_absent as node) ->
        Some node
    | AllQualifierPresent
      , (#all_qualifier_present as node) ->
        Some node
    | AttributeDecl
      , (#attribute_decl as node) ->
        Some node
    | AttributeReference
      , (#attribute_reference as node) ->
        Some node
    | BaseList
      , (#base_list as node) ->
        Some node
    | AdaContextClauseList
      , (#ada_context_clause_list as node) ->
        Some node
    | AdaPreludeNodeList
      , (#ada_prelude_node_list as node) ->
        Some node
    | AdaSkipList
      , (#ada_skip_list as node) ->
        Some node
    | CaseItemList
      , (#case_item_list as node) ->
        Some node
    | ExprList
      , (#expr_list as node) ->
        Some node
    | GprNodeList
      , (#gpr_node_list as node) ->
        Some node
    | Choices
      , (#choices as node) ->
        Some node
    | TermList
      , (#term_list as node) ->
        Some node
    | IdentifierList
      , (#identifier_list as node) ->
        Some node
    | StringLiteralList
      , (#string_literal_list as node) ->
        Some node
    | TermListList
      , (#term_list_list as node) ->
        Some node
    | WithDeclList
      , (#with_decl_list as node) ->
        Some node
    | BuiltinFunctionCall
      , (#builtin_function_call as node) ->
        Some node
    | CaseConstruction
      , (#case_construction as node) ->
        Some node
    | CaseItem
      , (#case_item as node) ->
        Some node
    | CompilationUnit
      , (#compilation_unit as node) ->
        Some node
    | EmptyDecl
      , (#empty_decl as node) ->
        Some node
    | Expr
      , (#expr as node) ->
        Some node
    | Prefix
      , (#prefix as node) ->
        Some node
    | SingleTokNode
      , (#single_tok_node as node) ->
        Some node
    | Identifier
      , (#identifier as node) ->
        Some node
    | NumLiteral
      , (#num_literal as node) ->
        Some node
    | StringLiteral
      , (#string_literal as node) ->
        Some node
    | LimitedNode
      , (#limited_node as node) ->
        Some node
    | LimitedAbsent
      , (#limited_absent as node) ->
        Some node
    | LimitedPresent
      , (#limited_present as node) ->
        Some node
    | OthersDesignator
      , (#others_designator as node) ->
        Some node
    | PackageDecl
      , (#package_decl as node) ->
        Some node
    | PackageExtension
      , (#package_extension as node) ->
        Some node
    | PackageRenaming
      , (#package_renaming as node) ->
        Some node
    | PackageSpec
      , (#package_spec as node) ->
        Some node
    | PrivateNode
      , (#private_node as node) ->
        Some node
    | PrivateAbsent
      , (#private_absent as node) ->
        Some node
    | PrivatePresent
      , (#private_present as node) ->
        Some node
    | Project
      , (#project as node) ->
        Some node
    | ProjectDeclaration
      , (#project_declaration as node) ->
        Some node
    | ProjectExtension
      , (#project_extension as node) ->
        Some node
    | ProjectQualifier
      , (#project_qualifier as node) ->
        Some node
    | ProjectQualifierAbstract
      , (#project_qualifier_abstract as node) ->
        Some node
    | ProjectQualifierAggregate
      , (#project_qualifier_aggregate as node) ->
        Some node
    | ProjectQualifierAggregateLibrary
      , (#project_qualifier_aggregate_library as node) ->
        Some node
    | ProjectQualifierConfiguration
      , (#project_qualifier_configuration as node) ->
        Some node
    | ProjectQualifierLibrary
      , (#project_qualifier_library as node) ->
        Some node
    | ProjectQualifierStandard
      , (#project_qualifier_standard as node) ->
        Some node
    | ProjectReference
      , (#project_reference as node) ->
        Some node
    | StringLiteralAt
      , (#string_literal_at as node) ->
        Some node
    | Terms
      , (#terms as node) ->
        Some node
    | TypeReference
      , (#type_reference as node) ->
        Some node
    | TypedStringDecl
      , (#typed_string_decl as node) ->
        Some node
    | VariableDecl
      , (#variable_decl as node) ->
        Some node
    | VariableReference
      , (#variable_reference as node) ->
        Some node
    | WithDecl
      , (#with_decl as node) ->
        Some node
    | _ ->
        None

  let find : type a. a node ->  [< gpr_node ] -> a =
    fun node_type node ->
      let exception Found of a in
      let aux node =
        match node_type, node with
        | GprNode
          , (#gpr_node as node) ->
            raise (Found node)
        | AdaPreludeNode
          , (#ada_prelude_node as node) ->
            raise (Found node)
        | AdaAccessSubp
          , (#ada_access_subp as node) ->
            raise (Found node)
        | AdaContextClause
          , (#ada_context_clause as node) ->
            raise (Found node)
        | AdaPragma
          , (#ada_pragma as node) ->
            raise (Found node)
        | AdaUse
          , (#ada_use as node) ->
            raise (Found node)
        | AdaWith
          , (#ada_with as node) ->
            raise (Found node)
        | AdaEntityKind
          , (#ada_entity_kind as node) ->
            raise (Found node)
        | AdaEntityKindFunction
          , (#ada_entity_kind_function as node) ->
            raise (Found node)
        | AdaEntityKindPackage
          , (#ada_entity_kind_package as node) ->
            raise (Found node)
        | AdaEntityKindProcedure
          , (#ada_entity_kind_procedure as node) ->
            raise (Found node)
        | AdaGeneric
          , (#ada_generic as node) ->
            raise (Found node)
        | AdaLibraryItem
          , (#ada_library_item as node) ->
            raise (Found node)
        | AdaMain
          , (#ada_main as node) ->
            raise (Found node)
        | AdaPkg
          , (#ada_pkg as node) ->
            raise (Found node)
        | AdaPkgBody
          , (#ada_pkg_body as node) ->
            raise (Found node)
        | AdaSubp
          , (#ada_subp as node) ->
            raise (Found node)
        | AdaPrelude
          , (#ada_prelude as node) ->
            raise (Found node)
        | AdaSeparate
          , (#ada_separate as node) ->
            raise (Found node)
        | AdaSkip
          , (#ada_skip as node) ->
            raise (Found node)
        | AdaWithFormal
          , (#ada_with_formal as node) ->
            raise (Found node)
        | AllQualifier
          , (#all_qualifier as node) ->
            raise (Found node)
        | AllQualifierAbsent
          , (#all_qualifier_absent as node) ->
            raise (Found node)
        | AllQualifierPresent
          , (#all_qualifier_present as node) ->
            raise (Found node)
        | AttributeDecl
          , (#attribute_decl as node) ->
            raise (Found node)
        | AttributeReference
          , (#attribute_reference as node) ->
            raise (Found node)
        | BaseList
          , (#base_list as node) ->
            raise (Found node)
        | AdaContextClauseList
          , (#ada_context_clause_list as node) ->
            raise (Found node)
        | AdaPreludeNodeList
          , (#ada_prelude_node_list as node) ->
            raise (Found node)
        | AdaSkipList
          , (#ada_skip_list as node) ->
            raise (Found node)
        | CaseItemList
          , (#case_item_list as node) ->
            raise (Found node)
        | ExprList
          , (#expr_list as node) ->
            raise (Found node)
        | GprNodeList
          , (#gpr_node_list as node) ->
            raise (Found node)
        | Choices
          , (#choices as node) ->
            raise (Found node)
        | TermList
          , (#term_list as node) ->
            raise (Found node)
        | IdentifierList
          , (#identifier_list as node) ->
            raise (Found node)
        | StringLiteralList
          , (#string_literal_list as node) ->
            raise (Found node)
        | TermListList
          , (#term_list_list as node) ->
            raise (Found node)
        | WithDeclList
          , (#with_decl_list as node) ->
            raise (Found node)
        | BuiltinFunctionCall
          , (#builtin_function_call as node) ->
            raise (Found node)
        | CaseConstruction
          , (#case_construction as node) ->
            raise (Found node)
        | CaseItem
          , (#case_item as node) ->
            raise (Found node)
        | CompilationUnit
          , (#compilation_unit as node) ->
            raise (Found node)
        | EmptyDecl
          , (#empty_decl as node) ->
            raise (Found node)
        | Expr
          , (#expr as node) ->
            raise (Found node)
        | Prefix
          , (#prefix as node) ->
            raise (Found node)
        | SingleTokNode
          , (#single_tok_node as node) ->
            raise (Found node)
        | Identifier
          , (#identifier as node) ->
            raise (Found node)
        | NumLiteral
          , (#num_literal as node) ->
            raise (Found node)
        | StringLiteral
          , (#string_literal as node) ->
            raise (Found node)
        | LimitedNode
          , (#limited_node as node) ->
            raise (Found node)
        | LimitedAbsent
          , (#limited_absent as node) ->
            raise (Found node)
        | LimitedPresent
          , (#limited_present as node) ->
            raise (Found node)
        | OthersDesignator
          , (#others_designator as node) ->
            raise (Found node)
        | PackageDecl
          , (#package_decl as node) ->
            raise (Found node)
        | PackageExtension
          , (#package_extension as node) ->
            raise (Found node)
        | PackageRenaming
          , (#package_renaming as node) ->
            raise (Found node)
        | PackageSpec
          , (#package_spec as node) ->
            raise (Found node)
        | PrivateNode
          , (#private_node as node) ->
            raise (Found node)
        | PrivateAbsent
          , (#private_absent as node) ->
            raise (Found node)
        | PrivatePresent
          , (#private_present as node) ->
            raise (Found node)
        | Project
          , (#project as node) ->
            raise (Found node)
        | ProjectDeclaration
          , (#project_declaration as node) ->
            raise (Found node)
        | ProjectExtension
          , (#project_extension as node) ->
            raise (Found node)
        | ProjectQualifier
          , (#project_qualifier as node) ->
            raise (Found node)
        | ProjectQualifierAbstract
          , (#project_qualifier_abstract as node) ->
            raise (Found node)
        | ProjectQualifierAggregate
          , (#project_qualifier_aggregate as node) ->
            raise (Found node)
        | ProjectQualifierAggregateLibrary
          , (#project_qualifier_aggregate_library as node) ->
            raise (Found node)
        | ProjectQualifierConfiguration
          , (#project_qualifier_configuration as node) ->
            raise (Found node)
        | ProjectQualifierLibrary
          , (#project_qualifier_library as node) ->
            raise (Found node)
        | ProjectQualifierStandard
          , (#project_qualifier_standard as node) ->
            raise (Found node)
        | ProjectReference
          , (#project_reference as node) ->
            raise (Found node)
        | StringLiteralAt
          , (#string_literal_at as node) ->
            raise (Found node)
        | Terms
          , (#terms as node) ->
            raise (Found node)
        | TypeReference
          , (#type_reference as node) ->
            raise (Found node)
        | TypedStringDecl
          , (#typed_string_decl as node) ->
            raise (Found node)
        | VariableDecl
          , (#variable_decl as node) ->
            raise (Found node)
        | VariableReference
          , (#variable_reference as node) ->
            raise (Found node)
        | WithDecl
          , (#with_decl as node) ->
            raise (Found node)
        | _ ->
          ()
      in
      try
        iter aux node;
        raise Not_found
      with (Found node) -> node



  let findall : type a. a node ->  [< gpr_node ] -> a list =
    fun node_type node ->
      let aux : a list -> [< gpr_node ] -> a list =
       fun acc node ->
        match node_type, node with
        | GprNode
          , (#gpr_node as node) ->
            node :: acc
        | AdaPreludeNode
          , (#ada_prelude_node as node) ->
            node :: acc
        | AdaAccessSubp
          , (#ada_access_subp as node) ->
            node :: acc
        | AdaContextClause
          , (#ada_context_clause as node) ->
            node :: acc
        | AdaPragma
          , (#ada_pragma as node) ->
            node :: acc
        | AdaUse
          , (#ada_use as node) ->
            node :: acc
        | AdaWith
          , (#ada_with as node) ->
            node :: acc
        | AdaEntityKind
          , (#ada_entity_kind as node) ->
            node :: acc
        | AdaEntityKindFunction
          , (#ada_entity_kind_function as node) ->
            node :: acc
        | AdaEntityKindPackage
          , (#ada_entity_kind_package as node) ->
            node :: acc
        | AdaEntityKindProcedure
          , (#ada_entity_kind_procedure as node) ->
            node :: acc
        | AdaGeneric
          , (#ada_generic as node) ->
            node :: acc
        | AdaLibraryItem
          , (#ada_library_item as node) ->
            node :: acc
        | AdaMain
          , (#ada_main as node) ->
            node :: acc
        | AdaPkg
          , (#ada_pkg as node) ->
            node :: acc
        | AdaPkgBody
          , (#ada_pkg_body as node) ->
            node :: acc
        | AdaSubp
          , (#ada_subp as node) ->
            node :: acc
        | AdaPrelude
          , (#ada_prelude as node) ->
            node :: acc
        | AdaSeparate
          , (#ada_separate as node) ->
            node :: acc
        | AdaSkip
          , (#ada_skip as node) ->
            node :: acc
        | AdaWithFormal
          , (#ada_with_formal as node) ->
            node :: acc
        | AllQualifier
          , (#all_qualifier as node) ->
            node :: acc
        | AllQualifierAbsent
          , (#all_qualifier_absent as node) ->
            node :: acc
        | AllQualifierPresent
          , (#all_qualifier_present as node) ->
            node :: acc
        | AttributeDecl
          , (#attribute_decl as node) ->
            node :: acc
        | AttributeReference
          , (#attribute_reference as node) ->
            node :: acc
        | BaseList
          , (#base_list as node) ->
            node :: acc
        | AdaContextClauseList
          , (#ada_context_clause_list as node) ->
            node :: acc
        | AdaPreludeNodeList
          , (#ada_prelude_node_list as node) ->
            node :: acc
        | AdaSkipList
          , (#ada_skip_list as node) ->
            node :: acc
        | CaseItemList
          , (#case_item_list as node) ->
            node :: acc
        | ExprList
          , (#expr_list as node) ->
            node :: acc
        | GprNodeList
          , (#gpr_node_list as node) ->
            node :: acc
        | Choices
          , (#choices as node) ->
            node :: acc
        | TermList
          , (#term_list as node) ->
            node :: acc
        | IdentifierList
          , (#identifier_list as node) ->
            node :: acc
        | StringLiteralList
          , (#string_literal_list as node) ->
            node :: acc
        | TermListList
          , (#term_list_list as node) ->
            node :: acc
        | WithDeclList
          , (#with_decl_list as node) ->
            node :: acc
        | BuiltinFunctionCall
          , (#builtin_function_call as node) ->
            node :: acc
        | CaseConstruction
          , (#case_construction as node) ->
            node :: acc
        | CaseItem
          , (#case_item as node) ->
            node :: acc
        | CompilationUnit
          , (#compilation_unit as node) ->
            node :: acc
        | EmptyDecl
          , (#empty_decl as node) ->
            node :: acc
        | Expr
          , (#expr as node) ->
            node :: acc
        | Prefix
          , (#prefix as node) ->
            node :: acc
        | SingleTokNode
          , (#single_tok_node as node) ->
            node :: acc
        | Identifier
          , (#identifier as node) ->
            node :: acc
        | NumLiteral
          , (#num_literal as node) ->
            node :: acc
        | StringLiteral
          , (#string_literal as node) ->
            node :: acc
        | LimitedNode
          , (#limited_node as node) ->
            node :: acc
        | LimitedAbsent
          , (#limited_absent as node) ->
            node :: acc
        | LimitedPresent
          , (#limited_present as node) ->
            node :: acc
        | OthersDesignator
          , (#others_designator as node) ->
            node :: acc
        | PackageDecl
          , (#package_decl as node) ->
            node :: acc
        | PackageExtension
          , (#package_extension as node) ->
            node :: acc
        | PackageRenaming
          , (#package_renaming as node) ->
            node :: acc
        | PackageSpec
          , (#package_spec as node) ->
            node :: acc
        | PrivateNode
          , (#private_node as node) ->
            node :: acc
        | PrivateAbsent
          , (#private_absent as node) ->
            node :: acc
        | PrivatePresent
          , (#private_present as node) ->
            node :: acc
        | Project
          , (#project as node) ->
            node :: acc
        | ProjectDeclaration
          , (#project_declaration as node) ->
            node :: acc
        | ProjectExtension
          , (#project_extension as node) ->
            node :: acc
        | ProjectQualifier
          , (#project_qualifier as node) ->
            node :: acc
        | ProjectQualifierAbstract
          , (#project_qualifier_abstract as node) ->
            node :: acc
        | ProjectQualifierAggregate
          , (#project_qualifier_aggregate as node) ->
            node :: acc
        | ProjectQualifierAggregateLibrary
          , (#project_qualifier_aggregate_library as node) ->
            node :: acc
        | ProjectQualifierConfiguration
          , (#project_qualifier_configuration as node) ->
            node :: acc
        | ProjectQualifierLibrary
          , (#project_qualifier_library as node) ->
            node :: acc
        | ProjectQualifierStandard
          , (#project_qualifier_standard as node) ->
            node :: acc
        | ProjectReference
          , (#project_reference as node) ->
            node :: acc
        | StringLiteralAt
          , (#string_literal_at as node) ->
            node :: acc
        | Terms
          , (#terms as node) ->
            node :: acc
        | TypeReference
          , (#type_reference as node) ->
            node :: acc
        | TypedStringDecl
          , (#typed_string_decl as node) ->
            node :: acc
        | VariableDecl
          , (#variable_decl as node) ->
            node :: acc
        | VariableReference
          , (#variable_reference as node) ->
            node :: acc
        | WithDecl
          , (#with_decl as node) ->
            node :: acc
        | _ ->
          acc
      in
      List.rev (fold aux [] node)

  let fields_with_names node =
    let aux i x =
      (Format.sprintf "item_%d" i), x
    in
    match (node :> gpr_node) with
    | `WithDecl value ->
        [
        (try
           ("is_limited"
           , Some (Lazy.force value.f_is_limited
                    :> gpr_node))
        with SyntaxError _ ->
          ("is_limited", None) );
        (try
           ("path_names"
           , Some (Lazy.force value.f_path_names
                    :> gpr_node))
        with SyntaxError _ ->
          ("path_names", None) );
        ]
    | `VariableReference value ->
        [
        (try
           ("variable_name"
           , Some (Lazy.force value.f_variable_name
                    :> gpr_node))
        with SyntaxError _ ->
          ("variable_name", None) );
        ("attribute_ref"
        , (Lazy.force value.f_attribute_ref
           :> gpr_node option));
        ]
    | `VariableDecl value ->
        [
        (try
           ("var_name"
           , Some (Lazy.force value.f_var_name
                    :> gpr_node))
        with SyntaxError _ ->
          ("var_name", None) );
        ("var_type"
        , (Lazy.force value.f_var_type
           :> gpr_node option));
        (try
           ("expr"
           , Some (Lazy.force value.f_expr
                    :> gpr_node))
        with SyntaxError _ ->
          ("expr", None) );
        ]
    | `TypedStringDecl value ->
        [
        (try
           ("type_id"
           , Some (Lazy.force value.f_type_id
                    :> gpr_node))
        with SyntaxError _ ->
          ("type_id", None) );
        (try
           ("string_literals"
           , Some (Lazy.force value.f_string_literals
                    :> gpr_node))
        with SyntaxError _ ->
          ("string_literals", None) );
        ]
    | `TypeReference value ->
        [
        (try
           ("var_type_name"
           , Some (Lazy.force value.f_var_type_name
                    :> gpr_node))
        with SyntaxError _ ->
          ("var_type_name", None) );
        ]
    | `Terms value ->
        [
        (try
           ("terms"
           , Some (Lazy.force value.f_terms
                    :> gpr_node))
        with SyntaxError _ ->
          ("terms", None) );
        ]
    | `StringLiteralAt value ->
        [
        (try
           ("str_lit"
           , Some (Lazy.force value.f_str_lit
                    :> gpr_node))
        with SyntaxError _ ->
          ("str_lit", None) );
        ("at_lit"
        , (Lazy.force value.f_at_lit
           :> gpr_node option));
        ]
    | `ProjectReference value ->
        [
        (try
           ("attr_ref"
           , Some (Lazy.force value.f_attr_ref
                    :> gpr_node))
        with SyntaxError _ ->
          ("attr_ref", None) );
        ]
    | `ProjectQualifierStandard value ->
        [
        ]
    | `ProjectQualifierLibrary value ->
        [
        ]
    | `ProjectQualifierConfiguration value ->
        [
        ]
    | `ProjectQualifierAggregateLibrary value ->
        [
        ]
    | `ProjectQualifierAggregate value ->
        [
        ]
    | `ProjectQualifierAbstract value ->
        [
        ]
    | `ProjectExtension value ->
        [
        (try
           ("is_all"
           , Some (Lazy.force value.f_is_all
                    :> gpr_node))
        with SyntaxError _ ->
          ("is_all", None) );
        (try
           ("path_name"
           , Some (Lazy.force value.f_path_name
                    :> gpr_node))
        with SyntaxError _ ->
          ("path_name", None) );
        ]
    | `ProjectDeclaration value ->
        [
        ("qualifier"
        , (Lazy.force value.f_qualifier
           :> gpr_node option));
        (try
           ("project_name"
           , Some (Lazy.force value.f_project_name
                    :> gpr_node))
        with SyntaxError _ ->
          ("project_name", None) );
        ("extension"
        , (Lazy.force value.f_extension
           :> gpr_node option));
        (try
           ("decls"
           , Some (Lazy.force value.f_decls
                    :> gpr_node))
        with SyntaxError _ ->
          ("decls", None) );
        (try
           ("end_name"
           , Some (Lazy.force value.f_end_name
                    :> gpr_node))
        with SyntaxError _ ->
          ("end_name", None) );
        ]
    | `Project value ->
        [
        (try
           ("context_clauses"
           , Some (Lazy.force value.f_context_clauses
                    :> gpr_node))
        with SyntaxError _ ->
          ("context_clauses", None) );
        (try
           ("project_decl"
           , Some (Lazy.force value.f_project_decl
                    :> gpr_node))
        with SyntaxError _ ->
          ("project_decl", None) );
        ]
    | `PrivatePresent value ->
        [
        ]
    | `PrivateAbsent value ->
        [
        ]
    | `PackageSpec value ->
        [
        ("extension"
        , (Lazy.force value.f_extension
           :> gpr_node option));
        (try
           ("decls"
           , Some (Lazy.force value.f_decls
                    :> gpr_node))
        with SyntaxError _ ->
          ("decls", None) );
        (try
           ("end_name"
           , Some (Lazy.force value.f_end_name
                    :> gpr_node))
        with SyntaxError _ ->
          ("end_name", None) );
        ]
    | `PackageRenaming value ->
        [
        (try
           ("renamed_name"
           , Some (Lazy.force value.f_renamed_name
                    :> gpr_node))
        with SyntaxError _ ->
          ("renamed_name", None) );
        ]
    | `PackageExtension value ->
        [
        (try
           ("extended_name"
           , Some (Lazy.force value.f_extended_name
                    :> gpr_node))
        with SyntaxError _ ->
          ("extended_name", None) );
        ]
    | `PackageDecl value ->
        [
        (try
           ("pkg_name"
           , Some (Lazy.force value.f_pkg_name
                    :> gpr_node))
        with SyntaxError _ ->
          ("pkg_name", None) );
        (try
           ("pkg_spec"
           , Some (Lazy.force value.f_pkg_spec
                    :> gpr_node))
        with SyntaxError _ ->
          ("pkg_spec", None) );
        ]
    | `OthersDesignator value ->
        [
        ]
    | `LimitedPresent value ->
        [
        ]
    | `LimitedAbsent value ->
        [
        ]
    | `StringLiteral value ->
        [
        ]
    | `NumLiteral value ->
        [
        ]
    | `Identifier value ->
        [
        ]
    | `Prefix value ->
        [
        (try
           ("prefix"
           , Some (Lazy.force value.f_prefix
                    :> gpr_node))
        with SyntaxError _ ->
          ("prefix", None) );
        (try
           ("suffix"
           , Some (Lazy.force value.f_suffix
                    :> gpr_node))
        with SyntaxError _ ->
          ("suffix", None) );
        ]
    | `EmptyDecl value ->
        [
        ]
    | `CompilationUnit value ->
        [
        (try
           ("project"
           , Some (Lazy.force value.f_project
                    :> gpr_node))
        with SyntaxError _ ->
          ("project", None) );
        ]
    | `CaseItem value ->
        [
        (try
           ("choice"
           , Some (Lazy.force value.f_choice
                    :> gpr_node))
        with SyntaxError _ ->
          ("choice", None) );
        (try
           ("decls"
           , Some (Lazy.force value.f_decls
                    :> gpr_node))
        with SyntaxError _ ->
          ("decls", None) );
        ]
    | `CaseConstruction value ->
        [
        (try
           ("var_ref"
           , Some (Lazy.force value.f_var_ref
                    :> gpr_node))
        with SyntaxError _ ->
          ("var_ref", None) );
        (try
           ("items"
           , Some (Lazy.force value.f_items
                    :> gpr_node))
        with SyntaxError _ ->
          ("items", None) );
        ]
    | `BuiltinFunctionCall value ->
        [
        (try
           ("function_name"
           , Some (Lazy.force value.f_function_name
                    :> gpr_node))
        with SyntaxError _ ->
          ("function_name", None) );
        (try
           ("parameters"
           , Some (Lazy.force value.f_parameters
                    :> gpr_node))
        with SyntaxError _ ->
          ("parameters", None) );
        ]
    | `WithDeclList value ->
        List.mapi aux (children_opt node)
    | `TermListList value ->
        List.mapi aux (children_opt node)
    | `StringLiteralList value ->
        List.mapi aux (children_opt node)
    | `IdentifierList value ->
        List.mapi aux (children_opt node)
    | `TermList value ->
        List.mapi aux (children_opt node)
    | `Choices value ->
        List.mapi aux (children_opt node)
    | `GprNodeList value ->
        List.mapi aux (children_opt node)
    | `ExprList value ->
        List.mapi aux (children_opt node)
    | `CaseItemList value ->
        List.mapi aux (children_opt node)
    | `AdaSkipList value ->
        List.mapi aux (children_opt node)
    | `AdaPreludeNodeList value ->
        List.mapi aux (children_opt node)
    | `AdaContextClauseList value ->
        List.mapi aux (children_opt node)
    | `AttributeReference value ->
        [
        (try
           ("attribute_name"
           , Some (Lazy.force value.f_attribute_name
                    :> gpr_node))
        with SyntaxError _ ->
          ("attribute_name", None) );
        ("attribute_index"
        , (Lazy.force value.f_attribute_index
           :> gpr_node option));
        ]
    | `AttributeDecl value ->
        [
        (try
           ("attr_name"
           , Some (Lazy.force value.f_attr_name
                    :> gpr_node))
        with SyntaxError _ ->
          ("attr_name", None) );
        ("attr_index"
        , (Lazy.force value.f_attr_index
           :> gpr_node option));
        (try
           ("expr"
           , Some (Lazy.force value.f_expr
                    :> gpr_node))
        with SyntaxError _ ->
          ("expr", None) );
        ]
    | `AllQualifierPresent value ->
        [
        ]
    | `AllQualifierAbsent value ->
        [
        ]
    | `AdaWithFormal value ->
        [
        (try
           ("kind"
           , Some (Lazy.force value.f_kind
                    :> gpr_node))
        with SyntaxError _ ->
          ("kind", None) );
        (try
           ("skips"
           , Some (Lazy.force value.f_skips
                    :> gpr_node))
        with SyntaxError _ ->
          ("skips", None) );
        ]
    | `AdaSkip value ->
        [
        ]
    | `AdaSeparate value ->
        [
        (try
           ("parent_name"
           , Some (Lazy.force value.f_parent_name
                    :> gpr_node))
        with SyntaxError _ ->
          ("parent_name", None) );
        ]
    | `AdaPrelude value ->
        [
        (try
           ("context_clauses"
           , Some (Lazy.force value.f_context_clauses
                    :> gpr_node))
        with SyntaxError _ ->
          ("context_clauses", None) );
        (try
           ("library_item"
           , Some (Lazy.force value.f_library_item
                    :> gpr_node))
        with SyntaxError _ ->
          ("library_item", None) );
        ]
    | `AdaSubp value ->
        [
        (try
           ("subp_kind"
           , Some (Lazy.force value.f_subp_kind
                    :> gpr_node))
        with SyntaxError _ ->
          ("subp_kind", None) );
        ]
    | `AdaPkgBody value ->
        [
        ]
    | `AdaPkg value ->
        [
        (try
           ("has_private"
           , Some (Lazy.force value.f_has_private
                    :> gpr_node))
        with SyntaxError _ ->
          ("has_private", None) );
        ]
    | `AdaLibraryItem value ->
        [
        ("generic_stub"
        , (Lazy.force value.f_generic_stub
           :> gpr_node option));
        ("separate"
        , (Lazy.force value.f_separate
           :> gpr_node option));
        (try
           ("main"
           , Some (Lazy.force value.f_main
                    :> gpr_node))
        with SyntaxError _ ->
          ("main", None) );
        ]
    | `AdaGeneric value ->
        [
        (try
           ("skips"
           , Some (Lazy.force value.f_skips
                    :> gpr_node))
        with SyntaxError _ ->
          ("skips", None) );
        ]
    | `AdaEntityKindProcedure value ->
        [
        ]
    | `AdaEntityKindPackage value ->
        [
        ]
    | `AdaEntityKindFunction value ->
        [
        ]
    | `AdaWith value ->
        [
        (try
           ("has_limited"
           , Some (Lazy.force value.f_has_limited
                    :> gpr_node))
        with SyntaxError _ ->
          ("has_limited", None) );
        (try
           ("has_private"
           , Some (Lazy.force value.f_has_private
                    :> gpr_node))
        with SyntaxError _ ->
          ("has_private", None) );
        (try
           ("packages"
           , Some (Lazy.force value.f_packages
                    :> gpr_node))
        with SyntaxError _ ->
          ("packages", None) );
        ]
    | `AdaUse value ->
        [
        (try
           ("skips"
           , Some (Lazy.force value.f_skips
                    :> gpr_node))
        with SyntaxError _ ->
          ("skips", None) );
        ]
    | `AdaPragma value ->
        [
        (try
           ("skips"
           , Some (Lazy.force value.f_skips
                    :> gpr_node))
        with SyntaxError _ ->
          ("skips", None) );
        ]
    | `AdaAccessSubp value ->
        [
        (try
           ("subp_kind"
           , Some (Lazy.force value.f_subp_kind
                    :> gpr_node))
        with SyntaxError _ ->
          ("subp_kind", None) );
        (try
           ("skips"
           , Some (Lazy.force value.f_skips
                    :> gpr_node))
        with SyntaxError _ ->
          ("skips", None) );
        ]

  let rec pp_tree fmt node =
    let rec pp_node_field fmt (name, node) =
      match node with
      | Some node ->
          Format.fprintf fmt "@[<v 2>%s:@ %a@]" name pp_node node
      | None ->
          Format.fprintf fmt "@[<v 2>%s: None@]" name
    and pp_node_fields fmt node =
      let name_field_list = fields_with_names node in
      match name_field_list with
      | [] ->
          ()
      | l ->
          Format.fprintf fmt "@ @[<v>%a@]"
            (Format.pp_print_list pp_node_field) l
    and pp_node fmt node =
      let repr = entity_image node in
      let len = String.length repr in
      let erepr = String.sub repr 1 (len - 2) in
      Format.fprintf fmt "@[<v 2>%s%s%a@]"
        erepr
        (if is_token_node node then (": " ^ (text node)) else "")
        pp_node_fields node
    in
    let default = Format.pp_get_formatter_out_functions fmt () in
    let out_indent n =
      let the_end = n in
      let rec make n =
        if n = the_end then ""
        else (if n mod 4 = 2 then "|" else " ") ^ make (n + 1)
      in
      default.out_string (make 0) 0 n
    in
    Format.pp_set_formatter_out_functions fmt {default with out_indent} ;
    Format.fprintf fmt "%a%!" pp_node (node :> gpr_node);
    Format.pp_set_formatter_out_functions fmt default


end

