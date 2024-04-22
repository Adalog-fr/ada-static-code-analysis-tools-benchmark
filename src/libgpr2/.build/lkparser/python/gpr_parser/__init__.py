
"""
Python binding of the GprParser API.

Please consider all exported entities whose names that start with an underscore
("_") as internal implementation details. They are not meant to be used
directly.
"""







from __future__ import annotations





import argparse
import collections
import ctypes
import io
import json
import os
import sys
from typing import (
    Any, AnyStr, Callable, ClassVar, Dict, Generic, IO, Iterator, List,
    Optional as Opt, TYPE_CHECKING, Tuple, Type, TypeVar, Union
)
import weakref


#
# Low-level binding - First part
#

_so_ext = {
    'win32':  'dll',
    'darwin': 'dylib',
}.get(sys.platform, 'so')

# Loading the shared library here is quite involved as we want to support
# Python packages that embed all the required shared libraries: if we can
# find the shared library in the package directory, import it from there
# directly.

# Directory that contains this __init__.py module
_self_path = os.path.dirname(os.path.abspath(__file__))

# Base and full names for the shared library to load. Full name assumes the
# shared lib is in the package directory.
_c_lib_name = 'libgpr_parser.{}'.format(_so_ext)
_c_lib_path = os.path.join(_self_path, _c_lib_name)

# If we can find the shared lirbray in the package directory, load it from
# here, otherwise let the dynamic loader find it in the environment. On
# Windows, there is no RPATH trick, so we need to temporarily alter the PATH
# environment variable in order to import the whole closure of DLLs.
_old_env_path = None
if os.path.exists(_c_lib_path):
    if sys.platform == 'win32':
        _old_env_path = os.environ['PATH']
        os.environ['PATH'] = '{}{}{}'.format(_self_path, os.path.pathsep,
                                             os.environ['PATH'])
else:
    _c_lib_path = _c_lib_name

# Finally load the library
_c_lib = ctypes.cdll.LoadLibrary(_c_lib_path)

# Restore the PATH environment variable if we altered it
if _old_env_path is not None:
    os.environ['PATH'] = _old_env_path


def _import_func(name, argtypes, restype, exc_wrap=True):
    """
    Import "name" from the C library, set its arguments/return types and return
    the binding.

    :param str name: Name of the symbol for the function to import.
    :param list[ctypes._CData] argtypes: Types for function argruments.
    :param None|ctypes._CData restype: Function return type, or None if it
        does not return anything.
    :param bool exc_wrap: If True, wrap the returned function to check for
      exceptions.
    """
    func = getattr(_c_lib, name)
    func.argtypes = argtypes
    func.restype = restype

    def check_argcount(args, kwargs):
        argcount = len(args) + len(kwargs)
        if argcount != len(argtypes):
            raise TypeError(
                '{} takes {} positional arguments but {} was given'
                .format(name, len(argtypes), argcount))

    # Wrapper for "func" that raises a NativeException in case of internal
    # error.

    if exc_wrap:
        def wrapper(*args, **kwargs):
            check_argcount(args, kwargs)
            result = func(*args, **kwargs)
            exc = _get_last_exception()
            if exc:
                raise exc.contents._wrap()
            return result
    else:
        def wrapper(*args, **kwargs):
            check_argcount(args, kwargs)
            return func(*args, **kwargs)

    return wrapper


class _Exception(ctypes.Structure):
    _fields_ = [('kind', ctypes.c_int),
                ('information', ctypes.c_char_p)]

    def _wrap(self):
        # Turn information into native strings, i.e. decode bytes.  These
        # strings are only informative, so do not raise an error if decoding
        # fails: do best effort decoding instead to be as helpful as possible.
        info = self.information.decode(errors='replace')
        return _exception_kind_to_type[self.kind](info)


def _type_fullname(t: type) -> str:
    """
    Return the fully qualified name for the given `t` type.
    """
    name = t.__name__
    module = t.__module__
    return (name
            if module in (None, object.__class__.__module__) else
            '{}.{}'.format(module, name))


def _raise_type_error(expected_type_name: str, actual_value: Any) -> Any:
    raise TypeError('{} instance expected, got {} instead'.format(
        expected_type_name, _type_fullname(type(actual_value))
    ))


_get_last_exception = _import_func(
   'gpr_get_last_exception',
   [], ctypes.POINTER(_Exception),
   exc_wrap=False
)


def _hashable_c_pointer(pointed_type=None):
    """
    Create a "pointer to `pointed_type` type and make it hashable.

    :param pointed_type: ctypes type class. If left to `None`, we return a
        subclass of `ctypes.c_void_p`.
    :rtype: ctypes.POINTER
    """

    if pointed_type is None:
        class _c_type(ctypes.c_void_p):
            @property
            def _pointer_value(self):
                return self.value or 0
    else:
        @property
        def _pointer_value(self):
            return ctypes.cast(self, ctypes.c_void_p).value or 0

        _c_type = ctypes.POINTER(pointed_type)
        _c_type._pointer_value = _pointer_value

    def __hash__(self):
        return self._pointer_value

    def __eq__(self, other):
        return self._pointer_value == other._pointer_value

    def __ne__(self, other):
        return not (self == other)

    _c_type.__hash__ = __hash__
    _c_type.__eq__ = __eq__
    _c_type.__ne__ = __ne__
    return _c_type


def _unwrap_filename(filename: Opt[AnyStr]) -> Opt[bytes]:
    """Turn filename into a suitable C value for filenames."""
    if filename is None:
        return None
    elif isinstance(filename, str):
        return filename.encode()
    elif not isinstance(filename, bytes):
        raise ValueError(f"invalid filename: {filename}")
    else:
        return filename


def _unwrap_charset(charset: Opt[AnyStr]) -> Opt[bytes]:
    """Turn charset into a suitable C value for charsets."""
    if charset is None:
        return None
    elif isinstance(charset, str):
        return charset.encode()
    elif not isinstance(charset, bytes):
        raise ValueError(f"invalid charset: {charset}")
    else:
        return charset


class _text(ctypes.Structure):
    """
    C value for unicode strings. This object is the owner of the underlying
    buffer, so the string will be deallocated when ``self`` is destroyed.

    ``_unwrap`` takes a string/unicode object and returns a ``_text`` instance,
    while ``_wrap`` retuns an unicode instance.
    """
    # The chars field really is a uint32_t* but considering it as a char* here
    # is more convenient for conversion in this binding layer. On the other
    # side, we have to be careful about converting the length when retrieving
    # the chars.
    _fields_ = [("chars", ctypes.POINTER(ctypes.c_char)),
                ("length", ctypes.c_size_t),
                ("is_allocated", ctypes.c_int),]

    encoding = 'utf-32le' if sys.byteorder == 'little' else 'utf-32be'

    # Instances can hold buffers that they own. In this case, the buffer must
    # be deallocated when the instance is destroyed. Thus instances will hold
    # a "text_buffer" attribute that will be automatically destroyed.
    text_buffer = None

    @classmethod
    def _create_buffer(cls, value: AnyStr) -> Tuple[Any, int]:
        """
        Turn `value` into the corresponding UTF-32 string buffer.

        Return both the string buffer and the number of codepoints it contains
        (not the number of bytes!).
        """
        string = cls.cast(value)
        buf = ctypes.create_string_buffer(string.encode(cls.encoding))
        return (buf, len(string))

    @classmethod
    def _decode_buffer(cls, buf: Any, length: int) -> str:
        """
        Decode the UTF-32 string in `buf`.

        :param buf: String buffer (of type `POINTER(c_char_p)`) to decode.
        :param length: Number of codepoints in `buf` (not the number of
            bytes!).
        """
        if length > 0:
            # `length` tells how much UTF-32 chars there are in `buf` but `buf`
            # is a char* so we have to fetch 4 times more bytes than bytes.
            return buf[:4 * length].decode(cls.encoding)
        else:
            return ""

    @classmethod
    def _unwrap(cls, value: AnyStr) -> _text:
        text_buffer, length = cls._create_buffer(value)
        text_buffer_ptr = ctypes.cast(
            ctypes.pointer(text_buffer),
            ctypes.POINTER(ctypes.c_char)
        )
        result = _text(text_buffer_ptr, length)
        result.text_buffer = text_buffer
        return result

    def _wrap(self) -> str:
        return self._decode_buffer(self.chars, self.length)

    @classmethod
    def cast(cls, value: AnyStr) -> str:
        """
        Try to cast ``value`` into an unicode object. Raise a TypeError, or
        raise a string decoding error when this is not possible.
        """
        if isinstance(value, bytes):
            return value.decode('ascii')
        elif not isinstance(value, str):
            _raise_type_error('text string', value)
        else:
            return value

    def __del__(self) -> None:
        _destroy_text(ctypes.byref(self))


class _symbol_type(ctypes.Structure):
    _fields_ = [('data', ctypes.c_void_p),
                ('bounds', ctypes.c_void_p)]

    @classmethod
    def wrap(cls, c_value: Any) -> str:
        # First extract the text associated to this symbol in "text"
        text = _text()
        _symbol_text(ctypes.byref(c_value), ctypes.byref(text))

        # Then wrap this text
        return text._wrap()

    @classmethod
    def unwrap(cls, py_value: AnyStr, context: Any) -> _symbol_type:
        # First turn the given symbol into a low-level text object
        text = _text._unwrap(py_value)

        # Then convert it to a symbol
        result = cls()
        if not _context_symbol(context, ctypes.byref(text),
                               ctypes.byref(result)):
            raise InvalidSymbolError(py_value)
        return result


class _big_integer:

    class c_type(ctypes.c_void_p):
        pass

    def __init__(self, c_value: Any):
        self.c_value = c_value

    @classmethod
    def unwrap(cls, value: int) -> _big_integer:
        if not isinstance(value, int):
            _raise_type_error('int or long', value)

        text = _text._unwrap(str(value))
        c_value = cls.create(ctypes.byref(text))
        return cls(c_value)

    @classmethod
    def wrap(cls, c_value: Any) -> int:
        helper = cls(c_value)
        text = _text()
        cls.text(helper.c_value, ctypes.byref(text))
        return int(text._wrap())

    def clear(self) -> None:
        self.c_value = None

    def __del__(self) -> None:
        self.decref(self.c_value)
        self.clear()

    create = staticmethod(_import_func(
        'gpr_create_big_integer',
        [ctypes.POINTER(_text)], c_type
    ))
    text = staticmethod(_import_func(
        'gpr_big_integer_text',
        [c_type, ctypes.POINTER(_text)], None
    ))
    decref = staticmethod(_import_func(
        'gpr_big_integer_decref',
        [c_type], None
    ))


class _String:
    """
    Helper to wrap/unwrap string values for properties arguments/return types.
    """

    class c_struct(ctypes.Structure):
        _fields_ = [("length", ctypes.c_int),
                    ("ref_count", ctypes.c_int),

                    # See the "chars" field in the _text structure
                    ("content", ctypes.c_char * 1)]
    c_type = ctypes.POINTER(c_struct)

    __slots__ = ("c_value", )

    def __init__(self, c_value):
        self.c_value = c_value

    def __del__(self):
        self.dec_ref(self.c_value)
        self.c_value = None

    @classmethod
    def unwrap(cls, value: AnyStr) -> _String:
        # Convert "value" into the corresponding UTF-32 string buffer
        buf, length = _text._create_buffer(value)
        return cls(cls.create(buf, length))

    @classmethod
    def wrap(cls, value: Any) -> str:
        struct = value.contents

        # "struct.content" will get a one-byte copy of the actual string
        # because of the hack above to handle variable-length struct field. To
        # get the whole string, compute a pointer to this field fierst.
        content_addr = _field_address(struct, "content")
        content = ctypes.pointer(ctypes.c_char.from_address(content_addr))

        return _text._decode_buffer(content, struct.length)

    create = staticmethod(_import_func(
        'gpr_create_string',
        [ctypes.POINTER(ctypes.c_char), ctypes.c_int], c_type
    ))
    dec_ref = staticmethod(_import_func(
        'gpr_string_dec_ref',
        [c_type], None
    ))


if TYPE_CHECKING:
    _EnumType = TypeVar("_EnumType", bound=_Enum)


class _Enum:

    _name: ClassVar[str]
    """
    Name for this enumeration type.
    """

    _c_to_py: ClassVar[List[str]]
    """
    Mapping from C values to user-level Python values.
    """

    _py_to_c: ClassVar[Dict[str, int]]
    """
    Mapping from user-level Python values to C values.
    """

    @classmethod
    def _unwrap(cls, py_value: str) -> int:
        if not isinstance(py_value, str):
            _raise_type_error('str', py_value)
        try:
            return cls._py_to_c[py_value]
        except KeyError:
            raise ValueError('Invalid {}: {}'.format(cls._name, py_value))

    @classmethod
    def _wrap(cls: Type[_EnumType], c_value: Any) -> _EnumType:
        if isinstance(c_value, ctypes.c_int):
            c_value = c_value.value
        return cls._c_to_py[c_value]


class AnalysisUnitKind(_Enum):
    """
    Specify a kind of analysis unit. Specification units provide an interface
    to the outer world while body units provide an implementation for the
    corresponding interface.
    """

    unit_specification = 'unit_specification'
    unit_body = 'unit_body'

    _name = 'AnalysisUnitKind'
    _c_to_py = [
        unit_specification, unit_body]
    _py_to_c = {name: index for index, name in enumerate(_c_to_py)}
class LookupKind(_Enum):
    """

    """

    recursive = 'recursive'
    flat = 'flat'
    minimal = 'minimal'

    _name = 'LookupKind'
    _c_to_py = [
        recursive, flat, minimal]
    _py_to_c = {name: index for index, name in enumerate(_c_to_py)}
class DesignatedEnvKind(_Enum):
    """
    Discriminant for DesignatedEnv structures.
    """

    none = 'none'
    current_env = 'current_env'
    named_env = 'named_env'
    direct_env = 'direct_env'

    _name = 'DesignatedEnvKind'
    _c_to_py = [
        none, current_env, named_env, direct_env]
    _py_to_c = {name: index for index, name in enumerate(_c_to_py)}
class GrammarRule(_Enum):
    """
    Gramar rule to use for parsing.
    """

    project_qualifier_rule = 'project_qualifier_rule'
    project_extension_rule = 'project_extension_rule'
    project_declaration_rule = 'project_declaration_rule'
    project_rule = 'project_rule'
    declarative_items_rule = 'declarative_items_rule'
    declarative_item_rule = 'declarative_item_rule'
    simple_declarative_items_rule = 'simple_declarative_items_rule'
    simple_declarative_item_rule = 'simple_declarative_item_rule'
    variable_decl_rule = 'variable_decl_rule'
    attribute_decl_rule = 'attribute_decl_rule'
    associative_array_index_rule = 'associative_array_index_rule'
    package_decl_rule = 'package_decl_rule'
    package_renaming_rule = 'package_renaming_rule'
    package_extension_rule = 'package_extension_rule'
    package_spec_rule = 'package_spec_rule'
    empty_declaration_rule = 'empty_declaration_rule'
    case_construction_rule = 'case_construction_rule'
    case_item_rule = 'case_item_rule'
    others_designator_rule = 'others_designator_rule'
    choice_rule = 'choice_rule'
    discrete_choice_list_rule = 'discrete_choice_list_rule'
    with_decl_rule = 'with_decl_rule'
    context_clauses_rule = 'context_clauses_rule'
    ada_with_clause_rule = 'ada_with_clause_rule'
    ada_context_rule = 'ada_context_rule'
    ada_context_item_rule = 'ada_context_item_rule'
    ada_context_skip_rule = 'ada_context_skip_rule'
    ada_use_clause_rule = 'ada_use_clause_rule'
    ada_pragma_rule = 'ada_pragma_rule'
    ada_subp_kind_rule = 'ada_subp_kind_rule'
    ada_pkg_kind_rule = 'ada_pkg_kind_rule'
    ada_library_item_rule = 'ada_library_item_rule'
    ada_prelude_rule = 'ada_prelude_rule'
    typed_string_decl_rule = 'typed_string_decl_rule'
    identifier_rule = 'identifier_rule'
    string_literal_rule = 'string_literal_rule'
    num_literal_rule = 'num_literal_rule'
    static_name_rule = 'static_name_rule'
    attribute_reference_rule = 'attribute_reference_rule'
    variable_reference_rule = 'variable_reference_rule'
    type_reference_rule = 'type_reference_rule'
    builtin_function_call_rule = 'builtin_function_call_rule'
    expression_rule = 'expression_rule'
    expression_list_rule = 'expression_list_rule'
    string_literal_at_rule = 'string_literal_at_rule'
    project_reference_rule = 'project_reference_rule'
    term_rule = 'term_rule'
    compilation_unit_rule = 'compilation_unit_rule'

    _name = 'GrammarRule'
    _c_to_py = [
        project_qualifier_rule, project_extension_rule, project_declaration_rule, project_rule, declarative_items_rule, declarative_item_rule, simple_declarative_items_rule, simple_declarative_item_rule, variable_decl_rule, attribute_decl_rule, associative_array_index_rule, package_decl_rule, package_renaming_rule, package_extension_rule, package_spec_rule, empty_declaration_rule, case_construction_rule, case_item_rule, others_designator_rule, choice_rule, discrete_choice_list_rule, with_decl_rule, context_clauses_rule, ada_with_clause_rule, ada_context_rule, ada_context_item_rule, ada_context_skip_rule, ada_use_clause_rule, ada_pragma_rule, ada_subp_kind_rule, ada_pkg_kind_rule, ada_library_item_rule, ada_prelude_rule, typed_string_decl_rule, identifier_rule, string_literal_rule, num_literal_rule, static_name_rule, attribute_reference_rule, variable_reference_rule, type_reference_rule, builtin_function_call_rule, expression_rule, expression_list_rule, string_literal_at_rule, project_reference_rule, term_rule, compilation_unit_rule]
    _py_to_c = {name: index for index, name in enumerate(_c_to_py)}


default_grammar_rule = GrammarRule.compilation_unit_rule


_file_reader = _hashable_c_pointer()
_unit_provider = _hashable_c_pointer()
_event_handler = _hashable_c_pointer()


def _canonicalize_buffer(buffer: AnyStr,
                         charset: Opt[bytes]) -> Tuple[bytes, Opt[bytes]]:
    """Canonicalize source buffers to be bytes buffers."""
    if isinstance(buffer, str):
        if charset:
            raise TypeError('`charset` must be null when the buffer is'
                            ' Unicode')
        return (buffer.encode('utf-8'), b'utf-8')
    elif not isinstance(buffer, bytes):
        raise TypeError('`buffer` must be a string')
    else:
        return (buffer, charset)


#
# High-level binding
#


class FileReadError(Exception):
    """
    Subprograms may raise this when they cannot open a source file. Note that
    this does *not* concern analysis unit getters, which create diagnostic
    vectors for such errors.
    """
    pass
class BadTypeError(Exception):
    """
    Raised when introspection functions (``GprParser.Introspection``) are
    provided mismatching types/values.
    """
    pass
class OutOfBoundsError(Exception):
    """
    Raised when introspection functions (``GprParser.Introspection``) are
    passed an out of bounds index.
    """
    pass
class InvalidInput(Exception):
    """
    Raised by lexing functions (``GprParser.Lexer``) when the input contains an
    invalid byte sequence.
    """
    pass
class InvalidSymbolError(Exception):
    """
    Exception raise when an invalid symbol is passed to a subprogram.
    """
    pass
class InvalidUnitNameError(Exception):
    """
    Raised when an invalid unit name is provided.
    """
    pass
class NativeException(Exception):
    """
    Exception raised in language bindings when the underlying C API reports an
    unexpected error that occurred in the library.

    This kind of exception is raised for internal errors: they should never
    happen in normal situations and if they are raised at some point, it means
    the library state is potentially corrupted.

    Nevertheless, the library does its best not to crash the program,
    materializing internal errors using this kind of exception.
    """
    pass
class PreconditionFailure(Exception):
    """
    Exception raised when an API is called while its preconditions are not
    satisfied.
    """
    pass
class PropertyError(Exception):
    """
    Exception that is raised when an error occurs while evaluating any AST node
    method whose name starts with ``p_``. This is the only exceptions that such
    functions can raise.
    """
    pass
class TemplateArgsError(Exception):
    """
    Exception raised when the provided arguments for a template don't match
    what the template expects.
    """
    pass
class TemplateFormatError(Exception):
    """
    Exception raised when a template has an invalid syntax, such as badly
    formatted placeholders.
    """
    pass
class TemplateInstantiationError(Exception):
    """
    Exception raised when the instantiation of a template cannot be parsed.
    """
    pass
class StaleReferenceError(Exception):
    """
    Exception raised while trying to access data that was deallocated. This
    happens when one tries to use a node whose unit has been reparsed, for
    instance.
    """
    pass
class SyntaxError(Exception):
    """
    Subprograms may raise this when they try to parse invalid syntax. Note that
    this does *not* concern analysis unit getters, which create diagnostic
    vectors for such errors.
    """
    pass
class UnknownCharset(Exception):
    """
    Raised by lexing functions (``GprParser.Lexer``) when the input charset is
    not supported.
    """
    pass

_exception_kind_to_type = [
    FileReadError,
    BadTypeError,
    OutOfBoundsError,
    InvalidInput,
    InvalidSymbolError,
    InvalidUnitNameError,
    NativeException,
    PreconditionFailure,
    PropertyError,
    TemplateArgsError,
    TemplateFormatError,
    TemplateInstantiationError,
    StaleReferenceError,
    SyntaxError,
    UnknownCharset,
]





class AnalysisContext:
    """
    This type represents a context for all source analysis. This is the first
    type you need to create to use GprParser. It will contain the results of
    all analysis, and is the main holder for all the data.

    You can create several analysis contexts if you need to, which enables you,
    for example to:

    * analyze several different projects at the same time;

    * analyze different parts of the same projects in parallel.

    In the current design, contexts always keep all of their analysis units
    allocated. If you need to get this memory released, the only option at your
    disposal is to destroy your analysis context instance.
    """

    __slots__ = ('_c_value', '_unit_provider', '_serial_number', '_unit_cache',
                 '__weakref__')

    _context_cache: weakref.WeakValueDictionary[Any, AnalysisContext] = (
        weakref.WeakValueDictionary()
    )
    """
    Cache for analysis context wrappers. Indexed by analysis context addresses,
    which are known to stay valid forever (and re-used).

    Unlike unit and node caches, this one should contain weak references so
    that analysis contexts (and their units/nodes) can be free'd when user code
    does not reference them anymore.
    """

    def __init__(self,
                 charset: Opt[str] = None,
                 file_reader: Opt[FileReader] = None,
                 unit_provider: Opt[UnitProvider] = None,
                 with_trivia: bool = True,
                 tab_stop: int = 8,
                 *,
                 _c_value: Any = None) -> None:
        """
        Create a new analysis context.

        ``Charset`` will be used as a default charset to decode input sources
        in analysis units. Please see ``GNATCOLL.Iconv`` for several supported
        charsets. Be careful: passing an unsupported charset is not guaranteed
        to raise an error here. If no charset is provided, ``"iso-8859-1"`` is
        the default.

        .. TODO: Passing an unsupported charset here is not guaranteed to raise
           an error right here, but this would be really helpful for users.

        When ``With_Trivia`` is true, the parsed analysis units will contain
        trivias.

        If provided, ``File_Reader`` will be used to fetch the contents of
        source files instead of the default, which is to just read it from the
        filesystem and decode it using the regular charset rules. Note that if
        provided, all parsing APIs that provide a buffer are forbidden, and any
        use of the rewriting API with the returned context is rejected.

        If provided, ``Unit_Provider`` will be used to query the file name that
        corresponds to a unit reference during semantic analysis. If it is
        ``None``, the default one is used instead.

        ``Tab_Stop`` is a positive number to describe the effect of tabulation
        characters on the column number in source files.
        """

        # Initialize this field in case we raise an exception during
        # construction, so that the destructor can run later on.
        self._c_value = None

        if _c_value is None:
            _charset = _unwrap_charset(charset)
            if not isinstance(tab_stop, int) or tab_stop < 1:
                raise ValueError(
                    'Invalid tab_stop (positive integer expected)')
            c_file_reader = file_reader._c_value if file_reader else None
            c_unit_provider = unit_provider._c_value if unit_provider else None
            self._c_value = _create_analysis_context(
                _charset,
                c_file_reader,
                c_unit_provider,
                None, # TODO: bind the event handler API to Python
                with_trivia,
                tab_stop
            )
        else:
            self._c_value = _context_incref(_c_value)
        assert self._c_value not in self._context_cache
        self._context_cache[self._c_value] = self

        # Keep a reference to the unit provider so that it is live at least as
        # long as the analysis context is live.
        self._unit_provider = unit_provider

        self._serial_number: Opt[int] = None
        self._unit_cache: Dict[str, AnalysisUnit] = {}
        """
        Cache for AnalysisUnit wrappers, indexed by analysis unit addresses,
        which are known to stay valid as long as the context is alive.
        """

        self._check_unit_cache()

    def __del__(self) -> None:
        if self._c_value:
            _context_decref(self._c_value)

    def __eq__(self, other: Any) -> bool:
        return self._c_value == other._c_value

    def __hash__(self) -> int:
        return hash(self._c_value)

    def get_from_file(self,
                      filename: AnyStr,
                      charset: Opt[str] = None,
                      reparse: bool = False,
                      rule: str = default_grammar_rule) -> AnalysisUnit:
        """
        Create a new analysis unit for ``Filename`` or return the existing one
        if any. If ``Reparse`` is true and the analysis unit already exists,
        reparse it from ``Filename``.

        ``Rule`` controls which grammar rule is used to parse the unit.

        Use ``Charset`` in order to decode the source. If ``Charset`` is empty
        then use the context's default charset.

        If any failure occurs, such as file opening, decoding, lexing or
        parsing failure, return an analysis unit anyway: errors are described
        as diagnostics of the returned analysis unit.
        """
        _filename = _unwrap_filename(filename)
        _charset = _unwrap_charset(charset)
        c_value = _get_analysis_unit_from_file(self._c_value, _filename,
                                               _charset, reparse,
                                               GrammarRule._unwrap(rule))
        return AnalysisUnit._wrap(c_value)

    def get_from_buffer(self,
                        filename: AnyStr,
                        buffer: AnyStr,
                        charset: Opt[str] = None,
                        reparse: bool = False,
                        rule: str = default_grammar_rule) -> AnalysisUnit:
        """
        Create a new analysis unit for ``Filename`` or return the existing one
        if any. Whether the analysis unit already exists or not, (re)parse it
        from the source code in ``Buffer``.

        ``Rule`` controls which grammar rule is used to parse the unit.

        Use ``Charset`` in order to decode the source. If ``Charset`` is empty
        then use the context's default charset.

        If any failure occurs, such as file opening, decoding, lexing or
        parsing failure, return an analysis unit anyway: errors are described
        as diagnostics of the returned analysis unit.
        """
        _filename = _unwrap_filename(filename)
        _charset = _unwrap_charset(charset)
        _buffer, _charset = _canonicalize_buffer(buffer, _charset)
        c_value = _get_analysis_unit_from_buffer(self._c_value, _filename,
                                                 _charset,
                                                 _buffer, len(_buffer),
                                                 GrammarRule._unwrap(rule))
        return AnalysisUnit._wrap(c_value)


    def discard_errors_in_populate_lexical_env(self,
                                               discard: bool) -> None:
        """
        Debug helper. Set whether ``Property_Error`` exceptions raised in
        ``Populate_Lexical_Env`` should be discarded. They are by default.
        """
        _discard_errors_in_populate_lexical_env(self._c_value, bool(discard))

    class _c_struct(ctypes.Structure):
        _fields_ = [('serial_number', ctypes.c_uint64)]
    _c_type = _hashable_c_pointer(_c_struct)

    @classmethod
    def _wrap(cls, c_value):
        try:
            return cls._context_cache[c_value]
        except KeyError:
            return cls(_c_value=c_value)

    def _check_unit_cache(self):
        """
        If this context has been re-used, invalidate its unit cache.
        """
        serial_number = self._c_value.contents.serial_number
        if self._serial_number != serial_number:
            self._unit_cache = {}
            self._serial_number = serial_number


class AnalysisUnit:
    """
    This type represents the analysis of a single file.
    """

    __slots__ = ('_c_value', '_context_link', '_cache_version_number',
                 '_node_cache')

    class TokenIterator:
        """
        Iterator over the tokens in an analysis unit.
        """

        def __init__(self, first: Opt[Token]):
            self.first: Opt[Token] = first

        def __iter__(self) -> AnalysisUnit.TokenIterator:
            return self

        def __next__(self) -> Token:
            if not self.first:
                raise StopIteration()
            result = self.first
            self.first = self.first.next
            return result
        next = __next__

    def __init__(self, context: AnalysisContext, c_value: Any) -> None:
        """
        This constructor is an implementation detail, and is not meant to be
        used directly. Please use AnalysisContext.get_from_* methods to create
        analysis unit instances instead.
        """
        self._c_value = c_value

        # Keep a reference on the owning context so that we keep it alive at
        # least as long as this unit is alive.
        self._context_link = context

        # Store this wrapper in caches for later re-use
        assert c_value not in context._unit_cache
        context._unit_cache[c_value] = self

        self._cache_version_number: Opt[int] = None
        """
        Last version number we saw for this analysis unit wrapper. If it's
        different from `self._unit_version`, it means that the unit was
        reparsed: in this case we need to clear the node cache below (see the
        `_check_node_cache` method).
        """

        self._node_cache: Dict[Tuple[Any, Any, Any], GprNode] = {}
        """
        Cache for all node wrappers in this unit. Indexed by couples:
        (c_value, metadata, rebindings).
        """

        self._check_node_cache()

    def __eq__(self, other: Any) -> bool:
        return self._c_value == other._c_value

    def __hash__(self) -> int:
        return hash(self._c_value)

    @property
    def context(self) -> AnalysisContext:
        """
        Return the context that owns this unit.
        """
        return self._context_link

    def reparse(self,
                buffer: Opt[AnyStr] = None,
                charset: Opt[str] = None) -> None:
        """
        Reparse an analysis unit from a buffer, if provided, or from the
        original file otherwise. If ``Charset`` is empty or ``None``, use the
        last charset successfuly used for this unit, otherwise use it to decode
        the content of the source file.

        If any failure occurs, such as decoding, lexing or parsing failure,
        diagnostic are emitted to explain what happened.
        """
        _charset = _unwrap_charset(charset)
        if buffer is None:
            _unit_reparse_from_file(self._c_value, _charset)
        else:
            _buffer, _charset = _canonicalize_buffer(buffer, _charset)
            _unit_reparse_from_buffer(self._c_value, _charset, _buffer,
                                      len(_buffer))

    def populate_lexical_env(self) -> None:
        """
        Create lexical environments for this analysis unit, according to the
        specifications given in the language spec.

        If not done before, it will be automatically called during semantic
        analysis. Calling it before enables one to control where the latency
        occurs.

        Depending on whether errors are discarded (see
        ``Discard_Errors_In_Populate_Lexical_Env``), raise a ``Property_Error``
        on failure.
        """
        if not _unit_populate_lexical_env(self._c_value):
            raise PropertyError()

    @property
    def root(self) -> GprNode:
        """
        Return the root node for this unit, or ``None`` if there is none.
        """
        result = _Entity_c_type()
        _unit_root(self._c_value, ctypes.byref(result))
        return GprNode._wrap(result)

    @property
    def first_token(self) -> Opt[Token]:
        """
        Return a reference to the first token scanned in this unit.
        """
        result = Token._c_struct()
        _unit_first_token(self._c_value, ctypes.byref(result))
        return Token._wrap(result)

    @property
    def last_token(self) -> Opt[Token]:
        """
        Return a reference to the last token scanned in this unit.
        """
        result = Token._c_struct()
        _unit_last_token(self._c_value, ctypes.byref(result))
        return Token._wrap(result)

    @property
    def text(self) -> str:
        """
        Return the source buffer associated to this unit.
        """
        if self.first_token:
            assert self.last_token
            return Token.text_range(self.first_token, self.last_token)
        else:
            return ""

    @property
    def token_count(self) -> int:
        """
        Return the number of tokens in this unit.
        """
        return _unit_token_count(self._c_value)

    @property
    def trivia_count(self) -> int:
        """
        Return the number of trivias in this unit. This is 0 for units that
        were parsed with trivia analysis disabled.
        """
        return _unit_trivia_count(self._c_value)

    def lookup_token(self, sloc: Sloc) -> Opt[Token]:
        """
        Look for a token in this unit that contains the given source location.
        If this falls before the first token, return the first token. If this
        falls between two tokens, return the token that appears before. If this
        falls after the last token, return the last token. If there is no token
        in this unit, return no token.
        """
        unit = AnalysisUnit._unwrap(self)
        _sloc = Sloc._c_type._unwrap(sloc)
        result = Token._c_struct()
        _unit_lookup_token(unit, ctypes.byref(_sloc), ctypes.byref(result))
        return Token._wrap(result)

    def _dump_lexical_env(self) -> None:
        """
        Debug helper: output the lexical envs for the given analysis unit.
        """
        unit = AnalysisUnit._unwrap(self)
        _unit_dump_lexical_env(unit)

    def iter_tokens(self) -> AnalysisUnit.TokenIterator:
        """
        Iterator over the tokens in an analysis unit.
        """
        return self.TokenIterator(self.first_token)

    @property
    def filename(self) -> str:
        """
        Return the filename this unit is associated to.
        """
        filename = _unit_filename(self._c_value)
        return _unwrap_str(filename)

    @property
    def diagnostics(self) -> List[Diagnostic]:
        """
        Diagnostics for this unit.
        """
        count = _unit_diagnostic_count(self._c_value)
        result = []
        diag = Diagnostic._c_type()
        for i in range(count):
            success = _unit_diagnostic(self._c_value, i, ctypes.byref(diag))
            assert success
            result.append(diag._wrap())
        return result

    def __repr__(self) -> str:
        return '<AnalysisUnit {}>'.format(repr(
            os.path.basename(self.filename)
        ))

    class _c_struct(ctypes.Structure):
        _fields_ = [('unit_version', ctypes.c_uint64)]
    _c_type = _hashable_c_pointer(_c_struct)

    @classmethod
    def _wrap(cls, c_value):
        if not c_value:
            return None

        # Invalidate the unit cache if needed, then look for an existing
        # wrapper for this unit.
        context = cls._context(c_value)
        context._check_unit_cache()

        try:
            return context._unit_cache[c_value]
        except KeyError:
            return cls(context, c_value)

    @classmethod
    def _unwrap(cls, value):
        if value is None:
            return value
        elif not isinstance(value, cls):
            _raise_type_error(cls.__name__, value)
        else:
            return value._c_value

    @classmethod
    def _context(cls, c_value) -> AnalysisContext:
        ctx = _unit_context(c_value)
        return AnalysisContext._wrap(ctx)

    @property
    def _unit_version(self) -> int:
        return self._c_value.contents.unit_version

    def _check_node_cache(self) -> None:
        """
        If this unit has been reparsed, invalidate its node cache.
        """
        if self._cache_version_number != self._unit_version:
            self._node_cache = {}
            self._cache_version_number = self._unit_version


class Sloc:
    """
    Location in a source file. Line and column numbers are one-based.
    """

    def __init__(self, line: int, column: int):
        assert line >= 0 and column >= 0
        self.line = line
        self.column = column

    def __bool__(self) -> bool:
        return bool(self.line or self.column)

    def __lt__(self, other: Sloc) -> bool:
        # First compare line numbers...
        if self.line < other.line:
            return True
        elif self.line > other.line:
            return False

        # Past this point, we know that both are on the same line, so now
        # compare column numbers.
        else:
            return self.column < other.column

    def __eq__(self, other: Any) -> bool:
        return self.line == other.line and self.column == other.column

    def __hash__(self) -> int:
        return hash((self.line, self.column))

    def __str__(self) -> str:
        return '{}:{}'.format(self.line, self.column)

    def __repr__(self) -> str:
        return '<Sloc {} at {:#x}>'.format(self, id(self))

    class _c_type(ctypes.Structure):
        _fields_ = [("line", ctypes.c_uint32),
                    ("column", ctypes.c_uint16)]

        def _wrap(self) -> Sloc:
            return Sloc(self.line, self.column)

        @classmethod
        def _unwrap(cls, sloc: Sloc) -> Sloc._c_type:
            return cls(sloc.line, sloc.column)


class SlocRange:
    """
    Location of a span of text in a source file.
    """

    def __init__(self, start: Sloc, end: Sloc):
        self.start = start
        self.end = end

    def __bool__(self) -> bool:
        return bool(self.start or self.end)

    def __lt__(self, other: SlocRange) -> bool:
        raise NotImplementedError('SlocRange comparison not supported')

    def __eq__(self, other: Any) -> bool:
        return self.start == other.start and self.end == other.end

    def __hash__(self) -> int:
        return hash((self.start, self.end))

    def __str__(self) -> str:
        return '{}-{}'.format(self.start, self.end)

    def __repr__(self) -> str:
        return "<SlocRange {}:{}-{}:{}>".format(
            self.start.line, self.start.column,
            self.end.line, self.end.column
        )

    class _c_type(ctypes.Structure):
        _fields_ = [("start", Sloc._c_type),
                    ("end", Sloc._c_type)]

        def _wrap(self) -> SlocRange:
            return SlocRange(self.start._wrap(), self.end._wrap())


class Diagnostic:
    """
    Diagnostic for an analysis unit: cannot open the source file, parsing
    error, ...
    """

    def __init__(self, sloc_range: SlocRange, message: str):
        self.sloc_range = sloc_range
        self.message = message

    @property
    def as_text(self) -> str:
        return (u'{}: {}'.format(self.sloc_range, self.message)
                if self.sloc_range else
                self.message)

    def __str__(self) -> str:
        return self.as_text

    def __repr__(self) -> str:
        return '<Diagnostic {}>'.format(self)


    class _c_type(ctypes.Structure):
        _fields_ = [('sloc_range', SlocRange._c_type),
                    ('message', _text)]

        def _wrap(self) -> Diagnostic:
            return Diagnostic(self.sloc_range._wrap(), self.message._wrap())


class _tdh_c_struct(ctypes.Structure):
    _fields_ = [('version', ctypes.c_uint64)]
_tdh_c_type = _hashable_c_pointer(_tdh_c_struct)


class Token:
    """
    Reference to a token in an analysis unit.
    """

    __slots__ = ("_c_value", "_context_version", "_tdh_version")

    class _c_struct(ctypes.Structure):
        _fields_ = [('context',      AnalysisContext._c_type),
                    ('token_data',   _tdh_c_type),
                    ('token_index',  ctypes.c_int),
                    ('trivia_index', ctypes.c_int),
                    ('kind',         ctypes.c_int),
                    ('text',         _text),
                    ('sloc_range',   SlocRange._c_type)]
    _c_type = _hashable_c_pointer(_c_struct)

    def __init__(self, c_value: Any):
        """
        This constructor is an implementation detail and is not meant to be
        used directly.
        """
        self._c_value = c_value
        self._context_version = c_value.context.contents.serial_number
        self._tdh_version = c_value.token_data.contents.version

    @classmethod
    def _wrap(cls, c_value: Any) -> Opt[Token]:
        return cls(c_value) if c_value.token_data else None

    @classmethod
    def _unwrap(cls, value):
        cls._check_token(value)
        return value._c_value

    def _check_stale_reference(self) -> None:
        # First, check that the reference to the context is not stale
        if (
            self._c_value.context.contents.serial_number
            != self._context_version
        ):
            raise StaleReferenceError("owning context was deallocated")

        # The context is valid, so the token data handler is, too: check that
        # no reparsing occured.
        if self._c_value.token_data.contents.version != self._tdh_version:
            raise StaleReferenceError("owning unit was reparsed")

    @staticmethod
    def _check_token(value: Any) -> None:
        if not isinstance(value, Token):
            raise TypeError('invalid token: {}'.format(value))
        value._check_stale_reference()

    def _check_same_unit(self, other: Token) -> None:
        if self._c_value.token_data != other._c_value.token_data:
            raise ValueError('{} and {} come from different analysis units'
                             .format(self, other))

    @property
    def next(self) -> Opt[Token]:
        """
        Return a reference to the next token in the corresponding analysis
        unit.
        """
        self._check_stale_reference()
        result = self._c_struct()
        _token_next(ctypes.byref(self._c_value), ctypes.byref(result))
        return self._wrap(result)

    @property
    def previous(self) -> Opt[Token]:
        """
        Return a reference to the previous token in the corresponding analysis
        unit.
        """
        self._check_stale_reference()
        result = self._c_struct()
        _token_previous(ctypes.byref(self._c_value), ctypes.byref(result))
        return self._wrap(result)

    def range_until(self, other: Token) -> Iterator[Token]:
        """
        Return an iterator on the list of tokens that spans between ``self``
        and ``other`` (included). This returns an empty list if the first token
        appears after the other one in the source code. Raise a ``ValueError``
        if both tokens come from different analysis units.
        """
        self._check_stale_reference()
        self._check_token(other)
        self._check_same_unit(other)

        # Keep the generator as a nested function so that the above checks are
        # executed when the generator is created, instead of only when its
        # first item is requested.
        #
        # Note that, because the execution of a generator stops and resumes,
        # the tokens may become stale after it resumes: check for stale
        # references at starting and resuming time.
        def generator() -> Iterator[Token]:
            self._check_stale_reference()
            if other < self:
                return

            yield self
            current = self
            while current < other:
                next = current.next
                assert next is not None
                yield next
                self._check_stale_reference()
                current = next
        return generator()

    def is_equivalent(self, other: Token) -> bool:
        """
        Return whether ``L`` and ``R`` are structurally equivalent tokens. This
        means that their position in the stream won't be taken into account,
        only the kind and text of the token.
        """
        self._check_stale_reference()
        self._check_token(other)
        return bool(_token_is_equivalent(
            ctypes.byref(self._c_value), ctypes.byref(other._c_value))
        )

    @property
    def kind(self) -> str:
        """
        Kind for this token.
        """
        self._check_stale_reference()
        name = _token_kind_name(self._c_value.kind)
        # The _token_kind_name wrapper is already supposed to handle exceptions
        # so this should always return a non-null value.
        assert name
        return _unwrap_str(name)

    @property
    def is_trivia(self) -> bool:
        """
        Return whether this token is a trivia. If it's not, it's a regular
        token.
        """
        self._check_stale_reference()
        return self._c_value.trivia_index != 0

    @property
    def index(self) -> int:
        """
        Zero-based index for this token/trivia. Tokens and trivias get their
        own index space.
        """
        self._check_stale_reference()
        return (self._c_value.token_index - 1
                if self._c_value.trivia_index == 0 else
                self._c_value.trivia_index - 1)

    @property
    def text(self) -> str:
        """
        Return the text of the given token.
        """
        self._check_stale_reference()
        return self._c_value.text._wrap()

    @classmethod
    def text_range(cls, first: Token, last: Token) -> str:
        """
        Compute the source buffer slice corresponding to the text that spans
        between the ``First`` and ``Last`` tokens (both included). This yields
        an empty slice if ``Last`` actually appears before ``First``.

        This raises a ``ValueError`` if ``First`` and ``Last`` don't belong to
        the same analysis unit.
        """
        cls._check_token(first)
        cls._check_token(last)
        first._check_same_unit(last)
        result = _text()
        success = _token_range_text(
            ctypes.byref(first._c_value),
            ctypes.byref(last._c_value),
            ctypes.byref(result),
        )
        assert success
        return result._wrap() or u''

    @property
    def sloc_range(self) -> SlocRange:
        """
        Return the source location range of the given token.
        """
        self._check_stale_reference()
        return self._c_value.sloc_range._wrap()

    def __eq__(self, other: Any) -> bool:
        """
        Return whether the two tokens refer to the same token in the same unit.

        Note that this does not actually compares the token data.
        """
        return (isinstance(other, Token)
                and self._identity_tuple == other._identity_tuple)

    def __hash__(self) -> int:
        return hash(self._identity_tuple)

    def __repr__(self) -> str:
        self._check_stale_reference()
        return '<Token {}{} at {}>'.format(
            self.kind,
            ' {}'.format(repr(self.text)) if self.text else '',
            self.sloc_range
        )

    def __lt__(self, other: Opt[Token]):
        """
        Consider that None comes before all tokens. Then, sort by unit, token
        index, and trivia index.
        """
        self._check_stale_reference()

        # None always comes first
        if other is None:
            return False

        self._check_token(other)
        self._check_same_unit(other)
        return self._identity_tuple < other._identity_tuple

    def __le__(self, other: Opt[Token]) -> bool:
        return self == other or self < other

    def __gt__(self, other: Opt[Token]) -> bool:
        return not (self <= other)

    def __ge__(self, other: Opt[Token]) -> bool:
        return not (self < other)

    def to_data(self) -> dict:
        """
        Return a dict representation of this Token.
        """
        return {"kind": "Token", "token_kind": self.kind, "text": self.text}

    @property
    def _identity_tuple(self) -> Tuple[Any, int, int]:
        """
        Return a tuple that return a tuple that contains "identity" information
        for this token. Think of it as a database primary key.

        This property is for internal use only.
        """
        return (
            self._c_value.token_data,
            self._c_value.token_index,
            self._c_value.trivia_index
        )


class FileReader:
    """
    Interface to override how source files are fetched and decoded.
    """

    def __init__(self, c_value: Any):
        """
        This constructor is an implementation detail, and is not meant to be
        used directly.
        """
        self._c_value = c_value

    def __del__(self) -> None:
        _dec_ref_file_reader(self._c_value)





class UnitProvider:
    """
    Interface to fetch analysis units from a name and a unit kind.

    The unit provider mechanism provides an abstraction which assumes that to
    any couple (unit name, unit kind) we can associate at most one source file.
    This means that several couples can be associated to the same source file,
    but on the other hand, only one one source file can be associated to a
    couple.

    This is used to make the semantic analysis able to switch from one analysis
    units to another.

    See the documentation of each unit provider for the exact semantics of the
    unit name/kind information.
    """

    def __init__(self, c_value: Any):
        """
        This constructor is an implementation detail, and is not meant to be
        used directly.
        """
        self._c_value = c_value

    def __del__(self) -> None:
        _dec_ref_unit_provider(self._c_value)





class GprNode:
    """

    """

    is_list_type = False
    __slots__ = ('_unprotected_c_value', '_node_c_value', '_metadata',
                 '_rebindings', '_unprotected_getitem_cache', '_unit',
                 '_unit_version', '_rebindings_version')

    _kind_name: str
    _field_names: Tuple[str, ...]

    
    

    
    @property
    def parent(
        self
    ) -> GprNode:
        """
        Return the syntactic parent for this node. Return null for the root
        node.
        """
        

        


        
        c_result = self._eval_field(_Entity_c_type(), _gpr_node_parent)
        result = GprNode._wrap(c_result)


        return result
    
    def parents(
        self, with_self: bool = True
    ) -> List[GprNode]:
        """
        Return an array that contains the lexical parents, this node included
        iff ``with_self`` is True. Nearer parents are first in the list.
        """
        

        

        unwrapped_with_self = bool(with_self)

        
        c_result = self._eval_field(_GprNodeArrayConverter.c_type(), _gpr_node_parents, unwrapped_with_self)
        result = _GprNodeArrayConverter.wrap(c_result, False)


        return result
    
    @property
    def children(
        self
    ) -> List[GprNode]:
        """
        Return an array that contains the direct lexical children.

        .. warning:: This constructs a whole array every-time you call it, and
           as such is less efficient than calling the ``Child`` built-in.
        """
        

        


        
        c_result = self._eval_field(_GprNodeArrayConverter.c_type(), _gpr_node_children)
        result = _GprNodeArrayConverter.wrap(c_result, False)


        return result
    
    @property
    def token_start(
        self
    ) -> Opt[Token]:
        """
        Return the first token used to parse this node.
        """
        

        


        
        c_result = self._eval_field(Token._c_struct(), _gpr_node_token_start)
        result = Token._wrap(c_result)


        return result
    
    @property
    def token_end(
        self
    ) -> Opt[Token]:
        """
        Return the last token used to parse this node.
        """
        

        


        
        c_result = self._eval_field(Token._c_struct(), _gpr_node_token_end)
        result = Token._wrap(c_result)


        return result
    
    @property
    def child_index(
        self
    ) -> int:
        """
        Return the 0-based index for Node in its parent's children.
        """
        

        


        
        c_result = self._eval_field(ctypes.c_int(), _gpr_node_child_index)
        result = c_result.value


        return result
    
    @property
    def previous_sibling(
        self
    ) -> GprNode:
        """
        Return the node's previous sibling, or null if there is no such
        sibling.
        """
        

        


        
        c_result = self._eval_field(_Entity_c_type(), _gpr_node_previous_sibling)
        result = GprNode._wrap(c_result)


        return result
    
    @property
    def next_sibling(
        self
    ) -> GprNode:
        """
        Return the node's next sibling, or null if there is no such sibling.
        """
        

        


        
        c_result = self._eval_field(_Entity_c_type(), _gpr_node_next_sibling)
        result = GprNode._wrap(c_result)


        return result
    
    @property
    def unit(
        self
    ) -> AnalysisUnit:
        """
        Return the analysis unit owning this node.
        """
        

        


        
        c_result = self._eval_field(AnalysisUnit._c_type(), _gpr_node_unit)
        result = AnalysisUnit._wrap(c_result)


        return result
    
    @property
    def is_ghost(
        self
    ) -> bool:
        """
        Return whether the node is a ghost.

        Unlike regular nodes, ghost nodes cover no token in the input source:
        they are logically located instead between two tokens. Both the
        ``token_start`` and the ``token_end`` of all ghost nodes is the token
        right after this logical position.
        """
        

        


        
        c_result = self._eval_field(ctypes.c_uint8(), _gpr_node_is_ghost)
        result = bool(c_result.value)


        return result
    
    @property
    def full_sloc_image(
        self
    ) -> str:
        """
        Return a string containing the filename + the sloc in GNU conformant
        format. Useful to create diagnostics from a node.
        """
        

        


        
        c_result = self._eval_field(_String.c_type(), _gpr_node_full_sloc_image)
        result = _String.wrap(c_result)


        return result

    _field_names = () + (
    )




    def __init__(self,
                 c_value: Any,
                 node_c_value: Any,
                 metadata: Any,
                 rebindings: Any):
        """
        This constructor is an implementation detail, and is not meant to be
        used directly. For now, the creation of AST nodes can happen only as
        part of the parsing of an analysis unit.
        """

        self._unprotected_c_value = c_value

        # Access to these fields is unprotected from stale references, but it
        # is supposed to be used only in _id_tuple, which itself should not be
        # used outside of hashing/equality use cases.
        self._node_c_value = node_c_value
        self._rebindings = rebindings
        self._metadata = metadata

        self._unprotected_getitem_cache: Dict[int,
                                              Opt[GprNode]] = {}
        """
        Cache for the __getitem__ override.
        """

        # Information to check before accessing node data that it is still
        # valid.
        self._unit = self._fetch_unit(c_value)
        self._unit_version = self._unit._unit_version
        self._rebindings_version = (
            rebindings.contents.version if rebindings else None
        )

    def _check_stale_reference(self) -> None:
        # We have a reference to the owning unit, so there is no need to
        # check that the unit and the context are still valid. Just check that
        # the unit has not been reparsed.
        if self._unit._unit_version != self._unit_version:
            raise StaleReferenceError("unit was reparsed")

        # Also check that the rebindings are still valid
        if (
            self._rebindings
            and self._rebindings.contents.version != self._rebindings_version
        ):
            raise StaleReferenceError("related unit was reparsed")

    @property
    def _c_value(self) -> Any:
        self._check_stale_reference()
        return self._unprotected_c_value

    @property
    def _getitem_cache(self) -> Dict[int, Opt[GprNode]]:
        self._check_stale_reference()
        return self._unprotected_getitem_cache

    @property
    def _id_tuple(self) -> Tuple[Any, Any]:
        return (self._node_c_value, self._rebindings)

    def __eq__(self, other: Any) -> bool:
        return (isinstance(other, GprNode) and
                self._id_tuple == other._id_tuple)

    def __ne__(self, other: Any) -> bool:
        return not (self == other)

    def __hash__(self) -> int:
        return hash(self._id_tuple)

    @property
    def kind_name(self) -> str:
        """
        Return the kind of this node.
        """
        return self._kind_name

    @property
    def is_token_node(self) -> bool:
        """
        Return whether this node is a node that contains only a single token.
        """
        node = self._unwrap(self)
        return bool(_node_is_token_node(ctypes.byref(node)))

    @property
    def is_synthetic(self) -> bool:
        """
        Return whether this node is synthetic.
        """
        node = self._unwrap(self)
        return bool(_node_is_synthetic(ctypes.byref(node)))

    @property
    def sloc_range(self) -> SlocRange:
        """
        Return the spanning source location range for this node.

        Note that this returns the sloc of the parent for synthetic nodes.
        """
        node = self._unwrap(self)
        result = SlocRange._c_type()
        _node_sloc_range(ctypes.byref(node), ctypes.byref(result))
        return result._wrap()

    @property
    def text(self) -> str:
        """
        Return the source buffer slice corresponding to the text that spans
        between the first and the last tokens of this node.

        Note that this returns the empty string for synthetic nodes.
        """
        node = self._unwrap(self)
        result = _text()
        _node_text(ctypes.byref(node), ctypes.byref(result))
        return result._wrap()

    @property
    def image(self) -> str:
        """
        Return a representation of this node as a string.
        """
        c_node = self._unwrap(self)
        c_result = _text()
        _node_image(ctypes.byref(c_node), ctypes.byref(c_result))
        return c_result._wrap()

    def lookup(self, sloc: Sloc) -> Opt[GprNode]:
        """
        Return the bottom-most node from in ``Node`` and its children which
        contains ``Sloc``, or ``None`` if there is none.
        """
        node = self._unwrap(self)
        c_sloc = Sloc._c_type._unwrap(sloc)
        result = _Entity_c_type()
        _lookup_in_node(ctypes.byref(node), ctypes.byref(c_sloc),
                        ctypes.byref(result))
        return GprNode._wrap(result)

    def __bool__(self) -> bool:
        """
        Return always True so that checking a node against None can be done as
        simply as:

        .. code::

           if node:
               ...
        """
        return True

    def __iter__(self) -> Iterator[Opt[GprNode]]:
        """
        Return an iterator on the children of this node.
        """
        for i in range(len(self)):
            yield self[i]

    def __len__(self) -> int:
        """
        Return the number of GprNode children this node has.
        """
        node = self._unwrap(self)
        return _node_children_count(ctypes.byref(node))

    def __getitem__(self, key: int) -> Opt[GprNode]:
        """
        Return the Nth GprNode child this node has.

        This handles negative indexes the same way Python lists do. Raise an
        IndexError if "key" is out of range.
        """
        if not isinstance(key, int):
            msg = ('GprNode children are integer-indexed'
                   ' (got {})').format(type(key))
            raise TypeError(msg)

        if key < 0:
            key += len(self)

        if key in self._getitem_cache:
            return self._getitem_cache[key]

        node = self._unwrap(self)
        result_struct = _Entity_c_type()
        success = _node_child(
            ctypes.byref(node), key, ctypes.byref(result_struct)
        )
        if not success:
            raise IndexError('child index out of range')
        else:
            result = GprNode._wrap(result_struct)
            self._getitem_cache[key] = result
            return result

    def iter_fields(self) -> Iterator[Tuple[str, Opt[GprNode]]]:
        """
        Iterate through all the fields this node contains.

        Return an iterator that yields (name, value) couples for all abstract
        fields in this node. If "self" is a list, field names will be
        "item_{n}" with "n" being the index.
        """
        if self.is_list_type:
            for i, value in enumerate(self):
                yield ('item_{}'.format(i), value)
        else:
            for field_name in self._field_names:
                yield (field_name, getattr(self, '{}'.format(field_name)))

    def dump_str(self) -> str:
        """
        Dump the sub-tree to a string in a human-readable format.
        """
        output = io.StringIO()
        self.dump(file=output)
        ret = output.getvalue()
        output.close()
        return ret

    def dump(self, indent: str = '', file: IO[str] = sys.stdout) -> None:
        """
        Dump the sub-tree in a human-readable format on the given file.

        :param str indent: Prefix printed on each line during the dump.

        :param file file: File in which the dump must occur.
        """

        def print_node(name, value):
            if isinstance(value, GprNode):
                print('{}{}:'.format(indent, name), file=file)
                value.dump(indent + '  ', file)
            else:
                print('{}{}: {}'.format(indent, name, value), file=file)

        erepr = self.entity_repr[1:-1]
        print('{}{}{}'.format(
            indent, erepr,
            ': {}'.format(self.text) if self.is_token_node else ''
        ), file=file)
        indent = indent + '|'
        if self.is_list_type:
            for i, value in enumerate(self):
                print_node("item_{}".format(i), value)
        else:
            for name, value in self.iter_fields():
                # Remove the f_ prefix to have the same behavior as the Ada
                # dumper.
                print_node(name[2:], value)

    def findall(
        self,
        ast_type_or_pred: Union[Type[GprNode],
                                Callable[[GprNode], bool]],
        **kwargs: Any
    ) -> List[GprNode]:
        """
        Helper for finditer that will return all results as a list. See
        finditer's documentation for more details.
        """
        return list(self.finditer(ast_type_or_pred, **kwargs))

    def find(
        self,
        ast_type_or_pred: Union[Type[GprNode],
                                Callable[[GprNode], bool]],
        **kwargs: Any
    ) -> Opt[GprNode]:
        """
        Helper for finditer that will return only the first result. See
        finditer's documentation for more details.
        """
        try:
            return next(self.finditer(ast_type_or_pred, **kwargs))
        except StopIteration:
            return None

    def finditer(
        self,
        ast_type_or_pred: Union[Type[GprNode],
                                Callable[[GprNode], bool]],
        **kwargs: Any
    ) -> Iterator[GprNode]:
        """
        Find every node corresponding to the passed predicates.

        :param ast_type_or_pred: If supplied with a subclass of GprNode, will
           constrain the resulting collection to only the instances of this
           type or any subclass. If supplied with a predicate, it will apply
           the predicate on every node and keep only the ones for which it
           returns True. If supplied with a list of subclasses of GprNode, it
           will match all instances of any of them.

        :param kwargs: Allows the user to filter on attributes of the node. For
           every key value association, if the node has an attribute of name
           key that has the specified value, then the child is kept.
        """
        # Create a "pred" function to use as the node filter during the
        # traversal.
        if isinstance(ast_type_or_pred, type):
            sought_type = ast_type_or_pred
            pred = lambda node: isinstance(node, sought_type)
        elif isinstance(ast_type_or_pred, collections.abc.Sequence):
            sought_types = ast_type_or_pred
            pred = lambda node: isinstance(node, tuple(sought_types))
        else:
            pred = ast_type_or_pred

        def match(left, right):
            """
            :param left: Node child to match.
            :param right: Matcher, coming from ``kwargs``.
            """
            if left is None:
                return
            if hasattr(left, "match"):
                return left.match(right)
            else:
                return left == right

        def helper(node):
            for child in node:
                if child is not None:
                    if pred(child):
                        if not kwargs:
                            yield child
                        elif all([match(getattr(child, key, None), val)
                                  for key, val in kwargs.items()]):
                            yield child
                    for c in helper(child):
                        if c is not None:
                            yield c

        return helper(self)

    @property
    def parent_chain(self) -> List[GprNode]:
        """
        Return the parent chain of self. Self will be the first element,
        followed by the first parent, then this parent's parent, etc.
        """
        def _parent_chain(node):
            yield node
            if node.parent is not None:
                for p in _parent_chain(node.parent):
                    yield p

        return list(_parent_chain(self))

    def __repr__(self) -> str:
        return self.image

    @property
    def entity_repr(self) -> str:
        c_value = self._unwrap(self)
        c_result = _text()
        _entity_image(ctypes.byref(c_value), ctypes.byref(c_result))
        return c_result._wrap()

    @property
    def tokens(self) -> Iterator[Token]:
        """
        Return an iterator on the range of tokens that self encompasses.
        """
        start = self.token_start
        end = self.token_end

        # All nodes have non-null start/end tokens
        assert start is not None
        assert end is not None

        while not start == end:
            yield start
            next = start.next
            assert next is not None
            start = next
        yield end

    def to_data(self) -> Union[list, dict]:
        """
        Return a nested python data-structure, constituted only of standard
        data types (dicts, lists, strings, ints, etc), and representing the
        portion of the AST corresponding to this node.
        """
        if self.is_list_type:
            return [i.to_data() for i in self if i is not None]
        else:
            return {n: v.to_data()
                    for n, v in self.iter_fields()
                    if v is not None}

    def to_json(self) -> str:
        """
        Return a JSON representation of this node.
        """
        return json.dumps(self.to_data())

    def is_a(self, *types: Type[GprNode]) -> bool:
        """
        Shortcut for isinstance(self, types).
        :rtype: bool
        """
        return isinstance(self, tuple(types))

    if TYPE_CHECKING:
        T = TypeVar('T', bound=GprNode)

    def cast(self, typ: Type[T]) -> T:
        """
        Fluent interface style method. Return ``self``, raise an error if self
        is not of type ``typ``.

        :type typ: () -> T
        :rtype: T
        """
        assert isinstance(self, typ)
        return self

    _node_c_type = _hashable_c_pointer()

    @classmethod
    def _wrap(cls, c_value):
        """
        Internal helper to wrap a low-level entity value into an instance of
        the the appropriate high-level Python wrapper subclass.
        """
        node_c_value = c_value.node
        if not node_c_value:
            return None

        rebindings = c_value.info.rebindings
        metadata = c_value.info.md

        # Look for an already existing wrapper for this node
        cache_key = (node_c_value, metadata, rebindings)
        unit = cls._fetch_unit(c_value)
        unit._check_node_cache()
        try:
            return unit._node_cache[cache_key]
        except KeyError:
            pass

        # Pick the right subclass to materialize this node in Python
        kind = _node_kind(ctypes.byref(c_value))
        result = _kind_to_astnode_cls[kind](c_value, node_c_value, metadata,
                                            rebindings)
        unit._node_cache[cache_key] = result
        return result

    @classmethod
    def _wrap_bare_node(cls, c_value: Any) -> Opt[GprNode]:
        return cls._wrap(_Entity_c_type.from_bare_node(c_value))

    @classmethod
    def _unwrap(cls, py_value: Opt[GprNode]) -> Any:
        """
        Internal helper to unwrap a high-level ASTNode instance into a
        low-level value. Raise a TypeError if the input value has unexpected
        type.
        """
        if py_value is None:
            return _Entity_c_type._null_value
        elif not isinstance(py_value, GprNode):
            _raise_type_error('GprNode', py_value)
        else:
            return py_value._c_value

    @property
    def _unwrap_einfo(self):
        return self._c_value.info

    @classmethod
    def _fetch_unit(cls, c_value: Any) -> AnalysisUnit:
        return AnalysisUnit._wrap(_node_unit(ctypes.byref(c_value)))

    def _eval_field(self, c_result: Any, c_accessor: Any, *c_args: Any) -> Any:
        """
        Internal helper to evaluate low-level field accessors/properties.

        This calls "c_accessor" on this node with the input arguments and puts
        the result in "c_result". This raises a PropertyError if the evaluation
        failed. Return "c_result" for convenience.
        """
        args = (self._unwrap(self), ) + c_args + (ctypes.byref(c_result), )
        if not c_accessor(*args):
            raise PropertyError()
        return c_result

    def _eval_astnode_field(self, c_accessor: Any) -> Any:
        """
        Internal helper. Wrapper around _eval_field for fields that return an
        AST node and that accept no explicit argument. This is useful as it's
        the most common case of field, so using this wrapper reduces generated
        code length.
        """
        return GprNode._wrap(
            self._eval_field(_Entity_c_type(), c_accessor)
        )




class AdaPreludeNode(GprNode):
    """

    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = GprNode._field_names + (
    )







class AdaAccessSubp(AdaPreludeNode):
    """

    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_subp_kind(
        self
    ) -> AdaEntityKind:
        """
        This field can contain one of the following nodes:
        :py:class:`AdaEntityKindFunction`, :py:class:`AdaEntityKindProcedure`
        """
        

        

        result = self._eval_astnode_field(_ada_access_subp_f_subp_kind)



        return result
    
    @property
    def f_skips(
        self
    ) -> AdaSkipList:
        """

        """
        

        

        result = self._eval_astnode_field(_ada_access_subp_f_skips)



        return result

    _field_names = AdaPreludeNode._field_names + (
        "f_subp_kind",
        "f_skips",
    )

    _kind_name = 'AdaAccessSubp'






class AdaContextClause(AdaPreludeNode):
    """

    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = AdaPreludeNode._field_names + (
    )







class AdaPragma(AdaContextClause):
    """

    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_skips(
        self
    ) -> AdaSkipList:
        """

        """
        

        

        result = self._eval_astnode_field(_ada_pragma_f_skips)



        return result

    _field_names = AdaContextClause._field_names + (
        "f_skips",
    )

    _kind_name = 'AdaPragma'






class AdaUse(AdaContextClause):
    """

    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_skips(
        self
    ) -> AdaSkipList:
        """

        """
        

        

        result = self._eval_astnode_field(_ada_use_f_skips)



        return result

    _field_names = AdaContextClause._field_names + (
        "f_skips",
    )

    _kind_name = 'AdaUse'






class AdaWith(AdaContextClause):
    """

    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_has_limited(
        self
    ) -> LimitedNode:
        """

        """
        

        

        result = self._eval_astnode_field(_ada_with_f_has_limited)



        return result
    
    @property
    def f_has_private(
        self
    ) -> PrivateNode:
        """

        """
        

        

        result = self._eval_astnode_field(_ada_with_f_has_private)



        return result
    
    @property
    def f_packages(
        self
    ) -> ExprList:
        """
        This field contains a list that itself contains one of the following
        nodes: :py:class:`Identifier`, :py:class:`Prefix`
        """
        

        

        result = self._eval_astnode_field(_ada_with_f_packages)



        return result

    _field_names = AdaContextClause._field_names + (
        "f_has_limited",
        "f_has_private",
        "f_packages",
    )

    _kind_name = 'AdaWith'






class AdaEntityKind(AdaPreludeNode):
    """

    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = AdaPreludeNode._field_names + (
    )







class AdaEntityKindFunction(AdaEntityKind):
    """

    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = AdaEntityKind._field_names + (
    )

    _kind_name = 'AdaEntityKindFunction'






class AdaEntityKindPackage(AdaEntityKind):
    """

    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = AdaEntityKind._field_names + (
    )

    _kind_name = 'AdaEntityKindPackage'






class AdaEntityKindProcedure(AdaEntityKind):
    """

    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = AdaEntityKind._field_names + (
    )

    _kind_name = 'AdaEntityKindProcedure'






class AdaGeneric(AdaPreludeNode):
    """

    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_skips(
        self
    ) -> GprNode:
        """

        """
        

        

        result = self._eval_astnode_field(_ada_generic_f_skips)



        return result

    _field_names = AdaPreludeNode._field_names + (
        "f_skips",
    )

    _kind_name = 'AdaGeneric'






class AdaLibraryItem(AdaPreludeNode):
    """

    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_generic_stub(
        self
    ) -> AdaGeneric:
        """

        """
        

        

        result = self._eval_astnode_field(_ada_library_item_f_generic_stub)



        return result
    
    @property
    def f_separate(
        self
    ) -> AdaSeparate:
        """

        """
        

        

        result = self._eval_astnode_field(_ada_library_item_f_separate)



        return result
    
    @property
    def f_main(
        self
    ) -> AdaMain:
        """

        """
        

        

        result = self._eval_astnode_field(_ada_library_item_f_main)



        return result

    _field_names = AdaPreludeNode._field_names + (
        "f_generic_stub",
        "f_separate",
        "f_main",
    )

    _kind_name = 'AdaLibraryItem'






class AdaMain(AdaPreludeNode):
    """

    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_name(
        self
    ) -> Expr:
        """
        This field can contain one of the following nodes:
        :py:class:`Identifier`, :py:class:`Prefix`
        """
        

        

        result = self._eval_astnode_field(_ada_main_f_name)



        return result

    _field_names = AdaPreludeNode._field_names + (
    )







class AdaPkg(AdaMain):
    """

    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_has_private(
        self
    ) -> PrivateNode:
        """

        """
        

        

        result = self._eval_astnode_field(_ada_pkg_f_has_private)



        return result

    _field_names = AdaMain._field_names + (
        "f_has_private",
        "f_name",
    )

    _kind_name = 'AdaPkg'






class AdaPkgBody(AdaMain):
    """

    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = AdaMain._field_names + (
        "f_name",
    )

    _kind_name = 'AdaPkgBody'






class AdaSubp(AdaMain):
    """

    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_subp_kind(
        self
    ) -> AdaEntityKind:
        """
        This field can contain one of the following nodes:
        :py:class:`AdaEntityKindFunction`, :py:class:`AdaEntityKindProcedure`
        """
        

        

        result = self._eval_astnode_field(_ada_subp_f_subp_kind)



        return result

    _field_names = AdaMain._field_names + (
        "f_subp_kind",
        "f_name",
    )

    _kind_name = 'AdaSubp'






class AdaPrelude(AdaPreludeNode):
    """

    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_context_clauses(
        self
    ) -> AdaContextClauseList:
        """

        """
        

        

        result = self._eval_astnode_field(_ada_prelude_f_context_clauses)



        return result
    
    @property
    def f_library_item(
        self
    ) -> AdaLibraryItem:
        """

        """
        

        

        result = self._eval_astnode_field(_ada_prelude_f_library_item)



        return result

    _field_names = AdaPreludeNode._field_names + (
        "f_context_clauses",
        "f_library_item",
    )

    _kind_name = 'AdaPrelude'






class AdaSeparate(AdaPreludeNode):
    """

    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_parent_name(
        self
    ) -> Expr:
        """
        This field can contain one of the following nodes:
        :py:class:`Identifier`, :py:class:`Prefix`
        """
        

        

        result = self._eval_astnode_field(_ada_separate_f_parent_name)



        return result

    _field_names = AdaPreludeNode._field_names + (
        "f_parent_name",
    )

    _kind_name = 'AdaSeparate'






class AdaSkip(AdaPreludeNode):
    """

    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = AdaPreludeNode._field_names + (
    )

    _kind_name = 'AdaSkip'






class AdaWithFormal(AdaPreludeNode):
    """

    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_kind(
        self
    ) -> AdaEntityKind:
        """

        """
        

        

        result = self._eval_astnode_field(_ada_with_formal_f_kind)



        return result
    
    @property
    def f_skips(
        self
    ) -> AdaSkipList:
        """

        """
        

        

        result = self._eval_astnode_field(_ada_with_formal_f_skips)



        return result

    _field_names = AdaPreludeNode._field_names + (
        "f_kind",
        "f_skips",
    )

    _kind_name = 'AdaWithFormal'






class AllQualifier(GprNode):
    """

    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def p_as_bool(
        self
    ) -> bool:
        """
        Return whether this is an instance of AllQualifierPresent
        """
        

        


        
        c_result = self._eval_field(ctypes.c_uint8(), _all_qualifier_p_as_bool)
        result = bool(c_result.value)


        return result

    _field_names = GprNode._field_names + (
    )







class AllQualifierAbsent(AllQualifier):
    """

    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = AllQualifier._field_names + (
    )

    _kind_name = 'AllQualifierAbsent'






class AllQualifierPresent(AllQualifier):
    """

    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = AllQualifier._field_names + (
    )

    _kind_name = 'AllQualifierPresent'






class AttributeDecl(GprNode):
    """

    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_attr_name(
        self
    ) -> Identifier:
        """

        """
        

        

        result = self._eval_astnode_field(_attribute_decl_f_attr_name)



        return result
    
    @property
    def f_attr_index(
        self
    ) -> GprNode:
        """
        This field can contain one of the following nodes:
        :py:class:`OthersDesignator`, :py:class:`StringLiteralAt`
        """
        

        

        result = self._eval_astnode_field(_attribute_decl_f_attr_index)



        return result
    
    @property
    def f_expr(
        self
    ) -> TermList:
        """
        This field contains a list that itself contains one of the following
        nodes: :py:class:`BuiltinFunctionCall`, :py:class:`ProjectReference`,
        :py:class:`StringLiteralAt`, :py:class:`Terms`,
        :py:class:`VariableReference`
        """
        

        

        result = self._eval_astnode_field(_attribute_decl_f_expr)



        return result

    _field_names = GprNode._field_names + (
        "f_attr_name",
        "f_attr_index",
        "f_expr",
    )

    _kind_name = 'AttributeDecl'






class AttributeReference(GprNode):
    """

    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_attribute_name(
        self
    ) -> Identifier:
        """

        """
        

        

        result = self._eval_astnode_field(_attribute_reference_f_attribute_name)



        return result
    
    @property
    def f_attribute_index(
        self
    ) -> GprNode:
        """
        This field can contain one of the following nodes:
        :py:class:`OthersDesignator`, :py:class:`StringLiteral`
        """
        

        

        result = self._eval_astnode_field(_attribute_reference_f_attribute_index)



        return result

    _field_names = GprNode._field_names + (
        "f_attribute_name",
        "f_attribute_index",
    )

    _kind_name = 'AttributeReference'






class BaseList(GprNode):
    """

    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = GprNode._field_names + (
    )







class AdaContextClauseList(BaseList):
    """
    List of AdaContextClause.
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = BaseList._field_names + (
    )

    _kind_name = 'AdaContextClauseList'

    is_list_type = True

    def __iter__(
        self
    ) -> Iterator[AdaContextClause]:
        return super().__iter__()  # type: ignore

    def __getitem__(
        self,
        index: int
    ) -> AdaContextClause:
        return super().__getitem__(index)  # type: ignore





class AdaPreludeNodeList(BaseList):
    """
    List of AdaPreludeNode.

    This list node can contain one of the following nodes:
    :py:class:`AdaAccessSubp`, :py:class:`AdaSkip`, :py:class:`AdaWithFormal`
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = BaseList._field_names + (
    )

    _kind_name = 'AdaPreludeNodeList'

    is_list_type = True

    def __iter__(
        self
    ) -> Iterator[AdaPreludeNode]:
        return super().__iter__()  # type: ignore

    def __getitem__(
        self,
        index: int
    ) -> AdaPreludeNode:
        return super().__getitem__(index)  # type: ignore





class AdaSkipList(BaseList):
    """
    List of AdaSkip.
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = BaseList._field_names + (
    )

    _kind_name = 'AdaSkipList'

    is_list_type = True

    def __iter__(
        self
    ) -> Iterator[AdaSkip]:
        return super().__iter__()  # type: ignore

    def __getitem__(
        self,
        index: int
    ) -> AdaSkip:
        return super().__getitem__(index)  # type: ignore





class CaseItemList(BaseList):
    """
    List of CaseItem.
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = BaseList._field_names + (
    )

    _kind_name = 'CaseItemList'

    is_list_type = True

    def __iter__(
        self
    ) -> Iterator[CaseItem]:
        return super().__iter__()  # type: ignore

    def __getitem__(
        self,
        index: int
    ) -> CaseItem:
        return super().__getitem__(index)  # type: ignore





class ExprList(BaseList):
    """
    List of Expr.

    This list node can contain one of the following nodes:
    :py:class:`Identifier`, :py:class:`Prefix`
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = BaseList._field_names + (
    )

    _kind_name = 'ExprList'

    is_list_type = True

    def __iter__(
        self
    ) -> Iterator[Expr]:
        return super().__iter__()  # type: ignore

    def __getitem__(
        self,
        index: int
    ) -> Expr:
        return super().__getitem__(index)  # type: ignore





class GprNodeList(BaseList):
    """
    List of GprNode.

    This list node can contain one of the following nodes:
    :py:class:`AttributeDecl`, :py:class:`BuiltinFunctionCall`,
    :py:class:`CaseConstruction`, :py:class:`EmptyDecl`,
    :py:class:`OthersDesignator`, :py:class:`PackageDecl`,
    :py:class:`ProjectReference`, :py:class:`StringLiteralAt`,
    :py:class:`StringLiteral`, :py:class:`Terms`, :py:class:`TypedStringDecl`,
    :py:class:`VariableDecl`, :py:class:`VariableReference`
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = BaseList._field_names + (
    )

    _kind_name = 'GprNodeList'

    is_list_type = True

    def __iter__(
        self
    ) -> Iterator[GprNode]:
        return super().__iter__()  # type: ignore

    def __getitem__(
        self,
        index: int
    ) -> GprNode:
        return super().__getitem__(index)  # type: ignore





class Choices(GprNodeList):
    """
    This list node can contain one of the following nodes:
    :py:class:`OthersDesignator`, :py:class:`StringLiteral`
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = GprNodeList._field_names + (
    )

    _kind_name = 'Choices'

    is_list_type = True

    def __iter__(
        self
    ) -> Iterator[GprNode]:
        return super().__iter__()  # type: ignore

    def __getitem__(
        self,
        index: int
    ) -> GprNode:
        return super().__getitem__(index)  # type: ignore





class TermList(GprNodeList):
    """
    This list node can contain one of the following nodes:
    :py:class:`BuiltinFunctionCall`, :py:class:`ProjectReference`,
    :py:class:`StringLiteralAt`, :py:class:`Terms`,
    :py:class:`VariableReference`
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = GprNodeList._field_names + (
    )

    _kind_name = 'TermList'

    is_list_type = True

    def __iter__(
        self
    ) -> Iterator[GprNode]:
        return super().__iter__()  # type: ignore

    def __getitem__(
        self,
        index: int
    ) -> GprNode:
        return super().__getitem__(index)  # type: ignore





class IdentifierList(BaseList):
    """
    List of Identifier.
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = BaseList._field_names + (
    )

    _kind_name = 'IdentifierList'

    is_list_type = True

    def __iter__(
        self
    ) -> Iterator[Identifier]:
        return super().__iter__()  # type: ignore

    def __getitem__(
        self,
        index: int
    ) -> Identifier:
        return super().__getitem__(index)  # type: ignore





class StringLiteralList(BaseList):
    """
    List of StringLiteral.
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = BaseList._field_names + (
    )

    _kind_name = 'StringLiteralList'

    is_list_type = True

    def __iter__(
        self
    ) -> Iterator[StringLiteral]:
        return super().__iter__()  # type: ignore

    def __getitem__(
        self,
        index: int
    ) -> StringLiteral:
        return super().__getitem__(index)  # type: ignore





class TermListList(BaseList):
    """
    List of TermList.
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = BaseList._field_names + (
    )

    _kind_name = 'TermListList'

    is_list_type = True

    def __iter__(
        self
    ) -> Iterator[TermList]:
        return super().__iter__()  # type: ignore

    def __getitem__(
        self,
        index: int
    ) -> TermList:
        return super().__getitem__(index)  # type: ignore





class WithDeclList(BaseList):
    """
    List of WithDecl.
    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = BaseList._field_names + (
    )

    _kind_name = 'WithDeclList'

    is_list_type = True

    def __iter__(
        self
    ) -> Iterator[WithDecl]:
        return super().__iter__()  # type: ignore

    def __getitem__(
        self,
        index: int
    ) -> WithDecl:
        return super().__getitem__(index)  # type: ignore





class BuiltinFunctionCall(GprNode):
    """

    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_function_name(
        self
    ) -> Identifier:
        """

        """
        

        

        result = self._eval_astnode_field(_builtin_function_call_f_function_name)



        return result
    
    @property
    def f_parameters(
        self
    ) -> Terms:
        """

        """
        

        

        result = self._eval_astnode_field(_builtin_function_call_f_parameters)



        return result

    _field_names = GprNode._field_names + (
        "f_function_name",
        "f_parameters",
    )

    _kind_name = 'BuiltinFunctionCall'






class CaseConstruction(GprNode):
    """

    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_var_ref(
        self
    ) -> VariableReference:
        """

        """
        

        

        result = self._eval_astnode_field(_case_construction_f_var_ref)



        return result
    
    @property
    def f_items(
        self
    ) -> CaseItemList:
        """

        """
        

        

        result = self._eval_astnode_field(_case_construction_f_items)



        return result

    _field_names = GprNode._field_names + (
        "f_var_ref",
        "f_items",
    )

    _kind_name = 'CaseConstruction'






class CaseItem(GprNode):
    """

    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_choice(
        self
    ) -> Choices:
        """
        This field contains a list that itself contains one of the following
        nodes: :py:class:`OthersDesignator`, :py:class:`StringLiteral`
        """
        

        

        result = self._eval_astnode_field(_case_item_f_choice)



        return result
    
    @property
    def f_decls(
        self
    ) -> GprNodeList:
        """
        This field contains a list that itself contains one of the following
        nodes: :py:class:`AttributeDecl`, :py:class:`CaseConstruction`,
        :py:class:`EmptyDecl`, :py:class:`VariableDecl`
        """
        

        

        result = self._eval_astnode_field(_case_item_f_decls)



        return result

    _field_names = GprNode._field_names + (
        "f_choice",
        "f_decls",
    )

    _kind_name = 'CaseItem'






class CompilationUnit(GprNode):
    """

    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_project(
        self
    ) -> Project:
        """

        """
        

        

        result = self._eval_astnode_field(_compilation_unit_f_project)



        return result

    _field_names = GprNode._field_names + (
        "f_project",
    )

    _kind_name = 'CompilationUnit'






class EmptyDecl(GprNode):
    """

    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = GprNode._field_names + (
    )

    _kind_name = 'EmptyDecl'






class Expr(GprNode):
    """

    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = GprNode._field_names + (
    )







class Prefix(Expr):
    """

    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_prefix(
        self
    ) -> Expr:
        """
        This field can contain one of the following nodes:
        :py:class:`Identifier`, :py:class:`Prefix`
        """
        

        

        result = self._eval_astnode_field(_prefix_f_prefix)



        return result
    
    @property
    def f_suffix(
        self
    ) -> Identifier:
        """

        """
        

        

        result = self._eval_astnode_field(_prefix_f_suffix)



        return result

    _field_names = Expr._field_names + (
        "f_prefix",
        "f_suffix",
    )

    _kind_name = 'Prefix'






class SingleTokNode(Expr):
    """

    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = Expr._field_names + (
    )







class Identifier(SingleTokNode):
    """

    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = SingleTokNode._field_names + (
    )

    _kind_name = 'Identifier'






class NumLiteral(SingleTokNode):
    """

    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = SingleTokNode._field_names + (
    )

    _kind_name = 'NumLiteral'






class StringLiteral(SingleTokNode):
    """

    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = SingleTokNode._field_names + (
    )

    _kind_name = 'StringLiteral'






class LimitedNode(GprNode):
    """

    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def p_as_bool(
        self
    ) -> bool:
        """
        Return whether this is an instance of LimitedPresent
        """
        

        


        
        c_result = self._eval_field(ctypes.c_uint8(), _limited_node_p_as_bool)
        result = bool(c_result.value)


        return result

    _field_names = GprNode._field_names + (
    )







class LimitedAbsent(LimitedNode):
    """

    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = LimitedNode._field_names + (
    )

    _kind_name = 'LimitedAbsent'






class LimitedPresent(LimitedNode):
    """

    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = LimitedNode._field_names + (
    )

    _kind_name = 'LimitedPresent'






class OthersDesignator(GprNode):
    """

    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = GprNode._field_names + (
    )

    _kind_name = 'OthersDesignator'






class PackageDecl(GprNode):
    """

    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_pkg_name(
        self
    ) -> Identifier:
        """

        """
        

        

        result = self._eval_astnode_field(_package_decl_f_pkg_name)



        return result
    
    @property
    def f_pkg_spec(
        self
    ) -> GprNode:
        """
        This field can contain one of the following nodes:
        :py:class:`PackageRenaming`, :py:class:`PackageSpec`
        """
        

        

        result = self._eval_astnode_field(_package_decl_f_pkg_spec)



        return result

    _field_names = GprNode._field_names + (
        "f_pkg_name",
        "f_pkg_spec",
    )

    _kind_name = 'PackageDecl'






class PackageExtension(GprNode):
    """

    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_extended_name(
        self
    ) -> IdentifierList:
        """

        """
        

        

        result = self._eval_astnode_field(_package_extension_f_extended_name)



        return result

    _field_names = GprNode._field_names + (
        "f_extended_name",
    )

    _kind_name = 'PackageExtension'






class PackageRenaming(GprNode):
    """

    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_renamed_name(
        self
    ) -> IdentifierList:
        """

        """
        

        

        result = self._eval_astnode_field(_package_renaming_f_renamed_name)



        return result

    _field_names = GprNode._field_names + (
        "f_renamed_name",
    )

    _kind_name = 'PackageRenaming'






class PackageSpec(GprNode):
    """

    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_extension(
        self
    ) -> PackageExtension:
        """

        """
        

        

        result = self._eval_astnode_field(_package_spec_f_extension)



        return result
    
    @property
    def f_decls(
        self
    ) -> GprNodeList:
        """
        This field contains a list that itself contains one of the following
        nodes: :py:class:`AttributeDecl`, :py:class:`CaseConstruction`,
        :py:class:`EmptyDecl`, :py:class:`VariableDecl`
        """
        

        

        result = self._eval_astnode_field(_package_spec_f_decls)



        return result
    
    @property
    def f_end_name(
        self
    ) -> Identifier:
        """

        """
        

        

        result = self._eval_astnode_field(_package_spec_f_end_name)



        return result

    _field_names = GprNode._field_names + (
        "f_extension",
        "f_decls",
        "f_end_name",
    )

    _kind_name = 'PackageSpec'






class PrivateNode(GprNode):
    """

    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def p_as_bool(
        self
    ) -> bool:
        """
        Return whether this is an instance of PrivatePresent
        """
        

        


        
        c_result = self._eval_field(ctypes.c_uint8(), _private_node_p_as_bool)
        result = bool(c_result.value)


        return result

    _field_names = GprNode._field_names + (
    )







class PrivateAbsent(PrivateNode):
    """

    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = PrivateNode._field_names + (
    )

    _kind_name = 'PrivateAbsent'






class PrivatePresent(PrivateNode):
    """

    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = PrivateNode._field_names + (
    )

    _kind_name = 'PrivatePresent'






class Project(GprNode):
    """

    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_context_clauses(
        self
    ) -> WithDeclList:
        """

        """
        

        

        result = self._eval_astnode_field(_project_f_context_clauses)



        return result
    
    @property
    def f_project_decl(
        self
    ) -> ProjectDeclaration:
        """

        """
        

        

        result = self._eval_astnode_field(_project_f_project_decl)



        return result

    _field_names = GprNode._field_names + (
        "f_context_clauses",
        "f_project_decl",
    )

    _kind_name = 'Project'






class ProjectDeclaration(GprNode):
    """

    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_qualifier(
        self
    ) -> ProjectQualifier:
        """

        """
        

        

        result = self._eval_astnode_field(_project_declaration_f_qualifier)



        return result
    
    @property
    def f_project_name(
        self
    ) -> Expr:
        """
        This field can contain one of the following nodes:
        :py:class:`Identifier`, :py:class:`Prefix`
        """
        

        

        result = self._eval_astnode_field(_project_declaration_f_project_name)



        return result
    
    @property
    def f_extension(
        self
    ) -> ProjectExtension:
        """

        """
        

        

        result = self._eval_astnode_field(_project_declaration_f_extension)



        return result
    
    @property
    def f_decls(
        self
    ) -> GprNodeList:
        """
        This field contains a list that itself contains one of the following
        nodes: :py:class:`AttributeDecl`, :py:class:`CaseConstruction`,
        :py:class:`EmptyDecl`, :py:class:`PackageDecl`,
        :py:class:`TypedStringDecl`, :py:class:`VariableDecl`
        """
        

        

        result = self._eval_astnode_field(_project_declaration_f_decls)



        return result
    
    @property
    def f_end_name(
        self
    ) -> Expr:
        """
        This field can contain one of the following nodes:
        :py:class:`Identifier`, :py:class:`Prefix`
        """
        

        

        result = self._eval_astnode_field(_project_declaration_f_end_name)



        return result

    _field_names = GprNode._field_names + (
        "f_qualifier",
        "f_project_name",
        "f_extension",
        "f_decls",
        "f_end_name",
    )

    _kind_name = 'ProjectDeclaration'






class ProjectExtension(GprNode):
    """

    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_is_all(
        self
    ) -> AllQualifier:
        """

        """
        

        

        result = self._eval_astnode_field(_project_extension_f_is_all)



        return result
    
    @property
    def f_path_name(
        self
    ) -> StringLiteral:
        """

        """
        

        

        result = self._eval_astnode_field(_project_extension_f_path_name)



        return result

    _field_names = GprNode._field_names + (
        "f_is_all",
        "f_path_name",
    )

    _kind_name = 'ProjectExtension'






class ProjectQualifier(GprNode):
    """

    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = GprNode._field_names + (
    )







class ProjectQualifierAbstract(ProjectQualifier):
    """

    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = ProjectQualifier._field_names + (
    )

    _kind_name = 'ProjectQualifierAbstract'






class ProjectQualifierAggregate(ProjectQualifier):
    """

    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = ProjectQualifier._field_names + (
    )

    _kind_name = 'ProjectQualifierAggregate'






class ProjectQualifierAggregateLibrary(ProjectQualifier):
    """

    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = ProjectQualifier._field_names + (
    )

    _kind_name = 'ProjectQualifierAggregateLibrary'






class ProjectQualifierConfiguration(ProjectQualifier):
    """

    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = ProjectQualifier._field_names + (
    )

    _kind_name = 'ProjectQualifierConfiguration'






class ProjectQualifierLibrary(ProjectQualifier):
    """

    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = ProjectQualifier._field_names + (
    )

    _kind_name = 'ProjectQualifierLibrary'






class ProjectQualifierStandard(ProjectQualifier):
    """

    """
    __slots__ : Tuple[str, ...] = ()

    


    _field_names = ProjectQualifier._field_names + (
    )

    _kind_name = 'ProjectQualifierStandard'






class ProjectReference(GprNode):
    """

    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_attr_ref(
        self
    ) -> AttributeReference:
        """

        """
        

        

        result = self._eval_astnode_field(_project_reference_f_attr_ref)



        return result

    _field_names = GprNode._field_names + (
        "f_attr_ref",
    )

    _kind_name = 'ProjectReference'






class StringLiteralAt(GprNode):
    """

    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_str_lit(
        self
    ) -> StringLiteral:
        """

        """
        

        

        result = self._eval_astnode_field(_string_literal_at_f_str_lit)



        return result
    
    @property
    def f_at_lit(
        self
    ) -> NumLiteral:
        """

        """
        

        

        result = self._eval_astnode_field(_string_literal_at_f_at_lit)



        return result

    _field_names = GprNode._field_names + (
        "f_str_lit",
        "f_at_lit",
    )

    _kind_name = 'StringLiteralAt'






class Terms(GprNode):
    """

    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_terms(
        self
    ) -> TermListList:
        """

        """
        

        

        result = self._eval_astnode_field(_terms_f_terms)



        return result

    _field_names = GprNode._field_names + (
        "f_terms",
    )

    _kind_name = 'Terms'






class TypeReference(GprNode):
    """

    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_var_type_name(
        self
    ) -> IdentifierList:
        """

        """
        

        

        result = self._eval_astnode_field(_type_reference_f_var_type_name)



        return result

    _field_names = GprNode._field_names + (
        "f_var_type_name",
    )

    _kind_name = 'TypeReference'






class TypedStringDecl(GprNode):
    """

    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_type_id(
        self
    ) -> Identifier:
        """

        """
        

        

        result = self._eval_astnode_field(_typed_string_decl_f_type_id)



        return result
    
    @property
    def f_string_literals(
        self
    ) -> StringLiteralList:
        """

        """
        

        

        result = self._eval_astnode_field(_typed_string_decl_f_string_literals)



        return result

    _field_names = GprNode._field_names + (
        "f_type_id",
        "f_string_literals",
    )

    _kind_name = 'TypedStringDecl'






class VariableDecl(GprNode):
    """

    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_var_name(
        self
    ) -> Identifier:
        """

        """
        

        

        result = self._eval_astnode_field(_variable_decl_f_var_name)



        return result
    
    @property
    def f_var_type(
        self
    ) -> TypeReference:
        """

        """
        

        

        result = self._eval_astnode_field(_variable_decl_f_var_type)



        return result
    
    @property
    def f_expr(
        self
    ) -> TermList:
        """
        This field contains a list that itself contains one of the following
        nodes: :py:class:`BuiltinFunctionCall`, :py:class:`ProjectReference`,
        :py:class:`StringLiteralAt`, :py:class:`Terms`,
        :py:class:`VariableReference`
        """
        

        

        result = self._eval_astnode_field(_variable_decl_f_expr)



        return result

    _field_names = GprNode._field_names + (
        "f_var_name",
        "f_var_type",
        "f_expr",
    )

    _kind_name = 'VariableDecl'






class VariableReference(GprNode):
    """

    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_variable_name(
        self
    ) -> IdentifierList:
        """

        """
        

        

        result = self._eval_astnode_field(_variable_reference_f_variable_name)



        return result
    
    @property
    def f_attribute_ref(
        self
    ) -> AttributeReference:
        """

        """
        

        

        result = self._eval_astnode_field(_variable_reference_f_attribute_ref)



        return result

    _field_names = GprNode._field_names + (
        "f_variable_name",
        "f_attribute_ref",
    )

    _kind_name = 'VariableReference'






class WithDecl(GprNode):
    """

    """
    __slots__ : Tuple[str, ...] = ()

    

    
    @property
    def f_is_limited(
        self
    ) -> LimitedNode:
        """

        """
        

        

        result = self._eval_astnode_field(_with_decl_f_is_limited)



        return result
    
    @property
    def f_path_names(
        self
    ) -> StringLiteralList:
        """

        """
        

        

        result = self._eval_astnode_field(_with_decl_f_path_names)



        return result

    _field_names = GprNode._field_names + (
        "f_is_limited",
        "f_path_names",
    )

    _kind_name = 'WithDecl'






class _EnvRebindingsType_c_type(ctypes.Structure):
    _fields_ = [("version", ctypes.c_uint64)]


_EnvRebindings_c_type = _hashable_c_pointer(_EnvRebindingsType_c_type)




class _BaseStruct:
    """
    Mixin for Ada struct wrappers.
    """

    # Subclasses will override this to a subclass of ctypes.Structure
    _c_type: ClassVar[ctypes.Structure]

    def __getitem__(self, key: int) -> Any:
        if not isinstance(key, int):
            raise TypeError(
               'Tuples items are indexed by integers, not {}'.format(type(key))
            )

        fields = self._c_type._fields_
        if 0 <= key < len(fields):
            field_name, _ = fields[key]
            return getattr(self, field_name)
        else:
            raise IndexError('There is no {}th field'.format(key))

    def __repr__(self) -> str:
        field_names = [name for name, _ in self._c_type._fields_]
        return '<{} {}>'.format(
            type(self).__name__,
            ' '.join('{}={}'.format(name, getattr(self, name))
                      for name in field_names)
        )

    @property
    def as_tuple(self) -> tuple:
        return tuple(getattr(self, f) for f, _ in self._c_type._fields_)

    def __eq__(self, other: Any) -> bool:
        return (isinstance(other, type(self)) and
                self.as_tuple == other.as_tuple)

    def __ne__(self, other: Any) -> bool:
        return not (self == other)

    def __hash__(self) -> int:
        return hash(self.as_tuple)




class _Metadata_c_type(ctypes.Structure):
    _fields_: ClassVar[List[Tuple[str, Any]]] = (
         [
] 
    )
    _null_value: ClassVar[_Metadata_c_type]

    @property
    def as_tuple(self):
        return tuple(getattr(self, f) for f, _ in self._fields_)

    def __eq__(self, other):
        return (isinstance(other, type(self)) and
                self.as_tuple == other.as_tuple)

    def __ne__(self, other):
        return not (self == other)

    def __hash__(self):
        return hash(self.as_tuple)
class _EntityInfo_c_type(ctypes.Structure):
    _fields_: ClassVar[List[Tuple[str, Any]]] = (
         [
        ('md',
            _Metadata_c_type
         ),
        ('rebindings',
            _EnvRebindings_c_type
         ),
        ('from_rebound',
            ctypes.c_uint8
         ),
] 
    )
    _null_value: ClassVar[_EntityInfo_c_type]
class _Entity_c_type(ctypes.Structure):
    _fields_: ClassVar[List[Tuple[str, Any]]] = (
         [
        ('node',
            GprNode._node_c_type
         ),
        ('info',
            _EntityInfo_c_type
         ),
] 
    )
    _null_value: ClassVar[_Entity_c_type]

    @classmethod
    def from_bare_node(cls, node_c_value):
        return cls(node_c_value, _EntityInfo_c_type._null_value)


_Metadata_c_type._null_value = _Metadata_c_type()
_EntityInfo_c_type._null_value = _EntityInfo_c_type(_Metadata_c_type._null_value,
                                                None)


#
# Low-level binding - Second part
#

# For performance, allocate a single C API entity for all uses of null
# entities.
_Entity_c_type._null_value = _Entity_c_type()
_Entity_c_type._null_value.node = None



class _BaseArray:
    """
    Base class for Ada arrays bindings.
    """

    c_element_type: ClassVar[Any]
    """
    Ctype class for array elements.
    """

    items_refcounted = False
    """
    Whether items for this arrays are ref-counted.
    """

    __slots__ = ('c_value', 'length', 'items')

    def __init__(self, c_value):
        self.c_value = c_value

        self.length = c_value.contents.n

        items_addr = _field_address(c_value.contents, 'items')
        items = self.c_element_type.from_address(items_addr)
        self.items = ctypes.pointer(items)

    def __repr__(self):
        return '<{} {}>'.format(type(self).__name__, list(self))

    def clear(self):
        self.c_value = None
        self.length = None
        self.items = None

    def __del__(self):
        self.dec_ref(self.c_value)
        self.clear()

    @classmethod
    def wrap(cls, c_value, from_field_access):
        helper = cls(c_value)

        result = []
        for i in range(helper.length):
            # In ctypes, accessing an array element does not copy it, which
            # means the the array must live at least as long as the accessed
            # element. We cannot guarantee that, so we must copy the element so
            # that it is independent of the array it comes from.
            #
            # The try/except block tries to do a copy if "item" is indeed a
            # buffer to be copied, and will fail if it's a mere integer, which
            # does not need the buffer copy anyway, hence the "pass".
            item = helper.items[i]
            try:
                item = cls.c_element_type.from_buffer_copy(item)
            except TypeError:
                pass
            result.append(helper.wrap_item(item))

        # If this array value comes from a structure field, we must not call
        # its dec_ref primitive, as it is up to the structure's dec_ref
        # primitive to take care of it.
        if from_field_access:
            helper.clear()

        return result

    @classmethod
    def unwrap(cls, value, context=None):
        if not isinstance(value, list):
            _raise_type_error('list', value)

        # Create a holder for the result
        result = cls(cls.create(len(value)))

        # Unwrap all items at once, preserving their holder so that resources
        # are deallocated if there is an error at some point.
        items = [result.unwrap_item(item, context) for item in value]

        # Initialize the resulting array
        for i, (_, item) in enumerate(items):
            result.items[i] = item

        # At this point, we know that this is successful. We don't want
        # holders to dec-ref the content so that the return array takes over
        # the corresponding refcounting shares.
        if cls.items_refcounted:
            for holder, _ in items:
                holder.clear()

        return result






class _GprNodeArrayConverter(_BaseArray):
    """
    Wrapper class for arrays of InternalEntity.

    This class is not meant to be directly instantiated: it is only used to
    convert values that various methods take/return.
    """

    __slots__ = _BaseArray.__slots__
    items_refcounted = False

    @staticmethod
    def wrap_item(item):
        return GprNode._wrap(item)

    @staticmethod
    def unwrap_item(item, context=None):
        c_holder = GprNode._unwrap(item)
        c_value = c_holder
        return (c_holder, c_value)

    c_element_type = _Entity_c_type

    class c_struct(ctypes.Structure):
        _fields_ = [('n', ctypes.c_int),
                    ('ref_count', ctypes.c_int),
                    ('items', _Entity_c_type * 1)]

    c_type = ctypes.POINTER(c_struct)

    create = staticmethod(_import_func(
        'gpr_gpr_node_array_create', [ctypes.c_int], c_type))
    inc_ref = staticmethod(_import_func(
        'gpr_gpr_node_array_inc_ref', [c_type], None))
    dec_ref = staticmethod(_import_func(
        'gpr_gpr_node_array_dec_ref', [c_type], None))





_IteratedType = TypeVar("_IteratedType")

class _BaseIterator(Generic[_IteratedType]):
    """
Base class for Ada iterator bindings.

An iterator provides a mean to retrieve values one-at-a-time.

Currently, each iterator is bound to the analysis context used to create it.
Iterators are invalidated as soon as any unit of that analysis is reparsed. Due
to the nature of iterators (lazy computations), this invalidation is necessary
to avoid use of inconsistent state, such as an iterator trying to use analysis
context data that is stale.
"""

    _c_element_type: ClassVar[Any]
    """
    Ctype class for iterator elements.
    """

    __slots__ = ('_c_value',)

    def __init__(self, c_value: Any):
        self._c_value = c_value

    def __repr__(self) -> str:
        return '<{}>'.format(type(self).__name__)

    def _clear(self) -> None:
        self._c_value = None

    def __del__(self) -> None:
        self._dec_ref(self._c_value)
        self._clear()

    @classmethod
    def _wrap(cls, c_value: Any) -> Opt[_BaseIterator]:
        return cls(c_value) if c_value else None

    @classmethod
    def unwrap(cls, value: Opt[_BaseIterator]) -> Any:
        if value is None:
            return None
        elif not isinstance(value, cls):
            _raise_type_error(cls.__name__, value)
        else:
            return value._c_value

    def __iter__(self) -> Iterator[_IteratedType]:
        return self

    def __next__(self) -> _IteratedType:
        """
      Return the next value from the iterator. Raises ``StopIteration`` if
      there is no more element to retrieve.

      This raises a ``Stale_Reference_Error`` exception if the iterator is
      invalidated.
      """
        x = self._c_element_type()
        if self._get_next(self._c_value, ctypes.byref(x)):
            return self._wrap_item(x)
        raise StopIteration

    # For Python2 compatibility
    next = __next__

    # The following methods are just for type hints: subclasses override them

    @staticmethod
    def _get_next(c_value: Any, item_ptr: Any) -> Any:
        pass

    @staticmethod
    def _inc_ref(c_value: Any) -> None:
        pass

    @staticmethod
    def _dec_ref(c_value: Any) -> None:
        pass

    @staticmethod
    def _wrap_item(item: Any) -> _IteratedType:
        pass




_free = _import_func(
    'gpr_free',
    [ctypes.c_void_p], None
)

_destroy_text = _import_func(
    'gpr_destroy_text', [ctypes.POINTER(_text)], None
)

_symbol_text = _import_func(
    'gpr_symbol_text',
    [ctypes.POINTER(_symbol_type), ctypes.POINTER(_text)], None
)

_get_versions = _import_func(
    'gpr_get_versions',
    [ctypes.POINTER(ctypes.c_char_p), ctypes.POINTER(ctypes.c_char_p)], None
)

# Analysis primitives
_create_analysis_context = _import_func(
    'gpr_create_analysis_context',
    [ctypes.c_char_p, # charset
     _file_reader,    # file_reader
     _unit_provider,  # unit_provider
     _event_handler,  # event_handler
     ctypes.c_int,    # with_trivia
     ctypes.c_int],   # tab_stop
    AnalysisContext._c_type
)
_context_incref = _import_func(
    'gpr_context_incref',
    [AnalysisContext._c_type], AnalysisContext._c_type
)
_context_decref = _import_func(
    'gpr_context_decref',
    [AnalysisContext._c_type], None
)
_context_symbol = _import_func(
    'gpr_context_symbol',
    [AnalysisContext._c_type,
     ctypes.POINTER(_text),
     ctypes.POINTER(_symbol_type)], ctypes.c_int
)
_discard_errors_in_populate_lexical_env = _import_func(
   'gpr_context_discard_errors_in_populate_lexical_env',
   [AnalysisContext._c_type, ctypes.c_int], None
)
_get_analysis_unit_from_file = _import_func(
    'gpr_get_analysis_unit_from_file',
    [AnalysisContext._c_type,  # context
     ctypes.c_char_p,          # filename
     ctypes.c_char_p,          # charset
     ctypes.c_int,             # reparse
     ctypes.c_int],            # grammar rule
    AnalysisUnit._c_type
)
_get_analysis_unit_from_buffer = _import_func(
    'gpr_get_analysis_unit_from_buffer',
    [AnalysisContext._c_type,  # context
     ctypes.c_char_p,          # filename
     ctypes.c_char_p,          # charset
     ctypes.c_char_p,          # buffer
     ctypes.c_size_t,          # buffer_size
     ctypes.c_int],            # grammar rule
    AnalysisUnit._c_type
)
_unit_root = _import_func(
    'gpr_unit_root',
    [AnalysisUnit._c_type, ctypes.POINTER(_Entity_c_type)], None
)
_unit_first_token = _import_func(
    "gpr_unit_first_token",
    [AnalysisUnit._c_type, Token._c_type], None
)
_unit_last_token = _import_func(
    "gpr_unit_last_token",
    [AnalysisUnit._c_type, Token._c_type], None
)
_unit_token_count = _import_func(
    "gpr_unit_token_count",
    [AnalysisUnit._c_type], ctypes.c_int
)
_unit_trivia_count = _import_func(
    "gpr_unit_trivia_count",
    [AnalysisUnit._c_type], ctypes.c_int
)
_unit_lookup_token = _import_func(
    "gpr_unit_lookup_token",
    [AnalysisUnit._c_type,
     ctypes.POINTER(Sloc._c_type),
     Token._c_type],
    None
)
_unit_dump_lexical_env = _import_func(
    "gpr_unit_dump_lexical_env",
    [AnalysisUnit._c_type], None
)
_unit_filename = _import_func(
    "gpr_unit_filename",
    [AnalysisUnit._c_type], ctypes.POINTER(ctypes.c_char)
)
_unit_diagnostic_count = _import_func(
    'gpr_unit_diagnostic_count',
    [AnalysisUnit._c_type], ctypes.c_uint
)
_unit_diagnostic = _import_func(
    'gpr_unit_diagnostic',
    [AnalysisUnit._c_type, ctypes.c_uint, ctypes.POINTER(Diagnostic._c_type)],
    ctypes.c_int
)
_unit_context = _import_func(
    'gpr_unit_context',
    [AnalysisUnit._c_type], AnalysisContext._c_type
)
_unit_reparse_from_file = _import_func(
    'gpr_unit_reparse_from_file',
    [AnalysisUnit._c_type,    # unit
     ctypes.c_char_p],        # charset
    ctypes.c_int
)
_unit_reparse_from_buffer = _import_func(
    'gpr_unit_reparse_from_buffer',
    [AnalysisUnit._c_type, # unit
     ctypes.c_char_p,      # charset
     ctypes.c_char_p,      # buffer
     ctypes.c_size_t],     # buffer_size
    None
)
_unit_populate_lexical_env = _import_func(
    'gpr_unit_populate_lexical_env',
    [AnalysisUnit._c_type], ctypes.c_int
)

# General AST node primitives
_node_kind = _import_func(
    'gpr_node_kind',
    [ctypes.POINTER(_Entity_c_type)], ctypes.c_int
)
_node_unit = _import_func(
    'gpr_node_unit',
    [ctypes.POINTER(_Entity_c_type)], AnalysisUnit._c_type
)
_node_is_token_node = _import_func(
    'gpr_node_is_token_node',
    [ctypes.POINTER(_Entity_c_type)], ctypes.c_int
)
_node_is_synthetic = _import_func(
    'gpr_node_is_synthetic',
    [ctypes.POINTER(_Entity_c_type)], ctypes.c_int
)
_node_image = _import_func(
    'gpr_node_image',
    [ctypes.POINTER(_Entity_c_type), ctypes.POINTER(_text)], None
)
_node_text = _import_func(
    'gpr_node_text',
    [ctypes.POINTER(_Entity_c_type), ctypes.POINTER(_text)], None
)
_node_sloc_range = _import_func(
    'gpr_node_sloc_range',
    [ctypes.POINTER(_Entity_c_type), ctypes.POINTER(SlocRange._c_type)], None
)
_lookup_in_node = _import_func(
    'gpr_lookup_in_node',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(Sloc._c_type),
     ctypes.POINTER(_Entity_c_type)], None
)
_node_children_count = _import_func(
    'gpr_node_children_count',
    [ctypes.POINTER(_Entity_c_type)], ctypes.c_uint
)
_node_child = _import_func(
    'gpr_node_child',
    [ctypes.POINTER(_Entity_c_type), ctypes.c_uint, ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)

_gpr_node_parent = _import_func(
    'gpr_gpr_node_parent',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_gpr_node_parents = _import_func(
    'gpr_gpr_node_parents',
    [ctypes.POINTER(_Entity_c_type),
        
        ctypes.c_uint8,
     ctypes.POINTER(_GprNodeArrayConverter.c_type)],
    ctypes.c_int
)
_gpr_node_children = _import_func(
    'gpr_gpr_node_children',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_GprNodeArrayConverter.c_type)],
    ctypes.c_int
)
_gpr_node_token_start = _import_func(
    'gpr_gpr_node_token_start',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(Token._c_struct)],
    ctypes.c_int
)
_gpr_node_token_end = _import_func(
    'gpr_gpr_node_token_end',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(Token._c_struct)],
    ctypes.c_int
)
_gpr_node_child_index = _import_func(
    'gpr_gpr_node_child_index',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(ctypes.c_int)],
    ctypes.c_int
)
_gpr_node_previous_sibling = _import_func(
    'gpr_gpr_node_previous_sibling',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_gpr_node_next_sibling = _import_func(
    'gpr_gpr_node_next_sibling',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_gpr_node_unit = _import_func(
    'gpr_gpr_node_unit',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(AnalysisUnit._c_type)],
    ctypes.c_int
)
_gpr_node_is_ghost = _import_func(
    'gpr_gpr_node_is_ghost',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(ctypes.c_uint8)],
    ctypes.c_int
)
_gpr_node_full_sloc_image = _import_func(
    'gpr_gpr_node_full_sloc_image',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_String.c_type)],
    ctypes.c_int
)
_ada_access_subp_f_subp_kind = _import_func(
    'gpr_ada_access_subp_f_subp_kind',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_ada_access_subp_f_skips = _import_func(
    'gpr_ada_access_subp_f_skips',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_ada_pragma_f_skips = _import_func(
    'gpr_ada_pragma_f_skips',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_ada_use_f_skips = _import_func(
    'gpr_ada_use_f_skips',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_ada_with_f_has_limited = _import_func(
    'gpr_ada_with_f_has_limited',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_ada_with_f_has_private = _import_func(
    'gpr_ada_with_f_has_private',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_ada_with_f_packages = _import_func(
    'gpr_ada_with_f_packages',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_ada_generic_f_skips = _import_func(
    'gpr_ada_generic_f_skips',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_ada_library_item_f_generic_stub = _import_func(
    'gpr_ada_library_item_f_generic_stub',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_ada_library_item_f_separate = _import_func(
    'gpr_ada_library_item_f_separate',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_ada_library_item_f_main = _import_func(
    'gpr_ada_library_item_f_main',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_ada_main_f_name = _import_func(
    'gpr_ada_main_f_name',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_ada_pkg_f_has_private = _import_func(
    'gpr_ada_pkg_f_has_private',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_ada_subp_f_subp_kind = _import_func(
    'gpr_ada_subp_f_subp_kind',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_ada_prelude_f_context_clauses = _import_func(
    'gpr_ada_prelude_f_context_clauses',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_ada_prelude_f_library_item = _import_func(
    'gpr_ada_prelude_f_library_item',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_ada_separate_f_parent_name = _import_func(
    'gpr_ada_separate_f_parent_name',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_ada_with_formal_f_kind = _import_func(
    'gpr_ada_with_formal_f_kind',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_ada_with_formal_f_skips = _import_func(
    'gpr_ada_with_formal_f_skips',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_all_qualifier_p_as_bool = _import_func(
    'gpr_all_qualifier_p_as_bool',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(ctypes.c_uint8)],
    ctypes.c_int
)
_attribute_decl_f_attr_name = _import_func(
    'gpr_attribute_decl_f_attr_name',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_attribute_decl_f_attr_index = _import_func(
    'gpr_attribute_decl_f_attr_index',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_attribute_decl_f_expr = _import_func(
    'gpr_attribute_decl_f_expr',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_attribute_reference_f_attribute_name = _import_func(
    'gpr_attribute_reference_f_attribute_name',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_attribute_reference_f_attribute_index = _import_func(
    'gpr_attribute_reference_f_attribute_index',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_builtin_function_call_f_function_name = _import_func(
    'gpr_builtin_function_call_f_function_name',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_builtin_function_call_f_parameters = _import_func(
    'gpr_builtin_function_call_f_parameters',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_case_construction_f_var_ref = _import_func(
    'gpr_case_construction_f_var_ref',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_case_construction_f_items = _import_func(
    'gpr_case_construction_f_items',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_case_item_f_choice = _import_func(
    'gpr_case_item_f_choice',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_case_item_f_decls = _import_func(
    'gpr_case_item_f_decls',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_compilation_unit_f_project = _import_func(
    'gpr_compilation_unit_f_project',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_prefix_f_prefix = _import_func(
    'gpr_prefix_f_prefix',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_prefix_f_suffix = _import_func(
    'gpr_prefix_f_suffix',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_limited_node_p_as_bool = _import_func(
    'gpr_limited_node_p_as_bool',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(ctypes.c_uint8)],
    ctypes.c_int
)
_package_decl_f_pkg_name = _import_func(
    'gpr_package_decl_f_pkg_name',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_package_decl_f_pkg_spec = _import_func(
    'gpr_package_decl_f_pkg_spec',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_package_extension_f_extended_name = _import_func(
    'gpr_package_extension_f_extended_name',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_package_renaming_f_renamed_name = _import_func(
    'gpr_package_renaming_f_renamed_name',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_package_spec_f_extension = _import_func(
    'gpr_package_spec_f_extension',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_package_spec_f_decls = _import_func(
    'gpr_package_spec_f_decls',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_package_spec_f_end_name = _import_func(
    'gpr_package_spec_f_end_name',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_private_node_p_as_bool = _import_func(
    'gpr_private_node_p_as_bool',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(ctypes.c_uint8)],
    ctypes.c_int
)
_project_f_context_clauses = _import_func(
    'gpr_project_f_context_clauses',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_project_f_project_decl = _import_func(
    'gpr_project_f_project_decl',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_project_declaration_f_qualifier = _import_func(
    'gpr_project_declaration_f_qualifier',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_project_declaration_f_project_name = _import_func(
    'gpr_project_declaration_f_project_name',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_project_declaration_f_extension = _import_func(
    'gpr_project_declaration_f_extension',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_project_declaration_f_decls = _import_func(
    'gpr_project_declaration_f_decls',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_project_declaration_f_end_name = _import_func(
    'gpr_project_declaration_f_end_name',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_project_extension_f_is_all = _import_func(
    'gpr_project_extension_f_is_all',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_project_extension_f_path_name = _import_func(
    'gpr_project_extension_f_path_name',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_project_reference_f_attr_ref = _import_func(
    'gpr_project_reference_f_attr_ref',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_string_literal_at_f_str_lit = _import_func(
    'gpr_string_literal_at_f_str_lit',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_string_literal_at_f_at_lit = _import_func(
    'gpr_string_literal_at_f_at_lit',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_terms_f_terms = _import_func(
    'gpr_terms_f_terms',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_type_reference_f_var_type_name = _import_func(
    'gpr_type_reference_f_var_type_name',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_typed_string_decl_f_type_id = _import_func(
    'gpr_typed_string_decl_f_type_id',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_typed_string_decl_f_string_literals = _import_func(
    'gpr_typed_string_decl_f_string_literals',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_variable_decl_f_var_name = _import_func(
    'gpr_variable_decl_f_var_name',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_variable_decl_f_var_type = _import_func(
    'gpr_variable_decl_f_var_type',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_variable_decl_f_expr = _import_func(
    'gpr_variable_decl_f_expr',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_variable_reference_f_variable_name = _import_func(
    'gpr_variable_reference_f_variable_name',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_variable_reference_f_attribute_ref = _import_func(
    'gpr_variable_reference_f_attribute_ref',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_with_decl_f_is_limited = _import_func(
    'gpr_with_decl_f_is_limited',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)
_with_decl_f_path_names = _import_func(
    'gpr_with_decl_f_path_names',
    [ctypes.POINTER(_Entity_c_type),
     ctypes.POINTER(_Entity_c_type)],
    ctypes.c_int
)

# File readers
_dec_ref_file_reader = _import_func(
    'gpr_dec_ref_file_reader',
    [_file_reader], None
)



# Unit providers
_dec_ref_unit_provider = _import_func(
    'gpr_dec_ref_unit_provider',
    [_unit_provider], None
)



# Misc
_token_kind_name = _import_func(
   "gpr_token_kind_name",
   [ctypes.c_int], ctypes.POINTER(ctypes.c_char)
)
_token_next = _import_func(
    "gpr_token_next",
    [Token._c_type, Token._c_type], None
)
_token_is_equivalent = _import_func(
    "gpr_token_is_equivalent",
    [Token._c_type, Token._c_type], ctypes.c_int
)
_token_previous = _import_func(
    "gpr_token_previous",
    [Token._c_type, Token._c_type], None
)
_token_range_text = _import_func(
    "gpr_token_range_text",
    [Token._c_type, Token._c_type, ctypes.POINTER(_text)],
    ctypes.c_int
)
_entity_image = _import_func(
    "gpr_entity_image",
    [ctypes.POINTER(_Entity_c_type), ctypes.POINTER(_text)], None
)


#
# Layering helpers
#

def _unwrap_str(c_char_p_value: Any) -> str:
    """
    Assuming c_char_p_value is a valid char*, convert it to a native Python
    string and free the C pointer.
    """
    result = ctypes.c_char_p(ctypes.addressof(c_char_p_value.contents)).value
    _free(c_char_p_value)
    return (result or b'').decode()


_kind_to_astnode_cls = {
    1: AdaAccessSubp,
    2: AdaPragma,
    3: AdaUse,
    4: AdaWith,
    5: AdaEntityKindFunction,
    6: AdaEntityKindPackage,
    7: AdaEntityKindProcedure,
    8: AdaGeneric,
    9: AdaLibraryItem,
    10: AdaPkg,
    11: AdaPkgBody,
    12: AdaSubp,
    13: AdaPrelude,
    14: AdaSeparate,
    15: AdaSkip,
    16: AdaWithFormal,
    17: AllQualifierAbsent,
    18: AllQualifierPresent,
    19: AttributeDecl,
    20: AttributeReference,
    21: AdaContextClauseList,
    22: AdaPreludeNodeList,
    23: AdaSkipList,
    24: CaseItemList,
    25: ExprList,
    26: GprNodeList,
    27: Choices,
    28: TermList,
    29: IdentifierList,
    30: StringLiteralList,
    31: TermListList,
    32: WithDeclList,
    33: BuiltinFunctionCall,
    34: CaseConstruction,
    35: CaseItem,
    36: CompilationUnit,
    37: EmptyDecl,
    38: Prefix,
    39: Identifier,
    40: NumLiteral,
    41: StringLiteral,
    42: LimitedAbsent,
    43: LimitedPresent,
    44: OthersDesignator,
    45: PackageDecl,
    46: PackageExtension,
    47: PackageRenaming,
    48: PackageSpec,
    49: PrivateAbsent,
    50: PrivatePresent,
    51: Project,
    52: ProjectDeclaration,
    53: ProjectExtension,
    54: ProjectQualifierAbstract,
    55: ProjectQualifierAggregate,
    56: ProjectQualifierAggregateLibrary,
    57: ProjectQualifierConfiguration,
    58: ProjectQualifierLibrary,
    59: ProjectQualifierStandard,
    60: ProjectReference,
    61: StringLiteralAt,
    62: Terms,
    63: TypeReference,
    64: TypedStringDecl,
    65: VariableDecl,
    66: VariableReference,
    67: WithDecl,
}


def _field_address(struct: ctypes.Structure, field_name: str) -> int:
    """
    Get the address of a structure field from a structure value.

    For instance::

        class Foo(ctypes.Structure):
            _fields_ = [('i', ctypes.c_int)]

        f = Foo()
        i_addr =_field_address(f, 'i')
    """
    struct_type = type(struct)
    struct_addr = ctypes.addressof(struct)
    field = getattr(struct_type, field_name)
    field_type = None
    for field_desc in struct_type._fields_:
        f_name = field_desc[0]
        f_type = field_desc[1]
        if f_name == field_name:
            field_type = f_type
            break
    assert field_type is not None
    return struct_addr + field.offset

def _extract_versions() -> Tuple[str, str]:
    v_ptr = ctypes.c_char_p()
    bd_ptr = ctypes.c_char_p()
    _get_versions(ctypes.byref(v_ptr), ctypes.byref(bd_ptr))

    _version = v_ptr.value
    assert isinstance(_version, bytes)
    version = _version.decode()
    _free(v_ptr)

    _build_version = bd_ptr.value
    assert isinstance(_build_version, bytes)
    build_version = _build_version.decode()
    _free(bd_ptr)

    return version, build_version

version, build_date = _extract_versions()


#
# Language specific extensions #
#




#
# App base class
#

class App:
    """
    Base class to regroup logic for an app. We use a class so that
    specific languages implementations can add specific arguments and
    processing by overriding specific methods:

    - `main`, which will be the main method of the app.

    - `add_arguments` to add arguments to the argparse.Parser instance

    - `create_unit_provider` to return a custom unit provider to be used by the
      AnalysisContext.

    - `description` to change the description of the app.

    Inside of `main`, the user can access app specific state:

    - `self.units` is a map of filenames to analysis units.
    - `self.ctx` is the analysis context.
    - `self.u` is the last parsed unit.

    The user can then run the app by calling `App.run()`.

    Here is a small example of an app subclassing `App`, that will simply print
    the tree of every unit passed as argument:

    .. code-block:: python

        from gpr_parser import App


        class ExampleApp(App):
            def main(self):
                for u in self.units.values():
                    print u.filename
                    print u.root.dump()

        ExampleApp.run()
    """

    parser: argparse.ArgumentParser
    args: argparse.Namespace
    u: AnalysisUnit
    units: Dict[str, AnalysisUnit]
    ctx: AnalysisContext

    @property
    def description(self) -> str:
        """
        Description for this app. Empty by default.
        """
        return ""

    def __init__(self, args: Opt[List[str]] = None):
        self.parser = argparse.ArgumentParser(description=self.description)
        self.parser.add_argument('files', nargs='*', help='Files')
        self.add_arguments()

        # Parse command line arguments
        self.args = self.parser.parse_args(args)

        self.ctx = AnalysisContext(
            'utf-8', with_trivia=True,
            unit_provider=self.create_unit_provider()
        )

        # Parse files
        self.units = {}
        for file_name in self.args.files:
            self.u = self.ctx.get_from_file(file_name)
            self.units[file_name] = self.u


    def add_arguments(self) -> None:
        """
        Hook for subclasses to add arguments to self.parser. Default
        implementation does nothing.
        """
        pass

    def create_unit_provider(self) -> Opt[UnitProvider]:
        """
        Hook for subclasses to return a custom unit provider.
        Default implementation returns None.
        """
        return None

    def main(self) -> None:
        """
        Default implementation for App.main: just iterates on every units and
        call ``process_unit`` on it.
        """
        for u in sorted(self.units.values(), key=lambda u: u.filename):
            self.process_unit(u)

    def process_unit(self, unit: AnalysisUnit) -> None:
        """
        Abstract method that processes one unit. Needs to be subclassed by
        implementors.
        """
        raise NotImplementedError()

    @classmethod
    def run(cls, args: Opt[List[str]]=None) -> None:
        """
        Instantiate and run this application.
        """
        cls(args).main()

    

