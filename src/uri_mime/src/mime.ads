-- Parse a MIME/media type according to RFC 2045 §5
-- <https://datatracker.ietf.org/doc/html/rfc2045#section-5>
--
-- This implements a *reasonable subset* of the MIME specification.  For
-- instance, quoted strings may not be "folded" accoding to RFC 822 in this
-- implementation.  However this should cover... just about any MIME type that
-- a reasonable person would generate, but won't tolerate technically-compliant
-- but mangled MIME types.
--
--
-- Copyright (c) 2022 nytpu <alex [at] nytpu.com>
-- SPDX-License-Identifier: MPL-2.0
-- For more license details, see LICENSE or <https://www.mozilla.org/en-US/MPL/2.0/>.

pragma Ada_2012;

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;

package MIME is
	pragma Preelaborate;

	-- A single parameter in a MIME type, separated from the base type with ";"
	type Parameter is record
		Name : Unbounded_String;
		Value : Unbounded_String;
	end record;

	-- Used to hold a list of Parameters, in the order they appear in the MIME
	-- type.
	package Parameter_Vectors is new Ada.Containers.Vectors
		(Index_Type => Positive, Element_Type => Parameter);
	use Parameter_Vectors;

	-- A parsed MIME type.
	-- Example type, doc comments show which part of the record hold which part
	-- of the parsed type:
	-- "image/svg+xml;charset=utf-8;x-example="hello, world!""
	type MIME_Type is record
		General : Unbounded_String; -- "image"
		Specific : Unbounded_String; -- "svg"
		Suffix : Unbounded_String; -- "xml"
		-- Parameters(1) = ("charset", "utf-8");
		-- Parameters(2) = ("x-example", "hello, world!");
		Parameters : Vector;
	end record;

	-- Parse a MIME type string into a MIME_Type record
	function Parse (M : String) return MIME_Type
		with
			Pre => M'Length > 0,
			Post =>
				Length(Parse'Result.General) > 0 and
				Length(Parse'Result.Specific) > 0;

	function Image (M : MIME_Type) return String;

	-- Raised when a MIME type being parsed is invalid.
	-- The exception message will contain more details about the error.
	Invalid : exception;


private
	function Type_Valid (S : Unbounded_String) return Boolean;
	function Parse_Params (P : String) return Vector;

end MIME;
