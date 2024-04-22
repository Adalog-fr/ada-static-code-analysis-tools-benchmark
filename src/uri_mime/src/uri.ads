-- RFC-compliant URI parser and manipulator library
-- RFC 3986: <https://datatracker.ietf.org/doc/html/rfc3986>
-- RFC 3987: <https://datatracker.ietf.org/doc/html/rfc3987>
--
--
-- Copyright (c) 2022 nytpu <alex [at] nytpu.com>
-- SPDX-License-Identifier: MPL-2.0
-- For more license details, see LICENSE or <https://www.mozilla.org/en-US/MPL/2.0/>.

pragma Ada_95;

with Ada.Strings.Maps;  use Ada.Strings.Maps;
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;

package URI is

	-- TCP Port Number
	type Port_Number is range 0 .. 65535;

	-- Stores a parsed URI.
	-- The *_Present booleans are used to enable/disable an element, even if
	-- the string is empty.  (e.g. if Query_Present = True and Length(Query) =
	-- 0, then the URI will have an empty query.  If Query_Present = False then
	-- the URI will have no query no matter what the contents of Query is).
	-- According to RFC 3986 the path MUST be present, however it may be the
	-- empty string.
	type URL is record
		Scheme_Present : Boolean := False;
		Scheme : Unbounded_String;

		Authority_Present : Boolean := False;
		User_Info_Present : Boolean := False;
		User_Info : Unbounded_String;
		Host : Unbounded_String;
		Port_Present : Boolean := False;
		Port : Port_Number := 0;

		Path : Unbounded_String;

		Query_Present : Boolean := False;
		Query : Unbounded_String;

		Fragment_Present : Boolean := False;
		Fragment : Unbounded_String;
	end record;

	-- Parse an absolute URI string into a URL record.  The Path, Query, and
	-- Fragment components will be percent-decoded.
	function Parse (S : String) return URL;

	-- Parse a URI string relative to an existing URL record.  The Path, Query,
	-- and Fragment components will be percent-decoded.
	-- This will, for example, resolve relative paths into an absolute URI
	-- using the existing scheme and authority in the given URL record, while
	-- overwriting the Path portion.  This will use Normalize_Path on both
	-- paths prior to resolving.
	-- RFC 3986 § 5.2.2 describes the algorithm used for Strict and Non-Strict:
	-- <https://datatracker.ietf.org/doc/html/rfc3986#section-5.2.2>
	function Parse_Relative
		(Base : URL; Relative : String; Strict : Boolean := True) return URL;

	-- Convert a set of URL components to a syntactically-correct URI string.
	-- Will NOT percent encode the Path and Query parts.
	function Image (U : URL) return String;

	-- By default Parse and Parse_Relative are very strict when parsing in a
	-- way not suitable for typical hand-typed URIs.  For example
	-- `example.com/hello-world` will be parsed into a URL with no authority
	-- and a path of `example.com/hello-world`.  Host_Heuristic attempts to
	-- modify an already parsed URL with a heuristic for determining the host,
	-- such that a URL with no Authority but with a Path of
	-- `example.com/hello-world` will be modified to have a Host of
	-- `example.com` and a Path of `/hello-world`.  If the given URL already
	-- has an authority, then the returned URL will be identical. The procedure
	-- variant will set Modified to True if the URL was modified, or False
	-- otherwise.
	--
	-- The heuristic works as such:
	-- 1. Ensure that the URL has no current Authority or Host component.
	-- 2. Locate the first '/' of of the Path, or the end of the Path if no '/'
	--    is present.
	-- 3. Test if Path(Path'First .. Slash_Index_Or_End) contains a '.'.
	-- 4. If it does, take the first path segment (or the entire path if there
	--    is only one segement) and move it to the Host, and set Path to the
	--    remainder.
	function Host_Heuristic (U : URL) return URL;
	procedure Host_Heuristic (U : in out URL; Modified : out Boolean);

	-- Canonicalize a path.
	-- Does the following:
	-- 1. Condense consecutive slashes into a single slash
	-- 2. Eliminate all . path name elements
	-- 3. Eliminate all .. path name elements, as well as the non-root, non-..
	--    element that preceeds it
	-- 4. Eliminate .. from the root
	--    (i.e. "/.." => "/" at the beginning of the path)
	function Normalize_Path (P : String) return String;
	function Normalize_Path (S : Unbounded_String) return Unbounded_String;

	-- Merge two paths together.
	-- If Relative is an absolute path (begins with '/') then it will be
	-- returned.  If Base'Length = 0 and Authority_Present = True, then return
	-- Relative with a '/' prepended.  Otherwise, return a string consisting of
	-- the relative appended to all but the last segment of the base path
	-- (i.e., excluding any characters after the right-most "/" ii the base
	-- path, or excluding the entire base path if it does not contain any "/"
	-- characters).
	function Merge_Paths
		(Base, Relative : String; Authority_Present : Boolean := True)
		return String;
	function Merge_Paths
		(Base, Relative : Unbounded_String; Authority_Present : Boolean := True)
		return Unbounded_String;

	-- Percent encode a string, by default with only the base set of A-Z, a-z,
	-- 0-9, '-', '_', '.', & '~' unreserved.
	-- If Additional_Unreserved is passed, then those characters will be
	-- written literally rather than being percent-encoded.
	-- If Query_Encoding is set, then '+' rather than "%20" will be used to
	-- encode spaces. For non-HTTP URIs Query_Encoding should never be set
	-- without checking the protocol's specification first.
	function Percent_Encode
		(S: String;
		Additional_Unreserved : Character_Set := Null_Set;
		Query_Encoding : Boolean := False)
		return String;
	function Percent_Encode
		(S: Unbounded_String;
		Additional_Unreserved : Character_Set := Null_Set;
		Query_Encoding : Boolean := False)
		return Unbounded_String;

	-- Equivalent to Percent_Encode(S, Additional_Unreserved => To_Set('/'));
	function Percent_Encode_Path (S : String) return String;
	function Percent_Encode_Path (S : Unbounded_String) return Unbounded_String;

	-- Equivalent to Percent_Encode(S, Additional_Unreserved => To_Set("=&;"), Query_Encoding => True);
	function Percent_Encode_Query (S : String) return String;
	function Percent_Encode_Query (S : Unbounded_String) return Unbounded_String;

	-- Decode any percent-encoded string.
	function Percent_Decode (S : String) return String;
	function Percent_Decode (S : Unbounded_String) return Unbounded_String;

	-- TODO
	-- function Punycode_Encode (S : String) return String;
	-- function Punycode_Encode (S : Unbounded_String) return Unbounded_String;

	-- function Punycode_Decode (S : String) return String;
	-- function Punycode_Decode (S : Unbounded_String) return Unbounded_String;

	-- A percent-encoded string being decoded was malformed
	Malformed_Percent_Encoded_String : exception;

	-- The given URI componenent was invalid
	Invalid_Scheme : exception;

end URI;
