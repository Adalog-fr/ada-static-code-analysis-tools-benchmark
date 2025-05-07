-- RFC-compliant URI parser and manipulator library
-- RFC 3986: <https://datatracker.ietf.org/doc/html/rfc3986>
-- RFC 3987: <https://datatracker.ietf.org/doc/html/rfc3987>
--
--
-- Copyright (c) 2022 nytpu <alex [at] nytpu.com>
-- SPDX-License-Identifier: MPL-2.0
-- For more license details, see LICENSE or <https://www.mozilla.org/en-US/MPL/2.0/>.

pragma Ada_2012;

with Ada.Containers;
with Ada.Containers.Indefinite_Vectors;
with Ada.Text_IO;
with Ada.Strings;
with Ada.Strings.Fixed;  use Ada.Strings.Fixed;

package body URI is

	-------------
	-- Parsing --
	-------------

	function Parse (S : String) return URL is
		O : URL := (
			Scheme_Present | Authority_Present | User_Info_Present |
				Port_Present | Query_Present | Fragment_Present => False,
			Port => 0,
			others => Null_Unbounded_String
		);
		I_Start : Natural := S'First;
		I_End, Sav : Natural := 0;
	begin
		-- <https://datatracker.ietf.org/doc/html/rfc3986#section-5.2> &
		-- <https://datatracker.ietf.org/doc/html/rfc3986#section-3> &
		-- <https://datatracker.ietf.org/doc/html/rfc3986#appendix-B>

		-- Scheme
		I_End := Index(S, To_Set(":/?#"), S'First);
		if I_End /= 0 and then S(I_End) = ':' then
			declare
				-- I points at the punctuation terminating the scheme
				Scheme : constant String := S(I_Start .. I_End - 1);
			begin
				if
					Index(
						Scheme,
						not (To_Set(Ranges => (('A', 'Z'), ('a', 'z'), ('0', '9'))) or To_Set("+-.")),
						S'First
					) /= 0
				then
					raise Invalid_Scheme with "Invalid character in '" & Scheme & "'";
				end if;
				O.Scheme_Present := True;
				O.Scheme := To_Unbounded_String(Scheme);
			end;
		else
			I_End := S'First;
		end if;

		-- Authority
		I_Start := Index(S, "//", I_End);
		if I_Start /= 0 then
			-- skip the "//"
			I_Start := I_Start + 2;

			I_End := Index(S, To_Set("/?#"), I_Start);
			if I_End /= 0 then
				Sav := I_End;
				I_End := I_End - 1;
			else
				I_End := S'Last;
				Sav := 0;
			end if;

			O.Authority_Present := True;
			declare
				Authority : constant String := S(I_Start .. I_End);
			begin
				I_Start := Authority'First;
				I_End := Index(Authority, "@", I_Start);
				if I_End /= 0 then
					O.User_Info_Present := True;
					O.User_Info := To_Unbounded_String(Authority(I_Start .. I_End - 1));
					I_Start := I_End + 1;
				end if;

				I_End := Index(Authority, To_Set(":]"), Authority'Last, Going => Ada.Strings.Backward);
				if I_End /= 0 and then Authority(I_End) = ':' then
					O.Port_Present := True;
					O.Port := Port_Number'Value(Authority(I_End + 1 .. Authority'Last));
					I_End := I_End - 1;
				else
					I_End := Authority'Last;
				end if;

				O.Host := To_Unbounded_String(Authority(I_Start .. I_End));
			end;
		else
			Sav := (if I_End < S'Last and then S(I_End) = ':' then I_End + 1 else I_End);
		end if;

		-- Path
		if Sav = 0 then
			Sav := S'Last;
			O.Path := Null_Unbounded_String;
		else
			I_Start := Sav;
			I_End := Index(S, To_Set("?#"), I_Start);
			I_End := (if I_End /= 0 then I_End - 1 else S'Last);
			O.Path := To_Unbounded_String(Percent_Decode(S(I_Start .. I_End)));
		end if;
		I_Start := Sav;

		-- Query
		I_Start := Index(S, "?", I_Start);
		if I_Start /= 0 then
			I_Start := I_Start + 1;
			I_End := Index(S, "#", I_Start);
			I_End := (if I_End /= 0 then I_End - 1 else S'Last);
			O.Query_Present := True;
			O.Query := To_Unbounded_String(Percent_Decode(S(I_Start .. I_End)));
		else
			I_Start := Sav;
		end if;

		-- Fragment
		I_Start := Index(S, "#", I_Start);
		if I_Start /= 0 then
			I_Start := I_Start + 1;
			I_End := S'Last;
			O.Fragment_Present := True;
			O.Fragment := To_Unbounded_String(Percent_Decode(S(I_Start .. I_End)));
		end if;

		return O;
	end Parse;

	function Parse_Relative
		(Base : URL; Relative : String; Strict : Boolean := True) return URL
	is
		R : URL := Parse(Relative);
		O : URL := (
			Scheme_Present | Authority_Present | User_Info_Present |
				Port_Present | Query_Present | Fragment_Present => False,
			Port => 0,
			others => Null_Unbounded_String
		);
	begin
		-- <https://datatracker.ietf.org/doc/html/rfc3986#section-5.2.2>
		if not Strict and R.Scheme_Present and R.Scheme = Base.Scheme then
			R.Scheme_Present := False;
		end if;

		O.Fragment_Present := R.Fragment_Present;
		O.Fragment := R.Fragment;

		if R.Scheme_Present then
			O := R;
			O.Path := Normalize_Path(O.Path);
		else
			O.Scheme_Present := Base.Scheme_Present;
			O.Scheme := Base.Scheme;
			if R.Authority_Present then
				O.Authority_Present := R.Authority_Present;
				O.User_Info_Present := R.User_Info_Present;
				O.User_Info := R.User_Info;
				O.Host := R.Host;
				O.Port_Present := R.Port_Present;
				O.Port := R.Port;

				O.Path := Normalize_Path(R.Path);

				O.Query_Present := R.Query_Present;
				O.Query := R.Query;
			else
				O.Authority_Present := Base.Authority_Present;
				O.User_Info_Present := Base.User_Info_Present;
				O.User_Info := Base.User_Info;
				O.Host := Base.Host;
				O.Port_Present := Base.Port_Present;
				O.Port := Base.Port;
				if Length(R.Path) = 0 then
					O.Path := Normalize_Path(Base.Path);
					if R.Query_Present then
						O.Query_Present := R.Query_Present;
						O.Query := R.Query;
					else
						O.Query_Present := Base.Query_Present;
						O.Query := Base.Query;
					end if;
				else
					O.Query_Present := R.Query_Present;
					O.Query := R.Query;
					-- No need to check if R.Path starts with '/' since
					-- Merge_Paths does that for us.
					O.Path := Merge_Paths(Base.Path, R.Path);
					O.Path := Normalize_Path(O.Path);
				end if;
			end if;
		end if;

		return O;
	end Parse_Relative;

	-----------
	-- Image --
	-----------

	function Image (U : URL) return String is
		O : Unbounded_String;
	begin
		-- <https://datatracker.ietf.org/doc/html/rfc3986#section-5.3>
		if U.Scheme_Present then
			Append(O, U.Scheme & ":");
		end if;

		if U.Authority_Present then
			Append(O, "//");
			if U.User_Info_Present then
				Append(O, U.User_Info & "@");
			end if;
			Append(O, U.Host);
			if U.Port_Present then
				-- Do this runaround to remove leading space from U.Port'Image
				declare
					Port_String : constant String := U.Port'Image;
				begin
					Append(O, ":" & Port_String(Port_String'First + 1 .. Port_String'Last));
				end;
			end if;
		end if;

		Append(O, To_String(U.Path));

		if U.Query_Present then
			Append(O, "?" & To_String(U.Query));
		end if;

		if U.Fragment_Present then
			Append(O, "#" & U.Fragment);
		end if;

		return To_String(O);
	end Image;


	--------------------
	-- Host_Heuristic --
	--------------------

	function Host_Heuristic (U : URL) return URL is
		Dummy : Boolean;
		N : URL := U;
	begin
		Host_Heuristic(N, Dummy);
		pragma Unreferenced(Dummy);
		return N;
	end Host_Heuristic;

	procedure Host_Heuristic (U : in out URL; Modified : out Boolean) is
		Right : Natural :=
			Index(U.Path, Ada.Strings.Maps.To_Set('/'));
		Idx : Natural;
	begin
		Modified := False;
		if not U.Authority_Present then
			Right :=
				(if Right = 0
					then Length(U.Path)
					else Right - 1);
			if Right > 0 then
				Idx := Index(
					Unbounded_Slice(U.Path, 1, Right),
					Ada.Strings.Maps.To_Set('.')
				);
				if Idx /= 0 then
					U.Authority_Present := True;
					U.Host := Unbounded_Slice(U.Path, 1, Right);
					U.Path :=
						(if Right + 1 <= Length(U.Path)
							then Unbounded_Slice(U.Path, Right + 1, Length(U.Path))
							else Null_Unbounded_String);
					Modified := True;
				end if;
			end if;
		end if;
	end Host_Heuristic;


	------------------
	-- Path Munging --
	------------------

	function Normalize_Path (S : Unbounded_String) return Unbounded_String
		is (To_Unbounded_String(Normalize_Path(To_String(S))));

	function Normalize_Path (P : String) return String is
		package String_Vectors is new Ada.Containers.Indefinite_Vectors (Positive, String);
		use Ada.Containers;
		use String_Vectors;

		-- Split a string into a vector of strings, breaking on the given
		-- tokens
		procedure Tokenize
			(V : out Vector; Is_Root, Is_Trailing : out Boolean;
			S : String; Toks : Character_Set)
		is
			use Ada.Strings;

			Start : Positive := S'First;
			Finish : Natural := 0;
		begin
			Is_Root :=
				(if S'Length > 0 and then S(S'First) = '/' then True else False);
			Is_Trailing :=
				(if
					(S'Length >= 1 and then S(S'Last) = '/') or
					(S'Length >= 2 and then S(S'Last - 1 .. S'Last) = "/.") or
					(S'Length >= 3 and then S(S'Last - 2 .. S'Last) = "/..")
				then True else False);
			V := Empty_Vector;
			while Start <= S'Last loop
				Find_Token(S, Toks, Start, Outside, Start, Finish);
				exit when Start > Finish;
				if S(Start .. Finish)'Length /= 0 then
					V.Append(S(Start .. Finish));
				end if;
				Start := Finish + 1;
			end loop;
		end Tokenize;

		function Image (V : Vector; Is_Root, Is_Trailing : Boolean) return String is
			O : Unbounded_String;
			First : Boolean := True;
		begin
			if Is_Root then
				Append(O, "/");
			end if;
			for E of V loop
				if First then
					Append(O, E);
					First := False;
				else
					Append(O, "/" & E);
				end if;
			end loop;
			if Is_Trailing and Length(V) /= 0 then
				Append(O, "/");
			end if;
			return To_String(O);
		end Image;

		Split : Vector;
		Is_Root, Is_Trailing : Boolean;
		O : Vector := Empty_Vector;
	begin
		-- <https://datatracker.ietf.org/doc/html/rfc3986#section-5.2.4>
		-- as well as Go's path.Clean() function.
		Tokenize(Split, Is_Root, Is_Trailing, P, To_Set('/'));

		for C in Split.Iterate loop
			if Split(C) = "." then
				null;
			elsif Split(C) = ".." then
				if Length(O) > 0 then
					Delete(O, O.Last_Index);
				end if;
			else
				Append(O, Split(C));
			end if;
		end loop;
		if not Is_Root and Length(O) = 0 then
			Is_Trailing := False;
		end if;
		return Image(O, Is_Root, Is_Trailing);
	end Normalize_Path;

	function Merge_Paths
		(Base, Relative : Unbounded_String; Authority_Present : Boolean := True)
		return Unbounded_String
	is (To_Unbounded_String(Merge_Paths(To_String(Base), To_String(Relative))));

	function Merge_Paths
		(Base, Relative : String; Authority_Present : Boolean := True)
		return String
	is
		Base_Component : Natural := 0;
	begin
		-- <https://datatracker.ietf.org/doc/html/rfc3986#section-5.2.3>
		if Relative'Length > 0 and then Relative(Relative'First) = '/' then
			return Relative;
		end if;
		if Authority_Present and Base'Length = 0 then
			return Normalize_Path("/" & Relative);
		end if;

		Base_Component := Index(Base, "/", Base'Last, Going => Ada.Strings.Backward);
		if Base_Component /= 0 then
			return Normalize_Path(Base(Base'First .. Base_Component) & Relative);
		end if;
		return Normalize_Path(Relative);
	end Merge_Paths;

	----------------------
	-- Percent Encoding --
	----------------------
	function Percent_Encode
		(S: Unbounded_String;
		Additional_Unreserved : Character_Set := Null_Set;
		Query_Encoding : Boolean := False)
		return Unbounded_String
	is (To_Unbounded_String(Percent_Encode(To_String(S), Additional_Unreserved, Query_Encoding)));

	type Character_Value is range 16#00# .. 16#FF#;

	function Percent_Encode
		(S: String;
		Additional_Unreserved : Character_Set := Null_Set;
		Query_Encoding : Boolean := False)
		return String
	is
		-- guarantee we'll never overflow the hex string
		package Character_Value_IO is new Ada.Text_IO.Integer_IO(Character_Value);
		use Character_Value_IO;

		-- <https://datatracker.ietf.org/doc/html/rfc3986#section-2.3>
		Unreserved : constant Character_Set :=
			To_Set(Ranges => (('A', 'Z'), ('a', 'z'), ('0', '9'))) or
			To_Set("-_.~") or
			Additional_Unreserved;

		O : Unbounded_String;
	begin
		for C of S loop
			if Is_In(C, Unreserved) then
				Append(O, C);
			elsif Query_Encoding and C = ' ' then
				Append(O, '+');
			else
				declare
					Char : constant Character_Value := Character'Pos(C);
					-- Long enough to hold "16#XX#"
					Hex_String : String(1..6);
				begin
					Put(Hex_String, Char, 16);
					if Char < 16#10# then
						-- Hex_String will be " 16#X#" for one-digit values so
						-- add a leading zero
						Hex_String(4) := '0';
					end if;
					-- Slice out solely "XX"
					Append(O, "%" & Hex_String(4..5));
				end;
			end if;
		end loop;
		return To_String(O);
	end Percent_Encode;

	function Percent_Encode_Path (S : Unbounded_String) return Unbounded_String is
		(To_Unbounded_String(Percent_Encode_Path(To_String(S))));

	function Percent_Encode_Path (S : String) return String is
		(Percent_Encode(S, Additional_Unreserved => To_Set('/')));

	function Percent_Encode_Query (S : Unbounded_String) return Unbounded_String
		is (To_Unbounded_String(Percent_Encode_Query(To_String(S))));

	function Percent_Encode_Query (S : String) return String is
		(Percent_Encode(S, Additional_Unreserved => To_Set("=&;"),
			Query_Encoding => True));

	function Percent_Decode (S : Unbounded_String) return Unbounded_String is
		(To_Unbounded_String(Percent_Decode(To_String(S))));

	function Percent_Decode (S : String) return String is
		O : Unbounded_String;
		I : Integer := S'First;
	begin
		while I <= S'Last loop
			if S(I) = '%' then
				if I + 2 > S'Last then
					return raise Malformed_Percent_Encoded_String
						with "Too few hex digits in '" & S(I .. S'Last) & "'";
				end if;
				Append(O, Character'Val(Character_Value'Value(
					"16#" & S(I + 1 .. I + 2) & "#")
				));
				I := I + 3;
			elsif S(I) = '+' then
				Append(O, ' ');
				I := I + 1;
			else
				Append(O, S(I));
				I := I + 1;
			end if;
		end loop;
		return To_String(O);
	end Percent_Decode;

	----------------
	-- Punycoding --
	----------------

-- 	function Punycode_Encode (S : Unbounded_String) return Unbounded_String is
-- 		(To_Unbounded_String(Punycode_Encode(To_String(S))));

-- 	function Punycode_Encode (S : String) return String is
-- 	begin
-- 		pragma Unreferenced(S);
-- 		pragma Compile_Time_Warning(Standard.True, "Punycode_Encode unimplemented");
-- 		return raise Program_Error with "Unimplemented function Punycode_Encode";
-- 	end Punycode_Encode;

-- 	function Punycode_Decode (S : Unbounded_String) return Unbounded_String is
-- 		(To_Unbounded_String(Punycode_Decode(To_String(S))));

-- 	function Punycode_Decode (S : String) return String is
-- 	begin
-- 		pragma Unreferenced(S);
-- 		pragma Compile_Time_Warning(Standard.True, "Punycode_Decode unimplemented");
-- 		return raise Program_Error with "Unimplemented function Punycode_Decode";
-- 	end Punycode_Decode;

end URI;
