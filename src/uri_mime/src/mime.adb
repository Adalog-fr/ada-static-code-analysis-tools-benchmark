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

with Ada.Strings;
with Ada.Strings.Fixed;  use Ada.Strings.Fixed;
with Ada.Strings.Maps;  use Ada.Strings.Maps;

package body MIME is

	-----------
	-- Parse --
	-----------

	function Parse (M : String) return MIME_Type is
		O : MIME_Type;

		Split : Natural := Index(M, ";");
		Base_Type : Unbounded_String;
	begin
		if Split = 0 then
			Base_Type := To_Unbounded_String(M);
			O.Parameters := Empty_Vector;
		else
			if Split <= M'First then
				raise Invalid with "media type may not be empty";
			elsif Split >= M'Last then
				raise Invalid with "parameter list may not be empty";
			end if;
			Base_Type := To_Unbounded_String(M(M'First .. Split - 1));
			O.Parameters := Parse_Params(M(Split + 1 .. M'Last));
		end if;

		Split := Index(Base_Type, "/");
		if Split <= 1 then
			raise Invalid with "type may not be empty";
		elsif Split >= M'Last then
			raise Invalid with "subtype may not be empty";
		end if;
		O.General := Unbounded_Slice(Base_Type, 1, Split - 1);
		O.Specific := Unbounded_Slice(Base_Type, Split + 1, Length(Base_Type));

		O.Suffix := Null_Unbounded_String;
		Split := Index(O.Specific, "+");
		if Split /= 0 then
			if Split <= 1 then
				raise Invalid with "subtype may not be empty";
			elsif Split >= Length(O.Specific) then
				raise Invalid with "suffix may not be empty";
			end if;
			O.Suffix :=
				Unbounded_Slice(O.Specific, Split + 1, Length(O.Specific));
			O.Specific := Unbounded_Slice(O.Specific, 1, Split - 1);
		end if;

		if not Type_Valid(O.General) then
			raise Invalid with "invalid character in general " & To_String(O.General);
		elsif not Type_Valid(O.Specific) then
			raise Invalid with "invalid character in general " & To_String(O.Specific);
		elsif not Type_Valid(O.Suffix) then
			raise Invalid with "invalid character in general " & To_String(O.Suffix);
		end if;

		return O;
	end Parse;


	-----------
	-- Image --
	-----------

	function Image (M : MIME_Type) return String is
		O : Unbounded_String;
	begin
		O := M.General & "/" & M.Specific;
		if Length(M.Suffix) > 0 then
			O := O & "+" & M.Suffix;
		end if;
		for P of M.Parameters loop
			O := O & ";" & P.Name & "=";
			if Type_Valid(P.Value) then
				Append(O, P.Value);
			else
				O := O & """" & P.Value & """";
			end if;
		end loop;
		return To_String(O);
	end Image;


	----------------
	-- Type_Valid --
	----------------

	function Type_Valid (S : Unbounded_String) return Boolean is
		Invalid_Chars : constant Character_Set :=
			To_Set(" ()<>@,;:\""/[]?=") or
			To_Set(Character_Range'(ASCII.NUL, ASCII.US)) or
			To_Set(ASCII.DEL);
	begin
		return (if Index(S, Invalid_Chars) /= 0 then False else True);
	end Type_Valid;



	------------------
	-- Parse_Params --
	------------------

	function Parse_Params (P : String) return Vector is
		use Ada.Strings;

		-----------------
		-- Parse_Value --
		-----------------

		function Parse_Quoted
			(Value : Unbounded_String) return Unbounded_String
		is
			Quote_Toks : constant Character_Set := To_Set('"');
			O : Unbounded_String;
		begin
			O := Trim(Value, Quote_Toks, Quote_Toks);
			if
				Index(O, "\") /= 0 or
				Index(O, "" & ASCII.CR) /= 0 or
				Index(O, "" & ASCII.LF) /= 0
			then
				raise Invalid with "folded parameters not supported";
			end if;
			return O;
		end Parse_Quoted;


		Separator_Tok : constant Character_Set := To_Set(';');

		Start : Positive := P'First;
		Finish : Natural := 0;
		O : Vector := Empty_Vector;
	begin
		while Start <= P'Last loop
			Find_Token(P, Separator_Tok, Start, Outside, Start, Finish);
			exit when Start > Finish;

			if P(Start .. Finish)'Length = 0 then
				raise Invalid with "parameter may not be empty";
			end if;

			declare
				Param : constant String := P(Start .. Finish);
				Split : constant Natural := Index(Param, "=");
				Value : Unbounded_String;
				Rec : Parameter;
			begin
				if
					Split = 0 or Split <= Param'First or Split >= Param'Last
				then
					raise Invalid
						with "parameter must be of format attribute=value";
				end if;

				Rec.Name :=
					To_Unbounded_String(Trim(Param(Param'First .. Split - 1), Both));
				Value :=
					To_Unbounded_String(Trim(Param(Split + 1 .. Param'Last), Both));
				if Length(Rec.Name) = 0 or Length(Value) = 0 then
					raise Invalid with "name and value must have nonzero length";
				elsif not Type_Valid(Rec.Name) then
					raise Invalid
						with "invalid character in name " & To_String(Rec.Name);
				end if;

				if Head(Value, 1) = """" then
					Rec.Value := Parse_Quoted(Value);
				else
					if not Type_Valid(Value) then
						raise Invalid
							with "invalid character in value " & To_String(Value);
					end if;
					Rec.Value := Value;
				end if;

				Append(O, Rec);
			end;

			Start := Finish + 1;
		end loop;
		return O;
	end Parse_Params;

end MIME;
