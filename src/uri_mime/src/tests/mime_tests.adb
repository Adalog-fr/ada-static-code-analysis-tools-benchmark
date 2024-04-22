-- Copyright (c) 2022 nytpu <alex [at] nytpu.com>
-- SPDX-License-Identifier: MPL-2.0
-- For more license details, see LICENSE or <https://www.mozilla.org/en-US/MPL/2.0/>.

pragma Ada_2012;

with Ada.Containers;  use Ada.Containers;
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with Ahven;  use Ahven;
with MIME;  use MIME;

package body MIME_Tests is
	use Ahven.Framework;
	use MIME.Parameter_Vectors;

	overriding procedure Initialize (T : in out Parsing_Tests) is
	begin
		Set_Name(T, "MIME Parsing Tests");
		Add_Test_Routine(T, Parse_Test'Access, "Parse");
	end Initialize;

	overriding procedure Initialize (T : in out Image_Tests) is
	begin
		Set_Name(T, "MIME Image Tests");
		Add_Test_Routine(T, Image_Test'Access, "Image");
	end Initialize;

	procedure Parse_Test is
		function T_S (Source : Unbounded_String) return String
			renames To_String;

		procedure Assert
			(Left : Unbounded_String; Right : String; Addtl : String := "")
		is
			L : constant String := T_S(Left);
		begin
			Assert(L = Right, Addtl & " " & L);
		end Assert;

		O : MIME_Type;
	begin
		O := Parse("text/plain");
		Assert(O.General, "text", "text/plain: invalid general");
		Assert(O.Specific, "plain", "text/plain: invalid specific");
		Assert(Length(O.Suffix) = 0, "text/plain: invalid suffix " & T_S(O.Suffix));
		Assert(Length(O.Parameters) = 0, "text/plain: invalid parameters");

		O := Parse("image/svg+xml");
		Assert(O.General, "image", "image/svg+xml: invalid general");
		Assert(O.Specific, "svg", "image/svg+xml: invalid specific");
		Assert(O.Suffix, "xml", "image/svg+xml: invalid suffix");
		Assert(Length(O.Parameters) = 0, "image/svg+xml: invalid parameters");

		O := Parse("image/png;x-test=""foo bar""");
		Assert(O.General, "image", "image/png;x-test=""foo bar"": invalid general");
		Assert(O.Specific, "png", "image/png;x-test=""foo bar"": invalid specific");
		Assert(Length(O.Suffix) = 0, "image/png;x-test=""foo bar"": invalid suffix " & T_S(O.Suffix));
		Assert(Length(O.Parameters) = 1, "image/png;x-test=""foo bar"": invalid parameters");
		Assert(O.Parameters(1).Name, "x-test", "image/png;x-test=""foo bar"": invalid parameter name");
		Assert(O.Parameters(1).Value, "foo bar", "image/png;x-test=""foo bar"": invalid parameter value");

		O := Parse("application/atom+xml; charset=utf-8; x-example = ""hello, world!""");
		Assert(O.General, "application", "application/atom+xml; charset=utf-8; x-example = ""hello, world!"": invalid general");
		Assert(O.Specific, "atom", "application/atom+xml; charset=utf-8; x-example = ""hello, world!"": invalid specific");
		Assert(O.Suffix, "xml", "application/atom+xml; charset=utf-8; x-example = ""hello, world!"": invalid suffix");
		Assert(Length(O.Parameters) = 2, "application/atom+xml; charset=utf-8; x-example = ""hello, world!"": invalid parameters");
		Assert(O.Parameters(1).Name, "charset", "application/atom+xml; charset=utf-8; x-example = ""hello, world!"": invalid parameter name");
		Assert(O.Parameters(1).Value, "utf-8", "application/atom+xml; charset=utf-8; x-example = ""hello, world!"": invalid parameter value");
		Assert(O.Parameters(2).Name, "x-example", "application/atom+xml; charset=utf-8; x-example = ""hello, world!"": invalid parameter name");
		Assert(O.Parameters(2).Value, "hello, world!", "application/atom+xml; charset=utf-8; x-example = ""hello, world!"": invalid parameter value");

		begin
			O := Parse("hi");
			Fail("hi: successful parse");
		exception
			when Invalid => null;
		end;

		begin
			O := Parse("text/ spaces ");
			Fail("text/ spaces : successful parse");
		exception
			when Invalid => null;
		end;

		begin
			O := Parse("text/@");
			Fail("text/@: successful parse");
		exception
			when Invalid => null;
		end;

		begin
			O := Parse("text/plain;charset");
			Fail("text/plain;charset: successful parse");
		exception
			when Invalid => null;
		end;
	end Parse_Test;

	procedure Image_Test is
		function T_US (Source : String) return Unbounded_String
			renames To_Unbounded_String;

		M : MIME_Type;
	begin
		M := (T_US("text"), T_US("plain"), Null_Unbounded_String, Empty_Vector);
		Assert(Image(M) = "text/plain", "text/plain");

		M := (T_US("image"), T_US("svg"), T_US("xml"), Empty_Vector);
		Assert(Image(M) = "image/svg+xml", "image/svg+xml");

		M := (
			T_US("image"), T_US("png"), Null_Unbounded_String,
			Empty_Vector & (T_US("x-test"), T_US("foo bar"))
		);
		Assert(Image(M) = "image/png;x-test=""foo bar""", "image/png;x-test=""foo bar""");

		M := (
			T_US("application"), T_US("atom"), T_US("xml"),
			Empty_Vector &
				(T_US("charset"), T_US("utf-8")) &
				(T_US("x-example"), T_US("hello, world!"))
		);
		Assert(Image(M) = "application/atom+xml;charset=utf-8;x-example=""hello, world!""", "application/atom+xml;charset=utf-8;x-example=""hello, world!""");
	end Image_Test;

end MIME_Tests;
