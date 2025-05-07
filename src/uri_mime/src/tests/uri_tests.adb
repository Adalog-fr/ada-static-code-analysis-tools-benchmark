-- Copyright (c) 2022 nytpu <alex [at] nytpu.com>
-- SPDX-License-Identifier: MPL-2.0
-- For more license details, see LICENSE or <https://www.mozilla.org/en-US/MPL/2.0/>.

pragma Ada_2012;

with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with Ahven;  use Ahven;
with URI;  use URI;

package body URI_Tests is
	use Ahven.Framework;

	overriding procedure Initialize (T : in out Parsing_Tests) is
	begin
		Set_Name(T, "Parsing Tests");
		Add_Test_Routine(T, Parse_Test'Access, "Parse");
		Add_Test_Routine(T, Parse_Relative_Test'Access, "Parse_Relative");
		Add_Test_Routine(T, Host_Heuristic_Test'Access, "Host_Heuristic");
	end Initialize;

	overriding procedure Initialize (T : in out Image_Tests) is
	begin
		Set_Name(T, "Image Tests");
		Add_Test_Routine(T, Image_Test'Access, "Image");
	end Initialize;

	overriding procedure Initialize (T : in out Path_Tests) is
	begin
		Set_Name(T, "Path Munging Tests");
		Add_Test_Routine(T, Normalize_Path_Test'Access, "Normalize_Path");
		Add_Test_Routine(T, Merge_Paths_Test'Access, "Merge_Paths");
	end Initialize;

	overriding procedure Initialize (T : in out Percent_Encoding_Tests) is
	begin
		Set_Name(T, "Percent Encoding Tests");
		Add_Test_Routine(T, Percent_Decode_Test'Access, "Percent_Decode");
		Add_Test_Routine(T, Percent_Encode_Test'Access, "Percent_Encode");
		Add_Test_Routine(T, Percent_Encode_Path_Test'Access, "Percent_Encode_Path");
		Add_Test_Routine(T, Percent_Encode_Query_Test'Access, "Percent_Encode(Query_Encoding => True)");
		Add_Test_Routine(T, Percent_Encode_Full_Query_Test'Access, "Percent_Encode_Query");
	end Initialize;

-- 	overriding procedure Initialize (T : in out Punycoding_Tests) is
-- 	begin
-- 		Set_Name(T, "Punycoding Tests");
-- 		Add_Test_Routine(T, Punycode_Encode_Test'Access, "Punycode_Encode");
-- 		Add_Test_Routine(T, Punycode_Decode_Test'Access, "Punycode_Decode");
-- 	end Initialize;

	-----------------------
	-- Utility Functions --
	-----------------------

	function Create_Message (Got : String; Expected : String) return String is
		("Got: " & Got & "; Expected: " & Expected);

	procedure Easy_Assert
		(Test : Unbounded_String; Expected : Unbounded_String) is
	begin
		Assert(Test = Expected, Create_Message(To_String(Test), To_String(Expected)));
	end Easy_Assert;

	procedure Easy_Assert (Test : String; Expected : String) is
	begin
		Assert(Test = Expected, Create_Message(Test, Expected));
	end Easy_Assert;

	function "+" (Source : String) return Unbounded_String
		renames To_Unbounded_String;

	-------------
	-- Parsing --
	-------------

	procedure Assert_Equal_URL is
		new Ahven.Assert_Equal (Data_Type => URL, Image => URI.Image);

	type Parser_Test is record
		Input : Unbounded_String;
		Expected : URL;
	end record;

	procedure Parse_Test is
		Parser_Tests : constant array (Positive range <>) of Parser_Test := (
			(
				+"gemini://nytpu:hunter2@www.example.com:1965/path/to/%20?query=weary#fragments%21",
				(True, +"gemini",
				True, True, +"nytpu:hunter2", +"www.example.com", True, 1965,
				+"/path/to/ ",
				True, +"query=weary",
				True, +"fragments!")
			),
			(
				+"//example.com#hmm-",
				(False, Null_Unbounded_String,
				True, False, Null_Unbounded_String, +"example.com", False, 0,
				+"",
				False, Null_Unbounded_String,
				True, +"hmm-")
			),
			(
				+"gemini:/hello/world.gmi",
				(True, +"gemini",
				False, False, Null_Unbounded_String, Null_Unbounded_String, False, 0,
				+"/hello/world.gmi",
				False, Null_Unbounded_String,
				False, Null_Unbounded_String)
			),
			(
				+"/hello%5Cworld.gmi#foo",
				(False, Null_Unbounded_String,
				False, False, Null_Unbounded_String, Null_Unbounded_String, False, 0,
				+"/hello\world.gmi",
				False, Null_Unbounded_String,
				True, +"foo")
			),
			(
				+"hello%5Cworld.gmi",
				(False, Null_Unbounded_String,
				False, False, Null_Unbounded_String, Null_Unbounded_String, False, 0,
				+"hello\world.gmi",
				False, Null_Unbounded_String,
				False, Null_Unbounded_String)
			),
			(
				+"?hello",
				(False, Null_Unbounded_String,
				False, False, Null_Unbounded_String, Null_Unbounded_String, False, 0,
				+"",
				True, +"hello",
				False, Null_Unbounded_String)
			),
			(
				+"#hello",
				(False, Null_Unbounded_String,
				False, False, Null_Unbounded_String, Null_Unbounded_String, False, 0,
				+"",
				False, Null_Unbounded_String,
				True, +"hello")
			)
		);

		U : URL;
	begin
		for T of Parser_Tests loop
			U := Parse(To_String(T.Input));
			Assert_Equal_URL(U, T.Expected, "Input: " & To_String(T.Input));
		end loop;
	end Parse_Test;

	procedure Parse_Relative_Test is
		Base : constant URL := (
			True, +"http",
			True, False, Null_Unbounded_String, +"a", False, 0,
			+"/b/c/d;p",
			True, +"q",
			False, Null_Unbounded_String
		);
		-- Derived from here: <https://lists.w3.org/Archives/Public/www-qa/2001Aug/0000.html>
		Parser_Tests : constant array (Positive range <>) of Parser_Test := (
			(
				+"g:h",
				(True, +"g",
				False, False, Null_Unbounded_String, Null_Unbounded_String, False, 0,
				+"h",
				False, Null_Unbounded_String,
				False, Null_Unbounded_String)
			),
			(
				+"g",
				(True, +"http",
				True, False, Null_Unbounded_String, +"a", False, 0,
				+"/b/c/g",
				False, Null_Unbounded_String,
				False, Null_Unbounded_String)
			),
			(
				+"./g",
				(True, +"http",
				True, False, Null_Unbounded_String, +"a", False, 0,
				+"/b/c/g",
				False, Null_Unbounded_String,
				False, Null_Unbounded_String)
			),
			(
				+"g/",
				(True, +"http",
				True, False, Null_Unbounded_String, +"a", False, 0,
				+"/b/c/g/",
				False, Null_Unbounded_String,
				False, Null_Unbounded_String)
			),
			(
				+"/g",
				(True, +"http",
				True, False, Null_Unbounded_String, +"a", False, 0,
				+"/g",
				False, Null_Unbounded_String,
				False, Null_Unbounded_String)
			),
			(
				+"//g",
				(True, +"http",
				True, False, Null_Unbounded_String, +"g", False, 0,
				+"",
				False, Null_Unbounded_String,
				False, Null_Unbounded_String)
			),
			(
				+"?y",
				(True, +"http",
				True, False, Null_Unbounded_String, +"a", False, 0,
				+"/b/c/d;p",
				True, +"y",
				False, Null_Unbounded_String)
			),
			(
				+"g?y",
				(True, +"http",
				True, False, Null_Unbounded_String, +"a", False, 0,
				+"/b/c/g",
				True, +"y",
				False, Null_Unbounded_String)
			),
			(
				+"#s",
				(True, +"http",
				True, False, Null_Unbounded_String, +"a", False, 0,
				+"/b/c/d;p",
				True, +"q",
				True, +"s")
			),
			(
				+"g#s",
				(True, +"http",
				True, False, Null_Unbounded_String, +"a", False, 0,
				+"/b/c/g",
				False, Null_Unbounded_String,
				True, +"s")
			),
			(
				+"g?y#s",
				(True, +"http",
				True, False, Null_Unbounded_String, +"a", False, 0,
				+"/b/c/g",
				True, +"y",
				True, +"s")
			),
			(
				+";x",
				(True, +"http",
				True, False, Null_Unbounded_String, +"a", False, 0,
				+"/b/c/;x",
				False, Null_Unbounded_String,
				False, Null_Unbounded_String)
			),
			(
				+"g;x",
				(True, +"http",
				True, False, Null_Unbounded_String, +"a", False, 0,
				+"/b/c/g;x",
				False, Null_Unbounded_String,
				False, Null_Unbounded_String)
			),
			(
				+"g;x?y#s",
				(True, +"http",
				True, False, Null_Unbounded_String, +"a", False, 0,
				+"/b/c/g;x",
				True, +"y",
				True, +"s")
			),
			(
				+".",
				(True, +"http",
				True, False, Null_Unbounded_String, +"a", False, 0,
				+"/b/c/",
				False, Null_Unbounded_String,
				False, Null_Unbounded_String)
			),
			(
				+"./",
				(True, +"http",
				True, False, Null_Unbounded_String, +"a", False, 0,
				+"/b/c/",
				False, Null_Unbounded_String,
				False, Null_Unbounded_String)
			),
			(
				+"..",
				(True, +"http",
				True, False, Null_Unbounded_String, +"a", False, 0,
				+"/b/",
				False, Null_Unbounded_String,
				False, Null_Unbounded_String)
			),
			(
				+"../",
				(True, +"http",
				True, False, Null_Unbounded_String, +"a", False, 0,
				+"/b/",
				False, Null_Unbounded_String,
				False, Null_Unbounded_String)
			),
			(
				+"../g",
				(True, +"http",
				True, False, Null_Unbounded_String, +"a", False, 0,
				+"/b/g",
				False, Null_Unbounded_String,
				False, Null_Unbounded_String)
			),
			(
				+"../..",
				(True, +"http",
				True, False, Null_Unbounded_String, +"a", False, 0,
				+"/",
				False, Null_Unbounded_String,
				False, Null_Unbounded_String)
			),
			(
				+"../../g",
				(True, +"http",
				True, False, Null_Unbounded_String, +"a", False, 0,
				+"/g",
				False, Null_Unbounded_String,
				False, Null_Unbounded_String)
			),
			(
				+"",
				Base
			),
			(
				+"../../../g",
				(True, +"http",
				True, False, Null_Unbounded_String, +"a", False, 0,
				+"/g",
				False, Null_Unbounded_String,
				False, Null_Unbounded_String)
			),
			(
				+"../../../../g",
				(True, +"http",
				True, False, Null_Unbounded_String, +"a", False, 0,
				+"/g",
				False, Null_Unbounded_String,
				False, Null_Unbounded_String)
			),
			(
				+"/./g",
				(True, +"http",
				True, False, Null_Unbounded_String, +"a", False, 0,
				+"/g",
				False, Null_Unbounded_String,
				False, Null_Unbounded_String)
			),
			(
				+"/../g",
				(True, +"http",
				True, False, Null_Unbounded_String, +"a", False, 0,
				+"/g",
				False, Null_Unbounded_String,
				False, Null_Unbounded_String)
			),
			(
				+"g.",
				(True, +"http",
				True, False, Null_Unbounded_String, +"a", False, 0,
				+"/b/c/g.",
				False, Null_Unbounded_String,
				False, Null_Unbounded_String)
			),
			(
				+".g",
				(True, +"http",
				True, False, Null_Unbounded_String, +"a", False, 0,
				+"/b/c/.g",
				False, Null_Unbounded_String,
				False, Null_Unbounded_String)
			),
			(
				+"g..",
				(True, +"http",
				True, False, Null_Unbounded_String, +"a", False, 0,
				+"/b/c/g..",
				False, Null_Unbounded_String,
				False, Null_Unbounded_String)
			),
			(
				+"..g",
				(True, +"http",
				True, False, Null_Unbounded_String, +"a", False, 0,
				+"/b/c/..g",
				False, Null_Unbounded_String,
				False, Null_Unbounded_String)
			),
			(
				+"./../g",
				(True, +"http",
				True, False, Null_Unbounded_String, +"a", False, 0,
				+"/b/g",
				False, Null_Unbounded_String,
				False, Null_Unbounded_String)
			),
			(
				+"./g/.",
				(True, +"http",
				True, False, Null_Unbounded_String, +"a", False, 0,
				+"/b/c/g/",
				False, Null_Unbounded_String,
				False, Null_Unbounded_String)
			),
			(
				+"g/./h",
				(True, +"http",
				True, False, Null_Unbounded_String, +"a", False, 0,
				+"/b/c/g/h",
				False, Null_Unbounded_String,
				False, Null_Unbounded_String)
			),
			(
				+"g/../h",
				(True, +"http",
				True, False, Null_Unbounded_String, +"a", False, 0,
				+"/b/c/h",
				False, Null_Unbounded_String,
				False, Null_Unbounded_String)
			),
			(
				+"g;x=1/./y",
				(True, +"http",
				True, False, Null_Unbounded_String, +"a", False, 0,
				+"/b/c/g;x=1/y",
				False, Null_Unbounded_String,
				False, Null_Unbounded_String)
			),
			(
				+"g;x=1/../y",
				(True, +"http",
				True, False, Null_Unbounded_String, +"a", False, 0,
				+"/b/c/y",
				False, Null_Unbounded_String,
				False, Null_Unbounded_String)
			),
			(
				+"g?y/./x",
				(True, +"http",
				True, False, Null_Unbounded_String, +"a", False, 0,
				+"/b/c/g",
				True, +"y/./x",
				False, Null_Unbounded_String)
			),
			(
				+"g?y/../x",
				(True, +"http",
				True, False, Null_Unbounded_String, +"a", False, 0,
				+"/b/c/g",
				True, +"y/../x",
				False, Null_Unbounded_String)
			),
			(
				+"g#y/./x",
				(True, +"http",
				True, False, Null_Unbounded_String, +"a", False, 0,
				+"/b/c/g",
				False, Null_Unbounded_String,
				True, +"y/./x")
			),
			(
				+"g#y/../x",
				(True, +"http",
				True, False, Null_Unbounded_String, +"a", False, 0,
				+"/b/c/g",
				False, Null_Unbounded_String,
				True, +"y/../x")
			)
		);

		U : URL;
	begin
		for T of Parser_Tests loop
			U := Parse_Relative(Base, To_String(T.Input));
			Assert_Equal_URL(U, T.Expected, "Input: " & To_String(T.Input));
		end loop;
	end Parse_Relative_Test;

	-----------
	-- Image --
	-----------

	procedure Image_Test is
		U : URL := (
			Scheme_Present | Authority_Present | User_Info_Present |
				Port_Present | Query_Present | Fragment_Present => False,
			Port => 0,
			others => Null_Unbounded_String
		);
	begin
		Easy_Assert(Image(U), "");

		U.Path := +"/1/2/ /4/5";
		Easy_Assert(Image(U), "/1/2/ /4/5");
		U.Path := +Percent_Encode_Path("/1/2/ /4/5");
		Easy_Assert(Image(U), "/1/2/%20/4/5");

		U.Scheme_Present := True;
		Easy_Assert(Image(U), ":/1/2/%20/4/5");
		U.Scheme := +"gemini";
		Easy_Assert(Image(U), "gemini:/1/2/%20/4/5");

		U.Authority_Present := True;
		Easy_Assert(Image(U), "gemini:///1/2/%20/4/5");
		U.Scheme_Present := False;
		Easy_Assert(Image(U), "///1/2/%20/4/5");
		U.Host := +"example.com";
		Easy_Assert(Image(U), "//example.com/1/2/%20/4/5");
		U.Port_Present := True;
		Easy_Assert(Image(U), "//example.com:0/1/2/%20/4/5");
		U.Port := 65535;
		Easy_Assert(Image(U), "//example.com:65535/1/2/%20/4/5");
		U.User_Info_Present := True;
		Easy_Assert(Image(U), "//@example.com:65535/1/2/%20/4/5");
		U.User_Info := +"nytpu:hunter2";
		Easy_Assert(Image(U), "//nytpu:hunter2@example.com:65535/1/2/%20/4/5");

		U.Query_Present := True;
		Easy_Assert(Image(U), "//nytpu:hunter2@example.com:65535/1/2/%20/4/5?");
		U.Authority_Present := False;
		Easy_Assert(Image(U), "/1/2/%20/4/5?");
		U.Scheme_Present := True;
		Easy_Assert(Image(U), "gemini:/1/2/%20/4/5?");

		U.Fragment_Present := True;
		Easy_Assert(Image(U), "gemini:/1/2/%20/4/5?#");

		U.Query := +"Hello=World!";
		Easy_Assert(Image(U), "gemini:/1/2/%20/4/5?Hello=World!#");

		U.Fragment := +"~nyah--";
		Easy_Assert(Image(U), "gemini:/1/2/%20/4/5?Hello=World!#~nyah--");

		U.Authority_Present := True;
		Easy_Assert(Image(U), "gemini://nytpu:hunter2@example.com:65535/1/2/%20/4/5?Hello=World!#~nyah--");
	end Image_Test;


	-------------------------
	-- Host_Heuristic_Test --
	-------------------------

	procedure Host_Heuristic_Test is
		type Host_Heuristic_Test is record
			Input, Expected : URL;
		end record;

		Host_Heuristic_Tests : constant array (Positive range <>) of Host_Heuristic_Test := (
			(
				(Authority_Present => True, Host => +"example.com", Path => +"example.com/hello-world", others => <>),
				(Authority_Present => True, Host => +"example.com", Path => +"example.com/hello-world", others => <>)
			),
			(
				(Authority_Present => False, Path => +"example.com/hello-world", others => <>),
				(Authority_Present => True, Host => +"example.com", Path => +"/hello-world", others => <>)
			),
			(
				(Authority_Present => False, Path => +"example.com", others => <>),
				(Authority_Present => True, Host => +"example.com", Path => +"", others => <>)
			),
			(
				(Authority_Present => False, Path => +"/example.com/hello-world", others => <>),
				(Authority_Present => False, Path => +"/example.com/hello-world", others => <>)
			),
			(
				(Authority_Present => False, Path => +"/example.com", others => <>),
				(Authority_Present => false, Path => +"/example.com", others => <>)
			),
			(
				(Authority_Present => False, Path => +"hello-world", others => <>),
				(Authority_Present => False, Path => +"hello-world", others => <>)
			),
			(
				(Authority_Present => False, Path => +"dos-stuff/graphics.com", others => <>),
				(Authority_Present => False, Path => +"dos-stuff/graphics.com", others => <>)
			)
		);
	begin
		for I in Host_Heuristic_Tests'Range loop
			Assert(
				Host_Heuristic(Host_Heuristic_Tests(I).Input) = Host_Heuristic_Tests(I).Expected,
				"Index: " & Positive'Image(I)
			);
		end loop;
	end Host_Heuristic_Test;


	------------------
	-- Path Munging --
	------------------

	procedure Normalize_Path_Test is
		type Normalization_Test is record
			Input : Unbounded_String;
			Expected : Unbounded_String;
		end record;
		-- Very loosely adapted from:
		-- <https://go.dev/src/path/filepath/path_test.go#L26>
		--
		-- Copyright (c) 2009 The Go Authors. All rights reserved.
		--
		-- Redistribution and use in source and binary forms, with or without
		-- modification, are permitted provided that the following conditions
		-- are met:
		--
		--    * Redistributions of source code must retain the above copyright
		--      notice, this list of conditions and the following disclaimer.
		--    * Redistributions in binary form must reproduce the above
		--      copyright notice, this list of conditions and the following
		--      disclaimer in the documentation and/or other materials provided
		--      with the distribution.
		--    * Neither the name of Google Inc. nor the names of its
		--      contributors may be used to endorse or promote products derived
		--      from this software without specific prior written permission.
		--
		-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
		-- "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
		-- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
		-- FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
		-- COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
		-- INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
		-- BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
		-- LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
		-- CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
		-- LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
		-- ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
		-- POSSIBILITY OF SUCH DAMAGE.
		Normalization_Tests :
				constant array (Positive range <>) of Normalization_Test := (
			(+"abc", +"abc"),
			(+"abc/", +"abc/"),
			(+"abc/def", +"abc/def"),
			(+"abc/def/", +"abc/def/"),
			(+"a/b/c", +"a/b/c"),
			(+"a/b/c/", +"a/b/c/"),
			(+"/abc", +"/abc"),
			(+"/abc/", +"/abc/"),
			(+".", +""),
			(+"/.", +"/"),
			(+"..", +""),
			(+"/..", +"/"),
			(+"../..", +""),
			(+"/../..", +"/"),
			(+"../../abc", +"abc"),
			(+"", +""),
			(+"/", +"/"),
			(+"abc//def//ghi", +"abc/def/ghi"),
			(+"//abc", +"/abc"),
			(+"///abc", +"/abc"),
			(+"//abc//", +"/abc/"),
			(+"abc//", +"abc/"),
			(+"abc/./def", +"abc/def"),
			(+"/./abc/def", +"/abc/def"),
			(+"abc/.", +"abc/"),
			(+"abc/def/ghi/../jkl", +"abc/def/jkl"),
			(+"abc/def/../ghi/../jkl", +"abc/jkl"),
			(+"abc/def/..", +"abc/"),
			(+"abc/def/../..", +""),
			(+"/abc/def/../..", +"/"),
			(+"abc/def/../../..", +""),
			(+"/abc/def/../../..", +"/"),
			(+"abc/def/../../../ghi/jkl/../../../mno", +"mno"),
			(+"/abc/def/../../../ghi/jkl/../../../mno", +"/mno"),
			(+"/../abc", +"/abc"),
			(+"abc/./../def", +"def"),
			(+"abc//./../def", +"def"),
			(+"abc/../../././../def", +"def")
		);
	begin
		for E of Normalization_Tests loop
			Easy_Assert(Normalize_Path(E.Input), E.Expected);
		end loop;
	end Normalize_Path_Test;

	procedure Merge_Paths_Test is
		type Path_Merge_Test is record
			Base, Relative : Unbounded_String;
			Expected : Unbounded_String;
		end record;
		-- Very loosely adapted from:
		-- <https://go.dev/src/net/url/url_test.go#L1094>
		--
		-- Copyright (c) 2009 The Go Authors. All rights reserved.
		--
		-- Redistribution and use in source and binary forms, with or without
		-- modification, are permitted provided that the following conditions
		-- are met:
		--
		--    * Redistributions of source code must retain the above copyright
		--      notice, this list of conditions and the following disclaimer.
		--    * Redistributions in binary form must reproduce the above
		--      copyright notice, this list of conditions and the following
		--      disclaimer in the documentation and/or other materials provided
		--      with the distribution.
		--    * Neither the name of Google Inc. nor the names of its
		--      contributors may be used to endorse or promote products derived
		--      from this software without specific prior written permission.
		--
		-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
		-- "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
		-- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
		-- FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
		-- COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
		-- INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
		-- BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
		-- LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
		-- CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
		-- LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
		-- ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
		-- POSSIBILITY OF SUCH DAMAGE.
		Path_Merge_Tests :
				constant array (Positive range <>) of Path_Merge_Test := (
			(+"/a/b/", +"/g", +"/g"),
			(+"/a/b", +".", +"/a/"),
			(+"/a/b", +"c", +"/a/c"),
			(+"/a/b", +"..", +"/"),
			(+"/a/", +"..", +"/"),
			(+"/a/", +"../..", +"/"),
			(+"/a/b/c", +"..", +"/a/"),
			(+"/a/b/c", +"../d", +"/a/d"),
			(+"/a/b/c", +".././d", +"/a/d"),
			(+"/a/b", +"./..", +"/"),
			(+"/a/./b", +".", +"/a/"),
			(+"/a/../", +".", +"/"),
			(+"/a/.././b", +"c", +"/c")
		);
	begin
		for E of Path_Merge_Tests loop
			Easy_Assert(Merge_Paths(E.Base, E.Relative), E.Expected);
		end loop;
	end Merge_Paths_Test;

	----------------------
	-- Percent Encoding --
	----------------------

	type Encode_Test is record
		Decoded : Unbounded_String;
		Fully_Encoded : Unbounded_String;
		Path_Encoded : Unbounded_String;
		Query_Encoded : Unbounded_String;
		Full_Query_Encoded : Unbounded_String;
	end record;
	type Encode_Tests is array (Positive range <>) of Encode_Test;

	Percent_Encode_Tests : constant Encode_Tests := (
		(
			+"+",
			others => +"%2B"
		),
		(
			+"naïve.nytpu.com",
			others => +"na%C3%AFve.nytpu.com"
		),
		(
			+"_~円£~_",
			others => +"_~%E5%86%86%C2%A3~_"
		),
		(
			+"what?????",
			others => +"what%3F%3F%3F%3F%3F"
		),
		(
			+"foo://bar.com/cafè.gmi.gmi",
			Path_Encoded => +"foo%3A//bar.com/caf%C3%A8.gmi.gmi",
			others => +"foo%3A%2F%2Fbar.com%2Fcaf%C3%A8.gmi.gmi"
		),
		(
			+"foo://bar.com/caffè+macchiato.gmi",
			Path_Encoded => +"foo%3A//bar.com/caff%C3%A8%2Bmacchiato.gmi",
			others => +"foo%3A%2F%2Fbar.com%2Fcaff%C3%A8%2Bmacchiato.gmi"
		),
		(
			+"NULL! " & ASCII.NUL,
			Query_Encoded | Full_Query_Encoded => +"NULL%21+%00",
			others => +"NULL%21%20%00"
		),
		(
			+"%00%01 ... %FE%FF",
			Query_Encoded | Full_Query_Encoded => +"%2500%2501+...+%25FE%25FF",
			others => +"%2500%2501%20...%20%25FE%25FF"
		),
		(
			+"Lorem ipsum dolor sit amet",
			Query_Encoded | Full_Query_Encoded => +"Lorem+ipsum+dolor+sit+amet",
			others => +"Lorem%20ipsum%20dolor%20sit%20amet"
		),
		(
			+"foo://bar.com/caffè macchiato.gmi",
			Fully_Encoded => +"foo%3A%2F%2Fbar.com%2Fcaff%C3%A8%20macchiato.gmi",
			Path_Encoded => +"foo%3A//bar.com/caff%C3%A8%20macchiato.gmi",
			Query_Encoded | Full_Query_Encoded => +"foo%3A%2F%2Fbar.com%2Fcaff%C3%A8+macchiato.gmi"
		),
		(
			+"foo://bar.com/caffè macchiato.gmi?hello-world!",
			Fully_Encoded => +"foo%3A%2F%2Fbar.com%2Fcaff%C3%A8%20macchiato.gmi%3Fhello-world%21",
			Path_Encoded => +"foo%3A//bar.com/caff%C3%A8%20macchiato.gmi%3Fhello-world%21",
			Query_Encoded | Full_Query_Encoded => +"foo%3A%2F%2Fbar.com%2Fcaff%C3%A8+macchiato.gmi%3Fhello-world%21"
		),
		(
			+"foo://bar.com/caffè_macchiato.gmi?hello=world&bazinga=true",
			Fully_Encoded => +"foo%3A%2F%2Fbar.com%2Fcaff%C3%A8_macchiato.gmi%3Fhello%3Dworld%26bazinga%3Dtrue",
			Path_Encoded => +"foo%3A//bar.com/caff%C3%A8_macchiato.gmi%3Fhello%3Dworld%26bazinga%3Dtrue",
			Query_Encoded => +"foo%3A%2F%2Fbar.com%2Fcaff%C3%A8_macchiato.gmi%3Fhello%3Dworld%26bazinga%3Dtrue",
			Full_Query_Encoded => +"foo%3A%2F%2Fbar.com%2Fcaff%C3%A8_macchiato.gmi%3Fhello=world&bazinga=true"
		),
		(
			+"x = (-b ± √(b^2 - 4ac)) / 2a",
			Fully_Encoded => +"x%20%3D%20%28-b%20%C2%B1%20%E2%88%9A%28b%5E2%20-%204ac%29%29%20%2F%202a",
			Path_Encoded => +"x%20%3D%20%28-b%20%C2%B1%20%E2%88%9A%28b%5E2%20-%204ac%29%29%20/%202a",
			Query_Encoded => +"x+%3D+%28-b+%C2%B1+%E2%88%9A%28b%5E2+-+4ac%29%29+%2F+2a",
			Full_Query_Encoded => +"x+=+%28-b+%C2%B1+%E2%88%9A%28b%5E2+-+4ac%29%29+%2F+2a"
		)
	);

	procedure Percent_Decode_Test is
		Current : Encode_Test;
		Decoded_Full, Decoded_Path, Decoded_Query, Decoded_Full_Query : Unbounded_String;
	begin
		for I in Percent_Encode_Tests'Range loop
			Current := Percent_Encode_Tests(I);

			Decoded_Full := Percent_Decode(Current.Fully_Encoded);
			Decoded_Path := Percent_Decode(Current.Path_Encoded);
			Decoded_Query := Percent_Decode(Current.Query_Encoded);
			Decoded_Full_Query := Percent_Decode(Current.Full_Query_Encoded);

			Assert(Decoded_Full = Current.Decoded, Create_Message(To_String(Decoded_Full), To_String(Current.Decoded)));
			Assert(Decoded_Path = Current.Decoded, Create_Message(To_String(Decoded_Path), To_String(Current.Decoded)));
			Assert(Decoded_Query = Current.Decoded, Create_Message(To_String(Decoded_Query), To_String(Current.Decoded)));
			Assert(Decoded_Full_Query = Current.Decoded, Create_Message(To_String(Decoded_Full_Query), To_String(Current.Decoded)));
		end loop;
	end Percent_Decode_Test;

	procedure Percent_Encode_Test is
		Encoded : Unbounded_String;
		Current : Encode_Test;
	begin
		for I in Percent_Encode_Tests'Range loop
			Current := Percent_Encode_Tests(I);
			Encoded := Percent_Encode(Current.Decoded);
			Assert(Encoded = Current.Fully_Encoded, Create_Message(To_String(Encoded), To_String(Current.Fully_Encoded)));
		end loop;
	end Percent_Encode_Test;

	procedure Percent_Encode_Path_Test is
		Encoded : Unbounded_String;
		Current : Encode_Test;
	begin
		for I in Percent_Encode_Tests'Range loop
			Current := Percent_Encode_Tests(I);
			Encoded := Percent_Encode_Path(Current.Decoded);
			Assert(Encoded = Current.Path_Encoded, Create_Message(To_String(Encoded), To_String(Current.Path_Encoded)));
		end loop;
	end Percent_Encode_Path_Test;

	procedure Percent_Encode_Query_Test is
		Encoded : Unbounded_String;
		Current : Encode_Test;
	begin
		for I in Percent_Encode_Tests'Range loop
			Current := Percent_Encode_Tests(I);
			Encoded := Percent_Encode(Current.Decoded, Query_Encoding => True);
			Assert(Encoded = Current.Query_Encoded, Create_Message(To_String(Encoded), To_String(Current.Query_Encoded)));
		end loop;
	end Percent_Encode_Query_Test;

	procedure Percent_Encode_Full_Query_Test is
		Encoded : Unbounded_String;
		Current : Encode_Test;
	begin
		for I in Percent_Encode_Tests'Range loop
			Current := Percent_Encode_Tests(I);
			Encoded := Percent_Encode_Query(Current.Decoded);
			Assert(Encoded = Current.Full_Query_Encoded, Create_Message(To_String(Encoded), To_String(Current.Full_Query_Encoded)));
		end loop;
	end Percent_Encode_Full_Query_Test;

	----------------
	-- Punycoding --
	----------------

-- 	procedure Punycode_Encode_Test is
-- 	begin
-- 		pragma Compile_Time_Warning(Standard.True, "Punycode_Encode_Test unimplemented");
-- 		Skip("Unimplemented procedure Punycode_Encode_Test");
-- 	end Punycode_Encode_Test;

-- 	procedure Punycode_Decode_Test is
-- 	begin
-- 		pragma Compile_Time_Warning(Standard.True, "Punycode_Decode_Test unimplemented");
-- 		Skip("Unimplemented procedure Punycode_Decode_Test");
-- 	end Punycode_Decode_Test;

end URI_Tests;
