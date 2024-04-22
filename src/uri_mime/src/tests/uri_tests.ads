-- Copyright (c) 2022 nytpu <alex [at] nytpu.com>
-- SPDX-License-Identifier: MPL-2.0
-- For more license details, see LICENSE or <https://www.mozilla.org/en-US/MPL/2.0/>.

pragma Ada_2005;

with Ahven.Framework;

package URI_Tests is

	type Parsing_Tests is new Ahven.Framework.Test_Case with null record;
	overriding procedure Initialize (T : in out Parsing_Tests);

	type Image_Tests is new Ahven.Framework.Test_Case with null record;
	overriding procedure Initialize (T : in out Image_Tests);

	type Path_Tests is new Ahven.Framework.Test_Case with null record;
	overriding procedure Initialize (T : in out Path_Tests);

	type Percent_Encoding_Tests is new Ahven.Framework.Test_Case with null record;
	overriding procedure Initialize (T : in out Percent_Encoding_Tests);

-- 	type Punycoding_Tests is new Ahven.Framework.Test_Case with null record;
-- 	overriding procedure Initialize (T : in out Punycoding_Tests);


private
	function Create_Message(Got : String; Expected : String) return String;
	procedure Easy_Assert(Test : String; Expected : String);

	procedure Parse_Test;
	procedure Parse_Relative_Test;

	procedure Image_Test;

	procedure Host_Heuristic_Test;

	procedure Normalize_Path_Test;
	procedure Merge_Paths_Test;

	procedure Percent_Decode_Test;
	procedure Percent_Encode_Test;
	procedure Percent_Encode_Path_Test;
	procedure Percent_Encode_Query_Test;
	procedure Percent_Encode_Full_Query_Test;

-- 	procedure Punycode_Encode_Test;
-- 	procedure Punycode_Decode_Test;
end URI_Tests;
