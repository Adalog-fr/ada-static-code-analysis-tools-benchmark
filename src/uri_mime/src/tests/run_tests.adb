-- Copyright (c) 2022 nytpu <alex [at] nytpu.com>
-- SPDX-License-Identifier: MPL-2.0
-- For more license details, see LICENSE or <https://www.mozilla.org/en-US/MPL/2.0/>.

pragma Ada_95;

with Ahven.Text_Runner;
with Ahven.Framework;
with MIME_Tests;
with URI_Tests;

procedure Run_Tests is
	S : Ahven.Framework.Test_Suite := Ahven.Framework.Create_Suite("All");
begin
	Ahven.Framework.Add_Test(S, new MIME_Tests.Parsing_Tests);
	Ahven.Framework.Add_Test(S, new MIME_Tests.Image_Tests);
	Ahven.Framework.Add_Test(S, new URI_Tests.Parsing_Tests);
	Ahven.Framework.Add_Test(S, new URI_Tests.Image_Tests);
	Ahven.Framework.Add_Test(S, new URI_Tests.Path_Tests);
	Ahven.Framework.Add_Test(S, new URI_Tests.Percent_Encoding_Tests);
-- 	Ahven.Framework.Add_Test(S, new URI_Tests.Punycoding_Tests);
	Ahven.Text_Runner.Run(S);
end Run_Tests;
