-- Copyright (c) 2022 nytpu <alex [at] nytpu.com>
-- SPDX-License-Identifier: MPL-2.0
-- For more license details, see LICENSE or <https://www.mozilla.org/en-US/MPL/2.0/>.

pragma Ada_2005;

with Ahven.Framework;

package MIME_Tests is

	type Parsing_Tests is new Ahven.Framework.Test_Case with null record;
	overriding procedure Initialize (T : in out Parsing_Tests);

	type Image_Tests is new Ahven.Framework.Test_Case with null record;
	overriding procedure Initialize (T : in out Image_Tests);

private
	procedure Parse_Test;
	procedure Image_Test;

end MIME_Tests;
