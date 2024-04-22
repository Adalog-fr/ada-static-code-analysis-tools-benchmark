-- Example file for getopt
--
--
-- Copyright (c) 2021-2022 nytpu <alex [at] nytpu.com>
-- SPDX-License-Identifier: MPL-2.0
-- For more license details, see LICENSE or <https://www.mozilla.org/en-US/MPL/2.0/>.

pragma Ada_2012;

with Ada.Text_IO;  use Ada.Text_IO;
with Getopts;

procedure Example is
	P : Getopts.Parser;
begin
	for C in P.Getopt(":hi:jk:") loop
		case Getopts.Option(C) is
			when 'h' => Put_Line("Got option 'h'");
			when 'i' =>
				Put_Line("Got option 'i'");
				Put_Line("Got parameter: " & Getopts.Optarg(C));
			when 'j' => Put_Line("Got option 'j'");
			when 'k' =>
				Put_Line("Got option 'k'");
				Put_Line("Got parameter: " & Getopts.Optarg(C));
			when others => exit;
		end case;
	end loop;

	for I in 1 .. P.Argument_Count loop
		Put_Line("Argument " & I'Image & ": " & P.Argument(I));
	end loop;
end Example;
