-- SPDX-License-Identifier: BSD-2-Clause
-- For more license details, see LICENSE.

pragma Ada_2012;

with Linenoise;

package Callbacks is
	pragma Preelaborate;

	function Completer (Line : String)
		return Linenoise.String_Vectors.Vector;

	function Hinter (
		Line : String; Color : out Linenoise.Color_Codes; Bold : out Boolean
	) return String;

end Callbacks;
