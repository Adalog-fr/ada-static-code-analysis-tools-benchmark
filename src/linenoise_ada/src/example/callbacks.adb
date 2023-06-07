-- SPDX-License-Identifier: BSD-2-Clause
-- For more license details, see LICENSE.

pragma Ada_2012;

with Ada.Characters.Handling;  use Ada.Characters.Handling;
with Ada.Strings.Fixed;  use Ada.Strings.Fixed;

package body Callbacks is

	---------------
	-- Completer --
	---------------

	function Completer
		(Line : String) return Linenoise.String_Vectors.Vector
	is
		O : Linenoise.String_Vectors.Vector :=
			Linenoise.String_Vectors.Empty_Vector;
	begin
		case To_Lower(Line(Line'First)) is
			when 'c' => O.Append("clear");
			when 'e' => O.Append("exit");
			when 'h' =>
				O.Append("hello");
				O.Append("hello world");
			when 'm' =>
				O.Append("multiline");
				O.Append("mask");
			when 'n' => O.Append("nomultiline");
			when others => null;
		end case;
		return O;
	end Completer;


	------------
	-- Hinter --
	------------

	function Hinter
		(Line : String; Color : out Linenoise.Color_Codes; Bold : out Boolean)
		return String
	is
		function Begins_With (S : String; Prefix : String) return Boolean is
		begin
			return S'Length >= Prefix'Length and then
				Head(S, Prefix'Length) = Prefix;
		end Begins_With;

		Downcased : constant String := To_Lower(Line);
	begin
		Color := Linenoise.Yellow;
		Bold := False;
		if Begins_With(Downcased, "hello") then
			return " world";
		elsif Begins_With(Downcased, "mask") then
			return "  prompt to enter a masked ""password""";
		elsif Begins_With(Downcased, "multiline") then
			return "  enable multiline editing";
		elsif Begins_With(Downcased, "nomultiline") then
			return "  disable multiline editing";
		elsif Begins_With(Downcased, "clear") then
			return "  clear the screen";
		elsif Begins_With(Downcased, "exit") then
			return "  exit the example program";
		end if;
		-- Return the empty string when there's no hint
		return "";
	end Hinter;

end Callbacks;
