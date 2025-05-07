-- SPDX-License-Identifier: BSD-2-Clause
-- For more license details, see LICENSE.

pragma Ada_2012;

with Ada.Strings.Maps;

package body Linenoise is

	---------------
	-- Linenoise --
	---------------

	function Get_Line (Prompt : String := "") return String is
		Prompt_Ptr, Line_Ptr : chars_ptr;
	begin
		Prompt_Ptr := New_String(Prompt);
		Line_Ptr := linenoise(Prompt_Ptr);
		Free(Prompt_Ptr);
		if Line_Ptr = Null_Ptr then
			raise End_Error;
		else
			declare
				Line : constant String := Value(Line_Ptr);
			begin
				Free(Line_Ptr);
				return Line;
			end;
		end if;
	end Get_Line;


	--------------------
	-- Multiline_Mode --
	--------------------

	procedure Multiline_Mode (Enabled : Boolean) is
		Dummy : int;
	begin
		Dummy := linenoiseSetMultiLine((if Enabled then 1 else 0));
		pragma Unreferenced(Dummy);
	end Multiline_Mode;


	-----------------
	-- History_Add --
	-----------------

	procedure History_Add (Line : String) is
		Line_Ptr : chars_ptr;
		Dummy : int;
	begin
		Line_Ptr := New_String(Line);
		Dummy := linenoiseHistoryAdd(Line_Ptr);
		pragma Unreferenced(Dummy);
		Free(Line_Ptr);
	end History_Add;


	--------------------------------
	-- History_Set_Maximum_Length --
	--------------------------------

	procedure History_Set_Maximum_Length (Length : History_Length) is
		Dummy : int;
	begin
		Dummy := linenoiseHistorySetMaxLen(int(Length));
		pragma Unreferenced(Dummy);
	end History_Set_Maximum_Length;


	------------------
	-- History_Load --
	------------------

	procedure History_Load (Filename : String) is
		Filename_Ptr : chars_ptr;
		Status : int;
	begin
		Filename_Ptr := New_String(Filename);
		Status := linenoiseHistoryLoad(Filename_Ptr);
		Free(Filename_Ptr);
		if Status < 0 then
			raise History_File_Error;
		end if;
	end History_Load;

	------------------
	-- History_Save --
	------------------

	procedure History_Save (Filename : String) is
		Filename_Ptr : chars_ptr;
		Status : int;
	begin
		Filename_Ptr := New_String(Filename);
		Status := linenoiseHistorySave(Filename_Ptr);
		Free(Filename_Ptr);
		if Status < 0 then
			raise History_File_Error;
		end if;
	end History_Save;


	----------------------------------
	-- Register_Completion_Callback --
	----------------------------------

	procedure Register_Completion_Callback (Callback : Completion_Callback) is
	begin
		Completer := Callback;
		linenoiseSetCompletionCallback(Completion_Wrapper'Access);
	end Register_Completion_Callback;


	-------------------------------
	-- Clear_Completion_Callback --
	-------------------------------

	procedure Clear_Completion_Callback is
	begin
		Completer := null;
		linenoiseSetCompletionCallback(null);
	end Clear_Completion_Callback;


	----------------------------
	-- Register_Hint_Callback --
	----------------------------

	procedure Register_Hint_Callback (Callback : Hint_Callback) is
	begin
		Hinter := Callback;
		linenoiseSetHintsCallback(Hint_Wrapper'Access);
		linenoiseSetFreeHintsCallback(free_hint_helper'Access);
	end Register_Hint_Callback;


	-------------------------
	-- Clear_Hint_Callback --
	-------------------------

	procedure Clear_Hint_Callback is
	begin
		Hinter := null;
		linenoiseSetHintsCallback(null);
		linenoiseSetFreeHintsCallback(null);
	end Clear_Hint_Callback;


	---------------
	-- Mask_Mode --
	---------------

	procedure Mask_Mode (Enabled : Boolean) is
	begin
		if Enabled then
			linenoiseMaskModeEnable;
		else
			linenoiseMaskModeDisable;
		end if;
	end Mask_Mode;


	-----------
	-- Split --
	-----------

	function Split (S : String) return String_Vectors.Vector is
		use Ada.Strings.Maps;
		use String_Vectors;

		Whitespace : constant Character_Set := To_Set(" " & ASCII.HT);

		Quote_Char : Character;
		Start : Positive := S'First;
		Finish : Positive;
		O : Vector := Empty_Vector;
	begin
		if S'Length = 0 then
			return O;
		end if;

		while Start <= S'Last loop
			while Start <= S'Last and then Is_In(S(Start), Whitespace) loop
				Start := Start + 1;
			end loop;
			if Start > S'Last then
				return O;
			end if;
			Finish := Start;

			if S(Start) = '"' or S(Start) = ''' then
				Quote_Char := S(Start);
				Finish := Start + 1;
				while Finish <= S'Last and then S(Finish) /= Quote_Char loop
					Finish := Finish + 1;
				end loop;
				if Finish > S'Last or else S(Finish) /= Quote_Char then
					raise Malformed_Command with "Unclosed quoted argument";
				end if;
				pragma Assert(S(Start) = Quote_Char);
				pragma Assert(S(Finish) = Quote_Char);
				O.Append(S(Start + 1 .. Finish - 1));
				if
					Finish + 1 <= S'Last and then
					not Is_In(S(Finish + 1), Whitespace)
				then
					raise Malformed_Command with "Quoted arguments must be separated from adjacent arguments by whitespace";
				end if;
				Start := Finish + 1;
			else
				while
					Finish <= S'Last and then
					not Is_In(S(Finish), Whitespace)
				loop
					Finish := Finish + 1;
				end loop;
				if Finish > S'Last then
					Finish := S'Last;
				else
					-- Since Finish will be on whitespace if we're not past the
					-- end, we back up one to be on the end of the word.
					Finish := Finish - 1;
				end if;
				O.Append(S(Start .. Finish));
				Start := Finish + 1;
			end if;
		end loop;
		return O;
	end Split;


	------------------------
	-- Completion_Wrapper --
	------------------------

	procedure Completion_Wrapper
		(line : chars_ptr; lc : access linenoiseCompletions)
	is
		use String_Vectors;

		Completions : Vector;
		Ptr : chars_ptr := Null_Ptr;
	begin
		Completions := Completer(Value(line));
		for C of Completions loop
			Ptr := New_String(C);
			linenoiseAddCompletion(lc, Ptr);
			-- Linenoise makes a copy of the string
			Free(Ptr);
		end loop;
	end Completion_Wrapper;


	------------------
	-- Hint_Wrapper --
	------------------

	function Hint_Wrapper
		(line : chars_ptr; color : access int; bold : access int)
		return chars_ptr
	is
		Color_Ada : Color_Codes := Default;
		Bold_Ada : Boolean := False;
		Hint : constant String := Hinter(Value(line), Color_Ada, Bold_Ada);
		Ptr, Corrected : chars_ptr;
	begin
		if Color_Ada /= Default then
			color.all := (
				case Color_Ada is
					when Red => 31,
					when Green => 32,
					when Yellow => 33,
					when Blue => 34,
					when Magenta => 35,
					when Cyan => 36,
					when White => 37,
					when Default => raise Program_Error with "Unreachable"
			);
		end if;
		bold.all := (if Bold_Ada then 1 else 0);
		Ptr := New_String(Hint);
		Corrected := hint_string_helper(Ptr);
		Free(Ptr);
		return Corrected;
	end Hint_Wrapper;

end Linenoise;
