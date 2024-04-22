-- SPDX-License-Identifier: BSD-2-Clause
-- For more license details, see LICENSE.

pragma Ada_2012;

with Ada.Containers.Indefinite_Vectors;
with Ada.IO_Exceptions;
with Interfaces.C;

private with Interfaces.C.Strings;

package Linenoise is
	pragma Preelaborate;

	-- A vector of strings package, used by Completion_Callbacks and Split.
	package String_Vectors is new Ada.Containers.Indefinite_Vectors(
		Index_Type => Positive, Element_Type => String
	);
	use type String_Vectors.Vector;

	-- Show the user the given prompt at the beginning of the line and block
	-- until the user enters a line of input.  The given line of input is then
	-- returned.
	-- If standard input is at end of file (for instance Ctrl-D was pressed)
	-- then End_Error is raised.
	function Get_Line (Prompt : String := "") return String;

	-- Add the given string (presumed to be a line given from `Linenoise` but
	-- it may be anything) to the history.  Will have no effect if
	-- `History_Set_Maximum_Length` has not yet been called, as the history
	-- length defaults to zero.
	procedure History_Add (Line : String);

	-- The number of lines that may be contained in the line history.
	type History_Length is range 0 .. Interfaces.C.int'Last;

	-- Set the maximum length the history may be before old lines are trimmed
	-- from the end.  A value of zero (the default) means that no history will
	-- ever be stored.
	procedure History_Set_Maximum_Length (Length : History_Length);

	-- Load a history file (saved with History_Save) from the given filename.
	procedure History_Load (Filename : String);

	-- Save the currently stored history to the given filename.
	procedure History_Save (Filename : String);

	-- A callback that given a partially-typed line should return a
	-- Completion_Vectors.Vector of all possible completions for that partial
	-- line.
	-- The vector may be empty if there are no completions available.
	type Completion_Callback is access
		function (Line : String) return String_Vectors.Vector;

	-- Register a custom completion callback.
	procedure Register_Completion_Callback (Callback : Completion_Callback);

	-- Clear the currently-registered completion callback, if any.
	procedure Clear_Completion_Callback;

	-- Possible color codes to color hints with.  Default is the current
	-- terminal foreground color.
	type Color_Codes is
		(Default, Red, Green, Yellow, Blue, Magenta, Cyan, White);

	-- A callback that returns a hint string given the current partially-typed
	-- line.  Will be colored according to the given color code, and if Bold is
	-- True then the hint text will be rendered as bold.
	-- Should return an empty string if there is no hint for the given line.
	type Hint_Callback is access
		function (
			Line : String;
			Color : out Color_Codes; Bold : out Boolean
		) return String;

	-- Register a custom hint callback.
	procedure Register_Hint_Callback (Callback : Hint_Callback);

	-- Clear the currently-registered hint callback, if any.
	procedure Clear_Hint_Callback;


	-----------------
	-- Miscellanea --
	-----------------

	-- Clear the entire terminal window.
	procedure Clear_Screen
		with Import => True, Convention => C,
			External_Name => "linenoiseClearScreen";

	-- Enable "multiline editing mode".  When disabled (the default), long
	-- lines will remain on one line and the excess text will be scrolled to
	-- the left; when enabled, long lines will be wrapped over multiple screen
	-- rows instead.
	procedure Multiline_Mode (Enabled : Boolean);

	-- Enable or disable "mask mode".  When enabled, all typed characters will
	-- be replaced with '*', intended for password entry.  Disabled by default.
	procedure Mask_Mode (Enabled : Boolean);


	---------------
	-- Utilities --
	---------------

	-- Split a string into a string vector, in a similar manner to the way a
	-- shell splits arguments.
	-- Arguments will be split on whitespace, unless a word begins with a
	-- quote ('"' or ''') in which case the word will be until the matching
	-- quote.  Returned quoted arguments are not enclosed in their quotes.
	-- Raises Malformed_Command if a quoted argument does not have a closing
	-- quote or if quoted arguments aren't separated from adjacent arguments
	-- with whitespace (i.e. '"arg 1"arg2' is not allowed, it must be
	-- '"arg 1" arg2').  Quotes not matching the enclosing quotes are allowed
	-- (e.g. "me ol' chum" is allowed)
	function Split (S : String) return String_Vectors.Vector;


	----------------
	-- Exceptions --
	----------------

	-- Raised when Linenoise is called when standard input has been closed or
	-- the input has been otherwise ended.
	End_Error : exception renames Ada.IO_Exceptions.End_Error;

	-- Raised if history failed to be loaded from or saved to the given
	-- filename.
	History_File_Error : exception;

	-- Raised by Split if it is unable to parse a command string.
	Malformed_Command : exception;


private
	use Interfaces.C;
	use Interfaces.C.Strings;

	-------------------
	-- Package state --
	-------------------

	Completer : Completion_Callback := null;
	Hinter : Hint_Callback := null;


	--------------------------------
	-- Import Linenoise functions --
	--------------------------------

	-- Opaque type
	type linenoiseCompletions is null record;

	type linenoiseCompletionCallback is access
		procedure (line : chars_ptr; lc : access linenoiseCompletions)
			with Convention => C;

	type linenoiseHintsCallback is access
		function (line : chars_ptr; color : access int; bold : access int)
			return chars_ptr
			with Convention => C;

	type linenoiseFreeHintsCallback is access
		procedure (ptr : chars_ptr)
			with Convention => C;

	procedure linenoiseSetCompletionCallback
		(callback : linenoiseCompletionCallback)
		with Import => True, Convention => C,
			External_Name => "linenoiseSetCompletionCallback";

	procedure linenoiseSetHintsCallback
		(callback : linenoiseHintsCallback)
		with Import => True, Convention => C,
			External_Name => "linenoiseSetHintsCallback";

	procedure linenoiseSetFreeHintsCallback
		(callback : linenoiseFreeHintsCallback)
		with Import => True, Convention => C,
			External_Name => "linenoiseSetFreeHintsCallback";

	procedure linenoiseAddCompletion
		(completions : access linenoiseCompletions; line : chars_ptr)
		with Import => True, Convention => C,
			External_Name => "linenoiseAddCompletion";

	function linenoise (prompt : chars_ptr) return chars_ptr
		with Import => True, Convention => C,
			External_Name => "linenoise";

	procedure linenoiseFree (ptr : in out chars_ptr)
		with Import => True, Convention => C,
			External_Name => "linenoiseFree";

	function linenoiseHistoryAdd (line : chars_ptr) return int
		with Import => True, Convention => C,
			External_Name => "linenoiseHistoryAdd";

	function linenoiseHistorySetMaxLen (len : int) return int
		with Import => True, Convention => C,
			External_Name => "linenoiseHistorySetMaxLen";

	function linenoiseHistorySave (filename : chars_ptr) return int
		with Import => True, Convention => C,
			External_Name => "linenoiseHistorySave";

	function linenoiseHistoryLoad (filename : chars_ptr) return int
		with Import => True, Convention => C,
			External_Name => "linenoiseHistoryLoad";

	function linenoiseSetMultiLine (ml : int) return int
		with Import => True, Convention => C,
			External_Name => "linenoiseSetMultiLine";

	procedure linenoiseMaskModeEnable
		with Import => True, Convention => C,
			External_Name => "linenoiseMaskModeEnable";

	procedure linenoiseMaskModeDisable
		with Import => True, Convention => C,
			External_Name => "linenoiseMaskModeDisable";


	-- Linenoise UTF-8 extensions

	type linenoisePrevCharLen is access
		function (
			buf : chars_ptr; buf_len : size_t;
			pos : size_t; col_len : access size_t
		) return size_t
			with Convention => C;

	type linenoiseNextCharLen is access
		function (
			buf : chars_ptr; buf_len : size_t;
			pos : size_t; col_len : access size_t
		) return size_t
			with Convention => C;

	type linenoiseReadCode is access
		function (fd : int; buf : chars_ptr; buf_len : size_t; c : access int)
			return size_t
			with Convention => C;

	procedure linenoiseSetEncodingFunctions (
		prevCharLenFunc : linenoisePrevCharLen;
		nextCharLenFunc : linenoiseNextCharLen;
		readCodeFunc : linenoiseReadCode
	) with Import => True, Convention => C,
		External_Name => "linenoiseSetEncodingFunctions";

	function linenoiseUtf8PrevCharLen (
		buf : chars_ptr; buf_len : size_t;
		pos : size_t; col_len : access size_t
	) return size_t
		with Import => True, Convention => C,
			External_Name => "linenoiseUtf8PrevCharLen";

	function linenoiseUtf8NextCharLen (
		buf : chars_ptr; buf_len : size_t;
		pos : size_t; col_len : access size_t
	) return size_t
		with Import => True, Convention => C,
			External_Name => "linenoiseUtf8NextCharLen";

	function linenoiseUtf8ReadCode (
		fd : int; buf : chars_ptr; buf_len : size_t; c : access int
	) return size_t
		with Import => True, Convention => C,
			External_Name => "linenoiseUtf8ReadCode";


	------------------------------
	-- Private Helper Functions --
	------------------------------

	procedure Completion_Wrapper
		(line : chars_ptr; lc : access linenoiseCompletions)
		with Export => True, Convention => C;

	function Hint_Wrapper
		(line : chars_ptr; color : access int; bold : access int)
		return chars_ptr
		with Export => True, Convention => C;

	function hint_string_helper (str : chars_ptr) return chars_ptr
		with Import => True, Convention => C,
			External_Name => "hint_string_helper";

	procedure free_hint_helper (ptr : chars_ptr)
		with Import => True, Convention => C,
			External_Name => "free_hint_helper";

end Linenoise;
