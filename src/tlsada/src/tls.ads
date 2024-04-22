-- Base TLS package, containing some helper functions and common elements.
--
--
-- Copyright (c) 2022 nytpu <alex [at] nytpu.com>
-- SPDX-License-Identifier: MPL-2.0
-- For more license details, see LICENSE or <https://www.mozilla.org/en-US/MPL/2.0/>.

pragma Ada_2012;

with Ada.IO_Exceptions;
with Ada.Streams;
with Ada.Strings.Maps;  use Ada.Strings.Maps;

package TLS is
	pragma Preelaborate;

	----------------------
	-- Helper Functions --
	----------------------

	-- Return the file path that contains the system's default Certificate
	-- Authority certificates.
	function Get_Default_CA_File return String;

	-- Helper funtion to read an arbitrary-length string from a stream (up
	-- to the length given in Max_Length if Max_Length /= 0) until a character
	-- in the Delimiters set is encountered.  The string is returned, NOT
	-- including the delimiter.
	-- Encountered_Delim will be set to True if the line was returned due to
	-- encountering a delimiter, and will be set to False if the line was
	-- returned due to reaching the end of the available data (i.e. an
	-- End_Error was raised).
	function Get_Delim (
		Stream : access Ada.Streams.Root_Stream_Type'Class;
		Delimiters : Character_Set;
		Encountered_Delim : out Boolean;
		Max_Length : Natural := 0
	) return String
		with Post =>
			(if Max_Length > 0 then Get_Delim'Result'Length <= Max_Length) and
			(for all C of Get_Delim'Result =>
				not Is_In(C, Delimiters));

	-- Equivalent to the previous Get_Delim, but use the given string as a
	-- delimiter instead of a single character from a set.
	function Get_Delim (
		Stream : access Ada.Streams.Root_Stream_Type'Class;
		Delimiter : String;
		Encountered_Delim : out Boolean;
		Max_Length : Natural := 0
	) return String
		with
			Pre => Delimiter'Length > 0,
			Post =>
				(if Max_Length > 0 then Get_Delim'Result'Length <= Max_Length) and
				Delimiter not in Get_Delim'Result;


	------------
	-- Errors --
	------------

	-- Raised if there is an error not otherwise covered by other exceptions.
	-- The exception message will contain the details of the error.
	TLS_Error : exception;

	-- Raised if there is an internal error during the course of processing a
	-- Config.  The exception message will contain the details of the error.
	TLS_Config_Error : exception;

	-- Raised if an error occurs opening or closing a TLS connection.  The
	-- exception message wll contain the details of the error.
	Connect_Error : exception;

	-- Raised if an error occurs during I/O.  The exception message will
	-- contain the details of the error.
	Device_Error : exception renames Ada.IO_Exceptions.Device_Error;

	-- Raised when no recievable data is available.
	-- It can be assumed that the connection was closed or the remote is
	-- awaiting a response.
	End_Error : exception renames Ada.IO_Exceptions.End_Error;

end TLS;
