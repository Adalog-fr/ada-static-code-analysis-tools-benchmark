-- Base TLS package, containing some helper functions and common elements.
--
--
-- Copyright (c) 2022 nytpu <alex [at] nytpu.com>
-- SPDX-License-Identifier: MPL-2.0
-- For more license details, see LICENSE or <https://www.mozilla.org/en-US/MPL/2.0/>.

pragma Ada_2012;

with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with Interfaces.C.Strings;  use Interfaces.C.Strings;
with libTLS_Bindings;

package body TLS is

	package Bs renames libTLS_Bindings;
	pragma Style_Checks(Off, Bs);


	-------------------------
	-- Get_Default_CA_File --
	-------------------------

	function Get_Default_CA_File return String is
		R : constant chars_ptr := Bs.tls_default_ca_cert_file;
	begin
		if R = Null_Ptr then
			raise TLS_Error;
		end if;
		return Value(R);
		-- Bs.tls_default_ca_cert_file returns a pointer to a static value, so
		-- it does not need to be freed
	end Get_Default_CA_File;


	---------------
	-- Get_Delim --
	---------------

	function Get_Delim
		(Stream : access Ada.Streams.Root_Stream_Type'Class;
		Delimiters : Character_Set;
		Encountered_Delim : out Boolean;
		Max_Length : Natural := 0)
		return String
	is
		C : Character;
		O : Unbounded_String := Null_Unbounded_String;
	begin
		Encountered_Delim := False;
		loop
			begin
				Character'Read(Stream, C);
				if Is_In(C, Delimiters) then
					Encountered_Delim := True;
					exit;
				end if;
				Append(O, C);
				exit when Max_Length > 0 and Length(O) >= Max_Length;

			exception
				when End_Error => exit;
			end;
		end loop;
		return To_String(O);
	end Get_Delim;

	function Get_Delim
		(Stream : access Ada.Streams.Root_Stream_Type'Class;
		Delimiter : String;
		Encountered_Delim : out Boolean;
		Max_Length : Natural := 0)
		return String
	is
		Sliding : String(Delimiter'Range);
		C : Character;
		O : Unbounded_String := Null_Unbounded_String;
	begin
		Encountered_Delim := False;

		-- Fill the sliding buffer first before entering the main loop.
		for I in Sliding'First .. Sliding'Last loop
			begin
				Character'Read(Stream, Sliding(I));
			exception
				when End_Error =>
					if I = Sliding'First then
						return "";
					end if;
					return Sliding(Sliding'First .. I - 1);
			end;
		end loop;
		if Sliding = Delimiter then
			Encountered_Delim := True;
			return "";
		end if;

		loop
			begin
				Character'Read(Stream, C);
				Append(O, Sliding(Sliding'First));
				if Max_Length > 0 and Length(O) >= (Max_Length - Delimiter'Length) then
					Append(O, Sliding(Sliding'First + 1 .. Sliding'Last));
					exit;
				end if;
				Sliding := Sliding(Sliding'First + 1 .. Sliding'Last) & C;
				if Sliding = Delimiter then
					Encountered_Delim := True;
					exit;
				end if;

			exception
				when End_Error =>
					Append(O, Sliding);
					exit;
			end;
		end loop;
		return To_String(O);
	end Get_Delim;


end TLS;
