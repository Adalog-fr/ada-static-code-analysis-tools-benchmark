-- The base types and operations used by all context types
--
--
-- Copyright (c) 2022 nytpu <alex [at] nytpu.com>
-- SPDX-License-Identifier: MPL-2.0
-- For more license details, see LICENSE or <https://www.mozilla.org/en-US/MPL/2.0/>.

pragma Ada_2012;

with Ada.Calendar.Formatting;
with Interfaces;
with Interfaces.C;  use Interfaces.C;
with System.Storage_Elements;

pragma Style_Checks(Off);

package body TLS.Contexts is

	package Bs renames libTLS_Bindings;


-------------------------------------------------------------------------------
--------------------------------- Base Context --------------------------------
-------------------------------------------------------------------------------

	--------------------
	-- Is_Initialized --
	--------------------

	function Is_Initialized (Ctx : Context) return Boolean
	is (Ctx.Context.Context /= null);


	------------------
	-- Is_Configued --
	------------------

	function Is_Configued (Ctx : Context) return Boolean
	is (Ctx.Context.Configured);


	------------------
	-- Is_Connected --
	------------------

	function Is_Connected (Ctx : Context) return Boolean
	is (Ctx.Context.Connected);


	---------------
	-- Configure --
	---------------

	procedure Configure (Ctx : in out Context; Conf : Config) is
		use Interfaces;


		-------------------
		-- Assign_String --
		-------------------

		-- This is the function prototype used for most of the
		-- tls_config_set_*_file functions
		type Config_String_Function is access
			function (C : access Bs.tls_config; S : chars_ptr) return int
				with Convention => C;

		-- Given an Ada string, convert that to a C string and call the given
		-- String_Function with it, throwing an exception if the
		-- String_Function call didn't return 0.
		procedure Assign_String
			(Cfg : access Bs.tls_config; Str : String;
			Call : Config_String_Function)
		is
			String_Holder : chars_ptr := Null_Ptr;
			RC : int;
		begin
			if Str'Length /= 0 then
				String_Holder := Create_C_String(Str);
				RC := Call(Cfg, String_Holder);
				Free(String_Holder);
				if RC /= 0 then
					raise TLS_Config_Error
						with "unable to load file: " &
							Value(Bs.tls_config_error(Cfg));
				end if;
			end if;
		end Assign_String;

		-- Same as Assign_String but take an Unbounded_String for convenience
		procedure Assign_String
			(Cfg : access Bs.tls_config; Str : Unbounded_String;
			Call : Config_String_Function)
		is
		begin
			Assign_String(Cfg, To_String(Str), Call);
		end Assign_String;

		C : access Bs.tls_config;
		RC : int;
	begin
		C := Bs.tls_config_new;
		if C = null then
			raise TLS_Config_Error
				with "unable to generate libtls tls_config";
		end if;

		if Conf.Require_OCSP_Stapling then
			Bs.tls_config_ocsp_require_stapling(C);
		end if;

		declare
			Protocols : constant Unsigned_32 :=
				(if Conf.TLSv1_0 then Bs.TLS_PROTOCOL_TLSv1_0 else 0) or
				(if Conf.TLSv1_1 then Bs.TLS_PROTOCOL_TLSv1_1 else 0) or
				(if Conf.TLSv1_2 then Bs.TLS_PROTOCOL_TLSv1_2 else 0) or
				(if Conf.TLSv1_3 then Bs.TLS_PROTOCOL_TLSv1_3 else 0);
		begin
			RC := Bs.tls_config_set_protocols(C, Protocols);
			if RC < 0 then
				raise TLS_Config_Error
					with "unable to configure protocols: " &
						Value(Bs.tls_config_error(C));
			end if;
		end;

		case Conf.Cipher_Preference is
			when Server => Bs.tls_config_prefer_ciphers_server(C);
			when Client => Bs.tls_config_prefer_ciphers_client(C);
		end case;

		-- due to the way libTLS works, we have to enable all verifications
		-- then disable one-by-one
		Bs.tls_config_verify(C);
		if not Conf.Verify_Certs then
			Bs.tls_config_insecure_noverifycert(C);
		end if;
		if not Conf.Verify_Server_Name then
			Bs.tls_config_insecure_noverifyname(C);
		end if;
		if not Conf.Verify_Expirys then
			Bs.tls_config_insecure_noverifytime(C);
		end if;

		Assign_String(C, Conf.CA_Search_Path,
			Bs.tls_config_set_ca_path'Access);
		Assign_String(C, Conf.CA_File,
			Bs.tls_config_set_ca_file'Access);
		Assign_String(C, Conf.Cert_File,
			Bs.tls_config_set_cert_file'Access);
		Assign_String(C, Conf.CRL_File,
			Bs.tls_config_set_crl_file'Access);
		Assign_String(C, Conf.Key_File,
			Bs.tls_config_set_key_file'Access);
		Assign_String(C, Conf.OCSP_Staple_File,
			Bs.tls_config_set_ocsp_staple_file'Access);

		for KP of Conf.Additional_Keypairs loop
			if Length(KP.Cert_File) /= 0 and Length(KP.Key_File) /= 0 then
				declare
					Cert, Key, OCSP : chars_ptr := Null_Ptr;
					RC : int;
				begin
					Cert := Create_C_String(To_String(KP.Cert_File));
					Key := Create_C_String(To_String(KP.Key_File));
					if Length(KP.OCSP_Staple_File) /= 0 then
						OCSP := Create_C_String(To_String(KP.OCSP_Staple_File));
						RC := Bs.tls_config_add_keypair_ocsp_file
							(C, Cert, Key, OCSP);
					else
						RC := Bs.tls_config_add_keypair_file
							(C, Cert, Key);
					end if;
					Free(Cert);
					Free(Key);
					Free(OCSP);
					if RC /= 0 then
						raise TLS_Config_Error
							with "unable to load file: " &
								Value(Bs.tls_config_error(C));
					end if;
				end;
			end if;
		end loop;

		RC := Bs.tls_config_set_verify_depth
			(C, int(Conf.Cert_Verify_Depth));
		if RC /= 0 then
			raise TLS_Config_Error
				with "unable to set verify depth: " &
					Value(Bs.tls_config_error(C));
		end if;

		case Conf.Verify_Client_Cert is
			when Mandatory => Bs.tls_config_verify_client(C);
			when Optional => Bs.tls_config_verify_client_optional(C);
			when None => null;
		end case;

		-- Unchecked and unsafe settings
		case Conf.DHE_Parameters is
			when None => null; -- current libtls default
			when Auto =>
				Assign_String(C, "auto", Bs.tls_config_set_dheparams'Access);
			when Legacy =>
				Assign_String(C, "legacy", Bs.tls_config_set_dheparams'Access);
		end case;
		Assign_String(C, Conf.ALPN_Protocols,
			Bs.tls_config_set_alpn'Access);
		Assign_String(C, Conf.Ciphers,
			Bs.tls_config_set_ciphers'Access);
		Assign_String(C, Conf.ECDHE_Curves,
			Bs.tls_config_set_ecdhecurves'Access);

		-- Actually configure the context
		RC := Bs.tls_configure(Ctx.Context.Context, C);
		Bs.tls_config_free(C);
		if RC /= 0 then
			raise TLS_Error
				with "unable to configure context: " & Retrieve_Error_Message(Ctx.Context);
		end if;
		Ctx.Context.Configured := True;
	end Configure;


	----------
	-- Read --
	----------

	overriding procedure Read
		(Ctx : in out Context;
		Item : out Stream_Element_Array; Last : out Stream_Element_Offset)
	is
		-- make sure we get the ceiling of the value (i.e. always rounded up)
		-- <https://old.reddit.com/r/ada/comments/sxxxis/convert_array_length_tofrom_size_t/hxw2v2h/>
		Stream_Element_Size : constant size_t :=
			(Stream_Element'Size + CHAR_BIT - 1) / CHAR_BIT;

		Sz : Bs.ssize_t;
	begin
		loop
			Sz := Bs.tls_read(
				Ctx.Context.Context,
				Item(Item'First)'Address,
				Item'Length * Stream_Element_Size
				-- Alternatively:
				-- size_t(Item'Size + CHAR_BIT - 1) / CHAR_BIT
			);
			exit when Sz /= Bs.TLS_WANT_POLLIN and Sz /= Bs.TLS_WANT_POLLOUT;
		end loop;

		if Sz < 0 then
			raise Device_Error with "unable to read data: " &
				Retrieve_Error_Message(Ctx.Context);
		elsif Sz = 0 then
			Last := Stream_Element_Offset'First;
		else
			-- Subtract one from Item'First since C is zero-indexed while Item
			-- isn't necessarily.
			Last := (Item'First - 1) + Stream_Element_Offset(size_t(Sz) / Stream_Element_Size);
		end if;
	end Read;


	-----------
	-- Write --
	-----------

	overriding procedure Write
		(Ctx : in out Context; Item : Stream_Element_Array)
	is
		use System.Storage_Elements;

		Remaining : Bs.ssize_t :=
			Bs.ssize_t(Item'Length) * Bs.ssize_t(Stream_Element'Size / CHAR_BIT);
		Current : System.Address := Item(Item'First)'Address;

		Sz : Bs.ssize_t;
	begin
		-- Keep going until we write the entire buffer, since tls_write can
		-- decide to write as little as it wants.
		while Remaining > 0 loop
			Sz := Bs.tls_write(
				Ctx.Context.Context,
				Current,
				size_t(Remaining)
			);
			case Sz is
				when -1 =>
					raise Device_Error with "unable to write data: " &
						Retrieve_Error_Message(Ctx.Context);

				when Bs.TLS_WANT_POLLIN | Bs.TLS_WANT_POLLOUT => null;

				when others =>
					-- FIXME: pointer arithmetic, really? There has to be a
					-- better way to do this...
					Remaining := Remaining - Sz;
					Current := Current + Storage_Offset(Sz);
			end case;
		end loop;
	end Write;


	-----------
	-- Close --
	-----------

	procedure Close (Ctx : in out Context) is
	begin
		Close(Ctx.Context);
		Bs.tls_reset(Ctx.Context.Context);
		Ctx.Context.Configured := False;
	end Close;


	-------------------------
	-- Get_Connection_Info --
	-------------------------

	function Get_Connection_Info (Ctx : Context) return Connection_Info is
		R : int;
		O : Connection_Info;
	begin
		O.TLS_Version :=
			Call_String_Function(Bs.tls_conn_version'Access, Ctx.Context);
		O.Cipher :=
			Call_String_Function(Bs.tls_conn_cipher'Access, Ctx.Context);
		O.ALPN :=
			Call_String_Function(Bs.tls_conn_alpn_selected'Access, Ctx.Context);

		R := Bs.tls_conn_cipher_strength(Ctx.Context.Context);
		if R < 0 then
			raise TLS_Error with Retrieve_Error_Message(Ctx.Context);
		end if;
		O.Cipher_Strength := Natural(R);

		return O;
	end Get_Connection_Info;


	-------------------------------
	-- Peer_Certificate_Provided --
	-------------------------------

	function Peer_Certificate_Provided (Ctx : Context) return Boolean
	is (
		case Bs.tls_peer_cert_provided(Ctx.Context.Context) is
			when 1 => True,
			when 0 => False,
			when others =>
				raise TLS_Error
					with Retrieve_Error_Message(Ctx.Context)
	);


	--------------------------
	-- Get_Certificate_Info --
	--------------------------

	function Get_Certificate_Info (Ctx : Context) return Certificate_Info is
		R : Bs.time_t;
		O : Certificate_Info;
	begin
		O.Hash :=
			Call_String_Function(Bs.tls_peer_cert_hash'Access, Ctx.Context);
		O.Issuer :=
			Call_String_Function(Bs.tls_peer_cert_issuer'Access, Ctx.Context);
		O.Subject :=
			Call_String_Function(Bs.tls_peer_cert_subject'Access, Ctx.Context);

		R := Bs.tls_peer_cert_notbefore(Ctx.Context.Context);
		if R < 0 then
			raise TLS_Error with Retrieve_Error_Message(Ctx.Context);
		end if;
		O.Not_Before := Convert_Time(R);

		R := Bs.tls_peer_cert_notafter(Ctx.Context.Context);
		if R < 0 then
			raise TLS_Error with Retrieve_Error_Message(Ctx.Context);
		end if;
		O.Not_Before := Convert_Time(R);

		return O;
	end Get_Certificate_Info;


	------------------------------------
	-- Peer_Certificate_Contains_Name --
	------------------------------------

	function Peer_Certificate_Contains_Name
		(Ctx : Context; Name : String)
		return Boolean
	is
		Name_Ptr : chars_ptr := Create_C_String(Name);
		R : int;
	begin
		R := Bs.tls_peer_cert_contains_name(Ctx.Context.Context, Name_Ptr);
		Free(Name_Ptr);
		case R is
			when 1 => return True;
			when 0 => return False;
			when others =>
				raise TLS_Error with Retrieve_Error_Message(Ctx.Context);
		end case;
	end Peer_Certificate_Contains_Name;


	-------------------
	-- Get_OCSP_Info --
	-------------------

	function Get_OCSP_Info (Ctx : Context) return OCSP_Info is
		O : OCSP_Info;
		RT : Bs.time_t;
	begin
		O.OCSP_URL :=
			Call_String_Function(Bs.tls_peer_ocsp_url'Access, Ctx.Context);

		O.Response_Status := (
			case Bs.tls_peer_ocsp_response_status(Ctx.Context.Context) is
				when Bs.TLS_OCSP_RESPONSE_SUCCESSFUL => Successful,
				when Bs.TLS_OCSP_RESPONSE_MALFORMED => Malformed,
				when Bs.TLS_OCSP_RESPONSE_INTERNALERROR => Internal_Error,
				when Bs.TLS_OCSP_RESPONSE_TRYLATER => Try_Later,
				when Bs.TLS_OCSP_RESPONSE_SIGREQUIRED => Sig_Required,
				when Bs.TLS_OCSP_RESPONSE_UNAUTHORIZED => Unauthorized,
				when others => raise TLS_Error with "no ocsp validation available"
		);

		O.Certificate_Status := (
			case Bs.tls_peer_ocsp_cert_status(Ctx.Context.Context) is
				when Bs.TLS_OCSP_CERT_GOOD => Good,
				when Bs.TLS_OCSP_CERT_REVOKED => Revoked,
				when Bs.TLS_OCSP_CERT_UNKNOWN => Unknown,
				when others => raise TLS_Error with "no ocsp validation available"
		);

		O.Revocation_Reason := (
			case Bs.tls_peer_ocsp_crl_reason(Ctx.Context.Context) is
				when Bs.TLS_CRL_REASON_UNSPECIFIED => Unspecified,
				when Bs.TLS_CRL_REASON_KEY_COMPROMISE => Key_Compromise,
				when Bs.TLS_CRL_REASON_CA_COMPROMISE => CA_Compromise,
				when Bs.TLS_CRL_REASON_AFFILIATION_CHANGED => Affiliation_Changed,
				when Bs.TLS_CRL_REASON_SUPERSEDED => Superseded,
				when Bs.TLS_CRL_REASON_CESSATION_OF_OPERATION => Cessation_of_Operation,
				when Bs.TLS_CRL_REASON_CERTIFICATE_HOLD => Certificate_Hold,
				when Bs.TLS_CRL_REASON_REMOVE_FROM_CRL => Remove_From_CRL,
				when Bs.TLS_CRL_REASON_PRIVILEGE_WITHDRAWN => Priviledge_Withdrawn,
				when Bs.TLS_CRL_REASON_AA_COMPROMISE => AA_Compromise,
				when others => raise TLS_Error with "no ocsp validation available"
		);

		RT := Bs.tls_peer_ocsp_revocation_time(Ctx.Context.Context);
		if RT < 0 then
			raise TLS_Error with "no ocsp validation available";
		end if;
		O.Revocation_Time := Convert_Time(RT);

		RT := Bs.tls_peer_ocsp_this_update(Ctx.Context.Context);
		if RT < 0 then
			raise TLS_Error with "no ocsp validation available";
		end if;
		O.This_Update := Convert_Time(RT);

		RT := Bs.tls_peer_ocsp_next_update(Ctx.Context.Context);
		if RT < 0 then
			raise TLS_Error with "no ocsp validation available";
		end if;
		O.Next_Update := Convert_Time(RT);

		return O;
	end Get_OCSP_Info;


-------------------------------------------------------------------------------
------------------------------ Helper Subprograms -----------------------------
-------------------------------------------------------------------------------

	------------------
	-- Convert_Time --
	------------------

	function Convert_Time (T : libTLS_Bindings.time_t) return Time is
		Epoch : constant Time :=
			Ada.Calendar.Formatting.Time_Of(1970, 1, 1, 0.0);
	begin
		return Epoch + Duration(T);
	end Convert_Time;


	---------------------
	-- Create_C_String --
	---------------------

	function Create_C_String (S : String) return chars_ptr is
		O : chars_ptr := New_String(S);
	begin
		if O = Null_Ptr then
			raise TLS_Error with "error allocating c string for '" & S & "'";
		end if;
		return O;
	end Create_C_String;


	----------------------------
	-- Retrieve_Error_Message --
	----------------------------

	function Retrieve_Error_Message (Ctx : Context_Wrapper) return String is
		S : constant chars_ptr := Bs.tls_error(Ctx.Context);
	begin
		if S = Null_Ptr then
			return "";
		end if;
		-- Returns a static pointer so there is no need to free it
		return Value(S);
	end Retrieve_Error_Message;


	--------------------------
	-- Call_String_Function --
	--------------------------

	function Call_String_Function
		(Func : String_Function; Ctx : Context_Wrapper)
		return Unbounded_String
	is
		S : constant chars_ptr := Func(Ctx.Context);
	begin
		if S = Null_Ptr then
			raise TLS_Error with Retrieve_Error_Message(Ctx);
		end if;
		return To_Unbounded_String(Value(S));
	end Call_String_Function;


-------------------------------------------------------------------------------
------------------------------- Context_Wrapper -------------------------------
-------------------------------------------------------------------------------

	-----------------------
	-- Initialize_Client --
	-----------------------

	procedure Initialize_Client (Object : in out Context_Wrapper) is
	begin
		Object.Context := Bs.tls_client;
		if Object.Context = null then
			raise TLS_Error
				with "unable to generate libtls tls context";
		end if;
	end Initialize_Client;


	-----------------------
	-- Initialize_Server --
	-----------------------

	procedure Initialize_Server (Object : in out Context_Wrapper) is
	begin
		Object.Context := Bs.tls_server;
		if Object.Context = null then
			raise TLS_Error
				with "unable to generate libtls tls context";
		end if;
	end Initialize_Server;


	-----------
	-- Close --
	-----------

	procedure Close (Ctx : in out Context_Wrapper) is
		R : int;
	begin
		if Ctx.Connected then
			loop
				R := Bs.tls_close(Ctx.Context);
				if R = -1 then
					raise Connect_Error
						with "unable to close connection: " &
							Retrieve_Error_Message(Ctx);
				end if;
				exit when R /= Bs.TLS_WANT_POLLIN and R /= Bs.TLS_WANT_POLLOUT;
			end loop;
			Ctx.Connected := False;
		end if;
	end Close;


	--------------
	-- Finalize --
	--------------

	overriding procedure Finalize (Ctx : in out Context_Wrapper) is
	begin
		if Ctx.Context /= null then
			Close(Ctx);

			-- Both types of internal contexts are freed the same way
			Bs.tls_free(Ctx.Context);

			Ctx.Context := null;
			Ctx.Configured := False;
		end if;
	end Finalize;

end TLS.Contexts;
