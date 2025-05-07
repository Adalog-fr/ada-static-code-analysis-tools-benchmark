-- The base types and operations used by all context types
--
--
-- Copyright (c) 2022 nytpu <alex [at] nytpu.com>
-- SPDX-License-Identifier: MPL-2.0
-- For more license details, see LICENSE or <https://www.mozilla.org/en-US/MPL/2.0/>.

pragma Ada_2012;

with Ada.Calendar;  use Ada.Calendar;
with Ada.Streams;  use Ada.Streams;
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with TLS.Configure;  use TLS.Configure;

private with Ada.Finalization;
private with Interfaces.C.Strings;
private with libTLS_Bindings;

package TLS.Contexts is

	------------------
	-- Helper Types --
	------------------

	-- A TCP Port Number
	type Port_Number is range 0 .. 65535;

	-- Information about the current connection
	type Connection_Info is record
		-- The TLS version negotiated with the peer
		TLS_Version : Unbounded_String;

		-- The cipher suite negotiated with the peer
		Cipher : Unbounded_String;

		-- The strength, in bits, of the symmetric cipher negotiated with the
		-- peer
		Cipher_Strength : Natural;

		-- The ALPN protocol selected for use with the peer.
		-- ALPN = Null_Unbounded_String if no ALPN protocol was selected.
		ALPN : Unbounded_String;
	end record;

	-- Information about the conection peer's certificate
	type Certificate_Info is record
		-- The certificate hash.  The hash of the raw peer certificate in
		-- hexadecimal format, prefixed by the hash name followed by a colon.
		-- The hash is currently always SHA-256, but may change in the future
		-- Example:
		-- "SHA256:66a3107d5ad6a058aab753eaac2047ccb2ed0e39465dd0fe5844da3e300d5172"
		-- The hash string for a certificate in file mycert.crt can be generated using the commands:
		--     h="$(openssl x509 -outform der -in mycert.crt | sha256)"
		--     printf "SHA256:%s\n" "${h}"
		Hash : Unbounded_String;

		-- The certificate issuer and subject, respectively
		Issuer, Subject : Unbounded_String;

		-- The NotBefore and NotAfter times specifying the validity period of
		-- the certificate.
		Not_Before, Not_After : Time;
	end record;

	-- The possible OCSP response codes
	type OCSP_Response_Statuses is
		(Successful, Malformed, Internal_Error, Try_Later,
		Sig_Required, Unauthorized);

	-- The possible OCSP certificate statuses
	type OCSP_Certificate_Statuses is (Good, Revoked, Unknown);

	-- The possible reasons for a certificate to be revoked
	type OCSP_Certificate_Revocation_Reasons is
		(Not_Revoked, Unspecified, Key_Compromise, CA_Compromise,
		Affiliation_Changed, Superseded, Cessation_of_Operation,
		Certificate_Hold, Remove_From_CRL, Priviledge_Withdrawn,
		AA_Compromise);

	type OCSP_Info is record
		-- The URL used for OCSP validation of the peer certificate.
		OCSP_URL : Unbounded_String;

		-- The OCSP response status per RFC 6960 § 2.3
		Response_Status : OCSP_Response_Statuses;

		-- The OCSP certificate status per RFC 6960 § 2.2
		Certificate_Status : OCSP_Certificate_Statuses;

		-- The OCSP certificate revocation reason per RFC 6960 § 5.3.1
		Revocation_Reason : OCSP_Certificate_Revocation_Reasons;

		-- The time the certificate was revoked in OCSP
		Revocation_Time : Time;

		-- The time of the most recent and next OCSP update
		This_Update, Next_Update : Time;
	end record
		with Dynamic_Predicate =>
			(if OCSP_Info.Certificate_Status /= Revoked then OCSP_Info.Revocation_Reason = Not_Revoked);


	------------------
	-- Base Context --
	------------------

	-- Has the same interface as Ada.Streams.Stream_IO
	type Context is abstract limited new Root_Stream_Type with private;

	-- Return whether a context has been properly initialized
	function Is_Initialized (Ctx : Context) return Boolean;

	-- Return whether a Context is configured or not.
	function Is_Configued (Ctx : Context) return Boolean
		with Pre => Ctx.Is_Initialized;

	-- Return whether a Context has a currently active connection.
	function Is_Connected (Ctx : Context) return Boolean
		with Pre => Ctx.Is_Initialized;

	-- Configure a Context with the given Config.  A Context may be
	-- reconfigured multiple times, and the previous configuration will be
	-- overwritten.  Note that a Config is not "tied" to a Context, i.e, if you
	-- modify the Config after calling Configure, you must call Configure again
	-- to update the Context's configuration.
	procedure Configure (Ctx : in out Context; Conf : Config)
		with
			Pre => Ctx.Is_Initialized,
			Post => Ctx.Is_Configued;

	-- Read incoming data from a Context's connection
	overriding procedure Read
		(Ctx : in out Context;
		Item : out Stream_Element_Array; Last : out Stream_Element_Offset)
		with Pre => Ctx.Is_Connected;

	-- Write data to a Context's connection
	overriding procedure Write
		(Ctx : in out Context; Item : Stream_Element_Array)
		with Pre => Ctx.Is_Connected;

	-- Close an open TLS connection, or have no effect if there is no active
	-- connection.  The Context will be suitable for reconfiguration and then
	-- reconnection.  If a connection was estabilished using an existing socket
	-- then the socket itself will NOT be closed.  Due to libtls' internals,
	-- the configuration of the context will be reset.
	-- Note that even if the peer has closed their end of the connection, you
	-- MUST close your end as well.
	procedure Close (Ctx : in out Context)
		with Post => Ctx.Is_Initialized and then (not Ctx.Is_Configued and not Ctx.Is_Connected);

	-- Return a record containing information about the current TLS connection.
	function Get_Connection_Info (Ctx : Context) return Connection_Info
		with Pre => Ctx.Is_Connected;

	-- Indicate whether the server has provided a certificate.
	function Peer_Certificate_Provided (Ctx : Context) return Boolean
		with Pre => Ctx.Is_Connected;

	-- Return a record containing information about the server's TLS
	-- certificate
	function Get_Certificate_Info (Ctx : Context) return Certificate_Info
		with Pre => Ctx.Is_Connected and Ctx.Peer_Certificate_Provided;

	-- Check if the server's certificate's SAN or CN match the given Name.
	function Peer_Certificate_Contains_Name
		(Ctx : Context; Name : String)
		return Boolean
		with Pre => Ctx.Is_Connected and Ctx.Peer_Certificate_Provided;

	function Get_OCSP_Info (Ctx : Context) return OCSP_Info
		with Pre => Ctx.Is_Connected;


private
	use Interfaces.C.Strings;

	-- Convert seconds since the Unix epoch to the standard Ada time type
	function Convert_Time (T : libTLS_Bindings.time_t) return Time;

	function Create_C_String (S : String) return chars_ptr
		with Post => Create_C_String'Result /= Null_Ptr;

	-- Wrapper to automatically deallocate the internal libtls context
	type Context_Wrapper is
		new Ada.Finalization.Limited_Controlled with
	record
		Configured, Connected : Boolean := False;
		Context : access libTLS_Bindings.tls := null;
	end record;

	-- Initialize an internal client context
	procedure Initialize_Client (Object : in out Context_Wrapper)
		with Pre => Object.Context = null,
			Post => Object.Context /= null;

	-- Initialize an internal server context
	procedure Initialize_Server (Object : in out Context_Wrapper)
		with Pre => Object.Context = null,
			Post => Object.Context /= null;

	-- Close the active TLS connection
	procedure Close (Ctx : in out Context_Wrapper);

	-- Close the TLS connection and free the context
	overriding procedure Finalize (Ctx : in out Context_Wrapper);

	-- Return an error message as retrieved by tls_error
	function Retrieve_Error_Message (Ctx : Context_Wrapper) return String
		with Pre => Ctx.Context /= null;

	-- Take a C function operating on a libtls context and returns a C string
	-- and call it with the given Context_Wrapper, and return an
	-- Unbounded_String.
	type String_Function is access
		function (C : access libTLS_Bindings.tls) return chars_ptr
			with Convention => C;
	function Call_String_Function
		(Func : String_Function; Ctx : Context_Wrapper)
		return Unbounded_String
		with Pre => Ctx.Context /= null;

	type Context is abstract limited new Ada.Streams.Root_Stream_Type with record
		Context : Context_Wrapper;
	end record;

end TLS.Contexts;
