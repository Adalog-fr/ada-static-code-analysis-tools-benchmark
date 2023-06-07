-- Configuration options for TLS contexts
--
--
-- Copyright (c) 2022 nytpu <alex [at] nytpu.com>
-- SPDX-License-Identifier: MPL-2.0
-- For more license details, see LICENSE or <https://www.mozilla.org/en-US/MPL/2.0/>.

pragma Ada_2012;

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;

package TLS.Configure is
	pragma Preelaborate;

	------------------
	-- Helper Types --
	------------------

	-- This record is used to specify filenames for supplementary certificate
	-- files that may be used by Server_Contexts.  If either a Cert_File or
	-- Key_File is provided, then the other must be provided as well.  If an
	-- OCSP_Staple_File is provided, then a Cert and Key_File must be provided.
	type Additional_Keypair is record
		Cert_File, Key_File : Unbounded_String;
		OCSP_Staple_File : Unbounded_String;
	end record
		with Dynamic_Predicate =>
			(not (Length(Cert_File) = 0 xor Length(Key_File) = 0)) and
			(if Length(OCSP_Staple_File) /= 0 then
				Length(Cert_File) /= 0 and Length(Key_File) /= 0);

	-- Vector used to hold a list of additional keypairs to be loaded by a
	-- context.
	package Keypair_Vectors is new Ada.Containers.Vectors
		(Index_Type => Natural, Element_Type => Additional_Keypair);

	-- When negotiating the connection's ciphers, specify whether the Client's
	-- request or the Server's request should be preferred.
	type Cipher_Preferences is (Client, Server);

	-- Whether a client certificate is necessary.
	-- * None: refuse client certs.
	-- * Optional: request them to be sent but don't require or validate them.
	-- * Mandatory: require a client cert be sent, and validate the given cert.
	type Client_Cert_Verification is (None, Optional, Mandatory);

	-- Diffie-Hellman Ephemeral Key Exchange parameters.
	-- * None: do not use DHE Key Exchange.
	-- * Auto: the key size for the ephemeral key is automatically selected
	--         based on the size of the private key being used for signing.
	-- * Legacy: 1024 bit ephemeral keys are used.
	type DHE_Possible_Parameters is (None, Auto, Legacy);


	---------------------
	-- The Main Config --
	---------------------

	-- Holds all the pre-connection configuration for both Context types.
	-- Reusable, can be used to configure many contexts (e.g. if you have many
	-- identically-configured contexts in different tasks)
	type Config is record
		-- Require a valid stapled OCSP response be provided during the TLS
		-- handshake.
		Require_OCSP_Stapling : Boolean := False;

		-- Enable/disable various TLS versions.  Note that at least one must be
		-- enabled.
		-- Legacy TLS versions
		TLSv1_0, TLSv1_1 : Boolean := False;
		-- Default TLS versions
		TLSv1_2, TLSv1_3 : Boolean := True;

		-- When negotiating the connection's ciphers, specify whether the
		-- Client's request or the Server's request should be preferred.
		-- Cipher_Preference only affects Server_Contexts.
		Cipher_Preference : Cipher_Preferences := Server;

		-- Should certificates be validated by a CA and OCSP?
		Verify_Certs : Boolean := True;

		-- Should the server name field be verified?
		-- Verify_Server_Name only affects Client_Contexts.
		Verify_Server_Name : Boolean := True;

		-- Should certificate's expiry not_before and not_after be verified?
		Verify_Expirys : Boolean := True;

		-- Set the filename that contains the CA root certificates.
		CA_File : Unbounded_String;

		-- Set the directory that is searched for CA root certificate files.
		CA_Search_Path : Unbounded_String;

		-- Set the primary public and private keys
		-- Optional for Client_Contexts, mandatory for Server_Contexts.
		Cert_File, Key_File : Unbounded_String;

		-- Specify additional supplementary keypairs to be used alongside the
		-- primary keypair.
		-- Additional_Keypairs only affects Server_Contexts.
		Additional_Keypairs : Keypair_Vectors.Vector;

		-- Set the file containing the Certificate Revocation List
		CRL_File : Unbounded_String;

		-- Set the file containing the ER-encoded OCSP response to be stapled
		-- during the TLS handshake.
		OCSP_Staple_File : Unbounded_String;

		-- Should client certificates be verified?
		-- Verify_Client_Cert only affects Server_Contexts.
		Verify_Client_Cert : Client_Cert_Verification := None;

		-- Set a limit on the number of intermediate certificates that will be
		-- followed during certificate validation.
		Cert_Verify_Depth : Natural := 100;

		------------------- !!!DANGEROUS!!! -------------------
		-- These fields should not typically need to be used --
		--       and can severely compromise security.       --
		-- No documentation is provided, refer to the libtls --
		--         and LibreSSL docs for information.        --
		-------------------------------------------------------
		-- Refer to tls_config_set_dheparams()
		DHE_Parameters : DHE_Possible_Parameters := None;
		-- String directly passed to tls_config_set_alpn()
		ALPN_Protocols : Unbounded_String;
		-- String directly passed to tls_config_set_ciphers()
		Ciphers : Unbounded_String;
		-- String directly passed to tls_config_set_ecdhecurves()
		ECDHE_Curves : Unbounded_String;
	end record
		with Dynamic_Predicate =>
			(Config.TLSv1_0 or Config.TLSv1_1 or Config.TLSv1_2 or Config.TLSv1_3);


	------------------------
	-- Helper Subprograms --
	------------------------

	-- Enable the more secure TLS versions (currently TLSv1.2-1.3) while
	-- disabling the insecure "legacy" versions.  This is equivalent to the
	-- default versions enabled in a new Config.
	procedure Enable_Secure_TLS (Conf : in out Config);

	-- Enable all TLSv1 versions (TLSv1.0-1.3), disabling everything else.
	procedure Enable_TLSv1 (Conf : in out Config);

	-- Enable all available TLS versions.  Currently (but not necessarily in
	-- the future) equivalent to Enable_TLSv1.
	procedure Enable_All_TLS (Conf : in out Config);

end TLS.Configure;
