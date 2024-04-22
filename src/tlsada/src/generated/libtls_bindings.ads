-- Automatically generated bindings to tls.h, manually revised and simplified.
--
-- Generated using:
--     gcc -c -fdump-ada-spec -C /usr/include/tls.h
-- on an x86-64 Artix Linux system using OpenSSL and LibreTLS.
--
-- Copyright (c) 2014 Joel Sing <jsing@openbsd.org>
-- Copyright (c) 2022 nytpu <alex [at] nytpu.com>
--
-- Permission to use, copy, modify, and distribute this software for any
-- purpose with or without fee is hereby granted, provided that the above
-- copyright notice and this permission notice appear in all copies.
--
-- THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
-- WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
-- MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
-- ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
-- WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
-- ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
-- OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

pragma Ada_2012;
pragma Style_Checks(Off);

with Interfaces;  use Interfaces;
with Interfaces.C;  use Interfaces.C;
with Interfaces.C.Strings;  use Interfaces.C.Strings;
with System;

package libTLS_Bindings is
	pragma Preelaborate;
	pragma Linker_Options("-ltls");

	-- FIXME: somehow extract the implementation-dependent types
	subtype ssize_t is long; -- /usr/include/sys/types.h:108
	subtype time_t is long; -- /usr/include/bits/types/time_t.h:7

	TLS_API : constant := 20200120; -- /usr/include/tls.h:37

	TLS_PROTOCOL_TLSv1_0 : constant := (2 ** 1); -- /usr/include/tls.h:39
	TLS_PROTOCOL_TLSv1_1 : constant := (2 ** 2); -- /usr/include/tls.h:40
	TLS_PROTOCOL_TLSv1_2 : constant := (2 ** 3); -- /usr/include/tls.h:41
	TLS_PROTOCOL_TLSv1_3 : constant := (2 ** 4); -- /usr/include/tls.h:42

	-- unsupported macro: TLS_PROTOCOL_TLSv1 (TLS_PROTOCOL_TLSv1_0|TLS_PROTOCOL_TLSv1_1| TLS_PROTOCOL_TLSv1_2|TLS_PROTOCOL_TLSv1_3)
	TLS_PROTOCOL_TLSv1 : constant := TLS_PROTOCOL_TLSv1_0 + TLS_PROTOCOL_TLSv1_1 + TLS_PROTOCOL_TLSv1_2 + TLS_PROTOCOL_TLSv1_3;

	-- unsupported macro: TLS_PROTOCOLS_ALL TLS_PROTOCOL_TLSv1
	TLS_PROTOCOLS_ALL : constant := TLS_PROTOCOL_TLSv1;

	-- unsupported macro: TLS_PROTOCOLS_DEFAULT (TLS_PROTOCOL_TLSv1_2|TLS_PROTOCOL_TLSv1_3)
	TLS_PROTOCOLS_DEFAULT : constant := TLS_PROTOCOL_TLSv1_2 + TLS_PROTOCOL_TLSv1_3;

	TLS_WANT_POLLIN : constant := -2; -- /usr/include/tls.h:51
	TLS_WANT_POLLOUT : constant := -3; -- /usr/include/tls.h:52

	TLS_OCSP_RESPONSE_SUCCESSFUL : constant := 0; -- /usr/include/tls.h:55
	TLS_OCSP_RESPONSE_MALFORMED : constant := 1; -- /usr/include/tls.h:56
	TLS_OCSP_RESPONSE_INTERNALERROR : constant := 2; -- /usr/include/tls.h:57
	TLS_OCSP_RESPONSE_TRYLATER : constant := 3; -- /usr/include/tls.h:58
	TLS_OCSP_RESPONSE_SIGREQUIRED : constant := 4; -- /usr/include/tls.h:59
	TLS_OCSP_RESPONSE_UNAUTHORIZED : constant := 5; -- /usr/include/tls.h:60

	TLS_OCSP_CERT_GOOD : constant := 0; -- /usr/include/tls.h:63
	TLS_OCSP_CERT_REVOKED : constant := 1; -- /usr/include/tls.h:64
	TLS_OCSP_CERT_UNKNOWN : constant := 2; -- /usr/include/tls.h:65

	TLS_CRL_REASON_UNSPECIFIED : constant := 0; -- /usr/include/tls.h:68
	TLS_CRL_REASON_KEY_COMPROMISE : constant := 1; -- /usr/include/tls.h:69
	TLS_CRL_REASON_CA_COMPROMISE : constant := 2; -- /usr/include/tls.h:70
	TLS_CRL_REASON_AFFILIATION_CHANGED : constant := 3; -- /usr/include/tls.h:71
	TLS_CRL_REASON_SUPERSEDED : constant := 4; -- /usr/include/tls.h:72
	TLS_CRL_REASON_CESSATION_OF_OPERATION : constant := 5; -- /usr/include/tls.h:73
	TLS_CRL_REASON_CERTIFICATE_HOLD : constant := 6; -- /usr/include/tls.h:74
	TLS_CRL_REASON_REMOVE_FROM_CRL : constant := 8; -- /usr/include/tls.h:75
	TLS_CRL_REASON_PRIVILEGE_WITHDRAWN : constant := 9; -- /usr/include/tls.h:76
	TLS_CRL_REASON_AA_COMPROMISE : constant := 10; -- /usr/include/tls.h:77

	TLS_MAX_SESSION_ID_LENGTH : constant := 32; -- /usr/include/tls.h:79
	TLS_TICKET_KEY_SIZE : constant := 48; -- /usr/include/tls.h:80

	-- RFC 6960 Section 2.3
	-- RFC 6960 Section 2.2
	-- RFC 5280 Section 5.3.1
	type tls is null record; -- incomplete struct

	type tls_config is null record; -- incomplete struct

	type tls_read_cb is access function
		(arg1 : access tls;
		arg2 : System.Address;
		arg3 : size_t;
		arg4 : System.Address) return ssize_t
	with Convention => C; -- /usr/include/tls.h:85

	type tls_write_cb is access function
		(arg1 : access tls;
		arg2 : System.Address;
		arg3 : size_t;
		arg4 : System.Address) return ssize_t
	with Convention => C; -- /usr/include/tls.h:87

	function tls_init return int -- /usr/include/tls.h:90
	with Import => True,
		Convention => C,
		External_Name => "tls_init";

	function tls_config_error (u_config : access tls_config) return chars_ptr -- /usr/include/tls.h:92
	with Import => True,
		Convention => C,
		External_Name => "tls_config_error";

	function tls_error (u_ctx : access tls) return chars_ptr -- /usr/include/tls.h:93
	with Import => True,
		Convention => C,
		External_Name => "tls_error";

	function tls_config_new return access tls_config -- /usr/include/tls.h:95
	with Import => True,
		Convention => C,
		External_Name => "tls_config_new";

	procedure tls_config_free (u_config : access tls_config) -- /usr/include/tls.h:96
	with Import => True,
		Convention => C,
		External_Name => "tls_config_free";

	function tls_default_ca_cert_file return chars_ptr -- /usr/include/tls.h:98
	with Import => True,
		Convention => C,
		External_Name => "tls_default_ca_cert_file";

	function tls_config_add_keypair_file
		(u_config : access tls_config;
		u_cert_file : chars_ptr;
		u_key_file : chars_ptr) return int -- /usr/include/tls.h:100
	with Import => True,
		Convention => C,
		External_Name => "tls_config_add_keypair_file";

	function tls_config_add_keypair_mem
		(u_config : access tls_config;
		u_cert : access Unsigned_8;
		u_cert_len : size_t;
		u_key : access Unsigned_8;
		u_key_len : size_t) return int -- /usr/include/tls.h:102
	with Import => True,
		Convention => C,
		External_Name => "tls_config_add_keypair_mem";

	function tls_config_add_keypair_ocsp_file
		(u_config : access tls_config;
		u_cert_file : chars_ptr;
		u_key_file : chars_ptr;
		u_ocsp_staple_file : chars_ptr) return int -- /usr/include/tls.h:104
	with Import => True,
		Convention => C,
		External_Name => "tls_config_add_keypair_ocsp_file";

	function tls_config_add_keypair_ocsp_mem
		(u_config : access tls_config;
		u_cert : access Unsigned_8;
		u_cert_len : size_t;
		u_key : access Unsigned_8;
		u_key_len : size_t;
		u_staple : access Unsigned_8;
		u_staple_len : size_t) return int -- /usr/include/tls.h:107
	with Import => True,
		Convention => C,
		External_Name => "tls_config_add_keypair_ocsp_mem";

	function tls_config_set_alpn (u_config : access tls_config; u_alpn : chars_ptr) return int -- /usr/include/tls.h:110
	with Import => True,
		Convention => C,
		External_Name => "tls_config_set_alpn";

	function tls_config_set_ca_file (u_config : access tls_config; u_ca_file : chars_ptr) return int -- /usr/include/tls.h:111
	with Import => True,
		Convention => C,
		External_Name => "tls_config_set_ca_file";

	function tls_config_set_ca_path (u_config : access tls_config; u_ca_path : chars_ptr) return int -- /usr/include/tls.h:112
	with Import => True,
		Convention => C,
		External_Name => "tls_config_set_ca_path";

	function tls_config_set_ca_mem
		(u_config : access tls_config;
		u_ca : access Unsigned_8;
		u_len : size_t) return int -- /usr/include/tls.h:113
	with Import => True,
		Convention => C,
		External_Name => "tls_config_set_ca_mem";

	function tls_config_set_cert_file (u_config : access tls_config; u_cert_file : chars_ptr) return int -- /usr/include/tls.h:115
	with Import => True,
		Convention => C,
		External_Name => "tls_config_set_cert_file";

	function tls_config_set_cert_mem
		(u_config : access tls_config;
		u_cert : access Unsigned_8;
		u_len : size_t) return int -- /usr/include/tls.h:117
	with Import => True,
		Convention => C,
		External_Name => "tls_config_set_cert_mem";

	function tls_config_set_ciphers (u_config : access tls_config; u_ciphers : chars_ptr) return int -- /usr/include/tls.h:119
	with Import => True,
		Convention => C,
		External_Name => "tls_config_set_ciphers";

	function tls_config_set_crl_file (u_config : access tls_config; u_crl_file : chars_ptr) return int -- /usr/include/tls.h:120
	with Import => True,
		Convention => C,
		External_Name => "tls_config_set_crl_file";

	function tls_config_set_crl_mem
		(u_config : access tls_config;
		u_crl : access Unsigned_8;
		u_len : size_t) return int -- /usr/include/tls.h:121
	with Import => True,
		Convention => C,
		External_Name => "tls_config_set_crl_mem";

	function tls_config_set_dheparams (u_config : access tls_config; u_params : chars_ptr) return int -- /usr/include/tls.h:123
	with Import => True,
		Convention => C,
		External_Name => "tls_config_set_dheparams";

	function tls_config_set_ecdhecurve (u_config : access tls_config; u_curve : chars_ptr) return int -- /usr/include/tls.h:124
	with Import => True,
		Convention => C,
		External_Name => "tls_config_set_ecdhecurve";

	function tls_config_set_ecdhecurves (u_config : access tls_config; u_curves : chars_ptr) return int -- /usr/include/tls.h:125
	with Import => True,
		Convention => C,
		External_Name => "tls_config_set_ecdhecurves";

	function tls_config_set_key_file (u_config : access tls_config; u_key_file : chars_ptr) return int -- /usr/include/tls.h:126
	with Import => True,
		Convention => C,
		External_Name => "tls_config_set_key_file";

	function tls_config_set_key_mem
		(u_config : access tls_config;
		u_key : access Unsigned_8;
		u_len : size_t) return int -- /usr/include/tls.h:127
	with Import => True,
		Convention => C,
		External_Name => "tls_config_set_key_mem";

	function tls_config_set_keypair_file
		(u_config : access tls_config;
		u_cert_file : chars_ptr;
		u_key_file : chars_ptr) return int -- /usr/include/tls.h:129
	with Import => True,
		Convention => C,
		External_Name => "tls_config_set_keypair_file";

	function tls_config_set_keypair_mem
		(u_config : access tls_config;
		u_cert : access Unsigned_8;
		u_cert_len : size_t;
		u_key : access Unsigned_8;
		u_key_len : size_t) return int -- /usr/include/tls.h:131
	with Import => True,
		Convention => C,
		External_Name => "tls_config_set_keypair_mem";

	function tls_config_set_keypair_ocsp_file
		(u_config : access tls_config;
		u_cert_file : chars_ptr;
		u_key_file : chars_ptr;
		u_staple_file : chars_ptr) return int -- /usr/include/tls.h:133
	with Import => True,
		Convention => C,
		External_Name => "tls_config_set_keypair_ocsp_file";

	function tls_config_set_keypair_ocsp_mem
		(u_config : access tls_config;
		u_cert : access Unsigned_8;
		u_cert_len : size_t;
		u_key : access Unsigned_8;
		u_key_len : size_t;
		u_staple : access Unsigned_8;
		staple_len : size_t) return int -- /usr/include/tls.h:135
	with Import => True,
		Convention => C,
		External_Name => "tls_config_set_keypair_ocsp_mem";

	function tls_config_set_ocsp_staple_mem
		(u_config : access tls_config;
		u_staple : access Unsigned_8;
		u_len : size_t) return int -- /usr/include/tls.h:138
	with Import => True,
		Convention => C,
		External_Name => "tls_config_set_ocsp_staple_mem";

	function tls_config_set_ocsp_staple_file (u_config : access tls_config; u_staple_file : chars_ptr) return int -- /usr/include/tls.h:140
	with Import => True,
		Convention => C,
		External_Name => "tls_config_set_ocsp_staple_file";

	function tls_config_set_protocols (u_config : access tls_config; u_protocols : Unsigned_32) return int -- /usr/include/tls.h:142
	with Import => True,
		Convention => C,
		External_Name => "tls_config_set_protocols";

	function tls_config_set_session_fd (u_config : access tls_config; u_session_fd : int) return int -- /usr/include/tls.h:143
	with Import => True,
		Convention => C,
		External_Name => "tls_config_set_session_fd";

	function tls_config_set_verify_depth (u_config : access tls_config; u_verify_depth : int) return int -- /usr/include/tls.h:144
	with Import => True,
		Convention => C,
		External_Name => "tls_config_set_verify_depth";

	procedure tls_config_prefer_ciphers_client (u_config : access tls_config) -- /usr/include/tls.h:146
	with Import => True,
		Convention => C,
		External_Name => "tls_config_prefer_ciphers_client";

	procedure tls_config_prefer_ciphers_server (u_config : access tls_config) -- /usr/include/tls.h:147
	with Import => True,
		Convention => C,
		External_Name => "tls_config_prefer_ciphers_server";

	procedure tls_config_insecure_noverifycert (u_config : access tls_config) -- /usr/include/tls.h:149
	with Import => True,
		Convention => C,
		External_Name => "tls_config_insecure_noverifycert";

	procedure tls_config_insecure_noverifyname (u_config : access tls_config) -- /usr/include/tls.h:150
	with Import => True,
		Convention => C,
		External_Name => "tls_config_insecure_noverifyname";

	procedure tls_config_insecure_noverifytime (u_config : access tls_config) -- /usr/include/tls.h:151
	with Import => True,
		Convention => C,
		External_Name => "tls_config_insecure_noverifytime";

	procedure tls_config_verify (u_config : access tls_config) -- /usr/include/tls.h:152
	with Import => True,
		Convention => C,
		External_Name => "tls_config_verify";

	procedure tls_config_ocsp_require_stapling (u_config : access tls_config) -- /usr/include/tls.h:154
	with Import => True,
		Convention => C,
		External_Name => "tls_config_ocsp_require_stapling";

	procedure tls_config_verify_client (u_config : access tls_config) -- /usr/include/tls.h:155
	with Import => True,
		Convention => C,
		External_Name => "tls_config_verify_client";

	procedure tls_config_verify_client_optional (u_config : access tls_config) -- /usr/include/tls.h:156
	with Import => True,
		Convention => C,
		External_Name => "tls_config_verify_client_optional";

	procedure tls_config_clear_keys (u_config : access tls_config) -- /usr/include/tls.h:158
	with Import => True,
		Convention => C,
		External_Name => "tls_config_clear_keys";

	function tls_config_parse_protocols (u_protocols : access Unsigned_32; u_protostr : chars_ptr) return int -- /usr/include/tls.h:159
	with Import => True,
		Convention => C,
		External_Name => "tls_config_parse_protocols";

	function tls_config_set_session_id
		(u_config : access tls_config;
		u_session_id : access unsigned_char;
		u_len : size_t) return int -- /usr/include/tls.h:161
	with Import => True,
		Convention => C,
		External_Name => "tls_config_set_session_id";

	function tls_config_set_session_lifetime (u_config : access tls_config; u_lifetime : int) return int -- /usr/include/tls.h:163
	with Import => True,
		Convention => C,
		External_Name => "tls_config_set_session_lifetime";

	function tls_config_add_ticket_key
		(u_config : access tls_config;
		u_keyrev : Unsigned_32;
		u_key : access unsigned_char;
		u_keylen : size_t) return int -- /usr/include/tls.h:164
	with Import => True,
		Convention => C,
		External_Name => "tls_config_add_ticket_key";

	function tls_client return access tls -- /usr/include/tls.h:167
	with Import => True,
		Convention => C,
		External_Name => "tls_client";

	function tls_server return access tls -- /usr/include/tls.h:168
	with Import => True,
		Convention => C,
		External_Name => "tls_server";

	function tls_configure (u_ctx : access tls; u_config : access tls_config) return int -- /usr/include/tls.h:169
	with Import => True,
		Convention => C,
		External_Name => "tls_configure";

	procedure tls_reset (u_ctx : access tls) -- /usr/include/tls.h:170
	with Import => True,
		Convention => C,
		External_Name => "tls_reset";

	procedure tls_free (u_ctx : access tls) -- /usr/include/tls.h:171
	with Import => True,
		Convention => C,
		External_Name => "tls_free";

	function tls_accept_fds
		(u_ctx : access tls;
		u_cctx : System.Address;
		u_fd_read : int;
		u_fd_write : int) return int -- /usr/include/tls.h:173
	with Import => True,
		Convention => C,
		External_Name => "tls_accept_fds";

	function tls_accept_socket
		(u_ctx : access tls;
		u_cctx : System.Address;
		u_socket : int) return int -- /usr/include/tls.h:175
	with Import => True,
		Convention => C,
		External_Name => "tls_accept_socket";

	function tls_accept_cbs
		(u_ctx : access tls;
		u_cctx : System.Address;
		u_read_cb : tls_read_cb;
		u_write_cb : tls_write_cb;
		u_cb_arg : System.Address) return int -- /usr/include/tls.h:176
	with Import => True,
		Convention => C,
		External_Name => "tls_accept_cbs";

	function tls_connect
		(u_ctx : access tls;
		u_host : chars_ptr;
		u_port : chars_ptr) return int -- /usr/include/tls.h:178
	with Import => True,
		Convention => C,
		External_Name => "tls_connect";

	function tls_connect_fds
		(u_ctx : access tls;
		u_fd_read : int;
		u_fd_write : int;
		u_servername : chars_ptr) return int -- /usr/include/tls.h:179
	with Import => True,
		Convention => C,
		External_Name => "tls_connect_fds";

	function tls_connect_servername
		(u_ctx : access tls;
		u_host : chars_ptr;
		u_port : chars_ptr;
		u_servername : chars_ptr) return int -- /usr/include/tls.h:181
	with Import => True,
		Convention => C,
		External_Name => "tls_connect_servername";

	function tls_connect_socket
		(u_ctx : access tls;
		u_s : int;
		u_servername : chars_ptr) return int -- /usr/include/tls.h:183
	with Import => True,
		Convention => C,
		External_Name => "tls_connect_socket";

	function tls_connect_cbs
		(u_ctx : access tls;
		u_read_cb : tls_read_cb;
		u_write_cb : tls_write_cb;
		u_cb_arg : System.Address;
		u_servername : chars_ptr) return int -- /usr/include/tls.h:184
	with Import => True,
		Convention => C,
		External_Name => "tls_connect_cbs";

	function tls_handshake (u_ctx : access tls) return int -- /usr/include/tls.h:186
	with Import => True,
		Convention => C,
		External_Name => "tls_handshake";

	function tls_read
		(u_ctx : access tls;
		u_buf : System.Address;
		u_buflen : size_t) return ssize_t -- /usr/include/tls.h:187
	with Import => True,
		Convention => C,
		External_Name => "tls_read";

	function tls_write
		(u_ctx : access tls;
		u_buf : System.Address;
		u_buflen : size_t) return ssize_t -- /usr/include/tls.h:188
	with Import => True,
		Convention => C,
		External_Name => "tls_write";

	function tls_close (u_ctx : access tls) return int -- /usr/include/tls.h:189
	with Import => True,
		Convention => C,
		External_Name => "tls_close";

	function tls_peer_cert_provided (u_ctx : access tls) return int -- /usr/include/tls.h:191
	with Import => True,
		Convention => C,
		External_Name => "tls_peer_cert_provided";

	function tls_peer_cert_contains_name (u_ctx : access tls; u_name : chars_ptr) return int -- /usr/include/tls.h:192
	with Import => True,
		Convention => C,
		External_Name => "tls_peer_cert_contains_name";

	function tls_peer_cert_hash (u_ctx : access tls) return chars_ptr -- /usr/include/tls.h:194
	with Import => True,
		Convention => C,
		External_Name => "tls_peer_cert_hash";

	function tls_peer_cert_issuer (u_ctx : access tls) return chars_ptr -- /usr/include/tls.h:195
	with Import => True,
		Convention => C,
		External_Name => "tls_peer_cert_issuer";

	function tls_peer_cert_subject (u_ctx : access tls) return chars_ptr -- /usr/include/tls.h:196
	with Import => True,
		Convention => C,
		External_Name => "tls_peer_cert_subject";

	function tls_peer_cert_notbefore (u_ctx : access tls) return time_t -- /usr/include/tls.h:197
	with Import => True,
		Convention => C,
		External_Name => "tls_peer_cert_notbefore";

	function tls_peer_cert_notafter (u_ctx : access tls) return time_t -- /usr/include/tls.h:198
	with Import => True,
		Convention => C,
		External_Name => "tls_peer_cert_notafter";

	function tls_peer_cert_chain_pem (u_ctx : access tls; u_len : access size_t) return access Unsigned_8 -- /usr/include/tls.h:199
	with Import => True,
		Convention => C,
		External_Name => "tls_peer_cert_chain_pem";

	function tls_conn_alpn_selected (u_ctx : access tls) return chars_ptr -- /usr/include/tls.h:201
	with Import => True,
		Convention => C,
		External_Name => "tls_conn_alpn_selected";

	function tls_conn_cipher (u_ctx : access tls) return chars_ptr -- /usr/include/tls.h:202
	with Import => True,
		Convention => C,
		External_Name => "tls_conn_cipher";

	function tls_conn_cipher_strength (u_ctx : access tls) return int -- /usr/include/tls.h:203
	with Import => True,
		Convention => C,
		External_Name => "tls_conn_cipher_strength";

	function tls_conn_servername (u_ctx : access tls) return chars_ptr -- /usr/include/tls.h:204
	with Import => True,
		Convention => C,
		External_Name => "tls_conn_servername";

	function tls_conn_session_resumed (u_ctx : access tls) return int -- /usr/include/tls.h:205
	with Import => True,
		Convention => C,
		External_Name => "tls_conn_session_resumed";

	function tls_conn_version (u_ctx : access tls) return chars_ptr -- /usr/include/tls.h:206
	with Import => True,
		Convention => C,
		External_Name => "tls_conn_version";

	function tls_load_file
		(u_file : chars_ptr;
		u_len : access size_t;
		u_password : chars_ptr) return access Unsigned_8 -- /usr/include/tls.h:208
	with Import => True,
		Convention => C,
		External_Name => "tls_load_file";

	procedure tls_unload_file (u_buf : access Unsigned_8; len : size_t) -- /usr/include/tls.h:209
	with Import => True,
		Convention => C,
		External_Name => "tls_unload_file";

	function tls_ocsp_process_response
		(u_ctx : access tls;
		u_response : access unsigned_char;
		u_size : size_t) return int -- /usr/include/tls.h:211
	with Import => True,
		Convention => C,
		External_Name => "tls_ocsp_process_response";

	function tls_peer_ocsp_cert_status (u_ctx : access tls) return int -- /usr/include/tls.h:213
	with Import => True,
		Convention => C,
		External_Name => "tls_peer_ocsp_cert_status";

	function tls_peer_ocsp_crl_reason (u_ctx : access tls) return int -- /usr/include/tls.h:214
	with Import => True,
		Convention => C,
		External_Name => "tls_peer_ocsp_crl_reason";

	function tls_peer_ocsp_next_update (u_ctx : access tls) return time_t -- /usr/include/tls.h:215
	with Import => True,
		Convention => C,
		External_Name => "tls_peer_ocsp_next_update";

	function tls_peer_ocsp_response_status (u_ctx : access tls) return int -- /usr/include/tls.h:216
	with Import => True,
		Convention => C,
		External_Name => "tls_peer_ocsp_response_status";

	function tls_peer_ocsp_result (u_ctx : access tls) return chars_ptr -- /usr/include/tls.h:217
	with Import => True,
		Convention => C,
		External_Name => "tls_peer_ocsp_result";

	function tls_peer_ocsp_revocation_time (u_ctx : access tls) return time_t -- /usr/include/tls.h:218
	with Import => True,
		Convention => C,
		External_Name => "tls_peer_ocsp_revocation_time";

	function tls_peer_ocsp_this_update (u_ctx : access tls) return time_t -- /usr/include/tls.h:219
	with Import => True,
		Convention => C,
		External_Name => "tls_peer_ocsp_this_update";

	function tls_peer_ocsp_url (u_ctx : access tls) return chars_ptr -- /usr/include/tls.h:220
	with Import => True,
		Convention => C,
		External_Name => "tls_peer_ocsp_url";

end libTLS_Bindings;
