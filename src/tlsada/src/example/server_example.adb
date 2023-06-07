-- A simplistic TLS server example.
-- Use with the client example by running the server and then running the
-- client like so:
--     bin/client_example localhost 1965
--
--
-- Copyright (c) 2022 nytpu <alex [at] nytpu.com>
-- SPDX-License-Identifier: MPL-2.0
-- For more license details, see LICENSE or <https://www.mozilla.org/en-US/MPL/2.0/>.

with Ada.Calendar.Formatting;
with Ada.Directories;  use Ada.Directories;
with Ada.Text_IO;  use Ada.Text_IO;
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with GNAT.Sockets;  use GNAT.Sockets;

with TLS;
with TLS.Configure;
with TLS.Contexts;
with TLS.Contexts.Server;

procedure Server_Example is

	-----------------------------
	-- Configuration constants --
	-----------------------------

	Bind_Address : constant String := "0.0.0.0";
	Bind_Port : constant Port_Type := 1965;

	-- These must be generated for the example to run.
	-- The following command will generate a suitable cert and key:
	--     openssl req -nodes -x509 -newkey rsa:4096 -sha256 -days 9999 \
	--         -subj '/CN=localhost' -keyout key.pem -out cert.pem
	Cert_File : constant String := Compose(Current_Directory, "cert", "pem");
	Key_File : constant String := Compose(Current_Directory, "key", "pem");


	---------------
	-- Variables --
	---------------

	-- Server configuration values
	-- A server REQUIRES a Cert_File and Key_File to be specified
	Conf : constant TLS.Configure.Config := (
		Cert_File => To_Unbounded_String(Cert_File),
		Key_File => To_Unbounded_String(Key_File),
		-- For Gemini (and possibly even HTTP applications) you probably don't
		-- care about CA-certified client cert.
		Verify_Certs => False,
		Verify_Expirys => False,
		-- Do not *require* a client cert to even connect, however request one
		-- if the client has one to send.
		Verify_Client_Cert => TLS.Configure.Optional,
		-- Configuration options are sensible by default so we can leave the
		-- others alone
		others => <>
	);

	Address : Sock_Addr_Type;
	Server_Socket : Socket_Type;
	Server_TLS : aliased TLS.Contexts.Server.Server_Context;
begin
	Put_Line("Binding to '" & Bind_Address & "'");
	Put_Line("Press Control-C to exit.");

	-- Set up the unencrypted socket and then listen for incoming connections
	-- Basically taken from the example given in GNAT.Sockets, so there'll be a
	-- better explaination there:
	-- <https://en.wikibooks.org/wiki/Ada_Programming/Libraries/GNAT.Sockets>
	Address.Addr := Inet_Addr(Bind_Address);
	Address.Port := Bind_Port;
	Create_Socket(Server_Socket);
	Set_Socket_Option(Server_Socket, Socket_Level, (Reuse_Address, True));
	Bind_Socket(Server_Socket, Address);
	Listen_Socket(Server_Socket);

	-- Configure the server
	Server_TLS.Configure(Conf);

	-- Accept incoming connections and write output.  If you were fancy you
	-- could dispatch incoming connections to tasks instead of dealing with
	-- them sequentially like here.
	loop
		declare
			Connection_Socket : Socket_Type;
			Connection_TLS : aliased TLS.Contexts.Server.Server_Context;
			Cert_Info : TLS.Contexts.Certificate_Info;
		begin
			-- First accept the unencrypted connection.  If you allow
			-- unencrypted connections then do whatever you need to here to
			-- check if TLS should be enabled.
			Accept_Socket(Server_Socket, Connection_Socket, Address);

			-- Upgrade the socket to an encrypted connection
			TLS.Contexts.Server.Accept_TLS(Server_TLS, Connection_Socket, Connection_TLS);

			Put_Line("Got connection from '" & Image(Address.Addr) & "'!");
			if Connection_TLS.Peer_Certificate_Provided then
				-- Building the *_Info structures are comparatively expensive
				-- operations, so you should save the values if you need them
				-- more than once or twice.
				Cert_Info := Connection_TLS.Get_Certificate_Info;
				Put_Line("Certificate provided:");
				Put_Line("  Hash: " & To_String(Cert_Info.Hash));
				Put_Line("  Issuer: " & To_String(Cert_Info.Issuer));
				Put_Line("  Subject: " & To_String(Cert_Info.Subject));
				Put_Line("  Expiry: " & Ada.Calendar.Formatting.Image(Cert_Info.Not_After));
			else
				Put_Line("No certificate provided by " & Image(Address.Addr));
			end if;

			-- Read request header (and discard).  Read/write work exactly the
			-- same as for Client_Contexts
			declare
				E : Boolean;
				S : constant String :=
					TLS.Get_Delim(Connection_TLS'Access, ASCII.CR & ASCII.LF, E, 1024);
			begin
				if not E then
					Put_Line("Malformed request: " & S);
					String'Write(
						Connection_TLS'Access,
						"59 malformed request" & ASCII.CR & ASCII.LF
					);
				else
					String'Write(
						Connection_TLS'Access,
						"20 text/gemini;charset=utf-8" & ASCII.CR & ASCII.LF &
						"hello, world!" & ASCII.LF
					);
				end if;
			end;

			Connection_TLS.Close;
			Close_Socket(Connection_Socket);
		end;
	end loop;

	-- To avoid complicating the example don't have a signal handler, but in a
	-- real server you should add a signal handler that would close these
	-- before exiting.
	Server_TLS.Close;
	Close_Socket(Server_Socket);
end Server_Example;
