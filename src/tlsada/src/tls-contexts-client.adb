-- The context used for making and managing Client connections.
--
--
-- Copyright (c) 2022 nytpu <alex [at] nytpu.com>
-- SPDX-License-Identifier: MPL-2.0
-- For more license details, see LICENSE or <https://www.mozilla.org/en-US/MPL/2.0/>.

pragma Ada_2012;

with Interfaces.C;  use Interfaces.C;

pragma Style_Checks(Off, int);

package body TLS.Contexts.Client is

	package Bs renames libTLS_Bindings;
	pragma Style_Checks(Off, Bs);


	-------------
	-- Connect --
	-------------

	procedure Connect
		(Ctx : in out Client_Context; Host : String; Port : Port_Number)
	is
		function Generate_Error_Msg (Message : String) return String
		is (
			Message &
			" '" & Host & ":" & Port_Number'Image(Port) & "':" &
			Retrieve_Error_Message(Ctx.Context)
		);

		R : int;
		Host_C : chars_ptr := New_String(Host);
		Port_C : chars_ptr := New_String(Port'Image);
	begin
		if Host_C = Null_Ptr then
			raise Connect_Error with "unable to allocate hostname string";
		elsif Port_C = Null_Ptr then
			raise Connect_Error with "unable to allocate port number string";
		end if;

		R := Bs.tls_connect(Ctx.Context.Context, Host_C, Port_C);
		Free(Host_C);
		Free(Port_C);
		if R /= 0 then
			raise Connect_Error with Generate_Error_Msg("unable to connect to");
		end if;

		R := Bs.tls_handshake(Ctx.Context.Context);
		if R /= 0 then
			raise Connect_Error with Generate_Error_Msg("unable to do TLS handshake with");
		end if;

		Ctx.Context.Connected := True;
	end Connect;

	procedure Connect
		(Ctx : in out Client_Context;
		Host : String;
		Port : Port_Number;
		Server_Name : String)
	is
		R : int;
		Host_C : chars_ptr := New_String(Host);
		Port_C : chars_ptr := New_String(Port'Image);
		Server_Name_C : chars_ptr := New_String(Server_Name);
	begin
		if Host_C = Null_Ptr then
			raise Connect_Error with "unable to allocate hostname string";
		elsif Port_C = Null_Ptr then
			raise Connect_Error with "unable to allocate port number string";
		elsif Server_Name_C = Null_Ptr then
			raise Connect_Error with "unable to allocate server name string";
		end if;

		R := Bs.tls_connect_servername(
			Ctx.Context.Context, Host_C, Port_C, Server_Name_C
		);
		Free(Host_C);
		Free(Port_C);
		Free(Server_Name_C);
		if R /= 0 then
			raise Connect_Error
				with "unable to connect to '" &
					Host & ":" & Port'Image & "': " &
					Retrieve_Error_Message(Ctx.Context);
		end if;
		Ctx.Context.Connected := True;
	end Connect;

	procedure Connect
		(Ctx : in out Client_Context;
		Socket : GNAT.Sockets.Socket_Type;
		Server_Name : String)
	is
		R : int;
		Server_Name_C : chars_ptr := New_String(Server_Name);
	begin
		if Server_Name_C = Null_Ptr then
			raise Connect_Error with "unable to allocate server name string";
		end if;
		R := Bs.tls_connect_socket(
			Ctx.Context.Context, int(To_C(Socket)), Server_Name_C
		);
		Free(Server_Name_C);
		if R /= 0 then
			raise Connect_Error
				with "unable to enable tls on socket '" &
					Image(Socket) & "': " &
					Retrieve_Error_Message(Ctx.Context);
		end if;
		Ctx.Context.Connected := True;
	end Connect;


	---------------------
	-- Session_Resumed --
	---------------------

	function Session_Resumed (Ctx : Client_Context) return Boolean
	is (
		case Bs.tls_conn_session_resumed(Ctx.Context.Context) is
			when 1 => True,
			when 0 => False,
			when others =>
				raise TLS_Error with Retrieve_Error_Message(Ctx.Context)
	);


	----------------
	-- Initialize --
	----------------

	overriding procedure Initialize (Object : in out Controlled_Mixin) is
	begin
		Initialize_Client(Object.Enclosing.Context);
	end Initialize;

end TLS.Contexts.Client;
