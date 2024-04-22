-- The context used for making and managing Server connections.
--
--
-- Copyright (c) 2022 nytpu <alex [at] nytpu.com>
-- SPDX-License-Identifier: MPL-2.0
-- For more license details, see LICENSE or <https://www.mozilla.org/en-US/MPL/2.0/>.

pragma Ada_2012;

with Interfaces.C;  use Interfaces.C;

pragma Style_Checks(Off, int);

package body TLS.Contexts.Server is

	package Bs renames libTLS_Bindings;
	pragma Style_Checks(Off, Bs);


	----------------
	-- Accept_TLS --
	----------------

	procedure Accept_TLS
		(Server_Ctx : Server_Context; Socket : Socket_Type;
		Connected_Context : out Server_Context)
	is
		function Generate_Error_Msg (Message : String) return String
		is (Message & " '" & Image(Socket) & "': " & Retrieve_Error_Message(Server_Ctx.Context));

		R : int;
	begin
		R := Bs.tls_accept_socket(
			Server_Ctx.Context.Context, Connected_Context.Context.Context'Address, int(To_C(Socket))
		);
		if R = -1 then
			raise Connect_Error with Generate_Error_Msg("unable to enable tls on socket");
		end if;

		R := Bs.tls_handshake(Connected_Context.Context.Context);
		if R /= 0 then
			raise Connect_Error with Generate_Error_Msg("unable to do TLS handshake on socket");
		end if;

		Connected_Context.Context.Configured := True;
		Connected_Context.Context.Connected := True;
	end Accept_TLS;


	--------------------
	-- SNI_Servername --
	--------------------

	function SNI_Servername (Ctx : Server_Context) return String
	is (To_String(Call_String_Function(Bs.tls_conn_servername'Access, Ctx.Context)));


	----------------
	-- Initialize --
	----------------

	overriding procedure Initialize (Object : in out Controlled_Mixin) is
	begin
		Initialize_Server(Object.Enclosing.Context);
	end Initialize;

end TLS.Contexts.Server;
