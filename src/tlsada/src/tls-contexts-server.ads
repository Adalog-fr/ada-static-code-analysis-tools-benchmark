-- The context used for making and managing Server connections.
--
--
-- Copyright (c) 2022 nytpu <alex [at] nytpu.com>
-- SPDX-License-Identifier: MPL-2.0
-- For more license details, see LICENSE or <https://www.mozilla.org/en-US/MPL/2.0/>.

pragma Ada_2012;

with GNAT.Sockets;  use GNAT.Sockets;

package TLS.Contexts.Server is

	type Server_Context is new Context with private;

	-- Accept a connection over a socket with an existing connection and output
	-- a new context suitable for reading and writing.  The Server_Ctx remains
	-- open to accept additional connections.
	procedure Accept_TLS
		(Server_Ctx : Server_Context; Socket : Socket_Type;
		Connected_Context : out Server_Context)
		with
			Pre =>
				Server_Ctx.Is_Initialized and Server_Ctx.Is_Configued and
				not Server_Ctx.Is_Connected,
			Post =>
				not Server_Ctx.Is_Connected and
				Connected_Context.Is_Initialized and Connected_Context.Is_Configued and Connected_Context.Is_Connected;

	-- Returns a string corresponding to the servername that the client
	-- requested by sending a TLS Server Name Indication extension
	function SNI_Servername (Ctx : Server_Context) return String
		with Pre => Ctx.Is_Connected;


private
	type Controlled_Mixin (Enclosing : access Server_Context) is
		new Ada.Finalization.Limited_Controlled with null record;
	overriding procedure Initialize (Object : in out Controlled_Mixin);

	type Server_Context is new Context with record
		-- Client_Context'Access refers to the actual enclosing instance!
		Mix_In : Controlled_Mixin(Enclosing => Server_Context'Access);
	end record;

end TLS.Contexts.Server;
