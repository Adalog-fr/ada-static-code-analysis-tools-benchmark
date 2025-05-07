-- The context used for making and managing Client connections.
--
--
-- Copyright (c) 2022 nytpu <alex [at] nytpu.com>
-- SPDX-License-Identifier: MPL-2.0
-- For more license details, see LICENSE or <https://www.mozilla.org/en-US/MPL/2.0/>.

pragma Ada_2012;

with GNAT.Sockets;  use GNAT.Sockets;

private with Ada.Finalization;

package TLS.Contexts.Client is

	type Client_Context is new Context with private;

	-- Connect to a given hostname and port using a given context
	procedure Connect
		(Ctx : in out Client_Context; Host : String; Port : Port_Number)
		with
			Pre => Ctx.Is_Initialized and Ctx.Is_Configued and not Ctx.Is_Connected,
			Post => Ctx.Is_Connected;

	-- Connect to a given hostname and port using a given context, in the same
	-- manner as the other Connect; however this will also specify an
	-- additional server name, for the case where the TLS name differs from the
	-- DNS hostname.
	procedure Connect (
		Ctx : in out Client_Context;
		Host : String;
		Port : Port_Number;
		Server_Name : String
	) with
		Pre => Ctx.Is_Initialized and Ctx.Is_Configued and not Ctx.Is_Connected,
		Post => Ctx.Is_Connected;

	-- Upgrade an existing socket connection to use TLS.  The server name is
	-- used for TLS' server name indication.  If this procedure is used to
	-- establish a connection then the socket will not be closed when calling
	-- Close.
	procedure Connect (
		Ctx : in out Client_Context;
		Socket : GNAT.Sockets.Socket_Type;
		Server_Name : String
	) with
		Pre =>
			Ctx.Is_Initialized and Ctx.Is_Configued and not Ctx.Is_Connected,
		Post => Ctx.Is_Connected;

	-- Indicate whether a TLS session has been resumed during the handshake
	-- with the connected server
	function Session_Resumed (Ctx : Client_Context) return Boolean
		with Pre => Ctx.Is_Connected;


private
	type Controlled_Mixin (Enclosing : access Client_Context) is
		new Ada.Finalization.Limited_Controlled with null record;
	overriding procedure Initialize (Object : in out Controlled_Mixin);

	type Client_Context is new Context with record
		-- Client_Context'Access refers to the actual enclosing instance!
		Mix_In : Controlled_Mixin(Enclosing => Client_Context'Access);
	end record;

end TLS.Contexts.Client;
