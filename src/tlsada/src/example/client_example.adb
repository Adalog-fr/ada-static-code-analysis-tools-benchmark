-- Simple TLS client example: make a Gemini request to a given host and port
--
--
-- Copyright (c) 2022 nytpu <alex [at] nytpu.com>
-- SPDX-License-Identifier: MPL-2.0
-- For more license details, see LICENSE or <https://www.mozilla.org/en-US/MPL/2.0/>.

with Ada.Command_Line;  use Ada.Command_Line;
with Ada.Text_IO;  use Ada.Text_IO;

with TLS.Configure;
with TLS.Contexts;
with TLS.Contexts.Client;

procedure Client_Example is
	Conf : TLS.Configure.Config;
	Client : aliased TLS.Contexts.Client.Client_Context;
begin
	-- Argument parsing
	if Argument_Count < 2 or Argument_Count > 3 then
		Put_Line(Standard_Error, "Usage: " & Command_Name & " <host> <port> [path]");
		Set_Exit_Status(Failure);
		return;
	end if;

	-- Configure the client
	Conf.Verify_Certs := False;
	Client.Configure(Conf);

	-- Initiate the connection
	Client.Connect(Argument(1), TLS.Contexts.Port_Number'Value(Argument(2)));

	-- send the request
	String'Write(
		Client'Access,
		"gemini:" & -- Scheme
		"//" & Argument(1) & ':' & Argument(2) & -- Authority
		(if Argument_Count = 3 then Argument(3) else "/") & -- Path
		ASCII.CR & ASCII.LF -- Request Terminator
	);

	-- Recieve response
	declare
		E : Boolean;
		S : constant String :=
			TLS.Get_Delim(Client'Access, ASCII.CR & ASCII.LF, E);
	begin
		if S'Length > 1024 then
			Put_Line(Standard_Error, "Invalid response header length!");
			Set_Exit_Status(Failure);
			return;
		elsif not E then
			Put_Line(Standard_Error, "Header not terminated by CRLF!");
			Set_Exit_Status(Failure);
			return;
		end if;
		Put_Line("Response Header: " & S);
		New_Line;
	end;
	loop
		declare
			C : Character;
		begin
			-- Doing this characterwise is inefficient so you probably
			-- shouldn't be doing this in a real program unless you're sure you
			-- need to.  We're doing this just to demonstrate, but in real
			-- situations Get_Delim would be preferable.  Or directly
			-- Client.Read'ing into a buffer.
			Character'Read(Client'Access, C);
			Put(C);
		exception
			-- Assume the response is complete when there is no more data
			when TLS.End_Error => exit;
		end;
	end loop;

	-- Close the connection.  Backend libtls resources will be finalized
	-- automatically.
	Client.Close;
end Client_Example;
