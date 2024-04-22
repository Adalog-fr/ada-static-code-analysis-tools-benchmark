-- SPDX-License-Identifier: BSD-2-Clause
-- For more license details, see LICENSE.

pragma Ada_2012;

with Ada.Characters.Handling;  use Ada.Characters.Handling;
with Ada.Command_Line;
with Ada.Containers;  use type Ada.Containers.Count_Type;
with Ada.Exceptions;
with Ada.Text_IO;  use Ada.Text_IO;
with Callbacks;
with Linenoise;

procedure Example is
	History_Filename : constant String := "history.txt";

	procedure Cleanup is
	begin
		Linenoise.History_Save(History_Filename);
	end Cleanup;
begin
	Linenoise.History_Set_Maximum_Length(100);
	begin
		Linenoise.History_Load(History_Filename);
	exception
		-- History file likely hasn't been created yet so just ignore it
		when Linenoise.History_File_Error => null;
	end;

	Linenoise.Register_Completion_Callback(Callbacks.Completer'Access);
	Linenoise.Register_Hint_Callback(Callbacks.Hinter'Access);

	loop
		declare
			Raw : constant String := Linenoise.Get_Line("> ");
			Line : constant Linenoise.String_Vectors.Vector :=
				Linenoise.Split(To_Lower(Raw));
		begin
			if Line.Length > 0 then
				Linenoise.History_Add(Raw);
			end if;
			if Line.Element(1) = "clear" then
				Linenoise.Clear_Screen;
			elsif Line.Element(1) = "multiline" then
				Linenoise.Multiline_Mode(True);
				Put_Line("Multiline mode Enabled");
			elsif Line.Element(1) = "nomultiline" then
				Linenoise.Multiline_Mode(False);
				Put_Line("Multiline mode disabled");
			elsif Line.Element(1) = "exit" then
				Put_Line("Goodbye.");
				exit;
			elsif Line.Element(1) = "mask" then
				-- You can prompt the user again again arbitrarily
				Linenoise.Mask_Mode(True);
				declare
					Password : constant String := Linenoise.Get_Line("passwd> ");
				begin
					Put_Line("Your password '" & Password & "' will be stored 100% securely, you can trust us.");
				end;
				Linenoise.Mask_Mode(False);
			else
				Put_Line("Entered: " & Raw);
			end if;
		end;
	end loop;

	Cleanup;


exception
	when Linenoise.End_Error =>
		Cleanup;
	when Linenoise.History_File_Error =>
		Put_Line("Unable to save command history file.");
		Ada.Command_Line.Set_Exit_Status(Ada.Command_Line.Failure);
	when E : Linenoise.Malformed_Command =>
		Put_Line("Malformed command: " & Ada.Exceptions.Exception_Message(E));
		Ada.Command_Line.Set_Exit_Status(Ada.Command_Line.Failure);
		Cleanup;
end Example;
