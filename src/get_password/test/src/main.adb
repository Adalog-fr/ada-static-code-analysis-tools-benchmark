with Ada.Command_Line;
with Ada.Text_IO;  use Ada.Text_IO;
with Get_Password;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Main is
   Pin  : String (1 .. 6);
   Last : Natural;

   Zorro : Unbounded_String;
begin
   Get_Password.Get (Buffer => Pin,
                     Last   => Last,
                     Marker => "<>");

   Put_Line ("<" & Pin (Pin'First .. Last) & ">");

   Get_Password.Get (Buffer => Pin,
                     Last   => Last,
                     Marker => "*",
                     End_On => Get_Password.Buffer_Full);

   Put_Line ("<" & Pin (Pin'First .. Last) & ">");

   Get_Password.Get (Buffer => Pin,
                     Last   => Last,
                     Marker => "???",
                     End_On => Get_Password.End_Of_Line);

   Put_Line ("<" & Pin (Pin'First .. Last) & ">");

   Get_Password.Get (Buffer => Zorro,
                     Marker => "%");

   Put_Line ("<" & To_String (Zorro) & ">");



   Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Success);
exception
   when Get_Password.Not_A_Tty_Error =>
      Put_Line (Standard_Error, "Error : Input is not a terminal");

      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
end Main;
