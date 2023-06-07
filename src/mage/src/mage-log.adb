with Ada.Text_IO;

procedure Mage.Log (Name, Message : String) is
begin
   Ada.Text_IO.Put_Line (Name & ": " & Message);
end Mage.Log;
