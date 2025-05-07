with Mage;       use Mage;
with Mage.Draw;  use Mage.Draw;
with Mage.Event; use Mage.Event;

procedure Init is
   W : Window_ID := Create_Window (800, 600, "Hello");
begin
   while not Is_Killed loop
      Handle_Events (W);
      delay 0.1;
   end loop;
end Init;
