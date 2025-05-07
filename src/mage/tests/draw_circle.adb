with Mage;       use Mage;
with Mage.Draw;  use Mage.Draw;
with Mage.Event; use Mage.Event;

procedure Draw_Circle is
   W : Window_ID := Create_Window (800, 600, "Hello");
   C : Canvas_ID := Get_Canvas (W);
begin

   while not Is_Killed loop
      Draw_Circle (C, (400, 300), 300, Red);

      Handle_Events (W);
      delay 0.1;
   end loop;
end Draw_Circle;
