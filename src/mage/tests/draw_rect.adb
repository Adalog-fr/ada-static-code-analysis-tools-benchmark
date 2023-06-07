with Mage;       use Mage;
with Mage.Draw;  use Mage.Draw;
with Mage.Event; use Mage.Event;

procedure Draw_Rect is
   W : Window_ID := Create_Window (800, 600, "Hello");
   C : Canvas_ID := Get_Canvas (W);
begin

   while not Is_Killed loop
      Draw_Fill_Rect (C, (0, 100), 100, 100, Blue);
      Handle_Events (W);
      delay 0.1;
   end loop;
end Draw_Rect;
