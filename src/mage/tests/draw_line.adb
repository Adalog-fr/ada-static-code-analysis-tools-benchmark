with Mage;       use Mage;
with Mage.Model; use Mage.Model;
with Mage.Draw;  use Mage.Draw;
with Mage.Event; use Mage.Event;

procedure Draw_Line is
   W : Window_ID := Create_Window (800, 600, "Hello");
   C : Canvas_ID := Get_Canvas (W);
begin

   while not Is_Killed loop
      Draw_Line (C, Screen_Point'(400, 300), (300, 400), Violet);

      Draw_Line (C, Screen_Point'(400, 200), (600, 200), Blue);

      Handle_Events (W);
      delay 0.1;
   end loop;
end Draw_Line;
