with Mage;       use Mage;
with Mage.Draw;  use Mage.Draw;
with Mage.Event; use Mage.Event;

procedure Mage.Draw.Pixels is
   W : Window_ID := Create_Window (800, 600, "Hello");
   C : Canvas_ID := Get_Canvas (W);
begin

   while not Is_Killed loop
      Prepare_Pixel_Draw (C, Green);
      Draw_Pixel (C, (400, 300));
      Handle_Events (W);
      delay 0.1;
   end loop;

end Mage.Draw.Pixels;
