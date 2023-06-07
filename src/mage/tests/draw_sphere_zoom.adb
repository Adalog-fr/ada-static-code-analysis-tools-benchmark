with Mage;       use Mage;
with Mage.Draw;  use Mage.Draw;
with Mage.Event; use Mage.Event;

procedure Draw_Sphere_Zoom is
   W     : Window_ID := Create_Window (800, 600, "Hello");
   C     : Canvas_ID := Get_Canvas (W);
   Z_Mul : Float_Pos := 1.002;
begin
   while not Is_Killed loop
      Draw_Sphere (C, (0.0, 0.0, 0.0), 150.0, (200, 150, 60, 255));
      Handle_Events (W);

      if Zoom_Factor (C) > 2.0 or else Zoom_Factor (C) < 0.5 then
         Z_Mul := 1.0 / Z_Mul;
      end if;

      Zoom_Factor (C, Zoom_Factor (C) * Z_Mul);
      delay 0.03;
   end loop;
end Draw_Sphere_Zoom;
