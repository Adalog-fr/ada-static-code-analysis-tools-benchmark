with Mage;       use Mage;
with Mage.Draw;  use Mage.Draw;
with Mage.Event; use Mage.Event;

procedure Draw_Magic is
   W : Window_ID := Create_Window (800, 600, "Hello");
   C : Canvas_ID := Get_Canvas (W);

   S     : Float range 0.0 .. 1.0 := 0.0;
   Color : constant RGBA_T        := (200, 150, 60, 255);

   function Apply_S
     (S : Float; Comp : Color_Component_T) return Color_Component_T is
     (Color_Component_T
        ((S * Float (Comp)) + (1.0 - S) * (255.0 - Float (Comp))));
begin

   while not Is_Killed loop
      if S >= 0.99 then
         S := 0.0;
      else
         S := S + 0.005;
      end if;

      Draw_Sphere
        (C, (400, 300), 300,
         (Apply_S (S, Color.R), Apply_S (S, Color.G), Apply_S (S, Color.B),
          255));

      Handle_Events (W);
      delay 0.01;
   end loop;
end Draw_Magic;
