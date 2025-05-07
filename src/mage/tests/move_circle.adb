with Mage;       use Mage;
with Mage.Draw;  use Mage.Draw;
with Mage.Model; use Mage.Model;
with Mage.Input; use Mage.Input;
with Mage.Event; use Mage.Event;

procedure Move_Circle is
   W : Window_ID := Create_Window (800, 600, "Hello");
   C : Canvas_ID := Get_Canvas (W);

   Coord : Point_3d       := Origin;
   Speed : constant Float := 4.0;

   procedure Move (Coord : in out Point_3d; D : Absolute_Direction) is
   begin
      case D is
         when Up =>
            Coord := Coord + Y_Axis;
         when Down =>
            Coord := Coord - Y_Axis;
         when Right =>
            Coord := Coord + X_Axis;
         when Left =>
            Coord := Coord - X_Axis;
      end case;
   end Move;

begin

   while not Is_Killed loop
      declare
         E : Action_Set := Mage.Input.Global_Actions;
      begin
         for D in Absolute_Direction loop
            if Has_Action (E, D) then
               Move (Coord, D);
            end if;
         end loop;
      end;

      Draw_Circle (C, Coord, 30.0, Red);

      Handle_Events (W);
      delay 0.05;
   end loop;
end Move_Circle;
