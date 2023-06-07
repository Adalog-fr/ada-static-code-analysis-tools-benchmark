-----------------------------------------------------------------------
--                              Ada Labs                             --
--                                                                   --
--                 Copyright (C) 2008-2019, AdaCore                  --
--                                                                   --
-- Labs is free  software; you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Mage;       use Mage;
with Mage.Draw;  use Mage.Draw;
with Mage.Model; use Mage.Model;
with Mage.Event; use Mage.Event;

with Ada.Real_Time; use Ada.Real_Time;

with Ada.Text_IO;

procedure Mage_Hat is
   Width  : constant := 240.0;
   Height : constant := 320.0;
   Length : constant := 30.0;

   X       : Float range -Width / 2.0 .. Width / 2.0   := 0.0;
   Y       : Float range -Height / 2.0 .. Height / 2.0 := 0.0;
   Speed_X : Float                                     := 1.0;
   Speed_Y : Float                                     := 2.0;
   Next    : Time;
   Period  : constant Time_Span                        := Milliseconds (40);

   --  reference to the application window
   Window : Window_ID;

   --  reference to the graphical canvas associated with the application window
   Canvas : Canvas_ID;

   function "+" (A, B : Point_3d) return Point_3d is
     ((A.X + B.X, A.Y + B.Y, A.Z + B.Z));

   T : Positive := Positive'First;

   procedure Draw_Hat (Center : Screen_Point; Square_Side : Positive) is
      Top_Left    : constant Screen_Point :=
        Center - (Square_Side / 2, Square_Side / 2);
      Bottom_Size : constant Natural      := 2;
   begin
      for N in 0 .. Square_Side - Bottom_Size - 1 loop
         declare
            L  : constant Positive     := 1 + N / 6;
            P1 : constant Screen_Point := Top_Left + (Square_Side / 2 - L, N);
            P2 : constant Screen_Point := Top_Left + (Square_Side / 2 + L, N);
         begin
            if P1 = P2 then
               Set_Pixel (Canvas, P1, Blue);
            else
               Draw_Line (Canvas, P1, P2, Blue);
               if N mod 7 = 0 then
                  Set_Pixel
                    (Canvas, (P1.X + ((2 * N) mod (1 + P2.X - P1.X)), P1.Y),
                     (case ((T / 20 + N) mod 3) is when 0 => Cyan,
                        when 1 => Yellow, when 2 => Red, when others => Blue));
               end if;
            end if;
         end;
      end loop;

      for B in 1 .. Bottom_Size loop
         declare
            Line : constant Positive := Square_Side - B;
         begin
            Draw_Line
              (Canvas, Top_Left + (0, Line),
               Top_Left + (Square_Side - 1, Line), Blue);
         end;
      end loop;

   end Draw_Hat;

begin
   Window :=
     Create_Window
       (Width => Integer (Width), Height => Integer (Height),
        Name  => "Magic Hat");
   Canvas := Get_Canvas (Window);

   Next := Clock + Period;

   while not Is_Killed loop

      T := T + 1;

      if abs (X + Speed_X) + Length / 2.0 >= Width / 2.0 then
         Speed_X := -Speed_X;
      end if;

      if abs (Y + Speed_Y) + Length / 2.0 >= Height / 2.0 then
         Speed_Y := -Speed_Y;
      end if;

      X := X + Speed_X;
      Y := Y + Speed_Y;

      Draw_Hat
        (Center      => To_Screen_Point (Canvas, (X, Y, 0.0)),
         Square_Side => Positive (Length));

      Handle_Events (Window);

      delay until Next;
      Next := Next + Period;

   end loop;

end Mage_Hat;
