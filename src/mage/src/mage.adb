with Ada.Strings.Unbounded;

with SDL; use SDL;
with SDL.Video;

with Ada.Text_IO;

package body Mage is
   function "+" (F : SDL.Init_Flags) return String is
      use all type Ada.Strings.Unbounded.Unbounded_String;

      S : Ada.Strings.Unbounded.Unbounded_String;

      procedure If_Set_Then_Add
        (S    : in out Ada.Strings.Unbounded.Unbounded_String;
         A, B :        SDL.Init_Flags; Name : String)
      is
      begin
         if (A and B) /= 0 then
            if Length (S) = 0 then
               S := To_Unbounded_String (Name);
            else
               S := " " & To_Unbounded_String (Name);
            end if;
         end if;
      end If_Set_Then_Add;

   begin

      If_Set_Then_Add (S, F, Enable_Timer, "timer");
      If_Set_Then_Add (S, F, Enable_Audio, "audio");
      If_Set_Then_Add (S, F, Enable_Screen, "screen");
      If_Set_Then_Add (S, F, Enable_Joystick, "joystick");
      If_Set_Then_Add (S, F, Enable_Haptic, "haptic");
      If_Set_Then_Add (S, F, Enable_Game_Controller, "game-controller");
      If_Set_Then_Add (S, F, Enable_Events, "events");
      If_Set_Then_Add (S, F, Enable_No_Parachute, "no-parachute");

      return To_String (S);
   end "+";

   procedure Initialize (Flags : SDL.Init_Flags := Enable_Everything) is
      Double_Init : constant SDL.Init_Flags := SDL.Was_Initialised and Flags;
   begin
      Ada.Text_IO.Put_Line ("mage: initialize");
      pragma Assert
        (Double_Init = 0,
         "tried to initialize the SDL more than once (flag " & (+Double_Init) &
         ")");

      if not SDL.Video.Initialise ("") then
         raise Mage_Error with "video initialization failed";
      end if;

      if not SDL.Initialise (Flags) then
         raise Mage_Error with "SDL initialization failed";
      end if;
   end Initialize;

begin
   Initialize;
end Mage;
