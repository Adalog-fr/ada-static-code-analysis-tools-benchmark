with Ada.Containers.Vectors;

package body Mage.Event is

   App_Killed : Boolean := False;

   function Is_Killed return Boolean is (App_Killed);

   package Event_Vector_Pkg is new Ada.Containers.Vectors (Positive, Event);
   subtype Event_Vector is Event_Vector_Pkg.Vector;

   Keyboard_Events : Event_Vector;
   procedure Handle_Events (Window : Mage.Draw.Window_ID) is
      Ev : Event;
   begin
      while Events.Poll (Ev) loop
         case Ev.Common.Event_Type is
            when SDL.Events.Quit =>
               App_Killed := True;
            when SDL.Events.Keyboards.Key_Down | SDL.Events.Keyboards.Key_Up =>
               Keyboard_Events.Append (Ev);
            when others =>
               null;
         end case;
      end loop;

      Mage.Draw.Swap_Buffers (Window);
   end Handle_Events;

   function Poll_Keyboard (Evt : out Keyboard_Event) return Boolean is
   begin
      if not Keyboard_Events.Is_Empty then
         Evt := Keyboard_Events.First_Element.Keyboard;
         Keyboard_Events.Delete_First;
         return True;
      else
         return False;
      end if;
   end Poll_Keyboard;

end Mage.Event;
