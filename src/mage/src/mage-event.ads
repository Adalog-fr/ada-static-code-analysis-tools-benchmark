with SDL; use SDL; -- simple SDL types
with SDL.Events.Events;
with SDL.Events.Keyboards;

with Mage.Draw;

package Mage.Event is

   package Events renames SDL.Events.Events;
   subtype Event is Events.Events; -- What is not named events at this point...
   use all type Event;

   subtype Keyboard_Event is
     SDL.Events.Keyboards
       .Keyboard_Events; -- What is not named events at this point...

   procedure Handle_Events (Window : Mage.Draw.Window_ID);
   function Is_Killed return Boolean;

   function Poll_Keyboard (Evt : out Keyboard_Event) return Boolean;

end Mage.Event;
