--                                                                    --
--  procedure Test_Gtk_Missed       Copyright (c)  Dmitry A. Kazakov  --
--  Test for Gtk.Missed                            Luebeck            --
--                                                 Summer, 2006       --
--                                                                    --
--                                Last revision :  11:26 01 Jun 2020  --
--                                                                    --
--  This  library  is  free software; you can redistribute it and/or  --
--  modify it under the terms of the GNU General Public  License  as  --
--  published by the Free Software Foundation; either version  2  of  --
--  the License, or (at your option) any later version. This library  --
--  is distributed in the hope that it will be useful,  but  WITHOUT  --
--  ANY   WARRANTY;   without   even   the   implied   warranty   of  --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU  --
--  General  Public  License  for  more  details.  You  should  have  --
--  received  a  copy  of  the GNU General Public License along with  --
--  this library; if not, write to  the  Free  Software  Foundation,  --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.    --
--                                                                    --
--  As a special exception, if other files instantiate generics from  --
--  this unit, or you link this unit with other files to produce  an  --
--  executable, this unit does not by  itself  cause  the  resulting  --
--  executable to be covered by the GNU General Public License. This  --
--  exception  does not however invalidate any other reasons why the  --
--  executable file might be covered by the GNU Public License.       --
--____________________________________________________________________--

with Ada.Calendar;            use Ada.Calendar;
with Ada.Characters.Latin_1;  use Ada.Characters.Latin_1;
with Ada.Exceptions;          use Ada.Exceptions;
with GLib;                    use GLib;
with GtkAda.Handlers;         use GtkAda.Handlers;
with Gtk.Missed;              use Gtk.Missed;
with Gtk.Main.Router;         use Gtk.Main.Router;
with Gtk.Window;              use Gtk.Window;
with Gtk.Widget;              use Gtk.Widget;
with Gtk.Table;               use Gtk.Table;
with Gtk.Label;               use Gtk.Label;

with Ada.Unchecked_Conversion;
with GLib.Time_Zone;

procedure Test_Gtk_Missed is
   Window  : Gtk_Window;
   Grid    : Gtk_Table;
   Label   : Gtk_Label;
   Counter : Integer;

   type Local_Callback is access procedure;
   function "+" is
      new Ada.Unchecked_Conversion (Local_Callback, Gtk_Callback);

   task type Process;

   procedure Update is
      X      : GInt;
      Y      : GInt;
      Width  : GInt;
      Height : GInt;
   begin
      Get_Position (Window, X, Y);
      Width  := Window.Get_Allocated_Width;
      Height := Window.Get_Allocated_Height;
      Label.Set_Text
      (  "Move and resize it until ""Success"" appears"
      &  LF
      &  LF
      &  "Position X ="
      &  GInt'Image (X)
      &  " Y =" & GInt'Image (Y)
      &  LF
      &  "Size Width ="
      &  GInt'Image (Width)
      &  " Height ="
      &  GInt'Image (Height)
      );
   end Update;

   procedure Done is
   begin
      Label.Set_Text ("Success");
   end Done;

   -- The task that calls to Update
   task body Process is
   begin
      for Index in 1..100 loop
         Counter := Index;
         Request (+Update'Access); -- Request execution of Update
         delay 0.2;
      end loop;
      Request (+Done'Access);
   exception
      when Error : others =>
         Say (Exception_Information (Error)); -- This is safe
   end Process;

begin
   Gtk.Main.Init;
   Gtk.Window.Gtk_New (Window);
   Gtk.Main.Router.Init (Window); -- This must be called once
   Window.Set_Title ("Test Missed Stuff");
   Window.On_Delete_Event (Gtk.Missed.Delete_Event_Handler'Access);
   Window.On_Destroy (Gtk.Missed.Destroy_Handler'Access);
   Gtk_New (Grid, 4, 4, False);
   Window.Add (Grid);
   declare
      use GLib.Time_Zone;
      TZ       : GTime_Zone renames Gtk_New_Local.all;
--    TZ       : GTime_Zone renames Gtk_New ("Europe/Helsinki").all;
      Zone     : Gtk_Label;
      Year     : Year_Number;
      Month    : Month_Number;
      Day      : Day_Number;
      Seconds  : Day_Duration;
      Interval : GInt;
      Winter   : Time;
      Summer   : Time;
   begin
      Gtk_New (Zone, "Identifier");
      Grid.Attach (Zone, 0, 1, 0, 1);
      Gtk_New (Zone, Get_Identifier (TZ));
      Grid.Attach (Zone, 1, 4, 0, 1);

      Split (Clock, Year, Month, Day, Seconds);
      Winter := Time_Of (Year, 1, 1, 0.0);
      Summer := Time_Of (Year, 6, 1, 0.0);

      Gtk_New (Zone, "Winter time");
      Grid.Attach (Zone, 0, 1, 1, 2);
      Gtk_New (Zone, "Offset");
      Grid.Attach (Zone, 2, 3, 1, 2);
      Interval := Find_Interval (TZ, Winter);
      if Interval >= 0 then
         Gtk_New (Zone, Get_Abbreviation (TZ, Interval));
         Grid.Attach (Zone,  1, 2, 1, 2);
         Gtk_New
         (  Zone,
            Integer'Image (Integer (Get_Offset (TZ, Interval)))
         );
         Grid.Attach (Zone,  3, 4, 1, 2);
      end if;

      Gtk_New (Zone, "Summer time");
      Grid.Attach (Zone, 0, 1, 2, 3);
      Gtk_New (Zone, "Offset");
      Grid.Attach (Zone, 2, 3, 2, 3);
      Interval := Find_Interval (TZ, Summer);
      if Interval >= 0 and then Is_DST (TZ, Interval) then
         Gtk_New (Zone, Get_Abbreviation (TZ, Interval));
         Grid.Attach (Zone, 1, 2, 2, 3);
         Gtk_New
         (  Zone,
            Integer'Image (Integer (Get_Offset (TZ, Interval)))
         );
         Grid.Attach (Zone, 3, 4, 2, 3);
      end if;

--        Gtk_New (Zone, Image ());
--        Grid.Attach (Zone,  1, 2, 1, 2);
--
--        Gtk_New (Zone, "Offset");
--        Grid.Attach (Zone,  0, 1, 1, 2);
--        Gtk_New (Zone, Image ());
--        Grid.Attach (Zone,  1, 2, 1, 2);

      Unref (TZ);
   end;
   Gtk_New (Label, "label");
   Grid.Attach (Label, 0, 4, 3, 4);

   Window.Show_All;
   declare
      Worker : Process; -- Now the task is on
   begin
      -- Enter the events processing loop
      Gtk.Main.Main;
   end;
exception
   when Error : others =>
      Say (Exception_Information (Error)); -- This is safe
end Test_Gtk_Missed;
