--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Oscilloscope                            Luebeck            --
--        On_Button_Press                          Summer, 2011       --
--  Separate body                                                     --
--                                Last revision :  13:15 14 Sep 2019  --
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

with Gtk.Image_Menu_Item;  use Gtk.Image_Menu_Item;
with Gtk.Menu;             use Gtk.Menu;

separate (Gtk.Oscilloscope)
   function On_Button_Press
            (  Object       : access GObject_Record'Class;
               Event        : Gdk_Event;
               Oscilloscope : Gtk_Oscilloscope
            )  return Boolean is
   procedure Free is new
      Ada.Unchecked_Deallocation (Gtk_Menu_Record'Class, Gtk_Menu);

   use Menu_Handlers;
   Menu : Gtk_Menu;
   Item : Gtk_Image_Menu_Item;
   Icon : Gtk_Image;

   procedure Save is
      First : Boolean := True;
   begin
      for Group in 1..Oscilloscope.Groups_Number loop
         Oscilloscope.Save_Amplifier (Group, First);
         Oscilloscope.Set_Auto_Scaling (Group, False);
      end loop;
      for Index in Sweeper_Type'Range loop
         Oscilloscope.Save_Sweeper (Index, First);
         Oscilloscope.Set_Frozen (Index, True);
      end loop;
      Oscilloscope.Selection.Saved := True;
   end Save;
begin
   case Get_Button (Event) is
      when 1 =>
         if Oscilloscope.Selection_Mode /= None then
            if Oscilloscope.Selection.Area = null then
               Oscilloscope.Selection.Engaged := True;
               declare
                  Box   : constant Cairo_Box := Oscilloscope.Get_Box;
                  Point : constant Cairo_Tuple :=
                          Oscilloscope.Mouse_Event (Event, False);
               begin
                  if Point.X in Box.X1..Box.X2 and then
                     Point.Y in Box.Y1..Box.Y2
                  then
                     Oscilloscope.Selection.Area :=
                        Add_Rectangle
                        (  Under => Oscilloscope.Layers,
                           Box   => (  X1 => Point.X,
                                       X2 => Point.X,
                                       Y1 => Point.Y,
                                       Y2 => Point.Y
                                    ),
                           Line_Width => 1.0,
                           Opacity    => 0.0,
                           Color      => Style_Get
                                         (  Oscilloscope,
                                            "selection-color",
                                            Selection_Color
                        )                ) .all'Unchecked_Access;
                     Oscilloscope.Selection.Right := True;
                     Oscilloscope.Selection.Below := True;
                     Save;
                  end if;
               end;
               Oscilloscope.Selection.Engaged := False;
            else
               Oscilloscope.Change_Selection
               (  Oscilloscope.Mouse_Event (Event, False)
               );
            end if;
         end if;
      when 3 =>
         declare
            Have_Menu : Boolean := False;
         begin
            Gtk_New (Menu);
            if (  Oscilloscope.Manual_Sweep
               and then
                  0 /= (Oscilloscope.Menu_Enabled and Hold_Release_Item)
               )
            then
               for Sweeper in Oscilloscope.Time_Axis'Range loop
                  if Oscilloscope.Get_Frozen (Sweeper) then
                     -- Add release button
                     Gtk_New
                     (  Item,
                        Style_Get (Oscilloscope, "menu-release")
                     );
                     Gtk_New (Icon, Stock_Media_Play, Icon_Size_Menu);
                     Set_Image (Item, Icon);
                     Append (Menu, Item);
                     Connect
                     (  Item,
                        "activate",
                        On_Release'Access,
                        Oscilloscope
                     );
                     Have_Menu := True;
                     exit;
                  end if;
               end loop;
               for Sweeper in Oscilloscope.Time_Axis'Range loop
                  if not Oscilloscope.Get_Frozen (Sweeper) then
                     -- Add hold button
                     Gtk_New
                     (  Item,
                        Style_Get (Oscilloscope, "menu-pause")
                     );
                     Gtk_New (Icon, Stock_Media_Pause, Icon_Size_Menu);
                     Set_Image (Item, Icon);
                     Append (Menu, Item);
                     Connect
                     (  Item,
                        "activate",
                        On_Pause'Access,
                        Oscilloscope
                     );
                     Have_Menu := True;
                     exit;
                  end if;
               end loop;
            end if;
            -- Add latest
            if 0 /= (Oscilloscope.Menu_Enabled and Latest_Data_Item)
            then
               declare
                  Have_Time : Boolean := False;
               begin
                  for Sweeper in Sweeper_Type'Range loop
                     declare
                        Data : Time_Axis_Data renames
                               Oscilloscope.Time_Axis (Sweeper);
                     begin
                        if Data.On and then Data.Time_Mode then
                           Have_Time := True;
                           exit;
                        end if;
                     end;
                  end loop;
                  if Have_Time then
                     Gtk_New
                     (  Item,
                        Style_Get (Oscilloscope, "menu-latest")
                     );
                     Gtk_New
                     (  Icon,
                        Stock_Media_Forward,
                        Icon_Size_Menu
                     );
                     Set_Image (Item, Icon);
                     Append (Menu, Item);
                     Connect
                     (  Item,
                        "activate",
                        On_Latest'Access,
                        Oscilloscope
                     );
                     Have_Menu := True;
                  end if;
               end;
            end if;
            -- Add undo
            if (  Oscilloscope.Undo_Stack.Actions /= null
               and then
                  0 /= (Oscilloscope.Menu_Enabled and Undo_Redo_Item)
               )
            then
               Gtk_New
               (  Item,
                  Style_Get (Oscilloscope, "menu-undo")
               );
               Gtk_New (Icon, Stock_Undo, Icon_Size_Menu);
               Set_Image (Item, Icon);
               Append (Menu, Item);
               Connect
               (  Item,
                  "activate",
                  On_Undo'Access,
                  Oscilloscope
               );
               Have_Menu := True;
            end if;
            -- Add redo
            if (  Oscilloscope.Redo_Stack.Actions /= null
               and then
                  0 /= (Oscilloscope.Menu_Enabled and Undo_Redo_Item)
               )
            then
               Gtk_New
               (  Item,
                  Style_Get (Oscilloscope, "menu-redo")
               );
               Gtk_New (Icon, Stock_Redo, Icon_Size_Menu);
               Set_Image (Item, Icon);
               Append (Menu, Item);
               Connect
               (  Item,
                  "activate",
                  On_Redo'Access,
                  Oscilloscope
               );
               Have_Menu := True;
            end if;
            -- Add toggle grid
            if 0 /= (Oscilloscope.Menu_Enabled and Grid_Item) then
               Gtk_New
               (  Item,
                  Style_Get (Oscilloscope, "menu-toggle-grid")
               );
               Gtk_New (Icon, Stock_Index, Icon_Size_Menu);
               Set_Image (Item, Icon);
               Append (Menu, Item);
               Connect
               (  Item,
                  "activate",
                  On_Toggle_Grid'Access,
                  Oscilloscope
               );
               Have_Menu := True;
            end if;
            -- Add toggle interpolation
            if 0 /= (Oscilloscope.Menu_Enabled and Interpolation_Item)
            then
               Gtk_New
               (  Item,
                  Style_Get (Oscilloscope, "menu-toggle-interpolation")
               );
               Gtk_New (Icon, Stock_Italic, Icon_Size_Menu);
               Set_Image (Item, Icon);
               Append (Menu, Item);
               Connect
               (  Item,
                  "activate",
                  On_Toggle_Interpolation'Access,
                  Oscilloscope
               );
               Have_Menu := True;
            end if;
            if (  Oscilloscope.Format /= No_Snapshot
               and then
                  0 /= (Oscilloscope.Menu_Enabled and Snapshot_Item)
               and then
                  Oscilloscope.File /= null
               and then
                  Oscilloscope.File'Length > 0
               )
            then -- Add snapshot button
               Gtk_New
               (  Item,
                  (  Style_Get (Oscilloscope, "menu-snapshot")
                  &  " to "
                  &  Oscilloscope.File.all
               )  );
               Gtk_New (Icon, Stock_Save, Icon_Size_Menu);
               Set_Image (Item, Icon);
               Append (Menu, Item);
               Connect
               (  Item,
                  "activate",
                  On_Snapshot'Access,
                  Oscilloscope
               );
               Have_Menu := True;
            end if;
            if Have_Menu then
               Show_All (Menu);
               Popup
               (  Menu,
                  Button => Gdk.Event.Get_Button (Event),
                  Activate_Time => Gdk.Event.Get_Time (Event)
               );
            else
               Menu.Destroy;
               Free (Menu);
            end if;
         end;
      when others =>
         null;
   end case;
   return True;
exception
   when Error : others =>
      Log
      (  GtkAda_Contributions_Domain,
         Log_Level_Critical,
         (  "Fault: "
         &  Exception_Information (Error)
         &  Where ("On_Button_Press")
      )  );
      return True;
end On_Button_Press;
