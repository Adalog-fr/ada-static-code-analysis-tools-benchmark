--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Oscilloscope                            Luebeck            --
--        On_Button_Release                        Summer, 2011       --
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
   function On_Button_Release
            (  Object       : access GObject_Record'Class;
               Event        : Gdk_Event;
               Oscilloscope : Gtk_Oscilloscope
            )  return Boolean is
   use Menu_Handlers;
   Menu      : Gtk_Menu;
   Item      : Gtk_Image_Menu_Item;
   Separator : Gtk_Separator_Menu_Item;
   Icon      : Gtk_Image;
   Box       : Cairo_Box;
begin
   case Get_Button (Event) is
      when 1 =>
         if Oscilloscope.Selection.Area /= null then
            Oscilloscope.Change_Selection
            (  Oscilloscope.Mouse_Event (Event, False)
            );
            Box := Oscilloscope.Selection.Area.Get_Box;
            if Box.X2 - Box.X1 < 2.0 or else Box.Y2 - Box.Y1 < 2.0 then
               Oscilloscope.Restore_State;
               Free (Oscilloscope.Selection.Area);
            else
               case Oscilloscope.Selection_Mode is
                  when Interactive =>
                     Gtk_New (Menu);
                        -- Zoom in
                     Gtk_New
                     (  Item,
                        Style_Get (Oscilloscope, "menu-zoom-in")
                     );
                     Gtk_New (Icon, Stock_Zoom_In, Icon_Size_Menu);
                     Set_Image (Item, Icon);
                     Append (Menu, Item);
                     Connect
                     (  Item,
                        "activate",
                        On_Zoom_In'Access,
                        Oscilloscope
                     );
                     Gtk_New
                     (  Item,
                        Style_Get (Oscilloscope, "menu-zoom-in-t")
                     );
                     Append (Menu, Item);
                     Connect
                     (  Item,
                        "activate",
                        On_Zoom_In_T'Access,
                        Oscilloscope
                     );
                     Gtk_New
                     (  Item,
                        Style_Get (Oscilloscope, "menu-zoom-in-v")
                     );
                     Append (Menu, Item);
                     Connect
                     (  Item,
                        "activate",
                        On_Zoom_In_V'Access,
                        Oscilloscope
                     );
                        -- Zoom out
                     Gtk_New
                     (  Item,
                        Style_Get (Oscilloscope, "menu-zoom-out")
                     );
                     Gtk_New (Icon, Stock_Zoom_Out, Icon_Size_Menu);
                     Set_Image (Item, Icon);
                     Append (Menu, Item);
                     Connect
                     (  Item,
                        "activate",
                        On_Zoom_Out'Access,
                        Oscilloscope
                     );
                     Gtk_New
                     (  Item,
                        Style_Get (Oscilloscope, "menu-zoom-out-t")
                     );
                     Append (Menu, Item);
                     Connect
                     (  Item,
                        "activate",
                        On_Zoom_Out_T'Access,
                        Oscilloscope
                     );
                     Gtk_New
                     (  Item,
                        Style_Get (Oscilloscope, "menu-zoom-out-v")
                     );
                     Append (Menu, Item);
                     Connect
                     (  Item,
                        "activate",
                        On_Zoom_Out_V'Access,
                        Oscilloscope
                     );
                        -- Copy values
                     Gtk_New
                     (  Item,
                        Style_Get (Oscilloscope, "menu-copy-values")
                     );
                     Gtk_New (Icon, Stock_Copy, Icon_Size_Menu);
                     Set_Image (Item, Icon);
                     Append (Menu, Item);
                     Connect
                     (  Item,
                        "activate",
                        On_Copy_Selection'Access,
                        Oscilloscope
                     );
                        -- Copy differences
                     Gtk_New
                     (  Item,
                        Style_Get
                        (  Oscilloscope,
                           "menu-copy-differences"
                     )  );
                     Gtk_New (Icon, Stock_Remove, Icon_Size_Menu);
                     Set_Image (Item, Icon);
                     Append (Menu, Item);
                     Connect
                     (  Item,
                        "activate",
                        On_Difference_Selection'Access,
                        Oscilloscope
                     );
                        -- Copy range
                     Gtk_New
                     (  Item,
                        Style_Get (Oscilloscope, "menu-copy-range")
                     );
                     Gtk_New (Icon, Stock_Paste, Icon_Size_Menu);
                     Set_Image (Item, Icon);
                     Append (Menu, Item);
                     Connect
                     (  Item,
                        "activate",
                        On_Range_Selection'Access,
                        Oscilloscope
                     );
                        -- Separator
                     Gtk_New (Separator);
                     Append (Menu, Separator);
                        -- Delete selection
                     Gtk_New
                     (  Item,
                        Style_Get (Oscilloscope, "menu-cancel")
                     );
                     Gtk_New (Icon, Stock_Cancel, Icon_Size_Menu);
                     Set_Image (Item, Icon);
                     Append (Menu, Item);
                     Connect
                     (  Item,
                        "activate",
                        On_Cancel_Selection'Access,
                        Oscilloscope
                     );
                     Connect
                     (  Menu,
                        "destroy",
                        On_Cancel_Selection'Access,
                        Oscilloscope
                     );
                     Show_All (Menu);
                     Popup
                     (  Menu,
                        Button => Gdk.Event.Get_Button (Event),
                        Activate_Time => Gdk.Event.Get_Time (Event)
                     );
                  when Zoom_In =>
                     On_Zoom_In (Oscilloscope, Oscilloscope);
                  when Zoom_In_Time =>
                     On_Zoom_In_T (Oscilloscope, Oscilloscope);
                  when Zoom_In_Values =>
                     On_Zoom_In_V (Oscilloscope, Oscilloscope);
                  when Zoom_Out =>
                     On_Zoom_Out (Oscilloscope, Oscilloscope);
                  when Zoom_Out_Time =>
                     On_Zoom_Out_T (Oscilloscope, Oscilloscope);
                  when Zoom_Out_Values =>
                     On_Zoom_Out_V (Oscilloscope, Oscilloscope);
                  when Copy_Range =>
                     On_Range_Selection (Oscilloscope, Oscilloscope);
                  when Copy_Values =>
                     On_Copy_Selection (Oscilloscope, Oscilloscope);
                  when Copy_Differences =>
                     On_Difference_Selection
                     (  Oscilloscope,
                        Oscilloscope
                     );
                  when User_Action =>
                     declare
                        Box : constant Cairo_Box :=
                              Oscilloscope.Selection.Area.Get_Box;
                     begin
                        Free (Oscilloscope.Selection.Area);
                        Oscilloscope.Restore_State;
                        Oscilloscope.On_Selection (Box);
                     end;
                  when None =>
                     Free (Oscilloscope.Selection.Area);
                     Oscilloscope.Restore_State;
               end case;
            end if;
         end if;
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
         &  Where ("On_Button_Release")
      )  );
      return True;
end On_Button_Release;
