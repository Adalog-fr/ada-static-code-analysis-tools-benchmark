--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Valve.Round_90                          Luebeck            --
--  Interface                                      Winter, 2017       --
--                                                                    --
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

with Gtk.Adjustment;             use Gtk.Adjustment;
with Gtk.Enums.String_Lists;     use Gtk.Enums.String_Lists;
with Gtk.Layered;                use Gtk.Layered;
with Gtk.Layered.Sector_Needle;  use Gtk.Layered.Sector_Needle;
with Gtk.Layered.Disk_Needle;    use Gtk.Layered.Disk_Needle;
with Gtk.Widget;                 use Gtk.Widget;

with Gtk.Enums;
with Gtk.Layered.Elliptic_Scale;

with Gtk.Layered.Elliptic_Annotation;
use  Gtk.Layered.Elliptic_Annotation;

with Gtk.Layered.Elliptic_Background;
use  Gtk.Layered.Elliptic_Background;

package Gtk.Valve.Round_90 is
--
-- Class_Name - Of the widget
--
   Class_Name : constant String := "GtkValveRound90";
--
-- Gtk_Gauge_Round_270 -- Round 270 degrees gauge
--
   type Gtk_Valve_Round_90_Record is
      new Gtk_Layered_Record with private;
   type Gtk_Valve_Round_90 is
      access all Gtk_Valve_Round_90_Record'Class;
--
-- Get_Type -- The type of the widget
--
-- Returns :
--
--    The GTK type of the widget
--
   function Get_Type return GType;
--
-- Gtk_New -- Widget construction
--
--    Widget      - The result
--    Texts       - The texts to be placed near major ticks of the scale
--  [ Delimiter ] - The delimiter character used in Texts
--    Adjustment  - The adjustment object to indicate
--    Sectors     - The number of intervals between major ticks
--
-- Normally there should be Sector + 1 texts in the  list  Texts.  Extra
-- texts are ignored. Missing texts are shown empty.
--
   procedure Gtk_New
             (  Widget     : out Gtk_Valve_Round_90;
                Texts      : Gtk.Enums.String_List.GList;
                Adjustment : Gtk_Adjustment := null;
                Sectors    : Positive       := 5
             );
   procedure Gtk_New
             (  Widget     : out Gtk_Valve_Round_90;
                Texts      : Controlled_String_List;
                Adjustment : Gtk_Adjustment := null;
                Sectors    : Positive       := 5
             );
   procedure Gtk_New
             (  Widget     : out Gtk_Valve_Round_90;
                Texts      : UTF8_String;
                Delimiter  : Character      := ' ';
                Adjustment : Gtk_Adjustment := null;
                Sectors    : Positive       := 5
             );
--
-- Initialize -- The widget initialization
--
--    Widget      - The widget to initialize
--    Texts       - The texts to be placed near major ticks of the scale
--  [ Delimiter ] - The delimiter character used in Texts
--    Adjustment  - The adjustment object to indicate
--    Sectors     - The number of intervals between major ticks
--
-- When  a  widget  type  is  derived from this one, it has to call this
-- procedure from its version of Initialize.
--
   procedure Initialize
             (  Widget     : not null access
                                Gtk_Valve_Round_90_Record'Class;
                Texts      : Gtk.Enums.String_List.GList;
                Adjustment : Gtk_Adjustment;
                Sectors    : Positive
             );
   procedure Initialize
             (  Widget     : not null access
                                Gtk_Valve_Round_90_Record'Class;
                Texts      : Controlled_String_List;
                Adjustment : Gtk_Adjustment;
                Sectors    : Positive
             );
   procedure Initialize
             (  Widget     : not null access
                                Gtk_Valve_Round_90_Record'Class;
                Texts      : UTF8_String;
                Delimiter  : Character;
                Adjustment : Gtk_Adjustment;
                Sectors    : Positive
             );
--
-- Get_Annotation -- The gauge annotation
--
--    Widget - The widget
--
-- Returns :
--
--    The annotation layer
--
   function Get_Annotation
            (  Widget : not null access Gtk_Valve_Round_90_Record
            )  return not null access Elliptic_Annotation_Layer;
--
-- Get_Background -- The gauge's background
--
--    Widget - The widget
--
-- Returns :
--
--    The background layer
--
   function Get_Background
            (  Widget : not null access Gtk_Valve_Round_90_Record
            )  return not null access Elliptic_Background_Layer;
--
-- Get_Needle -- The gauge needle
--
--    Widget - The widget
--
-- Returns :
--
--    The needle layer of the widget
--
   function Get_Needle
            (  Widget : not null access Gtk_Valve_Round_90_Record
            )  return not null access Disk_Needle_Layer;
--
-- Set_Value -- Change the value indicated by the gauge
--
--    Widget - The widget
--    Value  - The value in the range From .. From + Length
--
-- When  the value is out of range it is saturated to the nearest bound.
-- Note  that  procedure does not emit any events, if the widget need to
-- be redrawn the event "draw" should be emitted.
--
   procedure Set_Value
             (  Widget : not null access Gtk_Valve_Round_90_Record;
                Value  : GDouble
             );

   overriding
      procedure Style_Changed
                (  Widget : not null access Gtk_Valve_Round_90_Record
                );

private
   use Gtk.Layered.Elliptic_Scale;

   type Sector is record
      Minor_Ticks : access Elliptic_Scale_Layer;
      Major_Ticks : access Elliptic_Scale_Layer;
      Annotation  : access Elliptic_Annotation_Layer;
   end record;

   type Gtk_Valve_Round_90_Record is
      new Gtk_Layered_Record with
   record
      Sectors     : Positive := 5;
      Upper       : Sector;
      Lower       : Sector;
      Background  : access Elliptic_Background_Layer;
      Pin         : access Elliptic_Background_Layer;
      Left_Cover  : access Sector_Needle_Layer;
      Right_Cover : access Sector_Needle_Layer;
      Needle      : access Disk_Needle_Layer;
   end record;

end Gtk.Valve.Round_90;
