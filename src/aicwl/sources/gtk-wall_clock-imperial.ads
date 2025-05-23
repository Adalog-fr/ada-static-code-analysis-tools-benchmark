--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Gauge.Wall_Clock.Imperial               Luebeck            --
--  Interface                                      Winter, 2010       --
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

with Ada.Calendar;             use Ada.Calendar;
with Gtk.Adjustment;           use Gtk.Adjustment;
with Gtk.Handlers;             use Gtk.Handlers;
with Gtk.Handlers.References;  use Gtk.Handlers.References;
with Gtk.Layered;              use Gtk.Layered;
with Gtk.Layered.Clock_Hand;   use Gtk.Layered.Clock_Hand;
with Gtk.Layered.Cache;        use Gtk.Layered.Cache;
with Gtk.Widget;               use Gtk.Widget;

with Gtk.Layered.Arc;
with Gtk.Layered.Elliptic_Scale;
with Gtk.Layered.Elliptic_Annotation;
with Gtk.Layered.Elliptic_Background;

package Gtk.Wall_Clock.Imperial is
   use Gtk.Layered.Elliptic_Annotation;
   use Gtk.Layered.Elliptic_Background;
--
-- Class_Name - Of the widget
--
   Class_Name : constant String := "GtkWallClockImperial";
--
-- Gtk_Clock_Rococo -- Rococo clock
--
   type Gtk_Wall_Clock_Imperial_Record is
      new Gtk_Layered_Record with private;
   type Gtk_Wall_Clock_Imperial is
      access all Gtk_Wall_Clock_Imperial_Record'Class;
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
--    Widget     - The result
--    Adjustment - The adjustment object to indicate
--
   procedure Gtk_New
             (  Widget     : out Gtk_Wall_Clock_Imperial;
                Adjustment : Gtk_Adjustment := null
             );
--
-- Initialize -- The widget initialization
--
--    Widget     - The widget to initialize
--    Adjustment - The adjustment object to indicate
--
-- When  a  widget  type  is  derived from this one, it has to call this
-- procedure from its version of Initialize.
--
   procedure Initialize
             (  Widget : not null access
                         Gtk_Wall_Clock_Imperial_Record'Class;
                Adjustment : Gtk_Adjustment
             );
--
-- Get_Annotation -- The clock's annotation
--
--    Widget - The widget
--
-- Returns :
--
--    The annotation layer
--
   function Get_Annotation
            (  Widget : not null access Gtk_Wall_Clock_Imperial_Record
            )  return not null access Elliptic_Annotation_Layer;
--
-- Get_Background -- The clock's background
--
--    Widget - The widget
--
-- Returns :
--
--    The background layer
--
   function Get_Background
            (  Widget : not null access Gtk_Wall_Clock_Imperial_Record
            )  return not null access Elliptic_Background_Layer;
--
-- Get_Hour_Hand -- The clock hand
--
--    Widget - The widget
--
-- Returns :
--
--    The hour hand layer of the widget
--
   function Get_Hour_Hand
            (  Widget : not null access Gtk_Wall_Clock_Imperial_Record
            )  return not null access Clock_Hand_Layer;
--
-- Get_Minute_Hand -- The clock hand
--
--    Widget - The widget
--
-- Returns :
--
--    The minute hand layer of the widget
--
   function Get_Minute_Hand
            (  Widget : not null access Gtk_Wall_Clock_Imperial_Record
            )  return not null access Clock_Hand_Layer;
--
-- Get_Cache -- The gauge caching layer
--
--    Widget - The widget
--
-- If the widget is extended, static things which do not change with the
-- widget state should be placed below the caching layer for performance
-- reasons.
--
-- Returns :
--
--    The cache layer of the widget
--
   function Get_Cache
            (  Widget : not null access Gtk_Wall_Clock_Imperial_Record
            )  return not null access Cache_Layer;
--
-- Set_Value -- Change the value indicated by the gauge
--
--    Widget - The widget
--    Value  - The value
--
   procedure Set_Value
             (  Widget : not null access Gtk_Wall_Clock_Imperial_Record;
                Value  : Time
             );

   overriding
      procedure Style_Changed
                (  Widget : not null access
                            Gtk_Wall_Clock_Imperial_Record
                );

private
   use Gtk.Layered.Arc;
   use Gtk.Layered.Cache;
   use Gtk.Layered.Elliptic_Scale;

   type Gtk_Wall_Clock_Imperial_Record is
      new Gtk_Layered_Record with
   record
      Adjustment    : Gtk_Adjustment;
      Changed       : Handler_Reference;
      Value_Changed : Handler_Reference;
      Background    : access Elliptic_Background_Layer;
      Cache         : access Cache_Layer;
      Line_1        : access Arc_Layer;
      Line_2        : access Arc_Layer;
      Pin           : access Elliptic_Background_Layer;
      Minor_Ticks   : access Elliptic_Scale_Layer;
      Major_Ticks   : access Elliptic_Scale_Layer;
      Annotation    : access Elliptic_Annotation_Layer;
      Hour_Needle   : access Clock_Hand_Layer;
      Minute_Needle : access Clock_Hand_Layer;
   end record;

   procedure Set
             (  Widget  : not null access
                          Gtk_Wall_Clock_Imperial_Record;
                Seconds : GDouble
             );

end Gtk.Wall_Clock.Imperial;
