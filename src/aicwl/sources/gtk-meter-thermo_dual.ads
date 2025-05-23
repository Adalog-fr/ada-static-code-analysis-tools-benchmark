--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Meter.Thermo_Dual                       Luebeck            --
--  Interface                                      Summer, 2012       --
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

with Gdk.Color;                    use Gdk.Color;
with Gtk.Adjustment;               use Gtk.Adjustment;
with Gtk.Layered;                  use Gtk.Layered;
with Gtk.Layered.Bar;              use Gtk.Layered.Bar;
with Gtk.Layered.Cache;            use Gtk.Layered.Cache;
with Gtk.Layered.Flat_Annotation;  use Gtk.Layered.Flat_Annotation;
with Gtk.Layered.Label;            use Gtk.Layered.Label;
with Gtk.Layered.Line;             use Gtk.Layered.Line;
with Gtk.Missed;                   use Gtk.Missed;
with Gtk.Widget;                   use Gtk.Widget;

with Gtk.Layered.Flat_Scale;

with Gtk.Layered.Rectangular_Background;
use  Gtk.Layered.Rectangular_Background;

package Gtk.Meter.Thermo_Dual is
--
-- Class_Name - Of the widget
--
   Class_Name : constant String := "GtkMeterThermoDual";
--
-- Gtk_Meter_Thermo_Dual -- Thermometer with dual annotation
--
   type Gtk_Meter_Thermo_Dual_Record is
      new Gtk_Layered_Record with private;
   type Gtk_Meter_Thermo_Dual is
      access all Gtk_Meter_Thermo_Dual_Record'Class;
--
-- Get_Type -- The type of the widget
--
-- Returns :
--
--    The GTK type of the widget
--
   function Get_Type return GType;
--
-- Gtk_New_{Celsius|Fahrenheit} -- Widget construction
--
--    Widget     - The result
--    Adjustment - The adjustment object to indicate
--    Sectors    - The number of intervals between major ticks
--    Color      - The bar color
--
-- Normally there should be Sector + 1 texts in the  list  Texts.  Extra
-- texts are ignored. Missing texts are shown empty.
--
   procedure Gtk_New_Celsius
             (  Widget     : out Gtk_Meter_Thermo_Dual;
                Adjustment : not null access
                             Gtk_Adjustment_Record'Class;
                Sectors    : Positive  := 8;
                Color      : Gdk_Color := RGB (1.0, 0.0, 0.0)
             );
   procedure Gtk_New_Fahrenheit
             (  Widget     : out Gtk_Meter_Thermo_Dual;
                Adjustment : not null access
                             Gtk_Adjustment_Record'Class;
                Sectors    : Positive  := 8;
                Color      : Gdk_Color := RGB (1.0, 0.0, 0.0)
             );
--
-- Gtk_New_{Celsius|Fahrenheit} -- Widget construction
--
--    Widget  - The result
--    Lower   - The lower temperature
--    Upper   - The upper temperature
--    Sectors - The number of intervals between major ticks
--    Color   - The bar color
--
-- Normally there should be Sector + 1 texts in the  list  Texts.  Extra
-- texts are ignored. Missing texts are shown empty.
--
   procedure Gtk_New_Celsius
             (  Widget  : out Gtk_Meter_Thermo_Dual;
                Lower   : GDouble   := -40.0;
                Upper   : GDouble   := 50.0;
                Sectors : Positive  := 8;
                Color   : Gdk_Color := RGB (1.0, 0.0, 0.0)
             );
   procedure Gtk_New_Fahrenheit
             (  Widget  : out Gtk_Meter_Thermo_Dual;
                Lower   : GDouble   := 20.0;
                Upper   : GDouble   := 220.0;
                Sectors : Positive  := 8;
                Color   : Gdk_Color := RGB (1.0, 0.0, 0.0)
             );
--
-- Initialize_{Celsius|Fahrenheit} -- The widget initialization
--
--    Widget     - The widget to initialize
--    Adjustment - The adjustment object to indicate
--    Sectors    - The number of intervals between major ticks
--    Color      - The bar color
--
-- When  a  widget  type  is  derived from this one, it has to call this
-- procedure from its version of Initialize.
--
   procedure Initialize_Celsius
             (  Widget  : not null access
                          Gtk_Meter_Thermo_Dual_Record'Class;
                Lower   : GDouble;
                Upper   : GDouble;
                Sectors : Positive;
                Color   : Gdk_Color
             );
   procedure Initialize_Fahrenheit
             (  Widget  : not null access
                          Gtk_Meter_Thermo_Dual_Record'Class;
                Lower   : GDouble;
                Upper   : GDouble;
                Sectors : Positive;
                Color   : Gdk_Color
             );
--
-- Initialize_{Celsius|Fahrenheit} -- The widget initialization
--
--    Widget  - The widget to initialize
--    Lower   - The lower temperature
--    Upper   - The upper temperature
--    Sectors - The number of intervals between major ticks
--    Color   - The bar color
--
-- When  a  widget  type  is  derived from this one, it has to call this
-- procedure from its version of Initialize.
--
   procedure Initialize_Celsius
             (  Widget     : not null access
                             Gtk_Meter_Thermo_Dual_Record'Class;
                Adjustment : not null access
                             Gtk_Adjustment_Record'Class;
                Sectors    : Positive;
                Color      : Gdk_Color
             );
   procedure Initialize_Fahrenheit
             (  Widget     : not null access
                             Gtk_Meter_Thermo_Dual_Record'Class;
                Adjustment : not null access
                             Gtk_Adjustment_Record'Class;
                Sectors    : Positive;
                Color      : Gdk_Color
             );
--
-- Get_Background -- The thermometer's background
--
--    Widget - The widget
--
-- Returns :
--
--    The background layer
--
   function Get_Background
            (  Widget : not null access Gtk_Meter_Thermo_Dual_Record
            )  return not null access Rectangular_Background_Layer;
--
-- Get_Bar -- The themperature bar
--
--    Widget - The widget
--
-- Returns :
--
--    The bar layer of the widget
--
   function Get_Bar
            (  Widget : not null access Gtk_Meter_Thermo_Dual_Record
            )  return not null access Bar_Layer;
--
-- Get_Bar_Color -- The themperature bar color
--
--    Widget - The widget
--
-- Returns :
--
--    The color of the bar
--
   function Get_Bar_Color
            (  Widget : not null access Gtk_Meter_Thermo_Dual_Record
            )  return Gdk_Color;
--
-- Get_Cache -- The thermometer's caching layer
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
            (  Widget : not null access Gtk_Meter_Thermo_Dual_Record
            )  return not null access Cache_Layer;
--
-- Get_{Celsius|Fahrenheit}_Annotation -- The thermometer's annotation
--
--    Widget - The widget
--
-- Returns :
--
--    The annotation layer
--
   function Get_Celsius_Annotation
            (  Widget : not null access Gtk_Meter_Thermo_Dual_Record
            )  return not null access Flat_Annotation_Layer;
   function Get_Fahrenheit_Annotation
            (  Widget : not null access Gtk_Meter_Thermo_Dual_Record
            )  return not null access Flat_Annotation_Layer;
--
-- Get_{Celsius|Fahrenheit}_Label -- The thermometer label
--
--    Widget - The widget
--
-- Returns :
--
--    The label layer of the widget
--
   function Get_Celsius_Label
            (  Widget : not null access Gtk_Meter_Thermo_Dual_Record
            )  return not null access Label_Layer;
   function Get_Fahrenheit_Label
            (  Widget : not null access Gtk_Meter_Thermo_Dual_Record
            )  return not null access Label_Layer;
--
-- Get_{Celsius|Fahrenheit}_Value -- The thermometer's value
--
--    Widget - The widget
--
-- Returns :
--
--    The value
--
   function Get_Celsius_Value
            (  Widget : not null access Gtk_Meter_Thermo_Dual_Record
            )  return GDouble;
   function Get_Fahrenheit_Value
            (  Widget : not null access Gtk_Meter_Thermo_Dual_Record
            )  return GDouble;
--
-- Set_Bar_Color -- Set the themperature bar color
--
--    Widget - The widget
--    Color  - The color to set
--
-- Returns :
--
--    The color of the bar
--
   procedure Set_Bar_Color
             (  Widget : not null access Gtk_Meter_Thermo_Dual_Record;
                Color  : Gdk_Color
             );
--
-- Set_Celsius_Value -- Change the value indicated by the thermometer
--
--    Widget - The widget
--    Value  - The value in Celsius
--
-- When  the value is out of range it is saturated to the nearest bound.
-- Note  that  procedure does not emit any events, if the widget need to
-- be redrawn the event "draw" should be emitted.
--
   procedure Set_Celsius_Value
             (  Widget : not null access Gtk_Meter_Thermo_Dual_Record;
                Value  : GDouble
             );
   procedure Set_Fahrenheit_Value
             (  Widget : not null access Gtk_Meter_Thermo_Dual_Record;
                Value  : GDouble
             );

   overriding
      procedure Style_Changed
                (  Widget : not null access Gtk_Meter_Thermo_Dual_Record
                );

private
   use Gtk.Layered.Cache;
   use Gtk.Layered.Flat_Scale;

   type Temperature_Scale is record
      Minor_Ticks  : access Flat_Scale_Layer;
      Middle_Ticks : access Flat_Scale_Layer;
      Major_Ticks  : access Flat_Scale_Layer;
      Annotation   : access Flat_Annotation_Layer;
      Label        : access Label_Layer;
      Factor       : GDouble; -- length / span [gives screen coord]
      First        : GDouble; -- Lower major tick location
      Step         : GDouble; -- Major tick step
      Lower        : GDouble; -- Value at the lower major tick
      From         : GDouble; -- Lower value of the range
      Span         : GDouble; -- Values span
      Small        : Integer; -- Least significant digit
      Sectors      : Integer; -- Number of sectors
   end record;

   type Gtk_Meter_Thermo_Dual_Record is
      new Gtk_Layered_Record with
   record
      Background : access Rectangular_Background_Layer;
      Cache      : access Cache_Layer;
      Fahrenheit : Temperature_Scale;
      Celsius    : Temperature_Scale;
      Bar        : access Bar_Layer;
      Bulb       : access Line_Layer;
      Reflection : access Line_Layer;
      Stem       : access Line_Layer;
   end record;

end Gtk.Meter.Thermo_Dual;
