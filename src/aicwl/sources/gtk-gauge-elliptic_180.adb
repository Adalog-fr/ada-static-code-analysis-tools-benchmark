--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Gauge.Elliptic_180                      Luebeck            --
--  Implementation                                 Winter, 2011       --
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

with Ada.Numerics;              use Ada.Numerics;
with Cairo;                     use Cairo;
with Cairo.Ellipses;            use Cairo.Ellipses;
with Gdk.Color;                 use Gdk.Color;
with GLib.Properties.Creation;  use GLib.Properties.Creation;
with GLib.Types;                use GLib.Types;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Missed;                use Gtk.Missed;
with Gtk.Widget.Styles;         use Gtk.Widget.Styles;
with Pango.Cairo.Fonts;         use Pango.Cairo.Fonts;

with GLib.Object.Checked_Destroy;

with Cairo.Line_Cap_Property;
use  Cairo.Line_Cap_Property;

with Gtk.Widget.Styles.Line_Cap_Property;
use  Gtk.Widget.Styles.Line_Cap_Property;

package body Gtk.Gauge.Elliptic_180 is

   Needle_Color     : constant Gdk_Color := RGB (1.0, 0.0, 0.0);
   Background_Color : constant Gdk_Color := RGB (0.0, 0.0, 0.0);
   Scale_Area_Color : constant Gdk_Color := RGB (1.0, 1.0, 1.0);
   Major_Tick_Color : constant Gdk_Color := RGB (0.0, 0.0, 0.0);
   Text_Color       : constant Gdk_Color := RGB (1.0, 0.6, 0.0);

   Class_Record : aliased Ada_GObject_Class := Uninitialized_Class;

   Length : constant GDouble := Pi;
   First  : constant GDouble := Pi;
   Excess : constant GDouble := Pi / 30.0;
   Y : constant := 0.225;

   Inner : constant Ellipse_Parameters :=
             ((-0.045, Y), 1.0 / 0.40, 0.31, -Pi / 5.0);
   Outer : constant Ellipse_Parameters :=
             ((-0.020, Y), 1.0 / 0.43, 0.36, -Pi / 4.0);

   procedure Create_Background
             (  Widget  : not null access
                          Gtk_Gauge_Elliptic_180_Record'Class;
                Sectors : Positive
             )  is
   begin
      G_New (Widget, Get_Type);
      Gtk.Layered.Initialize (Widget);
      Widget.Sectors := Sectors;
      Set_Aspect_Ratio (Widget, 1.81072266020785);
      Widget.Background :=
         Add_Elliptic_Background
         (  Under         => Widget,
            Color         => Background_Color,
            Outer         => ((0.0, Y), 1.0 / 0.5, 0.5, 0.0),
            From          => First  - Excess,
            Length        => Length + Excess * 2.0,
            Border_Width  => 0.01,
            Border_Depth  => 0.005,
            Border_Shadow => Shadow_Etched_Out,
            Deepened      => True,
            Widened       => True,
            Scaled        => True
         );
      Widget.Scale_Area :=
         Add_Elliptic_Background
         (  Under         => Widget.Background.Get_Foreground,
            Color         => Scale_Area_Color,
            Inner         => Inner,
            Outer         => Outer,
            From          => Pi,
            Length        => Pi,
            Scaled        => True
         );
      Widget.Major_Ticks :=
         Add_Elliptic_Scale
         (  Under   => Widget.Background.Get_Foreground,
            Inner   => Outer * 0.5,
            Outer   => Outer,
            Color   => Background_Color,
            Width   => 3.0 / 400.0,
            Step    => Length / (2.0 * GDouble (Widget.Sectors)),
            From    => First,
            Length  => Length,
            Scaled  => True,
            Widened => True
         );
      Widget.Cache := Add_Cache (Widget.Background.Get_Foreground);
   end Create_Background;

   procedure Create_Needle
             (  Widget  : not null access
                          Gtk_Gauge_Elliptic_180_Record'Class;
                Adjustment : Gtk_Adjustment
             )  is
   begin
      Widget.Needle :=
         Add_Needle
         (  Under       => Widget.Background.Get_Foreground,
            Center      => Outer.Center,
            Tip_Cap     => CAIRO_LINE_CAP_SQUARE,
            Adjustment  => Adjustment,
            Tip_Length  => 0.38,
            Tip_Width   => 0.01,
            Rear_Length => 0.0,
            Rear_Width  => 0.03,
            Rear_Cap    => CAIRO_LINE_CAP_ROUND,
            Color       => Needle_Color,
            From        => First,
            Length      => Length,
            Scaled      => True
         );
   end Create_Needle;

   function Get_Annotation
            (  Widget : not null access Gtk_Gauge_Elliptic_180_Record
            )  return not null access Elliptic_Annotation_Layer is
   begin
      return Widget.Annotation;
   end Get_Annotation;

   function Get_Needle
            (  Widget : not null access Gtk_Gauge_Elliptic_180_Record
            )  return not null access Needle_Layer is
   begin
      return Widget.Needle;
   end Get_Needle;

   function Get_Background
            (  Widget : not null access Gtk_Gauge_Elliptic_180_Record
            )  return not null access Elliptic_Background_Layer is
   begin
      return Widget.Background;
   end Get_Background;

   function Get_Cache
            (  Widget : not null access Gtk_Gauge_Elliptic_180_Record
            )  return not null access Cache_Layer is
   begin
      return Widget.Cache;
   end Get_Cache;

   function Get_Type return GType is
   begin
      if Initialize_Class_Record
         (  Ancestor     => Gtk.Layered.Get_Type,
            Class_Record => Class_Record'Access,
            Type_Name    => Class_Name
         )
      then
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_Boxed
            (  Name       => "needle-color",
               Boxed_Type => Gdk_Color_Type,
               Nick       => "Needle color",
               Blurb      => "The color of the gauge's needle"
         )  );
         Install_Style
         (  Class_Ref (Class_Record.The_Type),
            Cairo.Line_Cap_Property.Gnew_Enum
            (  Name    => "needle-tip-cap",
               Nick    => "Tip cap",
               Blurb   => "The style used for the needle tip",
               Default => CAIRO_LINE_CAP_ROUND
         )  );
         Install_Style
         (  Class_Ref (Class_Record.The_Type),
            Cairo.Line_Cap_Property.Gnew_Enum
            (  Name    => "needle-rear-cap",
               Nick    => "Rear cap",
               Blurb   => "The style used for the needle rear",
               Default => CAIRO_LINE_CAP_BUTT
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_Boxed
            (  Name       => "backgound-color",
               Boxed_Type => Gdk_Color_Type,
               Nick       => "Background color",
               Blurb      => "The background color"
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_Boxed
            (  Name       => "major-tick-color",
               Boxed_Type => Gdk_Color_Type,
               Nick       => "Major ticks color",
               Blurb      => "Major ticks color"
         )  );
         Install_Style
         (  Class_Ref (Class_Record.The_Type),
            Cairo.Line_Cap_Property.Gnew_Enum
            (  Name    => "major-tick-line-cap",
               Nick    => "Major tick cap",
               Blurb   => "The line cap style used for major ticks",
               Default => CAIRO_LINE_CAP_BUTT
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_Boxed
            (  Name       => "text-color",
               Boxed_Type => Gdk_Color_Type,
               Nick       => "Text color",
               Blurb      => "Text color"
         )  );
      end if;
      return Class_Record.The_Type;
   end Get_Type;

   procedure Gtk_New
             (  Widget     : out Gtk_Gauge_Elliptic_180;
                Texts      : Gtk.Enums.String_List.GList;
                Adjustment : Gtk_Adjustment := null;
                Sectors    : Positive       := 8
             )  is
   begin
      Widget := new Gtk_Gauge_Elliptic_180_Record;
      Initialize (Widget, Texts, Adjustment, Sectors);
   exception
      when others =>
         GLib.Object.Checked_Destroy (Widget);
         Widget := null;
         raise;
   end Gtk_New;

   procedure Gtk_New
             (  Widget     : out Gtk_Gauge_Elliptic_180;
                Texts      : Controlled_String_List;
                Adjustment : Gtk_Adjustment := null;
                Sectors    : Positive       := 8
             )  is
   begin
      Widget := new Gtk_Gauge_Elliptic_180_Record;
      Initialize (Widget, Texts, Adjustment, Sectors);
   exception
      when others =>
         GLib.Object.Checked_Destroy (Widget);
         Widget := null;
         raise;
   end Gtk_New;

   procedure Gtk_New
             (  Widget     : out Gtk_Gauge_Elliptic_180;
                Texts      : UTF8_String;
                Delimiter  : Character      := ' ';
                Adjustment : Gtk_Adjustment := null;
                Sectors    : Positive       := 8
             )  is
   begin
      Widget := new Gtk_Gauge_Elliptic_180_Record;
      Initialize (Widget, Texts, Delimiter, Adjustment, Sectors);
   exception
      when others =>
         GLib.Object.Checked_Destroy (Widget);
         Widget := null;
         raise;
   end Gtk_New;

   procedure Initialize
             (  Widget     : not null access
                                Gtk_Gauge_Elliptic_180_Record'Class;
                Texts      : Gtk.Enums.String_List.GList;
                Adjustment : Gtk_Adjustment;
                Sectors    : Positive
             )  is
   begin
      Create_Background (Widget, Sectors);
      Widget.Annotation :=
         Add_Elliptic_Annotation
         (  Under   => Widget.Cache,
            Ellipse => Outer * 1.05,
            Texts   => Texts,
            Face    =>
               Create_Toy
               (  Family => "arial",
                  Slant  => CAIRO_FONT_SLANT_ITALIC,
                  Weight => CAIRO_FONT_WEIGHT_BOLD
               ),
            Step    => Length / GDouble (Sectors),
            Height  => 0.03,
            Stretch => 0.9,
            Color   => Text_Color,
            From    => First,
            Length  => Length,
            Mode    => Moved_Outside,
            Scaled  => True
         );
      Create_Needle (Widget, Adjustment);
   end Initialize;

   procedure Initialize
             (  Widget     : not null access
                                Gtk_Gauge_Elliptic_180_Record'Class;
                Texts      : Controlled_String_List;
                Adjustment : Gtk_Adjustment;
                Sectors    : Positive
             )  is
   begin
      Initialize (Widget, Get_GList (Texts), Adjustment, Sectors);
   end Initialize;

   procedure Initialize
             (  Widget     : not null access
                                Gtk_Gauge_Elliptic_180_Record'Class;
                Texts      : UTF8_String;
                Delimiter  : Character;
                Adjustment : Gtk_Adjustment;
                Sectors    : Positive
             )  is
   begin
      Create_Background (Widget, Sectors);
      Widget.Annotation :=
         Add_Elliptic_Annotation
         (  Under     => Widget.Cache,
            Ellipse   => Outer * 1.05,
            Texts     => Texts,
            Delimiter => Delimiter,
            Face =>
               Create_Toy
               (  Family => "arial",
                  Slant  => CAIRO_FONT_SLANT_ITALIC,
                  Weight => CAIRO_FONT_WEIGHT_BOLD
               ),
            Step    => Length / GDouble (Sectors),
            Height  => 0.03,
            Stretch => 0.9,
            Color   => Text_Color,
            From    => First,
            Length  => Length,
            Mode    => Moved_Outside,
            Scaled  => True
         );
      Create_Needle (Widget, Adjustment);
   end Initialize;

   procedure Set_Value
             (  Widget : not null access Gtk_Gauge_Elliptic_180_Record;
                Value  : GDouble
             )  is
   begin
      Widget.Needle.Set_Value (Value);
   end Set_Value;

   procedure Style_Changed
             (  Widget : not null access Gtk_Gauge_Elliptic_180_Record
             )  is
   begin
      Widget.Needle.Set
      (  Center => Widget.Needle.Get_Center,
         From   => Widget.Needle.Get_From,
         Length => Widget.Needle.Get_Length,
         Tip    => (  Length => Widget.Needle.Get_Tip.Length,
                      Width  => Widget.Needle.Get_Tip.Width,
                      Cap    => Style_Get (Widget, "needle-tip-cap")
                   ),
         Rear   => (  Length => Widget.Needle.Get_Rear.Length,
                      Width  => Widget.Needle.Get_Rear.Width,
                      Cap    => Style_Get (Widget, "needle-rear-cap")
                   ),
         Color  => Style_Get (Widget, "needle-color", Needle_Color)
      );
      Widget.Background.Set
      (  Outer         => Widget.Background.Get_Outer,
         Inner         => Widget.Background.Get_Inner,
         From          => Widget.Background.Get_From,
         Length        => Widget.Background.Get_Length,
         Border_Width  => Widget.Background.Get_Border_Width,
         Border_Depth  => Widget.Background.Get_Border_Depth,
         Border_Color  => Widget.Background.Get_Border_Color,
         Border_Shadow => Widget.Background.Get_Border_Shadow,
         Lens_Reflex   => Widget.Background.Get_Lens_Reflex,
         Lens_Shadow   => Widget.Background.Get_Lens_Shadow,
         Color  =>
            Style_Get (Widget, "backgound-color", Background_Color)
      );
      Widget.Major_Ticks.Set
      (  Inner  => Widget.Major_Ticks.Get_Inner,
         Outer  => Widget.Major_Ticks.Get_Outer,
         Ticks  => Widget.Major_Ticks.Get_Ticks,
         From   => Widget.Major_Ticks.Get_From,
         Length => Widget.Major_Ticks.Get_Length,
         Line =>
            (  Widget.Major_Ticks.Get_Line.Width,
               Style_Get (Widget, "major-tick-color", Major_Tick_Color),
               Style_Get (Widget, "major-tick-line-cap")
      )     );
      Widget.Annotation.Set
      (  Ellipse => Widget.Annotation.Get_Ellipse,
         Ticks   => Widget.Annotation.Get_Ticks,
         From    => Widget.Annotation.Get_From,
         Length  => Widget.Annotation.Get_Length,
         Face    => Widget.Annotation.Get_Face,
         Mode    => Widget.Annotation.Get_Mode,
         Height  => Widget.Annotation.Get_Height,
         Stretch => Widget.Annotation.Get_Stretch,
         Color   => Style_Get (Widget, "text-color", Text_Color)
      );
   end Style_Changed;

end Gtk.Gauge.Elliptic_180;
