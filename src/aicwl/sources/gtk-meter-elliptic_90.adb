--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Meter.Elliptic_90                       Luebeck            --
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
with Gdk.Color;                 use Gdk.Color;
with GLib.Properties.Creation;  use GLib.Properties.Creation;
with GLib.Types;                use GLib.Types;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Missed;                use Gtk.Missed;
with Gtk.Widget.Styles;         use Gtk.Widget.Styles;
with Pango.Cairo.Fonts;         use Pango.Cairo.Fonts;

with Cairo.Line_Cap_Property;
with Gtk.Layered.Rectangular_Clip_Region;
with GLib.Object.Checked_Destroy;
with Gtk.Widget.Styles.Line_Cap_Property;

package body Gtk.Meter.Elliptic_90 is
   use Gtk.Layered.Rectangular_Clip_Region;
   use Gtk.Widget.Styles.Line_Cap_Property;

   Needle_Color      : constant Gdk_Color := RGB (1.0, 0.0, 0.0);
   Background_Color  : constant Gdk_Color := RGB (1.0, 1.0, 1.0);
   Major_Tick_Color  : constant Gdk_Color := RGB (0.0, 0.0, 0.0);
   Middle_Tick_Color : constant Gdk_Color := RGB (0.0, 0.0, 0.0);
   Minor_Tick_Color  : constant Gdk_Color := RGB (0.0, 0.0, 0.0);
   Text_Color        : constant Gdk_Color := RGB (0.0, 0.0, 0.0);

   Class_Record : aliased Ada_GObject_Class := Uninitialized_Class;

   Corner : constant := 1.0 / 30.0;
   Height : constant := 0.5;
   Length : constant := 0.94 * Pi / 2.0;
   Pin    : constant := Height * 1.5 / 2.0;
   First  : constant := (Pi * 3.0 - Length) / 2.0;

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
            (  Name       => "line-color",
               Boxed_Type => Gdk_Color_Type,
               Nick       => "Line color",
               Blurb => "The color of the circle bounding the ticks"
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
            (  Name       => "minor-tick-color",
               Boxed_Type => Gdk_Color_Type,
               Nick       => "Minor ticks color",
               Blurb      => "Minor ticks color"
         )  );
         Install_Style
         (  Class_Ref (Class_Record.The_Type),
            Cairo.Line_Cap_Property.Gnew_Enum
            (  Name    => "minor-tick-line-cap",
               Nick    => "Minor tick cap",
               Blurb   => "The line cap style used for minor ticks",
               Default => CAIRO_LINE_CAP_BUTT
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_Boxed
            (  Name       => "middle-tick-color",
               Boxed_Type => Gdk_Color_Type,
               Nick       => "Middle ticks color",
               Blurb      => "Middle ticks color"
         )  );
         Install_Style
         (  Class_Ref (Class_Record.The_Type),
            Cairo.Line_Cap_Property.Gnew_Enum
            (  Name    => "middle-tick-line-cap",
               Nick    => "Middle tick cap",
               Blurb   => "The line cap style used for middle ticks",
               Default => CAIRO_LINE_CAP_BUTT
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_Boxed
            (  Name       => "pin-color",
               Boxed_Type => Gdk_Color_Type,
               Nick       => "Pin color",
               Blurb      => "Arrow pin color"
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

   procedure Create_Background
             (  Widget : not null access
                         Gtk_Meter_Elliptic_90_Record'Class;
                Sectors : Positive
             )  is
   begin
      G_New (Widget, Get_Type);
      Gtk.Layered.Initialize (Widget);
      Widget.Sectors := Sectors;
      Set_Aspect_Ratio (Widget, 2.0);
      Widget.Background :=
         Add_Rectangular_Background
         (  Under         => Widget,
            Height        => 0.5,
            Width         => 1.0,
            Center        => (0.0, 0.0),
            Corner_Radius => Corner,
            Color         => Background_Color,
            Border_Width  => 0.01,
            Border_Depth  => 0.005,
            Border_Shadow => Shadow_Etched_Out,
            Deepened      => True,
            Widened       => True,
            Scaled        => True
         );
      Widget.Major_Ticks :=
         Add_Elliptic_Scale
         (  Under => Widget.Background.Get_Foreground,
            Outer =>
               (  Center          => (0.0, 0.0 + Pin),
                  Major_Curvature => 1.0 / (Height * 1.8),
                  Minor_Radius    => Height * 1.0,
                  Angle           => 0.0
               ),
            Inner =>
               (  Center          => (0.0, 0.0 + Pin),
                  Major_Curvature => 1.0 / (Height * 1.6),
                  Minor_Radius    => Height * 0.85,
                  Angle           => 0.0
               ),
            Color   => Major_Tick_Color,
            Width   => 1.5 / 300.0,
            Step    => Length / GDouble (Sectors),
            From    => First,
            Length  => Length,
            Scaled  => True,
            Widened => True
         );
      Widget.Middle_Ticks :=
         Add_Elliptic_Scale
         (  Under => Widget.Background.Get_Foreground,
            Outer =>
               (  Center          => (0.0, 0.0 + Pin),
                  Major_Curvature => 1.0 / (Height * 1.8),
                  Minor_Radius    => Height * 1.0,
                  Angle           => 0.0
               ),
            Inner =>
               (  Center          => (0.0, 0.0 + Pin),
                  Major_Curvature => 1.0 / (Height * 1.6),
                  Minor_Radius    => Height * 0.85,
                  Angle           => 0.0
               ),
            Color   => Middle_Tick_Color,
            Width   => 1.5 / 300.0,
            Step    => 0.5 * Length / GDouble (Sectors),
            Skipped => 2,
            From    => First,
            Length  => Length,
            Scaled  => True,
            Widened => True
         );
      Widget.Minor_Ticks :=
         Add_Elliptic_Scale
         (  Under => Widget.Background.Get_Foreground,
            Outer =>
               (  Center          => (0.0, 0.0 + Pin),
                  Major_Curvature => 1.0 / (Height * 1.8),
                  Minor_Radius    => Height * 0.95,
                  Angle           => 0.0
               ),
            Inner =>
               (  Center          => (0.0, 0.0 + Pin),
                  Major_Curvature => 1.0 / (Height * 1.6),
                  Minor_Radius    => Height * 0.85,
                  Angle           => 0.0
               ),
            Color   => Minor_Tick_Color,
            Width   => 1.0 / 300.0,
            Step    => 0.1 * Length / GDouble (Sectors),
            Skipped => 5,
            From    => First,
            Length  => Length,
            Scaled  => True,
            Widened => True
         );
      Widget.Cache := Add_Cache (Widget.Background.Get_Foreground);
   end Create_Background;

   procedure Create_Foreground
             (  Widget : not null access
                         Gtk_Meter_Elliptic_90_Record'Class;
                Adjustment : Gtk_Adjustment
             )  is
   begin
      Widget.Needle :=
         Add_Needle
         (  Under =>
               Add_Rectangular_Clip_Region
               (  Under         => Widget.Background.Get_Foreground,
                  Height        => Height * 0.957,
                  Width         => 0.9,
                  Center        => (0.0, 0.0),
                  Corner_Radius => Corner,
                  Scaled        => True
               ) .Above,
            Center      => (0.0, 0.0 + Pin),
            Tip_Cap     => CAIRO_LINE_CAP_SQUARE,
            Adjustment  => Adjustment,
            Tip_Length  => 0.57,
            Tip_Width   => 0.005,
            Rear_Length => 0.0,
            Rear_Width  => 0.007,
            Color       => Needle_Color,
            From        => First,
            Length      => Length,
            Scaled      => True
         );
   end Create_Foreground;

   function Get_Annotation
            (  Widget : not null access
                        Gtk_Meter_Elliptic_90_Record
            )  return not null access Elliptic_Annotation_Layer is
   begin
      return Widget.Annotation;
   end Get_Annotation;

   function Get_Needle
            (  Widget : not null access
                        Gtk_Meter_Elliptic_90_Record
            )  return not null access Needle_Layer is
   begin
      return Widget.Needle;
   end Get_Needle;

   function Get_Background
            (  Widget : not null access
                        Gtk_Meter_Elliptic_90_Record
            )  return not null access Rectangular_Background_Layer is
   begin
      return Widget.Background;
   end Get_Background;

   function Get_Cache
            (  Widget : not null access
                        Gtk_Meter_Elliptic_90_Record
            )  return not null access Cache_Layer is
   begin
      return Widget.Cache;
   end Get_Cache;

   procedure Gtk_New
             (  Widget     : out Gtk_Meter_Elliptic_90;
                Texts      : Gtk.Enums.String_List.GList;
                Adjustment : Gtk_Adjustment := null;
                Sectors    : Positive       := 5
             )  is
   begin
      Widget := new Gtk_Meter_Elliptic_90_Record;
      Initialize (Widget, Texts, Adjustment, Sectors);
   exception
      when others =>
         GLib.Object.Checked_Destroy (Widget);
         Widget := null;
         raise;
   end Gtk_New;

   procedure Gtk_New
             (  Widget     : out Gtk_Meter_Elliptic_90;
                Texts      : Controlled_String_List;
                Adjustment : Gtk_Adjustment := null;
                Sectors    : Positive       := 5
             )  is
   begin
      Widget := new Gtk_Meter_Elliptic_90_Record;
      Initialize (Widget, Texts, Adjustment, Sectors);
   exception
      when others =>
         GLib.Object.Checked_Destroy (Widget);
         Widget := null;
         raise;
   end Gtk_New;

   procedure Gtk_New
             (  Widget     : out Gtk_Meter_Elliptic_90;
                Texts      : UTF8_String;
                Delimiter  : Character      := ' ';
                Adjustment : Gtk_Adjustment := null;
                Sectors    : Positive       := 5
             )  is
   begin
      Widget := new Gtk_Meter_Elliptic_90_Record;
      Initialize (Widget, Texts, Delimiter, Adjustment, Sectors);
   exception
      when others =>
         GLib.Object.Checked_Destroy (Widget);
         Widget := null;
         raise;
   end Gtk_New;

   procedure Initialize
             (  Widget : not null access
                         Gtk_Meter_Elliptic_90_Record'Class;
                Texts      : Gtk.Enums.String_List.GList;
                Adjustment : Gtk_Adjustment;
                Sectors    : Positive
             )  is
   begin
      Create_Background (Widget, Sectors);
      Widget.Annotation :=
         Add_Elliptic_Annotation
         (  Under   => Widget.Cache,
            Ellipse =>
               (  Center          => (0.0, 0.0 + Pin),
                  Major_Curvature => 1.0 / (Height * 2.0),
                  Minor_Radius    => Height * 1.055,
                  Angle           => 0.0
               ),
            Texts     => Texts,
            Face      => Create_Toy
                         (  Family => "lucida",
                            Slant  => CAIRO_FONT_SLANT_NORMAL,
                            Weight => CAIRO_FONT_WEIGHT_BOLD
                         ),
            Step      => Length / GDouble (Sectors),
            Height    => 0.036,
            Stretch   => 1.0,
            Color     => Text_Color,
            From      => First,
            Length    => Length,
            Mode      => Moved_Centered,
            Scaled    => True
         );
      Create_Foreground (Widget, Adjustment);
   end Initialize;

   procedure Initialize
             (  Widget : not null access
                         Gtk_Meter_Elliptic_90_Record'Class;
                Texts      : Controlled_String_List;
                Adjustment : Gtk_Adjustment;
                Sectors    : Positive
             )  is
   begin
      Initialize (Widget, Get_GList (Texts), Adjustment, Sectors);
   end Initialize;

   procedure Initialize
             (  Widget : not null access
                         Gtk_Meter_Elliptic_90_Record'Class;
                Texts      : UTF8_String;
                Delimiter  : Character;
                Adjustment : Gtk_Adjustment;
                Sectors    : Positive
             )  is
   begin
      Create_Background (Widget, Sectors);
      Widget.Annotation :=
         Add_Elliptic_Annotation
         (  Under   => Widget.Cache,
            Ellipse =>
               (  Center          => (0.0, 0.0 + Pin),
                  Major_Curvature => 1.0 / (Height * 2.0),
                  Minor_Radius    => Height * 1.055,
                  Angle           => 0.0
               ),
            Texts     => Texts,
            Delimiter => Delimiter,
            Face      => Create_Toy
                         (  Family => "lucida",
                            Slant  => CAIRO_FONT_SLANT_NORMAL,
                            Weight => CAIRO_FONT_WEIGHT_BOLD
                         ),
            Step      => Length / GDouble (Sectors),
            Height    => 0.036,
            Stretch   => 1.0,
            Color     => Text_Color,
            From      => First,
            Length    => Length,
            Mode      => Moved_Centered,
            Scaled    => True
         );
      Create_Foreground (Widget, Adjustment);
   end Initialize;

   procedure Set_Value
             (  Widget : not null access
                         Gtk_Meter_Elliptic_90_Record;
                Value  : GDouble
             )  is
   begin
      Widget.Needle.Set_Value (Value);
   end Set_Value;

   procedure Style_Changed
             (  Widget : not null access
                         Gtk_Meter_Elliptic_90_Record
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
      (  Height         => Widget.Background.Get_Height,
         Width          => Widget.Background.Get_Width,
         Center         => Widget.Background.Get_Center,
         Rotation_Angle => Widget.Background.Get_Rotation_Angle,
         Corner_Radius  => Widget.Background.Get_Corner_Radius,
         Border_Width   => Widget.Background.Get_Border_Width,
         Border_Depth   => Widget.Background.Get_Border_Depth,
         Border_Color   => Widget.Background.Get_Border_Color,
         Border_Shadow  => Widget.Background.Get_Border_Shadow,
         Lens_Reflex    => Widget.Background.Get_Lens_Reflex,
         Lens_Shadow    => Widget.Background.Get_Lens_Shadow,
         Color  =>
            Style_Get (Widget, "backgound-color", Background_Color)
      );
      Widget.Minor_Ticks.Set
      (  Inner  => Widget.Minor_Ticks.Get_Inner,
         Outer  => Widget.Minor_Ticks.Get_Outer,
         Ticks  => Widget.Minor_Ticks.Get_Ticks,
         From   => Widget.Minor_Ticks.Get_From,
         Length => Widget.Minor_Ticks.Get_Length,
         Line =>
            (  Widget.Minor_Ticks.Get_Line.Width,
               Style_Get (Widget, "minor-tick-color", Minor_Tick_Color),
               Style_Get (Widget, "minor-tick-line-cap")
      )     );
      Widget.Middle_Ticks.Set
      (  Inner  => Widget.Middle_Ticks.Get_Inner,
         Outer  => Widget.Middle_Ticks.Get_Outer,
         Ticks  => Widget.Middle_Ticks.Get_Ticks,
         From   => Widget.Middle_Ticks.Get_From,
         Length => Widget.Middle_Ticks.Get_Length,
         Line =>
            (  Widget.Middle_Ticks.Get_Line.Width,
               Style_Get (Widget, "middle-tick-color", Middle_Tick_Color),
               Style_Get (Widget, "middle-tick-line-cap")
      )     );
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

end Gtk.Meter.Elliptic_90;
