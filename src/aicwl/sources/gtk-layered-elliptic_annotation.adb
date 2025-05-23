--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Layered.Elliptic_Annotation             Luebeck            --
--  Implementation                                 Winter, 2010       --
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

with Cairo.Elementary_Functions;  use Cairo.Elementary_Functions;
with GLib.Properties.Creation;    use GLib.Properties.Creation;
with Gtk.Layered.Stream_IO;       use Gtk.Layered.Stream_IO;

with Ada.Unchecked_Deallocation;
with Cairo.Font_Slant_Property;
with Pango.Enums.Weight_Property;
with Gtk.Layered.Text_Transformation_Property;
with Pango.Cairo.Fonts.Font_Type_Property;

package body Gtk.Layered.Elliptic_Annotation is

   Eps : constant := 0.000_001;

   type Annotation_Ptr is access all Elliptic_Annotation_Layer;

   type Layer_Property is
        (  Property_Scaled,
           Property_Texts,
           Property_Markup,
           Property_Font_Type,
           Property_Family,
           Property_Slant,
           Property_Font_Size,
           Property_Weight,
           Property_Height,
           Property_Stretch,
           Property_Center_X,
           Property_Center_Y,
           Property_Curvature,
           Property_Radius,
           Property_Angle,
           Property_From,
           Property_Length,
           Property_Mode,
           Property_Tick_Step,
           Property_Tick_First,
           Property_Tick_Skipped,
           Property_Color
        );

   function "+" (Value : Annotation_Text_Ptr) return UTF8_String is
   begin
      if Value = null then
         return "";
      else
         return Value.Buffer (1..Value.Length);
      end if;
   end "+";

   function Where (Name : String) return String is
   begin
      return " in Gtk.Layered.Elliptic_Annotation." & Name;
   end Where;

   procedure Free is
      new Ada.Unchecked_Deallocation
          (  Elliptic_Annotation_Layer,
             Annotation_Ptr
          );
   procedure Free is
      new Ada.Unchecked_Deallocation
          (  Annotation_Text,
             Annotation_Text_Ptr
          );
   procedure Free is
      new Ada.Unchecked_Deallocation
          (  Annotation_List,
             Annotation_List_Ptr
          );

   function Get_List
            (  Texts  : Gtk.Enums.String_List.GList;
               Ticks  : Tick_Parameters;
               Markup : Boolean
            )  return Annotation_List_Ptr;

   function Get_List
            (  Texts     : UTF8_String;
               Delimiter : Character;
               Ticks     : Tick_Parameters;
               Markup    : Boolean
            )  return Annotation_List_Ptr;

   procedure Delete (List : in out Annotation_List_Ptr);

   function Add_Annotation_Implementation
            (  Under   : not null access Layer_Location'Class;
               Texts   : Annotation_List_Ptr;
               Height  : GDouble;
               Stretch : GDouble;
               Step    : GDouble;
               First   : Tick_Number;
               Skipped : Tick_Number;
               Ellipse : Ellipse_Parameters;
               From    : GDouble;
               Length  : GDouble;
               Face    : Pango_Cairo_Font;
               Mode    : Text_Transformation;
               Color   : Gdk_Color;
               Scaled  : Boolean
            )  return Annotation_Ptr is
      Ptr   : Annotation_Ptr := new Elliptic_Annotation_Layer;
      Layer : Elliptic_Annotation_Layer renames Ptr.all;
   begin
      Layer.Scaled := Scaled;
      Layer.Texts  := Texts;
      Add (Ptr, Under);
      Set
      (  Layer   => Layer,
         Ellipse => Ellipse,
         Ticks   => (Step, Get_First_Tick (First, Skipped), Skipped),
         From    => From,
         Length  => Length,
         Face    => Face,
         Mode    => Mode,
         Height  => Height,
         Stretch => Stretch,
         Color   => Color
      );
      return Ptr;
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add_Annotation_Implementation;

   procedure Add_Elliptic_Annotation
             (  Under   : not null access Layer_Location'Class;
                Texts   : Gtk.Enums.String_List.GList;
                Step    : GDouble;
                First   : Tick_Number        := Tick_Number'Last;
                Skipped : Tick_Number        := Tick_Number'Last;
                Ellipse : Ellipse_Parameters := Unit_Circle;
                From    : GDouble            := 0.0;
                Length  : GDouble            := 2.0 * Pi;
                Face    : Pango_Cairo_Font :=
                             Create_Toy
                             (  Family => "arial",
                                Slant  => CAIRO_FONT_SLANT_NORMAL,
                                Weight => CAIRO_FONT_WEIGHT_NORMAL
                             );
                Height  : GDouble             := 12.0;
                Stretch : GDouble             := 1.0;
                Mode    : Text_Transformation := Moved_Centered;
                Color   : Gdk_Color           := RGB (0.0, 0.0, 0.0);
                Markup  : Boolean             := False;
                Scaled  : Boolean             := False
             )  is
      Ptr : Annotation_Ptr;
   begin
      Ptr :=
         Add_Annotation_Implementation
         (  Under   => Under,
            Step    => Step,
            First   => First,
            Skipped => Skipped,
            Ellipse => Ellipse,
            From    => From,
            Length  => Length,
            Face    => Face,
            Height  => Height,
            Stretch => Stretch,
            Mode    => Mode,
            Color   => Color,
            Scaled  => Scaled,
            Texts   => Get_List (Texts, (Step, First, Skipped), Markup)
         );
   end Add_Elliptic_Annotation;

   function Add_Elliptic_Annotation
            (  Under   : not null access Layer_Location'Class;
               Texts   : Gtk.Enums.String_List.GList;
               Step    : GDouble;
               First   : Tick_Number        := Tick_Number'Last;
               Skipped : Tick_Number        := Tick_Number'Last;
               Ellipse : Ellipse_Parameters := Unit_Circle;
               From    : GDouble            := 0.0;
               Length  : GDouble            := 2.0 * Pi;
               Face    : Pango_Cairo_Font :=
                            Create_Toy
                            (  Family => "arial",
                               Slant  => CAIRO_FONT_SLANT_NORMAL,
                               Weight => CAIRO_FONT_WEIGHT_NORMAL
                            );
               Height  : GDouble             := 12.0;
               Stretch : GDouble             := 1.0;
               Mode    : Text_Transformation := Moved_Centered;
               Color   : Gdk_Color           := RGB (0.0, 0.0, 0.0);
               Markup  : Boolean             := False;
               Scaled  : Boolean             := False
            )  return not null access Elliptic_Annotation_Layer is
      Ptr : Annotation_Ptr;
   begin
      Ptr :=
         Add_Annotation_Implementation
         (  Under   => Under,
            Step    => Step,
            First   => First,
            Skipped => Skipped,
            Ellipse => Ellipse,
            From    => From,
            Length  => Length,
            Face    => Face,
            Height  => Height,
            Stretch => Stretch,
            Mode    => Mode,
            Color   => Color,
            Scaled  => Scaled,
            Texts   => Get_List (Texts, (Step, First, Skipped), Markup)
         );
      return Ptr.all'Unchecked_Access;
   end Add_Elliptic_Annotation;

   procedure Add_Elliptic_Annotation
             (  Under   : not null access Layer_Location'Class;
                Texts   : Controlled_String_List;
                Step    : GDouble;
                First   : Tick_Number        := Tick_Number'Last;
                Skipped : Tick_Number        := Tick_Number'Last;
                Ellipse : Ellipse_Parameters := Unit_Circle;
                From    : GDouble            := 0.0;
                Length  : GDouble            := 2.0 * Pi;
                Face    : Pango_Cairo_Font :=
                             Create_Toy
                             (  Family => "arial",
                                Slant  => CAIRO_FONT_SLANT_NORMAL,
                                Weight => CAIRO_FONT_WEIGHT_NORMAL
                             );
                Height  : GDouble             := 12.0;
                Stretch : GDouble             := 1.0;
                Mode    : Text_Transformation := Moved_Centered;
                Color   : Gdk_Color           := RGB (0.0, 0.0, 0.0);
                Markup  : Boolean             := False;
                Scaled  : Boolean             := False
             )  is
   begin
      Add_Elliptic_Annotation
      (  Under   => Under,
         Texts   => Get_GList (Texts),
         Step    => Step,
         First   => First,
         Skipped => Skipped,
         Ellipse => Ellipse,
         From    => From,
         Length  => Length,
         Face    => Face,
         Height  => Height,
         Stretch => Stretch,
         Mode    => Mode,
         Color   => Color,
         Markup  => Markup,
         Scaled  => Scaled
      );
   end Add_Elliptic_Annotation;

   function Add_Elliptic_Annotation
            (  Under   : not null access Layer_Location'Class;
               Texts   : Controlled_String_List;
               Step    : GDouble;
               First   : Tick_Number        := Tick_Number'Last;
               Skipped : Tick_Number        := Tick_Number'Last;
               Ellipse : Ellipse_Parameters := Unit_Circle;
               From    : GDouble            := 0.0;
               Length  : GDouble            := 2.0 * Pi;
               Face    : Pango_Cairo_Font :=
                            Create_Toy
                            (  Family => "arial",
                               Slant  => CAIRO_FONT_SLANT_NORMAL,
                               Weight => CAIRO_FONT_WEIGHT_NORMAL
                            );
               Height  : GDouble             := 12.0;
               Stretch : GDouble             := 1.0;
               Mode    : Text_Transformation := Moved_Centered;
               Color   : Gdk_Color           := RGB (0.0, 0.0, 0.0);
               Markup  : Boolean             := False;
               Scaled  : Boolean             := False
            )  return not null access Elliptic_Annotation_Layer is
   begin
      return
         Add_Elliptic_Annotation
         (  Under   => Under,
            Texts   => Get_GList (Texts),
            Step    => Step,
            First   => First,
            Skipped => Skipped,
            Ellipse => Ellipse,
            From    => From,
            Length  => Length,
            Face    => Face,
            Height  => Height,
            Stretch => Stretch,
            Mode    => Mode,
            Color   => Color,
            Markup  => Markup,
            Scaled  => Scaled
         );
   end Add_Elliptic_Annotation;

   procedure Add_Elliptic_Annotation
             (  Under     : not null access Layer_Location'Class;
                Texts     : UTF8_String;
                Step      : GDouble;
                First     : Tick_Number        := Tick_Number'Last;
                Skipped   : Tick_Number        := Tick_Number'Last;
                Ellipse   : Ellipse_Parameters := Unit_Circle;
                From      : GDouble            := 0.0;
                Length    : GDouble            := 2.0 * Pi;
                Face      : Pango_Cairo_Font :=
                               Create_Toy
                               (  Family => "arial",
                                  Slant  => CAIRO_FONT_SLANT_NORMAL,
                                  Weight => CAIRO_FONT_WEIGHT_NORMAL
                               );
                Height    : GDouble             := 12.0;
                Stretch   : GDouble             := 1.0;
                Mode      : Text_Transformation := Moved_Centered;
                Color     : Gdk_Color           := RGB (0.0, 0.0, 0.0);
                Delimiter : Character           := ' ';
                Markup    : Boolean             := False;
                Scaled    : Boolean             := False
             )  is
      Ptr : Annotation_Ptr;
   begin
      Ptr :=
         Add_Annotation_Implementation
         (  Under   => Under,
            Step    => Step,
            First   => First,
            Skipped => Skipped,
            Ellipse => Ellipse,
            From    => From,
            Length  => Length,
            Face    => Face,
            Height  => Height,
            Stretch => Stretch,
            Mode    => Mode,
            Color   => Color,
            Scaled  => Scaled,
            Texts   => Get_List
                       (  Texts,
                          Delimiter,
                          (Step, First, Skipped),
                          Markup
         )             );
   end Add_Elliptic_Annotation;

   function Add_Elliptic_Annotation
            (  Under     : not null access Layer_Location'Class;
               Texts     : UTF8_String;
               Step      : GDouble;
               First     : Tick_Number        := Tick_Number'Last;
               Skipped   : Tick_Number        := Tick_Number'Last;
               Ellipse   : Ellipse_Parameters := Unit_Circle;
               From      : GDouble            := 0.0;
               Length    : GDouble            := 2.0 * Pi;
               Face      : Pango_Cairo_Font :=
                              Create_Toy
                              (  Family => "arial",
                                 Slant  => CAIRO_FONT_SLANT_NORMAL,
                                 Weight => CAIRO_FONT_WEIGHT_NORMAL
                              );
               Height    : GDouble             := 12.0;
               Stretch   : GDouble             := 1.0;
               Mode      : Text_Transformation := Moved_Centered;
               Color     : Gdk_Color           := RGB (0.0, 0.0, 0.0);
               Delimiter : Character           := ' ';
               Markup    : Boolean             := False;
               Scaled    : Boolean             := False
            )  return not null access Elliptic_Annotation_Layer is
      Ptr : Annotation_Ptr;
   begin
      Ptr :=
         Add_Annotation_Implementation
         (  Under   => Under,
            Step    => Step,
            First   => First,
            Skipped => Skipped,
            Ellipse => Ellipse,
            From    => From,
            Length  => Length,
            Face    => Face,
            Height  => Height,
            Stretch => Stretch,
            Mode    => Mode,
            Color   => Color,
            Scaled  => Scaled,
            Texts   => Get_List
                       (  Texts,
                          Delimiter,
                          (Step, First, Skipped),
                          Markup
         )             );
      return Ptr.all'Unchecked_Access;
   end Add_Elliptic_Annotation;

   function Add
            (  Under  : not null access Layer_Location'Class;
               Stream : not null access Root_Stream_Type'Class
            )  return not null access Elliptic_Annotation_Layer is
      Ptr : Annotation_Ptr := new Elliptic_Annotation_Layer;
   begin
      Restore (Stream.all, Ptr.all);
      Add (Ptr, Under);
      return Ptr;
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add;

   procedure Delete (List : in out Annotation_List_Ptr) is
   begin
      if List /= null then
         for Index in List'Range loop
            Free (List (Index));
         end loop;
         Free (List);
      end if;
   end Delete;

   procedure Draw
             (  Layer   : in out Elliptic_Annotation_Layer;
                Context : Cairo_Context;
                Area    : Gdk_Rectangle
             )  is
      Angle   : GDouble;
      Gain    : GDouble;
      Thick   : Natural := Layer.Ticks.First;
      Ellipse : Ellipse_Parameters;
      Extents : Cairo_Text_Extents;
      Point   : Cairo_Tuple;
      Length  : constant GDouble := abs Layer.Length +
                                    abs Layer.Ticks.Step * 0.05;
   begin
      if Layer.Scaled then
         Ellipse :=
            (  Layer.Widget.Get_Size * Layer.Ellipse
            +  Layer.Widget.Get_Center
            );
      else
         Ellipse := Layer.Ellipse;
      end if;
      Set_Source_RGB
      (  Context,
         GDouble (Red   (Layer.Color)) / GDouble (Guint16'Last),
         GDouble (Green (Layer.Color)) / GDouble (Guint16'Last),
         GDouble (Blue  (Layer.Color)) / GDouble (Guint16'Last)
      );
      for Index in Natural'Range loop
         Angle := Layer.Ticks.Step * GDouble (Index);
         exit when Angle > Length;
         if Layer.Length < 0.0 then
            Angle := -Angle;
         end if;
         if Thick = Layer.Ticks.Skipped then
            Thick := 1;
         else
            Thick := Thick + 1;
            Angle := Layer.From + Angle;
            Point := Get_Point (Ellipse, Ellipse * Angle);
            exit when
                 (  Layer.Texts = null
                 or else
                    Index >= Layer.Texts'Length
                 or else
                    Layer.Texts (Index + 1) = null
                 or else
                    Layer.Texts (Index + 1).Length = 0
                 );
            if Layer.Texts (Index + 1).Markup then
               Get_Markup_Extents
               (  Layer.Face,
                  Context,
                  +Layer.Texts (Index + 1),
                  Extents
               );
            else
               Get_Text_Extents
               (  Layer.Face,
                  Context,
                  +Layer.Texts (Index + 1),
                  Extents
               );
            end if;
            declare
               State : Context_State := Save (Context);
            begin
               if Extents.Height > 0.0 then
                  Gain := Layer.Height / Extents.Height;
                  if Layer.Scaled then
                     Gain := Gain * Layer.Widget.Get_Size;
                  end if;
                  Translate
                  (  Cr => Context,
                     Tx => Point.X,
                     Ty => Point.Y
                  );
                  case Layer.Mode is
                     when Moved_Inside =>
                        Translate
                        (  Cr => Context,
                           Tx => ( -Extents.Width
                                 *  0.5 * Gain * Layer.Stretch
                                 *  cos (Angle)
                                 ),
                           Ty => ( -Extents.Height
                                 *  0.5 * Gain
                                 *  sin (Angle)
                        )        );
                     when Moved_Outside =>
                        Translate
                        (  Cr => Context,
                           Tx => (  Extents.Width
                                 *  0.5 * Gain * Layer.Stretch
                                 *  cos (Angle)
                                 ),
                           Ty => (  Extents.Height
                                 *  0.5 * Gain
                                 *  sin (Angle)
                        )        );
                     when Moved_Centered =>
                        null;
                     when Rotated =>
                        if abs Angle < Eps then
                           --
                           -- There seems to be a bug in rotating around
                           -- half Pi.  This replaces half Pi with a bit
                           -- greater angle.
                           --
                           Rotate (Context, Pi / 2.0 + Eps);
                        else
                           Rotate (Context, Angle + Pi / 2.0);
                        end if;
                     when Skewed =>
                        Rotate (Context, Ellipse.Angle);
                        declare
                           Matrix : aliased Cairo_Matrix;
                        begin
                           Matrix.XX := 1.0;
                           Matrix.XY := 1.0;
                           Matrix.XY :=
                              tan (Ellipse.Angle - Angle - Pi / 2.0);
                           Matrix.X0 := 0.0;
                           Matrix.YX := 0.0;
                           Matrix.YY := 1.0;
                           Matrix.Y0 := 0.0;
                           Transform (Context, Matrix'Access);
                        end;
                  end case;
                  Scale
                  (  Cr => Context,
                     Sx => Gain * Layer.Stretch,
                     Sy => Gain
                  );
                  Move_To
                  (  Cr => Context,
                     X  => -(Extents.X_Bearing + Extents.Width  * 0.5),
                     Y  => -(Extents.Y_Bearing + Extents.Height * 0.5)
                  );
                  if Layer.Texts (Index + 1).Markup then
                     Show_Markup
                     (  Layer.Face,
                        Context,
                       +Layer.Texts (Index + 1)
                     );
                  else
                     Show_Text
                     (  Layer.Face,
                        Context,
                       +Layer.Texts (Index + 1)
                     );
                  end if;
               end if;
            end;
         end if;
      end loop;
      Layer.Updated := False;
   end Draw;

   procedure Finalize (Layer : in out Elliptic_Annotation_Layer) is
   begin
      Finalize (Abstract_Layer (Layer));
      Delete (Layer.Texts);
   end Finalize;

   function Get_Color (Layer : Elliptic_Annotation_Layer)
      return Gdk_Color is
   begin
      return Layer.Color;
   end Get_Color;

   function Get_Ellipse (Layer : Elliptic_Annotation_Layer)
      return Ellipse_Parameters is
   begin
      return Layer.Ellipse;
   end Get_Ellipse;

   function Get_Face (Layer : Elliptic_Annotation_Layer)
      return Pango_Cairo_Font is
   begin
      return Layer.Face;
   end Get_Face;

   function Get_From (Layer : Elliptic_Annotation_Layer)
      return GDouble is
   begin
      return Layer.From;
   end Get_From;

   function Get_Height (Layer : Elliptic_Annotation_Layer)
      return GDouble is
   begin
      return Layer.Height;
   end Get_Height;

   function Get_Length (Layer : Elliptic_Annotation_Layer)
      return GDouble is
   begin
      return Layer.Length;
   end Get_Length;

   function Get_List
            (  Texts  : Gtk.Enums.String_List.GList;
               Ticks  : Tick_Parameters;
               Markup : Boolean
            )  return Annotation_List_Ptr is
      use Gtk.Enums.String_List;
      This  : GList   := Texts;
      Count : Natural := 0;
   begin
      while This /= Null_List loop
         Count := Count + 1;
         This  := Next (This);
      end loop;
      declare
         Result : constant Annotation_List_Ptr :=
                  new Annotation_List (1..Count);
         List   : Annotation_List renames Result.all;
      begin
         This := Texts;
         for Index in List'Range loop
            declare
               Text : constant UTF8_String := Get_Data (This);
            begin
               List (Index) :=
                  new Annotation_Text'
                      (  Size   => Text'Length,
                         Markup => Markup,
                         Length => Text'Length,
                         Buffer => Text
                      );
               This := Next (This);
            end;
         end loop;
         return Result;
      end;
   end Get_List;

   function Get_List
            (  Texts     : UTF8_String;
               Delimiter : Character;
               Ticks     : Tick_Parameters;
               Markup    : Boolean
            )  return Annotation_List_Ptr is
      Count : Natural := 1;
   begin
      for Index in Texts'Range loop
         if Texts (Index) = Delimiter then
            Count := Count + 1;
         end if;
      end loop;
      declare
         Result : constant Annotation_List_Ptr :=
                  new Annotation_List (1..Count);
         List   : Annotation_List renames Result.all;
         Start  : Integer := Texts'First;
         Stop   : Integer;
      begin
         for Index in List'Range loop
            Stop := Start;
            while Stop <= Texts'Last and then Texts (Stop) /= Delimiter
            loop
               Stop := Stop + 1;
            end loop;
            List (Index) :=
               new Annotation_Text'
                   (  Size   => Stop - Start,
                      Length => Stop - Start,
                      Markup => Markup,
                      Buffer => Texts (Start..Stop - 1)
                   );
            Start := Stop + 1;
         end loop;
         return Result;
      end;
   end Get_List;

   function Get_Markup
            (  Layer    : Elliptic_Annotation_Layer;
               Position : Positive
            )  return Boolean is
   begin
      if Layer.Texts = null or else Position > Layer.Texts'Last then
         raise Constraint_Error with "No such text";
      elsif Layer.Texts (Position) = null then
         return False;
      else
         return Layer.Texts (Position).Markup;
      end if;
   end Get_Markup;

   function Get_Mode (Layer : Elliptic_Annotation_Layer)
      return Text_Transformation is
   begin
      return Layer.Mode;
   end Get_Mode;

   function Get_Properties_Number
            (  Layer : Elliptic_Annotation_Layer
            )  return Natural is
   begin
      return
      (  Layer_Property'Pos (Layer_Property'Last)
      -  Layer_Property'Pos (Layer_Property'First)
      +  1
      );
   end Get_Properties_Number;

   function Get_Property_Specification
            (  Layer    : Elliptic_Annotation_Layer;
               Property : Positive
            )  return Param_Spec is
   begin
      if Property > Get_Properties_Number (Layer) then
         raise Constraint_Error;
      else
         case Layer_Property'Val (Property - 1) is
            when Property_Center_X =>
               return
                  Gnew_Double
                  (  Name    => "x",
                     Nick    => "x",
                     Minimum => GDouble'First,
                     Maximum => GDouble'Last,
                     Default => 0.0,
                     Blurb   => "The x-coordinate of the ellipse's " &
                                "center. The annotation's texts are " &
                                "arranged relatively to the ellipse"
                  );
            when Property_Center_Y =>
               return
                  Gnew_Double
                  (  Name    => "y",
                     Nick    => "y",
                     Minimum => GDouble'First,
                     Maximum => GDouble'Last,
                     Default => 0.0,
                     Blurb   => "The y-coordinate of the ellipse's " &
                                "center. The annotation's texts are " &
                                "arranged relatively to the ellipse"
                  );
            when Property_Curvature =>
               return
                  Gnew_Double
                  (  Name    => "k",
                     Nick    => "k",
                     Minimum => 0.0,
                     Maximum => GDouble'Last,
                     Default => 0.0,
                     Blurb   => "The curvature of the ellipse's " &
                                "major axis. The " &
                                "annotation's texts are " &
                                "arranged relatively to the ellipse"
                  );
            when Property_Radius =>
               return
                  Gnew_Double
                  (  Name    => "r",
                     Nick    => "r",
                     Minimum => 1.0E-6,
                     Maximum => GDouble'Last,
                     Default => 0.5,
                     Blurb   => "The radius of the ellipse " &
                                "minor axis. The " &
                                "annotation's texts are " &
                                "arranged relatively to the ellipse"
                  );
            when Property_Angle =>
               return
                  Gnew_Double
                  (  Name    => "angle",
                     Nick    => "angle",
                     Minimum =>-2.0 * Pi,
                     Maximum => 2.0 * Pi,
                     Default => 0.0,
                     Blurb   => "The angle of the major ellipse's " &
                                "axis. The " &
                                "annotation's texts are " &
                                "arranged relatively to the ellipse"
                  );
            when Property_From =>
               return
                  Gnew_Double
                  (  Name    => "from",
                     Nick    => "from",
                     Minimum =>-2.0 * Pi,
                     Maximum => 2.0 * Pi,
                     Default => 0.0,
                     Blurb   => "The angle of the first " &
                                "annotation text on the " &
                                "arc of the ellipse used to arrange " &
                                "the annotation texts"
                  );
            when Property_Length =>
               return
                  Gnew_Double
                  (  Name    => "length",
                     Nick    => "length",
                     Minimum =>-2.0 * Pi,
                     Maximum => 2.0 * Pi,
                     Default => 0.0,
                     Blurb   => "The angular length of the arc " &
                                "of the ellipse used to arrange " &
                                "the annotation texts"
                  );
            when Property_Stretch =>
               return
                  Gnew_Double
                  (  Name    => "stretch",
                     Nick    => "stretch",
                     Minimum => 0.0,
                     Maximum => GDouble'Last,
                     Default => 1.0,
                     Blurb   => "The relation of the rendered width " &
                                "of an annotation text to its " &
                                "original width. The stretch value " &
                                "1 keeps texts unchanged"
                  );
            when Property_Height =>
               return
                  Gnew_Double
                  (  Name    => "height",
                     Nick    => "height",
                     Minimum => 0.0,
                     Maximum => GDouble'Last,
                     Default => 12.0,
                     Blurb   => "The annotation text font height"
                  );
            when Property_Font_Type =>
               return
                  Pango.Cairo.Fonts.Font_Type_Property.Gnew_Enum
                  (  Name    => "font-type",
                     Nick    => "font type",
                     Default => Pango_Font,
                     Blurb   => "The backend used for the font, " &
                                "e.g. toy font, pango font"
                  );
            when Property_Family =>
               return
                  Gnew_String
                  (  Name    => "font-familiy",
                     Nick    => "font famility",
                     Default => "arial",
                     Blurb   => "The annotation text font family, " &
                                "e.g. courier"
                  );
            when Property_Mode =>
               return
                  Gtk.Layered.Text_Transformation_Property.Gnew_Enum
                  (  Name    => "text-transformation-mode",
                     Nick    => "text transformation mode",
                     Default => Moved_Centered,
                     Blurb   => "The method how annotation texts are " &
                                "transformed and aligned to the " &
                                "scale ticks"
                  );
            when Property_Slant =>
               return
                  Cairo.Font_Slant_Property.Gnew_Enum
                  (  Name    => "font-slant",
                     Nick    => "font slant",
                     Default => CAIRO_FONT_SLANT_NORMAL,
                     Blurb   => "The annotation text font slant"
                  );
            when Property_Font_Size =>
               return
                  Gnew_UInt
                  (  Name    => "font-size",
                     Nick    => "font size",
                     Minimum => 1,
                     Maximum => GUInt (GInt'Last),
                     Default => 12,
                     Blurb   => "The font size in points. " &
                                "The value is only relevant for " &
                                "pango fonts. For cairo toy size " &
                                "is ignored"
                  );
            when Property_Weight =>
               return
                  Pango.Enums.Weight_Property.Gnew_Enum
                  (  Name    => "font-weight",
                     Nick    => "font weight",
                     Default => Pango.Enums.Pango_Weight_Normal,
                     Blurb   => "The annotation text font weight"
                  );
            when Property_Texts =>
               return
                  Gnew_String
                  (  Name    => "texts",
                     Nick    => "annotation texts",
                     Default => "",
                     Blurb   => "The list of annotation texts, " &
                                "separated by LFs"
                  );
            when Property_Markup =>
               return
                  Gnew_String
                  (  Name    => "markup-flags",
                     Nick    => "annotation text markups",
                     Default => "",
                     Blurb   => "The list of annotation markup " &
                                "text flags. " &
                                "For each text it contains one " &
                                "character, which is " &
                                "T for plain text or " &
                                "M for markup"
                  );
            when Property_Color =>
               return
                  Gnew_Boxed
                  (  Name       => "color",
                     Boxed_Type => Gdk_Color_Type,
                     Nick       => "color",
                     Blurb      => "The annotation texts color"
                  );
            when Property_Tick_Step =>
               return
                  Gnew_Double
                  (  Name    => "step",
                     Nick    => "step",
                     Minimum => 1.0E-6,
                     Maximum => 2.0 * Pi,
                     Default => Pi / 12.0,
                     Blurb   => "The angular distance between two " &
                                "consequent scale ticks at where " &
                                "annotation texts are drawn"
                  );
            when Property_Tick_First =>
               return
                  Gnew_UInt
                  (  Name    => "first-tick",
                     Nick    => "first tick",
                     Minimum => GUInt (Tick_Number'First),
                     Maximum => GUInt (Tick_Number'Last),
                     Default => 1,
                     Blurb   => "The number of the first tick. " &
                                "The first tick is located at " &
                                "the beginning of the scale to which " &
                                "annotation texts are attached"
                  );
            when Property_Tick_Skipped =>
               return
                  Gnew_UInt
                  (  Name    => "skipped-tick",
                     Nick    => "skipped tick",
                     Minimum => 2,
                     Maximum => GUInt (Tick_Number'Last),
                     Default => GUInt (Tick_Number'Last),
                     Blurb   => "The number of the skipped tick. " &
                                "The ticks are numbered from 1 to " &
                                "skipped-tick. For the ticks with " &
                                "this number annotations are not drawn"
                  );
            when Property_Scaled =>
               return
                  Gnew_Boolean
                  (  Name    => "scaled",
                     Nick    => "scaled",
                     Default => False,
                     Blurb   => "The annotation size is changed when " &
                                "the widget is resized"
                  );
         end case;
      end if;
   end Get_Property_Specification;

   function Get_Property_Value
            (  Layer    : Elliptic_Annotation_Layer;
               Property : Positive
            )  return GValue is
   begin
      if Property > Get_Properties_Number (Layer) then
         raise Constraint_Error;
      else
         declare
            Value : GValue;
         begin
            case Layer_Property'Val (Property - 1) is
               when Property_Center_X =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.Ellipse.Center.X);
               when Property_Center_Y =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.Ellipse.Center.Y);
               when Property_Curvature =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.Ellipse.Major_Curvature);
               when Property_Radius =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.Ellipse.Minor_Radius);
               when Property_Angle =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.Ellipse.Angle);
               when Property_From =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.From);
               when Property_Length =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.Length);
               when Property_Color =>
                  Set_Value (Value, Layer.Color);
               when Property_Height =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.Height);
               when Property_Mode =>
                  Gtk.Layered.Text_Transformation_Property.Set_Enum
                  (  Value,
                     Layer.Mode
                  );
               when Property_Font_Type =>
                  Pango.Cairo.Fonts.Font_Type_Property.Set_Enum
                  (  Value,
                     Get_Type (Layer.Face)
                  );
               when Property_Family =>
                  Init (Value, GType_String);
                  Set_String (Value, Get_Family (Layer.Face));
               when Property_Slant =>
                  Cairo.Font_Slant_Property.Set_Enum
                  (  Value,
                     Get_Slant (Layer.Face)
                  );
               when Property_Font_Size =>
                  Init (Value, GType_UInt);
                  Set_UInt (Value, GUInt (Get_Size (Layer.Face)));
               when Property_Weight =>
                  Pango.Enums.Weight_Property.Set_Enum
                  (  Value,
                     Get_Weight (Layer.Face)
                  );
               when Property_Stretch =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.Stretch);
               when Property_Texts =>
                  Init (Value, GType_String);
                  if Layer.Texts = null then
                     Set_String (Value, "");
                  else
                     declare
                        Length : Natural := 0;
                        List   : Annotation_List renames
                                    Layer.Texts.all;
                     begin
                        for Index in List'Range loop
                           if Index > List'First then
                              Length := Length + 1;
                           end if;
                           Length := Length + List (Index).Length;
                        end loop;
                        declare
                           Text    : String (1..Length);
                           Pointer : Integer := Text'First;
                        begin
                           for Index in List'Range loop
                              if Index > List'First then
                                 Text (Pointer) := Character'Val (10);
                                 Pointer := Pointer + 1;
                              end if;
                              Text
                              (  Pointer
                              .. Pointer + List (Index).Length - 1
                              )  := +List (Index);
                              Pointer := Pointer + List (Index).Length;
                           end loop;
                           Set_String (Value, Text);
                        end;
                     end;
                  end if;
               when Property_Markup =>
                  Init (Value, GType_String);
                  if Layer.Texts = null then
                     Set_String (Value, "");
                  else
                     declare
                        List : Annotation_List renames Layer.Texts.all;
                        Text : String (List'Range);
                     begin
                        for Index in Text'Range loop
                           if List (Index).Markup then
                              Text (Index) := 'M';
                           else
                              Text (Index) := 'T';
                           end if;
                        end loop;
                        Set_String (Value, Text);
                     end;
                  end if;
               when Property_Tick_Step =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.Ticks.Step);
               when Property_Tick_First =>
                  Init (Value, GType_UInt);
                  Set_UInt (Value, GUInt (Layer.Ticks.First));
               when Property_Tick_Skipped =>
                  Init (Value, GType_UInt);
                  Set_UInt (Value, GUInt (Layer.Ticks.Skipped));
               when Property_Scaled =>
                  Init (Value, GType_Boolean);
                  Set_Boolean (Value, Layer.Scaled);
            end case;
            return Value;
         end;
      end if;
   end Get_Property_Value;

   function Get_Scaled (Layer : Elliptic_Annotation_Layer)
      return Boolean is
   begin
      return Layer.Scaled;
   end Get_Scaled;

   function Get_Stretch (Layer : Elliptic_Annotation_Layer)
      return GDouble is
   begin
      return Layer.Stretch;
   end Get_Stretch;

   function Get_Text
            (  Layer    : Elliptic_Annotation_Layer;
               Position : Positive
            )  return UTF8_String is
   begin
      if Layer.Texts = null or else Position > Layer.Texts'Last then
         raise Constraint_Error with "No such text";
      elsif Layer.Texts (Position) = null then
         return "";
      else
         return +Layer.Texts (Position);
      end if;
   end Get_Text;

   function Get_Texts_Number (Layer : Elliptic_Annotation_Layer)
      return Natural is
   begin
      if Layer.Texts = null then
         return 0;
      else
         return Layer.Texts'Length;
      end if;
   end Get_Texts_Number;

   function Get_Ticks (Layer : Elliptic_Annotation_Layer)
      return Tick_Parameters is
   begin
      return Layer.Ticks;
   end Get_Ticks;

   function Is_Updated (Layer : Elliptic_Annotation_Layer)
      return Boolean is
   begin
      return Layer.Updated;
   end Is_Updated;

   procedure Move
             (  Layer  : in out Elliptic_Annotation_Layer;
                Offset : Cairo_Tuple
             )  is
   begin
      Layer.Ellipse := Layer.Ellipse + Offset;
      Layer.Updated := True;
   end Move;

   procedure Restore
             (  Stream : in out Root_Stream_Type'Class;
                Layer  : in out Elliptic_Annotation_Layer
             )  is
      Ellipse : Ellipse_Parameters;
      Face    : Pango_Cairo_Font;
      Height  : GDouble;
      Stretch : GDouble;
      From    : GDouble;
      Length  : GDouble;
      Mode    : Text_Transformation;
      Ticks   : Tick_Parameters;
      Color   : Gdk_Color;
   begin
      Restore (Stream, Ellipse);
      Restore (Stream, Face);
      Restore (Stream, Height);
      Restore (Stream, Stretch);
      Restore (Stream, From);
      Restore (Stream, Length);
      Restore (Stream, Mode);
      Restore (Stream, Ticks);
      Restore (Stream, Color);
      Restore (Stream, Layer.Scaled);
      Set
      (  Layer   => Layer,
         Ellipse => Ellipse,
         Ticks   => Ticks,
         From    => From,
         Length  => Length,
         Face    => Face,
         Mode    => Mode,
         Height  => Height,
         Stretch => Stretch,
         Color   => Color
      );
      declare
         use Gtk.Layered.Stream_IO;
         Markup : constant Bit_Array := Restore (Stream'Access);
      begin
         Free (Layer.Texts);
         Layer.Texts := new Annotation_List (Markup'Range);
         for Index in Markup'Range loop
            Layer.Set_Text
            (  Index,
               Restore (Stream'Access),
               Markup (Index)
            );
         end loop;
      end;
   end Restore;

   procedure Scale
             (  Layer  : in out Elliptic_Annotation_Layer;
                Factor : GDouble
             )  is
   begin
      Set
      (  Layer   => Layer,
         Ellipse => Layer.Ellipse * Factor,
         Ticks   => Layer.Ticks,
         From    => Layer.From,
         Length  => Layer.Length,
         Face    => Layer.Face,
         Mode    => Layer.Mode,
         Height  => Layer.Height * Factor,
         Stretch => Layer.Stretch,
         Color   => Layer.Color
      );
   end Scale;

   procedure Set
             (  Layer   : in out Elliptic_Annotation_Layer;
                Ellipse : Ellipse_Parameters;
                Ticks   : Tick_Parameters;
                From    : GDouble;
                Length  : GDouble;
                Face    : Pango_Cairo_Font;
                Mode    : Text_Transformation;
                Height  : GDouble;
                Stretch : GDouble;
                Color   : Gdk_Color
             )  is
   begin
      if Ticks.Step < Min_Step then
         raise Constraint_Error with "Step is too small";
      elsif Ticks.First > Ticks.Skipped then
         raise Constraint_Error with
            "First tick is greater than the skipped tick";
      elsif Ellipse.Minor_Radius <= 0.0 then
         raise Constraint_Error with "Non-positive ellipse radius";
      elsif Ellipse.Major_Curvature < 0.0 then
         raise Constraint_Error with "Negative ellipse curvature";
      elsif Height <= 0.0 then
         raise Constraint_Error with "Non-positive height";
      elsif Stretch <= 0.0 then
         raise Constraint_Error with "Non-positive stretch";
      end if;
      Layer.Ellipse := Ellipse;
      Layer.Ticks   := Ticks;
      Layer.From    := From;
      Layer.Length  := Length;
      Layer.Face    := Face;
      Layer.Mode    := Mode;
      Layer.Height  := Height;
      Layer.Stretch := Stretch;
      Layer.Color   := Color;
      Layer.Updated := True;
   end Set;

   procedure Set_Face
             (  Layer : in out Elliptic_Annotation_Layer;
                Face  : Pango_Cairo_Font
             )  is
   begin
      Layer.Face    := Face;
      Layer.Updated := True;
   end Set_Face;

   procedure Set_Property_Value
             (  Layer    : in out Elliptic_Annotation_Layer;
                Property : Positive;
                Value    : GValue
             )  is
   begin
      if Property > Get_Properties_Number (Layer) then
         raise Constraint_Error;
      else
         case Layer_Property'Val (Property - 1) is
            when Property_Center_X =>
               Layer.Ellipse.Center.X := Get_Double (Value);
            when Property_Center_Y =>
               Layer.Ellipse.Center.Y := Get_Double (Value);
            when Property_Curvature =>
               Layer.Ellipse.Major_Curvature := Get_Double (Value);
               if Layer.Ellipse.Major_Curvature < 0.0 then
                  Layer.Ellipse.Major_Curvature := 0.0;
               end if;
            when Property_Radius =>
               Layer.Ellipse.Minor_Radius := Get_Double (Value);
               if Layer.Ellipse.Minor_Radius < 1.0E-6 then
                  Layer.Ellipse.Minor_Radius := 1.0E-6;
               end if;
            when Property_Angle =>
               Layer.Ellipse.Angle := Get_Double (Value);
               if Layer.Ellipse.Angle not in -2.0 * Pi..2.0 * Pi then
                  Layer.Ellipse.Angle :=
                     GDouble'Remainder (Layer.Ellipse.Angle, 2.0 * Pi);
               end if;
            when Property_From =>
               Layer.From := Get_Double (Value);
               if Layer.From not in -2.0 * Pi..2.0 * Pi then
                  Layer.From := GDouble'Remainder (Layer.From, 2.0 * Pi);
               end if;
            when Property_Length =>
               Layer.Length := Get_Double (Value);
               if Layer.Length not in -2.0 * Pi..2.0 * Pi then
                  Layer.Length :=
                     GDouble'Remainder (Layer.Length, 2.0 * Pi);
               end if;
            when Property_Stretch =>
               Layer.Stretch := Get_Double (Value);
               if Layer.Stretch < 0.0 then
                  Layer.Stretch := 0.0;
               end if;
            when Property_Height =>
               Layer.Height := Get_Double (Value);
               if Layer.Height < 0.0 then
                  Layer.Height := 0.0;
               end if;
            when Property_Mode =>
               Layer.Mode :=
                  Gtk.Layered.Text_Transformation_Property.Get_Enum
                  (  Value
                  );
            when Property_Font_Type =>
               Set_Type
               (  Layer.Face,
                  Pango.Cairo.Fonts.Font_Type_Property.Get_Enum (Value)
               );
            when Property_Family =>
               Set_Family (Layer.Face, Get_String (Value));
            when Property_Slant =>
               Set_Slant
               (  Layer.Face,
                  Cairo.Font_Slant_Property.Get_Enum (Value)
               );
            when Property_Font_Size =>
               Set_Size
               (  Layer.Face,
                  GInt
                  (  GUInt'Max
                     (  GUInt'Min
                        (  Get_UInt (Value),
                           GUInt (GInt'Last)
                        ),
                        1
               )  )  );
            when Property_Weight =>
               Set_Weight
               (  Layer.Face,
                  Pango.Enums.Weight_Property.Get_Enum (Value)
               );
            when Property_Tick_Step =>
               Layer.Ticks.Step := Get_Double (Value);
               if Layer.Ticks.Step < 1.0E-6 then
                  Layer.Ticks.Step := 1.0E-6;
               end if;
            when Property_Tick_First =>
               if Get_UInt (Value) < 1 then
                  Layer.Ticks.First := 1;
               elsif Get_UInt (Value) > GUInt (Tick_Number'Last) then
                  Layer.Ticks.First := Tick_Number'Last;
               else
                  Layer.Ticks.First := Tick_Number (Get_UInt (Value));
               end if;
            when Property_Tick_Skipped =>
               if Get_UInt (Value) < 2 then
                  Layer.Ticks.Skipped := 2;
               elsif Get_UInt (Value) > GUInt (Tick_Number'Last) then
                  Layer.Ticks.Skipped := Tick_Number'Last;
               else
                  Layer.Ticks.Skipped := Tick_Number (Get_UInt (Value));
               end if;
            when Property_Texts =>
               Set_Texts
               (  Layer,
                  Get_String (Value),
                  Character'Val (10),
                  False
               );
            when Property_Markup =>
               declare
                  Markup : constant String := Get_String (Value);
               begin
                  if Layer.Texts /= null then
                     for Index in Markup'Range loop
                        exit when Index not in Layer.Texts'Range;
                        Layer.Texts (Index).Markup :=
                           Markup (Index) = 'M';
                     end loop;
                  end if;
               end;
            when Property_Color =>
               Layer.Color := Get_Value (Value);
            when Property_Scaled =>
               Layer.Scaled := Get_Boolean (Value);
         end case;
      end if;
      Layer.Updated := True;
   end Set_Property_Value;

   procedure Set_Scaled
             (  Layer  : in out Elliptic_Annotation_Layer;
                Scaled : Boolean
             )  is
   begin
      Layer.Scaled  := Scaled;
      Layer.Updated := True;
   end Set_Scaled;

   procedure Set_Text
             (  Layer    : in out Elliptic_Annotation_Layer;
                Position : Positive;
                Text     : UTF8_String;
                Markup   : Boolean := False
             )  is
   begin
      if Layer.Texts = null then
         if Position = 1 then
            Layer.Texts :=
               new Annotation_List'
                   (  1..1 => new Annotation_Text'
                                  (  Size   => Text'Length,
                                     Length => Text'Length,
                                     Markup => Markup,
                                     Buffer => Text
                   )              );
            Layer.Updated := True;
            return;
         end if;
      elsif Position > Layer.Texts'Last then
         if Position = Layer.Texts'Last + 1 then
            declare
               Old_Texts : Annotation_List_Ptr := Layer.Texts;
            begin
               Layer.Texts := new Annotation_List (1..Position);
               Layer.Texts (Old_Texts'Range) := Old_Texts.all;
               Layer.Texts (Position) :=
                  new Annotation_Text'
                      (  Size   => Text'Length,
                         Length => Text'Length,
                         Markup => Markup,
                         Buffer => Text
                      );
               Free (Old_Texts);
               Layer.Updated := True;
               return;
            end;
         end if;
      else
         if (  Layer.Texts (Position) /= null
            and then
               Layer.Texts (Position).Size >= Text'Length
            )
         then
            declare
               This : Annotation_Text renames
                      Layer.Texts (Position).all;
            begin
               This.Buffer (1..Text'Length) := Text;
               This.Length := Text'Length;
               This.Markup := Markup;
            end;
         else
            Free (Layer.Texts (Position));
            Layer.Texts (Position) :=
               new Annotation_Text'
                   (  Size   => Text'Length,
                      Length => Text'Length,
                      Markup => Markup,
                      Buffer => Text
                   );
         end if;
         Layer.Updated := True;
         return;
      end if;
      raise Constraint_Error with "No such text";
   end Set_Text;

   procedure Set_Texts
             (  Layer  : in out Elliptic_Annotation_Layer;
                Texts  : Gtk.Enums.String_List.GList;
                Markup : Boolean := False
             )  is
      List : Annotation_List_Ptr :=
             Get_List (Texts, Layer.Ticks, Markup);
   begin
      Delete (Layer.Texts);
      Layer.Texts   := List;
      Layer.Updated := True;
   exception
      when others =>
         Delete (List);
   end Set_Texts;

   procedure Set_Texts
             (  Layer  : in out Elliptic_Annotation_Layer;
                Texts  : Controlled_String_List;
                Markup : Boolean := False
             )  is
   begin
      Set_Texts (Layer, Get_GList (Texts), Markup);
   end Set_Texts;

   procedure Set_Texts
             (  Layer     : in out Elliptic_Annotation_Layer;
                Texts     : UTF8_String;
                Delimiter : Character := ' ';
                Markup    : Boolean := False
             )  is
      List : Annotation_List_Ptr :=
                Get_List (Texts, Delimiter, Layer.Ticks, Markup);
   begin
      Delete (Layer.Texts);
      Layer.Texts   := List;
      Layer.Updated := True;
   exception
      when others =>
         Delete (List);
   end Set_Texts;

   procedure Store
             (  Stream : in out Root_Stream_Type'Class;
                Layer  : Elliptic_Annotation_Layer
             )  is
   begin
      Store (Stream, Layer.Ellipse);
      Store (Stream, Layer.Face);
      Store (Stream, Layer.Height);
      Store (Stream, Layer.Stretch);
      Store (Stream, Layer.From);
      Store (Stream, Layer.Length);
      Store (Stream, Layer.Mode);
      Store (Stream, Layer.Ticks);
      Store (Stream, Layer.Color);
      Store (Stream, Layer.Scaled);
      declare
         Markup : Bit_Array (1..Layer.Get_Texts_Number);
      begin
         for Index in 1..Layer.Get_Texts_Number loop
            Markup (Index) := Layer.Get_Markup (Index);
         end loop;
         Store (Stream, Markup);
      end;
      for Index in 1..Layer.Get_Texts_Number loop
         Store (Stream, Layer.Get_Text (Index));
      end loop;
   end Store;

end Gtk.Layered.Elliptic_Annotation;
