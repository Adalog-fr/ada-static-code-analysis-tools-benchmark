--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Layered.Elliptic_Annotation             Luebeck            --
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

with Ada.Numerics;  use Ada.Numerics;
with Gtk.Missed;    use Gtk.Missed;

package Gtk.Layered.Elliptic_Annotation is
--
-- Elliptic_Annotation_Layer -- A layer consisting of texts drawn at the
--                              positions relative to an elliptic arc
--
   type Elliptic_Annotation_Layer (<>) is
      new Abstract_Layer
      and Annotation_Layer
      and Scalable_Layer with private;
--
-- Add_Elliptic_Annotation -- Add annotation texts
--
--    Under   - The layer or widget where to place the annotation under
--    Texts   - The annotation texts
--    Step    - The tick step (angle) at which to draw the texts
--    First   - The position of the first tick
--    Skipped - The position of skipped ticks
--    Ellipse - An elliptic arc at which the texts to be drawn
--    From    - The angle of the first tick
--    Length  - The angular length of the arc where texts are drawn
--    Face    - The text font
--    Height  - The text height
--    Stretch - The text width scale relative to its height
--    Mode    - The way a text is transformed when drawn at its tick
--    Color   - The text color
--  [ Delimiter ] - The text delimiter character
--    Markup  - True if the texts use pango markup
--    Scaled  - The layer is scaled together with the parent widget
--
-- The  texts  are drawn at ticks in their position order. The texts can
-- be  specified as a list, a controlled list (in the form "a"/"b"/"c"),
-- or as a single string separated  by  the  Delimiter  character.  When
-- Length  is  positive  the   arc   is   drawn   clockwise,   otherwise
-- counterclockwise. When Scaled is true the annotation arc is scaled to
-- fit the parent widget. The scaling is performed as follows:
--
-- (o)  The arc center's X is multiplied by the widget's size and placed
--      in the coorinate system centered in the widget's center;
-- (o)  The arc center's Y is multiplied by the widget's size and placed
--      in the coorinate system centered in the widget's center;
-- (o)  The arc major axis curvature is divided by the widget's size;
-- (o)  The arc minor axis radius is multiplied by the widget's size.
--
-- Returns :
--
--    The layer (optional)
--
-- Exceptions :
--
--    Constraint_Error - Wrong parameters
--
   procedure Add_Elliptic_Annotation
             (  Under   : not null access Layer_Location'Class;
                Texts   : Gtk.Enums.String_List.GList;
                Step    : GDouble;
                First   : Tick_Number         := Tick_Number'Last;
                Skipped : Tick_Number         := Tick_Number'Last;
                Ellipse : Ellipse_Parameters  := Unit_Circle;
                From    : GDouble             := 0.0;
                Length  : GDouble             := 2.0 * Pi;
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
             );
   procedure Add_Elliptic_Annotation
             (  Under   : not null access Layer_Location'Class;
                Texts   : Controlled_String_List;
                Step    : GDouble;
                First   : Tick_Number         := Tick_Number'Last;
                Skipped : Tick_Number         := Tick_Number'Last;
                Ellipse : Ellipse_Parameters  := Unit_Circle;
                From    : GDouble             := 0.0;
                Length  : GDouble             := 2.0 * Pi;
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
             );
   procedure Add_Elliptic_Annotation
             (  Under     : not null access Layer_Location'Class;
                Texts     : UTF8_String;
                Step      : GDouble;
                First     : Tick_Number         := Tick_Number'Last;
                Skipped   : Tick_Number         := Tick_Number'Last;
                Ellipse   : Ellipse_Parameters  := Unit_Circle;
                From      : GDouble             := 0.0;
                Length    : GDouble             := 2.0 * Pi;
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
                Markup  : Boolean               := False;
                Scaled    : Boolean             := False
             );
   function Add_Elliptic_Annotation
            (  Under   : not null access Layer_Location'Class;
               Texts   : Gtk.Enums.String_List.GList;
               Step    : GDouble;
               First   : Tick_Number         := Tick_Number'Last;
               Skipped : Tick_Number         := Tick_Number'Last;
               Ellipse : Ellipse_Parameters  := Unit_Circle;
               From    : GDouble             := 0.0;
               Length  : GDouble             := 2.0 * Pi;
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
            )  return not null access Elliptic_Annotation_Layer;
   function Add_Elliptic_Annotation
            (  Under   : not null access Layer_Location'Class;
               Texts   : Controlled_String_List;
               Step    : GDouble;
               First   : Tick_Number         := Tick_Number'Last;
               Skipped : Tick_Number         := Tick_Number'Last;
               Ellipse : Ellipse_Parameters  := Unit_Circle;
               From    : GDouble             := 0.0;
               Length  : GDouble             := 2.0 * Pi;
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
            )  return not null access Elliptic_Annotation_Layer;
   function Add_Elliptic_Annotation
            (  Under     : not null access Layer_Location'Class;
               Texts     : UTF8_String;
               Step      : GDouble;
               First     : Tick_Number         := Tick_Number'Last;
               Skipped   : Tick_Number         := Tick_Number'Last;
               Ellipse   : Ellipse_Parameters  := Unit_Circle;
               From      : GDouble             := 0.0;
               Length    : GDouble             := 2.0 * Pi;
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
               Markup  : Boolean               := False;
               Scaled    : Boolean             := False
            )  return not null access Elliptic_Annotation_Layer;
--
-- Get_Color -- The text color
--
--    Layer - The annotation layer
--
-- Returns :
--
--    The text color
--
   function Get_Color (Layer : Elliptic_Annotation_Layer)
      return Gdk_Color;
--
-- Get_Ellipse -- Ellipse parameters of the annotation
--
--    Layer - The annotation layer
--
-- Returns :
--
--    The parameters of the ellipse where texts are drawn
--
   function Get_Ellipse (Layer : Elliptic_Annotation_Layer)
      return Ellipse_Parameters;
--
-- Get_From -- Angle of where the arc of texts begins
--
--    Layer - The annotation layer
--
-- Returns :
--
--    The angle
--
   function Get_From (Layer : Elliptic_Annotation_Layer) return GDouble;
--
-- Get_Length -- Angular length of the arc of texts
--
--    Layer - The annotation layer
--
-- Returns :
--
--    The angle
--
   function Get_Length (Layer : Elliptic_Annotation_Layer)
      return GDouble;
--
-- Get_Mode -- The text transformation mode
--
--    Layer - The annotation layer
--
-- Returns :
--
--    The mode the text is transformed when drawn
--
   function Get_Mode (Layer : Elliptic_Annotation_Layer)
      return Text_Transformation;
--
-- Set -- Parameters of the annotation
--
--    Layer   - The annotation layer
--    Ellipse - An elliptic arc at which the texts to be drawn
--    Ticks   - The ticks at the arc where the texts are placed
--    From    - The angle of the first tick
--    Length  - The angular length of the arc where texts are drawn
--    Face    - The text font
--    Mode    - The way a text is transformed when drawn at its tick
--    Height  - The text height
--    Stretch - The text stretch
--    Color   - The text color
--
-- Exceptions :
--
--    Constraint_Error - Wrong parameters
--
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
             );

   overriding
      function Add
               (  Under  : not null access Layer_Location'Class;
                  Stream : not null access Root_Stream_Type'Class
               )  return not null access Elliptic_Annotation_Layer;
   overriding
      procedure Draw
                (  Layer   : in out Elliptic_Annotation_Layer;
                   Context : Cairo_Context;
                   Area    : Gdk_Rectangle
                );
   overriding
      procedure Finalize (Layer : in out Elliptic_Annotation_Layer);
   overriding
      function Get_Face (Layer : Elliptic_Annotation_Layer)
         return Pango_Cairo_Font;
   overriding
      function Get_Height (Layer : Elliptic_Annotation_Layer)
         return GDouble;
   overriding
      function Get_Markup
               (  Layer    : Elliptic_Annotation_Layer;
                  Position : Positive
               )  return Boolean;
   overriding
      function Get_Properties_Number
               (  Layer : Elliptic_Annotation_Layer
               )  return Natural;
   overriding
      function Get_Property_Specification
               (  Layer    : Elliptic_Annotation_Layer;
                  Property : Positive
               )  return Param_Spec;
   overriding
      function Get_Property_Value
               (  Layer    : Elliptic_Annotation_Layer;
                  Property : Positive
               )  return GValue;
   overriding
      function Get_Scaled (Layer : Elliptic_Annotation_Layer)
         return Boolean;
   overriding
      function Get_Stretch (Layer : Elliptic_Annotation_Layer)
         return GDouble;
   overriding
      function Get_Text
               (  Layer    : Elliptic_Annotation_Layer;
                  Position : Positive
               )  return UTF8_String;
   overriding
      function Get_Texts_Number (Layer : Elliptic_Annotation_Layer)
         return Natural;
   overriding
      function Get_Ticks (Layer : Elliptic_Annotation_Layer)
         return Tick_Parameters;
   overriding
      function Is_Updated (Layer : Elliptic_Annotation_Layer)
         return Boolean;
   overriding
      procedure Move
                (  Layer  : in out Elliptic_Annotation_Layer;
                   Offset : Cairo_Tuple
                );
   overriding
      procedure Restore
                (  Stream : in out Root_Stream_Type'Class;
                   Layer  : in out Elliptic_Annotation_Layer
                );
   overriding
      procedure Scale
                (  Layer  : in out Elliptic_Annotation_Layer;
                   Factor : GDouble
                );
   overriding
      procedure Set_Face
                (  Layer : in out Elliptic_Annotation_Layer;
                   Face  : Pango_Cairo_Font
                );
   overriding
      procedure Set_Property_Value
                (  Layer    : in out Elliptic_Annotation_Layer;
                   Property : Positive;
                   Value    : GValue
                );
   overriding
      procedure Set_Scaled
                (  Layer  : in out Elliptic_Annotation_Layer;
                   Scaled : Boolean
                );
   overriding
      procedure Set_Text
                (  Layer    : in out Elliptic_Annotation_Layer;
                   Position : Positive;
                   Text     : UTF8_String;
                   Markup   : Boolean := False
                );
   overriding
      procedure Set_Texts
                (  Layer  : in out Elliptic_Annotation_Layer;
                   Texts  : Gtk.Enums.String_List.GList;
                   Markup : Boolean := False
                );
   overriding
      procedure Set_Texts
                (  Layer     : in out Elliptic_Annotation_Layer;
                   Texts     : UTF8_String;
                   Delimiter : Character := ' ';
                   Markup    : Boolean := False
                );
   overriding
      procedure Store
                (  Stream : in out Root_Stream_Type'Class;
                   Layer  : Elliptic_Annotation_Layer
                );
private
   type Annotation_Text (Size : Natural) is record
      Length : Natural;
      Markup : Boolean;
      Buffer : UTF8_String (1..Size);
   end record;
   type Annotation_Text_Ptr is access Annotation_Text;
   function "+" (Value : Annotation_Text_Ptr) return UTF8_String;
   pragma Inline ("+");

   type Annotation_List is
      array (Positive range <>) of Annotation_Text_Ptr;
   type Annotation_List_Ptr is access Annotation_List;

   type Elliptic_Annotation_Layer is
      new Abstract_Layer and Annotation_Layer and Scalable_Layer with
   record
      Ellipse : Ellipse_Parameters;
      Face    : Pango_Cairo_Font;
      Height  : GDouble;
      Stretch : GDouble;
      From    : GDouble;
      Length  : GDouble;
      Mode    : Text_Transformation;
      Ticks   : Tick_Parameters;
      Color   : Gdk_Color;
      Texts   : Annotation_List_Ptr;
      Scaled  : Boolean := False;
      Updated : Boolean := True;
   end record;

end Gtk.Layered.Elliptic_Annotation;
