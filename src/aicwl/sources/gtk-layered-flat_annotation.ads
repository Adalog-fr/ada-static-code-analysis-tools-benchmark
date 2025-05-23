--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Layered.Flat_Annotation                 Luebeck            --
--  Interface                                      Winter, 2011       --
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

with Ada.Strings;   use Ada.Strings;
with Gtk.Missed;    use Gtk.Missed;

package Gtk.Layered.Flat_Annotation is
--
-- Flat_Annotation_Layer -- A layer consisting of  texts  drawn  at  the
--                          positions relative to a line
--
   type Flat_Annotation_Layer (<>) is
      new Abstract_Layer
      and Annotation_Layer
      and Scalable_Layer with private;
--
-- Add_Flat_Annotation -- Add annotation texts
--
--    Under       - The layer or widget where to place it under
--    Texts       - The annotation texts
--    Step        - The tick step
--    First       - The position of the first tick
--    Skipped     - The position of skipped ticks
--    From        - Coordinates of the first tick
--    Length      - The scale length
--    Scale_Angle - The angle of the scale
--    Face        - The text font
--    Height      - The text height
--    Stretch     - The text width scale relative to its height
--    Color       - The text color
--    Text_Angle  - The text angle
--    Justify     - The text alignment
--  [ Delimiter ] - The text delimiter character
--    Markup      - True if the texts use pango markup
--    Scaled      - The layer is scaled together with the parent
--
-- The  texts  are drawn at ticks in their position order. The texts can
-- be  specified as a list, a controlled list (in the form "a"/"b"/"c"),
-- or as a single string separated  by  the  Delimiter  character.  When
-- Length  is  positive  the   arc   is   drawn   clockwise,   otherwise
-- counterclockwise. When Scaled is true the annotation arc is scaled to
-- fit the parent widget. The scaling is performed as follows:
--
-- (o)  The  first text postion X is multiplied by the widget's size and
--      placed in the coorinate system centered in the widget's center;
-- (o)  The first text position Y is multiplied by the widget's size and
--      placed in the coorinate system centered in the widget's center;
-- (o)  Length, height, step are multiplied by the widget's size.
--
-- Returns :
--
--    The layer (optional)
--
-- Exceptions :
--
--    Constraint_Error - Wrong parameters
--
   procedure Add_Flat_Annotation
             (  Under       : not null access Layer_Location'Class;
                Texts       : Gtk.Enums.String_List.GList;
                Step        : GDouble;
                First       : Tick_Number := Tick_Number'Last;
                Skipped     : Tick_Number := Tick_Number'Last;
                From        : Cairo_Tuple := (0.0, 0.0);
                Length      : GDouble     := 1.0;
                Scale_Angle : GDouble     := 0.0;
                Face        : Pango_Cairo_Font :=
                                 Create_Toy
                                 (  Family => "arial",
                                    Slant  => CAIRO_FONT_SLANT_NORMAL,
                                    Weight => CAIRO_FONT_WEIGHT_NORMAL
                                 );
                Height      : GDouble     := 12.0;
                Stretch     : GDouble     := 1.0;
                Color       : Gdk_Color   := RGB (0.0, 0.0, 0.0);
                Text_Angle  : GDouble     := 0.0;
                Justify     : Alignment   := Center;
                Markup      : Boolean     := False;
                Scaled      : Boolean     := False
             );
   procedure Add_Flat_Annotation
             (  Under       : not null access Layer_Location'Class;
                Texts       : Controlled_String_List;
                Step        : GDouble;
                First       : Tick_Number := Tick_Number'Last;
                Skipped     : Tick_Number := Tick_Number'Last;
                From        : Cairo_Tuple := (0.0, 0.0);
                Length      : GDouble     := 1.0;
                Scale_Angle : GDouble     := 0.0;
                Face        : Pango_Cairo_Font :=
                                 Create_Toy
                                 (  Family => "arial",
                                    Slant  => CAIRO_FONT_SLANT_NORMAL,
                                    Weight => CAIRO_FONT_WEIGHT_NORMAL
                                 );
                Height      : GDouble     := 12.0;
                Stretch     : GDouble     := 1.0;
                Color       : Gdk_Color   := RGB (0.0, 0.0, 0.0);
                Text_Angle  : GDouble     := 0.0;
                Justify     : Alignment   := Center;
                Markup      : Boolean     := False;
                Scaled      : Boolean     := False
             );
   procedure Add_Flat_Annotation
             (  Under       : not null access Layer_Location'Class;
                Texts       : UTF8_String;
                Step        : GDouble;
                First       : Tick_Number := Tick_Number'Last;
                Skipped     : Tick_Number := Tick_Number'Last;
                From        : Cairo_Tuple := (0.0, 0.0);
                Length      : GDouble     := 1.0;
                Scale_Angle : GDouble     := 0.0;
                Face        : Pango_Cairo_Font :=
                                 Create_Toy
                                 (  Family => "arial",
                                    Slant  => CAIRO_FONT_SLANT_NORMAL,
                                    Weight => CAIRO_FONT_WEIGHT_NORMAL
                                 );
                Height      : GDouble     := 12.0;
                Stretch     : GDouble     := 1.0;
                Color       : Gdk_Color   := RGB (0.0, 0.0, 0.0);
                Text_Angle  : GDouble     := 0.0;
                Justify     : Alignment   := Center;
                Delimiter   : Character   := ' ';
                Markup      : Boolean     := False;
                Scaled      : Boolean     := False
             );
   function Add_Flat_Annotation
            (  Under       : not null access Layer_Location'Class;
               Texts       : Gtk.Enums.String_List.GList;
               Step        : GDouble;
               First       : Tick_Number := Tick_Number'Last;
               Skipped     : Tick_Number := Tick_Number'Last;
               From        : Cairo_Tuple := (0.0, 0.0);
               Length      : GDouble     := 1.0;
               Scale_Angle : GDouble     := 0.0;
               Face        : Pango_Cairo_Font :=
                                Create_Toy
                                (  Family => "arial",
                                   Slant  => CAIRO_FONT_SLANT_NORMAL,
                                   Weight => CAIRO_FONT_WEIGHT_NORMAL
                                );
               Height      : GDouble     := 12.0;
               Stretch     : GDouble     := 1.0;
               Color       : Gdk_Color   := RGB (0.0, 0.0, 0.0);
               Text_Angle  : GDouble     := 0.0;
               Justify     : Alignment   := Center;
               Markup      : Boolean     := False;
               Scaled      : Boolean     := False
            )  return not null access Flat_Annotation_Layer;
   function Add_Flat_Annotation
            (  Under       : not null access Layer_Location'Class;
               Texts       : Controlled_String_List;
               Step        : GDouble;
               First       : Tick_Number := Tick_Number'Last;
               Skipped     : Tick_Number := Tick_Number'Last;
               From        : Cairo_Tuple := (0.0, 0.0);
               Length      : GDouble     := 1.0;
               Scale_Angle : GDouble     := 0.0;
               Face        : Pango_Cairo_Font :=
                                Create_Toy
                                (  Family => "arial",
                                   Slant  => CAIRO_FONT_SLANT_NORMAL,
                                   Weight => CAIRO_FONT_WEIGHT_NORMAL
                                );
               Height      : GDouble     := 12.0;
               Stretch     : GDouble     := 1.0;
               Color       : Gdk_Color   := RGB (0.0, 0.0, 0.0);
               Text_Angle  : GDouble     := 0.0;
               Justify     : Alignment   := Center;
               Markup      : Boolean     := False;
               Scaled      : Boolean     := False
            )  return not null access Flat_Annotation_Layer;
   function Add_Flat_Annotation
            (  Under       : not null access Layer_Location'Class;
               Texts       : UTF8_String;
               Step        : GDouble;
               First       : Tick_Number := Tick_Number'Last;
               Skipped     : Tick_Number := Tick_Number'Last;
               From        : Cairo_Tuple := (0.0, 0.0);
               Length      : GDouble     := 1.0;
               Scale_Angle : GDouble     := 0.0;
               Face        : Pango_Cairo_Font :=
                                Create_Toy
                                (  Family => "arial",
                                   Slant  => CAIRO_FONT_SLANT_NORMAL,
                                   Weight => CAIRO_FONT_WEIGHT_NORMAL
                                );
               Height      : GDouble     := 12.0;
               Stretch     : GDouble     := 1.0;
               Color       : Gdk_Color   := RGB (0.0, 0.0, 0.0);
               Text_Angle  : GDouble     := 0.0;
               Justify     : Alignment   := Center;
               Delimiter   : Character   := ' ';
               Markup      : Boolean     := False;
               Scaled      : Boolean     := False
            )  return not null access Flat_Annotation_Layer;
--
-- Get_Color -- The text color
--
--    Layer - The annotation layer
--
-- Returns :
--
--    The text color
--
   function Get_Color (Layer : Flat_Annotation_Layer)
      return Gdk_Color;
--
-- Get_From -- The point where texts begins
--
--    Layer - The annotation layer
--
-- Returns :
--
--    The first annotation position
--
   function Get_From (Layer : Flat_Annotation_Layer) return Cairo_Tuple;
--
-- Get_Justify -- The text justification
--
--    Layer - The annotation layer
--
-- Returns :
--
--    The text justification
--
   function Get_Justify (Layer : Flat_Annotation_Layer)
      return Alignment;
--
-- Get_Length -- The length of the line of texts
--
--    Layer - The annotation layer
--
-- Returns :
--
--    The length of the line
--
   function Get_Length (Layer : Flat_Annotation_Layer)
      return GDouble;
--
-- Get_Scale_Angle -- The angle of the annotation line
--
--    Layer - The annotation layer
--
-- Returns :
--
--    The angle of the line along which annotation texts are arranged
--
   function Get_Scale_Angle (Layer : Flat_Annotation_Layer)
      return GDouble;
--
-- Get_Text_Angle -- The angle of the annotation texts
--
--    Layer - The annotation layer
--
-- Returns :
--
--    The angle of the texts
--
   function Get_Text_Angle (Layer : Flat_Annotation_Layer)
      return GDouble;
--
-- Set -- Parameters of the annotation
--
--    Layer       - The annotation layer
--    Ticks       - The ticks at the arc where the texts are placed
--    From        - Coordinates of the first tick
--    Length      - The scale length
--    Scale_Angle - The angle of the scale
--    Face        - The text font
--    Height      - The text height
--    Stretch     - The text width scale relative to its height
--    Color       - The text color
--    Text_Angle  - The text angle
--    Justify     - The text alignment
--
-- Exceptions :
--
--    Constraint_Error - Wrong parameters
--
   procedure Set
             (  Layer       : in out Flat_Annotation_Layer;
                Ticks       : Tick_Parameters;
                From        : Cairo_Tuple;
                Length      : GDouble;
                Scale_Angle : GDouble;
                Face        : Pango_Cairo_Font;
                Height      : GDouble;
                Stretch     : GDouble;
                Color       : Gdk_Color;
                Text_Angle  : GDouble;
                Justify     : Alignment
             );

   overriding
      function Add
               (  Under  : not null access Layer_Location'Class;
                  Stream : not null access Root_Stream_Type'Class
               )  return not null access Flat_Annotation_Layer;
   overriding
      procedure Draw
                (  Layer   : in out Flat_Annotation_Layer;
                   Context : Cairo_Context;
                   Area    : Gdk_Rectangle
                );
   overriding
      procedure Finalize (Layer : in out Flat_Annotation_Layer);
   overriding
      function Get_Face (Layer : Flat_Annotation_Layer)
         return Pango_Cairo_Font;
   overriding
      function Get_Height (Layer : Flat_Annotation_Layer)
         return GDouble;
   overriding
      function Get_Markup
               (  Layer    : Flat_Annotation_Layer;
                  Position : Positive
               )  return Boolean;
   overriding
      function Get_Properties_Number
               (  Layer : Flat_Annotation_Layer
               )  return Natural;
   overriding
      function Get_Property_Specification
               (  Layer    : Flat_Annotation_Layer;
                  Property : Positive
               )  return Param_Spec;
   overriding
      function Get_Property_Value
               (  Layer    : Flat_Annotation_Layer;
                  Property : Positive
               )  return GValue;
   overriding
      function Get_Scaled (Layer : Flat_Annotation_Layer)
         return Boolean;
   overriding
      function Get_Stretch (Layer : Flat_Annotation_Layer)
         return GDouble;
   overriding
      function Get_Text
               (  Layer    : Flat_Annotation_Layer;
                  Position : Positive
               )  return UTF8_String;
   overriding
      function Get_Texts_Number (Layer : Flat_Annotation_Layer)
         return Natural;
   overriding
      function Get_Ticks (Layer : Flat_Annotation_Layer)
         return Tick_Parameters;
   overriding
      function Is_Updated (Layer : Flat_Annotation_Layer)
         return Boolean;
   overriding
      procedure Move
                (  Layer  : in out Flat_Annotation_Layer;
                   Offset : Cairo_Tuple
                );
   overriding
      procedure Restore
                (  Stream : in out Root_Stream_Type'Class;
                   Layer  : in out Flat_Annotation_Layer
                );
   overriding
      procedure Scale
                (  Layer  : in out Flat_Annotation_Layer;
                   Factor : GDouble
                );
   overriding
      procedure Set_Face
                (  Layer : in out Flat_Annotation_Layer;
                   Face  : Pango_Cairo_Font
                );
   overriding
      procedure Set_Property_Value
                (  Layer    : in out Flat_Annotation_Layer;
                   Property : Positive;
                   Value    : GValue
                );
   overriding
      procedure Set_Scaled
                (  Layer  : in out Flat_Annotation_Layer;
                   Scaled : Boolean
                );
   overriding
      procedure Set_Text
                (  Layer    : in out Flat_Annotation_Layer;
                   Position : Positive;
                   Text     : UTF8_String;
                   Markup   : Boolean := False
                );
   overriding
      procedure Set_Texts
             (  Layer  : in out Flat_Annotation_Layer;
                Texts  : Gtk.Enums.String_List.GList;
                Markup : Boolean := False
             );
   overriding
      procedure Set_Texts
                (  Layer     : in out Flat_Annotation_Layer;
                   Texts     : UTF8_String;
                   Delimiter : Character := ' ';
                   Markup    : Boolean := False
                );
   overriding
      procedure Store
                (  Stream : in out Root_Stream_Type'Class;
                   Layer  : Flat_Annotation_Layer
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

   type Flat_Annotation_Layer is
      new Abstract_Layer and Annotation_Layer and Scalable_Layer with
   record
      Face        : Pango_Cairo_Font;
      Height      : GDouble;
      Stretch     : GDouble;
      From        : Cairo_Tuple;
      Length      : GDouble;
      Scale_Angle : GDouble;
      Text_Angle  : GDouble;
      Ticks       : Tick_Parameters;
      Color       : Gdk_Color;
      Texts       : Annotation_List_Ptr;
      Justify     : Alignment;
      Scaled      : Boolean := False;
      Updated     : Boolean := True;
   end record;

end Gtk.Layered.Flat_Annotation;
