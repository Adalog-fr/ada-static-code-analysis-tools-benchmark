--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Layered.Elliptic_Background             Luebeck            --
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

with Ada.Numerics;                   use Ada.Numerics;
with Gdk.RGBA;                       use Gdk.RGBA;
with Gtk.Enums;                      use Gtk.Enums;
with Gtk.Layered.Abstract_Bordered;  use Gtk.Layered.Abstract_Bordered;
with Gtk.Missed;                     use Gtk.Missed;

package Gtk.Layered.Elliptic_Background is
--
-- Elliptic_Background_Layer -- A filled elliptic background
--
   type Elliptic_Background_Layer (<>) is
      new Abstract_Bordered_Layer with private;
--
-- Add_Elliptic_Background -- Add an elliptic background
--
--    Under  - The layer or widget where to place the background under
--    Outer  - The outer ellipse bound of the layer
--  [ Inner | Center ] - The inner ellipse bound of the layer
--    From             - The angle where the elliptic arcs begins
--    Length           - The angular length of the arcs
--    Color            - The background color
--    Border_Width     - Border width
--    Border_Depth     - Border depth
--    Border_Color     - The border color
--    Border_Shadow    - The border shape
--    Deepened         - The border depth is increased with the parent
--    Lens_Reflex      - Color of the lens on top with a reflex
--    Lens_Shadow      - Color of the lens on top with a shadow
--    Scaled           - The layer is scaled together with the parent
--    Widened          - The border line is widened with the parent
--
-- The  procedure  adds  an elliptic background and foreground above at.
-- The layers above, visually nested in the background should be  placed
-- above  the  background  and  below  the  foreground.  When  Inner  is
-- specified  the ends of the outer arc are connected to the ends of the
-- inner  arc.  When  Center  is specified the ends of the outer arc are
-- connected  to  the point. When neither Inner nor Center is specified,
-- the ends of the outer arc are connected by a direct line. When Length
-- is two Pi or longer the inner ellipse arc or center point are ignored
-- and  the shape is an ellipse. When Scaled is true the arcs are scaled
-- to fit the parent widget. The scaling is performed as follows:
--
-- (o)  Ellipse center's X is multiplied by the widget's size and placed
--      in the coorinate system centered in the widget's center;
-- (o)  Ellipse center's Y is multiplied by the widget's size and placed
--      in the coorinate system centered in the widget's center;
-- (o)  Ellipse major axis curvature is divided by the widget's size;
-- (o)  Ellipse minor axis radius is multiplied by the widget's size.
--
-- Returns :
--
--    The layer (optional)
--
-- Exceptions :
--
--    Constraint_Error - Wrong parameters
--
   procedure Add_Elliptic_Background
             (  Under  : not null access Layer_Location'Class;
                Outer  : Ellipse_Parameters := Unit_Circle;
                Inner  : Ellipse_Parameters;
                From          : GDouble   := 0.0;
                Length        : GDouble   := 2.0 * Pi;
                Color         : Gdk_Color := RGB (0.0, 0.0, 0.0);
                Border_Width  : GDouble   := 0.0;
                Border_Depth  : GDouble   := 1.0;
                Border_Color  : Border_Color_Type := Default_Color;
                Border_Shadow : Gtk_Shadow_Type   := Shadow_In;
                Deepened      : Boolean   := False;
                Lens_Reflex   : Gdk_RGBA  := (1.0, 1.0, 1.0, 0.0);
                Lens_Shadow   : Gdk_RGBA  := (0.0, 0.0, 0.0, 0.0);
                Scaled        : Boolean   := False;
                Widened       : Boolean   := False
             );
   procedure Add_Elliptic_Background
             (  Under  : not null access Layer_Location'Class;
                Outer  : Ellipse_Parameters := Unit_Circle;
                Center : Cairo_Tuple;
                From          : GDouble   := 0.0;
                Length        : GDouble   := 2.0 * Pi;
                Color         : Gdk_Color := RGB (0.0, 0.0, 0.0);
                Border_Width  : GDouble   := 0.0;
                Border_Depth  : GDouble   := 1.0;
                Border_Color  : Border_Color_Type := Default_Color;
                Border_Shadow : Gtk_Shadow_Type   := Shadow_In;
                Deepened      : Boolean   := False;
                Lens_Reflex   : Gdk_RGBA  := (1.0, 1.0, 1.0, 0.0);
                Lens_Shadow   : Gdk_RGBA  := (0.0, 0.0, 0.0, 0.0);
                Scaled        : Boolean   := False;
                Widened       : Boolean   := False
             );
   procedure Add_Elliptic_Background
             (  Under  : not null access Layer_Location'Class;
                Outer  : Ellipse_Parameters := Unit_Circle;
                From          : GDouble   := 0.0;
                Length        : GDouble   := 2.0 * Pi;
                Color         : Gdk_Color := RGB (0.0, 0.0, 0.0);
                Border_Width  : GDouble   := 0.0;
                Border_Depth  : GDouble   := 1.0;
                Border_Color  : Border_Color_Type := Default_Color;
                Border_Shadow : Gtk_Shadow_Type   := Shadow_In;
                Deepened      : Boolean   := False;
                Lens_Reflex   : Gdk_RGBA  := (1.0, 1.0, 1.0, 0.0);
                Lens_Shadow   : Gdk_RGBA  := (0.0, 0.0, 0.0, 0.0);
                Scaled        : Boolean   := False;
                Widened       : Boolean   := False
             );
   function Add_Elliptic_Background
            (  Under  : not null access Layer_Location'Class;
               Outer  : Ellipse_Parameters := Unit_Circle;
               Inner  : Ellipse_Parameters;
               From          : GDouble   := 0.0;
               Length        : GDouble   := 2.0 * Pi;
               Color         : Gdk_Color := RGB (0.0, 0.0, 0.0);
               Border_Width  : GDouble   := 0.0;
               Border_Depth  : GDouble   := 1.0;
               Border_Color  : Border_Color_Type := Default_Color;
               Border_Shadow : Gtk_Shadow_Type   := Shadow_In;
               Deepened      : Boolean   := False;
               Lens_Reflex   : Gdk_RGBA  := (1.0, 1.0, 1.0, 0.0);
               Lens_Shadow   : Gdk_RGBA  := (0.0, 0.0, 0.0, 0.0);
               Scaled        : Boolean   := False;
               Widened       : Boolean   := False
            )  return not null access Elliptic_Background_Layer;
   function Add_Elliptic_Background
            (  Under  : not null access Layer_Location'Class;
               Outer  : Ellipse_Parameters := Unit_Circle;
               Center : Cairo_Tuple;
               From          : GDouble   := 0.0;
               Length        : GDouble   := 2.0 * Pi;
               Color         : Gdk_Color := RGB (0.0, 0.0, 0.0);
               Border_Width  : GDouble   := 0.0;
               Border_Depth  : GDouble   := 1.0;
               Border_Color  : Border_Color_Type := Default_Color;
               Border_Shadow : Gtk_Shadow_Type   := Shadow_In;
               Deepened      : Boolean   := False;
               Lens_Reflex   : Gdk_RGBA  := (1.0, 1.0, 1.0, 0.0);
               Lens_Shadow   : Gdk_RGBA  := (0.0, 0.0, 0.0, 0.0);
               Scaled        : Boolean   := False;
               Widened       : Boolean   := False
            )  return not null access Elliptic_Background_Layer;
   function Add_Elliptic_Background
            (  Under  : not null access Layer_Location'Class;
               Outer  : Ellipse_Parameters := Unit_Circle;
               From          : GDouble   := 0.0;
               Length        : GDouble   := 2.0 * Pi;
               Color         : Gdk_Color := RGB (0.0, 0.0, 0.0);
               Border_Width  : GDouble   := 0.0;
               Border_Depth  : GDouble   := 1.0;
               Border_Color  : Border_Color_Type := Default_Color;
               Border_Shadow : Gtk_Shadow_Type   := Shadow_In;
               Deepened      : Boolean   := False;
               Lens_Reflex   : Gdk_RGBA  := (1.0, 1.0, 1.0, 0.0);
               Lens_Shadow   : Gdk_RGBA  := (0.0, 0.0, 0.0, 0.0);
               Scaled        : Boolean   := False;
               Widened       : Boolean   := False
            )  return not null access Elliptic_Background_Layer;
--
-- Get_Color -- The text color
--
--    Layer - The background layer
--
-- Returns :
--
--    The text color
--
   function Get_Color (Layer : Elliptic_Background_Layer)
      return Gdk_Color;
--
-- Get_From -- Angle where the elliptic arcs begin
--
--    Layer - The background layer
--
-- Returns :
--
--    The angle
--
   function Get_From (Layer : Elliptic_Background_Layer) return GDouble;
--
-- Get_Inner -- Inner ellipse parameters of the background
--
--    Layer - The background layer
--
-- Returns :
--
--    The parameters of the arc's ellipse
--
   function Get_Inner (Layer : Elliptic_Background_Layer)
      return Elliptic_Arc_Closure;
--
-- Get_Length -- Angular length of the elliptic arcs
--
--    Layer - The background layer
--
-- Returns :
--
--    The angle
--
   function Get_Length (Layer : Elliptic_Background_Layer)
      return GDouble;
--
-- Get_Outer -- Outer ellipse parameters of the background
--
--    Layer - The background layer
--
-- Returns :
--
--    The parameters of the arc's ellipse
--
   function Get_Outer (Layer : Elliptic_Background_Layer)
      return Ellipse_Parameters;
--
-- Set -- Parameters of the background
--
--    Layer         - The background layer
--    Outer         - The outer ellipse bound of the layer
--    Inner         - The inner ellipse bound of the layer
--    From          - The angle where the elliptic arcs begins
--    Length        - The angular length of the arcs
--    Color         - The background color
--    Border_Width  - Border width
--    Border_Depth  - Border depth
--    Border_Color  - The border color
--    Border_Shadow - The border shape
--    Lens_Reflex   - Color of the lens on top with a reflex
--    Lens_Shadow   - Color of the lens on top with a shadow
--
-- Exceptions :
--
--    Constraint_Error - Wrong parameters
--
   procedure Set
             (  Layer         : in out Elliptic_Background_Layer;
                Outer         : Ellipse_Parameters;
                Inner         : Elliptic_Arc_Closure;
                From          : GDouble;
                Length        : GDouble;
                Color         : Gdk_Color;
                Border_Width  : GDouble;
                Border_Depth  : GDouble;
                Border_Color  : Border_Color_Type;
                Border_Shadow : Gtk_Shadow_Type;
                Lens_Reflex   : Gdk_RGBA;
                Lens_Shadow   : Gdk_RGBA
             );

   overriding
      function Add
               (  Under  : not null access Layer_Location'Class;
                  Stream : not null access Root_Stream_Type'Class
               )  return not null access Elliptic_Background_Layer;
   overriding
      procedure Draw_Contents
                (  Layer   : in out Elliptic_Background_Layer;
                   Context : Cairo_Context;
                   Area    : Gdk_Rectangle
                );
   overriding
      function Get_Properties_Number
               (  Layer : Elliptic_Background_Layer
               )  return Natural;
   overriding
      function Get_Property_Specification
               (  Layer    : Elliptic_Background_Layer;
                  Property : Positive
               )  return Param_Spec;
   overriding
      function Get_Property_Value
               (  Layer    : Elliptic_Background_Layer;
                  Property : Positive
               )  return GValue;
   overriding
      procedure Move
                (  Layer  : in out Elliptic_Background_Layer;
                   Offset : Cairo_Tuple
                );
   overriding
      procedure Restore
                (  Stream : in out Root_Stream_Type'Class;
                   Layer  : in out Elliptic_Background_Layer
                );
   overriding
      procedure Scale
                (  Layer  : in out Elliptic_Background_Layer;
                   Factor : GDouble
                );
   overriding
      procedure Set_Property_Value
                (  Layer    : in out Elliptic_Background_Layer;
                   Property : Positive;
                   Value    : GValue
                );
   overriding
      procedure Set_Contents_Path
                (  Layer   : in out Elliptic_Background_Layer;
                   Context : Cairo_Context;
                   Area    : Gdk_Rectangle
                );
   overriding
      procedure Store
                (  Stream : in out Root_Stream_Type'Class;
                   Layer  : Elliptic_Background_Layer
                );
private
   type Elliptic_Background_Layer is
      new Abstract_Bordered_Layer with
   record
      Outer  : Ellipse_Parameters;
      Inner  : Elliptic_Arc_Closure;
      From   : GDouble;
      Length : GDouble;
      Color  : Gdk_Color;
   end record;

end Gtk.Layered.Elliptic_Background;
