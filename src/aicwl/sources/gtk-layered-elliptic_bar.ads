--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Layered.Elliptic_Bar                    Luebeck            --
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

with Ada.Numerics;  use Ada.Numerics;
with Gtk.Handlers;  use Gtk.Handlers;
with Gtk.Missed;    use Gtk.Missed;

package Gtk.Layered.Elliptic_Bar is
--
-- Elliptic_Bar_Layer -- An elliptic line indicating a value
--
   type Elliptic_Bar_Layer (<>) is
      new Abstract_Layer
      and Gauge_Needle
      and Scalable_Layer
      and Widened_Layer with private;
--
-- Add_Elliptic_Bar_Layer -- Add Elliptic_Bar
--
--    Under      - The layer or widget where to place the arc under
--    Ellipse    - The ellipse to which the arc belongs
--    From       - The angle (position) of the lowest value
--    Length     - The angular length of the values range
--    Width      - The bar's width
--    Color      - The Elliptic_Bar color
--    Line_Cap   - The style of elliptic bar ending
--    Adjustment - The value source
--    Scaled     - The layer is scaled together with the parent widget
--    Widened    - The border line is widened with the parent
--
-- When Length is positive the bar's moves clockwise, otherwise it  does
-- counterclockwise. When Adjustment is not null the bar moves each time
-- the adjustment is changed. Note that  it  also  redraws  the  layered
-- widget  it  belongs  to. For complex widgets it is not recommended to
-- use  adjustment  and  event  controlled  layered   widgets.   As   an
-- alternative  consider  using  Set_Value  instead  and  redrawing  the
-- layered widget periodically independently on the  value  state.  When
-- Scaled is true the bar is  scaled  to  fit  the  parent  widget.  The
-- scaling of bar is performed as follows:
--
-- (o)  The ellipse center's X is multiplied by the  widget's  size  and
--      placed in the coorinate system centered in the widget's center;
-- (o)  The ellipse center's Y is multiplied by the  widget's  size  and
--      placed in the coorinate system centered in the widget's center;
-- (o)  The  ellipse  major  axis  curvature  is divided by the widget's
--      size;
-- (o)  The  ellipse  minor  axis  radius  is multiplied by the widget's
--      size.
--
-- Returns :
--
--    The layer (optional)
--
-- Exceptions :
--
--    Constraint_Error - Wrong parameters
--
   procedure Add_Elliptic_Bar
             (  Under      : not null access Layer_Location'Class;
                Ellipse    : Ellipse_Parameters := Unit_Circle;
                From       : GDouble        := 3.0 * Pi / 4.0;
                Length     : GDouble        := 3.0 * Pi / 2.0;
                Width      : GDouble        := 1.0;
                Color      : Gdk_Color      := RGB (1.0, 0.0, 0.0);
                Line_Cap   : Cairo_Line_Cap := CAIRO_LINE_CAP_BUTT;
                Adjustment : access Gtk_Adjustment_Record'Class := null;
                Scaled     : Boolean        := False;
                Widened    : Boolean        := False
            );
   function Add_Elliptic_Bar
            (  Under      : not null access Layer_Location'Class;
               Ellipse    : Ellipse_Parameters := Unit_Circle;
               From       : GDouble        := 3.0 * Pi / 4.0;
               Length     : GDouble        := 3.0 * Pi / 2.0;
               Width      : GDouble        := 1.0;
               Color      : Gdk_Color      := RGB (1.0, 0.0, 0.0);
               Line_Cap   : Cairo_Line_Cap := CAIRO_LINE_CAP_BUTT;
               Adjustment : access Gtk_Adjustment_Record'Class := null;
               Scaled     : Boolean        := False;
               Widened    : Boolean        := False
            )  return not null access Elliptic_Bar_Layer;
--
-- Get_Ellipse -- Ellipse parameters of the bar
--
--    Layer - The elliptic bar
--
-- Returns :
--
--    The parameters of the bar's arc ellipse
--
   function Get_Ellipse (Layer : Elliptic_Bar_Layer)
      return Ellipse_Parameters;
--
-- Get_From -- The angle (position) of the lowest value
--
--    Layer - The elliptic bar
--
-- Returns :
--
--    The angle
--
   function Get_From (Layer : Elliptic_Bar_Layer) return GDouble;
--
-- Get_Length -- The angular length of the bar positions
--
--    Layer - The elliptic bar
--
-- Returns :
--
--    The angle
--
   function Get_Length (Layer : Elliptic_Bar_Layer) return GDouble;
--
-- Get_Line -- Arc's line parameters
--
--    Layer - The elliptic bar
--
-- Returns :
--
--    The arc's line parameters
--
   function Get_Line (Layer : Elliptic_Bar_Layer)
      return Line_Parameters;
--
-- Set -- Parameters of the arc
--
--    Layer   - The elliptic bar
--    Ellipse - The ellipse to which the arc belongs
--    From    - The angle (position) of the lowest value
--    Length  - The angular length of the values range
--    Line    - The arc line parameters
--
-- Exceptions :
--
--    Constraint_Error - Wrong parameters
--
   procedure Set
             (  Layer   : in out Elliptic_Bar_Layer;
                Ellipse : Ellipse_Parameters;
                From    : GDouble;
                Length  : GDouble;
                Line    : Line_Parameters
             );

   overriding
      function Add
               (  Under  : not null access Layer_Location'Class;
                  Stream : not null access Root_Stream_Type'Class
               )  return not null access Elliptic_Bar_Layer;
   overriding
      function Get_Adjustment (Layer : Elliptic_Bar_Layer)
         return Gtk_Adjustment;
   overriding
      function Get_Properties_Number
               (  Layer : Elliptic_Bar_Layer
               )  return Natural;
   overriding
      function Get_Property_Specification
               (  Layer    : Elliptic_Bar_Layer;
                  Property : Positive
               )  return Param_Spec;
   overriding
      function Get_Property_Value
               (  Layer    : Elliptic_Bar_Layer;
                  Property : Positive
               )  return GValue;
   overriding
      function Get_Scaled (Layer : Elliptic_Bar_Layer) return Boolean;
   overriding
      function Get_Widened (Layer : Elliptic_Bar_Layer) return Boolean;
   overriding
      procedure Draw
                (  Layer   : in out Elliptic_Bar_Layer;
                   Context : Cairo_Context;
                   Area    : Gdk_Rectangle
                );
   overriding procedure Finalize (Layer : in out Elliptic_Bar_Layer);
   overriding
      function Get_Value (Layer : Elliptic_Bar_Layer) return GDouble;
   overriding
      procedure Move
                (  Layer  : in out Elliptic_Bar_Layer;
                   Offset : Cairo_Tuple
                );
   overriding
      function Is_Updated (Layer : Elliptic_Bar_Layer) return Boolean;
   overriding
      procedure Restore
                (  Stream : in out Root_Stream_Type'Class;
                   Layer  : in out Elliptic_Bar_Layer
                );
   overriding
      procedure Scale
                (  Layer  : in out Elliptic_Bar_Layer;
                   Factor : GDouble
                );
   overriding
      procedure Set_Property_Value
                (  Layer    : in out Elliptic_Bar_Layer;
                   Property : Positive;
                   Value    : GValue
                );
   overriding
      procedure Set_Scaled
                (  Layer  : in out Elliptic_Bar_Layer;
                   Scaled : Boolean
                );
   overriding
      procedure Set_Value
             (  Layer : in out Elliptic_Bar_Layer;
                Value : GDouble
             );
   overriding
      procedure Set_Widened
                (  Layer   : in out Elliptic_Bar_Layer;
                   Widened : Boolean
                );
   overriding
      procedure Store
                (  Stream : in out Root_Stream_Type'Class;
                   Layer  : Elliptic_Bar_Layer
                );
private
   type Elliptic_Bar_Layer is
      new Abstract_Layer
      and Gauge_Needle
      and Scalable_Layer
      and Widened_Layer with
   record
      Ellipse       : Ellipse_Parameters;
      From          : GDouble;
      Length        : GDouble;
      Line          : Line_Parameters;
      Value         : GDouble := 0.0;
      Adjustment    : Gtk_Adjustment;
      Changed       : Handler_Id;
      Value_Changed : Handler_Id;
      Scaled        : Boolean := False;
      Widened       : Boolean := False;
      Updated       : Boolean := True;
      pragma Atomic (Value);
   end record;

end Gtk.Layered.Elliptic_Bar;
