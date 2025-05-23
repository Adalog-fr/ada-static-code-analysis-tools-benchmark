--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Layered.Disk_Needle                     Luebeck            --
--  Interface                                      Winter, 2017       --
--                                                                    --
--                                Last revision :  13:14 14 Sep 2019  --
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

package Gtk.Layered.Disk_Needle is

   Needle_On_Color  : constant Gdk_Color :=
                      RGB
                      (  Red   => GDouble (106.0) / GDouble (255.0),
                         Green => GDouble (173.0) / GDouble (255.0),
                         Blue  => GDouble ( 68.0) / GDouble (255.0)
                      );
   Needle_Off_Color : constant Gdk_Color :=
                      RGB
                      (  Red   => GDouble (221.0) / GDouble (255.0),
                         Green => GDouble ( 60.0) / GDouble (255.0),
                         Blue  => GDouble ( 40.0) / GDouble (255.0)
                      );
--
-- Disk_Needle_Layer -- A needle  represented  by an rotating disk.  The
--                      disk  is  split  into two halves or four sectors
--                      of interleaving "on" and "off" colors.
--
   type Disk_Needle_Layer (<>) is
      new Abstract_Layer
      and Gauge_Needle
      and Scalable_Layer with private;
--
-- Add_Disk_Needle -- Add needle
--
--    Under       - The layer or widget where to place the arc under
--    Center      - The needle's rotation center
--    Radius      - The needle's disk radius
--    From        - The angle (position) of the lowest value
--    Length      - The angular length of the values range
--    Sectors     - True if four sectors, false if two halves
--    On_Color    - The color of the needle's "on"
--    Off_Color   - The color of the needle's "off"
--    Adjustment  - The value source
--    Scaled      - The layer is scaled together with the parent widget
--
-- When Adjustment is not null the needle moves each time the adjustment
-- is changed. Note that it also redraws the layered widget  it  belongs
-- to. For complex widgets it is not recommended to use  adjustment  and
-- event controlled layered widgets. As an  alternative  consider  using
-- Set_Value instead  and  redrawing  the  layered  widget  periodically
-- independently on the value state. When Scaled is true the  needle  is
-- scaled to fit the parent widget. The scaling of needle  is  performed
-- as follows:
--
-- (o)  Center's X is multiplied by the widget's  size and placed in the
--      coordinate system centered in the widget's center;
-- (o)  Center's Y  is multiplied by the widget's size and placed in the
--      coordinate system centered in the widget's center;
-- (o)  Radius is multiplied by the widget's size.
--
-- Returns :
--
--    The layer (optional)
--
-- Exceptions :
--
--    Constraint_Error - Wrong parameters
--
   procedure Add_Disk_Needle
             (  Under      : not null access Layer_Location'Class;
                Center     : Cairo_Tuple := (0.0, 0.0);
                Radius     : GDouble     := 0.5;
                From       : GDouble     := Pi;
                Length     : GDouble     := Pi;
                Sectors    : Boolean     := False;
                On_Color   : Gdk_Color   := Needle_On_Color;
                Off_Color  : Gdk_Color   := Needle_Off_Color;
                Adjustment : access Gtk_Adjustment_Record'Class := null;
                Scaled     : Boolean     := False
             );
   function Add_Disk_Needle
            (  Under      : not null access Layer_Location'Class;
               Center     : Cairo_Tuple := (0.0, 0.0);
               Radius     : GDouble     := 0.5;
               From       : GDouble     := Pi;
               Length     : GDouble     := Pi;
               Sectors    : Boolean     := False;
               On_Color   : Gdk_Color   := Needle_On_Color;
               Off_Color  : Gdk_Color   := Needle_Off_Color;
               Adjustment : access Gtk_Adjustment_Record'Class := null;
               Scaled     : Boolean     := False
            )  return not null access Disk_Needle_Layer;
--
-- Get_Center -- The needle's rotation center
--
--    Layer - The needle
--
-- Returns :
--
--    The position
--
   function Get_Center (Layer : Disk_Needle_Layer) return Cairo_Tuple;
--
-- Get_From -- The angle (position) of the lowest value
--
--    Layer - The needle
--
-- Returns :
--
--    The angle
--
   function Get_From (Layer : Disk_Needle_Layer) return GDouble;
--
-- Get_Length -- The angular length of the needle positions
--
--    Layer - The needle
--
-- Returns :
--
--    The angle
--
   function Get_Length (Layer : Disk_Needle_Layer) return GDouble;
--
-- Get_Off_Color -- The color of the needle's off half
--
--    Layer - The needle
--
-- Returns :
--
--    The needle's color
--
   function Get_Off_Color (Layer : Disk_Needle_Layer) return Gdk_Color;
--
-- Get_On_Color -- The color of the needle's on half
--
--    Layer - The needle
--
-- Returns :
--
--    The needle's color
--
   function Get_On_Color (Layer : Disk_Needle_Layer) return Gdk_Color;
--
-- Get_Radius -- The needle's disk radius
--
--    Layer - The needle
--
-- Returns :
--
--    The radius
--
   function Get_Radius (Layer : Disk_Needle_Layer) return GDouble;
--
-- Get_Sectors -- The needle's mode
--
--    Layer - The needle
--
-- Returns :
--
--    True if sectors, False if halves
--
   function Get_Sectors (Layer : Disk_Needle_Layer) return Boolean;
--
-- Set -- Parameters of the needle
--
--    Layer     - The needle
--    Center    - The needle's rotation center
--    Radius    - The needle's disk radius
--    From      - The angle (position) of the lowest value
--    Length    - The angular length of the values range
--    Sectors   - True if four sectors, False if two halves
--    On_Color  - The color of the needle's "on" half
--    Off_Color - The color of the needle's "off" half
--
-- Exceptions :
--
--    Constraint_Error - Wrong parameters
--
   procedure Set
             (  Layer     : in out Disk_Needle_Layer;
                Center    : Cairo_Tuple;
                Radius    : GDouble;
                From      : GDouble;
                Length    : GDouble;
                Sectors   : Boolean;
                On_Color  : Gdk_Color;
                Off_Color : Gdk_Color
             );

   overriding
      function Add
               (  Under  : not null access Layer_Location'Class;
                  Stream : not null access Root_Stream_Type'Class
               )  return not null access Disk_Needle_Layer;
   overriding
      procedure Draw
                (  Layer   : in out Disk_Needle_Layer;
                   Context : Cairo_Context;
                   Area    : Gdk_Rectangle
                );
   overriding procedure Finalize (Layer : in out Disk_Needle_Layer);
   overriding
      function Get_Adjustment (Layer : Disk_Needle_Layer)
         return Gtk_Adjustment;
   overriding
      function Get_Properties_Number
               (  Layer : Disk_Needle_Layer
               )  return Natural;
   overriding
      function Get_Property_Specification
               (  Layer    : Disk_Needle_Layer;
                  Property : Positive
               )  return Param_Spec;
   overriding
      function Get_Property_Value
               (  Layer    : Disk_Needle_Layer;
                  Property : Positive
               )  return GValue;
   overriding
      function Get_Scaled (Layer : Disk_Needle_Layer) return Boolean;
   overriding
      function Get_Value (Layer : Disk_Needle_Layer) return GDouble;
   overriding
      function Is_Updated (Layer : Disk_Needle_Layer) return Boolean;
   overriding
      procedure Move
                (  Layer  : in out Disk_Needle_Layer;
                   Offset : Cairo_Tuple
                );
   overriding
      procedure Restore
                (  Stream : in out Root_Stream_Type'Class;
                   Layer  : in out Disk_Needle_Layer
                );
   overriding
      procedure Scale
                (  Layer  : in out Disk_Needle_Layer;
                   Factor : GDouble
                );
   overriding
      procedure Set_Property_Value
                (  Layer    : in out Disk_Needle_Layer;
                   Property : Positive;
                   Value    : GValue
                );
   overriding
      procedure Set_Scaled
                (  Layer  : in out Disk_Needle_Layer;
                   Scaled : Boolean
                );
   overriding
      procedure Set_Value
                (  Layer : in out Disk_Needle_Layer;
                   Value : GDouble
                );
   overriding
      procedure Store
                (  Stream : in out Root_Stream_Type'Class;
                   Layer  : Disk_Needle_Layer
                );
private
   type Disk_Needle_Layer is
      new Abstract_Layer and Gauge_Needle and Scalable_Layer with
   record
      Center        : Cairo_Tuple;
      Radius        : GDouble;
      From          : GDouble;
      Length        : GDouble;
      Sectors       : Boolean;
      On_Color      : Gdk_Color;
      Off_Color     : Gdk_Color;
      Value         : GDouble := 0.0;
      Adjustment    : Gtk_Adjustment;
      Changed       : Handler_Id;
      Value_Changed : Handler_Id;
      Scaled        : Boolean := False;
      Updated       : Boolean := True;
      pragma Atomic (Value);
      pragma Atomic (Updated);
   end record;

end Gtk.Layered.Disk_Needle;
