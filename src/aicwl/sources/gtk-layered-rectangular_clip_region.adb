--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Layered.                                Luebeck            --
--        Rectangular_Clip_Region                  Winter, 2010       --
--  Implementation                                                    --
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
with Cairo.Transformations;     use Cairo.Transformations;
with GLib.Properties.Creation;  use GLib.Properties.Creation;
with Gtk.Layered.Stream_IO;     use Gtk.Layered.Stream_IO;

with Ada.Unchecked_Deallocation;

package body Gtk.Layered.Rectangular_Clip_Region is

   type Layer_Property is
        (  Property_Height,
           Property_Width,
           Property_Center_X,
           Property_Center_Y,
           Property_Angle,
           Property_Radius,
           Property_Scaled
        );

   procedure Free is
      new Ada.Unchecked_Deallocation
          (  Rectangular_Clip_Region_On_Layer,
             Rectangular_Clip_Region_On_Layer_Ptr
          );

   procedure Free is
      new Ada.Unchecked_Deallocation
          (  Rectangular_Clip_Region_Off_Layer,
             Rectangular_Clip_Region_Off_Layer_Ptr
          );

   function Add
            (  Under  : not null access Layer_Location'Class;
               Stream : not null access Root_Stream_Type'Class
            )  return not null access
                      Rectangular_Clip_Region_Off_Layer is
      Ptr : Rectangular_Clip_Region_Off_Layer_Ptr :=
               new Rectangular_Clip_Region_Off_Layer;
   begin
      Restore (Stream.all, Ptr.all);
      Add (Ptr, Under);
      return Ptr;
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add;

   function Add
            (  Under  : not null access Layer_Location'Class;
               Stream : not null access Root_Stream_Type'Class
            )  return not null access
                      Rectangular_Clip_Region_On_Layer is
      Ptr : Rectangular_Clip_Region_On_Layer_Ptr :=
               new Rectangular_Clip_Region_On_Layer;
   begin
      Restore (Stream.all, Ptr.all);
      Add (Ptr, Under);
      return Ptr;
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add;

   procedure Add_Rectangular_Clip_Region
             (  Under          : not null access Layer_Location'Class;
                Height         : GDouble     := 1.0;
                Width          : GDouble     := 1.0;
                Center         : Cairo_Tuple := (0.0, 0.0);
                Rotation_Angle : GDouble     := 0.0;
                Corner_Radius  : GDouble     := 0.0;
                Scaled         : Boolean     := False
             )  is
      On_Ptr  : Rectangular_Clip_Region_On_Layer_Ptr :=
                   new Rectangular_Clip_Region_On_Layer;
      Off_Ptr : Rectangular_Clip_Region_Off_Layer_Ptr :=
                   new Rectangular_Clip_Region_Off_Layer;
      Layer   : Rectangular_Clip_Region_On_Layer renames On_Ptr.all;
   begin
      Layer.Scaled := Scaled;
      Add (On_Ptr, Under);
      Set
      (  Layer          => Layer,
         Height         => Height,
         Width          => Width,
         Center         => Center,
         Rotation_Angle => Rotation_Angle,
         Corner_Radius  => Corner_Radius
      );
      Add (Off_Ptr, Under);
      On_Ptr.Off := Off_Ptr;
      Off_Ptr.On := On_Ptr;
   exception
      when others =>
         Free (On_Ptr);
         Free (Off_Ptr);
         raise;
   end Add_Rectangular_Clip_Region;

   function Add_Rectangular_Clip_Region
            (  Under          : not null access Layer_Location'Class;
               Height         : GDouble     := 1.0;
               Width          : GDouble     := 1.0;
               Center         : Cairo_Tuple := (0.0, 0.0);
               Rotation_Angle : GDouble     := 0.0;
               Corner_Radius  : GDouble     := 0.0;
               Scaled         : Boolean     := False
            )  return not null access
                      Rectangular_Clip_Region_On_Layer is
      On_Ptr  : Rectangular_Clip_Region_On_Layer_Ptr :=
                   new Rectangular_Clip_Region_On_Layer;
      Off_Ptr : Rectangular_Clip_Region_Off_Layer_Ptr :=
                   new Rectangular_Clip_Region_Off_Layer;
      Layer   : Rectangular_Clip_Region_On_Layer renames On_Ptr.all;
   begin
      Layer.Scaled := Scaled;
      Add (On_Ptr, Under);
      Set
      (  Layer          => Layer,
         Height         => Height,
         Width          => Width,
         Center         => Center,
         Rotation_Angle => Rotation_Angle,
         Corner_Radius  => Corner_Radius
      );
      Add (Off_Ptr, Under);
      On_Ptr.Off := Off_Ptr;
      Off_Ptr.On := On_Ptr;
      return Layer'Unchecked_Access;
   exception
      when others =>
         Free (On_Ptr);
         Free (Off_Ptr);
         raise;
   end Add_Rectangular_Clip_Region;

   procedure Draw
             (  Layer   : in out Rectangular_Clip_Region_On_Layer;
                Context : Cairo_Context;
                Area    : Gdk_Rectangle
             )  is
      Half_Width  : GDouble := Layer.Width  / 2.0;
      Half_Height : GDouble := Layer.Height / 2.0;
      Radius      : GDouble := Layer.Radius;
      T           : Translate_And_Rotate;
   begin
      Rotate (T, Layer.Angle);
      Save (Context);
      New_Path (Context);
      if Layer.Scaled then
         declare
            Center : constant Cairo_Tuple := Layer.Widget.Get_Center;
            Size   : constant GDouble     := Layer.Widget.Get_Size;
         begin
            Half_Width  := Half_Width  * Size;
            Half_Height := Half_Height * Size;
            Radius      := Radius * Size;
            Translate
            (  T => T,
               X => Center.X + Layer.Center.X * Size,
               Y => Center.Y + Layer.Center.Y * Size
            );
         end;
      else
         Translate
         (  T => T,
            X => Layer.Center.X,
            Y => Layer.Center.Y
         );
      end if;
      Move_To (Context, T, Radius - Half_Width, -Half_Height);
      Line_To (Context, T, Half_Width - Radius, -Half_Height);
      if Layer.Radius > 0.0 then
         Arc
         (  Context => Context,
            T       => T,
            X       => Half_Width - Radius,
            Y       => Radius - Half_Height,
            Radius  => Radius,
            Angle1  => 3.0 * Pi / 2.0,
            Angle2  => 2.0 * Pi
         );
      end if;
      Line_To (Context, T, Half_Width, Half_Height);
      if Layer.Radius > 0.0 then
         Arc
         (  Context => Context,
            T       => T,
            X       => Half_Width  - Radius,
            Y       => Half_Height - Radius,
            Radius  => Radius,
            Angle1  => 0.0,
            Angle2  => Pi / 2.0
         );
      end if;
      Line_To (Context, T, -Half_Width,  Half_Height);
      if Layer.Radius > 0.0 then
         Arc
         (  Context => Context,
            T       => T,
            X       => Radius - Half_Width,
            Y       => Half_Height - Radius,
            Radius  => Radius,
            Angle1  => Pi / 2.0,
            Angle2  => Pi
         );
      end if;
      Line_To (Context, T, -Half_Width, -Half_Height);
      if Layer.Radius > 0.0 then
         Arc
         (  Context => Context,
            T       => T,
            X       => Radius - Half_Width,
            Y       => Radius - Half_Height,
            Radius  => Radius,
            Angle1  => Pi,
            Angle2  => 3.0 * Pi /2.0
         );
      end if;
      Close_Path (Context);
      Clip (Context);
      Layer.Updated := False;
      Layer.Drawn   := True;
   end Draw;

   procedure Draw
             (  Layer   : in out Rectangular_Clip_Region_Off_Layer;
                Context : Cairo_Context;
                Area    : Gdk_Rectangle
             )  is
   begin
      if Layer.On /= null and then Layer.On.Drawn then
         Layer.On.Drawn := False;
         Restore (Context);
      end if;
   end Draw;

   procedure Finalize
             (  Layer : in out Rectangular_Clip_Region_On_Layer
             )  is
   begin
      if Layer.Off /= null then
         Layer.Off.On := null;
         Free (Layer.Off);
      end if;
      Finalize (Abstract_Layer (Layer));
   end Finalize;

   procedure Finalize
             (  Layer : in out Rectangular_Clip_Region_Off_Layer
             )  is
   begin
      if Layer.On /= null then
         Layer.On.Off := null;
         Layer.On     := null;
      end if;
      Finalize (Abstract_Layer (Layer));
   end Finalize;

   function Get_Center (Layer : Rectangular_Clip_Region_On_Layer)
      return Cairo_Tuple is
   begin
      return Layer.Center;
   end Get_Center;

   function Get_Height (Layer : Rectangular_Clip_Region_On_Layer)
     return GDouble is
   begin
      return Layer.Height;
   end Get_Height;

   function Get_Corner_Radius (Layer : Rectangular_Clip_Region_On_Layer)
      return GDouble is
   begin
      return Layer.Radius;
   end Get_Corner_Radius;

   function Get_Properties_Number
            (  Layer : Rectangular_Clip_Region_On_Layer
            )  return Natural is
   begin
      return
      (  Layer_Property'Pos (Layer_Property'Last)
      -  Layer_Property'Pos (Layer_Property'First)
      +  1
      );
   end Get_Properties_Number;

   function Get_Properties_Number
            (  Layer : Rectangular_Clip_Region_Off_Layer
            )  return Natural is
   begin
      return 0;
   end Get_Properties_Number;

   function Get_Property_Specification
            (  Layer    : Rectangular_Clip_Region_On_Layer;
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
                     Blurb =>
                        "The x-coordinate of the rectangle's center"
                  );
            when Property_Center_Y =>
               return
                  Gnew_Double
                  (  Name    => "y",
                     Nick    => "y",
                     Minimum => GDouble'First,
                     Maximum => GDouble'Last,
                     Default => 0.0,
                     Blurb =>
                        "The y-coordinate of the rectangle's center"
                  );
            when Property_Height =>
               return
                  Gnew_Double
                  (  Name    => "height",
                     Nick    => "height",
                     Minimum => 0.0,
                     Maximum => GDouble'Last,
                     Default => 1.0,
                     Blurb   => "The rectnagle's height"
                  );
            when Property_Width =>
               return
                  Gnew_Double
                  (  Name    => "width",
                     Nick    => "width",
                     Minimum => 0.0,
                     Maximum => GDouble'Last,
                     Default => 1.0,
                     Blurb   => "The rectnagle's width"
                  );
            when Property_Angle =>
               return
                  Gnew_Double
                  (  Name    => "angle",
                     Nick    => "angle",
                     Minimum =>-2.0 * Pi,
                     Maximum => 2.0 * Pi,
                     Default => 0.0,
                     Blurb   => "The angle of the size " &
                                "corresponding to the width of the " &
                                "rectangle"
                  );
            when Property_Radius =>
               return
                  Gnew_Double
                  (  Name    => "corner-r",
                     Nick    => "corner r",
                     Minimum => 0.0,
                     Maximum => GDouble'Last,
                     Default => 0.0,
                     Blurb   => "The radius of the circles rounding " &
                                "the corners of the rectangle"
                  );
            when Property_Scaled =>
               return
                  Gnew_Boolean
                  (  Name    => "scaled",
                     Nick    => "scaled",
                     Default => False,
                     Blurb   => "The region size is changed when " &
                                "the widget is resized"
                  );
         end case;
      end if;
   end Get_Property_Specification;

   function Get_Property_Specification
            (  Layer    : Rectangular_Clip_Region_Off_Layer;
               Property : Positive
            )  return Param_Spec is
      Result : Param_Spec;
   begin
      raise Constraint_Error;
      return Result;
   end Get_Property_Specification;

   function Get_Property_Value
            (  Layer    : Rectangular_Clip_Region_On_Layer;
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
                  Set_Double (Value, Layer.Center.X);
               when Property_Center_Y =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.Center.Y);
               when Property_Height =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.Height);
               when Property_Width =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.Width);
               when Property_Radius =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.Radius);
               when Property_Angle =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.Angle);
               when Property_Scaled =>
                  Init (Value, GType_Boolean);
                  Set_Boolean (Value, Layer.Scaled);
            end case;
            return Value;
         end;
      end if;
   end Get_Property_Value;

   function Get_Property_Value
            (  Layer    : Rectangular_Clip_Region_Off_Layer;
               Property : Positive
            )  return GValue is
      Result : GValue;
   begin
      raise Constraint_Error;
      return Result;
   end Get_Property_Value;

   function Get_Rotation_Angle
            (  Layer : Rectangular_Clip_Region_On_Layer
            )  return GDouble is
   begin
      return Layer.Angle;
   end Get_Rotation_Angle;

   function Get_Scaled (Layer : Rectangular_Clip_Region_On_Layer)
      return Boolean is
   begin
      return Layer.Scaled;
   end Get_Scaled;

   function Get_Width (Layer : Rectangular_Clip_Region_On_Layer)
      return GDouble is
   begin
      return Layer.Width;
   end Get_Width;

   function Is_Updated (Layer : Rectangular_Clip_Region_On_Layer)
      return Boolean is
   begin
      return Layer.Updated;
   end Is_Updated;

   function Is_Updated (Layer : Rectangular_Clip_Region_Off_Layer)
      return Boolean is
   begin
      return False;
   end Is_Updated;

   procedure Move
             (  Layer  : in out Rectangular_Clip_Region_On_Layer;
                Offset : Cairo_Tuple
             )  is
   begin
      Layer.Center.X := Layer.Center.X + Offset.X;
      Layer.Center.Y := Layer.Center.Y + Offset.Y;
      Layer.Updated  := True;
   end Move;

   procedure Remove
             (  Layer : in out Rectangular_Clip_Region_On_Layer
             )  is
   begin
      if Layer.Off /= null then
         Layer.Off.On := null;
         Free (Layer.Off);
      end if;
      Remove (Abstract_Layer (Layer));
   end Remove;

   procedure Restore
             (  Stream : in out Root_Stream_Type'Class;
                Layer  : in out Rectangular_Clip_Region_On_Layer
             )  is
      Height : GDouble;
      Width  : GDouble;
      Center : Cairo_Tuple;
      Angle  : GDouble;
      Radius : GDouble;
   begin
      Restore (Stream, Height);
      Restore (Stream, Width);
      Restore (Stream, Center);
      Restore (Stream, Angle);
      Restore (Stream, Radius);
      Restore (Stream, Layer.Scaled);
      Set
      (  Layer          => Layer,
         Height         => Height,
         Width          => Width,
         Center         => Center,
         Rotation_Angle => Angle,
         Corner_Radius  => Radius
      );
   end Restore;

   procedure Scale
             (  Layer  : in out Rectangular_Clip_Region_On_Layer;
                Factor : GDouble
             )  is
      Radius : constant GDouble := Layer.Radius * Factor;
      Height : constant GDouble := Layer.Height * Factor;
      Width  : constant GDouble := Layer.Width  * Factor;
   begin
      if 2.0 * Radius > Height then
         raise Constraint_Error with
            "Corner radius is greater then the height";
      elsif 2.0 * Radius > Width then
         raise Constraint_Error with
            "Corner radius is greater then the width";
      elsif Height <= 0.0 then
         raise Constraint_Error with "Non-positive rectangle height";
      elsif Width <= 0.0 then
         raise Constraint_Error with "Non-positive rectangle width";
      end if;
      Layer.Height  := Height;
      Layer.Width   := Width;
      Layer.Radius  := Radius;
      Layer.Updated := True;
   end Scale;

   procedure Set
             (  Layer   : in out Rectangular_Clip_Region_On_Layer;
                Height         : GDouble;
                Width          : GDouble;
                Center         : Cairo_Tuple;
                Rotation_Angle : GDouble;
                Corner_Radius  : GDouble
             )  is
   begin
      if 2.0 * Corner_Radius > Height then
         raise Constraint_Error with
            "Corner radius is greater then the height";
      elsif 2.0 * Corner_Radius > Width then
         raise Constraint_Error with
            "Corner radius is greater then the width";
      elsif Height <= 0.0 then
         raise Constraint_Error with "Non-positive rectangle height";
      elsif Width <= 0.0 then
         raise Constraint_Error with "Non-positive rectangle width";
      end if;
      Layer.Height  := Height;
      Layer.Width   := Width;
      Layer.Center  := Center;
      Layer.Angle   := Rotation_Angle;
      Layer.Radius  := Corner_Radius;
      Layer.Updated := True;
   end Set;

   procedure Set_Property_Value
             (  Layer    : in out Rectangular_Clip_Region_On_Layer;
                Property : Positive;
                Value    : GValue
             )  is
   begin
      if Property > Get_Properties_Number (Layer) then
         raise Constraint_Error;
      else
         case Layer_Property'Val (Property - 1) is
            when Property_Center_X =>
               Layer.Center.X := Get_Double (Value);
            when Property_Center_Y =>
               Layer.Center.Y := Get_Double (Value);
            when Property_Height =>
               Layer.Height := Get_Double (Value);
               if Layer.Height < 0.0 then
                  Layer.Height := 0.0;
               end if;
            when Property_Width =>
               Layer.Width := Get_Double (Value);
               if Layer.Width < 0.0 then
                  Layer.Width := 0.0;
               end if;
            when Property_Radius =>
               Layer.Radius := Get_Double (Value);
               if Layer.Radius < 0.0 then
                  Layer.Radius := 0.0;
               elsif 2.0 * Layer.Radius > Layer.Height then
                  Layer.Radius := Layer.Height / 2.0;
               elsif 2.0 * Layer.Radius > Layer.Width then
                  Layer.Radius := Layer.Width / 2.0;
               end if;
            when Property_Angle =>
               Layer.Angle := Get_Double (Value);
               if Layer.Angle not in -2.0 * Pi..2.0 * Pi then
                  Layer.Angle :=
                     GDouble'Remainder (Layer.Angle, 2.0 * Pi);
               end if;
            when Property_Scaled =>
               Layer.Scaled := Get_Boolean (Value);
         end case;
      end if;
      Layer.Updated := True;
   end Set_Property_Value;

   procedure Set_Property_Value
             (  Layer    : in out Rectangular_Clip_Region_Off_Layer;
                Property : Positive;
                Value    : GValue
             )  is
   begin
      raise Constraint_Error;
   end Set_Property_Value;

   procedure Set_Scaled
             (  Layer  : in out Rectangular_Clip_Region_On_Layer;
                Scaled : Boolean
             )  is
   begin
      Layer.Scaled  := Scaled;
      Layer.Updated := True;
   end Set_Scaled;

   procedure Store
             (  Stream : in out Root_Stream_Type'Class;
                Layer  : Rectangular_Clip_Region_On_Layer
             )  is
   begin
      Store (Stream, Layer.Height);
      Store (Stream, Layer.Width);
      Store (Stream, Layer.Center);
      Store (Stream, Layer.Angle);
      Store (Stream, Layer.Radius);
      Store (Stream, Layer.Drawn, Layer.Scaled);
   end Store;

end Gtk.Layered.Rectangular_Clip_Region;
