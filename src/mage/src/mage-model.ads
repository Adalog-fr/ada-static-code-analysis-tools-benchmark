with Ada.Numerics.Generic_Elementary_Functions;

package Mage.Model is

   --  Operations used for modeling: moving pixels around on the screen
   --  or on a 3d model which will then be rendered on the screen.

   --  This package is self-sufficient, its API does not depend on render
   --  engine, it's only focused on providing pure operations, typically
   --  on vectors.

   type Point_3d is record
      X : Float;
      Y : Float;
      Z : Float;
   end record;

   type Screen_Point is record
      X, Y : Integer;
   end record;

   package Float_Elementary_Functions is new Ada.Numerics
     .Generic_Elementary_Functions
     (Float);

   -----------------------
   -- Vector operations --
   -----------------------

   --  Comparison, only for Screen Point
   --  Be careful: ordering is incomplete!

   function ">" (A, B : Screen_Point) return Boolean is
     (A.X > B.X and then A.Y > B.Y);

   function ">=" (A, B : Screen_Point) return Boolean is
     (A.X >= B.X and then A.Y >= B.Y);

   function "<" (A, B : Screen_Point) return Boolean is
     (A.X < B.X and then A.Y < B.Y);

   function "<=" (A, B : Screen_Point) return Boolean is
     (A.X <= B.X and then A.Y <= B.Y);

   --  Arithmetics

   function "+" (A, B : Screen_Point) return Screen_Point is
     (A.X + B.X, A.Y + B.Y);

   function "+" (A, B : Point_3d) return Point_3d is
     (A.X + B.X, A.Y + B.Y, A.Z + B.Z);

   function "+" (A : Point_3d) return Point_3d is (A.X, A.Y, A.Z);

   function "-" (A, B : Screen_Point) return Screen_Point is
     (A.X - B.X, A.Y - B.Y) with
     Pre => A >= B;

   function "-" (A, B : Point_3d) return Point_3d is
     (A.X - B.X, A.Y - B.Y, A.Z - B.Z);

   function "-" (A : Point_3d) return Point_3d is (-A.X, -A.Y, -A.Z);

   function "*" (A : Screen_Point; F : Natural) return Screen_Point is
     (A.X * F, A.Y * F);

   function "*" (A : Point_3d; F : Float) return Point_3d is
     (A.X * F, A.Y * F, A.Z * F);

   function "/" (A : Screen_Point; F : Positive) return Screen_Point is
     (A.X / F, A.Y / F);

   function "/" (A : Point_3d; F : Float) return Point_3d is
     (A.X / F, A.Y / F, A.Z / F) with
     Pre => F /= 0.0;

   --  Norm (abs)

   function "abs" (A : Screen_Point) return Float is
     (Float_Elementary_Functions.Sqrt (Float (A.X**2 + A.Y**2)));

   function "abs" (A : Point_3d) return Float is
     (Float_Elementary_Functions.Sqrt (A.X**2 + A.Y**2 + A.Z**2));

   --  Conversion operations

   function Extension (P : Screen_Point; Z : Float := 0.0) return Point_3d is
     (Float (P.X), Float (P.Y), Z);
   --  2d screen position extended to a point in space, at the given Z

   function Projection (P : Point_3d) return Screen_Point is
     (Integer (P.X), Integer (P.Y));
   --  Projection of the point in space to screen

   ---------------
   -- Constants --
   ---------------

   Origin        : constant Point_3d := (0.0, 0.0, 0.0);
   Null_Point_3d : constant Point_3d := (0.0, 0.0, 0.0);
   X_Axis        : constant Point_3d := (1.0, 0.0, 0.0);
   --  Y goes in reverse: max Y is the top
   Y_Axis        : constant Point_3d := (0.0, -1.0, 0.0);
   Z_Axis        : constant Point_3d := (0.0, 0.0, 1.0);

end Mage.Model;
