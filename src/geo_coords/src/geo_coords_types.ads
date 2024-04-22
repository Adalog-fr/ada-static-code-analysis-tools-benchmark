with Interfaces;
with Ada.Text_IO;
with Ada.Numerics.Generic_Elementary_Functions;

--  Contains all the required types

package Geo_Coords_Types is

   type Float_Digits_15 is new
      Interfaces.IEEE_Float_64;

   package Math is new
      Ada.Numerics.Generic_Elementary_Functions (Float_Digits_15);

   package Float_Digits_15_IO is new
      Ada.Text_IO.Float_IO (Float_Digits_15);

   type Geo_Location is record
      Latitude : Float_Digits_15;
      Longitude : Float_Digits_15;
   end record;

   type Distance_Bearing_type is record
      Distance : Float_Digits_15;  --  Distance in Metres
      Bearing : Float_Digits_15;  -- Bearing in Degrees
   end record;

   --  Radius of Earth (m)
   Radius_Of_Earth : constant Float_Digits_15 := 6.371E6;

end Geo_Coords_Types;