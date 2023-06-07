with Geo_Coords_Utilities; use Geo_Coords_Utilities;

package body Geo_Coords is

   function Distance_Bearing (Home : Geo_Location;
                              Destination : Geo_Location)
                              return Distance_Bearing_type is

      Home_Radians, Destination_Radians, Difference_Radians : Geo_Location;
      A, C, X, Y : Float_Digits_15;

   begin
      Home_Radians := Geo_Location_To_Radians (Home);
      Destination_Radians := Geo_Location_To_Radians (Destination);
      Difference_Radians := Geo_Location_To_Radians (
                              (Destination.Latitude - Home.Latitude,
                               Destination.Longitude - Home.Longitude));

      A := (Math.Sin (Difference_Radians.Latitude / 2.0) *
            Math.Sin (Difference_Radians.Latitude / 2.0)) +
           (Math.Cos (Home_Radians.Latitude) *
            Math.Cos (Destination_Radians.Latitude) *
           (Math.Sin (Difference_Radians.Longitude / 2.0) *
            Math.Sin (Difference_Radians.Longitude / 2.0)));

      C := 2.0 * Math.Arctan (Math.Sqrt (A), Math.Sqrt (1.0 - A));

      Y := Math.Sin (Difference_Radians.Longitude) *
           Math.Cos (Destination_Radians.Latitude);

      X := Math.Cos (Home_Radians.Latitude) *
           Math.Sin (Destination_Radians.Latitude) -
           Math.Sin (Home_Radians.Latitude) *
           Math.Cos (Destination_Radians.Latitude) *
           Math.Cos (Difference_Radians.Longitude);
      if (Home_Radians = Destination_Radians) = False then
         return (Radius_Of_Earth * C, To_Degrees (Math.Arctan (Y, X)));
      else
         return (0.0, 0.0);
      end if;
   end Distance_Bearing;

   function Destination_Location (Home : Geo_Location;
                                  Dist_Bearing : Distance_Bearing_type)
                                  return Geo_Location is

      Home_Radians, Dest_Radians : Geo_Location;
      Bearing_Radians : Float_Digits_15;

   begin
      Home_Radians := Geo_Location_To_Radians (Home);
      Bearing_Radians := To_Radians (Dist_Bearing.Bearing);

      Dest_Radians.Latitude := Math.Arcsin (Math.Sin (Home_Radians.Latitude) *
                                            Math.Cos (Dist_Bearing.Distance /
                                                      Radius_Of_Earth) +
                                            Math.Cos (Home_Radians.Latitude) *
                                            Math.Sin (Dist_Bearing.Distance /
                                                      Radius_Of_Earth) *
                                            Math.Cos (Bearing_Radians));

      Dest_Radians.Longitude := Home_Radians.Longitude +
                                Math.Arctan (Math.Sin (Bearing_Radians) *
                                             Math.Sin (Dist_Bearing.Distance /
                                                       Radius_Of_Earth) *
                                             Math.Cos (Home_Radians.Latitude),
                                             Math.Cos (Dist_Bearing.Distance /
                                                       Radius_Of_Earth) -
                                             Math.Sin (Home_Radians.Latitude) *
                                             Math.Sin (Dest_Radians.Latitude));

      return Geo_Location_To_Degrees (Dest_Radians);
   end Destination_Location;
end Geo_Coords;