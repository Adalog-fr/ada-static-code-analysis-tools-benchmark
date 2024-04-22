with Geo_Coords_Types; use Geo_Coords_Types;
with Geo_Coords_Utilities; use Geo_Coords_Utilities;
with Geo_Coords; use Geo_Coords;

procedure Lat_Lon_Calculation is
   Home_Loc, Dest_Loc : Geo_Location;
   Destination_Bearing : Distance_Bearing_type;
begin
   Home_Loc := (Latitude => 51.502310,
                Longitude => -0.097335);

   Destination_Bearing := (Distance => 102.14937,
                           Bearing => 84.50248);

   Dest_Loc := (Destination_Location (Home => Home_Loc,
                                      Dist_Bearing => Destination_Bearing));

   Put_Destination_Location (Geo_Loc => Dest_Loc);

end Lat_Lon_Calculation;
