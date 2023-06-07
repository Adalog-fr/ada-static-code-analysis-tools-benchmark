with Geo_Coords_Types; use Geo_Coords_Types;
with Geo_Coords_Utilities; use Geo_Coords_Utilities;
with Geo_Coords; use Geo_Coords;

procedure Distance_Bearing_Calculation is
   Home_Loc, Dest_Loc : Geo_Location;
   Destination_Bearing : Distance_Bearing_type;
begin
   Home_Loc := (Latitude => 51.502310,
                Longitude => -0.097335);

   Dest_Loc := (Latitude => 51.502398,
                Longitude => -0.095866);

   Destination_Bearing := Distance_Bearing (Home => Home_Loc,
                                            Destination => Dest_Loc);

   Put_Distance_Bearing (Destination_Bearing);

end Distance_Bearing_Calculation;
