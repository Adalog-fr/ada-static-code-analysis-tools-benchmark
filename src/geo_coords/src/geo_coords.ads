with Geo_Coords_Types; use Geo_Coords_Types;

package Geo_Coords is

   --  Function Distance_Bearing_Calculation returns the distance and
   --  bearing between two geographic locations (home and destination).
   --  @param Home in Decimal Degrees
   --  @param Destination in Decimal Degrees
   function Distance_Bearing (Home : Geo_Location;
                              Destination : Geo_Location)
                              return Distance_Bearing_type;

   --  Function Destination_Location returns the destination's location
   --  from home location by using the distance and bearing between
   --  two locations.
   --  @param Home in Decimal Degrees
   --  @param Dist_Bearing in Distance_Bearing_type (Distance and Bearing)
   function Destination_Location (Home : Geo_Location;
                                  Dist_Bearing : Distance_Bearing_type)
                                  return Geo_Location;

end Geo_Coords;