with Geo_Coords_Types; use Geo_Coords_Types;

--  Contains all the required utilties (Functions/Procedures)

package Geo_Coords_Utilities is

   function To_Radians (Degree_Value : Float_Digits_15)
                        return Float_Digits_15;

   function To_Degrees (Radian_Value : Float_Digits_15)
                        return Float_Digits_15;

   function Geo_Location_To_Radians (Lat_Lon_Degrees : Geo_Location)
                                     return Geo_Location;

   function Geo_Location_To_Degrees (Lat_Lon_Radians : Geo_Location)
                                     return Geo_Location;

   procedure Put_Float_Digits_15 (Value_String : String;
                                  Float_Digits_15_Value : Float_Digits_15);

   function Get_Float_Digits_15 (Value_String : String)
                                 return Float_Digits_15;

   procedure Put_Distance_Bearing (Dest_Bearing : Distance_Bearing_type);

   procedure Put_Destination_Location (Geo_Loc : Geo_Location);

   function Get_Input_Options return Integer;

   procedure Put_Invalid_Input_Option;

end Geo_Coords_Utilities;