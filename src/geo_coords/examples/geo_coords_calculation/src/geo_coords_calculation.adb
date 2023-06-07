with Geo_Coords_Types; use Geo_Coords_Types;
with Geo_Coords_Utilities; use Geo_Coords_Utilities;
with Geo_Coords; use Geo_Coords;

procedure Geo_Coords_Calculation is
   Home_Loc, Dest_Loc : Geo_Location;
   Dist_Bear_Input : Distance_Bearing_type;
   Input : Integer;
begin

   Input := Get_Input_Options;

   case Input is
      when 1 =>
         Home_Loc := (Latitude =>
                        Get_Float_Digits_15 (Value_String => "Latitude"),
                     Longitude =>
                        Get_Float_Digits_15 (Value_String => "Longitude"));

         Dest_Loc := (Latitude =>
                        Get_Float_Digits_15 (Value_String => "Latitude"),
                     Longitude =>
                        Get_Float_Digits_15 (Value_String => "Longitude"));

         Put_Distance_Bearing (Dest_Bearing =>
                                 Distance_Bearing (Home => Home_Loc,
                                                   Destination => Dest_Loc));
      when 2 =>
         Home_Loc := (Latitude =>
                        Get_Float_Digits_15 (Value_String => "Latitude"),
                     Longitude =>
                        Get_Float_Digits_15 (Value_String => "Longitude"));

         Dist_Bear_Input := (Distance =>
                              Get_Float_Digits_15 (Value_String => "Distance"),
                             Bearing =>
                              Get_Float_Digits_15 (Value_String => "Bearing"));

         Put_Destination_Location (Geo_Loc =>
                                    Destination_Location (Home =>
                                                            Home_Loc,
                                                          Dist_Bearing =>
                                                            Dist_Bear_Input));
      when others =>
         Put_Invalid_Input_Option;
   end case;

end Geo_Coords_Calculation;