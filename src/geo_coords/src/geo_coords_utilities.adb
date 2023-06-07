with Ada.Text_IO;
with Ada.Numerics;

package body Geo_Coords_Utilities is

   function To_Radians (Degree_Value : Float_Digits_15)
                        return Float_Digits_15 is
   begin
      return Degree_Value * (Ada.Numerics.Pi / 180.0);
   end To_Radians;

   function To_Degrees (Radian_Value : Float_Digits_15)
                        return Float_Digits_15 is
   begin
      return Radian_Value * (180.0 / Ada.Numerics.Pi);
   end To_Degrees;

   function Geo_Location_To_Radians (Lat_Lon_Degrees : Geo_Location)
                                     return Geo_Location is
   begin
      return (Latitude => To_Radians (Lat_Lon_Degrees.Latitude),
              Longitude => To_Radians (Lat_Lon_Degrees.Longitude));
   end Geo_Location_To_Radians;

   function Geo_Location_To_Degrees (Lat_Lon_Radians : Geo_Location)
                                     return Geo_Location is
   begin
      return (Latitude => To_Degrees (Lat_Lon_Radians.Latitude),
              Longitude => To_Degrees (Lat_Lon_Radians.Longitude));
   end Geo_Location_To_Degrees;

   procedure Put_Float_Digits_15 (Value_String : String;
                                  Float_Digits_15_Value : Float_Digits_15) is
   begin
      Ada.Text_IO.Put (Value_String & " = ");
      Float_Digits_15_IO.Put (Item => Float_Digits_15_Value, Exp => 0);
      if Value_String = "Latitude" or else Value_String = "Longitude" then
         Ada.Text_IO.Put (" (Decimal Degrees)");
      elsif Value_String = "Distance" then
         Ada.Text_IO.Put (" (Metres)");
      elsif Value_String = "Bearing" then
         Ada.Text_IO.Put (" (Degrees)");
      end if;
      Ada.Text_IO.New_Line;
   end Put_Float_Digits_15;

   function Get_Float_Digits_15 (Value_String : String)
                                 return Float_Digits_15 is
   begin
      Ada.Text_IO.Put_Line ("Enter the " & Value_String & " Value:");
      return Float_Digits_15'Value (Ada.Text_IO.Get_Line);
   end Get_Float_Digits_15;

   procedure Put_Distance_Bearing (Dest_Bearing : Distance_Bearing_type) is
   begin
      Put_Float_Digits_15 (Value_String => "Distance",
                           Float_Digits_15_Value => Dest_Bearing.Distance);

      Put_Float_Digits_15 (Value_String => "Bearing",
                           Float_Digits_15_Value => Dest_Bearing.Bearing);

   end Put_Distance_Bearing;

   procedure Put_Destination_Location (Geo_Loc : Geo_Location) is
   begin
      Put_Float_Digits_15 (Value_String => "Latitude",
                           Float_Digits_15_Value => Geo_Loc.Latitude);

      Put_Float_Digits_15 (Value_String => "Longitude",
                           Float_Digits_15_Value => Geo_Loc.Longitude);
   end Put_Destination_Location;

   function Get_Input_Options return Integer is
   begin
      Ada.Text_IO.Put_Line ("Input Options:");
      Ada.Text_IO.Put_Line ("Enter 1 for Calculating Distance and Bearing");
      Ada.Text_IO.Put_Line ("Enter 2 for Calculating Destination Location");
      return Integer'Value (Ada.Text_IO.Get_Line);
   exception
      when Constraint_Error =>
         return 0;
   end Get_Input_Options;

   procedure Put_Invalid_Input_Option is
   begin
      Ada.Text_IO.Put_Line ("Please Enter Valid Input!!!");
   end Put_Invalid_Input_Option;

end Geo_Coords_Utilities;