--  REST API Validation
--  API to validate
--
--  The version of the OpenAPI document: 1.0.0
--  Contact: Stephane.Carrez@gmail.com
--
--  NOTE: This package is auto generated by OpenAPI-Generator 6.1.0-2022-07-31.
--  https://openapi-generator.tech
--  Do not edit the class manually.

pragma Warnings (Off, "*is not referenced");
with Swagger.Streams;
package body TestBinary.Clients is
   pragma Style_Checks ("-bmrIu");

   Mime_1       : aliased constant String    := "image/jpeg";
   Mime_2       : aliased constant String    := "image/png";
   Media_List_1 : constant Swagger.Mime_List :=
     (1 => Mime_1'Access,
2       => Mime_2'Access);
   Media_List_2 : constant Swagger.Mime_List :=
     (1 => Swagger.Mime_Json,
2       => Swagger.Mime_Xml);

   --  Get an image
   --  Get an image
   procedure Do_Get_Image
     (Client : in out Client_Type;
      Status : in     TestBinary.Models.Status_Type;
      Owner  : in     Swagger.Nullable_UString;
      Result :    out Swagger.Blob_Ref)
   is
      URI   : Swagger.Clients.URI_Type;
      Reply : Swagger.Value_Type;
   begin
      Client.Set_Accept (Media_List_1);

      URI.Add_Param ("status", TestBinary.Models.To_String (Status));
      URI.Add_Param ("owner", Owner);
      URI.Set_Path ("/binary");
      Client.Call (Swagger.Clients.GET, URI, Reply);
      Swagger.Streams.Deserialize (Reply, "", Result);
   end Do_Get_Image;

   --  Get some stat from external struct
   procedure Do_Get_Stats
     (Client : in out Client_Type;
      Status : in     TestBinary.Models.Status_Type;
      Result :    out External.Stat_Vector)
   is
      URI   : Swagger.Clients.URI_Type;
      Reply : Swagger.Value_Type;
   begin
      Client.Set_Accept (Media_List_2);

      URI.Set_Path ("/external/{status}");
      URI.Set_Path_Param ("status", TestBinary.Models.To_String (Status));
      Client.Call (Swagger.Clients.GET, URI, Reply);
      TestBinary.Models.Deserialize (Reply, "", Result);
   end Do_Get_Stats;
end TestBinary.Clients;