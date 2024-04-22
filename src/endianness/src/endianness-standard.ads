package Endianness.Standard with
   Pure,
   Preelaborate,
   SPARK_Mode => On
is
   --  @summary
   --  Instance of Endianness' functions for Standard integer types

   function Swap_Endian is new Endianness.Swap_Endian (Short_Short_Integer);
   function Swap_Endian is new Endianness.Swap_Endian (Short_Integer);
   function Swap_Endian is new Endianness.Swap_Endian (Integer);
   function Swap_Endian is new Endianness.Swap_Endian (Long_Integer);
   function Swap_Endian is new Endianness.Swap_Endian (Long_Long_Integer);

   function Native_To_Big_Endian is new Endianness.Native_To_Big_Endian
     (Short_Short_Integer);
   function Native_To_Big_Endian is new Endianness.Native_To_Big_Endian
     (Short_Integer);
   function Native_To_Big_Endian is new Endianness.Native_To_Big_Endian
     (Integer);
   function Native_To_Big_Endian is new Endianness.Native_To_Big_Endian
     (Long_Integer);
   function Native_To_Big_Endian is new Endianness.Native_To_Big_Endian
     (Long_Long_Integer);

   function Native_To_Little_Endian is new Endianness.Native_To_Little_Endian
     (Short_Short_Integer);
   function Native_To_Little_Endian is new Endianness.Native_To_Little_Endian
     (Short_Integer);
   function Native_To_Little_Endian is new Endianness.Native_To_Little_Endian
     (Integer);
   function Native_To_Little_Endian is new Endianness.Native_To_Little_Endian
     (Long_Integer);
   function Native_To_Little_Endian is new Endianness.Native_To_Little_Endian
     (Long_Long_Integer);

   function Big_Endian_To_Native is new Endianness.Big_Endian_To_Native
     (Short_Short_Integer);
   function Big_Endian_To_Native is new Endianness.Big_Endian_To_Native
     (Short_Integer);
   function Big_Endian_To_Native is new Endianness.Big_Endian_To_Native
     (Integer);
   function Big_Endian_To_Native is new Endianness.Big_Endian_To_Native
     (Long_Integer);
   function Big_Endian_To_Native is new Endianness.Big_Endian_To_Native
     (Long_Long_Integer);

   function Little_Endian_To_Native is new Endianness.Little_Endian_To_Native
     (Short_Short_Integer);
   function Little_Endian_To_Native is new Endianness.Little_Endian_To_Native
     (Short_Integer);
   function Little_Endian_To_Native is new Endianness.Little_Endian_To_Native
     (Integer);
   function Little_Endian_To_Native is new Endianness.Little_Endian_To_Native
     (Long_Integer);
   function Little_Endian_To_Native is new Endianness.Little_Endian_To_Native
     (Long_Long_Integer);
end Endianness.Standard;
