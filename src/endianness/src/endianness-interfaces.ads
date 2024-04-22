with Interfaces; use Interfaces;

package Endianness.Interfaces with
   Pure,
   Preelaborate,
   SPARK_Mode => On
is
   --  @summary
   --  Instance of Endianness' functions for Interfaces' integer types

   function Swap_Endian is new Endianness.Swap_Endian (Integer_8);
   function Swap_Endian is new Endianness.Swap_Endian (Integer_16);
   function Swap_Endian is new Endianness.Swap_Endian (Integer_32);
   function Swap_Endian is new Endianness.Swap_Endian (Integer_64);
   function Swap_Endian is new Endianness.Swap_Endian (Unsigned_8);
   function Swap_Endian is new Endianness.Swap_Endian (Unsigned_16);
   function Swap_Endian is new Endianness.Swap_Endian (Unsigned_32);
   function Swap_Endian is new Endianness.Swap_Endian (Unsigned_64);

   function Native_To_Big_Endian is new Endianness.Native_To_Big_Endian
     (Integer_8);
   function Native_To_Big_Endian is new Endianness.Native_To_Big_Endian
     (Integer_16);
   function Native_To_Big_Endian is new Endianness.Native_To_Big_Endian
     (Integer_32);
   function Native_To_Big_Endian is new Endianness.Native_To_Big_Endian
     (Integer_64);
   function Native_To_Big_Endian is new Endianness.Native_To_Big_Endian
     (Unsigned_8);
   function Native_To_Big_Endian is new Endianness.Native_To_Big_Endian
     (Unsigned_16);
   function Native_To_Big_Endian is new Endianness.Native_To_Big_Endian
     (Unsigned_32);
   function Native_To_Big_Endian is new Endianness.Native_To_Big_Endian
     (Unsigned_64);

   function Native_To_Little_Endian is new Endianness.Native_To_Little_Endian
     (Integer_8);
   function Native_To_Little_Endian is new Endianness.Native_To_Little_Endian
     (Integer_16);
   function Native_To_Little_Endian is new Endianness.Native_To_Little_Endian
     (Integer_32);
   function Native_To_Little_Endian is new Endianness.Native_To_Little_Endian
     (Integer_64);
   function Native_To_Little_Endian is new Endianness.Native_To_Little_Endian
     (Unsigned_8);
   function Native_To_Little_Endian is new Endianness.Native_To_Little_Endian
     (Unsigned_16);
   function Native_To_Little_Endian is new Endianness.Native_To_Little_Endian
     (Unsigned_32);
   function Native_To_Little_Endian is new Endianness.Native_To_Little_Endian
     (Unsigned_64);

   function Big_Endian_To_Native is new Endianness.Big_Endian_To_Native
     (Integer_8);
   function Big_Endian_To_Native is new Endianness.Big_Endian_To_Native
     (Integer_16);
   function Big_Endian_To_Native is new Endianness.Big_Endian_To_Native
     (Integer_32);
   function Big_Endian_To_Native is new Endianness.Big_Endian_To_Native
     (Integer_64);
   function Big_Endian_To_Native is new Endianness.Big_Endian_To_Native
     (Unsigned_8);
   function Big_Endian_To_Native is new Endianness.Big_Endian_To_Native
     (Unsigned_16);
   function Big_Endian_To_Native is new Endianness.Big_Endian_To_Native
     (Unsigned_32);
   function Big_Endian_To_Native is new Endianness.Big_Endian_To_Native
     (Unsigned_64);

   function Little_Endian_To_Native is new Endianness.Little_Endian_To_Native
     (Integer_8);
   function Little_Endian_To_Native is new Endianness.Little_Endian_To_Native
     (Integer_16);
   function Little_Endian_To_Native is new Endianness.Little_Endian_To_Native
     (Integer_32);
   function Little_Endian_To_Native is new Endianness.Little_Endian_To_Native
     (Integer_64);
   function Little_Endian_To_Native is new Endianness.Little_Endian_To_Native
     (Unsigned_8);
   function Little_Endian_To_Native is new Endianness.Little_Endian_To_Native
     (Unsigned_16);
   function Little_Endian_To_Native is new Endianness.Little_Endian_To_Native
     (Unsigned_32);
   function Little_Endian_To_Native is new Endianness.Little_Endian_To_Native
     (Unsigned_64);
end Endianness.Interfaces;
