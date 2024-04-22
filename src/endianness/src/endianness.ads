with Ada.Streams; use Ada.Streams;

package Endianness with
   Pure,
   Preelaborate,
   SPARK_Mode => On
is
   --  @summary
   --  Convenience subprograms to convert between Big- and Little-endianness
   --
   --  @description
   --  This package contains a few convenient subprograms that allow you to
   --  simply switch between native endianness of the running system and big
   --  or little endianness. Ada's byte order handling is quite sophisticated
   --  and it doesn't provide such routines at all. Hopefully, there is a
   --  GNAT.Byte_Swapping available in GNAT, but it only allows for simple
   --  swapping of bytes.
   --  This package is built on top of GNAT.Byte_Swapping, but provides much
   --  simpler programming interface.
   --
   --  This package provides generic function interfaces, but mostly you would
   --  be better off utilizing already instantiated Endiannes.Standard and
   --  Endianness.Interfaces packages which initialize all of the provided
   --  functions for compatible types from Standard and Interfaces packages,
   --  respectively.

   generic
      type Source is (<>);
      --  Input source type
   function Swap_Endian (Value : Source) return Source with
      Global => null,
      Pre => Source'Size = 8 or else Source'Size = 16 or else Source'Size = 32
      or else Source'Size = 64;
      --  Convert from one endianness to another.

   generic
      type Source is (<>);
      --  Input source types
   function Native_To_Big_Endian
     (Value : Source) return Stream_Element_Array with
      Global => null,
      Pre => Source'Size = 8 or else Source'Size = 16 or else Source'Size = 32
      or else Source'Size = 64,
      Post => Native_To_Big_Endian'Result'Length =
      Source'Max_Size_In_Storage_Elements;
      --  Convert Value from native endianness into an array of bytes, ordered
      --  as in big endian.

   generic
      type Source is (<>);
   function Native_To_Little_Endian
     (Value : Source) return Stream_Element_Array with
      Global => null,
      Pre => Source'Size = 8 or else Source'Size = 16 or else Source'Size = 32
      or else Source'Size = 64,
      Post => Native_To_Little_Endian'Result'Length =
      Source'Max_Size_In_Storage_Elements;
      --  Convert Value from native endianness into an array of bytes, ordered
      --  as in little endian.

   generic
      type Target is (<>);
   function Big_Endian_To_Native
     (Value : Stream_Element_Array) return Target with
      Global => null,
      Pre    =>
      (Target'Size = 8 or else Target'Size = 16 or else Target'Size = 32
       or else Target'Size = 64)
      and then Value'Length = Target'Max_Size_In_Storage_Elements;
      --  Convert Value from an array of bytes into a native value, order
      --  of input bytes is assumed to be in big endian

   generic
      type Target is (<>);
   function Little_Endian_To_Native
     (Value : Stream_Element_Array) return Target with
      Global => null,
      Pre    =>
      (Target'Size = 8 or else Target'Size = 16 or else Target'Size = 32
       or else Target'Size = 64)
      and then Value'Length = Target'Max_Size_In_Storage_Elements;
      --  Convert Value from an array of bytes into a native value, order
      --  of input bytes is assumed to be in little endian
end Endianness;
