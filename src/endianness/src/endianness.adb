pragma Ada_2012;

with Ada.Unchecked_Conversion;
with GNAT.Byte_Swapping;
with System; use System;

package body Endianness is
   pragma Compile_Time_Error
     (Stream_Element'Modulus /= 2**8,
      "'Stream_Element' type must be mod 2**8, i.e. represent a byte");

   generic
      type Source is (<>);
   function Split_Into_Bytes (Value : Source) return Stream_Element_Array with
      Global => null,
      Pre => Source'Size = 8 or else Source'Size = 16 or else Source'Size = 32
      or else Source'Size = 64,
      Post => Split_Into_Bytes'Result'Length =
      Source'Max_Size_In_Storage_Elements;

   generic
      type Target is (<>);
   function Assemble_From_Bytes
     (Value : Stream_Element_Array) return Target with
      Global => null,
      Pre    =>
      (Target'Size = 8 or else Target'Size = 16 or else Target'Size = 32
       or else Target'Size = 64)
      and then Value'Length = Target'Max_Size_In_Storage_Elements;

   function Swap_Endian (Value : Source) return Source is
      use GNAT.Byte_Swapping;
   begin
      pragma Warnings
        (Off, "types for unchecked conversion have different sizes",
         Reason =>
           "We are dynamically choosing different SwappedN implementations," &
           " there is no way to actually perform a conversion on types with" &
           " different sizes.");
      case Source'Max_Size_In_Storage_Elements is
         when 1 =>
            return Value;
         when 2 =>
            declare
               function Swap is new Swapped2 (Source);
            begin
               return Swap (Value);
            end;
         when 4 =>
            declare
               function Swap is new Swapped4 (Source);
            begin
               return Swap (Value);
            end;
         when 8 =>
            declare
               function Swap is new Swapped8 (Source);
            begin
               return Swap (Value);
            end;
         when others =>
            raise Constraint_Error
              with "`Source'Max_Size_In_Storage_Elements` is not" &
              " 1, 2, 4 or 8 bytes.";
      end case;
      pragma Warnings
        (On, "types for unchecked conversion have different sizes");
   end Swap_Endian;

   function Native_To_Big_Endian (Value : Source) return Stream_Element_Array
   is
      function Swap is new Swap_Endian (Source);
      function Split is new Split_Into_Bytes (Source);
   begin
      return
        Split
          (if Default_Bit_Order /= High_Order_First then Swap (Value)
           else Value);
   end Native_To_Big_Endian;

   function Native_To_Little_Endian
     (Value : Source) return Stream_Element_Array
   is
      function Swap is new Swap_Endian (Source);
      function Split is new Split_Into_Bytes (Source);
   begin
      return
        Split
          (if Default_Bit_Order /= Low_Order_First then Swap (Value)
           else Value);
   end Native_To_Little_Endian;

   function Big_Endian_To_Native (Value : Stream_Element_Array) return Target
   is
      function Swap is new Swap_Endian (Target);
      function Assemble is new Assemble_From_Bytes (Target);
   begin
      return
        (if Default_Bit_Order /= High_Order_First then Swap (Assemble (Value))
         else Assemble (Value));
   end Big_Endian_To_Native;

   function Little_Endian_To_Native
     (Value : Stream_Element_Array) return Target
   is
      function Swap is new Swap_Endian (Target);
      function Assemble is new Assemble_From_Bytes (Target);
   begin
      return
        (if Default_Bit_Order /= Low_Order_First then Swap (Assemble (Value))
         else Assemble (Value));
   end Little_Endian_To_Native;

   function Split_Into_Bytes (Value : Source) return Stream_Element_Array is
      subtype Output_Type is
        Stream_Element_Array (0 .. Source'Max_Size_In_Storage_Elements - 1);

      function Convert is new Ada.Unchecked_Conversion (Source, Output_Type);
      pragma Annotate
        (GNATprove, Intentional,
         "types used for unchecked conversion do not have the same size",
         "We correctly define Output_Type");
      pragma Annotate
        (GNATprove, Intentional,
         "type is unsuitable as a target for unchecked conversion", "ditto");
   begin
      if Source'Size /= 8 and then Source'Size /= 16 and then Source'Size /= 32
        and then Source'Size /= 64
      then
         raise Constraint_Error with "Source'Size must be 8, 16, 32, or 64";
      end if;
      return Convert (Value);
   end Split_Into_Bytes;

   function Assemble_From_Bytes (Value : Stream_Element_Array) return Target is
      pragma Assert (Value'Length = Target'Max_Size_In_Storage_Elements);

      function Convert is new Ada.Unchecked_Conversion
        (Stream_Element_Array, Target);
      pragma Annotate
        (GNATprove, Intentional,
         "types used for unchecked conversion do not have the same size",
         "We're checking it with a pragma");
      pragma Annotate
        (GNATProve, Intentional, "type is unsuitable for unchecked conversion",
         "ditto");
   begin
      if Target'Size /= 8 and then Target'Size /= 16 and then Target'Size /= 32
        and then Target'Size /= 64
      then
         raise Constraint_Error with "Target'Size must be 8, 16, 32, or 64";
      end if;
      return Convert (Value);
   end Assemble_From_Bytes;
end Endianness;
