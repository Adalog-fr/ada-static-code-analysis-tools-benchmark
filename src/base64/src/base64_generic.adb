pragma Ada_2012;

with Interfaces; use Interfaces;

package body Base64_Generic is
   function Encode (Input : Element_Array) return String is
      Result  : String (1 .. Input'Length * 4); -- Worst case scenario
      Current : Natural    := 0;
      Step    : Natural    := 0;
      Last    : Unsigned_8 := 0;
   begin
      for Byte of Input loop
         case Step is
            when 0 =>
               Step                 := 1;
               Result (Current + 1) :=
                 Value_To_Character
                   (Element
                      (Shift_Right (Unsigned_8 (Byte), 2) and
                       Unsigned_8 (16#3F#)));
               Current := Current + 1;
            when 1 =>
               Step                 := 2;
               Result (Current + 1) :=
                 Value_To_Character
                   (Element
                      (Shift_Left (Last and Unsigned_8 (16#3#), 4) or
                       (Shift_Right (Unsigned_8 (Byte), 4) and
                        Unsigned_8 (16#F#))));
               Current := Current + 1;
            when 2 =>
               Step                 := 0;
               Result (Current + 1) :=
                 Value_To_Character
                   (Element
                      (Shift_Left (Last and Unsigned_8 (16#F#), 2) or
                       (Shift_Right (Unsigned_8 (Byte), 6) and
                        Unsigned_8 (16#3#))));
               Result (Current + 2) :=
                 Value_To_Character
                   (Element (Unsigned_8 (Byte) and Unsigned_8 (16#3F#)));
               Current := Current + 2;
            when others =>
               raise Program_Error;
         end case;
         Last := Unsigned_8 (Byte);
      end loop;

      case Step is
         when 1 =>
            Result (Current + 1) :=
              Value_To_Character
                (Element (Shift_Left (Last and Unsigned_8 (16#3#), 4)));
            Result (Current + 2) := Padding;
            Result (Current + 3) := Padding;
            Current              := Current + 3;
         when 2 =>
            Result (Current + 1) :=
              Value_To_Character
                (Element (Shift_Left (Last and Unsigned_8 (16#F#), 2)));
            Result (Current + 2) := Padding;
            Current              := Current + 2;
         when others =>
            null;
      end case;

      return Result (1 .. Current);
   end Encode;

   function Decode (Input : String) return Element_Array is
      Result : Element_Array
        (1 .. 3 * (Input'Length / 4)); -- Worst case scenario
      Current : Index := Result'First;
      Byte    : Unsigned_8;
   begin
      if (Unsigned_32 (Input'Length) and Unsigned_32 (16#3#)) /= 0 then
         raise Base64_Error with "Invalid base64 string length";
      end if;

      for I in Input'Range loop
         exit when Input (I) = Padding;

         Byte := Unsigned_8 (Character_To_Value (Input (I)));
         if Byte = Unsigned_8 (Element'Last) then
            raise Base64_Error with "Invalid base64 character";
         end if;

         case Unsigned_32 (I - Input'First) and Unsigned_32 (16#3#) is
            when 0 =>
               Result (Current) :=
                 Element (Shift_Left (Byte, 2) and Unsigned_8 (16#FF#));
            when 1 =>
               Result (Current) :=
                 Element
                   (Unsigned_8 (Result (Current)) or
                    (Shift_Right (Byte, 4) and Unsigned_8 (16#3#)));
               Result (Current + 1) :=
                 Element (Shift_Left (Byte and Unsigned_8 (16#F#), 4));
               Current := Current + 1;
            when 2 =>
               Result (Current) :=
                 Element
                   (Unsigned_8 (Result (Current)) or
                    (Shift_Right (Byte, 2) and Unsigned_8 (16#F#)));
               Result (Current + 1) :=
                 Element (Shift_Left (Byte and Unsigned_8 (16#3#), 6));
               Current := Current + 1;
            when 3 =>
               Result (Current) :=
                 Element (Unsigned_8 (Result (Current)) or Byte);
               Current := Current + 1;
            when others =>
               raise Program_Error;
         end case;
      end loop;

      return Result (1 .. Current - 1);
   end Decode;
end Base64_Generic;
