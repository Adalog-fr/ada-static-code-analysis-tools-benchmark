private package Get_Password.Fixed_Password_Buffers is
   --  SPARK_Mode is Off here since we define a derived type with
   --  discriminant and SPARK is not happy about that...
   pragma SPARK_Mode (Off);

   type Fixed_Password_Buffer (Length : Positive) is new Abstract_Password_Buffer
   with
      record
         Data   : String (1 .. Length);
         Cursor : Positive;
      end record;

   overriding function Length (Buffer : Fixed_Password_Buffer) return Natural;

   overriding function Is_Empty (Buffer : Fixed_Password_Buffer) return Boolean;

   overriding function Is_Full (Buffer : Fixed_Password_Buffer) return Boolean;

   overriding procedure Clean (Buffer : out Fixed_Password_Buffer);

   overriding procedure Append (Buffer : in out Fixed_Password_Buffer;
                                Item   : Character);

   overriding procedure Delete_Last (Buffer : in out Fixed_Password_Buffer);

end Get_Password.Fixed_Password_Buffers;
