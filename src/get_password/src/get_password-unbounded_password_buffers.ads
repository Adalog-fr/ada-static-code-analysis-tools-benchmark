with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;

private package Get_Password.Unbounded_Password_Buffers is
   type Unbounded_Password_Buffer is new Abstract_Password_Buffer
   with
      record
         Data : Unbounded_String;
      end record;

   overriding function Length (Buffer : Unbounded_Password_Buffer) return Natural;

   overriding function Is_Empty (Buffer : Unbounded_Password_Buffer) return Boolean;

   overriding function Is_Full (Buffer : Unbounded_Password_Buffer) return Boolean;

   overriding procedure Clean (Buffer : out Unbounded_Password_Buffer);

   overriding procedure Append (Buffer : in out Unbounded_Password_Buffer;
                                Item   : Character);

   overriding procedure Delete_Last (Buffer : in out Unbounded_Password_Buffer);
end Get_Password.Unbounded_Password_Buffers;
