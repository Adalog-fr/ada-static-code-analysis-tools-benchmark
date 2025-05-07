pragma Ada_2012;
package body Get_Password.Unbounded_Password_Buffers is
   overriding function Is_Empty (Buffer : Unbounded_Password_Buffer) return Boolean
   is (Buffer.Length = 0);


   overriding function Is_Full (Buffer : Unbounded_Password_Buffer) return Boolean
   is (Length (Buffer.Data) = Natural'Last);

   overriding procedure Clean (Buffer : out Unbounded_Password_Buffer)
   is
   begin
      Buffer.Data := Null_Unbounded_String;
   end Clean;

   overriding procedure Append (Buffer : in out Unbounded_Password_Buffer;
                                Item   : Character)
   is
   begin
      if Buffer.Is_Full then
         raise Constraint_Error;
      end if;

      Append (Buffer.Data, Item);
   end Append;


   overriding procedure Delete_Last (Buffer : in out Unbounded_Password_Buffer)
   is
   begin
      if Buffer.Is_Empty then
         raise Constraint_Error;
      end if;

      declare
         L : constant Positive := Length (Buffer);
      begin
         Delete (Source  => Buffer.Data,
                 From    => L,
                 Through => L);
      end;
   end Delete_Last;

   overriding function Length (Buffer : Unbounded_Password_Buffer) return Natural
   is
   begin
      return Length (Buffer.Data);
   end Length;

end Get_Password.Unbounded_Password_Buffers;
