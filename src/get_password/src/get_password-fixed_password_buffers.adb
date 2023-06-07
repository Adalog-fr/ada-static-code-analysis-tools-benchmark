pragma Ada_2012;
package body Get_Password.Fixed_Password_Buffers is


   overriding function Length (Buffer : Fixed_Password_Buffer) return Natural
   is (Buffer.Cursor - 1);

   overriding function Is_Empty (Buffer : Fixed_Password_Buffer) return Boolean
   is (Buffer.Length = 0);

   overriding function Is_Full (Buffer : Fixed_Password_Buffer) return Boolean
   is (Buffer.Cursor = Buffer.Data'Last + 1);

   overriding procedure Clean (Buffer : out Fixed_Password_Buffer)
   is
   begin
      Buffer.Cursor := 1;
      Buffer.Data := (others => ASCII.NUL);
   end Clean;

   overriding procedure Append (Buffer : in out Fixed_Password_Buffer;
                                Item   : Character)
   is
   begin
      if Buffer.Is_Full then
         raise Constraint_Error;
      end if;

      Buffer.Data (Buffer.Cursor) := Item;
      Buffer.Cursor := Buffer.Cursor + 1;
   end Append;


   overriding procedure Delete_Last (Buffer : in out Fixed_Password_Buffer)
   is
   begin
      if Buffer.Is_Empty then
         raise Constraint_Error;
      end if;

      pragma Assert (Buffer.Cursor > Buffer.Data'First);

      Buffer.Cursor := Buffer.Cursor - 1;
      Buffer.Data (Buffer.Cursor) := ASCII.NUL;
   end Delete_Last;

end Get_Password.Fixed_Password_Buffers;
