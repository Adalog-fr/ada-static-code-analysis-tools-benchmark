pragma Ada_2012;
package body HMAC_Generic is
   function Initialize (Key : String) return Context is
      Result : Context;
   begin
      Initialize (Result, Key);
      return Result;
   end Initialize;

   function Initialize (Key : Element_Array) return Context is
      Result : Context;
   begin
      Initialize (Result, Key);
      return Result;
   end Initialize;

   procedure Initialize (Ctx : out Context; Key : String) is
      Buffer : Element_Array (Index (Key'First) .. Index (Key'Last));
      for Buffer'Address use Key'Address;
      pragma Import (Ada, Buffer);
   begin
      Initialize (Ctx, Buffer);
   end Initialize;

   procedure Initialize (Ctx : out Context; Key : Element_Array) is
      Block_Sized_Key : Element_Array (0 .. Block_Length - 1) := (others => 0);
   begin
      Ctx.Outer := Hash_Initialize;
      Ctx.Inner := Hash_Initialize;

      if Key'Length > Block_Length then
         --  Keys longer than block size are hashed to shorten them
         declare
            Hash_Ctx : Hash_Context := Hash_Initialize;
         begin
            Hash_Update (Hash_Ctx, Key);
            Block_Sized_Key (0 .. Digest_Length - 1) :=
              Hash_Finalize (Hash_Ctx);
         end;
      else
         --  Otherwise we just copy, Block_Sized_Key is already containing 0s
         Block_Sized_Key (0 .. Key'Length - 1) := Key;
      end if;

      --  Prepare outer padded key
      declare
         Outer_Padded_Key : Element_Array := Block_Sized_Key;
      begin
         for B of Outer_Padded_Key loop
            B := B xor 16#5c#;
         end loop;

         Hash_Update (Ctx.Outer, Outer_Padded_Key);
      end;

      --  Prepare inner padded key
      declare
         Inner_Padded_Key : Element_Array := Block_Sized_Key;
      begin
         for B of Inner_Padded_Key loop
            B := B xor 16#36#;
         end loop;

         Hash_Update (Ctx.Inner, Inner_Padded_Key);
      end;
   end Initialize;

   procedure Update (Ctx : in out Context; Input : String) is
      Buffer : Element_Array (Index (Input'First) .. Index (Input'Last));
      for Buffer'Address use Input'Address;
      pragma Import (Ada, Buffer);
   begin
      Update (Ctx, Buffer);
   end Update;

   procedure Update (Ctx : in out Context; Input : Element_Array) is
   begin
      Hash_Update (Ctx.Inner, Input);
   end Update;

   function Finalize (Ctx : Context) return Digest is
      Result : Digest;
   begin
      Finalize (Ctx, Result);
      return Result;
   end Finalize;

   procedure Finalize (Ctx : Context; Output : out Digest) is
      Ctx_Copy : Context := Ctx;
   begin
      Hash_Update (Ctx_Copy.Outer, Hash_Finalize (Ctx_Copy.Inner));
      Output := Hash_Finalize (Ctx_Copy.Outer);
   end Finalize;

   function HMAC (Key : String; Message : String) return Digest is
      Ctx : Context := Initialize (Key);
   begin
      Update (Ctx, Message);
      return Finalize (Ctx);
   end HMAC;

   function HMAC (Key : Element_Array; Message : Element_Array) return Digest
   is
      Ctx : Context := Initialize (Key);
   begin
      Update (Ctx, Message);
      return Finalize (Ctx);
   end HMAC;
end HMAC_Generic;
