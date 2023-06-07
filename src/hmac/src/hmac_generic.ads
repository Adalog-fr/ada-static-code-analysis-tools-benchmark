generic
   type Element is mod <>;
   type Index is range <>;
   type Element_Array is array (Index range <>) of Element;

   Digest_Length : Index;
   Block_Length : Index;

   type Hash_Context is private;
   with function Hash_Initialize return Hash_Context;
   with procedure Hash_Update
     (Ctx : in out Hash_Context; Input : Element_Array);
   with function Hash_Finalize (Ctx : Hash_Context) return Element_Array;
package HMAC_Generic with
   Pure,
   Preelaborate
is
   pragma Compile_Time_Error
     (Element'Modulus /= 256,
      "'Element' type must be mod 2**8, i.e. represent a byte");

   subtype Digest is Element_Array (0 .. Digest_Length - 1);

   type Context is private;

   function Initialize (Key : String) return Context;
   function Initialize (Key : Element_Array) return Context;

   procedure Initialize (Ctx : out Context; Key : String);
   procedure Initialize (Ctx : out Context; Key : Element_Array);

   procedure Update (Ctx : in out Context; Input : String);
   procedure Update (Ctx : in out Context; Input : Element_Array);

   function Finalize (Ctx : Context) return Digest;
   procedure Finalize (Ctx : Context; Output : out Digest);

   function HMAC (Key : String; Message : String) return Digest;
   function HMAC (Key : Element_Array; Message : Element_Array) return Digest;
private
   type Context is record
      Inner : Hash_Context;
      Outer : Hash_Context;
   end record;
end HMAC_Generic;
