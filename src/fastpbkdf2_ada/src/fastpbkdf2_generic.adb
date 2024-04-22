pragma Ada_2012;

with Interfaces;
with Interfaces.C;

with fastpbkdf2_h_generic;

package body Fastpbkdf2_Generic is
   package fastpbkdf2 is new fastpbkdf2_h_generic
     (Element, Interfaces.Unsigned_32, Interfaces.C.size_t);
   use fastpbkdf2;

   procedure PBKDF2_HMAC_SHA1
     (Password :     Element_Array; Salt : Element_Array; Iterations : Natural;
      Output   : out Element_Array)
   is
   begin
      fastpbkdf2_hmac_sha1
        (Password (Password'First)'Access, Password'Length,
         Salt (Salt'First)'Access, Salt'Length,
         Interfaces.Unsigned_32 (Iterations), Output (Output'First)'Access,
         Output'Length);
   end PBKDF2_HMAC_SHA1;

   function PBKDF2_HMAC_SHA1
     (Password : Element_Array; Salt : Element_Array; Iterations : Natural)
      return Element_Array
   is
      Output : Element_Array
        (Index'Max (Index'First, 0) .. Index'Max (Index'First, 0) + 19) :=
        (others => Element'First);
   begin
      PBKDF2_HMAC_SHA1 (Password, Salt, Iterations, Output);
      return Output;
   end PBKDF2_HMAC_SHA1;

   procedure PBKDF2_HMAC_SHA256
     (Password :     Element_Array; Salt : Element_Array; Iterations : Natural;
      Output   : out Element_Array)
   is
   begin
      fastpbkdf2_hmac_sha256
        (Password (Password'First)'Access, Password'Length,
         Salt (Salt'First)'Access, Salt'Length,
         Interfaces.Unsigned_32 (Iterations), Output (Output'First)'Access,
         Output'Length);
   end PBKDF2_HMAC_SHA256;

   function PBKDF2_HMAC_SHA256
     (Password : Element_Array; Salt : Element_Array; Iterations : Natural)
      return Element_Array
   is
      Output : Element_Array
        (Index'Max (Index'First, 0) .. Index'Max (Index'First, 0) + 31) :=
        (others => Element'First);
   begin
      PBKDF2_HMAC_SHA256 (Password, Salt, Iterations, Output);
      return Output;
   end PBKDF2_HMAC_SHA256;

   procedure PBKDF2_HMAC_SHA512
     (Password :     Element_Array; Salt : Element_Array; Iterations : Natural;
      Output   : out Element_Array)
   is
   begin
      fastpbkdf2_hmac_sha512
        (Password (Password'First)'Access, Password'Length,
         Salt (Salt'First)'Access, Salt'Length,
         Interfaces.Unsigned_32 (Iterations), Output (Output'First)'Access,
         Output'Length);
   end PBKDF2_HMAC_SHA512;

   function PBKDF2_HMAC_SHA512
     (Password : Element_Array; Salt : Element_Array; Iterations : Natural)
      return Element_Array
   is
      Output : Element_Array
        (Index'Max (Index'First, 0) .. Index'Max (Index'First, 0) + 63) :=
        (others => Element'First);
   begin
      PBKDF2_HMAC_SHA512 (Password, Salt, Iterations, Output);
      return Output;
   end PBKDF2_HMAC_SHA512;
end Fastpbkdf2_Generic;
