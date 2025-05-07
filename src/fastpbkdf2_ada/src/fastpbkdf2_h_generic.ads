pragma Ada_2012;

with System.Parameters;

generic
   type U8 is mod <>;
   type U32 is mod <>;
   type SZT is mod <>;
package fastpbkdf2_h_generic with
   Pure,
   Preelaborate
is
   pragma Compile_Time_Error
     (U8'Modulus /= 2**8, "'U8' type must be mod 2**8");
   pragma Compile_Time_Error
     (U32'Modulus /= 2**32, "'U32' type must be mod 2**32");
   pragma Compile_Time_Error
     (SZT'Modulus /= 2**System.Parameters.ptr_bits,
      "'SZT' type must be 2**(system ptr bits)");

   procedure fastpbkdf2_hmac_sha1
     (pw    : access constant U8; npw : SZT; salt : access constant U8;
      nsalt : SZT; iterations : U32; c_out : access U8; nout : SZT) with
      Import        => True,
      Convention    => C,
      External_Name => "fastpbkdf2_hmac_sha1";

   procedure fastpbkdf2_hmac_sha256
     (pw    : access constant U8; npw : SZT; salt : access constant U8;
      nsalt : SZT; iterations : U32; c_out : access U8; nout : SZT) with
      Import        => True,
      Convention    => C,
      External_Name => "fastpbkdf2_hmac_sha256";

   procedure fastpbkdf2_hmac_sha512
     (pw    : access constant U8; npw : SZT; salt : access constant U8;
      nsalt : SZT; iterations : U32; c_out : access U8; nout : SZT) with
      Import        => True,
      Convention    => C,
      External_Name => "fastpbkdf2_hmac_sha512";
end fastpbkdf2_h_generic;
