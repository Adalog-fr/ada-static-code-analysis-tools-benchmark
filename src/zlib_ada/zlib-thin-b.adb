package body ZLib.Thin.B is

   --------------
   -- Compress --
   --------------

   function Compress
     (strm : access Stream_Type;
      action : in Int)
      return Int
   is
      pragma Unreferenced (strm, action);
   begin
      return BZ_CONFIG_ERROR;
   end Compress;

   ------------------
   -- Compress_End --
   ------------------

   function Compress_End (strm : access Stream_Type) return Int is
      pragma Unreferenced (strm);
   begin
      return BZ_CONFIG_ERROR;
   end Compress_End;

   -------------------
   -- Compress_Init --
   -------------------

   function Compress_Init
     (strm          : access Stream_Type;
      blockSize100k : in     Int;
      verbosity     : in     Int := 0;
      workFactor    : in     Int := 0)
      return Int
   is
      pragma Unreferenced (strm, blockSize100k, verbosity, workFactor);
   begin
      return BZ_CONFIG_ERROR;
   end Compress_Init;

   ----------------
   -- Decompress --
   ----------------

   function Decompress (strm : access Stream_Type) return Int is
      pragma Unreferenced (strm);
   begin
      return BZ_CONFIG_ERROR;
   end Decompress;

   --------------------
   -- Decompress_End --
   --------------------

   function Decompress_End (strm : access Stream_Type) return Int is
      pragma Unreferenced (strm);
   begin
      return BZ_CONFIG_ERROR;
   end Decompress_End;

   ---------------------
   -- Decompress_Init --
   ---------------------

   function Decompress_Init
     (strm      : access Stream_Type;
      verbosity : in     Int := 0;
      small     : in     Boolean := False)
      return Int
   is
      pragma Unreferenced (strm, verbosity, small);
   begin
      return BZ_CONFIG_ERROR;
   end Decompress_Init;

end ZLib.Thin.B;
