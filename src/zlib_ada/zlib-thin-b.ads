private package ZLib.Thin.B is

   BZ_RUN    : constant := 0;
   BZ_FLUSH  : constant := 1;
   BZ_FINISH : constant := 2;

   BZ_OK               : constant := 0;
   BZ_RUN_OK           : constant := 1;
   BZ_FLUSH_OK         : constant := 2;
   BZ_FINISH_OK        : constant := 3;
   BZ_STREAM_END       : constant := 4;
   BZ_SEQUENCE_ERROR   : constant := -1;
   BZ_PARAM_ERROR      : constant := -2;
   BZ_MEM_ERROR        : constant := -3;
   BZ_DATA_ERROR       : constant := -4;
   BZ_DATA_ERROR_MAGIC : constant := -5;
   BZ_IO_ERROR         : constant := -6;
   BZ_UNEXPECTED_EOF   : constant := -7;
   BZ_OUTBUFF_FULL     : constant := -8;
   BZ_CONFIG_ERROR     : constant := -9;

   type Stream_Type is private;

   function Compress_Init
     (strm          : access Stream_Type;
      blockSize100k : in     Int;
      verbosity     : in     Int := 0;
      workFactor    : in     Int := 0)
   return Int;

   function Compress (strm : access Stream_Type; action : in Int) return Int;

   function Compress_End (strm : access Stream_Type) return Int;

   function Decompress_Init
     (strm      : access Stream_Type;
      verbosity : in     Int := 0;
      small     : in     Boolean := False)
   return Int;

   function Decompress (strm : access Stream_Type) return Int;

   function Decompress_End (strm : access Stream_Type) return Int;

private

   type Stream_Type is record
      next_in        : Voidp := Nul;
      avail_in       : UInt  := 0;
      total_in_lo32  : UInt  := 0;
      total_in_hi32  : UInt  := 0;
      next_out       : Voidp := Nul;
      avail_out      : UInt  := 0;
      total_out_lo32 : UInt  := 0;
      total_out_hi32 : UInt  := 0;
      state          : Voidp;
      zalloc         : alloc_func := null; -- user defined allocate routine
      zfree          : free_func  := null; -- user defined free routine
      opaque         : Voidp;              -- private data object passed to
   end record;

end ZLib.Thin.B;
