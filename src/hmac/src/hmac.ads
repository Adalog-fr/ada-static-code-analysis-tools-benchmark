with Ada.Streams; use Ada.Streams;

with SHA1;
with SHA2;

with HMAC_Generic;

package HMAC with
   Pure,
   Preelaborate
is
   package HMAC_SHA_1 is new HMAC_Generic
     (Element       => Stream_Element, Index => Stream_Element_Offset,
      Element_Array => Stream_Element_Array,

      Digest_Length => SHA1.Digest_Length, Block_Length => SHA1.Block_Length,

      Hash_Context => SHA1.Context, Hash_Initialize => SHA1.Initialize,
      Hash_Update  => SHA1.Update, Hash_Finalize => SHA1.Finalize);

   package HMAC_SHA_224 is new HMAC_Generic
     (Element       => Stream_Element, Index => Stream_Element_Offset,
      Element_Array => Stream_Element_Array,

      Digest_Length => SHA2.SHA_224.Digest_Length,
      Block_Length  => SHA2.SHA_224.Block_Length,

      Hash_Context    => SHA2.SHA_224.Context,
      Hash_Initialize => SHA2.SHA_224.Initialize,
      Hash_Update     => SHA2.SHA_224.Update,
      Hash_Finalize   => SHA2.SHA_224.Finalize);

   package HMAC_SHA_256 is new HMAC_Generic
     (Element       => Stream_Element, Index => Stream_Element_Offset,
      Element_Array => Stream_Element_Array,

      Digest_Length => SHA2.SHA_256.Digest_Length,
      Block_Length  => SHA2.SHA_256.Block_Length,

      Hash_Context    => SHA2.SHA_256.Context,
      Hash_Initialize => SHA2.SHA_256.Initialize,
      Hash_Update     => SHA2.SHA_256.Update,
      Hash_Finalize   => SHA2.SHA_256.Finalize);

   package HMAC_SHA_384 is new HMAC_Generic
     (Element       => Stream_Element, Index => Stream_Element_Offset,
      Element_Array => Stream_Element_Array,

      Digest_Length => SHA2.SHA_384.Digest_Length,
      Block_Length  => SHA2.SHA_384.Block_Length,

      Hash_Context    => SHA2.SHA_384.Context,
      Hash_Initialize => SHA2.SHA_384.Initialize,
      Hash_Update     => SHA2.SHA_384.Update,
      Hash_Finalize   => SHA2.SHA_384.Finalize);

   package HMAC_SHA_512 is new HMAC_Generic
     (Element       => Stream_Element, Index => Stream_Element_Offset,
      Element_Array => Stream_Element_Array,

      Digest_Length => SHA2.SHA_512.Digest_Length,
      Block_Length  => SHA2.SHA_512.Block_Length,

      Hash_Context    => SHA2.SHA_512.Context,
      Hash_Initialize => SHA2.SHA_512.Initialize,
      Hash_Update     => SHA2.SHA_512.Update,
      Hash_Finalize   => SHA2.SHA_512.Finalize);
end HMAC;
