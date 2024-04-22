pragma Ada_2012;

with Interfaces;   use Interfaces;
with Interfaces.C; use Interfaces.C;

with fastpbkdf2_h_generic;

package fastpbkdf2_h is new fastpbkdf2_h_generic
  (Unsigned_8, Unsigned_32, size_t);
