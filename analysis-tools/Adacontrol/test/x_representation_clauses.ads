with System;
package X_Representation_Clauses is
   I : Integer;

   V1 : Integer;
   for V1'Address use I'Address;      -- 'Address, global 'Address, rng_over, overlay

   package Inner is
      V2  : Integer;
      for V2'Address use I'Address;   -- 'Address, global 'Address, rng_over, overlay
   end Inner;

end X_Representation_Clauses;
