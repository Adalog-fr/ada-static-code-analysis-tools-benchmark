------------------------------------------------------------------------------
--                                                                          --
--                            Matreshka Project                             --
--                                                                          --
--         Localization, Internationalization, Globalization for Ada        --
--                                                                          --
--                        Runtime Library Component                         --
--                                                                          --
------------------------------------------------------------------------------
--                                                                          --
-- Copyright © 2010-2023, Vadim Godunko <vgodunko@gmail.com>                --
-- All rights reserved.                                                     --
--                                                                          --
-- Redistribution and use in source and binary forms, with or without       --
-- modification, are permitted provided that the following conditions       --
-- are met:                                                                 --
--                                                                          --
--  * Redistributions of source code must retain the above copyright        --
--    notice, this list of conditions and the following disclaimer.         --
--                                                                          --
--  * Redistributions in binary form must reproduce the above copyright     --
--    notice, this list of conditions and the following disclaimer in the   --
--    documentation and/or other materials provided with the distribution.  --
--                                                                          --
--  * Neither the name of the Vadim Godunko, IE nor the names of its        --
--    contributors may be used to endorse or promote products derived from  --
--    this software without specific prior written permission.              --
--                                                                          --
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS      --
-- "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT        --
-- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR    --
-- A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT     --
-- HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,   --
-- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED --
-- TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR   --
-- PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF   --
-- LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING     --
-- NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS       --
-- SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.             --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Unchecked_Conversion;
with System.Address_To_Access_Conversions;
with System.Storage_Elements;

with Matreshka.Internals.Strings.Constants;
with Matreshka.SIMD.Intel.SSE2;

package body Matreshka.Internals.Strings.Handlers.Generic_X86_SSE2 is

   use Interfaces;
   use Matreshka.Internals.Strings.Constants;
   use Matreshka.Internals.Unicode;
   use Matreshka.Internals.Utf16;
   use Matreshka.SIMD.Intel;
   use Matreshka.SIMD.Intel.SSE2;
   use type System.Address;
   use type System.Storage_Elements.Storage_Offset;

   function builtin_ffs (A : Interfaces.Integer_32) return Interfaces.Integer_32;
   pragma Import (Intrinsic, builtin_ffs, "__builtin_ffs");

   function ffs (A : Interfaces.Unsigned_32) return Interfaces.Unsigned_32;
   pragma Inline (ffs);

   function clz (X : Interfaces.Unsigned_32) return Interfaces.Integer_32;
   pragma Import (Intrinsic, clz, "__builtin_clz");

   package v8hi_Conversions is new System.Address_To_Access_Conversions (v8hi);
   use v8hi_Conversions;

   function To_Unsigned_32 is
     new Ada.Unchecked_Conversion
          (Interfaces.Integer_32, Interfaces.Unsigned_32);

   function To_Integer_32 is
     new Ada.Unchecked_Conversion
          (Interfaces.Unsigned_32, Interfaces.Integer_32);

   type v8hi_Unrestricted_Array is array (Utf16_String_Index) of v8hi;

   function mm_and_si128 (Left : v8hi; Right : v8hi) return v8hi;
   function mm_movemask_epi8 (Item : v8hi) return Interfaces.Unsigned_32;
   --  Overloaded functions to remove type conversions from primary code.

   function Index_16
    (Value         : v8hi_Unrestricted_Array;
     From_Index    : Positive;
     From_Position : Matreshka.Internals.Utf16.Utf16_String_Index;
     To_Position   : Matreshka.Internals.Utf16.Utf16_String_Index;
     Code          : Matreshka.Internals.Unicode.Code_Point) return Natural;
   --  Implementation of Index operation when Code is less than or equal to
   --  16#FFFF#, so its representation occupies one code unit.
   --
   --  Note, To_Position must be greater than From_Position.

   function Index_16_Without_Surrogates
    (Value         : v8hi_Unrestricted_Array;
     From_Index    : Positive;
     From_Position : Matreshka.Internals.Utf16.Utf16_String_Index;
     To_Position   : Matreshka.Internals.Utf16.Utf16_String_Index;
     Code          : Matreshka.Internals.Unicode.Code_Point) return Natural;
   --  Implementation of Index operation when Code is less than or equal to
   --  16#FFFF#, so its representation occupies one code unit. This function
   --  assumes that string doesn't contain surrogate pairs.
   --
   --  Note, To_Position must be greater than From_Position.

   function Last_Index_16
    (Value         : v8hi_Unrestricted_Array;
     From_Position : Utf16_String_Index;
     To_Index      : Positive;
     To_Position   : Utf16_String_Index;
     Code          : Matreshka.Internals.Unicode.Code_Point) return Natural;
   --  Implementation of Last_Index operation when Code is less than or equal
   --  to 16#FFFF#, so its representation occupies one code unit.

   function Last_Index_16_Without_Surrogates
    (Value         : v8hi_Unrestricted_Array;
     From_Position : Utf16_String_Index;
     To_Index      : Positive;
     To_Position   : Utf16_String_Index;
     Code          : Matreshka.Internals.Unicode.Code_Point) return Natural;
   --  Implementation of Last_Index operation when Code is less than or equal
   --  to 16#FFFF#, so its representation occupies one code unit. This function
   --  assumes that string doesn't contain surrogate pairs.

   --------------------------
   -- Fill_Null_Terminator --
   --------------------------

   overriding procedure Fill_Null_Terminator
    (Self : X86_SSE2_String_Handler;
     Item : not null Shared_String_Access)
   is
      pragma Unreferenced (Self);

      pragma Suppress (Access_Check);
      --  Suppress not null check of Self which is generated by compiler; but
      --  not needed actually.
      pragma Suppress (Alignment_Check);
      --  Suppress alignment check of Value below which is generated because of
      --  bug in the GNAT GPL 2010 compiler.

      Value  : v8hi_Unrestricted_Array;
      for Value'Address use Item.Value'Address;
      Index  : constant Utf16_String_Index := Item.Unused / 8;
      Offset : constant Utf16_String_Index := Item.Unused mod 8;

   begin
      Value (Index) :=
        mm_and_si128 (Value (Index), Terminator_Mask_x86_64 (Offset));
   end Fill_Null_Terminator;

   ---------
   -- ffs --
   ---------

   function ffs (A : Interfaces.Unsigned_32) return Interfaces.Unsigned_32 is
   begin
      return To_Unsigned_32 (builtin_ffs (To_Integer_32 (A)));
   end ffs;

   -----------
   -- Index --
   -----------

   overriding function Index
    (Self          : X86_SSE2_String_Handler;
     Item          : Matreshka.Internals.Strings.Shared_String_Access;
     From_Index    : Positive;
     From_Position : Matreshka.Internals.Utf16.Utf16_String_Index;
     To_Position   : Matreshka.Internals.Utf16.Utf16_String_Index;
     Code          : Matreshka.Internals.Unicode.Code_Point)
       return Natural
   is
      Value : v8hi_Unrestricted_Array;
      for Value'Address use Item.Value'Address;
      pragma Import (Ada, Value);

   begin
      if Code <= 16#FFFF# then
         if Utf16_String_Index (Item.Length) = Item.Unused then
            return
              Index_16_Without_Surrogates
               (Value, From_Index, From_Position, To_Position, Code);

         else
            return
              Index_16 (Value, From_Index, From_Position, To_Position, Code);
         end if;

      else
         return
           Base_String_Handler (Self).Index
            (Item, From_Index, From_Position, To_Position, Code);
      end if;
   end Index;

   --------------
   -- Index_16 --
   --------------

   function Index_16
    (Value         : v8hi_Unrestricted_Array;
     From_Index    : Positive;
     From_Position : Utf16_String_Index;
     To_Position   : Utf16_String_Index;
     Code          : Matreshka.Internals.Unicode.Code_Point) return Natural
   is
      pragma Suppress (All_Checks);

      From_Vector    : constant Utf16_String_Index := From_Position / 8;
      From_Offset    : constant Utf16_String_Index := From_Position mod 8;
      To_Vector      : constant Utf16_String_Index := (To_Position - 1) / 8;
      To_Offset      : constant Utf16_String_Index := (To_Position - 1) mod 8;

      Pattern        : constant v8hi := (others => Integer_16 (Code));
      Match_Mask     : Unsigned_32;
      Exclusion_Mask : Unsigned_32;
      Current_Vector : v8hi;
      Head_Mask      : constant Unsigned_32
        := 16#FFFF_FFFF# * (2 ** Natural (Unsigned_32 (From_Offset * 2)));
      Tail_Mask      : constant Unsigned_32
        := 16#FFFF_FFFF# / (2 ** Natural (30 - To_Offset * 2));
      Index          : Integer := From_Index;
      Current        : System.Address := Value (From_Vector)'Address;

   begin
      if From_Vector = To_Vector then
         Current_Vector := To_Pointer (Current).all;

         --  Compute 'match' mask by compare vector of string with pattern and
         --  excluding match of needed leading and trailing code units. If
         --  match is found construct mask to exclude all leading and trailing
         --  code units.

         Match_Mask :=
           ffs
            (mm_movemask_epi8 (mm_cmpeq_epi16 (Current_Vector, Pattern))
               and Head_Mask and Tail_Mask);

         if Match_Mask = 0 then
            --  No match found, return 0.

            return 0;
         end if;

         Match_Mask := 16#FFFF_FFFF# * (2 ** Natural (Match_Mask - 1));

         --  Code units from high surrogate range must be ignored when
         --  computing index of character. Complete exclusion mask includes all
         --  leading code units as well as all code units after found one.

         Exclusion_Mask :=
           mm_movemask_epi8
            (mm_cmpeq_epi16
              (mm_and_si128 (Current_Vector, Surrogate_Kind_Mask_x86_64),
               Masked_High_Surrogate_x86_64)) or Match_Mask or not Head_Mask;

         --  Update index of character. It will be index of found character.

         Update_Index_Forward (Exclusion_Mask and 16#FFFF#, Index);

         return Index;

      else
         --  First step: process vector which includes first character of the
         --  slice.

         Current_Vector := To_Pointer (Current).all;
         Current := Current + 16;

         --  Compute 'match' mask by compare vector of string with pattern and
         --  excluding match of needed first code units. If match is found
         --  construct mask to exclude all trailing code units.

         Match_Mask :=
           ffs
            (mm_movemask_epi8 (mm_cmpeq_epi16 (Current_Vector, Pattern))
               and Head_Mask);

         if Match_Mask /= 0 then
            Match_Mask := 16#FFFF_FFFF# * (2 ** Natural (Match_Mask - 1));
         end if;

         --  Code units from high surrogate range must be ignored when
         --  computing index of character. Complete exclusion mask includes all
         --  leading code units as well as all code units after found one.

         Exclusion_Mask :=
           mm_movemask_epi8
            (mm_cmpeq_epi16
              (mm_and_si128 (Current_Vector, Surrogate_Kind_Mask_x86_64),
               Masked_High_Surrogate_x86_64)) or Match_Mask or not Head_Mask;

         --  Update index of character. It will be index of found character of
         --  index of first character in the next string's vector.

         Update_Index_Forward (Exclusion_Mask and 16#FFFF#, Index);

         if Match_Mask /= 0 then
            return Index;
         end if;

         --  Second step: process all string's vectors between first and last.

         while Current /= Value (To_Vector)'Address loop
            Current_Vector := To_Pointer (Current).all;
            Current := Current + 16;

            --  Compute 'match' mask by compare vector of string with pattern.
            --  If match is found construct mask to exclude all trailing code
            --  units.

            Match_Mask :=
              ffs
               (mm_movemask_epi8 (mm_cmpeq_epi16 (Current_Vector, Pattern)));

            if Match_Mask /= 0 then
               Match_Mask := 16#FFFF_FFFF# * (2 ** Natural (Match_Mask - 1));
            end if;

            --  Code units from high surrogate range must be ignored when
            --  computing index of character. Complete exclusion mask includes
            --  all code units after found one.

            Exclusion_Mask :=
              mm_movemask_epi8
               (mm_cmpeq_epi16
                 (mm_and_si128 (Current_Vector, Surrogate_Kind_Mask_x86_64),
                  Masked_High_Surrogate_x86_64)) or Match_Mask;

            --  Update index of character. It will be index of found character
            --  of index of first character in the next string's vector.

            Update_Index_Forward (Exclusion_Mask and 16#FFFF#, Index);

            if Match_Mask /= 0 then
               return Index;
            end if;
         end loop;

         --  Third step: process vector which includes last character of the
         --  slice.

         Current_Vector := To_Pointer (Current).all;

         --  Compute 'match' mask by compare vector of string with pattern and
         --  excluding match of needed last code units. If match is found
         --  construct mask to exclude all trailing code units.

         Match_Mask :=
           ffs
            (mm_movemask_epi8 (mm_cmpeq_epi16 (Current_Vector, Pattern))
               and Tail_Mask);

         if Match_Mask /= 0 then
            Match_Mask := 16#FFFF_FFFF# * (2 ** Natural (Match_Mask - 1));

         else
            --  No match found, return 0.

            return 0;
         end if;

         --  Code units from high surrogate range must be ignored when
         --  computing index of character. Complete exclusion mask includes all
         --  code units after found one.

         Exclusion_Mask :=
           mm_movemask_epi8
            (mm_cmpeq_epi16
              (mm_and_si128 (Current_Vector, Surrogate_Kind_Mask_x86_64),
               Masked_High_Surrogate_x86_64)) or Match_Mask;

         --  Update index of character. It will be index of found character of
         --  index of first character in the next string's vector.

         Update_Index_Forward (Exclusion_Mask and 16#FFFF#, Index);

         return Index;
      end if;
   end Index_16;

   ---------------------------------
   -- Index_16_Without_Surrogates --
   ---------------------------------

   function Index_16_Without_Surrogates
    (Value         : v8hi_Unrestricted_Array;
     From_Index    : Positive;
     From_Position : Utf16_String_Index;
     To_Position   : Utf16_String_Index;
     Code          : Matreshka.Internals.Unicode.Code_Point) return Natural
   is
      pragma Suppress (All_Checks);

      From_Vector    : constant Utf16_String_Index := From_Position / 8;
      From_Offset    : constant Utf16_String_Index := From_Position mod 8;
      To_Vector      : constant Utf16_String_Index := (To_Position - 1) / 8;
      To_Offset      : constant Utf16_String_Index := (To_Position - 1) mod 8;

      Pattern        : constant v8hi := (others => Integer_16 (Code));
      Match_Mask     : Unsigned_32;
      Exclusion_Mask : Unsigned_32;
      Current_Vector : v8hi;
      Head_Mask      : constant Unsigned_32
        := 16#FFFF_FFFF# * (2 ** Natural (Unsigned_32 (From_Offset * 2)));
      Tail_Mask      : constant Unsigned_32
        := 16#FFFF_FFFF# / (2 ** Natural (30 - To_Offset * 2));
      Index          : Integer := From_Index;
      Current        : System.Address := Value (From_Vector)'Address;

   begin
      if From_Vector = To_Vector then
         Current_Vector := To_Pointer (Current).all;

         --  Compute 'match' mask by compare vector of string with pattern and
         --  excluding match of needed leading and trailing code units. If
         --  match is found construct mask to exclude all leading and trailing
         --  code units.

         Match_Mask :=
           ffs
            (mm_movemask_epi8 (mm_cmpeq_epi16 (Current_Vector, Pattern))
               and Head_Mask and Tail_Mask);

         if Match_Mask = 0 then
            --  No match found, return 0.

            return 0;
         end if;

         Match_Mask := 16#FFFF_FFFF# * (2 ** Natural (Match_Mask - 1));

         --  Complete exclusion mask includes all leading code units as well as
         --  all code units after found one.

         Exclusion_Mask := Match_Mask or not Head_Mask;

         --  Update index of character. It will be index of found character.

         Update_Index_Forward (Exclusion_Mask and 16#FFFF#, Index);

         return Index;

      else
         --  First step: process vector which includes first character of the
         --  slice.

         Current_Vector := To_Pointer (Current).all;
         Current := Current + 16;

         --  Compute 'match' mask by compare vector of string with pattern and
         --  excluding match of needed first code units. If match is found
         --  construct mask to exclude all trailing code units.

         Match_Mask :=
           ffs
            (mm_movemask_epi8 (mm_cmpeq_epi16 (Current_Vector, Pattern))
               and Head_Mask);

         if Match_Mask /= 0 then
            Match_Mask := 16#FFFF_FFFF# * (2 ** Natural (Match_Mask - 1));
         end if;

         --  Complete exclusion mask includes all leading code units as well as
         --  all code units after found one.

         Exclusion_Mask := Match_Mask or not Head_Mask;

         --  Update index of character. It will be index of found character of
         --  index of first character in the next string's vector.

         Update_Index_Forward (Exclusion_Mask and 16#FFFF#, Index);

         if Match_Mask /= 0 then
            return Index;
         end if;

         --  Second step: process all string's vectors between first and last.

         while Current /= Value (To_Vector)'Address loop
            Current_Vector := To_Pointer (Current).all;
            Current := Current + 16;

            --  Compute 'match' mask by compare vector of string with pattern.
            --  If match is found construct mask to exclude all trailing code
            --  units.

            Match_Mask :=
              ffs
               (mm_movemask_epi8 (mm_cmpeq_epi16 (Current_Vector, Pattern)));

            if Match_Mask /= 0 then
               Match_Mask := 16#FFFF_FFFF# * (2 ** Natural (Match_Mask - 1));
            end if;

            --  Complete exclusion mask includes all code units after found
            --  one.

            Exclusion_Mask := Match_Mask;

            --  Update index of character. It will be index of found character
            --  of index of first character in the next string's vector.

            Update_Index_Forward (Exclusion_Mask and 16#FFFF#, Index);

            if Match_Mask /= 0 then
               return Index;
            end if;
         end loop;

         --  Third step: process vector which includes last character of the
         --  slice.

         Current_Vector := To_Pointer (Current).all;

         --  Compute 'match' mask by compare vector of string with pattern and
         --  excluding match of needed last code units. If match is found
         --  construct mask to exclude all trailing code units.

         Match_Mask :=
           ffs
            (mm_movemask_epi8 (mm_cmpeq_epi16 (Current_Vector, Pattern))
               and Tail_Mask);

         if Match_Mask /= 0 then
            Match_Mask := 16#FFFF_FFFF# * (2 ** Natural (Match_Mask - 1));

         else
            --  No match found, return 0.

            return 0;
         end if;

         --  Complete exclusion mask includes all code units after found one.

         Exclusion_Mask := Match_Mask;

         --  Update index of character. It will be index of found character of
         --  index of first character in the next string's vector.

         Update_Index_Forward (Exclusion_Mask and 16#FFFF#, Index);

         return Index;
      end if;
   end Index_16_Without_Surrogates;

   --------------
   -- Is_Equal --
   --------------

   overriding function Is_Equal
    (Self  : X86_SSE2_String_Handler;
     Left  : not null Shared_String_Access;
     Right : not null Shared_String_Access) return Boolean
   is
      pragma Unreferenced (Self);

      pragma Suppress (Access_Check);

   begin
      if Left = Right then
         return True;
      end if;

      if Left.Unused /= Right.Unused then
         return False;
      end if;

      declare
         LV : System.Address := Left.Value'Address;
         RV : System.Address := Right.Value'Address;
         J  : Utf16_String_Index := Left.Unused / 8;

      begin
         loop
            if mm_movemask_epi8
                (mm_cmpeq_epi16 (To_Pointer (LV).all, To_Pointer (RV).all))
                 /= 16#0000_FFFF#
            then
               return False;
            end if;

            exit when J = 0;

            J := J - 1;
            LV := LV + 16;
            RV := RV + 16;
         end loop;
      end;

      return True;
   end Is_Equal;

   ----------------
   -- Is_Greater --
   ----------------

   overriding function Is_Greater
    (Self  : X86_SSE2_String_Handler;
     Left  : not null Shared_String_Access;
     Right : not null Shared_String_Access) return Boolean
   is
      pragma Unreferenced (Self);

      pragma Suppress (Access_Check);
      pragma Suppress (Index_Check);

   begin
      if Left = Right then
         return False;
      end if;

      declare
         Min   : constant Utf16_String_Index
           := Utf16_String_Index'Min (Left.Unused, Right.Unused);
         LV    : System.Address := Left.Value'Address;
         RV    : System.Address := Right.Value'Address;
         J     : Utf16_String_Index := 0;
         M     : Unsigned_32;
         Index : Utf16_String_Index;

      begin
         loop
            M :=
              mm_movemask_epi8
               (mm_cmpeq_epi16 (To_Pointer (LV).all, To_Pointer (RV).all));

            if M /= 16#0000_FFFF# then
               Index := J * 8 + Utf16_String_Index (ffs (not M) / 2);

               return Is_Greater (Left.Value (Index), Right.Value (Index));
            end if;

            exit when J = Min / 8;

            J := J + 1;
            LV := LV + 16;
            RV := RV + 16;
         end loop;
      end;

      return Left.Unused > Right.Unused;
   end Is_Greater;

   -------------------------
   -- Is_Greater_Or_Equal --
   -------------------------

   overriding function Is_Greater_Or_Equal
    (Self  : X86_SSE2_String_Handler;
     Left  : not null Shared_String_Access;
     Right : not null Shared_String_Access) return Boolean
   is
      pragma Unreferenced (Self);

      pragma Suppress (Access_Check);
      pragma Suppress (Index_Check);

   begin
      if Left = Right then
         return True;
      end if;

      declare
         Min   : constant Utf16_String_Index
           := Utf16_String_Index'Min (Left.Unused, Right.Unused);
         LV    : System.Address := Left.Value'Address;
         RV    : System.Address := Right.Value'Address;
         J     : Utf16_String_Index := 0;
         M     : Unsigned_32;
         Index : Utf16_String_Index;

      begin
         loop
            M :=
              mm_movemask_epi8
               (mm_cmpeq_epi16 (To_Pointer (LV).all, To_Pointer (RV).all));

            if M /= 16#0000_FFFF# then
               Index := J * 8 + Utf16_String_Index (ffs (not M) / 2);

               return Is_Greater (Left.Value (Index), Right.Value (Index));
            end if;

            exit when J = Min / 8;

            J := J + 1;
            LV := LV + 16;
            RV := RV + 16;
         end loop;
      end;

      return Left.Unused >= Right.Unused;
   end Is_Greater_Or_Equal;

   -------------
   -- Is_Less --
   -------------

   overriding function Is_Less
    (Self  : X86_SSE2_String_Handler;
     Left  : not null Shared_String_Access;
     Right : not null Shared_String_Access) return Boolean
   is
      pragma Unreferenced (Self);

      pragma Suppress (Access_Check);
      pragma Suppress (Index_Check);

   begin
      if Left = Right then
         return False;
      end if;

      declare
         Min   : constant Utf16_String_Index
           := Utf16_String_Index'Min (Left.Unused, Right.Unused);
         LV    : System.Address := Left.Value'Address;
         RV    : System.Address := Right.Value'Address;
         J     : Utf16_String_Index := 0;
         M     : Unsigned_32;
         Index : Utf16_String_Index;

      begin
         loop
            M :=
              mm_movemask_epi8
               (mm_cmpeq_epi16 (To_Pointer (LV).all, To_Pointer (RV).all));

            if M /= 16#0000_FFFF# then
               Index := J * 8 + Utf16_String_Index (ffs (not M) / 2);

               return Is_Less (Left.Value (Index), Right.Value (Index));
            end if;

            exit when J = Min / 8;

            J := J + 1;
            LV := LV + 16;
            RV := RV + 16;
         end loop;
      end;

      return Left.Unused < Right.Unused;
   end Is_Less;

   ----------------------
   -- Is_Less_Or_Equal --
   ----------------------

   overriding function Is_Less_Or_Equal
    (Self  : X86_SSE2_String_Handler;
     Left  : not null Shared_String_Access;
     Right : not null Shared_String_Access) return Boolean
   is
      pragma Unreferenced (Self);

      pragma Suppress (Access_Check);
      pragma Suppress (Index_Check);

   begin
      if Left = Right then
         return True;
      end if;

      declare
         Min   : constant Utf16_String_Index
           := Utf16_String_Index'Min (Left.Unused, Right.Unused);
         LV    : System.Address := Left.Value'Address;
         RV    : System.Address := Right.Value'Address;
         J     : Utf16_String_Index := 0;
         M     : Unsigned_32;
         Index : Utf16_String_Index;

      begin
         loop
            M :=
              mm_movemask_epi8
               (mm_cmpeq_epi16
                 (To_Pointer (LV).all, To_Pointer (RV).all));

            if M /= 16#0000_FFFF# then
               Index := J * 8 + Utf16_String_Index (ffs (not M) / 2);

               return Is_Less (Left.Value (Index), Right.Value (Index));
            end if;

            exit when J = Min / 8;

            J := J + 1;
            LV := LV + 16;
            RV := RV + 16;
         end loop;
      end;

      return Left.Unused <= Right.Unused;
   end Is_Less_Or_Equal;

   ----------------
   -- Last_Index --
   ----------------

   overriding function Last_Index
    (Self          : X86_SSE2_String_Handler;
     Item          : Matreshka.Internals.Strings.Shared_String_Access;
     From_Position : Matreshka.Internals.Utf16.Utf16_String_Index;
     To_Index      : Positive;
     To_Position   : Matreshka.Internals.Utf16.Utf16_String_Index;
     Code          : Matreshka.Internals.Unicode.Code_Point) return Natural
   is
      Value : v8hi_Unrestricted_Array;
      for Value'Address use Item.Value'Address;
      pragma Import (Ada, Value);

   begin
      if Code <= 16#FFFF# then
         if Utf16_String_Index (Item.Length) = Item.Unused then
            return
              Last_Index_16_Without_Surrogates
               (Value, From_Position, To_Index, To_Position, Code);

         else
            return
              Last_Index_16
               (Value, From_Position, To_Index, To_Position, Code);
         end if;

      else
         return
           Base_String_Handler (Self).Last_Index
            (Item, From_Position, To_Index, To_Position, Code);
      end if;
   end Last_Index;

   -------------------
   -- Last_Index_16 --
   -------------------

   function Last_Index_16
    (Value         : v8hi_Unrestricted_Array;
     From_Position : Utf16_String_Index;
     To_Index      : Positive;
     To_Position   : Utf16_String_Index;
     Code          : Matreshka.Internals.Unicode.Code_Point) return Natural
   is
      pragma Suppress (All_Checks);

      From_Vector    : constant Utf16_String_Index := From_Position / 8;
      From_Offset    : constant Utf16_String_Index := From_Position mod 8;
      To_Vector      : constant Utf16_String_Index := (To_Position - 1) / 8;
      To_Offset      : constant Utf16_String_Index := (To_Position - 1) mod 8;

      Pattern        : constant v8hi := (others => Integer_16 (Code));
      Match_Mask     : Unsigned_32;
      Aux_Mask       : Unsigned_32;
      Exclusion_Mask : Unsigned_32;
      Current_Vector : v8hi;
      Head_Mask      : constant Unsigned_32
        := 16#FFFF_FFFF# * (2 ** Natural (Unsigned_32 (From_Offset * 2)));
      Tail_Mask      : constant Unsigned_32
        := 16#FFFF_FFFF# / (2 ** Natural (30 - To_Offset * 2));
      Index          : Integer := To_Index;
      Current        : System.Address := Value (To_Vector)'Address;

   begin
      if From_Vector = To_Vector then
         Current_Vector := To_Pointer (Current).all;

         --  Compute 'match' mask by compare vector of string with pattern and
         --  excluding match of needed leading and trailing code units. If
         --  match is found construct mask to exclude all leading and trailing
         --  code units.

         Match_Mask :=
           mm_movemask_epi8 (mm_cmpeq_epi16 (Current_Vector, Pattern))
             and Head_Mask and Tail_Mask;

         if Match_Mask = 0 then
            --  No match found, return 0.

            return 0;
         end if;

         Match_Mask := 16#FFFF_FFFF# * (2 ** Natural (32 - clz (Match_Mask)));

         --  Code units from high surrogate range must be ignored when
         --  computing index of character. Complete exclusion mask includes all
         --  leading code units as well as all code units after found one.

         Exclusion_Mask :=
           mm_movemask_epi8
            (mm_cmpeq_epi16
              (mm_and_si128 (Current_Vector, Surrogate_Kind_Mask_x86_64),
               Masked_High_Surrogate_x86_64)) or not Match_Mask or not Tail_Mask;

         --  Update index of character. It will be index of found character.

         Update_Index_Backward (Exclusion_Mask and 16#FFFF#, Index);

         return Index;

      else
         --  First step: process vector which includes last character of the
         --  slice.

         Current_Vector := To_Pointer (Current).all;
         Current := Current - 16;

         --  Compute 'match' mask by compare vector of string with pattern and
         --  excluding match of needed last code units. If match is found
         --  construct mask to exclude all trailing code units.

         Match_Mask :=
           mm_movemask_epi8 (mm_cmpeq_epi16 (Current_Vector, Pattern))
             and Tail_Mask;

         if Match_Mask /= 0 then
            Aux_Mask :=
              16#FFFF_FFFF# * (2 ** Natural (32 - clz (Match_Mask)));

         else
            Aux_Mask := 16#FFFF_FFFF#;
         end if;

         --  Code units from high surrogate range must be ignored when
         --  computing index of character. Complete exclusion mask includes all
         --  leading code units as well as all code units after found one.

         Exclusion_Mask :=
           mm_movemask_epi8
            (mm_cmpeq_epi16
              (mm_and_si128 (Current_Vector, Surrogate_Kind_Mask_x86_64),
               Masked_High_Surrogate_x86_64)) or not Aux_Mask or not Tail_Mask;

         --  Update index of character. It will be index of found character.

         Update_Index_Backward (Exclusion_Mask and 16#FFFF#, Index);

         if Match_Mask /= 0 then
            return Index;
         end if;

         --  Second step: process all string's vectors between first and last.

         while Current /= Value (From_Vector)'Address loop
            Current_Vector := To_Pointer (Current).all;
            Current := Current - 16;

            --  Compute 'match' mask by compare vector of string with pattern
            --  and excluding match of needed leading and trailing code units.
            --  If match is found construct mask to exclude all leading and
            --  trailing code units.

            Match_Mask :=
              mm_movemask_epi8 (mm_cmpeq_epi16 (Current_Vector, Pattern));

            if Match_Mask /= 0 then
               Aux_Mask :=
                 16#FFFF_FFFF# * (2 ** Natural (32 - clz (Match_Mask)));

            else
               Aux_Mask := 16#FFFF_FFFF#;
            end if;

            --  Code units from high surrogate range must be ignored when
            --  computing index of character. Complete exclusion mask includes
            --  all leading code units as well as all code units after found
            --  one.

            Exclusion_Mask :=
              mm_movemask_epi8
               (mm_cmpeq_epi16
                 (mm_and_si128 (Current_Vector, Surrogate_Kind_Mask_x86_64),
                  Masked_High_Surrogate_x86_64)) or not Aux_Mask;

            --  Update index of character. It will be index of found character.

            Update_Index_Backward (Exclusion_Mask and 16#FFFF#, Index);

            if Match_Mask /= 0 then
               return Index;
            end if;
         end loop;

         --  Third step: process vector which includes first character of the
         --  slice.

         Current_Vector := To_Pointer (Current).all;

         --  Compute 'match' mask by compare vector of string with pattern and
         --  excluding match of needed leading and trailing code units. If
         --  match is found construct mask to exclude all leading and trailing
         --  code units.

         Match_Mask :=
           mm_movemask_epi8 (mm_cmpeq_epi16 (Current_Vector, Pattern))
             and Head_Mask;

         if Match_Mask = 0 then
            --  No match found, return 0.

            return 0;
         end if;

         Match_Mask := 16#FFFF_FFFF# * (2 ** Natural (32 - clz (Match_Mask)));

         --  Code units from high surrogate range must be ignored when
         --  computing index of character. Complete exclusion mask includes all
         --  leading code units as well as all code units after found one.

         Exclusion_Mask :=
           mm_movemask_epi8
            (mm_cmpeq_epi16
              (mm_and_si128 (Current_Vector, Surrogate_Kind_Mask_x86_64),
               Masked_High_Surrogate_x86_64)) or not Match_Mask;

         --  Update index of character. It will be index of found character.

         Update_Index_Backward (Exclusion_Mask and 16#FFFF#, Index);

         return Index;
      end if;
   end Last_Index_16;

   --------------------------------------
   -- Last_Index_16_Without_Surrogates --
   --------------------------------------

   function Last_Index_16_Without_Surrogates
    (Value         : v8hi_Unrestricted_Array;
     From_Position : Utf16_String_Index;
     To_Index      : Positive;
     To_Position   : Utf16_String_Index;
     Code          : Matreshka.Internals.Unicode.Code_Point) return Natural
   is
      pragma Suppress (All_Checks);

      From_Vector    : constant Utf16_String_Index := From_Position / 8;
      From_Offset    : constant Utf16_String_Index := From_Position mod 8;
      To_Vector      : constant Utf16_String_Index := (To_Position - 1) / 8;
      To_Offset      : constant Utf16_String_Index := (To_Position - 1) mod 8;

      Pattern        : constant v8hi := (others => Integer_16 (Code));
      Match_Mask     : Unsigned_32;
      Aux_Mask       : Unsigned_32;
      Exclusion_Mask : Unsigned_32;
      Current_Vector : v8hi;
      Head_Mask      : constant Unsigned_32
        := 16#FFFF_FFFF# * (2 ** Natural (Unsigned_32 (From_Offset * 2)));
      Tail_Mask      : constant Unsigned_32
        := 16#FFFF_FFFF# / (2 ** Natural (30 - To_Offset * 2));
      Index          : Integer := To_Index;
      Current        : System.Address := Value (To_Vector)'Address;

   begin
      if From_Vector = To_Vector then
         Current_Vector := To_Pointer (Current).all;

         --  Compute 'match' mask by compare vector of string with pattern and
         --  excluding match of needed leading and trailing code units. If
         --  match is found construct mask to exclude all leading and trailing
         --  code units.

         Match_Mask :=
           mm_movemask_epi8 (mm_cmpeq_epi16 (Current_Vector, Pattern))
             and Head_Mask and Tail_Mask;

         if Match_Mask = 0 then
            --  No match found, return 0.

            return 0;
         end if;

         Match_Mask := 16#FFFF_FFFF# * (2 ** Natural (32 - clz (Match_Mask)));

         --  Complete exclusion mask includes all leading code units as well as
         --  all code units after found one.

         Exclusion_Mask := (not Match_Mask) or (not Tail_Mask);

         --  Update index of character. It will be index of found character.

         Update_Index_Backward (Exclusion_Mask and 16#FFFF#, Index);

         return Index;

      else
         --  First step: process vector which includes last character of the
         --  slice.

         Current_Vector := To_Pointer (Current).all;
         Current := Current - 16;

         --  Compute 'match' mask by compare vector of string with pattern and
         --  excluding match of needed last code units. If match is found
         --  construct mask to exclude all trailing code units.

         Match_Mask :=
           mm_movemask_epi8 (mm_cmpeq_epi16 (Current_Vector, Pattern))
             and Tail_Mask;

         if Match_Mask /= 0 then
            Aux_Mask :=
              16#FFFF_FFFF# * (2 ** Natural (32 - clz (Match_Mask)));

         else
            Aux_Mask := 16#FFFF_FFFF#;
         end if;

         --  Complete exclusion mask includes all leading code units as well as
         --  all code units after found one.

         Exclusion_Mask := (not Aux_Mask) or (not Tail_Mask);

         --  Update index of character. It will be index of found character.

         Update_Index_Backward (Exclusion_Mask and 16#FFFF#, Index);

         if Match_Mask /= 0 then
            return Index;
         end if;

         --  Second step: process all string's vectors between first and last.

         while Current /= Value (From_Vector)'Address loop
            Current_Vector := To_Pointer (Current).all;
            Current := Current - 16;

            --  Compute 'match' mask by compare vector of string with pattern
            --  and excluding match of needed leading and trailing code units.
            --  If match is found construct mask to exclude all leading and
            --  trailing code units.

            Match_Mask :=
              mm_movemask_epi8 (mm_cmpeq_epi16 (Current_Vector, Pattern));

            if Match_Mask /= 0 then
               Aux_Mask :=
                 16#FFFF_FFFF# * (2 ** Natural (32 - clz (Match_Mask)));

            else
               Aux_Mask := 16#FFFF_FFFF#;
            end if;

            --  Complete exclusion mask includes all leading code units as well
            --  as all code units after found one.

            Exclusion_Mask := not Aux_Mask;

            --  Update index of character. It will be index of found character.

            Update_Index_Backward (Exclusion_Mask and 16#FFFF#, Index);

            if Match_Mask /= 0 then
               return Index;
            end if;
         end loop;

         --  Third step: process vector which includes first character of the
         --  slice.

         Current_Vector := To_Pointer (Current).all;

         --  Compute 'match' mask by compare vector of string with pattern and
         --  excluding match of needed leading and trailing code units. If
         --  match is found construct mask to exclude all leading and trailing
         --  code units.

         Match_Mask :=
           mm_movemask_epi8 (mm_cmpeq_epi16 (Current_Vector, Pattern))
             and Head_Mask;

         if Match_Mask = 0 then
            --  No match found, return 0.

            return 0;
         end if;

         Match_Mask := 16#FFFF_FFFF# * (2 ** Natural (32 - clz (Match_Mask)));

         --  Complete exclusion mask includes all leading code units as well as
         --  all code units after found one.

         Exclusion_Mask := not Match_Mask;

         --  Update index of character. It will be index of found character.

         Update_Index_Backward (Exclusion_Mask and 16#FFFF#, Index);

         return Index;
      end if;
   end Last_Index_16_Without_Surrogates;

   ------------------
   -- mm_and_si128 --
   ------------------

   function mm_and_si128 (Left : v8hi; Right : v8hi) return v8hi is
   begin
      return To_v8hi (mm_and_si128 (To_v2di (Left), To_v2di (Right)));
   end mm_and_si128;

   ----------------------
   -- mm_movemask_epi8 --
   ----------------------

   function mm_movemask_epi8 (Item : v8hi) return Interfaces.Unsigned_32 is
   begin
      return To_Unsigned_32 (mm_movemask_epi8 (To_v16qi (Item)));
   end mm_movemask_epi8;

end Matreshka.Internals.Strings.Handlers.Generic_X86_SSE2;
