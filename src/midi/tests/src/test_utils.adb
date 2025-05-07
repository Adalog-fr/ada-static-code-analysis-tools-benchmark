with Ada.Strings.Unbounded;
with BBqueue.Buffers;

package body Test_Utils is

   function Shift_Right
     (Value  : Storage_Element;
      Amount : Natural) return Storage_Element
     with Import, Convention => Intrinsic;

   type UInt4 is mod 2 ** 4
     with Size => 4;

   UInt4_To_Char : constant array (UInt4) of Character
     := (0 =>  '0',
         1 =>  '1',
         2 =>  '2',
         3 =>  '3',
         4 =>  '4',
         5 =>  '5',
         6 =>  '6',
         7 =>  '7',
         8 =>  '8',
         9 =>  '9',
         10 => 'A',
         11 => 'B',
         12 => 'C',
         13 => 'D',
         14 => 'E',
         15 => 'F');

   -------------------
   -- To_Str_Vector --
   -------------------

   function To_Str_Vector (Arr : Message_Array) return AAA.Strings.Vector is
      Result : AAA.Strings.Vector := AAA.Strings.Empty_Vector;
   begin
      for Elt of Arr loop
         Result.Append (MIDI.Img (Elt));
      end loop;
      return Result;
   end To_Str_Vector;

   --------------
   -- Hex_Dump --
   --------------

   function Hex_Dump (Data : Storage_Array) return AAA.Strings.Vector is
      Result : AAA.Strings.Vector;
      Cnt : Natural := 0;
   begin

      for Elt of Data loop
         Result.Append_To_Last_Line
           (UInt4_To_Char (UInt4 (Shift_Right (Elt, 4))) &
              UInt4_To_Char (UInt4 (Elt and 16#0F#)));

         Cnt := Cnt + 1;
         if Cnt = 16 then
            Result.Append ("");
            Cnt := 0;
         else
            Result.Append_To_Last_Line (" ");
         end if;
      end loop;
      return Result;
   end Hex_Dump;

   ----------
   -- Diff --
   ----------

   function Diff
     (A, B   : Storage_Array; A_Name : String := "Expected";
      B_Name : String := "Output"; Skip_Header : Boolean := False)
      return AAA.Strings.Vector
   is
   begin
      return AAA.Strings.Diff (Hex_Dump (A), Hex_Dump (B),
                               A_Name, B_Name, Skip_Header);
   end Diff;

   -----------
   -- Image --
   -----------

   function Image (D : Storage_Array) return String is
      use Ada.Strings.Unbounded;
      Result : Unbounded_String;

      First : Boolean := True;
   begin
      Append (Result, "[");
      for Elt of D loop
         if First then
            First := False;
         else
            Append (Result, ", ");
         end if;

         Append (Result,
                 UInt4_To_Char (UInt4 (Shift_Right (Elt, 4))) &
                   UInt4_To_Char (UInt4 (Elt and 16#0F#)));
      end loop;

      Append (Result, "]");
      return To_String (Result);
   end Image;

   -----------------------
   -- To_Storrage_Array --
   -----------------------

   function To_Storrage_Array (Q : in out MIDI.Encoder.Queue.Instance)
                               return Storage_Array
   is
      use type BBqueue.Result_Kind;
      use BBqueue.Buffers;

      Result : Storage_Array (1 .. Q.Capacity);
      Index : Storage_Count := Result'First - 1;

      RG : Read_Grant;
   begin
      loop
         Q.Read (RG);

         exit when State (RG) /= BBqueue.Valid;

         declare
            Src : Storage_Array (1 .. Slice (RG).Length)
              with Address => Slice (RG).Addr;
         begin
            for Elt of Src loop
               if Index < Result'Last then
                  Index := Index + 1;
                  Result (Index) := Elt;
               end if;
            end loop;
         end;
         Q.Release (RG);
      end loop;

      return Result (Result'First .. Index);
   end To_Storrage_Array;

end Test_Utils;
