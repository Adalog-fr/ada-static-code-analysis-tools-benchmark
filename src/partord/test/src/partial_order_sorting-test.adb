with Ada.Text_IO; use Ada.Text_IO;
with Partial_Order_Sorting.Array_Sort;

procedure Partial_Order_Sorting.Test is
   type Positive_Array is array (Positive range <>) of Positive;

   function Is_Multiple_Of (X, Y : Positive) return Boolean
   is ((X /= Y) and (Y mod X = 0));

   package Integer_Sort is
     new Array_Sort (Index_Type     => Positive,
                     Element_Type   => Positive,
                     Container_Type => Positive_Array,
                     "<"            => Is_Multiple_Of);

   Data : Positive_Array := (13, 25, 42, 7, 8, 9, 4, 6, 14, 21, 3, 2);
begin
   Integer_Sort.Sort (Data);

   for D of Data loop
      Put_Line (D'Image);
   end loop;
end Partial_Order_Sorting.Test;
