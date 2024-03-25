with Ada.Text_IO;

package body MathUtility is

   function Add (x, y : Integer) return Integer is
   begin
      return x + y;
   end Add;

   function Subtract (x, y : Integer) return Integer is
   begin
      return x - y;
   end Subtract;

   procedure PrintResult (result : Integer) is
      use Ada.Text_IO;
   begin
      Ada.Text_IO.Put_Line (Integer'Image (result));
   end PrintResult;

end MathUtility;
