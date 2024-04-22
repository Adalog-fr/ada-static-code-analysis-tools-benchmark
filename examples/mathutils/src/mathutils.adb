with MathUtility; use MathUtility;

procedure Mathutils is
   Result1 : Integer;
   Result2 : Integer;

begin

   Result1 := MathUtility.Add (5, 3);
   MathUtility.PrintResult (Result1);

   Result2 := MathUtility.Subtract (5, 3);
   MathUtility.PrintResult (Result2);
end Mathutils;
