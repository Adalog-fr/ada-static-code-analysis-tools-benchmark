pragma Ada_2012;
with Ada.Strings.Unbounded;

package body Function_Samples is

   -----------
   -- Image --
   -----------

   function Image (X : Argument_Array) return String
   is
      use Ada.Strings.Unbounded;

      Result : Unbounded_String := Null_Unbounded_String;
   begin
      Append (Result, "(");

      for I in X'Range loop
         Append (Result, X (I)'Image);

         if I < X'Last then
            Append (Result, ", ");
         end if;
      end loop;

      Append (Result, ")");

      return To_String (Result);
   end Image;

   procedure Test_Function
     (Data    : Singleton_Test.Sample_Array;
      Fun     : access function (X : Float_Type) return Float_Type;
      Name    : String;
      Max_Err : out Error_Type;
      Max_Pos : out Argument_Type;
      Verbose : Boolean := False)
   is
      package IO is
        new Singleton_Test.IO (Argument_Image => Argument_Type'Image,
                               Result_Image   => Result_Type'Image,
                               Error_Image    => Error_Type'Image);

      function F (X : Argument_Type) return Result_Type
      is (Result_Type (Fun (Float_Type (X))));
   begin
      Singleton_Maxerr.Test_Function
        (Data         => Data,
         Tested       => F'Access,
         Max_Err      => Max_Err,
         Max_Pos      => Max_Pos,
         Logger       => (if Verbose then
                               Io.Create
                          else
                             Singleton_Test.Void_Logger));

      Io.Report (Name    => Name,
                 Max_Err => Max_Err,
                 Where   => Max_Pos);
   end Test_Function;

   procedure Test_Function
     (Data    : Pair_Test.Sample_Array;
      Fun     : access function (X, Y : Float_Type) return Float_Type;
      Name    : String;
      Max_Err : out Error_Type;
      Max_Pos : out Argument_Pair;
      Verbose : Boolean := False)
   is
      package IO is
        new Pair_Test.IO (Argument_Image => Image,
                          Result_Image   => Result_Type'Image,
                          Error_Image    => Error_Type'Image);

      function F (X : Argument_Pair) return Result_Type
      is (Result_Type
          (Fun
             (Float_Type (X (1)), Float_Type (X (2)))
            ));
   begin
      Pair_Maxerr.Test_Function
        (Data         => Data,
         Tested       => F'Access,
         Max_Err      => Max_Err,
         Max_Pos      => Max_Pos,
         Logger       => (if Verbose then
                               Io.Create
                          else
                             Pair_Test.Void_Logger));

      Io.Report (Name    => Name,
                 Max_Err => Max_Err,
                 Where   => Max_Pos);
   end Test_Function;

   procedure Test_Function
     (Data    : Triplet_Test.Sample_Array;
      Fun     : access function (X, Y, Z : Float_Type) return Float_Type;
      Name    : String;
      Max_Err : out Error_Type;
      Max_Pos : out Argument_Triplet;
      Verbose : Boolean := False)
   is
      package IO is
        new Triplet_Test.IO (Argument_Image => Image,
                             Result_Image   => Result_Type'Image,
                             Error_Image    => Error_Type'Image);

      function F (X : Argument_Triplet) return Result_Type
      is (Result_Type
          (Fun
             (Float_Type (X (1)), Float_Type (X (2)), Float_Type (X (3)))
            ));
   begin
      Triplet_Maxerr.Test_Function
        (Data         => Data,
         Tested       => F'Access,
         Max_Err      => Max_Err,
         Max_Pos      => Max_Pos,
         Logger       => (if Verbose then
                               Io.Create
                          else
                             Triplet_Test.Void_Logger));

      Io.Report (Name    => Name,
                 Max_Err => Max_Err,
                 Where   => Max_Pos);
   end Test_Function;

end Function_Samples;
--  -------------------
--  -- Test_Function --
--  -------------------
--
--  procedure Test_Function
--    (Data    : Sample_Array;
--     Fun     : access function (X : Float_Type) return Float_Type;
--     Name    : String;
--     Max_Err : out Error_Type;
--     Max_Pos : out Argument_Type;
--     Verbose : Boolean := True)
--  is
--     function F (X : Argument_Type) return Result_Type
--     is (Result_Type (Fun (Float_Type (X))));
--
--     Err     : Error_Type;
--     Y       : Result_Type;
--  begin
--     Max_Err := 0.0;
--     Max_Pos := -42.0;
--
--     for Sample of Data loop
--        Y := F (Sample.X);
--        Err := abs Error_Type ((Sample.Y - Y) / Sample.Y);
--
--        if Err > Max_Err then
--           Max_Err := Err;
--           Max_Pos := Sample.X;
--        end if;
--
--        if Verbose then
--           Put_Line (Sample.X'Image
--                     & "  " & Y'Image
--                     & " " & Sample.Y'Image
--                     & " " & Err'Image);
--        end if;
--     end loop;
--
--     Put_Line (Name & ": " & Max_Err'Image & " @ " & Max_Pos'Image);
--  end Test_Function;
--
--  -------------------
--  -- Test_Function --
--  -------------------
--
--  procedure Test_Function
--    (Data    : Sample_Array;
--     Fun     : access function (Extra, X : Float_Type) return Float_Type;
--     Name    : String;
--     Extra   : Float_Type;
--     Max_Err : out Error_Type;
--     Max_Pos : out Argument_Type;
--     Verbose : Boolean := True)
--  is
--     function F (A : Float_Type) return Float_Type
--     is (Fun (Extra, A));
--  begin
--     Test_Function (Data    => Data,
--                    Fun     => F'Access,
--                    Name    => Name,
--                    Max_Err => Max_Err,
--                    Max_Pos => Max_Pos,
--                    Verbose => Verbose);
--  end Test_Function;

