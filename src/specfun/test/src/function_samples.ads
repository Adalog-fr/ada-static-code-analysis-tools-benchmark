with Generic_Function_Test;

package Function_Samples is
   subtype Float_Type is Long_Float;

   type Argument_Type is new Float_Type;

   type Result_Type is new Float_Type;

   type Error_Type is new Float_Type;

   type Argument_Array is array (Positive range <>) of Argument_Type;

   subtype Argument_Pair is Argument_Array (1 .. 2);

   subtype Argument_Triplet is Argument_Array (1 .. 3);

   function Image (X : Argument_Array) return String;

   --
   --  Kind of "symmetric relative error," using the average of X and Y
   --  as reference.  Reasonable if X and Y are quite close (as we expect them
   --  to be.
   --
   function Distance (X, Y : Result_Type) return Error_Type
   is (abs Error_Type ((X - Y) / Result_Type'Max (abs X, abs Y)));
   --  function Distance (X, Y : Result_Type) return Error_Type
   --  is (abs Error_Type (X - Y));

   package Singleton_Test is
     new Generic_Function_Test (Argument_Type  => Argument_Type,
                                Result_Type    => Result_Type,
                                Error_Type     => Error_Type,
                                Distance       => Distance);

   package Singleton_Maxerr is
     new Singleton_Test.Norm_Infinite (Smallest_Error => 0.0);

   package Pair_Test is
     new Generic_Function_Test (Argument_Type  => Argument_Pair,
                                Result_Type    => Result_Type,
                                Error_Type     => Error_Type,
                                Distance       => Distance);
   package Pair_Maxerr is
     new Pair_Test.Norm_Infinite (Smallest_Error => 0.0);

   package Triplet_Test is
     new Generic_Function_Test (Argument_Type  => Argument_Triplet,
                                Result_Type    => Result_Type,
                                Error_Type     => Error_Type,
                                Distance       => Distance);

   package Triplet_Maxerr is
     new Triplet_Test.Norm_Infinite (Smallest_Error => 0.0);


   --  type Sample_Type is
   --     record
   --        X : Argument_Type;
   --        Y : Result_Type;
   --     end record;
   --
   --  type Sample_Array is array (Positive range <>) of Sample_Type;
   --
   procedure Test_Function
     (Data    : Singleton_Test.Sample_Array;
      Fun     : access function (X : Float_Type) return Float_Type;
      Name    : String;
      Max_Err : out Error_Type;
      Max_Pos : out Argument_Type;
      Verbose : Boolean := False);

   procedure Test_Function
     (Data    : Triplet_Test.Sample_Array;
      Fun     : access function (X, Y, Z : Float_Type) return Float_Type;
      Name    : String;
      Max_Err : out Error_Type;
      Max_Pos : out Argument_Triplet;
      Verbose : Boolean := False);

   procedure Test_Function
     (Data    : Pair_Test.Sample_Array;
      Fun     : access function (X, Y : Float_Type) return Float_Type;
      Name    : String;
      Max_Err : out Error_Type;
      Max_Pos : out Argument_Pair;
      Verbose : Boolean := False);

   --  --
   --  procedure Test_Function
   --    (Data    : Sample_Array;
   --     Fun     : access function (Extra, X : Float_Type) return Float_Type;
   --     Name    : String;
   --     Extra   : Float_Type;
   --     Max_Err : out Error_Type;
   --     Max_Pos : out Argument_Type;
   --     Verbose : Boolean := True);
end Function_Samples;
