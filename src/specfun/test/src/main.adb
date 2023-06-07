with Specfun.Generic_Special_Functions;
with Ada.Text_IO;  use Ada.Text_IO;
with Ada.Numerics.Generic_Elementary_Functions;

with Function_Samples.Gamma_Samples;
with Function_Samples.Log_Gamma_Samples;
with Function_Samples.Log_Beta_Samples;
with Function_Samples.Beta_Inc_Samples;
--  with Function_Samples.Log_Beta_Samples_2;
--  with Function_Samples.Log_Beta_Samples_3;
--  with Function_Samples.Log_Beta_Samples_4;
--  with Function_Samples.Log_Beta_Samples_5;

procedure Main is
   pragma SPARK_Mode;

   use Function_Samples;

   package Spec is
     new Specfun.Generic_Special_Functions (Float_Type);

   use Spec;

   package Ele is
     new Ada.Numerics.Generic_Elementary_Functions (Float_Type);

   function Log (X : Float_Type) return Float_Type
     with
       Pre => X > 0.0,
       Global => null;

   function Log (X : Float_Type) return Float_Type
   is (Ele.Log (X))
     with
       SPARK_Mode => Off;


   procedure Test_Ln_X_Plus_1 (Verbose : Boolean := True) is
      Step    : constant Float_Type := 0.01;
      Start   : constant Float_Type := -0.1;
      Stop    : constant Float_Type := 0.1;
      X       : Float_Type;
      Err     : Float_Type;
      Max_Err : Float_Type := 0.0;
   begin
      X := Start;
      while X <= Stop loop
         Err := abs (Log_X_Plus_1 (X)-Log (X + 1.0));

         Max_Err := Float_Type'Max (Err, Max_Err);

         if Verbose then
            Put_Line (Log_X_Plus_1 (X)'Image
                      & "  "
                      & Log (X + 1.0)'Image
                      & "  "
                      & Float_Type'Image (Err));
         end if;

         X := X + Step;
      end loop;

      Put_Line ("ln(x+1)   : " & Max_Err'Image);
   end Test_Ln_X_Plus_1;

   procedure Multi_Test;

   procedure Multi_Test
   is
      pragma SPARK_Mode (Off);

      Max_Err     : Error_Type;
      Max_Pos     : Argument_Type;
      Max_Pair    : Function_Samples.Argument_Pair;
      Max_Triplet : Function_Samples.Argument_Triplet;
   begin

      Test_Function (Data    => Gamma_Samples.Samples,
                     Fun     => Gamma'Access,
                     Name    => "gamma     ",
                     Max_Err => Max_Err,
                     Max_Pos => Max_Pos,
                     Verbose => False);

      Test_Function (Data    => Log_Gamma_Samples.Samples,
                     Fun     => Log_Gamma'Access,
                     Name    => "log gamma ",
                     Max_Err => Max_Err,
                     Max_Pos => Max_Pos,
                     Verbose => False);

      Test_Function (Data    => Log_Beta_Samples.Samples,
                     Fun     => Log_Beta'Access,
                     Name    => "log beta  ",
                     Max_Err => Max_Err,
                     Max_Pos => Max_Pair,
                     Verbose => False);

      Test_Function (Data    => Beta_Inc_Samples.Samples,
                     Fun     => Beta_Incomplete'Access,
                     Name    => "beta inc. ",
                     Max_Err => Max_Err,
                     Max_Pos => Max_Triplet,
                     Verbose => False);

   end Multi_Test;
begin
   Test_Ln_X_Plus_1 (Verbose => False);

   Multi_Test;
end Main;
