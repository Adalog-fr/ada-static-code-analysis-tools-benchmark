generic
   type Float_Type is digits <>;
package Specfun.Generic_Special_Functions is
   pragma SPARK_Mode;

   subtype Probability is Float_Type range 0.0 .. 1.0;

   function Largest_Spacing return Float_Type
   is (Float_Type'Model_Epsilon);


   function Beta_Incomplete (X, A, B : Float_Type) return Float_Type
     with
       Pre => (X in Probability) and A > 0.0 and B > 0.0;

   function Log_Beta (A, B : Float_Type) return Float_Type
     with
       Pre => A >= 0.0 and B >= 0.0;

   function Log_Gamma (X : Float_Type) return Float_Type;

   function Gamma (X : Float_Type) return Float_Type;

   function Log_Gamma_Correction (X : Float_Type) return Float_Type
     with
       Pre => X >= 10.0;

   --  Evaluate ln(1+X) accurate in the sense of relative error.
   function Log_X_Plus_1 (X : Float_Type) return Float_Type
     with Pre => X > -1.0 + Largest_Spacing;

   type Chebyshev_Coefficients is array (Positive range <>) of Float_Type;


   --
   --  Evaluate the N-term Chebyshev series with the given coefficients at X.
   --
   function Eval_Chebyshev (X     : Float_Type;
                            Coeff : Chebyshev_Coefficients)
                            return Float_Type
     with
       Pre => abs (X) <= 1.01 and Coeff'Length > 0;



end Specfun.Generic_Special_Functions;
