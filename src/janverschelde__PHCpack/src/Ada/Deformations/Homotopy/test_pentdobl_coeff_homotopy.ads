with Standard_Natural_Numbers;          use Standard_Natural_Numbers;
with Standard_Integer_Numbers;          use Standard_Integer_Numbers;
with PentDobl_Complex_Matrices;
with PentDobl_Complex_Poly_Systems;

package Test_PentDobl_Coeff_Homotopy is

-- DESCRIPTION :
--   Development of the evaluation of (1-t)*f + t*g
--   where monomials of f and g are shared
--   as in a coefficient-parameter homotopy, in penta double precision.

  procedure Write_Elements ( A,B : in PentDobl_Complex_Matrices.Matrix );

  -- DESCRIPTION :
  --   Writes the elements of the matrices A and B to screen,
  --   next to each other, for easy comparisons.

  procedure PentDobl_Compared_Encapsulated_Eval ( n : in natural32 );

  -- DESCRIPTION :
  --   Evaluates (1-t)*p + t*q at some random t in [0,1]
  --   and at a random vector in penta double complex arithmetic.

  -- ON ENTRY :
  --   n        number of variables.

  procedure PentDobl_Random_Systems
              ( n : in integer32;
                p,q : out PentDobl_Complex_Poly_Systems.Poly_Sys );

  -- DESCRIPTION :
  --   Generates two n-dimensional systems p and q,
  --   with penta double complex coefficients.

  procedure PentDobl_Random_Coefficient_Systems
              ( n : in integer32;
                p,q : out PentDobl_Complex_Poly_Systems.Poly_Sys );

  -- DESCRIPTION :
  --   Generates two n-dimensional systems p and q,
  --   with the same supports.

  procedure PentDobl_Compared_Encapsulation_Test;

  -- DESCRIPTION :
  --   Performs the evaluation test on randomly generated systems,
  --   comparing with the PentDobl_Homotopy package.

  procedure PentDobl_Homotopy_Performance ( n,m : natural32 );

  -- DESCRIPTION :
  --   Generates m random values for x and t
  --   for evaluation in the homotopy with timings taken.

  procedure PentDobl_Coefficient_Homotopy_Performance ( n,m : natural32 );

  -- DESCRIPTION :
  --   Generates m random values for x and t
  --   for evaluation in the coefficient homotopy with timings taken.

  procedure PentDobl_Performance_Test;

  -- DESCRIPTION :
  --   Performs the evaluation test on randomly generated systems,
  --   comparing with the PentDobl_Homotopy package.

  procedure Main;

  -- DESCRIPTION :
  --   Displays a menu and prompts for a test.

end Test_PentDobl_Coeff_Homotopy;
