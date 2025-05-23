--  See specification for license and credits.

with Ada.Numerics.Generic_Elementary_Functions;
with Error_function;

package body Phi_function is

  -- Ada 95 Quality and Style Guide, 7.2.7:
  -- Tests for
  --
  -- (1) absolute "equality" to 0 in storage,
  -- (2) absolute "equality" to 0 in computation,
  -- (3) relative "equality" to 0 in storage, and
  -- (4) relative "equality" to 0 in computation:
  --
  --  abs X <= Float_Type'Model_Small                      -- (1)
  --  abs X <= Float_Type'Base'Model_Small                 -- (2)
  --  abs X <= abs X * Float_Type'Model_Epsilon            -- (3)
  --  abs X <= abs X * Float_Type'Base'Model_Epsilon       -- (4)

  package GEF is new Ada.Numerics.Generic_Elementary_Functions(Real);
  use GEF;

  -- Algorithm 26.2.17 <http://www.math.sfu.ca/~cbm/aands/page_932.htm>
  -- from Abromowitz and Stegun, Handbook of Mathematical Functions.
  -- It has a maximum absolute error of 7.5e-8.

  function Phi_Single_Precision (x: Real) return Real is
    t, poly, y: Real;
    b1 : constant:=  0.31938_1530;
    b2 : constant:= -0.35656_3782;
    b3 : constant:=  1.78147_7937;
    b4 : constant:= -1.82125_5978;
    b5 : constant:=  1.33027_4429;
    p  : constant:=  0.23164_19;
    c  : constant:=  0.398942280401433; -- ~= 1/Sqrt(2*pi)
    mh : constant:=  -1.0 / 2.0;
  begin
    t := 1.0 / (1.0 + p * abs x);
    poly:= t * (t * (t * (t * (t * b5 + b4) + b3) + b2) + b1);
    y:= c * Exp(x * x * mh) * poly;
    if x >= 0.0 then
      return 1.0 - y;
    else
      return y;
    end if;
  end Phi_Single_Precision;
  pragma Unreferenced (Phi_Single_Precision);

  package Erf_pkg is new Error_function (Real);

  --  ************************************************************************
  --  Normal cumulative distribution function
  --
  --  Returns the area under the Gaussian probability density
  --  function, integrated from minus infinity to x:
  --
  --                           x
  --                           -
  --                 1        | |          2
  --  ndtr(x)  = ---------    |    exp( - t /2 ) dt
  --             sqrt(2pi)  | |
  --                         -
  --                       -inf.
  --
  --           =  ( 1 + erf(z) ) / 2
  --
  --  where z = x/sqrt(2). Computation is via the functions erf and erfc.
  --
  --
  --  ACCURACY:
  --
  --                    Relative error:
  --  arithmetic   domain     # trials      peak         rms
  --  IEEE          -13,0        30000      3.4e-14      6.7e-15
  --
  --  Cephes Math Library Release 2.8:  June, 2000
  --  Copyright 1984, 1987, 1988, 1992, 2000 by Stephen L. Moshier
  --  ************************************************************************

  function Phi (x: Real) return Real is
  begin
    return 0.5 * (Erf_pkg.Erf (x * 0.70710678118654752440084436210485) + 1.0);
  end Phi;

  --  ************************************************************************
  --  Inverse of Normal distribution function
  --
  --  Returns the argument, x, for which the area under the
  --  Gaussian probability density function (integrated from
  --  minus infinity to x) is equal to y.
  --
  --
  --  For small arguments 0 < y < exp(-2), the program computes
  --  z = sqrt( -2.0 * log(y) );  then the approximation is
  --  x = z - log(z)/z  - (1/z) P(1/z) / Q(1/z).
  --  There are two rational functions P/Q, one for 0 < y < exp(-32)
  --  and the other for y up to exp(-2).  For larger arguments,
  --  w = y - 0.5, and  x/sqrt(2pi) = w + w**3 R(w**2)/S(w**2)).
  --
  --  ACCURACY:
  --
  --                    Relative error:
  --  arithmetic   domain        # trials      peak         rms
  --  IEEE     0.125, 1        20000       7.2e-16     1.3e-16
  --  IEEE     3e-308, 0.135   50000       4.6e-16     9.8e-17
  --
  --  Cephes Math Library Release 2.8:  June, 2000
  --  Copyright 1984, 1987, 1988, 1992, 2000 by Stephen L. Moshier
  --  ************************************************************************

  function Inverse_Phi (y : Real) return Real is
    Expm2: constant:= 0.13533528323661269189;
    S2Pi : constant:= 2.50662827463100050242;
    x, y_var, z, y2, x0, x1:Real;
    code: Integer:= 1;
    P0, Q0, P1, Q1, P2, Q2:Real;
  begin
    if y <= 0.0 then  --  was < in the Pascal code -> causes a Log(0.0) later...
      return Real'First;
    end if;
    if y >= 1.0 then
      return Real'Last;
    end if;
    y_var := y;
    if y_var > 1.0 - Expm2 then
      y_var := 1.0 - y_var;
      code := 0;
    end if;
    if y_var > Expm2 then
        y_var := y_var - 0.5;
        y2 := y_var*y_var;
        P0 := -59.9633501014107895267;
        P0 :=  98.0010754185999661536+y2*P0;
        P0 := -56.6762857469070293439+y2*P0;
        P0 :=  13.9312609387279679503+y2*P0;
        P0 := -1.23916583867381258016+y2*P0;
        Q0 := 1.0;
        Q0 :=  1.95448858338141759834+y2*Q0;
        Q0 :=  4.67627912898881538453+y2*Q0;
        Q0 :=  86.3602421390890590575+y2*Q0;
        Q0 := -225.462687854119370527+y2*Q0;
        Q0 :=  200.260212380060660359+y2*Q0;
        Q0 := -82.0372256168333339912+y2*Q0;
        Q0 :=  15.9056225126211695515+y2*Q0;
        Q0 := -1.18331621121330003142+y2*Q0;
        x := y_var + y_var*y2*P0/Q0;
        return x*S2Pi;
    end if;
    x := Sqrt(-2.0*Log(y_var));
    x0 := x - Log(x)/x;
    z := 1.0/x;
    if x < 8.0 then
        P1 := 4.05544892305962419923;
        P1 := 31.5251094599893866154+z*P1;
        P1 := 57.1628192246421288162+z*P1;
        P1 := 44.0805073893200834700+z*P1;
        P1 := 14.6849561928858024014+z*P1;
        P1 := 2.18663306850790267539+z*P1;
        P1 := -1.40256079171354495875*0.1+z*P1;
        P1 := -3.50424626827848203418*0.01+z*P1;
        P1 := -8.57456785154685413611*0.0001+z*P1;
        Q1 := 1.0;
        Q1 := 15.7799883256466749731+z*Q1;
        Q1 := 45.3907635128879210584+z*Q1;
        Q1 := 41.3172038254672030440+z*Q1;
        Q1 := 15.0425385692907503408+z*Q1;
        Q1 := 2.50464946208309415979+z*Q1;
        Q1 := -1.42182922854787788574*0.1+z*Q1;
        Q1 := -3.80806407691578277194*0.01+z*Q1;
        Q1 := -9.33259480895457427372*0.0001+z*Q1;
        x1 := z*P1/Q1;
    else
        P2 := 3.23774891776946035970;
        P2 := 6.91522889068984211695+z*P2;
        P2 := 3.93881025292474443415+z*P2;
        P2 := 1.33303460815807542389+z*P2;
        P2 := 2.01485389549179081538*0.1+z*P2;
        P2 := 1.23716634817820021358*0.01+z*P2;
        P2 := 3.01581553508235416007*0.0001+z*P2;
        P2 := 2.65806974686737550832*0.000001+z*P2;
        P2 := 6.23974539184983293730*0.000000001+z*P2;
        Q2 := 1.0;
        Q2 := 6.02427039364742014255+z*Q2;
        Q2 := 3.67983563856160859403+z*Q2;
        Q2 := 1.37702099489081330271+z*Q2;
        Q2 := 2.16236993594496635890*0.1+z*Q2;
        Q2 := 1.34204006088543189037*0.01+z*Q2;
        Q2 := 3.28014464682127739104*0.0001+z*Q2;
        Q2 := 2.89247864745380683936*0.000001+z*Q2;
        Q2 := 6.79019408009981274425*0.000000001+z*Q2;
        x1 := z*P2/Q2;
    end if;
    x := x0 - x1;
    if code /= 0 then
      x := -x;
    end if;
    return x;
  end Inverse_Phi;

end Phi_function;
