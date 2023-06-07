pragma Ada_2012;

with Ada.Numerics.Generic_Elementary_Functions;

package body Specfun.Generic_Special_Functions is
   pragma SPARK_Mode;

   package Spark_Friendly_Elementary_Functions is
      --
      --  This package redefines some elementary functions adding
      --  Global => null contracts, otherwise SPARK complains
      --
      package Elementary_Functions is
        new Ada.Numerics.Generic_Elementary_Functions (Float_Type);

      function Log (X : Float_Type) return Float_Type
        with
          Pre => X > 0.0,
          Global => null;

      function Exp (X : Float_Type) return Float_Type
        with
          Global => null;

      function Sin (X : Float_Type) return Float_Type
        with
          Global => null;

      function Log_10 (X : Float_Type) return Float_Type
        with
          Pre => X > 0.0,
          Global => null;

      function Sqrt (X : Float_Type) return Float_Type
        with
          Pre => X >= 0.0,
          Global => null;
   end Spark_Friendly_Elementary_Functions;

   package body Spark_Friendly_Elementary_Functions  is

      function Log (X : Float_Type) return Float_Type
      is
         pragma SPARK_Mode (Off);
      begin
         return Elementary_Functions.Log (X);
      end Log;

      function Log_10 (X : Float_Type) return Float_Type
      is
         pragma SPARK_Mode (Off);
      begin
         return Elementary_Functions.Log (X, 10.0);
      end Log_10;

      function Exp (X : Float_Type) return Float_Type
      is
         pragma SPARK_Mode (Off);
      begin
         return Elementary_Functions.Exp (X);
      end Exp;

      function Sin (X : Float_Type) return Float_Type
      is
         pragma SPARK_Mode (Off);
      begin
         return Elementary_Functions.Sin (X);
      end Sin;

      function Sqrt (X : Float_Type) return Float_Type
      is
         pragma SPARK_Mode (Off);
      begin
         return Elementary_Functions.Sqrt (X);
      end Sqrt;
   end Spark_Friendly_Elementary_Functions;

   use Spark_Friendly_Elementary_Functions;

   function Largest_Float return Float_Type
   is (Float_Type'Last);

   --  is ((Float_Type (Float_Type'Machine_Radix) ** Float_Type'Machine_Emax)
   --      * (1.0 - Float_Type'Model_Epsilon));

   function Smallest_Spacing return Float_Type
   is (Float_Type'Model_Epsilon / Float_Type (Float_Type'Machine_Radix));

   type Float_Array is array (Positive range <>) of Float_Type;

   --
   --  D1Mach is a function that returns some floating point attributes
   --  and it is widely used in the original FORTRAN code.  In order to
   --  simplify the translation, I am defining this vector here.
   --
   D1mach : constant Float_Array :=
              (1 => Float_Type'Model_Small,
               2 => Largest_Float,
               3 => Smallest_Spacing,
               4 => Float_Type'Model_Epsilon,
               5 => Log_10 (Float_Type (Float_Type'Machine_Radix)));


   function Compute_Smallest_Gamma_Parameter return Float_Type;
   function Compute_Largest_Gamma_Parameter return Float_Type;

   function Compute_Smallest_Gamma_Parameter return Float_Type
   is
      Log_Small : constant Float_Type := Log (D1mach (1));
      Result    : Float_Type := -Log_Small;
      Update    : Float_Type;
   begin
      for I in 1 .. 10 loop
         Update := Result * ((Result + 0.5) * Log (Result) - Result - 0.2258 + Log_Small)
           / (Result * Log (Result)-0.5);

         Result := Result - Update;

         if abs Update < 0.005 then
            return Float_Type'Max (0.01 - Result, 1.0 - Compute_Largest_Gamma_Parameter);
         end if;
      end loop;

      raise Program_Error with "Unable to find xmin";
   end Compute_Smallest_Gamma_Parameter;

   function Compute_Largest_Gamma_Parameter return Float_Type
   is
      Log_Big   : constant Float_Type := Log (D1mach (2));
      Result    : Float_Type := Log_Big;
      Update    : Float_Type;
   begin
      for I in 1 .. 10 loop
         Update := Result * ((Result - 0.5) * Log (Result) - Result + 0.9189 - Log_Big)
           / (Result * Log (Result)-0.5);
         Result := Result - Update;

         if abs Update < 0.005 then
            return Result - 0.01;
         end if;
      end loop;

      raise Program_Error with "Unable to find xmax";
   end Compute_Largest_Gamma_Parameter;


   Largest_Gamma_Param  : constant Float_Type := Compute_Largest_Gamma_Parameter;
   Smallest_Gamma_Param : constant Float_Type := Compute_Smallest_Gamma_Parameter;

   --  Constant used by the FORTRAN code in few routines.  It is the logarithm
   --  of the square root of 2*PI
   Sq2pil : constant := 0.9189385332_0467274178_0329736405_62;

   -- log(sqrt(pi/2))
   SQPI2L : constant := 0.2257913526_4472743236_3097614947_441;

   package Chebyshev is
      --  Determine the number of terms needed in an orthogonal
      --  polynomial series so that it meets a specified accuracy.
      function N_Terms (Coefficients : Chebyshev_Coefficients;
                        Accuracy     : Float)
                        return Positive
        with
          Pre => Coefficients'Length > 0,
          Global => null;

      function Gamma_Coefficients return Chebyshev_Coefficients;

      function Log_X_Plus_1_Coefficients return Chebyshev_Coefficients;

      function Log_Gamma_Correction_Coefficients return Chebyshev_Coefficients;

   end Chebyshev;

   package body Chebyshev is
      Standard_Accuracy : constant Float := 0.1 * Float (Smallest_Spacing);

      function N_Terms (Coefficients : Chebyshev_Coefficients;
                        Accuracy     : Float)
                        return Positive
      is
         Err    : Float := 0.0;
      begin
         if Float (abs Coefficients (Coefficients'Last)) > Accuracy then
            raise Program_Error
              with "Chebyshev series too short for specified accuracy";
         end if;

         for I in reverse Coefficients'Range loop
            ERR := ERR + abs (Float (Coefficients (I)));

            if Err > Accuracy then
               return I;
            end if;
         end loop;

         return Coefficients'First;
      end N_Terms;

      Gamma             : constant Chebyshev_Coefficients :=
                            (
                             1  => +0.857119559_989331421_200623999_2e-2,
                             2  => +0.441538132_841006757_913157716_2e-2,
                             3  => +0.568504368_599363378_326645887_9e-1,
                             4  => -0.421983539_418560501_125001866_4e-2,
                             5  => +0.132680818_212460220_840067963_2e-2,
                             6  => -0.189302452_798880432_239470238_6e-3,
                             7  => +0.360692532_441245256_780822172_5e-4,
                             8  => -0.605676190_460864218_855482903_5e-5,
                             9  => +0.105582954_302283344_318235090_3e-5,
                             10 => -0.181196736_542384048_918558911_6e-6,
                             11 => +0.311772496_715322277_902545931_9e-7,
                             12 => -0.535421963_019687140_740810243_7e-8,
                             13 => +0.919327551_859588946_877868259_0e-9,
                             14 => -0.157794128_288339761_674232739_3e-9,
                             15 => +0.270798062_934954543_665404330_9e-10,
                             16 => -0.464681865_825730144_816610589_3e-11,
                             17 => +0.797335019_007419656_607671753_9e-12,
                             18 => -0.136807820_830916025_994991723_9e-12,
                             19 => +0.234731948_563800657_334717716_8e-13,
                             20 => -0.402743261_949066932_665705346_9e-14,
                             21 => +0.691005174_372100912_383369752_7e-15,
                             22 => -0.118558450_221992907_523871261_2e-15,
                             23 => +0.203414854_496373955_010260519_2e-16,
                             24 => -0.349005434_717405849_740129491_8e-17,
                             25 => +0.598799385_485305567_350510660_6e-18,
                             26 => -0.102737805_872228074_900697784_1e-18,
                             27 => +0.176270281_060529824_427596607_8e-19,
                             28 => -0.302432065_735306260_587721120_2e-20,
                             29 => +0.518891466_218397839_178335505_6e-21,
                             30 => -0.890277084_456576692_492516010_6e-22,
                             31 => +0.152747406_493342602_745968913_6e-22,
                             32 => -0.262073125_187362900_573283327_9e-23,
                             33 => +0.449646404_830538670_310465706_6e-24,
                             34 => -0.771471273_336877911_039015253_3e-25,
                             35 => +0.132363545_126044036_865727146_6e-25,
                             36 => -0.227099941_942928816_023138133_3e-26,
                             37 => +0.389641899_003991449_208166399_9e-27,
                             38 => -0.668519811_125953327_921279999_9e-28,
                             39 => +0.114699866_140024384_476138666_6e-28,
                             40 => -0.196793858_345134677_951039999_9e-29,
                             41 => +0.337644881_585338090_348906666_6e-30,
                             42 => -0.579307033_782135784_254933333_3e-31
                            );

      Last_Gamma_Coefficient : constant Positive :=
                                 N_Terms (Gamma, Standard_Accuracy);

      Log_X_Plus_1 : constant Chebyshev_Coefficients :=
                       (
                        1  => +0.1037869356_274376980_686267719_98e+1 ,
                        2  => -0.1336430150_490891809_766041553_33e+0 ,
                        3  => +0.1940824913_552056335_926199374_50e-1 ,
                        4  => -0.3010755112_753577769_376537776_92e-2 ,
                        5  => +0.4869461479_715485009_456366509_37e-3 ,
                        6  => -0.8105488189_317535606_809943008_22e-4 ,
                        7  => +0.1377884779_955952478_938251496_59e-4 ,
                        8  => -0.2380221089_435897025_369992914_35e-5 ,
                        9  => +0.4164041621_386518347_391859901_89e-6 ,
                        10 => -0.7359582837_807599498_266837031_98e-7 ,
                        11 => +0.1311761187_624167494_152294345_11e-7 ,
                        12 => -0.2354670931_774242513_696092330_75e-8 ,
                        13 => +0.4252277327_603499777_638052962_67e-9 ,
                        14 => -0.7719089413_484079682_108107493_00e-10,
                        15 => +0.1407574648_135906990_215356472_91e-10,
                        16 => -0.2576907205_802468062_537078627_84e-11,
                        17 => +0.4734240666_629442184_154395005_38e-12,
                        18 => -0.8724901267_474264174_301263292_75e-13,
                        19 => +0.1612461490_274055146_739833119_15e-13,
                        20 => -0.2987565201_566577300_710792416_15e-14,
                        21 => +0.5548070120_908288798_041321697_79e-15,
                        22 => -0.1032461915_827156959_141333961_32e-15,
                        23 => +0.1925023920_304985117_878503244_68e-16,
                        24 => -0.3595507346_526515001_189707844_66e-17,
                        25 => +0.6726454253_787685789_194574226_73e-18,
                        26 => -0.1260262416_873521925_082425637_46e-18,
                        27 => +0.2364488440_860621004_916158955_19e-19,
                        28 => -0.4441937705_080793689_878389179_33e-20,
                        29 => +0.8354659446_403425901_241293994_66e-21,
                        30 => -0.1573155941_647956257_899253521_66e-21,
                        31 => +0.2965312874_024742268_154369706_66e-22,
                        32 => -0.5594958348_181594729_156013226_66e-23,
                        33 => +0.1056635426_883568104_187284138_66e-23,
                        34 => -0.1997248368_067020454_314999466_66e-24,
                        35 => +0.3778297781_883936142_049855999_99e-25,
                        36 => -0.7153158688_908174034_038165333_33e-26,
                        37 => +0.1355248846_367421364_502024533_33e-26,
                        38 => -0.2569467304_848756743_079829333_33e-27,
                        39 => +0.4874775606_621694907_459519999_99e-28,
                        40 => -0.9254211253_084971532_132373333_33e-29,
                        41 => +0.1757859784_176023923_269760000_00e-29,
                        42 => -0.3341002667_773101035_377066666_66e-30,
                        43 => +0.6353393618_023618735_180266666_66e-31
                       );

      Last_Log_X_Plus_1 : constant Positive :=
                            N_Terms (Log_X_Plus_1, Standard_Accuracy);

      Log_Gamma_Correction : constant Chebyshev_Coefficients :=
                               (
                                +0.1666389480_4518632472_0572965082_2e+0,
                                -0.1384948176_0675638407_3298605913_5e-4,
                                +0.9810825646_9247294261_5717154748_7e-8,
                                -0.1809129475_5724941942_6330626671_9e-10,
                                +0.6221098041_8926052271_2601554341_6e-13,
                                -0.3399615005_4177219443_0333059966_6e-15,
                                +0.2683181998_4826987489_5753884666_6e-17,
                                -0.2868042435_3346432841_4462239999_9e-19,
                                +0.3962837061_0464348036_7930666666_6e-21,
                                -0.6831888753_9857668701_1199999999_9e-23,
                                +0.1429227355_9424981475_7333333333_3e-24,
                                -0.3547598158_1010705471_9999999999_9e-26,
                                +0.1025680058_0104709120_0000000000_0e-27,
                                -0.3401102254_3167487999_9999999999_9e-29,
                                +0.1276642195_6300629333_3333333333_3e-30
                               );

      Last_Log_Gamma_Correction : constant Natural :=
                                    N_Terms (Log_Gamma_Correction, Standard_Accuracy);

      function Log_Gamma_Correction_Coefficients return Chebyshev_Coefficients
      is (Log_Gamma_Correction (Log_Gamma_Correction'First .. Last_Log_Gamma_Correction));

      function Log_X_Plus_1_Coefficients return Chebyshev_Coefficients
      is (Log_X_Plus_1 (Log_X_Plus_1'First .. Last_Log_X_Plus_1));

      function Gamma_Coefficients return Chebyshev_Coefficients
      is (Gamma (Gamma'First .. Last_Gamma_Coefficient));
   end Chebyshev;



   ---------------------
   -- Beta_Incomplete --
   ---------------------

   function Beta_Incomplete (X, A, B : Float_Type) return Float_Type is
      Eps         : constant Float_Type := Float_Type'Model_Epsilon;
      Log_Epsilon : constant Float_Type := Log (Eps);
      Small       : constant Float_Type := Float_Type'Model_Small;
      Log_Small   : constant Float_Type := Log (Small);

      function Special_Case (Y, P, Q : Float_Type;
                             Swapped : Boolean) return Float_Type is
         Xb : constant Float_Type :=
                P * Log (Float_Type'Max (Y, Small))
                - Log_Beta (P, Q)
                - Log (P);

         Result : Float_Type := 0.0;
      begin
         if Xb > Log_Small and Y /= 0.0 then
            Result := Exp (Xb);
         end if;

         if Swapped  then
            Result := 1.0 - Result;
         end if;

         return Result;
      end Special_Case;

      function First_Sum (Y, Xb, P, Ps : Float_Type) return Float_Type
      is
         Result : Float_Type := Exp (Xb);
      begin
         if Ps = 1.0 then
            return Result;
         end if;

         declare
            N      : constant Natural :=
                       Natural
                         (Float_Type'Truncation
                            (Float_Type'Max (Log_Epsilon / Log (Y), 4.0)));

            Term   : Float_Type := Result * P;
            Xi     : Float_Type;
         begin
            for I in 1 .. N loop
               Xi := Float_Type (I);

               TERM := TERM * (XI - PS) * Y / XI;
               Result := Result + TERM / (P + XI);
            end loop;

            return Result;
         end;
      end First_Sum;

      function Finite_Sum (P, Y, Q : Float_Type) return Float_Type
      is
         Xb : constant Float_Type :=
                P * LOG (Y)
                + Q * LOG (1.0 - Y)
                - Log_Beta (P, Q)
                - LOG (Q);

         Ib : Integer :=
                Integer
                  (Float_Type'Truncation
                     (Float_Type'Max (Xb / Log_Small, 0.0)));

         Term : Float_Type := EXP (XB - Float_Type (Ib) * Log_Small);

         C    : constant Float_Type := 1.0 / (1.0 - Y);

         P1 : constant Float_Type := Q * C / (P + Q - 1.0);

         N  : Integer := Integer (Float_Type'Truncation (Q));

         Result : Float_Type := 0.0;
      begin
         if Q = Float_Type (N) then
            N := N - 1;
         end if;

         for I in 1 .. N loop
            declare
               Xi : constant Float_Type := Float_Type (I);
            begin
               if P1 <= 1.0 and Term / Eps <= Result then
                  exit;
               end if;

               Term := (Q - XI + 1.0) * C * TERM / (P + Q - XI);

               if Term > 1.0 then
                  Ib := Ib - 1;
                  Term := Term * Small;
               end if;

               if Ib = 0 then
                  Result := Result + Term;
               end if;
            end;
         end loop;

         return Result;
      end Finite_Sum;

      Y, P, Q     : Float_Type;
      Swapped     : Boolean;
   begin
      if not (X in Probability) or A <= 0.0 or B <= 0.0 then
         raise Constraint_Error;
      end if;

      if (B <= A and X < 0.8) or (X < 0.2) then
         Y := X;
         P := A;
         Q := B;

         Swapped := False;
      else
         Y := 1.0 - X;
         P := B;
         Q := A;

         Swapped := True;
      end if;

      if (P + Q) * Y / (P + 1.0) < Eps then
         return Special_Case (Y, P, Q, Swapped);
      end if;

      declare
         Ps     : Float_Type;
         Xb     : Float_Type;
         Result : Float_Type;
      begin
         Ps := Q - Float_Type'Truncation (Q);
         if Ps = 0.0 then
            Ps := 1.0;
         end if;

         Xb := P * LOG (Y) - Log_Beta (PS, P) - LOG (P);

         if Xb >= Log_Small then
            Result := First_Sum (Y  => Y,
                                 Xb => Xb,
                                 P  => P,
                                 Ps => Ps);
         else
            Result := 0.0;
         end if;

         if Q > 1.0 then
            Result := Result + Finite_Sum (P => P,
                                           Y => Y,
                                           Q => Q);
         end if;

         if Swapped then
            Result := 1.0 - Result;
         end if;

         Result := Float_Type'Max (Float_Type'Min (Result, 1.0), 0.0);

         return Result;
      end;
   end Beta_Incomplete;

   ----------
   -- Beta --
   ----------

   function Log_Beta (A, B : Float_Type) return Float_Type is
      function P_And_Q_Large_Case (P, Q : Float_Type) return Float_Type
      is
         Correction : constant Float_Type :=
                        Log_Gamma_Correction (P)
                        + Log_Gamma_Correction (Q)
                        - Log_Gamma_Correction (P + Q);
      begin
         return Correction
           + SQ2PIL
           - 0.5 * LOG (Q)
           + (P - 0.5) * LOG (P / (P + Q))
           + Q * Log_X_Plus_1 (-P / (P + Q));
      end P_And_Q_Large_Case;

      function Q_Large_Case (P, Q : Float_Type) return Float_Type
      is
         Correction : constant Float_Type :=
                        Log_Gamma_Correction (Q)
                        - Log_Gamma_Correction (P + Q);
      begin
         return Correction
           + Log_Gamma (P)
           + P - P * Log (P + Q)
           + (Q - 0.5) * Log_X_Plus_1 (-P / (P + Q));
      end Q_Large_Case;

      function P_And_Q_Small_Case (P, Q : Float_Type) return Float_Type
      is
      begin
         return Log (Gamma (P) * (Gamma (Q) / Gamma (P + Q)));
      end P_And_Q_Small_Case;

      P, Q       : Float_Type;
   begin
      P := Float_Type'Min (A, B);
      Q := Float_Type'Max (A, B);

      if P <= 0.0 then
         raise Constraint_Error;
      end if;

      return (if P >= 10.0 then
                 P_And_Q_Large_Case (P, Q)

              elsif Q >= 10.0 then
                 Q_Large_Case (P, Q)

              else
                 P_And_Q_Small_Case (P, Q));
   end Log_Beta;

   function Log_Gamma (X : Float_Type) return Float_Type
   is
      use Ada.Numerics;

      X_MAX : constant Float_Type := D1MACH (2) / LOG (D1MACH (2));
      X_REL : constant Float_Type := SQRT (D1MACH (4));
   begin
      declare
         Abs_X : constant Float_Type := abs X;
      begin
         if Abs_X < 10.0 then
            return Log (abs (Gamma (X)));
         end if;

         if Abs_X > Largest_Gamma_Param then
            raise Constraint_Error with "Gamma parameter too large";
         end if;

         if X > 0.0 then
            return SQ2PIL
              + Log_Gamma_Correction (Abs_X)
              + (X - 0.5) * LOG (X) - X;

         else
            if Sin (Pi * Abs_X) = 0.0 then
               raise Constraint_Error with "Gamma parameter is a negative integer";
            end if;

            return SQPI2L
              - Log_Gamma_Correction (Abs_X)
              - Log (abs Sin (Pi * Abs_X))
              + (X - 0.5) * LOG (Abs_X) - X;
         end if;
      end;
   end Log_Gamma;

   function Gamma (X : Float_Type) return Float_Type
   is

      Xrel       : constant Float_Type := Sqrt (D1mach (4));

      function Large_Abs_Case (X : Float_Type) return Float_Type
      is
      begin
         if X > Largest_Gamma_Param then
            raise Constraint_Error with "Gamma parameter too large";
         end if;

         if X < Smallest_Gamma_Param then
            raise Constraint_Error with "Gamma parameter too small";
         end if;

         declare
            use Ada.Numerics;

            Abs_X : constant Float_Type := abs X;

            U : constant Float_Type :=
                  EXP (
                       (Abs_X - 0.5) * LOG (Abs_X)
                       - Abs_X
                       + SQ2PIL
                       + Log_Gamma_Correction (Abs_X)
                      );
         begin
            return (if X > 0.0 then
                       U
                    else
                       -Pi / (Abs_X * Sin (Pi * Abs_X) * U));
         end;
      end Large_Abs_Case;

      function Pseudo_Floor (X : Float_Type) return Integer
        with
          Post =>
            X - Float_Type (Pseudo_Floor'Result) >= 0.0
            and X - Float_Type (Pseudo_Floor'Result) < 1.0;

      function Pseudo_Floor (X : Float_Type) return Integer
      is
         --  This function is equivalent to the following FORTRAN code
         --  present in the original version.  The "N=X" assignment has
         --  caused some confusion
         --
         --  N = X
         --  IF (X.LT.0.D0) N = N - 1

         Result : Integer := Integer (Float_Type'Truncation (X));
      begin
         if X < 0.0 then
            return Result - 1;
         else
            return Result;
         end if;
      end Pseudo_Floor;
   begin
      if abs X > 10.0 then
         return Large_Abs_Case (X);
      end if;

      declare
         N : constant Integer := Pseudo_Floor (X);

         Y      : constant Float_Type := X - Float_Type (N);
         U      : constant Float_Type := 2.0 * Y - 1.0;
         Result : Float_Type :=
                    0.9375 + Eval_Chebyshev (U, Chebyshev.Gamma_Coefficients);
      begin
         pragma Assert (Y >= 0.0 and Y < 1.0);
         pragma Assert (U >= -1.0 and U < 1.0);

         --  Note that in the following code N is equal to
         --  "N in original FORTRAN code"+1

         if N = 1 then  -- it was N .EQ. 0
            return Result;

         elsif N <= 1 then
            pragma Assert (N < 1); -- The case N=1 is handled above
            --
            -- the FORTRAN code arrived here when N .LE. 0
            -- the FORTRAN code did a loop between 1 and -FORTRAN_N
            -- since N = FORTRAN_N+1, that is, FORTRAN_N = N-1,
            -- we need to loop between 1 and -(N-1)
            --
            for I in 1 .. -(N - 1) loop
               Result := Result / (X + Float_Type (I)-1.0);
            end loop;

            return Result;
         else
            pragma Assert (N > 1);
            --
            -- the FORTRAN code arrived here when N .GT. 0
            -- and has a loop between 1 and FORTRAN_N
            --
            for I in 1 .. N - 1 loop
               Result := Result * (Y + Float_Type (I));
            end loop;

            return Result;
         end if;
      end;
   end Gamma;


   function Log_Gamma_Correction (X : Float_Type) return Float_Type
   is
      Xbig  : constant Float_Type := 1.0 / Sqrt (Float_Type'Model_Epsilon);
      Xmax  : constant Float_Type := EXP (Float_Type'Min (LOG (D1MACH (2) / 12.0), -LOG (12.0 * D1MACH (1))));
   begin
      if X < 10.0 then
         raise Constraint_Error;
      end if;

      if X > Xmax then
         return 0.0;
      end if;

      if X < Xbig then
         return Eval_Chebyshev
           (2.0 * (10.0 / X) ** 2 - 1.0,
            Chebyshev.Log_Gamma_Correction_Coefficients) / X;
      else
         return 1.0 / (12.0 * X);
      end if;
   end Log_Gamma_Correction;


   function Log_X_Plus_1 (X : Float_Type) return Float_Type
   is
   begin
      if abs (X) <=  0.375 then
         return X * (1.0 - X * Eval_Chebyshev (X / 0.375, Chebyshev.Log_X_Plus_1_Coefficients));
      else
         return Log (1.0 + X);
      end if;
   end Log_X_Plus_1;


   function Eval_Chebyshev (X     : Float_Type;
                            Coeff : Chebyshev_Coefficients)
                            return Float_Type
   is
   begin
      if abs (X) > 1.0 + Largest_Spacing then
         raise Constraint_Error;
      end if;

      declare
         B0, B1, B2 : Float_Type;
      begin
         B1 := 0.0;
         B0 := 0.0;
         B2 := 0.0; -- Not strictly necessary since Coeff is not empty, but SPARK

         for I in reverse Coeff'Range loop
            B2 := B1;
            B1 := B0;
            B0 := (2.0 * X) * B1 - B2 + Coeff (I);
         end loop;

         return 0.5 * (B0 - B2);
      end;
   end Eval_Chebyshev;




end Specfun.Generic_Special_Functions;
