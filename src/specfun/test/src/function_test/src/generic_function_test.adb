pragma Ada_2012;

package body Generic_Function_Test is

   package body Norm_Infinite is
      overriding
      procedure Reset (Accum : out Norm_Infinite_Accumulator)
      is
      begin
         Accum.Max := Smallest_Error;
      end Reset;

      overriding
      procedure Update (Accum : in out Norm_Infinite_Accumulator;
                        Error : Error_Type;
                        Where : Argument_Type)
      is
      begin
         if Error > Accum.Max then
            Accum.Max := Error;
            Accum.Where := Where;
         end if;
      end Update;

      function Value (Accum : Norm_Infinite_Accumulator) return Error_Type
      is (Accum.Max);

      function Where (Accum : Norm_Infinite_Accumulator) return Argument_Type
      is (Accum.Where);

      -------------------
      -- Test_Function --
      -------------------

      procedure Test_Function
        (Data     : Sample_Array;
         Tested   : access function (X : Argument_Type) return Result_Type;
         Max_Err  : out Error_Type;
         Max_Pos  : out Argument_Type;
         Logger   : Abstract_Logger'Class := Void_Logger)
      is
         Accum : Norm_Infinite_Accumulator;
      begin
         Test_Function (Data         => Data,
                        Tested       => Tested,
                        Accumulator  => Accum,
                        Logger       => Logger);

         Max_Err := Accum.Value;
         Max_Pos := Accum.Where;
      end Test_Function;
   end Norm_Infinite;

   -------------------
   -- Test_Function --
   -------------------

   procedure Test_Function
     (Data         : Sample_Array;
      Tested       : access function (X : Argument_Type) return Result_Type;
      Accumulator  : out Abstract_Norm_Accumulator'Class;
      Logger       : Abstract_Logger'Class := Void_Logger)
   is
      Err     : Error_Type;
      Y       : Result_Type;
   begin
      Accumulator.Reset;

      for Sample of Data loop
         Y := Tested (Sample.X);
         Err := Distance (Sample.Y, Y);

         Accumulator.Update (Error => Err,
                             Where => Sample.X);

         Logger.Log (Argument => Sample.X,
                     Result   => Y,
                     Expected => Sample.Y,
                     Error    => Err);
      end loop;

   end Test_Function;


   package body Io is

      ------------
      -- Report --
      ------------

      procedure Report (Name    : String;
                        Max_Err : Error_Type;
                        Where   : Argument_Type;
                        Output  : File_Type := Standard_Error)
      is
      begin
         Put_Line (Output,
                   Name & ": "
                   & Error_Image (Max_Err)
                   & " @ " & Argument_Image (Where));
      end Report;

      function Create (Output : File_Access := Standard_Error) return File_Logger
      is (File_Logger'(Output => Output));

      procedure Log (Logger   : File_Logger;
                     Argument : Argument_Type;
                     Result   : Result_Type;
                     Expected : Result_Type;
                     Error    : Error_Type)
      is
      begin
         Put_Line (Logger.Output.all,
                   Argument_Image (Argument)
                   & " " & Result_Image (Result)
                   & " " & Result_Image (Expected)
                   & " " & Error_Image (Error));
      end Log;
   end Io;

   package body Norm_P is

      function Create (P : Float) return Norm_P_Accumulator
      is (Norm_P_Accumulator'(P     => p,
                              Accum => Zero));


      overriding
      procedure Reset (Accum : out Norm_P_Accumulator)
      is
      begin
         Accum.Accum := Zero;
      end Reset;

      overriding
      procedure Update (Accum : in out Norm_P_Accumulator;
                        Error : Error_Type;
                        Where : Argument_Type)
      is
         pragma Unreferenced (Where);
      begin
         Accum.Accum := Accum.Accum + Error ** Accum.P;
      end Update;

      function Value (Accum : Norm_P_Accumulator) return Error_Type
      is (Accum.Accum ** (1.0 / Accum.P));
   end Norm_P;

end Generic_Function_Test;
