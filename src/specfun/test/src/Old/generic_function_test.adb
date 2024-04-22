pragma Ada_2012;

package body Generic_Function_Test is

   -------------------
   -- Test_Function --
   -------------------

   procedure Test_Function
     (Data         : Sample_Array;
      Ground_Truth : access function (X : Argument_Type) return Result_Type;
      Max_Err      : out Error_Type;
      Max_Pos      : out Argument_Type;
      Logger       : Abstract_Logger'Class := Void_Logger)
   is
      Err     : Error_Type;
      Y       : Result_Type;
   begin
      Max_Err := Smallest_Error;

      for Sample of Data loop
         Y := Ground_Truth (Sample.X);
         Err := Distance (Sample.Y, Y);

         if Err > Max_Err then
            Max_Err := Err;
            Max_Pos := Sample.X;
         end if;

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
end Generic_Function_Test;
