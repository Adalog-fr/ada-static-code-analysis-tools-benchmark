with Ada.Text_IO;   use Ada.Text_IO;

--
--  This package provides a function that can be handy when testing
--  numeric functions against a "ground truth."  The idea is to compute
--  the function to be tested with different arguments and taking the
--  difference between the obtained result and the expected one.
--
--  The function to be tested can accept any kind of arguments and can
--  return any kind of result, as long as it is possible to compute a
--  "distance" between results and as long as the "distances" can be
--  compared.
--
generic
   type Argument_Type is private;

   type Result_Type is private;

   type Error_Type is private;

   Smallest_Error : Error_Type;

   with function Distance (X, Y : Result_Type) return Error_Type;

   with function ">" (X, Y : Error_Type) return Boolean is <>;
package Generic_Function_Test is
   --
   --  Loggers are used by the test function to output a verbose
   --  description of the tests made.  A logger exports just a procedure
   --  Log.
   --
   type Abstract_Logger is limited interface;

   procedure Log (Logger   : Abstract_Logger;
                  Argument : Argument_Type;
                  Result   : Result_Type;
                  Expected : Result_Type;
                  Error    : Error_Type)
   is abstract;

   --
   --  A Void_Logger is just the /dev/null of loggers
   --
   type Void_Logger_Type is new Abstract_Logger with null record;

   overriding procedure Log (Logger   : Void_Logger_Type;
                             Argument : Argument_Type;
                             Result   : Result_Type;
                             Expected : Result_Type;
                             Error    : Error_Type)
   is null;

   Void_Logger : constant Void_Logger_Type := (null record);


   type Sample_Type is
      record
         X : Argument_Type;
         Y : Result_Type;
      end record;

   type Sample_Array is array (Positive range <>) of Sample_Type;

   procedure Test_Function
     (Data         : Sample_Array;
      Ground_Truth : access function (X : Argument_Type) return Result_Type;
      Max_Err      : out Error_Type;
      Max_Pos      : out Argument_Type;
      Logger       : Abstract_Logger'Class := Void_Logger);

   generic
      with function Argument_Image (X : Argument_Type) return String;
      with function Result_Image (X : Result_Type) return String;
      with function Error_Image (X : Error_Type) return String;
   package IO is
      --
      --  This package provides some handy procedures and loggers
      --  that can be useful in many contexts.
      --

      --
      --  Print to the specified output a report message.  Not strictly
      --  fundamental, I agree, but quite convenient nevertheless.
      --
      procedure Report (Name    : String;
                        Max_Err : Error_Type;
                        Where   : Argument_Type;
                        Output  : File_Type := Standard_Error);

      --
      --  A File_Logger is the second most common case and writes to a file
      --
      type File_Logger (<>) is new Abstract_Logger  with private;

      function Create (Output : File_Access := Standard_Error) return File_Logger;

      overriding procedure Log (Logger   : File_Logger;
                                Argument : Argument_Type;
                                Result   : Result_Type;
                                Expected : Result_Type;
                                Error    : Error_Type);

   private
      type File_Logger is new Abstract_Logger
      with
         record
            Output : File_Access;
         end record;
   end IO;
end Generic_Function_Test;
