pragma Ada_2012;

with Ada.Streams; use Ada.Streams;
with Interfaces;  use Interfaces;

with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Caller;

with Endianness; use Endianness;

package body Tests is
   package Caller is new AUnit.Test_Caller (Fixture);

   Test_Suite : aliased AUnit.Test_Suites.Test_Suite;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Name : constant String := "[endianness] ";
   begin
      Test_Suite.Add_Test
        (Caller.Create (Name & "Swap_Endian()", Swap_Endian_Test'Access));
      Test_Suite.Add_Test
        (Caller.Create
           (Name & "Native_To_Big_Endian() and Big_Endian_To_Native()",
            Native_To_Big_Endian_And_Back_Test'Access));
      Test_Suite.Add_Test
        (Caller.Create
           (Name & "Native_To_Little_Endian() and Little_Endian_To_Native()",
            Native_To_Little_Endian_And_Back_Test'Access));

      return Test_Suite'Access;
   end Suite;

   procedure Swap_Endian_Test (Object : in out Fixture) is
   begin
      declare
         function Swap is new Swap_Endian (Integer);
      begin
         Assert (Swap (42) = 704_643_072, "Integer swap");
         Assert (Swap (Swap (42)) = 42, "Integer swap reflexive");
      end;

      declare
         function Swap is new Swap_Endian (Integer_8);
      begin
         Assert (Swap (Integer_8 (10)) = 10, "Integer_8 swap");
      end;

      declare
         function Swap is new Swap_Endian (Unsigned_16);
      begin
         Assert (Swap (Unsigned_16 (10)) = 2_560, "Unsigned_16 swap");
      end;

      declare
         function Swap is new Swap_Endian (Long_Long_Integer);
      begin
         Assert
           (Swap (Long_Long_Integer (10)) = 720_575_940_379_279_360,
            "Long_Long_Integer swap");
      end;

      declare
         function Swap is new Swap_Endian (Unsigned_64);
      begin
         Assert
           (Swap (Unsigned_64 (10)) = 720_575_940_379_279_360,
            "Unsigned_64 swap");
      end;
   end Swap_Endian_Test;

   procedure Native_To_Big_Endian_And_Back_Test (Object : in out Fixture) is
   begin
      declare
         function To is new Native_To_Big_Endian (Integer);
         function From is new Big_Endian_To_Native (Integer);

         X : constant Integer                       := 12_345;
         A : constant Stream_Element_Array (0 .. 3) := To (X);
      begin
         Assert (From (A) = X, "Integer");
      end;

      declare
         function To is new Native_To_Big_Endian (Unsigned_16);
         function From is new Big_Endian_To_Native (Unsigned_16);

         X : constant Unsigned_16                   := 12_345;
         A : constant Stream_Element_Array (0 .. 1) := To (X);
      begin
         Assert (From (A) = X, "Unsigned_16");
      end;

      declare
         function To is new Native_To_Big_Endian (Integer_8);
         function From is new Big_Endian_To_Native (Integer_8);

         X : constant Integer_8                     := 123;
         A : constant Stream_Element_Array (0 .. 0) := To (X);
      begin
         Assert (From (A) = X, "Integer_8");
      end;

      declare
         function To is new Native_To_Big_Endian (Long_Long_Integer);
         function From is new Big_Endian_To_Native (Long_Long_Integer);

         X : constant Long_Long_Integer             := 1_234_567_890;
         A : constant Stream_Element_Array (0 .. 7) := To (X);
      begin
         Assert (From (A) = X, "Long_Long_Integer");
      end;
   end Native_To_Big_Endian_And_Back_Test;

   procedure Native_To_Little_Endian_And_Back_Test (Object : in out Fixture) is
   begin
      declare
         function To is new Native_To_Little_Endian (Integer);
         function From is new Little_Endian_To_Native (Integer);

         X : constant Integer                       := 12_345;
         A : constant Stream_Element_Array (0 .. 3) := To (X);
      begin
         Assert (From (A) = X, "Integer");
      end;

      declare
         function To is new Native_To_Little_Endian (Unsigned_16);
         function From is new Little_Endian_To_Native (Unsigned_16);

         X : constant Unsigned_16                   := 12_345;
         A : constant Stream_Element_Array (0 .. 1) := To (X);
      begin
         Assert (From (A) = X, "Unsigned_16");
      end;

      declare
         function To is new Native_To_Little_Endian (Integer_8);
         function From is new Little_Endian_To_Native (Integer_8);

         X : constant Integer_8                     := 123;
         A : constant Stream_Element_Array (0 .. 0) := To (X);
      begin
         Assert (From (A) = X, "Integer_8");
      end;

      declare
         function To is new Native_To_Little_Endian (Long_Long_Integer);
         function From is new Little_Endian_To_Native (Long_Long_Integer);

         X : constant Long_Long_Integer             := 1_234_567_890;
         A : constant Stream_Element_Array (0 .. 7) := To (X);
      begin
         Assert (From (A) = X, "Long_Long_Integer");
      end;
   end Native_To_Little_Endian_And_Back_Test;
end Tests;
