with AUnit.Test_Fixtures;
with AUnit.Test_Suites;

package Tests is
   function Suite return AUnit.Test_Suites.Access_Test_Suite;
private
   type Fixture is new AUnit.Test_Fixtures.Test_Fixture with null record;

   procedure Swap_Endian_Test (Object : in out Fixture);
   procedure Native_To_Big_Endian_And_Back_Test (Object : in out Fixture);
   procedure Native_To_Little_Endian_And_Back_Test (Object : in out Fixture);
end Tests;
