with AUnit.Test_Suites;
with AUnit.Test_Fixtures;

package Test_Standard is
   function Suite return AUnit.Test_Suites.Access_Test_Suite;
private
   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;

   procedure Test_Encoding_Empty (Object : in out Test);
   procedure Test_Encoding_Pad2 (Object : in out Test);
   procedure Test_Encoding_Pad1 (Object : in out Test);
   procedure Test_Encoding_Pad0 (Object : in out Test);
   procedure Test_Encoding_Big (Object : in out Test);
   procedure Test_Encoding_RFC4648 (Object : in out Test);
   procedure Test_Decoding_Empty (Object : in out Test);
   procedure Test_Decoding_Pad2 (Object : in out Test);
   procedure Test_Decoding_Pad1 (Object : in out Test);
   procedure Test_Decoding_Pad0 (Object : in out Test);
   procedure Test_Decoding_Big (Object : in out Test);
   procedure Test_Decoding_RFC4648 (Object : in out Test);
end Test_Standard;
