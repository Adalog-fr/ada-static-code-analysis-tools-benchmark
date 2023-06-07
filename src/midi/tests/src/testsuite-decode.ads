with MIDI.Decoder;

with AUnit.Test_Suites;
with AUnit.Test_Fixtures;
private with AUnit.Test_Caller;

package Testsuite.Decode is

   procedure Add_Tests (Suite : in out AUnit.Test_Suites.Test_Suite'Class);

private

   type Decoder_Fixture
   is new AUnit.Test_Fixtures.Test_Fixture with record
      Decoder : MIDI.Decoder.Instance;
   end record;

   overriding
   procedure Set_Up (Test : in out Decoder_Fixture)
   is null;

   overriding
   procedure Tear_Down (Test : in out Decoder_Fixture)
   is null;

   package Decoder_Caller is new AUnit.Test_Caller (Decoder_Fixture);

end Testsuite.Decode;
