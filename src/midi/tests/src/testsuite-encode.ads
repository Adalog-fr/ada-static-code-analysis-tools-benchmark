with MIDI.Encoder;
with MIDI.Encoder.Queue;

with AUnit.Test_Suites;
with AUnit.Test_Fixtures;
private with AUnit.Test_Caller;

package Testsuite.Encode is

   procedure Add_Tests (Suite : in out AUnit.Test_Suites.Test_Suite'Class);

private

   type Encoder_Fixture
   is new AUnit.Test_Fixtures.Test_Fixture with null record;

   overriding
   procedure Set_Up (Test : in out Encoder_Fixture)
   is null;

   overriding
   procedure Tear_Down (Test : in out Encoder_Fixture)
   is null;

   package Encoder_Caller
   is new AUnit.Test_Caller (Encoder_Fixture);

   type Encoder_Queue_Fixture
   is new AUnit.Test_Fixtures.Test_Fixture with record
      Q : MIDI.Encoder.Queue.Instance (1024);
   end record;

   overriding
   procedure Set_Up (Test : in out Encoder_Queue_Fixture)
   is null;

   overriding
   procedure Tear_Down (Test : in out Encoder_Queue_Fixture)
   is null;

   package Encoder_Queue_Caller
   is new AUnit.Test_Caller (Encoder_Queue_Fixture);

end Testsuite.Encode;
