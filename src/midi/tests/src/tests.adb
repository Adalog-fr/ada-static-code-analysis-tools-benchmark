with AUnit; use AUnit;
with AUnit.Test_Suites;
with AUnit.Run;
with AUnit.Reporter.Text;

with GNAT.OS_Lib;

with Testsuite.Decode;
with Testsuite.Encode;

procedure Tests is
   --  Dec : MIDI.Decoder.Instance;
   --
   --  ---------------
   --  -- Print_Msg --
   --  ---------------
   --
   --  procedure Print_Msg (Msg : MIDI.Message) is
   --  begin
   --     Ada.Text_IO.Put_Line (MIDI.Img (Msg));
   --  end Print_Msg;
   --
   --  procedure Push is new MIDI.Decoder.Push (Print_Msg);
   --
   --  Data_In : constant HAL.UInt8_Array :=
   --    (16#80#, 16#2A#, 16#2A#,
   --     16#91#, 16#2A#, 16#2A#,
   --     16#A2#, 16#2A#, 16#2A#,
   --     16#B3#, 16#2A#, 16#2A#,
   --     16#C4#, 16#2A#,
   --     16#D4#, 16#2A#,
   --     16#E4#, 16#2A#,
   --     16#F2#, 16#2A#, 16#2A#,
   --     16#F3#, 16#2A#,
   --     16#F5#, 16#2A#,
   --     16#F0#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#F7#,
   --     16#F6#,
   --     16#F7#,
   --     16#F8#,
   --     16#FA#,
   --     16#FB#,
   --     16#FC#,
   --     16#FE#,
   --     16#FF#);

   Failures : Natural := 0;

begin
   declare
      Suite : aliased AUnit.Test_Suites.Test_Suite;

      function Get_Suite return AUnit.Test_Suites.Access_Test_Suite
      is (Suite'Unchecked_Access);

      function Runner is new AUnit.Run.Test_Runner_With_Status (Get_Suite);

      Reporter : AUnit.Reporter.Text.Text_Reporter;
   begin
      Testsuite.Decode.Add_Tests (Suite);
      Testsuite.Encode.Add_Tests (Suite);

      Reporter.Set_Use_ANSI_Colors (True);

      if Runner (Reporter,
                 (Global_Timer     => True,
                  Test_Case_Timer  => True,
                  Report_Successes => True,
                  others           => <>))
        /= AUnit.Success
      then
         Failures := Failures + 1;
      end if;
   end;

   if Failures /= 0 then
      GNAT.OS_Lib.OS_Exit (1);
   end if;
end Tests;
