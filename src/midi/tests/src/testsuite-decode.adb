with MIDI; use MIDI;

with System.Storage_Elements; use System.Storage_Elements;
with AAA.Strings;
with Test_Utils;
with HAL;
with AUnit.Assertions; use AUnit.Assertions;

package body Testsuite.Decode is
   pragma Style_Checks ("M120");

   ----------------
   -- Basic_Test --
   ----------------

   procedure Basic_Test (Fixture    : in out Decoder_Fixture;
                         Input      :        Storage_Array;
                         Expected   :        Test_Utils.Message_Array)
   is
      use type AAA.Strings.Vector;

      Expected_Str : constant AAA.Strings.Vector :=
        Test_Utils.To_Str_Vector (Expected);

      Result : AAA.Strings.Vector := AAA.Strings.Empty_Vector;

      procedure Got_Msg (Msg : MIDI.Message) is
      begin
         Result.Append (MIDI.Img (Msg));
      end Got_Msg;

      procedure Push is new MIDI.Decoder.Push (Got_Msg);

   begin

      for Elt of Input loop
         Push (Fixture.Decoder, HAL.UInt8 (Elt));
      end loop;

      if Expected_Str /= Result then
         declare
            Diff : constant AAA.Strings.Vector :=
              AAA.Strings.Diff (Expected_Str, Result,
                                "expected", "result");
         begin
            Assert (False,
                    "Input   : " & Test_Utils.Image (Input) & ASCII.LF &
                      Diff.Flatten (ASCII.LF));
         end;
      end if;

   end Basic_Test;

   -------------------
   -- Test_Note_Off --
   -------------------

   procedure Test_Note_Off (Fixture : in out Decoder_Fixture) is
   begin
      Basic_Test (Fixture,
                  Input    => (16#80#, 16#2A#, 16#2A#),
                  Expected => (0 => (Kind     => Note_Off,
                                     Chan     => 0,
                                     Key      => 42,
                                     Velocity => 42)));

   end Test_Note_Off;

   ------------------
   -- Test_Note_On --
   ------------------

   procedure Test_Note_On (Fixture : in out Decoder_Fixture) is
   begin
      Basic_Test (Fixture,
                  Input    => (16#91#, 16#2A#, 16#2A#),
                  Expected => (0 => (Kind     => Note_On,
                                     Chan     => 1,
                                     Key      => 42,
                                     Velocity => 42)));
   end Test_Note_On;

   ---------------------
   -- Test_Aftertouch --
   ---------------------

   procedure Test_Aftertouch (Fixture : in out Decoder_Fixture) is
   begin
      Basic_Test (Fixture,
                  Input    => (16#A2#, 16#2A#, 16#2A#),
                  Expected => (0 => (Kind     => Aftertouch,
                                     Chan     => 2,
                                     Key      => 42,
                                     Velocity => 42)));
   end Test_Aftertouch;

   -------------
   -- Test_CC --
   -------------

   procedure Test_CC (Fixture : in out Decoder_Fixture) is
   begin
      Basic_Test (Fixture,
                  Input    => (16#B3#, 16#2A#, 16#2A#),
                  Expected => (0 => (Kind             => Continous_Controller,
                                     Chan             => 3,
                                     Controller       => 42,
                                     Controller_Value => 42)));
   end Test_CC;

   -----------------------
   -- Test_Patch_Change --
   -----------------------

   procedure Test_Patch_Change (Fixture : in out Decoder_Fixture) is
   begin
      Basic_Test (Fixture,
                  Input    => (16#C4#, 16#2A#),
                  Expected => (0 => (Kind       => Patch_Change,
                                     Chan       => 4,
                                     Instrument => 42)));
   end Test_Patch_Change;

   ---------------------------
   -- Test_Channel_Pressure --
   ---------------------------

   procedure Test_Channel_Pressure (Fixture : in out Decoder_Fixture) is
   begin
      Basic_Test (Fixture,
                  Input    => (16#D5#, 16#2A#),
                  Expected => (0 => (Kind     => Channel_Pressure,
                                     Chan     => 5,
                                     Pressure => 42)));
   end Test_Channel_Pressure;

   ---------------------
   -- Test_Pitch_Bend --
   ---------------------

   procedure Test_Pitch_Bend (Fixture : in out Decoder_Fixture) is
   begin
      Basic_Test (Fixture,
                  Input    => (16#E6#, 16#2A#),
                  Expected => (0 => (Kind     => Pitch_Bend,
                                     Chan     => 6,
                                     Bend     => 42)));
   end Test_Pitch_Bend;

   --------------------
   -- Test_Sys_Start --
   --------------------

   procedure Test_Sys_Start (Fixture : in out Decoder_Fixture) is
   begin
      Basic_Test (Fixture,
                  Input    => (0 => 16#FA#),
                  Expected => (0 => (Kind => Sys,
                                     Cmd  => Start_Song,
                                     S1   => 0,
                                     S2   => 0)));
   end Test_Sys_Start;

   -----------------------
   -- Test_Sys_Continue --
   -----------------------

   procedure Test_Sys_Continue (Fixture : in out Decoder_Fixture) is
   begin
      Basic_Test (Fixture,
                  Input    => (0 => 16#FB#),
                  Expected => (0 => (Kind => Sys,
                                     Cmd  => Continue_Song,
                                     S1   => 0,
                                     S2   => 0)));
   end Test_Sys_Continue;

   -------------------
   -- Test_Sys_Stop --
   -------------------

   procedure Test_Sys_Stop (Fixture : in out Decoder_Fixture) is
   begin
      Basic_Test (Fixture,
                  Input    => (0 => 16#FC#),
                  Expected => (0 => (Kind => Sys,
                                     Cmd  => Stop_Song,
                                     S1   => 0,
                                     S2   => 0)));
   end Test_Sys_Stop;

   ---------------------------
   -- Test_Sys_Tune_Request --
   ---------------------------

   procedure Test_Sys_Tune_Request (Fixture : in out Decoder_Fixture) is
   begin
      Basic_Test (Fixture,
                  Input    => (0 => 16#F6#),
                  Expected => (0 => (Kind => Sys,
                                     Cmd  => Tune_Request,
                                     S1   => 0,
                                     S2   => 0)));
   end Test_Sys_Tune_Request;

   ---------------------------
   -- Test_Sys_Timming_Tick --
   ---------------------------

   procedure Test_Sys_Timming_Tick (Fixture : in out Decoder_Fixture) is
   begin
      Basic_Test (Fixture,
                  Input    => (0 => 16#F8#),
                  Expected => (0 => (Kind => Sys,
                                     Cmd  => Timming_Tick,
                                     S1   => 0,
                                     S2   => 0)));
   end Test_Sys_Timming_Tick;

   -----------------------------
   -- Test_Sys_Active_Sensing --
   -----------------------------

   procedure Test_Sys_Active_Sensing (Fixture : in out Decoder_Fixture) is
   begin
      Basic_Test (Fixture,
                  Input    => (0 => 16#FE#),
                  Expected => (0 => (Kind => Sys,
                                     Cmd  => Active_Sensing,
                                     S1   => 0,
                                     S2   => 0)));
   end Test_Sys_Active_Sensing;

   --------------------
   -- Test_Sys_Reset --
   --------------------

   procedure Test_Sys_Reset (Fixture : in out Decoder_Fixture) is
   begin
      Basic_Test (Fixture,
                  Input    => (0 => 16#FF#),
                  Expected => (0 => (Kind => Sys,
                                     Cmd  => Reset,
                                     S1   => 0,
                                     S2   => 0)));
   end Test_Sys_Reset;

   --------------------------
   -- Test_Sys_Song_Select --
   --------------------------

   procedure Test_Sys_Song_Select (Fixture : in out Decoder_Fixture) is
   begin
      Basic_Test (Fixture,
                  Input    => (16#F3#, 16#2A#),
                  Expected => (0 => (Kind => Sys,
                                     Cmd  => Song_Select,
                                     S1   => 42,
                                     S2   => 0)));
   end Test_Sys_Song_Select;

   -------------------------
   -- Test_Sys_Bus_Select --
   -------------------------

   procedure Test_Sys_Bus_Select (Fixture : in out Decoder_Fixture) is
   begin
      Basic_Test (Fixture,
                  Input    => (16#F5#, 16#2A#),
                  Expected => (0 => (Kind => Sys,
                                     Cmd  => Bus_Select,
                                     S1   => 42,
                                     S2   => 0)));
   end Test_Sys_Bus_Select;

   ----------------------------
   -- Test_Sys_Song_Position --
   ----------------------------

   procedure Test_Sys_Song_Position (Fixture : in out Decoder_Fixture) is
   begin
      Basic_Test (Fixture,
                  Input    => (16#F2#, 16#01#, 16#02#),
                  Expected => (0 => (Kind => Sys,
                                     Cmd  => Song_Position,
                                     S1   => 1,
                                     S2   => 2)));
   end Test_Sys_Song_Position;

   ------------------------
   -- Test_Ignore_Sys_Ex --
   ------------------------

   procedure Test_Ignore_Sys_Ex (Fixture : in out Decoder_Fixture) is
   begin
      Basic_Test (Fixture,
                  Input    => (0        => 16#FF#, -- Reset
                               1        => 16#F0#, -- Start Sys Ex
                               2 .. 120 => 16#2A#, -- Any data...
                               121      => 16#F7#, -- End Sys Ex
                               122      => 16#0FF# -- Reset
                              ),
                  Expected => (0 .. 1 => (Kind => Sys,
                                          Cmd  => Reset,
                                          S1   => 0,
                                          S2   => 0)));
   end Test_Ignore_Sys_Ex;

   -------------------------
   -- Test_Ignore_Invalid --
   -------------------------

   procedure Test_Ignore_Invalid (Fixture : in out Decoder_Fixture) is
   begin
      Basic_Test (Fixture,
                  Input    => (0 => 16#FF#, -- Reset
                               1 => 16#81#, -- Start Note Off
                               2 => 16#80#, -- Invalid data for note off
                               3 => 16#0FF# -- Reset
                              ),
                  Expected => (0 .. 1 => (Kind => Sys,
                                          Cmd  => Reset,
                                          S1   => 0,
                                          S2   => 0)));
   end Test_Ignore_Invalid;

   -------------------------
   -- Test_Ignore_Invalid --
   -------------------------

   procedure Test_Missing_Data (Fixture : in out Decoder_Fixture) is
   begin
      Basic_Test (Fixture,
                  Input    => (0 => 16#FF#, -- Reset
                               1 => 16#81#, -- Start Note Off
                               2 => 16#FF# -- Reset
                              ),
                  Expected => (0 .. 1 => (Kind => Sys,
                                          Cmd  => Reset,
                                          S1   => 0,
                                          S2   => 0)));
   end Test_Missing_Data;

   ---------------
   -- Add_Tests --
   ---------------

   procedure Add_Tests (Suite : in out AUnit.Test_Suites.Test_Suite'Class) is
   begin
      Suite.Add_Test (Decoder_Caller.Create ("Decode Note_Off", Test_Note_Off'Access));
      Suite.Add_Test (Decoder_Caller.Create ("Decode Note_On", Test_Note_On'Access));
      Suite.Add_Test (Decoder_Caller.Create ("Decode Aftertouch", Test_Aftertouch'Access));
      Suite.Add_Test (Decoder_Caller.Create ("Decode CC", Test_CC'Access));
      Suite.Add_Test (Decoder_Caller.Create ("Decode Patch_Change", Test_Patch_Change'Access));
      Suite.Add_Test (Decoder_Caller.Create ("Decode Channel_Pressure", Test_Channel_Pressure'Access));
      Suite.Add_Test (Decoder_Caller.Create ("Decode Pitch_Bend", Test_Pitch_Bend'Access));

      Suite.Add_Test (Decoder_Caller.Create ("Decode Sys Start", Test_Sys_Start'Access));
      Suite.Add_Test (Decoder_Caller.Create ("Decode Sys Stop", Test_Sys_Stop'Access));
      Suite.Add_Test (Decoder_Caller.Create ("Decode Sys Continue", Test_Sys_Continue'Access));
      Suite.Add_Test (Decoder_Caller.Create ("Decode Sys Tick", Test_Sys_Timming_Tick'Access));
      Suite.Add_Test (Decoder_Caller.Create ("Decode Sys Tune Request", Test_Sys_Tune_Request'Access));
      Suite.Add_Test (Decoder_Caller.Create ("Decode Sys Active Sensing", Test_Sys_Active_Sensing'Access));
      Suite.Add_Test (Decoder_Caller.Create ("Decode Sys Reset", Test_Sys_Reset'Access));

      Suite.Add_Test (Decoder_Caller.Create ("Decode Sys Song Select", Test_Sys_Song_Select'Access));
      Suite.Add_Test (Decoder_Caller.Create ("Decode Sys Song Position", Test_Sys_Song_Position'Access));
      Suite.Add_Test (Decoder_Caller.Create ("Decode Sys Bus Select", Test_Sys_Bus_Select'Access));

      Suite.Add_Test (Decoder_Caller.Create ("Decode Ingore Sys Ex", Test_Ignore_Sys_Ex'Access));
      Suite.Add_Test (Decoder_Caller.Create ("Decode Ingore Invalid", Test_Ignore_Invalid'Access));
      Suite.Add_Test (Decoder_Caller.Create ("Decode Missing Data", Test_Missing_Data'Access));

   end Add_Tests;

end Testsuite.Decode;
