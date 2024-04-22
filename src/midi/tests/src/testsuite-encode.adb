with MIDI; use MIDI;

with System.Storage_Elements; use System.Storage_Elements;
with AAA.Strings;
with Test_Utils;
with AUnit.Assertions; use AUnit.Assertions;

package body Testsuite.Encode is
   pragma Style_Checks ("M120");

   ----------------
   -- Basic_Test --
   ----------------

   procedure Basic_Test (Fixture    : in out Encoder_Queue_Fixture;
                         Input      :        Test_Utils.Message_Array;
                         Expected   :        Storage_Array)
   is
   begin

      for Elt of Input loop
         Fixture.Q.Push (Elt);
      end loop;

      declare
         Result : constant Storage_Array :=
           Test_Utils.To_Storrage_Array (Fixture.Q);
      begin
         if Expected /= Result then
            declare
               Diff : constant AAA.Strings.Vector :=
                 AAA.Strings.Diff (Test_Utils.Hex_Dump (Expected),
                                   Test_Utils.Hex_Dump (Result),
                                   "expected", "result");
            begin
               Assert (False,
                       "Input   : " &
                         Test_Utils.To_Str_Vector (Input).Flatten (ASCII.LF) &
                         ASCII.LF &
                         Diff.Flatten (ASCII.LF));
            end;
         end if;
      end;
   end Basic_Test;

   -------------------
   -- Test_Note_Off --
   -------------------

   procedure Test_Note_Off (Fixture : in out Encoder_Queue_Fixture) is
   begin
      Basic_Test (Fixture,
                  Input    => (0 => (Kind     => Note_Off,
                                     Chan     => 0,
                                     Key      => 42,
                                     Velocity => 42)),
                  Expected => (16#80#, 16#2A#, 16#2A#));

   end Test_Note_Off;

   ------------------
   -- Test_Note_On --
   ------------------

   procedure Test_Note_On (Fixture : in out Encoder_Queue_Fixture) is
   begin
      Basic_Test (Fixture,
                  Input    => (0 => (Kind     => Note_On,
                                     Chan     => 1,
                                     Key      => 42,
                                     Velocity => 42)),
                  Expected => (16#91#, 16#2A#, 16#2A#));
   end Test_Note_On;

   ---------------------
   -- Test_Aftertouch --
   ---------------------

   procedure Test_Aftertouch (Fixture : in out Encoder_Queue_Fixture) is
   begin
      Basic_Test (Fixture,
                  Input    => (0 => (Kind     => Aftertouch,
                                     Chan     => 2,
                                     Key      => 42,
                                     Velocity => 42)),
                  Expected => (16#A2#, 16#2A#, 16#2A#));
   end Test_Aftertouch;

   -------------
   -- Test_CC --
   -------------

   procedure Test_CC (Fixture : in out Encoder_Queue_Fixture) is
   begin
      Basic_Test (Fixture,
                  Input    => (0 => (Kind             => Continous_Controller,
                                     Chan             => 3,
                                     Controller       => 42,
                                     Controller_Value => 42)),
                  Expected => (16#B3#, 16#2A#, 16#2A#));
   end Test_CC;

   -----------------------
   -- Test_Patch_Change --
   -----------------------

   procedure Test_Patch_Change (Fixture : in out Encoder_Queue_Fixture) is
   begin
      Basic_Test (Fixture,
                  Input    => (0 => (Kind       => Patch_Change,
                                     Chan       => 4,
                                     Instrument => 42)),
                  Expected => (16#C4#, 16#2A#));
   end Test_Patch_Change;

   ---------------------------
   -- Test_Channel_Pressure --
   ---------------------------

   procedure Test_Channel_Pressure (Fixture : in out Encoder_Queue_Fixture) is
   begin
      Basic_Test (Fixture,
                  Input    => (0 => (Kind     => Channel_Pressure,
                                     Chan     => 5,
                                     Pressure => 42)),
                  Expected => (16#D5#, 16#2A#));
   end Test_Channel_Pressure;

   ---------------------
   -- Test_Pitch_Bend --
   ---------------------

   procedure Test_Pitch_Bend (Fixture : in out Encoder_Queue_Fixture) is
   begin
      Basic_Test (Fixture,
                  Input    => (0 => (Kind     => Pitch_Bend,
                                     Chan     => 6,
                                     Bend     => 42)),
                  Expected => (16#E6#, 16#2A#));
   end Test_Pitch_Bend;

   --------------------
   -- Test_Sys_Start --
   --------------------

   procedure Test_Sys_Start (Fixture : in out Encoder_Queue_Fixture) is
   begin
      Basic_Test (Fixture,
                  Input    => (0 => (Kind => Sys,
                                     Cmd  => Start_Song,
                                     S1   => 0,
                                     S2   => 0)),
                  Expected => (0 => 16#FA#));
   end Test_Sys_Start;

   -----------------------
   -- Test_Sys_Continue --
   -----------------------

   procedure Test_Sys_Continue (Fixture : in out Encoder_Queue_Fixture) is
   begin
      Basic_Test (Fixture,
                  Input    => (0 => (Kind => Sys,
                                     Cmd  => Continue_Song,
                                     S1   => 0,
                                     S2   => 0)),
                  Expected => (0 => 16#FB#));
   end Test_Sys_Continue;

   -------------------
   -- Test_Sys_Stop --
   -------------------

   procedure Test_Sys_Stop (Fixture : in out Encoder_Queue_Fixture) is
   begin
      Basic_Test (Fixture,
                  Input    => (0 => (Kind => Sys,
                                     Cmd  => Stop_Song,
                                     S1   => 0,
                                     S2   => 0)),
                  Expected => (0 => 16#FC#));
   end Test_Sys_Stop;

   ---------------------------
   -- Test_Sys_Tune_Request --
   ---------------------------

   procedure Test_Sys_Tune_Request (Fixture : in out Encoder_Queue_Fixture) is
   begin
      Basic_Test (Fixture,
                  Input    => (0 => (Kind => Sys,
                                     Cmd  => Tune_Request,
                                     S1   => 0,
                                     S2   => 0)),
                  Expected => (0 => 16#F6#));
   end Test_Sys_Tune_Request;

   ---------------------------
   -- Test_Sys_Timming_Tick --
   ---------------------------

   procedure Test_Sys_Timming_Tick (Fixture : in out Encoder_Queue_Fixture) is
   begin
      Basic_Test (Fixture,
                  Input    => (0 => (Kind => Sys,
                                     Cmd  => Timming_Tick,
                                     S1   => 0,
                                     S2   => 0)),
                  Expected => (0 => 16#F8#));
   end Test_Sys_Timming_Tick;

   -----------------------------
   -- Test_Sys_Active_Sensing --
   -----------------------------

   procedure Test_Sys_Active_Sensing (Fixture : in out Encoder_Queue_Fixture) is
   begin
      Basic_Test (Fixture,
                  Input    => (0 => (Kind => Sys,
                                     Cmd  => Active_Sensing,
                                     S1   => 0,
                                     S2   => 0)),
                  Expected => (0 => 16#FE#));
   end Test_Sys_Active_Sensing;

   --------------------
   -- Test_Sys_Reset --
   --------------------

   procedure Test_Sys_Reset (Fixture : in out Encoder_Queue_Fixture) is
   begin
      Basic_Test (Fixture,
                  Input    => (0 => (Kind => Sys,
                                     Cmd  => Reset,
                                     S1   => 0,
                                     S2   => 0)),
                  Expected => (0 => 16#FF#));
   end Test_Sys_Reset;

   --------------------------
   -- Test_Sys_Song_Select --
   --------------------------

   procedure Test_Sys_Song_Select (Fixture : in out Encoder_Queue_Fixture) is
   begin
      Basic_Test (Fixture,
                  Input    => (0 => (Kind => Sys,
                                     Cmd  => Song_Select,
                                     S1   => 42,
                                     S2   => 0)),
                  Expected => (16#F3#, 16#2A#));
   end Test_Sys_Song_Select;

   -------------------------
   -- Test_Sys_Bus_Select --
   -------------------------

   procedure Test_Sys_Bus_Select (Fixture : in out Encoder_Queue_Fixture) is
   begin
      Basic_Test (Fixture,
                  Input    => (0 => (Kind => Sys,
                                     Cmd  => Bus_Select,
                                     S1   => 42,
                                     S2   => 0)),
                  Expected => (16#F5#, 16#2A#));
   end Test_Sys_Bus_Select;

   ----------------------------
   -- Test_Sys_Song_Position --
   ----------------------------

   procedure Test_Sys_Song_Position (Fixture : in out Encoder_Queue_Fixture) is
   begin
      Basic_Test (Fixture,
                  Input    => (0 => (Kind => Sys,
                                     Cmd  => Song_Position,
                                     S1   => 1,
                                     S2   => 2)),
                  Expected => (16#F2#, 16#01#, 16#02#));
   end Test_Sys_Song_Position;

   -------------------------
   -- Test_Multi_Messages --
   -------------------------

   procedure Test_Multi_Messages (Fixture : in out Encoder_Queue_Fixture) is
   begin
      Basic_Test (Fixture,
                  Input    => ((Kind     => Note_On,
                                Chan     => 1,
                                Key      => 42,
                                Velocity => 42),

                               (Kind => Sys,
                                Cmd  => Timming_Tick,
                                S1   => 0,
                                S2   => 0),

                               (Kind     => Note_Off,
                                Chan     => 1,
                                Key      => 42,
                                Velocity => 42),

                               (Kind => Sys,
                                Cmd  => Timming_Tick,
                                S1   => 0,
                                S2   => 0),

                               (Kind             => Continous_Controller,
                                Chan             => 3,
                                Controller       => 42,
                                Controller_Value => 42),

                               (Kind => Sys,
                                Cmd  => Song_Position,
                                S1   => 1,
                                S2   => 2)),
                  Expected => (16#91#, 16#2A#, 16#2A#,
                               16#F8#,
                               16#81#, 16#2A#, 16#2A#,
                               16#F8#,
                               16#B3#, 16#2A#, 16#2A#,
                               16#F2#, 16#01#, 16#02#));

   end Test_Multi_Messages;

   ---------------
   -- Add_Tests --
   ---------------

   procedure Add_Tests (Suite : in out AUnit.Test_Suites.Test_Suite'Class) is
   begin
      Suite.Add_Test (Encoder_Queue_Caller.Create ("Encode Note_Off", Test_Note_Off'Access));
      Suite.Add_Test (Encoder_Queue_Caller.Create ("Encode Note_On", Test_Note_On'Access));
      Suite.Add_Test (Encoder_Queue_Caller.Create ("Encode Aftertouch", Test_Aftertouch'Access));
      Suite.Add_Test (Encoder_Queue_Caller.Create ("Encode CC", Test_CC'Access));
      Suite.Add_Test (Encoder_Queue_Caller.Create ("Encode Patch_Change", Test_Patch_Change'Access));
      Suite.Add_Test (Encoder_Queue_Caller.Create ("Encode Channel_Pressure", Test_Channel_Pressure'Access));
      Suite.Add_Test (Encoder_Queue_Caller.Create ("Encode Pitch_Bend", Test_Pitch_Bend'Access));

      Suite.Add_Test (Encoder_Queue_Caller.Create ("Encode Sys Start", Test_Sys_Start'Access));
      Suite.Add_Test (Encoder_Queue_Caller.Create ("Encode Sys Stop", Test_Sys_Stop'Access));
      Suite.Add_Test (Encoder_Queue_Caller.Create ("Encode Sys Continue", Test_Sys_Continue'Access));
      Suite.Add_Test (Encoder_Queue_Caller.Create ("Encode Sys Tick", Test_Sys_Timming_Tick'Access));
      Suite.Add_Test (Encoder_Queue_Caller.Create ("Encode Sys Tune Request", Test_Sys_Tune_Request'Access));
      Suite.Add_Test (Encoder_Queue_Caller.Create ("Encode Sys Active Sensing", Test_Sys_Active_Sensing'Access));
      Suite.Add_Test (Encoder_Queue_Caller.Create ("Encode Sys Reset", Test_Sys_Reset'Access));

      Suite.Add_Test (Encoder_Queue_Caller.Create ("Encode Sys Song Select", Test_Sys_Song_Select'Access));
      Suite.Add_Test (Encoder_Queue_Caller.Create ("Encode Sys Song Position", Test_Sys_Song_Position'Access));
      Suite.Add_Test (Encoder_Queue_Caller.Create ("Encode Sys Bus Select", Test_Sys_Bus_Select'Access));

      Suite.Add_Test (Encoder_Queue_Caller.Create ("Encode Multi Messages", Test_Multi_Messages'Access));
   end Add_Tests;

end Testsuite.Encode;
