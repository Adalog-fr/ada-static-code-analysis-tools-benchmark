--
--  Copyright 2021 (C) Holger Rodriguez
--
--  SPDX-License-Identifier: BSD-3-Clause
--
package body Edc_Client.Alpha.Common is

   --  LUT for codes for possible single letter positions
   SL_Position_Codes : constant array (1 .. 8) of
     Character := (1 => '1',
                   2 => '2',
                   3 => '3',
                   4 => '4',
                   5 => '5',
                   6 => '6',
                   7 => '7',
                   8 => '8');

   --  LUT for codes for possible double letter positions
   DL_Position_Codes : constant array (1 .. 4) of
     Character := (1 => '1',
                   2 => '2',
                   3 => '3',
                   4 => '4');

   procedure Show_Single_Letter (Position : Integer;
                                 Block    : Character;
                                 Value    : String) is
      Command  : String := "ABNPC";
   begin
      Command (2) := Block;
      Command (4) := SL_Position_Codes (Position);
      Command (5) := Value (Value'First);
      Transmitter (Command);
   end Show_Single_Letter;

   procedure Show_Double_Letters (Position : Integer;
                                  Block    : Character;
                                  Value    : String) is
      Command  : String := "ABBPCC";
   begin
      Command (2) := Block;
      Command (4) := DL_Position_Codes (Position);
      Command (5 .. 6) := Value (Value'First .. Value'First + 1);
      Transmitter (Command);
   end Show_Double_Letters;

   procedure Show_Four_Letters (Position : Integer;
                                Block    : Character;
                                Value    : String) is
      Command  : String := "ABW1CCCC";
   begin
      Command (2) := Block;
      Command (4) := DL_Position_Codes (Position);
      Command (5 .. 8) := Value (Value'First .. Value'First + 3);
      Transmitter (Command);
   end Show_Four_Letters;

end Edc_Client.Alpha.Common;
