--
--  Copyright 2021 (C) Holger Rodriguez
--
--  SPDX-License-Identifier: BSD-3-Clause
--
--  private with Edc_Client.Alpha.Common;

private with Edc_Client.Alpha.Common;

package body Edc_Client.Alpha.Block1 is

   Block : constant Character := '1';

   --------------------------------------------------------------------------
   --  see .ads
   procedure Show_Single_Letter (Position : Single_Letter_Positions;
                                 Value    : Single_Letter_String) is
   begin
      Edc_Client.Alpha.Common.Show_Single_Letter (Position => Position,
                                                  Block    => Block,
                                                  Value    => Value);
   end Show_Single_Letter;

   --------------------------------------------------------------------------
   --  see .ads
   procedure Show_Double_Letters (Position : Double_Letters_Positions;
                                  Value    : Double_Letters_String) is
   begin
      Edc_Client.Alpha.Common.Show_Double_Letters (Position => Position,
                                                   Block    => Block,
                                                   Value    => Value);
   end Show_Double_Letters;

   --------------------------------------------------------------------------
   --  see .ads
   procedure Show_Four_Letters (Position : Four_Letters_Positions;
                                Value    : Four_Letters_String) is
   begin
      Edc_Client.Alpha.Common.Show_Four_Letters (Position => Position,
                                                 Block => Block,
                                                 Value => Value);
   end Show_Four_Letters;

   --------------------------------------------------------------------------
   --  see .ads
   procedure Show_Eight_Letters (Value : Eight_Letters_String) is
      Command  : Cmd_Eight_Letters_String := "A1D1CCCCCCCC";
   begin
      Command (2) := Block;
      Command (5 .. 12) := Value (1 .. 8);
      Transmitter (Command);
   end Show_Eight_Letters;

end Edc_Client.Alpha.Block1;
