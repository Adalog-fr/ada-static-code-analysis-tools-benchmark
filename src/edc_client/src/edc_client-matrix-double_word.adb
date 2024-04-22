--
--  Copyright 2021 (C) Holger Rodriguez
--
--  SPDX-License-Identifier: BSD-3-Clause
--
with Interfaces;

private with Edc_Client.Matrix.Common;

with Edc_Client.Matrix.Types; use Edc_Client.Matrix.Types;

package body Edc_Client.Matrix.Double_Word is

   --------------------------------------------------------------------------
   --  see .ads
   procedure Show_LSW_LSB (Value : HAL.UInt8) is
   begin
      Edc_Client.Matrix.Common.Show_Byte (Value => Value,
                                          Side => '1',
                                          Block => '0');
   end Show_LSW_LSB;

   --------------------------------------------------------------------------
   --  see .ads
   procedure Show_LSW_MSB (Value : HAL.UInt8) is
   begin
      Edc_Client.Matrix.Common.Show_Byte (Value => Value,
                                          Side => '1',
                                          Block => '1');
   end Show_LSW_MSB;

   --------------------------------------------------------------------------
   --  see .ads
   procedure Show_MSW_LSB (Value : HAL.UInt8) is
   begin
      Edc_Client.Matrix.Common.Show_Byte (Value => Value,
                                          Side => '1',
                                          Block => '2');
   end Show_MSW_LSB;

   --------------------------------------------------------------------------
   --  see .ads
   procedure Show_MSW_MSB (Value : HAL.UInt8) is
   begin
      Edc_Client.Matrix.Common.Show_Byte (Value => Value,
                                          Side => '1',
                                          Block => '3');
   end Show_MSW_MSB;

   --------------------------------------------------------------------------
   --  see .ads
   procedure Show_LSW (Value : HAL.UInt16) is
   begin
      Edc_Client.Matrix.Common.Show_Word (Value => Value,
                 Side => '1',
                 Block => '0');
   end Show_LSW;

   --------------------------------------------------------------------------
   --  see .ads
   procedure Show_MSW (Value : HAL.UInt16) is
   begin
      Edc_Client.Matrix.Common.Show_Word (Value => Value,
                 Side => '1',
                 Block => '1');
   end Show_MSW;

   --------------------------------------------------------------------------
   --  see .ads
   procedure Show_Double_Word (Value : HAL.UInt32) is
      use HAL;
      use Interfaces;
      Command : Double_Word_String := "M1D000000000";
      MSW     : constant Interfaces.Unsigned_16 :=
        Interfaces.Unsigned_16 (Interfaces.Shift_Right (
                                Interfaces.Unsigned_32 (Value), 16));
      LSW     : constant Interfaces.Unsigned_16
        := Interfaces.Unsigned_16 (Value and 16#FFFF#);

      MSB : Interfaces.Unsigned_8;
      LSB : Interfaces.Unsigned_8;

      MSN : Interfaces.Unsigned_8;
      LSN : Interfaces.Unsigned_8;
   begin
      --
      MSB :=
        Interfaces.Unsigned_8 (Interfaces.Shift_Right (MSW, 8));
      MSN := Interfaces.Shift_Right (MSB, 4);
      Command (5) := Edc_Client.Matrix.Common.Nibble_To_Char (UInt8 (MSN));

      LSN := MSB and 16#F#;
      Command (6) := Edc_Client.Matrix.Common.Nibble_To_Char (UInt8 (LSN));

      LSB :=
        Interfaces.Unsigned_8 (MSW and 16#FF#);

      MSN := Interfaces.Shift_Right (LSB, 4);
      Command (7) := Edc_Client.Matrix.Common.Nibble_To_Char (UInt8 (MSN));

      LSN := LSB and 16#F#;
      Command (8) := Edc_Client.Matrix.Common.Nibble_To_Char (UInt8 (LSN));

      --
      MSB :=
        Interfaces.Unsigned_8 (Interfaces.Shift_Right (LSW, 8));
      MSN := Interfaces.Shift_Right (MSB, 4);
      Command (9) := Edc_Client.Matrix.Common.Nibble_To_Char (UInt8 (MSN));

      LSN := MSB and 16#F#;
      Command (10) := Edc_Client.Matrix.Common.Nibble_To_Char (UInt8 (LSN));

      LSB :=
        Interfaces.Unsigned_8 (LSW and 16#FF#);

      MSN := Interfaces.Shift_Right (LSB, 4);
      Command (11) := Edc_Client.Matrix.Common.Nibble_To_Char (UInt8 (MSN));

      LSN := LSB and 16#F#;
      Command (12) := Edc_Client.Matrix.Common.Nibble_To_Char (UInt8 (LSN));

      --
      Transmitter (Command);
   end Show_Double_Word;

end Edc_Client.Matrix.Double_Word;
