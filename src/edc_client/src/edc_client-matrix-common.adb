--
--  Copyright 2021 (C) Holger Rodriguez
--
--  SPDX-License-Identifier: BSD-3-Clause
--
with Interfaces;

with Edc_Client.Matrix.Types; use Edc_Client.Matrix.Types;

package body Edc_Client.Matrix.Common is

   --------------------------------------------------------------------------
   --  Returns the charactor for the nibble (= lower 4 bit value of a byte)
   --------------------------------------------------------------------------
   function Nibble_To_Char (Nibble : HAL.UInt8) return Character is
      type N_2_C is array (HAL.UInt8 (0) .. HAL.UInt8 (15)) of Character;
      N_2_C_Map : constant N_2_C := (
                                     16#0# => '0',
                                     16#1# => '1',
                                     16#2# => '2',
                                     16#3# => '3',
                                     16#4# => '4',
                                     16#5# => '5',
                                     16#6# => '6',
                                     16#7# => '7',
                                     16#8# => '8',
                                     16#9# => '9',
                                     16#A# => 'A',
                                     16#B# => 'B',
                                     16#C# => 'C',
                                     16#D# => 'D',
                                     16#E# => 'E',
                                     16#F# => 'F'
                                    );

   begin
      return N_2_C_Map (Nibble);
   end Nibble_To_Char;

   --------------------------------------------------------------------------
   --  see .ads
   procedure Show_Byte (Value : HAL.UInt8;
                        Side  : Character;
                        Block : Character) is
      use HAL;
      use Interfaces;
      Command : Byte_String := "MSB000";
      MSN     : Interfaces.Unsigned_8;
      LSN     : Interfaces.Unsigned_8;
   begin
      Command (2) := Side;

      Command (4) := Block;

      MSN := Interfaces.Shift_Right (Interfaces.Unsigned_8 (Value), 4);
      Command (5) := Edc_Client.Matrix.Common.Nibble_To_Char (UInt8 (MSN));

      LSN := Interfaces.Unsigned_8 (Value) and 16#F#;
      Command (6) := Edc_Client.Matrix.Common.Nibble_To_Char (UInt8 (LSN));
      Transmitter (Command);
   end Show_Byte;

   --------------------------------------------------------------------------
   --  see .ads
   procedure Show_Word (Value : HAL.UInt16;
                        Side  : Character;
                        Block : Character) is
      use HAL;
      use Interfaces;
      Command : Word_String := "MSW00000";
      MSB     : constant Interfaces.Unsigned_8 :=
        Interfaces.Unsigned_8 (Interfaces.
                                 Shift_Right (Interfaces.Unsigned_16 (Value),
                                   8));
      LSB     : constant Interfaces.Unsigned_8 :=
        Interfaces.Unsigned_8 (Value and 16#FF#);

      MSN     : Interfaces.Unsigned_8;
      LSN     : Interfaces.Unsigned_8;
   begin
      Command (2) := Side;

      Command (4) := Block;

      MSN := Interfaces.Shift_Right (MSB, 4);
      Command (5) := Edc_Client.Matrix.Common.Nibble_To_Char (UInt8 (MSN));

      LSN := MSB and 16#F#;
      Command (6) := Edc_Client.Matrix.Common.Nibble_To_Char (UInt8 (LSN));

      MSN := Interfaces.Shift_Right (LSB, 4);
      Command (7) := Edc_Client.Matrix.Common.Nibble_To_Char (UInt8 (MSN));

      LSN := LSB and 16#F#;
      Command (8) := Edc_Client.Matrix.Common.Nibble_To_Char (UInt8 (LSN));

      Transmitter (Command);
   end Show_Word;

end Edc_Client.Matrix.Common;
