--===========================================================================
--
--  This package is the interface the Matrix internal procedures/functions
--  for avoiding duplicating code
--
--===========================================================================
--
--  Copyright 2021 (C) Holger Rodriguez
--
--  SPDX-License-Identifier: BSD-3-Clause
--
with HAL;

private package Edc_Client.Matrix.Common is

   --------------------------------------------------------------------------
   --  Returns the charactor for the nibble (= lower 4 bit value of a byte)
   --------------------------------------------------------------------------
   function Nibble_To_Char (Nibble : HAL.UInt8) return Character;

   --------------------------------------------------------------------------
   --  Shows the byte value given on the matrix display
   --     Value : value to be shown
   --     Side  : which side of the two matrix blocks is used
   --     Block : block to use for showing the value
   --------------------------------------------------------------------------
   procedure Show_Byte (Value : HAL.UInt8;
                        Side  : Character;
                        Block : Character);

   --------------------------------------------------------------------------
   --  Shows the word value given on the matrix display
   --     Value : value to be shown
   --     Side  : which side of the two matrix blocks is used
   --     Block : block to use for showing the value
   --------------------------------------------------------------------------
   procedure Show_Word (Value : HAL.UInt16;
                        Side  : Character;
                        Block : Character);

end Edc_Client.Matrix.Common;
