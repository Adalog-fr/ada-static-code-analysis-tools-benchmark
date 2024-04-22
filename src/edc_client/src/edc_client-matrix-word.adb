--
--  Copyright 2021 (C) Holger Rodriguez
--
--  SPDX-License-Identifier: BSD-3-Clause
--
private with Edc_Client.Matrix.Common;

package body  Edc_Client.Matrix.Word is

   --------------------------------------------------------------------------
   --  see .ads
   procedure Show_LSB (Value : HAL.UInt8) is
   begin
      Edc_Client.Matrix.Common.Show_Byte (Value => Value,
                                          Side => '0',
                                          Block => '0');
   end Show_LSB;

   --------------------------------------------------------------------------
   --  see .ads
   procedure Show_MSB (Value : HAL.UInt8) is
   begin
      Edc_Client.Matrix.Common.Show_Byte (Value => Value,
                                          Side => '0',
                                          Block => '1');
   end Show_MSB;

   --------------------------------------------------------------------------
   --  see .ads
   procedure Show_Word (Value : HAL.UInt16) is
   begin
      Edc_Client.Matrix.Common.Show_Word (Value => Value,
                                          Side => '0',
                                          Block => '0');
   end Show_Word;

end  Edc_Client.Matrix.Word;
