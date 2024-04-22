--
--  Copyright 2021 (C) Holger Rodriguez
--
--  SPDX-License-Identifier: BSD-3-Clause
--
with HAL;

package Edc_Client.Matrix.Double_Word is

   --------------------------------------------------------------------------
   --  Shows the
   --     least significant byte of the least significant word on the matrix
   --     This is equivalent of the most right byte on the matrix
   --------------------------------------------------------------------------
   procedure Show_LSW_LSB (Value : HAL.UInt8)
     with Pre => Initialized;

   --------------------------------------------------------------------------
   --  Shows the
   --     most significant byte of the least significant word on the matrix
   --     This is equivalent of the second right byte on the matrix
   --------------------------------------------------------------------------
   procedure Show_LSW_MSB (Value : HAL.UInt8)
     with Pre => Initialized;

   --------------------------------------------------------------------------
   --  Shows the
   --     least significant byte of the most significant word on the matrix
   --     This is equivalent of the second left byte on the matrix
   --------------------------------------------------------------------------
   procedure Show_MSW_LSB (Value : HAL.UInt8)
     with Pre => Initialized;

   --------------------------------------------------------------------------
   --  Shows the
   --     most significant byte of the most significant word on the matrix
   --     This is equivalent of the most left byte on the matrix
   --------------------------------------------------------------------------
   procedure Show_MSW_MSB (Value : HAL.UInt8)
     with Pre => Initialized;

   --------------------------------------------------------------------------
   --  Shows the
   --     least significant word on the matrix
   --     This is equivalent of the right word on the matrix
   --------------------------------------------------------------------------
   procedure Show_LSW (Value : HAL.UInt16)
     with Pre => Initialized;

   --------------------------------------------------------------------------
   --  Shows the
   --     most significant word on the matrix
   --     This is equivalent of the left word on the matrix
   --------------------------------------------------------------------------
   procedure Show_MSW (Value : HAL.UInt16)
     with Pre => Initialized;

   --------------------------------------------------------------------------
   --  Shows the
   --     double word on the matrix
   --     This is equivalent using all bytes on the matrix
   --------------------------------------------------------------------------
   procedure Show_Double_Word (Value : HAL.UInt32)
     with Pre => Initialized;

end Edc_Client.Matrix.Double_Word;
