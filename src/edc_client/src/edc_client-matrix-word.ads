--===========================================================================
--
--  This package is the interface to the Matrix Word part of the EDC Client
--  As a client of this package, you can chose, if you want to display
--  bytes independently or a word or a combination of both.
--
--===========================================================================
--
--  Copyright 2021 (C) Holger Rodriguez
--
--  SPDX-License-Identifier: BSD-3-Clause
--
with HAL;

package Edc_Client.Matrix.Word is

   --------------------------------------------------------------------------
   --  Shows the least significant byte on the matrix
   --  This is equivalent of the right byte on the matrix
   --------------------------------------------------------------------------
   procedure Show_LSB (Value : HAL.UInt8)
     with Pre => Initialized;

   --------------------------------------------------------------------------
   --  Shows the most significant byte on the matrix
   --  This is equivalent of the left byte on the matrix
   --------------------------------------------------------------------------
   procedure Show_MSB (Value : HAL.UInt8)
     with Pre => Initialized;

   --------------------------------------------------------------------------
   --  Shows the full word on the matrix
   --------------------------------------------------------------------------
   procedure Show_Word (Value : HAL.UInt16)
     with Pre => Initialized;

end  Edc_Client.Matrix.Word;
