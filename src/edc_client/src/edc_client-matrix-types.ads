--===========================================================================
--
--  This package defines all types for the various Matrix packages.
--
--===========================================================================
--
--  Copyright 2021 (C) Holger Rodriguez
--
--  SPDX-License-Identifier: BSD-3-Clause
--
package Edc_Client.Matrix.Types is

   --------------------------------------------------------------------------
   --  Command string for controlling the byte part of the display
   --------------------------------------------------------------------------
   subtype Byte_String is String (1 .. 6);

   --------------------------------------------------------------------------
   --  Command string for controlling the word part of the display
   --------------------------------------------------------------------------
   subtype Word_String is String (1 .. 8);

   --------------------------------------------------------------------------
   --  Command string for controlling the double word of the display
   --------------------------------------------------------------------------
   subtype Double_Word_String is String (1 .. 12);

end Edc_Client.Matrix.Types;
