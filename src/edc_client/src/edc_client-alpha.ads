--===========================================================================
--
--  This package is the root package for the Alpha part of the EDC Client
--  All shared type definitions are defined here
--
--===========================================================================
--
--  Copyright 2021 (C) Holger Rodriguez
--
--  SPDX-License-Identifier: BSD-3-Clause
--
package Edc_Client.Alpha is

   --------------------------------------------------------------------------
   --  Type for displaying a single letter
   subtype Single_Letter_String is String (1 .. 1);

   --------------------------------------------------------------------------
   --  Type for displaying double letters
   subtype Double_Letters_String is String (1 .. 2);

   --------------------------------------------------------------------------
   --  Type for displaying four letters
   subtype Four_Letters_String is String (1 .. 4);

   --------------------------------------------------------------------------
   --  Type for displaying eight letters
   subtype Eight_Letters_String is String (1 .. 8);

   --------------------------------------------------------------------------
   --  Command string for controlling the single letter part of the display
   --------------------------------------------------------------------------
   subtype Cmd_Single_Letter_String is String (1 .. 5);

   --------------------------------------------------------------------------
   --  Command string for controlling the single letter part of the display
   --------------------------------------------------------------------------
   subtype Cmd_Double_Letters_String is String (1 .. 6);

   --------------------------------------------------------------------------
   --  Command string for controlling the single letter part of the display
   --------------------------------------------------------------------------
   subtype Cmd_Four_Letters_String is String (1 .. 8);

   --------------------------------------------------------------------------
   --  Command string for controlling the single letter part of the display
   --------------------------------------------------------------------------
   subtype Cmd_Eight_Letters_String is String (1 .. 12);

private
   Block_Codes : constant array (1 .. 2) of Character := (1 => '1',
                                                          2 => '2');

end Edc_Client.Alpha;
