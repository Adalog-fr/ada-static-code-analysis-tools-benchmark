--===========================================================================
--
--  This package is the interface to the LED part of the EDC Client
--
--===========================================================================
--
--  Copyright 2021 (C) Holger Rodriguez
--
--  SPDX-License-Identifier: BSD-3-Clause
--
package Edc_Client.LED is

   --------------------------------------------------------------------------
   --  Command string for controlling the LEDs
   --------------------------------------------------------------------------
   subtype LED_String is String (1 .. 4);

   --------------------------------------------------------------------------
   --  Procedures to control the red LED
   --------------------------------------------------------------------------
   procedure Red_On with Pre => Initialized;
   procedure Red_Off with Pre => Initialized;
   procedure Red_Toggle with Pre => Initialized;

   --------------------------------------------------------------------------
   --  Procedures to control the amber LED
   --------------------------------------------------------------------------
   procedure Amber_On with Pre => Initialized;
   procedure Amber_Off with Pre => Initialized;
   procedure Amber_Toggle with Pre => Initialized;

   --------------------------------------------------------------------------
   --  Procedures to control the green LED
   --------------------------------------------------------------------------
   procedure Green_On with Pre => Initialized;
   procedure Green_Off with Pre => Initialized;
   procedure Green_Toggle with Pre => Initialized;

   --------------------------------------------------------------------------
   --  Procedures to control the white LED
   --------------------------------------------------------------------------
   procedure White_On with Pre => Initialized;
   procedure White_Off with Pre => Initialized;
   procedure White_Toggle with Pre => Initialized;

   --------------------------------------------------------------------------
   --  Procedures to control the blue LED
   --------------------------------------------------------------------------
   procedure Blue_On with Pre => Initialized;
   procedure Blue_Off with Pre => Initialized;
   procedure Blue_Toggle with Pre => Initialized;

end Edc_Client.LED;
