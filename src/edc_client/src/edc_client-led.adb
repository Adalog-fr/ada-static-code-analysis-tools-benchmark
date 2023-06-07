--
--  Copyright 2021 (C) Holger Rodriguez
--
--  SPDX-License-Identifier: BSD-3-Clause
--
package body Edc_Client.LED is

   subtype LED_String_Block is String (1 .. 2);
   BLOCK : constant LED_String_Block := "L0";

   subtype LED_String_Specifier is String (1 .. 2);

   procedure Transmit (Specifier : LED_String_Specifier) is
      Command : LED_String;
   begin
      Command (1 .. 2) := BLOCK (1 .. 2);
      Command (3 .. 4) := Specifier (1 .. 2);
      Transmitter (Command);
   end Transmit;

   --========================================================================
   --
   --  All procedures below are described in the corresponding .ads file
   --
   --========================================================================

   procedure Red_On is
      Specifier : constant LED_String_Specifier := "R1";
   begin
      Transmit (Specifier);
   end Red_On;

   procedure Red_Off is
      Specifier : constant LED_String_Specifier := "R0";
   begin
      Transmit (Specifier);
   end Red_Off;

   procedure Red_Toggle is
      Specifier : constant LED_String_Specifier := "R2";
   begin
      Transmit (Specifier);
   end Red_Toggle;

   procedure Amber_On is
      Specifier : constant LED_String_Specifier := "A1";
   begin
      Transmit (Specifier);
   end Amber_On;

   procedure Amber_Off is
      Specifier : constant LED_String_Specifier := "A0";
   begin
      Transmit (Specifier);
   end Amber_Off;

   procedure Amber_Toggle is
      Specifier : constant LED_String_Specifier := "A2";
   begin
      Transmit (Specifier);
   end Amber_Toggle;

   procedure Green_On is
      Specifier : constant LED_String_Specifier := "G1";
   begin
      Transmit (Specifier);
   end Green_On;

   procedure Green_Off is
      Specifier : constant LED_String_Specifier := "G0";
   begin
      Transmit (Specifier);
   end Green_Off;

   procedure Green_Toggle is
      Specifier : constant LED_String_Specifier := "G2";
   begin
      Transmit (Specifier);
   end Green_Toggle;

   procedure White_On is
      Specifier : constant LED_String_Specifier := "W1";
   begin
      Transmit (Specifier);
   end White_On;

   procedure White_Off is
      Specifier : constant LED_String_Specifier := "W0";
   begin
      Transmit (Specifier);
   end White_Off;

   procedure White_Toggle is
      Specifier : constant LED_String_Specifier := "W2";
   begin
      Transmit (Specifier);
   end White_Toggle;

   procedure Blue_On is
      Specifier : constant LED_String_Specifier := "B1";
   begin
      Transmit (Specifier);
   end Blue_On;

   procedure Blue_Off is
      Specifier : constant LED_String_Specifier := "B0";
   begin
      Transmit (Specifier);
   end Blue_Off;

   procedure Blue_Toggle is
      Specifier : constant LED_String_Specifier := "B2";
   begin
      Transmit (Specifier);
   end Blue_Toggle;

end Edc_Client.LED;
