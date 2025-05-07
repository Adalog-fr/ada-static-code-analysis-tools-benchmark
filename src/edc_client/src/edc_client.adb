--
--  Copyright 2021 (C) Holger Rodriguez
--
--  SPDX-License-Identifier: BSD-3-Clause
--
package body Edc_Client is

   --------------------------------------------------------------------------
   --  Keeps the status about the initialization
   --------------------------------------------------------------------------
   Is_Initialized : Boolean := False;

   --------------------------------------------------------------------------
   --  see .ads
   procedure Null_Transmitter (Control : String) is
      pragma Unreferenced (Control);
   begin
      null;
   end Null_Transmitter;

   --------------------------------------------------------------------------
   --  see .ads
   procedure Initialize (T : Transmit_Procedure := Null_Transmitter'Access) is
   begin
      Client_Transmitter := T;
      Is_Initialized := True;
   end Initialize;

   --------------------------------------------------------------------------
   --  see .ads
   function Initialized return Boolean is
      (Is_Initialized);

   --------------------------------------------------------------------------
   --  see .ads
   procedure Transmitter (Control : String) is
   begin
      Client_Transmitter.all (Control);

      --  add a CR to the pure string transmission
      --  this does not add any functionality, but can be useful for
      --  eavesdropping with a terminal program to see the traffic
      --  without a logic analyzer
      declare
         CR_S : String (1 .. 1);
      begin
         CR_S (1) := Character'Val (13);
         Client_Transmitter.all (CR_S);
      end;
   end Transmitter;

end Edc_Client;
