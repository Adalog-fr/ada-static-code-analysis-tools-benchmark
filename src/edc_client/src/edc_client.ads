--===========================================================================
--
--  This package is the root package for the EDC Client
--
--===========================================================================
--
--  Copyright 2021 (C) Holger Rodriguez
--
--  SPDX-License-Identifier: BSD-3-Clause
--
package Edc_Client is

   --------------------------------------------------------------------------
   --  This is the baud rate for the UART serial interface
   SERIAL_BAUD_RATE : constant Natural := 115_200;

   --------------------------------------------------------------------------
   --  This type defines the callback procedure, which needs to be
   --  provided for the communication to the dashboard for any
   --  communication related procedures
   --------------------------------------------------------------------------
   type Transmit_Procedure is
     not null access procedure (Control : String);

   --------------------------------------------------------------------------
   --  This procedure offers a flexible way to use the library
   --  but not sending anything out,
   --  without changing code or configuration by doing nothing.
   --------------------------------------------------------------------------
   procedure Null_Transmitter (Control : String);

   --------------------------------------------------------------------------
   --  Initializes the client and must be called before any other procedure
   --  in the EDC Client world for any communication
   --------------------------------------------------------------------------
   procedure Initialize (T : Transmit_Procedure := Null_Transmitter'Access);

   --------------------------------------------------------------------------
   --  Returns the status if the EDC Client has been initialized
   --------------------------------------------------------------------------
   function Initialized return Boolean;

private
   --------------------------------------------------------------------------
   --  Stores the call back procedure for any operation
   --------------------------------------------------------------------------
   Client_Transmitter : Transmit_Procedure := Null_Transmitter'Access;

   --------------------------------------------------------------------------
   --  The intermediate transmitter procedure to ensure, that the
   --  flexbility is there for adjustments which should not propagate
   --  into the client code
   --------------------------------------------------------------------------
   procedure Transmitter (Control : String);

end Edc_Client;
