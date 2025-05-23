-- Fixed length message services using the Stream Framing Protocol.
-- Must be instantiated for each message size.

-- Copyright (C)2021, Philip Munts, President, Munts AM Corp.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
--
-- * Redistributions of source code must retain the above copyright notice,
--   this list of conditions and the following disclaimer.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
-- ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
-- LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
-- CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
-- SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
-- INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
-- CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
-- ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
-- POSSIBILITY OF SUCH DAMAGE.

WITH Ada.Streams.Stream_IO;

GENERIC

PACKAGE Messaging.Fixed.Ada_Streams IS

  -- Type definitions

  TYPE MessengerSubclass IS NEW MessengerInterface WITH PRIVATE;

  -- Constructor

  FUNCTION Create
   (stream    : NOT NULL Ada.Streams.Stream_IO.Stream_Access;
    trimzeros : Boolean := True) RETURN Messenger;

  -- Send a message

  PROCEDURE Send
   (Self : MessengerSubclass;
    msg  : Message);

  -- Receive a message

  PROCEDURE Receive
   (Self : MessengerSubclass;
    msg  : OUT Message);

PRIVATE

  TYPE MessengerSubclass IS NEW MessengerInterface WITH RECORD
    stream : Ada.Streams.Stream_IO.Stream_Access;
    trim   : Boolean;
  END RECORD;

END Messaging.Fixed.Ada_Streams;
