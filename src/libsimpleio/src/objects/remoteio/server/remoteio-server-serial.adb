-- Remote I/O Server Services using the Stream Framing Protocol over a serial
-- port

-- Copyright (C)2020-2021, Philip Munts, President, Munts AM Corp.
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

WITH Logging.libsimpleio;
WITH Message64.Stream_libsimpleio;
WITH RemoteIO.Server.Message64;

PACKAGE BODY RemoteIO.Server.Serial IS

  FUNCTION Create
   (exec      : NOT NULL RemoteIO.Executive.Executor;
    name      : String;
    devname   : String;
    baudrate  : Natural := 115200;
    parity    : Natural := libSerial.PARITY_NONE;
    databits  : Natural := 8;
    stopbits  : Natural := 1;
    timeoutms : Natural := 1000) RETURN Instance IS

    fd    : Integer;
    error : Integer;

  BEGIN
    libSerial.Open(devname & ASCII.NUL, baudrate, parity, databits, stopbits,
      fd, error);

    IF error /= 0 THEN
      Logging.libsimpleio.Error("Cannot open " & devname);
      RETURN NULL;
    END IF;

    RETURN RemoteIO.Server.Message64.Create(exec, name,
      Standard.Message64.Stream_libsimpleio.Create(fd, timeoutms));
  END Create;

END RemoteIO.Server.Serial;
