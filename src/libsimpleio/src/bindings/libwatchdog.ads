-- Minimal Ada wrapper for the Linux watchdog timer services
-- implemented in libsimpleio.so

-- Copyright (C)2017-2021, Philip Munts, President, Munts AM Corp.
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

-- String actual parameters *MUST* be NUL terminated, e.g. "FOO" & ASCII.NUL

PACKAGE libWatchdog IS
  PROCEDURE Open
   (devname    : String;
    fd         : OUT Integer;
    error      : OUT Integer);
  PRAGMA Import(C, Open, "WATCHDOG_open");

  PROCEDURE Close
   (fd         : Integer;
    error      : OUT Integer);
  PRAGMA Import(C, Close, "WATCHDOG_close");

  PROCEDURE GetTimeout
   (fd         : Integer;
    timeout    : OUT Integer;
    error      : OUT Integer);
  PRAGMA Import(C, GetTimeout, "WATCHDOG_get_timeout");

  PROCEDURE SetTimeout
   (fd         : Integer;
    newtimeout : Integer;
    timeout    : OUT Integer;
    error      : OUT Integer);
  PRAGMA Import(C, SetTimeout, "WATCHDOG_set_timeout");

  PROCEDURE Kick
   (fd         : Integer;
    error      : OUT Integer);
  PRAGMA Import(C, Kick, "WATCHDOG_kick");

END libWatchdog;
