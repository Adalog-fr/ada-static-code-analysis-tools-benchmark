-- Minimal Ada wrapper for the Linux TCP and UDP over IPV4 services
-- implemented in libsimpleio.so

-- Copyright (C)2016-2021, Philip Munts, President, Munts AM Corp.
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

WITH System;

PACKAGE libIPV4 IS
  INADDR_ANY       : CONSTANT Integer := 0;
  INADDR_LOOPBACK  : CONSTANT Integer := 16#7F000001#;	-- aka localhost
  INADDR_BROADCAST : CONSTANT Integer := -1;

  -- Flags for UDP_Send() and UDP_Receive()

  MSG_DONTROUTE : CONSTANT Integer := 16#0004#;
  MSG_DONTWAIT  : CONSTANT Integer := 16#0040#;
  MSG_MORE      : CONSTANT Integer := 16#8000#;

  -- Resolve host name to 32-bit IPv4 address

  PROCEDURE IP_Resolve
   (hostname : String;
    hostaddr : OUT Integer;
    error    : OUT Integer);
  PRAGMA Import(C, IP_Resolve, "IPV4_resolve");

  -- Convert IPv4 address to dotted decimal string

  PROCEDURE IP_NtoA
   (hostaddr : Integer;
    dst      : OUT String;
    dstsize  : Integer;
    error    : OUT Integer);
  PRAGMA Import(C, IP_NtoA, "IPV4_ntoa");

  -- Connect to an IPv4 TCP server

  PROCEDURE TCP_Connect
   (host     : Integer;
    port     : Integer;
    fd       : OUT Integer;
    error    : OUT Integer);
  PRAGMA Import(C, TCP_Connect, "TCP4_connect");

  -- Wait for a connection from an IPv4 TCP client

  PROCEDURE TCP_Accept
   (iface    : Integer;
    port     : Integer;
    fd       : OUT Integer;
    error    : OUT Integer);
  PRAGMA Import(C, TCP_Accept, "TCP4_accept");

  -- Start IPv4 TCP server

  PROCEDURE TCP_Server
   (iface    : Integer;
    port     : Integer;
    fd       : OUT Integer;
    error    : OUT Integer);
  PRAGMA Import(C, TCP_Server, "TCP4_server");

  -- Close a IPv4 TCP connection

  PROCEDURE TCP_Close
   (fd       : Integer;
    error    : OUT Integer);
  PRAGMA Import(C, TCP_Close, "TCP4_close");

  -- Send data to IPv4 TCP connection peer

  PROCEDURE TCP_Send
   (fd       : Integer;
    buf      : System.Address;
    size     : Integer;
    count    : OUT Integer;
    error    : OUT Integer);
  PRAGMA Import(C, TCP_Send, "TCP4_send");

  -- Receive data from IPv4 TCP connection peer

  PROCEDURE TCP_Receive
   (fd       : Integer;
    buf      : System.Address;
    size     : Integer;
    count    : OUT Integer;
    error    : OUT Integer);
  PRAGMA Import(C, TCP_Receive, "TCP4_receive");

  -- Open a UDP socket

  PROCEDURE UDP_Open
   (host     : Integer;
    port     : Integer;
    fd       : OUT Integer;
    error    : OUT Integer);
  PRAGMA Import(C, UDP_Open, "UDP4_open");

  -- Close a UDP socket

  PROCEDURE UDP_Close
   (fd       : Integer;
    error    : OUT Integer);
  PRAGMA Import(C, UDP_Close, "UDP4_close");

  -- Send a UDP datagram

  PROCEDURE UDP_Send
   (fd       : Integer;
    host     : Integer;
    port     : Integer;
    buf      : System.Address;
    bufsize  : Integer;
    flags    : Integer;
    count    : OUT Integer;
    error    : OUT Integer);
  PRAGMA Import(C, UDP_Send, "UDP4_send");

  -- Receive a UDP datagram

  PROCEDURE UDP_Receive
   (fd       : Integer;
    host     : OUT Integer;
    port     : OUT Integer;
    buf      : System.Address;
    bufsize  : Integer;
    flags    : Integer;
    count    : OUT Integer;
    error    : OUT Integer);
  PRAGMA Import(C, UDP_Receive, "UDP4_receive");

END libIPV4;
