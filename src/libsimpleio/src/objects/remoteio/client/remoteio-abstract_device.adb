-- Abstract device services using the Remote I/O Protocol

-- Copyright (C)2019-2021, Philip Munts, President, Munts AM Corp.
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

WITH Ada.Strings.Fixed;

WITH Messaging;

USE TYPE Messaging.Byte;

PACKAGE BODY RemoteIO.Abstract_Device IS

  -- Abstract device object constructor

  FUNCTION Create
   (remdev  : NOT NULL RemoteIO.Client.Device;
    channel : RemoteIO.ChannelNumber) RETURN Device IS

  BEGIN
    RETURN NEW DeviceClass'(remdev, channel);
  END Create;

  -- Get abstract device information string

  FUNCTION GetInfo
   (Self    : IN OUT DeviceClass) RETURN String IS

    mcmd  : Message64.Message;
    mresp : Message64.Message;
    info  : String(1 .. 61);

  BEGIN
    mcmd := (OTHERS => 0);
    mcmd(0) := Messaging.Byte(RemoteIO.MessageTypes'Pos(
      RemoteIO.DEVICE_INFO_REQUEST));
    mcmd(2) := Messaging.Byte(Self.channel);

    Self.remdev.Transaction(mcmd, mresp);

    info := (OTHERS => ' ');

    FOR i IN info'Range LOOP
      EXIT WHEN mresp(2 + i) = 0;
      info(i) := Character'Val(mresp(2 + i));
    END LOOP;

    RETURN Ada.Strings.Fixed.Trim(info, Ada.Strings.Right);
  END GetInfo;

  -- Perform abstract device operation

  PROCEDURE Operation
   (Self    : DeviceClass;
    cmd     : Command;
    resp    : OUT Response) IS

    mcmd  : Message64.Message;
    mresp : Message64.Message;

  BEGIN
    mcmd := FromCommand(cmd);
    mcmd(0) := Messaging.Byte(RemoteIO.MessageTypes'Pos(RemoteIO.DEVICE_OPERATION_REQUEST));
    mcmd(1) := 0;
    mcmd(2) := Messaging.Byte(Self.channel);

    -- Execute the command

    Self.remdev.Transaction(mcmd, mresp);

    -- Extract the response from the response message

    resp := ToResponse(mresp);
  END Operation;

END RemoteIO.Abstract_Device;
