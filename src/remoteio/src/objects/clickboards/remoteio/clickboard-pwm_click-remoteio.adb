-- Services for the Mikroelektronika PWM Click, using RemoteIO

-- Copyright (C)2016-2023, Philip Munts, President, Munts AM Corp.
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

WITH I2C.RemoteIO;

PACKAGE BODY ClickBoard.PWM_Click.RemoteIO IS

  -- Create PCA9685 device object from static socket instance

  FUNCTION Create
   (remdev    : NOT NULL Standard.RemoteIO.Client.Device;
    socket    : ClickBoard.RemoteIO.SocketSubclass;
    addr      : I2C.Address;
    speed     : Positive;
    frequency : Positive) RETURN PCA9685.Device IS

    bus : CONSTANT I2C.Bus := I2C.RemoteIO.Create(remdev, socket.I2C, speed);

  BEGIN
    RETURN Create(bus, addr, frequency);
  END Create;

  -- Create PCA9685 device object from socket number

  FUNCTION Create
   (remdev    : NOT NULL Standard.RemoteIO.Client.Device;
    socknum   : Positive;
    addr      : I2C.Address := DefaultAddress;
    speed     : Positive := PCA9685.MaxSpeed;
    frequency : Positive := 50) RETURN PCA9685.Device IS

    socket : ClickBoard.RemoteIO.SocketSubclass;

  BEGIN
    socket.Initialize(socknum);
    RETURN Create(remdev, socket, addr, speed, frequency);
  END Create;

  -- Create PCA9685 device object from socket

  FUNCTION Create
   (remdev    : NOT NULL Standard.RemoteIO.Client.Device;
    socket    : NOT NULL ClickBoard.RemoteIO.Socket;
    addr      : I2C.Address := DefaultAddress;
    speed     : Positive := PCA9685.MaxSpeed;
    frequency : Positive := 50) RETURN PCA9685.Device IS

  BEGIN
    RETURN Create(remdev, socket.ALL, addr, speed, frequency);
  END Create;

END ClickBoard.PWM_Click.RemoteIO;
