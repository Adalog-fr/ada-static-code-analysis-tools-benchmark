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

WITH ClickBoard.RemoteIO;
WITH I2C;
WITH PCA9685;
WITH RemoteIO.Client;

PACKAGE ClickBoard.PWM_Click.RemoteIO IS

  -- Create PCA9685 device object from socket number

  FUNCTION Create
   (remdev    : NOT NULL Standard.RemoteIO.Client.Device;
    socknum   : Positive;
    addr      : I2C.Address := DefaultAddress;
    speed     : Positive := PCA9685.MaxSpeed;
    frequency : Positive := 50) RETURN PCA9685.Device;

  -- Create PCA9685 device object from socket

  FUNCTION Create
   (remdev    : NOT NULL Standard.RemoteIO.Client.Device;
    socket    : NOT NULL ClickBoard.RemoteIO.Socket;
    addr      : I2C.Address := DefaultAddress;
    speed     : Positive := PCA9685.MaxSpeed;
    frequency : Positive := 50) RETURN PCA9685.Device;

END ClickBoard.PWM_Click.RemoteIO;
