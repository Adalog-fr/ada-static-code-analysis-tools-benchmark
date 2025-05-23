------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2002                            --
--                                ACT-Europe                                --
--                                                                          --
--  Authors: Dmitriy Anisimkov - Pascal Obry                                --
--                                                                          --
--  This library is free software; you can redistribute it and/or modify    --
--  it under the terms of the GNU General Public License as published by    --
--  the Free Software Foundation; either version 2 of the License, or (at   --
--  your option) any later version.                                         --
--                                                                          --
--  This library is distributed in the hope that it will be useful, but     --
--  WITHOUT ANY WARRANTY; without even the implied warranty of              --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       --
--  General Public License for more details.                                --
--                                                                          --
--  You should have received a copy of the GNU General Public License       --
--  along with this library; if not, write to the Free Software Foundation, --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.          --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
------------------------------------------------------------------------------

--  $Id: aws-net-ssl.ads,v 1.1 2003/10/05 19:59:56 Jano Exp $

--  This is the SSL based implementation of the Net package. The implementation
--  should depend only on AWS.Net.Std and the SSL library. It is important to
--  not call directly a socket binding here to ease porting.

with Ada.Streams;
with System;

with AWS.Net.Std;
with SSL.Thin;

package AWS.Net.SSL is

   use Ada.Streams;

   type Socket_Type is new Net.Std.Socket_Type with private;

   ----------------
   -- Initialize --
   ----------------

   procedure Accept_Socket
     (Socket     : in     Net.Socket_Type'Class;
      New_Socket : in out Socket_Type);
   --  Accept a connection on a socket.

   procedure Connect
     (Socket   : in out Socket_Type;
      Host     : in     String;
      Port     : in     Positive);
   --  Connect a socket on a given host/port. If Security is true an secure
   --  socket will be used.

   procedure Shutdown (Socket : in Socket_Type);
   --  Shutdown both side of the socket and close it.

   procedure Free (Socket : in out Socket_Type);
   --  Release memory associated with the socket object

   --------
   -- IO --
   --------

   procedure Send
     (Socket : in Socket_Type;
      Data   : in Stream_Element_Array);
   pragma Inline (Send);

   function Receive
     (Socket : in Socket_Type;
      Max    : in Stream_Element_Count := 4096)
      return Stream_Element_Array;
   pragma Inline (Receive);

   --------------------
   -- Initialization --
   --------------------

   type Method is
     (SSLv2,  SSLv2_Server,  SSLv2_Client,
      SSLv23, SSLv23_Server, SSLv23_Client,
      TLSv1,  TLSv1_Server,  TLSv1_Client,
      SSLv3,  SSLv3_Server,  SSLv3_Client);

   procedure Initialize
     (Certificate_Filename : in String;
      Security_Mode        : in Method := SSLv23;
      Key_Filename         : in String := "");
   --  Initialize the SSL layer. Certificate_Filename must point to a valid
   --  certificate. Security mode can be used to change the security method
   --  used by AWS. Key_Filename must be specified if the key is not in the
   --  same file as the certificate. Note that the security options are used
   --  by all running HTTPS server.

private

   package TSSL renames Standard.SSL.Thin;

   subtype SSL_Handle is TSSL.SSL_Handle;

   Null_Ptr : constant SSL_Handle := System.Null_Address;

   type Socket_Type is new Net.Std.Socket_Type with record
      SSL : SSL_Handle := Null_Ptr;
   end record;

end AWS.Net.SSL;
