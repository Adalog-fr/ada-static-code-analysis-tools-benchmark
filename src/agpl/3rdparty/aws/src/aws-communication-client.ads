------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2003                          --
--                               ACT-Europe                                 --
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

--  $Id: aws-communication-client.ads,v 1.1 2003/10/05 19:59:52 Jano Exp $

with AWS.Response;

package AWS.Communication.Client is

   function Send_Message
     (Server     : in String;
      Port       : in Positive;
      Name       : in String;
      Parameters : in Parameter_Set := Null_Parameter_Set)
      return Response.Data;
   --  Send a message to server with a set of parameters. The destination is
   --  server is http://Server:Port, the message name is Name and the set of
   --  parameters is to be found into Parameters.
   --
   --  The complete message format is:
   --
   --  http://<Server>:<Port>/AWS_Com?HOST=<host>&NAME=<name>
   --    &P1=<param1>&P2=<param2>

end AWS.Communication.Client;
