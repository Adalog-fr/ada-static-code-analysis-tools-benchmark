--                                                                    --
--  procedure Test_HTTP_Server      Copyright (c)  Dmitry A. Kazakov  --
--     Test_WebSocket_Server                       Luebeck            --
--  Half-duplex WebSockets tes                     Winter, 2013       --
--                                                                    --
--                                Last revision :  23:22 29 Sep 2017  --
--                                                                    --
--  This  library  is  free software; you can redistribute it and/or  --
--  modify it under the terms of the GNU General Public  License  as  --
--  published by the Free Software Foundation; either version  2  of  --
--  the License, or (at your option) any later version. This library  --
--  is distributed in the hope that it will be useful,  but  WITHOUT  --
--  ANY   WARRANTY;   without   even   the   implied   warranty   of  --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU  --
--  General  Public  License  for  more  details.  You  should  have  --
--  received  a  copy  of  the GNU General Public License along with  --
--  this library; if not, write to  the  Free  Software  Foundation,  --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.    --
--                                                                    --
--  As a special exception, if other files instantiate generics from  --
--  this unit, or you link this unit with other files to produce  an  --
--  executable, this unit does not by  itself  cause  the  resulting  --
--  executable to be covered by the GNU General Public License. This  --
--  exception  does not however invalidate any other reasons why the  --
--  executable file might be covered by the GNU Public License.       --
--____________________________________________________________________--

with Ada.Exceptions;          use Ada.Exceptions;
with Ada.Text_IO;             use Ada.Text_IO;
with Test_WebSocket_Servers;  use Test_WebSocket_Servers;

with GNAT.Sockets.Server.Pooled;

procedure Test_WebSocket_Server is
   Minutes : constant := 3.0;
   Port    : constant := 80;
   Tasks   : constant := 5;
begin
   declare
      Factory : aliased Chat_Factory
                        (  Request_Length  => 200,
                           Input_Size      => 80,
                           Output_Size     => 255,
                           Max_Connections => 100
                        );
      Server  : GNAT.Sockets.Server.
                Connections_Server (Factory'Access, Port);
   begin
      Trace_On
      (  Factory  => Factory,
         Received => GNAT.Sockets.Server.Trace_Decoded,
         Sent     => GNAT.Sockets.Server.Trace_Decoded
      );
      Put_Line ("HTTP server started");
      delay 60.0 * Minutes; -- Service
      Put_Line ("HTTP server stopping");
   end;
exception
   when Error : others =>
      Put_Line ("Error: " & Exception_Information (Error));
end Test_WebSocket_Server;
