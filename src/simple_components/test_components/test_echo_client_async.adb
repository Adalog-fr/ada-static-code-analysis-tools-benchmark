--                                                                    --
--  procedure Test_Echo_Server      Copyright (c)  Dmitry A. Kazakov  --
--  Test echo server                               Luebeck            --
--                                                 Winter, 2012       --
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

with Ada.Exceptions;       use Ada.Exceptions;
with Ada.Text_IO;          use Ada.Text_IO;
with GNAT.Sockets.Server;  use GNAT.Sockets.Server;
with Test_Echo_Clients;    use Test_Echo_Clients;

procedure Test_Echo_Client_Async is
   Minutes : constant := 1.0;
   Port    : constant := 5876;
begin
   declare
      Factory : aliased Echo_Factory;
      Server  : Connections_Server (Factory'Access, 0);
   begin
--    Trace_On (Factory, Trace_Any, Trace_Any);
      Connect (Server, new Echo_Connection (90, 90), "127.0.0.1", Port);
      delay 60.0 * Minutes; -- Sending
      Put_Line ("Echo client stopping");
   end;
exception
   when Error : others =>
      Put_Line ("Error: " & Exception_Information (Error));
end Test_Echo_Client_Async;
