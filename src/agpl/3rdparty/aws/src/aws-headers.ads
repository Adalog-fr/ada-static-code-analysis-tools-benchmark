------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2001                          --
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

--  $Id: aws-headers.ads,v 1.1 2003/10/05 19:59:54 Jano Exp $

with AWS.Containers.Tables;
with AWS.Net;

package AWS.Headers is

   type List is new AWS.Containers.Tables.Table_Type with private;
   --  Header container. This set handles a set of HTTP header line, each new
   --  header line is inserted at the end of the list (see AWS.Headers.Set API)
   --  and can be retrieved by the following services. Header lines are
   --  numbered from 1 to N.

   subtype VString_Array is AWS.Containers.Tables.VString_Array;

   subtype Element is AWS.Containers.Tables.Element;

   Format_Error : exception;
   --  Raised when header line format is wrong

   procedure Send_Header
     (Socket  : in Net.Socket_Type'Class;
      Headers : in List);
   --  Send all header lines in Headers list to the socket

   function Get_Line
     (Headers : in List;
      N       : in Positive)
      return String;
   --  Returns the Nth header line in Headers container. The returned value is
   --  formatted as a correct header line:
   --
   --     message-header = field-name ":" [ field-value ]
   --
   --  That is the header-name followed with character ':' and the header
   --  values. If there is less than Nth header line it returns the empty
   --  string. Note that this routine does returns all header line values, for
   --  example it would return:
   --
   --     Content_Type: multipart/mixed; boundary="0123_The_Boundary_Value_"
   --
   --  For a file upload content type header style.

   function Get_Values
     (Headers : in List;
      Name    : in String)
      return String;
   --  Returns all values for the specified header field Name in a
   --  comma-separated string. This format is conformant to [RFC 2616 - 4.2]
   --  (see last paragraph).

   --  See AWS.Containers.Tables for inherited routines.

private

   type List is new AWS.Containers.Tables.Table_Type with null record;

end AWS.Headers;
