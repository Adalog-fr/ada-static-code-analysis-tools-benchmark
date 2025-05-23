------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2002                          --
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

--  $Id: aws-parameters.ads,v 1.1 2003/10/05 19:59:57 Jano Exp $

with Ada.Strings.Unbounded;

with AWS.Containers.Tables;

package AWS.Parameters is

   type List is new AWS.Containers.Tables.Table_Type with private;

   subtype VString_Array is AWS.Containers.Tables.VString_Array;

   function URI_Format (Parameter_List : in List) return String;
   --  Returns the list of parameters in the URI format. This can be added
   --  after the ressource to form the complete URI. The format is:
   --  "?name1=value1&name2=value2..."

   --  See AWS.Containers.Tables for inherited routines.

private
   --  A List must be initialized by calling AWS.Parameters.Set.Reset, Server
   --  is responsible for doing that.

   use Ada.Strings.Unbounded;

   type List is new AWS.Containers.Tables.Table_Type with record
      Parameters : Unbounded_String;
   end record;

end AWS.Parameters;
