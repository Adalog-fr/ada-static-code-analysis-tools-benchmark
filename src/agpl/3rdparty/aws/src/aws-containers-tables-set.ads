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

--  $Id: aws-containers-tables-set.ads,v 1.1 2003/10/05 19:59:53 Jano Exp $

package AWS.Containers.Tables.Set is

   procedure Add
     (Table       : in out Table_Type;
      Name, Value : in     String);
   --  Add a new Key/Value pair into the parameter set.

   procedure Update
     (Table : in out Table_Type;
      Name  : in     String;
      Value : in     String;
      N     : in     Positive := 1);
   --  Update the N-th Value with the given Name into the Table.
   --  The container could already have more than one value associated with
   --  this name. If there is M values with this Name, then if:
   --     N <= M      => update the value
   --     N  = M + 1  => the pair name=value is appended to the table
   --     N  > M + 1  => Constraint_Error raised

   procedure Case_Sensitive
     (Table : in out Table_Type;
      Mode  : in     Boolean);
   --  If Mode is True it will use all parameters with case sensitivity.

   procedure Reset (Table : in out Table_Type);
   --  Removes all object from the Set. Set will be reinitialized and will be
   --  ready for new use.

   procedure Free (Table : in out Table_Type);
   --  Release all memory used by the table.

end AWS.Containers.Tables.Set;
