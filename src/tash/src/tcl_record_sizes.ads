--  Tash is free software; you can redistribute it and/or modify it
--  under terms of the GNU General Public License as published by the
--  Free Software Foundation; either version 2, or (at your option)
--  any later version. Tash is distributed in the hope that it will be
--  useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with Tash; see file COPYING. If not, write to
--
--          Free Software Foundation
--          59 Temple Place - Suite 330
--          Boston, MA 02111-1307, USA
--
--  As a special exception, if other files instantiate generics from this
--  unit, or you link this unit with other files to produce an executable,
--  this unit does not by itself cause the resulting executable to be
--  covered by the GNU General Public License. This exception does not
--  however invalidate any other reasons why the executable file might be
--  covered by the GNU Public License.

package Tcl_Record_Sizes is

   --  Size macros defined in tcl.h.

   NUM_STATIC_TOKENS : constant := 20;

   TCL_DSTRING_STATIC_SIZE : constant := 200;

   --  Sizes of structs defined in tcl.h.

   Tcl_CallFrame_Size : constant := 104;
   Tcl_CallFrame_Alignment : constant := 8;

   Tcl_HashTable_Size : constant := 88;
   Tcl_HashTable_Alignment : constant := 8;

   Tcl_HashSearch_Size : constant := 24;
   Tcl_HashSearch_Alignment : constant := 8;

   Tcl_Interp_Size : constant := 24;
   Tcl_Interp_Alignment : constant := 8;

   Tcl_SavedResult_Size : constant := 248;
   Tcl_SavedResult_Alignment : constant := 8;

end Tcl_Record_Sizes;
