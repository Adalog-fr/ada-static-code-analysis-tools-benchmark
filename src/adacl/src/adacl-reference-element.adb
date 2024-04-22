------------------------------------------------------------------------------
--  Copyright © 2003 … 2023 Martin Krischik «krischik@users.sourceforge.net»
------------------------------------------------------------------------------
--  This library is free software; you can redistribute it and/or modify it
--  under the terms of the GNU Library General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or (at your
--  option) any later version.
--
--  This library is distributed in the hope that it will be useful, but
--  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
--  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public
--  License for more details.
--
--  You should have received a copy of the GNU Library General Public License
--  along with this library; if not, write to the Free Software Foundation,
--  Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
----------------------------------------------------------------------------
pragma License (Modified_Gpl);
pragma Ada_2022;

with AdaCL.Trace;

---
--  Ada Class Library
--  Base Class for Reference Counted Instances.
--
--  Type for which we want to supply a reference counter. since, in Ada one
--  can not overload the ":=" operator the type need to be limited so the
--  counter is not damaged by assignment.
--
--  Mind you, in C++ I almost allways make the operator = private in in
--  Reference counted classes as well.
--
package body AdaCL.Reference.Element is

   ---
   --  Add a reference
   --
   --  Object itself.
   overriding procedure Add_Reference (This : in out Object)
   is
      --  Trace : AdaCL.Trace.Object := AdaCL.Trace.Function_Trace;
      --  pragma Unreferenced (Trace);
   begin
      This.RefCount := Natural'Succ (This.RefCount);

      AdaCL.Trace.Write ("Use Count :" & Natural'Image (This.RefCount));
   end Add_Reference;

   ---
   --  Remove a reference. However since an object and
   --  a pointer is passed the instanz is not deleted.
   --
   --  The caller is responsible to call Use_Count and
   --  check if delete is needed.
   --
   --  Object itself.
   overriding procedure Remove_Reference (This : in out Object)
   is
      --  Trace : AdaCL.Trace.Object := AdaCL.Trace.Function_Trace;
      --  pragma Unreferenced (Trace);
   begin
      This.RefCount := Natural'Pred (This.RefCount);

      AdaCL.Trace.Write ("Use Count :" & Natural'Image (This.RefCount));
   end Remove_Reference;

begin
   null;
end AdaCL.Reference.Element;
