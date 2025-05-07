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
--  Inc., 675 Mass Ave,Cambridge, MA 02139, USA.
----------------------------------------------------------------------------
pragma License (Modified_Gpl);
pragma Ada_2022;

with AdaCL.Limited_Base;

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
package AdaCL.Reference.Element is

   ---
   --  Interface Class for Reference Counted Instances.
   --
   type Object_Interface is limited interface;

   ---
   --  instanciate
   --
   function Create return Object_Interface
   is abstract;

   ---
   --  Add a reference
   --
   procedure Add_Reference (This : in out Object_Interface)
   is abstract;

   --  Documentation:
   --    Remove a reference. However since an object and
   --    a pointer is passed the instanz is not deleted.
   --
   --    The caller is responsible to call Use_Count and
   --    check if delete is needed.
   --
   --  Concurrency: Guarded
   --  Object itself.
   procedure Remove_Reference (This : in out Object_Interface)
   is abstract;

   --
   --  Documentation: Current reference counter.
   --
   --  Object itself.
   function Use_Count (This : in Object_Interface) return Natural
   is abstract;

   --
   --  Documentation: Get name of Class.
   --
   --  Object itself.
   function Get_Name (This : in Object_Interface) return String
   is abstract;

   ---------------------------------------------------------------------------

   --
   --  Base Class for Reference Counted Instances.
   --
   type Object is new AdaCL.Limited_Base.Object
   and Object_Interface
   with private;

   type Object_Class is access Object'Class;

   ---
   --  instanciate as object
   --
   overriding function Create return Object;

   ---
   --  instanciate as access
   --
   function Create return Object_Class;

   ---
   --    Add a reference
   --
   --  Concurrency: Guarded
   --  Object itself.
   overriding procedure Add_Reference (This : in out Object);
   pragma Inline (Add_Reference);

   ---
   --  Remove a reference. However since an object and a pointer is passed
   --  the instanz is not deleted.
   --
   --  The caller is responsible to call Use_Count and check if delete is needed.
   --
   --  This : Object itself.
   overriding procedure Remove_Reference (This : in out Object);

   ---
   --  Current reference counter.
   --
   --  This : Object itself.
   overriding function Use_Count (This : in Object) return Natural;
   pragma Inline (Use_Count);

private
   package Inherited renames AdaCL.Limited_Base;

   --
   --  Mixin Class for Reference Counted Instances.
   --
   --  RefCount: Reference Counter.
   type Object is new Inherited.Object
   and Object_Interface
   with  record
      RefCount : Natural := 0;
   end record;

   overriding function Create return Object
   is (Object'(Inherited.Object with RefCount => 0));

   ---
   --  instanciate as access
   --
   function Create return Object_Class
   is (new Object'(Inherited.Object with RefCount => 0));

   ---
   --  Remove a reference and if the use count reaches 0
   --  the instanz is deleted.
   --
   --  This : Object itself.
   overriding function Use_Count (This : in Object) return Natural
   is (This.RefCount);

end AdaCL.Reference.Element;
