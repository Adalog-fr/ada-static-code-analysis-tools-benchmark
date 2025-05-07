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

with Ada.Finalization;
with AdaCL.Limited_Base;
with AdaCL.Reference.Element;

---
--  AdaCL: Reference counted smart pointer.
--
--  Holder class for the actual element
generic

   ---
   --  Type for which we want to supply a reference counter. since, in Ada one
   --  can not overload the ":=" operator the type need to be limited so the
   --  counter is not damaged by assignment.
   --
   --  Mind you, in C++ I almost allways make the operator = private in
   --  Reference counted classes as well.
   type Element_Type (<>) is abstract limited new Element.Object with private;

   ---
   --  an access type to match the element type.
   type Element_Class is access Element_Type'Class;

package AdaCL.Reference.Holder is

   --  Parameterized Class AdaCL.Reference.Holder:Object
   --
   --  Access for Reference Counted Instances.
   --
   --  Most smart pointer librarys keep the counter inside
   --  the smart pointer. While this implementations allows
   --  the use with existing classes it is also very error
   --  prune: The counted instanz allways neet to be handled
   --  with the smart pointer and is never allowed to be
   --  used any other way.
   --
   --  I prefer to count inside the insanz itself. This
   --  keeps the pointer far more reliable.
   type Object is new Ada.Finalization.Controlled with private;

   ---
   --  Creates a new smart pointer from normal pointer.
   --
   --  Pointer to reference counted object
   function Create (Referent : in Element_Class := null) return Object;

   ---
   --  Returns the Pointer to the conted Object.
   --
   --  Object itself.
   function Get (This : in Object) return Element_Class;

   ---
   --  Returns the Pointer to the conted Object as base.
   --
   --  Object itself.
   function Get_Base (This : in Object) return AdaCL.Limited_Base.Object_Class;

   ---
   --  Set a new Pointer. This decreases the counter for the
   --  previous Instanz and increases the Pointer to the
   --  newly set instanz.
   --
   --  This     : Object itself.
   --  Referent : The Object. Set to null to clean the pointer.
   procedure Set (
      This     : in out Object;
      Referent : Element_Class := null);

   ---
   --  Set a new Pointer. This decreases the counter for the
   --  previous Instanz and increases the Pointer to the
   --  newly set instanz.
   --
   --  This      : Object itself.
   --  Reference : The Object. Set to null to clean the pointer.
   procedure Set (
      This      : in out Object;
      Reference : Object);

private

   package Inherited renames Ada.Finalization;

   ---
   --  A Pointer to an counted Object.
   type Object is new Ada.Finalization.Controlled with record
      Referent : Element_Class := null;
   end record;

   procedure Initialize (Object : in out Ada.Finalization.Controlled)
   renames Ada.Finalization.Initialize;

   ---
   --  When asjusting we need to increase the counter.
   --
   --  This : Object itself.
   overriding procedure Adjust (This : in out Object);

   ---
   --  When finalizing we need to decrease the counter. When
   --  the counter reaches 0 we delete the insanz.
   --
   --  This : Object itself.
   overriding procedure Finalize (This : in out Object);

   ---
   --  Returns the Pointer to the conted Object.
   --
   --  Object itself.
   function Get_Base (This : in Object) return AdaCL.Limited_Base.Object_Class
   is (AdaCL.Limited_Base.Object_Class (This.Get));

end AdaCL.Reference.Holder;
