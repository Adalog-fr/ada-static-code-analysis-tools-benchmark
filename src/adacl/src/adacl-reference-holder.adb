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

with Ada.Unchecked_Deallocation;
with AdaCL.Trace;

---
--  AdaCL: Reference counted smart pointer.
--
--  Holder class for the actual element
package body AdaCL.Reference.Holder is
   ---
   --  Add a reference
   --
   --  Element: The element to reference
   procedure Add_Reference (Element : in Element_Class);

   ---
   --  Remove a reference
   --
   --  Element: The element to reference
   procedure Remove_Reference (Element : in out Element_Class);

   ---
   --    Remove a reference
   --
   --  Object itself.
   procedure Add_Reference (Element : in Element_Class)
   is
      --  Trace : AdaCL.Trace.Object := AdaCL.Trace.Function_Trace;
      --  pragma Unreferenced (Trace);
   begin
      Element.Add_Reference;

      AdaCL.Trace.Write ("Use Count :" & Natural'Image (Element.Use_Count));
   end Add_Reference;

   ---
   --  When asjusting we need to increase the counter.
   --
   --  This : Object itself.
   overriding procedure Adjust (This : in out Object) is
      --  Trace : AdaCL.Trace.Object := AdaCL.Trace.Function_Trace;
      --  pragma Unreferenced (Trace);
   begin
      if This.Referent /= null then
         Add_Reference (This.Referent);
      end if;
   end Adjust;

   ---
   --  When finalizing we need to decrease the counter. When
   --  the counter reaches 0 we delete the insanz.
   --
   --  This : Object itself.
   overriding procedure Finalize (This : in out Object)
   is
      --  Trace : AdaCL.Trace.Object := AdaCL.Trace.Function_Trace;
      --  pragma Unreferenced (Trace);
   begin
      if This.Referent /= null then
         Remove_Reference (This.Referent);
      end if;
   end Finalize;

   ---
   --  Returns the Pointer to the conted Object.
   --
   --  This : Object itself.
   function Get (This : in Object) return Element_Class
   is
      --  Trace : AdaCL.Trace.Object := AdaCL.Trace.Function_Trace;
      --  pragma Unreferenced (Trace);
   begin
      return This.Referent;
   end Get;

   ---
   --  Creates a new smart pointer from normal pointer.
   --
   --  Referent : Pointer to reference counted object
   function Create (Referent : in Element_Class := null) return Object
   is
      --  Trace : AdaCL.Trace.Object := AdaCL.Trace.Function_Trace;
      --  pragma Unreferenced (Trace);

      Retval     : constant Object := (Ada.Finalization.Controlled with Referent => Referent);
   begin
      if Retval.Referent /= null then
         Add_Reference (Retval.Referent);
      end if;

      return Retval;
   end Create;

   ---
   --    Remove a reference
   --
   --  Object itself.
   procedure Remove_Reference (Element : in out Element_Class)
   is
      --  Trace : AdaCL.Trace.Object := AdaCL.Trace.Function_Trace;
      --  pragma Unreferenced (Trace);

      --  Documentation:
      --    When the counter reaches 0 we delete the insanz.
      --
      procedure Deallocate is new Ada.Unchecked_Deallocation (
         Object => Element_Type'Class,
         Name   => Element_Class);
   begin
      Element.Remove_Reference;

      AdaCL.Trace.Write ("Use Count :" & Natural'Image (Element.Use_Count));

      if Element.Use_Count = 0 then
         Deallocate (Element);
      end if;
   end Remove_Reference;

   ---
   --  Set a new Pointer. This decreases the counter for the
   --  previous Instanz and increases the Pointer to the
   --  newly set instanz.
   --
   --  This     : Object itself.
   --  Referent : The Object. Set to null to clean the pointer.
   procedure Set (
      This     : in out Object;
      Referent : Element_Class := null)
   is
      --  Trace : AdaCL.Trace.Object := AdaCL.Trace.Function_Trace;
      --  pragma Unreferenced (Trace);

      --  So we can savely use x := x without loosing the insanz in between
      Temp : Element_Class := This.Referent;
   begin
      This.Referent := Referent;

      if This.Referent /= null then
         Add_Reference (This.Referent);
      end if;

      if Temp /= null then
         Remove_Reference (Temp);
      end if;
   end Set;

   ---
   --  Set a new Pointer. This decreases the counter for the
   --  previous Instanz and increases the Pointer to the
   --  newly set instanz.
   --
   --  This      : Object itself.
   --  Reference : The Object. Set to null to clean the pointer.
   procedure Set (
      This      : in out Object;
      Reference : Object)
   is
      --  Trace : AdaCL.Trace.Object := AdaCL.Trace.Function_Trace;
      --  pragma Unreferenced (Trace);
   begin
      This.Set (Reference.Referent);
   end Set;

end AdaCL.Reference.Holder;
