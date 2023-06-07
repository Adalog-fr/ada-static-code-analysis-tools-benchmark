--------------------------------------------------------------- {{{1 ----------
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
--------------------------------------------------------------- }}}1 ----------

pragma License (Modified_Gpl);
pragma Ada_2022;

with Ada.Finalization;
with Ada.Tags;

---
--  Base class for all non limited AdaCL classes - which is currently. all
--  AdaCL Classes. Limited Objects are so limiting ;-).
--
package AdaCL.Base is

   ---------------------------------------------------------------------------

   type Object_Interface is interface;
   type Object_Class     is access Object_Interface'Class;

   ---
   --  Description: Get name of the Class. Shortcut for
   --  Ada.Tags.Expanded_Name(This'Class'Tag);
   --
   function Get_Name (This : in Object_Interface) return String
   is abstract;

   ---------------------------------------------------------------------------

   ---
   --  Base class for all non limited AdaCL classes - which is currently. all
   --  AdaCL Classes. Limited Objects are so limiting ;-).
   --
   type Object is abstract new Ada.Finalization.Controlled
   and Object_Interface
   with private;

   ---
   --  Description: Get name of the Class. Shortcut for
   --  Ada.Tags.Expanded_Name(This'Class'Tag);
   --
   overriding function Get_Name (This : in Object) return String
   is (Ada.Tags.Expanded_Name (Object'Class (This)'Tag));
   pragma Inline (Get_Name);
   pragma Pure_Function (Get_Name);

private
   package Inherited renames Ada.Finalization;

   ---
   --  Base class for all non limited AdaCL classes - which is currently. all
   --  AdaCL Classes. Limited Objects are so limiting ;-).
   --
   type Object is abstract new Ada.Finalization.Controlled
   and Object_Interface
   with null record;

end AdaCL.Base;

---------------------------------------------------------------- {{{ ----------
--  vim: set textwidth=0 nowrap tabstop=8 shiftwidth=3 softtabstop=3 expandtab :
--  vim: set filetype=ada fileencoding=utf-8 fileformat=unix foldmethod=expr :
--  vim: set spell spelllang=en_gb
