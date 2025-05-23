-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2020 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- History:
-- 2020 Feb 15     J. Carter          V1.2--Make more Object.Operation friendly
-- 2016 Jun 01     J. Carter          V1.1--Changed comments for empty declarative parts
-- 2013 Mar 01     J. Carter          V1.0--Initial Ada-07 version
--------------------------------------------------------------------
-- 2002 Oct 01     J. Carter          V1.3--Added Context to Iterate
-- 2001 May 01     J. Carter          V1.2--Improved time complexity of Empty
-- 2000 Dec 01     J. Carter          V1.1--Revised implementation of Iterate
-- 2000 May 01     J. Carter          V1.0--Initial release
--
package body PragmARC.Bag_Unbounded is
   protected body Handle is
      procedure Add (Item : in Element) is
         -- Empty
      begin -- Add
         Bag.Add (Item => Item);
      end Add;

      procedure Clear is
         -- Empty
      begin -- Clear
         Bag.Clear;
      end Clear;

      procedure Delete (Item : in Element) is
         -- Empty
      begin -- Delete
         Bag.Delete (Item => Item);
      end Delete;

      function Empty return Boolean is
         -- Empty
      begin -- Empty
         return Bag.Empty;
      end Empty;

      function Find (Key : Element) return Find_Result is
         Result : Implementation.Find_Result;
      begin -- Find
         Result := Bag.Find (Key);

         if Result.Found then
            return (Found => True, Item => Result.Item);
         else
            return (Found => False);
         end if;
      end Find;

      procedure Iterate (Action : access procedure (Item : in out Element; Continue : out Boolean) ) is
         procedure Local is new Implementation.Iterate (Action => Action.all);
      begin -- Iterate
         Local (Over => Bag);
      end Iterate;

      function Size return Natural is
         -- Empty
      begin -- Size
         return Bag.Size;
      end Size;

      procedure Update (Item : in Element) is
         -- Empty
      begin -- Update
         Bag.Update (Item => Item);
      end Update;
   end Handle;
end PragmARC.Bag_Unbounded;
--
-- This is free software; you can redistribute it and/or modify it under
-- terms of the GNU General Public License as published by the Free Software
-- Foundation; either version 2, or (at your option) any later version.
-- This software is distributed in the hope that it will be useful, but WITH
-- OUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
-- or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
-- for more details. Free Software Foundation, 59 Temple Place - Suite
-- 330, Boston, MA 02111-1307, USA.
--
-- As a special exception, if other files instantiate generics from this
-- unit, or you link this unit with other files to produce an executable,
-- this unit does not by itself cause the resulting executable to be
-- covered by the GNU General Public License. This exception does not
-- however invalidate any other reasons why the executable file might be
-- covered by the GNU Public License.
