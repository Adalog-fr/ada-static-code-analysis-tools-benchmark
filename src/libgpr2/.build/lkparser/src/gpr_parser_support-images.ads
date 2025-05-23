
------------------------------------------------------------------------------
--                                                                          --
--                            GPR PROJECT PARSER                            --
--                                                                          --
--            Copyright (C) 2015-2022, Free Software Foundation, Inc.       --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

--  DO NOT EDIT THIS IS AN AUTOGENERATED FILE

--
--  Copyright (C) 2014-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

--  This package provides string formatting helpers related to 'Image attribute
--  references.

package Gpr_Parser_Support.Images is

   function Stripped_Image (I : Integer) return String;
   --  Return the same as Integer'Image (I), but without any leading space

   generic
      type T is private;
      type Idx is (<>);
      type Array_Type is array (Idx range <>) of T;
      with function Image (Self : T) return String is <>;
   function Array_Image
     (Self : Array_Type; Limit : Positive := 80) return String;
   --  Return an image for the array, given an image function for elements. The
   --  array will be represented enclosed in brackets, and elements will be
   --  separated by colons. If the image is longer than ``Limit``, then some
   --  wrapping will be applied.

end Gpr_Parser_Support.Images;
