------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2003                          --
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

--  $Id: aws-translator.ads,v 1.1 2003/10/05 20:00:05 Jano Exp $

with Ada.Streams;
with Ada.Strings.Unbounded;

package AWS.Translator is

   function Base64_Encode
     (Data : in Ada.Streams.Stream_Element_Array)
      return String;
   --  Encode Data using the base64 algorithm

   function Base64_Encode (Data : in String) return String;
   --  Same as above but takes a string as input

   function Base64_Decode
     (B64_Data : in String)
      return Ada.Streams.Stream_Element_Array;
   --  Decode B64_Data using the base64 algorithm

   function QP_Decode
     (QP_Data : in String)
      return String;
   --  Decode QP_Data using the Quoted Printable algorithm

   function To_String
     (Data : in Ada.Streams.Stream_Element_Array)
      return String;
   pragma Inline (To_String);
   --  Convert a Stream_Element_Array to a string. Note that as this routine
   --  returns a String it should not be used with large array as this could
   --  break the stack size limit. Use the routine below for large array.

   function To_Unbounded_String
     (Data : in Ada.Streams.Stream_Element_Array)
      return Ada.Strings.Unbounded.Unbounded_String;
   --  Convert a Stream_Element_Array to an Unbounded_String.

   function To_Stream_Element_Array
     (Data : in String)
      return Ada.Streams.Stream_Element_Array;
   pragma Inline (To_Stream_Element_Array);
   --  Convert a String to a Stream_Element_Array.

end AWS.Translator;
