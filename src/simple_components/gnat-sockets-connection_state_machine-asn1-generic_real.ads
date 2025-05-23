--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Connection_State_Machine.      Luebeck            --
--     ASN1.Generic_Real                           Spring, 2019       --
--  Interface                                                         --
--                                Last revision :  18:41 01 Aug 2019  --
--                                                                    --
--  This  library  is  free software; you can redistribute it and/or  --
--  modify it under the terms of the GNU General Public  License  as  --
--  published by the Free Software Foundation; either version  2  of  --
--  the License, or (at your option) any later version. This library  --
--  is distributed in the hope that it will be useful,  but  WITHOUT  --
--  ANY   WARRANTY;   without   even   the   implied   warranty   of  --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU  --
--  General  Public  License  for  more  details.  You  should  have  --
--  received  a  copy  of  the GNU General Public License along with  --
--  this library; if not, write to  the  Free  Software  Foundation,  --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.    --
--                                                                    --
--  As a special exception, if other files instantiate generics from  --
--  this unit, or you link this unit with other files to produce  an  --
--  executable, this unit does not by  itself  cause  the  resulting  --
--  executable to be covered by the GNU General Public License. This  --
--  exception  does not however invalidate any other reasons why the  --
--  executable file might be covered by the GNU Public License.       --
--____________________________________________________________________--
--
--  ASN.1 encoding of reals
--
with Strings_Edit.Float_Edit;

generic
   type Number is digits <>;
   with package Edit is new Strings_Edit.Float_Edit (Number);
package GNAT.Sockets.Connection_State_Machine.ASN1.Generic_Real is
--
-- Implicit_Real_Data_Item -- ASN.1 encoded integer
--
   type Implicit_Real_Data_Item is new ASN1_Data_Item with record
      Value : Number;
   end record;
--
-- Encode -- Overrides ...Connection_State_Machine.ASN1...
--
   procedure Encode
             (  Item    : Implicit_Real_Data_Item;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             );
--
-- Feed -- Implementation of the feed operation
--
   procedure Feed
             (  Item    : in out Implicit_Real_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             );
--
-- Get_ASN1_Type -- Overrides ...Connection_State_Machine.ASN1...
--
   function Get_ASN1_Type
            (  Item : Implicit_Real_Data_Item
            )  return ASN1_Type;
--
-- Get_Value -- Get value of the data object
--
--    Item - The object
--
-- Returns :
--
--    The current value
--
   function Get_Value
            (  Item : Implicit_Real_Data_Item
            )  return Number;
--
-- Is_Implicit -- Overrides ...Connection_State_Machine.ASN1...
--
   function Is_Implicit (Item : Implicit_Real_Data_Item)
      return Boolean;
--
-- Set_Implicit_Tag -- Overrides ...Connection_State_Machine.ASN1...
--
   procedure Set_Implicit_Tag
             (  Item   : in out Implicit_Real_Data_Item;
                Tag    : Tag_Type;
                Length : Stream_Element_Offset
             );
--
-- Set_Value -- Set value of the data object
--
--    Item  - The object
--    Value - The value to set
--
   procedure Set_Value
             (  Item  : in out Implicit_Real_Data_Item;
                Value : Number
             );
--
-- Real_Data_Item -- Explicit form of encoding
--
   type Real_Data_Item is
      new Implicit_Real_Data_Item with null record;
--
-- Encode -- Overrides ...Connection_State_Machine.ASN1...
--
   procedure Encode
             (  Item    : Real_Data_Item;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             );
--
-- Feed -- Implementation of the feed operation
--
   procedure Feed
             (  Item    : in out Real_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             );
--
-- Is_Implicit -- Overrides ...Connection_State_Machine.ASN1...
--
   function Is_Implicit (Item : Real_Data_Item) return Boolean;
--
-- Get -- ASN.1 real from stream element array
--
--    Data    - The stream element array
--    Pointer - The first element to read
--    Length  - The encoding length
--    Value   - The result
--
-- Note that the data type is  not included.  The parameter  Pointer  is
-- advanced beyond the value obtained
--
-- Exceptions :
--
--    Constraint_Error - The value is too large
--    Data_Error       - The data encoding error
--    End_Error        - Not enough data
--    Layout_Error     - Pointer is outside bounds
--
   procedure Get
             (  Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Length  : Stream_Element_Offset;
                Value   : out Number
             );
--
-- Put -- ASN.1 real into stream element array
--
--    Data    - The stream element array
--    Pointer - The first element to write
--    Value   - The value to encode
--
-- The parameter Pointer  is advanced beyond the value output.  The data
-- type is not output.  When Value is string it must be a decimal number
-- representation.
--
-- Exceptions :
--
--    Data_Error   - Invalid Value
--    End_Error    - No room for output
--    Layout_Error - Pointer is outside bounds
--
   procedure Put
             (  Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Value   : Number
             );
   procedure Put
             (  Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Value   : String
             );

   pragma Assert
          (  Number'Machine_Radix = 2
          or else
             Number'Machine_Radix = 8
          or else
             Number'Machine_Radix = 16
          );
end GNAT.Sockets.Connection_State_Machine.ASN1.Generic_Real;
