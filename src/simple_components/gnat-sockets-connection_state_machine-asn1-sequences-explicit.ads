--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Connection_State_Machine.      Luebeck            --
--     ASN1.Sequences.Explicit                     Spring, 2019       --
--  Interface                                                         --
--                                Last revision :  13:37 03 Aug 2019  --
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

with GNAT.Sockets.Connection_State_Machine.ASN1.Sets.Explicit;
use  GNAT.Sockets.Connection_State_Machine.ASN1.Sets.Explicit;

with GNAT.Sockets.Connection_State_Machine.ASN1.Sequences.Implicit;
use  GNAT.Sockets.Connection_State_Machine.ASN1.Sequences.Implicit;

package GNAT.Sockets.Connection_State_Machine.ASN1.Sequences.Explicit is
--
-- Sequence_Data_Item -- Explicit form of encoding
--
   type Sequence_Data_Item is
      new Implicit_Sequence_Data_Item with null record;
   procedure Encode
             (  Item    : Sequence_Data_Item;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             );
   procedure Feed
             (  Item    : in out Sequence_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             );
   function Is_Implicit
            (  Item : Sequence_Data_Item
            )  return Boolean;
------------------------------------------------------------------------
--
-- Sequence_Data_Item -- Explicit form of encoding
--
   type Tagged_Sequence_Data_Item is
      new Implicit_Tagged_Sequence_Data_Item with null record;
   procedure Feed
             (  Item    : in out Tagged_Sequence_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             );
   procedure Encode
             (  Item    : Tagged_Sequence_Data_Item;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             );
   function Is_Implicit
            (  Item : Tagged_Sequence_Data_Item
            )  return Boolean;

------------------------------------------------------------------------
--
-- Sequence_Of_Data_Item -- Explicit form of encoding
--
   type Sequence_Of_Data_Item is
      new Implicit_Sequence_Of_Data_Item with null record;
   procedure Encode
             (  Item    : Sequence_Of_Data_Item;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             );
   procedure Feed
             (  Item    : in out Sequence_Of_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             );
   function Is_Implicit
            (  Item : Sequence_Of_Data_Item
            )  return Boolean;
------------------------------------------------------------------------
--
-- External_Sequence_Of_Data_Item -- Explicit form of encoding
--
   type External_Sequence_Of_Data_Item is
      abstract new External_Set_Of_Data_Item with null record;
--
-- Get_ASN1_Type -- Overrides ...Connection_State_Machine.ASN1...
--
   function Get_ASN1_Type
            (  Item : External_Sequence_Of_Data_Item
            )  return ASN1_Type;


end GNAT.Sockets.Connection_State_Machine.ASN1.Sequences.Explicit;
