------------------------------------------------------------------------
--                                                                    --
--                     McKae Software Utilities                       --
--                                                                    --
--           Copyright (C) 2005-2009 McKae Technologies               --
--                                                                    --
-- The  McKae   software  utilities   are  free  software;   you  can --
-- redistribute it  and/or modify it  under terms of the  GNU General --
-- Public  License  as published  by  the  Free Software  Foundation; --
-- either version  2, or (at  your option) any later  version.  McKae --
-- Software Utilities are  distributed in the hope that  they will be --
-- useful,  but  WITHOUT  ANY  WARRANTY;  without  even  the  implied --
-- warranty of  MERCHANTABILITY or FITNESS FOR  A PARTICULAR PURPOSE. --
-- See the GNU  General Public License for more  details.  You should --
-- have received a copy of the GNU General Public License distributed --
-- with DTraq; see file COPYING.   If not, write to the Free Software --
-- Foundation, 59  Temple Place -  Suite 330, Boston,  MA 02111-1307, --
-- USA.                                                               --
--                                                                    --
-- As a  special exception, if other files  instantiate generics from --
-- this unit,  or you link this  unit with other files  to produce an --
-- executable,  this unit  does  not by  itself  cause the  resulting --
-- executable to be covered by  the GNU General Public License.  This --
-- exception does  not however invalidate  any other reasons  why the --
-- executable file might be covered by the GNU Public License.        --
--                                                                    --
-- The McKae Software Utilities  are maintained by McKae Technologies --
-- (http://www.mckae.com).                                            --
------------------------------------------------------------------------

with Mckae.XML.EZ_Out.Generic_Medium;
with Ada.Strings.Fixed;
with Ada.Text_IO;
use  Ada.Text_IO;
with Unchecked_Deallocation;

package body Mckae.XML.EZ_Out.String_Stream is

   package body String_Buffering is
      procedure Free is new Unchecked_Deallocation(String, Buffer_Ptr);

      -- A basic in-memory string buffering package for building XML
      --  documents with EZ_Out.  This is not intended to be a robust,
      --  full-function memory buffering package.

      procedure Extend (F      : in String_Buffer;
                        To_Add : in Positive)is

         Temp_Buff        : Buffer_Ptr := F.Buff;

         Size_Delta       : Positive;
         Expansion_Factor : Positive;
         New_Size         : Positive := F.Size + To_Add;

      begin
         if New_Size > F.Allocation then
            Size_Delta        := New_Size - F.Allocation;
            Expansion_Factor  := (Size_Delta / F.Expansion) + 1;
            F.Self.Sb.Allocation
              := F.Allocation + (Expansion_Factor * F.Expansion);

            F.Self.Sb.Buff := new String(1 .. F.Allocation);
            F.Self.Sb.Buff(1 .. F.Size) := Temp_Buff(1 .. F.Size);
            Free (Temp_Buff);
         end if;
      end Extend;

      -- Copy the given string into the buffer, expanding it if needed.
      procedure Put(F : in String_Buffer;
                    S : in String) is
      begin
         if S'Length > 0 then
            Extend(F, S'Length);
            F.Buff(F.Size + 1 .. F.Size + S'Length) := S;
            F.Self.Sb.Size := F.Size + S'Length;
         end if;
      end Put;

      -- Insert a new line indicator into the buffer.
      procedure New_Line (F       : in String_Buffer;
                          Spacing : in Ada.Text_IO.Positive_Count := 1) is
         use Ada.Strings.Fixed;
      begin
         null;
      end New_Line;

      -- Clear the buffer
      procedure Clear(S : String_Buffer) is
      begin
         S.Self.Sb.Size := 0;
      end Clear;

      -- Free ressources in order to avoid memory leak
      procedure Full_Clear (S : in out String_Buffer) is
      begin
         Clear (S);
         Free (S.Buff);
         S.Buff := null;
      end Full_Clear;

      -- Return the current contents of the string buffer
      function Get_String(S : String_Buffer) return String is
      begin
         return S.Buff(1 .. S.Size);
      end Get_String;

      procedure Finalize (Object : in out String_Buffer) is
      begin
         Full_Clear (Object);
      end Finalize;

   end String_Buffering;

end Mckae.XML.EZ_Out.String_Stream;



