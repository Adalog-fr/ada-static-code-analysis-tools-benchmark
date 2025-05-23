--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Strings_Edit.UTF8.Windows_1257              Luebeck            --
--  Implementation                                 Autumn, 2018       --
--                                                                    --
--                                Last revision :  12:27 04 Nov 2018  --
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

with Ada.Streams;  use Ada.Streams;

package body Strings_Edit.UTF8.Windows_1257 is

   function Convert (Value : Character) return Code_Point is
   begin
      case Stream_Element (Character'Pos (Value)) is
         when 16#00#..16#7F# => return Character'Pos (Value);
         when 16#81# |
              16#83# |
              16#88# |
              16#8A# |
              16#8C# |
              16#90# |
              16#98# |
              16#9A# |
              16#9C# |
              16#9F# |
              16#A1# |
              16#A5# => return 16#0081#; 
         when 16#80# => return 16#20AC#;
         when 16#82# => return 16#201A#;
         when 16#84# => return 16#201E#;
         when 16#85# => return 16#2026#;
         when 16#86# => return 16#2020#;
         when 16#87# => return 16#2021#;
         when 16#89# => return 16#2030#;
         when 16#8B# => return 16#2039#;
         when 16#8D# => return 16#00A8#;
         when 16#8E# => return 16#02C7#;
         when 16#8F# => return 16#00B8#;
         when 16#91# => return 16#2018#;
         when 16#92# => return 16#2019#;
         when 16#93# => return 16#201C#;
         when 16#94# => return 16#201D#;
         when 16#95# => return 16#2022#;
         when 16#96# => return 16#2013#;
         when 16#97# => return 16#2014#;
         when 16#99# => return 16#2122#;
         when 16#9B# => return 16#203A#;
         when 16#9D# => return 16#00AF#;
         when 16#9E# => return 16#02DB#;
         when 16#A0# => return 16#00A0#;
         when 16#A2# => return 16#00A2#;
         when 16#A3# => return 16#00A3#;
         when 16#A4# => return 16#00A4#;
         when 16#A6# => return 16#00A6#;
         when 16#A7# => return 16#00A7#;
         when 16#A8# => return 16#00D8#;
         when 16#A9# => return 16#00A9#;
         when 16#AA# => return 16#0156#;
         when 16#AB# => return 16#00AB#;
         when 16#AC# => return 16#00AC#;
         when 16#AD# => return 16#00AD#;
         when 16#AE# => return 16#00AE#;
         when 16#AF# => return 16#00C6#;
         when 16#B0# => return 16#00B0#;
         when 16#B1# => return 16#00B1#;
         when 16#B2# => return 16#00B2#;
         when 16#B3# => return 16#00B3#;
         when 16#B4# => return 16#00B4#;
         when 16#B5# => return 16#00B5#;
         when 16#B6# => return 16#00B6#;
         when 16#B7# => return 16#00B7#;
         when 16#B8# => return 16#00F8#;
         when 16#B9# => return 16#00B9#;
         when 16#BA# => return 16#0157#;
         when 16#BB# => return 16#00BB#;
         when 16#BC# => return 16#00BC#;
         when 16#BD# => return 16#00BD#;
         when 16#BE# => return 16#00BE#;
         when 16#BF# => return 16#00E6#;
         when 16#C0# => return 16#0104#;
         when 16#C1# => return 16#012E#;
         when 16#C2# => return 16#0100#;
         when 16#C3# => return 16#0106#;
         when 16#C4# => return 16#00C4#;
         when 16#C5# => return 16#00C5#;
         when 16#C6# => return 16#0118#;
         when 16#C7# => return 16#0112#;
         when 16#C8# => return 16#010C#;
         when 16#C9# => return 16#00C9#;
         when 16#CA# => return 16#0179#;
         when 16#CB# => return 16#0116#;
         when 16#CC# => return 16#0122#;
         when 16#CD# => return 16#0136#;
         when 16#CE# => return 16#012A#;
         when 16#CF# => return 16#013B#;
         when 16#D0# => return 16#0160#;
         when 16#D1# => return 16#0143#;
         when 16#D2# => return 16#0145#;
         when 16#D3# => return 16#00D3#;
         when 16#D4# => return 16#014C#;
         when 16#D5# => return 16#00D5#;
         when 16#D6# => return 16#00D6#;
         when 16#D7# => return 16#00D7#;
         when 16#D8# => return 16#0172#;
         when 16#D9# => return 16#0141#;
         when 16#DA# => return 16#015A#;
         when 16#DB# => return 16#016A#;
         when 16#DC# => return 16#00DC#;
         when 16#DD# => return 16#017B#;
         when 16#DE# => return 16#017D#;
         when 16#DF# => return 16#00DF#;
         when 16#E0# => return 16#0105#;
         when 16#E1# => return 16#012F#;
         when 16#E2# => return 16#0101#;
         when 16#E3# => return 16#0107#;
         when 16#E4# => return 16#00E4#;
         when 16#E5# => return 16#00E5#;
         when 16#E6# => return 16#0119#;
         when 16#E7# => return 16#0113#;
         when 16#E8# => return 16#010D#;
         when 16#E9# => return 16#00E9#;
         when 16#EA# => return 16#017A#;
         when 16#EB# => return 16#0117#;
         when 16#EC# => return 16#0123#;
         when 16#ED# => return 16#0137#;
         when 16#EE# => return 16#012B#;
         when 16#EF# => return 16#013C#;
         when 16#F0# => return 16#0161#;
         when 16#F1# => return 16#0144#;
         when 16#F2# => return 16#0146#;
         when 16#F3# => return 16#00F3#;
         when 16#F4# => return 16#014D#;
         when 16#F5# => return 16#00F5#;
         when 16#F6# => return 16#00F6#;
         when 16#F7# => return 16#00F7#;
         when 16#F8# => return 16#0173#;
         when 16#F9# => return 16#0142#;
         when 16#FA# => return 16#015B#;
         when 16#FB# => return 16#016B#;
         when 16#FC# => return 16#00FC#;
         when 16#FD# => return 16#017C#;
         when 16#FE# => return 16#017E#;
         when 16#FF# => return 16#02D9#;
      end case;
   end Convert;

   procedure Convert
             (  Code  : Code_Point;
                Value : out Character;
                Valid : out Boolean
             )  is
   begin
      Valid := True;
      case Code is
         when 16#00#..16#7F# => Value := Character'Val (Code);
         when 16#20AC# => Value := Character'Val (16#80#);
         when 16#201A# => Value := Character'Val (16#82#);
         when 16#201E# => Value := Character'Val (16#84#);
         when 16#2026# => Value := Character'Val (16#85#);
         when 16#2020# => Value := Character'Val (16#86#);
         when 16#2021# => Value := Character'Val (16#87#);
         when 16#2030# => Value := Character'Val (16#89#);
         when 16#2039# => Value := Character'Val (16#8B#);
         when 16#00A8# => Value := Character'Val (16#8D#);
         when 16#02C7# => Value := Character'Val (16#8E#);
         when 16#00B8# => Value := Character'Val (16#8F#);
         when 16#2018# => Value := Character'Val (16#91#);
         when 16#2019# => Value := Character'Val (16#92#);
         when 16#201C# => Value := Character'Val (16#93#);
         when 16#201D# => Value := Character'Val (16#94#);
         when 16#2022# => Value := Character'Val (16#95#);
         when 16#2013# => Value := Character'Val (16#96#);
         when 16#2014# => Value := Character'Val (16#97#);
         when 16#2122# => Value := Character'Val (16#99#);
         when 16#203A# => Value := Character'Val (16#9B#);
         when 16#00AF# => Value := Character'Val (16#9D#);
         when 16#02DB# => Value := Character'Val (16#9E#);
         when 16#00A0# => Value := Character'Val (16#A0#);
         when 16#00A2# => Value := Character'Val (16#A2#);
         when 16#00A3# => Value := Character'Val (16#A3#);
         when 16#00A4# => Value := Character'Val (16#A4#);
         when 16#00A6# => Value := Character'Val (16#A6#);
         when 16#00A7# => Value := Character'Val (16#A7#);
         when 16#00D8# => Value := Character'Val (16#A8#);
         when 16#00A9# => Value := Character'Val (16#A9#);
         when 16#0156# => Value := Character'Val (16#AA#);
         when 16#00AB# => Value := Character'Val (16#AB#);
         when 16#00AC# => Value := Character'Val (16#AC#);
         when 16#00AD# => Value := Character'Val (16#AD#);
         when 16#00AE# => Value := Character'Val (16#AE#);
         when 16#00C6# => Value := Character'Val (16#AF#);
         when 16#00B0# => Value := Character'Val (16#B0#);
         when 16#00B1# => Value := Character'Val (16#B1#);
         when 16#00B2# => Value := Character'Val (16#B2#);
         when 16#00B3# => Value := Character'Val (16#B3#);
         when 16#00B4# => Value := Character'Val (16#B4#);
         when 16#00B5# => Value := Character'Val (16#B5#);
         when 16#00B6# => Value := Character'Val (16#B6#);
         when 16#00B7# => Value := Character'Val (16#B7#);
         when 16#00F8# => Value := Character'Val (16#B8#);
         when 16#00B9# => Value := Character'Val (16#B9#);
         when 16#0157# => Value := Character'Val (16#BA#);
         when 16#00BB# => Value := Character'Val (16#BB#);
         when 16#00BC# => Value := Character'Val (16#BC#);
         when 16#00BD# => Value := Character'Val (16#BD#);
         when 16#00BE# => Value := Character'Val (16#BE#);
         when 16#00E6# => Value := Character'Val (16#BF#);
         when 16#0104# => Value := Character'Val (16#C0#);
         when 16#012E# => Value := Character'Val (16#C1#);
         when 16#0100# => Value := Character'Val (16#C2#);
         when 16#0106# => Value := Character'Val (16#C3#);
         when 16#00C4# => Value := Character'Val (16#C4#);
         when 16#00C5# => Value := Character'Val (16#C5#);
         when 16#0118# => Value := Character'Val (16#C6#);
         when 16#0112# => Value := Character'Val (16#C7#);
         when 16#010C# => Value := Character'Val (16#C8#);
         when 16#00C9# => Value := Character'Val (16#C9#);
         when 16#0179# => Value := Character'Val (16#CA#);
         when 16#0116# => Value := Character'Val (16#CB#);
         when 16#0122# => Value := Character'Val (16#CC#);
         when 16#0136# => Value := Character'Val (16#CD#);
         when 16#012A# => Value := Character'Val (16#CE#);
         when 16#013B# => Value := Character'Val (16#CF#);
         when 16#0160# => Value := Character'Val (16#D0#);
         when 16#0143# => Value := Character'Val (16#D1#);
         when 16#0145# => Value := Character'Val (16#D2#);
         when 16#00D3# => Value := Character'Val (16#D3#);
         when 16#014C# => Value := Character'Val (16#D4#);
         when 16#00D5# => Value := Character'Val (16#D5#);
         when 16#00D6# => Value := Character'Val (16#D6#);
         when 16#00D7# => Value := Character'Val (16#D7#);
         when 16#0172# => Value := Character'Val (16#D8#);
         when 16#0141# => Value := Character'Val (16#D9#);
         when 16#015A# => Value := Character'Val (16#DA#);
         when 16#016A# => Value := Character'Val (16#DB#);
         when 16#00DC# => Value := Character'Val (16#DC#);
         when 16#017B# => Value := Character'Val (16#DD#);
         when 16#017D# => Value := Character'Val (16#DE#);
         when 16#00DF# => Value := Character'Val (16#DF#);
         when 16#0105# => Value := Character'Val (16#E0#);
         when 16#012F# => Value := Character'Val (16#E1#);
         when 16#0101# => Value := Character'Val (16#E2#);
         when 16#0107# => Value := Character'Val (16#E3#);
         when 16#00E4# => Value := Character'Val (16#E4#);
         when 16#00E5# => Value := Character'Val (16#E5#);
         when 16#0119# => Value := Character'Val (16#E6#);
         when 16#0113# => Value := Character'Val (16#E7#);
         when 16#010D# => Value := Character'Val (16#E8#);
         when 16#00E9# => Value := Character'Val (16#E9#);
         when 16#017A# => Value := Character'Val (16#EA#);
         when 16#0117# => Value := Character'Val (16#EB#);
         when 16#0123# => Value := Character'Val (16#EC#);
         when 16#0137# => Value := Character'Val (16#ED#);
         when 16#012B# => Value := Character'Val (16#EE#);
         when 16#013C# => Value := Character'Val (16#EF#);
         when 16#0161# => Value := Character'Val (16#F0#);
         when 16#0144# => Value := Character'Val (16#F1#);
         when 16#0146# => Value := Character'Val (16#F2#);
         when 16#00F3# => Value := Character'Val (16#F3#);
         when 16#014D# => Value := Character'Val (16#F4#);
         when 16#00F5# => Value := Character'Val (16#F5#);
         when 16#00F6# => Value := Character'Val (16#F6#);
         when 16#00F7# => Value := Character'Val (16#F7#);
         when 16#0173# => Value := Character'Val (16#F8#);
         when 16#0142# => Value := Character'Val (16#F9#);
         when 16#015B# => Value := Character'Val (16#FA#);
         when 16#016B# => Value := Character'Val (16#FB#);
         when 16#00FC# => Value := Character'Val (16#FC#);
         when 16#017C# => Value := Character'Val (16#FD#);
         when 16#017E# => Value := Character'Val (16#FE#);
         when 16#02D9# => Value := Character'Val (16#FF#);
         when others   => Valid := False;
      end case;
   end Convert;

   function From_Windows_1257 (Value : Character) return Code_Point is
      Result : constant Code_Point := Convert (Value);
   begin
      if Result = 16#81# then
         raise Constraint_Error;
      else
         return Result;
      end if;
   end From_Windows_1257;

   function From_Windows_1257
            (  Value      : Character;
               Substitute : Code_Point
            )  return Code_Point is
      Result : constant Code_Point := Convert (Value);
   begin
      if Result = 16#81# then
         return Substitute;
      else
         return Result;
      end if;
   end From_Windows_1257;

   function From_Windows_1257 (Value : String) return String is
      Result  : String (1..Value'Length * 2);
      Pointer : Integer := Result'First;
   begin
      for Index in Value'Range loop
         Put (Result, Pointer, From_Windows_1257 (Value));
      end loop;
      return Result (1..Pointer - 1);
   end From_Windows_1257;

   function From_Windows_1257
            (  Value      : String;
               Substitute : Code_Point
            )  return String is
      Result  : String (1..Value'Length * 2);
      Pointer : Integer := Result'First;
   begin
      for Index in Value'Range loop
         Put (Result, Pointer, From_Windows_1257 (Value, Substitute));
      end loop;
      return Result (1..Pointer - 1);
   end From_Windows_1257;

   function From_Windows_1257 (Value : String) return Wide_String is
      Result  : Wide_String (1..Value'Length);
      Pointer : Integer := Result'First;
   begin
      for Index in Value'Range loop
         Result (Pointer) :=
            Wide_Character'Val (From_Windows_1257 (Value (Index)));
         Pointer := Pointer + 1;
      end loop;
      return Result;
   end From_Windows_1257;

   function From_Windows_1257
            (  Value      : String;
               Substitute : Wide_Character
            )  return Wide_String is
      Default : constant Code_Point := Wide_Character'Pos (Substitute);
      Result  : Wide_String (1..Value'Length);
      Pointer : Integer := Result'First;
   begin
      for Index in Value'Range loop
         Result (Pointer) :=
            Wide_Character'Val
            (  From_Windows_1257 (Value (Index), Default)
            );
         Pointer := Pointer + 1;
      end loop;
      return Result;
   end From_Windows_1257;

   function To_Windows_1257 (Value : Code_Point) return Character is
      Result : Character;
      Valid  : Boolean;
   begin
      Convert (Value, Result, Valid);
      if Valid then
         return Result;
      else
         raise Constraint_Error;
      end if;
   end To_Windows_1257;

   function To_Windows_1257
            (  Value      : Code_Point;
               Substitute : Character
            )  return Character is
      Result : Character;
      Valid  : Boolean;
   begin
      Convert (Value, Result, Valid);
      if Valid then
         return Result;
      else
         return Substitute;
      end if;
   end To_Windows_1257;

   function To_Windows_1257 (Value : String) return String is
      Result  : String (1..Value'Length);
      Index   : Integer := 1;
      Code    : Code_Point;
      Pointer : Integer := Value'First;
   begin
      while Pointer <= Value'Last loop
         Get (Value, Pointer, Code);
         Result (Index) := To_Windows_1257 (Code);
         Index := Index + 1;
      end loop;
      return Result (1..Index - 1);
   end To_Windows_1257;

   function To_Windows_1257
            (  Value      : String;
               Substitute : Character
            )  return String is
      Result  : String (1..Value'Length);
      Index   : Integer := 1;
      Code    : Code_Point;
      Pointer : Integer := Value'First;
   begin
      while Pointer <= Value'Last loop
         Get (Value, Pointer, Code);
         Result (Index) := To_Windows_1257 (Code, Substitute);
         Index := Index + 1;
      end loop;
      return Result (1..Index - 1);
   end To_Windows_1257;

   function To_Windows_1257 (Value : Wide_String) return String is
      Result : String (1..Value'Length);
      Index  : Integer := 1;
   begin
      for Pointer in Value'Range loop
         Result (Index) :=
            To_Windows_1257 (Wide_Character'Pos (Value (Pointer)));
         Index := Index + 1;
      end loop;
      return Result;
   end To_Windows_1257;

   function To_Windows_1257
            (  Value      : Wide_String;
               Substitute : Character
            )  return String is
      Result : String (1..Value'Length);
      Index  : Integer := 1;
   begin
      for Pointer in Value'Range loop
         Result (Index) :=
            To_Windows_1257
            (  Wide_Character'Pos (Value (Pointer)),
               Substitute
            );
         Index := Index + 1;
      end loop;
      return Result;
   end To_Windows_1257;

end Strings_Edit.UTF8.Windows_1257;
