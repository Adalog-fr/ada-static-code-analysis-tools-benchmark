------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                       Copyright (C) 2022, AdaCore                        --
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

package body GPR2.FNMatch is

   -----------
   -- Match --
   -----------

   function Match (Str : String; Pattern : String) return Boolean
   is

      --  P_I and S_I are index to next character to read in respectively
      --  Pattern and String
      P_I : Natural := Pattern'First;
      S_I : Natural := Str'First;

      --  P_C and S_C contain the current character to be processed for
      --  Pattern and String
      P_C : Character;
      S_C : Character;

      procedure Next_Str_Char;
      --  Read the next chracter in String. Adjust S_C and S_I. When no
      --  character is available in String then S_C is set to ASCII.NUL.
      pragma Inline (Next_Str_Char);

      procedure Next_Pattern_Char;
      --  Read the next chracter in Pattern. Adjust P_C and P_I. When no
      --  character is available in Pattern then P_C is set to ASCII.NUL.
      pragma Inline (Next_Pattern_Char);

      function Lookahead_Pattern
        (N : Positive)
            return Character;
      --  Return character which is at N position of the current one without
      --  updating P_I

      function Bracket_Match return Boolean;
      --  Return True if next character in Str match a bracket expression.

      -------------------
      -- Bracket_Match --
      -------------------

      function Bracket_Match return Boolean is
         Saved_P_I           : constant Natural := P_I;
         Is_Negation         : Boolean := False;
         Has_Closing_Bracket : Boolean := False;
         Result              : Boolean := False;
      begin

         if Lookahead_Pattern (1) in '^' | '!' then
            Is_Negation := True;
            Next_Pattern_Char;
         end if;

         loop
            Next_Pattern_Char;
            case P_C is
               when ASCII.NUL =>
                  exit;
                  when ']' =>
                  Has_Closing_Bracket := True;
                  exit;
                  when others =>
                  --  This is either a character match or a range
                  declare
                     Range_Start : constant Character := P_C;
                     Range_End   : Character := P_C;
                  begin
                     if Lookahead_Pattern (1) = '-' and then
                       Lookahead_Pattern (2) not in ASCII.NUL | ']'
                     then
                        --  this is a range
                        Next_Pattern_Char;
                        Next_Pattern_Char;
                        Range_End := P_C;
                     end if;
                     if S_C in Range_Start .. Range_End then
                        Result := True;
                        exit;
                     end if;
                  end;
            end case;
         end loop;

         if Result then
            --  Go to next closing bracket
            while P_C /= ASCII.NUL loop
               Next_Pattern_Char;
               if P_C = ']' then
                  Has_Closing_Bracket := True;
                  exit;
               end if;
            end loop;
         end if;

         if not Has_Closing_Bracket then
            P_I := Saved_P_I;
            return S_C = '[';
         else
            if Is_Negation then
               return not Result;
            else
               return Result;
            end if;
         end if;
      end Bracket_Match;

      -----------------------
      -- Lookahead_Pattern --
      -----------------------

      function Lookahead_Pattern
        (N : Positive)
            return Character
      is
      begin

         if P_I + N - 1 <= Pattern'Last then
            return Pattern (P_I + N - 1);
         else
            return ASCII.NUL;
         end if;
      end Lookahead_Pattern;

      -----------------------
      -- Next_Pattern_Char --
      -----------------------

      procedure Next_Pattern_Char is
      begin
         if P_I <= Pattern'Last then
            P_C := Pattern (P_I);
            P_I := P_I + 1;
         else
            P_C := ASCII.NUL;
         end if;
      end Next_Pattern_Char;

      -------------------
      -- Next_Str_Char --
      -------------------

      procedure Next_Str_Char is
      begin
         if S_I <= Str'Last then
            S_C := Str (S_I);
            S_I := S_I + 1;
         else
            S_C := ASCII.NUL;
         end if;
      end Next_Str_Char;

   begin

      --  Handle special cases in which Pattern is an empty string
      if Pattern'Length = 0 then
         if Str'Length = 0 then
            return True;
         else
            return False;
         end if;
      end if;

      while P_I <= Pattern'Last loop
         Next_Pattern_Char;

         case P_C is
            when '?' =>
               --  A '?' is a pattern that shall match any character.
               --  The only case when match fails is when there is no more
               --  character to read from Str.
               Next_Str_Char;
               if S_C = ASCII.NUL then
                  return False;
               end if;

            when '\' =>
               --  Special characters can be escaped to remove their special
               --  meaning by preceding them with a '\' character.
               --  This escaping '\' is discarded. The sequence "\\"
               --  represents one literal '\'.

               --  Trailing backslash in the pattern result in an invalid
               --  pattern.
               if P_I > Pattern'Last then
                  return False;
               end if;

               Next_Pattern_Char;
               Next_Str_Char;

               if S_C /= P_C then
                  return False;
               end if;

            when '[' =>
               Next_Str_Char;
               if not Bracket_Match then
                  return False;
               end if;

            when '*' =>
               --  Collapse multiple successive '?' and '*'.
               --  For successive ' * ' there is nothing to do.
               --  For '?' only a character on Str is needed.
               while Lookahead_Pattern (1) in '*' | '?' loop

                  Next_Pattern_Char;

                  if P_C = '?' then
                     if S_C = ASCII.NUL then
                        return False;
                     end if;

                     Next_Str_Char;
                  end if;

               end loop;

               --  If we reach the end of the Pattern then match is ensured.
               if P_I > Pattern'Last then
                  return True;
               end if;

               --  Try all string suffixes against remaining part of the
               --  pattern. This simulates how many characters are consumed
               --  by the '*'. (0, 1, ...).
               --  The recursion is not infinite as next character in the
               --  pattern will consume a character from Str
               --  (i.e next character in Pattern is not a ' * ').

               for Index in S_I .. Str'Last loop
                  if Match
                    (Str (Index .. Str'Last),
                     Pattern (P_I .. Pattern'Last))
                  then
                     return True;
                  end if;
               end loop;

               return False;

            when others =>
               Next_Str_Char;
               if S_C /= P_C then
                  return False;
               end if;
         end case;

      end loop;

      return S_I > Str'Last;

   end Match;

end GPR2.FNMatch;
