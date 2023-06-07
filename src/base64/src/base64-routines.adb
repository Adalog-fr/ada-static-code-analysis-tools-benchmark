pragma Ada_2012;

package body Base64.Routines is
   function Standard_Character_To_Value (C : Character) return Element is
     (case C is when 'A' => 0, when 'B' => 1, when 'C' => 2, when 'D' => 3,
        when 'E'         => 4, when 'F' => 5, when 'G' => 6, when 'H' => 7,
        when 'I'         => 8, when 'J' => 9, when 'K' => 10, when 'L' => 11,
        when 'M'         => 12, when 'N' => 13, when 'O' => 14, when 'P' => 15,
        when 'Q'         => 16, when 'R' => 17, when 'S' => 18, when 'T' => 19,
        when 'U'         => 20, when 'V' => 21, when 'W' => 22, when 'X' => 23,
        when 'Y'         => 24, when 'Z' => 25, when 'a' => 26, when 'b' => 27,
        when 'c'         => 28, when 'd' => 29, when 'e' => 30, when 'f' => 31,
        when 'g'         => 32, when 'h' => 33, when 'i' => 34, when 'j' => 35,
        when 'k'         => 36, when 'l' => 37, when 'm' => 38, when 'n' => 39,
        when 'o'         => 40, when 'p' => 41, when 'q' => 42, when 'r' => 43,
        when 's'         => 44, when 't' => 45, when 'u' => 46, when 'v' => 47,
        when 'w'         => 48, when 'x' => 49, when 'y' => 50, when 'z' => 51,
        when '0'         => 52, when '1' => 53, when '2' => 54, when '3' => 55,
        when '4'         => 56, when '5' => 57, when '6' => 58, when '7' => 59,
        when '8'         => 60, when '9' => 61, when '+' => 62, when '/' => 63,
        when others      => Element'Last);

   function Standard_Value_To_Character (E : Element) return Character is
     (case E is when 0 => 'A', when 1 => 'B', when 2 => 'C', when 3 => 'D',
        when 4         => 'E', when 5 => 'F', when 6 => 'G', when 7 => 'H',
        when 8         => 'I', when 9 => 'J', when 10 => 'K', when 11 => 'L',
        when 12        => 'M', when 13 => 'N', when 14 => 'O', when 15 => 'P',
        when 16        => 'Q', when 17 => 'R', when 18 => 'S', when 19 => 'T',
        when 20        => 'U', when 21 => 'V', when 22 => 'W', when 23 => 'X',
        when 24        => 'Y', when 25 => 'Z', when 26 => 'a', when 27 => 'b',
        when 28        => 'c', when 29 => 'd', when 30 => 'e', when 31 => 'f',
        when 32        => 'g', when 33 => 'h', when 34 => 'i', when 35 => 'j',
        when 36        => 'k', when 37 => 'l', when 38 => 'm', when 39 => 'n',
        when 40        => 'o', when 41 => 'p', when 42 => 'q', when 43 => 'r',
        when 44        => 's', when 45 => 't', when 46 => 'u', when 47 => 'v',
        when 48        => 'w', when 49 => 'x', when 50 => 'y', when 51 => 'z',
        when 52        => '0', when 53 => '1', when 54 => '2', when 55 => '3',
        when 56        => '4', when 57 => '5', when 58 => '6', when 59 => '7',
        when 60        => '8', when 61 => '9', when 62 => '+', when 63 => '/',
        when others    => Character'Last);

   function URLSafe_Character_To_Value (C : Character) return Element is
     (case C is when 'A' => 0, when 'B' => 1, when 'C' => 2, when 'D' => 3,
        when 'E'         => 4, when 'F' => 5, when 'G' => 6, when 'H' => 7,
        when 'I'         => 8, when 'J' => 9, when 'K' => 10, when 'L' => 11,
        when 'M'         => 12, when 'N' => 13, when 'O' => 14, when 'P' => 15,
        when 'Q'         => 16, when 'R' => 17, when 'S' => 18, when 'T' => 19,
        when 'U'         => 20, when 'V' => 21, when 'W' => 22, when 'X' => 23,
        when 'Y'         => 24, when 'Z' => 25, when 'a' => 26, when 'b' => 27,
        when 'c'         => 28, when 'd' => 29, when 'e' => 30, when 'f' => 31,
        when 'g'         => 32, when 'h' => 33, when 'i' => 34, when 'j' => 35,
        when 'k'         => 36, when 'l' => 37, when 'm' => 38, when 'n' => 39,
        when 'o'         => 40, when 'p' => 41, when 'q' => 42, when 'r' => 43,
        when 's'         => 44, when 't' => 45, when 'u' => 46, when 'v' => 47,
        when 'w'         => 48, when 'x' => 49, when 'y' => 50, when 'z' => 51,
        when '0'         => 52, when '1' => 53, when '2' => 54, when '3' => 55,
        when '4'         => 56, when '5' => 57, when '6' => 58, when '7' => 59,
        when '8'         => 60, when '9' => 61, when '-' => 62, when '_' => 63,
        when others      => Element'Last);

   function URLSafe_Value_To_Character (E : Element) return Character is
     (case E is when 0 => 'A', when 1 => 'B', when 2 => 'C', when 3 => 'D',
        when 4         => 'E', when 5 => 'F', when 6 => 'G', when 7 => 'H',
        when 8         => 'I', when 9 => 'J', when 10 => 'K', when 11 => 'L',
        when 12        => 'M', when 13 => 'N', when 14 => 'O', when 15 => 'P',
        when 16        => 'Q', when 17 => 'R', when 18 => 'S', when 19 => 'T',
        when 20        => 'U', when 21 => 'V', when 22 => 'W', when 23 => 'X',
        when 24        => 'Y', when 25 => 'Z', when 26 => 'a', when 27 => 'b',
        when 28        => 'c', when 29 => 'd', when 30 => 'e', when 31 => 'f',
        when 32        => 'g', when 33 => 'h', when 34 => 'i', when 35 => 'j',
        when 36        => 'k', when 37 => 'l', when 38 => 'm', when 39 => 'n',
        when 40        => 'o', when 41 => 'p', when 42 => 'q', when 43 => 'r',
        when 44        => 's', when 45 => 't', when 46 => 'u', when 47 => 'v',
        when 48        => 'w', when 49 => 'x', when 50 => 'y', when 51 => 'z',
        when 52        => '0', when 53 => '1', when 54 => '2', when 55 => '3',
        when 56        => '4', when 57 => '5', when 58 => '6', when 59 => '7',
        when 60        => '8', when 61 => '9', when 62 => '-', when 63 => '_',
        when others    => Character'Last);
end Base64.Routines;
