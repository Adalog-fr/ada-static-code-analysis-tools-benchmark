with AAA.Strings;

package Wordlist with Elaborate_Body is

   type Word_Set is new AAA.Strings.Set with null record;

   type Word_Vector is new AAA.Strings.Vector with null record;

   --  Useful things

   function All_Words return Word_Set;

   function With_Length (Length : Positive) return Word_Set;

   not overriding
   function To_Vector (This : Word_Set'Class) return Word_Vector;

   not overriding
   function Random_Word (This : Word_Vector) return String;

end Wordlist;
