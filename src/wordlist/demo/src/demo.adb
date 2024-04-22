with Ada.Text_IO; use Ada.Text_IO;

with Wordlist;

procedure Demo is
   Longest  : Natural := 0;
   Shortest : Natural := Natural'Last;
begin
   for Word of Wordlist.All_Words loop
      Longest  := Natural'Max (Longest, Word'Length);
      Shortest := Natural'Min (Shortest, Word'Length);
   end loop;

   for Len in Shortest .. Longest loop
      Put_Line ("Words of length " & Len'Image & ":"
                & Wordlist.With_Length (Len).Length'Image);
   end loop;
end Demo;
