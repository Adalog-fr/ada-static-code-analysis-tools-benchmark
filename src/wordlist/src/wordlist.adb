with Ada.Numerics.Discrete_Random;
with Ada.Text_IO; use Ada.Text_IO;

with Resources;

with TOML.File_IO;

with Wordlist_Config;

package body Wordlist is

   package Intrnd is new Ada.Numerics.Discrete_Random (Integer);

   Generator : Intrnd.Generator;

   ---------
   -- Log --
   ---------

   procedure Log (Text : String) is
   begin
      if Wordlist_Config.Logging then
         Put_Line (Text);
      end if;
   end Log;

   package My_Resources is new Resources
     (Crate_Name => Wordlist_Config.Crate_Name);

   Words : Word_Set;

   ---------------
   -- All_Words --
   ---------------

   function All_Words return Word_Set is (Words);

   -----------------
   -- Random_Word --
   -----------------

   function Random_Word (This : Word_Vector) return String is
   begin
      return This
        (Intrnd.Random
           (Generator,
            This.First_Index,
            This.Last_Index));
   end Random_Word;

   ---------------
   -- To_Vector --
   ---------------

   function To_Vector (This : Word_Set'Class) return Word_Vector is
   begin
      return Result : Word_Vector do
         for Word of This loop
            Result.Append (Word);
         end loop;
      end return;
   end To_Vector;

   -----------------
   -- With_Length --
   -----------------

   function With_Length (Length : Positive) return Word_Set is
   begin
      return Result : Word_Set do
         for Word of Words loop
            if Word'Length = Length then
               Result.Include (Word);
            end if;
         end loop;
      end return;
   end With_Length;

begin
   Intrnd.Reset (Generator);

   Log ("Loading words from " & My_Resources.Resource_Path & "wordlist.json");

   declare
      Table : constant TOML.TOML_Value :=
                TOML.File_IO.Load_File
                  (My_Resources.Resource_Path
                   & AAA.Strings.To_Lower_Case (Wordlist_Config.Wordset'Image)
                   & ".toml").Value;

      List  : constant TOML.TOML_Value := Table.Get ("wordlist");
   begin
      for I in 1 .. List.Length loop
         Words.Include (List.Item (I).As_String);
      end loop;
   end;

   Log ("Loaded " & AAA.Strings.Trim (Words.Length'Image) & " words");
end Wordlist;
