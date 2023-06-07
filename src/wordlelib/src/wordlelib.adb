package body Wordlelib is

   --------------
   -- New_Game --
   --------------

   function New_Game (Target : Word) return Game is
   begin
      return Game'(Target   => Target,
                   Attempts => <>);
   end New_Game;

   ------------------
   -- Make_Attempt --
   ------------------

   procedure Make_Attempt (This : in out Game; Attempt : Word) is
   begin
      This.Attempts.Append (Attempt);
   end Make_Attempt;

   -------------------
   -- Attempt_Count --
   -------------------

   function Attempt_Count (This : Game) return Natural is
   begin
      return Natural (This.Attempts.Length);
   end Attempt_Count;

   --------------
   -- Attempts --
   --------------

   function Attempts (This : Game) return Attempt_Array is
   begin
      return Result : Attempt_Array (1 .. This.Attempt_Count) do
         for I in Result'Range loop
            Result (I) := This.Attempts (I);
         end loop;
      end return;
   end Attempts;

   -------------------
   -- Compute_Guess --
   -------------------

   function Compute_Guess (Attempt : Word; Target : Word) return Guess_Result
   is

      type Char_Count_Map is array (Valid_Chars) of Natural
        with Default_Component_Value => 0;

      Attempt_Count, Target_Count : Char_Count_Map;

   begin

      --  Prepare target count for later use:

      for Char of Target loop
         Target_Count (Char) := @ + 1;
      end loop;

      return Result : Guess_Result := (others => Miss) do

         --  Easy first : proper hits

         for I in Attempt'Range loop
            if Attempt (I) = Target (I) then
               Result (I) := Hit;
               Attempt_Count (Attempt (I)) := @ + 1;
            end if;
         end loop;

         --  Now, the missplacements

         for I in Attempt'Range loop
            declare
               Guess_Char : constant Character := Attempt (I);
            begin
               if Result (I) /= Hit then
                  if (for some Char of Target => Attempt (I) = Char) then
                     if Attempt_Count (Guess_Char) < Target_Count (Guess_Char)
                     then
                        Result (I) := Missplaced;
                     end if;

                     --  We have "used" one valid char in this guess
                     Attempt_Count (Guess_Char) := @ + 1;
                  end if;
               end if;
            end;
         end loop;

      end return;
   end Compute_Guess;

   -------------
   -- Guesses --
   -------------

   function Guesses (This : Game) return Guess_Array is
   begin
      return Result : Guess_Array (1 .. This.Attempt_Count) do
         for I in Result'Range loop
            Result (I) := Compute_Guess (Attempt => This.Attempts (I),
                                         Target  => This.Target);
         end loop;
      end return;
   end Guesses;

   ------------
   -- Target --
   ------------

   function Target (This : Game) return Word is
   begin
      return This.Target;
   end Target;

   ---------
   -- Won --
   ---------

   function Won (This : Game) return Boolean is
   begin
      return This.Attempt_Count > 0 and then
        This.Attempts.Last_Element = This.Target;
   end Won;

end Wordlelib;
