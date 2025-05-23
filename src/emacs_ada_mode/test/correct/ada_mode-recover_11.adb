function Ada_Mode.Recover_11
  (Grammar : in Wisitoken.Productions.Prod_Arrays.Vector;
   Empty   : in Token_Id_Set)
  return Token_Id_Set
is
   subtype Nonterminal is Token_Id range Grammar.First_Index .. Grammar.Last_Index;
begin
   return Result : Token_Id_Set (Nonterminal) := (others => False) do
      for Prod of Grammar loop
      Rhs_Loop :
         for Rhs of Prod.Rhss loop
         Id_Loop :
            for I in reverse Rhs.Tokens.First_Index + 1 .. Rhs.Tokens.Last_Index loop
               declare
               begin
                  if Id = Prod.Lhs then
                     Result (Id) := True;
                     exit Rhs_Loop;
                  elsif not (Id in Nonterminal) then
                     exit Id_Loop;
                  elsif not Empty (Id) then
                     exit Id_Loop;
                  end if;
               end;
            end loop Id_Loop;
         end loop Rhs_Loop;
      end loop;
   end return;
end Ada_Mode.Recover_11;
