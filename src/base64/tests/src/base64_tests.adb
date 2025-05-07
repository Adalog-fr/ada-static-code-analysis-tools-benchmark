with AUnit.Reporter.Text;
with AUnit.Run;
with AUnit.Test_Suites;

with Test_Standard;
with Test_URLSafe;

procedure Base64_Tests is
   function Suite return AUnit.Test_Suites.Access_Test_Suite;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Result : constant AUnit.Test_Suites.Access_Test_Suite :=
        AUnit.Test_Suites.New_Suite;
   begin
      Result.Add_Test (Test_Standard.Suite);
      Result.Add_Test (Test_URLSafe.Suite);

      return Result;
   end Suite;

   procedure Runner is new AUnit.Run.Test_Runner (Suite);

   Reporter : AUnit.Reporter.Text.Text_Reporter;
begin
   Reporter.Set_Use_ANSI_Colors (True);
   Runner (Reporter);
end Base64_Tests;
