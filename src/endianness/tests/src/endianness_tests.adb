with AUnit.Reporter.Text;
with AUnit.Run;

with Tests;

procedure Endianness_Tests is
   procedure Runner is new AUnit.Run.Test_Runner (Tests.Suite);

   Reporter : AUnit.Reporter.Text.Text_Reporter;
begin
   Reporter.Set_Use_ANSI_Colors (True);
   Runner (Reporter);
end Endianness_Tests;
