-------------------------------------------
--  HAC  <->  Native data exchange demo  --
-------------------------------------------
--  Native side                          --
-------------------------------------------

--  Native & master side of the demo.
--  HAC is embedded in this application.
--  Compile this program with a "full Ada" compiler like GNAT.

with Ada.Directories,
     Ada.Text_IO;

with HAC_Sys.Builder,
     HAC_Sys.PCode.Interpreter;

with Exchange_Native_Side_Pkg;

procedure Exchange_Native_Side is
  use Ada.Text_IO;
  use HAC_Sys.PCode.Interpreter;

  BD : HAC_Sys.Builder.Build_Data;

  procedure Build is
    hac_program_name : constant String := "exchange_hac_side.adb";
  begin
    Put_Line ("Native: building a HAC program: " & hac_program_name);
    New_Line;
    BD.Build_Main_from_File (hac_program_name);
  end Build;

  procedure Run is
    post_mortem : Post_Mortem_Data;
  begin
    Interpret_on_Current_IO (BD, 1, "", post_mortem);
    if Is_Exception_Raised (post_mortem.Unhandled) then
      Put_Line (Current_Error, "HAC VM: raised " & Image (post_mortem.Unhandled));
      Put_Line (Current_Error, Message (post_mortem.Unhandled));
    end if;
  end Run;

begin
  Put_Line ("Exchange_Native_Side is started.");
  New_Line;
  Exchange_Native_Side_Pkg.Register_All_Callbacks (BD);
  Exchange_Native_Side_Pkg.Set_Global (BD);
  Ada.Directories.Set_Directory ("src/apps");
  Build;
  if BD.Build_Successful then
    for i in 1 .. 2 loop
      Put_Line ("Native: Run #" & Integer'Image (i));
      Run;
      Put_Line
        ("Native: Run #" & Integer'Image (i) &
         ". Global string variable is: " & Exchange_Native_Side_Pkg.Get_Global (BD));
      New_Line;
    end loop;
  end if;
end Exchange_Native_Side;
