------------------------------------------------------------------------------
--                                                                          --
--                             GPR TECHNOLOGY                               --
--                                                                          --
--                     Copyright (C) 2006-2023, AdaCore                     --
--                                                                          --
-- This is  free  software;  you can redistribute it and/or modify it under --
-- terms of the  GNU  General Public License as published by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for more details.  You should have received  a copy of the  GNU  --
-- General Public License distributed with GNAT; see file  COPYING. If not, --
-- see <http://www.gnu.org/licenses/>.                                      --
--                                                                          --
------------------------------------------------------------------------------

--  gprbind is the executable called by gprbuild to bind Ada sources. It is
--  the driver for gnatbind. It gets its input from gprbuild through the
--  binding exchange file and gives back its results through the same file.

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Containers.Indefinite_Vectors;
with Ada.Directories;
with Ada.Text_IO;      use Ada.Text_IO;

with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;               use GNAT.OS_Lib;

with Gprexch;        use Gprexch;
with GPR.Script;     use GPR, GPR.Script;
with GPR.ALI;        use GPR.ALI;
with GPR.Names;      use GPR.Names;
with GPR.Osint;      use GPR.Osint;
with GPR.Tempdir;
with GPR.Util;       use GPR.Util;

procedure Gprbind is

   Executable_Suffix : constant String_Access := Get_Executable_Suffix;
   --  The suffix of executables on this platforms

   GNATBIND : String_Access := new String'("gnatbind");
   --  The file name of the gnatbind executable. May be modified by an option
   --  in the Minimum_Binder_Options.

   Gnatbind_Prefix_Equal : constant String := "gnatbind_prefix=";
   --  Start of the option to specify a prefix for the gnatbind executable

   Gnatbind_Path_Equal : constant String := "--gnatbind_path=";
   --  Start of the option to specify the absolute path of gnatbind

   Ada_Binder_Equal : constant String := "ada_binder=";
   --  Start of the option to specify the full name of the Ada binder
   --  executable. Introduced for GNAAMP, where it is gnaambind.

   Quiet_Output        : Boolean := False;
   Verbose_Low_Mode    : Boolean := False;
   Verbose_Higher_Mode : Boolean := False;

   Dash_O_Specified      : Boolean := False;
   Dash_O_File_Specified : Boolean := False;

   There_Are_Stand_Alone_Libraries : Boolean := False;
   --  Set to True if the corresponding label is in the exchange file

   No_Main_Option : constant String := "-n";
   Dash_o         : constant String := "-o";
   Dash_x         : constant String := "-x";
   Dash_Fequal    : constant String := "-F=";
   Dash_OO        : constant String := "-O";

   --  Minimum switches to be used to compile the binder generated file

   Dash_c      : constant String := "-c";
   Dash_gnatA  : constant String := "-gnatA";
   Dash_gnatWb : constant String := "-gnatWb";
   Dash_gnatiw : constant String := "-gnatiw";
   Dash_gnatws : constant String := "-gnatws";

   IO_File : File_Type;
   --  The file to get the inputs and to put the results of the binding

   Line : String (1 .. 1_000);
   Last : Natural;

   Exchange_File_Name : String_Access;
   Ada_Compiler_Path  : String_Access;
   FULL_GNATBIND      : String_Access;
   Gnatbind_Path      : String_Access;
   Gnatbind_Path_Specified : Boolean := False;

   Compiler_Options          : String_Vectors.Vector;
   Compiler_Trailing_Options : String_Vectors.Vector;
   Gnatbind_Options          : String_Vectors.Vector;

   Main_ALI : String_Access := null;

   Main_Base_Name        : String_Access := null;
   Binder_Generated_File : String_Access := null;
   BG_File               : File_Type;

   Mapping_File : String_Access := null;

   Success     : Boolean := False;
   Return_Code : Integer;

   Adalib_Dir  : String_Access;
   Prefix_Path : String_Access;
   Lib_Path    : String_Access;

   Static_Libs : Boolean := True;

   Current_Section : Binding_Section := No_Binding_Section;

   All_Binding_Options : Boolean;
   Get_Option          : Boolean;
   Xlinker_Seen        : Boolean;
   Stack_Equal_Seen    : Boolean;

   GNAT_Version : String_Access := new String'("000");
   --  The version of GNAT, coming from the Toolchain_Version for Ada

   GNAT_Version_First_2 : String (1 .. 2);

   GNAT_Version_Set : Boolean := False;
   --  True when the toolchain version is in the input exchange file

   Delete_Temp_Files : Boolean := True;

   FD_Objects   : File_Descriptor;
   Objects_Path : Path_Name_Type;
   Objects_File : File_Type;

   Ada_Object_Suffix : String_Access := Get_Object_Suffix;

   Display_Line : String_Access := new String (1 .. 1_000);
   Display_Last : Natural := 0;
   --  A String buffer to store temporarily the displayed gnatbind command
   --  invoked by gprbind.

   procedure Add_To_Display_Line (S : String);
   --  Add an argument to the Display_Line

   procedure Output_Lib_Path_Or_Line (Lib_Name : String);
   --  Output to IO_File full library pathname to the Other_Arguments if found
   --  in Prefix_Path, Output Line (1 .. Last) otherwise.

   Binding_Options_Table : String_Vectors.Vector;

   Binding_Option_Dash_V_Specified : Boolean := False;
   --  Set to True if -v is specified in the binding options

   GNAT_6_Or_Higher   : Boolean := False;
   --  Set to True when GNAT version is neither 3.xx nor 5.xx

   GNAT_6_4_Or_Higher : Boolean := False;
   --  Set to True when GNAT_6_Or_Higher is True and if GNAT version is 6.xy
   --  with x >= 4.

   ALI_Files_Table : String_Vectors.Vector;

   type Path_And_Stamp (Path_Len, Stamp_Len : Natural) is record
      Path  : String (1 .. Path_Len);
      Stamp : String (1 .. Stamp_Len);
   end record;

   package PS_Vectors is new Ada.Containers.Indefinite_Vectors
     (Positive, Path_And_Stamp);

   Project_Paths : PS_Vectors.Vector;

   type Bound_File;
   type Bound_File_Access is access Bound_File;
   type Bound_File is record
      Name : String_Access;
      Next : Bound_File_Access;
   end record;

   Bound_Files : Bound_File_Access;

   -------------------------
   -- Add_To_Display_Line --
   -------------------------

   procedure Add_To_Display_Line (S : String) is
   begin
      while Display_Last + 1 + S'Length > Display_Line'Last loop
         declare
            New_Buffer : constant String_Access :=
              new String (1 .. 2 * Display_Line'Length);
         begin
            New_Buffer (1 .. Display_Last) :=
              Display_Line (1 .. Display_Last);
            Free (Display_Line);
            Display_Line := New_Buffer;
         end;
      end loop;

      if Display_Last > 0 then
         Display_Last := Display_Last + 1;
         Display_Line (Display_Last) := ' ';
      end if;

      Display_Line (Display_Last + 1 .. Display_Last + S'Length) := S;
      Display_Last := Display_Last + S'Length;
   end Add_To_Display_Line;

   -----------------------------
   -- Output_Lib_Path_Or_Line --
   -----------------------------

   procedure Output_Lib_Path_Or_Line (Lib_Name : String) is
   begin
      Lib_Path := Locate_Regular_File (Lib_Name, Prefix_Path.all);

      if Lib_Path /= null then
         Put_Line (IO_File, Lib_Path.all);
         Free (Lib_Path);
      else
         Put_Line (IO_File, Line (1 .. Last));
      end if;
   end Output_Lib_Path_Or_Line;

begin
   Set_Program_Name ("gprbind");

   --  As the section header has alreading been displayed when gprlib was
   --  invoked, indicate that it should not be displayed again.

   GPR.Set (Section => GPR.Bind);

   if Argument_Count /= 1 then
      Fail_Program (null, "incorrect invocation");
   end if;

   Exchange_File_Name := new String'(Argument (1));

   --  DEBUG: save a copy of the exchange file

   declare
      Gprbind_Debug : constant String := Getenv ("GPRBIND_DEBUG").all;

   begin
      if Gprbind_Debug = "TRUE" then
         Copy_File
           (Exchange_File_Name.all,
            Exchange_File_Name.all & "__saved",
            Success,
            Mode => Overwrite,
            Preserve => Time_Stamps);
      end if;
   end;

   --  Open the binding exchange file

   begin
      Open (IO_File, In_File, Exchange_File_Name.all);
   exception
      when others =>
         Fail_Program (null, "could not read " & Exchange_File_Name.all);
   end;

   --  Get the information from the binding exchange file

   while not End_Of_File (IO_File) loop
      Get_Line (IO_File, Line, Last);

      if Last > 0 then
         if Line (1) = '[' then
            Current_Section := Get_Binding_Section (Line (1 .. Last));

            case Current_Section is
               when No_Binding_Section =>
                  Fail_Program
                    (null, "unknown section: " & Line (1 .. Last));

               when Quiet =>
                  Quiet_Output        := True;
                  Verbose_Low_Mode    := False;
                  Verbose_Higher_Mode := False;

               when Verbose_Low =>
                  Quiet_Output        := False;
                  Verbose_Low_Mode    := True;
                  Verbose_Higher_Mode := False;

               when Verbose_Higher =>
                  Quiet_Output        := False;
                  Verbose_Low_Mode    := True;
                  Verbose_Higher_Mode := True;

               when Shared_Libs =>
                  Static_Libs := False;

               when Gprexch.There_Are_Stand_Alone_Libraries =>
                  There_Are_Stand_Alone_Libraries := True;

               when others =>
                  null;
            end case;

         else
            case Current_Section is
               when No_Binding_Section =>
                  Fail_Program
                    (null, "no section specified: " & Line (1 .. Last));

               when Quiet =>
                  Fail_Program (null, "quiet section should be empty");

               when Verbose_Low | Verbose_Higher =>
                  Fail_Program (null, "verbose section should be empty");

               when Shared_Libs =>
                  Fail_Program
                    (null, "shared libs section should be empty");

               when Gprexch.There_Are_Stand_Alone_Libraries =>
                  Fail_Program
                    (null, "stand-alone libraries section should be empty");

               when Gprexch.Main_Base_Name =>
                  if Main_Base_Name /= null then
                     Fail_Program
                       (null, "main base name specified multiple times");
                  end if;

                  Main_Base_Name := new String'(Line (1 .. Last));

               when Gprexch.Mapping_File =>
                  Mapping_File := new String'(Line (1 .. Last));

               when Compiler_Path =>
                  if Ada_Compiler_Path /= null then
                     Fail_Program
                       (null, "compiler path specified multiple times");
                  end if;

                  Ada_Compiler_Path := new String'(Line (1 .. Last));

               when Compiler_Leading_Switches =>
                  Compiler_Options.Append (Line (1 .. Last));

               when Compiler_Trailing_Switches =>
                  Compiler_Trailing_Options.Append (Line (1 .. Last));

               when Main_Dependency_File =>
                  if Main_ALI /= null then
                     Fail_Program
                       (null, "main ALI file specified multiple times");
                  end if;

                  Main_ALI := new String'(Line (1 .. Last));

               when Dependency_Files =>
                  ALI_Files_Table.Append (Line (1 .. Last));

               when Binding_Options =>
                  --  Check if a gnatbind absolute is specified

                  if Last > Gnatbind_Path_Equal'Length
                    and then Line (1 .. Gnatbind_Path_Equal'Length) =
                             Gnatbind_Path_Equal
                  then
                     Gnatbind_Path := new String'
                       (Line (Gnatbind_Path_Equal'Length + 1 .. Last));
                     Gnatbind_Path_Specified := True;

                  --  Check if a gnatbind prefix is specified

                  elsif Starts_With (Line (1 .. Last), Gnatbind_Prefix_Equal)
                  then
                     --  Ignore an empty prefix

                     if Last > Gnatbind_Prefix_Equal'Length then
                        --  There is always a '-' between <prefix> and
                        --  "gnatbind". Add one if not already in <prefix>.

                        if Line (Last) /= '-' then
                           Last := Last + 1;
                           Line (Last) := '-';
                        end if;

                        GNATBIND := new String'
                          (Line (Gnatbind_Prefix_Equal'Length + 1 .. Last) &
                           "gnatbind");
                     end if;

                  elsif Last > Ada_Binder_Equal'Length
                    and then Line (1 .. Ada_Binder_Equal'Length) =
                             Ada_Binder_Equal
                  then
                     GNATBIND := new String'
                       (Line (Ada_Binder_Equal'Length + 1 .. Last));

                  --  When -O is used, instead of -O=file, -v is ignored to
                  --  avoid polluting the output. Record occurence of -v and
                  --  check the GNAT version later.

                  elsif Line (1 .. Last) = "-v" then
                     Binding_Option_Dash_V_Specified := True;

                  --  Ignore -C, as the generated sources are always in Ada

                  elsif  Line (1 .. Last) /= "-C" then
                     Binding_Options_Table.Append (Line (1 .. Last));
                  end if;

               when Project_Files =>
                  if End_Of_File (IO_File) then
                     Fail_Program
                       (null, "no time stamp for " & Line (1 .. Last));

                  else
                     declare
                        Path : constant String := Line (1 .. Last);

                     begin
                        Get_Line (IO_File, Line, Last);
                        Project_Paths.Append
                          (Path_And_Stamp'
                             (Path_Len  => Path'Length,
                              Stamp_Len => Last,
                              Path      => Path,
                              Stamp     => Line (1 .. Last)));
                     end;
                  end if;

               when Gprexch.Toolchain_Version =>
                  if End_Of_File (IO_File) then
                     Fail_Program
                       (null,
                        "no toolchain version for language "
                        & Line (1 .. Last));

                  elsif Line (1 .. Last) = "ada" then
                     Get_Line (IO_File, Line, Last);

                     if Last > 5 and then Line (1 .. 5) = GNAT_And_Space then
                        GNAT_Version         := new String'(Line (6 .. Last));
                        GNAT_Version_Set     := True;
                        GNAT_Version_First_2 :=
                          (if Last = 6 then Line (6) & ' ' else Line (6 .. 7));
                     end if;

                  else
                     Skip_Line (IO_File);
                  end if;

               when Gprexch.Delete_Temp_Files =>
                  begin
                     Delete_Temp_Files := Boolean'Value (Line (1 .. Last));

                  exception
                     when Constraint_Error =>
                        null;
                  end;

               when Gprexch.Object_File_Suffix =>
                  if End_Of_File (IO_File) then
                     Fail_Program
                       (null,
                        "no object file suffix for language "
                        & Line (1 .. Last));

                  elsif Line (1 .. Last) = "ada" then
                     Get_Line (IO_File, Line, Last);
                     Ada_Object_Suffix := new String'(Line (1 .. Last));

                  else
                     Skip_Line (IO_File);
                  end if;

               when Script_Path =>
                  Build_Script_Name := new String'(Line (1 .. Last));

               when Nothing_To_Bind        |
                    Generated_Object_File  |
                    Generated_Source_Files |
                    Bound_Object_Files     |
                    Resulting_Options      |
                    Run_Path_Option =>
                  null;
            end case;
         end if;
      end if;
   end loop;

   if Main_Base_Name = null then
      Fail_Program (null, "no main base name specified");

   else
      Binder_Generated_File :=
        new String'("b__" & Main_Base_Name.all & ".adb");
   end if;

   Close (IO_File);

   --  Modify binding option -A=<file> if <file> is not an absolute path

   if not Project_Paths.Is_Empty then
      declare
         Project_Dir : constant String :=
                         Ada.Directories.Containing_Directory
                           (Project_Paths.First_Element.Path);
      begin
         for J in 1 .. Binding_Options_Table.Last_Index loop
            if Binding_Options_Table.Element (J)'Length >= 4 and then
               Binding_Options_Table (J) (1 .. 3) = "-A="
            then
               declare
                  Value : constant String := Binding_Options_Table.Element (J);
                  File  : constant String := Value (4 .. Value'Last);
               begin
                  if not Is_Absolute_Path (File) then
                     declare
                        New_File : constant String :=
                          Normalize_Pathname
                            (File, Project_Dir,
                             Resolve_Links => False);
                     begin
                        Binding_Options_Table.Replace_Element
                          (J, "-A=" & New_File);
                     end;
                  end if;
               end;
            end if;
         end loop;
      end;
   end if;

   --  Check if GNAT version is 6.4 or higher

   if GNAT_Version_Set
     and then GNAT_Version.all /= "000"
     and then GNAT_Version_First_2 not in "3." | "5."
   then
      GNAT_6_Or_Higher := True;

      if GNAT_Version_First_2 /= "6." or else GNAT_Version.all >= "6.4" then
         GNAT_6_4_Or_Higher := True;
      end if;
   end if;

   --  Check if binding option -v was specified and issue it only if the GNAT
   --  version is 6.4 or higher, otherwise the output of gnatbind -O will be
   --  polluted.

   if Binding_Option_Dash_V_Specified and then GNAT_6_4_Or_Higher then
      Binding_Options_Table.Append ("-v");
   end if;

   if not Static_Libs then
      Gnatbind_Options.Append (Dash_Shared);
   end if;

   --  Specify the name of the generated file to gnatbind

   Gnatbind_Options.Append (Dash_o);
   Gnatbind_Options.Append (Binder_Generated_File.all);

   if Ada_Compiler_Path = null then
      Fail_Program (null, "no Ada compiler path specified");

   elsif not Is_Regular_File (Ada_Compiler_Path.all) then
      Fail_Program (null, "could not find the Ada compiler");
   end if;

   if Main_ALI /= null then
      Gnatbind_Options.Append (Main_ALI.all);
   end if;

   --  If there are Stand-Alone Libraries, invoke gnatbind with -F (generate
   --  checks of elaboration flags) to avoid multiple elaborations.

   if There_Are_Stand_Alone_Libraries
     and then GNAT_Version_Set
     and then GNAT_Version_First_2 /= "3."
   then
      Gnatbind_Options.Append ("-F");
   end if;

   Gnatbind_Options.Append_Vector (ALI_Files_Table);

   for Option of Binding_Options_Table loop
      Gnatbind_Options.Append (Option);

      if Option = Dash_OO then
         Dash_O_Specified := True;

      elsif Starts_With (Option, Dash_OO & '=') then
         Dash_O_Specified := True;
         Dash_O_File_Specified := True;
         Objects_Path := Get_Path_Name_Id (Option (4 .. Option'Last));
      end if;
   end loop;

   --  Add -x at the end, so that if -s is specified in the binding options,
   --  gnatbind does not try to look for sources, as the binder mapping file
   --  specified by -F- is not for sources, but for ALI files.

   Gnatbind_Options.Append (Dash_x);

   if Is_Absolute_Path (GNATBIND.all) then
      FULL_GNATBIND := GNATBIND;

   else
      FULL_GNATBIND :=
        new String'
              (Dir_Name (Ada_Compiler_Path.all) &
               Directory_Separator &
               GNATBIND.all);
   end if;

   if Gnatbind_Path_Specified then
      FULL_GNATBIND := Gnatbind_Path;
   end if;

   Gnatbind_Path := Locate_Exec_On_Path (FULL_GNATBIND.all);

   --  If gnatbind is not found and its full path was not specified, check for
   --  gnatbind on the path.

   if Gnatbind_Path = null and then not Gnatbind_Path_Specified then
      Gnatbind_Path := Locate_Exec_On_Path (GNATBIND.all);
   end if;

   if Gnatbind_Path = null then
      --  Make sure Namelen has a non negative value

      Name_Len := 0;

      declare
         Path_Of_Gnatbind : String_Access := GNATBIND;
      begin

         if Gnatbind_Path_Specified then
            Path_Of_Gnatbind := FULL_GNATBIND;
         end if;

         Finish_Program
           (null,
            Osint.E_Fatal,
            "could not locate " & Path_Of_Gnatbind.all);
      end;

   else
      --  Normalize the path, so that gnaampbind does not complain about not
      --  being in a "bin" directory. But don't resolve symbolic links,
      --  because in GNAT 5.01a1 and previous releases, gnatbind was a symbolic
      --  link for .gnat_wrapper.

      Gnatbind_Path :=
        new String'
          (Normalize_Pathname (Gnatbind_Path.all, Resolve_Links => False));
   end if;

   if Main_ALI = null then
      Gnatbind_Options.Append (No_Main_Option);
   end if;

   --  Add the switch -F=<mapping file> if the mapping file was specified
   --  and the version of GNAT is recent enough.

   if Mapping_File /= null
     and then GNAT_Version_Set
     and then GNAT_Version_First_2 /= "3."
   then
      Gnatbind_Options.Append (Dash_Fequal & Mapping_File.all);
   end if;

   --  Create temporary file to get the list of objects

   if not Dash_O_File_Specified then
      Tempdir.Create_Temp_File (FD_Objects, Objects_Path);
   end if;

   if GNAT_6_4_Or_Higher then
      if not Dash_O_File_Specified then
         Gnatbind_Options.Append
           (Dash_OO & "=" & Get_Name_String (Objects_Path));
         Close (FD_Objects);
      end if;

   elsif not Dash_O_Specified then
      Gnatbind_Options.Append (Dash_OO);
   end if;

   if not Quiet_Output then
      if Verbose_Low_Mode then
         Display_Last := 0;
         Add_To_Display_Line (Gnatbind_Path.all);

         for Option of Gnatbind_Options loop
            Add_To_Display_Line (Option);
         end loop;

         Put_Line (Display_Line (1 .. Display_Last));

      else
         if Main_ALI /= null then
            Display
              (Section  => GPR.Bind,
               Command  => "Ada",
               Argument => Base_Name (Main_ALI.all));

         elsif not ALI_Files_Table.Is_Empty then
            Display
              (Section  => GPR.Bind,
               Command  => "Ada",
               Argument => Base_Name (ALI_Files_Table.First_Element)
                           & " " &  No_Main_Option);
         end if;
      end if;
   end if;

   declare
      Size : Natural := 0;
      Args_List : String_List_Access;

   begin
      for Option of Gnatbind_Options loop
         Size := Size + Option'Length + 1;
      end loop;

      --  Invoke gnatbind with the arguments if the size is not too large or
      --  if the version of GNAT is not recent enough.

      Script_Write (Gnatbind_Path.all, Gnatbind_Options);

      if not GNAT_6_Or_Higher or else Size <= Maximum_Size then
         Args_List := new String_List'(To_Argument_List (Gnatbind_Options));

         if not GNAT_6_4_Or_Higher then
            Spawn
              (Gnatbind_Path.all,
               Args_List.all,
               FD_Objects,
               Return_Code,
               Err_To_Out => False);
            Success := Return_Code = 0;

         else
            Return_Code := Spawn (Gnatbind_Path.all, Args_List.all);
         end if;

         Free (Args_List);

      else
         --  Otherwise create a temporary response file

         declare
            FD            : File_Descriptor;
            Path          : Path_Name_Type;
            Args          : Argument_List (1 .. 1);
            EOL           : constant String (1 .. 1) := (1 => ASCII.LF);
            Status        : Integer;
            Quotes_Needed : Boolean;
            Last_Char     : Natural;
            Ch            : Character;

         begin
            Tempdir.Create_Temp_File (FD, Path);
            Args (1) := new String'("@" & Get_Name_String (Path));

            for Option of Gnatbind_Options loop

               --  Check if the argument should be quoted

               Quotes_Needed := False;
               Last_Char     := Option'Length;

               for J in Option'Range loop
                  Ch := Option (J);

                  if Ch = ' ' or else Ch = ASCII.HT or else Ch = '"' then
                     Quotes_Needed := True;
                     exit;
                  end if;
               end loop;

               if Quotes_Needed then
                  --  Quote the argument, doubling '"'

                  declare
                     Arg : String (1 .. Option'Length * 2 + 2);

                  begin
                     Arg (1) := '"';
                     Last_Char := 1;

                     for J in Option'Range loop
                        Ch := Option (J);
                        Last_Char := Last_Char + 1;
                        Arg (Last_Char) := Ch;

                        if Ch = '"' then
                           Last_Char := Last_Char + 1;
                           Arg (Last_Char) := '"';
                        end if;
                     end loop;

                     Last_Char := Last_Char + 1;
                     Arg (Last_Char) := '"';

                     Status := Write (FD, Arg'Address, Last_Char);
                  end;

               else
                  Status := Write
                    (FD,
                     Option (Option'First)'Address,
                     Last_Char);
               end if;

               if Status /= Last_Char then
                  Fail_Program (null, "disk full");
               end if;

               Status := Write (FD, EOL (1)'Address, 1);

               if Status /= 1 then
                  Fail_Program (null, "disk full");
               end if;
            end loop;

            Close (FD);

            --  And invoke gnatbind with this response file

            if not GNAT_6_4_Or_Higher then
               Spawn
                 (Gnatbind_Path.all,
                  Args,
                  FD_Objects,
                  Return_Code,
                  Err_To_Out => False);

            else
               Return_Code := Spawn (Gnatbind_Path.all, Args);
            end if;

            if Delete_Temp_Files then
               declare
                  Succ : Boolean;
                  pragma Warnings (Off, Succ);

               begin
                  Delete_File (Get_Name_String (Path), Succ);
               end;
            end if;
         end;
      end if;
   end;

   if not GNAT_6_4_Or_Higher and then not Dash_O_File_Specified then
      Close (FD_Objects);
   end if;

   if Return_Code /= 0 then
      if Delete_Temp_Files and not Dash_O_File_Specified then
         Delete_File (Get_Name_String (Objects_Path), Success);
      end if;

      Fail_Program (null, "invocation of gnatbind failed");
   end if;

   Compiler_Options.Append (Dash_c);
   Compiler_Options.Append (Dash_gnatA);
   Compiler_Options.Append (Dash_gnatWb);
   Compiler_Options.Append (Dash_gnatiw);
   Compiler_Options.Append (Dash_gnatws);

   --  Read the ALI file of the first ALI file. Fetch the back end switches
   --  from this ALI file and use these switches to compile the binder
   --  generated file.

   if Main_ALI /= null or else not ALI_Files_Table.Is_Empty then
      Initialize_ALI;

      declare
         F : constant File_Name_Type :=
           Get_File_Name_Id
             (if Main_ALI = null then ALI_Files_Table.First_Element
              else Main_ALI.all);
         T : Text_Buffer_Ptr;
         A : ALI_Id;

      begin
         --  Load the ALI file

         T := Osint.Read_Library_Info (F, True);

         --  Read it. Note that we ignore errors, since we only want very
         --  limited information from the ali file, and likely a slightly
         --  wrong version will be just fine, though in normal operation
         --  we don't expect this to happen.

         A := Scan_ALI
               (F,
                T,
                Ignore_ED     => False,
                Err           => False,
                Read_Lines    => "A");

         if A /= No_ALI_Id then
            for
              Index in Units.Table (ALIs.Table (A).First_Unit).First_Arg ..
                       Units.Table (ALIs.Table (A).First_Unit).Last_Arg
            loop
               --  Do not compile with the front end switches

               declare
                  Arg : String_Access renames Args.Table (Index);
                  Argv : constant String (1 .. Arg'Length) := Arg.all;
               begin
                  if (Argv'Last <= 2 or else Argv (1 .. 2) /= "-I")
                    and then
                     (Argv'Last <= 5 or else Argv (1 .. 5) /= "-gnat")
                    and then
                     (Argv'Last <= 6 or else Argv (1 .. 6) /= "--RTS=")
                  then
                     Compiler_Options.Append (Arg.all);
                  end if;
               end;
            end loop;
         end if;
      end;
   end if;

   Compiler_Options.Append (Binder_Generated_File.all);

   declare
      Object : constant String :=
                 "b__" & Main_Base_Name.all & Ada_Object_Suffix.all;
   begin
      Compiler_Options.Append (Dash_o);
      Compiler_Options.Append (Object);

      --  Add the trailing options, if any
      Compiler_Options.Append_Vector (Compiler_Trailing_Options);

      if Verbose_Low_Mode then
         Set_Name_Buffer (Ada_Compiler_Path.all);

         --  Remove the executable suffix, if present

         if Executable_Suffix'Length > 0
           and then
             Name_Len > Executable_Suffix'Length
             and then
               Name_Buffer
                 (Name_Len - Executable_Suffix'Length + 1 .. Name_Len) =
               Executable_Suffix.all
         then
            Name_Len := Name_Len - Executable_Suffix'Length;
         end if;

         Display_Last := 0;
         Add_To_Display_Line (Name_Buffer (1 .. Name_Len));

         for Option of Compiler_Options loop
            Add_To_Display_Line (Option);
         end loop;

         Put_Line (Display_Line (1 .. Display_Last));
      end if;

      Spawn_And_Script_Write
        (Ada_Compiler_Path.all,
         Compiler_Options,
         Success);

      if not Success then
         Fail_Program (null, "compilation of binder generated file failed");
      end if;

      Create (IO_File, Out_File, Exchange_File_Name.all);

      --  First, the generated object file

      Put_Line (IO_File, Binding_Label (Generated_Object_File));
      Put_Line (IO_File, Object);

      --  Repeat the project paths with their time stamps

      Put_Line (IO_File, Binding_Label (Project_Files));

      for PS of Project_Paths loop
         Put_Line (IO_File, PS.Path);
         Put_Line (IO_File, PS.Stamp);
      end loop;

      --  Get the bound object files from the Object file

      Open (Objects_File, In_File, Get_Name_String (Objects_Path));

      Put_Line (IO_File, Binding_Label (Bound_Object_Files));

      while not End_Of_File (Objects_File) loop
         Get_Line (Objects_File, Line, Last);

         --  Only put in the exchange file the path of the object files.
         --  Output anything else on standard output.

         if Is_Regular_File (Line (1 .. Last)) then
            Put_Line (IO_File, Line (1 .. Last));

            Bound_Files := new Bound_File'
              (Name => new String'(Line (1 .. Last)), Next => Bound_Files);

            if Dash_O_Specified and then not Dash_O_File_Specified then
               Put_Line (Line (1 .. Last));
            end if;

         elsif not Dash_O_File_Specified then
            Put_Line (Line (1 .. Last));
         end if;
      end loop;

      Close (Objects_File);

      if Delete_Temp_Files and then not Dash_O_File_Specified then
         Delete_File (Get_Name_String (Objects_Path), Success);
      end if;

      --  For the benefit of gprclean, the generated files other than the
      --  generated object file.

      Put_Line (IO_File, Binding_Label (Generated_Source_Files));
      Put_Line (IO_File, "b__" & Main_Base_Name.all & ".ads");
      Put_Line (IO_File, Binder_Generated_File.all);
      Put_Line (IO_File, "b__" & Main_Base_Name.all & ".ali");

      --  Get the options from the binder generated file

      Open (BG_File, In_File, Binder_Generated_File.all);

      while not End_Of_File (BG_File) loop
         Get_Line (BG_File, Line, Last);
         exit when Line (1 .. Last) = Begin_Info;
      end loop;

      if not End_Of_File (BG_File) then
         Put_Line (IO_File, Binding_Label (Resulting_Options));

         All_Binding_Options := False;
         Xlinker_Seen        := False;
         Stack_Equal_Seen    := False;
         loop
            Get_Line (BG_File, Line, Last);
            exit when Line (1 .. Last) = End_Info;
            Line (1 .. Last - 8) := Line (9 .. Last);
            Last := Last - 8;

            if Line (1) = '-' then
               --  After the first switch, we take all options, because some
               --  of the options specified in pragma Linker_Options may not
               --  start with '-'.
               All_Binding_Options := True;
            end if;

            Get_Option :=
              All_Binding_Options
              or else
              Base_Name (Line (1 .. Last)) in "g-trasym.o" | "g-trasym.obj";
            --  g-trasym is a special case as it is not included in libgnat

            --  Avoid duplication of object file

            if Get_Option then
               declare
                  BF : Bound_File_Access := Bound_Files;

               begin
                  while BF /= null loop
                     if BF.Name.all = Line (1 .. Last) then
                        Get_Option := False;
                        exit;

                     else
                        BF := BF.Next;
                     end if;
                  end loop;
               end;
            end if;

            if Get_Option then
               if Line (1 .. Last) = "-Xlinker" then
                  Xlinker_Seen := True;

               elsif Xlinker_Seen then
                  Xlinker_Seen := False;

                  --  Make sure that only the first switch --stack= is put in
                  --  the exchange file.

                  if Last > 8 and then Line (1 .. 8) = "--stack=" then
                     if not Stack_Equal_Seen then
                        Stack_Equal_Seen := True;
                        Put_Line (IO_File, "-Xlinker");
                        Put_Line (IO_File, Line (1 .. Last));
                     end if;

                  else
                     Put_Line (IO_File, "-Xlinker");
                     Put_Line (IO_File, Line (1 .. Last));
                  end if;

               elsif Last > 12 and then Line (1 .. 12) = "-Wl,--stack=" then
                  if not Stack_Equal_Seen then
                     Stack_Equal_Seen := True;
                     Put_Line (IO_File, Line (1 .. Last));
                  end if;

               elsif Last >= 3 and then Line (1 .. 2) = "-L" then
                  --  Set Adalib_Dir only if libgnat is found inside.
                  if Is_Regular_File
                    (Line (3 .. Last) & Directory_Separator & "libgnat.a")
                  then
                     Adalib_Dir := new String'(Line (3 .. Last));

                     if Verbose_Higher_Mode then
                        Put_Line ("Adalib_Dir = """ & Adalib_Dir.all & '"');
                     end if;

                     --  Build the Prefix_Path, where to look for some
                     --  archives: libaddr2line.a, libbfd.a, libgnatmon.a,
                     --  libgnalasup.a and libiberty.a. It contains three
                     --  directories: $(adalib)/.., $(adalib)/../.. and the
                     --  subdirectory "lib" ancestor of $(adalib).

                     declare
                        Dir_Last       : Positive;
                        Prev_Dir_Last  : Positive;
                        First          : Positive;
                        Prev_Dir_First : Positive;
                        Nmb            : Natural;
                     begin
                        Set_Name_Buffer (Line (3 .. Last));

                        while Name_Buffer (Name_Len) = Directory_Separator
                          or else Name_Buffer (Name_Len) = '/'
                        loop
                           Name_Len := Name_Len - 1;
                        end loop;

                        while Name_Buffer (Name_Len) /= Directory_Separator
                          and then Name_Buffer (Name_Len) /= '/'
                        loop
                           Name_Len := Name_Len - 1;
                        end loop;

                        while Name_Buffer (Name_Len) = Directory_Separator
                          or else Name_Buffer (Name_Len) = '/'
                        loop
                           Name_Len := Name_Len - 1;
                        end loop;

                        Dir_Last := Name_Len;
                        Nmb := 0;

                        Dir_Loop : loop
                           Prev_Dir_Last := Dir_Last;
                           First := Dir_Last - 1;
                           while First > 3
                             and then
                              Name_Buffer (First) /= Directory_Separator
                             and then
                              Name_Buffer (First) /= '/'
                           loop
                              First := First - 1;
                           end loop;

                           Prev_Dir_First := First + 1;

                           exit Dir_Loop when First <= 3;

                           Dir_Last := First - 1;
                           while Name_Buffer (Dir_Last) = Directory_Separator
                             or else Name_Buffer (Dir_Last) = '/'
                           loop
                              Dir_Last := Dir_Last - 1;
                           end loop;

                           Nmb := Nmb + 1;

                           if Nmb <= 1 then
                              Add_Char_To_Name_Buffer (Path_Separator);
                              Add_Str_To_Name_Buffer
                                (Name_Buffer (1 .. Dir_Last));

                           elsif Name_Buffer (Prev_Dir_First .. Prev_Dir_Last)
                             = "lib"
                           then
                              Add_Char_To_Name_Buffer (Path_Separator);
                              Add_Str_To_Name_Buffer
                                (Name_Buffer (1 .. Prev_Dir_Last));
                              exit Dir_Loop;
                           end if;
                        end loop Dir_Loop;

                        Prefix_Path :=
                          new String'(Name_Buffer (1 .. Name_Len));

                        if Verbose_Higher_Mode then
                           Put_Line
                             ("Prefix_Path = """ & Prefix_Path.all & '"');
                        end if;
                     end;
                  end if;
                  Put_Line (IO_File, Line (1 .. Last));

               elsif Line (1 .. Last) in Static_Libgcc | Shared_Libgcc then
                  Put_Line (IO_File, Line (1 .. Last));

                  --  For a number of archives, we need to indicate the full
                  --  path of the archive, if we find it, to be sure that the
                  --  correct archive is used by the linker.

               elsif Line (1 .. Last) = Dash_Lgnat then
                  if Adalib_Dir = null then
                     if Verbose_Higher_Mode then
                        Put_Line ("No Adalib_Dir");
                     end if;

                     Put_Line (IO_File, Dash_Lgnat);

                  elsif Static_Libs then
                     Put_Line (IO_File, Adalib_Dir.all & "libgnat.a");

                  else
                     Put_Line (IO_File, Dash_Lgnat);
                  end if;

               elsif Line (1 .. Last) = Dash_Lgnarl
                 and then Static_Libs
                 and then Adalib_Dir /= null
               then
                  Put_Line (IO_File, Adalib_Dir.all & "libgnarl.a");

               elsif Line (1 .. Last) = "-laddr2line"
                 and then Prefix_Path /= null
               then
                  Output_Lib_Path_Or_Line ("libaddr2line.a");

               elsif Line (1 .. Last) = "-lbfd"
                 and then Prefix_Path /= null
               then
                  Output_Lib_Path_Or_Line ("libbfd.a");

               elsif Line (1 .. Last) = "-lgnalasup"
                 and then Prefix_Path /= null
               then
                  Output_Lib_Path_Or_Line ("libgnalasup.a");

               elsif Line (1 .. Last) = "-lgnatmon"
                 and then Prefix_Path /= null
               then
                  Output_Lib_Path_Or_Line ("libgnatmon.a");

               elsif Line (1 .. Last) = "-liberty"
                 and then Prefix_Path /= null
               then
                  Output_Lib_Path_Or_Line ("libiberty.a");

               else
                  Put_Line (IO_File, Line (1 .. Last));
               end if;
            end if;
         end loop;
      end if;

      Close (BG_File);

      if not Static_Libs
        and then Adalib_Dir /= null
      then
         Put_Line (IO_File, Binding_Label (Run_Path_Option));
         Put_Line (IO_File, Adalib_Dir.all);
         Name_Len := Adalib_Dir'Length;
         Name_Buffer (1 .. Name_Len) := Adalib_Dir.all;

         for J in reverse 2 .. Name_Len - 4 loop
            if Name_Buffer (J) = Directory_Separator and then
              Name_Buffer (J + 4) = Directory_Separator and then
              Name_Buffer (J + 1 .. J + 3) = "lib"
            then
               Name_Len := J + 3;
               Put_Line (IO_File, Name_Buffer (1 .. Name_Len));
               exit;
            end if;
         end loop;
      end if;

      Close (IO_File);
   end;
end Gprbind;
