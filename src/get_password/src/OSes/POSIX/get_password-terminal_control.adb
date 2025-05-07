pragma Ada_2012;
package body Get_Password.Terminal_Control is
   pragma SPARK_Mode;

   package ICS renames Interfaces.C.Strings;

   use type Interfaces.C.int;

   function C_Set_Mode return ICS.Chars_Ptr
     with
       Import => True,
       Convention => C,
       External_Name => "disable_echo",
       Global => null;

   procedure C_Restore_Mode (X : ICS.Chars_Ptr)
     with
       Import => True,
       Convention => C,
       External_Name => "restore_echo",
       Global => null;

   function C_Is_Atty return Interfaces.C.Int
     with
       Import => True,
       Convention => C,
       External_Name => "is_stdin_a_tty",
       Global => null;

   --------------
   -- Is_A_Tty --
   --------------

   function Is_A_Tty return Boolean
   is (C_Is_Atty = 1);

   -----------------------
   -- Set_Password_Mode --
   -----------------------

   function Set_Password_Mode return Terminal_Modes.Terminal_Mode is
   begin
      if not Is_A_Tty then
         raise Constraint_Error;
      end if;

      return Terminal_Modes.Terminal_Mode (C_Set_Mode);
   end Set_Password_Mode;

   ------------------
   -- Restore_Mode --
   ------------------

   procedure Restore_Mode (X : Terminal_Modes.Terminal_Mode) is
   begin
      if not Is_A_Tty then
         raise Constraint_Error;
      end if;
      C_Restore_Mode (ICS.Chars_Ptr (X));
   end Restore_Mode;

end Get_Password.Terminal_Control;
