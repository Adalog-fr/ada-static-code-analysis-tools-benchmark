--  MIT License
--  
--  Copyright (c) 2021 My Ada library
--  
--  Permission is hereby granted, free of charge, to any person obtaining a copy
--  of this software and associated documentation files (the "Software"), to deal
--  in the Software without restriction, including without limitation the rights
--  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
--  copies of the Software, and to permit persons to whom the Software is
--  furnished to do so, subject to the following conditions:
--  
--  The above copyright notice and this permission notice shall be included in all
--  copies or substantial portions of the Software.
--  
--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
--  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
--  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
--  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
--  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
--  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
--  SOFTWARE.

--
-----------------------------------------------------------------------
--
--  This package hides the low-level details of putting the terminal
--  in "password mode," e.g., disabling echo and buffering and maybe
--  other stuff.
--
--  In order to port this library to another system it suffices to change
--  the body of this package and the package Terminal_Modes that define
--  the type Terminal_Mode used by Set_Password_Mode and Restore_Mode. 
--  
-----------------------------------------------------------------------
--

with Interfaces.C.Strings;
with Get_Password.Terminal_Modes;


private package Get_Password.Terminal_Control is
   pragma SPARK_Mode;
   
   --
   --  Return true if the standard input is connected to a "terminal"
   --  (that is not a file nor a pipe)
   --
   function Is_A_Tty return Boolean;
   
   --
   --  Set the terminal in "password mode," e.g., avoid bufferization
   --  turn off echo, and so on... Return a "mode" value that will
   --  be used to restore the original terminal mode
   --
   function Set_Password_Mode return Terminal_Modes.Terminal_Mode
     with
       Pre => Is_A_Tty,
       Global => null;
   
   --
   --  Restore the original terminal mode using the value returned
   --  by Set_Password_Mode
   --
   procedure Restore_Mode (X : Terminal_Modes.Terminal_Mode)
     with
       Pre => Is_A_Tty,
       Global => null;
end Get_Password.Terminal_Control;
