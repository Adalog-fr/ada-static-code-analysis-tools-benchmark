with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
--
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

package Get_Password is
   pragma SPARK_Mode;

   function Is_A_Tty return Boolean;
   
   type Error_Code is (Success, Cancelled, Not_A_Tty);
   
   type End_Condition is (Buffer_Full, End_Of_Line, Both);

   --
   --  Read from terminal a string of at most Buffer'Length size, without
   --  echoing the characters, but using '*'.  If Allow_Abort is true and 
   --  the user presses Esc, the input is terminated and Aborted is set 
   --  to true
   --
   procedure Get (Buffer      : out String;
                  Last        : out Natural;
                  Err_Code    : out Error_Code;
                  Marker      : String := "*";
                  End_On      : End_Condition := Both;
                  Allow_Abort : Boolean := True)
     with
       Pre =>
         Buffer'Length > 0
         and Buffer'Last < Positive'Last
         and Is_A_Tty,
         Post =>
           Err_Code /= Not_A_Tty
           and then (if Err_Code /= Success then Last = Buffer'First - 1);
          
   
   --
   --  Like the other Get, but Allow_Abort is set to true and the empty string 
   --  is returned if the user presses Esc
   --
   procedure Get (Buffer      : out String;
                  Last        : out Natural;
                  Marker      : String := "*";
                  End_On      : End_Condition := Both)
     with
       Pre =>
         Buffer'Length > 0
         and Buffer'Last < Positive'Last
         and Is_A_Tty;
   
   --
   -- The same Get but with unbounded strings
   --
   procedure Get (Buffer      : out Unbounded_String;
                  Err_Code    : out Error_Code;
                  Marker      : String := "*";
                  End_On      : End_Condition := Both;
                  Allow_Abort : Boolean := True)
     with
       Pre => Is_A_Tty,
       Post =>
         Err_Code /= Not_A_Tty
         and then (if Err_Code /= Success then Buffer = Null_Unbounded_String);
          
   
   --
   --  Like the other Get, but Allow_Abort is set to true and the empty string 
   --  is returned if the user presses Esc
   --
   procedure Get (Buffer      : out Unbounded_String;
                  Marker      : String := "*";
                  End_On      : End_Condition := Both)
     with
       Pre => Is_A_Tty;
   
   Not_A_Tty_Error : exception;
   
private
   type Abstract_Password_Buffer is interface;
   
   function Length (Buffer : Abstract_Password_Buffer) return Natural
                    is abstract
     with
       Post'Class => Length'Result < Natural'Last;
   
   function Is_Empty (Buffer : Abstract_Password_Buffer) return Boolean
                      is abstract
     with 
       Post'Class => Is_Empty'Result = (Buffer.Length = 0);
   
   function Is_Full (Buffer : Abstract_Password_Buffer) return Boolean
                     is abstract;
   
   procedure Clean (Buffer : out Abstract_Password_Buffer)
   is abstract
     with
       Post'Class => Buffer.Is_Empty and not Buffer.Is_Full;
   
   
   procedure Append (Buffer : in out Abstract_Password_Buffer;
                     Item   : Character)
   is abstract
     with 
       Pre'Class => not Buffer.Is_Full,
         Post'Class => Buffer.Length - 1 = Buffer'Old.Length;
   
   procedure Delete_Last (Buffer : in out Abstract_Password_Buffer)
   is abstract
     with 
       Pre'Class => not Buffer.Is_Empty,
         Post'Class => Buffer.Length = Buffer.Length'Old - 1;
   
   
end Get_Password;
