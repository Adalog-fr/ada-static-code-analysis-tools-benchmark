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
pragma Ada_2012;
with Ada.Strings.Fixed;
with Ada.Text_IO;

with Interfaces.C.Strings;
with Get_Password.Terminal_Control;
with Get_Password.Terminal_Modes;
with Get_Password.Fixed_Password_Buffers;
with Get_Password.Unbounded_Password_Buffers;

package body Get_Password is
   pragma SPARK_Mode;

   
   procedure Basic_Get 
     (Buffer      : out Abstract_Password_Buffer'Class;
      Err_Code    : out Error_Code;
      Marker      : String := "*";
      End_On      : End_Condition := Both;
      Allow_Abort : Boolean := True); 
   
   function Is_A_Tty return Boolean
   is (Terminal_Control.Is_A_Tty);
   
   
   ---------
   -- Get --
   ---------

   procedure Basic_Get 
     (Buffer      : out Abstract_Password_Buffer'Class;
      Err_Code    : out Error_Code;
      Marker      : String := "*";
      End_On      : End_Condition := Both;
      Allow_Abort : Boolean := True) 
   is
      use Ada.Strings.Fixed;
      use Ada.Text_IO;

      subtype Lower_Graphic is
        Character range Character'Val (32) .. Character'Val (126);

      subtype Upper_Graphic is
        Character range Character'Val (160) .. Character'Val (255);


      C      : Character;

      Back   : constant String := Marker'Length * ASCII.BS;
      Eraser : constant String := Marker'Length * ' ';
      
      procedure Step_Back is
      begin
         Put (Back);
         Put (Eraser);
         Put (Back);
      end Step_Back;
      
      
      procedure Delete_All (Buffer : in out  Abstract_Password_Buffer'Class)
        with
          Post => Buffer.Length = 0;
      
      procedure Delete_All (Buffer : in out Abstract_Password_Buffer'Class) is
         Len : constant Natural := Buffer.Length with Ghost;
      begin 
         pragma Assert (Buffer.Length = Len);
         for I in 1 .. Buffer.Length loop
            pragma Loop_Invariant (Buffer.Length = Len - I + 1);
            
            Buffer.Delete_Last;
            
            --  Put_Line (I'Image & ", " & Buffer.Length'Image);
            
            Step_Back;
         end loop;
      end Delete_All;
   begin
      Buffer.Clean;
      
      if not Is_A_Tty then 
         Err_Code := Not_A_Tty;
         return;
      end if;
      
      declare
         use Terminal_Control;

         Saved_Status : constant Terminal_Modes.Terminal_Mode :=
                          Terminal_Control.Set_Password_Mode;
      begin
         Err_Code := Success; -- Let's be optimistic
         
         pragma Assert (Buffer.Is_Empty);
         
         loop
            exit when 
              Buffer.Is_Full  
              and (End_On = Both or End_On = Buffer_Full);

            Get (C);
 
            case C is
               when ASCII.Bs | ASCII.DEL =>
                  if not Buffer.Is_Empty then
                     Step_Back;
                     Buffer.Delete_Last;
                  end if;
                  
               when ASCII.CR | ASCII.LF =>
                  if End_On = Both or End_On = End_Of_Line then
                     exit;
                  end if;
                  
               when ASCII.NAK => -- Ctrl-U
                  Delete_All (Buffer);
                  
               when ASCII.ESC =>
                  if Allow_Abort then
                     Err_Code := Cancelled;
                     exit;
                  end if;

               when Lower_Graphic | Upper_Graphic =>
                  if not Buffer.Is_Full then
                     Buffer.Append (C);
                     Put (Marker);
                  end if;

               when others =>
                  null;

            end case;
         end loop;
         
         Terminal_Control.Restore_Mode (Saved_Status);

         pragma Assert (Err_Code = Success or Err_Code = Cancelled);
         
         if Err_Code /= Success then
            Buffer.Clean;
         end if;

         --  pragma Assert (Last >= Buffer'First - 1 and Last <= Buffer'Last);
      end;
   end Basic_Get;
   
   ---------
   -- Get --
   ---------

   procedure Get (Buffer      : out String;
                  Last        : out Natural;
                  Err_Code    : out Error_Code;
                  Marker      : String := "*";
                  End_On      : End_Condition := Both;
                  Allow_Abort : Boolean := True)
   is 
      pragma SPARK_Mode (Off);
      
      use Fixed_Password_Buffers;
      
      B : Fixed_Password_Buffer (Buffer'Length);
   begin
      Basic_Get (Buffer      => B,
                 Err_Code    => Err_Code,
                 Marker      => Marker,
                 End_On      => End_On,
                 Allow_Abort => Allow_Abort);
      
      if Err_Code = Success then 
         Buffer := B.Data;
         Last := B.Cursor - 1;
      else
         Buffer := (others => ASCII.NUL);
         Last := Buffer'First - 1;
      end if;
   end Get;
 
   procedure Get (Buffer      : out Unbounded_String;
                  Err_Code    : out Error_Code;
                  Marker      : String := "*";
                  End_On      : End_Condition := Both;
                  Allow_Abort : Boolean := True)
   is
      pragma SPARK_Mode (Off);
      
      use Unbounded_Password_Buffers;
      
      B : Unbounded_Password_Buffer;
   begin
      Basic_Get (Buffer      => B,
                 Err_Code    => Err_Code,
                 Marker      => Marker,
                 End_On      => End_On,
                 Allow_Abort => Allow_Abort);
      
      if Err_Code = Success then 
         Buffer := B.Data;
      else
         Buffer := Null_Unbounded_String;
      end if;
   end Get;
   ---------
   -- Get --
   ---------

   procedure Get (Buffer      : out String;
                  Last        : out Natural;
                  Marker      : String := "*";
                  End_On      : End_Condition := Both)
   is
      Err_Code : Error_Code;
   begin
      Get (Buffer      => Buffer,
           Last        => Last,
           Err_Code    => Err_Code,
           Marker      => Marker,
           End_On      => End_On,
           Allow_Abort => True);
      
      case Err_Code is
         when Success => 
            null;
            
         when Cancelled =>
            Last := Buffer'First - 1;
            
         when Not_A_Tty =>
            raise Not_A_Tty_Error;
      end case;
   end Get;
   
   ---------
   -- Get --
   ---------

   procedure Get (Buffer      : out Unbounded_String;
                  Marker      : String := "*";
                  End_On      : End_Condition := Both) 
   is
      Err_Code : Error_Code;
   begin
      Get (Buffer      => Buffer,
           Err_Code    => Err_Code,
           Marker      => Marker,
           End_On      => End_On,
           Allow_Abort => True);
      
      case Err_Code is
         when Success => 
            null;
            
         when Cancelled =>
            Buffer := Null_Unbounded_String;
            
         when Not_A_Tty =>
            raise Not_A_Tty_Error;
      end case;
   end Get;
 

end Get_Password;
