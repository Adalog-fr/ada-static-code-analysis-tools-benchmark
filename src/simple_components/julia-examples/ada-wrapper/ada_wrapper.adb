--                                                                    --
--  procedure Ada_Wrapper           Copyright (c)  Dmitry A. Kazakov  --
--                                                 Luebeck            --
--                                                 Winter, 2019       --
--                                                                    --
--                                Last revision :  20:47 23 Jun 2010  --
--                                                                    --
--  This  library  is  free software; you can redistribute it and/or  --
--  modify it under the terms of the GNU General Public  License  as  --
--  published by the Free Software Foundation; either version  2  of  --
--  the License, or (at your option) any later version. This library  --
--  is distributed in the hope that it will be useful,  but  WITHOUT  --
--  ANY   WARRANTY;   without   even   the   implied   warranty   of  --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU  --
--  General  Public  License  for  more  details.  You  should  have  --
--  received  a  copy  of  the GNU General Public License along with  --
--  this library; if not, write to  the  Free  Software  Foundation,  --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.    --
--                                                                    --
--  As a special exception, if other files instantiate generics from  --
--  this unit, or you link this unit with other files to produce  an  --
--  executable, this unit does not by  itself  cause  the  resulting  --
--  executable to be covered by the GNU General Public License. This  --
--  exception  does not however invalidate any other reasons why the  --
--  executable file might be covered by the GNU Public License.       --
--____________________________________________________________________--

with Ada.Calendar;           use Ada.Calendar;
with Ada.Text_IO;            use Ada.Text_IO;
with Interfaces.C;           use Interfaces.C;
with Julia;                  use Julia;
with Strings_Edit.ISO_8601;  use Strings_Edit.ISO_8601;

procedure Ada_Wrapper is
   Bin : constant String := "D:\Julia-1.6.3\bin";
begin
   Load (Bin & "\libjulia.dll");  -- Load library
   Init;
-- Init_With_Image (Bin);         -- Initialize environment

   declare
      function Increment (X : Double) return Double;
      pragma Convention (C, Increment);

      function Increment (X : Double) return Double is
      begin
         return X + 1.0;
      end Increment;
   begin
      Eval_String
      (  "function Increment(x::Cdouble); ccall("
      &  CCall_Address (Increment'Address)
      &  ",Cdouble,(Cdouble,),x); end"
      );

      Eval_String ("x = 1.0");
      Eval_String ("x = Increment(x)");
      Eval_String ("x = Increment(x)");
      Eval_String ("x = Increment(x)");
      Eval_String ("println(x)");
   end;

   declare
      Ada_Time       : Time := Clock;
      Ada_Day_Time   : Day_Duration;
      Julia_Time     : value_t;
      Julia_Day_Time : value_t;
   begin
      Put_Line ("Clock:" & Image (Ada_Time));
      Julia_Time := To_Julia (Ada_Time);
      Ada_Time   := Value (Julia_Time);
      Put_Line ("     :" & Image (Ada_Time));

      Julia_Day_Time := To_Julia (Day_Duration'(600.020513));
      Ada_Day_Time   := Value (Julia_Day_Time);
      Put_Line ("Time:" & Day_Duration'Image (Ada_Day_Time));

   end;
   AtExit_Hook;                   -- Finalize environment
end Ada_Wrapper;
