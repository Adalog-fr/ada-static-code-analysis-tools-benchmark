---------------------------------------------------------------- {{{ ----------
--  Copyright © 2020 … 2022 Martin Krischik «krischik@users.sourceforge.net»
-------------------------------------------------------------------------------
--  This library is free software; you can redistribute it and/or modify it
--  under the terms of the GNU Library General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or (at your
--  option) any later version.
--
--  This library is distributed in the hope that it will be useful, but WITHOUT
--  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public
--  License for more details.
--
--  You should have received a copy of the GNU Library General Public License
--  along with this library; if not, write to the Free Software Foundation,
--  Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
---------------------------------------------------------------- }}} ----------

pragma License (Modified_Gpl);
pragma Ada_2022;

with Ada.Command_Line;
with Ada.IO_Exceptions;
with Ada.Exceptions;
with Ada.Sequential_IO;
with Ada.Text_IO;
with Interfaces;

procedure Create_LED is
   type Byte    is new Interfaces.Unsigned_8;
   type Address is new Interfaces.Unsigned_16;

   --  One of the cool feature of Ada is it's ability to set the lower and
   --  upper bound of an array to any value you want. So we can set use the
   --  actual ROM addresses as array index an save ourself the work and
   --  remove one potential error source.
   --
   type ROM_Type is array (Address range 16#8000# .. 16#FFFF#) of Byte;

   package ROM_IO is new Ada.Sequential_IO (ROM_Type);

   ROM_Output : ROM_IO.File_Type;

   --  65C02 related constants as named in the original
   --  WDC design document
   --
   NOP  : constant Byte    := 16#EA#;
   NMIB : constant Address := 16#FFFA#;
   RESB : constant Address := 16#FFFC#;
   IRQB : constant Address := 16#FFFE#;

   --  Another interesting feature Ada arrays is specifying the index of
   --  elements when initialising the array. As well specifying an value
   --  for all not explicitly initialised values. Makes setting up the
   --  ROM array super easy.
   --
   ROM : constant ROM_Type := [
       16#8000# => 16#A9#,  --  LDA #$FF
       16#8001# => 16#FF#,
       16#8002# => 16#8D#,  --  STA $7F02
       16#8003# => 16#02#,
       16#8004# => 16#7F#,
       16#8005# => 16#A9#,  --  LDA #$55
       16#8006# => 16#55#,
       16#8007# => 16#8D#,  --  STA $7F00
       16#8008# => 16#00#,
       16#8009# => 16#7F#,
       16#800A# => 16#A9#,  --  LDA #$AA
       16#800B# => 16#AA#,
       16#800C# => 16#8D#,  --  STA $7F00
       16#800D# => 16#00#,
       16#800E# => 16#7F#,
       16#800F# => 16#4C#,  --  JMP $8005
       16#8010# => 16#05#,
       16#8011# => 16#80#,
       NMIB + 0 => 16#00#,  --  .word $8000
       NMIB + 1 => 16#80#,
       RESB + 0 => 16#00#,  --  .word $8000
       RESB + 1 => 16#80#,
       IRQB + 0 => 16#00#,  --  .word $8000
       IRQB + 1 => 16#80#,
       others   => NOP];

   File_Name : constant String := (
      if Ada.Command_Line.Argument_Count < 1 then
         "create_led.rom"
      else
         Ada.Command_Line.Argument (1));
begin
   --  Just like Python Ada can write the whole array in one go.
   --
   ROM_IO.Create (ROM_Output, ROM_IO.Out_File, File_Name);
   ROM_IO.Write (ROM_Output, ROM);
   ROM_IO.Close (ROM_Output);
exception
   when Error : Ada.IO_Exceptions.Name_Error =>
      Ada.Text_IO.Put_Line ("File “" & File_Name & "” couldn't be created.");
      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (Error));
   when Error : others =>
      Ada.Text_IO.Put_Line ("File “" & File_Name & "” couldn't be written.");
      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (Error));
end Create_LED;

---------------------------------------------------------------- {{{ ----------
--  vim: set textwidth=0 nowrap tabstop=8 shiftwidth=3 softtabstop=3 expandtab :
--  vim: set filetype=ada fileencoding=utf-8 fileformat=unix foldmethod=marker :
--  vim: set spell spelllang=en_gb :
