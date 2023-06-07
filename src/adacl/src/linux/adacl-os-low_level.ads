--------------------------------------------------------------- {{{1 ----------
--  Copyright © 2003 … 2022 Martin Krischik
------------------------------------------------------------------------------
--  This library is free software; you can redistribute it and/or modify it
--  under the terms of the GNU Library General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or (at your
--  option) any later version.
--
--  This library is distributed in the hope that it will be useful, but
--  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
--  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public
--  License for more details.
--
--  You should have received a copy of the GNU Library General Public License
--  along with this library; if not, write to the Free Software Foundation,
--  Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
--------------------------------------------------------------- }}}1 ----------

pragma License (Modified_Gpl);

with Ada.Unchecked_Conversion;
with GNAT.OS_Lib;
with Interfaces.C;
with System;

package AdaCL.OS.Low_Level is
   --
   --  Operating System Tools. Start extrnal Programm.
   --

   --  I/O Pipe
   type Pipe_Type is record
      Input  : GNAT.OS_Lib.File_Descriptor;
      Output : GNAT.OS_Lib.File_Descriptor;
   end record;

   --
   --  All operating systems known to AdaCL.
   --
   type Known_OS is (Windows, MacOS, Linux);

   --
   --  The Operating System under which this programs runs.
   --
   This_OS : constant Known_OS;

   --
   --  Error number of last C function called.
   --
   function Errno return Integer renames GNAT.OS_Lib.Errno;

   --
   --  Close a file given its file descriptor.
   --
   procedure Close (Fd : GNAT.OS_Lib.File_Descriptor);

   --
   --  Create pipe
   --
   function Pipe (
      Files : access Pipe_Type)
      return Interfaces.C.int;

   --
   --  Create pipe - GNAT Version.
   --
   function Create_Pipe (
      Pipe : access Pipe_Type)
      return Integer;

   --
   --  Create pipe - GNAT Version.
   --
   function Portable_Wait (
      S : System.Address)
      return GNAT.OS_Lib.Process_Id;

   --
   --  Cast process id to integer.
   --
   function Cast
   is new
      Ada.Unchecked_Conversion (
         Source => GNAT.OS_Lib.Process_Id,
         Target => Integer);
   --
   --  Cast process id to integer.
   --
   function Cast
   is new
      Ada.Unchecked_Conversion (
         Source => GNAT.OS_Lib.File_Descriptor,
         Target => Integer);

private

   This_OS : constant Known_OS := Linux;

   pragma Import (
      Convention  => C,
      Entity      => Close);

   pragma Import (
      Convention     => C,
      Entity         => Pipe,
      External_Name  => "pipe");

   pragma Import (
      Convention     => C,
      Entity         => Portable_Wait,
      External_Name  => "__gnat_portable_wait");

   pragma Import (
      Convention     => C,
      Entity         => Create_Pipe,
      External_Name  => "__gnat_pipe");

   pragma Linker_Options ("-lgnat");

end AdaCL.OS.Low_Level;
