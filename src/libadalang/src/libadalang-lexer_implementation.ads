--
--  Copyright (C) 2014-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with System;

with GNATCOLL.VFS;

with Langkit_Support.Diagnostics; use Langkit_Support.Diagnostics;
with Langkit_Support.Symbols;     use Langkit_Support.Symbols;

with Langkit_Support.Token_Data_Handlers;
use Langkit_Support.Token_Data_Handlers;

with Libadalang.Common; use Libadalang.Common;
limited with Libadalang.Implementation;

private package Libadalang.Lexer_Implementation is

   type Internal_Lexer_Input (Kind : Lexer_Input_Kind) is record
      case Kind is
      when File | Bytes_Buffer =>
         Charset  : Unbounded_String;
         Read_BOM : Boolean;

         case Kind is
            when File =>
               Filename : GNATCOLL.VFS.Virtual_File;
            when Bytes_Buffer =>
               Bytes       : System.Address;
               Bytes_Count : Natural;
            when others =>
               null;
         end case;

      when Text_Buffer =>
         Text       : System.Address;
         Text_Count : Natural;
      end case;
   end record;
   --  See Libadalang.Lexer.Lexer_Input for details. Resources pointed by
   --  access types must be free'd by Extract_Tokens's caller when done with
   --  it.

   procedure Extract_Tokens
     (Input       : Internal_Lexer_Input;
      With_Trivia : Boolean;
      File_Reader : access Implementation.Internal_File_Reader'Class;
      TDH         : in out Token_Data_Handler;
      Diagnostics : in out Diagnostics_Vectors.Vector);
   --  Implementation for Libadalang.Lexer.Extract_Tokens

   function Get_Symbol
     (Token : Token_Or_Trivia_Index;
      TDH   : Token_Data_Handler) return Symbol_Type;
   --  Assuming that ``Token`` refers to a token that contains a symbol, return
   --  the corresponding symbol.

end Libadalang.Lexer_Implementation;
