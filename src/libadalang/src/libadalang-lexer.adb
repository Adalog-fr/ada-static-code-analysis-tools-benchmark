--
--  Copyright (C) 2014-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

pragma Warnings (Off, "is an internal GNAT unit");
with Ada.Strings.Unbounded.Aux; use Ada.Strings.Unbounded.Aux;
with Ada.Strings.Wide_Wide_Unbounded.Aux;
use Ada.Strings.Wide_Wide_Unbounded.Aux;
pragma Warnings (On, "is an internal GNAT unit");

with Libadalang.Lexer_Implementation;
use Libadalang.Lexer_Implementation;

pragma Warnings (Off, "referenced");
with Langkit_Support.Symbols;
pragma Warnings (On, "referenced");


          with Libadalang.Implementation;


package body Libadalang.Lexer is

   --------------------
   -- Extract_Tokens --
   --------------------

   procedure Extract_Tokens
     (Input       : Lexer_Input;
      With_Trivia : Boolean;
      TDH         : in out Token_Data_Handler;
      Diagnostics : in out Diagnostics_Vectors.Vector)
   is
      Internal_Input : Internal_Lexer_Input (Input.Kind);
   begin
      case Input.Kind is
         when File | Bytes_Buffer =>
            Internal_Input.Charset := Input.Charset;
            Internal_Input.Read_BOM := Input.Read_BOM;

            case Input.Kind is
               when File =>
                  Internal_Input.Filename := Input.Filename;
               when Bytes_Buffer =>
                  declare
                     Bytes : Big_String_Access;
                  begin
                     Get_String
                       (Input.Bytes, Bytes, Internal_Input.Bytes_Count);
                     Internal_Input.Bytes := Bytes.all'Address;
                  end;
               when others =>
                  raise Program_Error;
            end case;

         when Text_Buffer =>
            declare
               Text : Big_Wide_Wide_String_Access;
            begin
               Get_Wide_Wide_String
                 (Input.Text, Text, Internal_Input.Text_Count);
               Internal_Input.Text := Text.all'Address;
            end;
      end case;

      Extract_Tokens
        (Input       => Internal_Input,
         With_Trivia => With_Trivia,
         File_Reader => null,
         TDH         => TDH,
         Diagnostics => Diagnostics);
   end Extract_Tokens;

   
      
----------------
-- Is_Keyword --
----------------

function Is_Keyword
  (TDH     : Token_Data_Handler;
   Index   : Token_Or_Trivia_Index;
   Version : Language_Version) return Boolean
is
   use Libadalang.Implementation;
   use Libadalang.Implementation.Precomputed_Symbols;
   use Langkit_Support.Symbols;
   use Langkit_Support.Token_Data_Handlers;

   Kind       : constant Token_Kind := To_Token_Kind (Data (Index, TDH).Kind);
   Sym        : Symbol_Type;

   function "+" (S : Precomputed_Symbol_Index) return Symbol_Type is
     (Precomputed_Symbol (Precomputed_Symbol_Table (TDH.Symbols), S));
begin
   --  Exit early on trivia tokens
   if Index.Trivia /= No_Token_Index then
      return False;

   --  Token that are not identifiers, decimal or integer literals but part of
   --  the Alphanumericals family are all keywords.
   elsif Kind not in Ada_Identifier | Ada_Decimal | Ada_Integer
         and then Token_Kind_To_Family (Kind) = Alphanumericals
   then
      return True;

   else
      Sym := Get_Symbol (Index, TDH);
      case Version is
         when Ada_83 =>
            return False;
         when Ada_95 =>
            return Sym in +Precomputed_Sym_Abstract
                        | +Precomputed_Sym_Protected
                        | +Precomputed_Sym_Requeue
                        | +Precomputed_Sym_Tagged
                        | +Precomputed_Sym_Until;
         when Ada_2005 =>
            return Sym in +Precomputed_Sym_Abstract
                        | +Precomputed_Sym_Interface
                        | +Precomputed_Sym_Overriding
                        | +Precomputed_Sym_Protected
                        | +Precomputed_Sym_Requeue
                        | +Precomputed_Sym_Synchronized
                        | +Precomputed_Sym_Tagged
                        | +Precomputed_Sym_Until;
         when Ada_2012 =>
            return Sym in +Precomputed_Sym_Abstract
                        | +Precomputed_Sym_Interface
                        | +Precomputed_Sym_Overriding
                        | +Precomputed_Sym_Protected
                        | +Precomputed_Sym_Requeue
                        | +Precomputed_Sym_Some
                        | +Precomputed_Sym_Synchronized
                        | +Precomputed_Sym_Tagged
                        | +Precomputed_Sym_Until;
      end case;
   end if;
end Is_Keyword;



end Libadalang.Lexer;
