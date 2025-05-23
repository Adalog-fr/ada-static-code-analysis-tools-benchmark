--
--  Copyright (C) 2014-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Text_IO; use Ada.Text_IO;

with Libadalang.Common; use Libadalang.Common;


          with Libadalang.Sources;


package body Libadalang.Debug is

   use Support.Slocs, Support.Text;

   --------
   -- PN --
   --------

   procedure PN (Node : Bare_Ada_Node) is
      Ent : constant Internal_Entity :=
        (Node => Node, Info => No_Entity_Info);
   begin
      Put_Line (Image (Ent));
   end PN;

   --------
   -- PT --
   --------

   procedure PT (Node : Bare_Ada_Node) is
   begin
      Print (Node, Show_Slocs => True);
   end PT;

   ----------
   -- PTok --
   ----------

   procedure PTok (TDH : Token_Data_Handler_Access; T : Token_Index) is
      Index : constant Natural := Natural (T);
   begin
      if Index not in TDH.Tokens.First_Index .. TDH.Tokens.Last_Index then
         Put_Line ("<invalid token>");

      else
         declare
            D : constant Stored_Token_Data := TDH.Tokens.Get (Index);
         begin
            Put (Token_Kind_Name (To_Token_Kind (D.Kind)));
            Put (" " & Image (Langkit_Support
                              .Token_Data_Handlers.Text (TDH.all, D),
                              With_Quotes => True));
            Put_Line (" [" & Image (Sloc_Range (TDH.all, D)) & "]");
         end;
      end if;
   end PTok;

   ----------
   -- PEnv --
   ----------

   procedure PEnv (Env : Lexical_Env) is
   begin
      AST_Envs.Dump_Lexical_Env_Parent_Chain (Env);
   end PEnv;

   -----------------
   -- Sym_Matches --
   -----------------

   function Sym_Matches (S : Symbol_Type; Text : String) return Boolean is
      Symbol : constant Symbolization_Result :=
            Libadalang.Sources.Canonicalize (To_Text (Text))
      ;
   begin
      return Symbol.Success and then Image (S.all) = Image (Symbol.Symbol);
   end Sym_Matches;

end Libadalang.Debug;
