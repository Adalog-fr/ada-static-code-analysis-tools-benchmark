--
--  Copyright (C) 2014-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

--  This package defines subprograms whose only purpose it to be used from a
--  debugger. This is supposed to make developpers' life easier.

with Langkit_Support.Lexical_Envs; use Langkit_Support.Lexical_Envs;
with Langkit_Support.Symbols;      use Langkit_Support.Symbols;
with Langkit_Support.Token_Data_Handlers;
use Langkit_Support.Token_Data_Handlers;

with Libadalang.Implementation;
use Libadalang.Implementation;

private package Libadalang.Debug is

   procedure PN (Node : Bare_Ada_Node);
   --  "Print Node".  Shortcut for Put_Line (Node.Image). This is useful
   --  because Image takes an implicit accessibility level parameter,
   --  which is not convenient in GDB.

   procedure PT (Node : Bare_Ada_Node);
   --  "Print Tree". Shortcut for Node.Print. This is useful because Print is a
   --  dispatching primitive whereas these are difficult to call from GDB.
   --  Besides, it removes the Level parameter.

   procedure PTok (TDH : Token_Data_Handler_Access; T : Token_Index);
   --  "Print Token". Print the data associated to the T token in the given
   --  token data handler.

   procedure PEnv (Env : Lexical_Env);
   --  "Print lexical Environment". Print the content of Env and all its parent
   --  chain.

   function Sym_Matches (S : Symbol_Type; Text : String) return Boolean;
   --  Return whether the text associated to S matches Text. There is a bug in
   --  GDB that makes comparison with "=" always return false.

end Libadalang.Debug;
