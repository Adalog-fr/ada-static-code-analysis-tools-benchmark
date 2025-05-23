--
--  Copyright (C) 2014-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with Libadalang.Analysis; use Libadalang.Analysis;

package Libadalang.Unparsing is

   function Unparse (Node : Ada_Node'Class) return String
      with Pre => not Node.Unit.Has_Diagnostics;
   --  Turn the Node tree into a string that can be re-parsed to yield the same
   --  tree (source locations excepted). The encoding used is the same as the
   --  one that was used to parse Node's analysis unit.
   --
   --  Note that this requires that Node's unit has no parsing error.

end Libadalang.Unparsing;
