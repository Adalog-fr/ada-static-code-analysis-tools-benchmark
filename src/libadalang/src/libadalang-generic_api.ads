--
--  Copyright (C) 2014-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with Langkit_Support.Generic_API; use Langkit_Support.Generic_API;
with Langkit_Support.Generic_API.Analysis;
use Langkit_Support.Generic_API.Analysis;

with Libadalang.Analysis; use Libadalang.Analysis;
with Libadalang.Common;   use Libadalang.Common;

package Libadalang.Generic_API is

   Ada_Lang_Id : constant Language_Id
     with Import, External_Name => "Libadalang__language_id";
   --  Unique identifier for Libadalang

   Self_Id : Language_Id renames Ada_Lang_Id;
   --  Shortcut for convenience in code generation

   function To_Generic_Context (Context : Analysis_Context) return Lk_Context;
   --  Convert the given ``Context`` into a value suitable to use in the
   --  Langkit generic API.

   function From_Generic_Context
     (Context : Lk_Context) return Analysis_Context;
   --  Convert the ``Context`` value from the Langkit generic API into the
   --  Libadalang-specific context type. Raise a
   --  ``Langkit_Support.Errors.Precondition_Failure`` if ``Context`` does not
   --  belong to Libadalang.

   function To_Generic_Unit (Unit : Analysis_Unit) return Lk_Unit;
   --  Convert the given ``Unit`` into a value suitable to use in the Langkit
   --  generic API.

   function From_Generic_Unit (Unit : Lk_Unit) return Analysis_Unit;
   --  Convert the ``Unit`` value from the Langkit generic API into the
   --  Libadalang-specific unit type. Raise a
   --  ``Langkit_Support.Errors.Precondition_Failure`` if ``Unit`` does not
   --  belong to Libadalang.

   function To_Generic_Grammar_Rule
     (Rule : Grammar_Rule) return Langkit_Support.Generic_API.Grammar_Rule_Ref;
   --  Convert the given ``rule`` into a value suitable to use in the Langkit
   --  generic API.

   function From_Generic_Grammar_Rule
     (Rule : Langkit_Support.Generic_API.Grammar_Rule_Ref) return Grammar_Rule;
   --  Convert the ``Rule`` value from the Langkit generic API into the
   --  Libadalang-specific unit type. Raise a
   --  ``Langkit_Support.Errors.Precondition_Failure`` if ``Rule`` does not
   --  belong to Libadalang or if it is ``No_Grammar_Rule_Ref``.

end Libadalang.Generic_API;
