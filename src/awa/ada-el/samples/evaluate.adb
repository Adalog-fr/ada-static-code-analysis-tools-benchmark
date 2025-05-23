-----------------------------------------------------------------------
--  el -- Evaluate an EL expression
--  Copyright (C) 2009, 2010, 2011 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.
-----------------------------------------------------------------------
with EL.Expressions;
with EL.Objects;
with EL.Contexts.Default;
with Ada.Text_IO;

procedure Evaluate is
   Expr   : constant String := "#{1 + (2 - 3) * 4}";
   Ctx    : EL.Contexts.Default.Default_Context;
   E      : EL.Expressions.Expression;
   Result : EL.Objects.Object;
begin
   Ada.Text_IO.Put_Line ("Evaluate: " & Expr);
   E := EL.Expressions.Create_Expression (Expr, Ctx);
   Result := E.Get_Value (Ctx);
   Ada.Text_IO.Put_Line ("Result: " & EL.Objects.To_String (Result));
end Evaluate;
