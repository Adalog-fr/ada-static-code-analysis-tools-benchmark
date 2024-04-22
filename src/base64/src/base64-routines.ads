pragma Ada_2012;

package Base64.Routines with
   Pure,
   Preelaborate
is
   generic
      type Element is mod <>;
   function Standard_Character_To_Value (C : Character) return Element;
   generic
      type Element is mod <>;
   function Standard_Value_To_Character (E : Element) return Character;
   generic
      type Element is mod <>;
   function URLSafe_Character_To_Value (C : Character) return Element;
   generic
      type Element is mod <>;
   function URLSafe_Value_To_Character (E : Element) return Character;
end Base64.Routines;
