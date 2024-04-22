pragma Ada_2012;

with Ada.Streams;

package Base64.Routines.Streams with
   Pure,
   Preelaborate
is
   function Standard_Character_To_Value is new Base64.Routines
     .Standard_Character_To_Value
     (Ada.Streams.Stream_Element);
   function Standard_Value_To_Character is new Base64.Routines
     .Standard_Value_To_Character
     (Ada.Streams.Stream_Element);
   function URLSafe_Character_To_Value is new Base64.Routines
     .URLSafe_Character_To_Value
     (Ada.Streams.Stream_Element);
   function URLSafe_Value_To_Character is new Base64.Routines
     .URLSafe_Value_To_Character
     (Ada.Streams.Stream_Element);
end Base64.Routines.Streams;
