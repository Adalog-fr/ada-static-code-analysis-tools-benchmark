with Ada.Containers.Ordered_Sets;

with SDL.Events.Keyboards;

package Mage.Input is

   package Keyboards renames SDL.Events.Keyboards;

   --  Inputs
   --
   --  Inputs are separated into actions and modifiers on those actions.
   --  Underneath they are mapped to any input device.

   type Action is
     (Up, Down, Left, Right, Turn_Left, Turn_Right, Strafe_Left, Strafe_Right,

      Jump, Crouch, Prone, Enter, Escape, Interact, Talk, Grab, Release, Push,
      Pull, Open, Close);

   subtype Absolute_Direction is Action range Up .. Right;
   subtype Relative_Direction is Action range Turn_Left .. Strafe_Right;
   subtype Posture is Action range Jump .. Prone;
   subtype Interaction is Action range Enter .. Close;

   type Modifier is (Any, None, Strong, Weak, Fast, Slow, High, Low, Super);

   subtype Configurable_Modifier is Modifier range None .. Modifier'Last;
   subtype Existing_Modifier is Modifier range Strong .. Modifier'Last;

   type Modifier_Flags is array (Existing_Modifier) of Boolean;

   type Mod_Key is (Shift, Ctrl, Alt);

   --------------
   -- Keyboard --
   --------------

   procedure Map_Keyboard
     (A           : Action; Pressed : Keyboards.Scan_Codes;
      Can_Be_Held : Boolean := True);

   procedure Map_Keyboard
     (A : Action; M : Configurable_Modifier; Pressed : Keyboards.Scan_Codes;
      Can_Be_Held : Boolean := True);

   procedure Map_Keyboard (M : Existing_Modifier; Held : Mod_Key);
   --  Map a modifier key to a modifier, will add the modifier to all actions
   --  that happen while the mod key is being pressed

   -------------
   -- General --
   -------------

   type Keyboard_Layouts is (Unknown, QWERTY, QWERTZ, AZERTY);

   function Keyboard return Keyboard_Layouts;

   procedure Keyboard (L : Keyboard_Layouts; Apply_Presets : Boolean := True);
   --  This is called at initialization

   procedure Clear (A : Action);

   procedure Clear (M : Modifier);

   procedure Clear_All;
   --  NB: Mage uses localized presets, this will clear them

   type Action_Set is tagged private;
   function Global_Actions return Action_Set;

   function Has_Action
     (E : Action_Set; A : Action; M : Modifier := Any) return Boolean;

private

   type Full_Action is record
      Act      : Action;
      Modifier : Modifier_Flags;
   end record;

   function Set_Ordering (A, B : Full_Action) return Boolean is
     (A.Act < B.Act);

   package Action_Sets_Pkg is new Ada.Containers.Ordered_Sets
     (Full_Action, "<" => Set_Ordering);

   type Action_Set is new Action_Sets_Pkg.Set with null record;

   function Is_Action
     (E : Full_Action; A : Action; M : Modifier := Any) return Boolean is
     (E.Act = A
      and then
      (if M /= Any then -- Any = accept all modifiers
         (if M /= None then E.Modifier (M) -- specified modifier must be set
          else
            (for all F of E.Modifier =>
               not F)))); -- None = no modifier must be set

   function Has_Action
     (E : Action_Set; A : Action; M : Modifier := Any) return Boolean is
     (for some V of E => Is_Action (V, A, M));

end Mage.Input;
