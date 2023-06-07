with Ada.Characters.Latin_1;
with Ada.Containers.Ordered_Maps;

with Interfaces;

with SDL.Inputs.Keyboards;

with Mage_Config;
with Mage.Event;
with Mage.Log;

package body Mage.Input is

   procedure Keyboard_Log (S : String) is
   begin
      Mage.Log ("mage.input.keyboard", S);
   end Keyboard_Log;

   use all type Keyboards.Scan_Codes;
   use all type Keyboards.Key_Modifiers;

   package Keyboard_Inputs renames SDL.Inputs.Keyboards;

   type Keyboard_Map_Entry is record
      A           : Action;
      M           : Configurable_Modifier;
      Can_Be_Held : Boolean;
   end record;

   package Keyboard_Maps_Pkg is new Ada.Containers.Ordered_Maps
     (Keyboards.Scan_Codes, Keyboard_Map_Entry);
   use Keyboard_Maps_Pkg;
   subtype Keyboard_Maps is Keyboard_Maps_Pkg.Map;
   Keyboard_Map : Keyboard_Maps;

   package Keyboard_Modifiers_Maps_Pkg is new Ada.Containers.Ordered_Maps
     (Mod_Key, Modifier);
   use Keyboard_Modifiers_Maps_Pkg;
   subtype Keyboard_Modifier_Maps is Keyboard_Modifiers_Maps_Pkg.Map;
   Keyboard_Modifier_Map : Keyboard_Modifier_Maps;

   procedure Map_Keyboard
     (A           : Action; Pressed : Keyboards.Scan_Codes;
      Can_Be_Held : Boolean := True)
   is
   begin
      Map_Keyboard (A, None, Pressed, Can_Be_Held);
   end Map_Keyboard;

   procedure Map_Keyboard
     (A : Action; M : Configurable_Modifier; Pressed : Keyboards.Scan_Codes;
      Can_Be_Held : Boolean := True)
   is
   begin
      Keyboard_Map.Insert (Pressed, (A, M, Can_Be_Held));
   end Map_Keyboard;

   procedure Map_Keyboard (M : Existing_Modifier; Held : Mod_Key) is
   begin
      Keyboard_Modifier_Map.Insert (Held, M);
   end Map_Keyboard;

   procedure Clear (A : Action) is
      C : Keyboard_Maps_Pkg.Cursor := Keyboard_Map.First;
   begin
      while C /= Keyboard_Maps_Pkg.No_Element loop
         if Element (C).A = A then
            Keyboard_Map.Delete (C);
         end if;
         Next (C);
      end loop;
   end Clear;

   procedure Clear (M : Modifier) is
      C : Keyboard_Modifiers_Maps_Pkg.Cursor := Keyboard_Modifier_Map.First;
   begin
      while C /= Keyboard_Modifiers_Maps_Pkg.No_Element loop
         if Element (C) = M then
            Keyboard_Modifier_Map.Delete (C);
         end if;
         Next (C);
      end loop;
   end Clear;

   procedure Clear_All is
   begin
      Keyboard_Modifier_Map.Clear;
      Keyboard_Map.Clear;
   end Clear_All;

   package Scan_Codes_Sets_Pkg is new Ada.Containers.Ordered_Sets
     (Keyboards.Scan_Codes);
   subtype Scan_Codes_Set is Scan_Codes_Sets_Pkg.Set;

   Globally_Pressed : Scan_Codes_Set;

   function Global_Actions return Action_Set is
      --  we poll all keyboard event so that we can apply a global
      --  modifier flag correctly

      function Get_Modifiers return Modifier_Flags is
         KM : constant Keyboards.Key_Modifiers :=
           Keyboard_Inputs.Get_Modifiers;

         --  Portability for MacOS: ctrl is replaced by command
         --  (called GUI in SDL)
         Modifier_Portable_Ctrl_Or_Command :
           constant Keyboards.Key_Modifiers :=
           (if Mage_Config.Alire_Host_OS = "darwin" then Keyboards.Modifier_GUI
            else Keyboards.Modifier_Control);

         Mod_Flags : Modifier_Flags;

         procedure Apply_Modifier (MK : Mod_Key) is
            C : constant Keyboard_Modifiers_Maps_Pkg.Cursor :=
              Keyboard_Modifier_Map.Find (MK);
         begin
            if C /= Keyboard_Modifiers_Maps_Pkg.No_Element then
               Mod_Flags (Element (C)) := True;
            end if;
         end Apply_Modifier;
      begin
         if (KM and Keyboards.Modifier_Shift) /= 0 then
            Apply_Modifier (Shift);
         end if;

         if (KM and Keyboards.Modifier_Alt) /= 0 then
            Apply_Modifier (Alt);
         end if;

         if (KM and Modifier_Portable_Ctrl_Or_Command) /= 0 then
            Apply_Modifier (Ctrl);
         end if;

         return Mod_Flags;
      end Get_Modifiers;

      function To_Flags (E : Configurable_Modifier) return Modifier_Flags is
         F : Modifier_Flags := (others => False);
      begin
         if E /= None then
            F (E) := True;
         end if;

         return F;
      end To_Flags;

      Global_Mod_Flags : constant Modifier_Flags := Get_Modifiers;
      --  Modifiers set globally by mod keys, to which we will add
      --  local modifiers on a per-action basis

      S : Action_Set;

      Locally_Pressed : Scan_Codes_Set;
   begin
      --  Start by updating the status of all keys
      declare
         Evt : Mage.Event.Keyboard_Event;
         use all type SDL.Events.Button_State;
         use all type Interfaces.Unsigned_8;
      begin
         while Mage.Event.Poll_Keyboard (Evt) loop
            declare
               SC : constant Keyboards.Scan_Codes := Evt.Key_Sym.Scan_Code;
            begin
               if Evt.State = Pressed then
                  if Evt.Repeat = 0 then
                     Locally_Pressed.Insert (SC);
                  end if;
               else
                  Globally_Pressed.Exclude (SC);
               end if;
            end;
         end loop;
      end;

      --  Turn the pressed keys into actions
      for SC of Scan_Codes_Sets_Pkg.Union (Locally_Pressed, Globally_Pressed)
      loop
         declare
            C : constant Keyboard_Maps_Pkg.Cursor := Keyboard_Map.Find (SC);
         begin
            if C /= Keyboard_Maps_Pkg.No_Element then
               declare
                  E : constant Keyboard_Map_Entry := Element (C);

                  procedure Insert_Current_Entry is
                  begin
                     S.Include ((E.A, To_Flags (E.M) and Global_Mod_Flags));
                  end Insert_Current_Entry;

               begin
                  if E.Can_Be_Held then
                     Globally_Pressed.Include (SC);
                     Insert_Current_Entry;
                  elsif not Globally_Pressed.Contains (SC) then
                     Insert_Current_Entry;
                  end if;
               end;
            end if;
         end;
      end loop;

      return S;
   end Global_Actions;

   --------------------
   -- Body Internals --
   --------------------

   use Keyboards;

   Keyboard_Layout : Keyboard_Layouts := Unknown;

   function Keyboard return Keyboard_Layouts is (Keyboard_Layout);

   procedure Keyboard_Apply_Presets;

   procedure Keyboard (L : Keyboard_Layouts; Apply_Presets : Boolean := True)
   is
   begin
      Keyboard_Layout := L;

      if Apply_Presets then
         Keyboard_Apply_Presets;
      end if;
   end Keyboard;

   procedure Keyboard_Apply_Presets is
   begin
      Map_Keyboard (Up, Scan_Code_Up);
      Map_Keyboard (Down, Scan_Code_Down);
      Map_Keyboard (Left, Scan_Code_Left);
      Map_Keyboard (Right, Scan_Code_Right);

      Map_Keyboard (Fast, Shift);
      Map_Keyboard (Slow, Ctrl);

      case Keyboard is
         when Unknown =>
            null;
         when QWERTY =>
            Map_Keyboard (Up, Scan_Code_W);
            Map_Keyboard (Left, Scan_Code_A);
         when QWERTZ =>
            Map_Keyboard (Up, Scan_Code_W);
            Map_Keyboard (Left, Scan_Code_A);
         when AZERTY =>
            Map_Keyboard (Up, Scan_Code_Z);
            Map_Keyboard (Left, Scan_Code_Q);
      end case;

      Map_Keyboard (Down, Scan_Code_S);
      Map_Keyboard (Right, Scan_Code_D);

   end Keyboard_Apply_Presets;

   function Infer_Keyboard_Layout return Keyboard_Layouts is
      --  https://gist.github.com/g2p/8597984
      type Keyboard_Fingerprint is array (1 .. 3) of Character;

      function Fingerprint_Keyboard return Keyboard_Fingerprint is
         type Maybe_Character_And_Key_Code (Set : Boolean := False) is record
            case Set is
               when True =>
                  C  : Character;
                  KC : Keyboards.Key_Codes;
               when False =>
                  null;
            end case;
         end record;

         function To_Char
           (KC : Keyboards.Key_Codes; From : Keyboards.Key_Codes;
            To : Character) return Character
         is
            Offset : constant Positive :=
              Keyboards.Key_Codes'Pos (KC) - Keyboards.Key_Codes'Pos (From);
         begin
            return Character'Val (Character'Pos (To) + Offset);
         end To_Char;

         function To_Char
           (SC      : Keyboards.Scan_Codes;
            Default : Character := Ada.Characters.Latin_1.NUL) return Character
         is
            KC    : constant Keyboards.Key_Codes := Keyboards.To_Key_Code (SC);
            Found : constant Maybe_Character_And_Key_Code :=
              (case KC is when Code_0 .. Code_9 => (True, '0', Code_0),
                 when Code_A .. Code_Z => (True, 'a', Code_A),
                 when others => (Set => False));
         begin
            return
              (if Found.Set then To_Char (KC, Found.KC, Found.C) else Default);
         end To_Char;
      begin
         return
           To_Char (Keyboards.Scan_Code_Q) & To_Char (Keyboards.Scan_Code_W) &
           To_Char (Keyboards.Scan_Code_Y);
      end Fingerprint_Keyboard;

      FP : constant Keyboard_Fingerprint := Fingerprint_Keyboard;

      Layout : constant Keyboard_Layouts :=
        (if FP = "qwy" then QWERTY elsif FP = "qwz" then QWERTZ
         elsif FP = "azy" then AZERTY else Unknown);
   begin
      Keyboard_Log
        ("inferred layout " & Layout'Image & " <fingerprint = """ &
         String (FP) & """>");
      return Layout;
   end Infer_Keyboard_Layout;

begin
   Keyboard (Infer_Keyboard_Layout);
end Mage.Input;
