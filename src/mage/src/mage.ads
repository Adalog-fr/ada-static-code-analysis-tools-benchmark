with SDL.Video.Palettes;

package Mage is

   pragma Elaborate_Body;

   Mage_Error : exception;

   subtype Float_Pos is Float range Float'Succ (0.0) .. Float'Last;

   subtype Color_Component_T is SDL.Video.Palettes.Colour_Component;

   type RGBA_T is record
      R : Color_Component_T;
      G : Color_Component_T;
      B : Color_Component_T;
      A : Color_Component_T;
   end record;

   function "+" (C : RGBA_T) return SDL.Video.Palettes.Colour is
     ((C.R, C.G, C.B, C.A));

   function "+" (C : RGBA_T) return SDL.Video.Palettes.RGB_Colour is
     ((C.R, C.G, C.B));

   Transparent : constant RGBA_T := (R => 0, G => 0, B => 0, A => 0);
   Black       : constant RGBA_T := (R => 0, G => 0, B => 0, A => 255);
   White       : constant RGBA_T := (R => 255, G => 255, B => 255, A => 255);
   Blue        : constant RGBA_T := (R => 0, G => 0, B => 255, A => 255);
   Green       : constant RGBA_T := (R => 0, G => 255, B => 0, A => 255);
   Red         : constant RGBA_T := (R => 255, G => 0, B => 0, A => 255);
   Cyan        : constant RGBA_T := (R => 0, G => 255, B => 255, A => 255);
   Magenta     : constant RGBA_T := (R => 255, G => 0, B => 255, A => 255);
   Yellow      : constant RGBA_T := (R => 255, G => 255, B => 0, A => 255);
   Violet      : constant RGBA_T := (R => 128, G => 0, B => 128, A => 255);
   Gray        : constant RGBA_T := (R => 128, G => 128, B => 128, A => 255);

   Display_Error       : exception;
   Unsupported_Feature : exception;

   type Button_Type is (Left, Right);

   type Mouse_Position is record
      X, Y   : Float;
      Button : Button_Type;
   end record;

   No_Mouse_Position : constant Mouse_Position :=
     (-10_000.0, -10_000.0, Right);

end Mage;
