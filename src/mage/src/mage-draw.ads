with Mage.Model; use Mage.Model;

package Mage.Draw is

   type Window_ID is private;

   function Create_Window
     (Width : Positive; Height : Positive; Name : String) return Window_ID;

   Graphical_Context_Not_Initialized : exception;
   Too_Many_Windows                  : exception;
   Too_Many_Canvas                   : exception;

   type Canvas_ID is private;

   function Get_Canvas (Window : Window_ID) return Canvas_ID;

   subtype Pixel_Dimensions is Screen_Point with
       Predicate =>
        Pixel_Dimensions.X in Positive and then Pixel_Dimensions.Y in Positive;

   type Cursor_T is record
      Position : Screen_Point;
      Pressed  : Boolean;
   end record;

   ----------------
   -- Rectangles --
   ----------------

   procedure Fill (Canvas : Canvas_ID; Color : RGBA_T);

   procedure Draw_Rect
     (Canvas : Canvas_ID; Position : Point_3d; Width, Height : Float_Pos;
      Color  : RGBA_T);

   procedure Draw_Rect
     (Canvas : Canvas_ID; Position : Screen_Point; Width, Height : Positive;
      Color  : RGBA_T);

   procedure Draw_Fill_Rect
     (Canvas : Canvas_ID; Position : Point_3d; Width, Height : Float_Pos;
      Color  : RGBA_T);

   procedure Draw_Fill_Rect
     (Canvas : Canvas_ID; Position : Screen_Point; Width, Height : Positive;
      Color  : RGBA_T);

   -------------
   -- Circles --
   -------------

   procedure Draw_Sphere
     (Canvas : Canvas_ID; Position : Point_3d; Radius : Float_Pos;
      Color  : RGBA_T);

   procedure Draw_Sphere
     (Canvas : Canvas_ID; Position : Screen_Point; Radius : Positive;
      Color  : RGBA_T);

   procedure Draw_Circle
     (Canvas : Canvas_ID; Position : Point_3d; Radius : Float_Pos;
      Color  : RGBA_T);

   procedure Draw_Circle
     (Canvas : Canvas_ID; Position : Screen_Point; Radius : Positive;
      Color  : RGBA_T);

   --------------------
   -- Lines & Points --
   --------------------

   procedure Draw_Line
     (Canvas : Canvas_ID; P1 : Point_3d; P2 : Point_3d; Color : RGBA_T);

   procedure Draw_Line
     (Canvas : Canvas_ID; P1 : Screen_Point; P2 : Screen_Point;
      Color  : RGBA_T);

   procedure Set_Pixel
     (Canvas : Canvas_ID; Position : Screen_Point; Color : RGBA_T);

   ----------
   -- Text --
   ----------

   procedure Draw_Text
     (Canvas   : Canvas_ID; Position : Point_3d; Text : String; Color : RGBA_T;
      Bg_Color : RGBA_T := Black; Wrap : Boolean := True);

   procedure Draw_Text
     (Canvas : Canvas_ID; Position : Screen_Point; Text : String;
      Color  : RGBA_T; Bg_Color : RGBA_T := Black; Wrap : Boolean := True);

   function Text_Size (Text : String) return Pixel_Dimensions;

   -----------
   -- Light --
   -----------

   procedure Enable_3d_Light (Canvas : Canvas_ID);

   procedure Disable_3d_Light (Canvas : Canvas_ID);

   procedure Set_3d_Light
     (Canvas        : Canvas_ID; Position : Point_3d; Diffuse_Color : RGBA_T;
      Ambient_Color : RGBA_T);

   ------------
   -- Camera --
   ------------

   function Zoom_Factor (Canvas : Canvas_ID) return Float_Pos;

   procedure Zoom_Factor (Canvas : Canvas_ID; ZF : Float_Pos);

   function Center (Canvas : Canvas_ID) return Point_3d;

   procedure Center (Canvas : Canvas_ID; Position : Point_3d);

   ------------
   -- Render --
   ------------

   procedure Swap_Buffers (Window : Window_ID; Erase : Boolean := True);
   --  Optionaly clear the screen, update the canvas, poll events
   --  and copy the hidden buffer to the visible buffer

   procedure Swap_Copy_Buffers (Window : Window_ID);
   --  Update the canvas

   function Get_Cursor_Status return Cursor_T;

   function To_Point_3d
     (Canvas : Canvas_ID; P : Screen_Point; Z : Float := 0.0) return Point_3d;
   --  Convert from a screen point to its 3D counterpart in the given Z plane

   function Has_Screen_Point (Canvas : Canvas_ID; P : Point_3d) return Boolean;
   --  Do the point in space have a pixel in screen?

   function To_Screen_Point
     (Canvas : Canvas_ID; P : Point_3d) return Screen_Point with
     Pre => Has_Screen_Point (Canvas, P);
   --  Convert from a model point in space to a projected screen point

private

   type Window_ID_Range is range 0 .. 10;
   type Window_ID is new Window_ID_Range;

   type Canvas_ID_Range is range 1 .. 1_024;
   type Canvas_ID is new Canvas_ID_Range;

   -------------------------------------------
   -- Basic Operations for Advanced Drawing --
   -------------------------------------------
   function Current_Canvas_Is (ID : Canvas_ID) return Boolean;

   procedure Prepare_Pixel_Draw (Canvas : Canvas_ID; Color : RGBA_T) with
     Pre => Current_Canvas_Is (Canvas);
   --  Prepares the renderer to draw, by selecting a painting color

   procedure Draw_Pixel (Canvas : Canvas_ID; Position : Screen_Point) with
     Pre => Current_Canvas_Is (Canvas);
   --  Draw a single pixel, using the prepared renderer.

end Mage.Draw;
