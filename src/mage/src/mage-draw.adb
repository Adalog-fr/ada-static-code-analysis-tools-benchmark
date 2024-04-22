with Mage_Config.Hardware;

with SDL; use SDL; -- simple SDL types
with SDL.Video.Windows;
with SDL.Video.Windows.Makers;
with SDL.Video.Rectangles;
with SDL.Video.Renderers;
with SDL.Video.Renderers.Makers;
with SDL.Video.Palettes;

package body Mage.Draw is

   use all type SDL.Video.Windows.Window_Flags;

   package Windows renames SDL.Video.Windows;
   subtype SDL_Window is Windows.Window;

   package Rectangles renames SDL.Video.Rectangles;
   subtype SDL_Rectangle is Rectangles.Rectangle;
   subtype SDL_Point is SDL.Coordinates;
   subtype SDL_Line is Rectangles.Line_Segment;

   package Renderers renames SDL.Video.Renderers;
   subtype SDL_Renderer is Renderers.Renderer;

   subtype SDL_Palette_Color is SDL.Video.Palettes.Colour;
   subtype SDL_Palette_Color_Component is SDL.Video.Palettes.Colour_Component;

   type Window_And_Canvas_ID (Set : Boolean := False) is record
      case Set is
         when True =>
            Win    : Window_ID;
            Canvas : Canvas_ID; -- GNAT Game's canvas are handled as Canvass
         when False =>
            null;
      end case;
   end record;

   type Window_And_Canvas is limited record
      Win  : SDL_Window;
      Rend : SDL_Renderer;
      ID   : Window_And_Canvas_ID;
      Zoom : Float_Pos;
   end record;

   --  SDL API around windows and Canvass is complicated
   --  and we only ever need one of each anyway
   Render : Window_And_Canvas;

   -------------
   -- Helpers --
   -------------

   function To_Color (Color : RGBA_T) return SDL_Palette_Color is
     (SDL_Palette_Color_Component (Color.R),
      SDL_Palette_Color_Component (Color.G),
      SDL_Palette_Color_Component (Color.B),
      SDL_Palette_Color_Component (Color.A));

   function Current_Window_Is (ID : Window_ID) return Boolean is
   --  Sanity check for the fact that we have created the window and the
   --  app is using it

     (Render.ID.Win = ID);

   function Current_Canvas_Is (ID : Canvas_ID) return Boolean is
   --  Sanity check for the fact that we have created the window and the
   --  app is using its canvas

     (Render.ID.Canvas = ID);

   function To_Size (Width, Height : Positive) return Sizes is
     ((Dimension (Width), Dimension (Height)));

   function To_Screen_Point (S : Sizes) return Screen_Point is
     ((Positive (S.Width), Positive (S.Height)));

   function Size (Canvas : Canvas_ID) return Screen_Point is
     (To_Screen_Point (Render.Win.Get_Size)) with
     Pre => Current_Canvas_Is (Canvas);

   function To_Rectangle
     (Position : Screen_Point; Width, Height : Positive)
      return SDL_Rectangle is
     ((Coordinate (Position.X), Coordinate (Position.Y), Dimension (Width),
       Dimension (Height)));

   function To_Point (Position : Screen_Point) return SDL_Point is
     (SDL.Coordinate (Position.X), SDL.Coordinate (Position.Y));

   function To_Line (P1, P2 : Screen_Point) return SDL_Line is
     (To_Point (P1), To_Point (P2));

   function Canvas_Position_3d (Canvas : Canvas_ID) return Point_3d
   --  Top-right position of the canvas on the origin plane,
   --  in model space
   --  NB: the origin point is right in the middle of the canvas
   is
     (Extension (Size (Canvas), 0.0) / 2.0);

   function To_Point_3d
     (Canvas : Canvas_ID; P : Screen_Point; Z : Float := 0.0)
      return Point_3d
   --  (Width / 2, Heigth / 2) becomes (0.0, 0.0, 0.0)
   --  with operation in model (continuous) space
   is
     ((Extension (P, Z) / Zoom_Factor (Canvas) - Canvas_Position_3d (Canvas)));

   function Has_Screen_Point
     (Canvas : Canvas_ID; P : Point_3d) return Boolean is
     (abs P.X <= Canvas_Position_3d (Canvas).X
      and then abs P.Y <= Canvas_Position_3d (Canvas).Y
      and then Projection (P) <= Size (Canvas));

   function To_Screen_Point
     (Canvas : Canvas_ID; P : Point_3d) return Screen_Point is
     (Projection (P * Zoom_Factor (Canvas) + Canvas_Position_3d (Canvas)));
   --  (0.0, 0.0, 0.0) becomes (Width / 2, Height / 2)
   --  with operation in model (continuous) space
   --  and absent zoom

   ------------------
   -- Constructors --
   ------------------

   function Create_Window
     (Width, Height : Positive; Name : String) return Window_ID
   is
      Size : constant Sizes := To_Size (Width, Height);

      Renderer_Flag : constant Renderers.Renderer_Flags :=
        (if Mage_Config.Hardware.Enabled then Renderers.Accelerated
         else Renderers.Software);
   begin
      pragma Assert
        (not Render.ID.Set,
         "The current implementation is limited to a single window");

      Render.ID   :=
        (Set => True, Win => Window_ID'First, Canvas => Canvas_ID'First);
      Render.Zoom := 1.0;

      Windows.Makers.Create
        (Win      => Render.Win, Title => Name,
         Position =>
           (Windows.Centered_Window_Position,
            Windows.Centered_Window_Position),
         Size     => Size, Flags => Windows.OpenGL or Windows.Shown);

      Renderers.Makers.Create (Render.Rend, Render.Win, Renderer_Flag);

      return Render.ID.Win;
   end Create_Window;

   function Get_Canvas (Window : Window_ID) return Canvas_ID is
   begin
      pragma Assert (Current_Window_Is (Window));

      return Render.ID.Canvas;
   end Get_Canvas;

   ------------------
   -- App Handling --
   ------------------

   procedure Swap_Buffers (Window : Window_ID; Erase : Boolean := True) is
   begin
      pragma Assert (Current_Window_Is (Window));

      Swap_Copy_Buffers (Window);

      if Erase then
         Fill (Get_Canvas (Window), Black);
      end if;
   end Swap_Buffers;
   --  Optionaly clear the screen, update the canvas, poll events

   procedure Swap_Copy_Buffers (Window : Window_ID) is
   begin
      pragma Assert (Current_Window_Is (Window));

      Render.Rend.Present;
   end Swap_Copy_Buffers;
   --  Update the canvas
   --  copy the hidden buffer to the visible buffer

   ------------------------
   -- Drawing Primitives --
   ------------------------

   procedure Fill (Canvas : Canvas_ID; Color : RGBA_T) is
      Size : constant SDL.Sizes := Render.Win.Get_Size;
   begin
      Draw_Fill_Rect
        (Canvas, (0, 0), Positive (Size.Width), Positive (Size.Height), Color);
   end Fill;

   procedure Draw_Circle_Opt
     (Canvas : Canvas_ID; Position : Screen_Point; Radius : Positive;
      Color  : RGBA_T; Filled : Boolean)
   is
      --  https://stackoverflow.com/a/48291620
      Diameter : constant Positive := Radius * 2;

      --  X and Y are counters, X decreases, while Y increases
      --  furthermore, they are delta from the center in the
      --  positive quadrant, so they can't go over the radius
      subtype Radius_Counter is Integer range 0 .. Radius;
      X : Radius_Counter := Radius_Counter'Last;
      Y : Radius_Counter := Radius_Counter'First;

      Tx, Ty : Positive := 1;

      Error : Integer := Tx - Diameter;

      procedure Draw_Segment (P1, P2 : Screen_Point) is
      begin
         if Filled then
            Render.Rend.Draw (To_Line (P1, P2));
         else
            Render.Rend.Draw (To_Point (P1));
            Render.Rend.Draw (To_Point (P2));
         end if;
      end Draw_Segment;
   begin
      Prepare_Pixel_Draw (Canvas, Color);

      if Radius = 1 then
         Draw_Pixel (Canvas, Position);
         return;
      end if;

      while X >= Y loop
         Draw_Segment (Position + (+X, -Y), Position + (+X, +Y));
         Draw_Segment (Position + (-X, -Y), Position + (-X, +Y));
         Draw_Segment (Position + (+Y, -X), Position + (+Y, +X));
         Draw_Segment (Position + (-Y, -X), Position + (-Y, +X));

         if Error <= 0 then
            Y     := Y + 1;
            Error := Error + Ty;
            Ty    := Ty + 2;
         end if;

         if Error > 0 then
            X     := X - 1;
            Tx    := Tx + 2;
            Error := Error + Tx - Diameter;
         end if;
      end loop;
   end Draw_Circle_Opt;

   procedure Draw_Circle
     (Canvas : Canvas_ID; Position : Screen_Point; Radius : Positive;
      Color  : RGBA_T)
   is
   begin
      Draw_Circle_Opt (Canvas, Position, Radius, Color, Filled => False);
   end Draw_Circle;

   procedure Draw_Line
     (Canvas : Canvas_ID; P1 : Screen_Point; P2 : Screen_Point; Color : RGBA_T)
   is
   begin
      Prepare_Pixel_Draw (Canvas, Color);
      Render.Rend.Draw (To_Line (P1, P2));
   end Draw_Line;

   procedure Draw_Rect
     (Canvas : Canvas_ID; Position : Screen_Point; Width, Height : Positive;
      Color  : RGBA_T)
   is
      C1 : constant Screen_Point := Position;
      C2 : constant Screen_Point := Position + (Width, 0);
      C3 : constant Screen_Point := Position + (0, Height);
      C4 : constant Screen_Point := Position + (Width, Height);
   begin
      Prepare_Pixel_Draw (Canvas, Color);
      Render.Rend.Draw (To_Line (C1, C2));
      Render.Rend.Draw (To_Line (C1, C3));
      Render.Rend.Draw (To_Line (C2, C4));
      Render.Rend.Draw (To_Line (C3, C4));
   end Draw_Rect;

   procedure Draw_Fill_Rect
     (Canvas : Canvas_ID; Position : Screen_Point; Width, Height : Positive;
      Color  : RGBA_T)
   is
   begin
      Prepare_Pixel_Draw (Canvas, Color);
      Render.Rend.Fill (To_Rectangle (Position, Width, Height));
   end Draw_Fill_Rect;

   procedure Set_Pixel
     (Canvas : Canvas_ID; Position : Screen_Point; Color : RGBA_T)
   is
   begin
      Prepare_Pixel_Draw (Canvas, Color);
      Render.Rend.Draw (To_Point (Position));
   end Set_Pixel;

   procedure Draw_Circle
     (Canvas : Canvas_ID; Position : Point_3d; Radius : Float_Pos;
      Color  : RGBA_T)
   is
   begin
      Draw_Circle
        (Canvas, To_Screen_Point (Canvas, Position),
         Positive (Radius * Zoom_Factor (Canvas)), Color);
   end Draw_Circle;

   procedure Draw_Sphere
     (Canvas : Canvas_ID; Position : Point_3d; Radius : Float_Pos;
      Color  : RGBA_T)
   is
   begin
      Draw_Sphere
        (Canvas, To_Screen_Point (Canvas, Position),
         Positive (Radius * Zoom_Factor (Canvas)), Color);
   end Draw_Sphere;

   procedure Draw_Sphere
     (Canvas : Canvas_ID; Position : Screen_Point; Radius : Positive;
      Color  : RGBA_T)
   is
   begin
      Draw_Circle_Opt (Canvas, Position, Radius, Color, Filled => True);
   end Draw_Sphere;

   procedure Draw_Line
     (Canvas : Canvas_ID; P1 : Point_3d; P2 : Point_3d; Color : RGBA_T)
   is
   begin
      Draw_Line
        (Canvas, To_Screen_Point (Canvas, P1), To_Screen_Point (Canvas, P2),
         Color);
   end Draw_Line;

   procedure Draw_Rect
     (Canvas : Canvas_ID; Position : Point_3d; Width, Height : Float_Pos;
      Color  : RGBA_T)
   is
   begin
      Draw_Rect
        (Canvas, To_Screen_Point (Canvas, Position),
         Positive (Width * Zoom_Factor (Canvas)),
         Positive (Height * Zoom_Factor (Canvas)), Color);
   end Draw_Rect;

   procedure Draw_Fill_Rect
     (Canvas : Canvas_ID; Position : Point_3d; Width, Height : Float_Pos;
      Color  : RGBA_T)
   is
   begin
      Draw_Fill_Rect
        (Canvas, To_Screen_Point (Canvas, Position),
         Positive (Width * Zoom_Factor (Canvas)),
         Positive (Height * Zoom_Factor (Canvas)), Color);
   end Draw_Fill_Rect;

   ------------
   -- Legacy --
   ------------

   --  Those are disabled and non-supported, they raise exceptions

   procedure Draw_Text
     (Canvas : Canvas_ID; Position : Screen_Point; Text : String;
      Color  : RGBA_T; Bg_Color : RGBA_T := Black; Wrap : Boolean := True)
   is
   begin
      raise Unsupported_Feature;
   end Draw_Text;

   procedure Draw_Text
     (Canvas   : Canvas_ID; Position : Point_3d; Text : String; Color : RGBA_T;
      Bg_Color : RGBA_T := Black; Wrap : Boolean := True)
   is
   begin
      raise Unsupported_Feature;
   end Draw_Text;

   function Text_Size (Text : String) return Pixel_Dimensions is
     (raise Unsupported_Feature);

   procedure Enable_3d_Light (Canvas : Canvas_ID) is
   begin
      raise Unsupported_Feature;
   end Enable_3d_Light;

   procedure Disable_3d_Light (Canvas : Canvas_ID) is
   begin
      raise Unsupported_Feature;
   end Disable_3d_Light;

   procedure Set_3d_Light
     (Canvas        : Canvas_ID; Position : Point_3d; Diffuse_Color : RGBA_T;
      Ambient_Color : RGBA_T)
   is
   begin
      raise Unsupported_Feature;
   end Set_3d_Light;

   function Zoom_Factor (Canvas : Canvas_ID) return Float_Pos is (Render.Zoom);

   procedure Zoom_Factor (Canvas : Canvas_ID; ZF : Float_Pos) is
   begin
      Render.Zoom := ZF;
   end Zoom_Factor;

   function Center (Canvas : Canvas_ID) return Point_3d is
     (raise Unsupported_Feature);

   procedure Center (Canvas : Canvas_ID; Position : Point_3d) is
   begin
      raise Unsupported_Feature;
   end Center;

   function Get_Cursor_Status return Cursor_T is (raise Unsupported_Feature);

   --------------
   -- Internal --
   --------------

   pragma Warnings (Off, "formal parameter ""Canvas"" is not referenced");

   procedure Prepare_Pixel_Draw (Canvas : Canvas_ID; Color : RGBA_T) is
   begin
      Render.Rend.Set_Draw_Colour (To_Color (Color));
   end Prepare_Pixel_Draw;

   procedure Draw_Pixel (Canvas : Canvas_ID; Position : Screen_Point) is
   begin
      Render.Rend.Draw (To_Point (Position));
   end Draw_Pixel;

   pragma Warnings (On, "formal parameter ""Canvas"" is not referenced");

end Mage.Draw;
