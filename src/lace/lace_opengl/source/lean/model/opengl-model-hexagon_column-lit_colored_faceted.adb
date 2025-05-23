with
     openGL.Primitive.indexed,
     openGL.Geometry.lit_colored,
     openGL.Model.hexagon;


package body openGL.Model.Hexagon_Column.lit_colored_faceted
is
   ---------
   --- Forge
   --

   function new_hexagon_Column (Radius : in Real;
                                Height : in Real;
                                Upper,
                                Lower  : in   hex_Face;
                                Shaft  : in shaft_Face) return View
   is
      Self : constant View := new Item;
   begin
      Self.Radius     := Radius;
      Self.Height     := Height;
      Self.upper_Face := Upper;
      Self.lower_Face := Lower;
      Self.Shaft      := Shaft;

      return Self;
   end new_hexagon_Column;


   --------------
   --- Attributes
   --

   overriding
   function to_GL_Geometries (Self : access Item;   Textures : access Texture.name_Map_of_texture'Class;
                                                    Fonts    : in     Font.font_id_Map_of_font) return Geometry.views
   is
      pragma unreferenced (Fonts, Textures);

      use Geometry.lit_colored,
          Model.hexagon;

      shaft_Height  : constant Real     := Self.Height;
      height_Offset : constant Vector_3 := (0.0, shaft_Height / 2.0, 0.0);

      mid_Sites     : constant hexagon.Sites := vertex_Sites (Self.Radius);
      upper_Sites   :          hexagon.Sites := mid_Sites;
      lower_Sites   :          hexagon.Sites := mid_Sites;


      function new_hexagon_Face (Vertices : access Geometry.lit_colored.Vertex_array;
                                 Flip     : in     Boolean := False) return Geometry.lit_colored.view
      is
         use Primitive;

         function the_Indices return Indices
         is
         begin
            if Flip
            then   return (1, 7, 6, 5, 4, 3, 2, 7);
            else   return (1, 2, 3, 4, 5, 6, 7, 2);
            end if;
         end the_Indices;

         the_Geometry  : constant Geometry.lit_colored.view
           := Geometry.lit_colored.new_Geometry;

         the_Primitive : constant Primitive.indexed.view
           := Primitive.indexed.new_Primitive (triangle_Fan,
                                               the_Indices);
      begin
         the_Geometry.Vertices_are (Vertices.all);
         the_Geometry.add (Primitive.view (the_Primitive));

         return the_Geometry;
      end new_hexagon_Face;


      function new_shaft_Face (Vertices : access Geometry.lit_colored.Vertex_array)
                               return Geometry.lit_colored.view
      is
         use Primitive;

         the_Indices   : constant Indices := (1, 2, 3, 4);

         the_Geometry  : constant Geometry.lit_colored.view
           := Geometry.lit_colored.new_Geometry;

         the_Primitive : constant Primitive.view
           := Primitive.indexed.new_Primitive (triangle_Strip, the_Indices).all'Access;
      begin
         the_Geometry.Vertices_are (Vertices.all);
         the_Geometry.add          (the_Primitive);

         return the_Geometry;
      end new_shaft_Face;


      upper_Face  : Geometry.lit_colored.view;
      lower_Face  : Geometry.lit_colored.view;

      shaft_Faces : array (1 .. 6) of Geometry.lit_colored.view;

   begin
      for Each in mid_Sites'Range
      loop
         upper_Sites (Each) := upper_Sites (Each) + height_Offset;
         lower_Sites (Each) := lower_Sites (Each) - height_Offset;
      end loop;

      --  Upper
      --
      declare
         the_Vertices : aliased Geometry.lit_colored.Vertex_array
           := (1 => (Site => height_Offset,     Normal => Normal,   Color => +Self.upper_Face.center_Color,   Shine => default_Shine),
               2 => (Site => upper_Sites (1),   Normal => Normal,   Color => +Self.upper_Face.Colors (1),     Shine => default_Shine),
               3 => (Site => upper_Sites (2),   Normal => Normal,   Color => +Self.upper_Face.Colors (2),     Shine => default_Shine),
               4 => (Site => upper_Sites (3),   Normal => Normal,   Color => +Self.upper_Face.Colors (3),     Shine => default_Shine),
               5 => (Site => upper_Sites (4),   Normal => Normal,   Color => +Self.upper_Face.Colors (4),     Shine => default_Shine),
               6 => (Site => upper_Sites (5),   Normal => Normal,   Color => +Self.upper_Face.Colors (5),     Shine => default_Shine),
               7 => (Site => upper_Sites (6),   Normal => Normal,   Color => +Self.upper_Face.Colors (6),     Shine => default_Shine));
      begin
         upper_Face := new_hexagon_Face (Vertices => the_Vertices'Access);
      end;

      --  Lower
      --
      declare
         the_Vertices : aliased Geometry.lit_colored.Vertex_array
           := (1 => (Site => -height_Offset,    Normal => -Normal,   Color => +Self.upper_Face.center_Color,   Shine => default_Shine),
               2 => (Site =>  lower_Sites (1),  Normal => -Normal,   Color => +Self.upper_Face.Colors (1),     Shine => default_Shine),
               3 => (Site =>  lower_Sites (2),  Normal => -Normal,   Color => +Self.upper_Face.Colors (2),     Shine => default_Shine),
               4 => (Site =>  lower_Sites (3),  Normal => -Normal,   Color => +Self.upper_Face.Colors (3),     Shine => default_Shine),
               5 => (Site =>  lower_Sites (4),  Normal => -Normal,   Color => +Self.upper_Face.Colors (4),     Shine => default_Shine),
               6 => (Site =>  lower_Sites (5),  Normal => -Normal,   Color => +Self.upper_Face.Colors (5),     Shine => default_Shine),
               7 => (Site =>  lower_Sites (6),  Normal => -Normal,   Color => +Self.upper_Face.Colors (6),     Shine => default_Shine));
      begin
         lower_Face := new_hexagon_Face (Vertices => the_Vertices'Access,
                                         Flip     => True);
      end;


      --  Shaft
      --
      declare
         type shaft_Normals is array (1 .. 6) of Vector_3;


         function get_Normals return shaft_Normals
         is
            use linear_Algebra_3D;

            Rotation   : constant Matrix_3x3   := y_Rotation_from (to_Radians (60.0));
            the_Normal :          Vector_3     := (0.0, 0.0, -1.0);
            Result     :          shaft_Normals;
         begin
            Result (2) := the_Normal;

            the_Normal := Rotation * the_Normal;
            Result (3) := the_Normal;

            the_Normal := Rotation * the_Normal;
            Result (4) := the_Normal;

            the_Normal := (0.0, 0.0, 1.0);
            Result (5) := the_Normal;

            the_Normal := Rotation * the_Normal;
            Result (6) := the_Normal;

            the_Normal := Rotation * the_Normal;
            Result (1) := the_Normal;

            return Result;
         end get_Normals;


         Normals     : constant shaft_Normals :=  get_Normals;
         shaft_Color : constant rgba_Color    := +Self.Shaft.Color;

         the_Vertices_1 : aliased Geometry.lit_colored.Vertex_array
           := (1  => (Site => upper_Sites (1),   Normal => Normals (1),   Color => shaft_Color,   Shine => default_Shine),
               2  => (Site => lower_Sites (1),   Normal => Normals (1),   Color => shaft_Color,   Shine => default_Shine),
               3  => (Site => upper_Sites (2),   Normal => Normals (1),   Color => shaft_Color,   Shine => default_Shine),
               4  => (Site => lower_Sites (2),   Normal => Normals (1),   Color => shaft_Color,   Shine => default_Shine));

         the_Vertices_2 : aliased Geometry.lit_colored.Vertex_array
           := (1  => (Site => upper_Sites (2),   Normal => Normals (2),   Color => shaft_Color,   Shine => default_Shine),
               2  => (Site => lower_Sites (2),   Normal => Normals (2),   Color => shaft_Color,   Shine => default_Shine),
               3  => (Site => upper_Sites (3),   Normal => Normals (2),   Color => shaft_Color,   Shine => default_Shine),
               4  => (Site => lower_Sites (3),   Normal => Normals (2),   Color => shaft_Color,   Shine => default_Shine));

         the_Vertices_3 : aliased Geometry.lit_colored.Vertex_array
           := (1  => (Site => upper_Sites (3),   Normal => Normals (3),   Color => shaft_Color,   Shine => default_Shine),
               2  => (Site => lower_Sites (3),   Normal => Normals (3),   Color => shaft_Color,   Shine => default_Shine),
               3  => (Site => upper_Sites (4),   Normal => Normals (3),   Color => shaft_Color,   Shine => default_Shine),
               4  => (Site => lower_Sites (4),   Normal => Normals (3),   Color => shaft_Color,   Shine => default_Shine));

         the_Vertices_4 : aliased Geometry.lit_colored.Vertex_array
           := (1  => (Site => upper_Sites (4),   Normal => Normals (4),   Color => shaft_Color,   Shine => default_Shine),
               2  => (Site => lower_Sites (4),   Normal => Normals (4),   Color => shaft_Color,   Shine => default_Shine),
               3  => (Site => upper_Sites (5),   Normal => Normals (4),   Color => shaft_Color,   Shine => default_Shine),
               4  => (Site => lower_Sites (5),   Normal => Normals (4),   Color => shaft_Color,   Shine => default_Shine));

         the_Vertices_5 : aliased Geometry.lit_colored.Vertex_array
           := (1  => (Site => upper_Sites (5),   Normal => Normals (5),   Color => shaft_Color,   Shine => default_Shine),
               2  => (Site => lower_Sites (5),   Normal => Normals (5),   Color => shaft_Color,   Shine => default_Shine),
               3  => (Site => upper_Sites (6),   Normal => Normals (5),   Color => shaft_Color,   Shine => default_Shine),
               4  => (Site => lower_Sites (6),   Normal => Normals (5),   Color => shaft_Color,   Shine => default_Shine));

         the_Vertices_6 : aliased Geometry.lit_colored.Vertex_array
           := (1  => (Site => upper_Sites (6),   Normal => Normals (6),   Color => shaft_Color,   Shine => default_Shine),
               2  => (Site => lower_Sites (6),   Normal => Normals (6),   Color => shaft_Color,   Shine => default_Shine),
               3  => (Site => upper_Sites (1),   Normal => Normals (6),   Color => shaft_Color,   Shine => default_Shine),
               4  => (Site => lower_Sites (1),   Normal => Normals (6),   Color => shaft_Color,   Shine => default_Shine));

         the_Vertices  : constant array (1 .. 6) of access Geometry.lit_colored.Vertex_array
           := (the_Vertices_1'Access,
               the_Vertices_2'Access,
               the_Vertices_3'Access,
               the_Vertices_4'Access,
               the_Vertices_5'Access,
               the_Vertices_6'Access);
      begin
         for i in shaft_Faces'Range
         loop
            shaft_Faces (i) := new_shaft_Face (vertices => the_Vertices (i));
         end loop;
      end;

      return (1 => upper_Face     .all'Access,
              2 => lower_Face     .all'Access,
              3 => shaft_Faces (1).all'Access,
              4 => shaft_Faces (2).all'Access,
              5 => shaft_Faces (3).all'Access,
              6 => shaft_Faces (4).all'Access,
              7 => shaft_Faces (5).all'Access,
              8 => shaft_Faces (6).all'Access);

   end to_GL_Geometries;


end openGL.Model.Hexagon_Column.lit_colored_faceted;
