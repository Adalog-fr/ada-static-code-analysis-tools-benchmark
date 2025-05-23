--//////////////////////////////////////////////////////////
-- SFML - Simple and Fast Multimedia Library
-- Copyright (C) 2007-2015 Laurent Gomila (laurent@sfml-dev.org)
-- This software is provided 'as-is', without any express or implied warranty.
-- In no event will the authors be held liable for any damages arising from the use of this software.
-- Permission is granted to anyone to use this software for any purpose,
-- including commercial applications, and to alter it and redistribute it freely,
-- subject to the following restrictions:
-- 1. The origin of this software must not be misrepresented;
--    you must not claim that you wrote the original software.
--    If you use this software in a product, an acknowledgment
--    in the product documentation would be appreciated but is not required.
-- 2. Altered source versions must be plainly marked as such,
--    and must not be misrepresented as being the original software.
-- 3. This notice may not be removed or altered from any source distribution.
--//////////////////////////////////////////////////////////


package Sf.Graphics.PrimitiveType is

   --//////////////////////////////////////////////////////////
   --/ @brief Types of primitives that a sf::VertexArray can render
   --/
   --/ Points and lines have no area, therefore their thickness
   --/ will always be 1 pixel, regardless the current transform
   --/ and view.
   --/
   --//////////////////////////////////////////////////////////
   --/< List of individual points
   --/< List of individual lines
   --/< List of connected lines, a point uses the previous point to form a line
   --/< List of individual triangles
   --/< List of connected triangles, a point uses the two previous points to form a triangle
   --/< List of connected triangles, a point uses the common center and the previous point to form a triangle
   --/< List of individual quads
   --/< @deprecated Use sfLineStrip instead
   --/< @deprecated Use sfTriangleStrip instead
   --/< @deprecated Use sfTriangleFan instead
   subtype sfPrimitiveType is sfUint32;
   sfPoints : constant sfPrimitiveType := 0;
   sfLines : constant sfPrimitiveType := 1;
   sfLineStrip : constant sfPrimitiveType := 2;
   sfTriangles : constant sfPrimitiveType := 3;
   sfTriangleStrip : constant sfPrimitiveType := 4;
   sfTriangleFan : constant sfPrimitiveType := 5;
   sfQuads : constant sfPrimitiveType := 6;
   sfLinesStrip : constant sfPrimitiveType := 2;
   sfTrianglesStrip : constant sfPrimitiveType := 4;
   sfTrianglesFan : constant sfPrimitiveType := 5;

end Sf.Graphics.PrimitiveType;
