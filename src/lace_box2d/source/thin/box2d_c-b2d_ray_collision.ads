-- This file is generated by SWIG. Please do *not* modify by hand.
--
with c_math_c;
with c_math_c.Vector_3;
with interfaces.C;



package box2d_c.b2d_ray_Collision is

   -- Item
   --

type Item is
      record
         near_Object : access box2d_c.Object;
         hit_Fraction : aliased c_math_c.Real;
         Normal_world : aliased c_math_c.Vector_3.Item;
         Site_world : aliased c_math_c.Vector_3.Item;
      end record;



   -- Items
   --
   type Items is array (interfaces.C.Size_t range <>) of aliased box2d_c.b2d_ray_Collision.Item;



   -- Pointer
   --
   type Pointer is access all box2d_c.b2d_ray_Collision.Item;

   -- Pointers
   --
   type Pointers is array (interfaces.C.Size_t range <>) of aliased box2d_c.b2d_ray_Collision.Pointer;



   -- Pointer_Pointer
   --
   type Pointer_Pointer is access all box2d_c.b2d_ray_Collision.Pointer;






   function  construct  return box2d_c.b2d_ray_Collision.Item;









private



   pragma Import (C, construct, "Ada_new_b2d_ray_Collision");



end box2d_c.b2d_ray_Collision;