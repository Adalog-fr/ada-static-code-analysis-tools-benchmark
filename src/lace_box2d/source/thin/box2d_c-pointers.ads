-- This file is generated by SWIG. Please do *not* modify by hand.
--
with interfaces.C;



package box2d_c.Pointers is



   -- Shape_Pointer
   --
   type Shape_Pointer is access all box2d_c.Shape;

   -- Shape_Pointers
   --
   type Shape_Pointers is array (interfaces.C.Size_t range <>) of aliased box2d_c.Pointers.Shape_Pointer;



   -- Object_Pointer
   --
   type Object_Pointer is access all box2d_c.Object;

   -- Object_Pointers
   --
   type Object_Pointers is array (interfaces.C.Size_t range <>) of aliased box2d_c.Pointers.Object_Pointer;



   -- Joint_Pointer
   --
   type Joint_Pointer is access all box2d_c.Joint;

   -- Joint_Pointers
   --
   type Joint_Pointers is array (interfaces.C.Size_t range <>) of aliased box2d_c.Pointers.Joint_Pointer;



   -- Space_Pointer
   --
   type Space_Pointer is access all box2d_c.Space;

   -- Space_Pointers
   --
   type Space_Pointers is array (interfaces.C.Size_t range <>) of aliased box2d_c.Pointers.Space_Pointer;



   -- b2Joint_Pointer
   --
   type b2Joint_Pointer is access all box2d_c.b2Joint;

   -- b2Joint_Pointers
   --
   type b2Joint_Pointers is array (interfaces.C.Size_t range <>) of aliased box2d_c.Pointers.b2Joint_Pointer;










end box2d_c.Pointers;