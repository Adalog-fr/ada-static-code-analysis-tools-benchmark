pragma Ada_2012;

with Ada.Streams; use Ada.Streams;

with Fastpbkdf2_Generic;

package Fastpbkdf2_Ada is new Fastpbkdf2_Generic
  (Stream_Element, Stream_Element_Offset, Stream_Element_Array);
