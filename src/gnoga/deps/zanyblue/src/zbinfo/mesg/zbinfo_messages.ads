--  -*- coding: utf-8 -*-
--
--  Ada specification generated by ZBMCompile, V1.4.0 (r3199).
--  This is a generated file and should not be edited.
--

with ZanyBlue.Text.Catalogs;
with ZanyBlue.Text.Formatting;
with UXStrings;

package ZBInfo_Messages is

   use UXStrings;
   subtype String is UXString;

   --  Since the body calls the Initialize procedure, this package need
   --  only be with'ed.  Suppress any compiler warnings about an unused
   --  with'ed package.
   pragma Warnings (Off, ZBInfo_Messages);

   function Standard_Catalog return ZanyBlue.Text.Catalogs.Catalog_Type
      renames ZanyBlue.Text.Formatting.Standard_Catalog;

   function Facility (Index : Positive) return String;
   --  The name of the Index'th Facility item compiled.
   --  This routine is normally only used by generated accessor code.

   function Facility_Count return Natural;
   --  Numnber of Facility items compiled.
   --  This routine is normally only used by generated accessor code.

   function Key (Index : Positive) return String;
   --  The name of the Index'th Key item compiled.
   --  This routine is normally only used by generated accessor code.

   function Key_Count return Natural;
   --  Numnber of Key items compiled.
   --  This routine is normally only used by generated accessor code.

   procedure Initialize (
      Catalog : ZanyBlue.Text.Catalogs.Catalog_Type := Standard_Catalog);
   --  The catalog initialization routine: load the compiled messages into
   --  a catalog.

end ZBInfo_Messages;