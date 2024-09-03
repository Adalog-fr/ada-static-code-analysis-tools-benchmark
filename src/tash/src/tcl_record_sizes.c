/*
 *  Tash is free software; you can redistribute it and/or modify it
 *  under terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 2, or (at your option)
 *  any later version. Tash is distributed in the hope that it will be
 *  useful, but WITHOUT ANY WARRANTY; without even the implied
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
 *  PURPOSE. See the GNU General Public License for more details. You
 *  should have received a copy of the GNU General Public License
 *  distributed with Tash; see file COPYING. If not, write to
 *
 *          Free Software Foundation
 *          59 Temple Place - Suite 330
 *          Boston, MA 02111-1307, USA
 */

/*
 * Created during configuration. Determines sizes of various structs whose
 * contents are "private" (or would be, if C supported it!)
 */

#include <stddef.h>
#include <stdio.h>
#include <tcl.h>

#define TYPE_ALIGNMENT(t) offsetof(struct { char x; t test; }, test) 

int main()
{
  printf("--  Tash is free software; you can redistribute it and/or modify it\n");
  printf("--  under terms of the GNU General Public License as published by the\n");
  printf("--  Free Software Foundation; either version 2, or (at your option)\n");
  printf("--  any later version. Tash is distributed in the hope that it will be\n");
  printf("--  useful, but WITHOUT ANY WARRANTY; without even the implied\n");
  printf("--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR\n");
  printf("--  PURPOSE. See the GNU General Public License for more details. You\n");
  printf("--  should have received a copy of the GNU General Public License\n");
  printf("--  distributed with Tash; see file COPYING. If not, write to\n");
  printf("--\n");
  printf("--          Free Software Foundation\n");
  printf("--          59 Temple Place - Suite 330\n");
  printf("--          Boston, MA 02111-1307, USA\n");
  printf("--\n");
  printf("--  As a special exception, if other files instantiate generics from this\n");
  printf("--  unit, or you link this unit with other files to produce an executable,\n");
  printf("--  this unit does not by itself cause the resulting executable to be\n");
  printf("--  covered by the GNU General Public License. This exception does not\n");
  printf("--  however invalidate any other reasons why the executable file might be\n");
  printf("--  covered by the GNU Public License.\n");
  printf("\n");
  printf("package Tcl_Record_Sizes is\n");

  printf("\n");
  printf("   --  Size macros defined in tcl.h.\n");

  printf("\n");
  printf("   NUM_STATIC_TOKENS : constant := %d;\n",
         NUM_STATIC_TOKENS);
  printf("\n");
  printf("   TCL_DSTRING_STATIC_SIZE : constant := %d;\n",
         TCL_DSTRING_STATIC_SIZE);
  printf("\n");
  printf("   --  Sizes of structs defined in tcl.h.\n");

  printf("\n");
  printf("   Tcl_CallFrame_Size : constant := %d;\n",
           sizeof(struct Tcl_CallFrame));
  printf("   Tcl_CallFrame_Alignment : constant := %d;\n",
         TYPE_ALIGNMENT(struct Tcl_CallFrame));

  printf("\n");
  printf("   Tcl_HashTable_Size : constant := %d;\n",
           sizeof(struct Tcl_HashTable));
  printf("   Tcl_HashTable_Alignment : constant := %d;\n",
         TYPE_ALIGNMENT(struct Tcl_HashTable));

  printf("\n");
  printf("   Tcl_HashSearch_Size : constant := %d;\n",
           sizeof(struct Tcl_HashSearch));
  printf("   Tcl_HashSearch_Alignment : constant := %d;\n",
         TYPE_ALIGNMENT(struct Tcl_HashSearch));

  printf("\n");
  printf("   Tcl_Interp_Size : constant := %d;\n",
           sizeof(struct Tcl_Interp));
  printf("   Tcl_Interp_Alignment : constant := %d;\n",
         TYPE_ALIGNMENT(struct Tcl_Interp));

  printf("\n");
  printf("   Tcl_SavedResult_Size : constant := %d;\n",
           sizeof(struct Tcl_SavedResult));
  printf("   Tcl_SavedResult_Alignment : constant := %d;\n",
         TYPE_ALIGNMENT(struct Tcl_SavedResult));

  printf("\n");
  printf("end Tcl_Record_Sizes;\n");
  return 0;
}
