/*
 * Copyright (C) 2014-2022, AdaCore
 * SPDX-License-Identifier: Apache-2.0
 */

/* The following will ask GDB to load our GDB initialization script for Langkit
   GDB helpers.  Note that ASM bits are architecture-specific, so support only
   Linux for now.  */

#if defined(DEBUG) && defined(linux)
asm(
".pushsection \".debug_gdb_scripts\", \"MS\",@progbits,1\n"
".byte 1 /* Python */\n"
".asciz \"/tmp/libadalang-23.0.0/gdbinit.py\"\n"
".popsection\n"
);
#endif
