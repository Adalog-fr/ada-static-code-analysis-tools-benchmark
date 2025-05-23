pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with vadefs_h;
with System;
with umingw_h;
with umingw_off_t_h;

package stdio_h is


   BUFSIZ : constant := 512;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:19

   EOF : constant := (-1);  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:23
   --  unsupported macro: L_tmpnam (sizeof(_P_tmpdir) + 12)

   SEEK_CUR : constant := 1;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:55
   SEEK_END : constant := 2;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:56
   SEEK_SET : constant := 0;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:57

   STDIN_FILENO : constant := 0;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:59
   STDOUT_FILENO : constant := 1;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:60
   STDERR_FILENO : constant := 2;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:61

   FILENAME_MAX : constant := 260;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:63
   FOPEN_MAX : constant := 20;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:64

   TMP_MAX : constant := 32767;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:66
   --  unsupported macro: stdin (&__iob_func()[0])
   --  unsupported macro: stdout (&__iob_func()[1])
   --  unsupported macro: stderr (&__iob_func()[2])
   --  unsupported macro: popen _popen
   --  unsupported macro: pclose _pclose
   --  unsupported macro: WEOF (wint_t)(0xFFFF)
   --  unsupported macro: wpopen _wpopen
   --  arg-macro: procedure getwchar ()
   --    fgetwc(stdin)
   --  arg-macro: procedure putwchar (_c)
   --    fputwc((_c),stdout)
   --  arg-macro: procedure getwc (_stm)
   --    fgetwc(_stm)
   --  arg-macro: procedure putwc (_c, _stm)
   --    fputwc(_c,_stm)
   --  unsupported macro: P_tmpdir _P_tmpdir
   --  unsupported macro: SYS_OPEN _SYS_OPEN

   type u_iobuf is record
      u_ptr : Interfaces.C.Strings.chars_ptr;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:27
      u_cnt : aliased int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:28
      u_base : Interfaces.C.Strings.chars_ptr;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:29
      u_flag : aliased int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:30
      u_file : aliased int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:31
      u_charbuf : aliased int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:32
      u_bufsiz : aliased int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:33
      u_tmpfname : Interfaces.C.Strings.chars_ptr;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:34
   end record;
   pragma Convention (C_Pass_By_Copy, u_iobuf);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:26

   subtype FILE is u_iobuf;

   subtype fpos_t is Long_Long_Integer;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:103

   function fprintf (u_File : access FILE; u_Format : Interfaces.C.Strings.chars_ptr  -- , ...
      ) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:348
   pragma Import (C, fprintf, "fprintf");

   function printf (u_Format : Interfaces.C.Strings.chars_ptr  -- , ...
      ) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:349
   pragma Import (C, printf, "printf");

   function sprintf (u_Dest : Interfaces.C.Strings.chars_ptr; u_Format : Interfaces.C.Strings.chars_ptr  -- , ...
      ) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:350
   pragma Import (C, sprintf, "sprintf");

   function vfprintf
     (u_File : access FILE;
      u_Format : Interfaces.C.Strings.chars_ptr;
      u_ArgList : vadefs_h.va_list) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:352
   pragma Import (C, vfprintf, "vfprintf");

   function vprintf (u_Format : Interfaces.C.Strings.chars_ptr; u_ArgList : vadefs_h.va_list) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:353
   pragma Import (C, vprintf, "vprintf");

   function vsprintf
     (u_Dest : Interfaces.C.Strings.chars_ptr;
      u_Format : Interfaces.C.Strings.chars_ptr;
      u_Args : vadefs_h.va_list) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:354
   pragma Import (C, vsprintf, "vsprintf");

   function fscanf (u_File : access FILE; u_Format : Interfaces.C.Strings.chars_ptr  -- , ...
      ) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:356
   pragma Import (C, fscanf, "fscanf");

   function scanf (u_Format : Interfaces.C.Strings.chars_ptr  -- , ...
      ) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:357
   pragma Import (C, scanf, "scanf");

   function sscanf (u_Src : Interfaces.C.Strings.chars_ptr; u_Format : Interfaces.C.Strings.chars_ptr  -- , ...
      ) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:358
   pragma Import (C, sscanf, "sscanf");

   function vscanf (Format : Interfaces.C.Strings.chars_ptr; argp : vadefs_h.va_list) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:360
   pragma Import (C, vscanf, "vscanf");

   function vfscanf
     (fp : access FILE;
      Format : Interfaces.C.Strings.chars_ptr;
      argp : vadefs_h.va_list) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:361
   pragma Import (C, vfscanf, "vfscanf");

   function vsscanf
     (u_Str : Interfaces.C.Strings.chars_ptr;
      Format : Interfaces.C.Strings.chars_ptr;
      argp : vadefs_h.va_list) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:362
   pragma Import (C, vsscanf, "vsscanf");

   --  skipped func _filbuf

   --  skipped func _flsbuf

   --  skipped func _fsopen

   procedure clearerr (u_File : access FILE);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:373
   pragma Import (C, clearerr, "clearerr");

   function fclose (u_File : access FILE) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:374
   pragma Import (C, fclose, "fclose");

   --  skipped func _fcloseall

   --  skipped func _fdopen

   function feof (u_File : access FILE) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:381
   pragma Import (C, feof, "feof");

   function ferror (u_File : access FILE) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:382
   pragma Import (C, ferror, "ferror");

   function fflush (u_File : access FILE) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:383
   pragma Import (C, fflush, "fflush");

   function fgetc (u_File : access FILE) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:384
   pragma Import (C, fgetc, "fgetc");

   --  skipped func _fgetchar

   function fgetpos (u_File : access FILE; u_Pos : access fpos_t) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:386
   pragma Import (C, fgetpos, "fgetpos");

   function fgetpos64 (u_File : access FILE; u_Pos : access fpos_t) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:387
   pragma Import (C, fgetpos64, "fgetpos64");

   function fgets
     (u_Buf : Interfaces.C.Strings.chars_ptr;
      u_MaxCount : int;
      u_File : access FILE) return Interfaces.C.Strings.chars_ptr;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:388
   pragma Import (C, fgets, "fgets");

   --  skipped func _fileno

   --  skipped func _tempnam

   --  skipped func _flushall

   function fopen (u_Filename : Interfaces.C.Strings.chars_ptr; u_Mode : Interfaces.C.Strings.chars_ptr) return access FILE;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:395
   pragma Import (C, fopen, "fopen");

   function fopen64 (filename : Interfaces.C.Strings.chars_ptr; mode : Interfaces.C.Strings.chars_ptr) return access FILE;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:396
   pragma Import (C, fopen64, "fopen64");

   function fputc (u_Ch : int; u_File : access FILE) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:397
   pragma Import (C, fputc, "fputc");

   --  skipped func _fputchar

   function fputs (u_Str : Interfaces.C.Strings.chars_ptr; u_File : access FILE) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:399
   pragma Import (C, fputs, "fputs");

   function fread
     (u_DstBuf : System.Address;
      u_ElementSize : umingw_h.size_t;
      u_Count : umingw_h.size_t;
      u_File : access FILE) return umingw_h.size_t;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:400
   pragma Import (C, fread, "fread");

   function freopen
     (u_Filename : Interfaces.C.Strings.chars_ptr;
      u_Mode : Interfaces.C.Strings.chars_ptr;
      u_File : access FILE) return access FILE;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:401
   pragma Import (C, freopen, "freopen");

   --  skipped func _fscanf_l

   function fsetpos (u_File : access FILE; u_Pos : access fpos_t) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:403
   pragma Import (C, fsetpos, "fsetpos");

   function fsetpos64 (u_File : access FILE; u_Pos : access fpos_t) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:404
   pragma Import (C, fsetpos64, "fsetpos64");

   function fseek
     (u_File : access FILE;
      u_Offset : long;
      u_Origin : int) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:405
   pragma Import (C, fseek, "fseek");

   function fseeko64
     (stream : access FILE;
      offset : umingw_off_t_h.u_off64_t;
      whence : int) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:409
   pragma Import (C, fseeko64, "fseeko64");

   function fseeko
     (stream : access FILE;
      offset : umingw_off_t_h.u_off_t;
      whence : int) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:410
   pragma Import (C, fseeko, "fseeko");

   function ftell (u_File : access FILE) return long;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:423
   pragma Import (C, ftell, "ftell");

   function ftello (stream : access FILE) return umingw_off_t_h.u_off_t;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:425
   pragma Import (C, ftello, "ftello");

   function ftello64 (stream : access FILE) return umingw_off_t_h.u_off64_t;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:426
   pragma Import (C, ftello64, "ftello64");

   --  skipped func _fseeki64

   --  skipped func _ftelli64

   function fwrite
     (u_Str : System.Address;
      u_Size : umingw_h.size_t;
      u_Count : umingw_h.size_t;
      u_File : access FILE) return umingw_h.size_t;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:437
   pragma Import (C, fwrite, "fwrite");

   function getc (u_File : access FILE) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:438
   pragma Import (C, getc, "getc");

   function getchar return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:439
   pragma Import (C, getchar, "getchar");

   --  skipped func _getmaxstdio

   function gets (u_Buffer : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:441
   pragma Import (C, gets, "gets");

   --  skipped func _getw

   procedure perror (u_ErrMsg : Interfaces.C.Strings.chars_ptr);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:445
   pragma Import (C, perror, "perror");

   --  skipped func _pclose

   --  skipped func _popen

   function putc (u_Ch : int; u_File : access FILE) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:453
   pragma Import (C, putc, "putc");

   function putchar (u_Ch : int) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:454
   pragma Import (C, putchar, "putchar");

   function puts (u_Str : Interfaces.C.Strings.chars_ptr) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:455
   pragma Import (C, puts, "puts");

   --  skipped func _putw

   function remove (u_Filename : Interfaces.C.Strings.chars_ptr) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:459
   pragma Import (C, remove, "remove");

   function rename (u_OldFilename : Interfaces.C.Strings.chars_ptr; u_NewFilename : Interfaces.C.Strings.chars_ptr) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:460
   pragma Import (C, rename, "rename");

   --  skipped func _unlink

   function unlink (u_Filename : Interfaces.C.Strings.chars_ptr) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:463
   pragma Import (C, unlink, "unlink");

   procedure rewind (u_File : access FILE);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:466
   pragma Import (C, rewind, "rewind");

   --  skipped func _rmtmp

   --  skipped func _scanf_l

   procedure setbuf (u_File : access FILE; u_Buffer : Interfaces.C.Strings.chars_ptr);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:469
   pragma Import (C, setbuf, "setbuf");

   --  skipped func _setmaxstdio

   --  skipped func _set_output_format

   --  skipped func _get_output_format

   function setvbuf
     (u_File : access FILE;
      u_Buf : Interfaces.C.Strings.chars_ptr;
      u_Mode : int;
      u_Size : umingw_h.size_t) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:479
   pragma Import (C, setvbuf, "setvbuf");

   --  skipped func _scprintf

   --  skipped func _sscanf_l

   --  skipped func _snscanf

   --  skipped func _snscanf_l

   function tmpfile return access FILE;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:484
   pragma Import (C, tmpfile, "tmpfile");

   function tmpnam (u_Buffer : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:485
   pragma Import (C, tmpnam, "tmpnam");

   function ungetc (u_Ch : int; u_File : access FILE) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:486
   pragma Import (C, ungetc, "ungetc");

   --  skipped func _snprintf

   --  skipped func _snprintf_l

   --  skipped func _vsnprintf

   --  skipped func _vsnprintf_l

   --  skipped func _sprintf_l

   function vsnprintf
     (d : Interfaces.C.Strings.chars_ptr;
      n : umingw_h.size_t;
      format : Interfaces.C.Strings.chars_ptr;
      arg : vadefs_h.va_list) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:501
   pragma Import (C, vsnprintf, "vsnprintf");

   function snprintf
     (s : Interfaces.C.Strings.chars_ptr;
      n : umingw_h.size_t;
      format : Interfaces.C.Strings.chars_ptr  -- , ...
      ) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:505
   pragma Import (C, snprintf, "snprintf");

   --  skipped func _vscprintf

   --  skipped func _set_printf_count_output

   --  skipped func _get_printf_count_output

   function fwscanf (u_File : access FILE; u_Format : access wchar_t  -- , ...
      ) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:692
   pragma Import (C, fwscanf, "fwscanf");

   function swscanf (u_Src : access wchar_t; u_Format : access wchar_t  -- , ...
      ) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:693
   pragma Import (C, swscanf, "swscanf");

   function wscanf (u_Format : access wchar_t  -- , ...
      ) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:694
   pragma Import (C, wscanf, "wscanf");

   function vwscanf (arg1 : access wchar_t; arg2 : vadefs_h.va_list) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:696
   pragma Import (C, vwscanf, "vwscanf");

   function vfwscanf
     (arg1 : access FILE;
      arg2 : access wchar_t;
      arg3 : vadefs_h.va_list) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:697
   pragma Import (C, vfwscanf, "vfwscanf");

   function vswscanf
     (arg1 : access wchar_t;
      arg2 : access wchar_t;
      arg3 : vadefs_h.va_list) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:698
   pragma Import (C, vswscanf, "vswscanf");

   function fwprintf (u_File : access FILE; u_Format : access wchar_t  -- , ...
      ) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:701
   pragma Import (C, fwprintf, "fwprintf");

   function wprintf (u_Format : access wchar_t  -- , ...
      ) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:702
   pragma Import (C, wprintf, "wprintf");

   function vfwprintf
     (u_File : access FILE;
      u_Format : access wchar_t;
      u_ArgList : vadefs_h.va_list) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:703
   pragma Import (C, vfwprintf, "vfwprintf");

   function vwprintf (u_Format : access wchar_t; u_ArgList : vadefs_h.va_list) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:704
   pragma Import (C, vwprintf, "vwprintf");

   function swprintf (arg1 : access wchar_t; arg2 : access wchar_t  -- , ...
      ) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:705
   pragma Import (C, swprintf, "swprintf");

   function vswprintf
     (arg1 : access wchar_t;
      arg2 : access wchar_t;
      arg3 : vadefs_h.va_list) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:706
   pragma Import (C, vswprintf, "vswprintf");

   --  skipped func _wfsopen

   function fgetwc (u_File : access FILE) return umingw_h.wint_t;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:719
   pragma Import (C, fgetwc, "fgetwc");

   --  skipped func _fgetwchar

   function fputwc (u_Ch : wchar_t; u_File : access FILE) return umingw_h.wint_t;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:721
   pragma Import (C, fputwc, "fputwc");

   --  skipped func _fputwchar

   function getwc (u_File : access FILE) return umingw_h.wint_t;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:723
   pragma Import (C, getwc, "getwc");

   function getwchar return umingw_h.wint_t;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:724
   pragma Import (C, getwchar, "getwchar");

   function putwc (u_Ch : wchar_t; u_File : access FILE) return umingw_h.wint_t;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:725
   pragma Import (C, putwc, "putwc");

   function putwchar (u_Ch : wchar_t) return umingw_h.wint_t;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:726
   pragma Import (C, putwchar, "putwchar");

   function ungetwc (u_Ch : umingw_h.wint_t; u_File : access FILE) return umingw_h.wint_t;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:727
   pragma Import (C, ungetwc, "ungetwc");

   function fgetws
     (u_Dst : access wchar_t;
      u_SizeInWords : int;
      u_File : access FILE) return access wchar_t;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:728
   pragma Import (C, fgetws, "fgetws");

   function fputws (u_Str : access wchar_t; u_File : access FILE) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:729
   pragma Import (C, fputws, "fputws");

   --  skipped func _getws

   --  skipped func _putws

   --  skipped func _scwprintf

   --  skipped func _swprintf_l

   --  skipped func _swprintf_c

   --  skipped func _vswprintf_c

   --  skipped func _snwprintf

   --  skipped func _vsnwprintf

   function snwprintf
     (s : access wchar_t;
      n : umingw_h.size_t;
      format : access wchar_t  -- , ...
      ) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:747
   pragma Import (C, snwprintf, "snwprintf");

   function vsnwprintf
     (arg1 : access wchar_t;
      arg2 : umingw_h.size_t;
      arg3 : access wchar_t;
      arg4 : vadefs_h.va_list) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:748
   pragma Import (C, vsnwprintf, "vsnwprintf");

   --  skipped func _fwprintf_p

   --  skipped func _wprintf_p

   --  skipped func _vfwprintf_p

   --  skipped func _vwprintf_p

   --  skipped func _swprintf_p

   --  skipped func _vswprintf_p

   --  skipped func _scwprintf_p

   --  skipped func _vscwprintf_p

   --  skipped func _wprintf_l

   --  skipped func _wprintf_p_l

   --  skipped func _vwprintf_l

   --  skipped func _vwprintf_p_l

   --  skipped func _fwprintf_l

   --  skipped func _fwprintf_p_l

   --  skipped func _vfwprintf_l

   --  skipped func _vfwprintf_p_l

   --  skipped func _swprintf_c_l

   --  skipped func _swprintf_p_l

   --  skipped func _vswprintf_c_l

   --  skipped func _vswprintf_p_l

   --  skipped func _scwprintf_l

   --  skipped func _scwprintf_p_l

   --  skipped func _vscwprintf_p_l

   --  skipped func _snwprintf_l

   --  skipped func _vsnwprintf_l

   --  skipped func _swprintf

   --  skipped func _vswprintf

   --  skipped func _vswprintf_l

   --  skipped func _wtempnam

   --  skipped func _vscwprintf

   --  skipped func _vscwprintf_l

   --  skipped func _fwscanf_l

   --  skipped func _swscanf_l

   --  skipped func _snwscanf

   --  skipped func _snwscanf_l

   --  skipped func _wscanf_l

   --  skipped func _wfdopen

   --  skipped func _wfopen

   --  skipped func _wfreopen

   --  skipped func _wperror

   --  skipped func _wpopen

   --  skipped func _wremove

   --  skipped func _wtmpnam

   --  skipped func _fgetwc_nolock

   --  skipped func _fputwc_nolock

   --  skipped func _ungetwc_nolock

   --  skipped func _lock_file

   --  skipped func _unlock_file

   --  skipped func _fclose_nolock

   --  skipped func _fflush_nolock

   --  skipped func _fread_nolock

   --  skipped func _fseek_nolock

   --  skipped func _ftell_nolock

   --  skipped func _fseeki64_nolock

   --  skipped func _ftelli64_nolock

   --  skipped func _fwrite_nolock

   --  skipped func _ungetc_nolock

   function tempnam (u_Directory : Interfaces.C.Strings.chars_ptr; u_FilePrefix : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:871
   pragma Import (C, tempnam, "tempnam");

   function fcloseall return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:872
   pragma Import (C, fcloseall, "fcloseall");

   function fdopen (u_FileHandle : int; u_Format : Interfaces.C.Strings.chars_ptr) return access FILE;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:873
   pragma Import (C, fdopen, "fdopen");

   function fgetchar return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:874
   pragma Import (C, fgetchar, "fgetchar");

   function fileno (u_File : access FILE) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:875
   pragma Import (C, fileno, "fileno");

   function flushall return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:876
   pragma Import (C, flushall, "flushall");

   function fputchar (u_Ch : int) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:877
   pragma Import (C, fputchar, "fputchar");

   function getw (u_File : access FILE) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:878
   pragma Import (C, getw, "getw");

   function putw (u_Ch : int; u_File : access FILE) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:879
   pragma Import (C, putw, "putw");

   function rmtmp return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/stdio.h:880
   pragma Import (C, rmtmp, "rmtmp");

end stdio_h;
