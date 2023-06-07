-- Copyright (c) 2022 nytpu <alex [at] nytpu.com>
-- SPDX-License-Identifier: BSD-2-Clause
-- For more license details, see LICENSE.
--
-- Derived from:
-- Copyright 2011-2021 Howard Chu, Symas Corp. All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted only as authorized by the OpenLDAP
-- Public License.
--
-- A copy of this license is available in the file LICENSE.lmdb in the
-- top-level directory of the distribution or, alternatively, at
-- <http://www.OpenLDAP.org/license.html>.
--
-- Derived From:
-- This code is derived from btree.c written by Martin Hedenfalk.
--
-- Copyright (c) 2009, 2010 Martin Hedenfalk <martin@bzero.se>
--
-- Permission to use, copy, modify, and distribute this software for any
-- purpose with or without fee is hereby granted, provided that the above
-- copyright notice and this permission notice appear in all copies.
--
-- THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
-- WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
-- MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
-- ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
-- WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
-- ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
-- OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

pragma Ada_2012;

pragma Style_Checks (Off);
pragma Warnings (Off, "-gnatwu");

with Ada.Streams;  use Ada.Streams;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;  use Interfaces.C.Strings;
with Interfaces.C.Pointers;

package lmdb_h is
	pragma Preelaborate;

	-- Version macros
	MDB_VERSION_MAJOR : constant := 0;
	MDB_VERSION_MINOR : constant := 9;
	MDB_VERSION_PATCH : constant := 70;
	MDB_VERSION_DATE : aliased constant String := "December 19, 2015" & ASCII.NUL;

	--  MDB_VERSION_STRING : aliased constant String :=
	--  	Natural'Image(MDB_VERSION_MAJOR) & "." &
	--  	Natural'Image(MDB_VERSION_MINOR) & "." &
	--  	Natural'Image(MDB_VERSION_PATCH) &
	--  	": (" & MDB_VERSION_DATE & ")";

	-- Environment flags
	MDB_FIXEDMAP : constant := 16#01#;
	MDB_NOSUBDIR : constant := 16#4000#;
	MDB_NOSYNC : constant := 16#10000#;
	MDB_RDONLY : constant := 16#20000#;
	MDB_NOMETASYNC : constant := 16#40000#;
	MDB_WRITEMAP : constant := 16#80000#;
	MDB_MAPASYNC : constant := 16#100000#;
	MDB_NOTLS : constant := 16#200000#;
	MDB_NOLOCK : constant := 16#400000#;
	MDB_NORDAHEAD : constant := 16#800000#;
	MDB_NOMEMINIT : constant := 16#1000000#;
	MDB_PREVSNAPSHOT : constant := 16#2000000#;

	-- Database flags
	MDB_REVERSEKEY : constant := 16#02#;
	MDB_DUPSORT : constant := 16#04#;
	MDB_INTEGERKEY : constant := 16#08#;
	MDB_DUPFIXED : constant := 16#10#;
	MDB_INTEGERDUP : constant := 16#20#;
	MDB_REVERSEDUP : constant := 16#40#;
	MDB_CREATE : constant := 16#40000#;

	-- Write flags
	MDB_NOOVERWRITE : constant := 16#10#;
	MDB_NODUPDATA : constant := 16#20#;
	MDB_CURRENT : constant := 16#40#;
	MDB_RESERVE : constant := 16#10000#;
	MDB_APPEND : constant := 16#20000#;
	MDB_APPENDDUP : constant := 16#40000#;
	MDB_MULTIPLE : constant := 16#80000#;

	-- Copy flags
	MDB_CP_COMPACT : constant := 16#01#;

	-- Return codes
	MDB_SUCCESS : constant := 0;
	MDB_KEYEXIST : constant := (-30799);
	MDB_NOTFOUND : constant := (-30798);
	MDB_PAGE_NOTFOUND : constant := (-30797);
	MDB_CORRUPTED : constant := (-30796);
	MDB_PANIC : constant := (-30795);
	MDB_VERSION_MISMATCH : constant := (-30794);
	MDB_INVALID : constant := (-30793);
	MDB_MAP_FULL : constant := (-30792);
	MDB_DBS_FULL : constant := (-30791);
	MDB_READERS_FULL : constant := (-30790);
	MDB_TLS_FULL : constant := (-30789);
	MDB_TXN_FULL : constant := (-30788);
	MDB_CURSOR_FULL : constant := (-30787);
	MDB_PAGE_FULL : constant := (-30786);
	MDB_MAP_RESIZED : constant := (-30785);
	MDB_INCOMPATIBLE : constant := (-30784);
	MDB_BAD_RSLOT : constant := (-30783);
	MDB_BAD_TXN : constant := (-30782);
	MDB_BAD_VALSIZE : constant := (-30781);
	MDB_BAD_DBI : constant := (-30780);
	MDB_PROBLEM : constant := (-30779);

	type MDB_cursor_op is
		(MDB_FIRST,
		MDB_FIRST_DUP,
		MDB_GET_BOTH,
		MDB_GET_BOTH_RANGE,
		MDB_GET_CURRENT,
		MDB_GET_MULTIPLE,
		MDB_LAST,
		MDB_LAST_DUP,
		MDB_NEXT,
		MDB_NEXT_DUP,
		MDB_NEXT_MULTIPLE,
		MDB_NEXT_NODUP,
		MDB_PREV,
		MDB_PREV_DUP,
		MDB_PREV_NODUP,
		MDB_SET,
		MDB_SET_KEY,
		MDB_SET_RANGE,
		MDB_PREV_MULTIPLE)
		with Convention => C;

	type MDB_env_record is null record;
	type MDB_env is access MDB_env_record;

	type MDB_txn_record is null record;
	type MDB_txn is access MDB_txn_record;

	subtype MDB_dbi is unsigned;

	type MDB_cursor_record is null record;
	type MDB_cursor is access MDB_cursor_record;

	-- Borrows memory! The underlying pointer will become invalid if the
	-- underlying Stream_Element_Array becomes invalid.
	package Stream_Element_Array_Pointers is new Interfaces.C.Pointers (
		Index => Stream_Element_Offset, Element => Stream_Element,
		Element_Array => Stream_Element_Array,
		-- Terminated arrays are unused but still need to provide this...
		Default_Terminator => Stream_Element'First
	);

	type MDB_val is record
		mv_size : aliased size_t;
		mv_data : Stream_Element_Array_Pointers.Pointer;
	end record
		with Convention => C_Pass_By_Copy;

	type MDB_cmp_func is
		access function
			(a : access constant MDB_val; b : access constant MDB_val)
			return int
			with Convention => C;

	type MDB_stat is record
		ms_psize : aliased unsigned;
		ms_depth : aliased unsigned;
		ms_branch_pages : aliased size_t;
		ms_leaf_pages : aliased size_t;
		ms_overflow_pages : aliased size_t;
		ms_entries : aliased size_t;
	end record
		with Convention => C_Pass_By_Copy;

	--  type MDB_envinfo is record
	--  	me_mapaddr : System.Address;
	--  	me_mapsize : aliased size_t;
	--  	me_last_pgno : aliased size_t;
	--  	me_last_txnid : aliased size_t;
	--  	me_maxreaders : aliased unsigned;
	--  	me_numreaders : aliased unsigned;
	--  end record
	--  	with Convention => C_Pass_By_Copy;

	function mdb_strerror (err : int) return chars_ptr
		with Import => True, Convention => C,
			External_Name => "mdb_strerror";

	function mdb_env_create (env : access MDB_env) return int
		with Import => True, Convention => C,
			External_Name => "mdb_env_create";

	function mdb_env_open
		(env : MDB_env; path : chars_ptr; flags : unsigned; mode : unsigned)
		return int
		with Import => True, Convention => C,
			External_Name => "mdb_env_open";

	function mdb_env_copy2
		(env : MDB_env; path : chars_ptr; flags : unsigned) return int
		with Import => True, Convention => C,
			External_Name => "mdb_env_copy2";

	function mdb_env_stat (env : MDB_env; stat : access MDB_stat) return int
		with Import => True, Convention => C,
			External_Name => "mdb_env_stat";

	--  function mdb_env_info (env : MDB_env; stat : MDB_envinfo) return int
	--  	with Import => True, Convention => C,
	--  		External_Name => "mdb_env_info";

	function mdb_env_sync (env : MDB_env; force : int) return int
		with Import => True, Convention => C,
			External_Name => "mdb_env_sync";

	procedure mdb_env_close (env : MDB_env)
		with Import => True, Convention => C,
			External_Name => "mdb_env_close";

	function mdb_env_set_flags
		(env : MDB_env; flags : unsigned; onoff : int)
		return int
		with Import => True, Convention => C,
			External_Name => "mdb_env_set_flags";

	function mdb_env_get_flags
		(env : MDB_env; flags : access unsigned) return int
		with Import => True, Convention => C,
			External_Name => "mdb_env_get_flags";

	function mdb_env_get_path
		(env : MDB_env; path : access chars_ptr) return int
		with Import => True, Convention => C,
			External_Name => "mdb_env_get_path";

	function mdb_env_set_mapsize (env : MDB_env; size : size_t) return int
		with Import => True, Convention => C,
			External_Name => "mdb_env_set_mapsize";

	function mdb_env_set_maxreaders (env : MDB_env; readers : unsigned) return int
		with Import => True, Convention => C,
			External_Name => "mdb_env_set_maxreaders";

	function mdb_env_get_maxreaders
		(env : MDB_env; readers : access unsigned) return int
		with Import => True, Convention => C,
			External_Name => "mdb_env_get_maxreaders";

	function mdb_env_set_maxdbs (env : MDB_env; dbs : MDB_dbi) return int
		with Import => True, Convention => C,
			External_Name => "mdb_env_set_maxdbs";

	function mdb_env_get_maxkeysize (env : MDB_env) return int
		with Import => True, Convention => C,
			External_Name => "mdb_env_get_maxkeysize";

	function mdb_txn_begin
		(env : MDB_env; parent : MDB_txn; flags : unsigned;
		txn : access MDB_txn)
		return int
		with Import => True, Convention => C,
			External_Name => "mdb_txn_begin";

	function mdb_txn_env (txn : MDB_txn) return MDB_env
		with Import => True, Convention => C,
			External_Name => "mdb_txn_env";

	function mdb_txn_id (txn : MDB_txn) return size_t
		with Import => True, Convention => C,
			External_Name => "mdb_txn_id";

	function mdb_txn_commit (txn : MDB_txn) return int
		with Import => True, Convention => C,
			External_Name => "mdb_txn_commit";

	procedure mdb_txn_abort (txn : MDB_txn)
		with Import => True, Convention => C,
			External_Name => "mdb_txn_abort";

	procedure mdb_txn_reset (txn : MDB_txn)
		with Import => True, Convention => C,
			External_Name => "mdb_txn_reset";

	function mdb_txn_renew (txn : MDB_txn) return int
		with Import => True, Convention => C,
			External_Name => "mdb_txn_renew";

	function mdb_dbi_open
		(txn : MDB_txn; name : chars_ptr;
		flags : unsigned; dbi : access MDB_dbi)
		return int
		with Import => True, Convention => C,
			External_Name => "mdb_dbi_open";

	function mdb_stat_func
		(txn : MDB_txn; dbi : MDB_dbi; stat : access MDB_stat) return int
		with Import => True, Convention => C,
			External_Name => "mdb_stat";

	function mdb_dbi_flags
		(txn : MDB_txn; dbi : MDB_dbi; flags : access unsigned) return int
		with Import => True, Convention => C,
			External_Name => "mdb_dbi_flags";

	procedure mdb_dbi_close (env : MDB_env; dbi : MDB_dbi)
		with Import => True, Convention => C,
			External_Name => "mdb_dbi_close";

	function mdb_drop
		(txn : MDB_txn; dbi : MDB_dbi; del : int) return int
		with Import => True, Convention => C,
			External_Name => "mdb_drop";

	function mdb_set_compare
		(txn : MDB_txn; dbi : MDB_dbi; cmp : MDB_cmp_func) return int
		with Import => True, Convention => C,
			External_Name => "mdb_set_compare";

	function mdb_set_dupsort
		(txn : MDB_txn; dbi : MDB_dbi; cmp : MDB_cmp_func) return int
		with Import => True, Convention => C,
			External_Name => "mdb_set_dupsort";

	function mdb_get
		(txn : MDB_txn; dbi : MDB_dbi; key : access MDB_val;
		data : access MDB_val)
		return int
		with Import => True, Convention => C,
			External_Name => "mdb_get";

	function mdb_put
		(txn : MDB_txn; dbi : MDB_dbi;
		key : access MDB_val; data : access MDB_val;
		flags : unsigned)
		return int
		with Import => True, Convention => C,
			External_Name => "mdb_put";

	function mdb_del
		(txn : MDB_txn; dbi : MDB_dbi;
		key : access MDB_val; data : access MDB_val)
		return int
		with Import => True, Convention => C,
			External_Name => "mdb_del";

	function mdb_cursor_open
		(txn : MDB_txn; dbi : MDB_dbi; cursor : access MDB_cursor) return int
		with Import => True, Convention => C,
			External_Name => "mdb_cursor_open";

	procedure mdb_cursor_close (cursor : MDB_cursor)
		with Import => True, Convention => C,
			External_Name => "mdb_cursor_close";

	function mdb_cursor_renew (txn : MDB_txn; cursor : MDB_cursor) return int
		with Import => True, Convention => C,
			External_Name => "mdb_cursor_renew";

	function mdb_cursor_txn (cursor : MDB_cursor) return MDB_txn
		with Import => True, Convention => C,
			External_Name => "mdb_cursor_txn";

	function mdb_cursor_dbi (cursor : MDB_cursor) return MDB_dbi
		with Import => True, Convention => C,
			External_Name => "mdb_cursor_dbi";

	function mdb_cursor_get
		(cursor : MDB_cursor; key : access MDB_val; data : access MDB_val;
		op : MDB_cursor_op)
		return int
		with Import => True, Convention => C,
			External_Name => "mdb_cursor_get";

	function mdb_cursor_put
		(cursor : MDB_cursor; key : access MDB_val; data : access MDB_val;
		flags : unsigned)
		return int
		with Import => True, Convention => C,
			External_Name => "mdb_cursor_put";

	function mdb_cursor_del
		(cursor : MDB_cursor; flags : unsigned) return int
		with Import => True, Convention => C,
			External_Name => "mdb_cursor_del";

	function mdb_cursor_count
		(cursor : MDB_cursor; countp : access size_t) return int
		with Import => True, Convention => C,
			External_Name => "mdb_cursor_count";

	function mdb_cmp
		(txn : MDB_txn; dbi : MDB_dbi;
		a : access constant MDB_val; b : access constant MDB_val)
		return int
		with Import => True, Convention => C,
			External_Name => "mdb_cmp";

	function mdb_dcmp
		(txn : MDB_txn; dbi : MDB_dbi;
		a : access constant MDB_val; b : access constant MDB_val)
		return int
		with Import => True, Convention => C,
			External_Name => "mdb_dcmp";

	function mdb_reader_check (env : MDB_env; dead : access int) return int
		with Import => True, Convention => C,
			External_Name => "mdb_reader_check";

end lmdb_h;

pragma Style_Checks (On);
pragma Warnings (On, "-gnatwu");
