-- SPDX-License-Identifier: BSD-2-Clause
-- For more license details, see LICENSE.

pragma Ada_2012;

with Ada.IO_Exceptions;
with Ada.Iterator_Interfaces;
with Ada.Streams;  use Ada.Streams;
with Ada.Strings.UTF_Encoding;  use Ada.Strings.UTF_Encoding;
with Interfaces.C;  use Interfaces.C;

private with Ada.Containers.Indefinite_Holders;
private with GNATCOLL.Refcount;
private with lmdb_h;

package LMDB is

	------------------
	-- Environments --
	------------------

	-- An LMDB "environment", equivalent to a database in other DBMSes.
	-- Reference-counted, as long as at least one copy of a given Environment
	-- object exists, then the underlying environment will be kept open.
	type Environment is tagged private;

	-- Whether an environment has been opened.
	function Is_Open (Env : Environment) return Boolean;

	-- Whether the environment is mutable or not.
	function Read_Only (Env : Environment) return Boolean
		with Pre => Env.Is_Open;

	-- The maximum number of named databases allowed within an environment.
	function Max_Databases (Env : Environment) return unsigned
		with Pre => Env.Is_Open;

	-- The maximum number of reader transactions allowed within an environment.
	function Max_Readers (Env : Environment) return unsigned
		with Pre => Env.Is_Open;

	-- The size of the memory map used for the environment.
	function Map_Size (Env : Environment) return size_t
		with Pre => Env.Is_Open;

	-- Open an environment at the given file path in the given environment.
	--
	-- If the specified path does not exist and Create_If_Not_Exist is True
	-- then it will be created, otherwise Name_Error will be raised.
	--
	-- If No_Subdirectory is True then the given path will be treated as a
	-- single file rather than a directory that the environment file will be
	-- stored in.  The same value should be passed every time the database is
	-- opened.
	--
	-- If Read_Only is True then no write operations on the environment will be
	-- possible.
	--
	-- Max_Databases specifies the maximum number of named databases supported
	-- by the environment.  If the default of 0 is specified then only the
	-- unnamed database may be used.  If the number is greater than 0 then the
	-- unnamed database may NOT be used, as the unnamed database is used to
	-- store named databases.
	--
	-- Max_Readers is the maximum number of read-only transactions that may be
	-- open in the environment.
	--
	-- Map_Size is the size of the total size of the memory map to use for the
	-- environment.  It SHOULD be a multiple of the OS page size.  The
	-- value should be as large as possible to allow future growth of the
	-- database.  The default value is 1 MiB.  The value given for map size
	-- must be greater than or equal to the previous map size given every
	-- time the database is opened; if it is greater than the previous size
	-- then the database will be grown.
	function Open
		(Path : UTF_8_String;
		Create_If_Not_Exist : Boolean := False;
		No_Subdirectory : Boolean := False;
		Read_Only : Boolean := False;
		Max_Databases : unsigned := 0;
		Max_Readers : unsigned := 126;
		Map_Size : size_t := 10485760)
		return Environment
		with Post =>
			Open'Result.Is_Open and then
			(Open'Result.Read_Only = Read_Only and
			Open'Result.Max_Databases = Max_Databases and
			Open'Result.Max_Readers = Max_Readers and
			Open'Result.Map_Size = Map_Size);

	-- Close the environment.
	procedure Close (Env : in out Environment)
		with Post => not Env.Is_Open;

	-- Copy the files backing an environment to the given path.  Useful for
	-- backing up the environment.
	-- If Compact is True then LMDB will perform compaction while copying.  It
	-- will omit free pages and sequentially renumber all pages in output. This
	-- consumes more CPU and runs more slowly than the default.
	procedure Copy
		(Env : Environment; New_Path : UTF_8_String; Compact : Boolean := False)
		with Pre => Env.Is_Open;

	-- Flush all buffered data to disk.  LMDB flushes its internal buffers when
	-- a transaction is committed but the OS may keep the data buffered for
	-- longer, this forces the OS to flush its buffers when possible.
	procedure Flush (Env : in out Environment)
		with Pre => Env.Is_Open and then not Env.Read_Only;

	-- Check for and remove stale entries in the reader lock table and return
	-- the number of entries cleared.
	function Clear_Stale_Readers (Env : in out Environment) return int
		with Pre => Env.Is_Open;


	----------------------------
	-- Environment References --
	----------------------------

	-- A weak reference to an environment that does not prevent the underlying
	-- environment from being implicitly closed and freed.  Preferrable to a
	-- standard access type when possible because it allows you to check if the
	-- environment was freed before dereferencing it.
	type Environment_Reference is tagged private;

	-- Create a new weak reference to a given Environment.
	function Make_Reference (Env : Environment)
		return Environment_Reference'Class;

	-- Whether or not the underlying environment referenced was freed.
	function Was_Freed (Ref : Environment_Reference) return Boolean;

	-- Get the referenced environment.  The returned object WILL prevent the
	-- environment from being freed as long as it exists.
	function Dereference (Ref : Environment_Reference'Class) return Environment
		with Pre => not Ref.Was_Freed;


	---------------
	-- Databases --
	---------------

	-- A database handle (analgous to a "table" in other DBMSes) within an
	-- envronment.  All database handles will be closed when the parent
	-- environment is closed.
	type Database is tagged private;

	-- Whether or not a database handle has an opened database associated.
	function Is_Open (DB : Database) return Boolean;

	-- Whether or not a given database supports duplicate keys.
	function Duplicates_Allowed (DB : Database) return Boolean
		with Pre => DB.Is_Open;

	-- Open the "unnamed database" (the default database) within an
	-- environment.
	-- If Reverse_Key or Reverse_Data are True then the key or value
	-- respectively will be compared in reverse order, from the end to the
	-- beginning.  Reverse_Data only has an effect if Allow_Duplicates is also
	-- True.
	-- If Allow_Duplicates is True then duplicate keys will be allowed in the
	-- database.
	function Open
		(Env : Environment'Class;
		Reverse_Key : Boolean := False;
		Reverse_Data : Boolean := False;
		Allow_Duplicates : Boolean := False)
		return Database
		with Pre => Env.Is_Open and then Env.Max_Databases = 0,
			Post =>
				Open'Result.Is_Open and then
				Open'Result.Duplicates_Allowed = Allow_Duplicates;

	-- Open a named database in an environment.
	-- If Create_If_Not_Exist is True the database will be created if it does
	-- not already exist. Due to the internal design of LMDB, Key_Not_Found
	-- will be raised if Create_If_Not_Exist is False and the database does not
	-- exist.
	-- All other parameters are identical to the unnamed database variant.
	function Open
		(Env : Environment'Class;
		DB_Name : UTF_8_String;
		Create_If_Not_Exist : Boolean := True;
		Reverse_Key : Boolean := False;
		Reverse_Data : Boolean := False;
		Allow_Duplicates : Boolean := False)
		return Database
		with Pre =>
				Env.Is_Open and then
				(Env.Max_Databases > 0 and
				(if Create_If_Not_Exist then not Env.Read_Only)),
			Post =>
				Open'Result.Is_Open and then
				Open'Result.Duplicates_Allowed = Allow_Duplicates;


	------------------
	-- Transactions --
	------------------

	-- An atomic environment transaction.
	-- Warning: A transaction will be automatically rolled back if it leaves
	-- the scope and is finalized prior to calling Commit.
	-- Reference-counted, as long as at least one copy of a Transaction object
	-- exists, then the underlying transaction as well as the underlying
	-- environment will be kept open.
	type Transaction is tagged private;

	-- Whether a transaction has been connected to an Environment or not.
	function Is_Connected (Txn : Transaction) return Boolean;

	-- Return the Environment a Transaction is connected to.
	function Get_Environment (Txn : Transaction) return Environment'Class
		with Pre => Txn.Is_Connected;

	-- Whether a transaction is read-only or mutable.
	function Read_Only (Txn : Transaction) return Boolean
		with Pre => Txn.Is_Connected,
			Post => (if Txn.Get_Environment.Read_Only then Read_Only'Result);

	-- Create a transaction for use within the given environment.  Note that
	-- LMDB enforces a single-writer policy so this call will block if
	-- Read_Only is False and another writer transaction exists.
	function Create
		(Env : Environment'Class; Read_Only : Boolean := True)
		return Transaction
		with Pre =>
				Env.Is_Open and then
				(if not Read_Only then not Env.Read_Only),
			Post =>
				Create'Result.Is_Connected and then
				Create'Result.Read_Only = Read_Only;

	-- Create a transaction that's nested inside another transaction.  The
	-- parent transaction's writes will be visible to the child transaction.
	-- LMDB itself only supports nesting write transactions, so if Read_Only is
	-- specified or the parent transaction is Read_Only then the returned
	-- transaction will be an ordinary transaction.
	function Create
		(Parent : Transaction; Read_Only : Boolean := True)
		return Transaction
		with Pre =>
				Parent.Is_Connected and then
				(if not Read_Only then not Parent.Get_Environment.Read_Only),
			Post =>
				Create'Result.Is_Connected and then
				Create'Result.Read_Only = Read_Only;

	-- Commit all operations of a transaction into the database. Equivalent to
	-- Rollback (i.e. no database modifications are made) for read-only
	-- transactions.
	procedure Commit (Txn : in out Transaction)
		with Post => not Txn.Is_Connected;

	-- Abandon all operations of a transaction and do not make any database
	-- modifications.
	procedure Rollback (Txn : in out Transaction)
		with Post => not Txn.Is_Connected;

	-- Whether or not a given key exists in the database.
	function Exists
		(Txn : Transaction; DB : Database'Class; Key : Stream_Element_Array)
		return Boolean
		with Pre => Txn.Is_Connected and DB.Is_Open;

	-- Get a value from a database given a key.  If the database supports
	-- multiple values, only the first value associated with a key will be
	-- returned.
	function Get
		(Txn : Transaction; DB : Database'Class; Key : Stream_Element_Array)
		return Stream_Element_Array
		with Pre => Txn.Is_Connected and DB.Is_Open;

	-- Put a key-value pair into a database.
	-- If No_Overwrite is True and Key already exists in the database, then
	-- Key_Already_Exists will be raised rather than overwriting the old data
	-- with the new Data.
	-- If the database has multiple values enabled and No_Duplicates is True,
	-- then the key/value pair will not be inserted if it already exists in the
	-- database.  No_Duplicates has no effect if multiple values are not
	-- enabled for the database.
	procedure Put
		(Txn : Transaction;
		DB : Database'Class;
		Key : Stream_Element_Array;
		Data : Stream_Element_Array;
		No_Overwrite : Boolean := False;
		No_Duplicates : Boolean := False)
		with Pre =>
				(Txn.Is_Connected and then not Txn.Read_Only) and DB.Is_Open,
			Post => Txn.Exists(DB, Key);

	-- Delete all items with a given key from the database.
	procedure Delete
		(Txn : Transaction; DB : Database'Class; Key : Stream_Element_Array)
		with Pre =>
				(Txn.Is_Connected and then not Txn.Read_Only) and DB.Is_Open,
			Post => not Txn.Exists(DB, Key);

	-- Delete a specific key-value pair from the database.
	procedure Delete
		(Txn : Transaction;
		DB : Database'Class;
		Key : Stream_Element_Array;
		Data : Stream_Element_Array)
		with Pre =>
			(Txn.Is_Connected and then not Txn.Read_Only) and
			(DB.Is_Open and then DB.Duplicates_Allowed);


	----------------------------
	-- Transaction References --
	----------------------------

	-- Equivalent to Environment_References but for Transactions.

	type Transaction_Reference is tagged private;

	function Make_Reference (Txn : Transaction)
		return Transaction_Reference'Class;

	function Was_Freed (Ref : Transaction_Reference) return Boolean;

	function Dereference (Ref : Transaction_Reference'Class) return Transaction
		with Pre => not Ref.Was_Freed;


	-------------
	-- Cursors --
	-------------

	type Cursor is private;

	function Has_Element (Csr : Cursor) return Boolean;

	-- Get the current key or value the cursor is pointing at.
	function Key (Csr : Cursor) return Stream_Element_Array
		with Pre => Has_Element(Csr);
	function Value (Csr : Cursor) return Stream_Element_Array
		with Pre => Has_Element(Csr);

	package Cursor_Iterator_Interfaces is
		new Ada.Iterator_Interfaces (Cursor, Has_Element);

	-- Iterate over every key/value pair in the database.
	function Iterate (Txn : Transaction; DB : Database'Class)
		return Cursor_Iterator_Interfaces.Reversible_Iterator'Class
		with Pre => Txn.Is_Connected and DB.Is_Open;

	-- Iterate over every unique key in the database, returning the first value
	-- for each key.  Equivalent to Iterate if the database does not allow
	-- duplicates.
	function Iterate_No_Duplicates
		(Txn : Transaction; DB : Database'Class)
		return Cursor_Iterator_Interfaces.Reversible_Iterator'Class
		with Pre => Txn.Is_Connected and DB.Is_Open;

	-- Iterate over every data item for a given key.
	function Iterate_Key
		(Txn : Transaction; DB : Database'Class; Key : Stream_Element_Array)
		return Cursor_Iterator_Interfaces.Reversible_Iterator'Class
		with Pre =>
			 Txn.Is_Connected and (DB.Is_Open and then DB.Duplicates_Allowed);


	----------------
	-- Exceptions --
	----------------

	-- The key/data pair already exists in the database.
	Key_Already_Exists : exception;

	-- The key/data pair was not found in the database.
	Key_Not_Found : exception;

	-- The maximum number of databases within an environment was reached.  Most
	-- often raised if Max_Databases was not specified when calling
	-- Environment.Open
	Databases_Full : exception;

	-- Raised if the database does not exist and Create_If_Not_Exist was False.
	Name_Error : exception renames Ada.IO_Exceptions.Name_Error;

	-- Raised when the underlying LMDB library returns an error.  The exception
	-- message will contain more information about the error.
	LMDB_Error : exception;


private
	use lmdb_h;

	-- Given an LMDB return value check, if it is a success or error value.  If
	-- it is an error value raise the appropriate exception, prepending the
	-- Additional error message to the LMDB error message (from mdb_strerror)
	-- with a colon and space.
	procedure Assert_LMDB_Return
		(Code : Interfaces.C.int; Additional : String := "");

	-- Convert a Stream_Element_Array to an MDB_val.
	-- Borrows memory! The returned value will become invalid if the underlying
	-- Stream_Element_Array becomes invalid.  'Unchecked_Access is used so *IT
	-- IS THE CALLER'S RESPONSIBILITY* to ensure the lifetime of the
	-- underlying array is longer than the MDB_val's.
	function To_MDB_Val
		(A : not null access Stream_Element_Array) return MDB_val
		with Inline => True;

	-- Convert an MDB_val to a Stream_Element_Array.  The returned array is not
	-- dependent upon the pointer within the MDB_val.
	function From_MDB_Val
		(V : not null access MDB_val) return Stream_Element_Array
		with Inline => True;

	-- Initialize a new MDB_cursor with the given database and transaction.
	function Open_Cursor (DB : Database'Class; Txn : Transaction'Class)
		return MDB_cursor
		with Pre => DB.Is_Open and Txn.Is_Connected,
			Post => Open_Cursor'Result /= null;

	-- Internal record storing state and C types for an environment.
	type Environment_Internal is record
		Internal : aliased MDB_env := null;
		Open : Boolean := False;
		Read_Only : Boolean;
		Max_Databases : unsigned;
		Max_Readers : unsigned;
		Map_Size : size_t;
	end record;

	-- Allocate and clean up internal state of an environment.  Finalize is
	-- idempotent and can be called an arbitrary number of times.
	procedure Initialize (Self : in out Environment_Internal)
		with Pre => Self.Internal = null,
			Post => Self.Internal /= null;
	procedure Finalize (Self : in out Environment_Internal)
		with Post => Self.Internal = null and Self.Open = False;

	-- Reference counting wrapper around Environment_Internal.
	package Environment_Pointers is new GNATCOLL.Refcount.Shared_Pointers
		(Element_Type => Environment_Internal, Release => Finalize);

	type Environment is new Environment_Pointers.Ref with null record;

	-- An empty, default environment.
	Null_Env : constant Environment := (Environment_Pointers.Null_Ref with null record);

	-- A safe weak reference that does not increment the reference counter, and
	-- knows when the referenced object is deallocated.
	type Environment_Reference is new Environment_Pointers.Weak_Ref with null record;

	-- Record storing state and C types for a database.  LMDB does a lot of
	-- implicit database management for us so there is no need for the
	-- additional reference counting machinery.  Even calling Open on the same
	-- database name multiple times will return the same database handle every
	-- time.
	type Database is tagged record
		Internal : aliased MDB_dbi;
		Initialized : Boolean := False;
		Allows_Duplicates : Boolean := False;
	end record;

	-- Equivalent to Environment_Internal and its reference-counting setup.
	type Transaction_Internal is record
		Internal : aliased MDB_txn := null;
		Read_Only : Boolean;
		Env : Environment := Null_Env;
		-- Parent : Transaction := Null_Txn;
	end record;

	-- Default cleanup operation.
	procedure Rollback (Self : in out Transaction_Internal)
		with Post => Self.Internal = null and Self.Env = Null_Env;

	package Transaction_Pointers is new GNATCOLL.Refcount.Shared_Pointers
		(Element_Type => Transaction_Internal, Release => Rollback);

	type Transaction is new Transaction_Pointers.Ref with null record;

	Null_Txn : constant Transaction := (Transaction_Pointers.Null_Ref with null record);

	type Transaction_Reference is new Transaction_Pointers.Weak_Ref with null record;

	package Stream_Element_Array_Holders is
		new Ada.Containers.Indefinite_Holders(Stream_Element_Array);

	type Iterator_Internal is record
		First_Op, Last_Op, Next_Op, Previous_Op : MDB_cursor_op;

		Internal : aliased MDB_cursor := null;

		Txn : Transaction := Null_Txn;
		DB : Database;

		Query_Key : Stream_Element_Array_Holders.Holder :=
			Stream_Element_Array_Holders.Empty_Holder;

		Cached : Boolean := False;
		Cached_Key, Cached_Value : aliased MDB_val;

		-- Required because if MDB_NEXT reaches the end of its iteration
		-- MDB_GET_CURRENT will continue to return the last item over and over.
		Last_Update : Boolean := True;
	end record
		with Dynamic_Predicate =>
			(if Internal = null
				then Txn = Null_Txn
				else Txn.Is_Connected and DB.Is_Open);

	procedure Finalize (Self : in out Iterator_Internal)
		with Post => Self.Internal = null and Self.Txn = Null_Txn;

	package Iterator_Pointers is new GNATCOLL.Refcount.Shared_Pointers
		(Element_Type => Iterator_Internal, Release => Finalize);

	type Iterator is
		new Iterator_Pointers.Ref and
		Cursor_Iterator_Interfaces.Reversible_Iterator
		with null record;

	type Cursor is record
		I : Iterator_Pointers.Weak_Ref := Iterator_Pointers.Null_Weak_Ref;
	end record;

	-- Helper function to unwrap the reference held by a cursor back into an
	-- iterator.
	function Get_Iterator (Csr : Cursor) return Iterator;

	-- Helper function to advance a cursor, given an operaton.
	function Update_Cursor (Object : Iterator; Op : MDB_cursor_op)
		return Cursor
		with Pre =>
			not Object.Is_Null and then
			((not Object.Get.Txn.Is_Null and then
			Object.Get.Txn.Is_Connected) and
			Object.Get.DB.Is_Open and
			Object.Get.Internal /= null);

	-- Get and cache the current key/value pair the cursor is positioned at.
	procedure Refresh_Cache (I : Iterator)
		with Pre => not I.Is_Null and then I.Get.Last_Update;

	overriding function First (Object : Iterator) return Cursor;
	overriding function Last (Object : Iterator) return Cursor;
	overriding function Next
		(Object : Iterator; Position : Cursor) return Cursor;
	overriding function Previous
		(Object : Iterator; Position : Cursor) return Cursor;

end LMDB;
