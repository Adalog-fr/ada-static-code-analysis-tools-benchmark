-- SPDX-License-Identifier: BSD-2-Clause
-- For more license details, see LICENSE.

pragma Ada_2012;

with Ada.Directories;
with Interfaces.C.Strings;  use Interfaces.C.Strings;

package body LMDB is

-------------------------------------------------------------------------------
--------------------------------- Environments --------------------------------
-------------------------------------------------------------------------------

	-------------
	-- Is_Open --
	-------------

	function Is_Open (Env : Environment) return Boolean
	is (not Env.Is_Null and then (Env.Get.Internal /= null and Env.Get.Open));

	---------------
	-- Read_Only --
	---------------

	function Read_Only (Env : Environment) return Boolean
	is (Env.Get.Read_Only);


	-------------------
	-- Max_Databases --
	-------------------

	function Max_Databases (Env : Environment) return unsigned
	is (Env.Get.Max_Databases);


	-----------------
	-- Max_Readers --
	-----------------

	function Max_Readers (Env : Environment) return unsigned
	is (Env.Get.Max_Readers);


	--------------
	-- Map_Size --
	--------------

	function Map_Size (Env : Environment) return size_t
	is (Env.Get.Map_Size);


	----------
	-- Open --
	----------

	function Open
		(Path : UTF_8_String;
		Create_If_Not_Exist : Boolean := False;
		No_Subdirectory : Boolean := False;
		Read_Only : Boolean := False;
		Max_Databases : unsigned := 0;
		Max_Readers : unsigned := 126;
		Map_Size : size_t := 10485760)
		return Environment
	is
		CS : chars_ptr;
		RC : int;
		Env : Environment_Internal;
	begin
		if not Ada.Directories.Exists(Path) then
			if not Create_If_Not_Exist then
				raise Name_Error with "No such file or directory: " & Path;
			elsif not No_Subdirectory then
				-- LMDB does not create the subdirectory for you, only the
				-- database files themselves.
				Ada.Directories.Create_Directory(Path);
			end if;
		end if;

		Initialize(Env);

		Env.Read_Only := Read_Only;

		Env.Max_Databases := Max_Databases;
		if Max_Databases > 0 then
			RC := mdb_env_set_maxdbs(Env.Internal, Max_Databases);
			Assert_LMDB_Return(RC, "Unable to set max databases");
		end if;

		Env.Max_Readers := Max_Readers;
		RC := mdb_env_set_maxreaders(Env.Internal, Max_Readers);
		Assert_LMDB_Return(RC, "Unable to set max readers");

		Env.Map_Size := Map_Size;
		RC := mdb_env_set_mapsize(Env.Internal, Map_Size);
		Assert_LMDB_Return(RC, "Unable to set map size");

		CS := New_String(Path);
		RC := mdb_env_open(
			Env.Internal, CS,
			(
				(if No_Subdirectory then MDB_NOSUBDIR else 0) or
				(if Read_Only then MDB_RDONLY else 0)
				-- XXX: Does Ada tasking need MDB_NOTLS?
			),
			8#664#
		);
		Free(CS);
		Assert_LMDB_Return(RC, "Unable to open environment");
		Env.Open := True;

		return O : Environment do
			O.Set(Env);
		end return;
	end Open;


	-----------
	-- Close --
	-----------

	procedure Close (Env : in out Environment) is
	begin
		if not Env.Is_Null then
			Finalize(Env.Get);
		end if;
	end Close;


	----------
	-- Copy --
	----------

	procedure Copy
		(Env : Environment; New_Path : UTF_8_String; Compact : Boolean := False)
	is
		CS : chars_ptr := New_String(New_Path);
		RC : int;
	begin
		RC := mdb_env_copy2(
			Env.Get.Internal, CS,
			(if Compact then MDB_CP_COMPACT else 0)
		);
		Free(CS);
		Assert_LMDB_Return(RC, "Unable to copy environment");
	end Copy;


	-----------
	-- Flush --
	-----------

	procedure Flush (Env : in out Environment) is
		RC : int;
	begin
		RC := mdb_env_sync(Env.Get.Internal, 0);
		Assert_LMDB_Return(RC, "Unable to flush buffers");
	end Flush;


	-------------------------
	-- Clear_Stale_Readers --
	-------------------------

	function Clear_Stale_Readers (Env : in out Environment) return int is
		O : aliased int;
		RC : int;
	begin
		RC := mdb_reader_check(Env.Get.Internal, O'Access);
		Assert_LMDB_Return(RC, "Unable to clear stale readers");
		return O;
	end Clear_Stale_Readers;


	----------------
	-- Initialize --
	----------------

	procedure Initialize (Self : in out Environment_Internal) is
		RC : int;
	begin
		if Self.Internal = null then
			RC := mdb_env_create(Self.Internal'Access);
			Assert_LMDB_Return(RC, "Unable to create environment");
		end if;
	end Initialize;


	--------------
	-- Finalize --
	--------------

	procedure Finalize (Self : in out Environment_Internal) is
	begin
		if Self.Internal /= null then
			mdb_env_close(Self.Internal);
			Self.Internal := null;
			Self.Open := False;
		end if;
	end Finalize;


-------------------------------------------------------------------------------
---------------------------- Environment References ---------------------------
-------------------------------------------------------------------------------

	--------------------
	-- Make_Reference --
	--------------------

	function Make_Reference
		(Env : Environment) return Environment_Reference'Class
	is (Environment_Reference'(Environment_Pointers.Weak(Env) with null record));


	---------------
	-- Was_Freed --
	---------------

	function Was_Freed (Ref : Environment_Reference) return Boolean
	is (Environment_Pointers.Was_Freed(Ref));


	-----------------
	-- Dereference --
	-----------------

	function Dereference
		(Ref : Environment_Reference'Class) return Environment
	is
	begin
		return Env : Environment do
			Environment_Pointers.Set(Env, Ref);
		end return;
	end Dereference;


-------------------------------------------------------------------------------
---------------------------------- Databases ----------------------------------
-------------------------------------------------------------------------------

	-------------
	-- Is_Open --
	-------------

	function Is_Open (DB : Database) return Boolean
	is (DB.Initialized);


	------------------------
	-- Duplicates_Allowed --
	------------------------

	function Duplicates_Allowed (DB : Database) return Boolean
	is (DB.Allows_Duplicates);


	----------
	-- Open --
	----------

	function Open
		(Env : Environment'Class;
		Reverse_Key : Boolean := False;
		Reverse_Data : Boolean := False;
		Allow_Duplicates : Boolean := False)
		return Database
	is
		Txn : Transaction;
		DB : Database;
		RC : int;
	begin
		Txn := Create(Env, Read_Only => True);
		RC := mdb_dbi_open(
			Txn.Get.Internal, Null_Ptr,
			(if Reverse_Key then MDB_REVERSEKEY else 0) or
			(if Reverse_Data then MDB_REVERSEDUP else 0) or
			(if Allow_Duplicates then MDB_DUPSORT else 0),
			DB.Internal'Access
		);
		Assert_LMDB_Return(RC, "Unable to open database");
		Txn.Commit;
		DB.Allows_Duplicates := Allow_Duplicates;
		DB.Initialized := True;
		return DB;
	end Open;


	------------------
	-- Open (Named) --
	------------------

	function Open
		(Env : Environment'Class;
		DB_Name : UTF_8_String;
		Create_If_Not_Exist : Boolean := True;
		Reverse_Key : Boolean := False;
		Reverse_Data : Boolean := False;
		Allow_Duplicates : Boolean := False)
		return Database
	is
		Txn : Transaction;
		DB_Name_Ptr : chars_ptr := New_String(DB_Name);
		DB : Database;
		RC : int;
	begin
		Txn := Create(
			Env,
			-- Transaction must be writeable if creating a new database,
			-- otherwise read-only is fine.
			Read_Only => (if Create_If_Not_Exist then False else True)
		);
		RC := mdb_dbi_open(
			Txn.Get.Internal, DB_Name_Ptr,
			(if Create_If_Not_Exist then MDB_CREATE else 0) or
			(if Reverse_Key then MDB_REVERSEKEY else 0) or
			(if Reverse_Data then MDB_REVERSEDUP else 0) or
			(if Allow_Duplicates then MDB_DUPSORT else 0),
			DB.Internal'Access
		);
		Free(DB_Name_Ptr);
		Assert_LMDB_Return(RC, "Unable to open database " & DB_Name);
		Txn.Commit;
		DB.Allows_Duplicates := Allow_Duplicates;
		DB.Initialized := True;
		return DB;
	end Open;


-------------------------------------------------------------------------------
--------------------------------- Transactions --------------------------------
-------------------------------------------------------------------------------

	------------------
	-- Is_Connected --
	------------------

	function Is_Connected (Txn : Transaction) return Boolean
	is (not Txn.Is_Null and then (Txn.Get.Internal /= null));


	---------------
	-- Read_Only --
	---------------

	function Read_Only (Txn : Transaction) return Boolean
	is (Txn.Get.Read_Only);


	---------------------
	-- Get_Environment --
	---------------------

	function Get_Environment (Txn : Transaction) return Environment'Class
	is (Txn.Get.Env);


	------------
	-- Create --
	------------

	function Create
		(Env : Environment'Class; Read_Only : Boolean := True)
		return Transaction
	is
		Txn : Transaction_Internal;
		RC : int;
	begin
		RC := mdb_txn_begin(
			Env.Get.Internal, null,
			(if Read_Only then MDB_RDONLY else 0),
			Txn.Internal'Access
		);
		Assert_LMDB_Return(RC, "Unable to create transaction");
		Txn.Read_Only := Read_Only;
		Txn.Env := Environment(Env);
		-- Txn.Parent := Null_Txn;

		return O : Transaction do
			O.Set(Txn);
		end return;
	end Create;


	---------------------
	-- Create (Nested) --
	---------------------

	function Create
		(Parent : Transaction; Read_Only : Boolean := True)
		return Transaction
	is
		Txn : Transaction_Internal;
		RC : int;
	begin
		if Read_Only or Parent.Read_Only then
			return Create(Parent.Get.Env, Read_Only);
		end if;

		RC := mdb_txn_begin(
			Parent.Get.Env.Get.Internal,
			Parent.Get.Internal,
			0,
			Txn.Internal'Access
		);
		Assert_LMDB_Return(RC, "Unable to create transaction");
		Txn.Read_Only := False;
		Txn.Env := Parent.Get.Env;

		return O : Transaction do
			O.Set(Txn);
		end return;
	end Create;


	------------
	-- Commit --
	------------

	procedure Commit (Txn : in out Transaction) is
		RC : int;
	begin
		if not Txn.Is_Null then
			RC := mdb_txn_commit(Txn.Get.Internal);
			Assert_LMDB_Return(RC, "Unable to commit transaction");
			Txn.Get.Internal := null;
			Txn.Get.Env := Null_Env;
			-- Txn.Get.Parent := Null_Txn;
		end if;
	end Commit;


	--------------
	-- Rollback --
	--------------

	procedure Rollback (Txn : in out Transaction) is
	begin
		if not Txn.Is_Null then
			Rollback(Txn.Get);
		end if;
	end Rollback;


	--------------
	-- Rollback --
	--------------

	procedure Rollback (Self : in out Transaction_Internal) is
	begin
		if Self.Internal /= null then
			mdb_txn_abort(Self.Internal);
			Self.Internal := null;
			Self.Env := Null_Env;
			-- Self.Parent := Null_Txn;
		end if;
	end Rollback;


	------------
	-- Exists --
	------------

	function Exists
		(Txn : Transaction; DB : Database'Class; Key : Stream_Element_Array)
		return Boolean
	is
	begin
		declare
			Dummy : Stream_Element_Array := Get(Txn, DB, Key);
		begin
			pragma Unreferenced(Dummy);
			null;
		end;
		return True;
	exception
		when Key_Not_Found => return False;
	end Exists;


	---------
	-- Get --
	---------

	function Get
		(Txn : Transaction; DB : Database'Class; Key : Stream_Element_Array)
		return Stream_Element_Array
	is
		-- Make a copy of the Key because Stream_Element_Array_Pointers
		-- doesn't know LMDB doesn't modify the pointers it's given.
		Key_Copy : aliased Stream_Element_Array := Key;

		Key_Val : aliased MDB_val := To_MDB_Val(Key_Copy'Access);
		Data_Val : aliased MDB_val;

		RC : int;
	begin
		RC := mdb_get(
			Txn.Get.Internal, DB.Internal,
			Key_Val'Access, Data_Val'Access
		);
		Assert_LMDB_Return(RC, "Unable to get value from database");
		return From_MDB_Val(Data_Val'Access);
	end Get;


	---------
	-- Put --
	---------

	procedure Put
		(Txn : Transaction;
		DB : Database'Class;
		Key : Stream_Element_Array;
		Data : Stream_Element_Array;
		No_Overwrite : Boolean := False;
		No_Duplicates : Boolean := False)
	is
		Key_Copy : aliased Stream_Element_Array := Key;
		Data_Copy : aliased Stream_Element_Array := Data;

		Key_Val : aliased MDB_val := To_MDB_Val(Key_Copy'Access);
		Data_Val : aliased MDB_val := To_MDB_Val(Data_Copy'Access);

		RC : int;
	begin
		RC := mdb_put(
			Txn.Get.Internal, DB.Internal,
			Key_Val'Access, Data_Val'Access,
			(if No_Overwrite then MDB_NOOVERWRITE else 0) or
			(if No_Duplicates then MDB_NODUPDATA else 0)
		);
		Assert_LMDB_Return(RC, "Unable to insert value into database");
	end Put;


	------------
	-- Delete --
	------------

	procedure Delete
		(Txn : Transaction; DB : Database'Class; Key : Stream_Element_Array)
	is
		Key_Copy : aliased Stream_Element_Array := Key;

		Key_Val : aliased MDB_val := To_MDB_Val(Key_Copy'Access);

		RC : int;
	begin
		RC := mdb_del(
			Txn.Get.Internal, DB.Internal,
			Key_Val'Access, null
		);
		Assert_LMDB_Return(RC);
	end Delete;


	------------------------
	-- Delete (Key-Value) --
	------------------------

	procedure Delete
		(Txn : Transaction; DB : Database'Class;
		Key : Stream_Element_Array; Data : Stream_Element_Array)
	is
		Key_Copy : aliased Stream_Element_Array := Key;
		Data_Copy : aliased Stream_Element_Array := Data;

		Key_Val : aliased MDB_val := To_MDB_Val(Key_Copy'Access);
		Data_Val : aliased MDB_val := To_MDB_Val(Data_Copy'Access);

		RC : int;
	begin
		RC := mdb_del(
			Txn.Get.Internal, DB.Internal,
			Key_Val'Access, Data_Val'Access
		);
		Assert_LMDB_Return(RC);
	end Delete;


-------------------------------------------------------------------------------
---------------------------- Transaction References ---------------------------
-------------------------------------------------------------------------------

	--------------------
	-- Make_Reference --
	--------------------

	function Make_Reference
		(Txn : Transaction) return Transaction_Reference'Class
	is (Transaction_Reference'(Transaction_Pointers.Weak(Txn) with null record));


	---------------
	-- Was_Freed --
	---------------

	function Was_Freed (Ref : Transaction_Reference) return Boolean
	is (Transaction_Pointers.Was_Freed(Ref));


	-----------------
	-- Dereference --
	-----------------

	function Dereference
		(Ref : Transaction_Reference'Class) return Transaction
	is
	begin
		return Txn : Transaction do
			Transaction_Pointers.Set(Txn, Ref);
		end return;
	end Dereference;


-------------------------------------------------------------------------------
----------------------------------- Cursors -----------------------------------
-------------------------------------------------------------------------------

	-----------------
	-- Has_Element --
	-----------------

	function Has_Element (Csr : Cursor) return Boolean is
		I : Iterator;
	begin
		if Iterator_Pointers.Was_Freed(Csr.I) then
			return False;
		end if;
		Iterator_Pointers.Set(I, Csr.I);
		if I.Is_Null or else I.Get.Internal = null then
			I.Get.Cached := False;
			return False;
		end if;

		if not I.Get.Last_Update then
			return False;
		end if;

		if not I.Get.Cached then
			Refresh_Cache(I);
		end if;
		return True;

	exception
		when Key_Not_Found => return False;
	end Has_Element;


	---------
	-- Key --
	---------

	function Key (Csr : Cursor) return Stream_Element_Array is
		I : constant Iterator := Get_Iterator(Csr);
	begin
		if I.Is_Null or else I.Get.Internal = null then
			raise Program_Error with "Cursor not initialized";
		elsif not I.Get.Cached then
			Refresh_Cache(I);
		end if;

		return From_MDB_Val(I.Get.Cached_Key'Access);
	end Key;


	-----------
	-- Value --
	-----------

	function Value (Csr : Cursor) return Stream_Element_Array is
		I : constant Iterator := Get_Iterator(Csr);
	begin
		if I.Is_Null or else I.Get.Internal = null then
			raise Program_Error with "Cursor not initialized";
		elsif not I.Get.Cached then
			Refresh_Cache(I);
		end if;

		return From_MDB_Val(I.Get.Cached_Value'Access);
	end Value;


	-------------
	-- Iterate --
	-------------

	function Iterate (Txn : Transaction; DB : Database'Class)
		return Cursor_Iterator_Interfaces.Reversible_Iterator'Class
	is
	begin
		return O : Iterator do
			Iterator_Pointers.Set(
				O,
				Iterator_Internal'(
					First_Op => MDB_FIRST,
					Last_Op => MDB_LAST,
					Next_Op => MDB_NEXT,
					Previous_Op => MDB_PREV,
					Internal => Open_Cursor(DB, Txn),
					DB => Database(DB),
					Txn => Txn,
					others => <>
			));
		end return;
	end Iterate;


	---------------------------
	-- Iterate_No_Duplicates --
	---------------------------

	function Iterate_No_Duplicates
		(Txn : Transaction; DB : Database'Class)
		return Cursor_Iterator_Interfaces.Reversible_Iterator'Class
	is
	begin
		return O : Iterator do
			Iterator_Pointers.Set(
				O,
				Iterator_Internal'(
					First_Op => MDB_FIRST,
					Last_Op => MDB_LAST,
					Next_Op => MDB_NEXT_NODUP,
					Previous_Op => MDB_PREV_NODUP,
					Internal => Open_Cursor(DB, Txn),
					DB => Database(DB),
					Txn => Txn,
					others => <>
				));
		end return;
	end Iterate_No_Duplicates;


	-----------------
	-- Iterate_Key --
	-----------------

	function Iterate_Key
		(Txn : Transaction; DB : Database'Class; Key : Stream_Element_Array)
		return Cursor_Iterator_Interfaces.Reversible_Iterator'Class
	is
	begin
		return O : Iterator do
			Iterator_Pointers.Set(
				O,
				Iterator_Internal'(
					First_Op => MDB_FIRST_DUP,
					Last_Op => MDB_LAST_DUP,
					Next_Op => MDB_NEXT_DUP,
					Previous_Op => MDB_PREV_DUP,
					Internal => Open_Cursor(DB, Txn),
					DB => Database(DB),
					Txn => Txn,
					Query_Key => Stream_Element_Array_Holders.To_Holder(Key),
					others => <>
			));

			-- We have to make sure to set the current position at the
			-- requested key.
			declare
				Key_Copy : aliased Stream_Element_Array := Key;
				Key_Val : aliased MDB_val := To_MDB_Val(Key_Copy'Access);
				Dummy : aliased MDB_val;
				RC : int;
			begin
				RC := mdb_cursor_get(
					O.Get.Internal,
					Key_Val'Access,
					Dummy'Access,
					MDB_SET
				);
				pragma Unreferenced(Dummy);
				if RC = MDB_NOTFOUND then
					O.Get.Cached := False;
					O.Get.Last_Update := False;
				else
					Assert_LMDB_Return(RC, "Unable to set cursor position");
				end if;
			end;
		end return;
	end Iterate_Key;


	--------------
	-- Finalize --
	--------------

	procedure Finalize (Self : in out Iterator_Internal) is
	begin
		if Self.Internal /= null then
			mdb_cursor_close(Self.Internal);
			Self.Internal := null;
			Self.Txn := Null_Txn;
			Self.Cached := False;
		end if;
	end Finalize;


	-----------
	-- First --
	-----------

	overriding function First (Object : Iterator) return Cursor
	is (Update_Cursor(Object, Object.Get.First_Op));


	----------
	-- Last --
	----------

	overriding function Last (Object : Iterator) return Cursor
	is (Update_Cursor(Object, Object.Get.Last_Op));


	----------
	-- Next --
	----------

	overriding function Next
		(Object : Iterator; Position : Cursor) return Cursor
	is (Update_Cursor(Object, Object.Get.Next_Op));


	--------------
	-- Previous --
	--------------

	overriding function Previous
		(Object : Iterator; Position : Cursor) return Cursor
	is (Update_Cursor(Object, Object.Get.Previous_Op));


-------------------------------------------------------------------------------
------------------------------ Helper Subprograms -----------------------------
-------------------------------------------------------------------------------

	------------------------
	-- Assert_LMDB_Return --
	------------------------

	procedure Assert_LMDB_Return (Code : int; Additional : String := "") is
		-- If Additional is not empty, prepend it to S separated by ": "
		function Concat (S : String) return String
		is ((if Additional'Length > 0 then Additional & ": " else "") & S);

		Err : chars_ptr := Null_Ptr;
	begin
		if Code = MDB_SUCCESS then
			return;
		end if;

		case Code is
			when MDB_KEYEXIST =>
				raise Key_Already_Exists with Concat("Key already exists");
			when MDB_NOTFOUND =>
				raise Key_Not_Found with Concat("Key not found");
			when MDB_DBS_FULL =>
				raise Databases_Full with Concat("Maximum databases for the environment reached.");
			when others =>
				Err := mdb_strerror(Code);
				if Err = Null_Ptr then
					raise LMDB_Error
						with Concat("Unable to retrieve LMDB error string");
				end if;
				-- LMDB returns a pointer to a string constant so don't free it
				raise LMDB_Error with Concat(Value(Err) & " (Code " & int'Image(Code) &")");
		end case;
	end Assert_LMDB_Return;


	----------------
	-- To_MDB_Val --
	----------------

	function To_MDB_Val
		(A : not null access Stream_Element_Array) return MDB_val
	is ((
		mv_size => A.all'Size / CHAR_BIT,
		mv_data => A.all(A.all'First)'Unchecked_Access
	));


	------------------
	-- From_MDB_Val --
	------------------

	function From_MDB_Val
		(V : not null access MDB_val) return Stream_Element_Array
	is (Stream_Element_Array_Pointers.Value(
		V.mv_data,
		ptrdiff_t((V.mv_size * CHAR_BIT) / Stream_Element'Size)
	));


	-----------------
	-- Open_Cursor --
	-----------------

	function Open_Cursor
		(DB : Database'Class; Txn : Transaction'Class) return MDB_cursor
	is
		O : aliased MDB_cursor;
		RC : int;
	begin
		RC := mdb_cursor_open(Txn.Get.Internal, DB.Internal, O'Access);
		Assert_LMDB_Return(RC, "Unable to open cursor");
		if O = null then
			raise LMDB_Error with "Unable to open cursor: erroneous null returned";
		end if;
		return O;
	end Open_Cursor;


	------------------
	-- Get_Iterator --
	------------------

	function Get_Iterator (Csr : Cursor) return Iterator is
		I : Iterator;
	begin
		if Iterator_Pointers.Was_Freed(Csr.I) then
			raise Program_Error with "Cursor not initialized";
		end if;
		Iterator_Pointers.Set(I, Csr.I);
		return I;
	end Get_Iterator;


	-------------------
	-- Update_Cursor --
	-------------------

	function Update_Cursor
		(Object : Iterator; Op : MDB_cursor_op) return Cursor
	is
		Key, Value : aliased MDB_val;

		RC : int;
	begin
		RC := mdb_cursor_get(Object.Get.Internal, Key'Access, Value'Access, Op);
		if RC = MDB_NOTFOUND then
			Object.Get.Cached := False;
			Object.Get.Last_Update := False;
			pragma Assert(
				not Has_Element((I => Iterator_Pointers.Weak(Object)))
			);
		else
			Assert_LMDB_Return(RC, "Unable to set cursor position");

			Object.Get.Cached_Key := Key;
			Object.Get.Cached_Value := Value;
			Object.Get.Cached := True;

			Object.Get.Last_Update := True;
		end if;

		return (I => Iterator_Pointers.Weak(Object));
	end Update_Cursor;


	-------------------
	-- Refresh_Cache --
	-------------------

	procedure Refresh_Cache (I : Iterator) is
		Key, Value : aliased MDB_val;
		RC : int;
	begin
		if I.Is_Null or else I.Get.Internal = null then
			raise Program_Error with "Cursor not initialized";
		end if;

		RC := mdb_cursor_get(
			I.Get.Internal,
			Key'Access,
			Value'Access,
			MDB_GET_CURRENT
		);
		Assert_LMDB_Return(RC, "Unable to retrieve cursor element");
		I.Get.Cached_Key := Key;
		I.Get.Cached_Value := Value;
		I.Get.Cached := True;
	end Refresh_Cache;

end LMDB;
