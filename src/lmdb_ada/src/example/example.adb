-- SPDX-License-Identifier: BSD-2-Clause
-- For more license details, see LICENSE.

pragma Ada_2012;

with Ada.Streams;  use Ada.Streams;
with Ada.Text_IO;  use Ada.Text_IO;
with LMDB;
with LMDB.Utils;

procedure Example is

	function Deserialize is new LMDB.Utils.From_Stream_Element_Array
		(Natural, Natural'Input);
	function Serialize is new LMDB.Utils.To_Stream_Element_Array
		(Natural, Natural'Output);

	Env : constant LMDB.Environment :=
		LMDB.Open("example.db", Create_If_Not_Exist => True, Max_Databases => 2);

	DB1 : constant LMDB.Database := LMDB.Open(Env, "database 1");
	DB2 : constant LMDB.Database :=
		LMDB.Open(Env, "database 2", Allow_Duplicates => True);

	Txn : LMDB.Transaction := LMDB.Create(Env, Read_Only => False);
begin
	-- Inserting a single key
	Txn.Put(
		DB1,
		Stream_Element_Array'(1, 2, 3, 4, 5),
		Stream_Element_Array'(5, 4, 3, 2, 1)
	);

	-- Inserting multiple values
	for I in 1 .. 5 loop
		for J in 1 .. 5 loop
			Txn.Put(
				DB2,
				-- If a database supports multiple values, inserting duplicates
				-- is implicit
				Serialize(I),
				Serialize(J)
			);
		end loop;
	end loop;
	Txn.Commit;

	Txn := LMDB.Create(Env);
	Put_Line("--- Getting a single key/value ---");
	declare
		A : constant Stream_Element_Array :=
			Txn.Get(DB1, Stream_Element_Array'(1, 2, 3, 4, 5));
	begin
		for E of A loop
			Put(Stream_Element'Image(E) & ", ");
		end loop;
	end;
	New_Line;

	New_Line;
	Put_Line("--- Iterating over all keys in database (excluding duplicate values) ---");
	for Csr in Txn.Iterate_No_Duplicates(DB2) loop
		Put_Line(
			Natural'Image(Deserialize(LMDB.Key(Csr))) &
			": " &
			Natural'Image(Deserialize(LMDB.Value(Csr)))
		);
	end loop;

	New_Line;
	Put_Line("--- Iterating over all duplicate values for a key ---");
	for I in 1 .. 5 loop
		Put_Line("Key: " & Natural'Image(I));
		for Csr in Txn.Iterate_Key(DB2, Serialize(I)) loop
			Put_Line(
				"  " & Natural'Image(Deserialize(LMDB.Value(Csr)))
			);
		end loop;
	end loop;

	-- Implicitly done upon finalization if the explicit call is omitted.
	-- Commit could've been called just as easily, as Rollback and Commit are
	-- equivalent for read-only transactions.
	Txn.Rollback;
end Example;
