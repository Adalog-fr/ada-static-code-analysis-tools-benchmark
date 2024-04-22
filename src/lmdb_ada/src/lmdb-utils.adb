-- SPDX-License-Identifier: BSD-2-Clause
-- For more license details, see LICENSE.

pragma Ada_2012;

package body LMDB.Utils is

	--------------
	-- Is_Empty --
	--------------

	function Is_Empty (Stream : Serializer) return Boolean
	is (Stream.Arr.Is_Empty or else Stream.Pos > Stream.Arr.Element'Last);


	---------------
	-- Remaining --
	---------------

	function Remaining (Stream : Serializer) return Stream_Element_Count
	is ((if Stream.Arr.Is_Empty then 0 else Stream.Arr.Element'Length));


	----------
	-- Read --
	----------

	overriding procedure Read
		(Stream : in out Serializer;
		Item : out Stream_Element_Array;
		Last : out Stream_Element_Offset)
	is
	begin
		if Stream.Arr.Is_Empty then
			raise End_Error;
		end if;
		declare
			Val : constant Stream_Element_Array := Stream.Arr.Element;
		begin
			if Stream.Pos > Val'Last then
				Last := Item'First - 1;
				return;
			elsif Stream.Pos < Val'First then
				Stream.Pos := Val'First;
			end if;
			for I in Item'Range loop
				exit when Stream.Pos > Val'Last;
				Item(I) := Val(Stream.Pos);
				Last := I;
				Stream.Pos := Stream.Pos + 1;
			end loop;
		end;
	end Read;


	-----------
	-- Write --
	-----------

	overriding procedure Write
		(Stream : in out Serializer;
		Item : Stream_Element_Array)
	is
	begin
		if Stream.Arr.Is_Empty then
			Stream.Arr := Stream_Element_Array_Holders.To_Holder(Item);
		else
			Stream.Arr := Stream_Element_Array_Holders.To_Holder(
				Stream.Arr.Element & Item
			);
		end if;
	end Write;


	-----------------------------
	-- Dump_Remaining_Contents --
	-----------------------------

	function Dump_Remaining_Contents
		(Stream : in out Serializer) return Stream_Element_Array
	is
	begin
		if Stream.Arr.Is_Empty then
			raise End_Error;
		end if;
		if Stream.Pos < Stream.Arr.Element'First then
			Stream.Pos := Stream.Arr.Element'First;
		end if;
		return
			A : constant Stream_Element_Array :=
				Stream.Arr.Element(Stream.Pos .. Stream.Arr.Element'Last)
		do
			Stream.Arr := Stream_Element_Array_Holders.Empty_Holder;
			Stream.Pos := Stream_Element_Offset'First;
		end return;
	end Dump_Remaining_Contents;


	------------------
	-- Set_Contents --
	------------------

	procedure Set_Contents
		(Stream : in out Serializer; Item : Stream_Element_Array)
	is
	begin
		Stream.Arr := Stream_Element_Array_Holders.To_Holder(Item);
		Stream.Pos := Stream.Arr.Element'First;
	end Set_Contents;


	-------------------------------
	-- From_Stream_Element_Array --
	-------------------------------

	function From_Stream_Element_Array (Raw : Stream_Element_Array) return T is
		S : aliased Serializer;
	begin
		S.Set_Contents(Raw);
		return Input(S'Access);
	end From_Stream_Element_Array;


	-----------------------------
	-- To_Stream_Element_Array --
	-----------------------------

	function To_Stream_Element_Array (Object : T) return Stream_Element_Array
	is
		S : aliased Serializer;
	begin
		Output(S'Access, Object);
		return S.Dump_Remaining_Contents;
	end To_Stream_Element_Array;

end LMDB.Utils;
