-- SPDX-License-Identifier: BSD-2-Clause
-- For more license details, see LICENSE.

pragma Ada_2012;

package LMDB.Utils is

	-- Raised by Serializer when reading past the end of the available data.
	End_Error : exception renames Ada.IO_Exceptions.End_Error;

	-- A helper stream type that allows you to use the stream oriented
	-- attributes to write objects to it and get the resultant array, or to
	-- give it an array and read objects out of it.
	type Serializer is new Root_Stream_Type with private;

	-- Whether or not the Serializer has data remaining.
	function Is_Empty (Stream : Serializer) return Boolean;

	-- The number of Stream_Elements remaining in the serializer.
	function Remaining (Stream : Serializer) return Stream_Element_Count
		with Post => (if Stream.Is_Empty then Remaining'Result = 0);

	-- Read up to Item'Length Stream_Elements from the serializer.  Last will
	-- be set to Stream_Element_Offset'First if no elements are available.
	overriding procedure Read
		(Stream : in out Serializer;
		Item : out Stream_Element_Array;
		Last : out Stream_Element_Offset);

	-- Append the given Stream_Element_Array to the existing elements in the
	-- Serializer.
	overriding procedure Write
		(Stream : in out Serializer;
		Item : Stream_Element_Array);

	-- Dump the remaining unread Stream_Elements from the Serializer and clear
	-- it.
	function Dump_Remaining_Contents
		(Stream : in out Serializer) return Stream_Element_Array
		with Post => Stream.Is_Empty;

	-- Set the contents of the Serializer.  Use Write to append to the existing
	-- contents instead.
	procedure Set_Contents
		(Stream : in out Serializer; Item : Stream_Element_Array)
		with Post => Stream.Remaining = Item'Length;

	-- Easily deserialize a raw Stream_Element_Array to a given type.
	generic
		type T (<>) is limited private;
		with function Input
			(Stream : not null access Root_Stream_Type'Class)
			return T;
	function From_Stream_Element_Array (Raw : Stream_Element_Array) return T;

	-- Easily serialize a given type to a Stream_Element_Array.
	generic
		type T (<>) is limited private;
		with procedure Output
			(Stream : not null access Root_Stream_Type'Class;
			Item : T);
	function To_Stream_Element_Array (Object : T) return Stream_Element_Array;


private

	type Serializer is new Root_Stream_Type with record
		Arr : Stream_Element_Array_Holders.Holder :=
			Stream_Element_Array_Holders.Empty_Holder;
		Pos : Stream_Element_Offset := Stream_Element_Offset'First;
	end record;

end LMDB.Utils;
