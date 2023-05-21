-- Copyright (c) 2021-2022 nytpu <alex [at] nytpu.com>
-- SPDX-License-Identifier: MPL-2.0
-- For more license details, see LICENSE or <https://www.mozilla.org/en-US/MPL/2.0/>.

pragma Ada_2012;

with Ada.Command_Line;
with Ada.Strings.Maps;
with Ada.Unchecked_Deallocation;

package body Getopts is

	----------------------
	-- Finished_Options --
	----------------------

	function Finished_Options (P : Parser) return Boolean
	is (P.I.Finished_Options);


	--------------------
	-- Argument_Count --
	--------------------

	function Argument_Count (P : Parser) return Natural
	is
		-- Add 1 because P.Argument is pointing at the first non-option
		-- argument and Ada.Command_Line.Argument_Count is inclusive, not
		-- exclusive.
		C : constant Integer :=
			Ada.Command_Line.Argument_Count - (P.I.Start_Of_Arguments - 1);
	begin
		return (if C < 0 then 0 else Natural(C));
	end Argument_Count;


	--------------
	-- Argument --
	--------------

	function Argument (P : Parser; Idx : Positive) return String is
		-- Idx - 1 to negate the + 1 in Argument_Count
		I : constant Natural := P.I.Start_Of_Arguments + (Natural(Idx) - 1);
	begin
		if I = 0 then
			raise Program_Error;
		end if;
		return Ada.Command_Line.Argument(I);
	end Argument;


	----------------
	-- In_Options --
	----------------

	function In_Options (Csr : Options_Cursor) return Boolean
	is (not Csr.I.Finished_Options);


	------------------
	-- Has_Argument --
	------------------

	function Has_Argument (Csr : Options_Cursor) return Boolean
	is (Csr.Optarg /= Null_Unbounded_String);


	------------
	-- Option --
	------------

	function Option (Csr : Options_Cursor) return Character
	is (Csr.Optchar);


	------------
	-- Optarg --
	------------

	function Optarg (Csr : Options_Cursor) return String
	is (To_String(Csr.Optarg));


	------------
	-- Getopt --
	------------

	function Getopt
		(P : Parser; Optstring : String)
		return Options_Iterators.Forward_Iterator'Class
	is
	begin
		if Ada.Command_Line.Argument_Count = 0 then
			P.I.Finished_Options := True;
		end if;
		return Options_Iterator'(
			I => P.I,
			Optstring => To_Unbounded_String(
				(if Optstring'Length > 1 and then Optstring(Optstring'First) = ':'
				then Optstring(Optstring'First + 1 .. Optstring'Last)
				else Optstring)
			)
		);
	end Getopt;


	----------------
	-- Initialize --
	----------------

	overriding procedure Initialize (Object : in out Parser) is
	begin
		if Object.I = null then
			Object.I := new Parser_Internal;
		end if;
	end Initialize;


	------------
	-- Adjust --
	------------

	overriding procedure Adjust (Object : in out Parser) is
		O : constant Parser_Internal := Object.I.all;
	begin
		Object.I := new Parser_Internal'(O);
	end Adjust;


	--------------
	-- Finalize --
	--------------

	overriding procedure Finalize (Object : in out Parser) is
		procedure Free is new Ada.Unchecked_Deallocation
			(Parser_Internal, Parser_Internal_Access);
	begin
		if Object.I /= null then
			Free(Object.I);
		end if;
	end Finalize;


	-----------
	-- First --
	-----------

	overriding function First
		(Object : Options_Iterator) return Options_Cursor
	is (Next(
		Object,
		(
			I => Object.I,
			Argument => 1,
			Fresh_Argument => True,
			others => <>
		)
	));


	----------
	-- Next --
	----------

	overriding function Next
		(Object : Options_Iterator; Position : Options_Cursor)
		return Options_Cursor
	is
		C : Options_Cursor := Position;
	begin
		if C.I.Finished_Options then
			return C;
		elsif C.Argument > Ada.Command_Line.Argument_Count then
			C.I.Finished_Options := True;
			C.I.Start_Of_Arguments := C.Argument;
			return C;
		end if;

		declare
			Current : constant String := Ada.Command_Line.Argument(C.Argument);
			Optstring_Index : Natural := 0;
		begin
			if C.Fresh_Argument then
				if
					Current'Length <= 1 or else
					Current(Current'First) /= '-'
				then
					C.I.Finished_Options := True;
					C.I.Start_Of_Arguments := C.Argument;
					return C;
				elsif Current = "--" then
					C.I.Finished_Options := True;
					C.I.Start_Of_Arguments := C.Argument + 1;
					return C;
				end if;
				C.Index := Current'First + 1;
			end if;
			C.Fresh_Argument := False;

			C.Optchar := Current(C.Index);
			Optstring_Index := Index(
				Object.Optstring,
				Ada.Strings.Maps.To_Set(C.Optchar)
			);
			if Optstring_Index = 0 then
				C.Optarg := Null_Unbounded_String;
				raise Invalid_Option with "Invalid option '" & C.Optchar & "'";
			end if;

			if
				Optstring_Index < Length(Object.Optstring) and then
				Element(Object.Optstring, Optstring_Index + 1) = ':'
			then
				if C.Index + 1 <= Current'Last then
					C.Optarg := To_Unbounded_String(
						Current(C.Index + 1 .. Current'Last)
					);
					C.Argument := C.Argument + 1;
					C.Fresh_Argument := True;
				else
					if C.Argument + 1 > Ada.Command_Line.Argument_Count then
						raise Missing_Parameter
							with "Option '" & C.Optchar & "' missing argument.";
					end if;
					C.Optarg := To_Unbounded_String(
						Ada.Command_Line.Argument(C.Argument + 1)
					);
					C.Argument := C.Argument + 2;
					C.Fresh_Argument := True;
				end if;
			else
				C.Optarg := Null_Unbounded_String;
				if C.Index + 1 > Current'Last then
					C.Argument := C.Argument + 1;
					C.Fresh_Argument := True;
				else
					C.Index := C.Index + 1;
				end if;
			end if;
		end;
		return C;
	end Next;

end Getopts;
