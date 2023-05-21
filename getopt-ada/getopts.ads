-- Copyright (c) 2021-2022 nytpu <alex [at] nytpu.com>
-- SPDX-License-Identifier: MPL-2.0
-- For more license details, see LICENSE or <https://www.mozilla.org/en-US/MPL/2.0/>.

pragma Ada_2012;

with Ada.Finalization;
with Ada.Iterator_Interfaces;
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;

package Getopts is
	pragma Preelaborate;

	-----------------
	-- Base Parser --
	-----------------

	type Parser is tagged private;

	-- Whether or not the Parser has finished parsing the options and has
	-- reached the non-option arguments.
	function Finished_Options (P : Parser) return Boolean;

	-- The number of non-option arguments after the options have been parsed.
	function Argument_Count (P : Parser) return Natural
		with Pre => P.Finished_Options;

	-- Return the non-option argument at index Idx
	function Argument (P : Parser; Idx : Positive) return String
		with Pre => P.Finished_Options and then Idx <= P.Argument_Count;


	-----------------------
	-- Options Iteration --
	-----------------------

	type Options_Cursor is private;

	-- Simple inverse of Finished_Options on the Parser used to instantiate the
	-- cursor.
	function In_Options (Csr : Options_Cursor) return Boolean;

	-- Whether or not the current option has an associated argument.
	function Has_Argument (Csr : Options_Cursor) return Boolean
		with Pre => In_Options(Csr);

	-- The current option character
	function Option (Csr : Options_Cursor) return Character
		with Pre => In_Options(Csr);

	-- The current option argument, if present.
	function Optarg (Csr : Options_Cursor) return String
		with Pre => In_Options(Csr) and then Has_Argument(Csr);

	-- Set up Ada 2005 iterators
	package Options_Iterators is
		new Ada.Iterator_Interfaces (Options_Cursor, In_Options);

	-- Given an optstring of the format specified in the POSIX specification,
	-- parse the command line parameters from Ada.Command_Line following the
	-- POSIX Utility Syntax Guidelines.
	-- Sample optstrings:
	-- abcd - accept flags a, b, c, and d; none of which take parameters.
	-- a:b:c:d: - accept flags a, b, c, and d; all of which require parameters.
	-- W:xy:Z - accept flags W, x, y, and Z.  x and Z don't take parameters, W
	--          and y require parameters.
	-- :ab:cd - accept flags a, b, c, and d; b requires a parameter.  while
	--          POSIX specifies a purpose for a leading colon (':'), this
	--          library ignores it as the appropriate error exceptions are
	--          always raised.
	function Getopt (P : Parser; Optstring : String)
		return Options_Iterators.Forward_Iterator'Class
		with Pre => not P.Finished_Options;


	----------------
	-- Exceptions --
	----------------

	-- Raised when an option character not present in Optstring is passed.
	Invalid_Option : exception;

	-- Raised when an option that requires an argument does not have an
	-- argument.
	Missing_Parameter : exception;


private
	type Parser_Internal  is record
		Finished_Options : Boolean := False;

		-- The start of the non-option arguments in the range
		-- 1 .. Ada.Command_Line.Argument_Count.  Only valid when
		-- Finished_Options is True.
		Start_Of_Arguments : Positive := 1;
	end record;

	type Parser_Internal_Access is access Parser_Internal;

	type Parser is new Ada.Finalization.Controlled with record
		I : Parser_Internal_Access := null;
	end record;

	overriding procedure Initialize (Object : in out Parser);
	overriding procedure Adjust (Object : in out Parser);
	overriding procedure Finalize (Object : in out Parser);

	type Options_Iterator is new Options_Iterators.Forward_Iterator with record
		I : Parser_Internal_Access := null;

		-- POSIX optstring we are using to parse
		Optstring : Unbounded_String := Null_Unbounded_String;
	end record;

	overriding function First (Object : Options_Iterator)
		return Options_Cursor;
	overriding function Next
		(Object : Options_Iterator; Position : Options_Cursor)
		return Options_Cursor;

	type Options_Cursor is record
		I : Parser_Internal_Access := null;

		-- The argument we are currently parsing options from.
		Argument : Positive := 1;

		-- Whether or not we just incremented Argument or if we're in the
		-- middle of parsing one.
		Fresh_Argument : Boolean := True;

		-- The current index within the current argument that we are
		-- parsing; only valid when not Fresh_Argument
		Index : Positive;

		-- The most recently parsed option character and argument.
		Optchar : Character;
		Optarg : Unbounded_String := Null_Unbounded_String;
	end record;

end Getopts;
