--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

private with System;
private with interfaces.C.Strings;

package Lua is

   --  This procedure executes the given script with the internal Lua 5.4 interpreter.
   --  The interpreter has been modified as follows:
   --  * os.execute() has been removed
   --  * os.open()    has been modified to assume filenames relative to rootdir
   --  * os.remove()  has been modified to assume filenames relative to rootdir
   --  * os.rename()  has been modified to assume filenames relative to rootdir
   --
   --  The following variables have been populated:
   --  * pkg_namebase
   --  * pkg_subpackage
   --  * pkg_variant
   --  * pkg_prefix
   --  * pkg_rootdir
   --  * pkg_upgrade (boolean)
   --
   --  The following functions have been added (see rvn-lua-scripts.5 man page)
   --  pkg.prefixed_path (in) returns out string
   --  pkg.filecmp (file1, file2) returns (0|1|2)
   --  pkg.copy (source, destination) returns (0|-1)
   --  pkg.stat (file) returns st structure (type, size, uid, gid)
   --  pkg.readdir (path) returns ipair list of elements of the directory (not . or ..)
   --
   --  The following procedures have been added (see man page again)
   --  pkg,symlink (source, destination)    (creates a symlink)
   --  pkg.print_msg (msg)                  (sends messages to user shown at end of process)
   --  pkg.exec (arguments)                 (executes a command)

   procedure run_lua_script
     (namebase    : String;
      subpackage  : String;
      variant     : String;
      prefix      : String;
      root_dir    : String;
      upgrading   : Boolean;
      script      : String);

private

   package IC renames Interfaces.C;

   Lua_Error : exception;

   --  Lua state is the structure containing the state of the interpreter.
   type Lua_State is new System.Address;

   --  Callback
   type Lua_Function is access function (State : Lua_State) return Integer;

   type Lua_Return_Code is
     (LUA_OK,
      LUA_YIELD,
      LUA_ERRRUN,
      LUA_ERRSYNTAX,
      LUA_ERRMEM,
      LUA_ERRGCMM,
      LUA_ERRERR,
      LUA_ERRFILE);
   pragma Convention (C, Lua_Return_Code);

   type Lua_Type is
     (LUA_TNONE,
      LUA_TNIL,
      LUA_TBOOLEAN,
      LUA_TLIGHTUSERDATA,
      LUA_TNUMBER,
      LUA_TSTRING,
      LUA_TTABLE,
      LUA_TFUNCTION,
      LUA_TUSERDATA,
      LUA_TTHREAD);

   for Lua_Type use
     (LUA_TNONE          => -1,
      LUA_TNIL           => 0,
      LUA_TBOOLEAN       => 1,
      LUA_TLIGHTUSERDATA => 2,
      LUA_TNUMBER        => 3,
      LUA_TSTRING        => 4,
      LUA_TTABLE         => 5,
      LUA_TFUNCTION      => 6,
      LUA_TUSERDATA      => 7,
      LUA_TTHREAD        => 8);
   pragma Convention (C, Lua_Function);

   subtype Lua_Index is Integer;



   --  Returns the nanosecond portion of the current time.
   --  This is used for a temporary file prefix.
   function random_extension return String;

   --  Return a randomly-named msgfile path that isn't currently being used.
   function unique_msgfile_path return String;

   --  Create a new state using the default memory allocator.
   function New_State return Lua_State;
   pragma Import (C, New_State, "luaL_newstate");

   --  Load a string and raise Lua_Error in case of error
   procedure Load_String
     (State : Lua_State;
      Str   : String);

   --  Opens all standard Lua libraries into the given state.
   procedure Open_Libs (State : Lua_State);
   pragma Import (C, Open_Libs, "luaL_openlibs");

   function Protected_Call
     (State    : Lua_State;
      Nargs    : Integer := 0;
      NResults : Integer := 0;
      Err_Fun  : Integer := 0;
      Context  : Integer := 0;
      Cont_Fun : Lua_Function := null)
      return Lua_Return_Code;
   pragma Import (C, Protected_Call, "lua_pcallk");

   procedure Set_Global_String
     (State : Lua_State;
      Name  : String;
      value : String);

   procedure Set_Global_Boolean
     (State : Lua_State;
      Name  : String;
      value : Boolean);

   function API_luaL_loadstring
     (State : Lua_State;
      Str   : IC.Strings.chars_ptr)
      return Lua_Return_Code;
   pragma Import (C, API_luaL_loadstring, "luaL_loadstring");

   function API_lua_tolstring
     (State  : Lua_State;
      Index  : Integer;
      Length : out IC.size_t)
      return IC.Strings.chars_ptr;
   pragma Import (C, API_lua_tolstring, "lua_tolstring");

   function API_lua_pushlstring
     (State    : Lua_State;
      Str_Addr : System.Address;
      Str_Size : IC.size_t)
      return IC.Strings.chars_ptr;
   pragma Import (C, API_lua_pushlstring, "lua_pushlstring");

   procedure API_lua_pushboolean
     (State : Lua_State;
      Data : Integer);
   pragma Import (C, API_lua_pushboolean, "lua_pushboolean");

   procedure API_lua_setglobal
     (State : Lua_State;
      Name  : IC.Strings.chars_ptr);
   pragma Import (C, API_lua_setglobal, "lua_setglobal");


   -----------------------
   --  Stack Functions  --
   -----------------------

   --  Accepts any index, or 0, and sets the stack top to this index. If the
   --  new top is larger than the old one, then the new elements are filled
   --  with nil. If index is 0, then all stack elements are removed.
   procedure API_lua_settop (State : Lua_State; Index : Lua_Index);
   pragma Import (C, API_lua_settop, "lua_settop");

   --  Pops n elements from the stack.
   procedure Pop (State : Lua_State; N : Integer := 1);


   -------------------
   --  Conversions  --
   -------------------

   --  Converts the Lua value at the given index to a Ada string. The Lua value must be a string
   --  or a number; otherwise, the function raise Lua_Type_Error. If the value is a number, then
   --  lua_tolstring also changes the actual value in the stack to a string. (This change
   --  confuses lua_next when lua_tolstring is applied to keys during a table traversal.)
   function convert_to_string (State : Lua_State; Index : Lua_Index) return String;

end Lua;
