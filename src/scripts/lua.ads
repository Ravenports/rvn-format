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

   Lua_Error          : exception;
   Lua_Type_Error     : exception;
   Lua_Override_Error : exception;

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
   subtype Lua_Integer is IC.ptrdiff_t;


   top_slot : constant Lua_Index := -1;

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

   function Get_Global_String
      (State : Lua_State;
       Name  : String) return String;

   function Get_Global_Type
     (State : Lua_State;
       Name  : String) return Lua_Type;

   procedure validate_argument
     (State : Lua_State;
      valid : Boolean;
      index : Positive;
      fail_msg : String);

   function retrieve_argument
     (State : Lua_State;
      index : Positive) return String;

   function API_luaL_loadstring
     (State : Lua_State;
      Str   : IC.Strings.chars_ptr) return Lua_Return_Code;
   pragma Import (C, API_luaL_loadstring, "luaL_loadstring");

   function API_lua_tolstring
     (State  : Lua_State;
      Index  : Integer;
      Length : access IC.size_t) return IC.Strings.chars_ptr;
   pragma Import (C, API_lua_tolstring, "lua_tolstring");

   function API_lua_pushlstring
     (State    : Lua_State;
      Str_Addr : System.Address;
      Str_Size : IC.size_t) return IC.Strings.chars_ptr;
   pragma Import (C, API_lua_pushlstring, "lua_pushlstring");

   procedure API_lua_pushboolean
     (State : Lua_State;
      Data : Integer);
   pragma Import (C, API_lua_pushboolean, "lua_pushboolean");

   procedure API_lua_getglobal
     (State : Lua_State;
      Name  : IC.Strings.chars_ptr);
   pragma Import (C, API_lua_getglobal, "lua_getglobal");

   --  Raises an error; it never returns
   function API_luaL_argerror
     (State    : Lua_State;
      narg     : Integer;
      extramsg : IC.Strings.chars_ptr) return Integer;
   pragma Import (C, API_luaL_argerror, "luaL_argerror");

   function API_luaL_checklstring
     (State : Lua_State;
      narg  : Integer;
      l     : access IC.size_t) return IC.Strings.chars_ptr;
   pragma Import (C, API_luaL_checklstring, "luaL_checklstring");

   procedure API_lua_pushcclosure
     (State : Lua_State;
      Fun   : Lua_Function;
      Closure_Size : Integer := 0);
   pragma Import (C, API_lua_pushcclosure, "lua_pushcclosure");

   function API_lua_absindex
     (State : Lua_State;
      Index : Lua_Index) return Lua_Index;
   pragma Import (C, API_lua_absindex, "lua_absindex");

   function API_lua_type
     (State : Lua_State;
      Index : Lua_Index) return Lua_Type;
   pragma Import (C, API_lua_type, "lua_type");


   -----------------------
   --  Stack Functions  --
   -----------------------

   --  Accepts any index, or 0, and sets the stack top to this index. If the
   --  new top is larger than the old one, then the new elements are filled
   --  with nil. If index is 0, then all stack elements are removed.
   procedure API_lua_settop (State : Lua_State; Index : Lua_Index);
   pragma Import (C, API_lua_settop, "lua_settop");

   --  Returns the index of the top element in the stack. Because indices start
   --  at 1, this result is equal to the number of elements in the stack (and
   --  so 0 means an empty stack).
   function API_lua_gettop (State : Lua_State) return Lua_Index;
   pragma Import (C, API_lua_gettop, "lua_gettop");

   function API_lua_atpanic (State : Lua_State; Fun : Lua_Function) return Lua_Function;
   pragma Import (C, API_lua_atpanic, "lua_atpanic");

   procedure API_lua_pushnil (State : Lua_State);
   pragma Import (C, API_lua_pushnil, "lua_pushnil");

   procedure API_lua_pushinteger (State : Lua_State; Data : Lua_Integer);
   pragma Import (C, API_lua_pushinteger, "lua_pushinteger");

   --  Pushes a copy of the element at the given index onto the stack.
   procedure API_lua_pushvalue (State : Lua_State; Index : Lua_Index);
   pragma Import (C, API_lua_pushvalue, "lua_pushvalue");

   --  Pops n elements from the stack.
   procedure Pop (State : Lua_State; N : Integer := 1);

   --  Pushes a nil value onto the stack.
   procedure Push (State : Lua_State);

   --  Push a string onto the stack
   procedure Push (State : Lua_State; Data : String);

   --  Push an integer onto the stack
   procedure Push (State : Lua_State; Data : Integer);

   --  Replace (Define?) the panic routine
   procedure set_panic (State : Lua_State; Fun : Lua_Function);


   -------------------
   --  Conversions  --
   -------------------

   function API_lua_toboolean
     (State : Lua_State;
      Index : Lua_Index) return Integer;
   pragma Import (C, API_lua_toboolean, "lua_toboolean");

   function API_lua_tonumberx
     (State   : Lua_State;
      Index   : Lua_Index;
      Success : access Integer) return Lua_Integer;
   pragma Import (C, API_lua_tonumberx, "lua_tonumberx");

   function API_lua_typename
     (State : Lua_State;
      Index : Lua_Index) return IC.Strings.chars_ptr;
   pragma Import (C, API_lua_typename, "lua_typename");

   --  Converts the Lua value at the given index to a Ada string. The Lua value must be a string
   --  or a number; otherwise, the function raise Lua_Type_Error. If the value is a number, then
   --  lua_tolstring also changes the actual value in the stack to a string. (This change
   --  confuses lua_next when lua_tolstring is applied to keys during a table traversal.)
   function convert_to_string (State : Lua_State; Index : Lua_Index) return String;

   --  Converts the Lua value at the given index to a boolean value.  Like all tests in Lua,
   --  To_Boolean returns true for any Lua value different from false and nil; otherwise it
   --  returns false. (If you want to accept only actual boolean values, use Is_Boolean to
   --  test the value's type.)
   function convert_to_boolean (State : Lua_State; Index : Lua_Index) return Boolean;

   --  Converts the Lua value at the given index to the Ada type Lua_Integer. The Lua value
   --  must be a number or a string convertible to a number otherwise, raise Lua_Type_Error.
   function convert_to_integer (State : Lua_State; Index : Lua_Index) return Integer;

   --  Returns the name of the type encoded by the value tp, which must be
   --  one the values returned by lua_type.
   function convert_to_type_name (State : Lua_State; Index : Lua_Index) return String;


   -----------------------
   --  Table Functions  --
   -----------------------

   --  Pushes onto the stack the value t[k], where t is the value at the given index.
   --  As in Lua, this function may trigger a metamethod for the "index" event
   procedure Get_Field
     (State : Lua_State;
      Index : Lua_Index;
      Name  : String);

   procedure API_lua_getfield
     (State : Lua_State;
      Index : Lua_Index;
      Name  : IC.Strings.chars_ptr);
   pragma Import (C, API_lua_getfield, "lua_getfield");

   procedure API_lua_createtable
     (State       : Lua_State;
      N_Seq_Elmts : Integer := 0;
      N_Elmts     : Integer := 0);
   pragma Import (C, API_lua_createtable, "lua_createtable");

   --  Does the equivalent to t[k] = v, where t is the value at the given index,
   --  v is the value at the top of the stack, and k is the value just below the top.
   --
   --  This function pops both the key and the value from the stack. As in Lua,
   --  this function may trigger a metamethod for the "newindex" event
   procedure API_lua_settable
     (State : Lua_State;
      Index : Lua_Index);
   pragma Import (C, API_lua_settable, "lua_settable");

   procedure API_lua_setfield
     (State : Lua_State;
      Index : Lua_Index;
      Name  : IC.Strings.chars_ptr);
   pragma Import (C, API_lua_setfield, "lua_setfield");

   --  Does the equivalent to t[k] = v, where t is the value at the given
   --  index and v is the value at the top of the stack.
   --
   --  This function pops the value from the stack. As in Lua, this function
   --  may trigger a metamethod for the "newindex" event
   procedure Set_Field
     (State    : Lua_State;
      Index    : Lua_Index;
      Name     : String;
      Override : Boolean := True);


   --------------------------
   --  Global Environment  --
   --------------------------
   procedure API_lua_setglobal
     (State : Lua_State;
      Name  : IC.Strings.chars_ptr);
   pragma Import (C, API_lua_setglobal, "lua_setglobal");

   --  Pops a value from the stack and sets it as the new value of global name.
   procedure Set_Global
     (State : Lua_State;
      Name  : String);

   --  Helper function to register an Ada object inside lua state. Name is the
   --  name in Lua global environment with which the function is associated to.
   --  To ease creation of hierarchies in the global environment, if Name
   --  contains '.' then a hierarchy using Lua tables is created. For example:
   --
   --     Register (S, "a.b.c");
   --
   --  will create a global table called "a". The table "a" will contain a
   --  table at index "b" and this last table will contain one element "b" set
   --  to the object on top of the stack. Note that an error will be raised
   --  in case you try to register twice at the same location.
   procedure Register_Object
     (State : Lua_State;
      Name  : String);

   --  Same as previous function except that the registered object is a
   --  closure passed as argument to the function instead of using the stack
   procedure Register_Function
     (State : Lua_State;
      Name  : String;
      Fun   : Lua_Function);


   -----------------------------------------
   --  PKG custom functions and routines  --
   -----------------------------------------


   function custom_print_msg (State : Lua_State) return Integer;
   pragma Convention (C, custom_print_msg);

   function custom_panic (State : Lua_State) return Integer;
   pragma Convention (C, custom_panic);

   function custum_prefix_path (State : Lua_State) return Integer;
   pragma Convention (C, custum_prefix_path);

   function custom_filecmp (State : Lua_State) return Integer;
   pragma Convention (C, custom_filecmp);

end Lua;
