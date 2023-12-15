--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

private with System;
private with Interfaces.C.Strings;
private with Interfaces.C_Streams;
private with Archive.Unix;

package Lua is

   --  This procedure executes the given script with the internal Lua 5.4 interpreter.
   --  The interpreter has been modified as follows:
   --  * os.execute() has been removed
   --  * os.exit()    has been removed
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
      script      : String;
      arg_chain   : String;
      msg_outfile : String;
      success     : out Boolean);

   --  Return a randomly-named msgfile path that isn't currently being used.
   function unique_msgfile_path return String;

   --  Disable any postrun messages and remove the temporary file
   procedure show_post_run_messages (msg_outfile : String);

private

   package IC renames Interfaces.C;
   package CS renames Interfaces.C_Streams;

   Lua_Error          : exception;
   Lua_Type_Error     : exception;
   Lua_Override_Error : exception;

   --  Lua state is the structure containing the state of the interpreter.
   type Lua_State is new System.Address;

   type Lua_User_Data is new System.Address;

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

   type luaL_Stream is record
      f : CS.FILEs;
      closef : Lua_Function;
   end record
     with Convention => C_Pass_By_Copy;
   type luaL_Stream_access is access luaL_Stream;

   subtype Lua_Index is Integer;
   subtype Lua_Integer is IC.ptrdiff_t;



   custerr_print_msg : constant IC.char_array := Archive.Unix.convert_to_char_array
     ("pkg.print_msg takes exactly one argument");

   custerr_prefix_path : constant IC.char_array := Archive.Unix.convert_to_char_array
     ("pkg.prefix_path takes exactly one argument");

   custerr_filecmp : constant IC.char_array := Archive.Unix.convert_to_char_array
     ("pkg.filecmp takes exactly two arguments");

   custerr_symlink : constant IC.char_array := Archive.Unix.convert_to_char_array
     ("pkg.symlink takes exactly two arguments");

   custerr_filecopy : constant IC.char_array := Archive.Unix.convert_to_char_array
     ("pkg.symlink takes exactly two arguments");

   custerr_exec : constant IC.char_array := Archive.Unix.convert_to_char_array
     ("pkg.exec takes exactly one argument");

   custerr_exec_payload : constant IC.char_array := Archive.Unix.convert_to_char_array
     ("pkg.exec payload: expected array of strings");

   custerr_stat : constant IC.char_array := Archive.Unix.convert_to_char_array
     ("pkg.stat takes exactly one argument");

   custerr_readdir : constant IC.char_array := Archive.Unix.convert_to_char_array
     ("pkg.readdir takes exactly one argument");

   custerr_os_exec : constant IC.char_array := Archive.Unix.convert_to_char_array
     ("os.execute not available");

   custerr_os_exit : constant IC.char_array := Archive.Unix.convert_to_char_array
     ("os.exit not available");

   custerr_os_remove : constant IC.char_array := Archive.Unix.convert_to_char_array
     ("os.remove takes exactly one argument");

   custerr_os_rename : constant IC.char_array := Archive.Unix.convert_to_char_array
     ("os.rename takes exactly two arguments");

   custerr_io_open : constant IC.char_array := Archive.Unix.convert_to_char_array
     ("io.open takes exactly two arguments");

   top_slot : constant Lua_Index := -1;
   MULTRET  : constant Integer := -1;

   --  Returns the nanosecond portion of the current time.
   --  This is used for a temporary file prefix.
   function random_extension return String;

   --  The argument chain is null-character delimited.  Push each chain link
   --  in as a separate argument.
   procedure insert_arguments (state : Lua_State; argument_chain : String);

   --  Create a new state using the default memory allocator.
   function New_State return Lua_State;
   pragma Import (C, New_State, "luaL_newstate");

   --  Destroys all objects in the given Lua state (calling the corresponding
   --  garbage - collection metamethods, if any) and frees all dynamic memory
   --  used by this state. On several platforms, you may not need to call this
   --  function, because all resources are naturally released when the host
   --  program ends. On the other hand, long - running programs that create
   --  multiple states, such as daemons or web servers, might need to close
   --  states as soon as they are not needed.
   procedure Close (State : Lua_State);
   pragma Import (C, Close, "lua_close");

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
      fail_msg : IC.char_array);

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

   procedure API_lua_getglobal
     (State : Lua_State;
      Name  : IC.Strings.chars_ptr);
   pragma Import (C, API_lua_getglobal, "lua_getglobal");

   --  Raises an error; it never returns
   function API_luaL_argerror
     (State    : Lua_State;
      narg     : Integer;
      extramsg : System.Address) return Integer;
   pragma Import (C, API_luaL_argerror, "luaL_argerror");

   --  Raises an error; it never returns
   function API_luaL_error
     (State    : Lua_State;
      fmt      : System.Address) return Integer;
   pragma Import (C, API_luaL_error, "luaL_error");

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

   function API_luaL_fileresult
      (State : Lua_State;
       stat  : Integer;
       Fname : System.Address) return Integer;
   pragma Import (C, API_luaL_fileresult, "luaL_fileresult");


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

   function API_lua_pushlstring
     (State    : Lua_State;
      Str_Addr : System.Address;
      Str_Size : IC.size_t) return IC.Strings.chars_ptr;
   pragma Import (C, API_lua_pushlstring, "lua_pushlstring");

   procedure API_lua_pushboolean
     (State : Lua_State;
      Data : Integer);
   pragma Import (C, API_lua_pushboolean, "lua_pushboolean");

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

   --  Returns the raw "length" of the value at the given index: for strings, this is the
   --  string length; for tables, this is the result of the length operator ('#') with no
   --  metamethods; for userdata, this is the size of the block of memory allocated for the
   --  userdata; for other values, it is 0.
   function API_lua_rawlen (State : Lua_State; Index : Lua_Index) return IC.size_t;
   pragma Import (C, API_lua_rawlen, "lua_rawlen");

   --  Pushes onto the stack the value t[n], where t is the table at the given
   --  index. The access is raw; that is, it does not invoke metamethods.
   procedure Raw_Geti
     (State : Lua_State;
      Index : Lua_Index;
      N     : Integer);
   pragma Import (C, Raw_Geti, "lua_rawgeti");

   --  Does the equivalent of t[n] = v, where t is the table at the given
   --  index and v is the value at the top of the stack.
   --  This function pops the value from the stack. The assignment is raw;
   --  that is, it does not invoke metamethods.
   procedure Raw_Seti
     (State : Lua_State;
      Index : Lua_Index;
      N     : Integer);
   pragma Import (C, Raw_Seti, "lua_rawseti");

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

   --  Returns true if the value at the given acceptable index is a table, and false otherwise.
   function is_table
     (State    : Lua_State;
      Index    : Lua_Index) return Boolean;

   --  Returns true if the value at the given acceptable index is a string, and false otherwise.
   function is_string
     (State    : Lua_State;
      Index    : Lua_Index) return Boolean;


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


   -----------------
   --  MetaTables --
   -----------------

   function API_lua_newuserdatauv
     (State : Lua_State;
      Size  : IC.size_t;
      nuvalue : Integer) return Lua_User_Data;
   pragma Import (C, API_lua_newuserdatauv, "lua_newuserdatauv");

   function API_luaL_checkudata
     (State : Lua_State;
      Index : Lua_Index;
      Str   : IC.Strings.chars_ptr) return Lua_User_Data;
   pragma Import (C, API_luaL_checkudata, "luaL_checkudata");

   --  Check if object at position Index has a metatable and it is the same as
   --  metatable stored at Name entry in the registry. In case the object has
   --  not metatable or the metatables do not correspond then return null and
   --  set an error in the current state. Otherwise return the address of the
   --  user data of the object.
   function Check_User_Data
     (State : Lua_State;
      Index : Lua_Index;
      Name  : String) return Lua_User_Data;

   function c_fdopen
     (fildes : Archive.Unix.File_Descriptor;
      mode   : IC.Strings.chars_ptr) return CS.FILEs;
   pragma Import (C, c_fdopen, "fdopen");

   function c_fclose (stream : CS.FILEs) return Integer;
   pragma Import (C, c_fclose, "fclose");

   procedure API_luaL_setmetatable
     (State : Lua_State;
      Str   : IC.Strings.chars_ptr);
   pragma Import (C, API_luaL_setmetatable, "luaL_setmetatable");

   procedure Set_Metatable
     (State : Lua_State;
      Name  : String);


   -----------------------------------------
   --  PKG custom functions and routines  --
   -----------------------------------------

   --  Returns given_path if given_starts with "/"
   --  Returns rootdir + / + given_path otherwise
   function dynamic_path (State : Lua_State; given_path : String) return String;

   function custom_print_msg (State : Lua_State) return Integer;
   pragma Convention (C, custom_print_msg);

   function custom_panic (State : Lua_State) return Integer;
   pragma Convention (C, custom_panic);

   function custom_prefix_path (State : Lua_State) return Integer;
   pragma Convention (C, custom_prefix_path);

   function custom_filecmp (State : Lua_State) return Integer;
   pragma Convention (C, custom_filecmp);

   function custom_symlink (State : Lua_State) return Integer;
   pragma Convention (C, custom_symlink);

   function custom_filecopy (State : Lua_State) return Integer;
   pragma Convention (C, custom_filecopy);

   function custom_exec (State : Lua_State) return Integer;
   pragma Convention (C, custom_exec);

   function custom_stat (State : Lua_State) return Integer;
   pragma Convention (C, custom_stat);

   function custom_readdir (State : Lua_State) return Integer;
   pragma Convention (C, custom_readdir);

   function override_os_execute (State : Lua_State) return Integer;
   pragma Convention (C, override_os_execute);

   function override_os_exit (State : Lua_State) return Integer;
   pragma Convention (C, override_os_exit);

   function override_remove (State : Lua_State) return Integer;
   pragma Convention (C, override_remove);

   function override_rename (State : Lua_State) return Integer;
   pragma Convention (C, override_rename);

   function override_open (State : Lua_State) return Integer;
   pragma Convention (C, override_open);

   function override_close (State : Lua_State) return Integer;
   pragma Convention (C, override_close);

end Lua;
