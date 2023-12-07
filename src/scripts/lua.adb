--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt


with Ada.Text_IO;
with Ada.Real_Time;
with Ada.Exceptions;
with Ada.Directories;
with Ada.Strings.Unbounded;


package body Lua is

   package TIO renames Ada.Text_IO;
   package RT  renames Ada.Real_Time;
   package EX  renames Ada.Exceptions;
   package DIR renames Ada.Directories;
   package ASU renames Ada.Strings.Unbounded;


   ----------------------
   --  run_lua_script  --
   ----------------------
   procedure run_lua_script
     (namebase    : String;
      subpackage  : String;
      variant     : String;
      prefix      : String;
      root_dir    : String;
      upgrading   : Boolean;
      script      : String)
   is
      msg_outfile : constant String := unique_msgfile_path;
      state  : constant Lua_State := New_State;
      status : Lua_Return_Code;
   begin
      Open_Libs (state);

      begin
         Load_String (state, script);
      exception
         when LE : Lua_Error =>
            TIO.Put_Line (TIO.Standard_Error,
                          "Failed to load Lua script> " &  EX.Exception_Message (LE));
            return;
      end;

      Set_Global_String (state, "pkg_namebase",   namebase);
      Set_Global_String (state, "pkg_subpackage", subpackage);
      Set_Global_String (state, "pkg_variant",    variant);
      Set_Global_String (state, "pkg_prefix",     prefix);
      Set_Global_String (state, "pkg_rootdir",    root_dir);
      Set_Global_String (state, "msgfile_path",   msg_outfile);
      Set_Global_Boolean (State, "pkg_upgrade",   upgrading);

      set_panic (State, custom_panic'Access);
      Register_Function (State, "pkg.print_msg", custom_print_msg'Access);
      Register_Function (State, "pkg.prefixed_path", custum_prefix_path'Access);

      status := Protected_Call (state);
      case status is
         when LUA_OK =>
            null;
         when others =>
            TIO.Put_Line (TIO.Standard_Error, "Failed to execute Lua script:" & status'Img);
            TIO.Put_Line (TIO.Standard_Error, convert_to_string (State, top_slot));
      end case;

      --  Provide delayed message text if it exists
      if DIR.Exists (msg_outfile) then
         declare
            handle : TIO.File_Type;
         begin
            TIO.Open (File => handle,
                      Mode => TIO.In_File,
                      Name => msg_outfile);
            while not TIO.End_Of_File (handle) loop
               TIO.Put_Line (TIO.Get_Line (handle));
            end loop;
         exception
            when others => null;
         end;
         DIR.Delete_File (msg_outfile);
      end if;

   end run_lua_script;


   ------------------------
   --  random_extension  --
   ------------------------
   function random_extension return String
   is
      right_now : constant RT.Time := RT.Clock;
      seconds   : RT.Seconds_Count;
      nanospan  : RT.Time_Span;
      nduration : Duration;
   begin
      RT.Split (right_now, seconds, nanospan);
      nduration := RT.To_Duration (nanospan);
      declare
         durstr : constant String := nduration'Img;
      begin
         --  durstr in format " 0.xxxxxxxxx" (leading space)
         return durstr (durstr'First + 3 .. durstr'Last);
      end;
   end random_extension;


   ---------------------------
   --  unique_msgfile_path  --
   ---------------------------
   function unique_msgfile_path return String
   is
      function tmp return String
      is
         root_tmp : constant String := "/tmp";
      begin
         if DIR.Exists (root_tmp) then
            case DIR.Kind (root_tmp) is
               when DIR.Directory => return root_tmp & "/";
               when others => null;
            end case;
         end if;
         return "";
      end tmp;
   begin
      loop
         declare
            extension  : constant String := random_extension;
            candidate  : constant String := tmp & ".rvn_outmsg." & extension;
         begin
            if not DIR.Exists (candidate) then
               return candidate;
            end if;
         end;
      end loop;
   end unique_msgfile_path;


   -------------------
   --  Load_String  --
   -------------------
   procedure Load_String
     (State : Lua_State;
      Str   : String)
   is
      Str_Ptr : IC.Strings.chars_ptr := IC.Strings.New_String (Str);
      Result  : constant Lua_Return_Code := API_luaL_loadstring (State, Str_Ptr);
   begin
      IC.Strings.Free (Str_Ptr);

      if Result /= LUA_OK then
         declare
            Error_Msg : constant String := convert_to_string (State, -1);
         begin
            Pop (State);
            raise Lua_Error with Result'Img & ": " & Error_Msg;
         end;
      end if;
   end Load_String;


   ------------
   --  Pop  --
   ------------
   procedure Pop (State : Lua_State; N : Integer := 1) is
   begin
      API_lua_settop (State, -N - 1);
   end Pop;


   ---------------
   --  Push #1  --
   ---------------
   procedure Push (State : Lua_State) is
   begin
      API_lua_pushnil (State);
   end Push;

   ---------------
   --  Push #2  --
   ---------------
   procedure Push (State : Lua_State; Data : String)
   is
      Result : IC.Strings.chars_ptr;
      pragma Unreferenced (Result);
   begin
      Result := API_lua_pushlstring (State, Data'Address, Data'Length);
   end Push;


   -------------------------
   --  convert_to_string  --
   -------------------------
   function convert_to_string (State : Lua_State; Index : Lua_Index) return String is
   begin
      return IC.Strings.Value (API_lua_tolstring (State, Index, null));
   exception
      when others =>
         return "Failed to retrieve string value";
   end convert_to_string;


   --------------------------
   --  convert_to_boolean  --
   --------------------------
   function convert_to_boolean (State : Lua_State; Index : Lua_Index) return Boolean is
   begin
      if API_lua_toboolean (State, Index) = 0 then
         return False;
      end if;
      return True;
   end convert_to_boolean;


   --------------------------
   --  convert_to_integer  --
   --------------------------
   function convert_to_integer (State : Lua_State; Index : Lua_Index) return Integer
   is
      Result : constant IC.ptrdiff_t := API_lua_tonumberx (State, Index, null);
   begin
      return Integer (Result);
   end convert_to_integer;


   ----------------------------
   --  convert_to_type_name  --
   ----------------------------
   function convert_to_type_name
     (State : Lua_State;
      Index : Lua_Index) return String
   is
      Result : constant IC.Strings.chars_ptr := API_lua_typename (State, Index);
   begin
      return IC.Strings.Value (Result);
   end convert_to_type_name;


   -------------------------
   --  Set_Global_String  --
   -------------------------
   procedure Set_Global_String
     (State : Lua_State;
      Name  : String;
      value : String)
   is
      Result   : IC.Strings.chars_ptr;
      Name_Ptr : IC.Strings.chars_ptr := IC.Strings.New_String (Name);

      pragma Unreferenced (Result);
   begin
      Result := API_lua_pushlstring (State, value'Address, value'Length);
      API_lua_setglobal (State, Name_Ptr);
      IC.Strings.Free (Name_Ptr);
   end Set_Global_String;


   --------------------------
   --  Set_Global_Boolean  --
   --------------------------
   procedure Set_Global_Boolean
     (State : Lua_State;
      Name  : String;
      value : Boolean)
   is
      Name_Ptr : IC.Strings.chars_ptr := IC.Strings.New_String (Name);
   begin
      if value then
         API_lua_pushboolean (State, 1);
      else
         API_lua_pushboolean (State, 0);
      end if;
      API_lua_setglobal (State, Name_Ptr);
      IC.Strings.Free (Name_Ptr);
   end Set_Global_Boolean;


   ------------------
   --  Set_Global  --
   ------------------
   procedure Set_Global
     (State : Lua_State;
      Name  : String)
   is
      Name_Ptr : IC.Strings.chars_ptr := IC.Strings.New_String (Name);
   begin
      API_lua_setglobal (State, Name_Ptr);
      IC.Strings.Free (Name_Ptr);
   end Set_Global;


   -------------------------
   --  Get_Global_String  --
   -------------------------
   function Get_Global_String
      (State : Lua_State;
       Name  : String) return String
   is
      Name_Ptr : IC.Strings.chars_ptr := IC.Strings.New_String (Name);
   begin
      API_lua_getglobal (State, Name_Ptr);
      IC.Strings.Free (Name_Ptr);
      return convert_to_string (state, top_slot);
   end Get_Global_String;


   -----------------------
   --  Get_Global_Type  --
   -----------------------
   function Get_Global_Type
     (State : Lua_State;
      Name  : String) return Lua_Type
   is
      Name_Ptr : IC.Strings.chars_ptr := IC.Strings.New_String (Name);
   begin
      API_lua_getglobal (State, Name_Ptr);
      IC.Strings.Free (Name_Ptr);
      return API_lua_type (State, top_slot);
   end Get_Global_Type;


   -------------------------
   --  validate_argument  --
   -------------------------
   procedure validate_argument
     (State : Lua_State;
      valid : Boolean;
      index : Positive;
      fail_msg : String)
   is
      extramsg : IC.Strings.chars_ptr := IC.Strings.New_String (fail_msg);
      result : Integer;
      pragma Unreferenced (result);
   begin
      if not valid then
         result := API_luaL_argerror (State, index, extramsg);
      end if;
      --  memory leak, we need get here.
      IC.Strings.Free (extramsg);
   end validate_argument;


   -------------------------
   --  retrieve_argument  --
   -------------------------
   function retrieve_argument
     (State : Lua_State;
      index : Positive) return String
   is
      result : IC.Strings.chars_ptr;
   begin
      result := API_luaL_checklstring (State, index, null);
      return IC.Strings.Value (result);
   end retrieve_argument;


   ------------------------
   --  custom_print_msg  --
   ------------------------
   function custom_print_msg (State : Lua_State) return Integer
   is
      n       : constant Lua_Index := API_lua_gettop (State);
      msgfile : constant String := Get_Global_String (State, "msgfile_path");
      handle  : TIO.File_Type;
      valid   : Boolean;
      narg    : Positive := n;
   begin
      valid := n = 1;
      if n > 1 then
         narg := 2;
      end if;
      --  validate_argument will not return on failure
      validate_argument (State, valid, narg, "pkg.print_msg takes exactly one argument");

      if DIR.Exists (msgfile) then
         TIO.Open (handle, TIO.Append_File, msgfile);
      else
         TIO.Create (handle, TIO.Out_File, msgfile);
      end if;
      TIO.Put_Line (handle, retrieve_argument (State, 1));
      TIO.Close (handle);
      return 0;
   exception
      when others =>
         if TIO.Is_Open (handle) then
            TIO.Close (handle);
         end if;
         return 1;
   end custom_print_msg;


   -----------------
   --  Get_Field  --
   -----------------
   procedure Get_Field
     (State : Lua_State;
      Index : Lua_Index;
      Name  : String)
   is
      Result : IC.Strings.chars_ptr := IC.Strings.New_String (Name);
   begin
      API_lua_getfield (State, Index, Result);
      IC.Strings.Free (Result);
   end Get_Field;


   -----------------
   --  Set_Field  --
   -----------------
   procedure Set_Field
     (State    : Lua_State;
      Index    : Lua_Index;
      Name     : String;
      Override : Boolean := True)
   is
      Result : IC.Strings.chars_ptr := IC.Strings.New_String (Name);
   begin
      if not Override then
         Get_Field (State, Index, Name);
         if API_lua_type (State, top_slot) /= LUA_TNIL then
            Pop (State);
            IC.Strings.Free (Result);
            raise Lua_Override_Error with "element already set";
         end if;
         Pop (State);
      end if;

      API_lua_setfield (State, Index, Result);
      IC.Strings.Free (Result);
   end Set_Field;


   -------------------------
   --  Register_Function  --
   -------------------------
   procedure Register_Function
     (State : Lua_State;
      Name  : String;
      Fun   : Lua_Function)
   is
   begin
      API_lua_pushcclosure (State, Fun);
      Register_Object (State, Name);
   end Register_Function;


   -----------------------
   --  Register_Object  --
   -----------------------
   procedure Register_Object
     (State : Lua_State;
      Name  : String)
   is
      Start           : Integer := Name'First;
      Is_First        : Boolean := True;
      Need_Global     : Boolean := False;
      Pop_Times       : Integer := 0;
      Set_Table_Times : Integer := 0;
      Global_First    : Integer := 0;
      Global_Last     : Integer := 0;
      Obj_Index       : constant Lua_Index := API_lua_absindex (State, top_slot);
   begin
      --  check that name does not start or ends with . and cannot be empty

      for Index in Name'Range loop
         if Name (Index) = '.' then

            declare
               Name_Comp : constant String := Name (Start .. Index - 1);
               Comp_Type : Lua_Type;
            begin

               if Is_First then
                  Comp_Type := Get_Global_Type (State, Name_Comp);
                  Global_First := Start;
                  Global_Last := Index - 1;
               else
                  Get_Field (State, top_slot, Name_Comp);
                  Comp_Type := API_lua_type (State, top_slot);
               end if;

               if Comp_Type = LUA_TNIL then
                  --  Create the table
                  Pop (State);
                  if not Is_First then
                     Push (State, Name_Comp);
                     Set_Table_Times := Set_Table_Times + 1;
                  else
                     Need_Global := True;
                  end if;

                  API_lua_createtable (State);

               elsif Comp_Type /= LUA_TTABLE then
                  raise Lua_Type_Error with "expecting table";
               else
                  Pop_Times := Pop_Times + 1;
               end if;

               Is_First := False;
            end;
            Start := Index + 1;
         end if;
      end loop;

      if Start = Name'First then
         Set_Global (State, Name);
      else
         API_lua_pushvalue (State, Obj_Index);
         --  At least one dot has been found so create a hierarchy
         Set_Field (State, -2, Name (Start .. Name'Last),
                    Override => False);

         for J in 1 .. Set_Table_Times loop
            API_lua_settable (State, -3);
         end loop;

         if Pop_Times > 0 then
            Pop (State, Pop_Times);
         end if;

         if Need_Global then
            Set_Global (State, Name (Global_First .. Global_Last));
         end if;

         --  Remove the object from the stack
         Pop (State);
      end if;
   end Register_Object;


   --------------------
   --  custom_panic  --
   --------------------
   function custom_panic (State : Lua_State) return Integer
   is
      top   : Lua_Index;
      ltype : Lua_Type;
      stack : ASU.Unbounded_String;
      LF    : constant Character := Character'Val (10);
      TAB   : constant Character := Character'Val (9);
   begin
      top := API_lua_gettop (State);
      ASU.Append (stack, "Stack" & LF);
      ASU.Append (stack, "---------" & LF);
      ASU.Append (stack, TAB & "Type   Data" & LF);
      ASU.Append (stack, TAB & "-----------" & LF);

      for index in 1 .. top loop
         ltype := API_lua_type (State, index);
         ASU.Append (stack, ltype'Img & TAB);
         case ltype is
            when LUA_TSTRING  =>
               ASU.Append (stack, "String:  " & convert_to_string (State, index) & LF);
            when LUA_TBOOLEAN =>
               ASU.Append (stack, "Boolean: " & convert_to_boolean (State, index)'Img & LF);
            when LUA_TNUMBER  =>
               ASU.Append (stack, "Number:  " & convert_to_integer (State, index)'Img & LF);
            when others =>
               ASU.Append (Stack, "Other:   " & convert_to_type_name (State, index) & LF);
         end case;
      end loop;

      TIO.Put (ASU.To_String (stack));
      return 0;
   end custom_panic;


   -----------------
   --  set_panic  --
   -----------------
   procedure set_panic (State : Lua_State; Fun : Lua_Function)
   is
      old_function : Lua_Function;

      pragma Unreferenced (old_function);
   begin
      old_function := API_lua_atpanic (State, Fun);
   end set_panic;


   --------------------------
   --  custum_prefix_path  --
   --------------------------
   function custum_prefix_path (State : Lua_State) return Integer
   is
      n       : constant Lua_Index := API_lua_gettop (State);
      prefix  : constant String := Get_Global_String (State, "pkg_prefix");
      valid   : Boolean;
      narg    : Positive := n;
   begin
      valid := n = 1;
      if n > 1 then
         narg := 2;
      end if;
      --  validate_argument will not return on failure
      validate_argument (State, valid, narg, "pkg.prefix_path takes exactly one argument");
      declare
         inpath : constant String := retrieve_argument (State, 1);
      begin
         if inpath (inpath'First) = '/' then
            push (state, inpath);
         else
            push (state, prefix & '/' & inpath);
         end if;
      end;
      return 1;
   end custum_prefix_path;

end Lua;
