--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Ada.Real_Time;
with Ada.Exceptions;
with Ada.Directories;
with Ada.Strings.Unbounded;
with Archive.Dirent.Scan;
with Archive.Misc;
with GNAT.OS_Lib;
with Blake_3;


package body Lua is

   package TIO renames Ada.Text_IO;
   package RT  renames Ada.Real_Time;
   package EX  renames Ada.Exceptions;
   package DIR renames Ada.Directories;
   package ASU renames Ada.Strings.Unbounded;
   package MSC renames Archive.Misc;


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
      script      : String;
      arg_chain   : String;
      msg_outfile : String;
      out_handle  : Ada.Text_IO.File_Type;
      success     : out Boolean)
   is
      state  : constant Lua_State := New_State;
      status : Lua_Return_Code;
      top    : Lua_Index;
   begin
      Open_Libs (state);

      begin
         top := API_lua_gettop (state);
         Load_String (state, script);
      exception
         when LE : Lua_Error =>
            TIO.Put_Line (TIO.Standard_Error,
                          "Failed to load Lua script> " &  EX.Exception_Message (LE));
            success := False;
            return;
      end;

      Set_Global_String (state, "pkg_namebase",   namebase);
      Set_Global_String (state, "pkg_subpackage", subpackage);
      Set_Global_String (state, "pkg_variant",    variant);
      Set_Global_String (state, "pkg_prefix",     prefix);
      Set_Global_String (state, "pkg_rootdir",    root_dir);
      Set_Global_String (state, "msgfile_path",   msg_outfile);
      Set_Global_Boolean (state, "pkg_upgrade",   upgrading);

      --  set_panic (state, custom_panic'Access);
      Register_Function (state, "pkg.print_msg", custom_print_msg'Access);
      Register_Function (state, "pkg.prefixed_path", custom_prefix_path'Access);
      Register_Function (state, "pkg.filecmp", custom_filecmp'Access);
      Register_Function (state, "pkg.symlink", custom_symlink'Access);
      Register_Function (state, "pkg.readdir", custom_readdir'Access);
      Register_Function (state, "pkg.copy", custom_filecopy'Access);
      Register_Function (state, "pkg.exec", custom_exec'Access);
      Register_Function (state, "pkg.stat", custom_stat'Access);

      --  Override / disable existing functions
      Register_Function (state, "os.execute", override_os_execute'Access);
      Register_Function (state, "os.exit", override_os_exit'Access);
      Register_Function (state, "os.remove", override_remove'Access);
      Register_Function (state, "os.rename", override_rename'Access);
      Register_Function (state, "io.open", override_open'Access);

      --  The arguments are concatenated with null characters.
      insert_arguments (state, arg_chain);

      begin
         TIO.Set_Output (out_handle);
         status := Protected_Call (state, 0, MULTRET);
         TIO.Set_Output (TIO.Standard_Output);
      exception
         when others =>
            TIO.Put_Line (TIO.Standard_Error, "Failed to redirect Lua call to output file");
            status := LUA_ERRFILE;
      end;
      case status is
         when LUA_OK =>
            declare
               res_count : constant Integer := Integer (API_lua_gettop (state) - top);
            begin
               case res_count is
                  when 0      => success := True;
                  when others =>
                     declare
                        rc : constant String := convert_to_string (state, top_slot);
                     begin
                        if rc = "0" then
                           success := True;
                        else
                           success := False;
                        end if;
                     end;
               end case;
            end;
         when others =>
            success := False;
            TIO.Put_Line (TIO.Standard_Error, "Failed to execute Lua script:" & status'Img);
            TIO.Put_Line (TIO.Standard_Error, convert_to_string (state, top_slot));
      end case;

      Close (state);

   end run_lua_script;


   ------------------------------
   --  show_post_run_messages  --
   ------------------------------
   procedure show_post_run_messages
     (msg_outfile : String;
      namebase    : String;
      subpackage  : String;
      variant     : String;
      extract_log : TIO.File_Type)
   is
      redirected : constant Boolean := TIO.Is_Open (extract_log);
      std_outfile : constant String := MSC.new_filename (msg_outfile, MSC.ft_lua);
      features1 : Archive.Unix.File_Characteristics;
      features2 : Archive.Unix.File_Characteristics;
      msg_file_exists : Boolean := False;
      std_file_exists : Boolean := False;
      msg_big_enough  : Boolean := False;
      std_big_enough  : Boolean := False;

      procedure display_and_delete_file (filename : String; big_enough : Boolean)
      is
         handle : TIO.File_Type;
      begin
         if big_enough then
            TIO.Open (File => handle,
                      Mode => TIO.In_File,
                      Name => filename);
            while not TIO.End_Of_File (handle) loop
               if redirected then
                  TIO.Put_Line (extract_log, TIO.Get_Line (handle));
               else
                  TIO.Put_Line (TIO.Get_Line (handle));
               end if;
            end loop;
            TIO.Close (handle);
         end if;
         DIR.Delete_File (filename);
      exception
         when others => null;
      end display_and_delete_file;
   begin
      features1 := Archive.Unix.get_charactistics (msg_outfile);
      features2 := Archive.Unix.get_charactistics (std_outfile);

      case features1.ftype is
         when Archive.regular =>
            msg_file_exists := True;
         when others => null;
      end case;
      msg_big_enough := Archive.">" (features1.size, Archive.exabytes (1));

      case features2.ftype is
         when Archive.regular =>
            std_file_exists := True;
         when others => null;
      end case;
      std_big_enough := Archive.">" (features2.size, Archive.exabytes (1));

      if redirected and then (msg_big_enough or else std_big_enough) then
         declare
            divlength : constant Natural := 75;
            partone : constant String := namebase & '-' & subpackage & '-' & variant &
              " Lua script messages  ";
            divider : String (1 .. divlength) := (others => '-');
         begin
            if partone'Length > divlength then
               divider := partone (partone'First .. partone'First + divlength - 1);
            else
               divider (divider'First .. divider'First + partone'Length - 1) := partone;
            end if;
            TIO.Put_Line (extract_log, divider);
         end;
      end if;

      if std_file_exists then
         display_and_delete_file (std_outfile, std_big_enough);
      end if;

      if msg_file_exists then
         display_and_delete_file (msg_outfile, msg_big_enough);
      end if;
   end show_post_run_messages;


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
            stdoutfile : constant String := tmp & ".rvn_stdout." & extension;
         begin
            if not DIR.Exists (candidate) and then
              not DIR.Exists (stdoutfile)
            then
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


   ---------------
   --  Push #3  --
   ---------------
   procedure Push (State : Lua_State; Data : Integer)
   is
   begin
      API_lua_pushinteger (State, Lua_Integer (Data));
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
      return convert_to_string (State, top_slot);
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
      fail_msg : IC.char_array)
   is
      result : Integer;
      pragma Unreferenced (result);
   begin
      if not valid then
         result := API_luaL_argerror (State, index, fail_msg'Address);
      end if;
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
      validate_argument (State, valid, narg, custerr_print_msg);

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
                    Override => True);

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


   ----------------
   --  is_table  --
   ----------------
   function is_table
     (State    : Lua_State;
      Index    : Lua_Index) return Boolean
   is
      this_type : constant Lua_Type := API_lua_type (State, Index);
   begin
      case this_type is
         when LUA_TTABLE => return True;
         when others => return False;
      end case;
   end is_table;


   -----------------
   --  is_string  --
   -----------------
   function is_string
     (State    : Lua_State;
      Index    : Lua_Index) return Boolean
   is
      this_type : constant Lua_Type := API_lua_type (State, Index);
   begin
      case this_type is
         when LUA_TSTRING => return True;
         when others => return False;
      end case;
   end is_string;


   -----------------------
   --  Check_User_Data  --
   -----------------------
   function Check_User_Data
     (State : Lua_State;
      Index : Lua_Index;
      Name  : String) return Lua_User_Data
   is
      Str_Ptr : IC.Strings.chars_ptr := IC.Strings.New_String (Name);
      Result  : constant Lua_User_Data := API_luaL_checkudata (State, Index, Str_Ptr);
   begin
      IC.Strings.Free (Str_Ptr);
      return Result;
   end Check_User_Data;


   ---------------------
   --  Set_Metatable  --
   ---------------------
   procedure Set_Metatable
     (State : Lua_State;
      Name  : String)
   is
      Str_Ptr : IC.Strings.chars_ptr := IC.Strings.New_String (Name);
   begin
      API_luaL_setmetatable (State, Str_Ptr);
      IC.Strings.Free (Str_Ptr);
   end Set_Metatable;


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
               ASU.Append (stack, "Other:   " & convert_to_type_name (State, index) & LF);
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


   --------------------
   --  dynamic_path  --
   --------------------
   function dynamic_path (State : Lua_State; given_path : String) return String
   is
      --  This function was necessary for FreeBSD Pkg because it uses the "AT" functions
      --  relative to rootfd file description, and the path couldn't start with a slash.
      --  We are basically prepending the root to everything, so consecutive slashes are
      --  only ugly but wouldn't have caused a problem.

      function strip_slashes (raw : String) return String
      is
         --  basically this strips leading forward slashes, then prepends with "$rootdir/"
         marker : Natural := 0;
         slash_seen : Boolean := False;
      begin
         if raw'Length = 0 then
            return "";
         end if;
         for index in raw'Range loop
            case raw (index) is
               when '/' =>
                  slash_seen := True;  -- keep going
               when others =>
                  marker := index;
                  exit;
            end case;
         end loop;
         if slash_seen then
            if marker = 0 then --- edge case: string only contained slashes!
               return "";
            end if;
            return raw (marker .. raw'Last);
         end if;
         return raw;
      end strip_slashes;
   begin
      return Get_Global_String (State, "pkg_rootdir") & '/' & strip_slashes (given_path);
   end dynamic_path;


   ------------------------
   --  insert_arguments  --
   ------------------------
   procedure insert_arguments (state : Lua_State; argument_chain : String)
   is
      nfields : Natural := 0;
      marker  : Natural;
      delim   : constant Character := Character'Val (0);
      line    : String := argument_chain;
   begin
      if argument_chain'Length = 0 then
         API_lua_createtable (state, 0, 1);
         Set_Global (state, "arg");
         Set_Global_String (state, "line", "");
         return;
      end if;

      for x in line'Range loop
         if line (x) = delim then
            line (x) := ' ';
         end if;
      end loop;

      declare
         ac2 : String (1 .. argument_chain'Length + 1) := (others => delim);
      begin
         ac2 (ac2'First .. ac2'Last - 1) := argument_chain;

         for x in ac2'Range loop
            if ac2 (x) = delim then
               nfields := nfields + 1;
            end if;
         end loop;

         API_lua_createtable (state, nfields, 1);
         nfields := 0;
         marker := ac2'First;
         for x in ac2'Range loop
            if ac2 (x) = delim then
               if marker = x then
                  --  rare case of zero-length argument
                  Push (state, "");
               else
                  Push (state, ac2 (marker .. x - 1));
               end if;
               nfields := nfields + 1;
               Raw_Seti (state, -2, nfields);
               marker := x + 1;
            end if;
         end loop;

      end;
      Set_Global (state, "arg");
      Set_Global_String (state, "line", line);

   end insert_arguments;


   --------------------------
   --  custom_prefix_path  --
   --------------------------
   function custom_prefix_path (State : Lua_State) return Integer
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
      validate_argument (State, valid, narg, custerr_prefix_path);
      declare
         inpath : constant String := retrieve_argument (State, 1);
      begin
         if inpath (inpath'First) = '/' then
            Push (State, inpath);
         else
            if prefix = "" then
               Push (State, '/' & inpath);
            elsif prefix (prefix'Last) = '/' then
               Push (State, prefix & inpath);
            else
               Push (State, prefix & '/' & inpath);
            end if;
         end if;
      end;
      return 1;
   end custom_prefix_path;


   ----------------------
   --  custom_filecmp  --
   ----------------------
   function custom_filecmp (State : Lua_State) return Integer
   is
      n       : constant Lua_Index := API_lua_gettop (State);
      valid   : Boolean;
      narg    : Positive := n;
   begin
      valid := n = 2;
      if n > 2 then
         narg := 3;
      elsif n = 1 then
         narg := 2;
      end if;
      --  validate_argument will not return on failure
      validate_argument (State, valid, narg, custerr_filecmp);

      declare
         package UNX renames Archive.Unix;
         package B3 renames Blake_3;

         file_1 : constant String := dynamic_path (State, retrieve_argument (State, 1));
         file_2 : constant String := dynamic_path (State, retrieve_argument (State, 2));
         files_identical : constant Integer := 0;
         files_differ    : constant Integer := 1;
         file_not_found  : constant Integer := 2;
         f1_attributes : UNX.File_Characteristics;
         f2_attributes : UNX.File_Characteristics;

         use type Archive.file_type;
         use type Archive.exabytes;
      begin
         f1_attributes := UNX.get_charactistics (file_1);
         f2_attributes := UNX.get_charactistics (file_2);

         --  both files exist?
         if f1_attributes.ftype = Archive.unsupported or else
           f2_attributes.ftype = Archive.unsupported
         then
            Push (State, file_not_found);
            return 1;
         end if;

         --  Do file sizes match?
         if f1_attributes.size /= f2_attributes.size then
            Push (State, files_differ);
            return 1;
         end if;

         --  File sizes are the same, so compare the digests
         if B3.hex (B3.file_digest (file_1)) /= B3.hex (B3.file_digest (file_2)) then
            Push (State, files_differ);
            return 1;
         end if;

         --  Files are the same
         Push (State, files_identical);
         return 1;
      end;
   end custom_filecmp;


   ----------------------
   --  custom_symlink  --
   ----------------------
   function custom_symlink (State : Lua_State) return Integer
   is
      n       : constant Lua_Index := API_lua_gettop (State);
      valid   : Boolean;
      narg    : Positive := n;
   begin
      valid := n = 2;
      if n > 2 then
         narg := 3;
      elsif n = 1 then
         narg := 2;
      end if;
      --  validate_argument will not return on failure
      validate_argument (State, valid, narg, custerr_symlink);

      declare
         package UNX renames Archive.Unix;

         source : constant String := dynamic_path (State, retrieve_argument (State, 1));
         destin : constant String := retrieve_argument (State, 2);
      begin
         if not UNX.create_symlink (source, destin) then
            declare
               fname : IC.char_array := UNX.convert_to_char_array (source);
            begin
               return API_luaL_fileresult (State, 0, fname'Address);
            end;
         end if;
         return 1;
      end;
   end custom_symlink;


   -----------------------
   --  custom_filecopy  --
   -----------------------
   function custom_filecopy (State : Lua_State) return Integer
   is
      n       : constant Lua_Index := API_lua_gettop (State);
      valid   : Boolean;
      narg    : Positive := n;
   begin
      valid := n = 2;
      if n > 2 then
         narg := 3;
      elsif n = 1 then
         narg := 2;
      end if;
      --  validate_argument will not return on failure
      validate_argument (State, valid, narg, custerr_filecopy);

      declare
         package UNX renames Archive.Unix;

         source : constant String := dynamic_path (State, retrieve_argument (State, 1));
         destin : constant String := dynamic_path (State, retrieve_argument (State, 2));
         src_attributes : UNX.File_Characteristics;

         copy_success    : constant Integer := 0;
         copy_failure    : constant Integer := 1;
         file_not_found  : constant Integer := 2;
         wrong_file_type : constant Integer := 3;
         missing_parent  : constant Integer := 4;
      begin
         src_attributes := UNX.get_charactistics (source);
         case src_attributes.ftype is
            when Archive.unsupported =>
               Push (State, file_not_found);
               return 1;
            when Archive.directory | Archive.fifo =>
               Push (State, wrong_file_type);
               return 1;
            when Archive.regular | Archive.hardlink | Archive.symlink =>
               null;
         end case;
         declare
            function head (S : String) return String
            is
               delimiter    : constant String  := "/";
               dl_size      : constant Natural := delimiter'Length;
               back_marker  : constant Natural := S'First;
               front_marker : Natural := S'Last - dl_size + 1;
            begin
               loop
                  if front_marker < back_marker then
                     --  delimiter never found
                     return "";
                  end if;
                  if S (front_marker .. front_marker + dl_size - 1) = delimiter then
                     return S (back_marker .. front_marker - 1);
                  end if;
                  front_marker := front_marker - 1;
               end loop;
            end head;

            destino_padre : constant String := head (destin);
            dest_attributes : UNX.File_Characteristics;
         begin
            if destino_padre /= "" then
               dest_attributes := UNX.get_charactistics (destino_padre);
               case dest_attributes.ftype is
                  when Archive.directory => null;
                  when others =>
                     Push (State, missing_parent);
                     return 1;
               end case;
            end if;
         end;
         --  passed validation checks, attempt to copy
         declare
            Args : GNAT.OS_Lib.Argument_List :=
              (1 => new String'("/bin/cp"),
               2 => new String'("-RpP"),
               3 => new String'(source),
               4 => new String'(destin)
              );
            succeeded : Boolean;
         begin
            GNAT.OS_Lib.Spawn
              (Program_Name => Args (Args'First).all,
               Args         => Args (Args'First + 1 .. Args'Last),
               Success      => succeeded);

            --  Free memory
            for Index in Args'Range loop
               GNAT.OS_Lib.Free (Args (Index));
            end loop;

            if succeeded then
               Push (State, copy_success);
               return 0;
            else
               Push (State, copy_failure);
               return 1;
            end if;
         end;
      end;
   end custom_filecopy;


   -------------------
   --  custum_exec  --
   -------------------
   function custom_exec (State : Lua_State) return Integer
   is
      n       : constant Lua_Index := API_lua_gettop (State);
      valid   : Boolean;
      narg    : Positive := n;
      tablen  : Natural;
   begin
      valid := n = 1;
      if n > 1 then
         narg := 2;
      end if;
      --  validate_argument will not return on failure
      validate_argument (State, valid, narg, custerr_exec);

      if not is_table (State, top_slot) then
         TIO.Put_Line ("error: exec argument was not a table");
         return 1;
      end if;

      tablen := Natural (API_lua_rawlen (State, 1));
      declare
         Args : GNAT.OS_Lib.Argument_List (1 .. tablen);
         succeeded : Boolean;
         exec_success : constant Integer := 0;
         exec_failure : constant Integer := 1;
      begin
         for index in 1 .. tablen loop
            Raw_Geti (State, 1, index);
            if not is_string (State, top_slot) then
               validate_argument (State, False, index, custerr_exec_payload);
            end if;
            Args (index) := new String'(convert_to_string (State, top_slot));
            Pop (State, 1);
         end loop;

         GNAT.OS_Lib.Spawn
           (Program_Name => Args (Args'First).all,
            Args         => Args (Args'First + 1 .. Args'Last),
            Success      => succeeded);

         for Index in Args'Range loop
            GNAT.OS_Lib.Free (Args (Index));
         end loop;

         if succeeded then
            Push (State, exec_success);
            return 0;
         else
            Push (State, exec_failure);
            return 1;
         end if;
      end;
   end custom_exec;


   -------------------
   --  custom_stat  --
   -------------------
   function custom_stat (State : Lua_State) return Integer
   is
      n       : constant Lua_Index := API_lua_gettop (State);
      valid   : Boolean;
      narg    : Positive := n;
   begin
      valid := n = 1;
      if n > 1 then
         narg := 2;
      end if;
      --  validate_argument will not return on failure
      validate_argument (State, valid, narg, custerr_stat);

      declare
         package UNX renames Archive.Unix;

         inpath : constant String := dynamic_path (State, retrieve_argument (State, 1));
         attributes : UNX.File_Characteristics;
      begin
         attributes := UNX.get_charactistics (inpath);
         case attributes.ftype is
            when Archive.unsupported =>
               Push (State);
               return 1;
            when others => null;
         end case;

         API_lua_settop (State, 2);
         if not is_table (State, 2) then
            API_lua_createtable (State);
         end if;

         Push (State, Integer (attributes.size));
         Set_Field (State, -2, "size");

         Push (State, Integer (attributes.uid));
         Set_Field (State, -2, "uid");

         Push (State, Integer (attributes.gid));
         Set_Field (State, -2, "gid");

         case attributes.ftype is
            when Archive.unsupported => null;
            when Archive.directory   => Push (State, "dir");
            when Archive.regular     => Push (State, "reg");
            when Archive.symlink     => Push (State, "lnk");
            when Archive.fifo        => Push (State, "fifo");
            when Archive.hardlink    => Push (State, "reg");
         end case;
         Set_Field (State, -2, "type");

         return 1;
      end;
   end custom_stat;


   ----------------------
   --  custom_readdir  --
   ----------------------
   function custom_readdir (State : Lua_State) return Integer
   is
      n       : constant Lua_Index := API_lua_gettop (State);
      valid   : Boolean;
      narg    : Positive := n;
   begin
      valid := n = 1;
      if n > 1 then
         narg := 2;
      end if;
      --  validate_argument will not return on failure
      validate_argument (State, valid, narg, custerr_readdir);

      declare
         package UNX renames Archive.Unix;
         package SCN renames Archive.Dirent.Scan;

         inpath  : constant String := retrieve_argument (State, 1);
         dynpath : constant String := dynamic_path (State, inpath);
         fname   : IC.char_array := UNX.convert_to_char_array (inpath);
         attributes : UNX.File_Characteristics;
      begin
         attributes := UNX.get_charactistics (dynpath);
         case attributes.ftype is
            when Archive.directory => null;
            when others =>
               return API_luaL_fileresult (State, 0, fname'Address);
         end case;
         declare
            procedure walkdir (position : SCN.dscan_crate.Cursor);

            crate : SCN.dscan_crate.Vector;
            index : Integer := 0;

            procedure walkdir (position : SCN.dscan_crate.Cursor)
            is
               item  : Archive.Dirent.Directory_Entity renames SCN.dscan_crate.Element (position);
               filename  : constant String := item.simple_name;
            begin
               index := index + 1;
               Push (State, index);
               Push (State, filename);
               API_lua_settable (State, -3);
            end walkdir;
         begin
            SCN.scan_directory (dynpath, crate);
            API_lua_createtable (State);
            crate.Iterate (walkdir'Access);
         exception
            when others =>
               return API_luaL_fileresult (State, 0, fname'Address);
         end;
         return 1;
      end;
   end custom_readdir;


   ---------------------------
   --  override_os_execute  --
   ---------------------------
   function override_os_execute (State : Lua_State) return Integer is
   begin
      return API_luaL_error (State, custerr_os_exec'Address);
   end override_os_execute;


   ------------------------
   --  override_os_exit  --
   ------------------------
   function override_os_exit (State : Lua_State) return Integer is
   begin
      return API_luaL_error (State, custerr_os_exit'Address);
   end override_os_exit;


   -----------------------
   --  override_remove  --
   -----------------------
   function override_remove (State : Lua_State) return Integer
   is
      n       : constant Lua_Index := API_lua_gettop (State);
      valid   : Boolean;
      narg    : Positive := n;
   begin
      valid := n = 1;
      if n > 1 then
         narg := 2;
      end if;
      --  validate_argument will not return on failure
      validate_argument (State, valid, narg, custerr_os_remove);
      declare
         package UNX renames Archive.Unix;

         target_path : constant String := dynamic_path (State, retrieve_argument (State, 1));
         attributes : UNX.File_Characteristics;
      begin
         attributes := UNX.get_charactistics (target_path);
         case attributes.ftype is
            when Archive.unsupported =>
               return API_luaL_fileresult (State, 0, System.Null_Address);
            when Archive.directory =>
               DIR.Delete_Directory (target_path);
               return API_luaL_fileresult (State, 1, System.Null_Address);
            when Archive.regular | Archive.hardlink | Archive.symlink | Archive.fifo =>
               DIR.Delete_File (target_path);
               return API_luaL_fileresult (State, 1, System.Null_Address);
         end case;
      exception
         when others =>
            return API_luaL_fileresult (State, 0, System.Null_Address);
      end;
   end override_remove;


   -----------------------
   --  override_rename  --
   -----------------------
   function override_rename (State : Lua_State) return Integer
   is
      n       : constant Lua_Index := API_lua_gettop (State);
      valid   : Boolean;
      narg    : Positive := n;
   begin
      valid := n = 2;
      if n > 2 then
         narg := 3;
      elsif n = 1 then
         narg := 2;
      end if;
      --  validate_argument will not return on failure
      validate_argument (State, valid, narg, custerr_os_rename);

      declare
         old_name : constant String := dynamic_path (State, retrieve_argument (State, 1));
         new_name : constant String := dynamic_path (State, retrieve_argument (State, 2));
      begin
         DIR.Rename (old_name, new_name);
         return API_luaL_fileresult (State, 1, System.Null_Address);
      exception
         when others =>
            return API_luaL_fileresult (State, 0, System.Null_Address);
      end;
   end override_rename;


   ---------------------
   --  override_open  --
   ---------------------
   function override_open (State : Lua_State) return Integer
   is
      n       : constant Lua_Index := API_lua_gettop (State);
      valid   : Boolean;
      narg    : Positive := n;
   begin
      valid := n = 2;
      if n > 2 then
         narg := 3;
      elsif n = 1 then
         narg := 2;
      end if;
      --  validate_argument will not return on failure
      validate_argument (State, valid, narg, custerr_os_rename);

      declare
         package UNX renames Archive.Unix;

         filename  : constant String := dynamic_path (State, retrieve_argument (State, 1));
         mode      : constant String := retrieve_argument (State, 2);
         bad_mode  : constant IC.char_array := UNX.convert_to_char_array ("invalid mode");
         cfilename : constant IC.char_array := UNX.convert_to_char_array (filename);
         flags     : UNX.T_Open_Flags;
         fd        : UNX.File_Descriptor;
      begin
         --  mode options are: r  (read), w (write), a (append), b (binary which is ignored),
         --                    r+, w+, a+
         if mode = "r" or else mode = "rb" then
            flags.RDONLY := True;
         elsif mode = "r+" or else mode = "rb+" then
            flags.RDONLY := True;
            flags.WRONLY := True;
         elsif mode = "w" or else mode = "wb" then
            flags.WRONLY := True;
            flags.CREAT  := True;
            flags.TRUNC  := True;
         elsif mode = "w+" or else mode = "wb+" then
            flags.WRONLY := True;
            flags.RDONLY := True;
            flags.CREAT  := True;
            flags.TRUNC  := True;
         elsif mode = "a" or else mode = "ab" then
            flags.WRONLY := True;
            flags.CREAT  := True;
            flags.APPEND := True;
         elsif mode = "a+" or else mode = "ab+" then
            flags.WRONLY := True;
            flags.RDONLY := True;
            flags.CREAT  := True;
            flags.APPEND := True;
         else
            validate_argument (State, False, 2, bad_mode);
         end if;

         fd := UNX.open_file (filename, flags);
         if not UNX.file_connected (fd) then
            return API_luaL_fileresult (State, 0, cfilename'Address);
         end if;

         declare
            use type CS.FILEs;

            cmode : IC.Strings.chars_ptr := IC.Strings.New_String (mode);
            lsize : constant IC.size_t := IC.size_t (luaL_Stream'Size);
            userdata : constant Lua_User_Data := API_lua_newuserdatauv (State, lsize, 1);
            LStream  : luaL_Stream;
            for LStream'Address use System.Address (userdata);
            pragma Import (Ada, LStream);
         begin
            LStream.f := c_fdopen (fd, cmode);
            LStream.closef := override_close'Access;
            IC.Strings.Free (cmode);
            Set_Metatable (State,  "FILE*" & ASCII.NUL);
            if LStream.f = CS.NULL_Stream then
               return API_luaL_fileresult (State, 0, cfilename'Address);
            else
               return 1;
            end if;
         end;
      end;
   end override_open;


   ----------------------
   --  override_close  --
   ----------------------
   function override_close (State : Lua_State) return Integer
   is
      res : Integer;
      filehandle : IC.Strings.chars_ptr := IC.Strings.New_String ("FILE*" & ASCII.NUL);
      userdata : constant Lua_User_Data := API_luaL_checkudata (State, 1, filehandle);

      LStream  : luaL_Stream;
      for LStream'Address use System.Address (userdata);
      pragma Import (Ada, LStream);
   begin
      IC.Strings.Free (filehandle);
      res := c_fclose (LStream.f);
      if res = 0 then
         return API_luaL_fileresult (State, 1, System.Null_Address);
      end if;
      return API_luaL_fileresult (State, 0, System.Null_Address);
   end override_close;

end Lua;
