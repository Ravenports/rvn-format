--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt


with Ada.Text_IO;
with Ada.Real_Time;
with Ada.Exceptions;
with Ada.Directories;


package body Lua is

   package TIO renames Ada.Text_IO;
   package RT  renames Ada.Real_Time;
   package EX  renames Ada.Exceptions;
   package DIR renames Ada.Directories;


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

      status := Protected_Call (state);
      case status is
         when LUA_OK =>
            null;
         when others =>
            TIO.Put_Line (TIO.Standard_Error, "Failed to execute Lua script:" & status'Img);
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


   -------------------------
   --  convert_to_string  --
   -------------------------
   function convert_to_string (State : Lua_State; Index : Lua_Index) return String
   is
      Length : IC.size_t;
   begin
      return IC.Strings.Value (API_lua_tolstring (State, Index, Length));
   end convert_to_string;


   -------------------------
   --  Set_Global_String  --
   -------------------------
   procedure Set_Global_String
     (State : Lua_State;
      Name  : String;
      value : String)
   is
      Result   : IC.Strings.chars_ptr;
      Name_Ptr : constant IC.Strings.chars_ptr := IC.Strings.New_String (Name);

      pragma Unreferenced (Result);
   begin
      Result := API_lua_pushlstring (State, value'Address, value'Length);
      API_lua_setglobal (State, Name_Ptr);
   end Set_Global_String;


end Lua;
