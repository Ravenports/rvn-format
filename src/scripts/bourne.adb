--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Ada.Environment_Variables;
with Ada.Strings.Fixed;
with Ada.Directories;
with Ada.Real_Time;
with Ada.Direct_IO;
with Ada.Text_IO;
with GNAT.OS_Lib;
with Archive.Misc;

package body Bourne is

   package RT  renames Ada.Real_Time;
   package DIR renames Ada.Directories;
   package ENV renames Ada.Environment_Variables;
   package TIO renames Ada.Text_IO;
   package MSC renames Archive.Misc;


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
            scriptfile : constant String := tmp & ".rvn_script." & extension;
         begin
            if not DIR.Exists (candidate) and then
              not DIR.Exists (scriptfile)
            then
               return candidate;
            end if;
         end;
      end loop;
   end unique_msgfile_path;


   ------------------------
   --  run_shell_script  --
   ------------------------
   procedure run_shell_script
     (namebase    : String;
      subpackage  : String;
      variant     : String;
      prefix      : String;
      root_dir    : String;
      upgrading   : Boolean;
      interpreter : String;
      script      : String;
      arguments   : String;
      msg_outfile : String;
      success     : out Boolean)
   is
      num_args : Natural;
   begin
      if not DIR.Exists (interpreter) then
         raise interpreter_missing;
      end if;
      ENV.Set ("PKG_NAMEBASE", namebase);
      ENV.Set ("PKG_SUBPACKAGE", subpackage);
      ENV.Set ("PKG_VARIANT", variant);
      ENV.Set ("PKG_PREFIX", prefix);
      ENV.Set ("PKG_ROOTDIR", root_dir);
      ENV.Set ("PKG_OUTFILE", msg_outfile);
      if upgrading then
         ENV.Set ("PKG_UPGRADE", "TRUE");
      else
         ENV.Clear ("PKG_UPGRADE");
      end if;
      if arguments = "" then
         num_args := 0;
      else
         num_args := MSC.count_char (arguments, ' ') + 1;
      end if;

      if script'Length > 4000 then
         if script'Length > 262_144 then
            raise ginormous_script with "> 256KB";
         end if;
         declare
            script_file : String := msg_outfile;
            start_index : constant Natural := Ada.Strings.Fixed.Index (msg_outfile, "outmsg");
         begin
            script_file (start_index .. start_index + 5) := "script";
            dump_contents_to_file (script, script_file);

            declare
               last_arg : constant Natural := 2 + num_args;
               Args : GNAT.OS_Lib.Argument_List (1 .. last_arg);
            begin
               Args (1) := new String'(interpreter);
               Args (2) := new String'(script_file);
               for x in 1 .. num_args loop
                  declare
                     new_arg : constant String := MSC.specific_field (arguments, x);
                  begin
                     Args (2 + x) := new String'(new_arg);
                  end;
               end loop;

               GNAT.OS_Lib.Spawn
                 (Program_Name => Args (Args'First).all,
                  Args         => Args (Args'First + 1 .. Args'Last),
                  Success      => success);

               --  Free memory
               for Index in Args'Range loop
                  GNAT.OS_Lib.Free (Args (Index));
               end loop;
            end;

            DIR.Delete_File (script_file);
         end;
      else
         declare
            last_arg : constant Natural := 4 + num_args;
            Args : GNAT.OS_Lib.Argument_List (1 .. last_arg);
         begin
            Args (1) := new String'(interpreter);
            Args (2) := new String'("-c");
            Args (3) := new String'(script);
            Args (4) := new String'("inline");
            for x in 1 .. num_args loop
               declare
                  new_arg : constant String := MSC.specific_field (arguments, x);
               begin
                  Args (4 + x) := new String'(new_arg);
               end;
            end loop;

            GNAT.OS_Lib.Spawn
              (Program_Name => Args (Args'First).all,
               Args         => Args (Args'First + 1 .. Args'Last),
               Success      => success);

            --  Free memory
            for Index in Args'Range loop
               GNAT.OS_Lib.Free (Args (Index));
            end loop;
         end;
      end if;

      ENV.Clear ("PKG_NAMEBASE");
      ENV.Clear ("PKG_SUBPACKAGE");
      ENV.Clear ("PKG_VARIANT");
      ENV.Clear ("PKG_PREFIX");
      ENV.Clear ("PKG_ROOTDIR");
      ENV.Clear ("PKG_OUTFILE");
      ENV.Clear ("PKG_UPGRADE");
   end run_shell_script;


   ------------------------------
   --  show_post_run_messages  --
   ------------------------------
   procedure show_post_run_messages (msg_outfile : String) is
   begin
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
   end show_post_run_messages;


   -----------------------------
   --  dump_contents_to_file  --
   -----------------------------
   procedure dump_contents_to_file (contents : String; dossier  : String)
   is
      File_Size : constant Natural := contents'Length;

      subtype File_String    is String (1 .. File_Size);
      package File_String_IO is new Ada.Direct_IO (File_String);

      file_handle : File_String_IO.File_Type;
   begin
      File_String_IO.Create (File => file_handle,
                             Mode => File_String_IO.Out_File,
                             Name => dossier);
      File_String_IO.Write  (File => file_handle,
                             Item => contents);
      File_String_IO.Close  (file_handle);
   exception
      when Storage_Error =>
         if File_String_IO.Is_Open (file_handle) then
            File_String_IO.Close (file_handle);
         end if;
         raise file_dump with "failed to allocate memory";
      when others =>
         if File_String_IO.Is_Open (file_handle) then
            File_String_IO.Close (file_handle);
         end if;
         raise file_dump with "unknown error";
   end dump_contents_to_file;


end Bourne;
