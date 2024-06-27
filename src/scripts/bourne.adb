--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Ada.Environment_Variables;
with Ada.Directories;
with Ada.Real_Time;
with Ada.Direct_IO;
with GNAT.OS_Lib;
with Archive.Misc;
with Archive.Unix;

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
            stdoutfile : constant String := tmp & ".rvn_stdout." & extension;
         begin
            if not DIR.Exists (candidate) and then
              not DIR.Exists (scriptfile) and then
              not DIR.Exists (stdoutfile)
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
      out_handle  : Ada.Text_IO.File_Type;
      success     : out Boolean)
   is
      num_args : Natural;
      return_code : Integer;
      run_success : Boolean;
      script_file : constant String := MSC.new_filename (msg_outfile, MSC.ft_script);
      tmp_outfile : constant String := MSC.new_filename (msg_outfile, MSC.ft_internal);
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
               Output_File  => tmp_outfile,
               Success      => run_success,
               Return_Code  => return_code,
               Err_To_Out   => True);

               --  Free memory
            for Index in Args'Range loop
               GNAT.OS_Lib.Free (Args (Index));
            end loop;
         end;

         DIR.Delete_File (script_file);
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
               Output_File  => tmp_outfile,
               Success      => run_success,
               Return_Code  => return_code,
               Err_To_Out   => True);

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

      declare
         tmp_handle : Ada.Text_IO.File_Type;
         use type DIR.File_Size;
      begin
         if DIR.Size (tmp_outfile) > 1 then
            Ada.Text_IO.Open (tmp_handle, Ada.Text_IO.In_File, tmp_outfile);
            while not Ada.Text_IO.End_Of_File (tmp_handle) loop
               Ada.Text_IO.Put_Line (out_handle, Ada.Text_IO.Get_Line (tmp_handle));
            end loop;
            Ada.Text_IO.Close (tmp_handle);
         end if;
         DIR.Delete_File (tmp_outfile);
      exception
         when others =>
            if Ada.Text_IO.Is_Open (tmp_handle) then
               Ada.Text_IO.Close (tmp_handle);
            end if;
      end;

      case return_code is
         when 0 => success := True;
         when others => success := False;
      end case;

   end run_shell_script;


   ------------------------------
   --  show_post_run_messages  --
   ------------------------------
   procedure show_post_run_messages
     (msg_outfile : String;
      namebase    : String;
      subpackage  : String;
      variant     : String;
      extract_log : Ada.Text_IO.File_Type)
   is
      redirected : constant Boolean := TIO.Is_Open (extract_log);
      std_outfile : constant String := MSC.new_filename (msg_outfile, MSC.ft_stdout);
      features1 : Archive.Unix.File_Characteristics;
      features2 : Archive.Unix.File_Characteristics;
      msg_file_exists : Boolean := False;
      std_file_exists : Boolean := False;

      procedure display_and_delete_file (filename : String)
      is
         handle : TIO.File_Type;
      begin
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

      if Archive.">" (features2.size, Archive.exabytes (1)) then
         std_file_exists := True;
      end if;

      if not msg_file_exists and then not std_file_exists then
         return;
      end if;

      if redirected then
         declare
            divlength : constant Natural := 75;
            partone : constant String := namebase & '-' & subpackage & '-' & variant &
              " shell script messages  ";
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
         display_and_delete_file (std_outfile);
      end if;

      if msg_file_exists then
         display_and_delete_file (msg_outfile);
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


   -------------------
   --  append_file  --
   -------------------
   procedure append_file (file_to_write : String; file_to_read : String)
   is
      main_file : TIO.File_Type;
      temp_file : TIO.File_Type;
   begin
      TIO.Open (main_file, TIO.Append_File, file_to_write);
      TIO.Open (temp_file, TIO.In_File, file_to_read);
      while not TIO.End_Of_File (temp_file) loop
         TIO.Put_Line (main_file, TIO.Get_Line (temp_file));
      end loop;
      TIO.Close (temp_file);
      TIO.Close (main_file);
   exception
      when others =>
         if TIO.Is_Open (temp_file) then
            TIO.Close (temp_file);
         end if;
         if TIO.Is_Open (main_file) then
            TIO.Close (main_file);
         end if;
   end append_file;

end Bourne;
