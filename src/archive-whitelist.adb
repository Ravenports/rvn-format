--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt


with Ada.Strings.Hash;
with Ada.Strings.Fixed;
with Ada.Text_IO;
with Archive.Unix;
with Archive.Whitelist.Keywords;
with GNAT.Regpat;

package body Archive.Whitelist is

   package TIO renames Ada.Text_IO;
   package ASF renames Ada.Strings.Fixed;
   package REX renames GNAT.Regpat;
   package KWD renames Archive.Whitelist.Keywords;

   -----------------------
   --  whiteist_in_use  --
   -----------------------
   function whitelist_in_use (whitelist : A_Whitelist) return Boolean is
   begin
      return whitelist.list_used;
   end whitelist_in_use;


   -------------------------
   --  file_on_whitelist  --
   -------------------------
   function file_on_whitelist
     (whitelist     : A_Whitelist;
      file_path     : String) return Boolean
   is
      file_hash : Blake_3.blake3_hash;
   begin
      file_hash := Blake_3.digest (file_path);

      if whitelist.files.Contains (file_hash) then
         --  True on files, False on directories
         return not whitelist.files.Element (file_hash).is_directory;
      end if;
      return False;
   end file_on_whitelist;


   ------------------------------
   --  directory_on_whitelist  --
   ------------------------------
   function directory_on_whitelist
     (whitelist     : A_Whitelist;
      file_path     : String) return Boolean
   is
      file_hash : Blake_3.blake3_hash;
   begin
      file_hash := Blake_3.digest (file_path);

      if whitelist.files.Contains (file_hash) then
         --  False on files, True on directories
         return whitelist.files.Element (file_hash).is_directory;
      end if;
      return False;
   end directory_on_whitelist;


   ----------------------------
   --  ingest_file_manifest  --
   ----------------------------
   function ingest_file_manifest
     (whitelist          : out A_Whitelist;
      manifest_file      : String;
      stage_directory    : String;
      prefix_directory   : String;
      keywords_directory : String;
      level              : info_level) return Boolean
   is
      type linecat is (jump, external_keyword, file_path, mode_override, key_error);

      function get_true_path (line : String) return String;
      function categorize_line (line : String) return linecat;
      procedure handle_file_path (line : String);
      procedure handle_mode_keyword (line : String);
      procedure invoke_keyword_callback (line : String);

      features    : Unix.File_Characteristics;
      file_handle : TIO.File_Type;
      succeeded   : Boolean := True;
      regmech     : constant REX.Pattern_Matcher := REX.Compile ("^@[(].*,.*,.*[)] ");
      captured    : constant REX.Pattern_Matcher := REX.Compile ("^@[(](.*),(.*),(.*)[)] (.*)");
      keymech     : constant REX.Pattern_Matcher := REX.Compile ("^@([^(]\S*) (.*)");
      dirmech     : constant REX.Pattern_Matcher := REX.Compile ("^dir[(](.*),(.*),(.*)[)]");
      key_jar     : REX.Match_Array (0 .. 2);
      match_jar   : REX.Match_Array (0 .. 1);
      capture_jar : REX.Match_Array (0 .. 5);
      dir_jar     : REX.Match_Array (0 .. 4);

      real_top_directory : constant String := Unix.real_path (stage_directory);

      function get_true_path (line : String) return String is
      begin
         if line (line'First) = '/' then
            return Unix.real_path (real_top_directory & line);
         end if;
         return Unix.real_path (real_top_directory & prefix_directory & "/" & line);
      end get_true_path;

      function categorize_line (line : String) return linecat
      is
         use type REX.Match_Location;

         position : Integer;
      begin
         if line = "" then
            return jump;
         end if;
         if line (line'First) = '@' then
            if line (line'First .. line'First + 8) = "@comment " then
               return jump;
            end if;
            position := ASF.Index (line, " ");
            if position > 2 then
               --  possible keyword.  See if it matches @(,,) pattern
               REX.Match (Self => regmech, Data => line, Matches => match_jar);
               if match_jar (0) = REX.No_Match then
                  return external_keyword;
               else
                  return mode_override;
               end if;
            end if;
            return key_error;
         end if;
         return file_path;
      end categorize_line;

      procedure handle_file_path (line : String)
      is
         true_path : constant String := get_true_path (line);
         insert_succeeded : Boolean;
      begin
         if Archive.Unix.real_path (true_path) = "" then
            if level >= normal then
               TIO.Put_Line ("Manifest entity [" & line & "] does not exist, ignoring");
            end if;
         else
            insert_succeeded := whitelist.insert_file_into_whitelist
              (full_path     => true_path,
               real_top_path => real_top_directory,
               level         => level);
            if not insert_succeeded then
               succeeded := False;
            end if;
         end if;
      end handle_file_path;

      procedure handle_mode_keyword (line : String) is
      begin
         REX.Match (Self => captured, Data => line, Matches => capture_jar);
         if ASF.Trim (line (capture_jar (4).First .. capture_jar (4).Last), Ada.Strings.Both) = ""
         then
            succeeded := False;
            if level >= normal then
               TIO.Put_Line ("Manifest entity [" & line & "] mode keyword has no path argument");
            end if;
            return;
         end if;

         declare
            insert_succeeded : Boolean;
            grp_owner : constant String := line (capture_jar (1).First .. capture_jar (1).Last);
            grp_group : constant String := line (capture_jar (2).First .. capture_jar (2).Last);
            grp_perms : constant String := line (capture_jar (3).First .. capture_jar (3).Last);
            grp_path  : constant String := line (capture_jar (4).First .. capture_jar (4).Last);
            true_path : constant String := get_true_path (ASF.Trim (grp_path, Ada.Strings.Both));
         begin
            if Archive.Unix.real_path (true_path) = "" then
               if level >= normal then
                  TIO.Put_Line ("Manifest entity [" & line & "] does not exist, ignoring");
               end if;
            else
               insert_succeeded := whitelist.ingest_manifest_with_mode_override
                 (full_path     => true_path,
                  real_top_path => real_top_directory,
                  new_owner     => grp_owner,
                  new_group     => grp_group,
                  new_perms     => grp_perms,
                  level         => level);
               if not insert_succeeded then
                  succeeded := False;
               end if;
            end if;
         end;
      end handle_mode_keyword;

      procedure invoke_keyword_callback (line : String)
      is
         use type REX.Match_Location;
      begin
         REX.Match (Self => keymech, Data => line, Matches => key_jar);
         if key_jar (0) = REX.No_Match then
            if level >= normal then
               TIO.Put_Line ("Manifest entity [" & line & "] keyword line failed to parse");
               return;
            end if;
            succeeded := False;
            return;
         end if;

         declare
            keyword   : constant String := line (key_jar (1).First .. key_jar (1).Last);
            arguments : constant String := line (key_jar (2).First .. key_jar (2).Last);
            relative  : constant String := first_word (arguments);
            full_path : constant String := get_true_path (relative);
         begin
            if keyword = "dir" then
               --  standard directory creation/destruction
               whitelist.insert_temporary_directory (dir_path  => relative,
                                                     full_path => full_path,
                                                     level     => level);
               return;
            end if;

            --  check @dir(,,) keyword
            REX.Match (dirmech, keyword, dir_jar);
            if dir_jar (0) /= REX.No_Match then
               --  Either directory creation/destruction or POG override
               whitelist.insert_temporary_directory
                 (dir_path   => relative,
                  full_path  => full_path,
                  attr_owner => keyword (dir_jar (1).First .. dir_jar (1).Last),
                  attr_group => keyword (dir_jar (2).First .. dir_jar (2).Last),
                  attr_perms => keyword (dir_jar (3).First .. dir_jar (3).Last),
                  level      => level);
               return;
            end if;

            --  External keyword
            if not KWD.process_external_keyword
              (whitelist     => whitelist,
               keyword       => keyword,
               arguments     => arguments,
               keyword_dir   => keywords_directory,
               full_path     => full_path,
               real_top_path => real_top_directory,
               prefix_dir    => prefix_directory,
               level         => level)
            then
               succeeded := False;
            end if;
         end;
      end invoke_keyword_callback;

   begin
      if real_top_directory = "" then
         if level >= normal then
            TIO.Put_Line ("The top directory [" & stage_directory &
                            "] does not resolve to a real path.");
         end if;
         return False;
      end if;
      features := Unix.get_charactistics (real_top_directory);
      if features.ftype /= directory then
         if level >= normal then
            TIO.Put_Line ("The top directory [" & stage_directory & "] is not really a directory.");
         end if;
         return False;
      end if;

      features := Unix.get_charactistics (manifest_file);
      if features.error then
         if level >= normal then
            TIO.Put_Line ("The indicated manifest (" & manifest_file & ") does not exist.");
         end if;
         return False;
      elsif features.ftype /= regular then
         if level >= normal then
            TIO.Put_Line ("The indicated manifest is not a regular file.");
         end if;
         return False;
      end if;

      TIO.Open (File => file_handle,
                Mode => TIO.In_File,
                Name => manifest_file);
      while not TIO.End_Of_File (file_handle) loop
         declare
            line     : constant String := ASF.Trim (TIO.Get_Line (file_handle), Ada.Strings.Both);
            category : constant linecat := categorize_line (line);
         begin
            case category is
               when jump =>
                  null;
               when key_error =>
                  if level >= normal then
                     TIO.Put_Line ("Manifest entity [" & line & "] has invalid keyword, ignoring");
                  end if;
               when external_keyword =>
                  invoke_keyword_callback (line);
               when file_path  =>
                  handle_file_path (line);
               when mode_override =>
                  handle_mode_keyword (line);
            end case;
         end;
      end loop;
      TIO.Close (file_handle);

      whitelist.process_temporary_directories (level);
      whitelist.list_used := True;
      return succeeded;

   end ingest_file_manifest;


   ----------------------------------
   --  insert_file_into_whitelist  --
   ----------------------------------
   function insert_file_into_whitelist
     (whitelist     : in out A_Whitelist;
      full_path     : String;
      real_top_path : String;
      level         : info_level) return Boolean
   is
      file_hash : Blake_3.blake3_hash;
      features  : Unix.File_Characteristics;
      props     : white_properties;
   begin
      features := Unix.get_charactistics (full_path);
      if features.error then
         if level >= normal then
            TIO.Put_Line ("The whitelisted file '" & full_path & "' does not exist.");
         end if;
         return False;
      end if;

      if features.ftype = directory then
         if level >= normal then
            TIO.Put_Line ("The whitelisted file '" & full_path & "' is of type directory.");
         end if;
         return False;
      end if;

      if whitelist.file_on_whitelist (full_path) then
         if level >= normal then
            TIO.Put_Line ("Duplicate line discovered: " & full_path);
         end if;
         return False;
      end if;

      if level >= verbose then
         TIO.Put_Line ("Adding to whitelist: " & full_path);
      end if;
      file_hash := Blake_3.digest (full_path);
      props.is_directory := False;
      whitelist.files.Insert (file_hash, props);

      --  Now insert the file's directory tree
      whitelist.insert_directory_into_whitelist (dir_path      => head (full_path, "/"),
                                                 real_top_path => real_top_path,
                                                 level         => level);
      return True;
   end insert_file_into_whitelist;


   ---------------------------------------
   --  insert_directory_into_whitelist  --
   ---------------------------------------
   procedure insert_directory_into_whitelist
     (whitelist     : in out A_Whitelist;
      dir_path      : String;
      real_top_path : String;
      level         : info_level)
   is
      file_hash : Blake_3.blake3_hash;
      props     : white_properties;
   begin
      if real_top_path = dir_path then
         --  stop recursion
         return;
      end if;
      if not whitelist.directory_on_whitelist (dir_path) then
         if level >= debug then
            TIO.Put_Line ("Adding directory to whitelist: " & dir_path);
         end if;
         file_hash := Blake_3.digest (dir_path);
         props.is_directory := True;
         whitelist.files.Insert (file_hash, props);
      end if;
      insert_directory_into_whitelist (whitelist     => whitelist,
                                       dir_path      => head (dir_path, "/"),
                                       real_top_path => real_top_path,
                                       level         => level);
   end insert_directory_into_whitelist;


   -------------------------------------
   --  insert_temporary_directory #1  --
   -------------------------------------
   procedure insert_temporary_directory
     (whitelist     : in out A_Whitelist;
      dir_path      : String;
      full_path     : String;
      level         : info_level)
   is
      --  This directory usually does not exist (it needs to be created/destroyed with package
      --  installation and deinstallation
      file_hash : Blake_3.blake3_hash;
      props     : white_properties;
   begin
      file_hash := Blake_3.digest (full_path);
      if whitelist.temp_dirs.Contains (file_hash) then
         if level >= normal then
            TIO.Put_Line ("Ignoring duplicate @dir " & dir_path);
         end if;
         return;
      end if;
      if level >= debug then
         TIO.Put_Line ("Adding directory to temporary heap: " & dir_path);
      end if;
      props.is_directory := True;
      props.path := ASU.To_Unbounded_String (dir_path);
      whitelist.temp_dirs.Insert (file_hash, props);
   end insert_temporary_directory;


   -------------------------------------
   --  insert_temporary_directory #2  --
   -------------------------------------
   procedure insert_temporary_directory
     (whitelist     : in out A_Whitelist;
      dir_path      : String;
      full_path     : String;
      attr_owner    : String;
      attr_group    : String;
      attr_perms    : String;
      level         : info_level)
   is
      --  This directory usually does not exist (it needs to be created/destroyed with package
      --  installation and deinstallation
      file_hash : Blake_3.blake3_hash;
      props     : white_properties;
   begin
      file_hash := Blake_3.digest (full_path);
      if whitelist.temp_dirs.Contains (file_hash) then
         if level >= normal then
            TIO.Put_Line ("Ignoring duplicate @dir " & dir_path);
         end if;
         return;
      end if;
      if level >= debug then
         TIO.Put_Line ("Adding directory to temporary heap: " & dir_path & " (" &
                      attr_owner & "," & attr_group & "," & attr_perms & ")");
      end if;
      props.is_directory := True;
      props.path := ASU.To_Unbounded_String (dir_path);

      if attr_owner /= "" then
         props.override_owner := True;
         props.owner_spec := Archive.Unix.str2owngrp (attr_owner);
      end if;

      if attr_group /= "" then
         props.override_group := True;
         props.group_spec := Archive.Unix.str2owngrp (attr_group);
      end if;

      if attr_perms /= "" then
         declare
            op_success  : Boolean;
            octal_perms : permissions;
         begin
            octal_perms := convert_mode (attr_perms, op_success);
            if op_success then
               props.override_perms := True;
               props.perms_spec := octal_perms;
            else
               if level >= verbose then
                  TIO.Put_Line ("failed to convert " & attr_perms &
                               " to an octal value, so mode change request ignored.");
               end if;
            end if;
         end;
      end if;
      whitelist.temp_dirs.Insert (file_hash, props);
   end insert_temporary_directory;


   ------------------------------------------
   --  ingest_manifest_with_mode_override  --
   ------------------------------------------
   function ingest_manifest_with_mode_override
     (whitelist     : in out A_Whitelist;
      full_path     : String;
      real_top_path : String;
      new_owner     : String;
      new_group     : String;
      new_perms     : String;
      level         : info_level) return Boolean
   is
      file_hash  : Blake_3.blake3_hash;
      features   : Unix.File_Characteristics;
      props      : white_properties;
   begin
      features := Unix.get_charactistics (full_path);
      if features.error then
         if level >= normal then
            TIO.Put_Line ("The whitelisted file '" & full_path & "' does not exist.");
         end if;
         return False;
      end if;

      if features.ftype = directory then
         if level >= normal then
            TIO.Put_Line ("The whitelisted file '" & full_path & "' is of type directory.");
         end if;
         return False;
      end if;

      if whitelist.file_on_whitelist (full_path) then
         if level >= normal then
            TIO.Put_Line ("Duplicate line discovered: " & full_path);
         end if;
         return False;
      end if;

      if level >= verbose then
         TIO.Put_Line ("Adding to whitelist: " & full_path & " (" & new_owner & "," &
                         new_group & "," & new_perms & ")");
      end if;
      file_hash := Blake_3.digest (full_path);
      props.is_directory := False;

      if new_owner /= "" then
         props.override_owner := True;
         props.owner_spec := Archive.Unix.str2owngrp (new_owner);
      end if;

      if new_group /= "" then
         props.override_group := True;
         props.group_spec := Archive.Unix.str2owngrp (new_group);
      end if;

      if new_perms /= "" then
         declare
            op_success  : Boolean;
            octal_perms : permissions;
         begin
            octal_perms := convert_mode (new_perms, op_success);
            if op_success then
               props.override_perms := True;
               props.perms_spec := octal_perms;
            else
               if level >= verbose then
                  TIO.Put_Line ("failed to convert " & new_perms &
                               " to an octal value, so mode change request ignored.");
               end if;
            end if;
         end;
      end if;

      whitelist.files.Insert (file_hash, props);

      --  Now insert the file's directory tree
      whitelist.insert_directory_into_whitelist (dir_path      => head (full_path, "/"),
                                                 real_top_path => real_top_path,
                                                 level         => level);
      return True;
   end ingest_manifest_with_mode_override;


   -------------------------------------
   --  process_temporary_directories  --
   -------------------------------------
   procedure process_temporary_directories
     (whitelist : in out A_Whitelist;
      level     : info_level)
   is
      procedure process (position : white_crate.Cursor);
      procedure process (position : white_crate.Cursor)
      is
         procedure reset_POG (key : Blake_3.blake3_hash; Element : in out white_properties);

         old_props : white_properties;
         props     : white_properties renames white_crate.Element (position);
         file_hash : Blake_3.blake3_hash renames white_crate.Key (position);

         procedure reset_POG (key : Blake_3.blake3_hash; Element : in out white_properties)
         is
            pragma Unreferenced (key);
         begin
            Element.override_group := props.override_group;
            Element.override_owner := props.override_owner;
            Element.override_perms := props.override_perms;
            Element.owner_spec     := props.owner_spec;
            Element.group_spec     := props.group_spec;
            Element.perms_spec     := props.perms_spec;
         end reset_POG;
      begin
         if whitelist.files.Contains (file_hash) then
            old_props := whitelist.files.Element (file_hash);
            if not old_props.is_directory then
               if level >= normal then
                  TIO.Put_Line ("whitelist error: @dir " & ASU.To_String (props.path) &
                                  " matches a listed file, ignoring");
               end if;
            else
               --  old_props ALWAYS have overrides set to false
               --  We only need to check override settings on new props
               if not props.override_group and then
                 not props.override_owner and then
                 not props.override_perms
               then
                  if level >= normal then
                     TIO.Put_Line ("whitelist notice: @dir " & ASU.To_String (props.path) &
                                     " is unnecessary; the directory is already being created.");
                  end if;
               else
                  --  The POG attributes don't match, so reset the files version of them.
                  if level >= debug then
                     TIO.Put_Line ("Adjust perms/owner/group of auto-created directory " &
                                     ASU.To_String (props.path));
                  end if;
                  whitelist.files.Update_Element
                    (Position => whitelist.files.Find (file_hash),
                     Process  => reset_POG'Access);
               end if;
            end if;
         else
            if level >= debug then
               TIO.Put_Line ("just_dirs += " & ASU.To_String (props.path));
            end if;
            whitelist.just_dirs.Insert (file_hash, props);
            whitelist.dirs_keys.Append (file_hash);
         end if;
      end process;
   begin
      whitelist.temp_dirs.Iterate (process'Access);
      whitelist.temp_dirs.Clear;
   end process_temporary_directories;


   -------------------
   --  digest_hash  --
   -------------------
   function digest_hash (key : Blake_3.blake3_hash) return CON.Hash_Type
   is
   begin
      return Ada.Strings.Hash (key);
   end digest_hash;


   -------------------------
   --  digest_equivalent  --
   -------------------------
   function digest_equivalent (key1, key2 : Blake_3.blake3_hash) return Boolean
   is
   begin
      return key1 = key2;
   end digest_equivalent;


   ------------
   --  head  --
   ------------
   function head (S : String; delimiter : String) return String
   is
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


   ------------
   --  tail  --
   ------------
   function tail (S : String; delimiter : String) return String
   is
      dl_size      : constant Natural := delimiter'Length;
      back_marker  : constant Natural := S'First;
      front_marker : Natural := S'Last - dl_size + 1;
   begin
      loop
         if front_marker < back_marker then
            --  delimiter never found
            return S;
         end if;
         if S (front_marker .. front_marker + dl_size - 1) = delimiter then
            return S (front_marker + dl_size .. S'Last);
         end if;
         front_marker := front_marker - 1;
      end loop;
   end tail;


   ------------------
   --  first_word  --
   ------------------
   function first_word (S : String) return String
   is
      cleaner : constant String := ASF.Trim (S, Ada.Strings.Both);
      space   : Natural;
   begin
      space := ASF.Index (Source => cleaner, Pattern => " ");
      if space = 0 then
         return cleaner;
      end if;
      return cleaner (cleaner'First .. space - 1);
   end first_word;


   --------------------
   --  convert_mode  --
   --------------------
   function convert_mode (S : String; success : out Boolean) return permissions
   is
      no_permissions : constant permissions := 0;
      index    : Natural := 0;
      mychar   : Character;
      result   : permissions;
      fragment : permissions;
   begin
      result := 0;
      success := True;
      if S'Length < 3 or else S'Length > 4 then
         success := False;
         return no_permissions;
      end if;
      for arrow in reverse S'Range loop
         mychar := S (arrow);
         case mychar is
            when '0' .. '7' => null;
               fragment := permissions (Character'Pos (mychar) - Character'Pos ('0'));
               --  shift
               fragment := fragment * (8 ** index);
            when others =>
               success := False;
               return no_permissions;
         end case;
         result := result + fragment;
         index := index + 1;
      end loop;
      return result;
   end convert_mode;


   -------------------------
   --  get_file_features  --
   -------------------------
   function get_file_features
     (whitelist     : A_Whitelist;
      file_path     : String;
      actual_owner  : ownergroup;
      actual_group  : ownergroup;
      actual_perms  : permissions) return white_features
   is
      file_hash : Blake_3.blake3_hash;
      result : white_features;
   begin
      file_hash := Blake_3.digest (file_path);
      result.group_spec := actual_group;
      result.owner_spec := actual_owner;
      result.perms_spec := actual_perms;

      if whitelist.files.Contains (file_hash) then
         if whitelist.files.Element (file_hash).override_group then
            result.group_spec := whitelist.files.Element (file_hash).group_spec;
         end if;
         if whitelist.files.Element (file_hash).override_owner then
            result.owner_spec := whitelist.files.Element (file_hash).owner_spec;
         end if;
         if whitelist.files.Element (file_hash).override_perms then
            result.perms_spec := whitelist.files.Element (file_hash).perms_spec;
         end if;
      end if;
      return result;
   end get_file_features;


   -----------------------------
   --  empty_directory_count  --
   -----------------------------
   function empty_directory_count
     (whitelist     : A_Whitelist) return Natural
   is
   begin
      return Natural (whitelist.dirs_keys.Length);
   end empty_directory_count;


   --------------------------------
   --  get_empty_directory_path  --
   --------------------------------
   function get_empty_directory_path
     (whitelist     : A_Whitelist;
      index         : Natural) return String
   is
      key : constant Blake_3.blake3_hash := whitelist.get_empty_directory_hash (index);
   begin
      if whitelist.just_dirs.Contains (key) then
         return ASU.To_String (whitelist.just_dirs.Element (key).path);
      end if;
      return "";
   end get_empty_directory_path;


   --------------------------------
   --  get_empty_directory_hash  --
   --------------------------------
   function get_empty_directory_hash
     (whitelist     : A_Whitelist;
      index         : Natural) return Blake_3.blake3_hash is
   begin
      if index < whitelist.empty_directory_count then
         return whitelist.dirs_keys.Element (index);
      end if;
      declare
         dummy : Blake_3.blake3_hash := (others => Character'Val (0));
      begin
         return dummy;
      end;
   end get_empty_directory_hash;


   --------------------------------------
   --  get_empty_directory_attributes  --
   --------------------------------------
   function get_empty_directory_attributes
     (whitelist     : A_Whitelist;
      index         : Natural) return white_features
   is
      attributes : white_features;
   begin
      attributes.owner_spec := null_owngrp;
      attributes.group_spec := null_owngrp;
      attributes.perms_spec := 0;

      if index < whitelist.empty_directory_count then
         declare
            file_hash : constant Blake_3.blake3_hash := whitelist.dirs_keys.Element (index);
         begin
            if whitelist.just_dirs.Contains (file_hash) then
               if whitelist.just_dirs.Element (file_hash).override_owner then
                  attributes.owner_spec := whitelist.just_dirs.Element (file_hash).owner_spec;
               end if;
               if whitelist.just_dirs.Element (file_hash).override_group then
                  attributes.group_spec := whitelist.just_dirs.Element (file_hash).group_spec;
               end if;
               if whitelist.just_dirs.Element (file_hash).override_perms then
                  attributes.perms_spec := whitelist.just_dirs.Element (file_hash).perms_spec;
               end if;
            end if;
         end;
      end if;
      return attributes;
   end get_empty_directory_attributes;


   ---------------------
   --  convert_phase  --
   ---------------------
   function convert_phase (phase : package_phase) return String is
   begin
      case phase is
         when pre_install        => return "pre-install";
         when pre_install_lua    => return "pre-install-lua";
         when pre_deinstall      => return "pre-deinstall";
         when pre_deinstall_lua  => return "pre-deinstall-lua";
         when post_install       => return "post-install";
         when post_install_lua   => return "post-install-lua";
         when post_deinstall     => return "post-deinstall";
         when post_deinstall_lua => return "post-deinstall-lua";
         when prepackaging       => return "prepackaging";
      end case;
   end convert_phase;


   -------------------
   --  script_count  --
   --------------------
   function script_count
     (whitelist     : A_Whitelist;
      phase         : package_phase) return Natural
   is
   begin
      return Natural (whitelist.scripts (phase).Length);
   end script_count;


   ------------------
   --  get_script  --
   ------------------
   function get_script
     (whitelist     : A_Whitelist;
      phase         : package_phase;
      index         : Natural) return String
   is
   begin
      return ASU.To_String (whitelist.scripts (phase).Element (index).script);
   end get_script;


end Archive.Whitelist;
