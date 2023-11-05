--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt


with Ada.Strings.Hash;
with Ada.Strings.Fixed;
with Ada.Text_IO;
with Archive.Unix;
with GNAT.Regpat;

package body Archive.Whitelist is

   package TIO renames Ada.Text_IO;
   package ASF renames Ada.Strings.Fixed;
   package REX renames GNAT.Regpat;

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
     (whitelist        : out A_Whitelist;
      manifest_file    : String;
      stage_directory  : String;
      prefix_directory : String;
      level            : info_level) return Boolean
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
      keymech     : constant REX.Pattern_Matcher := REX.compile ("^@([^(]\w*) (.*)");
      key_jar     : REX.Match_Array (0 .. 2);
      match_jar   : REX.Match_Array (0 .. 1);
      capture_jar : REX.Match_Array (0 .. 5);

      real_top_directory : constant String := Unix.real_path (stage_directory);

      function get_true_path (line : String) return String is
      begin
         if line (line'First) = '/' then
            return real_top_directory & line;
         end if;
         return real_top_directory & prefix_directory & "/" & line;
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
         if true_path = "" then
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
            if true_path = "" then
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
         begin
            TIO.Put_line ("Todo: Handle keyword " & keyword & " (" & arguments & ")");
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

end Archive.Whitelist;
