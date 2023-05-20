--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Ada.Characters.Latin_1;
with Ada.Strings.Fixed;

package body Archive.Unix is

   package LAT renames Ada.Characters.Latin_1;
   package ASF renames Ada.Strings.Fixed;

   ------------------------------------------------------------------------------------------
   --  success
   ------------------------------------------------------------------------------------------
   function success (rc : IC.int) return Boolean
   is
      use type IC.int;
   begin
      return (rc = IC.int (0));
   end success;


   ------------------------------------------------------------------------------------------
   --  failure
   ------------------------------------------------------------------------------------------
   function failure (rc : IC.int) return Boolean
   is
      use type IC.int;
   begin
      return (rc = IC.int (1));
   end failure;


   ------------------------------------------------------------------------------------------
   --  stat_ok
   ------------------------------------------------------------------------------------------
   function stat_ok (path : String; sb : struct_stat_Access) return Boolean
   is
      c_path : IC.Strings.chars_ptr;
      res    : IC.int;
   begin
      c_path := IC.Strings.New_String (path);
      res := arc_stat (c_path, sb);
      IC.Strings.Free (c_path);
      return success (res);
   end stat_ok;


   ------------------------------------------------------------------------------------------
   --  get_charactistics
   ------------------------------------------------------------------------------------------
   function get_charactistics (path : String) return File_Characteristics
   is
      result : File_Characteristics;
      sb     : aliased Unix.struct_stat;
      tspec  : time_specification;
   begin
      result.ftype := unsupported;
      result.mtime := 0;
      result.perms := 0;
      result.inode := 0;
      result.error := True;
      begin
         if Unix.stat_ok (path, sb'Unchecked_Access) then
            tspec := file_modification_time (sb'Unchecked_Access);
            result.ftype := type_of_file (sb'Unchecked_Access);
            result.owner := file_owner (sb'Unchecked_Access);
            result.group := file_group (sb'Unchecked_Access);
            result.mtime := tspec.epoch;
            result.mnsec := tspec.nsecs;
            result.perms := file_permissions (sb'Unchecked_Access);
            result.inode := inode_number (sb'Unchecked_Access);
            result.error := False;
         else
            result.owner := str2owngrp ("xxx");
            result.group := str2owngrp ("xxx");
         end if;
      exception
         when others =>
            null;
      end;
      return result;
   end get_charactistics;


   ------------------------------------------------------------------------------------------
   --  get_mtime
   ------------------------------------------------------------------------------------------
   function file_modification_time (sb : struct_stat_Access) return time_specification
   is
      cres : timespec;
      result : time_specification;
   begin
      cres := arc_get_mtime (sb);
      result.epoch := filetime (cres.tv_sec);
      result.nsecs := nanoseconds (cres.tv_nsec);
      return result;
   end file_modification_time;


   ------------------------------------------------------------------------------------------
   --  file_permissions
   ------------------------------------------------------------------------------------------
   function file_permissions (sb : struct_stat_Access) return permissions
   is
      res : IC.short;
   begin
      res := arc_extract_permissions (sb);
      return permissions (res);
   end file_permissions;


   ------------------------------------------------------------------------------------------
   --  file_owner
   ------------------------------------------------------------------------------------------
   function file_owner (sb : struct_stat_Access) return ownergroup
   is
      use type IC.Strings.chars_ptr;

      c_owner : IC.Strings.chars_ptr;
   begin
      c_owner := arc_get_owner (sb);
      if c_owner = IC.Strings.Null_Ptr then
         declare
            id : constant IC.unsigned := arc_get_owner_id (sb);
         begin
            return str2owngrp (int2str (Integer (id)));
         end;
      end if;
      declare
         owner : constant String := IC.Strings.Value (c_owner);
      begin
         return str2owngrp (owner);
      end;
   end file_owner;


   ------------------------------------------------------------------------------------------
   --  file_group
   ------------------------------------------------------------------------------------------
   function file_group (sb : struct_stat_Access) return ownergroup
   is
      use type IC.Strings.chars_ptr;

      c_group : IC.Strings.chars_ptr;
   begin
      c_group := arc_get_group (sb);
      if c_group = IC.Strings.Null_Ptr then
         declare
            id : constant IC.unsigned := arc_get_group_id (sb);
         begin
            return str2owngrp (int2str (Integer (id)));
         end;
      end if;
      declare
         group : constant String := IC.Strings.Value (c_group);
      begin
         return str2owngrp (group);
      end;
   end file_group;


   ------------------------------------------------------------------------------------------
   --  str2owngrp
   ------------------------------------------------------------------------------------------
   function str2owngrp (name : String) return ownergroup
   is
      result  : ownergroup := (others => Character'Val (0));
   begin
      if name'Length > ownergroup'Length then
         result := name (name'First .. name'First + ownergroup'Length - 1);
      else
         result (result'First .. result'First + name'Length - 1) := name;
      end if;
      return result;
   end str2owngrp;


   ------------------------------------------------------------------------------------------
   --  int2str
   ------------------------------------------------------------------------------------------
   function int2str (A : Integer) return String
   is
      raw : constant String := A'Img;
   begin
      if A < 0 then
         return raw;
      end if;
      return raw (raw'First + 1 .. raw'Last);
   end int2str;


   ------------------------------------------------------------------------------------------
   --  type_of_file
   ------------------------------------------------------------------------------------------
   function type_of_file (sb : struct_stat_Access) return file_type
   is
      res : constant IC.unsigned_char := arc_get_file_type (sb);
   begin
      case res is
         when 1 => return directory;
         when 2 => return regular;
         when 3 => return symlink;
         when 4 => return hardlink;
         when 5 => return fifo;
         when others => return unsupported;
      end case;
   end type_of_file;


   ------------------------------------------------------------------------------------------
   --  inode_number
   ------------------------------------------------------------------------------------------
   function inode_number (sb : struct_stat_Access) return inode_type
   is
      res : constant IC.unsigned_long_long := arc_get_inode_number (sb);
   begin
      return inode_type (res);
   end inode_number;


   ------------------------------------------------------------------------------------------
      --  link_target
   ------------------------------------------------------------------------------------------
   function link_target (symlink_path : String) return String
   is
      bufsiz : constant IC.size_t := 1024;
      buffer : array (1 .. bufsiz) of aliased IC.unsigned_char;
      c_path : IC.Strings.chars_ptr;
      res    : IC.long;
   begin
      c_path := IC.Strings.New_String (symlink_path);
      res := arc_readlink (c_path, buffer (1)'Access, bufsiz);
      IC.Strings.Free (c_path);

      declare
         size   : constant Integer := Integer (res);
         result : String (1 .. size);
      begin
         for x in 1 .. size loop
            result (x) := Character'Val (buffer (IC.size_t (x)));
         end loop;
         return result;
      end;
   end link_target;


   --------------------------------------------------------------------------------------------
   --  create_symlink
   --------------------------------------------------------------------------------------------
   function create_symlink (actual_file : String; link_to_create : String) return Boolean
   is
      use type IC.int;
      path1  : constant IC.char_array := IC.To_C (actual_file);
      path2  : constant IC.char_array := IC.To_C (link_to_create);
      result : IC.int;
   begin
      if actual_file = "" or else link_to_create = "" then
         return False;
      end if;
      result := symlink (path1, path2);

      return result = 0;
   end create_symlink;


   --------------------------------------------------------------------------------------------
   --  create_hardlink
   --------------------------------------------------------------------------------------------
   function create_hardlink (actual_file : String; destination : String) return Boolean
   is
      use type IC.int;
      path1  : constant IC.char_array := IC.To_C (actual_file);
      path2  : constant IC.char_array := IC.To_C (destination);
      result : IC.int;
   begin
      if actual_file = "" or else destination = "" then
         return False;
      end if;
      result := link (path1, path2);

      return result = 0;
   end create_hardlink;


   --------------------------------------------------------------------------------------------
   --  create_fifo
   --------------------------------------------------------------------------------------------
   function create_fifo (fifo_path : String; perms : permissions) return Boolean
   is
      use type IC.int;
      path   : constant IC.char_array := IC.To_C (fifo_path);
      mode   : constant IC.unsigned_short := IC.unsigned_short (perms);
      result : IC.int;
   begin
      if fifo_path = "" then
         return False;
      end if;
      result := mkfifo (path, mode);

      return result = 0;
   end create_fifo;


   --------------------------------------------------------------------------------------------
   --  change_mode
   --------------------------------------------------------------------------------------------
   function change_mode (path : String; perms : permissions) return Boolean
   is
      use type IC.int;
      cpath  : constant IC.char_array := IC.To_C (path);
      mode   : constant IC.unsigned_short := IC.unsigned_short (perms);
   begin
      if path = "" then
         return False;
      end if;
      return (chmod (cpath, mode) = 0);
   end change_mode;


   --------------------------------------------------------------------------------------------
   --  unlink_file
   --------------------------------------------------------------------------------------------
   function unlink_file (path : String) return Boolean
   is
      use type IC.int;
      cpath  : constant IC.char_array := IC.To_C (path);
   begin
      if path = "" then
         return False;
      end if;
      return (unlink (cpath) = 0);
   end unlink_file;


   --------------------------------------------------------------------------------------------
   --  lookup_group
   --------------------------------------------------------------------------------------------
   function lookup_group (name : String) return owngrp_id
   is
      cname : constant IC.char_array := IC.To_C (name);
   begin
      return owngrp_id (clookup_group (cname));
   end lookup_group;


   --------------------------------------------------------------------------------------------
   --  lookup_user
   --------------------------------------------------------------------------------------------
   function lookup_user (name : String) return owngrp_id
   is
      cname : constant IC.char_array := IC.To_C (name);
   begin
      return owngrp_id (clookup_user (cname));
   end lookup_user;


   --------------------------------------------------------------------------------------------
   --  adjust_metadata
   --------------------------------------------------------------------------------------------
   function adjust_metadata
     (path         : String;
      reset_owngrp : Boolean;
      reset_perms  : Boolean;
      reset_mtime  : Boolean;
      type_of_file : file_type;
      new_uid      : owngrp_id;
      new_gid      : owngrp_id;
      new_perms    : permissions;
      new_m_secs   : filetime;
      new_m_nano   : nanoseconds) return metadata_rc
   is
      cpath     : constant IC.char_array := IC.To_C (path);
      do_owner  : IC.unsigned_char := 0;
      do_perms  : IC.unsigned_char := 0;
      do_mtime  : IC.unsigned_char := 0;
      rescode   : IC.unsigned_char := 0;
      c_uid     : IC.unsigned := 0;
      c_gid     : IC.unsigned := 0;
      c_perms   : IC.short := 0;
   begin
      if reset_owngrp then
         do_owner := 1;
         c_uid := IC.unsigned (new_uid);
         c_gid := IC.unsigned (new_gid);
      end if;
      if reset_perms then
         do_perms := 1;
         c_perms := IC.short (new_perms);
      end if;
      if reset_mtime then
         do_mtime := 1;
      end if;
      case type_of_file is
         when regular | directory =>
            rescode := set_metadata
              (path              => cpath,
               reset_modtime     => do_mtime,
               reset_ownership   => do_owner,
               reset_permissions => do_perms,
               new_user_id       => c_uid,
               new_group_id      => c_gid,
               new_permissions   => c_perms,
               new_mtime_epoch   => time_t (new_m_secs),
               new_mtime_nsecs   => IC.long (new_m_nano));
         when symlink =>
            rescode := set_symlink_metadata
              (path              => cpath,
               reset_modtime     => do_mtime,
               reset_ownership   => do_owner,
               reset_permissions => do_perms,
               new_user_id       => c_uid,
               new_group_id      => c_gid,
               new_permissions   => c_perms,
               new_mtime_epoch   => time_t (new_m_secs),
               new_mtime_nsecs   => IC.long (new_m_nano));
         when fifo =>
            rescode := set_fifo_metadata
              (path              => cpath,
               reset_modtime     => do_mtime,
               reset_ownership   => do_owner,
               reset_permissions => do_perms,
               new_user_id       => c_uid,
               new_group_id      => c_gid,
               new_permissions   => c_perms,
               new_mtime_epoch   => time_t (new_m_secs),
               new_mtime_nsecs   => IC.long (new_m_nano));
         when unsupported | hardlink =>
            --  Skip hardlinks.  This is set on the target.
            --  Doing it again is redundant
            null;
      end case;
      return metadata_rc (rescode);
   end adjust_metadata;


   --------------------------------------------------------------------------------------------
   --  metadata_error
   --------------------------------------------------------------------------------------------
   function metadata_error (errcode : metadata_rc) return String
   is
      function check_flag_1 return String;
      function check_flag_2 return String;
      function check_flag_3 return String;
      function check_flag_4 return String;
      function check_flag_5 return String;

      function check_flag_1 return String
      is
         flag : constant metadata_rc := 1;
      begin
         if (errcode and flag) > 0 then
            return "error: Failed to open file." & LAT.LF;
         end if;
         return "";
      end check_flag_1;

      function check_flag_2 return String
      is
         flag : constant metadata_rc := 2;
      begin
         if (errcode and flag) > 0 then
            return "error: Failed to close file." & LAT.LF;
         end if;
         return "";
      end check_flag_2;

      function check_flag_3 return String
      is
         flag : constant metadata_rc := 4;
      begin
         if (errcode and flag) > 0 then
            return "error: Failed to set owner and/or group." & LAT.LF;
         end if;
         return "";
      end check_flag_3;

      function check_flag_4 return String
      is
         flag : constant metadata_rc := 8;
      begin
         if (errcode and flag) > 0 then
            return "error: Failed to set file permissions." & LAT.LF;
         end if;
         return "";
      end check_flag_4;

      function check_flag_5 return String
      is
         flag : constant metadata_rc := 16;
      begin
         if (errcode and flag) > 0 then
            return "error: Failed to set file modification time." & LAT.LF;
         end if;
         return "";
      end check_flag_5;
   begin
      if errcode = 0 then
         return "";
      end if;
      return ASF.Trim (check_flag_1 & check_flag_2 & check_flag_3 & check_flag_4 & check_flag_5,
                       Ada.Strings.Right);

   end metadata_error;


   --------------------------------------------------------------------------------------------
   --  real_path
   --------------------------------------------------------------------------------------------
   function real_path (path : String) return String
   is
      use type IC.size_t;
      use type IC.Strings.chars_ptr;

      cpath : constant IC.char_array := IC.To_C (path);
      path_max : constant IC.size_t := maximum_path_length;
      resolved_path : IC.char_array (0 .. path_max - 1);
      respath : IC.Strings.chars_ptr;

   begin
      respath := realpath (path => cpath, resolvedname => resolved_path);
      if respath = IC.Strings.Null_Ptr then
         return "";
      end if;
      return IC.Strings.Value (respath);
   end real_path;

end Archive.Unix;
