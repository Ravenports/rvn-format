--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Interfaces.C.Strings;

package Archive.Unix is

   package IC renames Interfaces.C;

   type File_Characteristics is
      record
         ftype : file_type;
         owner : ownergroup;
         group : ownergroup;
         mtime : filetime;
         mnsec : nanoseconds;
         perms : permissions;
         inode : inode_type;
         error : Boolean;
         size  : exabytes;
         uid   : owngrp_id;
         gid   : owngrp_id;
      end record;

   --  Set both RDONLY and WRONLY to get RDRW flags
   type T_Open_Flags is
      record
         RDONLY    : Boolean := False;
         WRONLY    : Boolean := False;
         APPEND    : Boolean := False;
         NON_BLOCK : Boolean := False;
         DIRECTORY : Boolean := False;
         CLOEXEC   : Boolean := False;
         CREAT     : Boolean := False;
         TRUNC     : Boolean := False;
      end record;

   type Time_Characteristics is
      record
         ftype : file_type;
         mtime : filetime;
         mnsec : nanoseconds;
         error : Boolean;
      end record;

   type metadata_rc is mod 2 ** 5;
   type File_Descriptor is new Integer;
   not_connected : constant File_Descriptor := -1;

   --  Return set of file characteristis given the path to a file or a directory
   function get_charactistics (path : String) return File_Characteristics;

   --  True if the path points to a file, directory, symlink, hardlink or FIFO
   function file_exists (path : String) return Boolean;

   --  Return symlink target path
   function link_target (symlink_path : String) return String;

   --  Attempts to create a hardlink and returns True on success
   function create_hardlink (actual_file : String; destination : String) return Boolean;

   --  Attempts to create a symlink and returns True on success
   function create_symlink (actual_file : String; link_to_create : String) return Boolean;

   --  Attempts to create FIFO file and returns True on success
   function create_fifo (fifo_path : String; perms : permissions) return Boolean;

   --  Delete file via libc functions
   function unlink_file (path : String) return Boolean;

   --  Attempts to set file permissions
   function change_mode (path : String; perms : permissions) return Boolean;

   --  Returns the group ID given the group name (or 4,000,000,000 if not found)
   function lookup_group (name : String) return owngrp_id;

   --  Returns the User ID given the user name (or 4,000,000,000 if not found)
   function lookup_user (name : String) return owngrp_id;

   --  Sets file metadata (ownership, perms, modification time) in any combination at once.
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
      new_m_nano   : nanoseconds) return metadata_rc;

   --  Return error description from adjust_metadata return code
   function metadata_error (errcode : metadata_rc) return String;

   --  Ths function resolves all symbolic links, extra ``/'' characters and
   --  references to /./ and /../ in path and returns the result
   function real_path (path : String) return String;

   --  Returns true if the user running the program is root.
   function user_is_root return Boolean;

   --  Returns true if the user can write to the file (or directory)
   function file_is_writable (path : String) return Boolean;

   --  Display rwx permissions including sticky bit, setuid-on-exec, etc
   function display_permissions (perms : permissions; ftype : file_type) return String;

   --  Returns file modification time formated to yyyy-mm-dd HH:MM
   function format_file_time (modtime : filetime) return String;

   --  Useful function to format owners and groups
   function str2owngrp (name : String) return ownergroup;

   --  Converts a string to a null-terminals char array
   function convert_to_char_array (S : String) return IC.char_array;

   --  Use libc's open function to retrieve file descriptor
   function open_file (filename : String; flags : T_Open_Flags) return File_Descriptor;

   --  Return True if fd /= -1
   function file_connected (fd : File_Descriptor) return Boolean;

   --  Send log down file descriptor of event pipe
   procedure push_to_event_pipe (fd : File_Descriptor; message : String);

      --  Return file modification time given the path to a file or a directory
   function get_time_characteristics (path : String) return Time_Characteristics;

   --  Returns True if the given file has a modification time in the past
   function tag_expired (mtime : filetime) return Boolean;

   --  self-explanatory
   procedure delete_file_if_it_exists (filename : String);

private

   function success (rc : IC.int) return Boolean;
   function failure (rc : IC.int) return Boolean;

   type uid_t is new Integer;

   type stat_block is array (1 .. 256) of IC.unsigned_char;
   type struct_stat is limited
      record
         --  sizeof(struct stat) is 128 on DragonFly
         --  Double that to ensure we allocate enough
         block : stat_block;
      end record;

   type time_specification is
      record
         epoch : filetime;
         nsecs : nanoseconds;
      end record;

   type struct_stat_Access is access all struct_stat;
   pragma Convention (C, struct_stat_Access);

   type time_t is new IC.long;
   type timespec is record
      tv_sec   : time_t;
      tv_nsec  : IC.long;
   end record;
   pragma Convention (C, timespec);

   function arc_stat
     (path : IC.Strings.chars_ptr;
      sb   : struct_stat_Access) return IC.int;
   pragma Import (C, arc_stat, "lstat");

   function arc_readlink
     (path   : IC.Strings.chars_ptr;
      buf    : access IC.unsigned_char;
      bufsiz : IC.size_t) return IC.long;
   pragma Import (C, arc_readlink, "readlink");

   function arc_get_mtime (sb : struct_stat_Access) return timespec;
   pragma Import (C, arc_get_mtime, "get_mtime");

   function arc_extract_permissions (sb : struct_stat_Access) return IC.short;
   pragma Import (C, arc_extract_permissions, "extract_permissions");

   function arc_get_owner (sb : struct_stat_Access) return IC.Strings.chars_ptr;
   pragma Import (C, arc_get_owner, "get_owner_name");

   function arc_get_group (sb : struct_stat_Access) return IC.Strings.chars_ptr;
   pragma Import (C, arc_get_group, "get_group_name");

   function arc_get_owner_id (sb : struct_stat_Access) return IC.unsigned;
   pragma Import (C, arc_get_owner_id, "get_owner_id");

   function arc_get_group_id (sb : struct_stat_Access) return IC.unsigned;
   pragma Import (C, arc_get_group_id, "get_group_id");

   function arc_get_file_type (sb : struct_stat_Access) return IC.unsigned_char;
   pragma Import (C, arc_get_file_type, "get_file_type");

   function arc_get_inode_number (sb : struct_stat_Access) return IC.unsigned_long_long;
   pragma Import (C, arc_get_inode_number, "get_inode_number");

   function arc_get_file_size (sb : struct_stat_Access) return IC.unsigned_long_long;
   pragma Import (C, arc_get_file_size, "get_size");

   function arc_time (tloc : access filetime) return filetime;
   pragma Import (C, arc_time, "time");

   function symlink (path1, path2 : IC.char_array) return IC.int;
   pragma Import (C, symlink);

   function link (path1, path2 : IC.char_array) return IC.int;
   pragma Import (C, link);

   function unlink (path : IC.char_array) return IC.int;
   pragma Import (C, unlink);

   function mkfifo (path : IC.char_array; mode : IC.unsigned_short) return IC.int;
   pragma Import (C, mkfifo);

   function chmod (path : IC.char_array; mode : IC.unsigned_short) return IC.int;
   pragma Import (C, chmod);

   function realpath
     (path : IC.char_array;
      resolvedname : out IC.char_array) return IC.Strings.chars_ptr;
   pragma Import (C, realpath);

   function C_dprint2 (fd : IC.int; msg : IC.Strings.chars_ptr) return IC.int;
   pragma Import (C, C_dprint2, "dprint2");

   function geteuid return uid_t;
   pragma Import (C, geteuid);

   function writable_access (path : IC.char_array) return IC.int;
   pragma Import (C, writable_access);

   function maximum_path_length return IC.size_t;
   pragma Import (C, maximum_path_length);

   function clookup_group (name : IC.char_array) return IC.unsigned;
   pragma Import (C, clookup_group);

   function clookup_user (name : IC.char_array) return IC.unsigned;
   pragma Import (C, clookup_user);

   function cuid_on_exec_bit_set (permissions : IC.short) return IC.short;
   pragma Import (C, cuid_on_exec_bit_set);

   function cgid_on_exec_bit_set (permissions : IC.short) return IC.short;
   pragma Import (C, cgid_on_exec_bit_set);

   function csticky_bit_set (permissions : IC.short) return IC.short;
   pragma Import (C, csticky_bit_set);

   function cformat_file_time (mtime_epoch : filetime;
                               buf : access IC.unsigned_char;
                               maxsize : IC.size_t) return IC.size_t;
   pragma Import (C, cformat_file_time);

   function set_metadata
     (path              : IC.char_array;
      reset_modtime     : IC.unsigned_char;
      reset_ownership   : IC.unsigned_char;
      reset_permissions : IC.unsigned_char;
      new_user_id       : IC.unsigned;
      new_group_id      : IC.unsigned;
      new_permissions   : IC.short;
      new_mtime_epoch   : time_t;
      new_mtime_nsecs   : IC.long) return IC.unsigned_char;
   pragma Import (C, set_metadata);

   function set_symlink_metadata
     (path              : IC.char_array;
      path_directory    : IC.char_array;
      symlink_filename  : IC.char_array;
      reset_modtime     : IC.unsigned_char;
      reset_ownership   : IC.unsigned_char;
      reset_permissions : IC.unsigned_char;
      new_user_id       : IC.unsigned;
      new_group_id      : IC.unsigned;
      new_permissions   : IC.short;
      new_mtime_epoch   : time_t;
      new_mtime_nsecs   : IC.long) return IC.unsigned_char;
   pragma Import (C, set_symlink_metadata);

   function set_fifo_metadata
     (path              : IC.char_array;
      reset_modtime     : IC.unsigned_char;
      reset_ownership   : IC.unsigned_char;
      reset_permissions : IC.unsigned_char;
      new_user_id       : IC.unsigned;
      new_group_id      : IC.unsigned;
      new_permissions   : IC.short;
      new_mtime_epoch   : time_t;
      new_mtime_nsecs   : IC.long) return IC.unsigned_char;
   pragma Import (C, set_fifo_metadata);

   function copen
     (path      : IC.Strings.chars_ptr;
      rdonly    : IC.int;
      wronly    : IC.int;
      append    : IC.int;
      nonblock  : IC.int;
      directory : IC.int;
      cloexec   : IC.int;
      creat     : IC.int;
      trunc     : IC.int) return IC.int;
   pragma Import (C, copen, "try_open");

   --  function C_Errno return IC.int;
   --  pragma Import (C, C_Errno, "get_errno");

   function stat_ok (path : String; sb : struct_stat_Access) return Boolean;

   function file_modification_time (sb : struct_stat_Access) return time_specification;

   function file_permissions (sb : struct_stat_Access) return permissions;

   function file_owner (sb : struct_stat_Access) return ownergroup;

   function file_group (sb : struct_stat_Access) return ownergroup;

   function file_size  (sb : struct_stat_Access) return exabytes;

   function type_of_file (sb : struct_stat_Access) return file_type;

   function inode_number (sb : struct_stat_Access) return inode_type;

   function int2str (A : Integer) return String;

   --  Returns true if the setuid on exec bit is set
   function uid_on_exec_bit_set (perms : permissions) return Boolean;

   --  Returns true if the setgid on exec bit is set
   function gid_on_exec_bit_set (perms : permissions) return Boolean;

   --  Returns true if the sticky bit is set
   function sticky_bit_set (perms : permissions) return Boolean;


end Archive.Unix;
