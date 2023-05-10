--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Interfaces.C.Strings;

package Archive.Unix is

   package IC renames Interfaces.C;

   type File_Characteristics is
      record
         ftype : file_type;
         owner : ownergroup;
         group : ownergroup;
         mtime : filetime;
         perms : permissions;
         inode : inode_type;
         error : Boolean;
      end record;

   --  Return set of file characteristis given the path to a file or a directory
   function get_charactistics (path : String) return File_Characteristics;

   --  Return symlink target path
   function link_target (symlink_path : String) return String;

   --  Attempts to create a hardlink and returns True on success
   function create_hardlink (actual_file : String; destination : String) return Boolean;

   --  Attempts to create a symlink and returns True on success
   function create_symlink (actual_file : String; link_to_create : String) return Boolean;

   --  Attempts to create FIFO file and returns True on success
   function create_fifo (fifo_path : String; perms : permissions) return Boolean;

private

   function success (rc : IC.int) return Boolean;
   function failure (rc : IC.int) return Boolean;

   type stat_block is array (1 .. 256) of IC.unsigned_char;
   type struct_stat is limited
      record
         --  sizeof(struct stat) is 128 on DragonFly
         --  Double that to ensure we allocate enough
         block : stat_block;
      end record;

   type struct_stat_Access is access all struct_stat;
   pragma Convention (C, struct_stat_Access);

   function arc_stat
     (path : IC.Strings.chars_ptr;
      sb   : struct_stat_Access) return IC.int;
   pragma Import (C, arc_stat, "lstat");

   function arc_readlink
     (path   : IC.Strings.chars_ptr;
      buf    : access IC.unsigned_char;
      bufsiz : IC.size_t) return IC.long;
   pragma Import (C, arc_readlink, "readlink");

   function arc_get_mtime (sb : struct_stat_Access) return IC.long;
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

   function symlink (path1, path2 : IC.char_array) return IC.int;
   pragma Import (C, symlink);

   function link (path1, path2 : IC.char_array) return IC.int;
   pragma Import (C, link);

   function mkfifo (path : IC.char_array; mode : IC.unsigned_short) return IC.int;
   pragma Import (C, mkfifo);

   function stat_ok (path : String; sb : struct_stat_Access) return Boolean;

   function file_modification_time (sb : struct_stat_Access) return filetime;

   function file_permissions (sb : struct_stat_Access) return permissions;

   function file_owner (sb : struct_stat_Access) return ownergroup;

   function file_group (sb : struct_stat_Access) return ownergroup;

   function type_of_file (sb : struct_stat_Access) return file_type;

   function inode_number (sb : struct_stat_Access) return inode_type;

   function str2owngrp (name : String) return ownergroup;

   function int2str (A : Integer) return String;


end Archive.Unix;
