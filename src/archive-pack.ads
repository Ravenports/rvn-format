--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Ada.Containers.Vectors;
--  with Ada.Containers.Hashed_Maps;

package Archive.Pack is

   package CON renames Ada.Containers;

   --  Assemble contents of directory tree into a single file.
   --  Special files like character devices and block devices are excluded.

   procedure integrate
     (top_level_directory : String;
      output_file         : String;
      verbosity           : info_level);

private

   subtype ownergroup is String (1 .. 32);

   type owngrp_count is range 0 .. 2 ** 8 - 1;
   type File_Count is range 0 .. 2 ** 32 - 1;

   package owngrp_crate is new CON.Vectors
     (Index_Type   => owngrp_count,
      Element_Type => ownergroup,
      "="          => "=");

   type one_byte  is mod 2 ** 8;
   type bits_16   is mod 2 ** 16;
   type bits_40   is mod 2 ** 40;
   type filetime  is mod 2 ** 64;
   type max_path  is mod 2 ** 12;

   subtype A_filename is String (1 .. 256);
   subtype A_checksum is String (1 .. 32);

   type File_Block is
      record
         filename     : A_filename;
         blake_sum    : A_checksum;
         index_owner  : one_byte;
         index_group  : one_byte;
         type_of_file : file_type;
         permissions  : bits_16;
         flat_size    : bits_40;
         link_length  : max_path;
         modified     : filetime;
         index_parent : bits_16;
      end record;

   package file_block_crate is new CON.Vectors
     (Index_Type   => File_Count,
      Element_Type => File_Block);

   type Arc_Structure is tagged
      record
         owners : owngrp_crate.Vector;
         groups : owngrp_crate.Vector;
         files  : file_block_crate.Vector;
         level  : info_level := silent;
         dtrack : bits_16    := 0;
      end record;

   --  Given the string representation of the owner, return the index on the list.
   --  If the owner is unrecognized, add it to the list and return the last index.
   function get_owner_index (AS : in out Arc_Structure; owner : ownergroup) return one_byte;

   --  Given the string representation of the group, return the index on the list.
   --  If the group is unrecognized, add it to the list and return the last index.
   function get_group_index (AS : in out Arc_Structure; group : ownergroup) return one_byte;

   --  Sets the standard out information level
   procedure set_verbosity (AS : in out Arc_Structure; level : info_level);

   --  Prints message to standard out if the display level is high enough
   procedure print (AS : Arc_Structure; msg_level : info_level; message : String);

   --  Recursive scan, initiated with passing path to stage directory with "0" index
   procedure scan_directory
     (AS        : in out Arc_Structure;
      dir_path  : String;
      dir_index : bits_16);

end Archive.Pack;
