--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Ada.Containers.Vectors;

package Archive.Pack is

   package CON renames Ada.Containers;

   --  Assemble contents of directory tree into a single file.
   --  Special files like character devices and block devices are excluded.

   procedure integrate
     (top_level_directory : String;
      output_file         : String;
      verbosity           : info_level);

private

   type owngrp_count is range 0 .. 2 ** 8 - 1;
   type File_Count is range 0 .. 2 ** 31 - 1;
   subtype A_Path is String (1 .. 1024);

   package owngrp_crate is new CON.Vectors
     (Index_Type   => owngrp_count,
      Element_Type => ownergroup,
      "="          => "=");

   null_sum : constant A_checksum := (others => Character'Val (0));

   package file_block_crate is new CON.Vectors
     (Index_Type   => File_Count,
      Element_Type => File_Block);

   type inode_record is
      record
         inode : inode_type;
         path  : A_Path;
         psize : Natural;
      end record;

   package inode_crate is new CON.Vectors
     (Index_Type   => Natural,
      Element_Type => inode_record);

   type Arc_Structure is tagged
      record
         owners : owngrp_crate.Vector;
         groups : owngrp_crate.Vector;
         files  : file_block_crate.Vector;
         inodes : inode_crate.Vector;
         level  : info_level := silent;
         dtrack : index_type := 0;
         ftrack : File_Count := 0;
         tlevel : A_Path;
         tlsize : Natural;
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
      dir_index : index_type);

   --  Keep track of the compression directory
   procedure record_directory (AS : in out Arc_Structure; top_directory : String);

   --  Retreive the compression directory
   function retrieve_directory (AS : Arc_Structure) return String;

   --  Return true if the inode of a hardlink has already been seen
   function inode_already_seen (AS : Arc_Structure; inode : inode_type) return Boolean;

   --  Returns path of inode relative to the top level directory
   function retrieve_inode_path (AS : Arc_Structure; inode : inode_type) return String;

   --  Insert record of a hardlink
   procedure insert_inode (AS : in out Arc_Structure; inode : inode_type; path : String);

end Archive.Pack;
