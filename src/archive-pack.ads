--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt


with Ada.Containers.Vectors;
with Ada.Streams.Stream_IO;
with Archive.Whitelist;
with Zstandard;

package Archive.Pack is

   package CON renames Ada.Containers;
   package SIO renames Ada.Streams.Stream_IO;

   --  Assemble contents of directory tree into a single file.
   --  Special files like character devices and block devices are excluded.

   function integrate
     (top_level_directory : String;
      metadata_file       : String;
      manifest_file       : String;
      output_file         : String;
      verbosity           : info_level) return Boolean;

private

   type owngrp_count is range 0 .. 2 ** 8 - 1;
   type File_Count is range 0 .. 2 ** 31 - 1;
   subtype A_Path is String (1 .. 1024);

   package owngrp_crate is new CON.Vectors
     (Index_Type   => owngrp_count,
      Element_Type => ownergroup,
      "="          => "=");

   null_sum : constant A_checksum := (others => Character'Val (0));
   rvn_compression_level : constant Zstandard.Compression_Level := 9;

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

   package link_crate is new CON.Vectors
     (Index_Type   => Natural,
      Element_Type => Character);

   package filename_crate is new CON.Vectors
     (Index_Type   => Natural,
      Element_Type => Character);

   type Arc_Structure is tagged limited
      record
         owners : owngrp_crate.Vector;
         groups : owngrp_crate.Vector;
         files  : file_block_crate.Vector;
         inodes : inode_crate.Vector;
         fnames : filename_crate.Vector;
         links  : link_crate.Vector;
         level  : info_level := silent;
         dtrack : index_type := 0;
         ftrack : File_Count := 0;
         tlsize : Natural    := 0;
         tlevel : A_Path;
         rvn_handle : SIO.File_Type;
         rvn_stmaxs : SIO.Stream_Access;
         tmp_handle : SIO.File_Type;
         tmp_stmaxs : SIO.Stream_Access;
         ndx_handle : SIO.File_Type;
         ndx_stmaxs : SIO.Stream_Access;
         tmp_size   : zstd_size := 0;
         ndx_size   : zstd_size := 0;
         meta_size  : zstd_size := 0;
         white_list : Whitelist.A_Whitelist;
         serror     : Boolean := False;
      end record;

   --  Given the string representation of the owner, return the index on the list.
   --  If the owner is unrecognized, add it to the list and return the last index.
   function get_owner_index (AS : in out Arc_Structure; owner : ownergroup) return one_byte;

   --  Given the string representation of the group, return the index on the list.
   --  If the group is unrecognized, add it to the list and return the last index.
   function get_group_index (AS : in out Arc_Structure; group : ownergroup) return one_byte;

   --  Sets the standard out information level
   procedure set_verbosity (AS : in out Arc_Structure; level : info_level);

   --  Opens the temporary archive file and stores the handle and stream access
   procedure initialize_archive_file (AS : in out Arc_Structure; output_file_path : String);

   --  Closes the temporary archive file
   procedure finalize_archive_file (AS : in out Arc_Structure);

   --  Unlinks the termporary archive file
   procedure remove_archive_file (AS : Arc_Structure; output_file_path : String);

   --  Opens the temporary index file and stores the handle and stream access
   procedure initialize_index_file (AS : in out Arc_Structure; output_file_path : String);

      --  Closes the temporary index file
   procedure finalize_index_file (AS : in out Arc_Structure);

   --  Unlinks the termporary index file
   procedure remove_index_file (AS : Arc_Structure; output_file_path : String);

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

   --  Push a link on top of the link block
   procedure push_link (AS : in out Arc_Structure; link : String);

   --  Push a filename on top of the fname block
   procedure push_filename (AS : in out Arc_Structure; simple_name : String);

   --  Create the output file stream and write a blank premier block to it
   procedure write_blank_header (AS : in out Arc_Structure; output_file_path : String);

   --  Close archive file, and re-write the premier header with the final information
   procedure overwrite_header (AS : in out Arc_Structure; output_file_path : String);

   --  Write blocks FA and FB (groups and owners) to temporary file
   procedure write_owngrp_blocks (AS : Arc_Structure);

   --  Write block FC (contiguous strings of links) to temporary file
   procedure write_link_block (AS : Arc_Structure);

   --  Write block FD (All the file header blocks) to temporary file
   procedure write_file_index_block (AS : Arc_Structure);

   --  Write block FE (contiguous strings of filenames) to temporary file
   procedure write_filename_block (AS : Arc_Structure);

   --  Compress and insert given metadata file (block 2).  If the file does not exist or if
   --  an error occurs, 0 will be set for metadata (meaning it's not provided).
   procedure write_metadata_block (AS : in out Arc_Structure; metadata_path : String);

   --  Write block 3 (the compressed concatentation of blocks FA .. FE)
   procedure write_file_index_block (AS : in out Arc_Structure; output_file_path : String);

   --  Write block 4 (the compressed single archive)
   procedure write_archive_block (AS : in out Arc_Structure; output_file_path : String);

   --  Cut out trailing characters set to zero and return as a trimmed string
   function trim_trailing_zeros (full_string : String) return String;

   --  Returns false if the part of output_file_path is not writable
   function able_to_write_rvn_archive
     (AS : Arc_Structure;
      output_file_path : String) return Boolean;

   --  Return owner or group, 9 characters long.  If owner or group is longer than 8 characters,
   --  only the first seven is displayed followed by "*" and a space
   function verbose_display_owngrp (owngrp : ownergroup) return String;

   --  Returns 9-character string representing file size, left aligned after leftmost space.
   --  Values over 100 million are have "M" suffix and values over 1 gigabyte use G suffix.
   function verbose_display_filesize (fsize : size_type) return String;

end Archive.Pack;
