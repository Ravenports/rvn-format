--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Streams.Stream_IO;
with Archive.Whitelist;
with Archive.Unix;
with Zstandard;
with ThickUCL;
with Elf;

package Archive.Pack is

   package CON renames Ada.Containers;
   package SIO renames Ada.Streams.Stream_IO;

   --  Assemble contents of directory tree into a single file.
   --  Special files like character devices and block devices are excluded.
   --  Set fixed_timestamp to 0 to disable timestamp override

   function integrate
     (top_level_directory : String;
      metadata_file       : String;
      manifest_file       : String;
      prefix              : String;
      abi                 : String;
      keyword_dir         : String;
      output_file         : String;
      fixed_timestamp     : filetime;
      verbosity           : info_level;
      record_base_libs    : Boolean;
      integrate_log       : Ada.Text_IO.File_Type;
      optional_pipe       : Unix.File_Descriptor := Unix.not_connected)
      return Boolean;

private

   type owngrp_count is range 0 .. 2 ** 8 - 1;
   type File_Count is range 0 .. 2 ** 31 - 1;
   subtype A_Path is String (1 .. 1024);
   subtype A_Filename is String (1 .. 256);

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

   package libraries_crate is new CON.Vectors
     (Index_Type   => Natural,
      Element_Type => A_Filename);

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
         flat_meta  : mdata_size := 0;
         flat_ndx   : zstd_size := 0;
         flat_arc   : size_type := 0;
         white_list : Whitelist.A_Whitelist;
         serror     : Boolean := False;
         lib_prov   : libraries_crate.Vector;
         lib_need   : libraries_crate.Vector;
         lib_adj    : libraries_crate.Vector;
         not_found  : libraries_crate.Vector;
         need_seen  : libraries_crate.Vector;
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
   --  If timestamp is greater than zero, modification time of every file is set to it.
   procedure scan_directory
     (AS        : in out Arc_Structure;
      dir_path  : String;
      dir_index : index_type;
      timestamp : filetime;
      adjacent  : Boolean;
      record_base_libs : Boolean);

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
   procedure write_metadata_block
     (AS : in out Arc_Structure;
      output_file_path : String;
      metadata_path    : String;
      tree             : in out ThickUCL.UclTree);

   --  Write block 3 (the compressed concatentation of blocks FA .. FE)
   procedure write_file_index_block (AS : in out Arc_Structure; output_file_path : String);

   --  Write block 4 (the compressed single archive)
   procedure write_archive_block (AS : in out Arc_Structure; output_file_path : String);

   --  Returns false if the part of output_file_path is not writable
   function able_to_write_rvn_archive
     (AS : Arc_Structure;
      output_file_path : String) return Boolean;

   --  Read the metadata file into a UCL tree.
   --  Set the prefix from the command line if the metadata file doesn't contain it.
   --  Set the ABI to the given value (unless it is blank) if the metadata file doesn't
   --  already contain it, as it should.
   procedure scan_metadata_file
     (AS : in out Arc_Structure;
      metadata_path    : String;
      prefix           : String;
      abi              : String;
      tree             : in out ThickUCL.UclTree);

   --  Provides a filter for which DT_NEEDED entries should be recorded in database
   --  If record_base_libs is true, return True immediately (filter nothing)
   --  Otherwise check the known standard library paths (system-specific).  If the
   --  library is present on one of them, return False.
   --  If the library is not found on any known path, return true
   function record_library
     (library_file     : String;
      record_base_libs : Boolean;
      file_format      : Elf.elf_class) return Boolean;

   --  Returns true if library is found on the given runpath concatenation
   function found_on_runpath
     (library_file     : String;
      runpath          : String;
      origin_directory : String;
      stage_directory  : String;
      original_file    : String) return Boolean;

   --  We have a list of libraries that couldn't be found installed.
   --  So of them are installed by this archive.
   --  Others are installed by sister subpackages.
   --  If the missing library is present in the "provided" container, or if it's present
   --  in the "adjacent" container, ignore.  Otherwise emit a notice.
   procedure analyze_missing_required_libraries
     (AS : in out Arc_Structure);

end Archive.Pack;
