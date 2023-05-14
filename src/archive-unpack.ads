--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt


with Ada.Containers.Vectors;
with Ada.Streams.Stream_IO;
with Ada.Strings.Unbounded;

package Archive.Unpack is

   package CON renames Ada.Containers;
   package SIO renames Ada.Streams.Stream_IO;
   package ASU renames Ada.Strings.Unbounded;

   type DArc is tagged limited private;

   --  This procedure attempts to open an RVN archive.
   procedure open_rvn_archive
     (DS          : in out DArc;
      rvn_archive : String;
      verbosity   : info_level);

   --  This procedure attempts to close an RVN archive
   procedure close_rvn_archive (DS : in out DArc);

   --  This function indicates if the archive was successfully opened (and is still open).
   function rvn_archive_is_open (DS : DArc) return Boolean;

   --  This procedure decompresses the metadata and writes the output to the requested file.
   procedure write_metadata_to_file
     (DS       : in out DArc;
      filepath : String);

   --  This function decompresses the metadata and returns it as a string
   function extract_metadata (DS : in out DArc) return String;

   --  This procedure sends the file index to standard out.  Directories are omitted.
   --  The print order matches the order written to the archive.
   --  If 'show_b3sum' is true, each line is prefixed with the blake3 checksum (hex).
   procedure print_manifest (DS : in out DArc; show_b3sum : Boolean := False);

   --  This function extracts the archive and returns true if successful.
   function extract_archive
     (DS            : in out DArc;
      top_directory : String;
      set_owners    : Boolean;
      set_perms     : Boolean;
      set_modtime   : Boolean) return Boolean;

private

   fblk_size    : constant Natural := File_Block'Size / 8;
   id_not_found : constant owngrp_id := 4_000_000_000;

   type owngrp_lookup is (id_unset, id_valid, id_unknown);
   type owngrp_count is range 0 .. 2 ** 8 - 1;
   type File_Count is range 0 .. 2 ** 31 - 1;
   subtype A_Path is String (1 .. 1024);
   subtype FBString is String (1 .. fblk_size);
   subtype text is ASU.Unbounded_String;

   type Scanned_File_Block is
      record
         filename     : A_filename;
         blake_sum    : A_checksum;
         modified_sec : filetime;
         modified_ns  : nanoseconds;
         index_owner  : owngrp_count;
         index_group  : owngrp_count;
         type_of_file : file_type;
         file_size_tb : size_type;
         file_perms   : permissions;
         link_length  : max_path;
         index_parent : index_type;
         directory_id : index_type;
      end record;

   type A_Directory is
      record
         directory : text;
      end record;

   type ownergroup_info is
      record
         name   : ownergroup;
         id     : owngrp_id;
         status : owngrp_lookup;
      end record;

   package owngrp_crate is new CON.Vectors
     (Index_Type   => owngrp_count,
      Element_Type => ownergroup_info,
      "="          => "=");

   package file_block_crate is new CON.Vectors
     (Index_Type   => File_Count,
      Element_Type => Scanned_File_Block);

   package link_crate is new CON.Vectors
     (Index_Type   => Natural,
      Element_Type => Character);

   package directory_crate is new CON.Vectors
     (Index_Type   => Positive,
      Element_Type => A_Directory);

   type consumer_items is
      record
         num_groups  : Natural;
         num_owners  : Natural;
         link_blocks : Natural;
         file_blocks : Natural;
      end record;

   type DArc is tagged limited
      record
         header     : premier_block;
         valid      : Boolean;
         level      : info_level := silent;
         rvn_handle : SIO.File_Type;
         rvn_stmaxs : SIO.Stream_Access;
         b2_index   : SIO.Count;
         b3_index   : SIO.Count;
         b4_index   : SIO.Count;
         con_track  : consumer_items;
         owners     : owngrp_crate.Vector;
         groups     : owngrp_crate.Vector;
         links      : link_crate.Vector;
         files      : file_block_crate.Vector;
         folders    : directory_crate.Vector;
         processed  : Boolean := False;
         link_index : Natural := 0;
      end record;

   --  Prints message to standard out if the display level is high enough
   procedure print (DS : DArc; msg_level : info_level; message : String);

   --  Sets the standard out information level
   procedure set_verbosity (DS : in out DArc; level : info_level);

   --  Creates a file with contents in a single write
   procedure direct_file_creation
     (DS          : DArc;
      target_file : String;
      contents    : String);

   --  This function converts the contiguous index_data string into the datatypes
   --  it contains.  The returned result is how many characters are left over and need
   --  to prefix the next extraction of the index data.
   function consume_index (DS : in out DArc; index_data : String) return Natural;

   --  This function converts the 320-byte string into a File_Block structure
   function FBString_to_File_Block (Source : FBString) return Scanned_File_Block;

   --  Cut out trailing characters set to zero and return as a trimmed string
   function trim_trailing_zeros (full_string : String) return String;

   --  This procedure decompresses the file index, parses it, and stores it internally.
   procedure retrieve_file_index (DS : in out DArc);

   --  This function pops the next link out of the vectory, but it has to be
   --  given the length of the string first
   function retrieve_link_target (DS : in out DArc; link_len : max_path) return String;

   --  This function filters out permissions beyond read-write-execute for root, user,
   --  and other.  This is includes the SUID.
   function rwx_filter (perms : permissions) return permissions;

end Archive.Unpack;
