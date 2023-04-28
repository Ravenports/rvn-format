--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt


with Ada.Containers.Vectors;
with Ada.Streams.Stream_IO;

package Archive.Unpack is

   package CON renames Ada.Containers;
   package SIO renames Ada.Streams.Stream_IO;

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

   --  This procedure decompresses the file index, parses it, and stores it internally.
   procedure retrieve_file_index (DS : in out DArc);

private

   type owngrp_count is range 0 .. 2 ** 8 - 1;
   type File_Count is range 0 .. 2 ** 31 - 1;
   subtype A_Path is String (1 .. 1024);

   package owngrp_crate is new CON.Vectors
     (Index_Type   => owngrp_count,
      Element_Type => ownergroup,
      "="          => "=");

   package file_block_crate is new CON.Vectors
     (Index_Type   => File_Count,
      Element_Type => File_Block);

   package link_crate is new CON.Vectors
     (Index_Type   => Natural,
      Element_Type => Character);

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

end Archive.Unpack;
