--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

--  with Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Streams.Stream_IO;
with Ada.Strings.Unbounded;
with Zstandard.Streaming_Decompression;

package Archive.JustExtract is

   package CON renames Ada.Containers;
   package SIO renames Ada.Streams.Stream_IO;
   package ASU renames Ada.Strings.Unbounded;

   type DArc is tagged limited private;

   --  This procedure attempts to open an RVN archive.
   procedure open_rvn_archive
     (DS            : in out DArc;
      rvn_archive   : String);

   --  This procedure attempts to close an RVN archive
   procedure close_rvn_archive (DS : in out DArc);

   --  This function decompresses the metadata and returns it as a string
   function extract_metadata (DS : in out DArc) return String;

private

   type owngrp_lookup is (id_unset, id_valid, id_unknown);
   type owngrp_count is range 0 .. 2 ** 8 - 1;
   type File_Count is range 0 .. 2 ** 31 - 1;
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
         fname_length : max_fname;
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
         name_blocks : Natural;
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
         rolled_up  : Boolean := True;
         call_again : Boolean := False;
         link_index : Natural := 0;
         buffer     : text;
         buf_arrow  : Natural := 0;
         buf_remain : Natural := 0;
         expander   : Zstandard.Streaming_Decompression.Decompressor;
      end record;

end Archive.JustExtract;
