--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

package Archive is

   type info_level is (silent, normal, verbose, debug);
   type file_type is (directory, regular, symlink, hardlink, fifo, unsupported);

   subtype ownergroup is String (1 .. 32);

   type one_byte    is mod 2 ** 8;
   type permissions is mod 2 ** 16;
   type index_type  is mod 2 ** 16;
   type size_multi  is mod 2 ** 8;
   type size_modulo is mod 2 ** 32;
   type size_type   is mod 2 ** 40;
   type zstd_size   is mod 2 ** 32;
   type mdata_size  is mod 2 ** 24;
   type filetime    is mod 2 ** 64;
   type nanoseconds is mod 2 ** 32;
   type max_path    is mod 2 ** 16;
   type max_fname   is mod 2 ** 8;
   type inode_type  is mod 2 ** 64;
   type exabytes    is mod 2 ** 64;
   type file_index  is mod 2 ** 32;
   type owngrp_id   is mod 2 ** 32;
   type zpadding    is mod 2 ** 32;

   subtype A_filename is String (1 .. 256);
   subtype A_checksum is String (1 .. 32);
   subtype Some_magic is String (1 .. 3);

   type A_padding is array (1 .. 3) of one_byte;

   type File_Block is
      record
         blake_sum    : A_checksum;
         modified_sec : filetime;
         modified_ns  : nanoseconds;
         index_owner  : one_byte;
         index_group  : one_byte;
         type_of_file : file_type;
         multiplier   : size_multi;
         flat_size    : size_modulo;
         file_perms   : permissions;
         link_length  : max_path;
         index_parent : index_type;
         directory_id : index_type;
         fname_length : max_fname;
         padding      : A_padding;
      end record;

   for File_Block'Size use 512;
   for File_Block'Alignment use 8;
   for File_Block use
      record
         blake_sum    at  0 range  0 .. 255;
         modified_sec at 32 range  0 .. 63;
         modified_ns  at 40 range  0 .. 31;
         index_owner  at 44 range  0 .. 7;
         index_group  at 45 range  0 .. 7;
         type_of_file at 46 range  0 .. 7;
         multiplier   at 47 range  0 .. 7;
         flat_size    at 48 range  0 .. 31;
         file_perms   at 52 range  0 .. 15;
         link_length  at 54 range  0 .. 15;
         index_parent at 56 range  0 .. 15;
         directory_id at 58 range  0 .. 15;
         fname_length at 60 range  0 .. 7;
         padding      at 61 range  0 .. 23;
      end record;

   type premier_block is
      record
         magic_bytes     : Some_magic;
         version         : one_byte;
         num_groups      : index_type;
         num_owners      : index_type;
         link_blocks     : file_index;
         file_blocks     : file_index;
         size_metadata   : zstd_size;
         size_filedata   : zstd_size;
         size_archive    : zstd_size;
         fname_blocks    : file_index;
         flat_metadata   : zstd_size;
         flat_filedata   : zstd_size;
         flat_archive    : exabytes;
         unused1         : zpadding;
         unused2         : zpadding;
         unused3         : zpadding;
         unused4         : zpadding;
      end record;

   for premier_block'Size use 512;
   for premier_block'Alignment use 8;
   for premier_block use
      record
         magic_bytes     at  0 range  0 .. 23;
         version         at  3 range  0 .. 7;
         num_groups      at  4 range  0 .. 15;
         num_owners      at  6 range  0 .. 15;
         link_blocks     at  8 range  0 .. 31;
         file_blocks     at 12 range  0 .. 31;

         size_metadata   at 16 range  0 .. 31;
         size_filedata   at 20 range  0 .. 31;
         size_archive    at 24 range  0 .. 31;
         fname_blocks    at 28 range  0 .. 31;

         flat_metadata   at 32 range  0 .. 31;
         flat_filedata   at 36 range  0 .. 31;
         flat_archive    at 40 range  0 .. 63;

         unused1         at 48 range  0 .. 31;
         unused2         at 52 range  0 .. 31;
         unused3         at 56 range  0 .. 31;
         unused4         at 60 range  0 .. 31;
      end record;

   magic : constant Some_magic := Character'Val (200) & Character'Val (100) & Character'Val (50);
   KB256 : constant Natural := 262_144;
   KB512 : constant Natural := 524_288;
   format_version : constant one_byte := 3;
   null_owngrp : constant ownergroup := (others => Character'Val (0));

private

   --  Return owner or group, 9 characters long.  If owner or group is longer than 8 characters,
   --  only the first seven is displayed followed by "*" and a space
   function verbose_display_owngrp (owngrp : ownergroup) return String;

   --  Returns 9-character string representing file size, left aligned after leftmost space.
   --  Values over 100 million are have "M" suffix and values over 1 gigabyte use G suffix.
   function verbose_display_filesize (fsize : size_type) return String;

   --  Cut out trailing characters set to zero and return as a trimmed string
   function trim_trailing_zeros (full_string : String) return String;

end Archive;
