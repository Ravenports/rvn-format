package Archive is

   type info_level is (silent, normal, verbose, debug);
   type file_type is (directory, regular, symlink, hardlink, fifo, unsupported);

   subtype ownergroup is String (1 .. 32);

   type one_byte    is mod 2 ** 8;
   type permissions is mod 2 ** 16;
   type index_type  is mod 2 ** 16;
   type size_type   is mod 2 ** 40;
   type zstd_size   is mod 2 ** 32;
   type filetime    is mod 2 ** 64;
   type max_path    is mod 2 ** 16;
   type inode_type  is mod 2 ** 64;
   type file_index  is mod 2 ** 32;
   type mfest_size  is mod 2 ** 24;

   subtype A_filename is String (1 .. 256);
   subtype A_fragment is String (1 .. 64);
   subtype A_checksum is String (1 .. 32);
   subtype A_padding  is String (1 .. 6);
   subtype Some_magic is String (1 .. 3);
   subtype B_padding  is String (1 .. 11);

   type File_Block is
      record
         filename_p1  : A_fragment;
         filename_p2  : A_fragment;
         filename_p3  : A_fragment;
         filename_p4  : A_fragment;
         blake_sum    : A_checksum;
         modified     : filetime;
         index_owner  : one_byte;
         index_group  : one_byte;
         type_of_file : file_type;
         flat_size    : size_type;
         compressed   : zstd_size;
         file_perms   : permissions;
         link_length  : max_path;
         index_parent : index_type;
         padding      : A_padding;
      end record;

   for File_Block'Size use 2560;
   for File_Block'Alignment use 8;
   for File_Block use
      record
         filename_p1  at   0 range  0 .. 511;
         filename_p2  at  64 range  0 .. 511;
         filename_p3  at 128 range  0 .. 511;
         filename_p4  at 192 range  0 .. 511;
         blake_sum    at 256 range  0 .. 255;
         modified     at 288 range  0 .. 63;
         index_owner  at 296 range  0 .. 7;
         index_group  at 296 range  8 .. 15;
         type_of_file at 296 range 16 .. 23;
         flat_size    at 296 range 24 .. 63;
         compressed   at 304 range  0 .. 31;
         file_perms   at 308 range  0 .. 15;
         link_length  at 310 range  0 .. 15;
         index_parent at 312 range  0 .. 15;
         padding      at 314 range  0 .. 47;
      end record;

   type premier_block is
      record
         magic_bytes     : Some_magic;
         version         : one_byte;
         num_groups      : index_type;
         num_owners      : index_type;
         link_blocks     : file_index;
         file_blocks     : file_index;
         manifest_blocks : index_type;
         manifest_size   : mfest_size;
         padding         : B_padding;
      end record;

   for premier_block'Size use 256;
   for premier_block'Alignment use 8;
   for premier_block use
      record
         magic_bytes     at  0 range  0 .. 23;
         version         at  3 range  0 .. 7;
         num_groups      at  4 range  0 .. 15;
         num_owners      at  6 range  0 .. 15;
         link_blocks     at  8 range  0 .. 31;
         file_blocks     at 12 range  0 .. 31;
         manifest_blocks at 16 range  0 .. 15;
         manifest_size   at 18 range  0 .. 23;
         padding         at 21 range  0 .. 87;
      end record;

   magic : constant Some_magic := Character'Val (200) & Character'Val (100) & Character'Val (50);
   format_version : constant one_byte := 1;

end Archive;
