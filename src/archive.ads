package Archive is

   type info_level is (silent, normal, verbose, debug);
   type file_type is (directory, regular, symlink, hardlink, fifo, unsupported);

   subtype ownergroup is String (1 .. 32);

   type one_byte    is mod 2 ** 8;
   type permissions is mod 2 ** 16;
   type index_type  is mod 2 ** 16;
   type size_type   is mod 2 ** 40;
   type filetime    is mod 2 ** 64;
   type max_path    is mod 2 ** 16;

   subtype A_filename is String (1 .. 256);
   subtype A_fragment is String (1 .. 64);
   subtype A_checksum is String (1 .. 32);
   subtype A_padding  is String (1 .. 10);

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
         file_perms   at 304 range  0 .. 15;
         link_length  at 304 range 16 .. 31;
         index_parent at 304 range 32 .. 47;
         padding      at 304 range 48 .. 127;
      end record;

end Archive;
