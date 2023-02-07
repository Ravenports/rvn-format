package Archive is

   type info_level is (silent, normal, verbose, debug);
   type file_type is (directory, regular, symlink, hardlink, fifo);

   subtype ownergroup is String (1 .. 32);

   type one_byte  is mod 2 ** 8;
   type bits_16   is mod 2 ** 16;
   type bits_40   is mod 2 ** 40;
   type filetime  is mod 2 ** 64;
   type max_path  is mod 2 ** 12;

end Archive;
