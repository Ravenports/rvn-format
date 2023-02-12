--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Ada.Unchecked_Conversion;

package body Zstandard is

   ----------------
   --  Compress  --
   ----------------
   function Compress
     (source_data : String;
      successful  : out Boolean;
      quality     : Compression_Level := Default_Compression) return String
   is
      type source_cdata is array (source_data'Range) of aliased IC.unsigned_char;
      function string_to_cdata is new Ada.Unchecked_Conversion (Source => String,
                                                                Target => source_cdata);

      comp_bytes  : IC.size_t;
      is_error    : IC.unsigned;
      level       : constant IC.int := IC.int (quality);

      src         : source_cdata := string_to_cdata (source_data);
      srcSize     : constant IC.size_t := IC.size_t (source_data'Length);
      dstCapacity : constant IC.size_t := ZSTD_compressBound (srcSize);
   begin
      declare
         type cdestination is array (1 .. dstCapacity) of aliased IC.unsigned_char;
         dst : cdestination;
      begin
         comp_bytes := ZSTD_compress (dst              => dst (dst'First)'Access,
                                      dstCapacity      => dstCapacity,
                                      src              => src (src'First)'Access,
                                      srcSize          => srcSize,
                                      compressionLevel => level);

         is_error := ZSTD_isError (code => comp_bytes);
         successful := (Natural (is_error) = 0);
         if successful then
            declare
               subtype adestination is String (1 .. Natural (comp_bytes));
               function dest_to_string is new Ada.Unchecked_Conversion (Source => cdestination,
                                                                        Target => adestination);
            begin
               return dest_to_string (dst);
            end;
         else
            return IC.Strings.Value (ZSTD_getErrorName (code => comp_bytes));
         end if;
      end;
   end Compress;


   ------------------
   --  Decompress  --
   ------------------
   function Decompress
     (source_data : String;
      successful  : out Boolean) return String
   is
      type source_cdata is array (source_data'Range) of aliased IC.unsigned_char;
      function string_to_cdata is new Ada.Unchecked_Conversion (Source => String,
                                                                Target => source_cdata);

      src         : source_cdata := string_to_cdata (source_data);
      srcSize     : constant IC.size_t := IC.size_t (source_data'Length);

      full_size   : constant Zstd_uint64 :=
        ZSTD_getDecompressedSize
          (src     => src (src'First)'Access,
           srcSize => srcSize);

      dstCapacity : constant IC.size_t := IC.size_t (full_size);
   begin
      if full_size = 0 then
         successful := False;
         return Warn_orig_size_fail;
      end if;

      declare
         type cdestination is array (1 .. dstCapacity) of aliased IC.unsigned_char;
         dcmp_bytes : IC.size_t;
         dst        : cdestination;

         use type IC.size_t;
      begin
         dcmp_bytes := ZSTD_decompress (dst            => dst (dst'First)'Access,
                                        dstCapacity    => dstCapacity,
                                        src            => src (src'First)'Access,
                                        compressedSize => srcSize);
         successful := (dcmp_bytes = dstCapacity);
         if successful then
            declare
               subtype adestination is String (1 .. Natural (dcmp_bytes));
               function dest_to_string is new Ada.Unchecked_Conversion (Source => cdestination,
                                                                        Target => adestination);
            begin
               return dest_to_string (dst);
            end;
         else
            return IC.Strings.Value (ZSTD_getErrorName (code => dcmp_bytes));
         end if;
      end;
   end Decompress;

end Zstandard;
