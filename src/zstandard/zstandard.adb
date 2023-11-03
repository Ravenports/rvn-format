--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Ada.Unchecked_Conversion;
with Ada.Directories;
with Ada.Direct_IO;
with Ada.IO_Exceptions;
with Zstandard.Streaming_Compression;

package body Zstandard is

   package DIR renames Ada.Directories;
   package IOX renames Ada.IO_Exceptions;

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
        ZSTD_getFrameContentSize
          (src     => src (src'First)'Access,
           srcSize => srcSize);

      dstCapacity : constant IC.size_t := IC.size_t (full_size);
   begin
      if full_size = ZSTD_CONTENTSIZE_UNKNOWN then
         successful := False;
         return "Error: Flat size cannot be determined.";
      elsif full_size = ZSTD_CONTENTSIZE_ERROR then
         successful := False;
         return "Error: invalid magic number or srcSize too small.";
      elsif full_size = 0 then
         successful := False;
         return "Error: size is valid, but it evaluates to zero which is unexpected.";
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


   ------------------
   --  Decompress  --
   ------------------
   function Decompress
     (archive_saxs : SIO.Stream_Access;
      data_length  : Natural;
      successful  : out Boolean) return String
   is
      type magazine_type is
         record
            data : String (1 .. data_length);
         end record;
      magazine : magazine_type;
   begin
      magazine_type'Read (archive_saxs, magazine);
      return Decompress (source_data => magazine.data,
                         successful  => successful);
   exception
      when others =>
         successful := False;
         return "Failed to read compressed chunk from archive";
   end Decompress;


   ---------------------
   --  File_Contents  --
   ---------------------
   function File_Contents (filename : String;
                           filesize : Natural;
                           nominal  : out Boolean) return String
   is
      subtype File_String    is String (1 .. filesize);
      package File_String_IO is new Ada.Direct_IO (File_String);
      File     : File_String_IO.File_Type;
      Contents : File_String;
   begin
      nominal := False;
      File_String_IO.Open (File => File,
                           Mode => File_String_IO.In_File,
                           Name => filename);
      File_String_IO.Read (File => File,
                           Item => Contents);
      File_String_IO.Close (File);
      nominal := True;
      return Contents;
   exception
      when others =>
         if File_String_IO.Is_Open (File) then
            File_String_IO.Close (File);
         end if;
         return "";
   end File_Contents;


   ----------------------------
   --  compress_into_memory  --
   ----------------------------

   function compress_into_memory
     (filename   : String;
      quality    : Compression_Level := Default_Compression;
      successful : out Boolean) return String
   is
      source_size : DIR.File_Size;
   begin
      successful := False;
      if not DIR.Exists (filename) then
         return Warn_src_file_DNE;
      end if;

      source_size := DIR.Size (filename);

      declare
         good_dump : Boolean;
         payload : constant String := File_Contents (filename => filename,
                                                     filesize => Natural (source_size),
                                                     nominal  => good_dump);
      begin
         if not good_dump then
            return Warn_src_read_fail;
         end if;

         declare
            good_compress : Boolean;
            compact : constant String := Compress (source_data => payload,
                                                   successful  => good_compress,
                                                   quality     => quality);
         begin
            if not good_compress then
               return Warn_compress_fail & " (" & compact & ")";
            end if;

            successful := True;
            return compact;
         end;
      end;
   end compress_into_memory;


   --------------------------------
   --   append_target_file  --
   --------------------------------

   procedure append_target_file
     (target_saxs : SIO.Stream_Access;
      compressed_text : String)
   is
      recsize : constant Natural := compressed_text'Length;
   begin
      declare
         type tray is record
            payload : String (1 .. recsize);
         end record;
         pragma Pack (tray);

         data : tray;
      begin
         data.payload := compressed_text;
         tray'Output (target_saxs, data);
      end;
   end append_target_file;


   --------------------------------
   --  Incorporate regular file  --
   --------------------------------

   procedure incorporate_regular_file
     (filename    : String;
      size        : File_Size;
      quality     : Compression_Level := Default_Compression;
      target_saxs : SIO.Stream_Access;
      target_file : SIO.File_Type;
      output_size : out File_Size;
      successful  : out Boolean)
   is
      max_one_pass_size : constant File_Size := 2 ** 18;  --  256 kb
   begin
      if size <= max_one_pass_size then
         declare
            compression_successful : Boolean;
            compressed_bytes : constant String :=
              compress_into_memory (filename   => filename,
                                    quality    => quality,
                                    successful => compression_successful);
         begin
            if not compression_successful then
               successful := False;
               output_size := 0;
               return;
            end if;
            append_target_file (target_saxs, compressed_bytes);
            output_size := compressed_bytes'Length;
            successful := True;
            return;
         end;
      end if;

      --  We've got to read this file in chunks
      declare
         src_file  : SIO.File_Type;
         mech      : Zstandard.Streaming_Compression.Compressor;
         chunksize : constant Ada.Streams.Stream_Element_Offset := 262_144;  --  256 kb
         chunk     : Ada.Streams.Stream_Element_Array (1 .. chunksize);
         Last      : Ada.Streams.Stream_Element_Offset;
         tare_size : SIO.Count;
         last_size : SIO.Count;

         use type SIO.Count;
      begin
         output_size := 0;
         tare_size := SIO.Size (target_file);
         begin
            SIO.Open (File => src_file,
                      Mode => SIO.In_File,
                      Name => filename);
         exception
            when IOX.Use_Error =>
               successful := False;
               return;
         end;

         mech.Initialize (output_stream => target_saxs,
                          quality       => quality);
         loop
            exit when SIO.End_Of_File (src_file);
            SIO.Read (File => src_file,
                      Item => chunk,
                      Last => Last);
            mech.Compress_Data (chunk (1 .. Last));
         end loop;
         mech.Finalize_Compression_Frame;
         SIO.Close (src_file);
         last_size := SIO.Size (target_file);
         output_size := File_Size (last_size - tare_size);
         successful := True;
      end;

   end incorporate_regular_file;


   --------------------------------
   --  assemble_regular_archive  --
   --------------------------------
   procedure assemble_regular_archive
     (filename    : String;
      file_size   : Natural;
      target_saxs : SIO.Stream_Access)
   is
      max_one_pass_size : constant Natural := 2 ** 18;  --  256 kb
   begin
      if file_size <= max_one_pass_size then
         if not DIR.Exists (filename) then
            append_target_file (target_saxs, Warn_src_file_DNE);
            return;
         end if;
         declare
            good_dump : Boolean;
            payload : constant String := File_Contents (filename => filename,
                                                        filesize => file_size,
                                                        nominal  => good_dump);
         begin
            append_target_file (target_saxs, payload);
         end;
         return;
      end if;


      --  We've got to read this file in chunks
      declare
         src_file  : SIO.File_Type;
         chunksize : constant Ada.Streams.Stream_Element_Offset := 262_144;  --  256 kb
         chunk     : Ada.Streams.Stream_Element_Array (1 .. chunksize);
         Last      : Ada.Streams.Stream_Element_Offset;

         procedure write_to_target (data : Ada.Streams.Stream_Element_Array);
         procedure write_to_target (data : Ada.Streams.Stream_Element_Array)
         is
            subtype data_out is String (1 .. data'Length);

            function stream_to_string is new Ada.Unchecked_Conversion
              (Source => Ada.Streams.Stream_Element_Array,
               Target => data_out);
         begin
            append_target_file (target_saxs, stream_to_string (data));
         end write_to_target;
      begin
         begin
            SIO.Open (File => src_file,
                      Mode => SIO.In_File,
                      Name => filename);
         exception
            when IOX.Use_Error =>
               return;
         end;

         loop
            exit when SIO.End_Of_File (src_file);
            SIO.Read (File => src_file,
                      Item => chunk,
                      Last => Last);
            write_to_target (data => chunk (1 .. Last));
         end loop;
         SIO.Close (src_file);
      end;

   end assemble_regular_archive;


   --------------------------------
   --  Natural_DStreamOutSize  --
   --------------------------------
   function Natural_DStreamOutSize return Natural is
   begin
      return Natural (ZSTD_DStreamOutSize);
   end Natural_DStreamOutSize;


   --------------------------------
   --  Natural_DStreamInSize  --
   --------------------------------
   function Natural_DStreamInSize return Natural is
   begin
      return Natural (ZSTD_DStreamInSize);
   end Natural_DStreamInSize;


end Zstandard;
