--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

private with System;
private with Interfaces.C.Strings;

package Zstandard is

   type Compression_Level is range 1 .. 22;
   type File_Size         is mod 2 ** 64;

   -----------------
   --  Constants  --
   -----------------

   Fastest_Compression : constant Compression_Level := Compression_Level'First;
   Highest_Compression : constant Compression_Level := Compression_Level'Last;
   Default_Compression : constant Compression_Level := 3;


   ------------------
   -- Compression  --
   ------------------

   --  This function returns the compressed version of "source_data".  Should the operation fail,
   --  "successful" variable will be set to False and the resulting string will contain the
   --  related error message.
   function Compress
     (source_data : String;
      successful  : out Boolean;
      quality     : Compression_Level := Default_Compression) return String;


   --------------------
   -- Decompression  --
   --------------------

   --  This function returns the decompressed version of "source_data".  Should the operation fail,
   --  "successful" variable will be set to False and the resulting string will contain the
   --  related error message.
   function Decompress
     (source_data : String;
      successful  : out Boolean) return String;

private

   package IC  renames Interfaces.C;

   ------------------
   --  Data Types  --
   ------------------

   type Zstd_uint64 is mod 2 ** 64;
   type Zstd_uint32 is mod 2 ** 32;


   ---------------
   --  Version  --
   ---------------

   function ZSTD_versionNumber return IC.unsigned;
   pragma Import (C, ZSTD_versionNumber, "ZSTD_versionNumber");

   ------------------------
   --  Helper functions  --
   ------------------------

   --  tells if a `size_t` function result is an error code
   function ZSTD_isError (code : IC.size_t) return IC.unsigned;
   pragma Import (C, ZSTD_isError, "ZSTD_isError");

   --  provides readable string for an error code
   function ZSTD_getErrorName (code : IC.size_t) return IC.Strings.chars_ptr;
   pragma Import (C, ZSTD_getErrorName, "ZSTD_getErrorName");

   --  maximum compressed size (worst case scenario)
   function ZSTD_compressBound (srcSize : IC.size_t) return IC.size_t;
   pragma Import (C, ZSTD_compressBound, "ZSTD_compressBound");

   --  maximum compression level available
   function ZSTD_maxCLevel return IC.int;
   pragma Import (C, ZSTD_maxCLevel, "ZSTD_maxCLevel");

   ------------------------
   --  Simple functions  --
   ------------------------

   --  Compresses `src` buffer into already allocated `dst`.
   --  Hint : compression runs faster if `dstCapacity` >=  `ZSTD_compressBound(srcSize)`.
   --  @return : the number of bytes written into `dst` (<= `dstCapacity),
   --            or an error code if it fails (which can be tested using ZSTD_isError())
   function ZSTD_compress
     (dst : access IC.unsigned_char; dstCapacity : IC.size_t;
      src : access IC.unsigned_char; srcSize     : IC.size_t;
      compressionLevel : IC.int) return IC.size_t;
   pragma Import (C, ZSTD_compress, "ZSTD_compress");

   --  @return : decompressed size as a 64-bits value _if known_, 0 otherwise.
   --   note 1 : decompressed size can be very large (64-bits value),
   --            potentially larger than what local system can handle as a single memory segment.
   --            In which case, it's necessary to use streaming mode to decompress data.
   --   note 2 : decompressed size is an optional field, that may not be present.
   --            When `return==0`, consider data to decompress could have any size.
   --            In which case, it's necessary to use streaming mode to decompress data,
   --            or rely on application's implied limits.
   --            (e.g., it may know that its own data is necessarily cut into blocks <= 16 KB).
   --   note 3 : decompressed size could be wrong or intentionally modified !
   --            Always ensure result fits within application's authorized limits !
   --            Each application can have its own set of conditions.
   --            If the intention is to decompress public data compressed by zstd command line
   --            utility, it is recommended to support at least 8 MB for extended compatibility.
   --   note 4 : when `return==0`, if precise failure cause is needed, use ZSTD_getFrameParams()
   --            to know more.
   function ZSTD_getDecompressedSize
     (src     : access IC.unsigned_char;
      srcSize : IC.size_t) return Zstd_uint64;
   pragma Import (C, ZSTD_getDecompressedSize, "ZSTD_getDecompressedSize");

   --  `compressedSize` : is the _exact_ size of compressed input, else decompression will fail.
   --  `dstCapacity` must be equal or larger than originalSize (see ZSTD_getDecompressedSize() ).
   --  If originalSize is unknown, and if there is no implied application-specific limitations,
   --  it's necessary to use streaming mode to decompress data.
   --  @return : the number of bytes decompressed into `dst` (<= `dstCapacity`),
   --            or an errorCode if it fails (which can be tested using ZSTD_isError())
   function ZSTD_decompress
     (dst : access IC.unsigned_char; dstCapacity    : IC.size_t;
      src : access IC.unsigned_char; compressedSize : IC.size_t) return IC.size_t;
   pragma Import (C, ZSTD_decompress, "ZSTD_decompress");


   ---------------------------
   --  Streaming Data Types  --
   ----------------------------

   type ZSTD_CStream_ptr is new System.Address;
   type ZSTD_DStream_ptr is new System.Address;

   Null_CStream_pointer : constant ZSTD_CStream_ptr := ZSTD_CStream_ptr (System.Null_Address);
   Null_DStream_pointer : constant ZSTD_DStream_ptr := ZSTD_DStream_ptr (System.Null_Address);

   --  *src <start of input buffer>
   --  size <size of input buffer>
   --  pos  <position where reading stopped. Will be updated. Necessarily 0 <= pos <= size>
   type ZSTD_inBuffer_s is record
      src  : access IC.unsigned_char;
      size : IC.size_t;
      pos  : IC.size_t;
   end record;

   --  *dst <start of output buffer>
   --  size <size of output buffer>
   --  pos  <position where reading stopped. Will be updated. Necessarily 0 <= pos <= size>
   type ZSTD_outBuffer_s is record
      dst  : access IC.unsigned_char;
      size : IC.size_t;
      pos  : IC.size_t;
   end record;

   type ZSTD_inBuffer_s_Access is access all ZSTD_inBuffer_s;
   pragma Convention (C, ZSTD_inBuffer_s_Access);

   type ZSTD_outBuffer_s_Access is access all ZSTD_outBuffer_s;
   pragma Convention (C, ZSTD_outBuffer_s_Access);

   -----------------------------
   --  Streaming Compression  --
   -----------------------------

   --  A ZSTD_CStream object is required to track streaming operation.
   --  Use ZSTD_createCStream() and ZSTD_freeCStream() to create/release resources.
   --  ZSTD_CStream objects can be reused multiple times on consecutive compression operations.

   function ZSTD_createCStream return ZSTD_CStream_ptr;
   pragma Import (C, ZSTD_createCStream, "ZSTD_createCStream");

   function ZSTD_freeCStream (zcs : ZSTD_CStream_ptr) return IC.size_t;
   pragma Import (C, ZSTD_freeCStream, "ZSTD_freeCStream");

   --  Start by initializing ZSTD_CStream.
   --  Use ZSTD_initCStream() to start a new compression operation.

   function ZSTD_initCStream
     (zcs : ZSTD_CStream_ptr;
      compressionLevel : IC.int) return IC.size_t;
   pragma Import (C, ZSTD_initCStream, "ZSTD_initCStream");

   --  Use ZSTD_compressStream() repetitively to consume input stream.
   --  The function will automatically update both `pos`.
   --  Note that it may not consume the entire input, in which case `pos < size`,
   --  and it's up to the caller to present again remaining data.
   --  @return : a size hint, preferred nb of bytes to use as input for next function call
   --           (it's just a hint, to help latency a little, any other value will work fine)
   --           (note : the size hint is guaranteed to be <= ZSTD_CStreamInSize() )
   --            or an error code, which can be tested using ZSTD_isError().

   --  recommended size for input buffer
   function ZSTD_CStreamInSize return IC.size_t;
   pragma Import (C, ZSTD_CStreamInSize, "ZSTD_CStreamInSize");

   --  recommended size for output buffer
   function ZSTD_CStreamOutSize return IC.size_t;
   pragma Import (C, ZSTD_CStreamOutSize, "ZSTD_CStreamOutSize");

   function ZSTD_compressStream
     (zcs    : ZSTD_CStream_ptr;
      output : ZSTD_outBuffer_s_Access;
      input  : ZSTD_inBuffer_s_Access) return IC.size_t;
   pragma Import (C, ZSTD_compressStream, "ZSTD_compressStream");

   --  At any moment, it's possible to flush whatever data remains within buffer,
   --  using ZSTD_flushStream().  `output->pos` will be updated.
   --  Note some content might still be left within internal buffer if `output->size` is too small.
   --  @return : nb of bytes still present within internal buffer (0 if it's empty)
   --            or an error code, which can be tested using ZSTD_isError().

   function ZSTD_flushStream
     (zcs    : ZSTD_CStream_ptr;
      output : ZSTD_outBuffer_s_Access) return IC.size_t;
   pragma Import (C, ZSTD_flushStream, "ZSTD_flushStream");

   --  ZSTD_endStream() instructs to finish a frame.
   --  It will perform a flush and write frame epilogue.
   --  The epilogue is required for decoders to consider a frame completed.
   --  Similar to ZSTD_flushStream(), it may not be able to flush the full content if
   --  `output->size` is too small so call again ZSTD_endStream() to complete the flush.
   --  @return : nb of bytes still present within internal buffer (0 if it's empty)
   --            or an error code, which can be tested using ZSTD_isError().

   function ZSTD_endStream
     (zcs    : ZSTD_CStream_ptr;
      output : ZSTD_outBuffer_s_Access) return IC.size_t;
   pragma Import (C, ZSTD_endStream, "ZSTD_endStream");

   -------------------------------
   --  Streaming Decompression  --
   -------------------------------

   --  A ZSTD_DStream object is required to track streaming operations.
   --  Use ZSTD_createDStream() and ZSTD_freeDStream() to create/release resources.
   --  ZSTD_DStream objects can be re-used multiple times.

   function ZSTD_createDStream return ZSTD_DStream_ptr;
   pragma Import (C, ZSTD_createDStream, "ZSTD_createDStream");

   function ZSTD_freeDStream (zds : ZSTD_DStream_ptr) return IC.size_t;
   pragma Import (C, ZSTD_freeDStream, "ZSTD_freeDStream");

   --  Use ZSTD_initDStream() to start a new decompression operation,

   function ZSTD_initDStream (zds : ZSTD_DStream_ptr) return IC.size_t;
   pragma Import (C, ZSTD_initDStream, "ZSTD_initDStream");

   --  Use ZSTD_decompressStream() repetitively to consume your input.
   --  The function will update both `pos`.
   --  Note that it may not consume the entire input (pos < size),
   --  in which case it's up to the caller to present remaining input again.
   --  @return : 0 when a frame is completely decoded and fully flushed,
   --            1 when there is still some data left within internal buffer to flush,
   --            >1 when more data is expected, with value being a suggested next input size
   --               (it's just a hint, which helps latency, any size is accepted),
   --               or an error code, which can be tested using ZSTD_isError().

   function ZSTD_DStreamInSize return IC.size_t;
   pragma Import (C, ZSTD_DStreamInSize, "ZSTD_DStreamInSize");

   function ZSTD_DStreamOutSize return IC.size_t;
   pragma Import (C, ZSTD_DStreamOutSize, "ZSTD_DStreamOutSize");

   function ZSTD_decompressStream
     (zds    : ZSTD_DStream_ptr;
      output : ZSTD_outBuffer_s_Access;
      input  : ZSTD_inBuffer_s_Access) return IC.size_t;
   pragma Import (C, ZSTD_decompressStream, "ZSTD_decompressStream");


   Warn_src_file_DNE    : constant String := "ERROR: Source file does not exist";
   Warn_src_read_fail   : constant String := "ERROR: Failed to read source file";
   Warn_dst_write_fail  : constant String := "ERROR: Failed to write to open output file";
   Warn_compress_fail   : constant String := "ERROR: Failed to compress data after reading " &
                                                    "source file";
   Warn_decompress_fail : constant String := "ERROR: Failed to decompress data after reading " &
                                                    "source file";
   Warn_way_too_big     : constant String := "ERROR: Hit size limit imposed by this architecture";
   Warn_orig_size_fail  : constant String := "ERROR: Original size unknown";

end Zstandard;