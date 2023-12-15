--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

package Zstandard.Streaming_Compression is

   type Compressor is tagged private;

   --  This is the initialization procedure.
   --  The output stream and compression level are externally provided
   procedure Initialize
     (mechanism     : out Compressor;
      output_stream : not null access Ada.Streams.Root_Stream_Type'Class;
      quality       : Compression_Level := Default_Compression);

   --  Compress data as they are received
   procedure Compress_Data
     (mechanism : out Compressor;
      data      : Ada.Streams.Stream_Element_Array);

   --  Finalize compression (flush)
   procedure Finalize_Compression_Frame (mechanism : Compressor);

   --  Recommended input buffer size (for Compress_Data).
   --  If called before "Compress_Data", it will be the standard ZSTD_CStreamInSize result
   --  After "Compress_Data" execution, it will be passed on the resultant hint
   function Next_Data_Size_Recommendation (mechanism : Compressor) return Positive;

   streaming_compression_initialization : exception;
   streaming_compression_error          : exception;
   streaming_compression_finalization   : exception;

private

   Buffer_Output_Size : constant IC.size_t := ZSTD_CStreamOutSize;

   type Compressor is tagged
      record
         target_stream  : access Ada.Streams.Root_Stream_Type'Class;
         zstd_stream    : ZSTD_CStream_ptr := Null_CStream_pointer;
         data_size_hint : IC.size_t := Buffer_Output_Size;
      end record;

end Zstandard.Streaming_Compression;
