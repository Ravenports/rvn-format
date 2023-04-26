--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Ada.Streams.Stream_IO;

package Zstandard.Streaming_Decompression is

   package SIO renames Ada.Streams.Stream_IO;

   subtype Output_Data_Container is String (1 .. Natural_DStreamOutSize);

   type Decompressor is tagged private;

   --  This is the initialization procedure.
   --  The input stream and output buffer capacity are externally provided
   procedure Initialize
     (mechanism       : out Decompressor;
      input_stream    : not null SIO.Stream_Access);

   --  Decompress data as each input chunk is received
   --  Since the size of the compressed data is known before the first call, the caller
   --  must track the remaining bytes, thus knowning when the decompression is complete.
   --  The "last_element" is the end of the container range (e.g. 1 .. last_element)
   procedure Decompress_Data
     (mechanism    :     Decompressor;
      chunk_size   :     Natural;
      output_data  : out Output_Data_Container;
      last_element : out Natural);

   streaming_decompression_initialization : exception;
   streaming_decompression_error          : exception;

private

   type Decompressor is tagged
      record
         source_stream    : SIO.Stream_Access;
         zstd_stream      : ZSTD_DStream_ptr := Null_DStream_pointer;
      end record;

end Zstandard.Streaming_Decompression;
