--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Ada.Streams;

package Zstandard.Streaming_Decompression is

   package STR renames Ada.Streams;

   subtype Output_Data_Container is
     STR.Stream_Element_Array (1 .. SEO_DStreamOutSize);

   type Decompressor is tagged private;

   --  This is the initialization procedure.
   --  The input stream and output buffer capacity are externally provided
   procedure Initialize
     (mechanism       : out Decompressor;
      input_stream    : not null access STR.Root_Stream_Type'Class);

   --  Decompress data as each input chunk is received
   --  if "complete" then the decompression is complete (don't call procedure any more)
   --  The "last_element" is the end of the container range (e.g. 1 .. last_element)
   procedure Decompress_Data
     (mechanism    :     Decompressor;
      complete     : out Boolean;
      output_data  : out Output_Data_Container;
      last_element : out Natural);

   streaming_decompression_initialization : exception;
   streaming_decompression_error          : exception;

private

   Recommended_Chunk_Size : constant IC.size_t := ZSTD_DStreamInSize;

   type Decompressor is tagged
      record
         source_stream    : access STR.Root_Stream_Type'Class;
         zstd_stream      : ZSTD_DStream_ptr := Null_DStream_pointer;
      end record;

end Zstandard.Streaming_Decompression;
