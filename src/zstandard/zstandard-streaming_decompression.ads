--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Ada.Streams.Stream_IO;

package Zstandard.Streaming_Decompression is

   package SIO renames Ada.Streams.Stream_IO;

   subtype Output_Data_Container is String (1 .. Natural_DStreamOutSize);

   type Decompressor is tagged private;

   --  This is the initialization function.
   --  The result signifies the size of the next chunk to read.
   function Initialize
     (mechanism : out Decompressor) return Natural;

   --  This frees up the resources used by the decompressor
   procedure Finalize
     (mechanism : Decompressor);

   --  Keep calling until you run out of data or the request length is zero
   procedure Push_Compressed_Data
     (mechanism : in out Decompressor;
      compressed_data : String);

   --  Returns decompressed data after processing chunk.
   --  Keep calling until "call_agein" is false.
   --  Ignore return result until "call_again" is false, at which point
   --  `push_compressed_data` is run again with the string the lenfth of the result.
   --  The last_element specifies how much of the output buffer is used.
   function Get_Uncompressed_Data
     (mechanism    : in out Decompressor;
      output_data  : out Output_Data_Container;
      last_element : out Natural;
      call_again   : out Boolean) return Natural;

   streaming_decompression_initialization : exception;
   streaming_decompression_error          : exception;

private

   input_size : constant Natural := Natural_DStreamInSize;
   type data_in_type  is array (1 .. input_size) of aliased IC.unsigned_char;

   type Decompressor is tagged
      record
         zstd_stream : ZSTD_DStream_ptr := Null_DStream_pointer;
         data_in     : data_in_type;
         input_pos   : Natural;
         input_size  : Natural;
      end record;

   function fast_input_buffer (chunk : String) return data_in_type;

end Zstandard.Streaming_Decompression;
