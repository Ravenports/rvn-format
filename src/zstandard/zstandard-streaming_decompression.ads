--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Ada.Streams.Stream_IO;
with Ada.Strings.Unbounded;

package Zstandard.Streaming_Decompression is

   package SIO renames Ada.Streams.Stream_IO;
   package ASU renames Ada.Strings.Unbounded;

   type Decompressor is tagged private;

   --  This function is provided stream access to the compressed input file and the
   --  output file the uncompressed data should be extracted to.  The returned result
   --  is the size of the uncompressed data
   function file_to_file_decompression
     (input_stream  : not null SIO.Stream_Access;
      output_stream : not null SIO.Stream_Access) return File_Size;

   --  This is the initialization function.
   procedure Initialize
     (mechanism    : out Decompressor;
      input_stream : not null SIO.Stream_Access);

   --  This frees up the resources used by the decompressor
   procedure Finalize (mechanism : Decompressor);

   --  This function reads compressed data from the input stream, decompresses it,
   --  and appends the buffer (which is expected to be empty).  The function returns
   --  True if theres more data to uncompress later, and False if the decompression
   --  is finished.
   function Get_Uncompressed_Data
     (mechanism    : in out Decompressor;
      buffer       : in out ASU.Unbounded_String) return Boolean;

   streaming_decompression_initialization : exception;
   streaming_decompression_error          : exception;

private

   input_size : constant Natural := Natural_DStreamInSize;
   type data_in_type  is array (1 .. input_size) of aliased IC.unsigned_char;

   type Decompressor is tagged
      record
         zstd_stream : ZSTD_DStream_ptr := Null_DStream_pointer;
         rvn_stmaxs  : not null SIO.Stream_Access;
         planned     : Natural;
      end record;

   function read_compressed_data
     (input_stream  : not null SIO.Stream_Access;
      bytes_planned : Natural;
      data_in       : out data_in_type) return Natural;

end Zstandard.Streaming_Decompression;
