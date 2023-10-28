--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt


with Ada.Unchecked_Conversion;

package body Zstandard.Streaming_Compression is

   ------------------
   --  Initialize  --
   ------------------
   procedure Initialize
     (mechanism     : out Compressor;
      output_stream : not null access Ada.Streams.Root_Stream_Type'Class;
      quality       : Compression_Level := Default_Compression)
   is
      initResult : IC.size_t;
   begin
      mechanism.target_stream := output_stream;
      mechanism.zstd_stream := ZSTD_createCStream;
      if mechanism.zstd_stream = Null_CStream_pointer then
         raise streaming_compression_initialization with "ZSTD_createCStream failure";
      end if;
      initResult := ZSTD_initCStream (zcs => mechanism.zstd_stream,
                                      compressionLevel => IC.int (quality));
      if Natural (ZSTD_isError (code => initResult)) /= 0 then
         raise streaming_compression_initialization with "ZSTD_initCStream failure";
      end if;
   end Initialize;


   ---------------------
   --  Compress_Data  --
   ---------------------
   procedure Compress_Data (mechanism : out Compressor;
                            data      : Ada.Streams.Stream_Element_Array)
   is
      use type IC.size_t;

      type data_in_type  is array (1 .. data'Length) of aliased IC.unsigned_char;
      type data_out_type is array (1 .. Buffer_Output_Size) of aliased IC.unsigned_char;

      data_in   : data_in_type := (others => 0);
      data_out  : data_out_type := (others => 0);

      inbuffer  : aliased ZSTD_inBuffer_s := (src  => data_in (data_in'First)'Unchecked_Access,
                                              size => data'Length,
                                              pos  => 0);
      outbuffer : aliased ZSTD_outBuffer_s := (dst  => data_out (data_out'First)'Unchecked_Access,
                                               size => Buffer_Output_Size,
                                               pos  => 0);

      function stream_to_char is new Ada.Unchecked_Conversion
        (Source => Ada.Streams.Stream_Element_Array,
         Target => data_in_type);

   begin
      if mechanism.zstd_stream = Null_CStream_pointer then
         raise streaming_compression_error with "Run initialize procedure first";
      end if;
      data_in := stream_to_char (data);
      loop
         exit when inbuffer.pos >= inbuffer.size;
         outbuffer.pos := 0;
         mechanism.data_size_hint := ZSTD_compressStream (zcs    => mechanism.zstd_stream,
                                                          output => outbuffer'Unchecked_Access,
                                                          input  => inbuffer'Unchecked_Access);
         declare
            type final_out is array (1 .. outbuffer.pos) of aliased IC.unsigned_char;
            subtype final_stream is Ada.Streams.Stream_Element_Array
              (1 .. Ada.Streams.Stream_Element_Offset (outbuffer.pos));

            function char_to_stream is new Ada.Unchecked_Conversion
              (Source => final_out,
               Target => final_stream);
         begin
            mechanism.target_stream.Write
              (char_to_stream (final_out (data_out (1 .. outbuffer.pos))));
         end;
      end loop;
   end Compress_Data;


   ------------------------------------
   --  Recommended_Data_Buffer_Size  --
   ------------------------------------
   function Next_Data_Size_Recommendation (mechanism : Compressor) return Positive is
   begin
      return Positive (mechanism.data_size_hint);
   end Next_Data_Size_Recommendation;


   ----------------------------------
   --  Finalize_Compression_Frame  --
   ----------------------------------
   procedure Finalize_Compression_Frame (mechanism : Compressor)
   is
      type data_out_type is array (1 .. Buffer_Output_Size) of aliased IC.unsigned_char;
      data_out  : data_out_type := (others => 0);
      outbuffer : aliased ZSTD_outBuffer_s := (dst  => data_out (data_out'First)'Unchecked_Access,
                                               size => Buffer_Output_Size,
                                               pos  => 0);
      remaining : IC.size_t;
   begin
      remaining := ZSTD_endStream (zcs    => mechanism.zstd_stream,
                                   output => outbuffer'Unchecked_Access);
      if Natural (remaining) > 0 then
         raise streaming_compression_finalization with "not fully flushed";
      end if;
      declare
         type final_out is array (1 .. outbuffer.pos) of aliased IC.unsigned_char;
         subtype final_stream is Ada.Streams.Stream_Element_Array
           (1 .. Ada.Streams.Stream_Element_Offset (outbuffer.pos));

         function char_to_stream is new Ada.Unchecked_Conversion (Source => final_out,
                                                                  Target => final_stream);
      begin
         mechanism.target_stream.Write
              (char_to_stream (final_out (data_out (1 .. outbuffer.pos))));
      end;
   end Finalize_Compression_Frame;



end Zstandard.Streaming_Compression;
