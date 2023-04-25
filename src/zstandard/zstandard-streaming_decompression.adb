--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt


with Ada.Unchecked_Conversion;

package body Zstandard.Streaming_Decompression is

   ------------------
   --  Initialize  --
   ------------------
   procedure Initialize
     (mechanism       : out Decompressor;
      input_stream    : not null access STR.Root_Stream_Type'Class)
   is
      initResult : IC.size_t;
   begin
      mechanism.source_stream := input_stream;
      mechanism.zstd_stream   := ZSTD_createDStream;

      if mechanism.zstd_stream = Null_DStream_pointer then
         raise streaming_decompression_initialization with "ZSTD_createDStream failure";
      end if;

      initResult := ZSTD_initDStream (zds => mechanism.zstd_stream);
      if Natural (ZSTD_isError (code => initResult)) /= 0 then
         raise streaming_decompression_initialization with "ZSTD_initDStream failure";
      end if;
   end Initialize;


   -----------------------
   --  Decompress_Data  --
   -----------------------
   procedure Decompress_Data
     (mechanism    :     Decompressor;
      complete     : out Boolean;
      output_data  : out Output_Data_Container;
      last_element : out Natural)
   is

      Last      : STR.Stream_Element_Offset;
      sin_last  : constant STR.Stream_Element_Offset :=
        STR.Stream_Element_Offset (Recommended_Chunk_Size);

      type data_in_type  is array (1 .. Recommended_Chunk_Size) of aliased IC.unsigned_char;
      type data_out_type is array (1 .. Output_Data_Container'Size) of aliased IC.unsigned_char;
      subtype data_sin_type is STR.Stream_Element_Array (1 .. sin_last);

      size_hint : IC.size_t;
      inbuffer  : aliased ZSTD_inBuffer_s;
      outbuffer : aliased ZSTD_outBuffer_s;

      data_in   : data_in_type  := (others => IC.unsigned_char (0));
      data_out  : data_out_type := (others => IC.unsigned_char (0));
      data_sin  : data_sin_type;

      function data_sin_to_data_in is
        new Ada.Unchecked_Conversion (Source => data_sin_type,
                                      Target => data_in_type);

      function data_out_to_container is
        new Ada.Unchecked_Conversion (Source => data_out_type,
                                      Target => Output_Data_Container);

      pragma Unreferenced (size_hint);

   begin
      if mechanism.zstd_stream = Null_DStream_pointer then
         raise streaming_decompression_error with "Run initialize procedure first";
      end if;

      mechanism.source_stream.Read (Item => data_sin, Last => Last);

      if Natural (Last) = 0 then
         last_element := 0;
         complete := True;
         return;
      end if;

      data_in := data_sin_to_data_in (data_sin);
      complete := (Natural (Last) /= Natural (Recommended_Chunk_Size));

      inbuffer := (src  => data_in (data_in'First)'Unchecked_Access,
                   size => Recommended_Chunk_Size,
                   pos  => 0);

      outbuffer := (dst  => data_out (data_out'First)'Unchecked_Access,
                    size => Output_Data_Container'Size,
                    pos  => 0);

      size_hint := ZSTD_decompressStream (zds    => mechanism.zstd_stream,
                                          output => outbuffer'Unchecked_Access,
                                          input  => inbuffer'Unchecked_Access);

      output_data := data_out_to_container (data_out);
      last_element := Natural (outbuffer.pos);

   end Decompress_Data;



end Zstandard.Streaming_Decompression;
