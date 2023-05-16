--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt


with Ada.Unchecked_Conversion;
with Ada.Text_IO;

package body Zstandard.Streaming_Decompression is


   ------------------
   --  Initialize  --
   ------------------
   function Initialize (mechanism : out Decompressor) return Natural
   is
      initResult : IC.size_t;
   begin
      mechanism.zstd_stream   := ZSTD_createDStream;

      if mechanism.zstd_stream = Null_DStream_pointer then
         raise streaming_decompression_initialization with "ZSTD_createDStream failure";
      end if;

      initResult := ZSTD_initDStream (zds => mechanism.zstd_stream);
      if Natural (ZSTD_isError (code => initResult)) /= 0 then
         raise streaming_decompression_initialization with "ZSTD_initDStream failure";
      end if;
      return Natural (initResult);
   end Initialize;


   ------------------
   --   Finalize   --
   ------------------
   procedure Finalize (mechanism : Decompressor)
   is
      res : IC.size_t;

      pragma Unreferenced (res);
   begin
      res := ZSTD_freeDStream (mechanism.zstd_stream);
   end Finalize;


   ----------------------------
   --  Push_Compressed_Data  --
   ----------------------------
   procedure Push_Compressed_Data
     (mechanism : in out Decompressor;
      compressed_data : String)
   is
   begin
      mechanism.data_in := fast_input_buffer (compressed_data);
      mechanism.input_size := Natural (compressed_data'Length);
      mechanism.input_pos  := 0;
   end Push_Compressed_Data;


   -----------------------------
   --  Get_Uncompressed_Data  --
   -----------------------------
   function Get_Uncompressed_Data
     (mechanism    : in out Decompressor;
      output_data  : out Output_Data_Container;
      last_element : out Natural;
      call_again   : out Boolean) return Natural
   is
      decomp_rc : IC.size_t;
      inbuffer  : aliased ZSTD_inBuffer_s;
      outbuffer : aliased ZSTD_outBuffer_s;

      type data_out_type is array (Output_Data_Container'Range) of aliased IC.unsigned_char;

      data_out  : data_out_type := (others => IC.unsigned_char (0));

      function data_out_to_container is
        new Ada.Unchecked_Conversion (Source => data_out_type,
                                      Target => Output_Data_Container);
   begin
      call_again := False;
      last_element := 0;

      if mechanism.zstd_stream = Null_DStream_pointer then
         raise streaming_decompression_error with "Run initialize procedure first";
      end if;

      if mechanism.input_size < mechanism.input_pos then
         return 0;
      end if;

      inbuffer := (src  => mechanism.data_in (mechanism.data_in'First)'Unchecked_Access,
                   size => IC.size_t (mechanism.input_size),
                   pos  => IC.size_t (mechanism.input_pos));

      outbuffer := (dst  => data_out (data_out'First)'Unchecked_Access,
                    size => IC.size_t (Output_Data_Container'Length),
                    pos  => 0);

      decomp_rc := ZSTD_decompressStream (zds    => mechanism.zstd_stream,
                                          output => outbuffer'Unchecked_Access,
                                          input  => inbuffer'Unchecked_Access);
      if Natural (ZSTD_isError (decomp_rc)) /= 0 then
         declare
            errmsg : constant IC.Strings.chars_ptr := ZSTD_getErrorName (decomp_rc);
         begin
            Ada.Text_IO.Put_Line ("ZDECOMPERR: " & IC.Strings.Value (errmsg));
         end;
      end if;

      output_data := data_out_to_container (data_out);
      last_element := Natural (outbuffer.pos);
      mechanism.input_pos := Natural (inbuffer.pos);
      call_again := True;

      return Natural (decomp_rc);

   end Get_Uncompressed_Data;


   -------------------------
   --  fast_input_buffer  --
   -------------------------
   function fast_input_buffer (chunk : String) return data_in_type
   is
      subtype data_string is String (1 .. input_size);

      function data_string_to_data_in is
        new Ada.Unchecked_Conversion (Source => data_string,
                                      Target => data_in_type);
      cartridge : data_string := (others => Character'Val (0));
   begin
      cartridge (cartridge'First .. cartridge'First + chunk'Length - 1) := chunk;
      return data_string_to_data_in (cartridge);
   end fast_input_buffer;


end Zstandard.Streaming_Decompression;
