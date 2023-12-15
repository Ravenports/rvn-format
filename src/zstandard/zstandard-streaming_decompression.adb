--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Ada.Unchecked_Conversion;
with Ada.Text_IO;

package body Zstandard.Streaming_Decompression is


   ------------------
   --  Initialize  --
   ------------------
   procedure Initialize
     (mechanism    : out Decompressor;
      input_stream : not null SIO.Stream_Access)
   is
      initResult : IC.size_t;
   begin
      mechanism.rvn_stmaxs  := input_stream;
      mechanism.zstd_stream := ZSTD_createDStream;

      if mechanism.zstd_stream = Null_DStream_pointer then
         raise streaming_decompression_initialization with "ZSTD_createDStream failure";
      end if;

      initResult := ZSTD_initDStream (zds => mechanism.zstd_stream);
      if Natural (ZSTD_isError (code => initResult)) /= 0 then
         raise streaming_decompression_initialization with "ZSTD_initDStream failure";
      end if;
      mechanism.planned := Natural (initResult);
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


   -----------------------------
   --  Get_Uncompressed_Data  --
   -----------------------------
   function Get_Uncompressed_Data
     (mechanism    : in out Decompressor;
      buffer       : in out ASU.Unbounded_String) return Boolean
   is
      data_in      : data_in_type := (others => IC.unsigned_char (0));
      bytes_read   : Natural;
      inbuffer     : aliased ZSTD_inBuffer_s;
      max_out_size : constant IC.size_t := IC.size_t (Natural_DStreamOutSize);
      call_again   : Boolean := False;
      bytes_out    : Natural := 0;

      type data_out_type is array (1 .. max_out_size) of aliased IC.unsigned_char;
   begin
      if mechanism.zstd_stream = Null_DStream_pointer then
         raise streaming_decompression_error with "Run initialize procedure first";
      end if;

      --  Outgoing buffer needs to be empty
      buffer := ASU.Null_Unbounded_String;

      --  We only loop when the outbuffer position is zero, meaning no output is
      --  written to the buffer.  So most of the time the logic flows straight through
      --  without looping.
      loop
         exit when mechanism.planned = 0;
         bytes_read := read_compressed_data (input_stream  => mechanism.rvn_stmaxs,
                                             bytes_planned => mechanism.planned,
                                             data_in       => data_in);
         exit when bytes_read = 0;

         inbuffer := (src  => data_in (data_in'First)'Unchecked_Access,
                      size => IC.size_t (bytes_read),
                      pos  => 0);

         loop
            --  If data is properly encoded, this doesn't loop because inbuffer.pos
            --  always equals inbuffer.size after the first pass, so it exits

            exit when Natural (inbuffer.pos) >= Natural (inbuffer.size);
            declare
               decomp_rc : IC.size_t;
               outbuffer : aliased ZSTD_outBuffer_s;
               data_out  : data_out_type := (others => IC.unsigned_char (0));
            begin
               outbuffer := (dst  => data_out (data_out'First)'Unchecked_Access,
                             size => max_out_size,
                             pos  => 0);
               decomp_rc := ZSTD_decompressStream (zds    => mechanism.zstd_stream,
                                                   output => outbuffer'Unchecked_Access,
                                                   input  => inbuffer'Unchecked_Access);
               if Natural (ZSTD_isError (decomp_rc)) /= 0 then
                  declare
                     errmsg : constant IC.Strings.chars_ptr := ZSTD_getErrorName (decomp_rc);
                  begin
                     Ada.Text_IO.Put_Line ("ZDECOMPERR: " & IC.Strings.Value (errmsg));
                     return False;
                  end;
               end if;
               mechanism.planned := Natural (decomp_rc);
               call_again := mechanism.planned > 0;

               bytes_out := Natural (outbuffer.pos);
               if bytes_out > 0 then
                  declare
                     type bin_array is array (1 .. bytes_out) of IC.unsigned_char;
                     subtype bin_string is String (1 .. bytes_out);
                     payload : bin_array;
                     payload_string : bin_string;

                     function bytes_to_string is
                       new Ada.Unchecked_Conversion (Source => bin_array,
                                                     Target => bin_string);
                  begin
                     payload := bin_array (data_out (1 .. outbuffer.pos));
                     payload_string := bytes_to_string (payload);
                     ASU.Append (buffer, payload_string);
                  end;
               end if;
            end;
         end loop;
         exit when bytes_out > 0;
      end loop;
      return call_again;
   end Get_Uncompressed_Data;



   ----------------------------------
   --  file_to_file_decompression  --
   ----------------------------------
   function file_to_file_decompression
     (input_stream  : not null SIO.Stream_Access;
      output_stream : not null SIO.Stream_Access) return File_Size
   is
      zstd_stream : ZSTD_DStream_ptr;
      initResult  : IC.size_t;
      plain_size  : File_Size := 0;
   begin
      zstd_stream := ZSTD_createDStream;

      if zstd_stream = Null_DStream_pointer then
         raise streaming_decompression_initialization with "ZSTD_createDStream failure";
      end if;

      initResult := ZSTD_initDStream (zds => zstd_stream);
      if Natural (ZSTD_isError (code => initResult)) /= 0 then
         raise streaming_decompression_initialization with "ZSTD_initDStream failure";
      end if;

      declare
         data_in      : data_in_type := (others => IC.unsigned_char (0));
         bytes_read   : Natural;
         planned      : Natural;
         inbuffer     : aliased ZSTD_inBuffer_s;
         max_out_size : constant IC.size_t := IC.size_t (Natural_DStreamOutSize);

         type data_out_type is array (1 .. max_out_size) of aliased IC.unsigned_char;
      begin
         planned := Natural (initResult);
         loop
            exit when planned = 0;
            bytes_read := read_compressed_data (input_stream  => input_stream,
                                                bytes_planned => planned,
                                                data_in       => data_in);
            exit when bytes_read = 0;

            inbuffer := (src  => data_in (data_in'First)'Unchecked_Access,
                         size => IC.size_t (bytes_read),
                         pos  => 0);
            loop
               exit when Natural (inbuffer.pos) >= Natural (inbuffer.size);
               declare
                  decomp_rc : IC.size_t;
                  outbuffer : aliased ZSTD_outBuffer_s;
                  data_out  : data_out_type := (others => IC.unsigned_char (0));
               begin
                  outbuffer := (dst  => data_out (data_out'First)'Unchecked_Access,
                                size => max_out_size,
                                pos  => 0);
                  decomp_rc := ZSTD_decompressStream (zds    => zstd_stream,
                                                      output => outbuffer'Unchecked_Access,
                                                      input  => inbuffer'Unchecked_Access);
                  if Natural (ZSTD_isError (decomp_rc)) /= 0 then
                     declare
                        errmsg : constant IC.Strings.chars_ptr := ZSTD_getErrorName (decomp_rc);
                     begin
                        Ada.Text_IO.Put_Line ("ZDECOMPERR: " & IC.Strings.Value (errmsg));
                        return File_Size'Last;
                     end;
                  end if;
                  planned := Natural (decomp_rc);

                  if Natural (outbuffer.pos) > 0 then
                     declare
                        type bin_array is array (1 .. Natural (outbuffer.pos)) of IC.unsigned_char;
                        type tray is record
                           payload : bin_array;
                        end record;
                        pragma Pack (tray);

                        data : tray;
                     begin
                        data.payload := bin_array (data_out (1 .. outbuffer.pos));
                        tray'Output (output_stream, data);
                        plain_size := plain_size + File_Size (outbuffer.pos);
                     end;
                  end if;
               end;
            end loop;
         end loop;

         initResult := ZSTD_freeDStream (zstd_stream);
         return plain_size;
      end;

   end file_to_file_decompression;


   ----------------------------
   --  read_compressed_data  --
   ----------------------------
   function read_compressed_data
     (input_stream  : not null SIO.Stream_Access;
      bytes_planned : Natural;
      data_in       : out data_in_type) return Natural
   is
      bytes_read : Natural := 0;
   begin
      declare
         datum : Ada.Streams.Stream_Element;
      begin
         for sindex in 1 .. bytes_planned loop
            Ada.Streams.Stream_Element'Read (input_stream, datum);
            data_in (sindex) := IC.unsigned_char (datum);
            bytes_read := bytes_read + 1;
         end loop;
      exception
         when Ada.Streams.Stream_IO.End_Error =>
            null;
      end;
      return bytes_read;
   end read_compressed_data;


end Zstandard.Streaming_Decompression;
