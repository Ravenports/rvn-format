--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with System;
with Ada.Direct_IO;
with Ada.Directories;
with Ada.Unchecked_Deallocation;
with Ada.Exceptions;
with Ada.IO_Exceptions;
with Ada.Strings.Fixed;
with Archive.Communication;
with Archive.Misc;
with ThickUCL.Files;
with Bourne;
with Lua;

package body Archive.Unpack is

   package TIO renames Ada.Text_IO;
   package DIR renames Ada.Directories;
   package EX  renames Ada.Exceptions;
   package IOX renames Ada.IO_Exceptions;
   package ASF renames Ada.Strings.Fixed;
   package SQW renames Archive.Communication;
   package ZST renames Zstandard;

   ------------------------------------------------------------------------------------------
   --  open_rvn_archive
   ------------------------------------------------------------------------------------------
   procedure open_rvn_archive
     (DS            : in out DArc;
      rvn_archive   : String;
      verbosity     : info_level;
      optional_pipe : Unix.File_Descriptor := Unix.not_connected)
   is
      use type ZST.File_Size;
      use type SIO.Count;
   begin
      DS.valid := False;
      DS.set_verbosity (verbosity);
      SQW.initialize (verbosity, optional_pipe);
      if DIR.Exists (rvn_archive) then
         case DIR.Kind (rvn_archive) is
            when DIR.Ordinary_File =>
               null;
            when others =>
               DS.print (normal, "The " & rvn_archive & " entity is not a regular file.");
               return;
         end case;
      else
         DS.print (normal, "The " & rvn_archive & " archive does not exist.");
         return;
      end if;

      if ZST.File_Size (DIR.Size (rvn_archive)) < (premier_block'Size / 8) then
         DS.print (normal, "The " & rvn_archive & " file is too small.  It's not an archive.");
         return;
      end if;

      SIO.Open (File => DS.rvn_handle,
                Mode => SIO.In_File,
                Name => rvn_archive);

      begin
         DS.rvn_stmaxs := SIO.Stream (DS.rvn_handle);
         premier_block'Read (DS.rvn_stmaxs, DS.header);
      exception
         when problem : others =>
            DS.print (normal, "Something went wrong opening " & rvn_archive);
            DS.print (normal, EX.Exception_Message (problem));
            return;
      end;

      if DS.header.magic_bytes = magic then
         DS.print (debug, "The magic bytes of " & rvn_archive & " match an RVN archive.");
      else
         DS.print (normal, "The magic bytes of " & rvn_archive & " are wrong.");
         DS.print (normal, "This is not a RVN archive.");
         return;
      end if;

      DS.print (debug, "RVN format version :" & DS.header.version'Img);
      DS.print (debug, "    groups defined :" & DS.header.num_groups'Img);
      DS.print (debug, "    owners defined :" & DS.header.num_owners'Img);
      DS.print (debug, "     links defined :" & DS.header.link_blocks'Img);
      DS.print (debug, " filenames defined :" & DS.header.fname_blocks'Img);
      DS.print (debug, "      number files :" & DS.header.file_blocks'Img);
      DS.print (debug, "    metadata bytes :" & DS.header.flat_metadata'Img);
      DS.print (debug, "        compressed :" & DS.header.size_metadata'Img);
      DS.print (debug, "  file index bytes :" & DS.header.flat_filedata'Img);
      DS.print (debug, "        compressed :" & DS.header.size_filedata'Img);
      DS.print (debug, "     archive bytes :" & DS.header.flat_archive'Img);
      DS.print (debug, "        compressed :" & DS.header.size_archive'Img);
      DS.print (debug, "             index :" & SIO.Index (DS.rvn_handle)'Img);

      DS.valid    := True;
      DS.b2_index := 65;
      DS.b3_index := DS.b2_index + SIO.Count (DS.header.size_metadata);
      DS.b4_index := DS.b3_index + SIO.Count (DS.header.size_filedata);

      DS.con_track.num_groups  := Natural (DS.header.num_groups);
      DS.con_track.num_owners  := Natural (DS.header.num_owners);
      DS.con_track.link_blocks := Natural (DS.header.link_blocks);
      DS.con_track.file_blocks := Natural (DS.header.file_blocks);
      DS.con_track.name_blocks := Natural (DS.header.fname_blocks);

   end open_rvn_archive;


   ------------------------------------------------------------------------------------------
   --  close_rvn_archive
   ------------------------------------------------------------------------------------------
   procedure close_rvn_archive (DS : in out DArc)
   is
   begin
      if DS.valid then
         SIO.Close (DS.rvn_handle);
         DS.valid := False;
         DS.print (debug, "RVN archive was closed.");
      else
         DS.print (debug, "Attempted to close an RVN archive that was not open.");
      end if;
   end close_rvn_archive;


   ------------------------------------------------------------------------------------------
   --  rvn_archive_is_open
   ------------------------------------------------------------------------------------------
   function rvn_archive_is_open (DS : DArc) return Boolean is
   begin
      return DS.valid;
   end rvn_archive_is_open;


   ------------------------------------------------------------------------------------------
   --  set_verbosity
   ------------------------------------------------------------------------------------------
   procedure set_verbosity (DS : in out DArc; level : info_level)
   is
   begin
      DS.level := level;
   end set_verbosity;


   ------------------------------------------------------------------------------------------
   --  print
   ------------------------------------------------------------------------------------------
   procedure print (DS : DArc; msg_level : info_level; message : String)
   is
      meets_criteria : constant Boolean := (msg_level <= DS.level);
   begin
      if meets_criteria then
         case msg_level is
            when debug   => SQW.emit_debug (message);
            when verbose => SQW.emit_notice (message);
            when normal  => SQW.emit_message (message);
            when silent  => null;
         end case;
      end if;
   end print;


   ------------------------------------------------------------------------------------------
   --  direct_file_creation
   ------------------------------------------------------------------------------------------
   procedure direct_file_creation
     (DS          : DArc;
      target_file : String;
      contents    : String)
   is
      subtype file_contents is String (1 .. contents'Length);
      package Blitz is new Ada.Direct_IO (file_contents);

      out_handle : Blitz.File_Type;
   begin
      begin
         Blitz.Create (File => out_handle,
                       Mode => Blitz.Out_File,
                       Name => target_file);
         if contents'Length > 0 then
            Blitz.Write (File => out_handle,
                         Item => file_contents (contents));
         end if;
         Blitz.Close (out_handle);
         DS.print (debug, "Successful direct file creation of " & target_file);
      exception
         when others =>
            DS.print (normal, "Failed to create output file at " & target_file);
      end;
   end direct_file_creation;


   ------------------------------------------------------------------------------------------
   --  write_metadata_to_file
   ------------------------------------------------------------------------------------------
   procedure write_metadata_to_file
     (DS      : in out DArc;
      filepath : String)
   is
      --  metadata is always starts on index of 64.
      --  Move to this point if not already there.
      --  However, this is unnecessary if metadata is zero bytes.
      use type SIO.Count;
      metasize : zstd_size renames DS.header.size_metadata;
   begin
      if metasize = 0 then
         DS.print (debug, "There is no metadata; writing an zero-byte file at " & filepath);
         return;
      end if;
      if SIO.Index (DS.rvn_handle) /= DS.b2_index then
         SIO.Set_Index (DS.rvn_handle, DS.b2_index);
      end if;
      DS.print (debug, "Single pass decompression for metadata, comp size:" & metasize'Img);
      declare
         decompress_success : Boolean;
         plain_text : constant String :=
           ZST.Decompress (archive_saxs => DS.rvn_stmaxs,
                           data_length  => Natural (DS.header.size_metadata),
                           final_size   => ZST.File_Size (DS.header.flat_metadata),
                           successful   => decompress_success);
      begin
         if decompress_success then
            DS.direct_file_creation (target_file => filepath,
                                     contents    => plain_text);
         else
            DS.print (normal, "Failed to extract metadata from archive");
         end if;
      end;
   end write_metadata_to_file;


   ------------------------------------------------------------------------------------------
   --  extract_metadata
   ------------------------------------------------------------------------------------------
   function extract_metadata (DS : in out DArc) return String
   is
      decompress_success : Boolean;
      use type SIO.Count;
   begin
      if DS.header.size_metadata = 0 then
         return "";
      end if;
      if DS.header.flat_metadata > zstd_size (KB512) then
         return "Invalid rvn archive - metadata longer than 256kb:" & DS.header.flat_metadata'Img;
      end if;
      if SIO.Index (DS.rvn_handle) /= DS.b2_index and then
        DS.b2_index > 0
      then
         SIO.Set_Index (DS.rvn_handle, DS.b2_index);
      end if;
      return ZST.Decompress (archive_saxs => DS.rvn_stmaxs,
                             data_length  => Natural (DS.header.size_metadata),
                             final_size   => ZST.File_Size (DS.header.flat_metadata),
                             successful   => decompress_success);
   end extract_metadata;


   ------------------------------------------------------------------------------------------
   --  consume_index
   ------------------------------------------------------------------------------------------
   function consume_index (DS : in out DArc; index_data : String) return Natural
   is
      function sufficient_data (chars_needed : Natural) return Boolean;
      function retrieve_filename (filename_length : max_fname) return A_filename;

      num_groups : constant Natural := DS.con_track.num_groups;
      num_owners : constant Natural := DS.con_track.num_owners;
      num_links  : constant Natural := DS.con_track.link_blocks;
      num_files  : constant Natural := DS.con_track.file_blocks;
      sindex    : Natural := index_data'First;
      fn_index  : Natural := 32 * (num_groups + num_owners + num_links) + (64 * num_files) + sindex;
      data_left : Natural := fn_index - 1;
      fn_remain : Natural := index_data'Last - fn_index + 1;

      function sufficient_data (chars_needed : Natural) return Boolean is
      begin
         return chars_needed <= data_left;
      end sufficient_data;

      function retrieve_filename (filename_length : max_fname) return A_filename
      is
         result : A_filename := (others => Character'Val (0));
         natural_length : constant Natural := Natural (filename_length);
         last_block_index : constant Natural := fn_index + natural_length - 1;
         last_res_index : constant Natural := A_filename'First + natural_length - 1;
      begin
         if fn_remain < natural_length then
            DS.print (normal, "consume_index:retrieve_filename error: insufficient data.");
            return result;
         end if;

         result (A_filename'First .. last_res_index) := index_data (fn_index .. last_block_index);
         fn_index := last_block_index + 1;
         fn_remain := fn_remain - natural_length;
         return result;
      end retrieve_filename;

   begin
      --  Read in groups data
      for x in 1 .. num_groups loop
         if sufficient_data (ownergroup'Length) then
            declare
               data : ownergroup_info;
            begin
               data.name := index_data (sindex .. sindex + ownergroup'Length - 1);
               data.id   := Unix.lookup_group (trim_trailing_zeros (data.name));
               case data.id is
                  when id_not_found => data.status := id_unknown;
                  when others => data.status := id_valid;
               end case;
               DS.groups.Append (data);
               sindex := sindex + ownergroup'Length;
               data_left := data_left - ownergroup'Length;
               DS.con_track.num_groups := DS.con_track.num_groups - 1;
               if DS.level = debug then
                  DS.print (debug, "Extract group: " & trim_trailing_zeros (data.name));
               end if;
            end;
         else
            return data_left;
         end if;
      end loop;

      --  Read in owners data
      for x in 1 .. num_owners loop
         if sufficient_data (ownergroup'Length) then
            declare
               data : ownergroup_info;
            begin
               data.name := index_data (sindex .. sindex + ownergroup'Length - 1);
               data.id   := Unix.lookup_user (trim_trailing_zeros (data.name));
               case data.id is
                  when id_not_found => data.status := id_unknown;
                  when others => data.status := id_valid;
               end case;
               DS.owners.Append (data);
               sindex := sindex + ownergroup'Length;
               data_left := data_left - ownergroup'Length;
               DS.con_track.num_owners := DS.con_track.num_owners - 1;
               if DS.level = debug then
                  DS.print (debug, "Extract owner: " & trim_trailing_zeros (data.name));
               end if;
            end;
         else
            return data_left;
         end if;
      end loop;

      --  Read in 32-byte link blocks
      for x in 1 .. num_links loop
         if sufficient_data (32) then
            declare
               link32 : String (1 .. 32);
            begin
               for y in 1 .. 32 loop
                  declare
                     data : Character;
                  begin
                     data := index_data (sindex);
                     sindex := sindex + 1;
                     data_left := data_left - 1;
                     DS.links.Append (data);
                     link32 (y) := data;
                  end;
               end loop;
               DS.print (debug, "Extract link block: " & link32);
            end;
         else
            return data_left;
         end if;
      end loop;

      --  Read in 64-byte file blocks
      for x in 1 .. num_files loop
         if sufficient_data (fblk_size) then
            declare
               data : Scanned_File_Block;
               datastr : FBString;
            begin
               datastr := index_data (sindex .. sindex + fblk_size - 1);
               sindex := sindex + fblk_size;
               data_left := data_left - fblk_size;
               data := FBString_to_File_Block (datastr);
               data.filename := retrieve_filename (data.fname_length);
               DS.files.Append (data);
               DS.con_track.file_blocks := DS.con_track.file_blocks - 1;
               if data.directory_id > 0 then
                  --  directory_ID starts with 1 and increases
                  declare
                     base : constant String := extract_filename (data);
                     cart : A_Directory;
                     parent : Positive;
                  begin
                     if data.index_parent = 0 then
                        cart.directory := ASU.To_Unbounded_String (base);
                        DS.folders.Append (cart);
                        DS.print (debug, "DIR index" & data.directory_id'Img & " => " & base);
                     else
                        parent  := Positive (data.index_parent);
                        cart.directory := DS.folders.Element (parent).directory;
                        ASU.Append (cart.directory, "/" & base);
                        DS.folders.Append (cart);
                        DS.print (debug, "DIR index" & data.directory_id'Img &
                                    " => " & ASU.To_String (cart.directory));
                     end if;
                  end;
               end if;
               if DS.level = debug then
                  DS.print (debug, "");
                  DS.print (debug, "extract filename: " & extract_filename (data));
                  DS.print (debug, "          b3 sum: " & Blake_3.hex (data.blake_sum));
                  DS.print (debug, "    type of file: " & data.type_of_file'Img);
                  DS.print (debug, "       flat size:" & data.file_size_tb'Img);
                  DS.print (debug, "    parent index:" & data.index_parent'Img);
                  DS.print (debug, "    directory ID:" & data.directory_id'Img);
                  DS.print (debug, "    modified sec:" & data.modified_sec'Img);
                  DS.print (debug, "    modified  ns:" & data.modified_ns'Img);
                  DS.print (debug, "           owner: " &
                              trim_trailing_zeros (DS.owners.Element (data.index_owner).name) &
                              "(" & DS.owners.Element (data.index_owner).id'Img & ")");
                  DS.print (debug, "           group: " &
                              trim_trailing_zeros (DS.groups.Element (data.index_group).name) &
                              "(" & DS.groups.Element (data.index_group).id'Img & ")");
               end if;
            end;
         else
            return data_left;
         end if;
      end loop;

      return data_left;  --  should be zero
   end consume_index;


   ------------------------------------------------------------------------------------------
   --  retrieve_file_index
   ------------------------------------------------------------------------------------------
   procedure retrieve_file_index (DS : in out DArc)
   is
      use type SIO.Count;
      left_over : Natural;
   begin
      if SIO.Index (DS.rvn_handle) /= DS.b3_index then
         SIO.Set_Index (DS.rvn_handle, DS.b3_index);
      end if;

      if Natural (DS.header.size_filedata) > KB256 then
         declare
            type String_Access is access String;
            procedure SFree is new Ada.Unchecked_Deallocation
              (Object => String, Name => String_Access);

            fblock_size : constant Natural :=
              Natural (DS.header.num_owners) * 32 +
              Natural (DS.header.num_groups) * 32 +
              Natural (DS.header.link_blocks) * 32 +
              Natural (DS.header.fname_blocks) * 32 +
              Natural (DS.header.file_blocks) * 64;
            index_dec   : ZST.Streaming_Decompression.Decompressor;
            heap_string : String_Access;
            recall      : Boolean;
            chunkbuffer : text;
            sindex      : Natural;
            findex      : Natural;
            lindex      : Natural;
         begin
            heap_string := new String (1 .. fblock_size);
            sindex := heap_string'First;
            index_dec.Initialize (DS.rvn_stmaxs);
            DS.print (debug, "heapstring 'last is" & fblock_size'Img);
            loop
               recall := index_dec.Get_Uncompressed_Data (chunkbuffer);
               findex := sindex;
               sindex := sindex + ASU.Length (chunkbuffer);
               lindex := sindex - 1;
               DS.print (debug, "Setting heap_string range" & findex'Img & " .."  & lindex'Img);
               heap_string (findex .. lindex) := ASU.To_String (chunkbuffer);
               exit when not recall;
            end loop;
            index_dec.Finalize;
            left_over := DS.consume_index (heap_string.all);
            if left_over > 0 then
               DS.print (normal, "Streaming index consumption has unexpected left over data.");
            end if;
            SFree (heap_string);
         exception
            when ZST.Streaming_Decompression.streaming_decompression_initialization =>
               DS.print (normal, "Failed to initialize streaming decompression (RFI)");
            when ZST.Streaming_Decompression.streaming_decompression_error =>
               DS.print (normal, "Failed to decompress file index (RFI)");
         end;
      else
         case DS.header.size_filedata is
            when 0 =>
               left_over := DS.consume_index ("");
               DS.processed := True;
               return;
            when others => null;
         end case;

         declare
            decompress_success : Boolean;
            all_files : constant String :=
              ZST.Decompress (archive_saxs => DS.rvn_stmaxs,
                              data_length  => Natural (DS.header.size_filedata),
                              final_size   => ZST.File_Size (DS.header.flat_filedata),
                              successful   => decompress_success);
         begin
            if not decompress_success then
               DS.print (normal, "Failed to decompress file index block.");
            else
               left_over := DS.consume_index (all_files);
               if left_over > 0 then
                  DS.print (normal, "Index consumption left data over unexpectedly.");
               end if;
            end if;
         end;
      end if;
      DS.processed := True;
   end retrieve_file_index;


   ------------------------------------------------------------------------------------------
   --  FBString_to_File_Block
   ------------------------------------------------------------------------------------------
   function FBString_to_File_Block (Source : FBString) return Scanned_File_Block
   is
      result : Scanned_File_Block;

      function str_to_8bits  (index : Natural) return Natural;
      function str_to_16bits (index : Natural) return Natural;
      function str_to_32bits (index : Natural) return Natural;
      function str_to_64bits (index : Natural) return filetime;

      function str_to_8bits (index : Natural) return Natural is
      begin
         return Natural (Character'Pos (Source (index)));
      end str_to_8bits;

      function str_to_16bits (index : Natural) return Natural
      is
         result  : Natural;
         hi_byte : constant Natural := Natural (Character'Pos (Source (index)));
         lo_byte : constant Natural := Natural (Character'Pos (Source (index + 1)));
      begin
         case System.Default_Bit_Order is
            when System.Low_Order_First =>
               --  Little Endian
               result := hi_byte + (lo_byte * 256);
            when System.High_Order_First =>
               --  Big Endian
               result := lo_byte + (hi_byte * 256);
         end case;
         return result;
      end str_to_16bits;

      function str_to_32bits (index : Natural) return Natural
      is
         result  : Natural;
         A_byte : constant Natural := Natural (Character'Pos (Source (index)));
         B_byte : constant Natural := Natural (Character'Pos (Source (index + 1)));
         C_byte : constant Natural := Natural (Character'Pos (Source (index + 2)));
         D_byte : constant Natural := Natural (Character'Pos (Source (index + 3)));
      begin
         case System.Default_Bit_Order is
            when System.Low_Order_First =>
               --  Little Endian
               result := A_byte + (B_byte * 256) + (C_byte * 65_536) + (D_byte * 16_777_216);
            when System.High_Order_First =>
               --  Big Endian
               result := D_byte + (C_byte * 256) + (B_byte * 65_536) + (A_byte * 16_777_216);
         end case;
         return result;
      end str_to_32bits;

      function str_to_64bits (index : Natural) return filetime
      is
         result : filetime := 0;
         multiplier : filetime;
         bytex : Natural;
         spos  : Natural := 0;
      begin
         case System.Default_Bit_Order is
            when System.Low_Order_First =>
               --  Little Endian
               for bitndx in 0 .. 7 loop
                  bytex := Character'Pos (Source (index + spos));
                  if bytex > 0 then
                     multiplier := 2 ** (8 * bitndx);
                     result := result + filetime (bytex) * multiplier;
                  end if;
                  spos := spos + 1;
               end loop;

            when System.High_Order_First =>
               --  Big Endian
               for bitndx in reverse 0 .. 7 loop
                  bytex := Character'Pos (Source (index + spos));
                  if bytex > 0 then
                     multiplier := 2 ** (8 * bitndx);
                     result := result + filetime (bytex) * multiplier;
                  end if;
                  spos := spos + 1;
               end loop;
         end case;
         return result;
      end str_to_64bits;
   begin
      --  result.filename populated later
      result.blake_sum    := Source (Source'First .. Source'First + 31);
      result.modified_sec := str_to_64bits (Source'First + 32);
      result.modified_ns  := nanoseconds (str_to_32bits (Source'First + 40));
      result.index_owner  := owngrp_count (Character'Pos (Source (Source'First + 44)));
      result.index_group  := owngrp_count (Character'Pos (Source (Source'First + 45)));
      result.type_of_file := file_type'Val (Character'Pos (Source (Source'First + 46)));
      result.file_size_tb := size_type (Character'Pos (Source (Source'First + 47))) * (2 ** 32) +
                             size_type (str_to_32bits (Source'First + 48));
      result.file_perms   := permissions (str_to_16bits (Source'First + 52));
      result.link_length  := max_path (str_to_16bits (Source'First + 54));
      result.index_parent := index_type (str_to_16bits (Source'First + 56));
      result.directory_id := index_type (str_to_16bits (Source'First + 58));
      result.fname_length := max_fname (str_to_8bits (Source'First + 60));

      return result;
   end FBString_to_File_Block;


   ------------------------------------------------------------------------------------------
   --  extract_filename
   ------------------------------------------------------------------------------------------
   function extract_filename (sfb : Scanned_File_Block) return String
   is
      last_char : constant Natural := A_filename'First + Natural (sfb.fname_length) - 1;
   begin
      return sfb.filename (A_filename'First .. last_char);
   end extract_filename;


   ------------------------------------------------------------------------------------------
   --  trim_trailing_zeros
   ------------------------------------------------------------------------------------------
   function trim_trailing_zeros (full_string : String) return String
   is
      first_zero : Natural;
      pattern    : constant String (1 .. 1) := (others => Character'Val (0));
   begin
      first_zero := ASF.Index (Source  => full_string, Pattern => pattern);
      if first_zero = full_string'First then
         return "";
      elsif first_zero > full_string'First then
         return full_string (full_string'First .. first_zero - 1);
      end if;
      return full_string;
   end trim_trailing_zeros;


   ------------------------------------------------------------------------------------------
   --  extract_manifest
   ------------------------------------------------------------------------------------------
   function extract_manifest
     (DS           : in out DArc;
      file_list    : in out file_records.Vector;
      install_root : String) return Boolean
   is
      function set_root return String is
      begin
         if install_root = "" then
            return "/";
         end if;
         declare
            testdir : constant String := Unix.real_path (install_root);
         begin
            if testdir = "" or else testdir = "/"
            then
               return "/";
            end if;
            return testdir & "/";
         end;
      end set_root;

      error_encountered : Boolean := False;
      install_prefix    : constant String := set_root;

      procedure scan (position : file_block_crate.Cursor)
      is
         block : Scanned_File_Block renames file_block_crate.Element (position);

         function get_fullpath (index_parent : index_type; filename : String) return String
         is
            parent : constant Natural := Natural (index_parent);
         begin
            if parent = 0 then
               return filename;
            end if;
            return ASU.To_String (DS.folders.Element (parent).directory) & "/" & filename;
         end get_fullpath;
      begin
         if block.type_of_file /= directory then
            declare
               filename : constant String := extract_filename (block);
               fullpath : constant String := get_fullpath (block.index_parent, filename);
               tray     : file_record;
            begin
               tray.path := ASU.To_Unbounded_String (install_prefix & fullpath);
               tray.digest := Blake_3.hex (block.blake_sum);
               file_list.Append (tray);
            end;
         end if;
      exception
         when Constraint_Error =>
            error_encountered := True;
      end scan;
   begin
      file_list.Clear;
      if not DS.processed then
         DS.retrieve_file_index;
      end if;
      DS.files.Iterate (scan'Access);
      return not error_encountered;
   end extract_manifest;


   ------------------------------------------------------------------------------------------
   --  print_manifest
   ------------------------------------------------------------------------------------------
   procedure print_manifest
     (DS : in out DArc;
      show_b3sum : Boolean := False;
      show_attr  : Boolean := False;
      indent     : Natural)
   is
      spacer : constant String (1 .. indent) := (others => ' ');

      procedure print (position : file_block_crate.Cursor)
      is
         block : Scanned_File_Block renames file_block_crate.Element (position);

         function get_fullpath (index_parent : index_type; filename : String) return String
         is
            parent : constant Natural := Natural (index_parent);
         begin
            if parent = 0 then
               return filename;
            end if;
            return ASU.To_String (DS.folders.Element (parent).directory) & "/" & filename;
         end get_fullpath;
      begin
         if block.type_of_file /= directory then
            declare
               filename : constant String := extract_filename (block);
               fullpath : constant String := get_fullpath (block.index_parent, filename);
            begin
               if show_b3sum then
                  DS.print (normal, Blake_3.hex (block.blake_sum) & " " & fullpath);
               elsif show_attr then
                  DS.print (normal,
                            Unix.display_permissions (block.file_perms, block.type_of_file)
                            & verbose_display_owngrp (DS.owners.Element (block.index_owner).name)
                            & verbose_display_owngrp (DS.groups.Element (block.index_group).name)
                            & verbose_display_filesize (block.file_size_tb)
                            & " "
                            & Unix.format_file_time (block.modified_sec)
                            & " "
                            & fullpath);
               else
                  DS.print (normal, spacer & fullpath);
               end if;
            end;
         end if;
      exception
         when Constraint_Error =>
            DS.print (normal, "CORRUPTION");
            DS.print (normal, "Filename    : " & extract_filename (block));
            DS.print (normal, "index_parent: " & block.index_parent'Img);
            DS.print (normal, "file type   : " & block.type_of_file'Img);
      end print;
   begin
      if not DS.processed then
         DS.retrieve_file_index;
      end if;
      DS.files.Iterate (print'Access);
   end print_manifest;


   ------------------------------------------------------------------------------------------
   --  write_manifest_to_file
   ------------------------------------------------------------------------------------------
   procedure write_manifest_to_file
     (DS         : in out DArc;
      show_b3sum : Boolean := False;
      show_attr  : Boolean := False;
      filepath   : String)
   is
      output_handle : TIO.File_Type;

      procedure print (position : file_block_crate.Cursor)
      is
         block : Scanned_File_Block renames file_block_crate.Element (position);
      begin
         if block.type_of_file /= directory then
            declare
               parent : constant Positive := Positive (block.index_parent);
               fullpath : constant String := ASU.To_String (DS.folders.Element (parent).directory)
                 & "/" & extract_filename (block);
            begin
               if show_b3sum then
                  TIO.Put_Line (output_handle, Blake_3.hex (block.blake_sum) & " " & fullpath);
               elsif show_attr then
                  TIO.Put_Line (output_handle,
                            Unix.display_permissions (block.file_perms, block.type_of_file)
                            & verbose_display_owngrp (DS.owners.Element (block.index_owner).name)
                            & verbose_display_owngrp (DS.groups.Element (block.index_group).name)
                            & verbose_display_filesize (block.file_size_tb)
                            & " "
                            & Unix.format_file_time (block.modified_sec)
                            & " "
                            & fullpath);
               else
                  TIO.Put_Line (output_handle, fullpath);
               end if;
            end;
         end if;
      end print;
   begin
      if not DS.processed then
         DS.retrieve_file_index;
      end if;
      begin
         TIO.Create (File => output_handle,
                     Mode => TIO.Out_File,
                     Name => filepath);
         DS.files.Iterate (print'Access);
         TIO.Close (output_handle);
      exception
         when others =>
            DS.print (normal, "Encountered error creating " & filepath & " file.");
      end;
   end write_manifest_to_file;


   ------------------------------------------------------------------------------------------
   --  retrieve_link_target
   ------------------------------------------------------------------------------------------
   function retrieve_link_target (DS : in out DArc; link_len : max_path) return String
   is
   begin
      if link_len = 0 then
         return "";
      end if;
      declare
         link : String (1 .. Natural (link_len));
      begin
         for x in link'Range loop
            link (x) := DS.links.Element (DS.link_index);
            DS.link_index := DS.link_index + 1;
         end loop;
         return link;
      end;
   end retrieve_link_target;


   ------------------------------------------------------------------------------------------
   --  extract_archive
   ------------------------------------------------------------------------------------------
   function extract_archive
     (DS            : in out DArc;
      top_directory : String;
      set_owners    : Boolean;
      set_perms     : Boolean;
      set_modtime   : Boolean;
      skip_scripts  : Boolean;
      upgrading     : Boolean;
      extract_log   : TIO.File_Type) return Boolean
   is
      procedure extract (position : file_block_crate.Cursor);
      procedure second_pass (position : file_block_crate.Cursor);
      procedure make_directory (directory_id : Positive);
      procedure make_symlink (link_path : String; link_len : max_path);
      procedure make_hardlink (duplicate : String; link_len : max_path);
      procedure make_fifo (fifo_path : String; perms : permissions);
      function absolute_path (parent_dir : index_type; file_name : String) return String;


      good_extraction : Boolean := True;
      metadata_tree   : ThickUCL.UclTree;
      scripts_index   : ThickUCL.object_index := 0;
      meta_scripts    : Boolean := False;
      interpreter     : constant String := Misc.get_interpreter;


      procedure extract (position : file_block_crate.Cursor)
      is
         block        : Scanned_File_Block renames file_block_crate.Element (position);
         errcode      : Unix.metadata_rc;
         block_uid    : owngrp_id;
         block_gid    : owngrp_id;
         valid_owngrp : Boolean;
         these_perms  : permissions := block.file_perms;
         file_path    : constant String := absolute_path (parent_dir => block.index_parent,
                                                          file_name  => extract_filename (block));

         use type Unix.metadata_rc;
      begin
         if DS.fail_init then
            return;
         end if;
         block_uid := DS.owners.Element (block.index_owner).id;
         block_gid := DS.groups.Element (block.index_group).id;
         valid_owngrp := (block_uid /= id_not_found) and then (block_gid /= id_not_found);
         if not set_perms then
            these_perms := rwx_filter (block.file_perms);
         end if;
         case block.type_of_file is
            when directory =>
               make_directory (Positive (block.directory_id));
            when regular =>
               Unix.delete_file_if_it_exists (file_path);
               DS.extract_regular_file (file_path => file_path, file_len  => block.file_size_tb);
            when symlink =>
               make_symlink (link_path => file_path, link_len => block.link_length);
            when hardlink =>
               make_hardlink (duplicate => file_path, link_len => block.link_length);
            when fifo =>
               make_fifo (fifo_path => file_path, perms => block.file_perms);
            when unsupported =>
               good_extraction := False;
         end case;
         case block.type_of_file is
            when directory | unsupported =>
               --  Update directory metadata on second pass
               null;
            when others =>
               errcode := Unix.adjust_metadata
                 (path         => file_path,
                  reset_owngrp => set_owners and valid_owngrp,
                  reset_perms  => True,
                  reset_mtime  => set_modtime,
                  type_of_file => block.type_of_file,
                  new_uid      => block_uid,
                  new_gid      => block_gid,
                  new_perms    => these_perms,
                  new_m_secs   => block.modified_sec,
                  new_m_nano   => block.modified_ns);
               if errcode > 0 then
                  DS.print (normal, Unix.metadata_error (errcode));
               end if;
         end case;
      end extract;

      procedure second_pass (position : file_block_crate.Cursor)
      is
         block : Scanned_File_Block renames file_block_crate.Element (position);
         errcode      : Unix.metadata_rc;
         block_uid    : owngrp_id;
         block_gid    : owngrp_id;
         valid_owngrp : Boolean;
         these_perms  : permissions := block.file_perms;
         file_path    : constant String := absolute_path (parent_dir => block.index_parent,
                                                          file_name  => extract_filename (block));

         use type Unix.metadata_rc;
      begin
         block_uid := DS.owners.Element (block.index_owner).id;
         block_gid := DS.groups.Element (block.index_group).id;
         valid_owngrp := (block_uid /= id_not_found) and then (block_gid /= id_not_found);
         if not set_perms then
            these_perms := rwx_filter (block.file_perms);
         end if;
         case block.type_of_file is
            when directory =>
               errcode := Unix.adjust_metadata
                 (path         => file_path,
                  reset_owngrp => set_owners and valid_owngrp,
                  reset_perms  => True,
                  reset_mtime  => set_modtime,
                  type_of_file => block.type_of_file,
                  new_uid      => block_uid,
                  new_gid      => block_gid,
                  new_perms    => these_perms,
                  new_m_secs   => block.modified_sec,
                  new_m_nano   => block.modified_ns);
               if errcode > 0 then
                  DS.print (normal, Unix.metadata_error (errcode));
               end if;
            when others =>
               null;
         end case;
      end second_pass;

      procedure make_directory (directory_id : Positive)
      is
         full_path : constant String :=
           Misc.join_path (top_directory,
                           ASU.To_String (DS.folders.Element (directory_id).directory));
      begin
         if DIR.Exists (full_path) then
            case DIR.Kind (full_path) is
               when DIR.Directory =>
                  DS.print (verbose, "Skip directory creation; " & full_path & " already exists.");
                  return;
               when DIR.Ordinary_File | DIR.Special_File =>
                  begin
                     DIR.Delete_File (full_path);
                     DS.print (verbose, "Deleted file with same path as needed directory.");
                  exception
                     when DIR.Use_Error =>
                        DS.print (normal, "Deletion of " & full_path & " file unsupported.");
                        good_extraction := False;
                        return;
                     when DIR.Name_Error =>
                        DS.print (normal, "Deletion of " & full_path & " failed (unknown reason)");
                        good_extraction := False;
                        return;
                  end;
            end case;
         end if;
         DIR.Create_Directory (New_Directory => full_path);
         DS.print (verbose, "Extracted [D] " & full_path);
      exception
         when DIR.Use_Error =>
            DS.print (normal, "Extract/create " & full_path & " directory unsupported");
            good_extraction := False;
         when DIR.Name_Error =>
            DS.print (normal, "Failed to Extract/create " & full_path & " directory");
            good_extraction := False;
      end make_directory;

      procedure make_symlink (link_path : String; link_len : max_path)
      is
         link_target : constant String := DS.retrieve_link_target (link_len);
         linkchar : Unix.File_Characteristics;
      begin
         linkchar := Unix.get_charactistics (link_path);
         if not linkchar.error then
            begin
               DS.print (verbose, link_path & " already exists (will delete).");
               DIR.Delete_File (link_path);
            exception
               when DIR.Use_Error =>
                  DS.print (normal, "File " & link_target & " deletion unsupported");
                  good_extraction := False;
                  return;
               when DIR.Name_Error =>
                  DS.print (normal, "Failed to delete " & link_target & " file");
                  good_extraction := False;
                  return;
            end;
         end if;

         if Unix.create_symlink (actual_file    => link_target,
                                 link_to_create => link_path)
         then
            DS.print (verbose, "Extracted [S] " & link_path & " => " & link_target);
         else
            DS.print (normal, "Failed to extract symlink " & link_path & " => " & link_target);
            good_extraction := False;
         end if;
      end make_symlink;

      procedure make_hardlink (duplicate : String; link_len : max_path)
      is
         source : constant String := Misc.join_path (top_directory,
                                                     DS.retrieve_link_target (link_len));
      begin
         DS.prepare_for_overwrite (duplicate);
         if Unix.create_hardlink (actual_file => source,
                                  destination => duplicate)
         then
            DS.print (verbose, "Extracted [H] " & duplicate & " => " & source);
         else
            DS.print (normal, "Failed to extract hardlink " & duplicate & " => " & source);
            good_extraction := False;
         end if;
      end make_hardlink;

      procedure make_fifo (fifo_path : String; perms : permissions)
      is
         fifo_perms : permissions := perms;
         fifochar : Unix.File_Characteristics;
      begin
         fifochar := Unix.get_charactistics (fifo_path);
         if not fifochar.error then
            DS.print (verbose, fifo_path & " already exists (will delete).");
            if not Unix.unlink_file (fifo_path) then
               DS.print (normal, "Failed to delete " & fifo_path & " file");
               good_extraction := False;
               return;
            end if;
         end if;
         if not set_perms then
            fifo_perms := rwx_filter (perms);
         end if;
         if Unix.create_fifo (fifo_path, fifo_perms) then
            DS.print (verbose, "Extracted [F] " & fifo_path);
         else
            DS.print (normal, "Failed to create fifo file " & fifo_path);
            good_extraction := False;
         end if;
      end make_fifo;

      function absolute_path (parent_dir : index_type; file_name : String) return String
      is
      begin
         if parent_dir = 0 then
            return Misc.join_path (top_directory, file_name);
         else
            return Misc.join_path
              (top_directory,
               ASU.To_String (DS.folders.Element (Positive (parent_dir)).directory) &
                 "/" & file_name);
         end if;
      end absolute_path;

      use type SIO.Count;
   begin
      if top_directory = "" then
         DS.print (normal, "top_directory is blank.  Perhaps '.' or '/' was intended?");
         return False;
      end if;
      if not Unix.file_is_writable (top_directory) then
         DS.print (normal, "You do not have permission to write to " & top_directory);
         return False;
      end if;

      if not skip_scripts then
         DS.populate_metadata_tree (metadata_tree);
         meta_scripts := scripts_key_exists (metadata_tree, scripts_index);
         if meta_scripts then
            if phase_scripts_exists (metadata_tree, scripts_index, "pre-install") then
               execute_bourne_scripts
                 (tree          => metadata_tree,
                  scripts_index => scripts_index,
                  phase_key     => "pre-install",
                  root_dir      => top_directory,
                  interpreter   => interpreter,
                  upgrading     => upgrading,
                  extract_log   => extract_log);
            end if;
            if phase_scripts_exists (metadata_tree, scripts_index, "pre-install-lua") then
               execute_lua_scripts
                 (tree          => metadata_tree,
                  scripts_index => scripts_index,
                  phase_key     => "pre-install-lua",
                  root_dir      => top_directory,
                  upgrading     => upgrading,
                  extract_log   => extract_log);
            end if;
         end if;
      end if;

      if not DS.processed then
         DS.retrieve_file_index;
      end if;
      if SIO.Index (DS.rvn_handle) /= DS.b4_index then
         DS.print (debug, "Stream index at" & SIO.Index (DS.rvn_handle)'Img & ", setting to " &
                     DS.b4_index'Img);
         SIO.Set_Index (DS.rvn_handle, DS.b4_index);
      end if;
      DS.files.Iterate (extract'Access);
      if DS.fail_init then
         good_extraction := False;
         return good_extraction;
      end if;
      DS.files.Iterate (second_pass'Access);

      if meta_scripts then
         if phase_scripts_exists (metadata_tree, scripts_index, "post-install") then
            execute_bourne_scripts
              (tree          => metadata_tree,
               scripts_index => scripts_index,
               phase_key     => "post-install",
               root_dir      => top_directory,
               interpreter   => interpreter,
               upgrading     => upgrading,
               extract_log   => extract_log);
         end if;
         if phase_scripts_exists (metadata_tree, scripts_index, "post-install-lua") then
            execute_lua_scripts
              (tree          => metadata_tree,
               scripts_index => scripts_index,
               phase_key     => "post-install-lua",
               root_dir      => top_directory,
               upgrading     => upgrading,
               extract_log   => extract_log);
         end if;
      end if;

      return good_extraction;
   end extract_archive;


   ------------------------------------------------------------------------------------------
   --  rwx_filter
   ------------------------------------------------------------------------------------------
   function rwx_filter (perms : permissions) return permissions
   is
      rwx : constant permissions := 2#0000000111111111#;
   begin
      return (perms and rwx);
   end rwx_filter;


   ------------------------------------------------------------------------------------------
   --  extract_regular_file
   ------------------------------------------------------------------------------------------
   procedure extract_regular_file (DS : in out DArc; file_path : String; file_len : size_type)
   is
      decomp_worked : Boolean;
   begin
      if DS.rolled_up then
         --  This is the first time this procedure has been run.  Get first data chunk.
         --  Originally it checked if compressed size was less than 256 Kb, but because
         --  NetBSD has a tiny stack, the one-shot decompression has been limited to 256 Kb flat.
         if DS.header.flat_archive > exabytes (KB256) then
            --  streaming decompression
            begin
               DS.expander.Initialize (input_stream => DS.rvn_stmaxs);
               DS.call_again := DS.expander.Get_Uncompressed_Data (DS.buffer);
               DS.print (debug, "Streaming archive decompression block 1 successful.");
            exception
               when ZST.Streaming_Decompression.streaming_decompression_initialization =>
                  DS.print (normal, "Failed to initialize streaming decompression.");
                  DS.fail_init := True;
                  return;
               when ZST.Streaming_Decompression.streaming_decompression_error =>
                  DS.print (normal, "Failed to decompress archive (needs initialization?)");
                  DS.fail_init := True;
                  return;
            end;
         elsif DS.header.flat_archive = 0 then
            DS.print (debug, "Uncompressed archive is zero-byte; skip decompression step");
            DS.buffer := ASU.Null_Unbounded_String;
         else
            --  single-shot decompression
            DS.buffer := ASU.To_Unbounded_String
              (ZST.Decompress (archive_saxs => DS.rvn_stmaxs,
                               data_length  => Natural (DS.header.size_archive),
                               final_size   => ZST.File_Size (DS.header.flat_archive),
                               successful   => decomp_worked));
            DS.print (debug, "One shot archive decompression successful : " & decomp_worked'Img);
            if not decomp_worked then
               DS.print (normal, "Failed to decompress archive in one pass.");
               DS.print (verbose, ASU.To_String (DS.buffer));
               DS.fail_init := True;
               return;
            end if;
         end if;
         DS.buf_remain := ASU.Length (DS.buffer);
         DS.rolled_up := False;
         if ASU.Length (DS.buffer) > 0 then
            DS.buf_arrow := 1;
         end if;
      end if;

      if file_len = 0 then
         DS.direct_file_creation (file_path, "");
         DS.print (verbose, "Extracted [R] " & file_path);
      elsif size_type (DS.buf_remain) >= file_len then
         --  Write file in a single pass
         declare
            high : constant Natural := DS.buf_arrow + Natural (file_len) - 1;
         begin
            DS.direct_file_creation (target_file => file_path,
                                     contents    => ASU.Slice (Source => DS.buffer,
                                                               Low    => DS.buf_arrow,
                                                               High   => high));
            DS.print (verbose, "Extracted [R] " & file_path);
            DS.buf_remain := DS.buf_remain - Natural (file_len);
            DS.buf_arrow := high + 1;
         end;
      else
         --  There's not enough data in the buffer.  We'll have to decompress more data and
         --  do muliple writes to the output file.

         declare
            high          : Natural;
            new_file      : SIO.File_Type;
            local_stmaxs  : SIO.Stream_Access;
            left_to_write : size_type := file_len;
            read_block    : Natural := 1;

            procedure append_target_file
              (target_saxs : SIO.Stream_Access;
               plain_text : String)
            is
               recsize : constant Natural := plain_text'Length;
            begin
               declare
                  type tray is record
                     payload : String (1 .. recsize);
                  end record;
                  pragma Pack (tray);

                  data : tray;
               begin
                  data.payload := plain_text;
                  tray'Output (target_saxs, data);
               end;
            end append_target_file;

         begin
            DS.prepare_for_overwrite (file_path);
            SIO.Create (File => new_file,
                        Mode => SIO.Out_File,
                        Name => file_path);

            local_stmaxs := SIO.Stream (new_file);
            high := DS.buf_arrow + DS.buf_remain - 1;
            append_target_file (target_saxs => local_stmaxs,
                                plain_text  => ASU.Slice (Source => DS.buffer,
                                                          Low    => DS.buf_arrow,
                                                          High   => high));
            DS.print (debug, "First chunk of file written, chars:" & DS.buf_remain'Img);
            left_to_write := left_to_write - size_type (DS.buf_remain);
            DS.buf_arrow := 0;
            DS.buf_remain := 0;

            loop
               begin
                  read_block := read_block + 1;
                  if DS.call_again then
                     DS.call_again := DS.expander.Get_Uncompressed_Data (DS.buffer);
                     DS.print (debug, "Decompressed streaming block" & read_block'Img);
                     if not DS.call_again then
                        DS.expander.Finalize;
                        DS.print (debug, "That's the end of the zstandard frame.");
                     end if;
                  else
                     DS.print (normal, "Problem: There's no more data to decompress.");
                  end if;
               exception
                  when unexpected : others =>
                     DS.print (normal, "ERF/Charlie: " & EX.Exception_Information (unexpected));
               end;
               DS.buf_remain := ASU.Length (DS.buffer);
               if ASU.Length (DS.buffer) > 0 then
                  DS.buf_arrow := 1;
               end if;

               if size_type (DS.buf_remain) >= left_to_write then
                  high := DS.buf_arrow + Natural (left_to_write) - 1;
                  append_target_file (target_saxs => local_stmaxs,
                                      plain_text  => ASU.Slice (Source => DS.buffer,
                                                                Low    => DS.buf_arrow,
                                                                High   => high));
                  DS.print (debug, "Last chunk of file written, chars:" & left_to_write'Img);
                  SIO.Close (new_file);
                  DS.print (verbose, "Extracted [R] " & file_path);
                  DS.buf_remain := DS.buf_remain - Natural (left_to_write);
                  left_to_write := 0;
                  DS.buf_arrow  := high + 1;
                  exit;
               else
                  high := DS.buf_arrow + DS.buf_remain - 1;
                  append_target_file (target_saxs => local_stmaxs,
                                      plain_text  => ASU.Slice (Source => DS.buffer,
                                                                Low    => DS.buf_arrow,
                                                                High   => high));
                  DS.print (debug, "Next chunk of file written, chars:" & DS.buf_remain'Img);
                  left_to_write := left_to_write - size_type (DS.buf_remain);
                  DS.buf_arrow := high + 1;
                  DS.buf_remain := 0;
               end if;
            end loop;
         exception
            when IOX.Use_Error =>
               DS.print (normal, "Failed to open " & file_path & " for writing.");
            when dunno : others =>
               DS.print (normal, "ERF/Delta: " & EX.Exception_Information (dunno));
         end;

      end if;
   end extract_regular_file;


   ------------------------------------------------------------------------------------------
   --  prepare_for_overwrite
   ------------------------------------------------------------------------------------------
   procedure prepare_for_overwrite (DS : DArc; file_path : String) is
   begin
      if DIR.Exists (file_path) then
         if not Unix.file_is_writable (file_path) then
            --  attempt to set the write permissions
            DS.print (verbose, file_path & " exists but is currently unwritable.");
            if Unix.change_mode (file_path, 2#111_111_111#) then
               DS.print (verbose, "Mode change to 0777 successful");
               begin
                  DIR.Delete_File (file_path);
               exception
                  when others =>
                     DS.print (verbose, "Failed to delete " & file_path);
               end;
            else
               DS.print (verbose, "Mode change to 0777 failed.");
            end if;
         end if;
      end if;
   end prepare_for_overwrite;


   ------------------------------
   --  populate_metadata_tree  --
   ------------------------------
   procedure populate_metadata_tree (DS : in out DArc; tree : in out ThickUCL.UclTree)
   is
      metadata : constant String := DS.extract_metadata;
   begin
      begin
         ThickUCL.Files.parse_ucl_string (tree, metadata, "");
      exception
         when ThickUCL.Files.ucl_data_unparseable =>
            return;
      end;
   end populate_metadata_tree;


   --------------------------
   --  scripts_key_exists  --
   --------------------------
   function scripts_key_exists
     (tree : ThickUCL.UclTree;
      scripts_index : in out ThickUCL.object_index) return Boolean
   is
      scripts_key : constant String := "scripts";
   begin
      case tree.get_data_type (scripts_key) is
         when ThickUCL.data_object =>
            scripts_index := tree.get_index_of_base_ucl_object (scripts_key);
            return True;
         when others =>
            return False;
      end case;
   end scripts_key_exists;


   ----------------------------
   --  phase_scripts_exists  --
   ----------------------------
   function phase_scripts_exists
     (tree          : ThickUCL.UclTree;
      scripts_index : ThickUCL.object_index;
      phase_key     : String) return Boolean
   is
      vndx : ThickUCL.array_index;
      num_scripts : Natural;
   begin
      case tree.get_object_data_type (scripts_index, phase_key) is
         when ThickUCL.data_array =>
            vndx := tree.get_object_array (scripts_index, phase_key);
            num_scripts := tree.get_number_of_array_elements (vndx);
            if num_scripts > 0 then
               return True;
            end if;
         when others => null;
      end case;
      return False;
   end phase_scripts_exists;


   -----------------------
   --  get_meta_string  --
   -----------------------
   function get_meta_string (tree : ThickUCL.UclTree; data_key : String) return String is
   begin
      case tree.get_data_type (data_key) is
         when ThickUCL.data_string =>
            return tree.get_base_value (data_key);
         when others =>
            return data_key & "-not-found";
      end case;
   end get_meta_string;


   ------------------------------
   --  execute_bourne_scripts  --
   ------------------------------
   procedure execute_bourne_scripts
     (tree          : ThickUCL.UclTree;
      scripts_index : ThickUCL.object_index;
      phase_key     : String;
      root_dir      : String;
      interpreter   : String;
      upgrading     : Boolean;
      extract_log   : Ada.Text_IO.File_Type)
   is
      vndx : constant ThickUCL.array_index := tree.get_object_array (scripts_index, phase_key);
      num_scripts  : constant Natural := tree.get_number_of_array_elements (vndx);
      error_prefix : constant String := phase_key & " Bourne shell script number";
      msg_outfile  : constant String := Bourne.unique_msgfile_path;
      std_outfile  : constant String := Misc.new_filename (msg_outfile, Misc.ft_stdout);
      out_handle   : Ada.Text_IO.File_Type;
      z_namebase   : constant String := get_meta_string (tree, "namebase");
      z_subpackage : constant String := get_meta_string (tree, "subpackage");
      z_variant    : constant String := get_meta_string (tree, "variant");
      z_prefix     : constant String := get_meta_string (tree, "prefix");
      vndx2        : ThickUCL.object_index;
      script_good  : Boolean;

      function get_script (vndx2 : ThickUCL.object_index; index : Natural) return String is
      begin
         case tree.get_object_data_type (vndx2, "code") is
            when ThickUCL.data_string =>
               return tree.get_object_value (vndx2, "code");
            when ThickUCL.data_not_present =>
               script_good := False;
               SQW.emit_error (error_prefix & index'Img & " code missing");
            when others =>
               script_good := False;
               SQW.emit_error (error_prefix & index'Img & " code not a string");
         end case;
         return "";
      end get_script;

      function get_arguments (vndx2 : ThickUCL.object_index; index : Natural) return String is
      begin
         case tree.get_object_data_type (vndx2, "args") is
            when ThickUCL.data_string =>
               return tree.get_object_value (vndx2, "args");
            when ThickUCL.data_not_present =>
               script_good := False;
               SQW.emit_error (error_prefix & index'Img & " args missing");
            when others =>
               script_good := False;
               SQW.emit_error (error_prefix & index'Img & " args not a string");
         end case;
         return "";
      end get_arguments;
   begin
      begin
         Ada.Text_IO.Create (File => out_handle, Name => std_outfile);
      exception
         when others =>
            SQW.emit_error ("failed to open standard output file");
      end;
      for index in 0 .. num_scripts - 1 loop
         case tree.get_array_element_type (vndx, index) is
            when ThickUCL.data_object =>
               vndx2 := tree.get_array_element_object (vndx, index);
               script_good := True;
               declare
                  script  : constant String := get_script (vndx2, index);
                  args    : constant String := get_arguments (vndx2, index);
                  success : Boolean;
               begin
                  if script_good then
                     Bourne.run_shell_script
                       (namebase    => z_namebase,
                        subpackage  => z_subpackage,
                        variant     => z_variant,
                        prefix      => z_prefix,
                        root_dir    => root_dir,
                        upgrading   => upgrading,
                        interpreter => interpreter,
                        script      => script,
                        arguments   => args,
                        msg_outfile => msg_outfile,
                        out_handle  => out_handle,
                        success     => success);
                     if not success then
                        SQW.emit_notice (error_prefix & index'Img & " failed");
                     end if;
                  end if;
               end;
            when others => SQW.emit_error (error_prefix & index'Img & " not of type object");
         end case;
      end loop;
      Ada.Text_IO.Close (out_handle);
      Bourne.show_post_run_messages (msg_outfile, z_namebase, z_subpackage, z_variant, extract_log);
   end execute_bourne_scripts;


   ---------------------------
   --  execute_lua_scripts  --
   ---------------------------
   procedure execute_lua_scripts
     (tree          : ThickUCL.UclTree;
      scripts_index : ThickUCL.object_index;
      phase_key     : String;
      root_dir      : String;
      upgrading     : Boolean;
      extract_log   : Ada.Text_IO.File_Type)
   is
      vndx : constant ThickUCL.array_index := tree.get_object_array (scripts_index, phase_key);
      num_scripts  : constant Natural := tree.get_number_of_array_elements (vndx);
      error_prefix : constant String := phase_key & " Lua script number";
      msg_outfile  : constant String := Lua.unique_msgfile_path;
      std_outfile  : constant String := Misc.new_filename (msg_outfile, Misc.ft_lua);
      out_handle   : Ada.Text_IO.File_Type;
      z_namebase   : constant String := get_meta_string (tree, "namebase");
      z_subpackage : constant String := get_meta_string (tree, "subpackage");
      z_variant    : constant String := get_meta_string (tree, "variant");
      z_prefix     : constant String := get_meta_string (tree, "prefix");
      vndx2        : ThickUCL.object_index;
      script_good  : Boolean;

      function get_script (vndx2 : ThickUCL.object_index; index : Natural) return String is
      begin
         case tree.get_object_data_type (vndx2, "code") is
            when ThickUCL.data_string =>
               return tree.get_object_value (vndx2, "code");
            when ThickUCL.data_not_present =>
               script_good := False;
               SQW.emit_error (error_prefix & index'Img & " code missing");
            when others =>
               script_good := False;
               SQW.emit_error (error_prefix & index'Img & " code not a string");
         end case;
         return "";
      end get_script;

      function get_arguments (vndx2 : ThickUCL.object_index; index : Natural) return String is
      begin
         case tree.get_object_data_type (vndx2, "args") is
            when ThickUCL.data_string =>
               declare
                  args : String := tree.get_object_value (vndx2, "args");
               begin
                  for x in args'Range loop
                     if args (x) = ' ' then
                        args (x) := Character'Val (0);
                     end if;
                  end loop;
                  return args;
               end;
            when ThickUCL.data_not_present =>
               script_good := False;
               SQW.emit_error (error_prefix & index'Img & " args missing");
            when others =>
               script_good := False;
               SQW.emit_error (error_prefix & index'Img & " args not a string");
         end case;
         return "";
      end get_arguments;
   begin
      begin
         Ada.Text_IO.Create (File => out_handle, Name => std_outfile);
      exception
         when others =>
            SQW.emit_error ("failed to open standard output file");
      end;
      for index in 0 .. num_scripts - 1 loop
         case tree.get_array_element_type (vndx, index) is
            when ThickUCL.data_object =>
               vndx2 := tree.get_array_element_object (vndx, index);
               script_good := True;
               declare
                  script  : constant String := get_script (vndx2, index);
                  args    : constant String := get_arguments (vndx2, index);
                  success : Boolean;
               begin
                  if script_good then
                     Lua.run_lua_script
                       (namebase    => z_namebase,
                        subpackage  => z_subpackage,
                        variant     => z_variant,
                        prefix      => z_prefix,
                        root_dir    => root_dir,
                        upgrading   => upgrading,
                        script      => script,
                        arg_chain   => args,
                        msg_outfile => msg_outfile,
                        out_handle  => out_handle,
                        success     => success);
                     if not success then
                        SQW.emit_error (error_prefix & index'Img & " failed");
                     end if;
                  end if;
               end;
            when others => SQW.emit_error (error_prefix & index'Img & " not of type object");
         end case;
      end loop;
      Ada.Text_IO.Close (out_handle);
      Lua.show_post_run_messages (msg_outfile, z_namebase, z_subpackage, z_variant, extract_log);
   end execute_lua_scripts;


   ------------------------------------------------------------------------------------------
   --  print_magic_block
   ------------------------------------------------------------------------------------------
   procedure print_magic_block (DS : DArc)
   is
   begin
      DS.print (normal, "RVN format version :" & DS.header.version'Img);
      DS.print (normal, "   filename blocks :" & DS.header.fname_blocks'Img);
      DS.print (normal, "     groups blocks :" & DS.header.num_groups'Img);
      DS.print (normal, "     owners blocks :" & DS.header.num_owners'Img);
      DS.print (normal, "      links blocks :" & DS.header.link_blocks'Img);
      DS.print (normal, "      dirs + files :" & DS.header.file_blocks'Img);
      DS.print (normal, "    metadata bytes :" & DS.header.flat_metadata'Img);
      DS.print (normal, "        compressed :" & DS.header.size_metadata'Img);
      DS.print (normal, "  file index bytes :" & DS.header.flat_filedata'Img);
      DS.print (normal, "        compressed :" & DS.header.size_filedata'Img);
      DS.print (normal, "     archive bytes :" & DS.header.flat_archive'Img);
      DS.print (normal, "        compressed :" & DS.header.size_archive'Img);
   end print_magic_block;

end Archive.Unpack;
