--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt


with Ada.Text_IO;
with Ada.Direct_IO;
with Ada.Directories;
with Ada.Unchecked_Conversion;
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Zstandard;

package body Archive.Unpack is

   package TIO renames Ada.Text_IO;
   package DIR renames Ada.Directories;
   package EX  renames Ada.Exceptions;
   package ASF renames Ada.Strings.Fixed;
   package ZST renames Zstandard;

   procedure open_rvn_archive
     (DS          : in out DArc;
      rvn_archive : String;
      verbosity   : info_level)
   is
      use type SIO.Count;
   begin
      DS.valid := False;
      DS.set_verbosity (verbosity);
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

      if Natural (DIR.Size (rvn_archive)) < File_Block'Size then
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
      DS.print (debug, "      number files :" & DS.header.file_blocks'Img);
      DS.print (debug, "    metadata bytes :" & DS.header.size_metadata'Img);
      DS.print (debug, "  file index bytes :" & DS.header.size_filedata'Img);
      DS.print (debug, "     archive bytes :" & DS.header.size_archive'Img);
      DS.print (debug, "             index :" & SIO.Index (DS.rvn_handle)'Img);

      DS.valid    := True;
      DS.b2_index := 33;
      DS.b3_index := DS.b2_index + SIO.Count (DS.header.size_metadata);
      DS.b4_index := DS.b3_index + SIO.Count (DS.header.size_filedata);

      DS.con_track.num_groups  := Natural (DS.header.num_groups);
      DS.con_track.num_owners  := Natural (DS.header.num_owners);
      DS.con_track.link_blocks := Natural (DS.header.link_blocks);
      DS.con_track.file_blocks := Natural (DS.header.file_blocks);

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
         if msg_level = debug then
            TIO.Put_Line ("DEBUG: " & message);
         else
            TIO.Put_Line (message);
         end if;
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
         Blitz.Write (File => out_handle,
                      Item => file_contents (contents));
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
      --  metadata is always starts on index of 32.
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
         plain_text : constant String := ZST.Decompress (archive_saxs => DS.rvn_stmaxs,
                                                         data_length  => Natural (metasize),
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
      if SIO.Index (DS.rvn_handle) /= DS.b2_index then
         SIO.Set_Index (DS.rvn_handle, DS.b2_index);
      end if;
      return ZST.Decompress (archive_saxs => DS.rvn_stmaxs,
                             data_length  => Natural (DS.header.size_metadata),
                             successful   => decompress_success);
   end extract_metadata;


   ------------------------------------------------------------------------------------------
   --  consume_index
   ------------------------------------------------------------------------------------------
   function consume_index (DS : in out DArc; index_data : String) return Natural
   is
      function sufficient_data (chars_needed : Natural) return Boolean;

      sindex     : Natural := index_data'First;
      data_left  : Natural := index_data'Length;
      num_groups : constant Natural := DS.con_track.num_groups;
      num_owners : constant Natural := DS.con_track.num_owners;
      num_links  : constant Natural := DS.con_track.link_blocks;
      num_files  : constant Natural := DS.con_track.file_blocks;
      fblk_size  : constant Natural := File_Block'Size / 8;

      subtype FBString is String (1 .. fblk_size);

      function sufficient_data (chars_needed : Natural) return Boolean is
      begin
         return chars_needed <= data_left;
      end sufficient_data;

      function FBString_to_File_Block is
        new Ada.Unchecked_Conversion (Source => FBString,
                                      Target => File_Block);
   begin
      --  Read in groups data
      for x in 1 .. num_groups loop
         if sufficient_data (ownergroup'Length) then
            declare
               data : ownergroup;
            begin
               data := index_data (sindex .. sindex + ownergroup'Length - 1);
               DS.groups.Append (data);
               sindex := sindex + ownergroup'Length;
               data_left := data_left - ownergroup'Length;
               DS.con_track.num_groups := DS.con_track.num_groups - 1;
               DS.print (debug, "Extract group: " & data);
            end;
         else
            return data_left;
         end if;
      end loop;

      --  Read in owners data
      for x in 1 .. num_owners loop
         if sufficient_data (ownergroup'Length) then
            declare
               data : ownergroup;
            begin
               data := index_data (sindex .. sindex + ownergroup'Length - 1);
               DS.owners.Append (data);
               sindex := sindex + ownergroup'Length;
               data_left := data_left - ownergroup'Length;
               DS.con_track.num_owners := DS.con_track.num_owners - 1;
               DS.print (debug, "Extract owner: " & data);
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
               DS.con_track.link_blocks := DS.con_track.link_blocks - 32;
               DS.print (debug, "Extract link block: " & link32);
            end;
         else
            return data_left;
         end if;
      end loop;

      --  Read in 320-byte file blocks
      for x in 1 .. num_files loop
         if sufficient_data (fblk_size) then
            declare
               data : File_Block;
               datastr : FBString;
            begin
               datastr := index_data (sindex .. sindex + FBString'Length - 1);
               sindex := sindex + FBString'Length;
               data_left := data_left - FBString'Length;
               data := FBString_to_File_Block (datastr);
               DS.files.Append (data);
               DS.con_track.file_blocks := DS.con_track.file_blocks - 1;
               DS.print (debug, "extract filename: " & ASF.Trim (data.filename_p1 &
                           data.filename_p2 & data.filename_p3 &
                           data.filename_p4, Ada.Strings.Both));
               DS.print (debug, "          b3 sum: " & data.blake_sum);
               DS.print (debug, "    type of file: " & data.type_of_file'Img);
               DS.print (debug, "       flat size:" & data.flat_size'Img);
               DS.print (debug, "    parent index:" & data.index_parent'Img);
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
   begin
      if SIO.Index (DS.rvn_handle) /= DS.b3_index then
         SIO.Set_Index (DS.rvn_handle, DS.b3_index);
      end if;

      if Natural (DS.header.size_filedata) > KB256 then
         DS.print (normal, "filedata > 256KB needs to be implemented");
      else
         declare
            decompress_success : Boolean;
            left_over : Natural;
            all_files : constant String :=
              ZST.Decompress (archive_saxs => DS.rvn_stmaxs,
                              data_length  => Natural (DS.header.size_filedata),
                              successful   => decompress_success);
         begin
            if not decompress_success then
               DS.print (normal, "Failed to decompress file index block.");
            else
               left_over := DS.consume_index (all_files);
               if left_over > 0 then
                  DS.print (normal, "All file consumption left data over unexpectedly.");
               end if;
            end if;
         end;
      end if;
   end retrieve_file_index;

end Archive.Unpack;
