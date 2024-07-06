--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Ada.Directories;
with Archive.Communication;
with Archive.Unix;

package body Archive.JustExtract is

   package DIR renames Ada.Directories;
   package SQW renames Archive.Communication;
   package ZST renames Zstandard;

   ------------------------
   --  open_rvn_archive  --
   ------------------------
   procedure open_rvn_archive
     (DS            : in out DArc;
      rvn_archive   : String)
   is
      use type ZST.File_Size;
      use type SIO.Count;
   begin
      DS.valid := False;
      DS.level := silent;
      SQW.initialize (silent, Unix.not_connected);
      if DIR.Exists (rvn_archive) then
         case DIR.Kind (rvn_archive) is
            when DIR.Ordinary_File =>
               null;
            when others =>
               return;
         end case;
      else
         return;
      end if;

      if ZST.File_Size (DIR.Size (rvn_archive)) < (premier_block'Size / 8) then
         return;
      end if;

      SIO.Open (File => DS.rvn_handle,
                Mode => SIO.In_File,
                Name => rvn_archive);

      begin
         DS.rvn_stmaxs := SIO.Stream (DS.rvn_handle);
         premier_block'Read (DS.rvn_stmaxs, DS.header);
      exception
         when others =>
            return;
      end;

      if DS.header.magic_bytes /= magic then
         return;
      end if;

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

   -------------------------
   --  close_rvn_archive  --
   -------------------------
   procedure close_rvn_archive (DS : in out DArc)
   is
   begin
      if DS.valid then
         SIO.Close (DS.rvn_handle);
         DS.valid := False;
      end if;
   end close_rvn_archive;


   ------------------------
   --  extract_metadata  --
   ------------------------
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
                             final_size   => ZST.File_Size (DS.header.flat_metadata),
                             successful   => decompress_success);
   end extract_metadata;


end Archive.JustExtract;
