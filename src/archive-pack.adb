--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Text_IO;
with Ada.Exceptions;
with Ada.IO_Exceptions;
with Ada.Directories;
with Ada.Direct_IO;
with Ada.Strings.Fixed;
with Archive.Unix;
with Archive.Dirent.Scan;
with Blake_3;

package body Archive.Pack is

   package TIO renames Ada.Text_IO;
   package DIR renames Ada.Directories;
   package EX  renames Ada.Exceptions;
   package IOX renames Ada.IO_Exceptions;
   package ASF renames Ada.Strings.Fixed;
   package UNX renames Archive.Unix;
   package SCN renames Archive.Dirent.Scan;
   package ZST renames Zstandard;

   ------------------------------------------------------------------------------------------
   --  integrate
   ------------------------------------------------------------------------------------------
   function integrate
     (top_level_directory : String;
      metadata_file       : String;
      manifest_file       : String;
      output_file         : String;
      verbosity           : info_level) return Boolean
   is
      metadata : Arc_Structure;
   begin
      metadata.set_verbosity (verbosity);
      metadata.record_directory (top_level_directory);

      if not metadata.able_to_write_rvn_archive (output_file) then
         return False;
      end if;

      if manifest_file /= "" then
         if not metadata.white_list.ingest_file_manifest (manifest_file => manifest_file,
                                                          top_directory => top_level_directory,
                                                          level         => verbosity)
         then
            return False;
         end if;
      end if;

      metadata.initialize_archive_file (output_file);
      metadata.scan_directory (top_level_directory, 0);
      metadata.finalize_archive_file;

      if metadata.serror then
         metadata.remove_archive_file (output_file);
         return False;
      end if;

      metadata.initialize_index_file (output_file);
      metadata.write_owngrp_blocks;                  --  block FA and FB
      metadata.write_link_block;                     --  block FC
      metadata.write_file_index_block;               --  block FD
      metadata.write_filename_block;                 --  block FE
      metadata.finalize_index_file;

      metadata.write_blank_header (output_file);     --  block 1
      metadata.write_metadata_block (metadata_file); --  block 2
      metadata.write_file_index_block (output_file); --  block 3
      metadata.write_archive_block (output_file);    --  block 4
      metadata.overwrite_header (output_file);

      metadata.remove_index_file (output_file);
      metadata.remove_archive_file (output_file);
      return True;
   end integrate;


   ------------------------------------------------------------------------------------------
   --  get_owner_index
   ------------------------------------------------------------------------------------------
   function get_owner_index (AS : in out Arc_Structure; owner : ownergroup) return one_byte
   is
   begin
      if not AS.owners.Contains (owner) then

         --  The owner has never been seen.
         --  We need to add it, but we are capped at 255
         if owngrp_count (AS.owners.Length) = owngrp_count'Last then
            raise Constraint_Error with "Archive cannot support more than 255 owners.";
         end if;
         AS.owners.Append (owner);
         AS.print (debug, "New owner " & trim_trailing_zeros (owner));
      end if;

      return one_byte (AS.owners.Find_Index (owner));
   end get_owner_index;


   ------------------------------------------------------------------------------------------
   --  get_group_index
   ------------------------------------------------------------------------------------------
   function get_group_index (AS : in out Arc_Structure; group : ownergroup) return one_byte
   is
   begin
      if not AS.groups.Contains (group) then

         --  The group has never been seen.
         --  We need to add it, but we are capped at 255
         if owngrp_count (AS.groups.Length) = owngrp_count'Last then
            raise Constraint_Error with "Archive cannot support more than 255 groups.";
         end if;
         AS.groups.Append (group);
         AS.print (debug, "New group " & trim_trailing_zeros (group));
      end if;

      return one_byte (AS.groups.Find_Index (group));
   end get_group_index;


   ------------------------------------------------------------------------------------------
   --  set_verbosity
   ------------------------------------------------------------------------------------------
   procedure set_verbosity (AS : in out Arc_Structure; level : info_level)
   is
   begin
      AS.level := level;
   end set_verbosity;


   ------------------------------------------------------------------------------------------
   --  print
   ------------------------------------------------------------------------------------------
   procedure print (AS : Arc_Structure; msg_level : info_level; message : String)
   is
      meets_criteria : constant Boolean := (msg_level <= AS.level);
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
   --  scan_directory
   ------------------------------------------------------------------------------------------
   procedure scan_directory
     (AS        : in out Arc_Structure;
      dir_path  : String;
      dir_index : index_type)
   is
      procedure walkdir   (position : SCN.dscan_crate.Cursor);
      procedure walkfiles (position : SCN.dscan_crate.Cursor);

      dirfiles  : SCN.dscan_crate.Vector;

      procedure walkdir (position : SCN.dscan_crate.Cursor)
      is
         item      : Archive.Dirent.Directory_Entity renames SCN.dscan_crate.Element (position);
         item_path : constant String := item.full_path;
         filename  : constant String := item.simple_name;
         features  : constant UNX.File_Characteristics := UNX.get_charactistics (item_path);
         new_block : File_Block;
      begin
         --  We only want true directories.  Symbolic links to directories are ignored.
         case features.ftype is
            when directory => null;
            when others => return;
         end case;

         if AS.white_list.whitelist_in_use then
            if not AS.white_list.directory_on_whitelist (item_path) then
               return;
            end if;
         end if;

         AS.dtrack := AS.dtrack + 1;
         AS.push_filename (filename);
         new_block.blake_sum    := null_sum;
         new_block.modified_sec := features.mtime;
         new_block.modified_ns  := features.mnsec;
         new_block.index_owner  := AS.get_owner_index (features.owner);
         new_block.index_group  := AS.get_group_index (features.group);
         new_block.type_of_file := directory;
         new_block.multiplier   := 0;
         new_block.flat_size    := 0;
         new_block.file_perms   := features.perms;
         new_block.link_length  := 0;
         new_block.index_parent := dir_index;
         new_block.directory_id := AS.dtrack;
         new_block.fname_length := max_fname (filename'Length);
         new_block.padding      := (others => 0);

         AS.files.Append (new_block);
         AS.print (debug, item_path & " (" & AS.dtrack'Img & ")");
         AS.print (debug, "owner =" & new_block.index_owner'Img & "  group =" &
                     new_block.index_group'Img);
         AS.print (debug, "perms =" & features.perms'Img & "   mod =" & features.mtime'Img);
         AS.print (verbose, "Record directory " & item_path);

         AS.scan_directory (item_path, AS.dtrack);
      exception
         when failed : others =>
            AS.print (normal, "walkdir exception => " & EX.Exception_Information (failed) &
                        "  file: " & item_path);
      end walkdir;

      procedure walkfiles (position : SCN.dscan_crate.Cursor)
      is
         item      : Archive.Dirent.Directory_Entity renames SCN.dscan_crate.Element (position);
         item_path : constant String := item.full_path;
         filename  : constant String := item.simple_name;
         features  : constant UNX.File_Characteristics := UNX.get_charactistics (item_path);
      begin
         --  Reject directories, but accept symlinks to directories
         case features.ftype is
            when directory => return;
            when unsupported =>
               AS.print
                 (normal, "NOTICE: Unsupported file " & item_path & " ignored.");
               return;
            when others =>
               if AS.white_list.whitelist_in_use then
                  if not AS.white_list.file_on_whitelist (item_path) then
                     return;
                  end if;
               end if;
         end case;

         declare
            --  Blake3 sums for regular files and hardlinks
            --  The first hardlink is written as a regular file.
            new_block : File_Block;

            use type DIR.File_Size;
         begin
            AS.push_filename (filename);
            new_block.modified_sec := features.mtime;
            new_block.modified_ns  := features.mnsec;
            new_block.index_owner  := AS.get_owner_index (features.owner);
            new_block.index_group  := AS.get_group_index (features.group);
            new_block.file_perms   := features.perms;
            new_block.index_parent := dir_index;
            new_block.directory_id := 0;
            new_block.fname_length := max_fname (filename'Length);
            new_block.padding      := (others => 0);

            case features.ftype is
               when directory | unsupported => null;   --  impossible
               when symlink =>
                  new_block.blake_sum    := null_sum;
                  new_block.type_of_file := symlink;
                  new_block.multiplier   := 0;
                  new_block.flat_size    := 0;
                  declare
                     target : constant String := UNX.link_target (item_path);
                  begin
                     new_block.link_length := max_path (target'Length);
                     AS.push_link (target);
                  end;
               when fifo =>
                  new_block.blake_sum    := null_sum;
                  new_block.type_of_file := fifo;
                  new_block.multiplier   := 0;
                  new_block.flat_size    := 0;
                  new_block.link_length  := 0;
               when regular =>
                  begin
                     new_block.blake_sum := Blake_3.file_digest (item_path);
                     if AS.level = debug then
                        AS.print (debug, Blake_3.hex (new_block.blake_sum) & " " & filename);
                     end if;
                  exception
                     when IOX.Use_Error =>
                        AS.serror := True;
                        new_block.blake_sum := null_sum;
                        AS.print (normal, "FATAL: Insufficient permissions to read " &
                                    item_path & "; This directory cannot be archived.");
                  end;
                  new_block.type_of_file := regular;
                  new_block.multiplier   := size_multi (DIR.Size (item_path) / 2 ** 32);
                  new_block.flat_size    := size_modulo (DIR.Size (item_path) mod 2 ** 32);
                  new_block.link_length  := 0;
                  ZST.assemble_regular_archive
                    (filename    => item_path,
                     file_size   => Natural (DIR.Size (item_path)),
                     target_saxs => AS.tmp_stmaxs);
               when hardlink =>
                  begin
                     new_block.blake_sum := Blake_3.file_digest (item_path);
                     if AS.level = debug then
                        AS.print (debug, Blake_3.hex (new_block.blake_sum) & " " & filename);
                     end if;
                  exception
                     when IOX.Use_Error =>
                        AS.serror := True;
                        new_block.blake_sum := null_sum;
                        AS.print (normal, "FATAL: Insufficient permissions to read " &
                                    item_path & "; This directory cannot be archived.");
                  end;

                  if AS.inode_already_seen (features.inode) then
                     new_block.type_of_file := hardlink;
                     new_block.multiplier   := 0;
                     new_block.flat_size    := 0;
                     declare
                        target : constant String := AS.retrieve_inode_path (features.inode);
                     begin
                        new_block.link_length := max_path (target'Length);
                        AS.push_link (target);
                     end;
                  else
                     AS.insert_inode (features.inode, item_path);
                     new_block.type_of_file := regular;
                     new_block.multiplier   := size_multi (DIR.Size (item_path) / 2 ** 32);
                     new_block.flat_size    := size_modulo (DIR.Size (item_path) mod 2 ** 32);
                     new_block.link_length  := 0;
                     ZST.assemble_regular_archive
                       (filename    => item_path,
                        file_size   => Natural (DIR.Size (item_path)),
                        target_saxs => AS.tmp_stmaxs);
                  end if;
            end case;
            AS.files.Append (new_block);
            if AS.level = verbose then
               AS.print (verbose,
                         Unix.display_permissions (new_block.file_perms)
                         & verbose_display_owngrp (features.owner)
                         & verbose_display_owngrp (features.group)
                         & verbose_display_filesize
                           (size_type (new_block.multiplier * (2 ** 32)) +
                              size_type (new_block.flat_size))
                         & " "
                         & Unix.format_file_time (new_block.modified_sec)
                         & " "
                         & filename);
            end if;
         end;

         AS.print (debug, "file = " & item_path & " (" & features.ftype'Img & ")");
      exception
         when failed : others =>
            AS.print (normal, "walkfiles exception => " & EX.Exception_Information (failed) &
                        "  file: " & item_path);
      end walkfiles;

   begin
      SCN.scan_directory (dir_path, dirfiles);
      dirfiles.Iterate (walkdir'Access);

      AS.print (verbose, "cd " & dir_path);
      dirfiles.Iterate (walkfiles'Access);

   exception
      when SCN.dscan_open_failure =>
         AS.print (normal, "The " & dir_path & " directory does not exist");
      when failed : others =>
         AS.print (normal, "scan_directory: Unknown error - directory search");
         AS.print (normal, "=> " & EX.Exception_Information (failed));
   end scan_directory;


   ------------------------------------------------------------------------------------------
   --  record_directory
   ------------------------------------------------------------------------------------------
   procedure record_directory (AS : in out Arc_Structure; top_directory : String)
   is
   begin
      AS.tlsize := top_directory'Length;
      AS.tlevel (AS.tlevel'First .. AS.tlevel'First + AS.tlsize - 1) := top_directory;
   end record_directory;


   ------------------------------------------------------------------------------------------
   --  retrieve_directory
   ------------------------------------------------------------------------------------------
   function retrieve_directory (AS : Arc_Structure) return String
   is
   begin
      return AS.tlevel (AS.tlevel'First .. AS.tlevel'First + AS.tlsize - 1);
   end retrieve_directory;


   ------------------------------------------------------------------------------------------
   --  inode_already_seen
   ------------------------------------------------------------------------------------------
   function inode_already_seen (AS : Arc_Structure; inode : inode_type) return Boolean
   is
      procedure scan (position : inode_crate.Cursor);

      found : Boolean := False;

      procedure scan (position : inode_crate.Cursor)
      is
         item : inode_record renames inode_crate.Element (position);
      begin
         if item.inode = inode then
            found := True;
         end if;
      end scan;
   begin
      AS.inodes.Iterate (scan'Access);
      return found;
   end inode_already_seen;


   ------------------------------------------------------------------------------------------
   --  retrieve_inode_path
   ------------------------------------------------------------------------------------------
   function retrieve_inode_path (AS : Arc_Structure; inode : inode_type) return String
   is
      procedure scan (position : inode_crate.Cursor);

      found      : Boolean := False;
      inode_path : A_Path;
      pathsize   : Natural := 0;

      procedure scan (position : inode_crate.Cursor)
      is
         item : inode_record renames inode_crate.Element (position);
      begin
         if not found then
            if item.inode = inode then
               found := True;
               inode_path := item.path;
               pathsize   := item.psize;
            end if;
         end if;
      end scan;
   begin
      AS.inodes.Iterate (scan'Access);
      return inode_path (inode_path'First .. inode_path'First + pathsize - 1);
   end retrieve_inode_path;


   ------------------------------------------------------------------------------------------
   --  insert_inode
   ------------------------------------------------------------------------------------------
   procedure insert_inode (AS : in out Arc_Structure; inode : inode_type; path : String)
   is
      --  The top_level prefix is stripped off the full path first
      new_inode_record : inode_record;
      relative_path : constant String := path (path'First + AS.tlsize + 1 .. path'Last);
      plast : constant Natural  := new_inode_record.path'First + relative_path'Length - 1;
   begin
      AS.print (debug, "inserting inode" & inode'Img & ": " & relative_path);
      new_inode_record.inode := inode;
      new_inode_record.psize := relative_path'Length;
      new_inode_record.path (new_inode_record.path'First .. plast) := relative_path;
      AS.inodes.Append (new_inode_record);
   end insert_inode;


   ------------------------------------------------------------------------------------------
   --  push_link
   ------------------------------------------------------------------------------------------
   procedure push_link (AS : in out Arc_Structure; link : String) is
   begin
      AS.print (debug, "Pushing " & link & " link target to stack");
      for index in link'Range loop
         AS.links.Append (link (index));
      end loop;
   end push_link;


   ------------------------------------------------------------------------------------------
   --  push_filename
   ------------------------------------------------------------------------------------------
   procedure push_filename (AS : in out Arc_Structure; simple_name : String) is
   begin
      AS.print (debug, "Pushing " & simple_name & " filename to stack");
      for index in simple_name'Range loop
         AS.fnames.Append (simple_name (index));
      end loop;
   end push_filename;


   ------------------------------------------------------------------------------------------
   --  initialize_archive_file
   ------------------------------------------------------------------------------------------
   procedure initialize_archive_file (AS : in out Arc_Structure; output_file_path : String)
   is
   begin
      SIO.Create (File => AS.tmp_handle,
                  Mode => SIO.Out_File,
                  Name => output_file_path & ".archive");
      AS.tmp_stmaxs := SIO.Stream (AS.tmp_handle);
   end initialize_archive_file;


   ------------------------------------------------------------------------------------------
   --  finalize_archive_file
   ------------------------------------------------------------------------------------------
   procedure finalize_archive_file (AS : in out Arc_Structure)
   is
   begin
      SIO.Close (AS.tmp_handle);
   end finalize_archive_file;


   ------------------------------------------------------------------------------------------
   --  initialize_index_file
   ------------------------------------------------------------------------------------------
   procedure initialize_index_file (AS : in out Arc_Structure; output_file_path : String)
   is
   begin
      SIO.Create (File => AS.ndx_handle,
                  Mode => SIO.Out_File,
                  Name => output_file_path & ".index");
      AS.ndx_stmaxs := SIO.Stream (AS.ndx_handle);
   end initialize_index_file;


   ------------------------------------------------------------------------------------------
   --  finalize_index_file
   ------------------------------------------------------------------------------------------
   procedure finalize_index_file (AS : in out Arc_Structure)
   is
   begin
      SIO.Close (AS.ndx_handle);
   end finalize_index_file;


   ------------------------------------------------------------------------------------------
   --  remove_archive_file
   ------------------------------------------------------------------------------------------
   procedure remove_archive_file (AS : Arc_Structure; output_file_path : String)
   is
      uncompressed_archive : constant String := output_file_path & ".archive";
   begin
      DIR.Delete_File (uncompressed_archive);
   exception
      when others =>
         AS.print (normal, "Failed to remove " & uncompressed_archive);
   end remove_archive_file;


   ------------------------------------------------------------------------------------------
   --  remove_index_file
   ------------------------------------------------------------------------------------------
   procedure remove_index_file (AS : Arc_Structure; output_file_path : String)
   is
      uncompressed_archive : constant String := output_file_path & ".index";
   begin
      DIR.Delete_File (uncompressed_archive);
   exception
      when others =>
         AS.print (normal, "Failed to remove " & uncompressed_archive);
   end remove_index_file;


   ------------------------------------------------------------------------------------------
   --  write_blank_header
   ------------------------------------------------------------------------------------------
   procedure write_blank_header (AS : in out Arc_Structure; output_file_path : String)
   is
      block : premier_block;
   begin
      block.magic_bytes := "INI";
      SIO.Create (File => AS.rvn_handle,
                  Mode => SIO.Out_File,
                  Name => output_file_path);

      AS.rvn_stmaxs := SIO.Stream (AS.rvn_handle);
      premier_block'Output (AS.rvn_stmaxs, block);
   end write_blank_header;


   ------------------------------------------------------------------------------------------
   --  overwrite_header
   ------------------------------------------------------------------------------------------
   procedure overwrite_header (AS : in out Arc_Structure; output_file_path : String)
   is
      block    : premier_block;
      nlbfloat : constant Float := Float (AS.links.Length) / 32.0;
      nfbfloat : constant Float := Float (AS.fnames.Length) / 32.0;
   begin
      SIO.Close (AS.rvn_handle);

      block.magic_bytes     := magic;
      block.version         := format_version;
      block.num_groups      := index_type (AS.groups.Length);
      block.num_owners      := index_type (AS.owners.Length);
      block.link_blocks     := file_index (Float'Ceiling (nlbfloat));
      block.file_blocks     := file_index (AS.files.Length);
      block.size_metadata   := AS.meta_size;
      block.size_filedata   := AS.ndx_size;
      block.size_archive    := AS.tmp_size;
      block.fname_blocks    := file_index (Float'Ceiling (nfbfloat));
      block.flat_metadata   := zstd_size (AS.flat_meta);
      block.flat_filedata   := AS.flat_ndx;
      block.flat_archive    := exabytes (AS.flat_arc);
      block.unused1         := 0;
      block.unused2         := 0;
      block.unused3         := 0;
      block.unused4         := 0;

      declare
         package Premier_IO is new Ada.Direct_IO (premier_block);
         file_handle : Premier_IO.File_Type;
      begin
         Premier_IO.Open (File => file_handle,
                          Mode => Premier_IO.Inout_File,
                          Name => output_file_path);
         Premier_IO.Write (File => file_handle,
                           Item => block);
         Premier_IO.Close (file_handle);
      end;

   end overwrite_header;


   ------------------------------------------------------------------------------------------
   --  write_owngrp_blocks
   ------------------------------------------------------------------------------------------
   procedure write_owngrp_blocks (AS : Arc_Structure)
   is
      procedure write_line (position : owngrp_crate.Cursor);

      type owngrp_block is
         record
            identifier : ownergroup;
         end record;
      for owngrp_block'Size use 256;

      procedure write_line (position : owngrp_crate.Cursor)
      is
         item : ownergroup renames owngrp_crate.Element (position);
         block : owngrp_block;
      begin
         block.identifier := item;
         owngrp_block'Output (AS.ndx_stmaxs, block);
      end write_line;
   begin
      AS.groups.Iterate (write_line'Access);
      AS.owners.Iterate (write_line'Access);
   end write_owngrp_blocks;


   ------------------------------------------------------------------------------------------
   --  write_link_block
   ------------------------------------------------------------------------------------------
   procedure write_link_block (AS : Arc_Structure)
   is
      procedure construct (position : link_crate.Cursor);

      nlbfloat   : constant Float := Float (AS.links.Length) / 32.0;
      num_lines  : constant Natural := Natural (Float'Ceiling (nlbfloat));
      block_size : constant Natural := num_lines * 32;

      type block_type is array (1 .. block_size) of one_byte;
      type line_type is array (1 .. 32) of one_byte;
      type single_line is record
         contents : line_type;
      end record;
      for single_line'Size use 256;

      block      : block_type := (others => 0);
      index      : Natural := 0;
      line_index : Natural := 1;

      procedure construct (position : link_crate.Cursor)
      is
         item : Character renames link_crate.Element (position);
      begin
         index := index + 1;
         block (index) := one_byte (Character'Pos (item));
      end construct;
   begin
      AS.links.Iterate (construct'Access);
      for line in 1 .. num_lines loop
         declare
            line : single_line;
         begin
            line.contents := line_type (block (line_index .. line_index + 31));
            single_line'Output (AS.ndx_stmaxs, line);
         end;
         line_index := line_index + 32;
      end loop;
   end write_link_block;


   ------------------------------------------------------------------------------------------
   --  write_filename_block
   ------------------------------------------------------------------------------------------
   procedure write_filename_block (AS : Arc_Structure)
   is
      procedure construct (position : filename_crate.Cursor);

      nfbfloat   : constant Float := Float (AS.fnames.Length) / 32.0;
      num_lines  : constant Natural := Natural (Float'Ceiling (nfbfloat));
      block_size : constant Natural := num_lines * 32;

      type block_type is array (1 .. block_size) of one_byte;
      type line_type is array (1 .. 32) of one_byte;
      type single_line is record
         contents : line_type;
      end record;
      for single_line'Size use 256;

      block      : block_type := (others => 0);
      index      : Natural := 0;
      line_index : Natural := 1;

      procedure construct (position : filename_crate.Cursor)
      is
         item : Character renames filename_crate.Element (position);
      begin
         index := index + 1;
         block (index) := one_byte (Character'Pos (item));
      end construct;
   begin
      AS.fnames.Iterate (construct'Access);
      for line in 1 .. num_lines loop
         declare
            line : single_line;
         begin
            line.contents := line_type (block (line_index .. line_index + 31));
            single_line'Output (AS.ndx_stmaxs, line);
         end;
         line_index := line_index + 32;
      end loop;
   end write_filename_block;


   ------------------------------------------------------------------------------------------
   --  write_file_index_block
   ------------------------------------------------------------------------------------------
   procedure write_file_index_block (AS : Arc_Structure)
   is
      procedure write (position : file_block_crate.Cursor);

      procedure write (position : file_block_crate.Cursor)
      is
         item : File_Block renames file_block_crate.Element (position);
      begin
         File_Block'Output (AS.ndx_stmaxs, item);
      end write;
   begin
      AS.files.Iterate (write'Access);
   end write_file_index_block;


   ------------------------------------------------------------------------------------------
   --  write_file_index_block
   ------------------------------------------------------------------------------------------
   procedure write_file_index_block (AS : in out Arc_Structure; output_file_path : String)
   is
      out_succ : Boolean;
      out_size : ZST.File_Size;
      uncompressed_archive : constant String := output_file_path & ".index";
      archive_size : constant ZST.File_Size := ZST.File_Size (DIR.Size (uncompressed_archive));
   begin
      AS.ndx_size := 0;
      ZST.incorporate_regular_file
        (filename    => uncompressed_archive,
         size        => archive_size,
         quality     => rvn_compression_level,
         target_saxs => AS.rvn_stmaxs,
         target_file => AS.rvn_handle,
         output_size => out_size,
         successful  => out_succ);
      if out_succ then
         AS.print (debug, "Compressed index from" & archive_size'Img & " to" & out_size'Img);
         AS.ndx_size := zstd_size (out_size);
         AS.flat_ndx := zstd_size (archive_size);
      else
         AS.print (normal, "Failed to compress " & uncompressed_archive);
      end if;
   end write_file_index_block;


   ------------------------------------------------------------------------------------------
   --  write_archive_block
   ------------------------------------------------------------------------------------------
   procedure write_archive_block (AS : in out Arc_Structure; output_file_path : String)
   is
      out_succ : Boolean;
      out_size : ZST.File_Size;
      uncompressed_archive : constant String := output_file_path & ".archive";
      archive_size : constant ZST.File_Size := ZST.File_Size (DIR.Size (uncompressed_archive));
   begin
      AS.tmp_size := 0;
      AS.flat_arc := 0;
      ZST.incorporate_regular_file
        (filename    => uncompressed_archive,
         size        => archive_size,
         quality     => rvn_compression_level,
         target_saxs => AS.rvn_stmaxs,
         target_file => AS.rvn_handle,
         output_size => out_size,
         successful  => out_succ);
      if out_succ then
         AS.print (debug, "Compressed archive from" & archive_size'Img & " to" & out_size'Img);
         AS.tmp_size := zstd_size (out_size);
         AS.flat_arc := size_type (archive_size);
      else
         AS.print (normal, "Failed to compress " & uncompressed_archive);
      end if;
   end write_archive_block;


   ------------------------------------------------------------------------------------------
   --  write_metadata_block
   ------------------------------------------------------------------------------------------
   procedure write_metadata_block (AS : in out Arc_Structure; metadata_path : String) is
   begin
      AS.meta_size := 0;
      AS.flat_meta := 0;
      if metadata_path = "" then
         AS.print (debug, "No metadata file has been provided.");
         return;
      end if;

      if not DIR.Exists (metadata_path) then
         AS.print (normal, "The metadata file for the given path does not exist.");
         AS.print (normal, "The archive will be built without additional metadata.");
         return;
      end if;

      declare
         use type ZST.File_Size;

         dossier_size : ZST.File_Size;
         out_succ : Boolean;
         out_size : ZST.File_Size;
      begin
         dossier_size := ZST.File_Size (DIR.Size (metadata_path));

         if dossier_size > ZST.File_Size (KB512)  then
            AS.print (normal, "The metadata file size exceeds the 512 KB limit.");
            AS.print (normal, "The archive will be built without this file.");
            return;
         end if;

         ZST.incorporate_regular_file
           (filename    => metadata_path,
            size        => dossier_size,
            quality     => rvn_compression_level,
            target_saxs => AS.rvn_stmaxs,
            target_file => AS.rvn_handle,
            output_size => out_size,
            successful  => out_succ);

         if out_succ then
            AS.print (debug, "Compressed metadata from" & dossier_size'Img & " to" & out_size'Img);
            AS.meta_size := zstd_size (out_size);
            AS.flat_meta := mdata_size (dossier_size);
         else
            AS.print (normal, "Failed to compress " & metadata_path);
         end if;
      exception
         when others =>
            AS.print (normal, "An error occurred while inserting the additional metadata.");
      end;
   end write_metadata_block;


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
   --  able_to_write_rvn_archive
   ------------------------------------------------------------------------------------------
   function able_to_write_rvn_archive
     (AS : Arc_Structure;
      output_file_path : String) return Boolean
   is
      function parent_dir return String;
      function parent_dir return String
      is
         S : String renames output_file_path;

         back_marker  : constant Natural := S'First;
         front_marker : Natural := S'Last;
      begin
         loop
            if front_marker < back_marker then
               --  delimiter never found
               return ".";
            end if;
            if S (front_marker) = '/' then
               return S (back_marker .. front_marker - 1);
            end if;
            front_marker := front_marker - 1;
         end loop;
      end parent_dir;

      out_dir : constant String := parent_dir;
      dir_is_writable : Boolean;
   begin
      dir_is_writable := Unix.file_is_writable (out_dir);
      if not dir_is_writable then
         AS.print (normal, "You do not have permission to write to " & out_dir & " directory.");
      end if;
      return dir_is_writable;
   end able_to_write_rvn_archive;


   ------------------------------------------------------------------------------------------
   --  verbose_display_owngrp
   ------------------------------------------------------------------------------------------
   function verbose_display_owngrp (owngrp : ownergroup) return String
   is
      name   : constant String := trim_trailing_zeros (owngrp);
      result : String (1 .. 9) := (others => ' ');
      rindex : Natural;
   begin
      if name'Length > 8 then
         result := " " & name (name'First .. name'First + 6) & "*";
      else
         rindex := 10 - name'Length;
         result (rindex .. 9) := name;
      end if;
      return result;
   end verbose_display_owngrp;


   ------------------------------------------------------------------------------------------
   --  verbose_display_filesize
   ------------------------------------------------------------------------------------------
   function verbose_display_filesize (fsize : size_type) return String
   is
      function trim_first (S : String) return String;
      function trim_first (S : String) return String is
      begin
         return S (S'First + 1 .. S'Last);
      end trim_first;

      result : String (1 .. 9) := (others => ' ');
   begin
      if fsize < 100_000_000 then
         declare
            myimage : constant String := trim_first (fsize'Img);
         begin
            result (10 - myimage'Length .. result'Last) := myimage;
         end;
      elsif fsize < 10_000_000_000 then
         declare
            basenum : constant Natural := Natural (fsize / 1_000_000);
            myimage : constant String := trim_first (basenum'Img & "M+");
         begin
            result (10 - myimage'Length .. result'Last) := myimage;
         end;
      else
         declare
            basenum : constant Natural := Natural (fsize / 1_000_000_000);
            myimage : constant String := trim_first (basenum'Img & "G+");
         begin
            result (10 - myimage'Length .. result'Last) := myimage;
         end;
      end if;
      return result;
   end verbose_display_filesize;

end Archive.Pack;
