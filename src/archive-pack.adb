--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Ada.Exceptions;
with Ada.IO_Exceptions;
with Ada.Directories;
with Ada.Direct_IO;
with Archive.Dirent.Scan;
with Archive.Communication;
with Archive.Misc;
with ThickUCL.Files;
with ThickUCL.Emitter;
with Blake_3;
with Ucl;

package body Archive.Pack is

   package DIR renames Ada.Directories;
   package EX  renames Ada.Exceptions;
   package IOX renames Ada.IO_Exceptions;
   package UNX renames Archive.Unix;
   package SCN renames Archive.Dirent.Scan;
   package SQW renames Archive.Communication;
   package TUC renames ThickUCL;
   package ZST renames Zstandard;

   ------------------------------------------------------------------------------------------
   --  integrate
   ------------------------------------------------------------------------------------------
   function integrate
     (top_level_directory : String;
      metadata_file       : String;
      manifest_file       : String;
      prefix              : String;
      abi                 : String;
      keyword_dir         : String;
      output_file         : String;
      fixed_timestamp     : filetime;
      verbosity           : info_level;
      record_base_libs    : Boolean;
      integrate_log       : Ada.Text_IO.File_Type;
      optional_pipe       : Unix.File_Descriptor := Unix.not_connected)
      return Boolean
   is
      metadata : Arc_Structure;
      metadata_tree : ThickUCL.UclTree;
      set_adjacent  : Boolean := False;
   begin
      SQW.initialize (verbosity, optional_pipe);
      metadata.set_verbosity (verbosity);
      metadata.record_directory (top_level_directory);

      if not metadata.able_to_write_rvn_archive (output_file) then
         return False;
      end if;

      metadata.scan_metadata_file
        (metadata_path => metadata_file,
         prefix        => prefix,
         abi           => abi,
         tree          => metadata_tree);

      if metadata_tree.key_exists ("subpackage") then
         declare
            subpackage : constant String := metadata_tree.get_base_value ("subpackage");
         begin
            --  skip adjacent on complete, single, dev, docs, nls, lang, examples, man, primary
            if subpackage /= "complete" and then
              subpackage /= "primary" and then
              subpackage /= "single" and then
              subpackage /= "dev" and then
              subpackage /= "man" and then
              subpackage /= "nls" and then
              subpackage /= "docs" and then
              subpackage /= "lang" and then
              subpackage /= "info" and then
              subpackage /= "examples"
            then
               set_adjacent := True;
            end if;
         end;
      end if;

      if manifest_file /= "" then
         if not metadata.white_list.ingest_file_manifest
           (manifest_file      => manifest_file,
            stage_directory    => top_level_directory,
            prefix_directory   => metadata_tree.get_base_value ("prefix"),
            keywords_directory => keyword_dir,
            namebase           => metadata_tree.get_base_value ("namebase"),
            subpackage         => metadata_tree.get_base_value ("subpackage"),
            variant            => metadata_tree.get_base_value ("variant"),
            level              => verbosity,
            integrate_log      => integrate_log)
         then
            return False;
         end if;
      end if;

      --  metadata.run_prepacking_actions;

      metadata.initialize_archive_file (output_file);
      metadata.scan_directory (top_level_directory, 0, fixed_timestamp, set_adjacent,
                               record_base_libs);
      metadata.finalize_archive_file;

      if metadata.serror then
         metadata.remove_archive_file (output_file);
         return False;
      end if;

      metadata.analyze_missing_required_libraries;

      metadata.initialize_index_file (output_file);
      metadata.write_owngrp_blocks;                  --  block FA and FB
      metadata.write_link_block;                     --  block FC
      metadata.write_file_index_block;               --  block FD
      metadata.write_filename_block;                 --  block FE
      metadata.finalize_index_file;

      metadata.write_blank_header (output_file);     --  block 1
      metadata.write_metadata_block (output_file,
                                     metadata_file,
                                     metadata_tree); --  block 2
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
         case msg_level is
            when debug   => SQW.emit_debug (message);
            when verbose => SQW.emit_notice (message);
            when normal  => SQW.emit_message (message);
            when silent  => null;
         end case;
      end if;
   end print;


   ------------------------------------------------------------------------------------------
   --  scan_directory
   ------------------------------------------------------------------------------------------
   procedure scan_directory
     (AS        : in out Arc_Structure;
      dir_path  : String;
      dir_index : index_type;
      timestamp : filetime;
      adjacent  : Boolean;
      record_base_libs : Boolean)
   is
      dirfiles  : SCN.dscan_crate.Vector;
      override_mtime : constant Boolean := timestamp > 0;
      last_reg_format : Elf.elf_class := Elf.format64;
      stage_directory : constant String := AS.tlevel (1 .. AS.tlsize);
      last_runpath : Elf.ASU.Unbounded_String;
      last_dirpath : Elf.ASU.Unbounded_String;
      last_fullpath : Elf.ASU.Unbounded_String;
      --  last_is_shlib : Boolean;

      procedure add_to_needed_libraries (Position : Elf.library_crate.Cursor)
      is
         library : constant String := Elf.ASU.To_String (Elf.library_crate.Element (Position));
         len2    : constant Natural := library'Length;
         ftray2  : A_Filename := (others => Character'Val (0));
         file_runpath : constant String := Elf.ASU.To_String (last_runpath);
         file_dirpath : constant String := Elf.ASU.To_String (last_dirpath);
         original    : constant String := Elf.ASU.To_String (last_fullpath);
      begin
         ftray2 (1 .. len2) := library;
         if AS.need_seen.Contains (ftray2) then
            return;
         end if;
         if record_library (library, record_base_libs, last_reg_format) then
            if not AS.lib_need.Contains (ftray2) then
               AS.lib_need.Append (ftray2);
            end if;
            --  if not last_is_shlib then  --  Don't do link checks on libraries (unclear why not)
            if not found_on_runpath (library, file_runpath, file_dirpath, stage_directory, original)
            then
               --  Make sure this is not a system library with ALLOW_BASE_SHLIBS=TRUE case
               if record_library (library, False, last_reg_format) then
                  AS.not_found.Append (ftray2);
               end if;
            end if;
            --  end if;
         end if;
         AS.need_seen.Append (ftray2);
      end add_to_needed_libraries;

      procedure walkdir (position : SCN.dscan_crate.Cursor)
      is
         item      : Archive.Dirent.Directory_Entity renames SCN.dscan_crate.Element (position);
         item_path : constant String := item.full_path;
         filename  : constant String := item.simple_name;
         features  : constant UNX.File_Characteristics := UNX.get_charactistics (item_path);
         new_block : File_Block;
         pog       : Archive.Whitelist.white_features;
      begin
         --  We only want true directories.  Symbolic links to directories are ignored.
         case features.ftype is
            when directory => null;
            when others => return;
         end case;

         if AS.white_list.whitelist_in_use then
            if not AS.white_list.directory_on_whitelist (item_path) then
               if adjacent then
                  --  We are looking for adjacent libraries so don't filter anything
                  --  (but don't record the irrelevant directory in the archive either!)
                  AS.scan_directory (item_path, AS.dtrack, timestamp, adjacent, record_base_libs);
               end if;
               return;
            end if;
         end if;

         pog := AS.white_list.get_file_features (item_path,
                                                 features.owner,
                                                 features.group,
                                                 features.perms);

         AS.dtrack := AS.dtrack + 1;
         AS.push_filename (filename);
         new_block.blake_sum    := null_sum;
         new_block.index_owner  := AS.get_owner_index (pog.owner_spec);
         new_block.index_group  := AS.get_group_index (pog.group_spec);
         new_block.type_of_file := directory;
         new_block.multiplier   := 0;
         new_block.flat_size    := 0;
         new_block.file_perms   := pog.perms_spec;
         new_block.link_length  := 0;
         new_block.index_parent := dir_index;
         new_block.directory_id := AS.dtrack;
         new_block.fname_length := max_fname (filename'Length);
         new_block.padding      := (others => 0);

         if override_mtime then
            new_block.modified_sec := timestamp;
            new_block.modified_ns  := 0;
         else
            new_block.modified_sec := features.mtime;
            new_block.modified_ns  := features.mnsec;
         end if;

         AS.files.Append (new_block);
         AS.print (debug, item_path & " (" & AS.dtrack'Img & ")");
         AS.print (debug, "owner =" & new_block.index_owner'Img & "  group =" &
                     new_block.index_group'Img);
         AS.print (debug, "perms =" & features.perms'Img & "   mod =" & features.mtime'Img);
         AS.print (verbose, "Record directory " & item_path);

         AS.scan_directory (item_path, AS.dtrack, timestamp, adjacent, record_base_libs);
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
         pog       : Archive.Whitelist.white_features;
         len       : Natural;
         file_data : Elf.ELF_File;
         filetray  : A_Filename := (others => Character'Val (0));
      begin
         --  Reject directories, but accept symlinks to directories
         case features.ftype is
            when directory => return;
            when unsupported =>
               AS.print
                 (normal, "NOTICE: Unsupported file " & item_path & " ignored.");
               return;
            when symlink | hardlink | fifo =>
               if AS.white_list.whitelist_in_use then
                  if not AS.white_list.file_on_whitelist (item_path) then
                     return;
                  end if;
               end if;
            when regular =>
               if AS.white_list.whitelist_in_use then
                  if not AS.white_list.file_on_whitelist (item_path) then
                     if adjacent then
                        Elf.readelf (item_path, file_data);
                        case file_data.file_type is
                           when Elf.not_elf => null;
                           when Elf.executable | Elf.shared_object =>
                              len := Elf.ASU.Length (file_data.soname);
                              if len > 0 then
                                 filetray (1 .. len) := Elf.ASU.To_String (file_data.soname);
                                 if not AS.lib_adj.Contains (filetray) then
                                    AS.lib_adj.Append (filetray);
                                 end if;
                              end if;
                           when others => null;
                        end case;
                     end if;
                     return;
                  end if;
               end if;
         end case;

         --  Scan for provided and required libraries
         case features.ftype is
            when regular =>
               Elf.readelf (item_path, file_data);
               last_reg_format := file_data.format;
               last_runpath := file_data.run_path;
               --  last_is_shlib := Elf.ASU.Length (file_data.soname) > 0;
               last_dirpath := Elf.ASU.To_Unbounded_String (item.parent_directory);
               last_fullpath := Elf.ASU.To_Unbounded_String (item_path);
               case file_data.file_type is
                  when Elf.not_elf => null;
                  when Elf.executable | Elf.shared_object =>
                     len := Elf.ASU.Length (file_data.soname);
                     if len > 0 then
                        filetray (1 .. len) := Elf.ASU.To_String (file_data.soname);
                        if not AS.lib_prov.Contains (filetray) then
                           AS.lib_prov.Append (filetray);
                        end if;
                     end if;
                     file_data.libs_needed.Iterate (add_to_needed_libraries'Access);
                  when others => null;
               end case;
            when others => null;
         end case;

         pog := AS.white_list.get_file_features (item_path,
                                                 features.owner,
                                                 features.group,
                                                 features.perms);

         declare
            --  Blake3 sums for regular files and hardlinks
            --  The first hardlink is written as a regular file.
            new_block : File_Block;

            use type DIR.File_Size;
         begin
            AS.push_filename (filename);
            new_block.index_owner  := AS.get_owner_index (pog.owner_spec);
            new_block.index_group  := AS.get_group_index (pog.group_spec);
            new_block.file_perms   := pog.perms_spec;
            new_block.index_parent := dir_index;
            new_block.directory_id := 0;
            new_block.fname_length := max_fname (filename'Length);
            new_block.padding      := (others => 0);

            if override_mtime then
               new_block.modified_sec := timestamp;
               new_block.modified_ns  := 0;
            else
               new_block.modified_sec := features.mtime;
               new_block.modified_ns  := features.mnsec;
            end if;

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
                         Unix.display_permissions (new_block.file_perms, new_block.type_of_file)
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
      case archive_size is
         when 0 =>
            --  We can't compress a zero-byte string
            AS.print (debug, "Zero-byte file_index, compression skipped");
            AS.flat_ndx := 0;
            out_succ := True;
            out_size := 0;
            return;
         when others => null;
      end case;
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
         AS.print (normal, "Failed to compress " & uncompressed_archive &
                     " (size" & archive_size'Img & ")");
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
      case archive_size is
         when 0 =>
            --  We can't compress a zero-byte string
            AS.print (debug, "Zero-byte archive block, compression skipped");
            out_succ := True;
            out_size := 0;
            return;
         when others => null;
      end case;
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
         AS.print (normal, "Failed to compress " & uncompressed_archive & " (size" &
                  archive_size'Img & ")");
      end if;
   end write_archive_block;


   ------------------------------------------------------------------------------------------
   --  scan_metadata_file
   ------------------------------------------------------------------------------------------
   procedure scan_metadata_file
     (AS : in out Arc_Structure;
      metadata_path    : String;
      prefix           : String;
      abi              : String;
      tree             : in out ThickUCL.UclTree)
   is
      use type ZST.File_Size;

      attempt_read : Boolean := False;
      dossier_size : ZST.File_Size;
      KEY_PREFIX   : constant String := "prefix";
      KEY_ABI      : constant String := "abi";
      KEY_DESCR    : constant String := "desc";
      KEY_NAMEBASE : constant String := "namebase";
      KEY_SUBPKG   : constant String := "subpackage";
      KEY_VARIANT  : constant String := "variant";
      VAL_UNSET    : constant String := "unset";
   begin
      if metadata_path = "" then
         AS.print (debug, "No metadata file has been provided.");
      else
         if DIR.Exists (metadata_path) then
            dossier_size := ZST.File_Size (DIR.Size (metadata_path));
            if dossier_size > ZST.File_Size (KB512)  then
               AS.print (normal, "The metadata file size exceeds the 512 KB limit.");
               AS.print (normal, "The archive will be built without this file.");
            else
               attempt_read := True;
            end if;
         else
            AS.print (normal, "The metadata file for the given path does not exist.");
         end if;
      end if;

      if attempt_read then
         begin
            TUC.Files.parse_ucl_file (tree, metadata_path, "");
         exception
            when TUC.Files.ucl_file_unparseable =>
               declare
                  good : Boolean;
                  desc : constant String :=
                    ZST.File_Contents (metadata_path, Natural (dossier_size), good);
               begin
                  if good then
                     tree.insert (KEY_DESCR, desc);
                  else
                     AS.print (normal, "Failed to read " & metadata_path &
                                 " file; description will be left out");
                  end if;
               end;
         end;
      end if;

      --  augment with prefix
      if tree.key_exists (KEY_PREFIX) then
         declare
            stored_prefix : constant String := tree.get_base_value (KEY_PREFIX);
         begin
            if stored_prefix /= prefix then
               AS.print (verbose, "Metadata unexpectedly contains prefix field; not overwriting.");
            end if;
         end;
      else
         tree.insert (KEY_PREFIX, prefix);
      end if;

      --  augment with abi
      if tree.key_exists (KEY_ABI) then
         AS.print (verbose, "Metadata unexpectedly contains abi field; not overwriting.");
      else
         tree.insert (KEY_ABI, abi);
      end if;

      if not tree.key_exists (KEY_NAMEBASE) then
         tree.insert (KEY_NAMEBASE, VAL_UNSET);
      end if;
      if not tree.key_exists (KEY_SUBPKG) then
         tree.insert (KEY_SUBPKG, VAL_UNSET);
      end if;
      if not tree.key_exists (KEY_VARIANT) then
         tree.insert (KEY_VARIANT, VAL_UNSET);
      end if;

   end scan_metadata_file;


   ------------------------------------------------------------------------------------------
   --  write_metadata_block
   ------------------------------------------------------------------------------------------
   procedure write_metadata_block
     (AS               : in out Arc_Structure;
      output_file_path : String;
      metadata_path    : String;
      tree             : in out ThickUCL.UclTree)
   is
      flat_archive : constant String := output_file_path & ".archive";
      archive_size : constant Ucl.ucl_integer := Ucl.ucl_integer (DIR.Size (flat_archive));
      KEY_FLATSIZE : constant String := "flatsize";
      KEY_DIRS     : constant String := "directories";
      KEY_SCRIPTS  : constant String := "scripts";
      KEY_LIBPROV  : constant String := "shlibs_provided";
      KEY_LIBNEED  : constant String := "shlibs_required";
      KEY_LIBADJ   : constant String := "shlibs_adjacent";

      procedure push_library (Position : libraries_crate.Cursor)
      is
         thislib : constant String := trim_trailing_zeros (libraries_crate.Element (Position));
      begin
         tree.insert ("", thislib);
      end push_library;
   begin
      AS.meta_size := 0;
      AS.flat_meta := 0;

      --  augment with flatsize
      if tree.key_exists (KEY_FLATSIZE) then
         AS.print (verbose, "Metadata unexpectedly contains flatsize field; not overwriting.");
      else
         tree.insert (KEY_FLATSIZE, archive_size);
      end if;

      --  Determine if external metadata file was previously provided.
      --  If the namebase is not unset, it wasn't, so it's not a normal package.
      declare
         namebase : constant String := tree.get_base_value ("namebase");
      begin
         if namebase = "unset" then
            goto Metadata_Block;
         end if;
      end;

      --  augment directories
      if tree.key_exists (KEY_DIRS) then
         AS.print (verbose, "Metadata unexpectedly contains directories field; not overwriting.");
      else
         tree.start_array (KEY_DIRS);
         for z in 0 .. AS.white_list.empty_directory_count - 1 loop
            tree.start_object ("");
            declare
               attr : constant Whitelist.white_features :=
                 AS.white_list.get_empty_directory_attributes (z);
               KEY_OWNER : constant String := "owner";
               KEY_GROUP : constant String := "group";
               KEY_PERMS : constant String := "perms";
               KEY_PATH  : constant String := "path";
            begin
               tree.insert (KEY_PATH, AS.white_list.get_empty_directory_path (z));
               if attr.owner_spec = null_owngrp then
                  tree.insert (KEY_OWNER, False);
               else
                  tree.insert (KEY_OWNER, trim_trailing_zeros (attr.owner_spec));
               end if;
               if attr.group_spec = null_owngrp then
                  tree.insert (KEY_GROUP, False);
               else
                  tree.insert (KEY_GROUP, trim_trailing_zeros (attr.group_spec));
               end if;
               if attr.perms_spec = 0 then
                  tree.insert (KEY_PERMS, False);
               else
                  tree.insert (KEY_PERMS, Ucl.ucl_integer (attr.perms_spec));
               end if;
            end;
            tree.close_object;
         end loop;
         tree.close_array;
      end if;

      --  Add phase scripts (append if they already exist)
      declare
         keyjar : ThickUCL.jar_string.Vector;
         keep_going : Boolean := True;
      begin
         if tree.key_exists (KEY_SCRIPTS) then
            AS.print (debug, "metadata already defined scripts object, possibly augmenting");
            if tree.ucl_object_field_exists (KEY_SCRIPTS) then
               tree.reopen_object (KEY_SCRIPTS);
               tree.get_object_object_keys
                 (vndx        => tree.get_index_of_base_ucl_object (KEY_SCRIPTS),
                  object_keys => keyjar);
            else
               AS.print (normal, "metadata defines scripts field, but it's not of type object.");
               AS.print (normal, "This is an input error. Any whitelist scripts will be ignored.");
               keep_going := False;
            end if;
         else
            tree.start_object (KEY_SCRIPTS);
         end if;

         if keep_going then
            for phase in Whitelist.package_phase'Range loop
               if AS.white_list.script_count (phase) > 0 then
                  AS.print (debug, "apply " & phase'Img & " phase scripts.");
                  if ThickUCL.key_found (keyjar, Whitelist.convert_phase (phase)) then
                     tree.reopen_array (Whitelist.convert_phase (phase));
                  else
                     tree.start_array (Whitelist.convert_phase (phase));
                  end if;
                  for z in 0 .. AS.white_list.script_count (phase) - 1 loop
                     tree.start_object ("");
                     tree.insert ("code", AS.white_list.get_script (phase, z));
                     tree.insert ("args", AS.white_list.get_arguments (phase, z));
                     tree.close_object;
                  end loop;
                  tree.close_array;
               end if;
            end loop;
            tree.close_object;
         end if;
      end;

      --  Build up message array
      --  If the array already exists, don't complain, but append to it.
      --  It must be an array of objects. Each object consists of a type (install, remove, upgrade)
      --  and a message.
      declare
         messages_key : constant String := "messages";
         keep_going   : Boolean := True;
         msg_count    : Natural;
      begin
         if tree.key_exists (messages_key) then
            case tree.get_data_type (messages_key) is
               when TUC.data_array =>
                  tree.reopen_array (messages_key);
               when others =>
                  keep_going := False;
                  AS.print (verbose, "Metadata unexpectedly contains non-array messages field;" &
                              "no messages will be written.");
            end case;
         else
            tree.start_array (messages_key);
         end if;

         if keep_going then
            for mtype in Archive.Whitelist.Message_Type'Range loop
               msg_count := AS.white_list.message_count (mtype);
               if msg_count > 0 then
                  declare
                     message_type : constant String := Archive.Whitelist.get_message_key (mtype);
                  begin
                     for m in 0 .. msg_count - 1 loop
                        tree.start_object ("");
                        tree.insert ("type", message_type);
                        tree.insert ("message",  AS.white_list.get_message (mtype, m));
                        tree.close_object;
                     end loop;
                  end;
               end if;
            end loop;
            tree.close_array;
         end if;
      end;

      --  Set provided libraries
      if tree.key_exists (KEY_LIBPROV) then
         AS.print (verbose, "Metadata contains provided libraries field; not overwriting.");
      else
         tree.start_array (KEY_LIBPROV);
         AS.lib_prov.Iterate (push_library'Access);
         tree.close_array;
      end if;

      --  Set required libraries
      if tree.key_exists (KEY_LIBNEED) then
         AS.print (verbose, "Metadata contains required libraries field; not overwriting.");
      else
         tree.start_array (KEY_LIBNEED);
         AS.lib_need.Iterate (push_library'Access);
         tree.close_array;
      end if;

      --  Set adjacent libraries
      if tree.key_exists (KEY_LIBADJ) then
         AS.print (verbose, "Metadata contains adjacent libraries field; not overwriting.");
      else
         tree.start_array (KEY_LIBADJ);
         AS.lib_adj.Iterate (push_library'Access);
         tree.close_array;
      end if;

      <<Metadata_Block>>
      declare
         out_succ : Boolean;
         out_size : ZST.File_Size;
         in_size  : Natural;
      begin
         declare
            ucl_data : constant String := TUC.Emitter.emit_ucl (tree);
         begin
            in_size := ucl_data'Length;
            ZST.incorporate_string
              (data        => ucl_data,
               quality     => rvn_compression_level,
               target_saxs => AS.rvn_stmaxs,
               output_size => out_size,
               successful  => out_succ);
         end;

         if out_succ then
            AS.print (debug, "Compressed metadata from" & in_size'Img & " to" & out_size'Img);
            AS.meta_size := zstd_size (out_size);
            AS.flat_meta := mdata_size (in_size);
         else
            AS.print (normal, "Failed to compress " & metadata_path);
         end if;
      exception
            when uclerror : TUC.ucl_type_mismatch |
              TUC.ucl_key_not_found |
              TUC.index_out_of_range =>
            AS.print (normal, "As error occurred with the UCL emitter for the metadata.");
            AS.print (normal, EX.Exception_Information (uclerror));
         when unknown : others =>
            AS.print (normal, "An error occurred while inserting the metadata.");
            AS.print (normal, EX.Exception_Information (unknown));
      end;
   end write_metadata_block;


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


   ----------------------
   --  record_library  --
   ----------------------
   function record_library
     (library_file     : String;
      record_base_libs : Boolean;
      file_format      : Elf.elf_class) return Boolean
   is
      function get_base_paths return String is
      begin
         case file_format is
            when Elf.format64 =>
               case platform is
                  when dragonfly             => return "/lib:/usr/lib";
                  when freebsd | midnightbsd => return "/lib:/usr/lib";
                  when netbsd | openbsd      => return "/lib:/usr/lib";
                  when solaris | omnios      => return "/usr/lib/amd64:/usr/lib:/lib";
                  when generic_unix          => return "/lib:/usr/lib";
                  when linux =>
                     return "/usr/lib/x86_64-linux-gnu:/usr/lib:/usr/lib64:/lib:/lib64";
               end case;
            when Elf.format32 =>
               case platform is
                  when dragonfly             => return "";
                  when openbsd               => return "";
                  when freebsd | midnightbsd => return "/lib:/usr/lib32:/usr/lib";
                  when netbsd                => return "/lib:/usr/lib32:/usr/lib";
                  when solaris | omnios      => return "/usr/lib:/lib";
                  when generic_unix          => return "/lib:/usr/lib";
                  when linux =>
                     return "/usr/lib/i386-linux-gnu:/usr/lib:/usr/lib32:/lib:/lib32";
               end case;
         end case;
      end get_base_paths;

      base_paths : constant String := get_base_paths;
      total_paths : Natural := 0;
   begin
      if record_base_libs then
         return True;
      end if;

      if base_paths /= "" then
         total_paths := Misc.count_char (base_paths, ':') + 1;
      end if;

      if total_paths = 0 then
         --  No system library paths are defined, allow everything
         return True;
      end if;

      for dirpath in 1 .. total_paths loop
         declare
            candidate : constant String := Misc.specific_field (base_paths, dirpath, ":") & "/" &
              library_file;
         begin
            if Unix.file_exists (candidate) then
               --  This is a system library.  Don't record it.
               return False;
            end if;
         end;
      end loop;

      --  This is not a system library.  Record it.
      return True;

   end record_library;


   ------------------------
   --  found_on_runpath  --
   ------------------------
   function found_on_runpath
     (library_file     : String;
      runpath          : String;
      origin_directory : String;
      stage_directory  : String;
      original_file    : String) return Boolean
   is
      total_paths : Natural := 0;
   begin
      if runpath /= "" then
         total_paths := Misc.count_char (runpath, ':') + 1;
      end if;
      if total_paths = 0 then
         --  No paths given, so search automatically fails
         return False;
      end if;

      for dirpath in 1 .. total_paths loop
         declare
            segment : constant String := Misc.specific_field (runpath, dirpath, ":") & "/" &
              library_file;
            candidate : constant String := Elf.ASU.To_String
              (Misc.replace_substring
                 (Elf.ASU.To_Unbounded_String (segment), "$ORIGIN", origin_directory));
         begin
            if Unix.file_exists (candidate) then
               --  installed library from a dependency package
               --  Also will catch this packages libraries if $ORIGIN is used.
               return True;
            end if;
            if Unix.file_exists (stage_directory & candidate) then
               --  The library was located in the stage directory.
               --  We don't know if the library is on the whitelist though -- only that the
               --  runpath will allow the linker to find it.
               --  We can't return "true" when file exists, but we can squawk if it doesn't.
               return False;
            end if;
         end;
      end loop;

      --  We didn't find it on the runpath or stage/runpath
      SQW.emit_debug (original_file & " - required shared library " & library_file &
                        " not found [" & runpath & "]");

      return False;

   end found_on_runpath;


   ------------------------------------------
   --  analyze_missing_required_libraries  --
   ------------------------------------------
   procedure analyze_missing_required_libraries (AS : in out Arc_Structure)
   is
      procedure check (Position : libraries_crate.Cursor)
      is
         missing_library : A_Filename renames libraries_crate.Element (Position);
      begin
         if AS.lib_prov.Contains (missing_library) then
            --  This package provides this library!
            return;
         end if;
         if AS.lib_adj.Contains (missing_library) then
            --  A sister subpackage provides this library
            return;
         end if;
         SQW.emit_notice ("The linked " & trim_trailing_zeros (missing_library) &
                         " library could not be located.");
      end check;
   begin
      AS.not_found.Iterate (check'Access);
   end analyze_missing_required_libraries;

end Archive.Pack;
