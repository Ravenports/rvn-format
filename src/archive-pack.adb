--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Text_IO;
with Ada.Exceptions;
with Ada.IO_Exceptions;
with Ada.Directories;
with Archive.Unix;
with Zstandard;
with Blake_3;

package body Archive.Pack is

   package TIO renames Ada.Text_IO;
   package DIR renames Ada.Directories;
   package EX  renames Ada.Exceptions;
   package IOX renames Ada.IO_Exceptions;
   package UNX renames Archive.Unix;
   package ZST renames Zstandard;

   ------------------------------------------------------------------------------------------
   --  integrate
   ------------------------------------------------------------------------------------------
   procedure integrate
     (top_level_directory : String;
      metadata_file       : String;
      output_file         : String;
      verbosity           : info_level)
   is
      metadata : Arc_Structure;
   begin
      metadata.set_verbosity (verbosity);
      metadata.record_directory (top_level_directory);
      metadata.create_working_file (output_file);
      metadata.scan_directory (top_level_directory, 0);
      metadata.finalize_working_file;

      if not metadata.serror then
         declare
            mdcomp : constant String := metadata.scan_metadata_file (metadata_file);
         begin
            metadata.write_output_header (output_file);  --  block 1
            metadata.write_owngrp_blocks;                --  block 2 and 3
            metadata.write_link_block;                   --  block 4
            metadata.write_file_index_block;             --  block 5
            metadata.write_metadata_block (mdcomp);      --  block 6 (compressed manifest file)
            metadata.write_archive_block (output_file);  --  block 7 (contiguous archive)
         end;
      end if;
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
         AS.print (debug, "New owner " & owner);
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
         AS.print (debug, "New group " & group);
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
      procedure walkdir   (item : DIR.Directory_Entry_Type);
      procedure walkfiles (item : DIR.Directory_Entry_Type);
      function get_filename (item : DIR.Directory_Entry_Type; part : Positive) return A_fragment;

      no_filter : constant DIR.Filter_Type := (others => True);

      procedure walkdir (item : DIR.Directory_Entry_Type) is
      begin
         --  We only want true directories.  Symbolic links to directories are ignored.
         case DIR.Kind (item) is
            when DIR.Directory => null;
            when others => return;
         end case;
         if DIR.Simple_Name (item) /= "." and then
           DIR.Simple_Name (item) /= ".."
         then
            declare
               new_block : File_Block;
               features  : UNX.File_Characteristics;
            begin
               features := UNX.get_charactistics (DIR.Full_Name (item));
               if features.ftype /= directory then
                  return;
               end if;

               AS.dtrack := AS.dtrack + 1;
               new_block.filename_p1  := get_filename (item, 1);
               new_block.filename_p2  := get_filename (item, 2);
               new_block.filename_p3  := get_filename (item, 3);
               new_block.filename_p4  := get_filename (item, 4);
               new_block.blake_sum    := null_sum;
               new_block.modified     := features.mtime;
               new_block.index_owner  := AS.get_owner_index (features.owner);
               new_block.index_group  := AS.get_group_index (features.group);
               new_block.type_of_file := directory;
               new_block.multiplier   := 0;
               new_block.flat_size    := 0;
               new_block.file_perms   := features.perms;
               new_block.link_length  := 0;
               new_block.index_parent := AS.dtrack;
               new_block.padding      := (others => 0);

               AS.files.Append (new_block);
               AS.print (debug, DIR.Full_Name (item) & " (" & AS.dtrack'Img & ")");
               AS.print (debug, "owner =" & new_block.index_owner'Img & "  group =" &
                           new_block.index_group'Img);
               AS.print (debug, "perms =" & features.perms'Img & "   mod =" & features.mtime'Img);
            end;
            AS.scan_directory (DIR.Full_Name (item), AS.dtrack);
         end if;
      exception
         when DIR.Name_Error =>
            AS.print (normal, "walkdir: " & dir_path & " directory does not exist");
         when failed : others =>
            AS.print (normal, "walkdir exception => " & EX.Exception_Information (failed) &
                        "  file: " & DIR.Full_Name (item));
      end walkdir;

      procedure walkfiles (item : DIR.Directory_Entry_Type)
      is
         --  Reject directories, but accept symlinks to directories
         features  : UNX.File_Characteristics;
         item_path : constant String := DIR.Full_Name (item);
      begin
         features := UNX.get_charactistics (item_path);

         case features.ftype is
            when directory => return;
            when unsupported =>
               AS.print
                 (normal, "NOTICE: Unsupported file " & item_path & " ignored.");
               return;
            when others => null;
         end case;

         declare
            --  Blake3 sums for regular files and hardlinks
            --  The first hardlink is written as a regular file.
            new_block : File_Block;

            use type DIR.File_Size;
         begin
            new_block.filename_p1  := get_filename (item, 1);
            new_block.filename_p2  := get_filename (item, 2);
            new_block.filename_p3  := get_filename (item, 3);
            new_block.filename_p4  := get_filename (item, 4);
            new_block.modified     := features.mtime;
            new_block.index_owner  := AS.get_owner_index (features.owner);
            new_block.index_group  := AS.get_group_index (features.group);
            new_block.file_perms   := features.perms;
            new_block.index_parent := dir_index;
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
                        AS.print (debug, Blake_3.hex (new_block.blake_sum) &
                                    " " & DIR.Simple_Name (item));
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
                        AS.print (debug, Blake_3.hex (new_block.blake_sum) &
                                    " " & DIR.Simple_Name (item));
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
         end;

         AS.print (debug, "file = " & item_path & " (" & features.ftype'Img & ")");
      exception
         when DIR.Name_Error =>
            AS.print (normal, "walkfiles: " & dir_path & " directory does not exist");
         when failed : others =>
            AS.print (normal, "walkfiles exception => " & EX.Exception_Information (failed) &
                        "  file: " & item_path);
      end walkfiles;

      function get_filename (item : DIR.Directory_Entry_Type; part : Positive) return A_fragment
      is
         result : A_fragment;
         tray   : A_filename := (others => Character'Val (0));
         sname  : constant String := DIR.Simple_Name (item);
         head   : constant Natural := tray'First + (part - 1) * A_fragment'Length;
         tail   : constant Natural := head + A_fragment'Length - 1;
      begin
         tray (result'First .. result'First + sname'Length - 1) := sname;
         result := tray (head .. tail);
         return result;
      end get_filename;
   begin
      DIR.Search (Directory => dir_path,
                  Pattern   => "*",
                  Filter    => no_filter,
                  Process   => walkdir'Access);
      DIR.Search (Directory => dir_path,
                  Pattern   => "*",
                  Filter    => no_filter,
                  Process   => walkfiles'Access);
   exception
      when DIR.Name_Error =>
         AS.print (normal, "The " & dir_path & " directory does not exist");
      when DIR.Use_Error =>
         AS.print (normal, "Searching " & dir_path & " directory is not supported");
      when failed : others =>
         AS.print (normal, "scan_directory: Unknown error - directory search");
         AS.print (normal, "=> " & EX.Exception_Information (failed));
   end scan_directory;


   ------------------------------------------------------------------------------------------
   --  record_redirectory
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
      AS.print (debug, "Pushing " & link & " to stack");
      for index in link'Range loop
         AS.links.Append (link (index));
      end loop;
   end push_link;


   ------------------------------------------------------------------------------------------
   --  create_working_file
   ------------------------------------------------------------------------------------------
   procedure create_working_file (AS : in out Arc_Structure; output_file_path : String)
   is
   begin
      SIO.Create (File => AS.tmp_handle,
                  Mode => SIO.Out_File,
                  Name => output_file_path & ".working");
      AS.tmp_stmaxs := SIO.Stream (AS.tmp_handle);
   end create_working_file;


   ------------------------------------------------------------------------------------------
   --  finalize_working_file
   ------------------------------------------------------------------------------------------
   procedure finalize_working_file (AS : in out Arc_Structure)
   is
   begin
      SIO.Close (AS.tmp_handle);
   end finalize_working_file;


   ------------------------------------------------------------------------------------------
   --  write_output_header
   ------------------------------------------------------------------------------------------
   procedure write_output_header (AS : in out Arc_Structure; output_file_path : String)
   is
      block    : premier_block;
      nlbfloat : constant Float := Float (AS.links.Length) / 32.0;
   begin
      block.magic_bytes     := magic;
      block.version         := format_version;
      block.num_groups      := index_type (AS.groups.Length);
      block.num_owners      := index_type (AS.owners.Length);
      block.link_blocks     := file_index (Float'Ceiling (nlbfloat));
      block.file_blocks     := file_index (AS.files.Length);
      block.metadata_blocks := AS.cmblocks;
      block.metadata_size   := AS.cmsize;
      block.padding         := (others => 0);

      SIO.Create (File => AS.rvn_handle,
                  Mode => SIO.Out_File,
                  Name => output_file_path);
      AS.rvn_stmaxs := SIO.Stream (AS.rvn_handle);

      premier_block'Output (AS.rvn_stmaxs, block);
   end write_output_header;


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
         owngrp_block'Output (AS.rvn_stmaxs, block);
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
            single_line'Output (AS.rvn_stmaxs, line);
         end;
         line_index := line_index + 32;
      end loop;
   end write_link_block;


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
         File_Block'Output (AS.rvn_stmaxs, item);
      end write;
   begin
      AS.files.Iterate (write'Access);
   end write_file_index_block;


   ------------------------------------------------------------------------------------------
   --  write_metadata_block
   ------------------------------------------------------------------------------------------
   procedure write_metadata_block (AS : Arc_Structure; compressed_data : String)
   is
      type metadata_block is
         record
            payload : String (1 .. 32) := (others => Character'Val (0));
         end record;
      for metadata_block'Size use 32;

      ndx_s : Natural := compressed_data'First;
   begin
      for blockid in 1 .. AS.cmblocks loop
         declare
            block : metadata_block;
            blast : Natural;
         begin
            if blockid = AS.cmblocks then
               blast := compressed_data'Last - ndx_s + 1;
               block.payload (1 .. blast) := compressed_data (ndx_s .. compressed_data'Last);
            else
               block.payload := compressed_data (ndx_s .. ndx_s + 31);
            end if;
            metadata_block'Output (AS.rvn_stmaxs, block);
         end;
         ndx_s := ndx_s + 32;
      end loop;
   end write_metadata_block;



   ------------------------------------------------------------------------------------------
   --  write_archive_block
   ------------------------------------------------------------------------------------------
   procedure write_archive_block (AS : Arc_Structure; output_file_path : String)
   is
      out_succ : Boolean;
      out_size : Natural;
      uncompressed_archive : constant String := output_file_path & ".working";
      archive_size : constant Natural := Natural (DIR.Size (uncompressed_archive));
   begin
      ZST.incorporate_regular_file
        (filename    => uncompressed_archive,
         file_size   => archive_size,
         quality     => 7,
         target_saxs => AS.rvn_stmaxs,
         target_file => AS.rvn_handle,
         output_size => out_size,
         successful  => out_succ);
      if out_succ then
         AS.print (debug, "Compressed from" & archive_size'Img & " to" & out_size'Img);
      else
         AS.print (normal, "Failed to compress " & uncompressed_archive);
      end if;
      begin
         DIR.Delete_File (uncompressed_archive);
      exception
         when others =>
            AS.print (normal, "Failed to remove " & uncompressed_archive);
      end;

   end write_archive_block;


   ------------------------------------------------------------------------------------------
   --  scan_metadata_file
   ------------------------------------------------------------------------------------------
   function scan_metadata_file (AS : in out Arc_Structure; metadata_path : String) return String
   is
      it_worked : Boolean;
      compstr : constant String := ZST.compress_into_memory (filename   => metadata_path,
                                                             quality    => 7,
                                                             successful => it_worked);
      cmfloat : Float;
   begin
      if it_worked then
         AS.cmsize := compstr'Length;
         cmfloat := Float (AS.cmsize) / 32.0;
         AS.cmblocks := index_type (Float'Ceiling (cmfloat));
         return compstr;
      else
         AS.cmsize := 0;
         return "";
      end if;
   end scan_metadata_file;

end Archive.Pack;
