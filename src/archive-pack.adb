--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Ada.Text_IO;
with Ada.Exceptions;
with Ada.Directories;
with Archive.Unix;
with Blake_3;

package body Archive.Pack is

   package TIO renames Ada.Text_IO;
   package DIR renames Ada.Directories;
   package EX  renames Ada.Exceptions;
   package UNX renames Archive.Unix;

   ------------------------------------------------------------------------------------------
   --  integrate
   ------------------------------------------------------------------------------------------
   procedure integrate
     (top_level_directory : String;
      output_file         : String;
      verbosity           : info_level)
   is
      pragma Unreferenced (output_file);

      metadata : Arc_Structure;
   begin
      metadata.set_verbosity (verbosity);
      metadata.record_directory (top_level_directory);
      metadata.scan_directory (top_level_directory, 0);
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
               new_block.flat_size    := 0;
               new_block.file_perms   := features.perms;
               new_block.link_length  := 0;
               new_block.index_parent := AS.dtrack;
               new_block.padding      := (others => Character'Val (0));

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
            new_block.padding      := (others => Character'Val (0));

            case features.ftype is
               when directory | unsupported => null;   --  impossible
               when symlink =>
                  new_block.blake_sum    := null_sum;
                  new_block.type_of_file := symlink;
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
                  new_block.flat_size    := 0;
                  new_block.link_length  := 0;
               when regular =>
                  new_block.blake_sum    := Blake_3.file_digest (item_path);
                  new_block.type_of_file := regular;
                  new_block.flat_size    := size_type (DIR.Size (item_path));
                  new_block.link_length  := 0;
               when hardlink =>
                  new_block.blake_sum    := Blake_3.file_digest (item_path);
                  new_block.flat_size    := size_type (DIR.Size (item_path));
                  if AS.inode_already_seen (features.inode) then
                     new_block.type_of_file := hardlink;
                     declare
                        target : constant String := AS.retrieve_inode_path (features.inode);
                     begin
                        new_block.link_length := max_path (target'Length);
                        AS.push_link (target);
                     end;
                  else
                     AS.insert_inode (features.inode, item_path);
                     new_block.type_of_file := regular;
                     new_block.link_length  := 0;
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
      --  "hello/there/you/shmuck"
      --  toplevel = "hello/there"
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


end Archive.Pack;
