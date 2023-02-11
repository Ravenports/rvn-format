--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Ada.Text_IO;
with Ada.Exceptions;
with Ada.Directories;
with Archive.Unix;

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
      function get_filename (item : DIR.Directory_Entry_Type) return A_filename;

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
               new_block.filename     := get_filename (item);
               new_block.blake_sum    := null_sum;
               new_block.index_owner  := AS.get_owner_index (features.owner);
               new_block.index_group  := AS.get_group_index (features.group);
               new_block.type_of_file := directory;
               new_block.file_perms   := features.perms;
               new_block.flat_size    := 0;
               new_block.link_length  := 0;
               new_block.modified     := features.mtime;
               new_block.index_parent := AS.dtrack;

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
      begin
         features := UNX.get_charactistics (DIR.Full_Name (item));

         case features.ftype is
            when directory => return;
            when unsupported =>
               AS.print
                 (normal, "NOTICE: Unsupported file " & DIR.Full_Name (item) & " ignored.");
               return;
            when others => null;
         end case;

         --  TODO create file header record from this file here
         AS.print (debug, "file = " & DIR.Full_Name (item) & " (" &
                  features.ftype'Img & ")");
      exception
         when DIR.Name_Error =>
            AS.print (normal, "walkfiles: " & dir_path & " directory does not exist");
         when failed : others =>
            AS.print (normal, "walkfiles exception => " & EX.Exception_Information (failed) &
                        "  file: " & DIR.Full_Name (item));
      end walkfiles;

      function get_filename (item : DIR.Directory_Entry_Type) return A_filename
      is
         result : A_filename := (others => Character'Val (0));
         sname  : constant String := DIR.Simple_Name (item);
      begin
         result (result'First .. result'First + sname'Length - 1) := sname;
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


end Archive.Pack;
