--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Ada.Text_IO;
with Ada.Exceptions;
with Ada.Directories;

package body Archive.Pack is

   package TIO renames Ada.Text_IO;
   package DIR renames Ada.Directories;
   package EX  renames Ada.Exceptions;

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
         if owngrp_count (AS.owners.Length) >= owngrp_count'Last then
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
         if owngrp_count (AS.groups.Length) >= owngrp_count'Last then
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
      meets_criteria : constant Boolean := (msg_level >= AS.level);
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
      dir_index : bits_16)
   is
      procedure walkdir   (item : DIR.Directory_Entry_Type);
      procedure walkfiles (item : DIR.Directory_Entry_Type);

      only_dirs : constant DIR.Filter_Type := (DIR.Directory => True, others => False);
      non_dirs  : constant DIR.Filter_Type := (DIR.Ordinary_File => True,
                                               DIR.Special_File => True,
                                               DIR.Directory => False);

      procedure walkdir (item : DIR.Directory_Entry_Type) is
      begin
         if DIR.Simple_Name (item) /= "." and then
           DIR.Simple_Name (item) /= ".."
         then
            AS.dtrack := AS.dtrack + 1;
            --  TODO create file header record from directory here
            AS.print (debug, DIR.Full_Name (item) & " (" & AS.dtrack'Img & ")");
            AS.scan_directory (DIR.Full_Name (item), AS.dtrack);
         end if;
      exception
         when DIR.Name_Error =>
            AS.print (normal, "walkdir: " & dir_path & " directory does not exist");
      end walkdir;

      procedure walkfiles (item : DIR.Directory_Entry_Type) is
      begin
         --  TODO create file header record from this file here
         AS.print (debug, "file = " & DIR.Full_Name (item));
      exception
         when DIR.Name_Error =>
            AS.print (normal, "walkfiles: " & dir_path & " directory does not exist");
      end walkfiles;
   begin
      DIR.Search (Directory => dir_path,
                  Pattern   => "*",
                  Filter    => only_dirs,
                  Process   => walkdir'Access);
      DIR.Search (Directory => dir_path,
                  Pattern   => "*",
                  Filter    => non_dirs,
                  Process   => walkfiles'Access);
   exception
      when DIR.Name_Error =>
         AS.print (normal, "The " & dir_path & " directory does not exist");
      when DIR.Use_Error =>
         AS.print (normal, "Searching " & dir_path & " directory is not supported");
      when failed : others =>
         AS.print (normal, "scan_directory: Unknown error - directory search");
         AS.print (normal, EX.Exception_Information (failed));
   end scan_directory;


end Archive.Pack;
