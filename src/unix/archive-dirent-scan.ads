--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Ada.Containers.Vectors;
private with Interfaces.C.Strings;

package Archive.Dirent.Scan is

   package CON renames Ada.Containers;

   package dscan_crate is new CON.Vectors
     (Index_Type => Natural,
      Element_Type => Directory_Entity);

   --  Fills the given contain with the entities of the given directory.  The
   --  container is sorted before the procedure returns.
   procedure scan_directory
     (directory : String;
      crate     : in out dscan_crate.Vector);

   dscan_folder_already_open : exception;
   dscan_folder_not_open     : exception;
   dscan_open_failure        : exception;
   dscan_close_failure       : exception;

private

   package IC renames Interfaces.C;
   package dscan_sorting is new dscan_crate.Generic_Sorting;

   function walkdir_open_folder (path : IC.char_array) return IC.int;
   pragma Import (C, walkdir_open_folder);

   function walkdir_close_folder return IC.int;
   pragma Import (C, walkdir_close_folder);

   function walkdir_next_entry return IC.Strings.chars_ptr;
   pragma Import (C, walkdir_next_entry);

end Archive.Dirent.Scan;
