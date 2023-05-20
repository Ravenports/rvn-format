--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

private with Blake_3;
private with Ada.Containers.Hashed_Maps;

package Archive.Whitelist is

   type A_Whitelist is tagged private;

   --  Returns true if archive should only archive specifically designated files.
   --  Likewise, returns false if all files in the root directory should be archived.
   function whitelist_in_use (whitelist : A_Whitelist) return Boolean;


   --  Takes a patch to a list of files contained in the stage directory that are designed
   --  to be archived.  There is one file per line, and every line must start with a
   --  forward slash.  Illegal lines will have the error send to standard out depending
   --  on the provided verbosity level.
   --  All directory components are individually whitelisted
   --  Returns True upon success, False if a single error occurs
   function ingest_file_manifest
     (whitelist     : out A_Whitelist;
      manifest_file : String;
      top_directory : String;
      level         : info_level) return Boolean;


   --  Returns true if given path has been whitelisted (and therefore needs to be archived).
   --  If the path is a directory, False is returned
   function file_on_whitelist
     (whitelist     : A_Whitelist;
      file_path     : String) return Boolean;


   --  Returns true if given path has been whitelisted and is a directory.
   function directory_on_whitelist
     (whitelist     : A_Whitelist;
      file_path     : String) return Boolean;

private

   package CON renames Ada.Containers;

   function digest_hash (key : Blake_3.blake3_hash) return CON.Hash_Type;
   function digest_equivalent (key1, key2 : Blake_3.blake3_hash) return Boolean;

   package white_crate is new CON.Hashed_Maps
     (Key_Type        => Blake_3.blake3_hash,
      Element_Type    => Boolean,
      Hash            => digest_hash,
      Equivalent_Keys => digest_equivalent);

   type A_Whitelist is tagged
      record
         list_used : Boolean := False;
         level     : info_level;
         files     : white_crate.Map;
      end record;

   --  If the full path is not already in the whitelist, it will be inserted.
   --  Returns false if the full_path doesn't point to a real object.
   function insert_file_into_whitelist
     (whitelist     : in out A_Whitelist;
      full_path     : String;
      real_top_path : String;
      level         : info_level) return Boolean;

   --  This procedure is only called by `insert_file_into_whitelist`.  It ensures every
   --  component of the directory tree has an entry.
   --  The caller ensures dir_path points to a directory.
   procedure insert_directory_into_whitelist
     (whitelist     : in out A_Whitelist;
      dir_path      : String;
      real_top_path : String;
      level         : info_level);

   --  Head (keep all but last delimiter and field)
   function head (S  : String; delimiter : String) return String;

end Archive.Whitelist;
