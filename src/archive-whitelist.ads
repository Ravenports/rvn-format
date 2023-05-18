--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Archive.Unix;
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
     (whitelist     : A_Whitelist;
      manifest_file : String;
      level         : info_level) return Boolean;


   --  Returns true if given path has been whitelisted (and therefore needs to be archived).
   function file_on_whitelist
     (whitelist     : A_Whitelist;
      file_path     : String;
      file_data     : Unix.File_Characteristics) return Boolean;

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
         list_used : Boolean;
         level     : info_level;
         files     : white_crate.Map;
      end record;


end Archive.Whitelist;
