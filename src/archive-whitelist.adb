--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt


with Ada.Strings.Hash;

package body Archive.Whitelist is

   -----------------------
   --  whiteist_in_use  --
   -----------------------
   function whitelist_in_use (whitelist : A_Whitelist) return Boolean is
   begin
      return whitelist.list_used;
   end whitelist_in_use;


   -------------------------
   --  file_on_whitelist  --
   -------------------------
   function file_on_whitelist
     (whitelist     : A_Whitelist;
      file_path     : String;
      file_data     : Unix.File_Characteristics) return Boolean
   is
      file_hash : Blake_3.blake3_hash;
   begin
      if file_data.error then
         --  No file (or directory) exists at the given file_path
         return False;
      end if;

      file_hash := Blake_3.digest (file_path);

      return whitelist.files.Contains (file_hash);

   end file_on_whitelist;


   ----------------------------
   --  ingest_file_manifest  --
   ----------------------------
   function ingest_file_manifest
     (whitelist     : A_Whitelist;
      manifest_file : String;
      level         : info_level) return Boolean
   is
   begin
      --  TODO: To be written
      return False;
   end ingest_file_manifest;


   -------------------
   --  digest_hash  --
   -------------------
   function digest_hash (key : Blake_3.blake3_hash) return CON.Hash_Type
   is
   begin
      return Ada.Strings.Hash (key);
   end digest_hash;


   -------------------------
   --  digest_equivalent  --
   -------------------------
   function digest_equivalent (key1, key2 : Blake_3.blake3_hash) return Boolean
   is
   begin
      return key1 = key2;
   end digest_equivalent;

end Archive.Whitelist;
