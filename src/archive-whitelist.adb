--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt


with Ada.Strings.Hash;
with Ada.Strings.Fixed;
with Ada.Text_IO;
with Archive.Unix;

package body Archive.Whitelist is

   package TIO renames Ada.Text_IO;
   package ASF renames Ada.Strings.Fixed;

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
      file_path     : String) return Boolean
   is
      file_hash : Blake_3.blake3_hash;
   begin
      file_hash := Blake_3.digest (file_path);

      if whitelist.files.Contains (file_hash) then
         --  True on files, False on directories
         return not whitelist.files.Element (file_hash);
      end if;
      return False;
   end file_on_whitelist;


   ------------------------------
   --  directory_on_whitelist  --
   ------------------------------
   function directory_on_whitelist
     (whitelist     : A_Whitelist;
      file_path     : String) return Boolean
   is
      file_hash : Blake_3.blake3_hash;
   begin
      file_hash := Blake_3.digest (file_path);

      if whitelist.files.Contains (file_hash) then
         --  False on files, True on directories
         return whitelist.files.Element (file_hash);
      end if;
      return False;
   end directory_on_whitelist;


   ----------------------------
   --  ingest_file_manifest  --
   ----------------------------
   function ingest_file_manifest
     (whitelist     : out A_Whitelist;
      manifest_file : String;
      top_directory : String;
      level         : info_level) return Boolean
   is
      features    : Unix.File_Characteristics;
      file_handle : TIO.File_Type;
      succeeded   : Boolean := True;
   begin
      if top_directory (top_directory'Last) = '/' then
         if level >= normal then
            TIO.Put_Line ("The top directory [" & top_directory & "] can't end with '/'");
         end if;
         return False;
      end if;
      features := Unix.get_charactistics (manifest_file);
      if features.error then
         if level >= normal then
            TIO.Put_Line ("The indicated manifest (" & manifest_file & ") does not exist.");
         end if;
         return False;
      elsif features.ftype /= regular then
         if level >= normal then
            TIO.Put_Line ("The indicated manifest is not a regular file.");
         end if;
         return False;
      end if;

      TIO.Open (File => file_handle,
                Mode => TIO.In_File,
                Name => manifest_file);
      while not TIO.End_Of_File (file_handle) loop
         declare
            line : constant String := ASF.Trim (TIO.Get_Line (file_handle), Ada.Strings.Both);
            insert_succeeded : Boolean;
         begin
            if line = "" then
               null;
            elsif line (line'First) = '/' then
               if level >= normal then
                  TIO.Put_Line ("Ignore [" & line & "] because it starts with '/'");
               end if;
               succeeded := False;
            else
               insert_succeeded := whitelist.insert_file_into_whitelist
                 (full_path     => Unix.real_path (top_directory & "/" & line),
                  real_top_path => Unix.real_path (top_directory),
                  level         => level);

               if not insert_succeeded then
                  succeeded := False;
               end if;
            end if;
         end;
      end loop;
      TIO.Close (file_handle);
      whitelist.list_used := True;
      return succeeded;

   end ingest_file_manifest;


   ----------------------------------
   --  insert_file_into_whitelist  --
   ----------------------------------
   function insert_file_into_whitelist
     (whitelist     : in out A_Whitelist;
      full_path     : String;
      real_top_path : String;
      level         : info_level) return Boolean
   is
      file_hash : Blake_3.blake3_hash;
      features  : Unix.File_Characteristics;
   begin
      features := Unix.get_charactistics (full_path);
      if features.error then
         if level >= normal then
            TIO.Put_Line ("The whitelisted file '" & full_path & "' does not exist.");
            return False;
         end if;
      elsif features.ftype = directory then
         whitelist.insert_directory_into_whitelist (dir_path      => full_path,
                                                    real_top_path => real_top_path,
                                                    level         => level);
         return True;
      else
         if whitelist.file_on_whitelist (full_path) then
            if level >= normal then
               TIO.Put_Line ("Duplicate line discovered: " & full_path);
            end if;
            return False;
         else
            if level >= verbose then
               TIO.Put_Line ("Adding to whitelist: " & full_path);
            end if;
            file_hash := Blake_3.digest (full_path);
            whitelist.files.Insert (file_hash, False);
         end if;
      end if;

      --  Now insert the file's directory tree
      whitelist.insert_directory_into_whitelist (dir_path      => head (full_path, "/"),
                                                 real_top_path => real_top_path,
                                                 level         => level);
      return True;
   end insert_file_into_whitelist;


   ---------------------------------------
   --  insert_directory_into_whitelist  --
   ---------------------------------------
   procedure insert_directory_into_whitelist
     (whitelist     : in out A_Whitelist;
      dir_path      : String;
      real_top_path : String;
      level         : info_level)
   is
      file_hash : Blake_3.blake3_hash;
   begin
      if real_top_path = dir_path then
         --  stop recursion
         return;
      end if;
      if not whitelist.directory_on_whitelist (dir_path) then
         if level >= debug then
            TIO.Put_Line ("Adding directory to whitelist: " & dir_path);
         end if;
         file_hash := Blake_3.digest (dir_path);
         whitelist.files.Insert (file_hash, True);
      end if;
      insert_directory_into_whitelist (whitelist     => whitelist,
                                       dir_path      => head (dir_path, "/"),
                                       real_top_path => real_top_path,
                                       level         => level);
   end insert_directory_into_whitelist;


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


   ------------
   --  head  --
   ------------
   function head (S : String; delimiter : String) return String
   is
      dl_size      : constant Natural := delimiter'Length;
      back_marker  : constant Natural := S'First;
      front_marker : Natural := S'Last - dl_size + 1;
   begin
      loop
         if front_marker < back_marker then
            --  delimiter never found
            return "";
         end if;
         if S (front_marker .. front_marker + dl_size - 1) = delimiter then
            return S (back_marker .. front_marker - 1);
         end if;
         front_marker := front_marker - 1;
      end loop;
   end head;

end Archive.Whitelist;
