--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Blake_3;
private with Ada.Containers.Hashed_Maps;
private with Ada.Containers.Vectors;
private with Ada.Strings.Unbounded;

package Archive.Whitelist is

   type A_Whitelist is tagged private;

   type white_features is
      record
         owner_spec : ownergroup;
         group_spec : ownergroup;
         perms_spec : permissions;
      end record;

   type package_phase is
     (pre_install,
      pre_install_lua,
      pre_deinstall,
      pre_deinstall_lua,
      post_install,
      post_install_lua,
      post_deinstall,
      post_deinstall_lua);

   type Message_Type is (install, deinstall, upgrade);

   --  Returns true if archive should only archive specifically designated files.
   --  Likewise, returns false if all files in the root directory should be archived.
   function whitelist_in_use (whitelist : A_Whitelist) return Boolean;


   --  Takes a path of a list of files contained in the stage directory that are designed
   --  to be archived.  There is one file per line.  Lines starting with forward slashes
   --  are relative to the stage directory while the others are relative to the prefix
   --  directory.  Illegal lines will have the error sent to standard out depending
   --  on the provided verbosity level.
   --  All directory components are individually whitelisted
   --  Returns True upon success, False if an error occurs
   function ingest_file_manifest
     (whitelist          : out A_Whitelist;
      manifest_file      : String;
      stage_directory    : String;
      prefix_directory   : String;
      keywords_directory : String;
      namebase           : String;
      subpackage         : String;
      variant            : String;
      level              : info_level) return Boolean;


   --  Returns true if given path has been whitelisted (and therefore needs to be archived).
   --  If the path is a directory, False is returned
   function file_on_whitelist
     (whitelist     : A_Whitelist;
      file_path     : String) return Boolean;


   --  Returns true if given path has been whitelisted and is a directory.
   function directory_on_whitelist
     (whitelist     : A_Whitelist;
      file_path     : String) return Boolean;


   --  Return features to record on the manifest
   function get_file_features
     (whitelist     : A_Whitelist;
      file_path     : String;
      actual_owner  : ownergroup;
      actual_group  : ownergroup;
      actual_perms  : permissions) return white_features;


   --  Return number of directories that must be created/destroyed during package operations
   function empty_directory_count
     (whitelist     : A_Whitelist) return Natural;


   --  Return directory hash referenced by index
   function get_empty_directory_hash
     (whitelist     : A_Whitelist;
      index         : Natural) return Blake_3.blake3_hash;


   --  return path to directory referenced by index
   function get_empty_directory_path
     (whitelist     : A_Whitelist;
      index         : Natural) return String;


   --  Return directory attributes referenced by index
   function get_empty_directory_attributes
     (whitelist     : A_Whitelist;
      index         : Natural) return white_features;


   --  Returns the number of scripts defined for this package package
   function script_count
     (whitelist     : A_Whitelist;
      phase         : package_phase) return Natural;


   --  Returns the script mapped to the given phase and index
   function get_script
     (whitelist     : A_Whitelist;
      phase         : package_phase;
      index         : Natural) return String;


   --  Returns the arguments mapped to a given phase and index
   function get_arguments
     (whitelist     : A_Whitelist;
      phase         : package_phase;
      index         : Natural) return String;


   --  Returns the number of messages defined for this message type
   function message_count
     (whitelist     : A_Whitelist;
      msg_type      : Message_Type) return Natural;


   --  Returns the script mapped to the given message type and index
   function get_message
     (whitelist     : A_Whitelist;
      msg_type      : Message_Type;
      index         : Natural) return String;


   --  Returns the metadata key mapped to the package phase
   function convert_phase (phase : package_phase) return String;


   --  Get the message object key for each type of message
   function get_message_key (msgtype : Message_Type) return String;


private

   package CON renames Ada.Containers;
   package ASU renames Ada.Strings.Unbounded;

   function digest_hash (key : Blake_3.blake3_hash) return CON.Hash_Type;
   function digest_equivalent (key1, key2 : Blake_3.blake3_hash) return Boolean;

   type white_properties is
      record
         is_directory   : Boolean;
         override_owner : Boolean := False;
         override_group : Boolean := False;
         override_perms : Boolean := False;
         owner_spec     : ownergroup := null_owngrp;
         group_spec     : ownergroup := null_owngrp;
         perms_spec     : permissions := 0;
         path           : ASU.Unbounded_String;
      end record;

   type phase_script is
      record
         code : ASU.Unbounded_String;
         args : ASU.Unbounded_String;
      end record;

   package phase_crate is new CON.Vectors
     (Index_Type => Natural,
      Element_Type => phase_script);

   type keyword_argument is
      record
         argument : ASU.Unbounded_String;
      end record;

   package arg_crate is new CON.Vectors
     (Index_Type => Natural,
      Element_Type => keyword_argument);

   package dir_keys_crate is new CON.Vectors
     (Index_Type => Natural,
      Element_Type => Blake_3.blake3_hash);

   type maintenance is array (package_phase'Range) of phase_crate.Vector;

   package white_crate is new CON.Hashed_Maps
     (Key_Type        => Blake_3.blake3_hash,
      Element_Type    => white_properties,
      Hash            => digest_hash,
      Equivalent_Keys => digest_equivalent);

   package text_crate is new CON.Vectors
     (Index_Type   => Natural,
      Element_Type => ASU.Unbounded_String,
      "="          => ASU."=");

   type Message_Collection is array (Message_Type) of text_crate.Vector;

   type A_Whitelist is tagged
      record
         list_used   : Boolean := False;
         level       : info_level;
         files       : white_crate.Map;
         temp_dirs   : white_crate.Map;
         just_dirs   : white_crate.Map;
         dirs_keys   : dir_keys_crate.Vector;
         scripts     : maintenance;
         messages    : Message_Collection;
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

   --  This procedure stores directories from @dir keyword
   --  After whitelist ingestion they are all checked they they aren't already defined
   procedure insert_temporary_directory
     (whitelist     : in out A_Whitelist;
      dir_path      : String;
      true_path     : String;
      level         : info_level);

   --  second version, handles @dir(,,) format
   --  If directory already exists, the attributes will be overwritten.
   procedure insert_temporary_directory
     (whitelist     : in out A_Whitelist;
      dir_path      : String;
      true_path     : String;
      attr_owner    : String;
      attr_group    : String;
      attr_perms    : String;
      level         : info_level);

   --  Similar to insert_file_into_whitelist() but it also records owner/group/mode changes
   function ingest_manifest_with_mode_override
     (whitelist     : in out A_Whitelist;
      full_path     : String;
      real_top_path : String;
      new_owner     : String;
      new_group     : String;
      new_perms     : String;
      level         : info_level) return Boolean;

   --  Iterate through the temporary directories.
   --  If one is already defined, check the POG attributes.
   --  If the POG attributes match, issue a notice that keyword is unnecessary and ignored.
   --  If the POG attributes don't match, update them to match (no notice)
   --  If the directory wasn't already defined, move it to the just_dirs container.
   --  When done, clear the temp directories container
   procedure process_temporary_directories
     (whitelist : in out A_Whitelist;
      level     : info_level);



   --  Takes a single like "4555" and returns the equivalent as permissions type.
   --  Must be 3 or 4 characters long, consisting of only '0' .. '7' characters
   function convert_mode (S : String; success : out Boolean) return permissions;

   --  Trims both sides first.  If there's no space, return the result.
   --  If it does contain a space, return the first word.
   function first_word (S : String) return String;

end Archive.Whitelist;
