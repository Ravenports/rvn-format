--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

private with Ada.Strings.Unbounded;

package Archive.Whitelist.Keywords is


   function process_external_keyword
     (whitelist     : in out A_Whitelist;
      keyword       : String;
      arguments     : String;
      keyword_dir   : String;
      full_path     : String;
      real_top_path : String;
      level         : info_level) return Boolean;

private

   package ASU renames Ada.Strings.Unbounded;

   type Action_Type is (no_action, file_action, directory_action);

   type A_Keyword is tagged
      record
         deprecated         : Boolean;
         deprecated_message : ASU.Unbounded_String;
      end record;

   --  returns the owner attribute of file and directory actions
   function get_owner (keyword : A_Keyword) return String;

   --  returns the group attribute of file and directory actions
   function get_group (keyword : A_Keyword) return String;

   --  returns the permissions attribute of file and directory actions
   function get_permissions (keyword : A_Keyword) return String;

   --  returns true if a script is defined for a given package phase.
   function phase_script_defined (keyword : A_Keyword; phase : package_phase) return Boolean;

   --  returns the script for a given package phase
   function retrieve_script (keyword : A_Keyword; phase : package_phase) return String;

end Archive.Whitelist.Keywords;
