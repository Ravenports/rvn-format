--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

private with Ada.Strings.Unbounded;
private with ThickUCL;

package Archive.Whitelist.Keywords is


   function process_external_keyword
     (whitelist     : in out A_Whitelist;
      keyword       : String;
      arguments     : String;
      keyword_dir   : String;
      full_path     : String;
      real_top_path : String;
      prefix_dir    : String;
      level         : info_level) return Boolean;

private

   package ASU renames Ada.Strings.Unbounded;

   type Action_Type is (no_action, file_action, directory_action);

   type A_Keyword is tagged
      record
         file_found         : Boolean;
         preformat          : Boolean;
         deprecated         : Boolean;
         deprecated_message : ASU.Unbounded_String;
         tree               : ThickUCL.UclTree;
         action             : Action_Type;
         split_args         : arg_crate.Vector;
         level              : info_level;
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

   --  convert phase into a UCL key
   function convert (phase : package_phase) return String;

   --  Read the keyword UCL files and set some internal variables from it
   procedure scan_file
     (keyword  : in out A_Keyword;
      filename : String;
      level    : info_level);

   --  Perform preformat_arguments if requested
   --  Split arguments into %@, %1, %2, etc (stored as 0,1,2,3, etc)
   procedure process_arguments
     (keyword : in out A_Keyword;
      arguments : String;
      prefix    : String;
      full_path : String;
      stagedir  : String);

   --  Returns number of instances of a given character in a given string
   function count_char (S : String; focus : Character) return Natural;

   --  Given a single line (presumably no line feeds) with data separated by <delimited>,
   --  return the field given by field_number (starts counting at 1).
   function specific_field
     (S            : String;
      field_number : Positive;
      delimiter    : String := " ") return String;

   --  Replace substring with another string
   function replace_substring
     (US : ASU.Unbounded_String;
      old_string : String;
      new_string : String) return ASU.Unbounded_String;

end Archive.Whitelist.Keywords;
