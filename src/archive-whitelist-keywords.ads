--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Ada.Text_IO;

private with Ada.Strings.Unbounded;
private with ThickUCL;

package Archive.Whitelist.Keywords is


   function process_external_keyword
     (whitelist     : in out A_Whitelist;
      keyword       : String;
      arguments     : String;
      keyword_dir   : String;
      real_top_path : String;
      prefix_dir    : String;
      last_file     : String;
      namebase      : String;
      subpackage    : String;
      variant       : String;
      level         : info_level;
      extract_log   : Ada.Text_IO.File_Type) return Boolean;

private

   package ASU renames Ada.Strings.Unbounded;

   type Action_Type is (file_action, directory_action);
   type Message_Set is array (Message_Type) of ASU.Unbounded_String;

   package action_set is new CON.Vectors
     (Index_Type => Natural,
      Element_Type => Action_Type);

   type A_Keyword is tagged
      record
         scan_failed        : Boolean;
         file_found         : Boolean;
         deprecated         : Boolean;
         deprecated_message : ASU.Unbounded_String;
         messages           : Message_Set;
         tree               : ThickUCL.UclTree;
         actions            : action_set.Vector;
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

   --  Returns false if script has more % argument tokens than arguments.
   function valid_template
     (keyword_obj : A_Keyword;
      keyword     : String;
      script      : String) return Boolean;

   --  Replaces %0, %1, %2, %@ tokens with $0, $1, $2, $@ and return as unbounded strings
   function populate_template (script : String) return ASU.Unbounded_String;

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
      last_file : String;
      stagedir  : String);

   --  Replaces every occurrance of "token" with "replacement" and returns the result
   function token_expansion (S, token, replacement : String; level : info_level) return String;

end Archive.Whitelist.Keywords;
