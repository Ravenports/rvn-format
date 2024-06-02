--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Ada.Strings.Unbounded;

package Archive.Misc is

   package ASU renames Ada.Strings.Unbounded;


   --  Head (keep all but last delimiter and field)
   function head (S : String; delimiter : String) return String;

   --  Tail (keep only last field)
   function tail (S : String; delimiter : String) return String;

   --  unpadded numeric image
   function int2str (A : Integer) return String;

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

   --  This is operating-system specific (normally /bin/sh though)
   function get_interpreter return String;

   --  This is operating system specific.  It can be overridden with ABI_FILE in
   --  the environment.  Each option is validated for existence.
   function select_abi_determination_file return String;

   --  This function reads the ELF notes of the abi determination file and assembles
   --  the ABI based on the note's contents.
   --  The algorithm is platform specific.
   function determine_abi return String;

   --  If first part is "/", returns "/<second_part>"  (avoids double slash)
   --  If first part is blank, returns "/<second_part>"
   --  If first part is not blank, returns "<first_part>/<second_part>"
   function join_path (first_part : String; second_part : String) return String;

private

   subtype doubleword is String (1 .. 4);

   --  This can overflow, make it return 7FFF_FFFF as the maximum
   function convert_doubleword (big_endian : Boolean; dw : doubleword) return Natural;

end Archive.Misc;
