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

end Archive.Misc;
