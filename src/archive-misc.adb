--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Ada.Strings.Fixed;

package body Archive.Misc is

   package AS renames Ada.Strings;

   ------------------
   --  count_char  --
   ------------------
   function count_char (S : String; focus : Character) return Natural
   is
      result : Natural := 0;
   begin
      for x in S'Range loop
         if S (x) = focus then
            result := result + 1;
         end if;
      end loop;
      return result;
   end count_char;


   ----------------------
   --  specific_field  --
   ----------------------
   function specific_field
     (S            : String;
      field_number : Positive;
      delimiter    : String := " ") return String
   is
      back  : Integer;
      dsize : constant Natural := delimiter'Length;
      front : Integer := S'First;
   begin
      for field in 1 .. field_number - 1 loop
         back := AS.Fixed.Index (Source => S, Pattern => delimiter, From => front);
         if back <= 0 then
            return "";
         end if;
         front := back + dsize;
      end loop;
      back := AS.Fixed.Index (Source => S, Pattern => delimiter, From => front);
      if back > 0 then
         return S (front .. back - 1);
      else
         return S (front .. S'Last);
      end if;
   end specific_field;


   -------------------------
   --  replace_substring  --
   -------------------------
   function replace_substring
     (US : ASU.Unbounded_String;
      old_string : String;
      new_string : String) return ASU.Unbounded_String
   is
      back_marker  : constant Natural := ASU.Index (Source => US, Pattern => old_string);
      front_marker : constant Natural := back_marker + old_string'Length - 1;
   begin
      if back_marker = 0 then
         return US;
      end if;
      return ASU.Replace_Slice (Source => US,
                                Low    => back_marker,
                                High   => front_marker,
                                By     => new_string);
   end replace_substring;


   ------------
   --  tail  --
   ------------
   function tail (S : String; delimiter : String) return String
   is
      dl_size      : constant Natural := delimiter'Length;
      back_marker  : constant Natural := S'First;
      front_marker : Natural := S'Last - dl_size + 1;
   begin
      loop
         if front_marker < back_marker then
            --  delimiter never found
            return S;
         end if;
         if S (front_marker .. front_marker + dl_size - 1) = delimiter then
            return S (front_marker + dl_size .. S'Last);
         end if;
         front_marker := front_marker - 1;
      end loop;
   end tail;


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


   ---------------
   --  int2str  --
   ---------------
   function int2str (A : Integer) return String
   is
      raw : constant String := A'Img;
   begin
      if A < 0 then
         return raw;
      else
         return raw (raw'First + 1 .. raw'Last);
      end if;
   end int2str;

end Archive.Misc;
