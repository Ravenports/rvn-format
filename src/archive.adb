--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Ada.Strings.Fixed;

package body Archive is

   package ASF renames Ada.Strings.Fixed;

   ------------------------------------------------------------------------------------------
   --  verbose_display_owngrp
   ------------------------------------------------------------------------------------------
   function verbose_display_owngrp (owngrp : ownergroup) return String
   is
      name   : constant String := trim_trailing_zeros (owngrp);
      result : String (1 .. 9) := (others => ' ');
      rindex : Natural;
   begin
      if name'Length > 8 then
         result := " " & name (name'First .. name'First + 6) & "*";
      else
         rindex := 10 - name'Length;
         result (rindex .. 9) := name;
      end if;
      return result;
   end verbose_display_owngrp;


   ------------------------------------------------------------------------------------------
   --  verbose_display_filesize
   ------------------------------------------------------------------------------------------
   function verbose_display_filesize (fsize : size_type) return String
   is
      function trim_first (S : String) return String;
      function trim_first (S : String) return String is
      begin
         return S (S'First + 1 .. S'Last);
      end trim_first;

      result : String (1 .. 9) := (others => ' ');
   begin
      if fsize < 100_000_000 then
         declare
            myimage : constant String := trim_first (fsize'Img);
         begin
            result (10 - myimage'Length .. result'Last) := myimage;
         end;
      elsif fsize < 10_000_000_000 then
         declare
            basenum : constant Natural := Natural (fsize / 1_000_000);
            myimage : constant String := trim_first (basenum'Img & "M+");
         begin
            result (10 - myimage'Length .. result'Last) := myimage;
         end;
      else
         declare
            basenum : constant Natural := Natural (fsize / 1_000_000_000);
            myimage : constant String := trim_first (basenum'Img & "G+");
         begin
            result (10 - myimage'Length .. result'Last) := myimage;
         end;
      end if;
      return result;
   end verbose_display_filesize;


   ------------------------------------------------------------------------------------------
   --  trim_trailing_zeros
   ------------------------------------------------------------------------------------------
   function trim_trailing_zeros (full_string : String) return String
   is
      first_zero : Natural;
      pattern    : constant String (1 .. 1) := (others => Character'Val (0));
   begin
      first_zero := ASF.Index (Source  => full_string, Pattern => pattern);
      if first_zero = full_string'First then
         return "";
      elsif first_zero > full_string'First then
         return full_string (full_string'First .. first_zero - 1);
      end if;
      return full_string;
   end trim_trailing_zeros;


end Archive;