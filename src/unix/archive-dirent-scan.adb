--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

package body Archive.Dirent.Scan is

   ----------------------
   --  scan_directory  --
   ----------------------
   procedure scan_directory
     (directory : String;
      crate     : in out dscan_crate.Vector)
   is
      c_directory : constant IC.char_array := IC.To_C (directory);
      tracker : Natural := 0;
      rc : IC.int;
   begin
      crate.Clear;
      loop
         rc := walkdir_open_folder (c_directory);
         case rc is
            when 1 =>
               tracker := tracker + 1;
               delay 0.05;
            when 2 =>
               raise dscan_folder_already_open;
            when others => exit;
         end case;
         if tracker = 100 then
            raise dscan_open_failure;
         end if;
      end loop;

      loop
         declare
            use type IC.Strings.chars_ptr;
            centry : IC.Strings.chars_ptr;
         begin
            centry := walkdir_next_entry;
            exit when centry = IC.Strings.Null_Ptr;

            declare
               filename : constant String := IC.Strings.Value (centry);
               entity   : constant Directory_Entity := create_entity (directory, filename);
            begin
               if filename /= "." and then filename /= ".."
               then
                  crate.Append (entity);
               end if;
            end;
         end;
      end loop;

      tracker := 0;
      loop
         rc := walkdir_close_folder;
         case rc is
            when 1 =>
               tracker := tracker + 1;
               delay 0.05;
            when 2 =>
               raise dscan_folder_not_open;
            when others => exit;
         end case;
         if tracker = 100 then
            raise dscan_close_failure;
         end if;
      end loop;

      dscan_sorting.Sort (crate);

   end scan_directory;

end Archive.Dirent.Scan;
