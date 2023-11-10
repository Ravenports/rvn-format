--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Text_IO;

package body Archive.Whitelist.Keywords is

   package TIO renames Ada.Text_IO;

   --------------------------------
   --  process_external_keyword  --
   --------------------------------
   function process_external_keyword
     (whitelist     : in out A_Whitelist;
      keyword       : String;
      arguments     : String;
      keyword_dir   : String;
      full_path     : String;
      real_top_path : String;
      level         : info_level) return Boolean
   is
      do_action   : Action_Type := no_action;
      keyword_obj : A_Keyword;
      relative    : constant String := first_word (arguments);
      result      : Boolean := True;
   begin
      case do_action is
         when no_action =>
            null;
         when file_action =>
            if not whitelist.ingest_manifest_with_mode_override
              (full_path     => full_path,
               real_top_path => real_top_path,
               new_owner     => keyword_obj.get_owner,
               new_group     => keyword_obj.get_group,
               new_perms     => keyword_obj.get_permissions,
               level         => level)
            then
               result := False;
            end if;
         when directory_action =>
            whitelist.insert_temporary_directory
              (dir_path   => relative,
               full_path  => full_path,
               attr_owner => keyword_obj.get_owner,
               attr_group => keyword_obj.get_group,
               attr_perms => keyword_obj.get_permissions,
               level      => level);
      end case;
      if keyword_obj.deprecated then
         if level >= normal then
            TIO.Put_Line ("The " & keyword & " keyword is deprecated and it should be retired.");
            TIO.Put_Line ("Deprecation message: " & ASU.To_String (keyword_obj.deprecated_message));
         end if;
      end if;
      for phase in package_phase'Range loop
         if keyword_obj.phase_script_defined (phase) then
            declare
               bourne : phase_script;
            begin
               bourne.script := ASU.To_Unbounded_String (keyword_obj.retrieve_script (phase));
               whitelist.scripts (phase).Append (bourne);
            end;
         end if;
      end loop;

      return Result;
   end process_external_keyword;

end Archive.Whitelist.Keywords;
