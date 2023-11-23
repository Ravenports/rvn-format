--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Text_IO;
with Ada.Strings.Fixed;
with ThickUCL.Files;
with Archive.Unix;

package body Archive.Whitelist.Keywords is

   package TUC renames ThickUCL;
   package TIO renames Ada.Text_IO;
   package AS  renames Ada.Strings;

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
      prefix_dir    : String;
      level         : info_level) return Boolean
   is
      keyword_obj : A_Keyword;
      relative    : constant String := first_word (arguments);
      result      : Boolean := True;
   begin
      keyword_obj.scan_file (keyword_dir & "/" & keyword & ".ucl", level);
      if not keyword_obj.file_found then
         --  Don't cause a missing keyword to break an archive build
         return True;
      end if;
      keyword_obj.process_arguments
        (arguments => arguments,
         prefix    => prefix_dir,
         full_path => full_path,
         stagedir  => real_top_path);

      case keyword_obj.action is
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


   -----------------
   --  get_owner  --
   -----------------
   function get_owner (keyword : A_Keyword) return String
   is
      attributes_key : constant String := "attributes";
   begin
      if keyword.tree.ucl_object_field_exists (attributes_key) then
         declare
            owner_key  : constant String := "owner";
            owner_type : TUC.Leaf_type;
            attr_index : Natural;
         begin
            attr_index := keyword.tree.get_index_of_base_ucl_object (attributes_key);
            owner_type := keyword.tree.get_object_data_type (attr_index, owner_key);
            case owner_type is
               when TUC.data_string =>
                  return keyword.tree.get_object_value (attr_index, owner_key);
               when others =>
                  null;
            end case;
         end;
      end if;
      return "";
   end get_owner;


   -----------------
   --  get_group  --
   -----------------
   function get_group (keyword : A_Keyword) return String
   is
      attributes_key : constant String := "attributes";
   begin
      if keyword.tree.ucl_object_field_exists (attributes_key) then
         declare
            group_key  : constant String := "group";
            group_type : TUC.Leaf_type;
            attr_index : Natural;
         begin
            attr_index := keyword.tree.get_index_of_base_ucl_object (attributes_key);
            group_type := keyword.tree.get_object_data_type (attr_index, group_key);
            case group_type is
               when TUC.data_string =>
                  return keyword.tree.get_object_value (attr_index, group_key);
               when others =>
                  null;
            end case;
         end;
      end if;
      return "";
   end get_group;


   -----------------------
   --  get_permissions  --
   -----------------------
   function get_permissions (keyword : A_Keyword) return String
   is
      attributes_key : constant String := "attributes";
   begin
      if keyword.tree.ucl_object_field_exists (attributes_key) then
         declare
            perms_key  : constant String := "mode";
            perms_type : TUC.Leaf_type;
            attr_index : Natural;
         begin
            attr_index := keyword.tree.get_index_of_base_ucl_object (attributes_key);
            perms_type := keyword.tree.get_object_data_type (attr_index, perms_key);
            case perms_type is
               when TUC.data_string =>
                  return keyword.tree.get_object_value (attr_index, perms_key);
               when others =>
                  null;
            end case;
         end;
      end if;
      return "";
   end get_permissions;


   ---------------
   --  convert  --
   ---------------
   function convert (phase : package_phase) return String is
   begin
      case phase is
         when pre_install          => return "pre-install";
         when pre_install_lua      => return "pre-install-lua";
         when pre_deinstall        => return "pre-deinstall";
         when pre_deinstall_lua    => return "pre-deinstall-lua";
         when post_install         => return "post-install";
         when post_install_lua     => return "post-install-lua";
         when post_deinstall       => return "post-deinstall";
         when post_deinstall_lua   => return "post-deinstall-lua";
      end case;
   end convert;


   ----------------------------
   --  phase_script_defined  --
   ----------------------------
   function phase_script_defined (keyword : A_Keyword; phase : package_phase) return Boolean
   is
      phase_key : constant String := convert (phase);
   begin
      return keyword.tree.string_field_exists (phase_key);
   end phase_script_defined;


   -----------------------
   --  retrieve_script  --
   -----------------------
   function retrieve_script (keyword : A_Keyword; phase : package_phase) return String
   is
      phase_key : constant String := convert (phase);
   begin
      if keyword.tree.string_field_exists (phase_key) then
         return keyword.tree.get_base_value (phase_key);
      end if;
      return "";
   end retrieve_script;


   -----------------
   --  scan_file  --
   -----------------
   procedure scan_file (keyword : in out A_Keyword; filename : String; level : info_level)
   is
      full_path  : constant String := Archive.Unix.real_path (filename);
      action_key : constant String := "actions";
      deprec_key : constant String := "deprecated";
      dmsg_key   : constant String := "deprecation_message";
      prefmt_key : constant String := "preformat_arguments";
   begin
      keyword.file_found := False;
      keyword.action := no_action;
      keyword.preformat := False;
      keyword.deprecated := False;
      keyword.deprecated_message := ASU.Null_Unbounded_String;
      keyword.level := level;

      if full_path = "" then
         if level >= normal then
            TIO.Put_Line (filename & ": UCL keyword not found, ignored.");
         end if;
         return;
      end if;

      keyword.file_found := True;
      TUC.Files.parse_ucl_file (keyword.tree, full_path);
      if keyword.tree.array_field_exists (action_key) then
         --  valid (only) is "[]", "[file]", "[dir]"
         --  Historically there could have been multiple fields, but currently all are
         --  mutually exclusive.  Therefore, only check the first field and ignore any others
         declare
            ai : constant TUC.array_index := keyword.tree.get_index_of_base_array (action_key);
         begin
            if keyword.tree.get_number_of_array_elements (ai) > 0 then
               if keyword.tree.get_array_element_value (ai, 0) = "file" then
                  keyword.action := file_action;
               elsif keyword.tree.get_array_element_value (ai, 0) = "dir" then
                  keyword.action := directory_action;
               end if;
            end if;
         end;
      end if;
      if level >= debug then
         TIO.Put_Line ("Action: " & keyword.action'Img & "   " & full_path);
      end if;
      if keyword.tree.boolean_field_exists (deprec_key) then
         keyword.deprecated := keyword.tree.get_base_value (deprec_key);
      end if;
      if keyword.tree.string_field_exists (dmsg_key) then
         keyword.deprecated_message :=
           ASU.To_Unbounded_String (keyword.tree.get_base_value (dmsg_key));
      end if;
      if keyword.tree.boolean_field_exists (prefmt_key) then
         keyword.preformat := keyword.tree.get_base_value (prefmt_key);
      end if;
   end scan_file;


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


   -------------------------
   --  process_arguments  --
   -------------------------
   procedure process_arguments
     (keyword : in out A_Keyword;
      arguments : String;
      prefix    : String;
      full_path : String;
      stagedir  : String)
   is
      procedure split_formatted_string (S : String);
      function perform_expansion (S, token, replacement : String) return String;

      procedure split_formatted_string (S : String)
      is
         num_spaces : constant Natural := count_char (S, ' ');
         tray_zero  : keyword_argument;
      begin
         tray_zero.argument := ASU.To_Unbounded_String (S);
         if keyword.level >= debug then
            TIO.Put_Line ("First argument (%@): " & S);
         end if;
         keyword.split_args.Append (tray_zero);
         for x in 1 .. num_spaces + 1 loop
            declare
               arg : constant String := specific_field (S, x);
               tray : keyword_argument;
            begin
               if arg /= "" then
                  tray.argument := ASU.To_Unbounded_String (arg);
                  keyword.split_args.Append (tray);
                  if keyword.level >= debug then
                     TIO.Put_Line ("Push into arguments: " & arg);
                  end if;
               end if;
            end;
         end loop;
      end split_formatted_string;

      function perform_expansion (S, token, replacement : String) return String
      is
         US : ASU.Unbounded_String := ASU.To_Unbounded_String (S);
      begin
         if keyword.level >= debug then
            TIO.Put_Line ("perform expansion of " & S);
            TIO.Put_Line ("Expand any " & token & " with " & replacement);
         end if;
         loop
            exit when ASU.Index (Source => US, Pattern => token) = 0;
            US := replace_substring (US, token, replacement);
         end loop;
         return ASU.To_String (US);
      end perform_expansion;
   begin
      if keyword.preformat then
         declare
            postD : constant String := perform_expansion (arguments, "%D", prefix);
            sdlen : constant Natural := stagedir'Length;
         begin
            declare
               postf : constant String := perform_expansion (postD, "%f", tail (full_path, "/"));
               path  : constant String := full_path (full_path'First + sdlen .. full_path'Last);
            begin
               declare
                  postB : constant String := perform_expansion (postf, "%B", head (path, "/"));
               begin
                  split_formatted_string (postB);
                  return;
               end;
            end;
         end;
      end if;
      split_formatted_string (arguments);
   end process_arguments;


end Archive.Whitelist.Keywords;
