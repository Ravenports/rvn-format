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
      real_top_path : String;
      prefix_dir    : String;
      last_file     : String;
      level         : info_level) return Boolean
   is
      procedure process_action (Position : action_set.Cursor);
      function get_true_path (line : String) return String;

      keyword_obj : A_Keyword;
      result      : Boolean := True;
      act_count   : Natural := 0;

      function get_true_path (line : String) return String is
      begin
         if line (line'First) = '/' then
            return real_top_path & line;
         end if;
         return real_top_path & prefix_dir & "/" & line;
      end get_true_path;

      procedure process_action (Position : action_set.Cursor)
      is
         --  split_args are zero-indexed
         action   : Action_Type renames action_set.Element (Position);
         num_args : constant Natural := Natural (keyword_obj.split_args.Length) - 1;
         num_act  : constant Natural := Natural (keyword_obj.actions.Length);
      begin
         act_count := act_count + 1;
         if act_count > num_args then
            TIO.Put_Line (TIO.Standard_Error, "The " & keyword & " has" & num_act'Img
                          & " actions, but only" & num_args'Img & " arguments.");
            result := False;
            return;
         end if;

         declare
            act_path : constant String :=
              ASU.To_String (keyword_obj.split_args.Element (act_count).argument);
            true_path : constant String := get_true_path (act_path);
            full_path : constant String := Unix.real_path (true_path);
         begin
            case action is
               when file_action =>
                  if full_path = "" then
                     if level >= normal then
                        TIO.Put_Line ("Manifest file [" & act_path & "] does not exist, ignoring");
                     end if;
                     result := False;
                     return;
                  end if;
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
                    (dir_path   => act_path,
                     true_path  => true_path,
                     attr_owner => keyword_obj.get_owner,
                     attr_group => keyword_obj.get_group,
                     attr_perms => keyword_obj.get_permissions,
                     level      => level);
            end case;
         end;
      end process_action;
   begin
      keyword_obj.scan_file (keyword_dir & "/" & keyword & ".ucl", level);
      if not keyword_obj.file_found or else keyword_obj.scan_failed then
         --  FreeBSD pkg testsuite considers these fatal issues, so mimic
         return False;
      end if;
      keyword_obj.process_arguments
        (arguments => arguments,
         prefix    => prefix_dir,
         last_file => last_file,
         stagedir  => real_top_path);

      keyword_obj.actions.Iterate (process_action'Access);

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
               script : constant String := keyword_obj.retrieve_script (phase);
            begin
               if keyword_obj.valid_template (keyword, script) then
                  bourne.script := keyword_obj.populate_template (script);
                  whitelist.scripts (phase).Append (bourne);
               else
                  return False;
               end if;
            end;
         end if;
      end loop;
      for mtype in Message_Type'Range loop
         if ASU.Length (keyword_obj.messages (mtype)) > 0 then
            whitelist.messages (mtype).Append (keyword_obj.messages (always));
         end if;
      end loop;

      return result;
   end process_external_keyword;

   ----------------------
   --  valid_template  --
   ----------------------
   function valid_template
     (keyword_obj : A_Keyword;
      keyword     : String;
      script      : String) return Boolean
   is
      num_args : constant Natural := Natural (keyword_obj.split_args.Length) - 1;
   begin
      for token in 0 .. 9 loop
         declare
            timage : constant String := token'Img;
            tokarg : constant String := "%" & timage (timage'First + 1 .. timage'Last);
         begin
            if AS.Fixed.Index (Source => script, Pattern => tokarg) > 0 then
               if token > num_args then
                  TIO.Put_Line (TIO.Standard_Error, "Requesting argument " & tokarg &
                                  " while only" & num_args'Img & " arguments are available");
                  TIO.Put_Line (TIO.Standard_Error, "Failed to apply keyword '" & keyword & "'");
                  return False;
               end if;
            end if;
         end;
      end loop;
      return True;
   end valid_template;


   -------------------------
   --  populate_template  --
   -------------------------
   function populate_template
     (keyword_obj : A_Keyword;
      script      : String) return ASU.Unbounded_String
   is
      result : ASU.Unbounded_String := ASU.To_Unbounded_String (script);
      num_args : constant Natural := Natural (keyword_obj.split_args.Length) - 1;
   begin
      for token in 0 .. num_args loop
         declare
            timage : constant String := token'Img;
            tokarg : constant String := "%" & timage (timage'First + 1 .. timage'Last);
            newstr : constant String := ASU.To_String (keyword_obj.split_args (token).argument);
         begin
            result := replace_substring (result, tokarg, newstr);
         end;
      end loop;
      return result;
   end populate_template;


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


   ----------------------------
   --  phase_script_defined  --
   ----------------------------
   function phase_script_defined (keyword : A_Keyword; phase : package_phase) return Boolean
   is
      phase_key : constant String := convert_phase (phase);
   begin
      return keyword.tree.string_field_exists (phase_key);
   end phase_script_defined;


   -----------------------
   --  retrieve_script  --
   -----------------------
   function retrieve_script (keyword : A_Keyword; phase : package_phase) return String
   is
      phase_key : constant String := convert_phase (phase);
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
      function message_exists (vndx : ThickUcl.object_index; key : String) return Boolean;
      function get_message (vndx : ThickUcl.object_index; key : String) return ASU.Unbounded_String;

      full_path  : constant String := Archive.Unix.real_path (filename);
      msgset_key : constant String := "messages";
      action_key : constant String := "actions";
      deprec_key : constant String := "deprecated";
      dmsg_key   : constant String := "deprecation_message";
      prefmt_key : constant String := "preformat_arguments";

      function message_exists (vndx : ThickUcl.object_index; key : String) return Boolean is
      begin
         case keyword.tree.get_object_data_type (vndx, key) is
            when ThickUCL.data_string => return True;
            when others => return False;
         end case;
      end message_exists;

      function get_message (vndx : ThickUcl.object_index; key : String) return ASU.Unbounded_String
      is
      begin
         return ASU.To_Unbounded_String (keyword.tree.get_object_value (vndx, key));
      end get_message;
   begin
      keyword.scan_failed := False;
      keyword.file_found := False;
      keyword.preformat := False;
      keyword.deprecated := False;
      keyword.deprecated_message   := ASU.Null_Unbounded_String;
      keyword.messages (always)    := ASU.Null_Unbounded_String;
      keyword.messages (install)   := ASU.Null_Unbounded_String;
      keyword.messages (deinstall) := ASU.Null_Unbounded_String;
      keyword.messages (upgrade)   := ASU.Null_Unbounded_String;
      keyword.level := level;

      if full_path = "" then
         --  level doesn't matter, show stderr even if silent
         TIO.Put_Line (TIO.Standard_Error, filename & ": UCL keyword not found, fatal.");
         return;
      end if;

      keyword.file_found := True;
      TUC.Files.parse_ucl_file (keyword.tree, full_path, "");
      if keyword.tree.array_field_exists (action_key) then
         --  valid (only) is "[]", "[file]", "[dir]"
         declare
            ai : constant TUC.array_index := keyword.tree.get_index_of_base_array (action_key);
            num_elements : constant Natural := keyword.tree.get_number_of_array_elements (ai);
         begin
            for index in 0 .. num_elements - 1 loop
               declare
                  action : constant String := keyword.tree.get_array_element_value (ai, index);
               begin
                  if action = "file" then
                     keyword.actions.Append  (file_action);
                  elsif action = "dir" then
                     keyword.actions.Append (directory_action);
                  else
                     keyword.scan_failed := True;
                     TIO.Put_Line (TIO.Standard_Error,  tail (filename, "/") & ": action '"
                                   & action & "' not recognized");
                  end if;
                  if level >= debug then
                     TIO.Put_Line ("Action: " & action & "   " & full_path);
                  end if;
               end;
            end loop;
         end;
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
      if keyword.tree.key_exists (msgset_key) then
         case keyword.tree.get_data_type (msgset_key) is
            when ThickUCL.data_object =>
               declare
                  vndx : constant ThickUCL.object_index :=
                    keyword.tree.get_index_of_base_ucl_object (msgset_key);
               begin
                  for msgtype in Message_Type'Range loop
                     declare
                        key : constant String := get_message_key (msgtype);
                     begin
                        if message_exists (vndx, key) then
                           keyword.messages (msgtype) := get_message (vndx, key);
                        end if;
                     end;
                  end loop;
               end;
            when others => null;
         end case;
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
      last_file : String;
      stagedir  : String)
   is
      procedure split_formatted_string (S : String);
      procedure split_formatted_string (S : String)
      is
         num_spaces : constant Natural := count_char (S, ' ');
         tray_zero  : keyword_argument;
         true_count : Natural := 0;
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
                  true_count := true_count + 1;
                  tray.argument := ASU.To_Unbounded_String (arg);
                  keyword.split_args.Append (tray);
                  if keyword.level >= debug then
                     declare
                        tcstr : constant String := true_count'Img;
                     begin
                        TIO.Put_Line ("Push into arguments: " & arg & " (%"
                                      & tcstr (tcstr'First + 1 .. tcstr'Last) & ")");
                     end;
                  end if;
               end if;
            end;
         end loop;
      end split_formatted_string;
   begin
      if keyword.preformat then
         split_formatted_string (perform_expansion (arguments, prefix, last_file, keyword.level));
      else
         split_formatted_string (arguments);
      end if;
   end process_arguments;


   -------------------------
   --  token_expansion  --
   -------------------------
   function token_expansion (S, token, replacement : String; level : info_level) return String
   is
      US : ASU.Unbounded_String := ASU.To_Unbounded_String (S);
   begin
      if level >= debug then
         TIO.Put_Line ("perform expansion of " & S);
         TIO.Put_Line ("Expand any " & token & " with " & replacement);
      end if;
      loop
         exit when ASU.Index (Source => US, Pattern => token) = 0;
         US := replace_substring (US, token, replacement);
      end loop;
      return ASU.To_String (US);
   end token_expansion;


   -------------------------
   --  perform_expansion  --
   -------------------------
   function perform_expansion
     (original  : String;
      prefix    : String;
      last_file : String;
      level     : info_level) return String
   is
      --  %D expands to prefix
      --  %F expands to $last_file
      --  %f expands to basename ($last_file)
      --  %B expands to dirname ($last_file)

      postD : constant String := token_expansion (original, "%D", prefix, level);
   begin
      declare
         postF : constant String := token_expansion (postD, "%F", last_file, level);
      begin
         declare
            post_f : constant String := token_expansion (postF, "%f", tail (last_file, "/"), level);
            fchar  : constant Character := last_file (last_file'First);
         begin
            if fchar = '/' then
               return token_expansion (post_f, "%B", head (last_file, "/"), level);
            else
               return token_expansion (post_f, "%B", head (prefix & "/" & last_file, "/"), level);
            end if;
         end;
      end;
   end perform_expansion;


end Archive.Whitelist.Keywords;
