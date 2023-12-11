--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Strings.Fixed;
with ThickUCL.Files;
with Archive.Communication;
with Archive.Unix;
with Lua;

package body Archive.Whitelist.Keywords is

   package TUC renames ThickUCL;
   package SQW renames Archive.Communication;
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
      namebase      : String;
      subpackage    : String;
      variant       : String;
      level         : info_level) return Boolean
   is
      procedure process_action (Position : action_set.Cursor);
      procedure process_arg (Position : arg_crate.Cursor);
      function get_true_path (line : String) return String;

      keyword_obj : A_Keyword;
      result      : Boolean := True;
      act_count   : Natural := 0;
      arg_count   : Natural := 0;

      KEY_PREPACK : constant String := "prepackaging";
      prepack_success : Boolean;
      script_args     : ASU.Unbounded_String;
      script_args_spc : ASU.Unbounded_String;

      function get_true_path (line : String) return String is
      begin
         if line (line'First) = '/' then
            return real_top_path & line;
         end if;
         if prefix_dir (prefix_dir'Last) = '/' then
            return real_top_path & prefix_dir & line;
         else
            return real_top_path & prefix_dir & "/" & line;
         end if;
      end get_true_path;

      procedure process_arg (Position : arg_crate.Cursor)
      is
         ka : keyword_argument renames arg_crate.Element (Position);
      begin
         --  skip the first argument, it's "%0", the entire line.
         if arg_count > 0 then
            if ASU.Length (script_args) > 0 then
               ASU.Append (script_args, Character'Val (0));
               ASU.Append (script_args_spc, ' ');
            end if;
            ASU.Append (script_args, ka.argument);
            ASU.Append (script_args_spc, ka.argument);
         end if;
         arg_count := arg_count + 1;
      end process_arg;

      procedure process_action (Position : action_set.Cursor)
      is
         --  split_args are zero-indexed
         action   : Action_Type renames action_set.Element (Position);
         num_args : constant Natural := Natural (keyword_obj.split_args.Length) - 1;
         num_act  : constant Natural := Natural (keyword_obj.actions.Length);
      begin
         act_count := act_count + 1;
         if act_count > num_args then
            SQW.emit_error ("The " & keyword & " has" & num_act'Img & " actions, but only"
                            & num_args'Img & " arguments.");
            result := False;
            return;
         end if;

         declare
            act_path : constant String :=
              ASU.To_String (keyword_obj.split_args.Element (act_count).argument);
            true_path : constant String := get_true_path (act_path);
         begin
            case action is
               when file_action =>
                  if not Unix.file_exists (true_path) then
                     if level >= normal then
                        SQW.emit_error
                          ("Manifest file [" & act_path & "] does not exist, ignoring");
                     end if;
                     result := False;
                     return;
                  end if;
                  if not whitelist.ingest_manifest_with_mode_override
                    (full_path     => true_path,
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

      --  handle prepackaging now
      keyword_obj.split_args.Iterate (process_arg'Access);
      if keyword_obj.tree.string_field_exists (KEY_PREPACK) then
         Lua.run_lua_script
           (namebase   => namebase,
            subpackage => subpackage,
            variant    => variant,
            prefix     => prefix_dir,
            root_dir   => real_top_path,
            upgrading  => False,
            script     => keyword_obj.tree.get_base_value (KEY_PREPACK),
            arg_chain  => ASU.To_String (script_args),
            success    => prepack_success);
         if not prepack_success then
            result := False;
            SQW.emit_error ("Fail to apply keyword '" & keyword & "'");
         end if;
      end if;

      if result then
         keyword_obj.actions.Iterate (process_action'Access);
      end if;

      if keyword_obj.deprecated then
         if level >= normal then
            declare
               err_part1 : constant String := "The use of '@" & keyword & "' is deprecated";
               deprecmsg : constant String := ASU.To_String (keyword_obj.deprecated_message);
            begin
               if deprecmsg /= "" then
                  SQW.emit_error (err_part1 & ": " & deprecmsg);
               else
                  SQW.emit_error (err_part1);
               end if;
            end;
         end if;
      end if;

      --  Lua scripts remember the arguments while bourne scripts do not.
      --  Bourne scripts replace %0, %1, .. tokens with arguments while Lua scripts do not
      --  Change storage from array of strings to array of objects with keys "code" and "args"
      --  For shell scripts, args will always be empty.
      for phase in package_phase'Range loop
         if keyword_obj.phase_script_defined (phase) then
            declare
               script : phase_script;
               code   : constant String := keyword_obj.retrieve_script (phase);
            begin
               case phase is
                  when pre_install    |
                       pre_deinstall  |
                       post_install   |
                       post_deinstall =>

                     if keyword_obj.valid_template (keyword, code) then
                        script.code := keyword_obj.populate_template (code);
                        script.args := ASU.Null_Unbounded_String;
                     else
                        return False;
                     end if;
                  when
                       pre_install_lua    |
                       pre_deinstall_lua  |
                       post_install_lua   |
                       post_deinstall_lua =>

                     script.code := ASU.To_Unbounded_String (code);
                     script.args := script_args_spc;
               end case;
               whitelist.scripts (phase).Append (script);
            end;
         end if;
      end loop;

      for mtype in Message_Type'Range loop
         if ASU.Length (keyword_obj.messages (mtype)) > 0 then
            whitelist.messages (mtype).Append (keyword_obj.messages (mtype));
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
                  SQW.emit_error ("Requesting argument " & tokarg &
                                  " while only" & num_args'Img & " arguments are available");
                  SQW.emit_error ("Failed to apply keyword '" & keyword & "'");
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
      full_path  : constant String := Archive.Unix.real_path (filename);
      msgset_key : constant String := "messages";
      action_key : constant String := "actions";
      deprec_key : constant String := "deprecated";
      dmsg_key   : constant String := "deprecation_message";
      prefmt_key : constant String := "preformat_arguments";

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
         SQW.emit_error (filename & ": UCL keyword not found, fatal.");
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
                     SQW.emit_error
                       (tail (filename, "/") & ": action '" & action & "' not recognized");
                  end if;
                  if level >= debug then
                     SQW.emit_debug ("Action: " & action & "   " & full_path);
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
         --  messages are an array of objects
         case keyword.tree.get_data_type (msgset_key) is
            when TUC.data_array =>
               declare
                  vndx : constant TUC.array_index :=
                    keyword.tree.get_index_of_base_array (msgset_key);
                  num_msgs : constant Natural := keyword.tree.get_number_of_array_elements (vndx);
               begin
                  for msg_index in 0 .. num_msgs - 1 loop
                     case keyword.tree.get_array_element_type (vndx, msg_index) is
                        when TUC.data_object =>
                           declare
                              ondx : constant TUC.object_index :=
                                keyword.tree.get_array_element_object (vndx, msg_index);
                              msg_key  : constant String := "message";
                              type_key : constant String := "type";
                           begin
                              case keyword.tree.get_object_data_type (ondx, msg_key) is
                                 when TUC.data_string =>
                                    declare
                                       msg : constant ASU.Unbounded_String :=
                                         ASU.To_Unbounded_String
                                           (keyword.tree.get_object_value (ondx, msg_key));
                                    begin
                                       case keyword.tree.get_object_data_type (ondx, type_key) is
                                          when TUC.data_string =>
                                             declare
                                                tstr : constant String :=
                                                  keyword.tree.get_object_value (ondx, type_key);
                                             begin
                                                if tstr = "upgrade" then
                                                   keyword.messages (upgrade) := msg;
                                                elsif tstr = "install" then
                                                   keyword.messages (install) := msg;
                                                elsif tstr = "remove" then
                                                   keyword.messages (deinstall) := msg;
                                                else
                                                   keyword.messages (always) := msg;
                                                end if;
                                             end;
                                          when TUC.data_not_present =>
                                             keyword.messages (always) := msg;
                                          when others => null;
                                       end case;
                                    end;
                                 when others => null;
                              end case;
                           end;
                        when others => null;
                     end case;
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
            SQW.emit_debug ("First argument (%@): " & S);
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
                        SQW.emit_debug ("Push into arguments: " & arg
                                        & " (%" & tcstr (tcstr'First + 1 .. tcstr'Last) & ")");
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
         SQW.emit_debug ("perform expansion of " & S);
         SQW.emit_debug ("Expand any " & token & " with " & replacement);
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
