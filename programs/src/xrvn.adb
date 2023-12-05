with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Command_Line;
with Ada.Directories;
with Archive.Unpack;
with Archive.Unix;
with ThickUCL.Files;
with Bourne;

procedure Xrvn
is
   package TIO renames Ada.Text_IO;
   package ASU renames Ada.Strings.Unbounded;
   package CLI renames Ada.Command_Line;
   package DIR renames Ada.Directories;

   subtype text is ASU.Unbounded_String;
   procedure process_arguments;
   procedure usage (error_msg : String);
   procedure error (error_msg : String);
   function extraction_directory return String;
   function tail (S : String; delimiter : String) return String;

   opt_verbose  : Boolean := False;
   opt_quiet    : Boolean := False;
   opt_digest   : Boolean := False;
   opt_extract  : Boolean := False;
   opt_metadata : Boolean := False;
   opt_manifest : Boolean := False;
   opt_attr     : Boolean := False;
   opt_outdir   : Boolean := False;
   opt_touch    : Boolean := False;
   opt_counts   : Boolean := False;
   filename_set : Boolean := False;
   arg_outdir   : text;
   arg_filename : text;

   procedure process_arguments
   is
      type compound is (waiting, outdir);
      argx : Natural := 0;
      next_parameter : compound := waiting;

   begin
      loop
         argx := argx + 1;
         exit when argx > CLI.Argument_Count;
         declare
            this_arg : String renames CLI.Argument (argx);
            continue : Boolean := False;
         begin
            case next_parameter is
               when waiting  => continue := True;
               when outdir   => arg_outdir   := ASU.To_Unbounded_String (this_arg);
            end case;
            next_parameter := waiting;
            if continue then
               if this_arg'Length > 1 then
                  if this_arg (this_arg'First .. this_arg'First + 1) = "--" then
                     if this_arg = "--extract" then
                        opt_extract := True;
                     elsif this_arg = "--metadata" then
                        opt_metadata := True;
                     elsif this_arg = "--list-manifest" then
                        opt_manifest := True;
                     elsif this_arg = "--attributes" then
                        opt_attr := True;
                     elsif this_arg = "--out-dir" then
                        opt_outdir := True;
                        next_parameter := outdir;
                     elsif this_arg = "--verbose" then
                        opt_verbose := True;
                     elsif this_arg = "--quiet" then
                        opt_quiet := True;
                     elsif this_arg = "--digest" then
                        opt_digest := True;
                     elsif this_arg = "--touch" then
                        opt_touch := True;
                     elsif this_arg = "--counts" then
                        opt_counts := True;
                     end if;
                  elsif this_arg (this_arg'First) = '-' then
                     declare
                        single : Natural := this_arg'First;
                     begin
                        loop
                           single := single + 1;
                           exit when single > this_arg'Last;
                           case this_arg (single) is
                              when 'v' => opt_verbose := True;
                              when 'd' => opt_digest := True;
                              when 'q' => opt_quiet := True;
                              when 't' => opt_touch := True;
                              when 'x' => opt_extract := True;
                              when 'm' => opt_metadata := True;
                              when 'l' => opt_manifest := True;
                              when 'a' => opt_attr:= True;
                              when 'c' => opt_counts := True;
                              when 'o' =>
                                 case this_arg (single) is
                                    when 'o' => opt_outdir := True;
                                    when others => null;
                                 end case;
                                 if single = this_arg'Last then
                                    case this_arg (single) is
                                       when 'o' => next_parameter := outdir;
                                       when others => null;
                                    end case;
                                 else
                                    declare
                                       remainder : constant text := ASU.To_Unbounded_String
                                         (this_arg (single + 1 .. this_arg'Last));
                                    begin
                                       case this_arg (single) is
                                          when 'o' => arg_outdir   := remainder;
                                          when others => null;
                                       end case;
                                    end;
                                    single := this_arg'Last;
                                 end if;
                              when others => null;
                           end case;
                        end loop;
                     end;
                  else
                     if not filename_set then
                        arg_filename := ASU.To_Unbounded_String (this_arg);
                        filename_set := True;
                     end if;
                     --  Subsequent times here will drop the argument
                  end if;
               else
                  --  single character argument
                  if not filename_set then
                     arg_filename := ASU.To_Unbounded_String (this_arg);
                     filename_set := True;
                  end if;
               end if;
            end if;
         end;
      end loop;
   end process_arguments;

   procedure usage (error_msg : String) is
   begin
      TIO.Put_Line (error_msg);
      TIO.Put_Line ("xrvn [-vq] [-xmlc] [-d|-a] [-t] [-o outdir] filename");
   end usage;

   procedure error (error_msg : String) is
   begin
      if not opt_quiet then
         TIO.Put_Line (error_msg);
      end if;
   end error;

   function extraction_directory return String is
   begin
      if opt_outdir then
         return ASU.To_String (arg_outdir);
      else
         return ".";
      end if;
   end extraction_directory;

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

   function get_meta_string (metatree : ThickUCL.UclTree; data_key : String) return String is
   begin
      case metatree.get_data_type (data_key) is
         when ThickUCL.data_string =>
            return metatree.get_base_value (data_key);
         when others =>
            return data_key & "-not-found";
      end case;
   end get_meta_string;

   procedure execute_scripts
     (metatree  : ThickUCL.UclTree;
      vndx      : ThickUCL.object_index;
      phase_key : String)
   is
      andx        : ThickUCL.array_index;
      num_scripts : Natural;
   begin
      case metatree.get_object_data_type (vndx, phase_key) is
         when ThickUCL.data_array =>
            andx := metatree.get_object_array (vndx, phase_key);
            num_scripts := metatree.get_number_of_array_elements (andx);
            for index in 0 .. num_scripts - 1 loop
               case metatree.get_array_element_type (andx, index) is
                  when ThickUCL.data_string =>
                     declare
                        script : constant String := metatree.get_array_element_value (andx, index);
                     begin
                        Bourne.run_shell_script
                          (namebase    => get_meta_string (metatree, "namebase"),
                           subpackage  => get_meta_string (metatree, "subpackage"),
                           variant     => get_meta_string (metatree, "variant"),
                           prefix      => get_meta_string (metatree, "prefix"),
                           root_dir    => ASU.To_String (arg_outdir),
                           upgrading   => False,
                           interpreter => "/bin/sh",
                           script      => script);
                     end;
                  when others => null;
               end case;
            end loop;
         when others => null;
      end case;
   end execute_scripts;

begin
   process_arguments;
   declare
      xmlc : Natural := 0;
   begin
      if opt_extract then
         xmlc := xmlc + 1;
      end if;
      if opt_metadata then
         xmlc := xmlc + 1;
      end if;
      if opt_manifest then
         xmlc := xmlc + 1;
      end if;
      if opt_counts then
         xmlc := xmlc + 1;
      end if;
      if xmlc = 0 then
         usage ("At least one argument of -x, -m, -l, -c is required.");
         return;
      end if;
      if xmlc > 1 then
         usage ("Only one argument of -x, -m, -l, -c can be selected.");
         return;
      end if;
   end;
   if opt_quiet and then opt_verbose then
      usage ("The --quiet and --verbose options are mutually exclusive");
      return;
   end if;
   if opt_digest and then opt_attr then
      usage ("The --digest and --attributes options are mutually exclusive");
      return;
   end if;
   if opt_touch and then not opt_extract then
      error ("Notice: The --touch option only has effect while extracting.");
   end if;
   if not filename_set then
      usage ("The filename argument (path to rvn file) is required.");
      return;
   end if;
   if opt_outdir then
      --  verify output directory exists and is a directory.
      declare
         checkdir : constant String := ASU.To_String (arg_outdir);
      begin
         if DIR.Exists (checkdir) then
            case DIR.Kind (checkdir) is
               when DIR.Directory => null;
               when others =>
                  error ("The output directory (" & checkdir & ") exists but is not a directory.");
                  return;
            end case;
         else
            error ("The output directory (" & checkdir & ") does not exist.");
            return;
         end if;
      end;
   end if;

   declare
      filename  : constant String := ASU.To_String (arg_filename);
      outputdir : constant String := Archive.Unix.real_path (ASU.To_String (arg_outdir));
      basename  : constant String := tail (Archive.Unix.real_path (filename), "/");
      operation : Archive.Unpack.Darc;
      level     : Archive.info_level := Archive.normal;
      exitcode  : CLI.Exit_Status := 0;
   begin
      if not DIR.Exists (filename) then
         error ("The rvn archive (" & filename & ") does not exist.");
         return;
      end if;
      case DIR.Kind (filename) is
         when DIR.Ordinary_File => null;
         when others =>
            error ("The rvn archive (" & filename & ") is not a regular file.");
            return;
      end case;

      if opt_verbose then
         level := Archive.verbose;
      elsif opt_quiet then
         level := Archive.silent;
      end if;

      operation.open_rvn_archive (filename, level);
      if opt_manifest then
         if opt_outdir then
            declare
               target : constant String := outputdir & "/" & basename & ".manifest";
            begin
               operation.write_manifest_to_file (opt_digest, opt_attr, target);
            end;
         else
            operation.print_manifest (opt_digest, opt_attr, 0);
         end if;
      elsif opt_metadata then
         if opt_outdir then
            declare
               target : constant String := outputdir & "/" & basename & ".metadata";
            begin
               operation.write_metadata_to_file (target);
            end;
         else
            TIO.Put_Line (operation.extract_metadata);
         end if;
      elsif opt_extract then
         declare
            set_modtime : constant Boolean := not opt_touch;
            set_owners  : constant Boolean := Archive.Unix.user_is_root;
            set_perms   : constant Boolean := set_owners;
            success     : Boolean;
            scripts_yes : Boolean := False;
            metadata    : constant String := operation.extract_metadata;
            scripts_key : constant String := "scripts";
            pre_key     : constant String := "pre-install";
            post_key    : constant String := "post-install";
            metatree    : ThickUCL.UclTree;
            vndx        : ThickUCL.object_index;
         begin
            begin
               ThickUCL.Files.parse_ucl_string (metatree, metadata, "");
            exception
               when ThickUCL.Files.ucl_data_unparseable =>
                  if not opt_quiet then
                     TIO.Put_Line ("Failed to parse metadata; loss of any phase actions");
                  end if;
            end;
            --  Run the pre-install actions
            case metatree.get_data_type (scripts_key) is
               when ThickUCL.data_object =>
                  scripts_yes := True;
                  vndx := metatree.get_index_of_base_ucl_object (scripts_key);
               when others => null;
            end case;
            if scripts_yes then
               execute_scripts (metatree, vndx, pre_key);
            end if;

            success := operation.extract_archive (top_directory => extraction_directory,
                                                  set_owners    => set_owners,
                                                  set_perms     => set_perms,
                                                  set_modtime   => set_modtime);

            if success then
               if scripts_yes then
                  execute_scripts (metatree, vndx, post_key);
               end if;
            else
               if not opt_quiet then
                  TIO.Put_Line ("Extraction did not fully succeed.");
               end if;
               exitcode := CLI.Exit_Status (-1);
            end if;
         end;
      elsif opt_counts then
         operation.print_magic_block;
      else
         TIO.Put_Line ("programming error - fallthrough can't happen.");
      end if;

      operation.close_rvn_archive;
      CLI.Set_Exit_Status (exitcode);
   end;
end Xrvn;
