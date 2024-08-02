with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Characters.Handling;
with Ada.Command_Line;
with Ada.Directories;
with Archive.Pack;
with Archive.Unix;

procedure Packrvn
is
   package TIO renames Ada.Text_IO;
   package ASU renames Ada.Strings.Unbounded;
   package HAN renames Ada.Characters.Handling;
   package CLI renames Ada.Command_Line;
   package DIR renames Ada.Directories;

   subtype text is ASU.Unbounded_String;

   procedure process_arguments;
   procedure usage (error_msg : String);
   procedure error (error_msg : String);
   function creation_directory return String;
   function rvn_file return String;
   function tail (S : String; delimiter : String) return String;
   function provide_timestamp (arg : text) return Archive.filetime;
   function valid_directory
     (opt_directory : Boolean;
      arg_directory : text;
      description   : String) return Boolean;
   function valid_file
     (opt_file      : Boolean;
      arg_file      : text;
      description   : String) return Boolean;

   opt_verbose   : Boolean := False;
   opt_quiet     : Boolean := False;
   opt_whitelist : Boolean := False;
   opt_metadata  : Boolean := False;
   opt_rootdir   : Boolean := False;
   opt_outdir    : Boolean := False;
   opt_kwdir     : Boolean := False;
   filename_set  : Boolean := False;
   arg_whitelist : text;
   arg_metadata  : text;
   arg_rootdir   : text;
   arg_outdir    : text;
   arg_filename  : text;
   arg_prefix    : text;
   arg_abi       : text;
   arg_timestamp : text;
   arg_keyword   : text := ASU.To_Unbounded_String ("/var/ravenports/conspiracy/Mk/Keywords");

   procedure process_arguments
   is
      type compound is (waiting, rootdir, outdir, whitelist, metadata, prefix, abi,
                        timestamp, kwdir);
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
               when waiting   => continue := True;
               when rootdir   => arg_rootdir   := ASU.To_Unbounded_String (this_arg);
               when outdir    => arg_outdir    := ASU.To_Unbounded_String (this_arg);
               when whitelist => arg_whitelist := ASU.To_Unbounded_String (this_arg);
               when metadata  => arg_metadata  := ASU.To_Unbounded_String (this_arg);
               when prefix    => arg_prefix    := ASU.To_Unbounded_String (this_arg);
               when abi       => arg_abi       := ASU.To_Unbounded_String (this_arg);
               when timestamp => arg_timestamp := ASU.To_Unbounded_String (this_arg);
               when kwdir     => arg_keyword   := ASU.To_Unbounded_String (this_arg);
            end case;
            next_parameter := waiting;
            if continue then
               if this_arg'Length > 1 then
                  if this_arg (this_arg'First .. this_arg'First + 1) = "--" then
                     if this_arg = "--rootdir" then
                        opt_rootdir := True;
                        next_parameter := rootdir;
                     elsif this_arg = "--out-dir" then
                        opt_outdir := True;
                        next_parameter := outdir;
                     elsif this_arg = "--keyword-dir" then
                        opt_kwdir := True;
                        next_parameter := kwdir;
                     elsif this_arg = "--whitelist" then
                        opt_whitelist := True;
                        next_parameter := whitelist;
                     elsif this_arg = "--metadata" then
                        opt_metadata := True;
                        next_parameter := metadata;
                     elsif this_arg = "--prefix" then
                        next_parameter := prefix;
                     elsif this_arg = "--abi" then
                        next_parameter := abi;
                     elsif this_arg = "--timestamp" then
                        next_parameter := timestamp;
                     elsif this_arg = "--verbose" then
                        opt_verbose := True;
                     elsif this_arg = "--quiet" then
                        opt_quiet := True;
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
                              when 'q' => opt_quiet := True;
                              when 'r' | 'o' | 'w' | 'm' | 'p' | 'a' | 't' | 'k' =>
                                 case this_arg (single) is
                                    when 'r' => opt_rootdir   := True;
                                    when 'o' => opt_outdir    := True;
                                    when 'k' => opt_kwdir     := True;
                                    when 'w' => opt_whitelist := True;
                                    when 'm' => opt_metadata  := True;
                                    when others => null;
                                 end case;
                                 if single = this_arg'Last then
                                    case this_arg (single) is
                                       when 'r' => next_parameter := rootdir;
                                       when 'o' => next_parameter := outdir;
                                       when 'k' => next_parameter := kwdir;
                                       when 'w' => next_parameter := whitelist;
                                       when 'm' => next_parameter := metadata;
                                       when 'p' => next_parameter := prefix;
                                       when 'a' => next_parameter := abi;
                                       when 't' => next_parameter := timestamp;
                                       when others => null;
                                    end case;
                                 else
                                    declare
                                       remainder : constant text := ASU.To_Unbounded_String
                                         (this_arg (single + 1 .. this_arg'Last));
                                    begin
                                       case this_arg (single) is
                                          when 'r' => arg_rootdir   := remainder;
                                          when 'o' => arg_outdir    := remainder;
                                          when 'k' => arg_keyword   := remainder;
                                          when 'w' => arg_whitelist := remainder;
                                          when 'm' => arg_metadata  := remainder;
                                          when 'p' => arg_prefix    := remainder;
                                          when 'a' => arg_abi       := remainder;
                                          when 't' => arg_timestamp := remainder;
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
      TIO.Put_Line ("packrvn [-vq] -r rootdir [-o outdir] [-w whitelist] [-p prefix] [-a abi]");
      TIO.Put_Line ("        [-k keyword_dir] [-m metadata] [-t timestamp] filename");
   end usage;

   procedure error (error_msg : String) is
   begin
      if not opt_quiet then
         TIO.Put_Line (error_msg);
      end if;
   end error;

   function valid_directory
     (opt_directory : Boolean;
      arg_directory : text;
      description   : String) return Boolean
   is
      checkdir : constant String := ASU.To_String (arg_directory);
   begin
      if not opt_directory then
         return True;
      end if;

      --  verify directory exists and is a directory.
      if DIR.Exists (checkdir) then
         case DIR.Kind (checkdir) is
            when DIR.Directory =>
               return True;
            when others =>
               error ("The " & description & " argument (" & checkdir & ") is not a directory.");
               return False;
         end case;
      end if;

      error ("The " & description & " directory (" & checkdir & ") does not exist.");
      return False;
   end valid_directory;

   function valid_file
     (opt_file      : Boolean;
      arg_file      : text;
      description   : String) return Boolean
   is
      checkfile : constant String := ASU.To_String (arg_file);
   begin
      if not opt_file then
         return True;
      end if;

      --  verify file exists and is a regular file.
      if DIR.Exists (checkfile) then
         case DIR.Kind (checkfile) is
            when DIR.Ordinary_File =>
               return True;
            when others =>
               error ("The " & description & " argument (" & checkfile & ") is not a file.");
               return False;
         end case;
      end if;

      error ("The " & description & " file (" & checkfile & ") does not exist.");
      return False;
   end valid_file;

   function creation_directory return String is
   begin
      if opt_outdir then
         return ASU.To_String (arg_outdir);
      else
         return ".";
      end if;
   end creation_directory;

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

   function rvn_file return String
   is
      filename  : constant String := ASU.To_String (arg_filename);
      fname     : constant String := tail (filename, "/");
      out_level : constant String := Archive.Unix.real_path (creation_directory);
      extension : constant String := ".rvn";
   begin
      if fname'Length > extension'Length and then
        HAN.To_Lower (fname (fname'Last - extension'Length + 1 .. fname'Last)) = extension
      then
         return out_level & "/" & fname;
      end if;
      return out_level & "/" & fname & extension;
   end rvn_file;

   function provide_timestamp (arg : text) return Archive.filetime
   is
      result : Archive.filetime := 0;
   begin
      if ASU.Length (arg) > 0 then
         declare
            argstr : constant String := ASU.To_String (arg);
            tmpres : Archive.filetime;
         begin
            tmpres := Archive.filetime'Value (argstr);
            result := tmpres;
         exception
            when Constraint_Error =>
               error ("Unable to convert timestamp argument to an integer; skipping override");
         end;
      end if;
      return result;
   end provide_timestamp;

begin
   process_arguments;
   if opt_quiet and then opt_verbose then
      usage ("The --quiet and --verbose options are mutually exclusive");
      return;
   end if;
   if not filename_set then
      usage ("The filename argument (path to rvn file to create) is required.");
      return;
   end if;
   if not valid_directory (opt_rootdir, arg_rootdir, "root") then
      return;
   end if;
   if not valid_directory (opt_outdir, arg_outdir, "output") then
      return;
   end if;
   if not valid_directory (opt_kwdir, arg_keyword, "keywords") then
      return;
   end if;
   if not valid_file (opt_whitelist, arg_whitelist, "whitelist") then
      return;
   end if;
   if not valid_file (opt_metadata, arg_metadata, "metadata") then
      return;
   end if;
   if not opt_rootdir then
      usage ("The root directory argument is required.");
      return;
   end if;

   declare
      top_level : constant String := Archive.Unix.real_path (ASU.To_String (arg_rootdir));
      level     : Archive.info_level := Archive.normal;
      exitcode  : CLI.Exit_Status := 0;
      dummy     : TIO.File_Type;
   begin
      if opt_verbose then
         level := Archive.verbose;
      elsif opt_quiet then
         level := Archive.silent;
      end if;

      if not Archive.Pack.integrate (top_level_directory => top_level,
                                     metadata_file       => ASU.To_String (arg_metadata),
                                     manifest_file       => ASU.To_String (arg_whitelist),
                                     prefix              => ASU.To_String (arg_prefix),
                                     abi                 => ASU.To_String (arg_abi),
                                     keyword_dir         => ASU.To_String (arg_keyword),
                                     fixed_timestamp     => provide_timestamp (arg_timestamp),
                                     output_file         => rvn_file,
                                     verbosity           => level,
                                     record_base_libs    => False,
                                     integrate_log       => dummy)
      then
         error ("Archive creation failed.");
         exitcode := CLI.Exit_Status (-1);
      end if;
      CLI.Set_Exit_Status (exitcode);
   end;

end Packrvn;
