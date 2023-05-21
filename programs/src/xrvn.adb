with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Command_Line;

procedure Xrvn
is
   package TIO renames Ada.Text_IO;
   package ASU renames Ada.Strings.Unbounded;
   package CLI renames Ada.Command_Line;

   subtype text is ASU.Unbounded_String;
   procedure process_arguments;

   opt_verbose  : Boolean := False;
   opt_digest   : Boolean := False;
   opt_extract  : Boolean := False;
   opt_metadata : Boolean := False;
   opt_manifest : Boolean := False;
   opt_outdir   : Boolean := False;
   arg_extract  : text;
   arg_metadata : text;
   arg_manifest : text;
   arg_outdir   : text;
   arg_filename : text;

   procedure process_arguments
   is
      type compound is (waiting, extract, metadata, manifest, outdir);
      argx : Natural := 0;
      next_parameter : compound := waiting;
      filename_set : Boolean := False;
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
               when extract  => arg_extract  := ASU.To_Unbounded_String (this_arg);
               when metadata => arg_metadata := ASU.To_Unbounded_String (this_arg);
               when manifest => arg_manifest := ASU.To_Unbounded_String (this_arg);
               when outdir   => arg_outdir   := ASU.To_Unbounded_String (this_arg);
            end case;
            next_parameter := waiting;
            if continue then
               if this_arg'Length > 1 then
                  if this_arg (this_arg'First .. this_arg'First + 1) = "--" then
                     if this_arg = "--extract" then
                        opt_extract := True;
                        next_parameter := extract;
                     elsif this_arg = "--metadata" then
                        opt_metadata := True;
                        next_parameter := metadata;
                     elsif this_arg = "--list-manifest" then
                        opt_manifest := True;
                        next_parameter := manifest;
                     elsif this_arg = "--out-dir" then
                        opt_outdir := True;
                        next_parameter := outdir;
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
                              when 'x' | 'm' | 'l' | 'o' =>
                                 case this_arg (single) is
                                    when 'x' => opt_extract := True;
                                    when 'm' => opt_metadata := True;
                                    when 'l' => opt_manifest := True;
                                    when 'o' => opt_outdir := True;
                                    when others => null;
                                 end case;
                                 if single = this_arg'Last then
                                    case this_arg (single) is
                                       when 'x' => next_parameter := extract;
                                       when 'm' => next_parameter := metadata;
                                       when 'l' => next_parameter := manifest;
                                       when 'o' => next_parameter := outdir;
                                       when others => null;
                                    end case;
                                 else
                                    declare
                                       remainder : constant text := ASU.To_Unbounded_String
                                         (this_arg (single + 1 .. this_arg'Last));
                                    begin
                                       case this_arg (single) is
                                       when 'x' => arg_extract  := remainder;
                                       when 'm' => arg_metadata := remainder;
                                       when 'l' => arg_manifest := remainder;
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

begin
   process_arguments;
   TIO.Put_Line ("v: " & opt_verbose'Img);
   TIO.Put_Line ("d: " & opt_digest'Img);
   TIO.Put_Line ("x: " & opt_extract'Img & "   [" & ASU.To_String (arg_extract) & "]");
   TIO.Put_Line ("m: " & opt_metadata'Img & "   [" & ASU.To_String (arg_metadata) & "]");
   TIO.Put_Line ("l: " & opt_manifest'Img & "   [" & ASU.To_String (arg_manifest) & "]");
   TIO.Put_Line ("o: " & opt_outdir'Img & "   [" & ASU.To_String (arg_outdir) & "]");
   TIO.Put_Line ("filename: " & ASU.To_String (arg_filename));
end Xrvn;
