with Getopts;
with Ada.Text_IO;

procedure Xrvn
is
   package TIO renames Ada.Text_IO;

   gparser : Getopts.Parser;
begin
   for switch in gparser.Getopt(":vdx:m:l:o:") loop
      case Getopts.Option (switch) is
         when 'v' => TIO.Put_Line ("verbose [short]");
         when 'd' => TIO.Put_Line ("digest [short]");
         when 'x' =>
            TIO.Put_Line ("extract [short]");
            TIO.Put_Line ("follows: " & Getopts.Optarg (switch));
         when 'm' =>
            TIO.Put_Line ("metadata [short]");
            TIO.Put_Line ("follows: " & Getopts.Optarg (switch));
         when 'l' =>
            TIO.Put_Line ("list-manifest [short]");
            TIO.Put_Line ("follows: " & Getopts.Optarg (switch));
         when 'o' =>
            TIO.Put_Line ("outdir [short]");
            TIO.Put_Line ("follows: " & Getopts.Optarg (switch));
         when others =>
            TIO.Put_Line ("not handled: " & Getopts.option (switch));
      end case;
   end loop;

   	for I in 1 .. gparser.Argument_Count loop
		TIO.Put_Line ("Argument " & I'Image & ": " & gparser.Argument(I));
	end loop;
end Xrvn;
