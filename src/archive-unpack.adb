--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt


with Ada.Text_IO;
with Ada.Directories;

package body Archive.Unpack is

   package TIO renames Ada.Text_IO;
   package DIR  renames Ada.Directories;

   procedure open_rvn_archive
     (DS          : in out DArc;
      rvn_archive : String;
      verbosity   : info_level)
   is
   begin
      DS.valid := False;
      DS.set_verbosity (verbosity);
      if DIR.Exists (rvn_archive) then
         case DIR.Kind (rvn_archive) is
            when DIR.Ordinary_File =>
               null;
            when others =>
               DS.print (normal, "The " & rvn_archive & " entity is not a regular file.");
               return;
         end case;
      else
         DS.print (normal, "The " & rvn_archive & " archive does not exist.");
         return;
      end if;

      if Natural (DIR.Size (rvn_archive)) < File_Block'Size then
         DS.print (normal, "The " & rvn_archive & " file is too small.  It's not an archive.");
         return;
      end if;

      SIO.Open (File => DS.rvn_handle,
                Mode => SIO.In_File,
                Name => rvn_archive);

      begin
         premier_block'Read (DS.rvn_stmaxs, DS.header);
      exception
         when others =>
            DS.print (normal, "Something went wrong opening " & rvn_archive);
            return;
      end;

      if DS.header.magic_bytes = magic then
         DS.print (debug, "The magic bytes of " & rvn_archive & " match an RVN archive.");
      else
         DS.print (normal, "The magic bytes of " & rvn_archive & " are wrong.");
         DS.print (normal, "This is not a RVN archive.");
         return;
      end if;

      DS.print (debug, "RVN format version :" & DS.header.version'Img);
      DS.print (debug, "    groups defined :" & DS.header.num_groups'Img);
      DS.print (debug, "    owners defined :" & DS.header.num_owners'Img);
      DS.print (debug, "     links defined :" & DS.header.link_blocks'Img);
      DS.print (debug, "      number files :" & DS.header.file_blocks'Img);
      DS.print (debug, "    metadata bytes :" & DS.header.size_metadata'Img);
      DS.print (debug, "  file index bytes :" & DS.header.size_filedata'Img);
      DS.print (debug, "     archive bytes :" & DS.header.size_archive'Img);

      DS.valid := True;

   end open_rvn_archive;


   ------------------------------------------------------------------------------------------
   --  close_rvn_archive
   ------------------------------------------------------------------------------------------
   procedure close_rvn_archive (DS : in out DArc)
   is
   begin
      SIO.Close (DS.rvn_handle);
      DS.print (debug, "RVN archive was closed.");
   end close_rvn_archive;


   ------------------------------------------------------------------------------------------
   --  set_verbosity
   ------------------------------------------------------------------------------------------
   procedure set_verbosity (DS : in out DArc; level : info_level)
   is
   begin
      DS.level := level;
   end set_verbosity;


   ------------------------------------------------------------------------------------------
   --  print
   ------------------------------------------------------------------------------------------
   procedure print (DS : DArc; msg_level : info_level; message : String)
   is
      meets_criteria : constant Boolean := (msg_level <= DS.level);
   begin
      if meets_criteria then
         if msg_level = debug then
            TIO.Put_Line ("DEBUG: " & message);
         else
            TIO.Put_Line (message);
         end if;
      end if;
   end print;

end Archive.Unpack;
