--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt


with Ada.Streams.Stream_IO;

package Archive.Unpack is

   package SIO renames Ada.Streams.Stream_IO;

   type DArc is tagged limited private;

   --  This procedure attempts to open an RVN archive.
   procedure open_rvn_archive
     (DS          : in out DArc;
      rvn_archive : String;
      verbosity   : info_level);

   --  This procedure attempts to close an RVN archive
   procedure close_rvn_archive (DS : in out DArc);

   --  This function indicates if the archive was successfully opened (and is still open).
   function rvn_archive_is_open (DS : DArc) return Boolean;

   --  This procedure decompresses the metadata and writes the output to the requested file.
   procedure write_metadata_to_file
     (DS       : in out DArc;
      filepath : String);

   --  This function decompresses the metadata and returns it as a string
   function extract_metadata (DS : in out DArc) return String;

private

   type DArc is tagged limited
      record
         header     : premier_block;
         valid      : Boolean;
         level      : info_level := silent;
         rvn_handle : SIO.File_Type;
         rvn_stmaxs : SIO.Stream_Access;
         b2_index   : SIO.Count;
         b3_index   : SIO.Count;
         b4_index   : SIO.Count;
      end record;

   --  Prints message to standard out if the display level is high enough
   procedure print (DS : DArc; msg_level : info_level; message : String);

   --  Sets the standard out information level
   procedure set_verbosity (DS : in out DArc; level : info_level);

   --  Creates a file with contents in a single write
   procedure direct_file_creation
     (DS          : DArc;
      target_file : String;
      contents    : String);

end Archive.Unpack;
