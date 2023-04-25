--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt


with Ada.Streams.Stream_IO;

package Archive.Unpack is

   package SIO renames Ada.Streams.Stream_IO;

   type DArc is tagged limited private;

   procedure open_rvn_archive
     (DS          : in out DArc;
      rvn_archive : String;
      verbosity   : info_level);

   procedure close_rvn_archive
     (DS          : in out DArc);

private

   type DArc is tagged limited
      record
         header     : premier_block;
         valid      : Boolean;
         level      : info_level := silent;
         rvn_handle : SIO.File_Type;
         rvn_stmaxs : SIO.Stream_Access;
      end record;

   --  Prints message to standard out if the display level is high enough
   procedure print (DS : DArc; msg_level : info_level; message : String);

   --  Sets the standard out information level
   procedure set_verbosity (DS : in out DArc; level : info_level);

end Archive.Unpack;
