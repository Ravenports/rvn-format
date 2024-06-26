--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Ada.Text_IO;

package Bourne is

   --  At it's most basic, this procedure runs the given script by the given interpreter
   --  (normally /bin/sh) while setting some environment variables to the values of the
   --  other arguments.
   --
   --  If the script > 4000 characters, write it to a temporary file first, and make the
   --  interpreter run the temp file and delete it afterwards.  Otherwise just send the
   --  entire script through the command argument.
   --
   --  Finally, set PKG_MSGFILE in the environment to the path of a temporary file in the user's
   --  home directory.  When the script is complete, check for existence of this file.  If it
   --  exists, read and send to standard output, then delete the file.
   --
   --  Note this differs from FreeBSD pkg which uses File Descriptor 4 to store the communication
   --  in a pipe that would have to be read later.

   procedure run_shell_script
     (namebase    : String;
      subpackage  : String;
      variant     : String;
      prefix      : String;
      root_dir    : String;
      upgrading   : Boolean;
      interpreter : String;
      script      : String;
      arguments   : String;
      msg_outfile : String;
      out_handle  : Ada.Text_IO.File_Type;
      success     : out Boolean);

   --  Return a randomly-named msgfile path that isn't currently being used.
   function unique_msgfile_path return String;

   --  Disable any postrun messages and remove the temporary file
   procedure show_post_run_messages
     (msg_outfile : String;
      namebase    : String;
      subpackage  : String;
      variant     : String;
      extract_log : Ada.Text_IO.File_Type);

   interpreter_missing : exception;
   ginormous_script    : exception;
   file_dump           : exception;

private

   --  Returns the nanosecond portion of the current time.
   --  This is used for a temporary file prefix.
   function random_extension return String;

   --  Transfers the contents of a string to a file in one pass.
   procedure dump_contents_to_file (contents : String; dossier : String);

   --  Transfers the contents of file_to_read to the end of file_to_write
   procedure append_file (file_to_write : String; file_to_read : String);

end Bourne;
