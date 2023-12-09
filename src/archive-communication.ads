with Archive.Unix;

package Archive.Communication is

   procedure initialize    (new_squawk : info_level; pipe_descriptor : Unix.File_Descriptor);
   procedure emit_error    (message : String);
   procedure emit_notice   (message : String);
   procedure emit_message  (message : String);
   procedure emit_debug    (message : String);

private

   squawk_level : info_level := silent;
   pipe         : Unix.File_Descriptor := Unix.not_connected;

   --  warnx prints the message to standard error verbatim
   procedure warnx (verbatim_message : String);

   ----------------------
   --  JSON functions  --
   ----------------------

   --  Escape " and \ characters by prefacing them with a \ character
   function json_escape (S : String) return String;

   --  Returns '{ arg1 }'
   function json_object (content : String) return String;

   --  Returns '"arg1" : "json_escape(arg2)"'
   function json_pair (name, value : String) return String;

   --  Returns '"arg1" { arg2 }'
   function json_objectpair (name, content : String) return String;

   --  Returns '"arg1" [ arg2 ]'
   function json_arraypair (name, content : String) return String;

   --  return 'arg1, arg2'  (concatenates with comma)
   function CC (item1, item2 : String) return String;

   --  Return input surrounded by double quotation marks
   function DQ (txt : String) return String;


end Archive.Communication;
