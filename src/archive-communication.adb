--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Ada.Text_IO;
with Ada.Characters.Latin_1;


package body Archive.Communication is

   package TIO renames Ada.Text_IO;
   package LAT renames Ada.Characters.Latin_1;


   ------------------
   --  initialize  --
   ------------------
   procedure initialize (new_squawk : info_level; pipe_descriptor : Unix.File_Descriptor) is
   begin
      squawk_level := new_squawk;
      pipe := pipe_descriptor;
   end initialize;


   -------------
   --  warnx  --
   -------------
   procedure warnx (verbatim_message : String) is
   begin
      TIO.Put_Line (TIO.Standard_Error, verbatim_message);
   end warnx;


   ------------------
   --  pipe_event  --
   ------------------
   procedure pipe_event (json_message : String) is
   begin
      if Unix.file_connected (pipe) then
         Unix.push_to_event_pipe (pipe, json_message);
      end if;
   end pipe_event;


   --------------------
   --  emit_message  --
   --------------------
   procedure emit_message (message : String)
   is
      --  Do not send messages to event pipe
   begin
      TIO.Put_Line (message);
   end emit_message;


   -------------------
   --  emit_notice  --
   -------------------
   procedure emit_notice (message : String)
   is
      jmsg : constant String := json_object
        (CC
           (json_pair ("type", "NOTICE"),
            json_objectpair ("data", json_pair ("msg", message))));
   begin
      pipe_event (jmsg);
      TIO.Put_Line (message);
   end emit_notice;


   ------------------
   --  emit_error  --
   ------------------
   procedure emit_error (message : String)
   is
      jmsg : constant String := json_object
        (CC
           (json_pair ("type", "ERROR"),
            json_objectpair ("data", json_pair ("msg", message))));
   begin
      pipe_event (jmsg);
      warnx (message);
   end emit_error;


   ------------------
   --  emit_debug  --
   ------------------
   procedure emit_debug    (message : String)
   is
   begin
      warnx ("DEBUG: " & message);
   end emit_debug;


   ----------
   --  CC  --
   ----------
   function CC (item1, item2 : String) return String is
   begin
      return item1 & ", " & item2;
   end CC;


   -----------------
   --  json_pair  --
   -----------------
   function json_pair (name, value : String) return String is
   begin
      return DQ (name) & " : " & DQ (json_escape (value));
   end json_pair;


   -----------------------
   --  json_objectpair  --
   -----------------------
   function json_objectpair (name, content : String) return String is
   begin
      if name = "" then
         return "{ " & content & " }";
      else
         if content = "" then
            return DQ (name) & " {}";
         else
            return DQ (name) & " { " & content & " }";
         end if;
      end if;
   end json_objectpair;


   ----------------------
   --  json_arraypair  --
   ----------------------
   function json_arraypair (name, content : String) return String is
   begin
      if content = "" then
         return DQ (name) & " []";
      else
         return DQ (name) & " [ " & content & " ]";
      end if;
   end json_arraypair;


   -------------------
   --  json_object  --
   -------------------
   function json_object (content : String) return String is
   begin
      return "{ " & content & " }";
   end json_object;


   ----------
   --  DQ  --
   ----------
   function DQ (txt : String) return String is
   begin
      return LAT.Quotation & txt & LAT.Quotation;
   end DQ;


   -------------------
   --  json_escape  --
   -------------------
   function json_escape (S : String) return String
   is
      --  The following characters must be escaped:
      --    Backspace
      --    Form Feed
      --    Line Feed
      --    Carriage Return
      --    Tab
      --    Double Quote
      --    Backslash
      new_length : Natural := 0;
   begin
      for x in S'Range loop
         case S (x) is
            when LAT.BS | LAT.FF | LAT.LF | LAT.CR | LAT.HT
               | LAT.Quotation
               | LAT.Reverse_Solidus =>
               new_length := new_length + 2;
            when others =>
               new_length := new_length + 1;
         end case;
      end loop;
      if new_length = S'Length then
         --  No special characters found, return original string
         return S;
      end if;
      declare
         result : String (1 .. new_length);
         index  : Natural := result'First;
      begin
         for x in S'Range loop
            case S (x) is
               when LAT.BS =>
                  result (index .. index + 1) := "\b";
                  index := index + 2;
               when LAT.FF =>
                  result (index .. index + 1) := "\f";
                  index := index + 2;
               when LAT.LF =>
                  result (index .. index + 1) := "\n";
                  index := index + 2;
               when LAT.HT =>
                  result (index .. index + 1) := "\t";
                  index := index + 2;
               when LAT.Quotation =>
                  result (index .. index + 1) := LAT.Reverse_Solidus & LAT.Quotation;
                  index := index + 2;
               when LAT.Reverse_Solidus =>
                  result (index .. index + 1) := "\\";
                  index := index + 2;
            when others =>
               result (index) := S (x);
               index := index + 1;
            end case;
         end loop;
         return result;
      end;
   end json_escape;


end Archive.Communication;
