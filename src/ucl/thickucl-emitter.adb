--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

package body ThickUCL.Emitter is

   package ASF renames Ada.Strings.Fixed;
   package ASU renames Ada.Strings.Unbounded;


   ----------------
   --  emit_ucl  --
   ----------------
   function emit_ucl (tree : UclTree) return String
   is
      procedure scan_key (Position : jar_string.Cursor);
      procedure dive_into_array (vndx : array_index; indent_len : Natural);
      procedure dive_into_object (vndx : object_index; indent_len : Natural);

      canvas : ASU.Unbounded_String;
      stumpkeys : jar_string.Vector;

      procedure scan_key (Position : jar_string.Cursor)
      is
         raw_key : constant String := ASU.To_String (jar_string.Element (Position).payload);
         valtype : Leaf_type;
      begin
         ASU.Append (canvas, format_key (raw_key) & ": ");
         valtype := tree.get_data_type (raw_key);
         case valtype is
            when data_not_present =>
               --  This should not be possible
               ASU.Append (canvas, "null");
            when data_string =>
               ASU.Append (canvas, format_string_value (tree.get_base_value (raw_key)));
            when data_boolean =>
               ASU.Append (canvas, format_boolean_value (tree.get_base_value (raw_key)));
            when data_integer =>
               ASU.Append (canvas, format_integer_value (tree.get_base_value (raw_key)));
            when data_float =>
               ASU.Append (canvas, format_float_value (tree.get_base_value (raw_key)));
            when data_time =>
               ASU.Append (canvas, format_time_value (tree.get_base_value (raw_key)));
            when data_array =>
               ASU.Append (canvas, '[' & LF);
               dive_into_array (tree.get_index_of_base_array (raw_key), 2);
               ASU.Append (canvas, ']' & LF);
            when data_object =>
               ASU.Append (canvas, '{' & LF);
               dive_into_object (tree.get_index_of_base_ucl_object (raw_key), 2);
               ASU.Append (canvas, '}' & LF);
         end case;
      end scan_key;

      procedure dive_into_array (vndx : array_index; indent_len : Natural)
      is
         array_len : constant Natural := tree.get_number_of_array_elements (vndx);
         indent    : constant String (1 .. indent_len) := (others => ' ');
      begin
         for elndx in 1 .. array_len loop
            declare
               valtype : Leaf_type;
            begin
               valtype := tree.get_array_element_type (vndx, elndx);
               case valtype is
                  when data_not_present =>
                     null;  -- should be impossible
                  when data_integer =>
                     ASU.Append (canvas, indent & format_string_value
                                 (tree.get_array_element_value (vndx, elndx)));
                  when data_float =>
                     ASU.Append (canvas, indent & format_float_value
                                 (tree.get_array_element_value (vndx, elndx)));
                  when data_string =>
                     ASU.Append (canvas, indent & format_string_value
                                 (tree.get_array_element_value (vndx, elndx)));
                  when data_boolean =>
                     ASU.Append (canvas, indent & format_boolean_value
                                 (tree.get_array_element_value (vndx, elndx)));
                  when data_time =>
                     ASU.Append (canvas, indent & format_time_value
                                 (tree.get_array_element_value (vndx, elndx)));
                  when data_array =>
                     ASU.Append (canvas, indent & '[' & LF);
                     dive_into_array (tree.get_array_element_vector_index (vndx, elndx),
                                      indent_len + 2);
                     ASU.Append (canvas, indent & ']' & LF);
                  when data_object =>
                     ASU.Append (canvas, indent & '{' & LF);
                     dive_into_object (tree.get_array_element_object (vndx, elndx),
                                       indent_len + 2);
                     ASU.Append (canvas, indent & '}' & LF);
               end case;
            end;
         end loop;
      end dive_into_array;

      procedure dive_into_object (vndx : object_index; indent_len : Natural)
      is
         procedure scan_deeper_key (Position : jar_string.Cursor);

         keys   : jar_string.Vector;
         indent : constant String (1 .. indent_len) := (others => ' ');

         procedure scan_deeper_key (Position : jar_string.Cursor)
         is
            this_key : constant String := ASU.To_String (jar_string.Element (Position).payload);
            valtype  : Leaf_type;
         begin
            valtype := tree.get_object_data_type (vndx, this_key);
            ASU.Append (canvas, indent & format_key (this_key) & ": ");
            case valtype is
               when data_not_present =>
                  ASU.Append (canvas, "null");  -- should be impossible
               when data_boolean =>
                  ASU.Append (canvas, format_boolean_value
                              (tree.get_object_value (vndx, this_key)));
               when data_float =>
                  ASU.Append (canvas, format_float_value
                              (tree.get_object_value (vndx, this_key)));
               when data_integer =>
                  ASU.Append (canvas, format_integer_value
                              (tree.get_object_value (vndx, this_key)));
               when data_string =>
                  ASU.Append (canvas, format_string_value
                              (tree.get_object_value (vndx, this_key)));
               when data_time =>
                  ASU.Append (canvas, format_time_value
                              (tree.get_object_value (vndx, this_key)));
               when data_array =>
                  ASU.Append (canvas, '[' & LF);
                  dive_into_array (tree.get_object_array (vndx, this_key), indent_len + 2);
                  ASU.Append (canvas, ']' & LF);
               when data_object =>
                  ASU.Append (canvas, '{' & LF);
                  dive_into_object (tree.get_object_object (vndx, this_key), indent_len + 2);
                  ASU.Append (canvas, '}' & LF);
            end case;
         end scan_deeper_key;
      begin
         tree.get_object_object_keys (vndx, keys);
         keys.Iterate (scan_deeper_key'Access);
      end dive_into_object;

   begin
      tree.get_base_object_keys (stumpkeys);
      stumpkeys.Iterate (scan_key'Access);
      return ASU.To_String (canvas);
   end emit_ucl;


   -------------------------
   --  format_time_value  --
   -------------------------
   function format_time_value (raw : RT.Time_Span) return String
   is
      rawduration : Duration;
   begin
      rawduration := RT.To_Duration (raw);
      return ASF.Trim (rawduration'Img, Ada.Strings.Left) & 's' & LF;
   end format_time_value;


   --------------------------
   --  format_float_value  --
   --------------------------
   function format_float_value (raw : Float) return String
   is
      function trimzero (decimal_string : String) return String;

      int_part : Ucl.ucl_integer;
      dec_part : Ucl.ucl_integer;
      negative : Boolean;
      stripped : Float;
      abs_raw  : constant Float := abs (raw);

      function trimzero (decimal_string : String) return String
      is
         arrow : Natural := decimal_string'Last;
      begin
         loop
            exit when arrow = decimal_string'First;
            exit when decimal_string (arrow) /= '0';
            arrow := arrow - 1;
         end loop;
         return decimal_string (decimal_string'First .. arrow);
      end trimzero;
   begin
      negative := raw < 0.0;
      int_part := Ucl.ucl_integer (abs_raw);
      stripped := abs_raw - Float(int_part);
      dec_part := Ucl.ucl_integer (stripped * 1_000_000_000.0);
      declare
         sint_part : constant String := format_integer_value (int_part);
         sdec_part : constant String := format_integer_value (dec_part);
      begin
         if negative then
            return '-' & sint_part & "." & trimzero (sdec_part) & LF;
         end if;
         return sint_part & "." & trimzero (sdec_part) & LF;
      end;
   end format_float_value;


   ----------------------------
   --  format_integer_value  --
   ----------------------------
   function format_integer_value (raw : Ucl.ucl_integer) return String
   is
      raw_image : constant String := raw'Img;
   begin
      if raw < 0 then
         return raw_image & LF;
      end if;
      return raw_image (raw_image'First + 1 .. raw_image'Last) & LF;
   end format_integer_value;


   ----------------------------
   --  format_boolean_value  --
   ----------------------------
   function format_boolean_value (raw : Boolean) return String is
   begin
      case raw is
         when True  => return "true" & LF;
         when False => return "false" & LF;
      end case;
   end format_boolean_value;


   ---------------------------
   --  format_string_value  --
   ---------------------------
   function format_string_value (raw : String) return String
   is
      SQ        : constant Character := Character'Val (39);
      newline   : constant String (1 .. 1) := (1 => Character'Val (10));
      SQpattern : constant String (1 .. 1) := (1 => SQ);
   begin
      if ASF.Index (raw, newline) > 0 then
         return "<<EOD" & newline & raw & newline & "EOD" & LF;
      end if;
      if ASF.Index (raw, SQpattern) = 0 then
         return SQ & raw & SQ & LF;
      end if;
      declare
         procedure single_copy (char : Character);
         procedure escape_quote;

         canvas : String (1 .. raw'Length * 2);
         canlen : Natural := 0;

         procedure single_copy (char : Character) is
         begin
            canlen := canlen + 1;
            canvas (canlen) := char;
         end single_copy;

         procedure escape_quote is
         begin
            single_copy ('\');
            single_copy (SQ);
         end escape_quote;
      begin
         single_copy (SQ);
         for k in raw'Range loop
            case raw (k) is
               when SQ => escape_quote;
               when others => single_copy (raw (k));
            end case;
         end loop;
         single_copy (SQ);
         return canvas (1 .. canlen) & LF;
      end;
   end format_string_value;


   ------------------
   --  format_key  --
   ------------------
   function format_key (raw : String) return String
   is
      procedure single_copy (char : Character);
      procedure escape_quote;
      procedure copy_set_quote (char : Character);

      canvas : String (1 .. raw'Length * 2);
      canlen : Natural := 0;
      quotes : Boolean := False;
      SQ     : constant Character := Character'Val (39);

      procedure single_copy (char : Character) is
      begin
         canlen := canlen + 1;
         canvas (canlen) := char;
      end single_copy;

      procedure escape_quote is
      begin
         quotes := True;
         single_copy ('\');
         single_copy (SQ);
      end escape_quote;

      procedure copy_set_quote (char : Character) is
      begin
         quotes := True;
         single_copy (char);
      end copy_set_quote;
   begin
      for k in raw'Range loop
         case raw (k) is
            when '0' .. '9' | 'A' .. 'Z' | 'a' .. 'z' | '-' | '_' =>
               single_copy (raw (k));
            when Character'Val(0) .. Character'Val(31) |
                 Character'Val(127) .. Character'Val(255) =>
               null;
            when SQ =>
               escape_quote;
            when ' ' .. '&' | '(' .. ',' | '.' | '/' | ':' .. '@' |
               '[' .. '^' | '`' | '{' .. '~' =>
               copy_set_quote (raw (k));
         end case;
      end loop;
      if quotes then
         return SQ & canvas (1 .. canlen) & SQ;
      end if;
      return canvas (1 .. canlen);
   end format_key;

end ThickUCL.Emitter;
