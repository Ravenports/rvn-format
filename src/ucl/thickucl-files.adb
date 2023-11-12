--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt


with Ada.Text_IO;
with System;

package body ThickUCL.Files is

   package TIO renames Ada.Text_IO;

   ----------------------
   --  parse_ucl_file  --
   ----------------------
   procedure parse_ucl_file
     (tree : in out UclTree;
      path : String)
   is
      parser  : Ucl.T_parser;
      obj     : access libucl.ucl_object_t;
   begin
      parser := Ucl.ucl_parser_new_nofilevars;
      if not Ucl.ucl_parser_add_file (parser, path) then
         libucl.ucl_parser_free (parser);
         raise ucl_file_unparseable with path;
      end if;
      obj := Ucl.ucl_parser_get_object (parser);
      libucl.ucl_parser_free (parser);

      if obj = null then
         --  This shouldn't happen, but add check to avoid possible free-on-null
         return;
      end if;

      if Ucl.type_is_object (obj) then
         populate_the_tree (tree, obj);
      end if;

      libucl.ucl_object_unref (obj);
   end parse_ucl_file;


   ------------------
   --  extract_key --
   ------------------
   function extract_key (item : access constant libucl.ucl_object_t) return String is
   begin
      return Ucl.ucl_object_key (item);
   end extract_key;


   -------------------------
   --  populate_the_tree  --
   -------------------------
   procedure populate_the_tree
     (tree : in out UclTree;
      rootobj : access constant libucl.ucl_object_t)
   is
      iter : aliased libucl.ucl_object_iter_t := libucl.ucl_object_iter_t (System.Null_Address);
      item : access constant libucl.ucl_object_t;
      item_type : ucl.intermediate_ucl_type;
   begin
      loop
         item := Ucl.ucl_object_iterate (rootobj, iter'Access, True);
         exit when item = null;

         item_type := ucl.intermediate_type (item);
         case item_type is
            when ucl.med_null =>
               TIO.Put_Line ("Unexpected ucl parse error: object type is null, key="
                             & extract_key (item));

            when ucl.med_userdata =>
               TIO.Put_Line ("Data of type UCL_USERDATA found.  Skipping unsupported type, key="
                             & extract_key (item));

            when ucl.med_boolean =>
               tree.insert (extract_key (item), ucl.ucl_object_toboolean (item));
            when ucl.med_int =>
               tree.insert (extract_key (item), ucl.ucl_object_toint (item));
            when ucl.med_float =>
               tree.insert (extract_key (item), ucl.ucl_object_tofloat (item));
            when ucl.med_string =>
               tree.insert (extract_key (item), ucl.ucl_object_tostring_forced (item));
            when ucl.med_time =>
               tree.insert (extract_key (item), ucl.ucl_object_totime (item));

            when ucl.med_object =>
               tree.start_object (extract_key (item));
               populate_the_tree (tree, item);
               tree.close_object;

            when ucl.med_array =>
               tree.start_array (extract_key (item));
               populate_array (tree, item);
               tree.close_array;
         end case;
      end loop;
   end populate_the_tree;


   -------------------------
   --  populate_the_tree  --
   -------------------------
   procedure populate_array
     (tree : in out UclTree;
      arrayobj :  access constant libucl.ucl_object_t)
   is
      iter : aliased libucl.ucl_object_iter_t := libucl.ucl_object_iter_t (System.Null_Address);
      item : access constant libucl.ucl_object_t;
      item_type : ucl.intermediate_ucl_type;
   begin
      loop
         item := Ucl.ucl_object_iterate (arrayobj, iter'Access, True);
         exit when item = null;

         item_type := ucl.intermediate_type (item);
         case item_type is
            when ucl.med_null =>
               TIO.Put_Line ("Unexpected ucl parse error: array type is null");

            when ucl.med_userdata =>
               TIO.Put_Line ("Data of type UCL_USERDATA found.  Skipping unsupported type");

            when ucl.med_boolean =>
               tree.insert ("", ucl.ucl_object_toboolean (item));
            when ucl.med_int =>
               tree.insert ("", ucl.ucl_object_toint (item));
            when ucl.med_float =>
               tree.insert ("", ucl.ucl_object_tofloat (item));
            when ucl.med_string =>
               tree.insert ("", ucl.ucl_object_tostring_forced (item));
            when ucl.med_time =>
               tree.insert ("", ucl.ucl_object_totime (item));

            when ucl.med_object =>
               tree.start_object ("");
               populate_the_tree (tree, item);
               tree.close_object;

            when ucl.med_array =>
               tree.start_array ("");
               populate_array (tree, item);
               tree.close_array;
         end case;
      end loop;
   end populate_array;

end ThickUCL.Files;
