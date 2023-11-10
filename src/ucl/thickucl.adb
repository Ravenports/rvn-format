--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Ada.Strings.Hash;
with Ada.Text_IO;

package body ThickUCL is

   package TIO renames Ada.Text_IO;

   ----------------
   --  map_hash  --
   ----------------
   function map_hash (key : ASU.Unbounded_String) return CON.Hash_Type is
   begin
      return Ada.Strings.Hash (ASU.To_String (key));
   end map_hash;


   ------------------
   --  equivalent  --
   ------------------
   function equivalent (A, B : ASU.Unbounded_String) return Boolean
   is
      use type ASU.Unbounded_String;
   begin
      return A = B;
   end equivalent;


   -------------------
   --  key_missing  --
   -------------------
   function key_missing (name : String) return Boolean
   is
   begin
      return name = "";
   end key_missing;


   ---------------------------
   --  last_open_structure  --
   ---------------------------
   function last_open_structure (tree : UclTree) return DataType is
   begin
      return tree.open_structure.Last_Element.data_type;
   end last_open_structure;


   ----------------------------
   --  last_reference_index  --
   ----------------------------
   function last_reference_index (tree : UclTree) return Natural is
   begin
      return tree.open_structure.Last_Element.vector_index;
   end last_reference_index;


   ---------------------------------------------------------------------------------
   --  For the following Insert procedures:
   --  With new open structures, the pointer is on the stump.  That means each
   --  value requires a key (name) to be non-empty.
   --
   --  When to top open structure is an array, each name is required to be empty.
   --  Names that are non-empty in this case will invoke error messages.
   --
   --  When the top open structure is an object, names will again be required.
   ---------------------------------------------------------------------------------

   -----------------
   --  insert #1  --
   -----------------
   procedure insert
     (tree : in out UclTree;
      name : String;
      value : ucl.ucl_integer)
   is
      procedure wrap_payload;
      procedure append_into_array (Element : in out jar_array.Vector);
      procedure append_into_uclobj (Element : in out jar_ucl_objects.Map);

      struct_index : Natural;
      global_index : Natural;
      dref : DataReference;
      name_us : constant ASU.Unbounded_String := ASU.To_Unbounded_String (name);

      procedure wrap_payload is
      begin
         tree.store_integers.Append (value);
         global_index := tree.store_integers.Last_Index;
         dref.data_type := ucl_integer;
         dref.vector_index := global_index;
      end wrap_payload;

      procedure append_into_array (Element : in out jar_array.Vector) is
      begin
         Element.Append (dref);
      end append_into_array;

      procedure append_into_uclobj (Element : in out jar_ucl_objects.Map) is
      begin
         Element.Insert (name_us, dref);
      end append_into_uclobj;
   begin
      if tree.open_structure.Is_Empty then
         if key_missing (name) then
            TIO.Put_Line (ERR_NEEDS_KEY & ":" & value'Img);
            return;
         end if;
         wrap_payload;
         tree.tree_stump.Insert (name_us, dref);
         return;
      end if;

      case tree.last_open_structure is
         when ucl_array =>
            if not key_missing (name) then
               TIO.Put_Line (WARN_EXTRA_KEY & " (" & name & ")");
            end if;
            wrap_payload;
            struct_index := tree.last_reference_index;
            tree.store_arrays.Update_Element (Index => struct_index,
                                              Process => append_into_array'Access);
         when ucl_object =>
            if key_missing (name) then
               TIO.Put_Line (ERR_NEEDS_KEY & ":" & ucl_integer'Img);
               return;
            end if;
            wrap_payload;
            tree.store_objects.Update_Element (Index => struct_index,
                                               Process => append_into_uclobj'Access);
         when others =>
            --  It should be impossible to get here.
            null;
      end case;
   end insert;


   -----------------
   --  insert #2  --
   -----------------
   procedure insert
     (tree : in out UclTree;
      name : String;
      value : Float)
   is
      procedure wrap_payload;
      procedure append_into_array (Element : in out jar_array.Vector);
      procedure append_into_uclobj (Element : in out jar_ucl_objects.Map);

      struct_index : Natural;
      global_index : Natural;
      dref : DataReference;
      name_us : constant ASU.Unbounded_String := ASU.To_Unbounded_String (name);

      procedure wrap_payload is
      begin
         tree.store_floats.Append (value);
         global_index := tree.store_floats.Last_Index;
         dref.data_type := ucl_float;
         dref.vector_index := global_index;
      end wrap_payload;

      procedure append_into_array (Element : in out jar_array.Vector) is
      begin
         Element.Append (dref);
      end append_into_array;

      procedure append_into_uclobj (Element : in out jar_ucl_objects.Map) is
      begin
         Element.Insert (name_us, dref);
      end append_into_uclobj;
   begin
      if tree.open_structure.Is_Empty then
         if key_missing (name) then
            TIO.Put_Line (ERR_NEEDS_KEY & ":" & value'Img);
            return;
         end if;
         wrap_payload;
         tree.tree_stump.Insert (name_us, dref);
         return;
      end if;

      case tree.last_open_structure is
         when ucl_array =>
            if not key_missing (name) then
               TIO.Put_Line (WARN_EXTRA_KEY & " (" & name & ")");
            end if;
            wrap_payload;
            struct_index := tree.last_reference_index;
            tree.store_arrays.Update_Element (Index => struct_index,
                                              Process => append_into_array'Access);
         when ucl_object =>
            if key_missing (name) then
               TIO.Put_Line (ERR_NEEDS_KEY & ":" & ucl_integer'Img);
               return;
            end if;
            wrap_payload;
            tree.store_objects.Update_Element (Index => struct_index,
                                               Process => append_into_uclobj'Access);
         when others => null;
      end case;
   end insert;


   -----------------
   --  insert #3  --
   -----------------
   procedure insert
     (tree : in out UclTree;
      name : String;
      value : Boolean)
   is
      procedure wrap_payload;
      procedure append_into_array (Element : in out jar_array.Vector);
      procedure append_into_uclobj (Element : in out jar_ucl_objects.Map);

      struct_index : Natural;
      global_index : Natural;
      dref : DataReference;
      name_us : constant ASU.Unbounded_String := ASU.To_Unbounded_String (name);

      procedure wrap_payload is
      begin
         tree.store_booleans.Append (value);
         global_index := tree.store_booleans.Last_Index;
         dref.data_type := ucl_boolean;
         dref.vector_index := global_index;
      end wrap_payload;

      procedure append_into_array (Element : in out jar_array.Vector) is
      begin
         Element.Append (dref);
      end append_into_array;

      procedure append_into_uclobj (Element : in out jar_ucl_objects.Map) is
      begin
         Element.Insert (name_us, dref);
      end append_into_uclobj;
   begin
      if tree.open_structure.Is_Empty then
         if key_missing (name) then
            TIO.Put_Line (ERR_NEEDS_KEY & ":" & value'Img);
            return;
         end if;
         wrap_payload;
         tree.tree_stump.Insert (name_us, dref);
         return;
      end if;

      case tree.last_open_structure is
         when ucl_array =>
            if not key_missing (name) then
               TIO.Put_Line (WARN_EXTRA_KEY & " (" & name & ")");
            end if;
            wrap_payload;
            struct_index := tree.last_reference_index;
            tree.store_arrays.Update_Element (Index => struct_index,
                                              Process => append_into_array'Access);
         when ucl_object =>
            if key_missing (name) then
               TIO.Put_Line (ERR_NEEDS_KEY & ":" & ucl_integer'Img);
               return;
            end if;
            wrap_payload;
            tree.store_objects.Update_Element (Index => struct_index,
                                               Process => append_into_uclobj'Access);
         when others => null;
      end case;
   end insert;


   -----------------
   --  insert #4  --
   -----------------
   procedure insert
     (tree : in out UclTree;
      name : String;
      value : CAL.Time)
   is
      procedure wrap_payload;
      procedure append_into_array (Element : in out jar_array.Vector);
      procedure append_into_uclobj (Element : in out jar_ucl_objects.Map);

      struct_index : Natural;
      global_index : Natural;
      dref : DataReference;
      name_us : constant ASU.Unbounded_String := ASU.To_Unbounded_String (name);

      procedure wrap_payload is
      begin
         tree.store_times.Append (value);
         global_index := tree.store_times.Last_Index;
         dref.data_type := ucl_time;
         dref.vector_index := global_index;
      end wrap_payload;

      procedure append_into_array (Element : in out jar_array.Vector) is
      begin
         Element.Append (dref);
      end append_into_array;

      procedure append_into_uclobj (Element : in out jar_ucl_objects.Map) is
      begin
         Element.Insert (name_us, dref);
      end append_into_uclobj;
   begin
      if tree.open_structure.Is_Empty then
         if key_missing (name) then
            TIO.Put_Line (ERR_NEEDS_KEY & ": Time Type");
            return;
         end if;
         wrap_payload;
         tree.tree_stump.Insert (name_us, dref);
         return;
      end if;

      case tree.last_open_structure is
         when ucl_array =>
            if not key_missing (name) then
               TIO.Put_Line (WARN_EXTRA_KEY & " (" & name & ")");
            end if;
            wrap_payload;
            struct_index := tree.last_reference_index;
            tree.store_arrays.Update_Element (Index => struct_index,
                                              Process => append_into_array'Access);
         when ucl_object =>
            if key_missing (name) then
               TIO.Put_Line (ERR_NEEDS_KEY & ":" & ucl_integer'Img);
               return;
            end if;
            wrap_payload;
            tree.store_objects.Update_Element (Index => struct_index,
                                               Process => append_into_uclobj'Access);
         when others => null;
      end case;
   end insert;


   -----------------
   --  insert #5  --
   -----------------
   procedure insert
     (tree : in out UclTree;
      name : String;
      value : String)
    is
      procedure wrap_payload;
      procedure append_into_array (Element : in out jar_array.Vector);
      procedure append_into_uclobj (Element : in out jar_ucl_objects.Map);

      struct_index : Natural;
      global_index : Natural;
      dref : DataReference;
      name_us : constant ASU.Unbounded_String := ASU.To_Unbounded_String (name);

      procedure wrap_payload
      is
         tray : DataString;
      begin
         tray.payload := ASU.To_Unbounded_String (value);
         tree.store_strings.Append (tray);
         global_index := tree.store_strings.Last_Index;
         dref.data_type := ucl_string;
         dref.vector_index := global_index;
      end wrap_payload;

      procedure append_into_array (Element : in out jar_array.Vector) is
      begin
         Element.Append (dref);
      end append_into_array;

      procedure append_into_uclobj (Element : in out jar_ucl_objects.Map) is
      begin
         Element.Insert (name_us, dref);
      end append_into_uclobj;
   begin
      if tree.open_structure.Is_Empty then
         if key_missing (name) then
            TIO.Put_Line (ERR_NEEDS_KEY & ":" & value);
            return;
         end if;
         wrap_payload;
         tree.tree_stump.Insert (name_us, dref);
         return;
      end if;

      case tree.last_open_structure is
         when ucl_array =>
            if not key_missing (name) then
               TIO.Put_Line (WARN_EXTRA_KEY & " (" & name & ")");
            end if;
            wrap_payload;
            struct_index := tree.last_reference_index;
            tree.store_arrays.Update_Element (Index => struct_index,
                                              Process => append_into_array'Access);
         when ucl_object =>
            if key_missing (name) then
               TIO.Put_Line (ERR_NEEDS_KEY & ":" & ucl_integer'Img);
               return;
            end if;
            wrap_payload;
            tree.store_objects.Update_Element (Index => struct_index,
                                               Process => append_into_uclobj'Access);
         when others => null;
      end case;
   end insert;


   -------------------
   --  start_array  --
   -------------------
   procedure start_array
     (tree : in out UclTree;
      name : String)
   is
      procedure wrap_payload;
      procedure append_into_array (Element : in out jar_array.Vector);
      procedure append_into_uclobj (Element : in out jar_ucl_objects.Map);

      struct_index : Natural;
      global_index : Natural;
      dref : DataReference;
      name_us : constant ASU.Unbounded_String := ASU.To_Unbounded_String (name);

      procedure wrap_payload
      is
         spanking_array : jar_array.Vector;
      begin
         tree.store_arrays.append (spanking_array);
         global_index := tree.store_arrays.Last_Index;

         dref.data_type := ucl_array;
         dref.vector_index := global_index;

         tree.open_structure.append (dref);
      end wrap_payload;

      procedure append_into_array (Element : in out jar_array.Vector) is
      begin
         Element.Append (dref);
      end append_into_array;

      procedure append_into_uclobj (Element : in out jar_ucl_objects.Map) is
      begin
         Element.Insert (name_us, dref);
      end append_into_uclobj;
   begin
       if tree.open_structure.Is_Empty then
         if key_missing (name) then
            TIO.Put_Line (ERR_NEEDS_KEY & ": new array");
            return;
         end if;

         wrap_payload;
         tree.tree_stump.Insert (name_us, dref);
         return;
       end if;

      case tree.last_open_structure is
         when ucl_array =>
            if not key_missing (name) then
               TIO.Put_Line (WARN_EXTRA_KEY & " (" & name & ")");
            end if;
            wrap_payload;
            struct_index := tree.last_reference_index;
            tree.store_arrays.Update_Element (Index => struct_index,
                                              Process => append_into_array'Access);
         when ucl_object =>
            if key_missing (name) then
               TIO.Put_Line (ERR_NEEDS_KEY & ":" & ucl_integer'Img);
               return;
            end if;
            wrap_payload;
            tree.store_objects.Update_Element (Index => struct_index,
                                               Process => append_into_uclobj'Access);
         when others => null;
      end case;
   end start_array;


   -------------------
   --  close_array  --
   -------------------
   procedure close_array (tree : in out UclTree)
   is
      ERR_NO_ARRAY_OPEN : constant String := "Error: directive to close array when none are open.";
      ERR_MISMATCH      : constant String := "Error: closure mismatch (array close but object open";
   begin
      if tree.open_structure.Is_Empty then
         TIO.Put_Line (ERR_NO_ARRAY_OPEN);
         return;
      end if;
      case tree.last_open_structure is
         when ucl_array =>
            tree.open_structure.Delete_Last;
         when ucl_object =>
            TIO.Put_Line (ERR_MISMATCH);
         when others =>
            null;
      end case;
   end close_array;


   --------------------
   --  start_object  --
   --------------------
   procedure start_object
     (tree : in out UclTree;
      name : String)
   is
      procedure wrap_payload;
      procedure append_into_array (Element : in out jar_array.Vector);
      procedure append_into_uclobj (Element : in out jar_ucl_objects.Map);

      struct_index : Natural;
      global_index : Natural;
      dref : DataReference;
      name_us : constant ASU.Unbounded_String := ASU.To_Unbounded_String (name);

      procedure wrap_payload
      is
         spanking_map : jar_ucl_objects.Map;
      begin
         tree.store_objects.Append (spanking_map);
         global_index := tree.store_objects.Last_Index;

         dref.data_type := ucl_object;
         dref.vector_index := global_index;

         tree.open_structure.append (dref);
      end wrap_payload;

      procedure append_into_array (Element : in out jar_array.Vector) is
      begin
         Element.Append (dref);
      end append_into_array;

      procedure append_into_uclobj (Element : in out jar_ucl_objects.Map) is
      begin
         Element.Insert (name_us, dref);
      end append_into_uclobj;
   begin
       if tree.open_structure.Is_Empty then
         if key_missing (name) then
            TIO.Put_Line (ERR_NEEDS_KEY & ": new object map");
            return;
         end if;

         wrap_payload;
         tree.tree_stump.Insert (name_us, dref);
         return;
       end if;

      case tree.last_open_structure is
         when ucl_array =>
            if not key_missing (name) then
               TIO.Put_Line (WARN_EXTRA_KEY & " (" & name & ")");
            end if;
            wrap_payload;
            struct_index := tree.last_reference_index;
            tree.store_arrays.Update_Element (Index => struct_index,
                                              Process => append_into_array'Access);
         when ucl_object =>
            if key_missing (name) then
               TIO.Put_Line (ERR_NEEDS_KEY & ":" & ucl_integer'Img);
               return;
            end if;
            wrap_payload;
            tree.store_objects.Update_Element (Index => struct_index,
                                               Process => append_into_uclobj'Access);
         when others => null;
      end case;
   end start_object;


   --------------------
   --  close_object  --
   --------------------
   procedure close_object
     (tree : in out UclTree)
   is
      ERR_NO_OBJ_OPEN : constant String := "Error: directive to close object when none are open.";
      ERR_MISMATCH    : constant String := "Error: closure mismatch (object close but array open";
   begin
      if tree.open_structure.Is_Empty then
         TIO.Put_Line (ERR_NO_OBJ_OPEN);
         return;
      end if;
      case tree.last_open_structure is
         when ucl_object =>
            tree.open_structure.Delete_Last;
         when ucl_array =>
            TIO.Put_Line (ERR_MISMATCH);
         when others =>
            null;
      end case;
   end close_object;

end ThickUCL;
