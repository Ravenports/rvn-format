--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Ada.Calendar;
with ucl;

private with Ada.Strings.Unbounded;
private with Ada.Containers.Vectors;
private with Ada.Containers.Hashed_Maps;

package ThickUCL is

   package CAL renames Ada.Calendar;

   type UclTree is tagged private;

   type Leaf_type is
     (data_not_present,
      data_object,
      data_array,
      data_integer,
      data_float,
      data_string,
      data_boolean,
      data_time);

   subtype array_index  is Natural range Natural'First .. 499_999;
   subtype object_index is Natural range Natural'First .. 499_999;

   -----------------------------------------------------------
   --  Methods to build top level UCL Object (serial only)  --
   -----------------------------------------------------------

   procedure insert
     (tree : in out UclTree;
      name : String;
      value : ucl.ucl_integer);

   procedure insert
     (tree : in out UclTree;
      name : String;
      value : Float);

   procedure insert
     (tree : in out UclTree;
      name : String;
      value : Boolean);

   procedure insert
     (tree : in out UclTree;
      name : String;
      value : CAL.Time);

   procedure insert
     (tree : in out UclTree;
      name : String;
      value : String);

   procedure start_array
     (tree : in out UclTree;
      name : String);

   procedure close_array
     (tree : in out UclTree);

   procedure start_object
     (tree : in out UclTree;
      name : String);

   procedure close_object
     (tree : in out UclTree);


   ---------------------------------------------
   --  Methods to query top level UCL object  --
   ---------------------------------------------

   ucl_type_mismatch  : exception;
   ucl_key_not_found  : exception;
   index_out_of_range : exception;

   --  Stump-level fields
   function get_data_type
     (tree : UclTree;
      key  : String) return Leaf_type;

   --  Get stump-level integer values from key (possible exception)
   function get_base_value
     (tree : UclTree;
      key  : String) return Ucl.ucl_integer;

   --  Get stump-level float values from key (possible exception)
   function get_base_value
     (tree : UclTree;
      key  : String) return Float;

   --  Get stump-level boolean values from key (possible exception)
   function get_base_value
     (tree : UclTree;
      key  : String) return Boolean;

   --  Get stump-level time values from key (possible exception)
   function get_base_value
     (tree : UclTree;
      key  : String) return CAL.Time;

   --  Get stump-level string values from key (possible exception)
   function get_base_value
     (tree : UclTree;
      key  : String) return String;

   --  Get index to a stump-level array (possible exceptions)
   function get_index_second_level_array
     (tree  : UclTree;
      key   : String) return array_index;

   --  Get number of elements in the array (possible exceptions)
   function get_number_of_array_elements
     (tree : UclTree;
      vndx : array_index) return Natural;

   --  Get data-type of stump-level array element #index (zero-indexed) (possible exceptions)
   function get_array_element_type
     (tree  : UclTree;
      vndx  : array_index;
      index : Natural) return Leaf_type;

   --  Get stump-level array element #index integer value (zero-indexed) (possible exceptions)
   function get_array_element_value
     (tree  : UclTree;
      vndx  : array_index;
      index : Natural) return Ucl.ucl_integer;

   --  Get stump-level array element #index float value (zero-indexed) (possible exceptions)
   function get_array_element_value
     (tree  : UclTree;
      vndx  : array_index;
      index : Natural) return Float;

   --  Get stump-level array element #index boolean value (zero-indexed) (possible exceptions)
   function get_array_element_value
     (tree  : UclTree;
      vndx  : array_index;
      index : Natural) return Boolean;

   --  Get stump-level array element #index time value (zero-indexed) (possible exceptions)
   function get_array_element_value
     (tree  : UclTree;
      vndx  : array_index;
      index : Natural) return CAL.Time;

   --  Get stump-level array element #index string value (zero-indexed) (possible exceptions)
   function get_array_element_value
     (tree  : UclTree;
      vndx  : array_index;
      index : Natural) return String;

   --  Get index to a stump-level ucl object (possible exceptions)
   function get_index_second_level_ucl_object
     (tree  : UclTree;
      key   : String) return object_index;



private

   package ASU renames Ada.Strings.Unbounded;
   package CON renames Ada.Containers;

   use type Ucl.ucl_integer;
   use type CAL.Time;

   type DataType is
     (ucl_object,
      ucl_array,
      ucl_integer,
      ucl_float,
      ucl_string,
      ucl_boolean,
      ucl_time);

   package jar_integer is new CON.Vectors
     (Index_Type   => Natural,
      Element_Type => ucl.ucl_integer);

   package jar_float is new CON.Vectors
     (Index_Type   => Natural,
      Element_Type => Float);

   package jar_boolean is new CON.Vectors
     (Index_Type   => Natural,
      Element_Type => Boolean);

   package jar_time is new CON.Vectors
     (Index_Type   => Natural,
      Element_Type => CAL.Time);

   type DataString is
      record
         payload : ASU.Unbounded_String;
      end record;

   package jar_string is new CON.Vectors
     (Index_Type   => Natural,
      Element_Type => DataString);

   type DataReference is tagged
      record
         data_type    : DataType;
         vector_index : Natural;
      end record;

   package jar_array is new CON.Vectors
     (Index_Type   => Natural,
      Element_Type => DataReference);

   function map_hash (key : ASU.Unbounded_String) return CON.Hash_Type;
   function equivalent (A, B : ASU.Unbounded_String) return Boolean;

   package jar_ucl_objects is new CON.Hashed_Maps
     (Key_Type => ASU.Unbounded_String,
      Element_Type => DataReference,
      Hash => map_hash,
      Equivalent_Keys => equivalent);

   package box_of_array_jars is new CON.Vectors
     (Index_Type   => Natural,
      Element_Type => jar_array.Vector,
      "=" => jar_array."=");

   package box_of_ucl_object_jars is new CON.Vectors
     (Index_Type   => Natural,
      Element_Type => jar_ucl_objects.Map,
      "=" => jar_ucl_objects."=");


   type UclTree is tagged
      record
         store_integers : jar_integer.Vector;
         store_floats   : jar_float.Vector;
         store_booleans : jar_boolean.Vector;
         store_times    : jar_time.Vector;
         store_strings  : jar_string.Vector;
         store_arrays   : box_of_array_jars.Vector;
         store_objects  : box_of_ucl_object_jars.Vector;
         open_structure : jar_array.Vector;

         tree_stump     : jar_ucl_objects.Map;
      end record;

   --  Returns true if name is an empty string
   function key_missing (name : String) return Boolean;

   --  Helper to get the currently open structure type (array or object)
   function last_open_structure (tree : UclTree) return DataType;

   --  Helper to get index where the last open structure is stored
   function last_reference_index (tree : UclTree) return Natural;


   ERR_NEEDS_KEY  : constant String := "Error: key required but is missing.  Item skipped.";
   WARN_EXTRA_KEY : constant String := "Warning: key was found but not expected, ignoring.";

end ThickUCL;
