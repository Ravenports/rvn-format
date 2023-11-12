--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

private with libucl;

package ThickUCL.Files is

   ucl_file_unparseable : exception;

   --  Opens UCL file for reading and attempts to parse the contents.
   --  If it fails, the ucl_file_unparseable exception is thrown
   --  Otherwise the UclTree
   procedure parse_ucl_file
     (tree : in out UclTree;
      path : String);

private

   procedure populate_the_tree
     (tree : in out UclTree;
      rootobj : access constant libucl.ucl_object_t);

   procedure populate_array
     (tree : in out UclTree;
      arrayobj :  access constant libucl.ucl_object_t);

   function extract_key
     (item : access constant libucl.ucl_object_t) return String;

end ThickUCL.Files;
