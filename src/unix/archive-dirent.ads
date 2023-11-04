--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

private with Ada.Strings.Unbounded;

package Archive.Dirent is

   type Directory_Entity is tagged private;

   --  Returns the filename of the directory entity (up to 255 characters)
   function simple_name (item: Directory_Entity) return String;

   --  Returns the absolute path of the directory entity
   function full_path (item : Directory_Entity) return String;

   --  Returns a new Directory_Entity object
   function create_entity
     (directory : String;
      filename  : String) return Directory_Entity;

   function "<" (left, right : Directory_Entity) return Boolean;

private

   package ASU renames Ada.Strings.Unbounded;

   type Directory_Entity is tagged
      record
         filename  : ASU.Unbounded_String;
         directory : ASU.Unbounded_String;
      end record;


end Archive.Dirent;
