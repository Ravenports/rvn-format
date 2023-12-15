--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

package body Archive.Dirent is


   -------------------
   --  simple_name  --
   -------------------
   function simple_name (item : Directory_Entity) return String is
   begin
      return ASU.To_String (item.filename);
   end simple_name;


   -----------------
   --  full_path  --
   -----------------
   function full_path (item : Directory_Entity) return String is
   begin
      return ASU.To_String (item.directory) & "/" & ASU.To_String (item.filename);
   end full_path;


   ---------------------
   --  create_entity  --
   ---------------------
   function create_entity
     (directory : String;
      filename  : String) return Directory_Entity
   is
      result : Directory_Entity;
   begin
      result.directory := ASU.To_Unbounded_String (directory);
      result.filename  := ASU.To_Unbounded_String (filename);
      return result;
   end create_entity;


   ---------
   --  <  --
   ---------
   function "<" (left, right : Directory_Entity) return Boolean is
   begin
      return ASU."<" (left.filename, right.filename);
   end "<";

end Archive.Dirent;
