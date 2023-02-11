package body Archive.Unix is

   ------------------------------------------------------------------------------------------
   --  success
   ------------------------------------------------------------------------------------------
   function success (rc : IC.int) return Boolean
   is
      use type IC.int;
   begin
      return (rc = IC.int (0));
   end success;


   ------------------------------------------------------------------------------------------
   --  failure
   ------------------------------------------------------------------------------------------
   function failure (rc : IC.int) return Boolean
   is
      use type IC.int;
   begin
      return (rc = IC.int (1));
   end failure;


   ------------------------------------------------------------------------------------------
   --  stat_ok
   ------------------------------------------------------------------------------------------
   function stat_ok (path : String; sb : struct_stat_Access) return Boolean
   is
      c_path : IC.Strings.chars_ptr;
      res    : IC.int;
   begin
      c_path := IC.Strings.New_String (path);
      res := arc_stat (c_path, sb);
      IC.Strings.Free (c_path);
      return success (res);
   end stat_ok;


   ------------------------------------------------------------------------------------------
   --  get_charactistics
   ------------------------------------------------------------------------------------------
   function get_charactistics (path : String) return File_Characteristics
   is
      result : File_Characteristics;
      sb     : aliased Unix.struct_stat;
   begin
      result.owner := str2owngrp ("xxxx");
      result.group := str2owngrp ("wheel");
      result.mtime := 0;
      result.perms := 0;
      begin
         if Unix.stat_ok (path, sb'Unchecked_Access) then
            result.owner := file_owner (sb'Unchecked_Access);
            result.group := file_group (sb'Unchecked_Access);
            result.mtime := file_modification_time (sb'Unchecked_Access);
            result.perms := file_permissions (sb'Unchecked_Access);
            result.error := False;
         end if;
      exception
         when others =>
            result.error := True;
      end;
      return result;
   end get_charactistics;


   ------------------------------------------------------------------------------------------
   --  get_mtime
   ------------------------------------------------------------------------------------------
   function file_modification_time (sb : struct_stat_Access) return filetime
   is
      res : IC.long;
   begin
      res := arc_get_mtime (sb);
      return filetime (res);
   end file_modification_time;


   ------------------------------------------------------------------------------------------
   --  file_permissions
   ------------------------------------------------------------------------------------------
   function file_permissions (sb : struct_stat_Access) return bits_16
   is
      res : IC.short;
   begin
      res := arc_extract_permissions (sb);
      return bits_16 (res);
   end file_permissions;


   ------------------------------------------------------------------------------------------
   --  file_owner
   ------------------------------------------------------------------------------------------
   function file_owner (sb : struct_stat_Access) return ownergroup
   is
      use type IC.Strings.chars_ptr;

      c_owner : IC.Strings.chars_ptr;
   begin
      c_owner := arc_get_owner (sb);
      if c_owner = IC.Strings.Null_Ptr then
         declare
            id : constant IC.unsigned := arc_get_owner_id (sb);
         begin
            return str2owngrp (int2str (Integer (id)));
         end;
      end if;
      declare
         owner : constant String := IC.Strings.Value (c_owner);
      begin
         return str2owngrp (owner);
      end;
   end file_owner;


   ------------------------------------------------------------------------------------------
   --  file_group
   ------------------------------------------------------------------------------------------
   function file_group (sb : struct_stat_Access) return ownergroup
   is
      use type IC.Strings.chars_ptr;

      c_group : IC.Strings.chars_ptr;
   begin
      c_group := arc_get_group (sb);
      if c_group = IC.Strings.Null_Ptr then
         declare
            id : constant IC.unsigned := arc_get_group_id (sb);
         begin
            return str2owngrp (int2str (Integer (id)));
         end;
      end if;
      declare
         group : constant String := IC.Strings.Value (c_group);
      begin
         return str2owngrp (group);
      end;
   end file_group;


   ------------------------------------------------------------------------------------------
   --  str2owngrp
   ------------------------------------------------------------------------------------------
   function str2owngrp (name : String) return ownergroup
   is
      result  : ownergroup := (others => Character'Val (0));
   begin
      if name'Length > ownergroup'Length then
         result := name (name'First .. name'First + ownergroup'Length - 1);
      else
         result (result'First .. result'First + name'Length - 1) := name;
      end if;
      return result;
   end str2owngrp;


   ------------------------------------------------------------------------------------------
   --  int2str
   ------------------------------------------------------------------------------------------
   function int2str (A : Integer) return String
   is
      raw : constant String := A'Img;
   begin
      if A < 0 then
         return raw;
      end if;
      return raw (raw'First + 1 .. raw'Last);
   end int2str;

end Archive.Unix;
