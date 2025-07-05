--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Ada.Strings.Fixed;
with Ada.Environment_Variables;
with Archive.Unix;
with Elf;

package body Archive.Misc is

   package AS  renames Ada.Strings;
   package ENV renames Ada.Environment_Variables;

   ------------------
   --  count_char  --
   ------------------
   function count_char (S : String; focus : Character) return Natural
   is
      result : Natural := 0;
   begin
      for x in S'Range loop
         if S (x) = focus then
            result := result + 1;
         end if;
      end loop;
      return result;
   end count_char;


   ----------------------
   --  specific_field  --
   ----------------------
   function specific_field
     (S            : String;
      field_number : Positive;
      delimiter    : String := " ") return String
   is
      back  : Integer;
      dsize : constant Natural := delimiter'Length;
      front : Integer := S'First;
   begin
      for field in 1 .. field_number - 1 loop
         back := AS.Fixed.Index (Source => S, Pattern => delimiter, From => front);
         if back <= 0 then
            return "";
         end if;
         front := back + dsize;
      end loop;
      back := AS.Fixed.Index (Source => S, Pattern => delimiter, From => front);
      if back > 0 then
         return S (front .. back - 1);
      else
         return S (front .. S'Last);
      end if;
   end specific_field;


   -------------------------
   --  replace_substring  --
   -------------------------
   function replace_substring
     (US : ASU.Unbounded_String;
      old_string : String;
      new_string : String) return ASU.Unbounded_String
   is
      back_marker  : constant Natural := ASU.Index (Source => US, Pattern => old_string);
      front_marker : constant Natural := back_marker + old_string'Length - 1;
   begin
      if back_marker = 0 then
         return US;
      end if;
      return ASU.Replace_Slice (Source => US,
                                Low    => back_marker,
                                High   => front_marker,
                                By     => new_string);
   end replace_substring;


   ------------
   --  tail  --
   ------------
   function tail (S : String; delimiter : String) return String
   is
      dl_size      : constant Natural := delimiter'Length;
      back_marker  : constant Natural := S'First;
      front_marker : Natural := S'Last - dl_size + 1;
   begin
      loop
         if front_marker < back_marker then
            --  delimiter never found
            return S;
         end if;
         if S (front_marker .. front_marker + dl_size - 1) = delimiter then
            return S (front_marker + dl_size .. S'Last);
         end if;
         front_marker := front_marker - 1;
      end loop;
   end tail;


   ------------
   --  head  --
   ------------
   function head (S : String; delimiter : String) return String
   is
      dl_size      : constant Natural := delimiter'Length;
      back_marker  : constant Natural := S'First;
      front_marker : Natural := S'Last - dl_size + 1;
   begin
      loop
         if front_marker < back_marker then
            --  delimiter never found
            return "";
         end if;
         if S (front_marker .. front_marker + dl_size - 1) = delimiter then
            return S (back_marker .. front_marker - 1);
         end if;
         front_marker := front_marker - 1;
      end loop;
   end head;


   ---------------
   --  int2str  --
   ---------------
   function int2str (A : Integer) return String
   is
      raw : constant String := A'Img;
   begin
      if A < 0 then
         return raw;
      else
         return raw (raw'First + 1 .. raw'Last);
      end if;
   end int2str;


   -----------------------
   --  get_interpreter  --
   -----------------------
   function get_interpreter return String
   is
      standard_interpreter : constant String := "/bin/sh";
   begin
      case platform is
         when generic_unix |
              midnightbsd  |
              dragonfly    |
              freebsd      |
              openbsd      |
              netbsd       => return standard_interpreter;
         when linux        => return standard_interpreter;
         when omnios       |
              solaris      => return "/usr/xpg4/bin/sh";
      end case;
   end get_interpreter;


   -------------------------------------
   --  select_abi_determination_file  --
   -------------------------------------
   function select_abi_determination_file return String
   is
      env_name : constant String := "ABI_FILE";
      hostname : constant String := "/bin/hostname";
      sol_libc : constant String := "/lib/64/libc.so.1";
   begin
      if ENV.Exists (env_name) then
         if Unix.file_exists (ENV.Value (env_name)) then
            return ENV.Value (env_name);
         end if;
      end if;

      case platform is
         when generic_unix |
              midnightbsd  |
              dragonfly    |
              freebsd      |
              openbsd      |
              netbsd       |
              linux        =>
            if Unix.file_exists (hostname) then
               return hostname;
            end if;
         when solaris      |
              omnios       =>
            if Unix.file_exists (sol_libc) then
               return sol_libc;
            end if;
      end case;
      --  top choice doesn't exist, fall back to interpreter program
      return get_interpreter;
   end select_abi_determination_file;


   --------------------------
   --  convert_doubleword  --
   --------------------------
   function convert_doubleword (big_endian : Boolean; dw : doubleword) return Natural
   is
      A_byte : constant Natural := Character'Pos (dw (dw'First));
      B_byte : constant Natural := Character'Pos (dw (dw'First + 1));
      C_byte : constant Natural := Character'Pos (dw (dw'First + 2));
      D_byte : constant Natural := Character'Pos (dw (dw'First + 3));
      result : Natural;
   begin
      if big_endian then
         result := D_byte + (C_byte * 256) + (B_byte * 65_536) + (A_byte * 16_777_216);
      else
         result := A_byte + (B_byte * 256) + (C_byte * 65_536) + (D_byte * 16_777_216);
      end if;
      return result;
   exception
      when Constraint_Error =>
         return 16#7FFF_FFFF#;
   end convert_doubleword;


   ---------------------
   --  determine_abi  --
   ---------------------
   function determine_abi return String
   is
      abi_file : constant String := select_abi_determination_file;
      elf_data : Elf.ELF_File;

      function get_arch return String is
      begin
         case elf_data.machine is
            when Elf.x86       => return "x86";
            when Elf.x86_64    => return "x86_64";
            when Elf.arm       => return "arm";
            when Elf.aarch64   => return "arm64";
            when Elf.powerpc   => return "powerpc";
            when Elf.powerpc64 => return "powerpc64";
            when Elf.no_instruction     => return "no_instruction";
            when Elf.some_random_system => return "unsupported_arch";
         end case;
      end get_arch;

      --  Rather than determine OSNAME from the elf note, assume the ABI file is
      --  from the operating system defined at build time.  That's a great assumption.
      function get_osname return String is
      begin
         case platform is
            when freebsd      => return "freebsd";
            when dragonfly    => return "dragonfly";
            when netbsd       => return "netbsd";
            when openbsd      => return "openbsd";
            when solaris      => return "solaris";
            when omnios       => return "sunos";
            when midnightbsd  => return "midnightbsd";
            when generic_unix => return "unix";
            when linux        => return "linux";
         end case;
      end get_osname;

      function get_release return String
      is
         release : Elf.ASU.Unbounded_String := Elf.ASU.Null_Unbounded_String;

         procedure process_note (Position : Elf.note_crate.Cursor)
         is
            note : Elf.ELF_Note renames Elf.note_crate.Element (Position);
            is_big_endian : Boolean;
         begin
            if Elf.ASU.Length (release) > 0 then
               return;
            end if;
            case elf_data.dialect is
               when Elf.big_endian    => is_big_endian := True;
               when Elf.little_endian => is_big_endian := False;
            end case;
            if note.note_type = Elf.NT_GNU_ABI_TAG and then
              Elf.ASU.To_String (note.name) = "GNU"
            then
               --  NT_GNU_ABI_TAG
               --  Operating system (OS) ABI information.  The desc field contains 4 words:
               --  word 0: OS descriptor (ELF_NOTE_OS_LINUX, ELF_NOTE_OS_GNU, etc)
               --  word 1: major version of the ABI
               --  word 2: minor version of the ABI
               --  word 3: subminor version of the ABI
               declare
                  major : Natural;
                  minor : Natural;
                  point : Natural;
                  description : constant String := Elf.ASU.To_String (note.description);
                  F : constant Natural := description'First;
               begin
                  if description'Length < 16 then
                     release := Elf.ASU.To_Unbounded_String ("0.0.0");
                     return;
                  end if;
                  major := convert_doubleword (is_big_endian, description (F +  4 .. F +  7));
                  minor := convert_doubleword (is_big_endian, description (F +  8 .. F + 11));
                  point := convert_doubleword (is_big_endian, description (F + 12 .. F + 15));
                  release := Elf.ASU.To_Unbounded_String
                    (int2str (major) & "." & int2str (minor) & "." & int2str (point));
                  return;
               end;
            end if;
            if note.note_type = Elf.NT_VERSION then
               declare
                  name_str : constant String := Elf.ASU.To_String (note.name);
                  ver_str  : constant String := Elf.ASU.To_String (note.description);
                  version  : Natural;
                  major    : Natural;
                  minor    : Natural;
               begin
                  if ver_str'Length = 4 then
                     if name_str = "DragonFly" or else
                       name_str = "FreeBSD" or else
                       name_str = "NetBSD" or else
                       name_str = "OpenBSD" or else
                       name_str = "MidnightBSD"
                     then
                        version := convert_doubleword (is_big_endian, ver_str);
                        case platform is
                           when netbsd =>
                              major := (version + 1_000_000) / 100_000_000;
                              release := Elf.ASU.To_Unbounded_String (int2str (major));
                              return;
                           when freebsd | midnightbsd =>
                              major := version / 100_000;
                              release := Elf.ASU.To_Unbounded_String (int2str (major));
                              return;
                           when dragonfly =>
                              major := version / 100_000;
                              minor := ((((version / 100) mod 1000) + 1) / 2) * 2;
                              release := Elf.ASU.To_Unbounded_String (int2str (major) & '.' &
                                                                        int2str (minor));
                              return;
                           when openbsd =>
                              --  as of OpenBSD 7.4, it has an NT_VERSION tag with name "OpenBSD"
                              --  but the description is "0000" (no version info"
                              --  We will not get here because OpenBSD is handled earlier below
                              release := Elf.ASU.To_Unbounded_String ("0");
                           when omnios | solaris | generic_unix | linux =>
                              --  we should not get here
                              null;
                        end case;
                     end if;
                  end if;
               end;
            end if;
         end process_note;
      begin
         elf_data.notes.Iterate (process_note'Access);
         if Elf.ASU.Length (release) = 0 then
            return "0";
         end if;
         return Elf.ASU.To_String (release);
      end get_release;
   begin
      Elf.readelf (abi_file, elf_data);
      case elf_data.file_type is
         when Elf.not_elf =>
            return get_osname & ":*:0";
         when others => null;
      end case;

      if elf_data.notes.Is_Empty then
         case platform is
            when solaris =>
               --  solaris 10 does not support notes
               return get_osname & ":" & get_arch & ":10";
            when omnios =>
               --  OmniOS (SunOS 5.11) supports notes, but the system files did not use them.
               return get_osname & ":" & get_arch & "5.11";
            when openbsd =>
               --  OpenBSD 7.4 has NT_VERSION tag, but it's set to 0.
               --  Hardcode for now (OpenBSD isn't supported by Ravenports yet).
               --  Determine an ironclad way to get built version later.
               return get_osname & ":" & get_arch & ":7.4";
            when others =>
               return get_osname & ":" & get_arch & ":0";
         end case;
      end if;

      return get_osname & ":" & get_arch & ":" & get_release;
   end determine_abi;


   -----------------
   --  join_path  --
   -----------------
   function join_path (first_part : String; second_part : String) return String is
   begin
      if first_part = "/" or else first_part = "" then
         return "/" & second_part;
      end if;
      return first_part & "/" & second_part;
   end join_path;


   --------------------
   --  new_filename  --
   --------------------
   function new_filename (msg_outfile : String; tftype : temp_file_type) return String
   is
      new_file : String := msg_outfile;
      start_index : constant Natural := Ada.Strings.Fixed.Index (msg_outfile, "outmsg");
   begin
      case tftype is
         when ft_outmsg => null;
         when ft_script => new_file (start_index .. start_index + 5) := "script";
         when ft_stdout => new_file (start_index .. start_index + 5) := "stdout";
         when ft_lua    => new_file (start_index .. start_index + 5) := "lualua";
         when ft_internal => new_file (start_index .. start_index + 5) := "intern";
      end case;
      return new_file;
   end new_filename;

end Archive.Misc;
