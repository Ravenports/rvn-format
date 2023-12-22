--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Ada.Text_IO;
with Ada.Directories;
with Interfaces;

package body Elf is

   package DIR renames Ada.Directories;
   package TIO renames Ada.Text_IO;


   -----------------------------
   --  print_elf_information  --
   -----------------------------
   procedure print_elf_information (file_path : String)
   is
      fdata : ELF_File;
      BF : constant String := " (bytes into file)";
      B  : constant String := " (bytes)";

      procedure print_note (Position : note_crate.Cursor)
      is
         mytype : constant String  := note_crate.Element (Position).note_type'Img;
         myname : constant String  := ASU.To_String (note_crate.Element (Position).name);
         mydesc : constant String  := ASU.To_String (note_crate.Element (Position).description);
         tray   : String (1 .. 11)  := (others => ' ');
         tray2  : String (1 .. 20) := (others => ' ');
         tray3  : String (1 .. mydesc'Length * 2) := (others => ' ');
         index  : Positive := 1;
      begin
         for x in mydesc'Range loop
            tray3 (index .. index + 1) := char2hex (mydesc (x));
            index := index + 2;
         end loop;
         tray (tray'Last + 1 - mytype'Length .. tray'Last) := mytype;
         if myname'Length > 30 then
            TIO.Put_Line (tray & " " & myname & " " & tray3);
         else
            tray2 (1 .. myname'Length) := myname;
            TIO.Put_Line (tray & " " & tray2 & " " & tray3);
         end if;
      end print_note;

      procedure print_library (Position : library_crate.Cursor) is
      begin
         TIO.Put_Line (ASU.To_String (library_crate.Element (Position)));
      end print_library;
   begin
      readelf (file_path, fdata);
      case fdata.file_type is
         when not_elf =>
            TIO.Put_Line ("readelf: Error: Not an ELF file");
            return;
         when others => null;
      end case;

      TIO.Put_Line ("ELF Header:");
      TIO.Put_Line ("  Class:                     " & fdata.format'Img);
      TIO.Put_Line ("  Data:                      " & fdata.dialect'Img);
      TIO.Put_Line ("  Version:                  " &  fdata.version'Img);
      TIO.Put_Line ("  OS/ABI:                    " & fdata.abi'Img);
      TIO.Put_Line ("  Type:                      " & fdata.file_type'Img);
      TIO.Put_Line ("  Machine:                   " & fdata.machine'Img);
      TIO.Put_Line ("  Start of program headers: " & fdata.program_header_offset'Img & BF);
      TIO.Put_Line ("  Start of section headers: " & fdata.section_header_offset'Img & BF);
      TIO.Put_Line ("  Size of this header:      " & fdata.header_size'Img & B);
      TIO.Put_Line ("  Size of program headers:  " & fdata.PHT_entsize'Img & B);
      TIO.Put_Line ("  Number of program headers:" & fdata.PHT_nument'Img);
      TIO.Put_Line ("  Size of section headers:  " & fdata.SHT_entsize'Img & B);
      TIO.Put_Line ("  Number of section headers:" & fdata.SHT_nument'Img);
      TIO.Put_Line ("");
      TIO.Put_Line ("NOTES");
      TIO.Put_Line ("-------------------------------------------------------");
      fdata.notes.Iterate (print_note'Access);
      TIO.Put_Line ("");
      if ASU.Length (fdata.soname) > 0 then
         TIO.Put_Line ("SONAME:       " & ASU.To_String (fdata.soname));
      end if;
      if ASU.Length (fdata.run_path) > 0 then
         TIO.Put_Line ("RUNPATH:      " & ASU.To_String (fdata.run_path));
      end if;
      TIO.Put_Line ("");
      TIO.Put_Line ("LIBRARIES NEEDED");
      TIO.Put_Line ("-------------------------------------------------------");
      fdata.libs_needed.Iterate (print_library'Access);

   end print_elf_information;


   ---------------
   --  readelf  --
   ---------------
   procedure readelf (file_path : String; file_data : in out ELF_File)
   is
      use type DIR.File_Size;

      elf_handle : SIO.File_Type;
      elf_stmaxs : SIO.Stream_Access;
      header     : ELF_Header;
      valid      : Boolean := True;
   begin
      if not DIR.Exists (file_path) or else
        DIR.Size (file_path) < 200
      then
         file_data.file_type := not_elf;
         return;
      end if;

      SIO.Open (File => elf_handle,
                Mode => SIO.In_File,
                Name => file_path);

      begin
         elf_stmaxs := SIO.Stream (elf_handle);
         ELF_Header'Read (elf_stmaxs, header);
      exception
         when others =>
            file_data.file_type := not_elf;
            return;
      end;
      if header.magic (header.magic'First + 1 .. header.magic'Last) = "ELF" and then
        Character'Pos (header.magic (header.magic'First)) = 16#7F#
      then

         if header.class = 1 then
            file_data.format := format32;
         elsif header.class = 2 then
            file_data.format := format64;
         else
            valid := False;
         end if;

         if valid then
            if header.data = 2 then
               file_data.dialect := big_endian;
            elsif header.data = 1 then
               file_data.dialect := little_endian;
            else
               valid := False;
            end if;
         end if;

         if valid then
            if header.elf_ver = 1 then
               file_data.version := 1;
            else
               valid := False;
            end if;
         end if;

         if valid then
            case header.osabi is
               when  0 => file_data.abi := system_v;
               when  1 => file_data.abi := hp_ux;
               when  2 => file_data.abi := netbsd;
               when  3 => file_data.abi := linux;
               when  4 => file_data.abi := hurd;
               when  6 => file_data.abi := solaris;
               when  7 => file_data.abi := aix;
               when  8 => file_data.abi := irix;
               when  9 => file_data.abi := freebsd;
               when 10 => file_data.abi := tru64;
               when 11 => file_data.abi := novell_modesto;
               when 12 => file_data.abi := openbsd;
               when 13 => file_data.abi := openvms;
               when 14 => file_data.abi := nonstop_kernel;
               when 15 => file_data.abi := aros;
               when 16 => file_data.abi := fenix;
               when 17 => file_data.abi := nuxi_cloudabi;
               when 18 => file_data.abi := openvos;
               when others => valid := False;
            end case;
         end if;

         if valid then
            declare
               e_type : constant Natural :=
                 value (file_data.dialect, header.block2 (1), header.block2 (2));
               e_machine : constant Natural :=
                 value (file_data.dialect, header.block2 (3), header.block2 (4));
            begin
               case e_type is
                  when 0 => file_data.file_type := unknown_type;
                  when 1 => file_data.file_type := relocatable;
                  when 2 => file_data.file_type := executable;
                  when 3 => file_data.file_type := shared_object;
                  when 4 => file_data.file_type := core_file;
                  when 5 .. 16#FDFF# => valid := False;
                  when 16#FE00# .. 16#FEFF# => file_data.file_type := reserved_os_range;
                  when 16#FF00# .. 16#FFFF# => file_data.file_type := reserved_proc_range;
                  when 16#10000# .. 16#7FFF_FFFF# => valid := False;
               end case;
               case e_machine is
                  when      0 => file_data.machine := no_instruction;
                  when      3 => file_data.machine := x86;
                  when 16#14# => file_data.machine := powerpc;
                  when 16#15# => file_data.machine := powerpc64;
                  when 16#28# => file_data.machine := arm;
                  when 16#3E# => file_data.machine := x86_64;
                  when 16#B7# => file_data.machine := aarch64;
                  when others => file_data.machine := some_random_system;
               end case;
            end;
         end if;

         if valid then
            case file_data.format is
               when format32 =>
                  declare
                     lect   : endianess renames file_data.dialect;
                     myquad : constant elf_quad := elf_quad (header.block2 (13 .. 16));
                     quad02 : constant elf_quad := elf_quad (header.block3 (1 .. 4));
                  begin
                     file_data.program_header_offset := offset_64 (value (lect, myquad));
                     file_data.section_header_offset := offset_64 (value (lect, quad02));
                     file_data.header_size := value (lect, header.block3 (9), header.block3 (10));
                     file_data.PHT_entsize := value (lect, header.block3 (11), header.block3 (12));
                     file_data.PHT_nument  := value (lect, header.block3 (13), header.block3 (14));
                     file_data.SHT_entsize := value (lect, header.block3 (15), header.block3 (16));
                     file_data.SHT_nument  := value (lect, header.block4 (1), header.block4 (2));
                  end;
               when format64 =>
                  declare
                     lect   : endianess renames file_data.dialect;
                     myocto : constant elf_octo := elf_octo (header.block3 (1 .. 8));
                     octo02 : constant elf_octo := elf_octo (header.block3 (9 .. 16));
                  begin
                     file_data.program_header_offset := value (lect, myocto);
                     file_data.section_header_offset := value (lect, octo02);
                     file_data.header_size := value (lect, header.block4 (5), header.block4 (6));
                     file_data.PHT_entsize := value (lect, header.block4 (7), header.block4 (8));
                     file_data.PHT_nument  := value (lect, header.block4 (9), header.block4 (10));
                     file_data.SHT_entsize := value (lect, header.block4 (11), header.block4 (12));
                     file_data.SHT_nument  := value (lect, header.block4 (13), header.block4 (14));
                  end;
            end case;
         end if;

      else
         valid := False;
      end if;

      if not valid then
         file_data.file_type := not_elf;
      end if;

      if valid then
         declare
            xsect : Extracted_Sections;
            xdata : Relevant_Dynamic_Data.Vector;
         begin
            read_sections
              (elf_handle => elf_handle,
               elf_stmaxs => elf_stmaxs,
               file_data  => file_data,
               sections   => xsect);

            store_note_definitions
              (elf_handle => elf_handle,
               elf_stmaxs => elf_stmaxs,
               file_data  => file_data,
               sections   => xsect);

            if xsect.found_dynamic and then
              xsect.found_string_table
            then
               scan_dynamic_data
                 (elf_handle => elf_handle,
                  elf_stmaxs => elf_stmaxs,
                  file_data  => file_data,
                  dynsection => xsect.dynamic,
                  dyn_data   => xdata);

               populate_dynamic_data
                 (elf_handle => elf_handle,
                  elf_stmaxs => elf_stmaxs,
                  file_data  => file_data,
                  strtable   => xsect.strtab,
                  dyn_data   => xdata);
            end if;
         end;
      end if;

      SIO.Close (elf_handle);

   end readelf;


   -------------
   --  value  --
   -------------
   function value (dialect : endianess; byte1, byte2 : elf_byte) return Natural
   is
      result : Natural := 0;
      Hi_byte : constant Natural := Natural (byte1);
      Lo_byte : constant Natural := Natural (byte2);
   begin
      case dialect is
         when little_endian =>
            result := (Lo_byte * 256) + Hi_byte;
         when big_endian =>
            result := (Hi_byte * 256) + Lo_byte;
      end case;
      return result;
   end value;


   -------------
   --  value  --
   -------------
   function value (dialect : endianess; quad : elf_quad) return Natural
   is
      result : Natural := 0;
      A_byte : constant Natural := Natural (quad (quad'First));
      B_byte : constant Natural := Natural (quad (quad'First + 1));
      C_byte : constant Natural := Natural (quad (quad'First + 2));
      D_byte : constant Natural := Natural (quad (quad'First + 3));
   begin
      case dialect is
         when little_endian =>
            result := A_byte + (B_byte * 256) + (C_byte * 65_536) + (D_byte * 16_777_216);
         when big_endian =>
            result := D_byte + (C_byte * 256) + (B_byte * 65_536) + (A_byte * 16_777_216);
      end case;
      return result;
   end value;


   -------------
   --  value  --
   -------------
   function value (dialect : endianess; octo : elf_octo) return offset_64
   is
      result     : offset_64 := 0;
      multiplier : offset_64;
      bytex      : offset_64;
      spos       : Natural := 0;
   begin
      case dialect is
         when little_endian =>
            for x in octo'Range loop
               bytex := offset_64 (octo (x));
               if bytex > 0 then
                  multiplier := 2 ** (8 * spos);
                  result := result + (bytex * multiplier);
               end if;
               spos := spos + 1;
            end loop;
         when big_endian =>
            for x in reverse octo'Range loop
               bytex := offset_64 (octo (x));
               if bytex > 0 then
                  multiplier := 2 ** (8 * spos);
                  result := result + (bytex * multiplier);
               end if;
               spos := spos + 1;
            end loop;
      end case;
      return result;
   end value;


   ---------------------
   --  read_sections  --
   ---------------------
   procedure read_sections
     (elf_handle : SIO.File_Type;
      elf_stmaxs : SIO.Stream_Access;
      file_data  : ELF_File;
      sections   : in out Extracted_Sections)
   is
      SH_start : constant offset_64 := file_data.section_header_offset + 1;
   begin
      SIO.Set_Index (elf_handle, SIO.Positive_Count (SH_start));
      for x in 1 .. file_data.SHT_nument loop
         case file_data.format is
            when format32 =>
               declare
                  lect : endianess renames file_data.dialect;
                  sechead : ELF_Section_Header_32;
                  section_type_id : Natural;
                  new_note : generic_elf_section;
               begin
                  ELF_Section_Header_32'Read (elf_stmaxs, sechead);
                  section_type_id := value (lect, sechead.sh_type);
                  case section_type_id is
                     when SHT_DYNAMIC =>
                        sections.dynamic.important_section := dynamic;
                        sections.dynamic.file_offset := offset_64 (value (lect, sechead.sh_offset));
                        sections.dynamic.data_size   := offset_64 (value (lect, sechead.sh_size));
                        sections.found_dynamic := True;
                     when SHT_STRTAB =>
                        if not sections.found_string_table then
                           sections.strtab.important_section := string_table;
                           sections.strtab.file_offset :=
                             offset_64 (value (lect, sechead.sh_offset));
                           sections.strtab.data_size   := offset_64 (value (lect, sechead.sh_size));
                           sections.found_string_table := True;
                        end if;
                     when SHT_NOTE =>
                        new_note.important_section := note;
                        new_note.file_offset := offset_64 (value (lect, sechead.sh_offset));
                        new_note.data_size   := offset_64 (value (lect, sechead.sh_size));
                        sections.notes.Append (new_note);
                     when others =>
                        null;
                  end case;
               exception
                  when others => null;
               end;
            when format64 =>
               declare
                  lect : endianess renames file_data.dialect;
                  sechead : ELF_Section_Header_64;
                  section_type_id : Natural;
                  new_note : generic_elf_section;
               begin
                  ELF_Section_Header_64'Read (elf_stmaxs, sechead);
                  section_type_id := value (lect, sechead.sh_type);
                  case section_type_id is
                     when SHT_DYNAMIC =>
                        sections.dynamic.important_section := dynamic;
                        sections.dynamic.file_offset := value (lect, sechead.sh_offset);
                        sections.dynamic.data_size   := value (lect, sechead.sh_size);
                        sections.found_dynamic := True;
                     when SHT_STRTAB =>
                        if not sections.found_string_table then
                           sections.strtab.important_section := string_table;
                           sections.strtab.file_offset := value (lect, sechead.sh_offset);
                           sections.strtab.data_size   := value (lect, sechead.sh_size);
                           sections.found_string_table := True;
                        end if;
                     when SHT_NOTE =>
                        new_note.important_section := note;
                        new_note.file_offset := value (lect, sechead.sh_offset);
                        new_note.data_size   := value (lect, sechead.sh_size);
                        sections.notes.Append (new_note);
                     when others =>
                        null;
                  end case;
               exception
                  when others => null;
               end;
         end case;
      end loop;
   end read_sections;


   ------------------------------
   --  store_note_definitions  --
   ------------------------------
   procedure store_note_definitions
     (elf_handle : SIO.File_Type;
      elf_stmaxs : SIO.Stream_Access;
      file_data  : in out ELF_File;
      sections   : Extracted_Sections)
   is
      procedure get_note_data (Position : note_sections.Cursor)
      is
         note_size   : constant offset_64 := note_sections.Element (Position).data_size;
         note_offset : constant offset_64 := note_sections.Element (Position).file_offset + 1;

         type elf_string is array (1 .. Natural (note_size)) of elf_byte;
         datastring    : elf_string;
         name_size     : elf_quad;
         desc_size     : elf_quad;
         note_type     : elf_quad;
         name_size_nat : Natural;
         desc_size_nat : Natural;
         new_elf_note  : ELF_Note;
         name_index    : Natural;
         ds_index      : Natural;
         esx           : Natural := 1;
      begin
         if note_size < 12 then
            return;
         end if;

         SIO.Set_Index (elf_handle, SIO.Positive_Count (note_offset));
         elf_string'Read (elf_stmaxs, datastring);

         loop
            name_size := elf_quad (DataString (esx + 0 .. esx + 3));
            desc_size := elf_quad (DataString (esx + 4 .. esx + 7));
            note_type := elf_quad (DataString (esx + 8 .. esx + 11));
            name_index := esx + 12;

            name_size_nat := Value (file_data.dialect, name_size);
            desc_size_nat := Value (file_data.dialect, desc_size);
            new_elf_note.note_type   := Value (file_data.dialect, note_type);
            new_elf_note.name        := ASU.Null_Unbounded_String;
            new_elf_note.description := ASU.Null_Unbounded_String;

            if name_size_nat > 1 then
               --  size = 0 is a null string
               --  size = 1 is a blank string (first character is null character)
               --  Treat size = 0 and size = 1 the same
               declare
                  name_string : String (1 .. name_size_nat - 1) := (others => ' ');
               begin
                  for x in 0 .. name_size_nat - 2 loop
                     ds_index := name_index + x;
                     name_string (x + 1) := Character'Val (Integer (DataString (ds_index)));
                  end loop;
                  new_elf_note.name := ASU.To_Unbounded_String (name_string);
               end;
            end if;

            case name_size_nat is
               when  0 => null;       -- name_index is still esx + 12
               when  1 |  2 |  3 |  4 => name_index := esx + 16;
               when  5 |  6 |  7 |  8 => name_index := esx + 20;
               when  9 | 10 | 11 | 12 => name_index := esx + 24;
               when 13 | 14 | 15 | 16 => name_index := esx + 28;
               when 17 | 18 | 19 | 20 => name_index := esx + 32;
               when 21 | 22 | 23 | 24 => name_index := esx + 36;
               when 25 | 26 | 27 | 28 => name_index := esx + 40;
               when 29 | 30 | 31 | 32 => name_index := esx + 44;
               when 33 | 34 | 35 | 36 => name_index := esx + 48;
               when 37 | 38 | 39 | 40 => name_index := esx + 52;
               when others            => name_index := esx + 56;   --  This should be impossible
            end case;
            if desc_size_nat > 1 then
               --  Unlike name, description is not necessarily null-terminated.
               declare
                  desc_string : String (1 .. desc_size_nat) := (others => ' ');
               begin
                  for x in 0 .. desc_size_nat - 1 loop
                     ds_index := name_index + x;
                     desc_string (x + 1) := Character'Val (Integer (DataString (ds_index)));
                  end loop;
                  new_elf_note.description := ASU.To_Unbounded_String (desc_string);
               end;
            end if;

            file_data.notes.Append (new_elf_note);
            esx := name_index + desc_size_nat;
            exit when esx >= Natural (note_size);
         end loop;

      exception
         when others => null;
      end get_note_data;
   begin
      sections.notes.Iterate (get_note_data'Access);
   end store_note_definitions;


   ----------------
   --  char2hex  --
   ----------------
   function char2hex (quattro : Character) return hexrep
   is
      type halfbyte is mod 2 ** 4;
      type fullbyte is mod 2 ** 8;
      function halfbyte_to_hex (value : halfbyte) return Character;

      std_byte  : interfaces.Unsigned_8;
      work_4bit : halfbyte;
      result    : hexrep;

      function halfbyte_to_hex (value : halfbyte) return Character
      is
         zero     : constant Natural := Character'Pos ('0');
         alpham10 : constant Natural := Character'Pos ('a') - 10;
      begin
         case value is
            when 0 .. 9 => return Character'Val (zero + Natural (value));
            when others => return Character'Val (alpham10 + Natural (value));
         end case;
      end halfbyte_to_hex;

   begin
      std_byte   := interfaces.Unsigned_8 (Character'Pos (quattro));
      work_4bit  := halfbyte (interfaces.Shift_Right (std_byte, 4));
      result (1) := halfbyte_to_hex (work_4bit);

      work_4bit  := halfbyte (fullbyte (Character'Pos (quattro)) and 2#1111#);
      result (2) := halfbyte_to_hex (work_4bit);

      return result;
   end char2hex;


   -------------------------
   --  scan_dynamic_data  --
   -------------------------
   procedure scan_dynamic_data
     (elf_handle : SIO.File_Type;
      elf_stmaxs : SIO.Stream_Access;
      file_data  : ELF_File;
      dynsection : generic_elf_section;
      dyn_data   : in out Relevant_Dynamic_Data.Vector)
   is
      bytes_read : offset_64 := 0;
   begin
      SIO.Set_Index (elf_handle, SIO.Positive_Count (dynsection.file_offset + 1));
      loop
         declare
            data32 : Dynamic_Structure32;
            data64 : Dynamic_Structure;
         begin
            case file_data.format is
               when format32 =>
                  Dynamic_Structure32'Read (elf_stmaxs, data32);
                  data64.d_tag := offset_64 (data32.d_tag);
                  data64.d_val := offset_64 (data32.d_val);
                  bytes_read := bytes_read + 8;
               when format64 =>
                  Dynamic_Structure'Read (elf_stmaxs, data64);
                  bytes_read := bytes_read + 16;
            end case;
            case data64.d_tag is
               when DT_SONAME | DT_NEEDED | DT_RPATH | DT_RUNPATH =>
                  dyn_data.Append (data64);
               when DT_NULL =>
                  exit;
               when others => null;
            end case;
            exit when bytes_read >= dynsection.data_size;
         end;
      end loop;
   end scan_dynamic_data;


   -----------------------------
   --  populate_dynamic_data  --
   -----------------------------
   procedure populate_dynamic_data
     (elf_handle : SIO.File_Type;
      elf_stmaxs : SIO.Stream_Access;
      file_data  : in out ELF_File;
      strtable   : generic_elf_section;
      dyn_data   : Relevant_Dynamic_Data.Vector)
   is
      type canvas_type is array (1 .. Natural (strtable.data_size)) of elf_byte;
      canvas : canvas_type;

      function extract_string (table_offset : offset_64) return String
      is
         last_index : Natural;
         result_size : Natural;
         offset : constant Natural := Natural (table_offset);
      begin
         if table_offset > canvas'Length - 1 then
            return "";
         end if;
         if Natural (canvas (offset + 1)) = 0 then
            return "";
         end if;

         last_index := offset + 1;
         result_size := 1;
         loop
            exit when last_index + 1 > canvas'Last;
            exit when Natural (canvas (last_index + 1)) = 0;
            last_index := last_index + 1;
            result_size := result_size + 1;
         end loop;
         declare
            result : String (1 .. result_size);
            index  : Natural := offset + 1;
         begin
            for x in 1 .. result_size loop
               result (x) := Character'Val (Integer (canvas (index)));
               index := index + 1;
            end loop;
            return result;
         end;
      end extract_string;

      procedure process (Position : Relevant_Dynamic_Data.Cursor)
      is
         table_offset : offset_64 renames Relevant_Dynamic_Data.Element (Position).d_val;
         data_value   : constant String := extract_string (table_offset);
      begin
         case Relevant_Dynamic_Data.Element (Position).d_tag is
            when DT_SONAME =>
               file_data.soname := ASU.To_Unbounded_String (data_value);
            when DT_NEEDED =>
               file_data.libs_needed.Append (ASU.To_Unbounded_String (data_value));
            when DT_RPATH | DT_RUNPATH =>
               file_data.run_path := ASU.To_Unbounded_String (data_value);
            when others =>
               null;  -- should not be possible
         end case;
      end process;
   begin
      SIO.Set_Index (elf_handle, SIO.Positive_Count (strtable.file_offset + 1));
      canvas_type'Read (elf_stmaxs, canvas);
      dyn_data.Iterate (Process'Access);
   end populate_dynamic_data;


end Elf;
