--  SPDX-License-Identifier: ISC
--  Reference: /License.txt

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
private with Ada.Streams.Stream_IO;

package Elf is

   package ASU renames Ada.Strings.Unbounded;
   package CON renames Ada.Containers;

   type endianess is (little_endian, big_endian);
   type elf_class is (format32, format64);
   subtype elf_version is Positive range 1 .. 1;
   type offset_64 is mod 2 ** 64;
   type offset_32 is mod 2 ** 32;
   type os_abi is (system_v, hp_ux, netbsd, linux, hurd, solaris, aix, irix, freebsd, tru64,
                   novell_modesto, openbsd, openvms, nonstop_kernel, aros, fenix,
                   nuxi_cloudabi, openvos);
   type type_of_file is (not_elf, relocatable, executable, shared_object, core_file,
                         reserved_os_range, reserved_proc_range, unknown_type);
   type machine_type is (no_instruction, x86, x86_64, powerpc, powerpc64, arm, aarch64,
                         some_random_system);

   type ELF_Note is
      record
         name        : ASU.Unbounded_String;
         description : ASU.Unbounded_String;
         note_type   : Natural;
      end record;

   package note_crate is new CON.Vectors
     (Index_Type   => Natural,
      Element_Type => ELF_Note);

   package library_crate is new CON.Vectors
     (Index_Type   => Natural,
      Element_Type => ASU.Unbounded_String,
      "="          => ASU."=");

   type ELF_File is
      record
         dialect     : endianess;
         format      : elf_class;
         version     : elf_version;
         abi         : os_abi;
         file_type   : type_of_file;
         machine     : machine_type;
         run_path    : ASU.Unbounded_String;
         soname      : ASU.Unbounded_String;
         notes       : note_crate.Vector;
         libs_needed : library_crate.Vector;

         program_header_offset : offset_64;
         section_header_offset : offset_64;
         header_size           : Natural;
         PHT_entsize           : Natural;
         PHT_nument            : Natural;
         SHT_entsize           : Natural;
         SHT_nument            : Natural;

      end record;

   procedure readelf (file_path : String; file_data : in out ELF_File);

   procedure print_elf_information (file_path : String);

private

   package SIO renames Ada.Streams.Stream_IO;

   subtype magic_string is String (1 .. 4);
   type elf_byte  is mod 2 ** 8;
   type elf_word is array (1 .. 2) of elf_byte;
   type elf_quad is array (1 .. 4) of elf_byte;
   type elf_octo is array (1 .. 8) of elf_byte;
   type pad_type is array (1 .. 4) of elf_byte;
   type sixteen  is array (1 .. 16) of elf_byte;
   type an_important_section is (dynamic, note, string_table);
   subtype hexrep is String (1 .. 2);

   DT_NULL     : constant offset_64 := 0;
   DT_NEEDED   : constant offset_64 := 1;
   DT_SONAME   : constant offset_64 := 14;
   DT_RPATH    : constant offset_64 := 15;
   DT_RUNPATH  : constant offset_64 := 29;

   SHT_STRTAB  : constant Natural := 3;
   SHT_DYNAMIC : constant Natural := 6;
   SHT_NOTE    : constant Natural := 7;

   type ELF_Header is
      record
         magic    : magic_string;
         class    : elf_byte;
         data     : elf_byte;
         elf_ver  : elf_byte;
         osabi    : elf_byte;
         pad1     : pad_type;
         pad2     : pad_type;
         block2   : sixteen;
         block3   : sixteen;
         block4   : sixteen;
      end record;

   for ELF_Header'Size use 512;
   for ELF_Header'Alignment use 16;
   for ELF_Header use
      record
         magic    at  0 range 0 .. 31;
         class    at  4 range 0 .. 7;
         data     at  5 range 0 .. 7;
         elf_ver  at  6 range 0 .. 7;
         osabi    at  7 range 0 .. 7;
         pad1     at  8 range 0 .. 31;
         pad2     at 12 range 0 .. 31;

         block2   at 16 range 0 .. 127;
         block3   at 32 range 0 .. 127;
         block4   at 48 range 0 .. 127;
      end record;

   function value (dialect : endianess; byte1, byte2 : elf_byte) return Natural;
   function value (dialect : endianess; quad : elf_quad) return Natural;
   function value (dialect : endianess; octo : elf_octo) return offset_64;
   function char2hex (quattro : Character) return hexrep;

   type ELF_Section_Header_64 is
      record
         sh_name      : elf_quad;  -- 0x00
         sh_type      : elf_quad;  -- 0x04
         sh_flags     : elf_octo;  -- 0x08
         sh_addr      : elf_octo;  -- 0x10
         sh_offset    : elf_octo;  -- 0x18
         sh_size      : elf_octo;  -- 0x20
         sh_link      : elf_quad;  -- 0x28
         sh_info      : elf_quad;  -- 0x2C
         sh_addralign : elf_octo;  -- 0x30
         sh_entsize   : elf_octo;  -- 0x38
      end record;
   for ELF_Section_Header_64'Size use 512;
   for ELF_Section_Header_64'Alignment use 8;
   for ELF_Section_Header_64 use
      record
         sh_name      at  0 range 0 .. 31;
         sh_type      at  4 range 0 .. 31;
         sh_flags     at  8 range 0 .. 63;
         sh_addr      at 16 range 0 .. 63;
         sh_offset    at 24 range 0 .. 63;
         sh_size      at 32 range 0 .. 63;
         sh_link      at 40 range 0 .. 31;
         sh_info      at 44 range 0 .. 31;
         sh_addralign at 48 range 0 .. 63;
         sh_entsize   at 56 range 0 .. 63;
      end record;

   type ELF_Section_Header_32 is
      record
         sh_name      : elf_quad;  -- 0x00
         sh_type      : elf_quad;  -- 0x04
         sh_flags     : elf_quad;  -- 0x08
         sh_addr      : elf_quad;  -- 0x0C
         sh_offset    : elf_quad;  -- 0x10
         sh_size      : elf_quad;  -- 0x14
         sh_link      : elf_quad;  -- 0x18
         sh_info      : elf_quad;  -- 0x1C
         sh_addralign : elf_quad;  -- 0x20
         sh_entsize   : elf_quad;  -- 0x24
      end record;
   for ELF_Section_Header_32'Size use 320;
   for ELF_Section_Header_32'Alignment use 4;
   for ELF_Section_Header_32 use
      record
         sh_name      at  0 range 0 .. 31;
         sh_type      at  4 range 0 .. 31;
         sh_flags     at  8 range 0 .. 31;
         sh_addr      at 12 range 0 .. 31;
         sh_offset    at 16 range 0 .. 31;
         sh_size      at 20 range 0 .. 31;
         sh_link      at 24 range 0 .. 31;
         sh_info      at 28 range 0 .. 31;
         sh_addralign at 32 range 0 .. 31;
         sh_entsize   at 36 range 0 .. 31;
      end record;

   type generic_elf_section is
      record
         important_section : an_important_section;
         file_offset       : offset_64;
         data_size         : offset_64;
      end record;

   package note_sections is new CON.Vectors
     (Index_Type   => Natural,
      Element_Type => generic_elf_section);

   type Extracted_Sections is
      record
         dynamic : generic_elf_section;
         strtab  : generic_elf_section;
         notes   : note_sections.Vector;
         found_dynamic      : Boolean := False;
         found_string_table : Boolean := False;
      end record;

   type Dynamic_Structure is
      record
         d_tag : offset_64;
         d_val : offset_64;
      end record;

   type Dynamic_Structure32 is
      record
         d_tag : offset_32;
         d_val : offset_32;
      end record;

   package Relevant_Dynamic_Data is new CON.Vectors
     (Index_Type   => Natural,
      Element_Type => Dynamic_Structure);

   procedure read_sections
     (elf_handle : SIO.File_Type;
      elf_stmaxs : SIO.Stream_Access;
      file_data  : ELF_File;
      sections   : in out Extracted_Sections);

   procedure store_note_definitions
     (elf_handle : SIO.File_Type;
      elf_stmaxs : SIO.Stream_Access;
      file_data  : in out ELF_File;
      sections   : Extracted_Sections);

   procedure scan_dynamic_data
     (elf_handle : SIO.File_Type;
      elf_stmaxs : SIO.Stream_Access;
      file_data  : ELF_File;
      dynsection : generic_elf_section;
      dyn_data   : in out Relevant_Dynamic_Data.Vector);

   procedure populate_dynamic_data
     (elf_handle : SIO.File_Type;
      elf_stmaxs : SIO.Stream_Access;
      file_data  : in out ELF_File;
      strtable   : generic_elf_section;
      dyn_data   : Relevant_Dynamic_Data.Vector);

end Elf;
